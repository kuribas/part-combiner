;;;; part-combiner.scm -- Part combining, staff changes.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2005	Han-Wen Nienhuys <hanwen@cs.uu.nl>

(use-modules (srfi srfi-8))
(use-modules (srfi srfi-9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; let* with multiple values
;;;
;;; works like let*, but allows multiple names to take
;;; multiple values.
;;;
;;; i.e.:
;;;   (multi-let* ((a b (values 1 2)))
;;;     (+ a b))   => 3
;;;
;;;

(define-macro (multi-let* clauses . bodies)
  (letrec ((split-last (lambda (head tail)
			 (if (null? (cdr tail))
			     (values (reverse! head) (car tail))
			     (split-last (cons (car tail) head)
					 (cdr tail))))))
    (let ((clause1 (car clauses))
	  (rest (if (null? (cdr clauses))
		    bodies
		    `((multi-let* ,(cdr clauses) ,@bodies)))))
      (if (null? (cddr clause1))
	  `(let (,clause1) ,@rest)
	  (call-with-values
	      (lambda () (split-last (list (car clause1)) (cdr clause1)))
	    (lambda (names val)
	      `(call-with-values (lambda () ,val)
		 (lambda ,names ,@rest))))))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (recording-group-emulate music odef) 
  "Interprets music according to odef, but stores all events in a chronological list, similar to the Recording_group_engraver in 2.8 and earlier"
  (let*
      ((context-list '())
       (now-mom (ly:make-moment 0 0))
       (global (ly:make-global-context odef))
       (mom-listener (ly:make-listener 
		      (lambda (tev)
			(set! now-mom (ly:event-property tev 'moment)))))
       (new-context-listener
	(ly:make-listener
	 (lambda (sev)
	     (let*
		 ((child (ly:event-property sev 'context))
		  (this-moment-list
		   (cons (ly:context-id child) '()))
		  (dummy
		   (set! context-list (cons this-moment-list context-list)))
		  (acc '())
		  (accumulate-event-listener
		   (ly:make-listener (lambda (ev)
				       (set! acc (cons (cons ev #t) acc)))))
		  (save-acc-listener (ly:make-listener (lambda (tev)
							 (if (pair? acc)
							     (let ((this-moment (cons (cons now-mom (ly:context-property child 'instrumentTransposition))
										      acc)))
							       (set-cdr! this-moment-list (cons this-moment (cdr this-moment-list)))
							       (set! acc '())))))))
	       (ly:add-listener accumulate-event-listener (ly:context-event-source child) 'music-event)
	       (ly:add-listener save-acc-listener (ly:context-event-source global) 'OneTimeStep))))))
    (ly:add-listener new-context-listener (ly:context-events-below global) 'AnnounceNewContext)
    (ly:add-listener mom-listener (ly:context-event-source global) 'Prepare)
    (ly:interpret-music-expression (make-non-relative-music music) global))
  context-list)

; UGH - should pass noticed setter to part-combine-listener
(define-safe-public (set-part-combine-listener x)
  (set! part-combine-listener x))

(define-public (notice-the-events-for-pc context lst)
  "add CONTEXT-ID, EVENT list to NOTICED variable."
  
  (set! noticed (acons (ly:context-id context) lst noticed)))

(define-public (make-part-combine-music parser music-list)
  (let* ((m (make-music 'PartCombineMusic))
         (m1 (make-non-relative-music (context-spec-music (first music-list) 'Voice "one")))
         (m2  (make-non-relative-music  (context-spec-music (second music-list) 'Voice "two")))
         (listener (ly:parser-lookup parser 'partCombineListener))
         (evs2 (recording-group-emulate m2 listener))
         (evs1 (recording-group-emulate m1 listener)))
    
    (set! (ly:music-property m 'elements) (list m1 m2))
    (set! (ly:music-property m 'split-list)
          (if (and (assoc "one" evs1) (assoc "two" evs2))
              (determine-split-list (reverse! (cdr (assoc "one" evs1)) '())
                                    (reverse! (cdr (assoc "two" evs2)) '()))
              '() ))
    m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Roadmap for determine-split-list:
;; 
;; - Collect the lengths of notes and spanners in a list.  Each element
;;   of the list is an item-length type with three fields: item,
;;   start-moment, end-moment.
;; 
;; - Give a symbolic representation of the configuration types by
;;   comparing the voices at each moment.  Each configuration is a list
;;   with three elements:
;; 
;;   * input-config: the configuration of the input, one of:
;;     - mono: monophonic music
;;     - poly1: polyphonic music with one voice resting
;;     - poly2: polyphonic music with two voices playing
;;     - voice1: voice1 playing while voice2 rests
;;     - voice2: voice2 playing while voice1 rests
;;     - none: no voice playing
;; 
;;   * output-config: how to print the voices:
;;     - apart: two voices apart
;;     - chords: two different voices sharing a stem
;;     - unisono: both voices are merged
;;     - solo1: only voice1 is printed
;;     - solo2: only voice2 is printed
;;     - unisilence: the rests are merged
;; 
;;   * note-started: #t if a note has started playing at this moment.
;; 
;;   The keep symbol means the same value as the previous moment, and it
;;   is replaced in the follow-kept-types! function.
;; 
;; - Collect the measure-positions for each moment, and calculate the
;;   limits that depend on the size of the measure.  They are also put in
;;   a list to because they may change during a moment.
;; 
;; - Make sure that some rests and solos aren't merged, to enable a good
;;   flow of the voices.
;; 
;; - Make sure that solo, a due, and chord parts are used in blocks, to
;;   prevent having too many changes in the score.
;; 
;; - repeat the last two steps until no changes are made.
;; 
;; - extract the output configuration, and put it in an alist with each
;;   moment: ((moment . config-type) ... )
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (determine-split-list evl1 evl2)
  "EVL1 and EVL2 should be ascending"
  (multi-let*
      ((moments (merge-moments evl1 evl2))
       (notes1 (note-lengths evl1))
       (notes2 (note-lengths evl2))
       (span1 (spanner-lengths evl1))
       (span2 (spanner-lengths evl2))
       (measurepos (measure-positions moments evl1 evl2))
       (music-end longest (music-end+longest evl1 evl2))
       (measurelimits (measure-limits longest))
       (types (make-type-context-list moments notes1 notes2
				      span1 span2 measurepos
				      measurelimits)))
    (fix-rests-in-measures! types #f #f)
    (let loop ()
      (if (fix-blocks! types #f #t music-end)
	  (if (fix-rests-in-measures! types #f #f)
	      loop)))
    (synthesize-types types)))

(define (event-name event)
  (ly:music-property event 'name))

(define (ly:moment<=? m1 m2)
  (not (ly:moment<? m2 m1)))

;; fold with multiple arguments
(define (multi-fold fun inits list)
  (if (null? list) inits
      (multi-fold fun 
		  (apply fun (car list) inits)
		  (cdr list))))

;; a type that represents the lengts of items (notes or spanners)
(define (make-item-length item start end)
  (list item start end))

(define get-item first)
(define start-moment second)
(define end-moment third)

(define mom-events:moment caar)
(define mom-events:properties cdar)
(define mom-events:events cdr)

;; return lengths of the notes
(define (note-lengths events-list)
  (define (handle-events evmoment collected active)
    (multi-let* ((moment (mom-events:moment evmoment))
		 (events (mom-events:events evmoment))
		 (type note (extract-event events)))
      (letrec ((note-end (lambda () (ly:add-moment (ly:music-length note)
						      moment)))
	       (append-note (lambda () (make-item-length (car active) (cdr active)
							 (note-end))))
	       (make-note (lambda () (make-item-length note moment (note-end))))
	       (flush-active (lambda () (make-item-length (car active) (cdr active)
							  moment)))
	       (collect (lambda (item) (list (cons item collected) #f)))
	       (activate (lambda (item) (list collected (cons item moment))))
	       (pass (lambda () (list collected active))))
	(case type
	  ((tied-note)
	   (if active (pass) (activate note)))
	  ((note)
	   (if active
	       (collect (append-note))
	       (collect (make-note))))
	  ((rest)
	   (if active
	       (collect (flush-active))  ; this could also raise an error
	       (pass)))
	  (else (pass))))))
	   
  (reverse! (first (multi-fold handle-events '(() #f) events-list))))

(define (extract-note event-list)
  (let ((notes (filter (lambda (e)
			 (equal? (event-name e) 'NoteEvent))
		       event-list)))
    (cond ((null? notes) #f)
	  ((pair? (cdr notes)) 'chord)
	  (else (car notes)))))

(define (extract-event events)
  (let ((find-event (lambda (name)
		     (find (lambda (event)
			     (equal? (event-name event) name))
			   events))))
    (cond ((find-event 'TieEvent)
	   (values 'tied-note (extract-note events)))
	  ((extract-note events)
	   => (lambda (note) (values 'note note)))
	  ((or (find-event 'RestEvent)
	       (find-event 'MultiMeasureEvent))
	   (values 'rest 'nil))
	  (else (values 'nil 'nil)))))

;; return a list of lists: (spanner span-start span-end)
(define (spanner-lengths evlist)

  (define spanners '(SlurEvent PhrasingSlurEvent BeamEvent
			       CrescendoEvent DecrescendoEvent))

  (define (starting-spanners events)
    (sort
     (filter-map
      (lambda (event)
	(let ((name (event-name event)))
	  (and (member name spanners)
	       (equal? (ly:music-property event 'span-direction)
		       START)
	       name)))
      events)
     (lambda (s1 s2)
       (string<? (symbol->string s1) (symbol->string s2)))))

  (define (stopping-spanners events)
    (append-map!
     (lambda (event)
       (let ((name (event-name event)))
	 (cond ((and (member name spanners)
		     (equal? (ly:music-property event 'span-direction)
			     STOP))
		(list name))
	       ((member name '(CrescendoEvent DecrescendoEvent AbsoluteDynamicEvent))
		(list 'CrescendoEvent 'DecrescendoEvent))
	       (else '()  ))))
     events))

  ;; stop all given spanners and move them from active to collected
  ;; this function checks each active spanner
  (define (stop-spanners spanners moment collected active new-active)
    (if (null? active)
	(list collected (reverse! new-active))
	(let ((name (caar active))
	      (start-moment (cdar active)))
	  (if (member name spanners)
	      (stop-spanners spanners
			     moment
			     (cons (make-item-length name start-moment moment)
				   collected)
			     (cdr active)
			     new-active)
	      (stop-spanners spanners
			     moment
			     collected
			     (cdr active)
			     (cons (car active) new-active))))))

  ;; add new spanners to active
  (define (start-spanners collected spanners active moment)
    (list collected
	  (fold (lambda (spanner active)
		  (alist-cons spanner moment active))
		active spanners)))

  ;; add starting spanners to active and ended spanners to spanlist
  (define (update-spanner-list events collected active)
    (let ((moment (mom-events:moment events))
	  (events (mom-events:events events)))
      (let ((stop-span (and (pair? active)
			    (stopping-spanners events)))
	    (start-span (starting-spanners events)))
	(apply
	 (lambda (collected active)
	   (start-spanners collected start-span active moment))
	 (if stop-span
	     (stop-spanners stop-span moment collected active '())
	     (list collected active))))))

  (sort (first (multi-fold update-spanner-list '(() ()) evlist))
	(lambda (s1 s2) (ly:moment<? (start-moment s1)
				     (start-moment s2)))))

(define (notes-next-moment notes moment)
  (drop-while (lambda (n)
		(ly:moment<=? (end-moment n) moment))
	      notes))

(define (starting-note notes moment)
  (and (pair? notes)
       (let ((note (car notes)))
	 (and (equal? moment (start-moment note))
	      (get-item note)))))

(define (current-note-end notes)
  (end-moment (car notes)))

(define (notes-resting-until? notes until)
  (or (null? notes)
      (ly:moment<=? until (start-moment (car notes)))))

(define (notes-resting-at? notes moment)
  (or (null? notes)
      (ly:moment<? moment (start-moment (car notes)))))

(define (notes-equal-end? notes1 notes2)
  (equal? (current-note-end notes1)
	  (current-note-end notes2)))

(define (make-span-moment spanner)
  (cons spanner '()))

(define span-moment:after car)  ; spanners that start after the current moment
(define span-moment:active cdr) ; active spanners

;; this function modifies the previous moment to
;; avoid creating to many cons cells every iteration
;; However it doesn't modify the list of spanners
(define (spanners-next-moment! span-moment moment)
  (multi-let*
      ((spanners (span-moment:after span-moment))
       (active (span-moment:active span-moment))
       (during-moment? (lambda (s)
			 (ly:moment<=? moment (end-moment s))))
       (head tail (span (lambda (s)
			  (ly:moment<=? (start-moment s) moment))
			spanners))
       (new-active (append! (filter! during-moment? head)
			    (filter! during-moment? active))))
    (cons tail new-active)))

(define (spanners-equal? span1 span2 until-ev)
  (let ((before-moment? (lambda (s)
			  (ly:moment<? (start-moment s) until-ev))))
    (and (equal? (span-moment:active span1) (span-moment:active span2)) ;active spanners equal?
	 (equal? (take-while before-moment? (span-moment:after span1))
		 (take-while before-moment? (span-moment:after span2))))))

(define (spanners-resting-until? span moment)
  (and (null? (span-moment:active span))		;no active spanners
       (or (null? (span-moment:after span))
	   (ly:moment<=? moment (start-moment (car (span-moment:after span)))))))

(define (equal-pitch? note1 note2)
  (equal? (ly:music-property note1 'pitch)
	  (ly:music-property note2 'pitch)))

(define (within-octave? note1 note2)
  (let* ((p1 (ly:music-property note1 'pitch))
	 (p2 (ly:music-property note2 'pitch))
	 (steps (ly:pitch-steps (ly:pitch-diff p1 p2))))
    (<= 1 steps 8)))

(define (type-at-moment moment notes1 notes2 span1 span2)
  (let ((note1 (starting-note notes1 moment))
	(note2 (starting-note notes2 moment)))
    (cond ((or (eq? note1 'chord)
	       (eq? note2 'chord))
	   '(poly2 apart #t))
	  ((and note1 note2)
	   (cond ((not (notes-equal-end? notes1 notes2))
		  '(poly2 apart #t))
		 ((not (spanners-equal? span1 span2 (current-note-end notes1)))
		  '(mono apart #t))
		 ((equal-pitch? note1 note2)
		  '(mono unisono #t))
		 ((within-octave? note1 note2)
		  '(mono chords #t))
		 (else '(mono apart #t))))
	  (note1 (cond ((not (notes-resting-until? notes2 (current-note-end notes1)))
			'(poly1 apart #t))
		       ((not (spanners-resting-until? span2 (current-note-end notes1)))
			'(voice1 apart #t))
		       (else '(voice1 solo1 #t))))
	  (note2 (cond ((not (notes-resting-until? notes1 (current-note-end notes2)))
			'(poly1 apart #t))
		       ((not (spanners-resting-until? span1 (current-note-end notes2)))
			'(voice2 apart #t))
		       (else '(voice2 solo2 #t))))
	  ((and (notes-resting-at? notes1 moment)
		(notes-resting-at? notes2 moment))
	   '(none unisilence #f))
	  (else 'keep))))

(define (synthesize-types types)
  (map (lambda (type)
	 (cons (tc-moment type)
	       (second (tc-types type))))
       types))

;; efficient modifying unique
(define (unique! lst)
  (let uniquify ((lst lst))
    (if (pair? lst)
	(let* ((elem (car lst))
	       (next (drop-while (lambda (e)
				   (equal? elem e))
				 lst)))
	  (set-cdr! lst next)
	  (uniquify next))))
  lst)

;; Find the next context such that moment is
;; smaller that the next moment of context.
(define (next-moment-fun get-moment)
  (define (next-moment mc moment)
    (let ((next (cdr mc)))
      (cond ((null? next) mc)
	    ((ly:moment<? moment (get-moment (car next))) mc)
	    (else (next-moment next moment)))))
  next-moment)

(define event-list-next
  (next-moment-fun caar))

(define moment-context-next
  (next-moment-fun car))

(define (merge-moments evl1 evl2)
  (unique! (merge (map caar evl1)
		  (map caar evl2)
		  ly:moment<?)))

;;
;; Make an abstract context type.  The context:value element
;; represents some properties that are in effect at the moment.  The
;; context:advance-func is a function that takes a new moment, and
;; returns a new context:value for that moment.
;;

(define (make-context value adv-func)
  (cons value adv-func))

(define context:value car)
(define context:advance-func cdr)

(define (make-notes-context notes)
  (make-context notes notes-next-moment))

(define (make-spanner-context spanner)
  (make-context (make-span-moment spanner) spanners-next-moment!))

(define (make-event-list-context evl)
  (make-context evl event-list-next))

(define (make-moment-context mc)
  (make-context mc moment-context-next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The type-context type contains some context properties together
;; with the configuration types.  It represents a single moment.  It
;; will be put in a list, where the next moment is the next context.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-record-type :type-context
  (make-type-context types moment measurepos minim-block minim-sep)
  type-context?
  (types tc-types tc-set-types!)
  (moment tc-moment)
  (measurepos tc-measurepos)
  (minim-block tc-minimum-block)
  (minim-sep tc-minimum-separation))

(define null-moment (ly:make-moment 0 0))

;; I asume that moments at the beginning of a bar don't
;; have a grace timing, otherwise we might have a bug
;; here.
(define (tc-beginning-of-bar? context)
  (equal? (tc-measurepos context) null-moment))

;; for each moment, advance the contexts using their advancement
;; function, pass the values to fun, and collect the return values of
;; fun.

(define (map-with-contexts fun moments . contexts)
  (let mwc-aux
      ((moments moments)
       (contexts (map context:value contexts))
       (adv-funcs (map context:advance-func contexts))
       (acc '()))
    (if (null? moments)
	(reverse! acc)
	(let* ((moment (car moments))
	       (contexts (map (lambda (f c) (f c moment))
			      adv-funcs
			      contexts))
	       (value (apply fun moment contexts)))
	  (mwc-aux (cdr moments) contexts adv-funcs (cons value acc))))))

(define (find-last pred? lst)
  (let aux ((lst lst) (last #f))
    (cond ((null? lst) last)
	  ((pred? (car lst))
	   (aux (cdr lst) (car lst)))
	  (else
	   (aux (cdr lst) last)))))

(define (music-end+longest evl1 evl2)
  (let* ((find-event (lambda (evl)
		      (find (lambda (e)
			      (memq (event-name e)
				    '(NoteEvent RestEvent MultiMeasureRestEvent)))
			    (cdr evl))))
	 (last1 (find-last find-event evl1))
	 (last2 (find-last find-event evl2))
	 (end1 (ly:moment-add (caar last1)
			      (ly:music-length (find-event last1))))
	 (end2 (ly:moment-add (caar last2)
			      (ly:music-length (find-event last2)))))
    (if (ly:moment<? end1 end2)
	(values end2 evl2)
	(values end1 evl1))))

(define (measure-positions moments evl1 evl2)
  (map-with-contexts
   (lambda (moment events1 events2)
     (cond ((equal? (mom-events:moment (car events1)) moment)
	    (cons moment (cdr (assq 'measurePosition
				    (mom-events:properties (car events1))))))
	   ((equal? (mom-events:moment (car events2)) moment)
	    (cons moment (cdr (assq 'measurePosition
				    (mom-events:properties (car events2))))))
	   (else #f))) ; shouldn't happen
   moments
   (make-event-list-context evl1)
   (make-event-list-context evl2)))

(define (collect-property-values evl prop)
  (filter-map
   (lambda (events)
     (let ((moment (mom-events:moment events))
	   (pair (assq prop (mom-events:properties events))))
       (if pair (cons moment (cdr pair)) #f)))
   evl))
    
(define (measure-limits evl)
  (let ((fractions (collect-property-values evl 'timeSignatureFraction)))
    (map (lambda (prop)
	   (let ((moment (car prop))
		 (minim-block (calc-block-length (cdr prop)))
		 (minim-sep (calc-separation (cdr prop))))
	     (list moment minim-block minim-sep)))
	 fractions)))

(define (calc-block-length fraction)
  (let ((bars (if (< (car fraction) 4)
		  3 2)))
    (ly:make-moment (* (car fraction) bars)
		    (cdr fraction))))
      
(define (calc-separation fraction)
  (let ((bars (if (< (car fraction) 4)
		  2 1)))
    (ly:make-moment (* (car fraction) bars)
                    (cdr fraction))))

(define (make-type-context-list moments notes1
				notes2 span1 span2
				measurepos measurelimits)
  (let ((type-context-list
	 (map-with-contexts
	  (lambda (moment notes1 notes2 span1 span2 measurepos measurelimits)
	    (let ((type (type-at-moment moment notes1 notes2
					span1 span2)))
	      (make-type-context type
				 moment
				 (cdar measurepos) ; extract value from alist
				 (second (car measurelimits))
				 (third (car measurelimits)))))
	  moments
	  (make-notes-context notes1)
	  (make-notes-context notes2)
	  (make-spanner-context span1)
	  (make-spanner-context span2)
	  (make-moment-context measurepos)
	  (make-moment-context measurelimits))))
    (follow-kept-types! type-context-list #f)
    type-context-list))

(define (follow-kept-types! context prev)
  (if (pair? context)
      (let ((types (tc-types (car context))))
	(if (eq? types 'keep)
	    (let ((new-value (list (first prev) (second prev) #f)))
	      (tc-set-types! (car context) new-value)
	      (follow-kept-types! (cdr context) new-value))
	    (follow-kept-types! (cdr context) types)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following rules are used for each bar to ensure that the voices
;; and rests in the bar flow naturally, for example rests will not
;; disappear into nothing:
;;
;;  - Make a new solovoice
;;
;;    * only when preceded by
;;      - monophonic music
;;      - merged rest (unisilence)
;;
;;    * never when the two following voices aren't merged and at least
;;      one resting
;;
;;  - Make a merged rest
;;
;;    * only when preceded by
;;      - music without printed rest
;;      - merged rest
;;
;;    * never when the two following voices aren't merged and at least
;;      one resting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (two-voices-and-rest? context prev)
  (cond ((null? context)                      #f)
	((tc-beginning-of-bar? (car context)) #f) ; next bar
	(else
	 (let* ((types (tc-types (car context)))
		(type1 (first types)))
	   (cond ((memq type1 '(poly2 mono))           #f)
		 ((eq? type1 'poly1)                   #t)
		 ;; type1 == voice1, voice2, or none
		 ((eq? (second types) 'apart)          #t)
		 ((and (eq? type1 'voice1)
		       (eq? (first prev) 'voice2))     #t)
		 ((and (eq? type1 'voice2)
		       (eq? (first prev) 'voice1))     #t)
		 (else (two-voices-and-rest? (cdr context) types)))))))

(define (fix-rests-in-measures! context modified? prev)
  (if (null? context) modified?
      (let* ((types (tc-types (car context)))
	     (type1 (first types))
	     (fix-next (lambda ()
			 (fix-rests-in-measures! (cdr context) modified? types)))
	     (modify-fix-next (lambda ()
				(let ((new-value `(,type1 apart ,(third types))))
				  (tc-set-types! (car context) new-value)
				  (fix-rests-in-measures! (cdr context) #t new-value)))))
	(cond ((eq? (second types) 'apart)
	       (fix-next))
	      ((and (memq type1 '(voice1 voice2))
		    (put-voice-apart? types context prev))
	       (modify-fix-next))
	      ((and (eq? type1 'none)
		    (put-rests-apart? types context prev))
	       (modify-fix-next))
	      (else (fix-next))))))

;; Check if the voice/rests can be merged.
;; These functions are difficult to understand.
;; * If we are at the beginning of a bar:
;;   ->  search forward to see if it is followed by two printed
;;       voices with at least one rest.
;; * If we have a monophonic voice preceding we have to check
;;   again the following notes.
;; * Otherwise we can take the previous value, since for a merged
;;   solo or rests the next notes have been checked already, and
;;   for polyphonic music the notes will never be merged
;; * merging rests is also allowed after polyphonic music without
;; rests (poly2)

(define (put-voice-apart? types context prev)
  (if (or (tc-beginning-of-bar? (car context))
	  (eq? (first prev) 'mono))
      (two-voices-and-rest? (cdr context) types)
      (eq? (second prev) 'apart)))

(define (put-rests-apart? types context prev)
  (if (or (tc-beginning-of-bar? (car context))
	  (memq (first prev) '(mono poly2)))
      (two-voices-and-rest? (cdr context) types)
      (eq? (second prev) 'apart)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following rules are used to make sure that a solo or a due part
;; is only used for continguous blocks, to prevent to many changes in
;; the score.
;;
;; - all notes in a block should be part of the same kind (solo, a
;; due, chords).
;;
;; - a rest is part of the block only if the part of the block that
;; follows the rest is at least as long as the rest.
;;
;; - if a rest is larger than "minimum-separation" it will not be part
;; of the block
;;
;; - for a block to be put into a kind (solo, ...), it has to have at
;; least three notes, and must have a minimum length of
;; "minimum-block".
;;
;; - an isolated block, preceded and followed by a large rest can have
;; any size.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; puts voices apart for blocks that aren't big enough.  The
;; separate? argument indicates if the block should be considered
;; separate from other blocks.
(define (fix-blocks! context modified? separate? music-end)
  (if (null? context)
      modified?
      (let* ((types (tc-types (car context))))
	(cond
	 ((eq? (first types) 'none)  ; skip rests and check if the
	  (receive (separate? next)  ; next block will be separate
		   (skip-to-next context music-end)
	    (fix-blocks! next modified? separate? music-end)))
	 ((eq? (second types) 'apart) ; skip 'apart
	  (fix-blocks! (cdr context) modified? #f music-end))
	 (else
	  ;; type1  == voice1, voice2, mono
	  (receive (modified2? next-context)
		   (fix-following-block! context (second types)
					 separate? music-end)
	    (fix-blocks! next-context
			 (or modified? modified2?)
			 #f music-end)))))))

(define (fix-following-block! context type separate? music-end)
  (multi-let*
      ((after-context num-notes (take-block context type 0 music-end))
       (block-length (duration-between context after-context music-end))
       (separate-after? next-context (skip-to-next after-context music-end))
       (modify? (and (not (and separate-after? separate?)) ; not isolated
		     (or (< num-notes 3)
			 (ly:moment<? block-length
				      (tc-minimum-block (car context)))))))
    (if modify?
	(put-block-apart! context after-context)
	(adjust-rest-types! context after-context type))
    (values modify? after-context)))

(define (put-block-apart! context after-context)
  (if (not (eq? context after-context))
      (let ((types (tc-types (car context))))
	(if (not (eq? (first types) 'none)) ; don't put rests apart here!
	    (tc-set-types! (car context)
			   (list (first types) 'apart (third types))))
	(put-block-apart! (cdr context) after-context))))

(define (adjust-rest-types! context after-context type)
  (if (not (eq? context after-context))
      (let ((types (tc-types (car context))))
	(if (eq? (first types) 'none)
	    (tc-set-types! (car context)
			   (list (first types) type (third types))))
	(adjust-rest-types! (cdr context) after-context type))))

(define (take-block context block-type numnotes music-end)
  (if (null? context)
      (values context numnotes)
      (let ((types (tc-types (car context))))
	(cond ((eq? (first types) 'none)
	       (receive (succeed? next-context numnotes)
			(try-after-rest context block-type numnotes music-end)
		 (if succeed?
		     (take-block next-context block-type numnotes music-end)
		     (values context numnotes))))
	      ((eq? (second types) block-type)
	       (take-block (cdr context) block-type
			   (if (third types) (+ numnotes 1) numnotes)
			   music-end))
	      (else
	       (values context numnotes))))))

(define (duration-between context1 context2 music-end)
  (let ((get-moment (lambda (context)
		      (if (null? context) music-end
			  (tc-moment (car context))))))
    (ly:moment-sub (get-moment context2)
		   (get-moment context1))))

(define (skip-to-next context music-end)
  (receive (length next) (skip-rest context music-end)
    (values (and (pair? context)
		 (ly:moment<=? (tc-minimum-separation (car context))
			       length))
	    next)))

(define (skip-rest context music-end)
  (let* ((next (drop-while (lambda (c)
			     (eq? (first (tc-types c)) 'none))
			   context))
	 (length (duration-between context next music-end)))
    (values length next)))

(define (try-after-rest context block-type numnotes music-end)
  (receive (rest-length block-begin)
	   (skip-rest context music-end)
    (if (ly:moment<=? (tc-minimum-separation (car context)) rest-length)
	(values #f context numnotes)
	(receive (succeed? next-block numnotes2)
		 (take-minimum-block rest-length block-begin
				     block-type numnotes music-end)
	  (if succeed?
	      (values #t next-block numnotes2)
	      (values #f context numnotes))))))

;; Takes the smallest block that is at least min-length long.
;; If there is no such block return the start-position
(define (take-minimum-block min-length start block-type numnotes music-end)
  (let next ((context start)
	     (numnotes numnotes))
    (let ((fail (lambda () (values #f #f #f)))
	  (succeed (lambda (context notes) (values #t context notes)))
	  (add-note (lambda (types)
		      (if (third types) (+ numnotes 1) numnotes)))
	  (length (duration-between start context music-end))
	  (types (and context (tc-types (car context)))))
      (cond ((ly:moment<=? min-length length)
	     (succeed context numnotes))
	    ((null? context)
	     (fail))
	    ((eq? (second types) block-type)    ; same type: continue
	     (next (cdr context) (add-note types)))
	    ((eq? (first types) 'none)          ; rest
	     (receive (succeed? block-end numnotes2)
		      (try-after-rest context block-type numnotes music-end)
	       (if succeed?
		   (next block-end numnotes2) ;continue
		   ;; otherwise succeed if the distance to the next
		   ;; notes is least min-length
		   (multi-let* ((l next-block (skip-rest block-end))
				(length (duration-between start next-block music-end)))
		     (if (ly:moment<? length min-length)
			 (fail)
			 (succeed block-end numnotes2))))))
	    (else (fail))))))                ; end of block before min-length
		   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autochange - fairly related to part combining.

(define-public (make-autochange-music music)
  (define (generate-split-list change-moment event-list acc)
    (if (null? event-list)
	acc
	(let* ((now-tun (caar event-list))
	       (evs (map car (cdar event-list)))
	       (now (car now-tun))
	       (notes (filter (lambda (x)
				(equal? (ly:music-property  x 'name) 'NoteEvent))
			      evs))
	       (pitch (if (pair? notes)
			  (ly:music-property (car notes) 'pitch)
			  #f)))
	  ;; tail recursive.
	  (if (and pitch (not (= (ly:pitch-steps pitch) 0)))
	      (generate-split-list #f
				   (cdr event-list)
				   (cons (cons

					  (if change-moment
					      change-moment
					      now)
					  (sign (ly:pitch-steps pitch))) acc))
	      (generate-split-list
	       (if pitch #f now)
	       (cdr event-list) acc)))))
  
  (set! noticed '())
  (let* ((m (make-music 'AutoChangeMusic))
	 (context (ly:run-translator (make-non-relative-music music) part-combine-listener))
	 (evs (last-pair noticed))
	 (split (reverse! (generate-split-list
			   #f
			   (if (pair? evs)
			       (reverse! (cdar evs) '()) '())
			   '())
			  '())))
    (set! (ly:music-property m 'element) music)
    (set! (ly:music-property m 'split-list) split)
    (set! noticed '())
    m))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (add-quotable parser name mus)
  (let* ((tab (eval 'musicQuotes (current-module)))
         (context-list (recording-group-emulate (context-spec-music mus 'Voice)
                                                (ly:parser-lookup parser 'partCombineListener))))
    (if (pair? context-list)
        (hash-set! tab name
                   ;; cdr : skip name string
                   (list->vector (reverse! (cdar context-list)
                                           '()))))))
