\version "2.4.0"

%% < 1.8 compatibility switch
#(ly:set-option 'old-relative)

%% named durations
breve = #(ly:make-duration -1 0)
longa = #(ly:make-duration -2 0)
maxima = #(ly:make-duration -3 0)

\include "music-functions-init.ly"

%% default note names are dutch
\include "nederlands.ly"

\include "drumpitch-init.ly"		
\include "chord-modifiers-init.ly"
\include "script-init.ly"

% declarations for standard directions
left = -1
right = 1
up = 1
down = -1
start = -1
stop = 1
smaller = -1
bigger = 1
center = 0

%% FIXME
%% should also set allowBeamBreak, but how to do it "portably"? (ie. also
%% working with lyric sections)
%%
%% try \once \set Score.allowBeamBreak = ##t

%% rather name \newline, \pageBreak ?
break = #(make-event-chord (list (make-penalty-music -10001 0)))
noBreak = #(make-event-chord (list (make-penalty-music 10001 0)))
pageBreak = #(make-event-chord (list (make-penalty-music -10001 -10001)))
noPageBreak = #(make-event-chord (list (make-penalty-music 0 10001)))
stopStaff = #(make-event-chord (list (make-span-event 'StaffSpanEvent STOP)))
startStaff = #(make-event-chord (list (make-span-event 'StaffSpanEvent START)))


%
% Code articulation definitions
%
noBeam = #(make-music 'BeamForbidEvent) 
pipeSymbol = #(make-music 'BarCheck)
bracketOpenSymbol = #(make-span-event 'BeamEvent START)
bracketCloseSymbol = #(make-span-event 'BeamEvent STOP)
tildeSymbol = #(make-music 'TieEvent)
parenthesisOpenSymbol =  #(make-span-event 'SlurEvent START)
parenthesisCloseSymbol = #(make-span-event 'SlurEvent STOP)
escapedExclamationSymbol = #(make-span-event 'CrescendoEvent STOP)
escapedParenthesisOpenSymbol = #(make-span-event 'PhrasingSlurEvent START)
escapedParenthesisCloseSymbol = #(make-span-event 'PhrasingSlurEvent STOP)
escapedBiggerSymbol = #(make-span-event 'DecrescendoEvent START)
escapedSmallerSymbol = #(make-span-event 'CrescendoEvent START)



foo = { \pageBreak }

\include "scale-definitions-init.ly"

melisma = #(make-span-event 'ManualMelismaEvent START)
melismaEnd = #(make-span-event 'ManualMelismaEvent STOP)

\include "grace-init.ly"
\include "midi-init.ly"
\include "paper-defaults.ly"

\layout {
    mm = #(ly:output-def-lookup $defaultpaper 'mm)
    unit = #(ly:output-def-lookup $defaultpaper 'unit)

    in = #(* 25.4 mm)
    pt = #(/  in 72.27)
    cm = #(* 10 mm)
    
    \include "engraver-init.ly"

    #(set-paper-dimension-variables (current-module))
    
}


#(set-default-paper-size "a4")


partCombineListener = \layout {
    \context {
	\Voice
	\consists Note_heads_engraver
	\consists Rest_engraver
	\type "Recording_group_engraver"
	recordEventSequence = #notice-the-events-for-pc
	recordProperties = #'(timeSignatureFraction measurePosition)
    }
    \context {
	\Score
	skipTypesetting = ##t
	ignoreBarChecks = ##t 
    }
}

#(set-part-combine-listener partCombineListener)

\include "dynamic-scripts-init.ly"
\include "spanners-init.ly"
\include "property-init.ly"

setDefaultDurationToQuarter = { c4 }

%% MAKE-HASH-TABLE in GUILE 1.6 takes mandatory size parameter.
#(define musicQuotes (make-hash-table 29))

#(define toplevel-book-handler ly:parser-print-book)
#(define toplevel-music-handler collect-music-for-book)
#(define toplevel-score-handler collect-scores-for-book)
#(define toplevel-text-handler collect-scores-for-book)

