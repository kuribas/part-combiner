/*
  recording-group-engraver.cc -- implement Recording_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "engraver-group-engraver.hh"
#include "protected-scm.hh"

class Recording_group_engraver : public Engraver_group_engraver
{
public:
  TRANSLATOR_DECLARATIONS (Recording_group_engraver);
  virtual bool try_music (Music *m);
  void add_music (SCM, SCM);
  void detect_property_differences();
  void create_property_lists (SCM);
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  virtual void initialize ();
  virtual void derived_mark () const;
  SCM now_events_;
  SCM accumulator_;
  SCM property_values_;  // a list with the current values for the properties
  SCM property_alist_;    // an alist containing the properties and their values
};

void
Recording_group_engraver::derived_mark () const
{
  Engraver_group_engraver::derived_mark ();
  scm_gc_mark (now_events_);
  scm_gc_mark (accumulator_);
  scm_gc_mark (property_values_);
  scm_gc_mark (property_alist_);
}

void
Recording_group_engraver::initialize ()
{
  Engraver_group_engraver::initialize ();
}

Recording_group_engraver::Recording_group_engraver ()
{
  accumulator_ = SCM_EOL;
  now_events_ = SCM_EOL;
  property_alist_ = SCM_EOL;
  property_values_ = SCM_BOOL_F;
}

void
Recording_group_engraver::add_music (SCM music, SCM success)
{
  now_events_ = scm_cons (scm_cons (music, success), now_events_);
}

void
Recording_group_engraver::create_property_lists (SCM properties)
{
  property_values_ = SCM_EOL;
  
  for(; scm_is_pair (properties); properties = scm_cdr(properties))
    {
      SCM prop = scm_car (properties);
      SCM value = internal_get_property(prop);

      property_alist_ = scm_acons (prop, value, property_alist_);
      property_values_ = scm_cons (value, property_values_);
    }

  //property_values_ must be in the same order as the properties
  property_values_ = scm_reverse (property_values_);
}

void
Recording_group_engraver::detect_property_differences()
{
  SCM property_requests = get_property("recordProperties");

  // add changed properties to property_alist_

  property_alist_ = SCM_EOL;
  if( scm_is_pair (property_requests) )
    {
      if (SCM_BOOL_F == property_values_)
	create_property_lists (property_requests);
      else
	{
	  for(SCM value_list = property_values_;
	      scm_is_pair (property_requests);
	      property_requests = scm_cdr (property_requests),
		value_list = scm_cdr (value_list))
	    {
	      SCM prop = scm_car (property_requests);
	      SCM new_value = internal_get_property (prop);
	      if (new_value != scm_car (value_list))
		{
		  scm_set_car_x (value_list, new_value);
		  property_alist_ = scm_acons (prop, new_value, property_alist_);
		}
	    }
	}
    }
}

void
Recording_group_engraver::stop_translation_timestep ()
{
  Engraver_group_engraver::stop_translation_timestep ();

  detect_property_differences();
  accumulator_ = scm_acons (scm_cons (now_mom ().smobbed_copy (),
				      property_alist_),
			    now_events_,
			    accumulator_);

  now_events_ = SCM_EOL;
  property_alist_ = SCM_EOL;
}

void
Recording_group_engraver::finalize ()
{
  Engraver_group_engraver::finalize ();
  SCM proc = get_property ("recordEventSequence");

  if (ly_c_procedure_p (proc))
    scm_call_2 (proc, context ()->self_scm (), scm_cdr (accumulator_));

  property_values_ = SCM_BOOL_F;
  property_alist_ = SCM_EOL;
}

bool
Recording_group_engraver::try_music (Music *m)
{
  bool retval = Translator_group::try_music (m);

  add_music (m->self_scm (), ly_bool2scm (retval));
  return retval;
}

ADD_TRANSLATOR (Recording_group_engraver,
		"Engraver_group_engraver that records all music events "
		"for this context. Calls the procedure "
		"in @code{recordEventSequence} when finished.",
		"",
		"",
		"",
		"recordEventSequence",
		"");
