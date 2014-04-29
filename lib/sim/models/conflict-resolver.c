/** @file conflict-resolver.c
 * A special module, always loaded, that encapsulates the list of units.  It
 * gathers requests for changes to units and disambiguates the results of
 * (potentially) conflicting requests.  It also sends out notifications when
 * units change state.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date January 2005
 *
 * Copyright &copy; University of Guelph, 2005-2009
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when multiple modules have the same interface. */
#define is_singleton conflict_resolver_is_singleton
#define new conflict_resolver_new
#define set_params conflict_resolver_set_params
#define run conflict_resolver_run
#define reset conflict_resolver_reset
#define events_listened_for conflict_resolver_events_listened_for
#define is_listening_for conflict_resolver_is_listening_for
#define has_pending_actions conflict_resolver_has_pending_actions
#define to_string conflict_resolver_to_string
#define local_printf conflict_resolver_printf
#define local_fprintf conflict_resolver_fprintf
#define local_free conflict_resolver_free
#define handle_before_each_simulation_event conflict_resolver_handle_before_each_simulation_event
#define handle_midnight_event conflict_resolver_handle_midnight_event

#include "model.h"
#include "model_util.h"
#include "general.h"
#include "conflict-resolver.h"

#define MODEL_NAME "conflict-resolver"



#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_BeforeEachSimulation, EVT_Midnight };



/* Specialized information for this model. */
typedef struct
{
  int dummy;
}
local_data_t;



/**
 * Before each simulation, this module sets up the units' initial states.
 *
 * @param self this module.
 * @param herds the list of units.
 * @param queue for any new events this module generates.
 */
void
handle_before_each_simulation_event (struct naadsm_model_t_ * self,
                                     HRD_herd_list_t * herds,
                                     EVT_event_queue_t * queue)
{
  unsigned int nherds, i;
  HRD_herd_t *herd;
  HSC_scorecard_t *scorecard;
  EVT_event_t *event;

#if DEBUG
  g_debug ("----- ENTER handle_before_each_simulation_event (%s)", MODEL_NAME);
#endif

  /* Set each unit's initial state.  We don't need to go through the usual
   * conflict resolution steps here. */
  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      HRD_reset (herd);
      switch (herd->initial_status)
        {
        case Susceptible:
          break;
        case Latent:
        case InfectiousSubclinical:
        case InfectiousClinical:
        case NaturallyImmune:
        case DeadFromDisease:
          event = EVT_new_exposure_event (NULL, herd, 0, "Ini",
                                          /* traceable = */ FALSE,
                                          /* adequate = */ TRUE);
          event->u.exposure.contact_type = NAADSM_InitiallyInfected;
          event->u.exposure.override_initial_state = herd->initial_status;
          event->u.exposure.override_days_in_state = herd->days_in_initial_status;
          event->u.exposure.override_days_left_in_state = herd->days_left_in_initial_status;
          EVT_event_enqueue (queue, event);
          break;
        case VaccineImmune:
          event = EVT_new_inprogress_immunity_event (herd, 0, "Ini",
                                                     herd->initial_status,
                                                     herd->days_in_initial_status,
                                                     herd->days_left_in_initial_status);
          EVT_event_enqueue (queue, event);
          break;
        case Destroyed:
          HRD_destroy (herd);
          event = EVT_new_destruction_event (herd, 0, "Ini", -1);
          EVT_event_enqueue (queue, event);
          break;
        default:
          g_assert_not_reached ();
        }
      /* Clear the herd's scorecard, if it has one. */
      scorecard = naadsm_get_scorecard (herd);
      if (scorecard != NULL)
        {
          HSC_scorecard_reset (scorecard);
        }
    } /* end of loop over units */

#if DEBUG
  g_debug ("----- EXIT handle_before_each_simulation_event (%s)", MODEL_NAME);
#endif

  return;
} 



/**
 * Responds to a "midnight" event by making the herds change state.
 *
 * @param self this module.
 * @param event the "midnight" event.
 * @param herds the list of herds.
 * @param rng a random number generator.
 * @param queue for any new events this module creates.
 */
void
handle_midnight_event (struct naadsm_model_t_ *self,
                       EVT_midnight_event_t * event,
                       HRD_herd_list_t * herds,
                       RAN_gen_t * rng,
                       EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  unsigned int nherds, i;
  HRD_herd_t *herd;
  HRD_status_t old_state, new_state;
  gboolean was_infected;

#if DEBUG
  g_debug ("----- ENTER handle_midnight_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      old_state = herd->status;
      was_infected = HRD_is_infected (herd);
      /* _iteration is a global variable defined in general.c */
      new_state = HRD_step (herd, rng, _iteration.infectious_herds);
      if (old_state != new_state)
        {
          EVT_event_enqueue (queue,
                             EVT_new_unit_state_change_event (herd,
                                                              old_state,
                                                              new_state,
                                                              event->day));
          if (!was_infected && HRD_is_infected(herd))
            EVT_event_enqueue (queue, EVT_new_infection_event (herd, event->day));
        }
    }

#if DEBUG
  g_debug ("----- EXIT handle_midnight_event (%s)", MODEL_NAME);
#endif
}



/**
 * Runs this model.
 *
 * @param self the model.
 * @param herds a herd list.
 * @param zones a zone list.
 * @param event the event that caused the model to run.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
run (struct naadsm_model_t_ *self, HRD_herd_list_t * herds, ZON_zone_list_t * zones,
     EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
#if DEBUG
  g_debug ("----- ENTER run (%s)", MODEL_NAME);
#endif

  switch (event->type)
    {
    case EVT_BeforeEachSimulation:
      handle_before_each_simulation_event (self, herds, queue);
      break;
    case EVT_Midnight:
      handle_midnight_event (self, &(event->u.midnight), herds, rng, queue);
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_debug ("----- EXIT run (%s)", MODEL_NAME);
#endif
}



/**
 * Resets this model after a simulation run.
 *
 * @param self the model.
 */
void
reset (struct naadsm_model_t_ *self)
{
#if DEBUG
  g_debug ("----- ENTER reset (%s)", MODEL_NAME);
#endif

  /* Nothing to do. */

#if DEBUG
  g_debug ("----- EXIT reset (%s)", MODEL_NAME);
#endif
}



/**
 * Reports whether this model is listening for a given event type.
 *
 * @param self the model.
 * @param event_type an event type.
 * @return TRUE if the model is listening for the event type.
 */
gboolean
is_listening_for (struct naadsm_model_t_ *self, EVT_event_type_t event_type)
{
  int i;

  for (i = 0; i < self->nevents_listened_for; i++)
    if (self->events_listened_for[i] == event_type)
      return TRUE;
  return FALSE;
}



/**
 * Reports whether this model has any pending actions to carry out.
 *
 * @param self the model.
 * @return TRUE if the model has pending actions.
 */
gboolean
has_pending_actions (struct naadsm_model_t_ * self)
{
  return FALSE;
}



/**
 * Returns a text representation of this model.
 *
 * @param self the model.
 * @return a string.
 */
char *
to_string (struct naadsm_model_t_ *self)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_printf (s, "<%s>", MODEL_NAME);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Prints this model to a stream.
 *
 * @param stream a stream to write to.
 * @param self the model.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_fprintf (FILE * stream, struct naadsm_model_t_ *self)
{
  char *s;
  int nchars_written;

  s = to_string (self);
  nchars_written = fprintf (stream, "%s", s);
  free (s);
  return nchars_written;
}



/**
 * Prints this model.
 *
 * @param self the model.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_printf (struct naadsm_model_t_ *self)
{
  return local_fprintf (stdout, self);
}



/**
 * Frees this model.  Does not free the production type name.
 *
 * @param self the model.
 */
void
local_free (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER free (%s)", MODEL_NAME);
#endif

  /* Free the dynamically-allocated parts. */
  local_data = (local_data_t *) (self->model_data);
  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_debug ("----- EXIT free (%s)", MODEL_NAME);
#endif
}



/**
 * Returns whether this model is a singleton or not.
 */
gboolean
is_singleton (void)
{
  return TRUE;
}



/**
 * Adds a set of parameters to a conflict resolver model.
 */
void
set_params (struct naadsm_model_t_ *self, PAR_parameter_t * params)
{
#if DEBUG
  g_debug ("----- ENTER set_params (%s)", MODEL_NAME);
#endif

  /* Nothing to do. */

#if DEBUG
  g_debug ("----- EXIT set_params (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Returns a new conflict resolver model.
 */
naadsm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, projPJ projection,
     ZON_zone_list_t * zones)
{
  naadsm_model_t *self;
  local_data_t *local_data;

#if DEBUG
  g_debug ("----- ENTER new (%s)", MODEL_NAME);
#endif

  self = g_new (naadsm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  self->name = MODEL_NAME;
  self->events_listened_for = events_listened_for;
  self->nevents_listened_for = NEVENTS_LISTENED_FOR;
  self->outputs = g_ptr_array_new ();
  self->model_data = local_data;
  self->set_params = set_params;
  self->run = run;
  self->reset = reset;
  self->is_listening_for = is_listening_for;
  self->has_pending_actions = has_pending_actions;
  self->to_string = to_string;
  self->printf = local_printf;
  self->fprintf = local_fprintf;
  self->free = local_free;

#if DEBUG
  g_debug ("----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

/* end of file conflict-resolver.c */
