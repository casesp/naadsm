/** @file unit-state-monitor.c
 * Tracks the disease state of units.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 *
 * Copyright &copy; University of Guelph, 2009-2010
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
#define is_singleton unit_state_monitor_is_singleton
#define new unit_state_monitor_new
#define run unit_state_monitor_run
#define reset unit_state_monitor_reset
#define events_listened_for unit_state_monitor_events_listened_for
#define is_listening_for unit_state_monitor_is_listening_for
#define has_pending_actions unit_state_monitor_has_pending_actions
#define to_string unit_state_monitor_to_string
#define local_printf unit_state_monitor_printf
#define local_fprintf unit_state_monitor_fprintf
#define local_free unit_state_monitor_free
#define handle_before_any_simulations_event unit_state_monitor_handle_before_any_simulations_event
#define handle_new_day_event unit_state_monitor_handle_new_day_event

#include "model.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif


#include "unit-state-monitor.h"

/** This must match an element name in the DTD. */
#define MODEL_NAME "unit-state-monitor"



#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_BeforeAnySimulations, EVT_NewDay };



/** Specialized information for this model. */
typedef struct
{
  RPT_reporting_t *num_units_in_state;
  RPT_reporting_t *num_units_in_state_by_prodtype;
  RPT_reporting_t *num_animals_in_state;
  RPT_reporting_t *num_animals_in_state_by_prodtype;
  RPT_reporting_t *disease_duration;
}
local_data_t;



/**
 * Before any simulations, this module announces the output variables it is
 * recording.
 *
 * @param self this module.
 * @param queue for any new events this function creates.
 */
void
handle_before_any_simulations_event (struct naadsm_model_t_ *self,
                                     EVT_event_queue_t *queue)
{
  unsigned int n, i;
  RPT_reporting_t *output;
  GPtrArray *outputs = NULL;

  n = self->outputs->len;
  for (i = 0; i < n; i++)
    {
      output = (RPT_reporting_t *) g_ptr_array_index (self->outputs, i);
      if (output->frequency != RPT_never)
        {
          if (outputs == NULL)
            outputs = g_ptr_array_new();
          g_ptr_array_add (outputs, output);
        }
    }

  if (outputs != NULL)
    EVT_event_enqueue (queue, EVT_new_declaration_of_outputs_event (outputs));
  /* We don't free the pointer array, that will be done when the event is freed
   * after all interested modules have processed it. */

  return;
}



/**
 * Responds to a new day event by updating the count of units in each disease
 * state.
 *
 * @param self the model.
 * @param herds a list of herds.
 */
void
handle_new_day_event (struct naadsm_model_t_ * self,
                      HRD_herd_list_t * herds,
                      EVT_new_day_event_t * event)
{
  local_data_t *local_data;
  unsigned int nherds, nanimals, i, j;
  HRD_herd_t *herd;
  const char *drill_down_list[3] = { NULL, NULL, NULL };
  gboolean disease_present;

#if DEBUG
  g_debug ("----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  RPT_reporting_zero (local_data->num_units_in_state);
  RPT_reporting_zero (local_data->num_animals_in_state);

  disease_present = FALSE;
  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);

      RPT_reporting_add_integer1 (local_data->num_units_in_state, 1,
                                  HRD_status_abbrev[herd->status]);

      drill_down_list[0] = herd->production_type_name;
      drill_down_list[1] = HRD_status_abbrev[herd->status];
      RPT_reporting_add_integer (local_data->num_units_in_state_by_prodtype,
                                 1, drill_down_list);
      for (j = 0; j < HRD_NSTATES; j++)
        {
          nanimals = HRD_num_animals_in_state (herd, j);
          RPT_reporting_add_integer1 (local_data->num_animals_in_state, nanimals,
                                      HRD_status_abbrev[j]);
          drill_down_list[1] = HRD_status_abbrev[j];
          RPT_reporting_add_integer (local_data->num_animals_in_state_by_prodtype,
                                     nanimals, drill_down_list);
        }

      disease_present = disease_present || HRD_is_infected(herd);
    }

  if (disease_present)
    RPT_reporting_set_null (local_data->disease_duration, NULL);
  else if (RPT_reporting_is_null (local_data->disease_duration, NULL))
    RPT_reporting_set_integer (local_data->disease_duration, event->day - 1, NULL);

#if DEBUG
  g_debug ("----- EXIT handle_new_day_event (%s)", MODEL_NAME);
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
    case EVT_BeforeAnySimulations:
      handle_before_any_simulations_event (self, queue);
      break;
    case EVT_NewDay:
      handle_new_day_event (self, herds, &(event->u.new_day));
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

  /* No need to reset the output variables: they are cleared to zero and
   * re-calculated on each simulation day. */

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
  g_string_sprintf (s, "<%s>", MODEL_NAME);

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
 * Frees this model.
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
  RPT_free_reporting (local_data->num_units_in_state);
  RPT_free_reporting (local_data->num_animals_in_state);
  RPT_free_reporting (local_data->num_units_in_state_by_prodtype);
  RPT_free_reporting (local_data->num_animals_in_state_by_prodtype);
  RPT_free_reporting (local_data->disease_duration);
  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_debug ("----- EXIT free (%s)", MODEL_NAME);
#endif
}



/**
 * Returns whether this module is a singleton or not.
 */
gboolean
is_singleton (void)
{
  return TRUE;
}



/**
 * Returns a new unit state monitor.
 */
naadsm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, projPJ projection,
     ZON_zone_list_t * zones)
{
  naadsm_model_t *self;
  local_data_t *local_data;
  scew_element *e, **ee;
  unsigned int noutputs;
  const XML_Char *variable_name;
  RPT_frequency_t freq;
  gboolean success;
  gboolean broken_down;
  unsigned short int i, j;      /* loop counters */
  const char *drill_down_list[3] = { NULL, NULL, NULL };

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
  self->run = run;
  self->reset = reset;
  self->is_listening_for = is_listening_for;
  self->has_pending_actions = has_pending_actions;
  self->to_string = to_string;
  self->printf = local_printf;
  self->fprintf = local_fprintf;
  self->free = local_free;

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  local_data->num_units_in_state = RPT_new_reporting ("tsdU", RPT_group, RPT_never);
  local_data->num_units_in_state_by_prodtype = RPT_new_reporting ("tsdU", RPT_group, RPT_never);
  local_data->num_animals_in_state = RPT_new_reporting ("tsdA", RPT_group, RPT_never);
  local_data->num_animals_in_state_by_prodtype = RPT_new_reporting ("tsdA", RPT_group, RPT_never);
  local_data->disease_duration = RPT_new_reporting ("diseaseDuration", RPT_integer, RPT_never);
  g_ptr_array_add (self->outputs, local_data->num_units_in_state);
  g_ptr_array_add (self->outputs, local_data->num_units_in_state_by_prodtype);
  g_ptr_array_add (self->outputs, local_data->num_animals_in_state);
  g_ptr_array_add (self->outputs, local_data->num_animals_in_state_by_prodtype);
  g_ptr_array_add (self->outputs, local_data->disease_duration);

  /* Set the reporting frequency for the output variables. */
  ee = scew_element_list (params, "output", &noutputs);
#if DEBUG
  g_debug ("%u output variables", noutputs);
#endif
  for (i = 0; i < noutputs; i++)
    {
      e = ee[i];
      variable_name = scew_element_contents (scew_element_by_name (e, "variable-name"));
      freq = RPT_string_to_frequency (scew_element_contents
                                      (scew_element_by_name (e, "frequency")));
      broken_down = PAR_get_boolean (scew_element_by_name (e, "broken-down"), &success);
      if (!success)
      	broken_down = FALSE;
      if (strcmp (variable_name, "tsdU") == 0)
        {
          RPT_reporting_set_frequency (local_data->num_units_in_state, freq);
          if (broken_down)
            RPT_reporting_set_frequency (local_data->num_units_in_state_by_prodtype, freq);
        }
      else if (strcmp (variable_name, "tsdA") == 0)
        {
          RPT_reporting_set_frequency (local_data->num_animals_in_state, freq);
          if (broken_down)
            RPT_reporting_set_frequency (local_data->num_animals_in_state_by_prodtype, freq);
        }
      else if (strcmp (variable_name, "diseaseDuration") == 0)
        RPT_reporting_set_frequency (local_data->disease_duration, freq);
      else
        g_warning ("no output variable named \"%s\", ignoring", variable_name);        
    }
  free (ee);

  /* Initialize the output variables, so they have the correct sub-categories. */
  for (i = 0; i < HRD_NSTATES; i++)
    {
      RPT_reporting_set_integer1 (local_data->num_units_in_state, 0, HRD_status_abbrev[i]);
      RPT_reporting_set_integer1 (local_data->num_animals_in_state, 0, HRD_status_abbrev[i]);
      drill_down_list[1] = HRD_status_abbrev[i];
      for (j = 0; j < herds->production_type_names->len; j++)
        {
          drill_down_list[0] = (char *) g_ptr_array_index (herds->production_type_names, j);
          RPT_reporting_set_integer (local_data->num_units_in_state_by_prodtype, 0, drill_down_list);
          RPT_reporting_set_integer (local_data->num_animals_in_state_by_prodtype, 0, drill_down_list);
        }
    }

#if DEBUG
  g_debug ("----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

/* end of file unit-state-monitor.c */
