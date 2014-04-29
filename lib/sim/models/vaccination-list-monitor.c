/** @file vaccination-list-monitor.c
 * Tracks the number of units waiting to be vaccinated.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date April 2004
 *
 * Copyright &copy; University of Guelph, 2004-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when dlpreopening multiple modules that have the same
 * global symbols (interface).  See sec. 18.4 of "GNU Autoconf, Automake, and
 * Libtool". */
#define interface_version vaccination_list_monitor_LTX_interface_version
#define new vaccination_list_monitor_LTX_new
#define run vaccination_list_monitor_LTX_run
#define reset vaccination_list_monitor_LTX_reset
#define events_listened_for vaccination_list_monitor_LTX_events_listened_for
#define is_listening_for vaccination_list_monitor_LTX_is_listening_for
#define has_pending_actions vaccination_list_monitor_LTX_has_pending_actions
#define has_pending_infections vaccination_list_monitor_LTX_has_pending_infections
#define to_string vaccination_list_monitor_LTX_to_string
#define local_printf vaccination_list_monitor_LTX_printf
#define local_fprintf vaccination_list_monitor_LTX_fprintf
#define local_free vaccination_list_monitor_LTX_free
#define handle_new_day_event vaccination_list_monitor_LTX_handle_new_day_event
#define handle_commitment_to_vaccinate_event vaccination_list_monitor_LTX_handle_commitment_to_vaccinate_event
#define handle_vaccination_event vaccination_list_monitor_LTX_handle_vaccination_event
#define handle_destruction_event vaccination_list_monitor_LTX_handle_destruction_event
#define events_created vaccination_list_monitor_LTX_events_created

#include "model.h"

#if STDC_HEADERS
#  include <string.h>
#endif

/** This must match an element name in the DTD. */
#define MODEL_NAME "vaccination-list-monitor"

#define MODEL_DESCRIPTION "\
A module to track units waiting to be vaccinated.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 April 2004\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 0
EVT_event_type_t events_created[] = { 0 };

#define NEVENTS_LISTENED_FOR 4
EVT_event_type_t events_listened_for[] =
  { EVT_NewDay, EVT_CommitmentToVaccinate, EVT_Vaccination, EVT_Destruction };



extern const char *RPT_frequency_name[];



#define NOT_ON_WAITING_LIST 0
#define ON_WAITING_LIST 1
#define VACCINATED 2



/** Specialized information for this model. */
typedef struct
{
  GPtrArray *production_types;
  unsigned int nherds;          /* Number of herds. */
  int *status; /**< The status of each unit with respect to vaccination.  The
    value can be the special code NOT_ON_WAITING_LIST (0) or the day on which a
    commitment to vaccinate the herd was made. */
  unsigned int peak_nherds, peak_nanimals;
  unsigned int peak_wait;
  double sum_for_average; /**< The numerator for calculating the average wait
    time.  This is the fixed part, for units already vaccinated. */
  unsigned int count_for_average; /**< The denominator for calculating the
    average wait time.  This is the fixed part, for units already vaccinated. */
  RPT_reporting_t *nherds_awaiting_vaccination;
  RPT_reporting_t *nherds_awaiting_vaccination_by_prodtype;
  RPT_reporting_t *nanimals_awaiting_vaccination;
  RPT_reporting_t *nanimals_awaiting_vaccination_by_prodtype;
  RPT_reporting_t *peak_nherds_awaiting_vaccination;
  RPT_reporting_t *peak_nanimals_awaiting_vaccination;
  RPT_reporting_t *peak_wait_time;
  RPT_reporting_t *average_wait_time;
}
local_data_t;



/**
 * Responds to a new day event by updating the peak and average wait times.
 *
 * @param self the model.
 * @param event a new day event.
 */
void
handle_new_day_event (struct ergadm_model_t_ *self, EVT_new_day_event_t * event)
{
  local_data_t *local_data;
  unsigned int i;
  int status;
  unsigned int wait;
  double todays_sum;
  unsigned int todays_count;
  unsigned int denom;
  double average;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Don't do any work if we don't have to. */
  if (local_data->peak_wait_time->frequency == RPT_never
      && local_data->average_wait_time->frequency == RPT_never)
    goto end;

  /* The average is calculated using a fixed part (units already vaccinated)
   * and a part that varies daily (units waiting to be vaccinated).  Initialize
   * the daily sum & count to 0. */
  todays_sum = todays_count = 0;

  for (i = 0; i < local_data->nherds; i++)
    {
      status = local_data->status[i];
      if (status != NOT_ON_WAITING_LIST)
        {
          /* The herd is on a waiting list.  The day when it went onto the
           * waiting list is recorded in the "status" array. */
          wait = event->day - status;

          /* Update the peak wait time. */
          if (wait > local_data->peak_wait)
            local_data->peak_wait = wait;

          /* Record values for the average wait time calculation. */
          todays_sum += wait;
          todays_count += 1;
        }
    }
  RPT_reporting_set_integer (local_data->peak_wait_time, local_data->peak_wait, NULL);

  denom = local_data->count_for_average + todays_count;
  if (denom == 0)
    average = 0;
  else
    average = (local_data->sum_for_average + todays_sum) / denom;
  RPT_reporting_set_real (local_data->average_wait_time, average, NULL);

  /* This calculation is done at the start of a new day.  If, later in the day,
   * units are vaccinated or destroyed, the fixed parts of the average wait
   * time calculation will be updated, but it won't change today's value for
   * the average vaccination wait time output. */

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif

  return;
}



/**
 * Responds to a commitment to vaccinate event by recording the herd's status
 * as "waiting".
 *
 * @param self the model.
 * @param event a commitment to vaccinate event.
 */
void
handle_commitment_to_vaccinate_event (struct ergadm_model_t_ *self,
                                      EVT_commitment_to_vaccinate_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  unsigned int nherds, nanimals;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_commitment_to_vaccinate_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  if (local_data->status[herd->index] == NOT_ON_WAITING_LIST)
    {
      local_data->status[herd->index] = event->day;

      /* Increment the count of herds awaiting vaccination. */
      RPT_reporting_add_integer (local_data->nherds_awaiting_vaccination, 1, NULL);
      if (local_data->nherds_awaiting_vaccination_by_prodtype->frequency != RPT_never)
        RPT_reporting_add_integer1 (local_data->nherds_awaiting_vaccination_by_prodtype, 1,
                                    herd->production_type_name);
      nherds = RPT_reporting_get_integer (local_data->nherds_awaiting_vaccination, NULL);
      if (nherds > local_data->peak_nherds)
        {
          local_data->peak_nherds = nherds;
          RPT_reporting_set_integer (local_data->peak_nherds_awaiting_vaccination, nherds, NULL);
        }

      /* Increment the count of animals awaiting vaccination. */
      RPT_reporting_add_integer (local_data->nanimals_awaiting_vaccination, herd->size, NULL);
      if (local_data->nanimals_awaiting_vaccination_by_prodtype->frequency != RPT_never)
        RPT_reporting_add_integer1 (local_data->nanimals_awaiting_vaccination_by_prodtype,
                                    herd->size, herd->production_type_name);
      nanimals = RPT_reporting_get_integer (local_data->nanimals_awaiting_vaccination, NULL);
      if (nanimals > local_data->peak_nanimals)
        {
          local_data->peak_nanimals = nanimals;
          RPT_reporting_set_integer (local_data->peak_nanimals_awaiting_vaccination, nanimals,
                                     NULL);
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_commitment_to_vaccinate_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a vaccination event by removing the herd's "waiting" status.
 *
 * @param self the model.
 * @param event a vaccination event.
 */
void
handle_vaccination_event (struct ergadm_model_t_ *self, EVT_vaccination_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  int status;
  unsigned int wait;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_vaccination_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  status = local_data->status[herd->index];
  if (status != NOT_ON_WAITING_LIST)
    {
      /* The herd is on a waiting list.  The day when it went onto the waiting
       * list is recorded in the "status" array. */
      wait = event->day - status;

      /* Update the fixed parts of the average wait time calculation.  Note
       * that this doesn't change the average wait time we calculated in the
       * handle_new_day_event function. */
      local_data->sum_for_average += wait;
      local_data->count_for_average += 1;

      /* Mark the herd as no longer on a waiting list. */
      local_data->status[herd->index] = NOT_ON_WAITING_LIST;

      /* Decrement the counts of herds and animals awaiting vaccination. */
      RPT_reporting_sub_integer (local_data->nherds_awaiting_vaccination, 1, NULL);
      if (local_data->nherds_awaiting_vaccination_by_prodtype->frequency != RPT_never)
        RPT_reporting_sub_integer1 (local_data->nherds_awaiting_vaccination_by_prodtype, 1,
                                    herd->production_type_name);

      RPT_reporting_sub_integer (local_data->nanimals_awaiting_vaccination, herd->size, NULL);
      if (local_data->nanimals_awaiting_vaccination_by_prodtype->frequency != RPT_never)
        RPT_reporting_sub_integer1 (local_data->nanimals_awaiting_vaccination_by_prodtype,
                                    herd->size, herd->production_type_name);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_vaccination_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a destruction event by removing the herd's "waiting" status, if
 * it is waiting for vaccination.
 *
 * @param self the model.
 * @param event a destruction event.
 */
void
handle_destruction_event (struct ergadm_model_t_ *self, EVT_destruction_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  int status;
  unsigned int wait;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_destruction_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  status = local_data->status[herd->index];
  if (status != NOT_ON_WAITING_LIST)
    {
      /* The herd is on a waiting list.  The day when it went onto the waiting
       * list is recorded in the "status" array. */
      wait = event->day - status;

      /* Update the fixed parts of the average wait time calculation.  Note
       * that this doesn't change the average wait time we calculated in the
       * handle_new_day_event function. */
      local_data->sum_for_average += wait;
      local_data->count_for_average += 1;

      /* Mark the herd as no longer on a waiting list. */
      local_data->status[herd->index] = NOT_ON_WAITING_LIST;

      /* Decrement the counts of herds and animals awaiting vaccination. */
      RPT_reporting_sub_integer (local_data->nherds_awaiting_vaccination, 1, NULL);
      if (local_data->nherds_awaiting_vaccination_by_prodtype->frequency != RPT_never)
        RPT_reporting_sub_integer1 (local_data->nherds_awaiting_vaccination_by_prodtype, 1,
                                    herd->production_type_name);

      RPT_reporting_sub_integer (local_data->nanimals_awaiting_vaccination, herd->size, NULL);
      if (local_data->nanimals_awaiting_vaccination_by_prodtype->frequency != RPT_never)
        RPT_reporting_sub_integer1 (local_data->nanimals_awaiting_vaccination_by_prodtype,
                                    herd->size, herd->production_type_name);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_destruction_event (%s)", MODEL_NAME);
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
run (struct ergadm_model_t_ *self, HRD_herd_list_t * herds, ZON_zone_list_t * zones,
     EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER run (%s)", MODEL_NAME);
#endif

  switch (event->type)
    {
    case EVT_NewDay:
      handle_new_day_event (self, &(event->u.new_day));
      break;
    case EVT_CommitmentToVaccinate:
      handle_commitment_to_vaccinate_event (self, &(event->u.commitment_to_vaccinate));
      break;
    case EVT_Vaccination:
      handle_vaccination_event (self, &(event->u.vaccination));
      break;
    case EVT_Destruction:
      handle_destruction_event (self, &(event->u.destruction));
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT run (%s)", MODEL_NAME);
#endif
}



/**
 * Resets this model after a simulation run.
 *
 * @param self the model.
 */
void
reset (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;
  int i;

  local_data = (local_data_t *) (self->model_data);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  for (i = 0; i < local_data->nherds; i++)
    local_data->status[i] = NOT_ON_WAITING_LIST;

  RPT_reporting_zero (local_data->nherds_awaiting_vaccination);
  RPT_reporting_zero (local_data->nherds_awaiting_vaccination_by_prodtype);
  RPT_reporting_zero (local_data->nanimals_awaiting_vaccination);
  RPT_reporting_zero (local_data->nherds_awaiting_vaccination_by_prodtype);
  RPT_reporting_zero (local_data->peak_nherds_awaiting_vaccination);
  RPT_reporting_zero (local_data->peak_nanimals_awaiting_vaccination);
  RPT_reporting_zero (local_data->peak_wait_time);
  RPT_reporting_zero (local_data->average_wait_time);

  local_data->peak_nherds = local_data->peak_nanimals = 0;
  local_data->peak_wait = 0;
  local_data->sum_for_average = local_data->count_for_average = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT reset (%s)", MODEL_NAME);
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
is_listening_for (struct ergadm_model_t_ *self, EVT_event_type_t event_type)
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
has_pending_actions (struct ergadm_model_t_ * self)
{
  return FALSE;
}



/**
 * Reports whether this model has any pending infections to cause.
 *
 * @param self the model.
 * @return TRUE if the model has pending infections.
 */
gboolean
has_pending_infections (struct ergadm_model_t_ * self)
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
to_string (struct ergadm_model_t_ *self)
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
local_fprintf (FILE * stream, struct ergadm_model_t_ *self)
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
local_printf (struct ergadm_model_t_ *self)
{
  return local_fprintf (stdout, self);
}



/**
 * Frees this model.
 *
 * @param self the model.
 */
void
local_free (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  /* Free the dynamically-allocated parts. */
  local_data = (local_data_t *) (self->model_data);
  g_free (local_data->status);

  RPT_free_reporting (local_data->nherds_awaiting_vaccination, TRUE);
  RPT_free_reporting (local_data->nherds_awaiting_vaccination_by_prodtype, TRUE);
  RPT_free_reporting (local_data->nanimals_awaiting_vaccination, TRUE);
  RPT_free_reporting (local_data->nanimals_awaiting_vaccination_by_prodtype, TRUE);
  RPT_free_reporting (local_data->peak_nherds_awaiting_vaccination, TRUE);
  RPT_free_reporting (local_data->peak_nanimals_awaiting_vaccination, TRUE);
  RPT_free_reporting (local_data->peak_wait_time, TRUE);
  RPT_free_reporting (local_data->average_wait_time, TRUE);

  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT free (%s)", MODEL_NAME);
#endif
}



/**
 * Returns the version of the interface this model conforms to.
 */
char *
interface_version (void)
{
  return MODEL_INTERFACE_VERSION;
}



/**
 * Returns a new vaccination list monitor.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *m;
  local_data_t *local_data;
  scew_element *e, **ee;
  unsigned int noutputs;
  RPT_reporting_t *output;
  RPT_reporting_t *old_peak_wait_time, *old_average_wait_time;
  const XML_Char *variable_name;
  unsigned short int i, j;      /* loop counters */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER new (%s)", MODEL_NAME);
#endif

  m = g_new (ergadm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  m->name = MODEL_NAME;
  m->description = MODEL_DESCRIPTION;
  m->events_created = events_created;
  m->nevents_created = NEVENTS_CREATED;
  m->events_listened_for = events_listened_for;
  m->nevents_listened_for = NEVENTS_LISTENED_FOR;
  m->outputs = g_ptr_array_sized_new (10);
  m->model_data = local_data;
  m->run = run;
  m->reset = reset;
  m->is_listening_for = is_listening_for;
  m->has_pending_actions = has_pending_actions;
  m->has_pending_infections = has_pending_infections;
  m->to_string = to_string;
  m->printf = local_printf;
  m->fprintf = local_fprintf;
  m->free = local_free;

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  local_data->nherds_awaiting_vaccination =
    RPT_new_reporting ("num-units-awaiting-vaccination", NULL, RPT_integer, RPT_never, TRUE);
  local_data->nherds_awaiting_vaccination_by_prodtype =
    RPT_new_reporting ("num-units-awaiting-vaccination-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->nanimals_awaiting_vaccination =
    RPT_new_reporting ("num-animals-awaiting-vaccination", NULL, RPT_integer, RPT_never, TRUE);
  local_data->nanimals_awaiting_vaccination_by_prodtype =
    RPT_new_reporting ("num-animals-awaiting-vaccination-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->peak_nherds_awaiting_vaccination =
    RPT_new_reporting ("peak-num-units-awaiting-vaccination", NULL, RPT_integer, RPT_never, TRUE);
  local_data->peak_nanimals_awaiting_vaccination =
    RPT_new_reporting ("peak-num-animals-awaiting-vaccination", NULL, RPT_integer, RPT_never, TRUE);
  /* The name of these variables changed from "peak-wait-time" and "average-
   * wait-time" to "peak-vaccination-wait-time" and "average-vaccination-wait-
   * time", so that they're distinguishable from the similarly-named outputs in
   * the destruction list monitor.  Check for the old names, though, so that
   * old parameter files will still work. */
  local_data->peak_wait_time =
    RPT_new_reporting ("peak-vaccination-wait-time", NULL, RPT_integer, RPT_never, TRUE);
  local_data->average_wait_time =
    RPT_new_reporting ("average-vaccination-wait-time", NULL, RPT_real, RPT_never, TRUE);
  old_peak_wait_time = RPT_new_reporting ("peak-wait-time", NULL, RPT_integer, RPT_never, TRUE);
  old_average_wait_time = RPT_new_reporting ("average-wait-time", NULL, RPT_real, RPT_never, TRUE);
  g_ptr_array_add (m->outputs, old_peak_wait_time);
  g_ptr_array_add (m->outputs, old_average_wait_time);
  g_ptr_array_add (m->outputs, local_data->nherds_awaiting_vaccination);
  g_ptr_array_add (m->outputs, local_data->nherds_awaiting_vaccination_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->nanimals_awaiting_vaccination);
  g_ptr_array_add (m->outputs, local_data->nanimals_awaiting_vaccination_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->peak_nherds_awaiting_vaccination);
  g_ptr_array_add (m->outputs, local_data->peak_nanimals_awaiting_vaccination);
  g_ptr_array_add (m->outputs, local_data->peak_wait_time);
  g_ptr_array_add (m->outputs, local_data->average_wait_time);

  /* Set the reporting frequency for the output variables. */
  ee = scew_element_list (params, "output", &noutputs);
#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i output variables", noutputs);
#endif
  for (i = 0; i < noutputs; i++)
    {
      e = ee[i];
      variable_name = scew_element_contents (scew_element_by_name (e, "variable-name"));
      /* Do the outputs include a variable with this name? */
      for (j = 0; j < m->outputs->len; j++)
        {
          output = (RPT_reporting_t *) g_ptr_array_index (m->outputs, j);
          if (strcmp (output->name, variable_name) == 0)
            break;
        }
      if (j == m->outputs->len)
        g_warning ("no output variable named \"%s\", ignoring", variable_name);
      else
        {
          RPT_reporting_set_frequency (output,
                                       RPT_string_to_frequency (scew_element_contents
                                                                (scew_element_by_name
                                                                 (e, "frequency"))));
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "report \"%s\" %s", variable_name,
                 RPT_frequency_name[output->frequency]);
#endif
        }
    }
  free (ee);
  if (old_peak_wait_time->frequency != RPT_never)
    RPT_reporting_set_frequency (local_data->peak_wait_time, old_peak_wait_time->frequency);
  if (old_average_wait_time->frequency != RPT_never)
    RPT_reporting_set_frequency (local_data->average_wait_time, old_average_wait_time->frequency);
  g_ptr_array_remove (m->outputs, old_peak_wait_time);
  g_ptr_array_remove (m->outputs, old_average_wait_time);
  RPT_free_reporting (old_peak_wait_time, TRUE);
  RPT_free_reporting (old_average_wait_time, TRUE);

  local_data->nherds = HRD_herd_list_length (herds);
  local_data->production_types = herds->production_type_names;
  for (i = 0; i < local_data->production_types->len; i++)
    {
      RPT_reporting_set_integer1 (local_data->nherds_awaiting_vaccination_by_prodtype, 0,
                                  (char *) g_ptr_array_index (local_data->production_types, i));
      RPT_reporting_set_integer1 (local_data->nanimals_awaiting_vaccination_by_prodtype, 0,
                                  (char *) g_ptr_array_index (local_data->production_types, i));
    }

  /* No herds have been vaccinated or slated for vaccination yet. */
  local_data->status = g_new0 (int, local_data->nherds);
  local_data->peak_nherds = local_data->peak_nanimals = 0;
  local_data->peak_wait = 0;
  local_data->sum_for_average = local_data->count_for_average = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

/* end of file vaccination-list-monitor.c */
