/** @file trace-back-monitor.c
 * Tracks the number of attempted and successful trace backs.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date October 2005
 *
 * Copyright &copy; University of Guelph, 2005-2006
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
#define interface_version trace_back_monitor_LTX_interface_version
#define new trace_back_monitor_LTX_new
#define run trace_back_monitor_LTX_run
#define reset trace_back_monitor_LTX_reset
#define events_listened_for trace_back_monitor_LTX_events_listened_for
#define is_listening_for trace_back_monitor_LTX_is_listening_for
#define has_pending_actions trace_back_monitor_LTX_has_pending_actions
#define has_pending_infections trace_back_monitor_LTX_has_pending_infections
#define to_string trace_back_monitor_LTX_to_string
#define local_printf trace_back_monitor_LTX_printf
#define local_fprintf trace_back_monitor_LTX_fprintf
#define local_free trace_back_monitor_LTX_free
#define handle_attempt_to_trace_event trace_back_monitor_LTX_attempt_to_trace_event
#define handle_trace_result_event trace_back_monitor_LTX_handle_trace_result_event
#define events_created trace_back_monitor_LTX_events_created

#include "model.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#include "trace-back-monitor.h"

#include "guilib.h"

/** This must match an element name in the DTD. */
#define MODEL_NAME "trace-back-monitor"

#define MODEL_DESCRIPTION "\
A module to track the number of attempted and successful trace backs.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 October 2005\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 0
EVT_event_type_t events_created[] = { 0 };

#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_AttemptToTrace, EVT_TraceResult };



extern const char *EVT_contact_type_name[];
extern const char *RPT_frequency_name[];



/** Specialized information for this model. */
typedef struct
{
  GPtrArray *production_types;
  RPT_reporting_t *ntraces_attempted;
  RPT_reporting_t *ntraces_attempted_by_prodtype;
  RPT_reporting_t *cumul_ntraces_attempted;
  RPT_reporting_t *cumul_ntraces_attempted_by_prodtype;
  RPT_reporting_t *ncontacts_potentially_traced;
  RPT_reporting_t *ncontacts_potentially_traced_by_prodtype;
  RPT_reporting_t *cumul_ncontacts_potentially_traced;
  RPT_reporting_t *cumul_ncontacts_potentially_traced_by_prodtype;
  RPT_reporting_t *ncontacts_traced;
  RPT_reporting_t *ncontacts_traced_by_prodtype;
  RPT_reporting_t *cumul_ncontacts_traced;
  RPT_reporting_t *cumul_ncontacts_traced_by_prodtype;
}
local_data_t;



/**
 * Responds to an attempt to trace event by recording it.
 *
 * @param self the model.
 * @param event an attempt to trace event.
 */
void
handle_attempt_to_trace_event (struct ergadm_model_t_ *self, EVT_attempt_to_trace_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_attempt_to_trace_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  RPT_reporting_add_integer (local_data->ntraces_attempted, 1, NULL);
  RPT_reporting_add_integer1 (local_data->ntraces_attempted_by_prodtype, 1,
                              herd->production_type_name);

  RPT_reporting_add_integer (local_data->cumul_ntraces_attempted, 1, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_ntraces_attempted_by_prodtype, 1,
                              herd->production_type_name);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_attempt_to_trace_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a trace result event by recording it.
 *
 * @param self the model.
 * @param event a trace result event.
 */
void
handle_trace_result_event (struct ergadm_model_t_ *self, EVT_trace_result_event_t * event)
{
  local_data_t *local_data;
  char *contact_type_name;
  char *drill_down_list[3] = { NULL, NULL, NULL };

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_trace_result_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  contact_type_name = EVT_contact_type_name[event->contact_type];
  drill_down_list[0] = contact_type_name;
  drill_down_list[1] = event->exposed_herd->production_type_name;

  /* Record a potentially traced contact. */
  RPT_reporting_add_integer1 (local_data->ncontacts_potentially_traced, 1, contact_type_name);
  if (local_data->ncontacts_potentially_traced_by_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->ncontacts_potentially_traced_by_prodtype,
                               1, drill_down_list);

  RPT_reporting_add_integer1 (local_data->cumul_ncontacts_potentially_traced, 1, contact_type_name);
  if (local_data->cumul_ncontacts_potentially_traced_by_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->cumul_ncontacts_potentially_traced_by_prodtype,
                               1, drill_down_list);

  if (event->traced == TRUE)
    {
      /* Record a successfully traced contact. */
      RPT_reporting_add_integer1 (local_data->ncontacts_traced, 1, contact_type_name);
      if (local_data->ncontacts_traced_by_prodtype->frequency != RPT_never)
        RPT_reporting_add_integer (local_data->ncontacts_traced_by_prodtype, 1, drill_down_list);

      RPT_reporting_add_integer1 (local_data->cumul_ncontacts_traced, 1, contact_type_name);
      if (local_data->cumul_ncontacts_traced_by_prodtype->frequency != RPT_never)
        RPT_reporting_add_integer (local_data->cumul_ncontacts_traced_by_prodtype,
                                   1, drill_down_list);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_trace_result_event (%s)", MODEL_NAME);
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
    case EVT_AttemptToTrace:
      handle_attempt_to_trace_event (self, &(event->u.attempt_to_trace));
      break;
    case EVT_TraceResult:
      handle_trace_result_event (self, &(event->u.trace_result));
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
  unsigned int n, i, j;
  char *contact_type_name;
  char *drill_down_list[3] = { NULL, NULL, NULL };

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  RPT_reporting_reset (local_data->ntraces_attempted);
  RPT_reporting_reset (local_data->ntraces_attempted_by_prodtype);
  RPT_reporting_reset (local_data->cumul_ntraces_attempted);
  RPT_reporting_reset (local_data->cumul_ntraces_attempted_by_prodtype);
  RPT_reporting_reset (local_data->ncontacts_potentially_traced);
  RPT_reporting_reset (local_data->ncontacts_potentially_traced_by_prodtype);
  RPT_reporting_reset (local_data->cumul_ncontacts_potentially_traced);
  RPT_reporting_reset (local_data->cumul_ncontacts_potentially_traced_by_prodtype);
  RPT_reporting_reset (local_data->ncontacts_traced);
  RPT_reporting_reset (local_data->ncontacts_traced_by_prodtype);
  RPT_reporting_reset (local_data->cumul_ncontacts_traced);
  RPT_reporting_reset (local_data->cumul_ncontacts_traced_by_prodtype);

  /* Initialize counts to 0. */
  n = EVT_NCONTACT_TYPES;
  for (i = 0; i < n; i++)
    {
      if (i == UnknownContact)
        continue;
      contact_type_name = EVT_contact_type_name[i];
      RPT_reporting_add_integer1 (local_data->ncontacts_potentially_traced, 0, contact_type_name);
      RPT_reporting_add_integer1 (local_data->cumul_ncontacts_potentially_traced, 0,
                                  contact_type_name);
      RPT_reporting_add_integer1 (local_data->ncontacts_traced, 0, contact_type_name);
      RPT_reporting_add_integer1 (local_data->cumul_ncontacts_traced, 0, contact_type_name);
      drill_down_list[0] = contact_type_name;
      for (j = 0; j < local_data->production_types->len; j++)
        {
          drill_down_list[1] = (char *) g_ptr_array_index (local_data->production_types, j);
          RPT_reporting_add_integer (local_data->ncontacts_potentially_traced_by_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_ncontacts_potentially_traced_by_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->ncontacts_traced_by_prodtype, 0, drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_ncontacts_traced_by_prodtype, 0,
                                     drill_down_list);
        }
    }


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

  RPT_free_reporting (local_data->ntraces_attempted, TRUE);
  RPT_free_reporting (local_data->ntraces_attempted_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_ntraces_attempted, TRUE);
  RPT_free_reporting (local_data->cumul_ntraces_attempted_by_prodtype, TRUE);
  RPT_free_reporting (local_data->ncontacts_potentially_traced, TRUE);
  RPT_free_reporting (local_data->ncontacts_potentially_traced_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_ncontacts_potentially_traced, TRUE);
  RPT_free_reporting (local_data->cumul_ncontacts_potentially_traced_by_prodtype, TRUE);
  RPT_free_reporting (local_data->ncontacts_traced, TRUE);
  RPT_free_reporting (local_data->ncontacts_traced_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_ncontacts_traced, TRUE);
  RPT_free_reporting (local_data->cumul_ncontacts_traced_by_prodtype, TRUE);

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
 * Returns a new trace back monitor.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *m;
  local_data_t *local_data;
  scew_element *e, **ee;
  unsigned int noutputs;
  RPT_reporting_t *output;
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
  m->outputs = g_ptr_array_sized_new (12);
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

  local_data->ntraces_attempted =
    RPT_new_reporting ("num-traces-attempted", NULL, RPT_integer, RPT_never, FALSE);
  local_data->ntraces_attempted_by_prodtype =
    RPT_new_reporting ("num-traces-attempted-by-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->cumul_ntraces_attempted =
    RPT_new_reporting ("cumulative-num-traces-attempted", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_ntraces_attempted_by_prodtype =
    RPT_new_reporting ("cumulative-num-traces-attempted-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->ncontacts_potentially_traced =
    RPT_new_reporting ("num-contacts-potentially-traced", NULL, RPT_group, RPT_never, FALSE);
  local_data->ncontacts_potentially_traced_by_prodtype =
    RPT_new_reporting ("num-contacts-potentially-traced-by-production-type", NULL, RPT_group,
                       RPT_never, FALSE);
  local_data->cumul_ncontacts_potentially_traced =
    RPT_new_reporting ("cumulative-num-contacts-potentially-traced", NULL, RPT_group, RPT_never,
                       TRUE);
  local_data->cumul_ncontacts_potentially_traced_by_prodtype =
    RPT_new_reporting ("cumulative-num-contacts-potentially-traced-by-production-type", NULL,
                       RPT_group, RPT_never, TRUE);
  local_data->ncontacts_traced =
    RPT_new_reporting ("num-contacts-traced", NULL, RPT_group, RPT_never, FALSE);
  local_data->ncontacts_traced_by_prodtype =
    RPT_new_reporting ("num-contacts-traced-by-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->cumul_ncontacts_traced =
    RPT_new_reporting ("cumulative-num-contacts-traced", NULL, RPT_group, RPT_never, TRUE);
  local_data->cumul_ncontacts_traced_by_prodtype =
    RPT_new_reporting ("cumulative-num-contacts-traced-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  g_ptr_array_add (m->outputs, local_data->ntraces_attempted);
  g_ptr_array_add (m->outputs, local_data->ntraces_attempted_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_ntraces_attempted);
  g_ptr_array_add (m->outputs, local_data->cumul_ntraces_attempted_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->ncontacts_potentially_traced);
  g_ptr_array_add (m->outputs, local_data->ncontacts_potentially_traced_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_ncontacts_potentially_traced);
  g_ptr_array_add (m->outputs, local_data->cumul_ncontacts_potentially_traced_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->ncontacts_traced);
  g_ptr_array_add (m->outputs, local_data->ncontacts_traced_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_ncontacts_traced);
  g_ptr_array_add (m->outputs, local_data->cumul_ncontacts_traced_by_prodtype);

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

  local_data->production_types = herds->production_type_names;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}


char *
trace_back_monitor_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
trace_back_monitor_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file trace-back-monitor.c */
