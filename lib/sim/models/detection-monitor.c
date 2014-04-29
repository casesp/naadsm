/** @file detection-monitor.c
 * Records the day on which the first detection occurred.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date June 2004
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
#define interface_version detection_monitor_LTX_interface_version
#define new detection_monitor_LTX_new
#define run detection_monitor_LTX_run
#define reset detection_monitor_LTX_reset
#define events_listened_for detection_monitor_LTX_events_listened_for
#define is_listening_for detection_monitor_LTX_is_listening_for
#define has_pending_actions detection_monitor_LTX_has_pending_actions
#define has_pending_infections detection_monitor_LTX_has_pending_infections
#define to_string detection_monitor_LTX_to_string
#define local_printf detection_monitor_LTX_printf
#define local_fprintf detection_monitor_LTX_fprintf
#define local_free detection_monitor_LTX_free
#define handle_detection_event detection_monitor_LTX_handle_detection_event
#define events_created event_detection_monitor_LTX_events_created

#include "model.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#include "detection-monitor.h"

#include "guilib.h"

extern const char *HRD_status_name[];
extern const char *RPT_frequency_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "detection-monitor"

#define MODEL_DESCRIPTION "\
A module to record the day of the first detection.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 June 2004\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 0
EVT_event_type_t events_created[] = { 0 };

#define NEVENTS_LISTENED_FOR 1
EVT_event_type_t events_listened_for[] = { EVT_Detection };



/* Specialized information for this model. */
typedef struct
{
  GPtrArray *production_types;
  RPT_reporting_t *detections;
  RPT_reporting_t *day_1st_detection;
  RPT_reporting_t *nherds_detected;
  RPT_reporting_t *nherds_detected_by_prodtype;
  RPT_reporting_t *cumul_nherds_detected;
  RPT_reporting_t *cumul_nherds_detected_by_prodtype;
  gboolean first_detection;
  GString *target;              /* a temporary string used repeatedly. */
}
local_data_t;



/**
 * Records the day of the first detection.
 *
 * @param self the model.
 * @param event a detection event.
 */
void
handle_detection_event (struct ergadm_model_t_ *self, EVT_detection_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  char *peek;
  gboolean first;
  HRD_update_t update;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_detection_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  peek = RPT_reporting_get_text (local_data->detections, NULL);
  first = (peek == NULL) || (strlen (peek) == 0);

  g_string_printf (local_data->target, first ? "%u" : ",%u", herd->index);
  RPT_reporting_append_text (local_data->detections, local_data->target->str, NULL);

  if (NULL != guilib_detect_herd)
    {
      update.index = herd->index;
      update.success = 2;       /* Unused */
      guilib_detect_herd (update);
    }
  /*
     #if ARDEBUG
     printf( "Successful detection: herd index %d\n", herd->index );
     #endif
   */

  if (local_data->first_detection)
    {
      RPT_reporting_set_integer (local_data->day_1st_detection, event->day, NULL);
      local_data->first_detection = FALSE;
    }
  RPT_reporting_add_integer (local_data->nherds_detected, 1, NULL);
  RPT_reporting_add_integer1 (local_data->nherds_detected_by_prodtype, 1,
                              herd->production_type_name);
  RPT_reporting_add_integer (local_data->cumul_nherds_detected, 1, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_nherds_detected_by_prodtype, 1,
                              herd->production_type_name);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_detection_event (%s)", MODEL_NAME);
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
    case EVT_Detection:
      handle_detection_event (self, &(event->u.detection));
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
  unsigned int n, i;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  local_data->first_detection = TRUE;
  RPT_reporting_reset (local_data->detections);
  RPT_reporting_reset (local_data->day_1st_detection);
  RPT_reporting_zero (local_data->nherds_detected);
  RPT_reporting_zero (local_data->cumul_nherds_detected);
  RPT_reporting_reset (local_data->nherds_detected_by_prodtype);
  RPT_reporting_reset (local_data->cumul_nherds_detected_by_prodtype);
  n = local_data->production_types->len;
  for (i = 0; i < n; i++)
    {
      RPT_reporting_set_integer1 (local_data->nherds_detected_by_prodtype, 0,
                                  (char *) g_ptr_array_index (local_data->production_types, i));
      RPT_reporting_set_integer1 (local_data->cumul_nherds_detected_by_prodtype, 0,
                                  (char *) g_ptr_array_index (local_data->production_types, i));
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
  RPT_free_reporting (local_data->detections, TRUE);
  RPT_free_reporting (local_data->day_1st_detection, TRUE);
  RPT_free_reporting (local_data->nherds_detected, TRUE);
  RPT_free_reporting (local_data->nherds_detected_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_nherds_detected, TRUE);
  RPT_free_reporting (local_data->cumul_nherds_detected_by_prodtype, TRUE);

  g_string_free (local_data->target, TRUE);

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
 * Returns a new detection monitor.
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
  int i, j;                     /* loop counters */

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
  m->outputs = g_ptr_array_sized_new (6);
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

  local_data->detections = RPT_new_reporting ("detections", NULL, RPT_text, RPT_never, FALSE);
  local_data->day_1st_detection =
    RPT_new_reporting ("time-to-first-detection", NULL, RPT_integer, RPT_never, TRUE);
  local_data->nherds_detected =
    RPT_new_reporting ("num-units-detected", NULL, RPT_integer, RPT_never, FALSE);
  local_data->nherds_detected_by_prodtype =
    RPT_new_reporting ("num-units-detected-by-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->cumul_nherds_detected =
    RPT_new_reporting ("cumulative-num-units-detected", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_nherds_detected_by_prodtype =
    RPT_new_reporting ("cumulative-num-units-detected-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  g_ptr_array_add (m->outputs, local_data->detections);
  g_ptr_array_add (m->outputs, local_data->day_1st_detection);
  g_ptr_array_add (m->outputs, local_data->nherds_detected);
  g_ptr_array_add (m->outputs, local_data->nherds_detected_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_nherds_detected);
  g_ptr_array_add (m->outputs, local_data->cumul_nherds_detected_by_prodtype);

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

  /* No detections yet. */
  local_data->first_detection = TRUE;

  local_data->production_types = herds->production_type_names;
  for (i = 0; i < local_data->production_types->len; i++)
    {
      RPT_reporting_set_integer1 (local_data->nherds_detected_by_prodtype, 0,
                                  (char *) g_ptr_array_index (local_data->production_types, i));
      RPT_reporting_set_integer1 (local_data->cumul_nherds_detected_by_prodtype, 0,
                                  (char *) g_ptr_array_index (local_data->production_types, i));
    }

  local_data->target = g_string_new (NULL);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}


char *
detection_monitor_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
detection_monitor_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file detection-monitor.c */
