/** @file exposure-monitor.c
 * Tracks the cause of exposures.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date August 2004
 *
 * Copyright &copy; University of Guelph, 2004-2008
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
#define interface_version exposure_monitor_LTX_interface_version
#define new exposure_monitor_LTX_new
#define run exposure_monitor_LTX_run
#define reset exposure_monitor_LTX_reset
#define events_listened_for exposure_monitor_LTX_events_listened_for
#define is_listening_for exposure_monitor_LTX_is_listening_for
#define has_pending_actions exposure_monitor_LTX_has_pending_actions
#define has_pending_infections exposure_monitor_LTX_has_pending_infections
#define to_string exposure_monitor_LTX_to_string
#define local_printf exposure_monitor_LTX_printf
#define local_fprintf exposure_monitor_LTX_fprintf
#define local_free exposure_monitor_LTX_free
#define handle_new_day_event exposure_monitor_LTX_handle_new_day_event
#define handle_declaration_of_exposure_causes_event exposure_monitor_LTX_handle_declaration_of_exposure_causes_event
#define handle_exposure_event exposure_monitor_LTX_handle_exposure_event
#define events_created exposure_monitor_LTX_events_created

#include "model.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#include "exposure-monitor.h"

#include "guilib.h"

/** This must match an element name in the DTD. */
#define MODEL_NAME "exposure-monitor"

#define MODEL_DESCRIPTION "\
A module to track the cause of exposures.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 August 2004\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 1
EVT_event_type_t events_created[] = { EVT_RequestForExposureCauses };

#define NEVENTS_LISTENED_FOR 3
EVT_event_type_t events_listened_for[] =
  { EVT_NewDay, EVT_DeclarationOfExposureCauses, EVT_Exposure };



extern const char *RPT_frequency_name[];



/** Specialized information for this model. */
typedef struct
{
  GPtrArray *production_types;
  RPT_reporting_t *exposures;
  RPT_reporting_t *num_units_exposed;
  RPT_reporting_t *num_units_exposed_by_cause;
  RPT_reporting_t *num_units_exposed_by_prodtype;
  RPT_reporting_t *num_units_exposed_by_cause_and_prodtype;
  RPT_reporting_t *cumul_num_units_exposed;
  RPT_reporting_t *cumul_num_units_exposed_by_cause;
  RPT_reporting_t *cumul_num_units_exposed_by_prodtype;
  RPT_reporting_t *cumul_num_units_exposed_by_cause_and_prodtype;
  RPT_reporting_t *num_animals_exposed;
  RPT_reporting_t *num_animals_exposed_by_cause;
  RPT_reporting_t *num_animals_exposed_by_prodtype;
  RPT_reporting_t *num_animals_exposed_by_cause_and_prodtype;
  RPT_reporting_t *cumul_num_animals_exposed;
  RPT_reporting_t *cumul_num_animals_exposed_by_cause;
  RPT_reporting_t *cumul_num_animals_exposed_by_prodtype;
  RPT_reporting_t *cumul_num_animals_exposed_by_cause_and_prodtype;
  GPtrArray *causes;
  GString *source_and_target;
}
local_data_t;



/**
 * On the first day of the first simulation, this model requests that any
 * sub-models capable of causing exposures declare the causes they may state
 * for the exposures.  This is done so that this model can initialize counters
 * to 0.
 *
 * @param self the model.
 * @param event a new day event.
 * @param queue for any new events the model creates.
 */
void
handle_new_day_event (struct ergadm_model_t_ *self,
                      EVT_new_day_event_t * event, EVT_event_queue_t * queue)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  if (event->day == 1 && local_data->causes == NULL)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "requesting potential causes of exposure from other sub-models");
#endif
      local_data->causes = g_ptr_array_new ();
      EVT_event_enqueue (queue, EVT_new_request_for_exposure_causes_event ());
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a declaration of exposure causes by recording the potential
 * causes of exposure.
 *
 * @param self the model.
 * @param event a declaration of exposure causes event.
 */
void
handle_declaration_of_exposure_causes_event (struct ergadm_model_t_ *self,
                                             EVT_declaration_of_exposure_causes_event_t * event)
{
  local_data_t *local_data;
  unsigned int n, i, j;
  char *cause;
  char *drill_down_list[3] = { NULL, NULL, NULL };
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_declaration_of_exposure_causes_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Copy the list of potential causes of exposure.  (Note that we just copy
   * the pointers to the C strings, assuming that they are static strings.)  If
   * any potential cause is not already present in our reporting variables, add
   * it, with an initial count of 0 exposures. */
  n = event->causes->len;
  for (i = 0; i < n; i++)
    {
      cause = (char *) g_ptr_array_index (event->causes, i);
      g_ptr_array_add (local_data->causes, cause);
      RPT_reporting_add_integer1 (local_data->num_units_exposed_by_cause, 0, cause);
      RPT_reporting_add_integer1 (local_data->cumul_num_units_exposed_by_cause, 0, cause);
      RPT_reporting_add_integer1 (local_data->num_animals_exposed_by_cause, 0, cause);
      RPT_reporting_add_integer1 (local_data->cumul_num_animals_exposed_by_cause, 0, cause);

      drill_down_list[0] = cause;
      for (j = 0; j < local_data->production_types->len; j++)
        {
          drill_down_list[1] = (char *) g_ptr_array_index (local_data->production_types, j);
          RPT_reporting_add_integer (local_data->num_units_exposed_by_cause_and_prodtype, 0, drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_num_units_exposed_by_cause_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->num_animals_exposed_by_cause_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_num_animals_exposed_by_cause_and_prodtype, 0,
                                     drill_down_list);
        }
    }
#if DEBUG
  s = g_string_new ("list of causes now={");
  n = local_data->causes->len;
  for (i = 0; i < n; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (local_data->causes, i));
  g_string_append_c (s, '}');
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, s->str);
  g_string_free (s, TRUE);
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_declaration_of_exposure_causes_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to an exposure event by recording it.
 *
 * @param self the model.
 * @param event an exposure event.
 */
void
handle_exposure_event (struct ergadm_model_t_ *self, EVT_exposure_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *exposing_herd, *exposed_herd;
  char *peek;
  gboolean first_of_cause;
  char *drill_down_list[3] = { NULL, NULL, NULL };
  HRD_update_t update;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_exposure_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  exposing_herd = event->exposing_herd;
  exposed_herd = event->exposed_herd;

  /* Update the text string that lists exposed herd indices. */
  peek = RPT_reporting_get_text1 (local_data->exposures, event->cause);
  first_of_cause = (peek == NULL) || (strlen (peek) == 0);

  g_string_printf (local_data->source_and_target,
                   first_of_cause ? "%u->%u" : ",%u->%u",
                   event->exposing_herd->index, event->exposed_herd->index);
  RPT_reporting_append_text1 (local_data->exposures, local_data->source_and_target->str,
                              event->cause);

  if (NULL != guilib_expose_herd)
    {
      update.index = event->exposed_herd->index;
      update.success = 2;       /* Unused */
      update.msg = event->cause;
      guilib_expose_herd (update);
    }

#if UNDEFINED
  printf ("Herd at index %d exposed by method %s\n", event->exposed_herd->index, event->cause);
#endif

  /* Update the counts of exposures. */
  RPT_reporting_add_integer  (local_data->num_units_exposed, 1, NULL);
  RPT_reporting_add_integer1 (local_data->num_units_exposed_by_cause, 1, event->cause);
  RPT_reporting_add_integer1 (local_data->num_units_exposed_by_prodtype, 1, exposed_herd->production_type_name);
  RPT_reporting_add_integer  (local_data->num_animals_exposed, exposed_herd->size, NULL);
  RPT_reporting_add_integer1 (local_data->num_animals_exposed_by_cause, exposed_herd->size, event->cause);
  RPT_reporting_add_integer1 (local_data->num_animals_exposed_by_prodtype, exposed_herd->size, exposed_herd->production_type_name);
  RPT_reporting_add_integer  (local_data->cumul_num_units_exposed, 1, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_num_units_exposed_by_cause, 1, event->cause);
  RPT_reporting_add_integer1 (local_data->cumul_num_units_exposed_by_prodtype, 1, exposed_herd->production_type_name);
  RPT_reporting_add_integer  (local_data->cumul_num_animals_exposed, exposed_herd->size, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_num_animals_exposed_by_cause, exposed_herd->size,
                              event->cause);
  RPT_reporting_add_integer1 (local_data->cumul_num_animals_exposed_by_prodtype, exposed_herd->size,
                              exposed_herd->production_type_name);
  drill_down_list[0] = event->cause;
  drill_down_list[1] = exposed_herd->production_type_name;
  if (local_data->num_units_exposed_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->num_units_exposed_by_cause_and_prodtype, 1, drill_down_list);
  if (local_data->num_animals_exposed_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->num_animals_exposed_by_cause_and_prodtype, exposed_herd->size,
                               drill_down_list);
  if (local_data->cumul_num_units_exposed_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->cumul_num_units_exposed_by_cause_and_prodtype, 1, drill_down_list);
  if (local_data->cumul_num_animals_exposed_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->cumul_num_animals_exposed_by_cause_and_prodtype,
                               exposed_herd->size, drill_down_list);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_exposure_event (%s)", MODEL_NAME);
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
      handle_new_day_event (self, &(event->u.new_day), queue);
      break;
    case EVT_DeclarationOfExposureCauses:
      handle_declaration_of_exposure_causes_event (self,
                                                   &(event->u.declaration_of_exposure_causes));
      break;
    case EVT_Exposure:
      handle_exposure_event (self, &(event->u.exposure));
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
  char *prodtype_name;
  char *cause;
  char *drill_down_list[3] = { NULL, NULL, NULL };

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  RPT_reporting_reset (local_data->exposures);
  RPT_reporting_reset (local_data->num_units_exposed);
  RPT_reporting_reset (local_data->num_units_exposed_by_cause);
  RPT_reporting_reset (local_data->num_units_exposed_by_prodtype);
  RPT_reporting_reset (local_data->num_units_exposed_by_cause_and_prodtype);
  RPT_reporting_reset (local_data->cumul_num_units_exposed);
  RPT_reporting_reset (local_data->cumul_num_units_exposed_by_cause);
  RPT_reporting_reset (local_data->cumul_num_units_exposed_by_prodtype);
  RPT_reporting_reset (local_data->cumul_num_units_exposed_by_cause_and_prodtype);
  RPT_reporting_reset (local_data->num_animals_exposed);
  RPT_reporting_reset (local_data->num_animals_exposed_by_cause);
  RPT_reporting_reset (local_data->num_animals_exposed_by_prodtype);
  RPT_reporting_reset (local_data->num_animals_exposed_by_cause_and_prodtype);
  RPT_reporting_reset (local_data->cumul_num_animals_exposed);
  RPT_reporting_reset (local_data->cumul_num_animals_exposed_by_cause);
  RPT_reporting_reset (local_data->cumul_num_animals_exposed_by_prodtype);
  RPT_reporting_reset (local_data->cumul_num_animals_exposed_by_cause_and_prodtype);

  /* Initialize counts to 0. */

  /* These are the counts broken down by production type. */
  n = local_data->production_types->len;
  for (i = 0; i < n; i++)
    {
      prodtype_name = (char *) g_ptr_array_index (local_data->production_types, i);
      RPT_reporting_add_integer1 (local_data->num_units_exposed_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->cumul_num_units_exposed_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->num_animals_exposed_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->cumul_num_animals_exposed_by_prodtype, 0, prodtype_name);
    }

  if (local_data->causes != NULL)
    {
      n = local_data->causes->len;
      for (i = 0; i < n; i++)
        {
          /* These are the counts broken down by cause of exposure. */
          cause = (char *) g_ptr_array_index (local_data->causes, i);
          RPT_reporting_add_integer1 (local_data->num_units_exposed_by_cause, 0, cause);
          RPT_reporting_add_integer1 (local_data->cumul_num_units_exposed_by_cause, 0, cause);
          RPT_reporting_add_integer1 (local_data->num_animals_exposed_by_cause, 0, cause);
          RPT_reporting_add_integer1 (local_data->cumul_num_animals_exposed_by_cause, 0, cause);

          /* These are the counts broken down by cause and production type. */
          drill_down_list[0] = cause;
          for (j = 0; j < local_data->production_types->len; j++)
            {
              drill_down_list[1] = (char *) g_ptr_array_index (local_data->production_types, j);
              RPT_reporting_add_integer (local_data->num_units_exposed_by_cause_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->cumul_num_units_exposed_by_cause_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->num_animals_exposed_by_cause_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->cumul_num_animals_exposed_by_cause_and_prodtype, 0,
                                         drill_down_list);
            }
        }
    }

  /* Note that we don't reset the list of possible causes of exposure between
   * iterations. */

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
  RPT_free_reporting (local_data->exposures, TRUE);
  RPT_free_reporting (local_data->num_units_exposed, TRUE);
  RPT_free_reporting (local_data->num_units_exposed_by_cause, TRUE);
  RPT_free_reporting (local_data->num_units_exposed_by_prodtype, TRUE);
  RPT_free_reporting (local_data->num_units_exposed_by_cause_and_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_exposed, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_exposed_by_cause, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_exposed_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_exposed_by_cause_and_prodtype, TRUE);
  RPT_free_reporting (local_data->num_animals_exposed, TRUE);
  RPT_free_reporting (local_data->num_animals_exposed_by_cause, TRUE);
  RPT_free_reporting (local_data->num_animals_exposed_by_prodtype, TRUE);
  RPT_free_reporting (local_data->num_animals_exposed_by_cause_and_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_exposed, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_exposed_by_cause, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_exposed_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_exposed_by_cause_and_prodtype, TRUE);

  /* Note that we don't attempt to free the C strings in the exposure causes
   * list, because they're assumed to be static strings. */
  if (local_data->causes != NULL)
    g_ptr_array_free (local_data->causes, TRUE);

  g_string_free (local_data->source_and_target, TRUE);

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
 * Returns a new exposure monitor.
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
  m->outputs = g_ptr_array_sized_new (17);
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

  local_data->exposures = RPT_new_reporting ("exposures", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_units_exposed =
    RPT_new_reporting ("num-units-exposed", NULL, RPT_integer, RPT_never, FALSE);
  local_data->num_units_exposed_by_cause =
    RPT_new_reporting ("num-units-exposed-by-cause", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_units_exposed_by_prodtype =
    RPT_new_reporting ("num-units-exposed-by-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_units_exposed_by_cause_and_prodtype =
    RPT_new_reporting ("num-units-exposed-by-cause-and-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->cumul_num_units_exposed =
    RPT_new_reporting ("cumulative-num-units-exposed", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_num_units_exposed_by_cause =
    RPT_new_reporting ("cumulative-num-units-exposed-by-cause", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_units_exposed_by_prodtype =
    RPT_new_reporting ("cumulative-num-units-exposed-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_units_exposed_by_cause_and_prodtype =
    RPT_new_reporting ("cumulative-num-units-exposed-by-cause-and-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->num_animals_exposed =
    RPT_new_reporting ("num-animals-exposed", NULL, RPT_integer, RPT_never, FALSE);
  local_data->num_animals_exposed_by_cause =
    RPT_new_reporting ("num-animals-exposed-by-cause", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_animals_exposed_by_prodtype =
    RPT_new_reporting ("num-animals-exposed-by-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_animals_exposed_by_cause_and_prodtype =
    RPT_new_reporting ("num-animals-exposed-by-cause-and-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->cumul_num_animals_exposed =
    RPT_new_reporting ("cumulative-num-animals-exposed", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_num_animals_exposed_by_cause =
    RPT_new_reporting ("cumulative-num-animals-exposed-by-cause", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_animals_exposed_by_prodtype =
    RPT_new_reporting ("cumulative-num-animals-exposed-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_animals_exposed_by_cause_and_prodtype =
    RPT_new_reporting ("cumulative-num-animals-exposed-by-cause-and-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  g_ptr_array_add (m->outputs, local_data->exposures);
  g_ptr_array_add (m->outputs, local_data->num_units_exposed);
  g_ptr_array_add (m->outputs, local_data->num_units_exposed_by_cause);
  g_ptr_array_add (m->outputs, local_data->num_units_exposed_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_units_exposed_by_cause_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_exposed);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_exposed_by_cause);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_exposed_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_exposed_by_cause_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_animals_exposed);
  g_ptr_array_add (m->outputs, local_data->num_animals_exposed_by_cause);
  g_ptr_array_add (m->outputs, local_data->num_animals_exposed_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_animals_exposed_by_cause_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_exposed);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_exposed_by_cause);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_exposed_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_exposed_by_cause_and_prodtype);

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

  /* A list to hold possible causes of exposure.  Will be initialized to a
   * GPtrArray when other sub-models declare causes of exposure. */
  local_data->causes = NULL;

  local_data->production_types = herds->production_type_names;
  local_data->source_and_target = g_string_new (NULL);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

char *
exposure_monitor_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
exposure_monitor_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file exposure-monitor.c */
