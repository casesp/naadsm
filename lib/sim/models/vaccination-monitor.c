/** @file vaccination-monitor.c
 * Tracks the number of and reasons for vaccinations.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date January 2005
 *
 * Copyright &copy; University of Guelph, 2005-2008
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
#define interface_version vaccination_monitor_LTX_interface_version
#define new vaccination_monitor_LTX_new
#define run vaccination_monitor_LTX_run
#define reset vaccination_monitor_LTX_reset
#define events_listened_for vaccination_monitor_LTX_events_listened_for
#define is_listening_for vaccination_monitor_LTX_is_listening_for
#define has_pending_actions vaccination_monitor_LTX_has_pending_actions
#define has_pending_infections vaccination_monitor_LTX_has_pending_infections
#define to_string vaccination_monitor_LTX_to_string
#define local_printf vaccination_monitor_LTX_printf
#define local_fprintf vaccination_monitor_LTX_fprintf
#define local_free vaccination_monitor_LTX_free
#define handle_new_day_event vaccination_monitor_LTX_handle_new_day_event
#define handle_declaration_of_vaccination_reasons_event vaccination_monitor_LTX_handle_declaration_of_vaccination_reasons_event
#define handle_vaccination_event vaccination_monitor_LTX_handle_vaccination_event
#define events_created vaccination_monitor_LTX_events_created

#include "model.h"

#include "vaccination-monitor.h"

#include "guilib.h"

#if STDC_HEADERS
#  include <string.h>
#endif

/** This must match an element name in the DTD. */
#define MODEL_NAME "vaccination-monitor"

#define MODEL_DESCRIPTION "\
A module to track the number of and reasons for vaccinations.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 January 2005\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 1
EVT_event_type_t events_created[] = { EVT_RequestForVaccinationReasons };

#define NEVENTS_LISTENED_FOR 3
EVT_event_type_t events_listened_for[] =
  { EVT_NewDay, EVT_DeclarationOfVaccinationReasons, EVT_Vaccination };



extern const char *RPT_frequency_name[];



/** Specialized information for this model. */
typedef struct
{
  GPtrArray *production_types;
  RPT_reporting_t *vaccinations;
  RPT_reporting_t *num_units_vaccinated;
  RPT_reporting_t *num_units_vaccinated_by_reason;
  RPT_reporting_t *num_units_vaccinated_by_prodtype;
  RPT_reporting_t *num_units_vaccinated_by_reason_and_prodtype;
  RPT_reporting_t *cumul_num_units_vaccinated;
  RPT_reporting_t *cumul_num_units_vaccinated_by_reason;
  RPT_reporting_t *cumul_num_units_vaccinated_by_prodtype;
  RPT_reporting_t *cumul_num_units_vaccinated_by_reason_and_prodtype;
  RPT_reporting_t *num_animals_vaccinated;
  RPT_reporting_t *num_animals_vaccinated_by_reason;
  RPT_reporting_t *num_animals_vaccinated_by_prodtype;
  RPT_reporting_t *num_animals_vaccinated_by_reason_and_prodtype;
  RPT_reporting_t *cumul_num_animals_vaccinated;
  RPT_reporting_t *cumul_num_animals_vaccinated_by_reason;
  RPT_reporting_t *cumul_num_animals_vaccinated_by_prodtype;
  RPT_reporting_t *cumul_num_animals_vaccinated_by_reason_and_prodtype;
  GPtrArray *reasons;
  GString *target;              /* a temporary string used repeatedly. */
}
local_data_t;



/**
 * On the first day of the first simulation, this model requests that any
 * sub-models capable of requesting vaccinations declare the reasons they may
 * give for their requests.  This is done so that this model can initialize
 * counters to 0.
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
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "requesting potential reasons for vaccination from other sub-models");
#endif
  if (event->day == 1 && local_data->reasons == NULL)
    {
      local_data->reasons = g_ptr_array_new ();
      EVT_event_enqueue (queue, EVT_new_request_for_vaccination_reasons_event ());
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a declaration of vaccination reasons by recording the potential
 * reasons for vaccination.
 *
 * @param self the model.
 * @param event a declaration of vaccination reasons event.
 */
void
handle_declaration_of_vaccination_reasons_event (struct ergadm_model_t_ *self,
                                                 EVT_declaration_of_vaccination_reasons_event_t *
                                                 event)
{
  local_data_t *local_data;
  unsigned int n, i, j;
  char *reason;
  char *drill_down_list[3] = { NULL, NULL, NULL };
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_declaration_of_vaccination_reasons_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Copy the list of potential reasons for vaccination.  (Note that we just
   * copy the pointers to the C strings, assuming that they are static strings.)
   * If any potential reason is not already present in our reporting variables,
   * add it, with an initial count of 0 vaccinations. */
  n = event->reasons->len;
  for (i = 0; i < n; i++)
    {
      reason = (char *) g_ptr_array_index (event->reasons, i);
      g_ptr_array_add (local_data->reasons, reason);
      RPT_reporting_add_integer1 (local_data->num_units_vaccinated_by_reason, 0, reason);
      RPT_reporting_add_integer1 (local_data->cumul_num_units_vaccinated_by_reason, 0, reason);
      RPT_reporting_add_integer1 (local_data->num_animals_vaccinated_by_reason, 0, reason);
      RPT_reporting_add_integer1 (local_data->cumul_num_animals_vaccinated_by_reason, 0, reason);

      drill_down_list[0] = reason;
      for (j = 0; j < local_data->production_types->len; j++)
        {
          drill_down_list[1] = (char *) g_ptr_array_index (local_data->production_types, j);
          RPT_reporting_add_integer (local_data->num_units_vaccinated_by_reason_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_num_units_vaccinated_by_reason_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->num_animals_vaccinated_by_reason_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype, 0,
                                     drill_down_list);
        }
    }
#if DEBUG
  s = g_string_new ("list of reasons now={");
  n = local_data->reasons->len;
  for (i = 0; i < n; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (local_data->reasons, i));
  g_string_append_c (s, '}');
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, s->str);
  g_string_free (s, TRUE);
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_declaration_of_vaccination_reasons_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a vaccination by recording it.
 *
 * @param self the model.
 * @param event a vaccination event.
 */
void
handle_vaccination_event (struct ergadm_model_t_ *self, EVT_vaccination_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  char *peek;
  gboolean first_of_cause;
  char *drill_down_list[3] = { NULL, NULL, NULL };
  HRD_update_t update;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_vaccination_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  peek = RPT_reporting_get_text1 (local_data->vaccinations, event->reason);
  first_of_cause = (peek == NULL) || (strlen (peek) == 0);

  g_string_printf (local_data->target, first_of_cause ? "%u" : ",%u", herd->index);

  if (NULL != guilib_vaccinate_herd)
    {
      update.index = herd->index;
      update.msg = event->reason;
      update.success = 2;       /* Unused */
      guilib_vaccinate_herd (update);
    }

  RPT_reporting_append_text1 (local_data->vaccinations, local_data->target->str, event->reason);

  RPT_reporting_add_integer  (local_data->num_units_vaccinated, 1, NULL);
  RPT_reporting_add_integer1 (local_data->num_units_vaccinated_by_reason, 1, event->reason);
  RPT_reporting_add_integer1 (local_data->num_units_vaccinated_by_prodtype, 1, herd->production_type_name);
  RPT_reporting_add_integer  (local_data->num_animals_vaccinated, herd->size, NULL);
  RPT_reporting_add_integer1 (local_data->num_animals_vaccinated_by_reason, herd->size, event->reason);
  RPT_reporting_add_integer1 (local_data->num_animals_vaccinated_by_prodtype, herd->size, herd->production_type_name);
  RPT_reporting_add_integer  (local_data->cumul_num_units_vaccinated, 1, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_num_units_vaccinated_by_reason, 1, event->reason);
  RPT_reporting_add_integer1 (local_data->cumul_num_units_vaccinated_by_prodtype, 1, herd->production_type_name);
  RPT_reporting_add_integer  (local_data->cumul_num_animals_vaccinated, herd->size, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_num_animals_vaccinated_by_reason, herd->size, event->reason);
  RPT_reporting_add_integer1 (local_data->cumul_num_animals_vaccinated_by_prodtype, herd->size, herd->production_type_name);
  drill_down_list[0] = event->reason;
  drill_down_list[1] = herd->production_type_name;
  if (local_data->num_units_vaccinated_by_reason_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->num_units_vaccinated_by_reason_and_prodtype, 1, drill_down_list);
  if (local_data->num_animals_vaccinated_by_reason_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->num_animals_vaccinated_by_reason_and_prodtype, herd->size,
                               drill_down_list);
  if (local_data->cumul_num_units_vaccinated_by_reason_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->cumul_num_units_vaccinated_by_reason_and_prodtype, 1,
                               drill_down_list);
  if (local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype, herd->size,
                               drill_down_list);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_vaccination_event (%s)", MODEL_NAME);
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
    case EVT_DeclarationOfVaccinationReasons:
      handle_declaration_of_vaccination_reasons_event (self,
                                                       &(event->u.
                                                         declaration_of_vaccination_reasons));
      break;
    case EVT_Vaccination:
      handle_vaccination_event (self, &(event->u.vaccination));
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
  char *reason;
  char *drill_down_list[3] = { NULL, NULL, NULL };

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  RPT_reporting_reset (local_data->vaccinations);
  RPT_reporting_reset (local_data->num_units_vaccinated);
  RPT_reporting_reset (local_data->num_units_vaccinated_by_reason);
  RPT_reporting_reset (local_data->num_units_vaccinated_by_prodtype);
  RPT_reporting_reset (local_data->num_units_vaccinated_by_reason_and_prodtype);
  RPT_reporting_reset (local_data->cumul_num_units_vaccinated);
  RPT_reporting_reset (local_data->cumul_num_units_vaccinated_by_reason);
  RPT_reporting_reset (local_data->cumul_num_units_vaccinated_by_prodtype);
  RPT_reporting_reset (local_data->cumul_num_units_vaccinated_by_reason_and_prodtype);
  RPT_reporting_reset (local_data->num_animals_vaccinated);
  RPT_reporting_reset (local_data->num_animals_vaccinated_by_reason);
  RPT_reporting_reset (local_data->num_animals_vaccinated_by_prodtype);
  RPT_reporting_reset (local_data->num_animals_vaccinated_by_reason_and_prodtype);
  RPT_reporting_reset (local_data->cumul_num_animals_vaccinated);
  RPT_reporting_reset (local_data->cumul_num_animals_vaccinated_by_reason);
  RPT_reporting_reset (local_data->cumul_num_animals_vaccinated_by_prodtype);
  RPT_reporting_reset (local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype);

  /* Initialize counts to 0. */

  /* These are the counts broken down by production type. */
  n = local_data->production_types->len;
  for (i = 0; i < n; i++)
    {
      prodtype_name = (char *) g_ptr_array_index (local_data->production_types, i);
      RPT_reporting_add_integer1 (local_data->num_units_vaccinated_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->cumul_num_units_vaccinated_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->num_animals_vaccinated_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->cumul_num_animals_vaccinated_by_prodtype, 0, prodtype_name);
    }

  if (local_data->reasons != NULL)
    {
      n = local_data->reasons->len;
      for (i = 0; i < n; i++)
        {
          /* These are the counts broken down by reason. */
          reason = (char *) g_ptr_array_index (local_data->reasons, i);
          RPT_reporting_add_integer1 (local_data->num_units_vaccinated_by_reason, 0, reason);
          RPT_reporting_add_integer1 (local_data->cumul_num_units_vaccinated_by_reason, 0, reason);
          RPT_reporting_add_integer1 (local_data->num_animals_vaccinated_by_reason, 0, reason);
          RPT_reporting_add_integer1 (local_data->cumul_num_animals_vaccinated_by_reason, 0, reason);

          /* These are the counts broken down by reason and production type. */
          drill_down_list[0] = reason;
          for (j = 0; j < local_data->production_types->len; j++)
            {
              drill_down_list[1] = (char *) g_ptr_array_index (local_data->production_types, j);
              RPT_reporting_add_integer (local_data->num_units_vaccinated_by_reason_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->cumul_num_units_vaccinated_by_reason_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->num_animals_vaccinated_by_reason_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype, 0,
                                         drill_down_list);
            }
        }
    }

  /* Note that we don't reset the list of possible reasons for vaccination
   * between iterations. */

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
  RPT_free_reporting (local_data->vaccinations, TRUE);
  RPT_free_reporting (local_data->num_units_vaccinated, TRUE);
  RPT_free_reporting (local_data->num_units_vaccinated_by_reason, TRUE);
  RPT_free_reporting (local_data->num_units_vaccinated_by_prodtype, TRUE);
  RPT_free_reporting (local_data->num_units_vaccinated_by_reason_and_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_vaccinated, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_vaccinated_by_reason, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_vaccinated_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_vaccinated_by_reason_and_prodtype, TRUE);
  RPT_free_reporting (local_data->num_animals_vaccinated, TRUE);
  RPT_free_reporting (local_data->num_animals_vaccinated_by_reason, TRUE);
  RPT_free_reporting (local_data->num_animals_vaccinated_by_prodtype, TRUE);
  RPT_free_reporting (local_data->num_animals_vaccinated_by_reason_and_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_vaccinated, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_vaccinated_by_reason, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_vaccinated_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype, TRUE);

  /* Note that we don't attempt to free the C strings in the vaccination
   * reasons list, because they're assumed to be static strings. */
  if (local_data->reasons != NULL)
    g_ptr_array_free (local_data->reasons, TRUE);

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
 * Returns a new vaccination monitor.
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

  local_data->vaccinations = RPT_new_reporting ("vaccinations", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_units_vaccinated =
    RPT_new_reporting ("num-units-vaccinated", NULL, RPT_integer, RPT_never, FALSE);
  local_data->num_units_vaccinated_by_reason =
    RPT_new_reporting ("num-units-vaccinated-by-reason", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->num_units_vaccinated_by_prodtype =
    RPT_new_reporting ("num-units-vaccinated-by-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->num_units_vaccinated_by_reason_and_prodtype =
    RPT_new_reporting ("num-units-vaccinated-by-reason-and-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->cumul_num_units_vaccinated =
    RPT_new_reporting ("cumulative-num-units-vaccinated", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_num_units_vaccinated_by_reason =
    RPT_new_reporting ("cumulative-num-units-vaccinated-by-reason", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_units_vaccinated_by_prodtype =
    RPT_new_reporting ("cumulative-num-units-vaccinated-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_units_vaccinated_by_reason_and_prodtype =
    RPT_new_reporting ("cumulative-num-units-vaccinated-by-reason-and-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->num_animals_vaccinated =
    RPT_new_reporting ("num-animals-vaccinated", NULL, RPT_integer, RPT_never, FALSE);
  local_data->num_animals_vaccinated_by_reason =
    RPT_new_reporting ("num-animals-vaccinated-by-reason", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->num_animals_vaccinated_by_prodtype =
    RPT_new_reporting ("num-animals-vaccinated-by-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->num_animals_vaccinated_by_reason_and_prodtype =
    RPT_new_reporting ("num-animals-vaccinated-by-reason-and-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->cumul_num_animals_vaccinated =
    RPT_new_reporting ("cumulative-num-animals-vaccinated", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_num_animals_vaccinated_by_reason =
    RPT_new_reporting ("cumulative-num-animals-vaccinated-by-reason", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_animals_vaccinated_by_prodtype =
    RPT_new_reporting ("cumulative-num-animals-vaccinated-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype =
    RPT_new_reporting ("cumulative-num-animals-vaccinated-by-reason-and-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  g_ptr_array_add (m->outputs, local_data->vaccinations);
  g_ptr_array_add (m->outputs, local_data->num_units_vaccinated);
  g_ptr_array_add (m->outputs, local_data->num_units_vaccinated_by_reason);
  g_ptr_array_add (m->outputs, local_data->num_units_vaccinated_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_units_vaccinated_by_reason_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_vaccinated);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_vaccinated_by_reason);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_vaccinated_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_vaccinated_by_reason_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_animals_vaccinated);
  g_ptr_array_add (m->outputs, local_data->num_animals_vaccinated_by_reason);
  g_ptr_array_add (m->outputs, local_data->num_animals_vaccinated_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_animals_vaccinated_by_reason_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_vaccinated);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_vaccinated_by_reason);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_vaccinated_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_vaccinated_by_reason_and_prodtype);

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

  /* A list to hold possible reasons for vaccination.  Will be initialized to
   * a GPtrArray when other sub-models declare reasons for vaccination. */
  local_data->reasons = NULL;

  local_data->production_types = herds->production_type_names;
  local_data->target = g_string_new (NULL);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}


char *
vaccination_monitor_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
vaccination_monitor_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file vaccination-monitor.c */
