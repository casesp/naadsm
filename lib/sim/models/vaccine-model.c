/** @file vaccine-model.c
 * Module that encapsulates knowledge about a vaccine.  Specifically,
 * <ul>
 *   <li> how long the vaccine requires to produce immunity
 *   <li> how long the herd remains immune
 * </ul>
 *
 * When this module hears a Vaccination event, it decides how long the vaccine
 * will require to take effect and how long the herd will remain immune by
 * sampling from the distributions given as parameters.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date May 2003
 *
 * Copyright &copy; University of Guelph, 2003-2008
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
#define interface_version vaccine_model_LTX_interface_version
#define new vaccine_model_LTX_new
#define run vaccine_model_LTX_run
#define reset vaccine_model_LTX_reset
#define events_listened_for vaccine_model_LTX_events_listened_for
#define is_listening_for vaccine_model_LTX_is_listening_for
#define has_pending_actions vaccine_model_LTX_has_pending_actions
#define has_pending_infections vaccine_model_LTX_has_pending_infections
#define to_string vaccine_model_LTX_to_string
#define local_printf vaccine_model_LTX_printf
#define local_fprintf vaccine_model_LTX_fprintf
#define local_free vaccine_model_LTX_free
#define handle_request_for_vaccine_delay_event vaccine_model_LTX_handle_request_for_vaccine_delay_event
#define handle_vaccination_event vaccine_model_LTX_handle_vaccination_event
#define events_created vaccine_model_LTX_events_created

#include "model.h"
#include "model_util.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#include "guilib.h"

#include "vaccine-model.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

/* Temporary fix -- "round" and "rint" are in the math library on Red Hat 7.3,
 * but they're #defined so AC_CHECK_FUNCS doesn't find them. */
double round (double x);

/** This must match an element name in the DTD. */
#define MODEL_NAME "vaccine-model"

#define MODEL_DESCRIPTION "\
A module to encapsulate knowledge about a vaccine.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 May 2003\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 1
EVT_event_type_t events_created[] = { EVT_DeclarationOfVaccineDelay };

#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_RequestForVaccineDelay,
  EVT_Vaccination };



extern const char *HRD_status_name[];
extern const char *PDF_dist_type_name[];



#define EPSILON 0.01



/* Specialized information for this model. */
typedef struct
{
  gboolean *production_type;
  GPtrArray *production_types;
  double delay;
  PDF_dist_t *immunity_period;
}
local_data_t;



/**
 * Responds to a request for vaccine delay by declaring the number of days
 * between being vaccinated and becoming immune.  One instance of this module
 * may handle several production types, so one response is issued for each
 * production type that the instance handles.
 *
 * @param self the model.
 * @param queue for any new events the model creates.
 */
void
handle_request_for_vaccine_delay_event (struct ergadm_model_t_ *self,
                                        EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  unsigned int i;
  char * production_type_name;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_request_for_vaccine_delay_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  for (i = 0; i < local_data->production_types->len; i++)
    if (local_data->production_type[i] == TRUE)
      {
        production_type_name = (char *) g_ptr_array_index (local_data->production_types, i);
        EVT_event_enqueue (queue,
                           EVT_new_declaration_of_vaccine_delay_event (i,
                                                                       production_type_name,
                                                                       local_data->delay));
      }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_request_for_vaccine_delay_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Responds to an vaccination event by changing the herd's state from
 * susceptible to vaccine immune.
 *
 * @param self the model.
 * @param event a vaccination event.
 * @param rng a random number generator.
 */
void
handle_vaccination_event (struct ergadm_model_t_ *self,
                          EVT_vaccination_event_t * event, RAN_gen_t * rng)
{
  local_data_t *local_data;
  int delay, immunity_period;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_vaccination_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "override initial state = (%i)", event->override_initial_state);
#endif

  if (event->override_initial_state == VaccineImmune)
    delay = 0;
  else
    delay = local_data->delay;
#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "vaccine will take %hu days to take effect", delay);
#endif

  if (event->override_initial_state == VaccineImmune && event->override_days_left_in_state > 0)
    immunity_period = event->override_days_left_in_state;
  else
    {
      immunity_period = (int) round (PDF_random (local_data->immunity_period, rng));
      if (immunity_period < 0)
        {
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "%s distribution returned %i for immunity period, using 0 instead",
                 PDF_dist_type_name[local_data->immunity_period->type], immunity_period);
#endif
          immunity_period = 0;
        }
    }
#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "vaccine immunity will last %hu days", immunity_period);
#endif

  HRD_vaccinate (event->herd, delay, immunity_period);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_vaccination_event (%s)", MODEL_NAME);
#endif
}



/**
 * Runs this model.
 *
 * Side effects: may change the state of one or more herds in list.
 *
 * @param self the model.
 * @param herds a list of herds.
 * @param zones a list of zones.
 * @param event the event that caused the model to run.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
run (struct ergadm_model_t_ *self, HRD_herd_list_t * herds, ZON_zone_list_t * zones,
     EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  EVT_vaccination_event_t *vaccination_event;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER run (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }
  
  switch (event->type)
    {
    case EVT_RequestForVaccineDelay:
      handle_request_for_vaccine_delay_event (self, queue);
      break;
    case EVT_Vaccination:
      local_data = (local_data_t *) (self->model_data);
      vaccination_event = &(event->u.vaccination);
      if (local_data->production_type[vaccination_event->herd->production_type] == TRUE)
        {
          handle_vaccination_event (self, vaccination_event, rng);
        }
      else
        {
#if INFO
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
                 "herd is %s, sub-model does not apply",
                 vaccination_event->herd->production_type_name);
#endif
          ;
        }
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT run (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }
}



/**
 * Resets this model after a simulation run.
 *
 * @param self the model.
 */
void
reset (struct ergadm_model_t_ *self)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  /* Nothing to do. */

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
  gboolean already_names;
  unsigned int i;
  char *substring, *chararray;
  local_data_t *local_data;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_sprintf (s, "<%s for ", MODEL_NAME);
  already_names = FALSE;
  for (i = 0; i < local_data->production_types->len; i++)
    if (local_data->production_type[i] == TRUE)
      {
        if (already_names)
          g_string_append_printf (s, ",%s",
                                  (char *) g_ptr_array_index (local_data->production_types, i));
        else
          {
            g_string_append_printf (s, "%s",
                                    (char *) g_ptr_array_index (local_data->production_types, i));
            already_names = TRUE;
          }
      }

  g_string_sprintfa (s, "\n  delay=%g\n", local_data->delay);

  substring = PDF_dist_to_string (local_data->immunity_period);
  g_string_sprintfa (s, "  immunity-period=%s>\n", substring);
  free (substring);

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
 * Frees this model.  Does not free the production type names.
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
  g_free (local_data->production_type);
  PDF_free_dist (local_data->immunity_period);
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
 * Returns a new vaccine model.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *m;
  local_data_t *local_data;
  scew_element *e;
  gboolean success;

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
  m->outputs = g_ptr_array_new ();
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

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "setting production types");
#endif
  local_data->production_types = herds->production_type_names;
  local_data->production_type =
    ergadm_read_prodtype_attribute (params, "production-type", herds->production_type_names);

  e = scew_element_by_name (params, "delay");
  if (e != NULL)
    {
      local_data->delay = PAR_get_time (e, &success);
      if (success == FALSE)
        {
          g_warning ("setting vaccine model delay parameter to 0 days");
          local_data->delay = 0;
        }
      /* The delay cannot be negative. */
      if (local_data->delay < 0)
        {
          g_warning ("vaccine model delay parameter cannot be negative, setting to 0 days");
          local_data->delay = 0;
        }
    }
  else
    {
      g_warning ("vaccine model delay parameter missing, setting to 0 days");
      local_data->delay = 0;
    }

  e = scew_element_by_name (params, "immunity-period");
  if (e != NULL)
    {
      local_data->immunity_period = PAR_get_PDF (e);
      /* No part of the immunity period distribution should be negative. */
      if (local_data->immunity_period->has_inf_lower_tail == FALSE
          && PDF_cdf (-EPSILON, local_data->immunity_period) > 0)
        {
          g_warning
            ("vaccine model immunity period distribution should not include negative values");
        }
    }
  else
    {
      local_data->immunity_period = PDF_new_point_dist (1);
      g_warning ("vaccine model immunity period parameter missing, setting to 1 day");
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

char *
vaccine_model_interface_version (void)
{
  return interface_version ();
}

ergadm_model_t *
vaccine_model_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file vaccine-model.c */
