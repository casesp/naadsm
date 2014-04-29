/** @file conflict-resolver.c
 * A special module, always loaded, that runs at the end of each day and
 * disambiguates the results of (potentially) conflicting requests for changes.
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
#define interface_version conflict_resolver_LTX_interface_version
#define is_singleton conflict_resolver_LTX_is_singleton
#define new conflict_resolver_LTX_new
#define set_params conflict_resolver_LTX_set_params
#define run conflict_resolver_LTX_run
#define reset conflict_resolver_LTX_reset
#define events_listened_for conflict_resolver_LTX_events_listened_for
#define is_listening_for conflict_resolver_LTX_is_listening_for
#define has_pending_actions conflict_resolver_LTX_has_pending_actions
#define has_pending_infections conflict_resolver_LTX_has_pending_infections
#define to_string conflict_resolver_LTX_to_string
#define local_printf conflict_resolver_LTX_printf
#define local_fprintf conflict_resolver_LTX_fprintf
#define local_free conflict_resolver_LTX_free
#define handle_new_day_event conflict_resolver_LTX_handle_new_day_event
#define handle_declaration_of_vaccine_delay_event conflict_resolver_LTX_handle_declaration_of_vaccine_delay_event
#define handle_attempt_to_infect_event conflict_resolver_LTX_handle_attempt_to_infect_event
#define handle_attempt_to_vaccinate_event conflict_resolver_LTX_handle_attempt_to_vaccinate_event
#define handle_attempt_to_destroy_event conflict_resolver_LTX_handle_attempt_to_destroy_event
#define handle_end_of_day_event conflict_resolver_LTX_handle_end_of_day_event
#define EVT_free_event_as_GFunc conflict_resolver_LTX_EVT_free_event_as_GFunc
#define events_created conflict_resolver_LTX_events_created

#include "model.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#include "conflict-resolver.h"

/* Temporary fix -- missing from math header file? */
double trunc (double);

extern const char *HRD_status_name[];

#define MODEL_NAME "conflict-resolver"

#define MODEL_DESCRIPTION "\
A special module that runs at the end of each day.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 January 2005\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 4
EVT_event_type_t events_created[] = { EVT_RequestForVaccineDelay,
  EVT_Infection, EVT_Vaccination, EVT_Destruction
};

#define NEVENTS_LISTENED_FOR 6
EVT_event_type_t events_listened_for[] = { EVT_NewDay,
  EVT_DeclarationOfVaccineDelay, EVT_AttemptToInfect, EVT_AttemptToVaccinate,
  EVT_AttemptToDestroy, EVT_EndOfDay
};



/* Specialized information for this model. */
typedef struct
{
  GSList **attempts_to_infect; /**< One list per herd, for gathering attempts
    to infect. */
  GSList **attempts_to_vaccinate; /**< One list per herd, for gathering
    attempts to vaccinate. */
  GSList **attempts_to_destroy; /**< One list per herd, for gathering attempts
    to destroy. */
  unsigned int nherds;          /* The number of herds.  Stored here because it is also
                                   the length of the attempts arrays. */
  GPtrArray *production_types;
  gboolean *vaccine_0_delay; /**< An array of flags, one for each production
    type.  The flag is TRUE if the delay to vaccine immunity for that
    production type is 0. */
}
local_data_t;



/**
 * Wraps EVT_free_event so that it can be used in GLib calls.
 *
 * @param data a pointer to an EVT_event_t structure, but cast to a gpointer.
 * @param user_data not used, pass NULL.
 */
void
EVT_free_event_as_GFunc (gpointer data, gpointer user_data)
{
  EVT_free_event ((EVT_event_t *) data);
}



/**
 * On the first day of the first simulation, this model requests that instances
 * of the vaccine module declare how long a delay there is between being
 * vaccinated and becoming immune.  This is done so that this module can handle
 * a special case that occurs when the vaccine delay is 0.
 *
 * @param self the model.
 * @param event a new day event.
 * @param queue for any new events the module creates.
 */
void
handle_new_day_event (struct ergadm_model_t_ *self,
                      EVT_new_day_event_t * event, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  unsigned int nprod_types;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  if (local_data->vaccine_0_delay == NULL)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "requesting vaccine delay from vaccine module");
#endif
      nprod_types = local_data->production_types->len;
      local_data->vaccine_0_delay = g_new0 (gboolean, nprod_types);
      EVT_event_enqueue (queue, EVT_new_request_for_vaccine_delay_event ());
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Responds to a declaration of vaccine delay by recording whether the delay is
 * 0 for this production type.
 *
 * @param self the model.
 * @param event a declaration of vaccine delay event.
 */
void
handle_declaration_of_vaccine_delay_event (struct ergadm_model_t_ *self,
                                           EVT_declaration_of_vaccine_delay_event_t *event)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_declaration_of_vaccine_delay_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* We're only interested if the delay is 0. */
  if (event->delay == 0)
    {
      local_data->vaccine_0_delay[event->production_type] = TRUE;
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "production type \"%s\" has 0 vaccine delay",
             event->production_type_name);
#endif
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_declaration_of_vaccine_delay_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Responds to an attempt to infect event by recording it.
 *
 * @param self the model.
 * @param event an attempt to infect event.
 */
void
handle_attempt_to_infect_event (struct ergadm_model_t_ *self, EVT_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_attempt_to_infect_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->u.attempt_to_infect.infected_herd;
  local_data->attempts_to_infect[herd->index] =
    g_slist_prepend (local_data->attempts_to_infect[herd->index], EVT_clone_event (event));

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "now %i attempt(s) to infect unit \"%s\"",
         g_slist_length (local_data->attempts_to_infect[herd->index]), herd->official_id);
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_attempt_to_infect_event (%s)", MODEL_NAME);
#endif

}



/**
 * Responds to an attempt to vaccinate event by recording it.
 *
 * @param self the model.
 * @param event an attempt to vaccinate event.
 */
void
handle_attempt_to_vaccinate_event (struct ergadm_model_t_ *self, EVT_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_attempt_to_vaccinate_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->u.attempt_to_vaccinate.herd;
  local_data->attempts_to_vaccinate[herd->index] =
    g_slist_prepend (local_data->attempts_to_vaccinate[herd->index], EVT_clone_event (event));
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "now %i attempt(s) to vaccinate unit \"%s\"",
         g_slist_length (local_data->attempts_to_vaccinate[herd->index]), herd->official_id);
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_attempt_to_vaccinate_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to an attempt to destroy event by recording it.
 *
 * @param self the model.
 * @param event an attempt to destroy event.
 */
void
handle_attempt_to_destroy_event (struct ergadm_model_t_ *self, EVT_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_attempt_to_destroy_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->u.attempt_to_destroy.herd;
  local_data->attempts_to_destroy[herd->index] =
    g_slist_prepend (local_data->attempts_to_destroy[herd->index], EVT_clone_event (event));
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "now %i attempt(s) to destroy unit \"%s\"",
         g_slist_length (local_data->attempts_to_destroy[herd->index]), herd->official_id);
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_attempt_to_destroy_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to an end of day event by resolving competing requests for changes
 * and making one unambiguous change to the herd.
 *
 * @param self the model.
 * @param herds the list of herds.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
handle_end_of_day_event (struct ergadm_model_t_ *self, HRD_herd_list_t * herds,
                         RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  static EVT_event_type_t orderings[][3] = {
    {EVT_Destruction, EVT_Infection, EVT_Vaccination},
    {EVT_Destruction, EVT_Vaccination, EVT_Infection}
  };
  unsigned int nherds, i, j;
  HRD_herd_t *herd;
  int num_non_empty_lists;
  int ordering_num;
  EVT_event_type_t *ordering;
  EVT_event_type_t request_type;
  GSList *request_list;
  int request_num;
  EVT_event_t *request, *e;
  gboolean vaccinated;
  gboolean destroyed;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_end_of_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  nherds = local_data->nherds;
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);

      /* Look at the lists of infection, vaccination, and destruction change
       * requests.  If more than one list is non-empty, we need to stochastically
       * choose an ordering. */
      num_non_empty_lists = (local_data->attempts_to_infect[i] ? 1 : 0)
                            +(local_data->attempts_to_vaccinate[i] ? 1 : 0)
                            +(local_data->attempts_to_destroy[i] ? 1 : 0);
      if (num_non_empty_lists == 0)
        continue;

      if (num_non_empty_lists > 1)
        {
#if DEBUG
          s = g_string_new (NULL);
          g_string_printf (s, "unit \"%s\" competing changes:", herd->official_id);
          if (local_data->attempts_to_infect[i] != NULL)
            g_string_append_printf (s, " infection");
          if (local_data->attempts_to_vaccinate[i] != NULL)
            g_string_append_printf (s, " vaccination");
          if (local_data->attempts_to_destroy[i] != NULL)
            g_string_append_printf (s, " destruction");
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
          g_string_free (s, TRUE);
#endif
          ordering_num = (int) trunc (RAN_num (rng) * 2);
          ordering = orderings[ordering_num];
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "chose ordering %i stochastically", ordering_num);
#endif
        }
      else
        {
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "unit \"%s\" only 1 change, no conflict", herd->official_id);
#endif
          ordering = orderings[0];
        }

      /* At this point we have chosen an ordering in which the events happen. */
      vaccinated = FALSE;
      destroyed = FALSE;
      for (j = 0; j < 3; j++)
        {
          request_type = ordering[j];
          switch (request_type)
            {
            case EVT_Infection:
              request_list = local_data->attempts_to_infect[i];
              break;
            case EVT_Vaccination:
              request_list = local_data->attempts_to_vaccinate[i];
              break;
            case EVT_Destruction:
              request_list = local_data->attempts_to_destroy[i];
              break;
            default:
              request_list = NULL;
            }
          if (request_list == NULL)
            continue;

          request_num = g_slist_length (request_list);
          /* If there is more than one competing cause of infection, reason for
           * destruction, etc., choose one randomly. */
          if (request_num > 1)
            {
              request_num = (int) trunc (RAN_num (rng) * request_num);
              request = (EVT_event_t *) (g_slist_nth (request_list, request_num)->data);
            }
          else
            {
              request = (EVT_event_t *) (request_list->data);
            }

          if (request->type == EVT_AttemptToInfect
              && !destroyed
              && !herd->in_disease_cycle
              && !(vaccinated
                   && local_data->vaccine_0_delay[herd->production_type] == TRUE)
                                                                /* && !(herd->status == NaturallyImmune) */ )
                                                                /* ZZZ I CHANGED THIS LINE */
            {
              /* The attempt to infect results in an infection. */
              e = EVT_new_infection_event (request->u.attempt_to_infect.infecting_herd,
                                           herd,
                                           request->u.attempt_to_infect.day,
                                           request->u.attempt_to_infect.cause);
              e->u.infection.override_initial_state =
                request->u.attempt_to_infect.override_initial_state;
              e->u.infection.override_days_left_in_state =
                request->u.attempt_to_infect.override_days_left_in_state;
              EVT_event_enqueue (queue, e);
            }
          else if (request->type == EVT_AttemptToVaccinate && !destroyed)
            {
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "conflict resolver vaccinating unit \"%s\" for reason \"%s\"",
                     herd->official_id, request->u.attempt_to_vaccinate.reason);
#endif
              e = EVT_new_vaccination_event (herd,
                                             request->u.attempt_to_vaccinate.day,
                                             request->u.attempt_to_vaccinate.reason);
              e->u.vaccination.override_initial_state =
                request->u.attempt_to_vaccinate.override_initial_state;
              e->u.vaccination.override_days_left_in_state =
                request->u.attempt_to_vaccinate.override_days_left_in_state;
              EVT_event_enqueue (queue, e);
              vaccinated = TRUE;
            }
          else if (request->type == EVT_AttemptToDestroy)
            {
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "conflict resolver destroying unit \"%s\" for reason \"%s\"",
                     herd->official_id, request->u.attempt_to_destroy.reason);
#endif
              HRD_destroy (herd);
              EVT_event_enqueue (queue,
                                 EVT_new_destruction_event (herd,
                                                            request->u.attempt_to_destroy.day,
                                                            request->u.attempt_to_destroy.reason));
              destroyed = TRUE;
            }
        }

      /* Finally, clear the lists of attempts on this herd. */
      request_list = local_data->attempts_to_infect[i];
      if (request_list != NULL)
        {
          g_slist_foreach (request_list, EVT_free_event_as_GFunc, NULL);
          g_slist_free (request_list);
          local_data->attempts_to_infect[i] = NULL;
        }
      request_list = local_data->attempts_to_vaccinate[i];
      if (request_list != NULL)
        {
          g_slist_foreach (request_list, EVT_free_event_as_GFunc, NULL);
          g_slist_free (request_list);
          local_data->attempts_to_vaccinate[i] = NULL;
        }
      request_list = local_data->attempts_to_destroy[i];
      if (request_list != NULL)
        {
          g_slist_foreach (request_list, EVT_free_event_as_GFunc, NULL);
          g_slist_free (request_list);
          local_data->attempts_to_destroy[i] = NULL;
        }

    }                           /* end of loop over herds */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_end_of_day_event (%s)", MODEL_NAME);
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
    case EVT_DeclarationOfVaccineDelay:
      handle_declaration_of_vaccine_delay_event (self, &(event->u.declaration_of_vaccine_delay));
      break;
    case EVT_AttemptToInfect:
      handle_attempt_to_infect_event (self, event);
      break;
    case EVT_AttemptToVaccinate:
      handle_attempt_to_vaccinate_event (self, event);
      break;
    case EVT_AttemptToDestroy:
      handle_attempt_to_destroy_event (self, event);
      break;
    case EVT_EndOfDay:
      handle_end_of_day_event (self, herds, rng, queue);
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
  unsigned int nherds, i;
  GSList *tmp;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Empty the lists of attempts to infect, vaccinate, and destroy. */
  nherds = local_data->nherds;
  for (i = 0; i < nherds; i++)
    {
      tmp = local_data->attempts_to_infect[i];
      if (tmp != NULL)
        {
          g_slist_foreach (tmp, EVT_free_event_as_GFunc, NULL);
          g_slist_free (tmp);
          local_data->attempts_to_infect[i] = NULL;
        }
      tmp = local_data->attempts_to_vaccinate[i];
      if (tmp != NULL)
        {
          g_slist_foreach (tmp, EVT_free_event_as_GFunc, NULL);
          g_slist_free (tmp);
          local_data->attempts_to_vaccinate[i] = NULL;
        }
      tmp = local_data->attempts_to_destroy[i];
      if (tmp != NULL)
        {
          g_slist_foreach (tmp, EVT_free_event_as_GFunc, NULL);
          g_slist_free (tmp);
          local_data->attempts_to_destroy[i] = NULL;
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
 * Frees this model.  Does not free the production type name.
 *
 * @param self the model.
 */
void
local_free (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;
  unsigned int nherds, i;
  GSList *tmp;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  /* Free the dynamically-allocated parts. */
  local_data = (local_data_t *) (self->model_data);

  /* Empty the lists of attempts to infect, vaccinate, and destroy. */
  nherds = local_data->nherds;
  for (i = 0; i < nherds; i++)
    {
      tmp = local_data->attempts_to_infect[i];
      if (tmp != NULL)
        {
          g_slist_foreach (tmp, EVT_free_event_as_GFunc, NULL);
          g_slist_free (tmp);
        }
      tmp = local_data->attempts_to_vaccinate[i];
      if (tmp != NULL)
        {
          g_slist_foreach (tmp, EVT_free_event_as_GFunc, NULL);
          g_slist_free (tmp);
        }
      tmp = local_data->attempts_to_destroy[i];
      if (tmp != NULL)
        {
          g_slist_foreach (tmp, EVT_free_event_as_GFunc, NULL);
          g_slist_free (tmp);
        }
    }
  g_free (local_data->attempts_to_infect);
  g_free (local_data->attempts_to_vaccinate);
  g_free (local_data->attempts_to_destroy);

  /* Free the list of vaccine delay flags. */
  g_free (local_data->vaccine_0_delay);

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
set_params (struct ergadm_model_t_ *self, scew_element * params)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER set_params (%s)", MODEL_NAME);
#endif

  /* Nothing to do. */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT set_params (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Returns a new conflict resolver model.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *self;
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER new (%s)", MODEL_NAME);
#endif

  self = g_new (ergadm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  self->name = MODEL_NAME;
  self->description = MODEL_DESCRIPTION;
  self->events_created = events_created;
  self->nevents_created = NEVENTS_CREATED;
  self->events_listened_for = events_listened_for;
  self->nevents_listened_for = NEVENTS_LISTENED_FOR;
  self->outputs = g_ptr_array_new ();
  self->model_data = local_data;
  self->set_params = set_params;
  self->run = run;
  self->reset = reset;
  self->is_listening_for = is_listening_for;
  self->has_pending_actions = has_pending_actions;
  self->has_pending_infections = has_pending_infections;
  self->to_string = to_string;
  self->printf = local_printf;
  self->fprintf = local_fprintf;
  self->free = local_free;

  local_data->nherds = HRD_herd_list_length (herds);
  local_data->attempts_to_infect = g_new0 (GSList *, local_data->nherds);
  local_data->attempts_to_vaccinate = g_new0 (GSList *, local_data->nherds);
  local_data->attempts_to_destroy = g_new0 (GSList *, local_data->nherds);
  local_data->production_types = herds->production_type_names;
  local_data->vaccine_0_delay = NULL;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

char *
conflict_resolver_interface_version (void)
{
  return interface_version ();
}

ergadm_model_t *
conflict_resolver_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file conflict-resolver.c */
