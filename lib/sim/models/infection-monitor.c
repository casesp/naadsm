/** @file infection-monitor.c
 * Tracks the cause of infections.
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
#define interface_version infection_monitor_LTX_interface_version
#define new infection_monitor_LTX_new
#define run infection_monitor_LTX_run
#define reset infection_monitor_LTX_reset
#define events_listened_for infection_monitor_LTX_events_listened_for
#define is_listening_for infection_monitor_LTX_is_listening_for
#define has_pending_actions infection_monitor_LTX_has_pending_actions
#define has_pending_infections infection_monitor_LTX_has_pending_infections
#define to_string infection_monitor_LTX_to_string
#define local_printf infection_monitor_LTX_printf
#define local_fprintf infection_monitor_LTX_fprintf
#define local_free infection_monitor_LTX_free
#define handle_new_day_event infection_monitor_LTX_handle_new_day_event
#define handle_declaration_of_infection_causes_event infection_monitor_LTX_handle_declaration_of_infection_causes_event
#define handle_infection_event infection_monitor_LTX_handle_infection_event
#define events_created infection_monitor_LTX_events_created

#include "model.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif


#include "infection-monitor.h"

/* 
infection-monitor.c needs access to the functions defined in guilib.h,
even when compiled as a *nix executable (in which case, 
the functions defined will all be NULL). 
*/
#include "guilib.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

double round (double x);

/** This must match an element name in the DTD. */
#define MODEL_NAME "infection-monitor"

#define MODEL_DESCRIPTION "\
A module to track the cause of infections.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 August 2004\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 1
EVT_event_type_t events_created[] = { EVT_RequestForInfectionCauses };

#define NEVENTS_LISTENED_FOR 3
EVT_event_type_t events_listened_for[] =
  { EVT_NewDay, EVT_DeclarationOfInfectionCauses, EVT_Infection };



extern const char *RPT_frequency_name[];



/** Specialized information for this model. */
typedef struct
{
  GPtrArray *production_types;
  RPT_reporting_t *infections;
  RPT_reporting_t *num_units_infected;
  RPT_reporting_t *num_units_infected_by_cause;
  RPT_reporting_t *num_units_infected_by_prodtype;
  RPT_reporting_t *num_units_infected_by_cause_and_prodtype;
  RPT_reporting_t *cumul_num_units_infected;
  RPT_reporting_t *cumul_num_units_infected_by_cause;
  RPT_reporting_t *cumul_num_units_infected_by_prodtype;
  RPT_reporting_t *cumul_num_units_infected_by_cause_and_prodtype;
  RPT_reporting_t *num_animals_infected;
  RPT_reporting_t *num_animals_infected_by_cause;
  RPT_reporting_t *num_animals_infected_by_prodtype;
  RPT_reporting_t *num_animals_infected_by_cause_and_prodtype;
  RPT_reporting_t *cumul_num_animals_infected;
  RPT_reporting_t *cumul_num_animals_infected_by_cause;
  RPT_reporting_t *cumul_num_animals_infected_by_prodtype;
  RPT_reporting_t *cumul_num_animals_infected_by_cause_and_prodtype;
  RPT_reporting_t *ratio;
  unsigned int nrecent_days; /**< The time period over which to compare recent
    infections.  A value of 14 would mean to report the number of new
    infections in the last 2 weeks over the number of new infections in the
    2 weeks before that. */
  unsigned int *nrecent_infections; /**< The number of new infections on each
    day in the recent past.  The length of this array is nrecent_days * 2. */
  unsigned int recent_day_index; /**< The current index into the
    nrecent_infections array.  The index "rotates" through the array, arriving
    back at the beginning and starting to overwrite old values every
    nrecent_days * 2 days. */
  unsigned int numerator, denominator;
  GPtrArray *causes;
  GString *source_and_target;   /* a temporary string used repeatedly. */
}
local_data_t;



/**
 * On the first day of the first simulation, this model requests that any
 * sub-models capable of causing infections declare the causes they may state
 * for the infections.  This is done so that this model can initialize counters
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
  unsigned int current, hi, i, count;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  if (event->day == 1 && local_data->causes == NULL)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "requesting potential causes of infection from other sub-models");
#endif
      local_data->causes = g_ptr_array_new ();
      EVT_event_enqueue (queue, EVT_new_request_for_infection_causes_event ());
    }

  /* Move the pointer for the current day's infection count ahead. */
  hi = local_data->nrecent_days * 2;
  if (event->day == 1)
    {
      current = local_data->recent_day_index;
    }
  else
    {
      current = (local_data->recent_day_index + 1) % hi;
      local_data->recent_day_index = current;
      /* Zero the current day's count. */
      local_data->nrecent_infections[current] = 0;
    }

  /* Compute the denominator of the infection ratio for today. */
  local_data->denominator = 0;
  /* The numbers that sum to the denominator start at 1 past the current point
   * and continue for the period defined by nrecent_days. */
  for (i = (current + 1) % hi, count = 0;
       count < local_data->nrecent_days; i = (i + 1) % hi, count++)
    local_data->denominator += local_data->nrecent_infections[i];

  /* Compute the numerator, minus the current day's infections (which are yet
   * to happen). */
  local_data->numerator = 0;
  for (count = 0; count < local_data->nrecent_days; i = (i + 1) % hi, count++)
    local_data->numerator += local_data->nrecent_infections[i];

#if DEBUG
  s = g_string_new (NULL);
  g_string_append_printf (s, "at beginning of day %hu counts of recent infections = [", event->day);
  for (i = 0; i < hi; i++)
    {
      if (i > 0)
        g_string_append_c (s, ',');
      g_string_append_printf (s, i == current ? "(%u)" : "%u", local_data->nrecent_infections[i]);
    }
  g_string_append_c (s, ']');
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "denominator = sum from %u to %u (inclusive) = %u",
         (current + 1) % hi, (current + local_data->nrecent_days) % hi, local_data->denominator);
#endif

  /* Set the ratio of recent infections to infections before that.  If there
   * are no detections in the current day, this will be the value that will be
   * reported at the end of the day.  Note that the value is undefined until a
   * certain number of days has passed, e.g., if we are reporting new infection
   * this week over new infections last week, the value will be undefined until
   * 2 weeks have passed. */
  if (event->day >= (local_data->nrecent_days * 2) && local_data->denominator > 0)
    RPT_reporting_set_real (local_data->ratio,
                            1.0 * local_data->numerator / local_data->denominator, NULL);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a declaration of infection causes by recording the potential
 * causes of infection.
 *
 * @param self the model.
 * @param event a declaration of infection causes event.
 */
void
handle_declaration_of_infection_causes_event (struct ergadm_model_t_ *self,
                                              EVT_declaration_of_infection_causes_event_t * event)
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
         "----- ENTER handle_declaration_of_infection_causes_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Copy the list of potential causes of infection.  (Note that we just copy
   * the pointers to the C strings, assuming that they are static strings.)  If
   * any potential cause is not already present in our reporting variables, add
   * it, with an initial count of 0 infections. */
  n = event->causes->len;
  for (i = 0; i < n; i++)
    {
      cause = (char *) g_ptr_array_index (event->causes, i);
      g_ptr_array_add (local_data->causes, cause);
      RPT_reporting_add_integer1 (local_data->num_units_infected_by_cause, 0, cause);
      RPT_reporting_add_integer1 (local_data->cumul_num_units_infected_by_cause, 0, cause);
      RPT_reporting_add_integer1 (local_data->num_animals_infected_by_cause, 0, cause);
      RPT_reporting_add_integer1 (local_data->cumul_num_animals_infected_by_cause, 0, cause);

      drill_down_list[0] = cause;
      for (j = 0; j < local_data->production_types->len; j++)
        {
          drill_down_list[1] = (char *) g_ptr_array_index (local_data->production_types, j);
          RPT_reporting_add_integer (local_data->num_units_infected_by_cause_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_num_units_infected_by_cause_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->num_animals_infected_by_cause_and_prodtype, 0,
                                     drill_down_list);
          RPT_reporting_add_integer (local_data->cumul_num_animals_infected_by_cause_and_prodtype, 0,
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
         "----- EXIT handle_declaration_of_infection_causes_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to an infection event by recording it.
 *
 * @param self the model.
 * @param event an infection event.
 */
void
handle_infection_event (struct ergadm_model_t_ *self, EVT_infection_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *infecting_herd, *infected_herd;
  char *peek;
  gboolean first_of_cause;
  char *drill_down_list[3] = { NULL, NULL, NULL };
  unsigned int count;
  HRD_update_t update;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_infection_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  infecting_herd = event->infecting_herd;
  infected_herd = event->infected_herd;

  /* Update the text string that lists infected herd indices. */
  peek = RPT_reporting_get_text1 (local_data->infections, event->cause);
  first_of_cause = (peek == NULL) || (strlen (peek) == 0);

  if (infecting_herd == NULL)
    g_string_printf (local_data->source_and_target,
                     first_of_cause ? "%u" : ",%u", infected_herd->index);
  else
    g_string_printf (local_data->source_and_target,
                     first_of_cause ? "%u->%u" : ",%u->%u",
                     infecting_herd->index, infected_herd->index);

  RPT_reporting_append_text1 (local_data->infections, local_data->source_and_target->str,
                              event->cause);

  if (NULL != guilib_infect_herd)
    {
      update.index = infected_herd->index;
      update.success = 2;       /* Unused */
      update.msg = event->cause;
      guilib_infect_herd (update);
    }

#if UNDEFINED
  printf ("Herd at index %d INFECTED by method %s\n", infected_herd->index, event->cause);
#endif

  /* Update the counts of infections. */
  RPT_reporting_add_integer  (local_data->num_units_infected, 1, NULL);
  RPT_reporting_add_integer1 (local_data->num_units_infected_by_cause, 1, event->cause);
  RPT_reporting_add_integer1 (local_data->num_units_infected_by_prodtype, 1, infected_herd->production_type_name);
  RPT_reporting_add_integer  (local_data->num_animals_infected, infected_herd->size, NULL);
  RPT_reporting_add_integer1 (local_data->num_animals_infected_by_cause, infected_herd->size, event->cause);
  RPT_reporting_add_integer1 (local_data->num_animals_infected_by_prodtype, infected_herd->size, infected_herd->production_type_name);
  RPT_reporting_add_integer  (local_data->cumul_num_units_infected, 1, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_num_units_infected_by_cause, 1, event->cause);
  RPT_reporting_add_integer1 (local_data->cumul_num_units_infected_by_prodtype, 1, infected_herd->production_type_name);
  RPT_reporting_add_integer  (local_data->cumul_num_animals_infected, infected_herd->size, NULL);
  RPT_reporting_add_integer1 (local_data->cumul_num_animals_infected_by_cause, infected_herd->size,
                              event->cause);
  RPT_reporting_add_integer1 (local_data->cumul_num_animals_infected_by_prodtype, infected_herd->size,
                              infected_herd->production_type_name);
  drill_down_list[0] = event->cause;
  drill_down_list[1] = infected_herd->production_type_name;
  if (local_data->num_units_infected_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->num_units_infected_by_cause_and_prodtype, 1, drill_down_list);
  if (local_data->num_animals_infected_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->num_animals_infected_by_cause_and_prodtype, infected_herd->size,
                               drill_down_list);
  if (local_data->cumul_num_units_infected_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->cumul_num_units_infected_by_cause_and_prodtype, 1,
                               drill_down_list);
  if (local_data->cumul_num_animals_infected_by_cause_and_prodtype->frequency != RPT_never)
    RPT_reporting_add_integer (local_data->cumul_num_animals_infected_by_cause_and_prodtype,
                               infected_herd->size, drill_down_list);

  /* Update the ratio of recent infections to infections before that.  Note
   * that the value is undefined until a certain number of days has passed,
   * e.g., if we are reporting new infection this week over new infections last
   * week, the value will be undefined until 2 weeks have passed. */
  local_data->nrecent_infections[local_data->recent_day_index]++;
  count = local_data->nrecent_infections[local_data->recent_day_index];
#if DEBUG
  s = g_string_new (NULL);
  g_string_append_printf (s, "recent infection[%u] now = %u", local_data->recent_day_index, count);
#endif
  if (event->day >= local_data->nrecent_days * 2)
    {
      if (local_data->denominator > 0)
        {
          RPT_reporting_set_real (local_data->ratio,
                                  1.0 * (local_data->numerator + count) / local_data->denominator,
                                  NULL);
#if DEBUG
          g_string_append_printf (s, ", ratio %u/%u = %g",
                                  local_data->numerator + count,
                                  local_data->denominator,
                                  RPT_reporting_get_real (local_data->ratio, NULL));
#endif
        }
      else
        {
#if DEBUG
          g_string_append_printf (s, ", denominator=0, ratio remains at old value of %g",
                                  RPT_reporting_get_real (local_data->ratio, NULL));
#endif
          ;
        }
    }
  else
    {
#if DEBUG
      g_string_append_printf (s, ", no ratio before day %u", local_data->nrecent_days * 2);
#endif
      ;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_infection_event (%s)", MODEL_NAME);
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
    case EVT_DeclarationOfInfectionCauses:
      handle_declaration_of_infection_causes_event (self,
                                                    &(event->u.declaration_of_infection_causes));
      break;
    case EVT_Infection:
      handle_infection_event (self, &(event->u.infection));
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
  RPT_reporting_reset (local_data->infections);
  RPT_reporting_reset (local_data->num_units_infected);
  RPT_reporting_reset (local_data->num_units_infected_by_cause);
  RPT_reporting_reset (local_data->num_units_infected_by_prodtype);
  RPT_reporting_reset (local_data->num_units_infected_by_cause_and_prodtype);
  RPT_reporting_reset (local_data->cumul_num_units_infected);
  RPT_reporting_reset (local_data->cumul_num_units_infected_by_cause);
  RPT_reporting_reset (local_data->cumul_num_units_infected_by_prodtype);
  RPT_reporting_reset (local_data->cumul_num_units_infected_by_cause_and_prodtype);
  RPT_reporting_reset (local_data->num_animals_infected);
  RPT_reporting_reset (local_data->num_animals_infected_by_cause);
  RPT_reporting_reset (local_data->num_animals_infected_by_prodtype);
  RPT_reporting_reset (local_data->num_animals_infected_by_cause_and_prodtype);
  RPT_reporting_reset (local_data->cumul_num_animals_infected);
  RPT_reporting_reset (local_data->cumul_num_animals_infected_by_cause);
  RPT_reporting_reset (local_data->cumul_num_animals_infected_by_prodtype);
  RPT_reporting_reset (local_data->cumul_num_animals_infected_by_cause_and_prodtype);
  RPT_reporting_reset (local_data->ratio);

  /* Initialize counts to 0. */

  /* These are the counts broken down by production type. */
  n = local_data->production_types->len;
  for (i = 0; i < n; i++)
    {
      prodtype_name = (char *) g_ptr_array_index (local_data->production_types, i);
      RPT_reporting_add_integer1 (local_data->num_units_infected_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->cumul_num_units_infected_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->num_animals_infected_by_prodtype, 0, prodtype_name);
      RPT_reporting_add_integer1 (local_data->cumul_num_animals_infected_by_prodtype, 0, prodtype_name);
    }

  if (local_data->causes != NULL)
    {
      n = local_data->causes->len;
      for (i = 0; i < n; i++)
        {
          /* These are the counts broken down by cause of infection. */
          cause = (char *) g_ptr_array_index (local_data->causes, i);
          RPT_reporting_add_integer1 (local_data->num_units_infected_by_cause, 0, cause);
          RPT_reporting_add_integer1 (local_data->cumul_num_units_infected_by_cause, 0, cause);
          RPT_reporting_add_integer1 (local_data->num_animals_infected_by_cause, 0, cause);
          RPT_reporting_add_integer1 (local_data->cumul_num_animals_infected_by_cause, 0, cause);

          /* These are the counts broken down by cause and production type. */
          drill_down_list[0] = cause;
          for (j = 0; j < local_data->production_types->len; j++)
            {
              drill_down_list[1] = (char *) g_ptr_array_index (local_data->production_types, j);
              RPT_reporting_add_integer (local_data->num_units_infected_by_cause_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->cumul_num_units_infected_by_cause_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->num_animals_infected_by_cause_and_prodtype, 0,
                                         drill_down_list);
              RPT_reporting_add_integer (local_data->cumul_num_animals_infected_by_cause_and_prodtype, 0,
                                         drill_down_list);
            }
        }
    }

  /* Note that we don't reset the list of possible causes of infection between
   * iterations. */

  for (i = 0; i < local_data->nrecent_days * 2; i++)
    local_data->nrecent_infections[i] = 0;

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
  local_data_t *local_data;
  GString *s;
  char *chararray;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_sprintf (s, "<%s ratio-period=%u>", MODEL_NAME, local_data->nrecent_days);

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
  RPT_free_reporting (local_data->infections, TRUE);
  RPT_free_reporting (local_data->num_units_infected, TRUE);
  RPT_free_reporting (local_data->num_units_infected_by_cause, TRUE);
  RPT_free_reporting (local_data->num_units_infected_by_prodtype, TRUE);
  RPT_free_reporting (local_data->num_units_infected_by_cause_and_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_infected, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_infected_by_cause, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_infected_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_units_infected_by_cause_and_prodtype, TRUE);
  RPT_free_reporting (local_data->num_animals_infected, TRUE);
  RPT_free_reporting (local_data->num_animals_infected_by_cause, TRUE);
  RPT_free_reporting (local_data->num_animals_infected_by_prodtype, TRUE);
  RPT_free_reporting (local_data->num_animals_infected_by_cause_and_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_infected, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_infected_by_cause, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_infected_by_prodtype, TRUE);
  RPT_free_reporting (local_data->cumul_num_animals_infected_by_cause_and_prodtype, TRUE);
  RPT_free_reporting (local_data->ratio, TRUE);

  /* Note that we don't attempt to free the C strings in the infection causes
   * list, because they're assumed to be static strings. */
  if (local_data->causes != NULL)
    g_ptr_array_free (local_data->causes, TRUE);

  g_string_free (local_data->source_and_target, TRUE);
  g_free (local_data->nrecent_infections);

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
 * Returns a new infection monitor.
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
  gboolean success;
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
  m->outputs = g_ptr_array_sized_new (18);
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

  e = scew_element_by_name (params, "ratio-period");
  if (e != NULL)
    {
      local_data->nrecent_days = (int) round (PAR_get_time (e, &success));
      if (success == FALSE)
        {
          g_warning ("%s: setting ratio period to 2 weeks", MODEL_NAME);
          local_data->nrecent_days = 14;
        }
      if (local_data->nrecent_days < 1)
        {
          g_warning ("%s: ratio period cannot be less than 1, setting to 2 weeks", MODEL_NAME);
          local_data->nrecent_days = 14;
        }
    }
  else
    {
      g_warning ("%s: ratio period missing, setting 2 to weeks", MODEL_NAME);
      local_data->nrecent_days = 14;
    }

  local_data->infections = RPT_new_reporting ("infections", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_units_infected =
    RPT_new_reporting ("num-units-infected", NULL, RPT_integer, RPT_never, FALSE);
  local_data->num_units_infected_by_cause =
    RPT_new_reporting ("num-units-infected-by-cause", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_units_infected_by_prodtype =
    RPT_new_reporting ("num-units-infected-by-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->num_units_infected_by_cause_and_prodtype =
    RPT_new_reporting ("num-units-infected-by-cause-and-production-type", NULL, RPT_group, RPT_never, FALSE);
  local_data->cumul_num_units_infected =
    RPT_new_reporting ("cumulative-num-units-infected", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_num_units_infected_by_cause =
    RPT_new_reporting ("cumulative-num-units-infected-by-cause", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_units_infected_by_prodtype =
    RPT_new_reporting ("cumulative-num-units-infected-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_units_infected_by_cause_and_prodtype =
    RPT_new_reporting ("cumulative-num-units-infected-by-cause-and-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->num_animals_infected =
    RPT_new_reporting ("num-animals-infected", NULL, RPT_integer, RPT_never, FALSE);
  local_data->num_animals_infected_by_cause =
    RPT_new_reporting ("num-animals-infected-by-cause", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->num_animals_infected_by_prodtype =
    RPT_new_reporting ("num-animals-infected-by-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->num_animals_infected_by_cause_and_prodtype =
    RPT_new_reporting ("num-animals-infected-by-cause-and-production-type", NULL, RPT_group, RPT_never,
                       FALSE);
  local_data->cumul_num_animals_infected =
    RPT_new_reporting ("cumulative-num-animals-infected", NULL, RPT_integer, RPT_never, TRUE);
  local_data->cumul_num_animals_infected_by_cause =
    RPT_new_reporting ("cumulative-num-animals-infected-by-cause", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_animals_infected_by_prodtype =
    RPT_new_reporting ("cumulative-num-animals-infected-by-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->cumul_num_animals_infected_by_cause_and_prodtype =
    RPT_new_reporting ("cumulative-num-animals-infected-by-cause-and-production-type", NULL, RPT_group,
                       RPT_never, TRUE);
  local_data->ratio = RPT_new_reporting ("ratio", NULL, RPT_real, RPT_never, TRUE);
  g_ptr_array_add (m->outputs, local_data->infections);
  g_ptr_array_add (m->outputs, local_data->num_units_infected);
  g_ptr_array_add (m->outputs, local_data->num_units_infected_by_cause);
  g_ptr_array_add (m->outputs, local_data->num_units_infected_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_units_infected_by_cause_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_infected);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_infected_by_cause);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_infected_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_units_infected_by_cause_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_animals_infected);
  g_ptr_array_add (m->outputs, local_data->num_animals_infected_by_cause);
  g_ptr_array_add (m->outputs, local_data->num_animals_infected_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->num_animals_infected_by_cause_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_infected);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_infected_by_cause);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_infected_by_prodtype);
  g_ptr_array_add (m->outputs, local_data->cumul_num_animals_infected_by_cause_and_prodtype);
  g_ptr_array_add (m->outputs, local_data->ratio);

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

  /* A list to hold possible causes of infection.  Will be initialized to a
   * GPtrArray when other sub-models declare causes of infection. */
  local_data->causes = NULL;

  local_data->production_types = herds->production_type_names;
  local_data->source_and_target = g_string_new (NULL);

  /* A list to store the number of new infections on each day for the recent
   * past. */
  local_data->nrecent_infections = g_new0 (unsigned int, local_data->nrecent_days * 2);
  local_data->recent_day_index = 0;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

char *
infection_monitor_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
infection_monitor_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file infection-monitor.c */
