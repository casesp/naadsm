/** @file detection-on-given-day-model.c
 * Module that simulates a farmer or veterinarian detecting signs of disease.
 *
 * This module is identical to detection-model.c except that it delays the
 * first detection until a given day, then causes a detection of a randomly
 * selected diseased unit on that day or as soon after as possible.  After that
 * initial detection, it works just as detection-model.c does.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date December 2004
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
#define interface_version detection_on_given_day_model_LTX_interface_version
#define new detection_on_given_day_model_LTX_new
#define run detection_on_given_day_model_LTX_run
#define reset detection_on_given_day_model_LTX_reset
#define events_listened_for detection_on_given_day_model_LTX_events_listened_for
#define is_listening_for detection_on_given_day_model_LTX_is_listening_for
#define has_pending_actions detection_on_given_day_model_LTX_has_pending_actions
#define has_pending_infections detection_on_given_day_model_LTX_has_pending_infections
#define to_string detection_on_given_day_model_LTX_to_string
#define local_printf detection_on_given_day_model_LTX_printf
#define local_fprintf detection_on_given_day_model_LTX_fprintf
#define local_free detection_on_given_day_model_LTX_free
#define handle_new_day_event detection_on_given_day_model_LTX_handle_new_day_event
#define handle_detection_event detection_on_given_day_model_LTX_handle_detection_event
#define handle_public_announcement_event detection_on_given_day_model_LTX_handle_public_announcement_event
#define events_created detection_on_given_day_model_LTX_events_created

#include "model.h"
#include "model_util.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#include "guilib.h"

#include "detection-on-given-day-model.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

/* Temporary fix -- "round" and "rint" are in the math library on Red Hat 7.3,
 * but they're #defined so AC_CHECK_FUNCS doesn't find them. */
double round (double x);

extern const char *HRD_status_name[];
extern const char *RPT_frequency_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "detection-on-given-day-model"

#define MODEL_DESCRIPTION "\
A module that causes a detection of a diseased unit on a given day.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 December 2004\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 1
EVT_event_type_t events_created[] = { EVT_Detection };

#define NEVENTS_LISTENED_FOR 3
EVT_event_type_t events_listened_for[] = { EVT_NewDay, EVT_Detection, EVT_PublicAnnouncement };



/* Specialized information for this model. */
typedef struct
{
  gboolean *production_type;
  GPtrArray *production_types;
  unsigned int nherds; /**< Number of herds.  Stored here because it is also
    the length of the detected flag array. */
  REL_chart_t *prob_report_vs_days_clinical;
  REL_chart_t *prob_report_vs_days_since_outbreak;
  unsigned short int detection_day;
  gboolean outbreak_known;
  unsigned short int public_announcement_day;
  gboolean *detected;
  gboolean first_done;
}
local_data_t;



/**
 * Responds to a new day event by stochastically generating detections.
 *
 * @param self the model.
 * @param herds a herd list.
 * @param event a new day event.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
handle_new_day_event (struct ergadm_model_t_ *self, HRD_herd_list_t * herds,
                      EVT_new_day_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  unsigned int nherds;
  double prob_report_from_signs, prob_report_from_awareness;
  double P, r;
  unsigned int i;
  GArray *targets = NULL;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* If the given detection day hasn't arrived yet, there's nothing to do. */
  if (event->day < local_data->detection_day && local_data->first_done == FALSE)
    goto end;

  /* Compute the probability that the disease would be noticed, based on
   * community awareness of an outbreak. */
  if (local_data->outbreak_known)
    prob_report_from_awareness =
      REL_chart_lookup (event->day - local_data->public_announcement_day,
                        local_data->prob_report_vs_days_since_outbreak);
  else
    prob_report_from_awareness = 1;

  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);

      /* Check whether the herd is a production type we're interested in and
       * whether it is showing clinical signs of disease.  If not, go on to the
       * next herd. */
      if (herd->status != InfectiousClinical
          || local_data->production_type[herd->production_type] == FALSE)
        continue;

#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "unit \"%s\" is %s, state is %s, %s detected",
             herd->official_id,
             herd->production_type_name,
             HRD_status_name[herd->status], local_data->detected[i] ? "already" : "not");
#endif
      /* Check whether the herd has already been detected.  If so, go on to the
       * next herd. */
      if (local_data->detected[i])
        continue;

      /* Compute the probability that the disease would be noticed, based
       * on clinical signs.  This is multiplied with the probability
       * computed above. */
      prob_report_from_signs =
        REL_chart_lookup (herd->days_in_status, local_data->prob_report_vs_days_clinical);
      P = prob_report_from_signs * prob_report_from_awareness;
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "P = %g * %g",
             prob_report_from_signs, prob_report_from_awareness);
#endif
      r = RAN_num (rng);
      if (r < P)
        {
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "r (%g) < P (%g)", r, P);
#endif
#if INFO
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
                 "unit \"%s\" detected and reported", herd->official_id);
#endif

          EVT_event_enqueue (queue, EVT_new_detection_event (herd, event->day));
          local_data->first_done = TRUE;
        }
      else
        {
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "r (%g) >= P (%g), not detected", r, P);
#endif
          ;
        }
    }                           /* end of loop over herds */

  /* If the day of first detection has arrived and the regular detection
   * process above didn't produce a detection, try to find a herd to detect. */
  if (local_data->first_done == FALSE)
    {
      /* Create a list of herds that are diseased, not yet detected, and of the
       * production type we're interested in. */
      targets = g_array_new (FALSE, FALSE, sizeof (HRD_herd_t *));
      nherds = HRD_herd_list_length (herds);
      for (i = 0; i < nherds; i++)
        {
          herd = HRD_herd_list_get (herds, i);

          /* Check whether the herd is a production type we're interested in and
           * whether it is showing clinical signs of disease.  If not, go on to the
           * next herd. */
          if (herd->status != InfectiousClinical
              || local_data->production_type[herd->production_type] == FALSE)
            continue;

#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "unit \"%s\" is %s, state is %s, %s detected",
                 herd->official_id,
                 herd->production_type_name,
                 HRD_status_name[herd->status], local_data->detected[i] ? "already" : "not");
#endif
          /* Check whether the herd has already been detected.  If so, go on to the
           * next herd. */
          if (local_data->detected[i])
            continue;

          g_array_append_val (targets, herd);
        }                       /* end of loop over herds */

      if (targets->len == 0)
        {
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "no diseased units to detect");
#endif
        }
      else
        {
          i = (int) (RAN_num (rng) * targets->len);
          herd = g_array_index (targets, HRD_herd_t *, i);
#if INFO
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
                 "unit \"%s\" detected and reported", herd->official_id);
#endif

          EVT_event_enqueue (queue, EVT_new_detection_event (herd, event->day));
          local_data->first_done = TRUE;
        }

      /* Clean up. */
      g_array_free (targets, TRUE);
    }

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Records which herds have been detected (by this or other sub-models).
 *
 * @param self the model.
 * @param event a detection event.
 */
void
handle_detection_event (struct ergadm_model_t_ *self, EVT_detection_event_t * event)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_detection_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  local_data->detected[event->herd->index] = TRUE;
  local_data->first_done = TRUE;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_detection_event (%s)", MODEL_NAME);
#endif
}



/**
 * Records the day on which the outbreak is publically announced.  This is
 * important because the probability of detection increases with community
 * awareness of an outbreak.
 *
 * @param self the model.
 * @param event a public announcement event.
 */
void
handle_public_announcement_event (struct ergadm_model_t_ *self,
                                  EVT_public_announcement_event_t * event)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_public_announcement_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  if (local_data->outbreak_known == FALSE)
    {
      local_data->outbreak_known = TRUE;
      local_data->public_announcement_day = event->day;
#if INFO
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
             "community is now aware of outbreak, detection more likely");
#endif
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_public_announcement_event (%s)", MODEL_NAME);
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
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }

  switch (event->type)
    {
    case EVT_NewDay:
      handle_new_day_event (self, herds, &(event->u.new_day), rng, queue);
      break;
    case EVT_Detection:
      handle_detection_event (self, &(event->u.detection));
      break;
    case EVT_PublicAnnouncement:
      handle_public_announcement_event (self, &(event->u.public_announcement));
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
  local_data_t *local_data;
  int i;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  for (i = 0; i < local_data->nherds; i++)
    local_data->detected[i] = FALSE;
  local_data->outbreak_known = FALSE;
  local_data->public_announcement_day = 0;
  local_data->first_done = FALSE;

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
  g_string_printf (s, "<%s for ", MODEL_NAME);
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

  substring = REL_chart_to_string (local_data->prob_report_vs_days_clinical);
  g_string_append_printf (s, "\n  prob-report-vs-days-clinical=%s\n", substring);
  free (substring);

  substring = REL_chart_to_string (local_data->prob_report_vs_days_since_outbreak);
  g_string_append_printf (s, "  prob-report-vs-days-since-outbreak=%s\n", substring);
  free (substring);

  g_string_append_printf (s, "\n  detection-delay=%hu>", local_data->detection_day);

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

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  /* Free the dynamically-allocated parts. */
  local_data = (local_data_t *) (self->model_data);
  g_free (local_data->production_type);
  REL_free_chart (local_data->prob_report_vs_days_clinical);
  REL_free_chart (local_data->prob_report_vs_days_since_outbreak);

  g_free (local_data->detected);
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
 * Returns a new detection model.
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

  e = scew_element_by_name (params, "prob-report-vs-time-clinical");
  g_assert (e != NULL);
  local_data->prob_report_vs_days_clinical = PAR_get_relationship_chart (e);

  e = scew_element_by_name (params, "prob-report-vs-time-since-outbreak");
  g_assert (e != NULL);
  local_data->prob_report_vs_days_since_outbreak = PAR_get_relationship_chart (e);

  e = scew_element_by_name (params, "detection-delay");
  if (e != NULL)
    {
      local_data->detection_day = (int) round (PAR_get_time (e, &success));
      if (success == FALSE)
        {
          g_warning ("%s: will detect first InfectiousClinical unit", MODEL_NAME);
          local_data->detection_day = 0;
        }
    }
  else
    {
      g_warning ("%s: detection delay missing, will detect first InfectiousClinical unit",
                 MODEL_NAME);
      local_data->detection_day = 0;
    }

  local_data->nherds = HRD_herd_list_length (herds);

  /* Initialize the array of detected flags (one per herd) to all FALSE. */
  local_data->detected = g_new0 (gboolean, local_data->nherds);
  /* No outbreak has been announced yet. */
  local_data->outbreak_known = FALSE;
  local_data->public_announcement_day = 0;
  /* No detection by this model yet. */
  local_data->first_done = FALSE;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

char *
detection_on_given_day_model_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
detection_on_given_day_model_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file detection-on-given-day-model.c */
