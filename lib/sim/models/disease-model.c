/** @file disease-model.c
 * Module that encapsulates knowledge about a disease.
 *
 * When a herd is infected, this module changes the herd's state to Latent.  It
 * decides how long the herd will remain latent, infectious without clinical
 * signs, infectious with clinical signs, and immune by sampling from the
 * distributions given as parameters.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date April 2003
 *
 * Copyright &copy; University of Guelph, 2003-2009
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
#define new disease_model_new
#define run disease_model_run
#define reset disease_model_reset
#define events_listened_for disease_model_events_listened_for
#define is_listening_for disease_model_is_listening_for
#define has_pending_actions disease_model_has_pending_actions
#define to_string disease_model_to_string
#define local_printf disease_model_printf
#define local_fprintf disease_model_fprintf
#define local_free disease_model_free
#define handle_exposure_event disease_model_handle_exposure_event

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

#include "disease-model.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

/* Temporary fix -- "round" and "rint" are in the math library on Red Hat 7.3,
 * but they're #defined so AC_CHECK_FUNCS doesn't find them. */
double round (double x);

/** This must match an element name in the DTD. */
#define MODEL_NAME "disease-model"



#define NEVENTS_LISTENED_FOR 1
EVT_event_type_t events_listened_for[] = { EVT_Exposure };



extern const char *PDF_dist_type_name[];



/* Specialized information for this model. */
typedef struct
{
  gboolean *production_type;
  GPtrArray *production_types;
  PDF_dist_t *latent_period;
  PDF_dist_t *infectious_subclinical_period;
  PDF_dist_t *infectious_clinical_period;
  PDF_dist_t *immunity_period;
  double mortality;
  REL_chart_t *prevalence;
  REL_chart_t *prevalence_infectious;
}
local_data_t;



/**
 * Attaches the relevant prevalence chart to each herd structure.
 *
 * @param self the model.
 * @param herds the herd list.
 */
void
attach_prevalence_charts (struct naadsm_model_t_ *self,
			  HRD_herd_list_t *herds)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  unsigned int nherds;
  unsigned int i;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
	 "----- ENTER attach_prevalence_charts (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  
  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      if (local_data->production_type[herd->production_type] == TRUE)
        {
          herd->prevalence_curve = local_data->prevalence;
          herd->prevalence_infectious_curve = local_data->prevalence_infectious;
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
	 "----- EXIT attach_prevalence_charts (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Responds to an adequate exposure event by initiating an infection in the
 * herd.
 *
 * @param self the model.
 * @param event an exposure event.
 * @param rng a random number generator.
 */
void
handle_exposure_event (struct naadsm_model_t_ *self,
                       EVT_exposure_event_t * event, RAN_gen_t * rng)
{
  local_data_t *local_data;
  HRD_herd_t *herd;
  gboolean have_days_in_and_days_left;
  HRD_status_t initial_state;
  gboolean need_latent_period, need_infectious_subclinical_period, need_infectious_clinical_period, need_immunity_period;
  gboolean fatal;
  int latent_period, infectious_subclinical_period, infectious_clinical_period, immunity_period;
  unsigned int day_in_disease_cycle;

#if DEBUG
  g_debug ("----- ENTER handle_exposure_event (%s)", MODEL_NAME);
#endif

  if (!event->adequate)
    goto end;

  local_data = (local_data_t *) (self->model_data);
  herd = event->exposed_herd;
  if (local_data->production_type[herd->production_type] == FALSE)
    goto end;

  latent_period = infectious_subclinical_period = infectious_clinical_period = immunity_period = 0;
  /* Decide which disease stages we need to sample durations for. */
  have_days_in_and_days_left = (event->override_days_in_state > 0)
                               && (event->override_days_left_in_state > 0);
  initial_state = event->override_initial_state;
  switch (initial_state)
    {
    case DeadFromDisease:
      /* The simplest case, no need to sample durations for any of the disease
       * stages. */
      need_latent_period = FALSE;
      need_infectious_subclinical_period = FALSE;
      need_infectious_clinical_period = FALSE;
      fatal = TRUE;
      need_immunity_period = FALSE;
      break;
    case NaturallyImmune:
      /* A Naturally Immune unit does not have prevalence, so there is no need
       * sample durations for the previous disease stages. */
      need_latent_period = FALSE;
      need_infectious_subclinical_period = FALSE;
      need_infectious_clinical_period = FALSE;
      fatal = FALSE;
      need_immunity_period = !have_days_in_and_days_left;
      break;
    case Latent:
      /* If a unit starts out Latent, Infectious Subclinical, or Infectious
       * Clinical, we need the duration of all 3 states, because to calculate
       * prevalence we need to know where we are in the overall disease
       * cycle. */
      need_latent_period = !have_days_in_and_days_left;
      need_infectious_subclinical_period = TRUE;
      need_infectious_clinical_period = TRUE;
      fatal = (RAN_num (rng) < local_data->mortality);
      need_immunity_period = !fatal;
      break;
    case InfectiousSubclinical:
      need_latent_period = TRUE;
      need_infectious_subclinical_period = !have_days_in_and_days_left;
      need_infectious_clinical_period = TRUE;
      fatal = (RAN_num (rng) < local_data->mortality);
      need_immunity_period = !fatal;
      break;
    case InfectiousClinical:
      need_latent_period = TRUE;
      need_infectious_subclinical_period = TRUE;
      need_infectious_clinical_period = !have_days_in_and_days_left;
      fatal = (RAN_num (rng) < local_data->mortality);
      need_immunity_period = !fatal;
      break;
    default:
      g_assert_not_reached();
    }

  /* Sample durations of disease stages as needed. */
  if (need_latent_period)
    latent_period = PDF_random_non_neg_int (local_data->latent_period, rng);
  if (need_infectious_subclinical_period)
    infectious_subclinical_period = PDF_random_non_neg_int (local_data->infectious_subclinical_period, rng);
  if (need_infectious_clinical_period)
    infectious_clinical_period = PDF_random_non_neg_int (local_data->infectious_clinical_period, rng);
  if (need_immunity_period)
    immunity_period = PDF_random_non_neg_int (local_data->immunity_period, rng);

  /* We may need to modify the durations if the override_days_in_state and/or
   * override_days_left_in_state options have been used. */
  switch (initial_state)
    {
    case Latent:
      day_in_disease_cycle = 0;
      if (have_days_in_and_days_left) 
        {
          latent_period = event->override_days_in_state + event->override_days_left_in_state;
          day_in_disease_cycle = event->override_days_in_state;
        }
      else if (event->override_days_in_state > 0)
        {
          /* Override just the days elapsed in the latent period.  If the given
           * value is greater than the sampled length of the latent period,
           * extend the latent period. */
          latent_period = MAX(latent_period, event->override_days_in_state);
          day_in_disease_cycle = event->override_days_in_state;
        }
      else if (event->override_days_left_in_state > 0)
        {
          /* Override just the days left in the latent period.  If the given
           * value is greater than the sampled length of the latent period,
           * extend the latent period. */
          latent_period = MAX(latent_period, event->override_days_left_in_state);
          day_in_disease_cycle = latent_period - event->override_days_left_in_state;
        }
      break;
    case InfectiousSubclinical:
      day_in_disease_cycle = latent_period;
      if (have_days_in_and_days_left)
        {
          infectious_subclinical_period = event->override_days_in_state + event->override_days_left_in_state;
          day_in_disease_cycle += event->override_days_in_state;
        }
      else if (event->override_days_in_state > 0)
        {
          infectious_subclinical_period = MAX(infectious_subclinical_period, event->override_days_in_state);
          day_in_disease_cycle += event->override_days_in_state;
        }
      else if (event->override_days_left_in_state > 0)
        {
          infectious_subclinical_period = MAX(infectious_subclinical_period, event->override_days_left_in_state);
          day_in_disease_cycle += (infectious_subclinical_period - event->override_days_left_in_state);
        }
      break;
    case InfectiousClinical:
      day_in_disease_cycle = latent_period + infectious_subclinical_period;
      if (have_days_in_and_days_left)
        {
          infectious_clinical_period = event->override_days_in_state + event->override_days_left_in_state;
          day_in_disease_cycle += event->override_days_in_state;
        }
      else if (event->override_days_in_state > 0)
        {
          infectious_clinical_period = MAX(infectious_clinical_period, event->override_days_in_state);
          day_in_disease_cycle += event->override_days_in_state;
        }
      else if (event->override_days_left_in_state > 0)
        {
          infectious_clinical_period = MAX(infectious_clinical_period, event->override_days_left_in_state);
          day_in_disease_cycle += (infectious_clinical_period - event->override_days_left_in_state);
        }
      break;
    case NaturallyImmune:
      day_in_disease_cycle = 0;
      if (have_days_in_and_days_left)
        {
          immunity_period = event->override_days_in_state + event->override_days_left_in_state;
          day_in_disease_cycle += event->override_days_in_state;
        }
      else if (event->override_days_in_state > 0)
        {
          immunity_period = MAX(immunity_period, event->override_days_in_state);
          day_in_disease_cycle += event->override_days_in_state;
        }
      else if (event->override_days_left_in_state > 0)
        {
          immunity_period = MAX(immunity_period, event->override_days_left_in_state);
          day_in_disease_cycle += (immunity_period - event->override_days_left_in_state);
        }
      break;
    case DeadFromDisease:
      day_in_disease_cycle = 0;
      if (event->override_days_in_state > 0)
        {
          /* Override the days already dead.  Note that "days left in state" is
           * not applicable to the Dead from Disease state. */
          day_in_disease_cycle += event->override_days_in_state;
        }
      break;
    default:
      g_assert_not_reached();
    }
  
#if DEBUG
  g_debug ("latent period will last %i days", latent_period);
  g_debug ("infectiousness (with no visible signs) will last %i days", infectious_subclinical_period);
  g_debug ("infectiousness (with visible signs) will last %i days", infectious_clinical_period);
  g_debug ("natural immunity will last %i days", immunity_period);
  if (fatal)
    g_debug ("unit will die");
#endif

  HRD_infect (herd, latent_period, infectious_subclinical_period,
              infectious_clinical_period, immunity_period, fatal, day_in_disease_cycle);

end:
#if DEBUG
  g_debug ("----- EXIT handle_infection_event (%s)", MODEL_NAME);
#endif
  return;
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
run (struct naadsm_model_t_ *self, HRD_herd_list_t * herds, ZON_zone_list_t * zones,
     EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER run (%s)", MODEL_NAME);
#endif

  switch (event->type)
    {
    case EVT_Exposure:
      handle_exposure_event (self, &(event->u.exposure), rng);
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
reset (struct naadsm_model_t_ *self)
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
  gboolean already_names;
  unsigned int i;
  char *substring, *chararray;
  local_data_t *local_data;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_append_printf (s, "<%s for ", MODEL_NAME);
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

  substring = PDF_dist_to_string (local_data->latent_period);
  g_string_sprintfa (s, "\n  latent-period=%s\n", substring);
  g_free (substring);

  substring = PDF_dist_to_string (local_data->infectious_subclinical_period);
  g_string_sprintfa (s, "  infectious-subclinical-period=%s\n", substring);
  g_free (substring);

  substring = PDF_dist_to_string (local_data->infectious_clinical_period);
  g_string_sprintfa (s, "  infectious-clinical-period=%s\n", substring);
  g_free (substring);

  substring = PDF_dist_to_string (local_data->immunity_period);
  g_string_sprintfa (s, "  immunity-period=%s\n", substring);
  g_free (substring);

  g_string_sprintfa (s, "  mortality=%g", local_data->mortality);

  if (local_data->prevalence != NULL)
    {
      substring = REL_chart_to_string (local_data->prevalence);
      g_string_append_printf (s, "\n  prevalence=%s", substring);
      g_free (substring);
    }

  if (local_data->prevalence_infectious != NULL)
    {
      substring = REL_chart_to_string (local_data->prevalence_infectious);
      g_string_append_printf (s, "\n  prevalence_infectious=%s", substring);
      g_free (substring);
    }

  g_string_append_c (s, '>');

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

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER fprintf (%s)", MODEL_NAME);
#endif

  s = to_string (self);
  nchars_written = fprintf (stream, "%s", s);
  free (s);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT fprintf (%s)", MODEL_NAME);
#endif

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
  int nchars_written;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER printf (%s)", MODEL_NAME);
#endif

  nchars_written = local_fprintf (stdout, self);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT printf (%s)", MODEL_NAME);
#endif

  return nchars_written;
}



/**
 * Frees this model.  Does not free the production type names.
 *
 * @param self the model.
 */
void
local_free (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  /* Free the dynamically-allocated parts. */
  local_data = (local_data_t *) (self->model_data);
  g_free (local_data->production_type);
  PDF_free_dist (local_data->latent_period);
  PDF_free_dist (local_data->infectious_subclinical_period);
  PDF_free_dist (local_data->infectious_clinical_period);
  PDF_free_dist (local_data->immunity_period);
  if (local_data->prevalence != NULL)
    REL_free_chart (local_data->prevalence);
  if (local_data->prevalence_infectious != NULL)
    REL_free_chart (local_data->prevalence_infectious);
  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT free (%s)", MODEL_NAME);
#endif
}



/**
 * Returns a new disease model.
 */
naadsm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, projPJ projection,
     ZON_zone_list_t * zones)
{
  naadsm_model_t *m;
  local_data_t *local_data;
  scew_element const *e;
  gboolean success;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER new (%s)", MODEL_NAME);
#endif

  m = g_new (naadsm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  m->name = MODEL_NAME;
  m->events_listened_for = events_listened_for;
  m->nevents_listened_for = NEVENTS_LISTENED_FOR;
  m->outputs = g_ptr_array_new ();
  m->model_data = local_data;
  m->run = run;
  m->reset = reset;
  m->is_listening_for = is_listening_for;
  m->has_pending_actions = has_pending_actions;
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
    naadsm_read_prodtype_attribute (params, "production-type", herds->production_type_names);

  e = scew_element_by_name (params, "latent-period");
  g_assert (e != NULL);
  local_data->latent_period = PAR_get_PDF (e);

  e = scew_element_by_name (params, "infectious-subclinical-period");
  g_assert (e != NULL);
  local_data->infectious_subclinical_period = PAR_get_PDF (e);

  e = scew_element_by_name (params, "infectious-clinical-period");
  g_assert (e != NULL);
  local_data->infectious_clinical_period = PAR_get_PDF (e);

  e = scew_element_by_name (params, "immunity-period");
  g_assert (e != NULL);
  local_data->immunity_period = PAR_get_PDF (e);

  e = scew_element_by_name (params, "mortality");
  if (e == NULL)
    local_data->mortality = 0; /* default */
  else
    {
      local_data->mortality = PAR_get_probability (e, &success);
      if (success == FALSE)
	    {
          g_warning ("%s: setting mortality to 0", MODEL_NAME);
          local_data->mortality = 0;
        }
    }

  e = scew_element_by_name (params, "prevalence");
  if (e != NULL)
    {
      local_data->prevalence = PAR_get_relationship_chart (e);
      /* Scale and translate so that the x-values go from 0 to 1. */
      REL_chart_set_domain (local_data->prevalence, 0, 1);
    }
  else
    {
      /* Don't use prevalence, use the old behaviour. */
      local_data->prevalence = NULL;
    }

  e = scew_element_by_name (params, "prevalence-infectious");
  if (e != NULL)
    {
      local_data->prevalence_infectious = PAR_get_relationship_chart (e);
      /* Scale and translate so that the x-values go from 0 to 1. */
      REL_chart_set_domain (local_data->prevalence_infectious, 0, 1);
    }
  else
    {
      /* Don't use prevalence, use the old behaviour. */
      local_data->prevalence_infectious = NULL;
    }

  /* Attach the relevant prevalence chart to each herd structure. */
  attach_prevalence_charts (m, herds);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

/* end of file disease-model.c */
