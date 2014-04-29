/** @file airborne-spread-model.c
 * Module for airborne spread in which the probability of infection falls off
 * exponentially with distance and is relative to the sizes of the infectious
 * and susceptible herds.
 *
 * This module has 2 parameters:
 * <ul>
 *   <li>
 *     The constant (k) related to the infectiousness of the disease being modeled.
 *   <li>
 *     The area at risk of exposure, given as a range (<i>start</i> and <i>end</i>) in
 *     degrees, following the navigation and surveying convention that
 *     \htmlonly N=0/360&deg;, E=90&deg;, S=180&deg;, and W=270&deg;. \endhtmlonly
 *     \latexonly N=0/360$^\circ$, E=90$^\circ$, S=180$^\circ$, and W=270$^\circ$. \endlatexonly
 *
 *     Note that <i>start</i> can be greater than <i>end</i>.  For example, to
 *     get north winds (from the north), you might use <i>start</i>=135,
 *     <i>end</i>=225, but for south winds, you would use <i>start</i>=315,
 *     <i>end</i>=45 (see figure below).
 * </ul>
 *
 * @image html directions.png "Parameters for north winds (left) and south winds (right)"
 * @image latex directions.eps "Parameters for north winds (left) and south winds (right)" width=3in
 *
 * On each day, this module follows these steps:
 * <ol>
 *   <li>
 *     For each pair of herds <i>A</i> and <i>B</i>,
 *     <ol>
 *       <li>
 *         Check whether <i>A</i> or <i>B</i> can be the source of an
 *         infection.  That is, is it Infectious?
 *       <li>
 *         Check whether <i>A</i> or <i>B</i> can be the target of an
 *         infection.  That is, is it Susceptible?
 *       <li>
 *         If <i>A</i> can be the source and <i>B</i> can be the target, or
 *         vice versa, continue; otherwise, go on to the next pair of herds.
 *       <li>
 *         If the heading from the source herd to the target herd is inside the
 *         area at risk of exposure specified in the parameters, continue;
 *         otherwise, go on to the next pair of herds.
 *       <li>
 *         Compute the probability of infection <i>P</i> =
 *         1 - e<sup>(-k((nA
 *         \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 *         prevalence in <i>A</i>)
 *         \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 *         NB)/dAB)</sup>.
 *       <li>
 *         Generate a random number <i>r</i> in [0,1).
 *       <li>
 *         If <i>r</i> < <i>P</i>,
 *         <ol>
 *           <li>
 *             Infect the target herd.
 *             NB: This infection will not be discovered by trace-back
 *             investigations.
 *         </ol>
 *     </ol>
 * </ol>
 * where
 * <ul>
 *   <li>
 *     <i>k</i> is a constant calculated as shown below
 *   <li>
 *     <i>NA</i> is the herd size of infectious farm A
 *   <li>
 *     <i>NB</i> is the herd size of infectious farm B
 *   <li>
 *     <i>dAB</i> is the distance between farms A and B
 * </ul>
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @author Anthony "Drew" Schwickerath <Drew.Schwickerath@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   College of Veterinary Medicine and Biomedical Sciences<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80523<br>
 *   USA
 * @version 0.2
 * @date May 2009
 *
 * Copyright &copy; University of Guelph, 2003-2008
 * Copyright &copy; Colorado State University, 2009
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
#define is_singleton airborne_spread_model_is_singleton
#define new airborne_spread_model_new
#define set_params airborne_spread_model_set_params
#define run airborne_spread_model_run
#define reset airborne_spread_model_reset
#define events_listened_for airborne_spread_model_events_listened_for
#define is_listening_for airborne_spread_model_is_listening_for
#define has_pending_actions airborne_spread_model_has_pending_actions
#define to_string airborne_spread_model_to_string
#define local_printf airborne_spread_model_printf
#define local_fprintf airborne_spread_model_fprintf
#define local_free airborne_spread_model_free
#define handle_before_any_simulations_event airborne_spread_model_handle_before_any_simulations_event
#define handle_new_day_event airborne_spread_model_handle_new_day_event
#define check_and_infect airborne_spread_model_check_and_infect

#include "model.h"
#include "model_util.h"
#include "gis.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#include "spatial_search.h"

#include "airborne-spread-model.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

double round (double x);

extern const char *HRD_status_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "airborne-spread-model"



#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_BeforeAnySimulations, EVT_NewDay };



#define EPSILON 0.01 /* 0.01 km = 10 m */



/* Specialized information for this model. */
typedef struct
{
  double k;
  double wind_dir_start, wind_dir_end;
  gboolean wind_range_crosses_0;
  double max_spread;
  gboolean use_herd_size_adjustments;
}
param_block_t;



typedef struct
{
  GPtrArray *production_types; /**< Each item in the list is a char *. */
  param_block_t ***param_block;
  double *max_spread;
  unsigned int *max_herd_size_by_prod_type;
}
local_data_t;


/**
 * Before any simulations, this module declares all the causes which it may
 * state for an exposure.
 *
 * @param queue for any new events the module creates.
 */
void
handle_before_any_simulations_event (EVT_event_queue_t * queue)
{
  GPtrArray *causes;

#if DEBUG
  g_debug ("----- ENTER handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif

  causes = g_ptr_array_sized_new (1);
  g_ptr_array_add (causes, (gpointer) NAADSM_contact_type_abbrev[NAADSM_AirborneSpread]);
  EVT_event_enqueue (queue, EVT_new_declaration_of_exposure_causes_event (causes));

  /* Note that we don't clean up the GPtrArray.  It will be freed along with
   * the declaration event after all interested sub-models have processed the
   * event. */

#if DEBUG
  g_debug ("----- EXIT handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Computes the maximum herd size for each production type.
 *
 * @param herds a list of herds.
 * @return an array containing the maximum herd size for each production type.
 */
static unsigned int *
build_max_herd_size_by_prod_type (HRD_herd_list_t * herds)
{
  HRD_herd_t *herd;
  unsigned int nherds;          /* number of herds */
  unsigned int nprod_types;
  unsigned int *max_herd_size_by_prod_type = NULL;
  unsigned int i;               /* loop counter */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER build_max_herd_size_by_prod_type");
#endif

  nherds = HRD_herd_list_length (herds);
  nprod_types = herds->production_type_names->len;
  max_herd_size_by_prod_type = g_new0 (unsigned int, nprod_types);

  /* Find the largest herd for each production type. */
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      if (herd->size > max_herd_size_by_prod_type[herd->production_type])
        max_herd_size_by_prod_type[herd->production_type] = herd->size;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT build_max_herd_size_by_prod_type");
#endif

  return max_herd_size_by_prod_type;
}



/**
 * Special structure for use with the callback function below.
 */
typedef struct
{
  local_data_t *local_data;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd1;
  int day;
  RAN_gen_t *rng;
  EVT_event_queue_t *queue;
} callback_t;



/**
 * Check whether herd 1 can infect herd 2 and if so, attempt to infect herd 2.
 */
void
check_and_infect (int id, gpointer arg)
{
  callback_t *callback_data;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd1, *herd2;
  local_data_t *local_data;
  gboolean herd2_can_be_target;
  param_block_t *param_block;
  double distance, heading;
  int day;
  EVT_event_t *exposure;
  RAN_gen_t *rng;
  double r, P;
  gboolean exposure_is_adequate;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER check_and_infect (%s)", MODEL_NAME);
#endif

  callback_data = (callback_t *) arg;
  herds = callback_data->herds;
  herd1 = callback_data->herd1;
  herd2 = HRD_herd_list_get (herds, id);

  /* Are herd 1 and herd 2 the same? */
  if (herd1 == herd2)
    goto end;

  local_data = (callback_data->local_data);

  /* Can herd 2 be the target of an exposure? */
#if DEBUG
  s = g_string_new (NULL);
  g_string_sprintf (s, "herd %s is %s, state is %s: ",
                    herd2->official_id, herd2->production_type_name,
                    HRD_status_name[herd2->status]);
#endif
  param_block = local_data->param_block[herd1->production_type][herd2->production_type];
  herd2_can_be_target = (
    param_block != NULL 
    && herd2->status != Destroyed
    && herd2->status != DeadFromDisease
  );
#if DEBUG
  g_string_sprintfa (s, "%s be target", herd2_can_be_target ? "can" : "cannot");
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif
  if (!herd2_can_be_target)
    goto end;

  /* Is herd 2 within the area at risk of exposure? */
  heading = GIS_heading (herd1->x, herd1->y, herd2->x, herd2->y);
  if (param_block->wind_range_crosses_0
      ? (param_block->wind_dir_start - heading > EPSILON
         && heading - param_block->wind_dir_end >=
         EPSILON) : (param_block->wind_dir_start - heading > EPSILON
                     || heading - param_block->wind_dir_end >= EPSILON))
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "  herd \"%s\" outside wind angles (%g)", herd2->official_id, heading);
#endif
      goto end;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "  herd \"%s\" within wind angles (%g)", herd2->official_id, heading);
#endif

  distance = GIS_distance (herd1->x, herd1->y, herd2->x, herd2->y);

#if DEBUG
  if( TRUE == param_block->use_herd_size_adjustments ) {
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
           "  P = 1.0 - exp( -1.0 * %g * ( ( %d * %g ) * %d ) / %g )",
           param_block->k, herd1->size, herd1->prevalence_infectious, herd2->size, distance);
  } else {
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
           "  P = 1.0 - exp( -1.0 * %g / %g )",
           param_block->k, distance);
  }
#endif

  rng = callback_data->rng;
  if (fabs(distance) < EPSILON) /* avoid divide-by-zero */
    P = 1.0;
  else {
    if( TRUE == param_block->use_herd_size_adjustments ) {
      P = 1.0 - exp(-param_block->k * (((herd1->prevalence_infectious * herd1->size) * herd2->size)) / distance);
    } else {
      P = 1.0 - exp(-param_block->k / distance);
    }
  }
  r = RAN_num (rng);

  exposure_is_adequate = (r < P); 

  /* Record (queue) the exposure, whether it was adequate or not
   * (Most event handlers will ignore exposures by airborne spread,
   * which are not traceable). */
  day = callback_data->day;
  exposure = EVT_new_exposure_event (herd1, herd2, day,
                                     NAADSM_contact_type_abbrev[NAADSM_AirborneSpread],
                                     FALSE, exposure_is_adequate);
  exposure->u.exposure.contact_type = NAADSM_AirborneSpread;
                                      
  EVT_event_enqueue (callback_data->queue, exposure);  

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT check_and_infect (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Responds to a new day event by releasing any pending infections and
 * stochastically generating infections.
 *
 * @param self the model.
 * @param herds a list of herds.
 * @param event a new day event.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
handle_new_day_event (struct naadsm_model_t_ *self, HRD_herd_list_t * herds,
                      EVT_new_day_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  HRD_herd_t *herd1;
  unsigned int nherds;          /* number of herds */
  unsigned int herd1_index;
  gboolean herd1_can_be_source;
  callback_t callback_data;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Initialize a data structure used by the callback function. */
  callback_data.local_data = local_data;
  callback_data.herds = herds;
  callback_data.day = event->day;
  callback_data.rng = rng;
  callback_data.queue = queue;

  nherds = HRD_herd_list_length (herds);
  for (herd1_index = 0; herd1_index < nherds; herd1_index++)
    {
      herd1 = HRD_herd_list_get (herds, herd1_index);

      /* Can this herd be the source of an exposure? */
#if DEBUG
      s = g_string_new (NULL);
      g_string_sprintf (s, "herd \"%s\" is %s, state is %s: ",
                        herd1->official_id, herd1->production_type_name,
                        HRD_status_name[herd1->status]);
#endif
      herd1_can_be_source =
        local_data->param_block[herd1->production_type] != NULL
        && (herd1->status == InfectiousSubclinical || herd1->status == InfectiousClinical);
#if DEBUG
      g_string_sprintfa (s, "%s be source", herd1_can_be_source ? "can" : "cannot");
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
      g_string_free (s, TRUE);
#endif
      if (!herd1_can_be_source)
        continue;

      callback_data.herd1 = herd1;
      spatial_search_circle_by_id (herds->spatial_index, herd1_index,
                                   local_data->max_spread[herd1->production_type] * herd1->size + EPSILON,
                                   check_and_infect, &callback_data);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
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
    case EVT_BeforeAnySimulations:
      handle_before_any_simulations_event (queue);
      break;
    case EVT_NewDay:
      handle_new_day_event (self, herds, &(event->u.new_day), rng, queue);
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT run (%s)", MODEL_NAME);
#endif

  return;
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
  local_data_t *local_data;
  unsigned int nprod_types, i, j;
  param_block_t *param_block;
  char *chararray;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_printf (s, "<%s", MODEL_NAME);

  /* Add the parameter block for each to-from combination of production
   * types. */
  nprod_types = local_data->production_types->len;
  for (i = 0; i < nprod_types; i++)
    if (local_data->param_block[i] != NULL)
      for (j = 0; j < nprod_types; j++)
        if (local_data->param_block[i][j] != NULL)
          {
            param_block = local_data->param_block[i][j];
            g_string_append_printf (s, "\n  for %s -> %s",
                                    (char *) g_ptr_array_index (local_data->production_types, i),
                                    (char *) g_ptr_array_index (local_data->production_types, j));
            g_string_append_printf (s, "\n    k=%g", param_block->k);
            g_string_append_printf (s, "\n    wind-direction=(%g,%g)",
                                    param_block->wind_dir_start, param_block->wind_dir_end);
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
local_printf (struct naadsm_model_t_ *self)
{
  return local_fprintf (stdout, self);
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
  unsigned int nprod_types, i, j;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Free each of the parameter blocks. */
  nprod_types = local_data->production_types->len;
  for (i = 0; i < nprod_types; i++)
    if (local_data->param_block[i] != NULL)
      {
        for (j = 0; j < nprod_types; j++)
          if (local_data->param_block[i][j] != NULL)
            {
              g_free (local_data->param_block[i][j]);
            }
        /* Free this row of the 2D array. */
        g_free (local_data->param_block[i]);
      }
  /* Free the array of pointers to rows. */
  g_free (local_data->param_block);

  g_free (local_data->max_spread);
  g_free (local_data->max_herd_size_by_prod_type);

  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT free (%s)", MODEL_NAME);
#endif
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
 * Adds a set of parameters to an airborne spread model.
 */
void
set_params (struct naadsm_model_t_ *self, PAR_parameter_t * params)
{
  local_data_t *local_data;
  gboolean *from_production_type, *to_production_type;
  unsigned int nprod_types, i, j;
  param_block_t *param_block;
  scew_element const *e;
  gboolean success;
  unsigned long herd_size_infectious, herd_size_susceptible;
  double herd_distance;
  double prob;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER set_params (%s)", MODEL_NAME);
#endif

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  local_data = (local_data_t *) (self->model_data);

  /* Find out which to-from production type combinations these parameters apply
   * to. */
  from_production_type =
    naadsm_read_prodtype_attribute (params, "from-production-type", local_data->production_types);
  to_production_type =
    naadsm_read_prodtype_attribute (params, "to-production-type", local_data->production_types);

  nprod_types = local_data->production_types->len;
  for (i = 0; i < nprod_types; i++)
    if (from_production_type[i] == TRUE)
      for (j = 0; j < nprod_types; j++)
        if (to_production_type[j] == TRUE)
          {
            /* If necessary, create a row in the 2D array for this from-
             * production type. */
            if (local_data->param_block[i] == NULL)
              local_data->param_block[i] = g_new0 (param_block_t *, nprod_types);

            /* Create a parameter block for this to-from production type
             * combination, or overwrite the existing one. */
            param_block = local_data->param_block[i][j];
            if (param_block == NULL)
              {
                param_block = g_new (param_block_t, 1);
                local_data->param_block[i][j] = param_block;
#if DEBUG
                g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                       "setting parameters for %s -> %s",
                       (char *) g_ptr_array_index (local_data->production_types, i),
                       (char *) g_ptr_array_index (local_data->production_types, j));
#endif
              }
            else
              {
                g_warning ("overwriting previous parameters for %s -> %s",
                           (char *) g_ptr_array_index (local_data->production_types, i),
                           (char *) g_ptr_array_index (local_data->production_types, j));
              }

            e = scew_element_by_name (params, "herd-size-infectious");
            if( NULL != e ) {
              herd_size_infectious = PAR_get_unitless (e, &success);
              if (success == FALSE)
                {
                  g_warning ("setting infectious herd size to 0");
                  herd_size_infectious = 0;
                }

              e = scew_element_by_name (params, "herd-size-susceptible");
              herd_size_susceptible = PAR_get_unitless (e, &success);
              if (success == FALSE)
                {
                  g_warning ("setting susceptible herd size to 0");
                  herd_size_susceptible = 0;
                }
            } else {
              // In NAADSM 4.1, it is allowed for both of these values to be 0.
              // In this case, the size adjustment factors are not used.
              herd_size_infectious = 0;
              herd_size_susceptible = 0;
            }

            e = scew_element_by_name (params, "herd-distance");
            herd_distance = PAR_get_length (e, &success);
            if (success == FALSE)
              {
                g_warning ("setting herd distance to 0");
                herd_distance = 0;
              }
            /* The herd distance cannot be negative. */
            if (herd_distance < 0)
              {
                g_warning ("herd distance cannot be negative, setting to 0");
                herd_distance = 0;
              }
            /* FIX ME: herd distance cannot be 0.  What should be do in this
             * case? */
             
            e = scew_element_by_name (params, "prob");
            prob = PAR_get_probability (e, &success);
            if (success == FALSE)
              {
                g_warning ("setting probability of spread at 1 km to 0");
                prob = 0.0;
              }
              
            /* Calculate k. */
            if( ( 0 == herd_size_infectious ) && ( 0 == herd_size_susceptible)  ) {
              param_block->k =
                -(log (1.0 - prob) * herd_distance);
              param_block->use_herd_size_adjustments = FALSE;
            } else {
              param_block->k =
                -(log (1.0 - prob) * herd_distance) /
                 ((double) (herd_size_infectious * herd_size_susceptible));
              param_block->use_herd_size_adjustments = TRUE;
            }
            
            e = scew_element_by_name (params, "wind-direction-start");
            param_block->wind_dir_start = PAR_get_angle (e, &success);
            if (success == FALSE)
              {
                g_warning ("setting start of area at risk of exposure to 0");
                param_block->wind_dir_start = 0;
              }
            /* Force the angle into the range [0,360). */
            while (param_block->wind_dir_start < 0)
              param_block->wind_dir_start += 360;
            while (param_block->wind_dir_start >= 360)
              param_block->wind_dir_start -= 360;

            e = scew_element_by_name (params, "wind-direction-end");
            param_block->wind_dir_end = PAR_get_angle (e, &success);
            if (success == FALSE)
              {
                g_warning ("setting end of area at risk of exposure to 360");
                param_block->wind_dir_end = 360;
              }
            /* Force the angle into the range [0,360]. */
            while (param_block->wind_dir_end < 0)
              param_block->wind_dir_end += 360;
            while (param_block->wind_dir_end > 360)
              param_block->wind_dir_end -= 360;

            /* Note that start > end is allowed.  See the header comment. */
            param_block->wind_range_crosses_0 =
              (param_block->wind_dir_start > param_block->wind_dir_end);

            /* Setting both start and end to 0 seems like a sensible way to
             * turn off airborne spread, but headings close to north will
             * "sneak through" this restriction because we allow a little
             * wiggle room for rounding errors.  If the user tries this,
             * instead set k=0. */
            if (param_block->wind_dir_start == 0 && param_block->wind_dir_end == 0)
              param_block->k = 0;

            /* The exponential falloff curve never actually gets to 0, but we
             * set the maximum distance as the distance at which the probability
             * drops to 1 in a million. */
            /* The distance varies porportionally with the size of both the
             * infectious and susceptible herds.  We consider the size of the
             * susceptible herd to be the maximum herd size (worst case).  This
             * max_spread will then need to be multiplied by the infectious
             * herd size to get the actual max_spread distance for a given
             * case. */
            if (param_block->k > 0)
              param_block->max_spread = 
                -param_block->k * local_data->max_herd_size_by_prod_type[j] /
                log (1.0 - 1e-6);
            else
              param_block->max_spread = 0;

            /* Keep track of the maximum distance of spread from each
             * production type. */
            if (param_block->max_spread > local_data->max_spread[i])
              local_data->max_spread[i] = param_block->max_spread;
          }

  g_free (from_production_type);
  g_free (to_production_type);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT set_params (%s)", MODEL_NAME);
#endif

  return;
}



/**
 * Returns a new airborne spread model.
 */
naadsm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, projPJ projection,
     ZON_zone_list_t * zones)
{
  naadsm_model_t *self;
  local_data_t *local_data;
  unsigned int nprod_types, i;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER new (%s)", MODEL_NAME);
#endif

  self = g_new (naadsm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  self->name = MODEL_NAME;
  self->events_listened_for = events_listened_for;
  self->nevents_listened_for = NEVENTS_LISTENED_FOR;
  self->outputs = g_ptr_array_new ();
  self->model_data = local_data;
  self->set_params = set_params;
  self->run = run;
  self->reset = reset;
  self->is_listening_for = is_listening_for;
  self->has_pending_actions = has_pending_actions;
  self->to_string = to_string;
  self->printf = local_printf;
  self->fprintf = local_fprintf;
  self->free = local_free;

  /* local_data->param_block is a 2D array of parameter blocks, each block
   * holding the parameters for one to-from combination of production types.
   * Initially, all row pointers are NULL.  Rows will be created as needed in
   * the init function. */
  local_data->production_types = herds->production_type_names;
  nprod_types = local_data->production_types->len;
  local_data->param_block = g_new0 (param_block_t **, nprod_types);

  /* Compute the maximum herd size for each production type. */
  local_data->max_herd_size_by_prod_type =
    build_max_herd_size_by_prod_type (herds);

  /* Initialize an array to hold the maximum distance of spread from each
   * production type. */
  local_data->max_spread = g_new (double, nprod_types);
  for (i = 0; i < nprod_types; i++)
    local_data->max_spread[i] = 1;

  /* Send the XML subtree to the init function to read the production type
   * combination specific parameters. */
  self->set_params (self, params);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

/* end of file airborne-spread-model.c */
