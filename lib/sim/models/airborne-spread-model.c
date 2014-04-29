/** @file airborne-spread-model.c
 * Module for airborne spread in which the probability of infection falls off
 * linearly with distance.
 *
 * This module has 3 parameters:
 * <ul>
 *   <li>
 *     The probability of infection at 1 km distance.
 *   <li>
 *     The wind direction, given as a range (<i>start</i> and <i>end</i>) in
 *     degrees, following the navigation and surveying convention that
 *     \htmlonly N=0/360&deg;, E=90&deg;, S=180&deg;, and W=270&deg;. \endhtmlonly
 *     \latexonly N=0/360$^\circ$, E=90$^\circ$, S=180$^\circ$, and W=270$^\circ$. \endlatexonly
 *
 *     Note that <i>start</i> can be greater than <i>end</i>.  For example, to
 *     get north winds (from the north), you might use <i>start</i>=135,
 *     <i>end</i>=225, but for south winds, you would use <i>start</i>=315,
 *     <i>end</i>=45 (see figure below).
 *   <li>
 *     The maximum distance of spread, in km.
 * </ul>
 *
 * @image html directions.png "Parameters for north winds (left) and south winds (right)"
 * @image latex directions.eps "Parameters for north winds (left) and south winds (right)" width=3in
 *
 * This module pre-calculates some values for use during the simulation run.
 * It builds a histogram of herd sizes, then for each herd <i>A</i>, it
 * calculates <i>HerdSizeFactor</i>(<i>A</i>) = (area under the histogram from
 * 0 to size of herd <i>A</i>) \htmlonly &times; \endhtmlonly \latexonly
 * $\times$ \endlatexonly 2.
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
 *         If the distance between <i>A</i> and <i>B</i> < the maximum
 *         distance specified in the parameters, continue; otherwise, go on to
 *         the next pair of herds.
 *       <li>
 *         If the heading from the source herd to the target herd is inside the
 *         wind direction range specified in the parameters, continue;
 *         otherwise, go on to the next pair of herds.
 *       <li>
 *         Compute the probability of infection <i>P</i> =
 *         <i>HerdSizeFactor</i>(<i>A</i>)
 *         \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 *         prevalence in <i>A</i>
 *         \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 *         probability of infection at 1 km
 *         \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 *         <i>DistanceFactor</i>(<i>A</i>,<i>B</i>)
 *         \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 *         <i>HerdSizeFactor</i>(<i>B</i>).
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
 *     <i>DistanceFactor</i>(<i>A</i>,<i>B</i>) = (maximum distance of spread -
 *     distance from <i>A</i> to <i>B</i>) / (maximum distance of spread - 1)
 * </ul>
 *
 * @image html airborne.png
 * @image latex airborne.eps "" width=4in
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date March 2003
 *
 * Copyright &copy; University of Guelph, 2003-2007
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
#define interface_version airborne_spread_model_LTX_interface_version
#define is_singleton airborne_spread_model_LTX_is_singleton
#define new airborne_spread_model_LTX_new
#define set_params airborne_spread_model_LTX_set_params
#define run airborne_spread_model_LTX_run
#define reset airborne_spread_model_LTX_reset
#define events_listened_for airborne_spread_model_LTX_events_listened_for
#define is_listening_for airborne_spread_model_LTX_is_listening_for
#define has_pending_actions airborne_spread_model_LTX_has_pending_actions
#define has_pending_infections airborne_spread_model_LTX_has_pending_infections
#define to_string airborne_spread_model_LTX_to_string
#define local_printf airborne_spread_model_LTX_printf
#define local_fprintf airborne_spread_model_LTX_fprintf
#define local_free airborne_spread_model_LTX_free
#define handle_request_for_infection_causes_event airborne_spread_model_LTX_handle_request_for_infection_causes_event
#define handle_new_day_event airborne_spread_model_LTX_handle_new_day_event
#define callback airborne_spread_model_LTX_callback
#define check_and_infect airborne_spread_model_LTX_check_and_infect
#define events_created airborne_spread_model_LTX_events_created

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

#include <rTreeIndex.h>

#include "guilib.h"

#include "airborne-spread-model.h"

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

double round (double x);

extern const char *HRD_status_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "airborne-spread-model"

#define MODEL_DESCRIPTION "\
A module to simulate airborne spread of a disease.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 March 2003\
"

#define MODEL_INTERFACE_VERSION "0.93"

#define NEVENTS_CREATED 2
EVT_event_type_t events_created[] = { EVT_DeclarationOfInfectionCauses, EVT_AttemptToInfect };

#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_RequestForInfectionCauses, EVT_NewDay };



#define EPSILON 0.01



/* Specialized information for this model. */
typedef struct
{
  double prob_spread_1km;
  double wind_dir_start, wind_dir_end;
  gboolean wind_range_crosses_0;
  double max_spread;
  PDF_dist_t *delay;
}
param_block_t;



typedef struct
{
  GPtrArray *production_types; /**< Each item in the list is a char *. */
  param_block_t ***param_block;
  double *max_spread;
  gboolean *use_rtree_index;
  double *herd_size_factor;
  double short_axis_length;
  GPtrArray *pending_infections; /**< An array to store delayed contacts.  Each
    item in the array is a GQueue of Infection and Exposure events.  (Actually
    a singly-linked list would suffice, but the GQueue syntax is much more
    readable than the GSList syntax.)  An index "rotates" through the array, so
    an event that is to happen in 1 day is placed in the GQueue that is 1 place
    ahead of the rotating index, an event that is to happen in 2 days is placed
    in the GQueue that is 2 places ahead of the rotating index, etc. */
  unsigned int npending_infections;
  unsigned int rotating_index; /**< To go with pending_infections. */
}
local_data_t;


/**
 * Responds to a request for infection causes by declaring all the causes which
 * this model may state for an infection.
 *
 * @param self the model.
 * @param queue for any new events the model creates.
 */
void
handle_request_for_infection_causes_event (struct ergadm_model_t_ *self, EVT_event_queue_t * queue)
{
  GPtrArray *causes;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- ENTER handle_request_for_infection_causes_event (%s)", MODEL_NAME);
#endif

  causes = g_ptr_array_sized_new (1);
  g_ptr_array_add (causes, "airborne spread");
  EVT_event_enqueue (queue, EVT_new_declaration_of_infection_causes_event (causes));

  /* Note that we don't clean up the GPtrArray.  It will be freed along with
   * the declaration event after all interested sub-models have processed the
   * event. */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_request_for_infection_causes_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Computes the size factor for each herd.
 *
 * @param herds a list of herds.
 * @return an array containing the size factor for each herd.
 */
double *
build_size_factor_list (HRD_herd_list_t * herds)
{
  HRD_herd_t *herd;
  unsigned int nherds;          /* number of herds */
  unsigned int max_size = 0;    /* size of largest herd */
  gsl_histogram *histogram;
  PDF_dist_t *herd_size_dist;
  double *size_factor;
  unsigned int i;               /* loop counter */
#if DEBUG
  char *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER build_size_factor_list");
#endif

  nherds = HRD_herd_list_length (herds);
  size_factor = g_new (double, nherds);

  /* Find the largest herd. */
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      if (herd->size > max_size)
        max_size = herd->size;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "largest herd = %u", max_size);
#endif

  /* Build a histogram with one bin for each herd size. */
  histogram = gsl_histogram_alloc (max_size);
  g_assert (histogram != NULL);
  gsl_histogram_set_ranges_uniform (histogram, 0.5, (double) max_size + 0.5);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      gsl_histogram_increment (histogram, (double) (herd->size));
    }
  herd_size_dist = PDF_new_histogram_dist (histogram);
  g_assert (herd_size_dist != NULL);

#if DEBUG
  s = PDF_dist_to_string (herd_size_dist);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "herd size distribution =\n%s", s);
  free (s);
#endif

  /* Compute the herd size factors. */
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      size_factor[i] = PDF_cdf (herd->size, herd_size_dist) * 2;
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "herd size factors");
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "herd #%u (size %u) = %g",
             i, herd->size, size_factor[i]);
    }
#endif

  /* The following line also frees the histogram. */
  PDF_free_dist (herd_size_dist);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT build_size_factor_list");
#endif

  return size_factor;
}



/**
 * Check whether herd 1 can infect herd 2 and if so, attempt to infect herd 2.
 * This function is used by both the naive search and the R-tree (spatial
 * index) search.
 */
void
check_and_infect (struct ergadm_model_t_ *self, HRD_herd_list_t * herds,
                  HRD_herd_t * herd1, HRD_herd_t * herd2,
                  EVT_new_day_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  gboolean herd2_can_be_target;
  param_block_t *param_block;
  double max_spread, distance, heading;
  double distance_factor, herd1_size_factor, herd2_size_factor;
  EVT_event_t *attempt_to_infect;
  double r, P;
  int delay;
  int delay_index;
  GQueue *q;
  HRD_expose_t exposure_update;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER check_and_infect (%s)", MODEL_NAME);
#endif

  /* Are herd 1 and herd 2 the same? */
  if (herd1 == herd2)
    goto end;

  local_data = (local_data_t *) (self->model_data);

  /* Can herd 2 be the target of an exposure? */
#if DEBUG
  s = g_string_new (NULL);
  g_string_sprintf (s, "unit \"%s\" is %s, state is %s: ",
                    herd2->official_id, herd2->production_type_name,
                    HRD_status_name[herd2->status]);
#endif
  param_block = local_data->param_block[herd1->production_type][herd2->production_type];
  herd2_can_be_target = (param_block != NULL && herd2->status != Destroyed);
#if DEBUG
  g_string_sprintfa (s, "%s be target", herd2_can_be_target ? "can" : "cannot");
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif
  if (!herd2_can_be_target)
    goto end;

  /* Is herd 2 close enough to herd 1? */
  distance = GIS_local_distance (herd1->lat, herd1->lon, herd2->lat, herd2->lon);
  max_spread = param_block->max_spread;
  if (distance - max_spread > EPSILON)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  unit \"%s\" too far (%g > %g)",
             herd2->official_id, distance, max_spread);
#endif
      goto end;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  unit \"%s\" within range (%g <= %g)",
         herd2->official_id, distance, max_spread);
#endif

  /* Is herd 2 within the wind direction range? */
  heading = GIS_local_heading (herd1->lat, herd1->lon, herd2->lat, herd2->lon);
  if (param_block->wind_range_crosses_0
      ? (param_block->wind_dir_start - heading > EPSILON
         && heading - param_block->wind_dir_end >=
         EPSILON) : (param_block->wind_dir_start - heading > EPSILON
                     || heading - param_block->wind_dir_end >= EPSILON))
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "  unit \"%s\" outside wind angles (%g)", herd2->official_id, heading);
#endif
      goto end;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "  unit \"%s\" within wind angles (%g)", herd2->official_id, heading);
#endif

  distance_factor = (max_spread - distance) / (max_spread - 1);
  herd1_size_factor = local_data->herd_size_factor[herd1->index];
  herd2_size_factor = local_data->herd_size_factor[herd2->index];
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  P = %g * %g * %g * %g * %g",
         herd1_size_factor, herd1->prevalence, distance_factor, param_block->prob_spread_1km,
         herd2_size_factor);
#endif

  P =
    herd1_size_factor * herd1->prevalence * distance_factor * param_block->prob_spread_1km *
    herd2_size_factor;
  r = RAN_num (rng);

  if (r < P && herd2->status == Susceptible)
    {
      if (NULL != guilib_record_exposure)
        {
          exposure_update.dest_index = herd2->index;
          exposure_update.dest_status = herd2->status;
          exposure_update.src_index = herd1->index;
          exposure_update.src_status = herd1->status;
          exposure_update.exposure_method = "A";
          exposure_update.success = -1;
          guilib_record_exposure (exposure_update);
        }

      attempt_to_infect =
        EVT_new_attempt_to_infect_event (herd1, herd2, event->day, "airborne spread");
      delay = (int) round (PDF_random (param_block->delay, rng));
      if (delay <= 0)
        {
          EVT_event_enqueue (queue, attempt_to_infect);
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  r (%g) < P (%g), target herd infected", r, P);
#endif
        }
      else
        {
          attempt_to_infect->u.attempt_to_infect.day = event->day + delay;
          if (delay > local_data->pending_infections->len)
            ergadm_extend_rotating_array (local_data->pending_infections, delay,
                                          local_data->rotating_index);
          delay_index = (local_data->rotating_index + delay) % local_data->pending_infections->len;
          q = (GQueue *) g_ptr_array_index (local_data->pending_infections, delay_index);
          g_queue_push_tail (q, attempt_to_infect);
          local_data->npending_infections++;
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "  r (%g) < P (%g), target unit will be infected on day %i",
                 r, P, event->day + delay);
#endif
        }
    }
  else
    {
      if (NULL != guilib_record_exposure)
        {
          exposure_update.dest_index = herd2->index;
          exposure_update.dest_status = herd2->status;
          exposure_update.src_index = herd1->index;
          exposure_update.src_status = herd1->status;
          exposure_update.exposure_method = "A";
          exposure_update.success = 0;
          guilib_record_exposure (exposure_update);
        }

#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  r (%g) >= P (%g), target unit not infected", r, P);
#endif
      ;
    }

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT check_and_infect (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Special structure for use with the callback function below.
 */
typedef struct
{
  struct ergadm_model_t_ *self;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd1;
  EVT_new_day_event_t *event;
  RAN_gen_t *rng;
  EVT_event_queue_t *queue;
} callback_t;



int
callback (int id, void *arg)
{
  callback_t *callback_data;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd2;

  callback_data = (callback_t *) arg;
  herds = callback_data->herds;
  /* Because herd indices start at 0, and R-Tree rectangle IDs start at 1. */
  herd2 = HRD_herd_list_get (herds, id - 1);
  check_and_infect (callback_data->self, herds, callback_data->herd1, herd2,
                    callback_data->event, callback_data->rng, callback_data->queue);

  /* A return value of 0 would mean that the spatial search should stop early
   * (before all herds in the search rectangle were visited).  We don't want
   * that, so return 1. */
  return 1;
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
handle_new_day_event (struct ergadm_model_t_ *self, HRD_herd_list_t * herds,
                      EVT_new_day_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  HRD_herd_t *herd1;
  unsigned int nherds;          /* number of herds */
  unsigned int herd1_index, herd2_index;
  gboolean herd1_can_be_source;
  double distance;
  GQueue *q;
  EVT_event_t *pending_event;
  struct Rect search_rect;      /* for narrowing down radius searches using the
                                   R-tree (spatial index) */
  double mult;                  /* to account for latitude */
  callback_t callback_data;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Release any pending (due to airborne transport delays) events. */
  local_data->rotating_index =
    (local_data->rotating_index + 1) % local_data->pending_infections->len;
  q = (GQueue *) g_ptr_array_index (local_data->pending_infections, local_data->rotating_index);
  while (!g_queue_is_empty (q))
    {
      /* Remove the event from this model's internal queue and place it in the
       * simulation's event queue. */
      pending_event = (EVT_event_t *) g_queue_pop_head (q);
      EVT_event_enqueue (queue, pending_event);
      local_data->npending_infections--;
    }

  if (
#if defined(USE_RTREE) && USE_RTREE == 1
       /* For debugging purposes, you can #define USE_RTREE to 0 to never use
        * the spatial index, or 1 to always use it. */
       TRUE ||
#endif
       local_data->use_rtree_index)
    {
      /* Initialize a data structure used by the callback function. */
      callback_data.self = self;
      callback_data.herds = herds;
      callback_data.event = event;
      callback_data.rng = rng;
      callback_data.queue = queue;
    }

  nherds = HRD_herd_list_length (herds);
  for (herd1_index = 0; herd1_index < nherds; herd1_index++)
    {
      herd1 = HRD_herd_list_get (herds, herd1_index);

      /* Can this herd be the source of an exposure? */
#if DEBUG
      s = g_string_new (NULL);
      g_string_sprintf (s, "unit \"%s\" is %s, state is %s: ",
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

      if (local_data->use_rtree_index[herd1->production_type])
        {
          distance = local_data->max_spread[herd1->production_type] / GIS_DEGREE_DISTANCE;
          mult = 1.0 / MIN (cos (DEG2RAD * (herd1->lat + distance)), cos(DEG2RAD * (herd1->lat - distance))); 
          search_rect.boundary[0] = herd1->lon - (distance * mult) - EPSILON;
          search_rect.boundary[1] = herd1->lat - distance - EPSILON;
          search_rect.boundary[2] = herd1->lon + (distance * mult) + EPSILON;
          search_rect.boundary[3] = herd1->lat + distance + EPSILON;
          callback_data.herd1 = herd1;
          RTreeSearch (herds->spatial_index, &search_rect, callback, &callback_data);
        }
      else
        for (herd2_index = 0; herd2_index < nherds; herd2_index++)
          check_and_infect (self, herds, herd1,
                            HRD_herd_list_get (herds, herd2_index), event, rng, queue);
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
    case EVT_RequestForInfectionCauses:
      handle_request_for_infection_causes_event (self, queue);
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

  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }
  
  return;
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
  unsigned int i;
  GQueue *q;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  for (i = 0; i < local_data->pending_infections->len; i++)
    {
      q = (GQueue *) g_ptr_array_index (local_data->pending_infections, i);
      while (!g_queue_is_empty (q))
        EVT_free_event (g_queue_pop_head (q));
    }
  local_data->npending_infections = 0;
  local_data->rotating_index = 0;

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
  local_data_t *local_data;

  local_data = (local_data_t *) (self->model_data);
  return (local_data->npending_infections > 0);
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
  local_data_t *local_data;

  local_data = (local_data_t *) (self->model_data);
  return (local_data->npending_infections > 0);
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
  local_data_t *local_data;
  unsigned int nprod_types, i, j;
  param_block_t *param_block;
  char *substring, *chararray;

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
            g_string_append_printf (s, "\n    prob-spread-1km=%g", param_block->prob_spread_1km);
            g_string_append_printf (s, "\n    wind-direction=(%g,%g)",
                                    param_block->wind_dir_start, param_block->wind_dir_end);
            g_string_append_printf (s, "\n    max-spread=%g", param_block->max_spread);

            substring = PDF_dist_to_string (param_block->delay);
            g_string_append_printf (s, "\n    delay=%s", substring);
            free (substring);
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
  unsigned int nprod_types, i, j;
  GQueue *q;
  param_block_t *param_block;

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
              param_block = local_data->param_block[i][j];
              /* Free the dynamically-allocated parts of the parameter block. */
              PDF_free_dist (param_block->delay);
              /* Free the parameter block itself. */
              g_free (param_block);
            }
        /* Free this row of the 2D array. */
        g_free (local_data->param_block[i]);
      }
  /* Free the array of pointers to rows. */
  g_free (local_data->param_block);

  g_free (local_data->max_spread);
  g_free (local_data->use_rtree_index);
  g_free (local_data->herd_size_factor);

  for (i = 0; i < local_data->pending_infections->len; i++)
    {
      q = (GQueue *) g_ptr_array_index (local_data->pending_infections, i);
      while (!g_queue_is_empty (q))
        EVT_free_event (g_queue_pop_head (q));
      g_queue_free (q);
    }
  g_ptr_array_free (local_data->pending_infections, TRUE);

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
 * Adds a set of parameters to an airborne spread model.
 */
void
set_params (struct ergadm_model_t_ *self, scew_element * params)
{
  local_data_t *local_data;
  gboolean *from_production_type, *to_production_type;
  unsigned int nprod_types, i, j;
  param_block_t *param_block;
  scew_element const *e;
  gboolean success;
  gboolean use_rtree;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER set_params (%s)", MODEL_NAME);
#endif

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  local_data = (local_data_t *) (self->model_data);

  /* Find out which to-from production type combinations these parameters apply
   * to. */
  from_production_type =
    ergadm_read_prodtype_attribute (params, "from-production-type", local_data->production_types);
  to_production_type =
    ergadm_read_prodtype_attribute (params, "to-production-type", local_data->production_types);

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

            e = scew_element_by_name (params, "prob-spread-1km");
            param_block->prob_spread_1km = PAR_get_probability (e, &success);
            if (success == FALSE)
              {
                g_warning ("setting probability of spread at 1 km to 0");
                param_block->prob_spread_1km = 0;
              }

            e = scew_element_by_name (params, "wind-direction-start");
            param_block->wind_dir_start = PAR_get_angle (e, &success);
            if (success == FALSE)
              {
                g_warning ("setting start of wind direction range to 0");
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
                g_warning ("setting end of wind direction range to 360");
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
             * instead set prob-spread-1km=0. */
            if (param_block->wind_dir_start == 0 && param_block->wind_dir_end == 0)
              param_block->prob_spread_1km = 0;

            e = scew_element_by_name (params, "max-spread");
            param_block->max_spread = PAR_get_length (e, &success);
            if (success == FALSE)
              {
                g_warning ("setting maximum distance of spread to 0");
                param_block->max_spread = 0;
              }
            /* The maximum spread distance cannot be negative. */
            if (param_block->max_spread < 0)
              {
                g_warning ("maximum distance of spread cannot be negative, setting to 0");
                param_block->max_spread = 0;
              }
            /* Setting the maximum spread distance to 0 seems like a sensible
             * way to turn off airborne spread, but max-spread <= 1 doesn't
             * make sense in the formula.  If the user tries this, instead set
             * max-spread=2, prob-spread-1km=0. */
            if (param_block->max_spread <= 1)
              {
                param_block->max_spread = 2;
                param_block->prob_spread_1km = 0;
              }

            e = scew_element_by_name (params, "delay");
            if (e != NULL)
              {
                param_block->delay = PAR_get_PDF (e);
              }
            else
              param_block->delay = PDF_new_point_dist (0);

            /* Keep track of the maximum distance of spread from each
             * production type.  This determines whether we will use the R-tree
             * index when looking for herds to spread infection to. */
            if (param_block->max_spread > local_data->max_spread[i])
              {
                local_data->max_spread[i] = param_block->max_spread;

                /* Use the following heuristic to decide whether to use the
                 * R-tree index in searches: if the ratio of the diameter of
                 * circle of maximum spread to the short axis of the oriented
                 * bounding rectangle around the herds is <= 0.25, use the
                 * R-tree. */
                if (local_data->short_axis_length > 0)
                  {
                    use_rtree = (param_block->max_spread * 2 / local_data->short_axis_length) <= 0.25;
                  }
                else
                  {
                    use_rtree = FALSE;
                  }

#if defined(USE_RTREE) && USE_RTREE == 0
                /* For debugging purposes, you can #define USE_RTREE to 0 to never
                 * use the spatial index, or 1 to always use it. */
                use_rtree = FALSE;
#endif
                local_data->use_rtree_index[i] = use_rtree;
              }

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
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *self;
  local_data_t *local_data;
  unsigned int nprod_types, i;

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

  /* local_data->param_block is a 2D array of parameter blocks, each block
   * holding the parameters for one to-from combination of production types.
   * Initially, all row pointers are NULL.  Rows will be created as needed in
   * the init function. */
  local_data->production_types = herds->production_type_names;
  nprod_types = local_data->production_types->len;
  local_data->param_block = g_new0 (param_block_t **, nprod_types);

  /* Compute the herd size factors. */
  local_data->herd_size_factor = build_size_factor_list (herds);

  /* Initialize an array to hold the maximum distance of spread from each
   * production type. */
  local_data->max_spread = g_new (double, nprod_types);
  local_data->use_rtree_index = g_new (gboolean, nprod_types);
  for (i = 0; i < nprod_types; i++)
    {
      local_data->max_spread[i] = 1;
      local_data->use_rtree_index[i] = FALSE;
    }
  local_data->short_axis_length = herds->short_axis_length;

  /* Initialize an array for delayed contacts.  We don't know yet how long the
   * the array needs to be, since that will depend on values we sample from the
   * delay distribution, so we initialize it to length 1. */
  local_data->pending_infections = g_ptr_array_new ();
  g_ptr_array_add (local_data->pending_infections, g_queue_new ());
  local_data->npending_infections = 0;
  local_data->rotating_index = 0;

  /* Send the XML subtree to the init function to read the production type
   * combination specific parameters. */
  self->set_params (self, params);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

char *
airborne_spread_model_interface_version (void)
{
  return interface_version ();
}


ergadm_model_t *
airborne_spread_model_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}


gboolean
airborne_spread_model_is_singleton (void)
{
  return is_singleton ();
}

/* end of file airborne-spread-model.c */
