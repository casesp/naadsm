/** @file detection-model.c
 * Module that simulates a farmer or veterinarian detecting signs of disease or
 * mortality.
 *
 * On each day, this module follows these steps:
 * <ol>
 *   <li>
 *     Look up the probability <i>p</i><sub>1</sub> that a farmer or
 *     veterinarian will detect signs of disease based on the number of days
 *     since a public announcement of an outbreak.
 *   <li>
 *     For each InfectiousClinical or DeadFromDisease unit,
 *     <ol>
 *       <li>
 *         Look up the probability <i>p</i><sub>2</sub> that a farmer or
 *         veterinarian will detect signs of disease or mortality based on the
 *         number of days the unit has been in its current state.
 *       <li>
 *         If the unit is not inside a zone focus,
 *         <ol>
 *           <li>
 *             Compute the probability of detection <i>P</i> =
 *             <i>p</i><sub>1</sub> &times; <i>p</i><sub>2</sub>.
 *         </ol>
 *       <li>
 *         If the unit is inside a zone focus, <i>p</i><sub>1</sub> is assumed
 *         to be 1 and a multiplier may be applied to <i>p</i><sub>2</sub> to
 *         simulate increased scrutiny.
 *         <ol>
 *           <li>
 *             Compute the probability of detection <i>P</i> =
 *             <i>p</i><sub>2</sub> &times; zone multiplier.
 *         </ol>
 *       <li>
 *         Generate a random number <i>r</i> in [0,1).
 *       <li>
 *         If <i>r</i> < <i>P</i>, report a detection to the authorities.
 *     </ol>
 * </ol>
 *
 * New in version 3.2: This module also listens for requests for a visual
 * inspection.  Such a request might happen when a visit is made to perform
 * tracing, for example.  The request may have a multiplier to simulate
 * increased scrutiny.  The probability of detection <i>P</i> =
 * <i>p</i><sub>2</sub> &times; request multiplier.
 *
 * New in version 4.0: This module can detect a given unit twice: once if the
 * unit turns Infectious Clinical and again if the unit turns Dead from
 * Disease.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date June 2003
 *
 * Copyright &copy; University of Guelph, 2003-2009
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * @todo Reset detected flag when herd goes naturally immune.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when multiple modules have the same interface. */
#define is_singleton detection_model_is_singleton
#define new detection_model_new
#define set_params detection_model_set_params
#define run detection_model_run
#define reset detection_model_reset
#define events_listened_for detection_model_events_listened_for
#define is_listening_for detection_model_is_listening_for
#define has_pending_actions detection_model_has_pending_actions
#define to_string detection_model_to_string
#define local_printf detection_model_printf
#define local_fprintf detection_model_fprintf
#define local_free detection_model_free
#define handle_before_any_simulations_event detection_model_handle_before_any_simulations_event
#define handle_declaration_of_exam_reasons_event detection_model_handle_declaration_of_exam_reasons_event
#define handle_unit_state_change_event detection_model_handle_unit_state_change_event
#define handle_new_day_event detection_model_handle_new_day_event
#define handle_exam_event detection_model_handle_exam_event
#define handle_public_announcement_event detection_model_handle_public_announcement_event
#define handle_detection_event detection_model_detection_event

#include "model.h"
#include "model_util.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#include "detection-model.h"

extern const char *HRD_status_name[];
extern const char *RPT_frequency_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "detection-model"



#define NEVENTS_LISTENED_FOR 7
EVT_event_type_t events_listened_for[] = { EVT_BeforeAnySimulations,
  EVT_DeclarationOfExamReasons, EVT_UnitStateChange, EVT_NewDay,
  EVT_PublicAnnouncement, EVT_Exam, EVT_Detection };



/* Specialized information for this model. */
typedef struct
{
  REL_chart_t *prob_detect_vs_days_clinical;
  REL_chart_t *prob_report_clinical_vs_days_since_outbreak;
  REL_chart_t *prob_detect_vs_days_dead;
  REL_chart_t *prob_report_dead_vs_days_since_outbreak;
}
param_block_t;



typedef struct
{
  GPtrArray *production_types; /**< Each item in the list is a char *. */
  ZON_zone_list_t *zones;
  param_block_t **param_block; /**< Blocks of parameters.  Use an expression
    of the form param_block[production_type] to get a pointer to a particular
    param_block. */
  double **zone_multiplier; /**< A 2D array of multipliers.  Use an expression
    of the form zone_multiplier[zone->level-1][production_type] to get a
    particular multiplier. */
  gboolean outbreak_known;
  int public_announcement_day;
  GHashTable *detectable; /**< A table containing potentially detectable units.
    Units that are detectable are in the table with value TRUE.  Units that get
    detected have their value changed to FALSE on the day they are detected,
    and are removed from the table in time for the following day. */
  double *prob_report_clinical_from_awareness; /**< An array with the
    probability of reporting clinical signs, one value per production type.
    The array is initialized in the reset function, and the values are
    re-calculated each day once an outbreak is known. */
  double *prob_report_dead_from_awareness; /**< An array with the probability
    of reporting death from disease, one value per production type.  The array
    is initialized in the reset function, and the values are re-calculated each
    day once an outbreak is known. */
}
local_data_t;



/**
 * Before any simulations, this module declares all the means by which it may
 * create a detection.
 *
 * @param queue for any new events the module creates.
 */
void
handle_before_any_simulations_event (EVT_event_queue_t * queue)
{
  GPtrArray *means;

#if DEBUG
  g_debug ("----- ENTER handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif

  means = g_ptr_array_sized_new (1);
  g_ptr_array_add (means, (gpointer) NAADSM_detection_reason_abbrev[NAADSM_DetectionClinicalSigns]);
  g_ptr_array_add (means, (gpointer) NAADSM_detection_reason_abbrev[NAADSM_DetectionDeadFromDisease]);
  EVT_event_enqueue (queue, EVT_new_declaration_of_detection_means_event (means));

  /* Note that we don't clean up the GPtrArray.  It will be freed along with
   * the declaration event after all interested modules have processed the
   * event. */

#if DEBUG
  g_debug ("----- EXIT handle_before_any_simulations_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Responds to a declaration of exam reasons by giving those same reasons as
 * reasons why *this* module may request a test.
 *
 * @param event a declaration of exam reasons event.
 * @param queue for any new events the module creates.
 */
void
handle_declaration_of_exam_reasons_event (EVT_declaration_of_exam_reasons_event_t * event,
                                          EVT_event_queue_t * queue)
{
  GPtrArray *reasons;
  unsigned int n, i;

#if DEBUG
  g_debug ("----- ENTER handle_declaration_of_exam_reasons_event (%s)", MODEL_NAME);
#endif

  /* Copy the potential reasons for exams. */
  n = event->reasons->len;
  reasons = g_ptr_array_sized_new (n);
  for (i = 0; i < n; i++)
    g_ptr_array_add (reasons, (char *) g_ptr_array_index (event->reasons, i));

  /* Declare those same reasons as potential reasons for tests. */
  EVT_event_enqueue (queue, EVT_new_declaration_of_test_reasons_event (reasons));

  /* Note that we don't clean up the GPtrArray.  It will be freed along with
   * the declaration event after all interested modules have processed the
   * event. */

#if DEBUG
  g_debug ("----- EXIT handle_declaration_of_exam_reasons_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a unit state change event by updating the list of detectable
 * units.
 *
 * @param self this module.
 * @param event a unit state change event.
 */
void
handle_unit_state_change_event (struct naadsm_model_t_ *self,
                                EVT_unit_state_change_event_t * event)
{
  local_data_t *local_data;
  HRD_herd_t *herd;

#if DEBUG
  g_debug ("----- ENTER handle_unit_state_change_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  /* Check if the unit is a production type we handle. */
  if (NULL != local_data->param_block[herd->production_type])
    {
      switch (event->new_state)
        {
        case InfectiousClinical:
        case DeadFromDisease:
          /* The unit has entered a detectable state.  If the unit changed from
           * Infectious Clinical to Dead from Disease, we want to be able to
           * detect it again. */
#if DEBUG
          g_debug ("unit \"%s\" now in a detectable state", herd->official_id);
#endif
          g_hash_table_insert (local_data->detectable, GUINT_TO_POINTER(herd->index), GINT_TO_POINTER(TRUE));
          break;
        case Susceptible:
        case NaturallyImmune:
        case VaccineImmune:
        case Destroyed:
          /* The unit has entered an undetectable state.  Note that we do not
           * have to cover the Latent or Infectious Subclinical states, because
           * a unit must go through Susceptible first. */
#if DEBUG
          g_debug ("unit \"%s\" no longer in a detectable state", herd->official_id);
#endif
          g_hash_table_remove (local_data->detectable, GUINT_TO_POINTER(herd->index));
          break;
        default:
          break;
        }
    }

#if DEBUG
  g_debug ("----- EXIT handle_unit_state_change_event (%s)", MODEL_NAME);
#endif
  return;
}



typedef struct
{
  local_data_t *local_data;
  HRD_herd_list_t *herds;
  ZON_zone_list_t *zones;
  ZON_zone_fragment_t *background_zone;
  EVT_new_day_event_t *event;
  RAN_gen_t *rng;
  EVT_event_queue_t *queue;
  GSList *herds_to_remove;
}
check_and_detect_args_t;



/**
 * @param key index of a herd in the herd list.
 * @param value TRUE (not detected yet) or FALSE (detected already).
 * @param user_data a check_and_detect_args_t structure.
 */   
void
check_and_detect (gpointer key, gpointer value, gpointer user_data)
{
  HRD_herd_t *herd;
  gboolean detectable;
  check_and_detect_args_t *args;
  local_data_t *local_data;
  NAADSM_detection_reason means;
  HSC_scorecard_t *scorecard;
  
  detectable = (gboolean) GPOINTER_TO_INT(value);
  args = (check_and_detect_args_t *) user_data; 

  /* Unpack args to get the same parameters as handle_new_day_event. */
  local_data = args->local_data;
  herd = HRD_herd_list_get (args->herds, GPOINTER_TO_UINT(key));
  scorecard = naadsm_get_or_create_scorecard (herd);

  if (detectable == FALSE)
    {
      /* This unit was detected yesterday so it is no longer detectable. */
#if DEBUG
      g_debug ("unit \"%s\" detected yesterday, no longer detectable", herd->official_id);
#endif
      args->herds_to_remove = g_slist_prepend (args->herds_to_remove, key);
    }   
  else if ( HSC_herd_is_detected_as_dead(scorecard) )
    {
      /* This unit was already detected by another mechanism, so it is no longer detectable. */
#if DEBUG
      g_debug ("unit \"%s\" already detected as dead from disease, no longer detectable", herd->official_id);
#endif
      args->herds_to_remove = g_slist_prepend (args->herds_to_remove, key);
    }
  else  
    {
      ZON_zone_list_t *zones;
      ZON_zone_fragment_t *background_zone, *fragment;
      ZON_zone_t *zone;
      unsigned int prod_type;
      param_block_t *param_block;
      RAN_gen_t *rng;
      double prob_detect, P, r;
      EVT_new_day_event_t *event;
      EVT_event_queue_t *queue;

      /* Unpack args to get the same parameters as handle_new_day_event. */
      zones = args->zones;
      background_zone = args->background_zone;
      event = args->event;
      rng = args->rng;
      queue = args->queue;
      
      /* Find which zone the herd is in. */
      fragment = zones->membership[herd->index];
      zone = fragment->parent;
#if DEBUG
      g_debug ("unit \"%s\" is %s, in zone \"%s\", state is %s",
               herd->official_id,
               herd->production_type_name,
               zone->name,
               HRD_status_name[herd->status]);
#endif

      /* Compute the probability that the disease would be noticed, based
       * on clinical signs or mortality.  This is multiplied with the probability
       * of reporting from awareness. */
#if DEBUG
      g_debug ("using chart value for day %hu in current state", herd->days_in_status);
#endif
      prod_type = herd->production_type;
      param_block = local_data->param_block[prod_type];
      if (herd->status == InfectiousClinical)
        prob_detect =
          REL_chart_lookup (herd->days_in_status, param_block->prob_detect_vs_days_clinical);
      else
        prob_detect =
          REL_chart_lookup (herd->days_in_status, param_block->prob_detect_vs_days_dead);

      if (ZON_same_zone (background_zone, fragment))
        {
          if (herd->status == InfectiousClinical)
            {
              P = prob_detect * local_data->prob_report_clinical_from_awareness[prod_type];
              #if DEBUG
                g_debug ("P = %g * %g", prob_detect, local_data->prob_report_clinical_from_awareness[prod_type]);
              #endif
            }
          else
            {
              P = prob_detect * local_data->prob_report_dead_from_awareness[prod_type];
              #if DEBUG
                g_debug ("P = %g * %g", prob_detect, local_data->prob_report_dead_from_awareness[prod_type]);
              #endif
            }
        }
      else
        {
          P = prob_detect * local_data->zone_multiplier[zone->level - 1][prod_type];
#if DEBUG
          g_debug ("P = %g * %g", prob_detect, local_data->zone_multiplier[zone->level - 1][prod_type]);
#endif
        }
      r = RAN_num (rng);
      if (r < P)
        {
#if DEBUG
          g_debug ("r (%g) < P (%g)", r, P);
          g_debug ("unit \"%s\" detected and reported", herd->official_id);
#endif
          g_hash_table_insert (local_data->detectable, GUINT_TO_POINTER(herd->index), GINT_TO_POINTER(FALSE));
          /* There was no diagnostic test, so NAADSM_TestUnspecified is a legitimate value here. */
          if (herd->status == InfectiousClinical)
            means = NAADSM_DetectionClinicalSigns;
          else
            means = NAADSM_DetectionDeadFromDisease;
          EVT_event_enqueue (queue,
                             EVT_new_detection_event (herd, event->day,
                                                      means,
                                                      NAADSM_detection_reason_abbrev[means],
                                                      NAADSM_TestUnspecified));
        }
      else
        {
#if DEBUG
          g_debug ("r (%g) >= P (%g), not detected", r, P);
#endif
          ;
        }
    }
	
  return;
}



/**
 * Responds to a new day event by stochastically generating detections.
 *
 * @param self the model.
 * @param herds a herd list.
 * @param zones a zone list.
 * @param event a new day event.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
handle_new_day_event (struct naadsm_model_t_ *self, HRD_herd_list_t * herds,
                      ZON_zone_list_t * zones, EVT_new_day_event_t * event,
                      RAN_gen_t * rng, EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  unsigned int lookup_day;
  unsigned int nprod_types, i;
  param_block_t *param_block;
  check_and_detect_args_t check_and_detect_args;
  GSList *iter;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* For each production type, compute the probability that the disease would 
   * be reported, based on community awareness of an outbreak. */
  nprod_types = local_data->production_types->len;
  if (local_data->outbreak_known)
    {
      lookup_day = event->day - local_data->public_announcement_day;
      for (i = 0; i < nprod_types; i++)
        {
          param_block = local_data->param_block[i];

          if (param_block == NULL)
            continue;

          local_data->prob_report_clinical_from_awareness[i] =
            REL_chart_lookup (lookup_day, param_block->prob_report_clinical_vs_days_since_outbreak);
          local_data->prob_report_dead_from_awareness[i] =
            REL_chart_lookup (lookup_day, param_block->prob_report_dead_vs_days_since_outbreak);
        }
    }

  /* Iterate over all the detectable units. */
  check_and_detect_args.local_data = local_data;
  check_and_detect_args.herds = herds;
  check_and_detect_args.zones = zones;
  check_and_detect_args.background_zone = ZON_zone_list_get_background (zones);
  check_and_detect_args.event = event;
  check_and_detect_args.rng = rng;
  check_and_detect_args.queue = queue;
  check_and_detect_args.herds_to_remove = NULL; /* empty GSList */
  g_hash_table_foreach (local_data->detectable, check_and_detect, (gpointer) (&check_and_detect_args));

  for (iter = check_and_detect_args.herds_to_remove; iter != NULL; iter = g_slist_next(iter))
    {
      g_hash_table_remove (local_data->detectable, iter->data);
    }
  g_slist_free (check_and_detect_args.herds_to_remove);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to an exam event by potentially generating a detection.
 *
 * @param self the model.
 * @param herds a herd list.
 * @param event a new day event.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
handle_exam_event (struct naadsm_model_t_ *self, HRD_herd_list_t * herds,
                   EVT_exam_event_t * event, RAN_gen_t * rng,
                   EVT_event_queue_t * queue)
{
  local_data_t *local_data;
  param_block_t *param_block;
  HRD_herd_t *herd;
  unsigned int prod_type;
  double prob_detect;
  double P, r;
  NAADSM_detection_reason means;

#if DEBUG
  g_debug ("----- ENTER handle_exam_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  herd = event->herd;

  /* Check whether the herd is a production type we're interested in.  If not,
   * abort. */
  prod_type = herd->production_type;
  param_block = local_data->param_block[prod_type];
  if (param_block == NULL)
    goto end;

#if DEBUG
  g_debug ("unit \"%s\" is %s, state is %s",
           herd->official_id,
           herd->production_type_name,
           HRD_status_name[herd->status]);
#endif

  /* Check whether the herd is showing clinical signs of disease or mortality. */
  if (g_hash_table_lookup (local_data->detectable, GUINT_TO_POINTER(herd->index)) != NULL)
    {
      /* Compute the probability that the disease would be noticed, based on
       * clinical signs or mortality and the request multiplier. */
      if (herd->status == InfectiousClinical)
        prob_detect =
          REL_chart_lookup (herd->days_in_status, param_block->prob_detect_vs_days_clinical);
      else
        prob_detect =
          REL_chart_lookup (herd->days_in_status, param_block->prob_detect_vs_days_dead);

      P = prob_detect * event->detection_multiplier;
#if DEBUG
      g_debug ("P = %g * %g", prob_detect, event->detection_multiplier);
#endif
      r = RAN_num (rng);
      if (r < P)
        {
#if DEBUG
          g_debug ("r (%g) < P (%g)", r, P);
          g_debug ("unit \"%s\" detected and reported", herd->official_id);
#endif
          g_hash_table_insert (local_data->detectable, GUINT_TO_POINTER(herd->index), GINT_TO_POINTER(FALSE));
          /* There was no diagnostic test, so NAADSM_TestUnspecified is a legitimate value here. */
          if (herd->status == InfectiousClinical)
            means = NAADSM_DetectionClinicalSigns;
          else
            means = NAADSM_DetectionDeadFromDisease;
          EVT_event_enqueue (queue,
                             EVT_new_detection_event (herd, event->day,
                                                      means,
                                                      NAADSM_detection_reason_abbrev[means],
                                                      NAADSM_TestUnspecified));
        }
      else
        {
#if DEBUG
          g_debug ("r (%g) >= P (%g), not detected", r, P);
#endif
          if (event->test_if_no_signs == TRUE)
            EVT_event_enqueue (queue, EVT_new_test_event (herd, event->day, event->reason));
        }
    } /* end of case where herd is detectable */
  else
    {
      if (event->test_if_no_signs == TRUE)
        EVT_event_enqueue (queue, EVT_new_test_event (herd, event->day, event->reason));
    }

end:
#if DEBUG
  g_debug ("----- EXIT handle_exam_event (%s)", MODEL_NAME);
#endif
  return;
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
handle_public_announcement_event (struct naadsm_model_t_ *self,
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
#if DEBUG
      g_debug ("community is now aware of outbreak, detection more likely");
#endif
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "----- EXIT handle_public_announcement_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a Detection event (including ones generated by this module) by
 * updating the herd's scorecard.
 *
 * @param self this module.
 * @param event a detection event.
 */
void
handle_detection_event (struct naadsm_model_t_ *self,
                        EVT_detection_event_t * event)
{
  HRD_herd_t *herd;
  HSC_scorecard_t *scorecard;

#if DEBUG
  g_debug ("----- ENTER handle_detection_event (%s)", MODEL_NAME);
#endif

  herd = event->herd;
  scorecard = naadsm_get_or_create_scorecard (herd);
  if (herd->status == DeadFromDisease)
    HSC_record_detection_as_dead (scorecard, event->day);
  else
    HSC_record_detection_as_diseased (scorecard, event->day);

#if DEBUG
  g_debug ("----- EXIT handle_detection_event (%s)", MODEL_NAME);
#endif
  return;
}



/**
 * Runs this model.
 *
 * Side effects: may change the state of one or more herds in list.
 *
 * @param self the model.
 * @param herds a herd list.
 * @param zones a zone list.
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
    case EVT_DeclarationOfExamReasons:
      handle_declaration_of_exam_reasons_event (&(event->u.declaration_of_exam_reasons), queue);
      break;
    case EVT_UnitStateChange:
      handle_unit_state_change_event (self, &(event->u.unit_state_change));
      break;
    case EVT_NewDay:
      handle_new_day_event (self, herds, zones, &(event->u.new_day), rng, queue);
      break;
    case EVT_Exam:
      handle_exam_event (self, herds, &(event->u.exam), rng, queue);
      break;
    case EVT_PublicAnnouncement:
      handle_public_announcement_event (self, &(event->u.public_announcement));
      break;
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
reset (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;
  int nprod_types, i;
  param_block_t *param_block;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  g_hash_table_remove_all (local_data->detectable);
  local_data->outbreak_known = FALSE;
  local_data->public_announcement_day = 0;

  nprod_types = local_data->production_types->len;
  for (i = 0; i < nprod_types; i++)
    {
      param_block = local_data->param_block[i];
      if (param_block == NULL)
        continue;

      local_data->prob_report_clinical_from_awareness[i] =
        REL_chart_lookup (0, param_block->prob_report_clinical_vs_days_since_outbreak);
      local_data->prob_report_dead_from_awareness[i] =
        REL_chart_lookup (0, param_block->prob_report_dead_vs_days_since_outbreak);
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
  unsigned int nprod_types, nzones, i, j;
  param_block_t *param_block;
  char *substring, *chararray;
  ZON_zone_t *zone;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  g_string_printf (s, "<%s", MODEL_NAME);

  /* Add the parameter block for each production type. */
  nprod_types = local_data->production_types->len;
  nzones = ZON_zone_list_length (local_data->zones);
  for (i = 0; i < nprod_types; i++)
    {
      param_block = local_data->param_block[i];
      if (param_block == NULL)
        continue;

      g_string_append_printf (s, "\n  for %s",
                              (char *) g_ptr_array_index (local_data->production_types, i));

      substring = REL_chart_to_string (param_block->prob_detect_vs_days_clinical);
      g_string_append_printf (s, "\n    prob-detect-vs-days-clinical=%s", substring);
      g_free (substring);

      substring = REL_chart_to_string (param_block->prob_report_clinical_vs_days_since_outbreak);
      g_string_append_printf (s, "\n    prob-report-clinical-signs-vs-days-since-outbreak=%s", substring);
      g_free (substring);

      substring = REL_chart_to_string (param_block->prob_detect_vs_days_dead);
      g_string_append_printf (s, "\n    prob-detect-vs-days-dead=%s", substring);
      g_free (substring);

      substring = REL_chart_to_string (param_block->prob_report_dead_vs_days_since_outbreak);
      g_string_append_printf (s, "\n    prob-report-death-from-disease-vs-days-since-outbreak=%s", substring);
      g_free (substring);

      for (j = 0; j < nzones; j++)
        {
          zone = ZON_zone_list_get (local_data->zones, j);
          g_string_append_printf (s, "\n    prob-multiplier for \"%s\" zone=%g",
                                  zone->name, local_data->zone_multiplier[j][i]);
        }
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
 * Frees this model.  Does not free the production type name.
 *
 * @param self the model.
 */
void
local_free (struct naadsm_model_t_ *self)
{
  local_data_t *local_data;
  unsigned int nprod_types, nzones, i;
  param_block_t *param_block;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Free each of the parameter blocks. */
  nprod_types = local_data->production_types->len;
  for (i = 0; i < nprod_types; i++)
    {
      param_block = local_data->param_block[i];
      if (param_block == NULL)
        continue;

      REL_free_chart (param_block->prob_detect_vs_days_clinical);
      REL_free_chart (param_block->prob_report_clinical_vs_days_since_outbreak);
      REL_free_chart (param_block->prob_detect_vs_days_dead);
      REL_free_chart (param_block->prob_report_dead_vs_days_since_outbreak);
      g_free (param_block);
    }
  g_free (local_data->param_block);

  /* Free the 2D array of zone multipliers. */
  nzones = ZON_zone_list_length (local_data->zones);
  for (i = 0; i < nzones; i++)
    g_free (local_data->zone_multiplier[i]);
  g_free (local_data->zone_multiplier);

  g_hash_table_destroy (local_data->detectable);
  g_free (local_data->prob_report_clinical_from_awareness);
  g_free (local_data->prob_report_dead_from_awareness);
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
 * Adds a set of parameters to a detection model.
 */
void
set_params (struct naadsm_model_t_ *self, PAR_parameter_t * params)
{
  local_data_t *local_data;
  param_block_t t;
  scew_element const *e;
  gboolean success;
  double zone_multiplier;
  gboolean *production_type;
  gboolean *zone;
  unsigned int nprod_types, nzones, i, j;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER set_params (%s)", MODEL_NAME);
#endif

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  local_data = (local_data_t *) (self->model_data);

  /* Read the parameters and store them in a temporary param_block_t
   * structure. */

  e = scew_element_by_name (params, "prob-detect-vs-time-clinical");
  if (e != NULL)
    {
      t.prob_detect_vs_days_clinical = PAR_get_relationship_chart (e);
    }
  else
    {
      /* Default to 0 = no detection. */
      t.prob_detect_vs_days_clinical = REL_new_point_chart (0);
    }

  e = scew_element_by_name (params, "prob-report-clinical-signs-vs-time-since-outbreak");
  if (e != NULL)
    {
      t.prob_report_clinical_vs_days_since_outbreak = PAR_get_relationship_chart (e);
    }
  else
    {
      /* Default to 1 = public knowledge of outbreak has o effect. */
      t.prob_report_clinical_vs_days_since_outbreak = REL_new_point_chart (1);
    }

  e = scew_element_by_name (params, "prob-detect-vs-time-dead");
  if (e != NULL)
    {
      t.prob_detect_vs_days_dead = PAR_get_relationship_chart (e);
    }
  else
    {
      /* Default to 1 = notice a dead unit right away. */
      t.prob_detect_vs_days_dead = REL_new_point_chart (1);
    }

  e = scew_element_by_name (params, "prob-report-death-from-disease-vs-time-since-outbreak");
  if (e != NULL)
    {
      t.prob_report_dead_vs_days_since_outbreak = PAR_get_relationship_chart (e);
    }
  else
    {
      /* Default to 1 = public knowledge of outbreak has o effect. */
      t.prob_report_dead_vs_days_since_outbreak = REL_new_point_chart (1);
    }

  e = scew_element_by_name (params, "zone-prob-multiplier");
  if (e != NULL)
    {
      zone_multiplier = PAR_get_unitless (e, &success);
      if (success == FALSE)
        {
          g_warning ("%s: setting zone multiplier to 1 (no effect)", MODEL_NAME);
          zone_multiplier = 1;
        }
      else if (zone_multiplier < 0)
        {
          g_warning ("%s: zone multiplier cannot be negative, setting to 1 (no effect)",
                     MODEL_NAME);
          zone_multiplier = 1;
        }
      else if (zone_multiplier < 1)
        {
          g_warning ("%s: zone multiplier is less than 1, will result in slower detection inside zone",
                     MODEL_NAME);
        }      
    }
  else
    {
      zone_multiplier = 1;
    }

  /* Find out which production types, or which production type-zone
   * combinations, these parameters apply to. */
  production_type =
    naadsm_read_prodtype_attribute (params, "production-type", local_data->production_types);
  if (scew_attribute_by_name (params, "zone") != NULL)
    zone = naadsm_read_zone_attribute (params, local_data->zones);
  else
    zone = NULL;

  /* Copy the parameters to the appropriate place. */
  nprod_types = local_data->production_types->len;
  nzones = ZON_zone_list_length (local_data->zones);
  if (zone == NULL)
    {
      /* These parameters are detection charts by production type. */

      param_block_t *param_block;

      for (i = 0; i < nprod_types; i++)
        {
          if (production_type[i] == FALSE)
            continue;

          /* Create a parameter block for this production type, or overwrite
           * the existing one. */
          param_block = local_data->param_block[i];
          if (param_block == NULL)
            {
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "setting parameters for %s",
                     (char *) g_ptr_array_index (local_data->production_types, i));
#endif
              param_block = g_new (param_block_t, 1);
              local_data->param_block[i] = param_block;
            }
          else
            {
              g_warning ("overwriting previous parameters for %s",
                         (char *) g_ptr_array_index (local_data->production_types, i));
              REL_free_chart (param_block->prob_detect_vs_days_clinical);
              REL_free_chart (param_block->prob_report_clinical_vs_days_since_outbreak);
              REL_free_chart (param_block->prob_detect_vs_days_dead);
              REL_free_chart (param_block->prob_report_dead_vs_days_since_outbreak);
            }
          param_block->prob_detect_vs_days_clinical =
            REL_clone_chart (t.prob_detect_vs_days_clinical);
          param_block->prob_report_clinical_vs_days_since_outbreak =
            REL_clone_chart (t.prob_report_clinical_vs_days_since_outbreak);
          param_block->prob_detect_vs_days_dead =
            REL_clone_chart (t.prob_detect_vs_days_dead);
          param_block->prob_report_dead_vs_days_since_outbreak =
            REL_clone_chart (t.prob_report_dead_vs_days_since_outbreak);
        }
    }
  else
    {
      /* These parameters are the multiplier by production type-zone. */

      for (i = 0; i < nzones; i++)
        {
          if (zone[i] == FALSE)
            continue;

          for (j = 0; j < nprod_types; j++)
            {
              if (production_type[j] == FALSE)
                continue;

#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "setting multiplier for %s in \"%s\" zone",
                     (char *) g_ptr_array_index (local_data->production_types, j),
                     ZON_zone_list_get (local_data->zones, i)->name);
#endif
              local_data->zone_multiplier[i][j] = zone_multiplier;
            }
        }
    }

  g_free (production_type);
  if (zone != NULL)
    g_free (zone);
  REL_free_chart (t.prob_detect_vs_days_clinical);
  REL_free_chart (t.prob_report_clinical_vs_days_since_outbreak);
  REL_free_chart (t.prob_detect_vs_days_dead);
  REL_free_chart (t.prob_report_dead_vs_days_since_outbreak);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT set_params (%s)", MODEL_NAME);
#endif

  return;
}



/**
 * Returns a new detection model.
 */
naadsm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, projPJ projection,
     ZON_zone_list_t * zones)
{
  naadsm_model_t *self;
  local_data_t *local_data;
  unsigned int nprod_types, nzones, i, j;

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

  /* local_data->param_block holds an array of parameter blocks, where each
   * block holds the parameters for one production type.  Initially, all
   * pointers are NULL.  Blocks will be created as needed in the set_params
   * function. */
  local_data->production_types = herds->production_type_names;
  nprod_types = local_data->production_types->len;
  local_data->param_block = g_new0 (param_block_t *, nprod_types);

  /* local_data->zone_multiplier is a 2D array of detection multipliers.  The
   * first level of indexing is by zone (rows), then by production type
   * (columns).  The values are initialized to 1. */
  local_data->zones = zones;
  nzones = ZON_zone_list_length (zones);
  local_data->zone_multiplier = g_new (double *, nzones);

  for (i = 0; i < nzones; i++)
    {
      local_data->zone_multiplier[i] = g_new (double, nprod_types);
      for (j = 0; j < nprod_types; j++)
        local_data->zone_multiplier[i][j] = 1.0;
    }

  /* Initialize the table of detectable units. */
  local_data->detectable = g_hash_table_new (g_direct_hash, g_direct_equal);
  /* No outbreak has been announced yet. */
  local_data->outbreak_known = FALSE;
  local_data->public_announcement_day = 0;

  /* Allocate arrays for the probability of reporting due to community
   * awareness of an outbreak.  They will be initialized in the reset
   * function. */
  local_data->prob_report_clinical_from_awareness = g_new (double, nprod_types);
  local_data->prob_report_dead_from_awareness = g_new (double, nprod_types);

  /* Send the XML subtree to the init function to read the production type
   * combination specific parameters. */
  self->set_params (self, params);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

/* end of file detection-model.c */


