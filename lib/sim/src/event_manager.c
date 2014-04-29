/** @file event_manager.c
 * Functions for managing communication among sub-models.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date April 2003
 *
 * Copyright &copy; University of Guelph, 2003-2011
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "event_manager.h"

#include "naadsm.h"
#include "general.h"


/**
 * Creates a list mapping event types to the sub-models that listen for them.
 *
 * @param list an (uninitialized) array of lists of sub-models.
 * @param nmodels the number of sub-models.
 * @param models an array of sub-models.
 */
void
build_listener_list (GSList ** list, naadsm_model_t ** models, int nmodels)
{
  GSList *model_list;
  naadsm_model_t *model;
  EVT_event_type_t event_type;
  int i;                        /* loop counter */
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER build_listener_list");
#endif

  for (event_type = 0; event_type < EVT_NEVENT_TYPES; event_type++)
    {
#if DEBUG
      s = g_string_new (NULL);
      g_string_sprintf (s, "building list for %s events...", EVT_event_type_name[event_type]);
#endif

      model_list = NULL;
      for (i = 0; i < nmodels; i++)
        {
          model = models[i];
          if (model->is_listening_for (model, event_type))
            {
              model_list = g_slist_append (model_list, model);
#if DEBUG
              g_string_sprintfa (s, " %s", model->name);
#endif
            }
        }
      list[event_type] = model_list;
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
      g_string_free (s, TRUE);
#endif
    }

#if DEBUG
  for (event_type = 0; event_type < EVT_NEVENT_TYPES; event_type++)
    {
      s = g_string_new (NULL);
      g_string_sprintf (s, "%s models listening:", EVT_event_type_name[event_type]);
      model_list = list[event_type];
      if (model_list == NULL)
        g_string_sprintfa (s, " none");
      else
        for (; model_list != NULL; model_list = g_slist_next (model_list))
          {
            model = (naadsm_model_t *) (model_list->data);
            g_string_sprintfa (s, " %s", model->name);
          }
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
      g_string_free (s, TRUE);
    }
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT build_listener_list");
#endif
}



void
naadsm_exposure_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_exposure_event_t *details;
  HRD_expose_t update;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_Exposure);
  details = &(event->u.exposure);
  if (details->exposing_herd == NULL)
    {
      update.src_index = details->exposed_herd->index;
      update.src_status = NAADSM_StateUnspecified;
    }
  else
    {
      update.src_index = details->exposing_herd->index;
      update.src_status = (NAADSM_disease_state) details->exposing_herd->status;
    }
  update.dest_index = details->exposed_herd->index;
  update.dest_status = (NAADSM_disease_state) details->exposed_herd->status;
  update.day = (int) details->day;

  if( TRUE == details->adequate )
    update.is_adequate = NAADSM_SuccessTrue;
  else
    update.is_adequate = NAADSM_SuccessFalse;

  if (details->contact_type < NAADSM_NCONTACT_TYPES
      && details->contact_type != NAADSM_UnspecifiedInfectionType)
    update.exposure_method = details->contact_type;
  else
    {
      /* If this condition occurs, someone forgot something. */
      g_error( "Unrecognized exposure mechanism (%s) in exposure event", details->cause );
      update.exposure_method = NAADSM_UnspecifiedInfectionType;     
    }

#ifdef USE_SC_GUILIB
  sc_expose_herd( details->exposed_herd, update );
#else
  if (NULL != naadsm_expose_herd)
    {
      naadsm_expose_herd (update);
    }
#endif

  return;
}



void
naadsm_infection_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_infection_event_t *details;
  HRD_infect_t update;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_Infection);
  details = &(event->u.infection);
  
  update.herd_index = details->herd->index;
  update.day = details->day;

#ifdef USE_SC_GUILIB
  sc_infect_herd( details->day, details->herd, update );
#else
  if (NULL != naadsm_infect_herd)
    {
      naadsm_infect_herd (update);
    }
#endif

  return;
}


void
naadsm_detection_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_detection_event_t *details;
  HRD_detect_t detection;
  
  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_Detection);
  details = &(event->u.detection);
  
  detection.herd_index = details->herd->index;
  detection.reason = details->means;
  detection.test_result = details->test_result;
  
#ifdef USE_SC_GUILIB
  sc_detect_herd( details->day, herd, detection );
#else  
  if (NULL != naadsm_detect_herd)
    {
      naadsm_detect_herd (detection);
    }
#endif   
}


void
naadsm_trace_result_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_trace_result_event_t *details;
  HRD_herd_t *identified_herd, *origin_herd;
  HRD_trace_t trace;
    
  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_TraceResult);
  details = &(event->u.trace_result);
  identified_herd = (details->direction == NAADSM_TraceForwardOrOut) ? details->exposed_herd : details->exposing_herd;  
  origin_herd = (details->direction == NAADSM_TraceForwardOrOut) ? details->exposing_herd : details->exposed_herd; 
  
  trace.day = (int) details->day;
  trace.initiated_day = (int) details->initiated_day;
  
  trace.identified_index = identified_herd->index;
  trace.identified_status = (NAADSM_disease_state) identified_herd->status;
  
  trace.origin_index = origin_herd->index; 
  trace.origin_status = (NAADSM_disease_state) origin_herd->status;
  
  trace.trace_type = details->direction;
  trace.contact_type = details->contact_type;
  if (trace.contact_type != NAADSM_DirectContact
      && trace.contact_type != NAADSM_IndirectContact)
    {
      g_error( "Bad contact type in naadsm_trace_result_event_to_callback" );
      trace.contact_type = 0;
    }

  if( TRUE == details->traced )
    trace.success = NAADSM_SuccessTrue;
  else
    trace.success = NAADSM_SuccessFalse; 

  #ifdef USE_SC_GUILIB
    sc_trace_herd( details->exposed_herd, trace );
  #else
    if (NULL != naadsm_trace_herd)
      naadsm_trace_herd (trace);
  #endif
}



void
naadsm_commitment_to_vaccinate_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_commitment_to_vaccinate_event_t *details;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_CommitmentToVaccinate);
  details = &(event->u.commitment_to_vaccinate);

  if (NULL != naadsm_queue_herd_for_vaccination)
    {
      naadsm_queue_herd_for_vaccination (details->herd->index);
    }

  return;
}


void
naadsm_vaccination_canceled_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_vaccination_canceled_event_t *details;
  HRD_control_t update;
  
  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_VaccinationCanceled);
  details = &(event->u.vaccination_canceled);
  
  update.herd_index = details->herd->index;
  update.day_commitment_made = details->day_commitment_made;
  update.reason = 0; /* This is unused for "vaccination canceled" events */
 
#ifdef USE_SC_GUILIB
  sc_cancel_herd_vaccination( details->day, details->herd, update );
#else
  if (NULL != naadsm_cancel_herd_vaccination)
    {
      naadsm_cancel_herd_vaccination (update);
    };
#endif

  return;
}


void
naadsm_vaccination_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_vaccination_event_t *details;
  HRD_control_t update;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_Vaccination);
  details = &(event->u.vaccination);
  update.herd_index = details->herd->index;
  update.day_commitment_made = details->day_commitment_made;

  if( 0 == strcmp( "Ring", details->reason ) ) 
    update.reason = NAADSM_ControlRing;
  else if( 0 == strcmp( "Ini", details->reason ) ) 
    update.reason = NAADSM_ControlInitialState;
  else if( 0 == strcmp( "Prph", details->reason ) ) 
    update.reason = NAADSM_ControlRoutine;
  else
    {
      g_error( "Unrecognized reason for vaccination (%s) in vaccination event", details->reason );
      update.reason = NAADSM_ControlReasonUnspecified;
    }

#ifdef USE_SC_GUILIB
  sc_vaccinate_herd( details->day, details->herd, update );
#else
  if (NULL != naadsm_vaccinate_herd)
    {
      naadsm_vaccinate_herd (update);
    };
#endif

  return;
}



void
naadsm_commitment_to_destroy_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_commitment_to_destroy_event_t *details;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_CommitmentToDestroy);
  details = &(event->u.commitment_to_destroy);

  if (NULL != naadsm_queue_herd_for_destruction)
    {
      naadsm_queue_herd_for_destruction (details->herd->index);
    }

  return;
}



void
naadsm_destruction_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_destruction_event_t *details;
  HRD_control_t update;
  unsigned int i;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_Destruction);
  details = &(event->u.destruction);
  update.herd_index = details->herd->index;
  update.day_commitment_made = details->day_commitment_made;

  for (i = 0; i < NAADSM_NCONTROL_REASONS; i++)
    {
      if (i != NAADSM_ControlReasonUnspecified
          && 0 == strcmp(NAADSM_control_reason_abbrev[i], details->reason))
        {
          update.reason = i;
          break;
        }
    }
  if (i == NAADSM_NCONTROL_REASONS)
    {
      /* If this condition occurs, someone forgot something. */
      g_error( "Unrecognized reason for destruction (%s) in destruction event", details->reason );      
      update.reason = NAADSM_ControlReasonUnspecified;
    }

#ifdef USE_SC_GUILIB
  sc_destroy_herd( details->day, details->herd, update );
#else
  if (NULL != naadsm_destroy_herd)
    {
      naadsm_destroy_herd (update);
    }
#endif

  return;
}


void
naadsm_destruction_canceled_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_destruction_canceled_event_t *details;
  HRD_control_t update;
  
  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_DestructionCanceled);
  details = &(event->u.destruction_canceled);
  
  update.herd_index = details->herd->index;
  update.day_commitment_made = details->day_commitment_made;
  update.reason = 0; /* This is unused for "destruction canceled" events */
 
#ifdef USE_SC_GUILIB
  sc_cancel_herd_destruction( details->day, details->herd, update );
#else
  if (NULL != naadsm_cancel_herd_destruction)
    {
      naadsm_cancel_herd_destruction (update);
    }
#endif

  return;
}


void
naadsm_unit_state_change_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_unit_state_change_event_t *details;
  HRD_update_t update;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_UnitStateChange);
  details = &(event->u.unit_state_change);
  update.herd_index = details->herd->index;
  update.status = (NAADSM_disease_state) (details->new_state);

#ifdef USE_SC_GUILIB
  sc_change_herd_state ( details->herd, update );
#else
  if (NULL != naadsm_change_herd_state)
    {
      naadsm_change_herd_state (update);
    }
#endif

  return;
}


void
naadsm_examine_herd_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_exam_event_t *details;
  HRD_exam_t exam;

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_Exam);
  details = &(event->u.exam);
  
  /* Record the exam in the GUI */
  /* -------------------------- */
  exam.herd_index = details->herd->index;
  
  if ( 0 == strcmp( details->reason, "DirFwd" ) )
    {
      exam.trace_type = NAADSM_TraceForwardOrOut;
      exam.contact_type = NAADSM_DirectContact;
    }
  else if( 0 == strcmp( details->reason, "DirBack" ) )
    {
      exam.trace_type = NAADSM_TraceBackOrIn;
      exam.contact_type = NAADSM_DirectContact;
    }
  else if( 0 == strcmp( details->reason, "IndFwd" ) ) 
    {
      exam.trace_type = NAADSM_TraceForwardOrOut;
      exam.contact_type = NAADSM_IndirectContact;      
    }
  else if( 0 == strcmp( details->reason, "IndBack" ) )
    {
      exam.trace_type = NAADSM_TraceBackOrIn;
      exam.contact_type = NAADSM_IndirectContact;        
    }
  else
    {
      g_error( "Unrecognized event reason (%s) in naadsm_examine_herd_event_to_callback", details->reason );
    }

  #ifdef USE_SC_GUILIB
    sc_examine_herd( details->herd, exam );
  #else
    if (NULL != naadsm_examine_herd)
      naadsm_examine_herd (exam);
  #endif
    
  return;  
}


void
naadsm_test_result_event_to_callback (gpointer data, gpointer user_data)
{
  EVT_event_t *event;
  EVT_test_result_event_t *details;
  HRD_test_t test; 

  event = (EVT_event_t *) data;
  g_assert (event->type == EVT_TestResult);
  details = &(event->u.test_result); 
  
  /* Record the test in the GUI */
  /* -------------------------- */
  test.herd_index = details->herd->index;

  if( 0 == strcmp( "DirFwd", details->reason ) )
    {
      test.contact_type = NAADSM_DirectContact;
      test.trace_type = NAADSM_TraceForwardOrOut;  
    }
  else if( 0 == strcmp( "DirBack", details->reason ) )
    {
      test.contact_type = NAADSM_DirectContact;
      test.trace_type = NAADSM_TraceBackOrIn;    
    }
  else if( 0 == strcmp( "IndFwd", details->reason ) )
    {
      test.contact_type = NAADSM_IndirectContact;
      test.trace_type = NAADSM_TraceForwardOrOut;    
    }
  else if( 0 == strcmp( "IndBack", details->reason ) )
    {
      test.contact_type = NAADSM_IndirectContact;
      test.trace_type = NAADSM_TraceBackOrIn;    
    }
  else
    {
      g_error( "Unrecognize event reason (%s) in naadsm_test_result_event_to_callback", details->reason );  
    } 

  if( details->positive && details->correct )
    test.test_result = NAADSM_TestTruePositive;
  else if( details->positive && !(details->correct) )
    test.test_result = NAADSM_TestFalsePositive;
  else if( !(details->positive) && details->correct )
    test.test_result = NAADSM_TestTrueNegative;
  else if( !(details->positive) && !(details->correct) )
    test.test_result = NAADSM_TestFalseNegative;
    
  #ifdef USE_SC_GUILIB
    sc_test_herd( details->herd, test );
  #else
    if (NULL != naadsm_test_herd)
      naadsm_test_herd (test);
  #endif
  
  return;
}


/**
 * Creates a new event manager.
 *
 * @param models a list of sub-models.
 * @param nmodels the number of sub-models.
 * @return a pointer to a newly-created naadsm_event_manager_t structure.
 */
naadsm_event_manager_t *
naadsm_new_event_manager (naadsm_model_t ** models, int nmodels)
{
  naadsm_event_manager_t *manager;

#if DEBUG
  g_debug ("----- ENTER naadsm_new_event_manager");
#endif

  manager = g_new (naadsm_event_manager_t, 1);
  manager->nmodels = nmodels;
  manager->models = models;
  manager->queue = EVT_new_event_queue ();

  build_listener_list (manager->listeners, models, nmodels);

  /* Create a list of function pointers, one for each event type.  When an
   * event is popped from the event queue in naadsm_create_event, the callback
   * function matching the event type will be invoked. */
  manager->hooks = g_new0 (GFunc, EVT_NEVENT_TYPES);
  manager->hooks[EVT_Exposure] = naadsm_exposure_event_to_callback;
  manager->hooks[EVT_Infection] = naadsm_infection_event_to_callback;
  manager->hooks[EVT_Detection] = naadsm_detection_event_to_callback;
  manager->hooks[EVT_TraceResult] = naadsm_trace_result_event_to_callback;
  manager->hooks[EVT_CommitmentToVaccinate] = naadsm_commitment_to_vaccinate_event_to_callback;
  manager->hooks[EVT_Vaccination] = naadsm_vaccination_event_to_callback;
  manager->hooks[EVT_VaccinationCanceled] = naadsm_vaccination_canceled_event_to_callback;
  manager->hooks[EVT_CommitmentToDestroy] = naadsm_commitment_to_destroy_event_to_callback;
  manager->hooks[EVT_Destruction] = naadsm_destruction_event_to_callback;
  manager->hooks[EVT_DestructionCanceled] = naadsm_destruction_canceled_event_to_callback;  
  manager->hooks[EVT_UnitStateChange] = naadsm_unit_state_change_event_to_callback;
  manager->hooks[EVT_Exam] = naadsm_examine_herd_event_to_callback;
  manager->hooks[EVT_TestResult] = naadsm_test_result_event_to_callback;
  /* FIXME: naadsm_make_zone_focus should be an event! */

#if DEBUG
  g_debug ("----- EXIT naadsm_new_event_manager");
#endif

  return manager;
}



/**
 * Deletes an event manager from memory.  Does not delete the sub-models.
 */
void
naadsm_free_event_manager (naadsm_event_manager_t * manager)
{
  EVT_event_type_t event_type;

#if DEBUG
  g_debug ("----- ENTER naadsm_free_event_manager");
#endif

  if (manager == NULL)
    goto end;

  EVT_free_event_queue (manager->queue);
  for (event_type = 0; event_type < EVT_NEVENT_TYPES; event_type++)
    g_slist_free (manager->listeners[event_type]);
  g_free (manager->hooks);
  g_free (manager);

end:
#if DEBUG
  g_debug ("----- EXIT naadsm_free_event_manager");
#endif
  return;
}



/**
 * Carries out the consequences of a new event.
 *
 * Side effects: one or more herds might change state as a result of the event.
 *   \a new_event will be freed after it is processed.
 *
 * @image html events_flowchart.png
 *
 * @param manager an event manager.
 * @param new_event an event.
 * @param herds a list of herds.
 * @param zones a list of zones.
 * @param rng a random number generator.
 *
 * @todo Keep "request" events in the queue if no sub-model claims them.
 */
void
naadsm_create_event (naadsm_event_manager_t * manager, EVT_event_t * new_event,
                     HRD_herd_list_t * herds, ZON_zone_list_t * zones, RAN_gen_t * rng)
{
  EVT_event_t *event;
  GSList *iter;
  naadsm_model_t *model;
  GFunc callback_function;
#if DEBUG
  char *s;
#endif

#if DEBUG
  g_debug ("----- ENTER naadsm_create_event");
#endif

  EVT_event_enqueue (manager->queue, new_event);

#ifdef FIX_ME
#if DEBUG
  s = EVT_event_queue_to_string (manager->queue);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s);
  g_free (s);
#endif
#endif

  while (!EVT_event_queue_is_empty (manager->queue))
    {
      event = EVT_event_dequeue (manager->queue, rng);

#if DEBUG
      s = EVT_event_to_string (event);
      g_debug ("now handling %s", s);
      /* naadsm_printf( s ); */
      g_free (s);
#endif

      callback_function = manager->hooks[event->type];
      if (callback_function != NULL)
        callback_function (event, NULL);
      if (event->type == EVT_Detection)
        _iteration.first_detection = TRUE;

      for (iter = manager->listeners[event->type]; iter != NULL; iter = g_slist_next (iter))
        {
          /* Does the GUI user want to stop a simulation in progress? */
          if (NULL != naadsm_simulation_stop)
            {
              /* This check may break the day loop.
               * If necessary, another check (see above) will break the iteration loop.*/
              if (0 != naadsm_simulation_stop ())
                break;
            }

          model = (naadsm_model_t *) (iter->data);
#if DEBUG
          g_debug ("running %s", model->name);
#endif
          model->run (model, herds, zones, event, rng, manager->queue);                   
        }

#ifdef FIX_ME
#if DEBUG
      s = EVT_event_queue_to_string (manager->queue);
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "all models listening for %s run, queue now = %s",
             EVT_event_type_name[event->type], s);
      g_free (s);
#endif
#endif
      EVT_free_event (event);
    }

#if DEBUG
  g_debug ("----- EXIT naadsm_create_event");
#endif
}

/* end of file event_manager.c */
