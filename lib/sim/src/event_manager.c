/** @file event_manager.c
 * Functions for managing communication among sub-models.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date April 2003
 *
 * Copyright &copy; University of Guelph, 2003-2006
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

#include "guilib.h"


/**
 * Creates a list mapping event types to the sub-models that listen for them.
 *
 * @param list an (uninitialized) array of lists of sub-models.
 * @param nmodels the number of sub-models.
 * @param models an array of sub-models.
 */
void
build_listener_list (GSList ** list, ergadm_model_t ** models, int nmodels)
{
  GSList *model_list;
  ergadm_model_t *model;
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
            model = (ergadm_model_t *) (model_list->data);
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



/**
 * Creates a new event manager.
 *
 * @param models a list of sub-models.
 * @param nmodels the number of sub-models.
 * @return a pointer to a newly-created ergadm_event_manager_t structure.
 */
ergadm_event_manager_t *
ergadm_new_event_manager (ergadm_model_t ** models, int nmodels)
{
  ergadm_event_manager_t *manager;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_new_event_manager");
#endif

  manager = g_new (ergadm_event_manager_t, 1);
  manager->nmodels = nmodels;
  manager->models = models;
  manager->queue = EVT_new_event_queue ();

  build_listener_list (manager->listeners, models, nmodels);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_new_event_manager");
#endif

  return manager;
}



/**
 * Deletes an event manager from memory.  Does not delete the sub-models.
 */
void
ergadm_free_event_manager (ergadm_event_manager_t * manager)
{
  EVT_event_type_t event_type;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_free_event_manager");
#endif

  if (manager == NULL)
    return;

  EVT_free_event_queue (manager->queue);
  for (event_type = 0; event_type < EVT_NEVENT_TYPES; event_type++)
    g_slist_free (manager->listeners[event_type]);
  g_free (manager);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_free_event_manager");
#endif
}



/**
 * Carries out the consequences of a new event.
 *
 * Side effects: one or more herds might change state as a result of the event.
 *   \a new_event will be freed after it is processed.
 *
 * @image html events_flowchart.png
 * @image latex events_flowchart.eps "" width=4in
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
ergadm_create_event (ergadm_event_manager_t * manager, EVT_event_t * new_event,
                     HRD_herd_list_t * herds, ZON_zone_list_t * zones, RAN_gen_t * rng)
{
  EVT_event_t *event;
  GSList *iter;
  ergadm_model_t *model;
#if INFO || DEBUG
  char *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_create_event");
#endif

  EVT_event_enqueue (manager->queue, new_event);

#ifdef FIX_ME
#if DEBUG
  s = EVT_event_queue_to_string (manager->queue);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s);
  free (s);
#endif
#endif

  while (!EVT_event_queue_is_empty (manager->queue))
    {
      event = EVT_event_dequeue (manager->queue);

#ifdef FIX_ME
#if INFO
      s = EVT_event_to_string (event);
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "now handling %s", s);
      free (s);
#endif
#endif

      for (iter = manager->listeners[event->type]; iter != NULL; iter = g_slist_next (iter))
        {
          /* Does the GUI user want to stop a simulation in progress? */
          if (NULL != guilib_simulation_stop)
            {
              /* This check may break the day loop.
               * If necessary, Another check (see above) will break the iteration loop.*/
              if (0 != guilib_simulation_stop ())
                break;
            }

          model = (ergadm_model_t *) (iter->data);
#if INFO
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "running %s", model->name);
#endif
          model->run (model, herds, zones, event, rng, manager->queue);
        }

#ifdef FIX_ME
#if DEBUG
      s = EVT_event_queue_to_string (manager->queue);
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "all models listening for %s run, queue now = %s",
             EVT_event_type_name[event->type], s);
      free (s);
#endif
#endif
      EVT_free_event (event);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_create_event");
#endif
}

/* end of file event_manager.c */
