/** @file event_manager.h
 * Interface for event_manager.c.  The event manager is a singleton object that
 * handles all communication among sub-models.  See ergadm_create_event() for
 * a flowchart describing its actions.
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

#ifndef EVENT_MANAGER_H
#define EVENT_MANAGER_H

#include "model.h"
#include "event.h"



/**
 * An object that manages communication among sub-models.  It queries
 * sub-models as to what events they listen for and runs them when those events
 * occur.
 * 
 * This is a Singleton object; only one need exist.
 */
typedef struct
{
  int nmodels;
  ergadm_model_t **models;
  EVT_event_queue_t *queue;
  GSList *listeners[EVT_NEVENT_TYPES]; /** which models are listening for which events */
}
ergadm_event_manager_t;



/* Prototypes. */
ergadm_event_manager_t *ergadm_new_event_manager (ergadm_model_t **, int);
void ergadm_free_event_manager (ergadm_event_manager_t *);
void ergadm_create_event (ergadm_event_manager_t *, EVT_event_t *,
                          HRD_herd_list_t *, ZON_zone_list_t *, RAN_gen_t *);

#endif /* !EVENT_MANAGER_H */
