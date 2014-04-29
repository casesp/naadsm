/** @file unit-state-monitor.h
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 *
 * Copyright &copy; University of Guelph, 2009-2010
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef UNIT_STATE_MONITOR_H
#define UNIT_STATE_MONITOR_H

naadsm_model_t *unit_state_monitor_new (scew_element * params, HRD_herd_list_t *,
                                        projPJ, ZON_zone_list_t *);
gboolean unit_state_monitor_is_singleton (void);

#endif