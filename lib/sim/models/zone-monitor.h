/** @file zone-monitor.h
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date Feb 2007
 *
 * Copyright &copy; University of Guelph, 2007
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef ZONE_MONITOR_H
#define ZONE_MONITOR_H

char *zone_monitor_interface_version (void);
ergadm_model_t *zone_monitor_new (scew_element * params, HRD_herd_list_t * herds,
                                  ZON_zone_list_t * zones);

#endif
