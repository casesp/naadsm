/** @file herd_zone_updater.h
 * Interface for herd_zone_updater.c.  The module provides a function for
 * updating the assignment of herds to zones.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date May 2006
 *
 * Copyright &copy; University of Guelph, 2006-2007
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef HERD_ZONE_UPDATER_H
#define HERD_ZONE_UPDATER_H

#include "herd.h"
#include "zone.h"



/* Prototypes. */
void ergadm_update_herd_zones (HRD_herd_list_t *, ZON_zone_list_t *);

#endif /* !HERD_ZONE_UPDATER_H */
