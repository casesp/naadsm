/** @file model_util.h
 * Interface for model_util.c.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date October 2004
 *
 * Copyright &copy; University of Guelph, 2004-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef MODEL_UTIL_H
#define MODEL_UTIL_H

#include "herd.h"
#include "zone.h"
#include <glib.h>
#include <scew/scew.h>



/* Prototypes. */
gboolean *ergadm_read_prodtype_attribute (scew_element *, char *, GPtrArray *);
gboolean *ergadm_read_zone_attribute (scew_element *, ZON_zone_list_t *);
void ergadm_extend_rotating_array (GPtrArray * array, unsigned int length, unsigned int index);

#endif /* !MODEL_UTIL_H */
