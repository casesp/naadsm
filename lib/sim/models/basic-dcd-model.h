/** @file basic-dcd-model.h
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   School of Computer Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 *
 * Copyright &copy; University of Guelph, 2011
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef BASIC_DCD_MODEL_H
#define BASIC_DCD_MODEL_H

naadsm_model_t *basic_dcd_model_new (scew_element * params, HRD_herd_list_t *,
                                     projPJ, ZON_zone_list_t *);

#endif
