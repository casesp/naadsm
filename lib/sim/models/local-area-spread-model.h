/** @file local-area-spread-model.h
 *
 * @author Anthony "Drew" Schwickerath <Drew.Schwickerath@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   College of Veterinary Medicine and Biomedical Sciences<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80523<br>
 *   USA
 * @version 0.1
 * @date March 2009
 *
 * Copyright &copy; 2009 Animal Population Health Institute, Colorado State University
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef LOCAL_SPREAD_MODEL_H
#define LOCAL_SPREAD_MODEL_H

naadsm_model_t *local_area_spread_model_new (scew_element * params,
                                             HRD_herd_list_t *,
                                             projPJ,
                                             ZON_zone_list_t *);
gboolean local_area_spread_model_is_singleton (void);

#endif
