/** @file prophylactic-vaccination-model.h
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 *
 * Copyright &copy; 2009 University of Guelph
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef PROPHYLACTIC_VACCINATION_MODEL_H
#define PROPHYLACTIC_VACCINATION_MODEL_H

naadsm_model_t *prophylactic_vaccination_model_new (scew_element * params,
                                                    HRD_herd_list_t *,
                                                    projPJ, ZON_zone_list_t *);
gboolean prophylactic_vaccination_model_is_singleton (void);
#endif
