/** @file resources-and-implementation-of-controls-model.h
 *
 * @author Aaron Reeves <Aaron.Reeves@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   College of Veterinary Medicine and Biomedical Sciences<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80523<br>
 *   USA
 * @version 0.1
 * @date June 2005
 *
 * Copyright &copy; 2005 Animal Population Health Institute, Colorado State University
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef RESOURCES_AND_IMPLEMENTATION_OF_CONTROLS_MODEL_H
#define RESOURCES_AND_IMPLEMENTATION_OF_CONTROLS_MODEL_H

char *resources_and_implementation_of_controls_model_interface_version (void);
ergadm_model_t *resources_and_implementation_of_controls_model_new (scew_element * params,
                                                                    HRD_herd_list_t * herds,
                                                                    ZON_zone_list_t * zones);

#endif
