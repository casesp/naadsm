/** @file herd-randomizer.h
 *
 * @author Aaron Reeves <Aaron.Reeves@ucalgary.ca><br>
 *   Department of Production Animal Health<br>
 *   Faculty of Veterinary Medicine<br>
 *   University of Calgary<br>
 *   Calgary, AB T2N 4N1<br>
 *   Canada
 *
 * Copyright &copy; 2011 - 2013 NAADSM Development Team
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef HERD_RANDOMIZER_H
#define HERD_RANDOMIZER_H

#include "herd.h"
#include "rng.h"

void randomize_initial_states( HRD_herd_list_t *herds, int n_states, int *initial_state_numbers, RAN_gen_t *rng );

#endif /* HERD_RANDOMIZER_H */
