/** @file herd-randomizer.c
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


#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "herd-randomizer.h"

#include <glib.h>
#include <gsl/gsl_randist.h>

#include "naadsm.h"


/**
 * Generates a list of herds that are eligible to be initially infected.
 * The initial state is not specified here.  The point is just to identify
 * every unit that might eventually be selected.
 *
 * As a side effect, every herd is set to the initially susceptible state.
 * Elsewhere in the randomization function, this is changed.
 *
 * @param herds a herd list.
 * @param nherds the number of herds in the herd list.
 * @param array an array to fill with the list of herds that are eligible for selection.
 * @return the number of herds with the given production_type.
 */
unsigned int
get_herds_eligible_for_random_selection ( HRD_herd_list_t *herds, GArray* array )
{
  HRD_herd_t* herd;
  unsigned int count = 0;
  unsigned int i, nherds;

  nherds = HRD_herd_list_length( herds );
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get( herds, i );

      herd->initial_status = Susceptible;
      herd->status = Susceptible;
      herd->days_in_initial_status = 0;
      herd->days_left_in_initial_status = 0;

      if( herd->may_be_initially_infected )
        {
          g_array_append_val( array, herd );
          count++;
        }
    }
  return count;
}


void
randomize_initial_states( HRD_herd_list_t *herds, int n_states, int *initial_state_numbers, RAN_gen_t *rng )
{
  HRD_herd_t* herd;
  long int i, h;
  char str[1024];
  GArray* array1;
  GArray* array2;
  long int n_to_select, neligible_herds;
  HRD_herd_t** eligible_herds;
  HRD_herd_t** selected_herds;

  /* How many herds will be initially infected? */
  /*--------------------------------------------*/
  n_to_select = 0;
  for( i = 0; i < n_states; ++i )
    n_to_select = n_to_select + initial_state_numbers[i];

  if( NULL != naadsm_printf ) {
    sprintf( str, "Total number of units to initially infect: %ld", n_to_select );
    naadsm_printf( str );
    for( i = 0; i < n_states; ++i ) {
      sprintf( str, "Number init state %ld: %d", i, initial_state_numbers[i] );
      naadsm_printf( str );
    }
  }


  /* Make an array with only units that are eligible to be randomized. */
  /* In the process, reset every herd to initially susceptible state.   */
  /*--------------------------------------------------------------------*/
  array1 = g_array_new( FALSE, FALSE, sizeof( HRD_herd_t* ) );
  neligible_herds = get_herds_eligible_for_random_selection( herds, array1 );
  eligible_herds = (HRD_herd_t**) array1->data;

  if (NULL != naadsm_printf) {
    sprintf( str, "Number of eligible herds: %ld", neligible_herds );
    naadsm_printf( str );
  }


  /* Choose n_to_select of these herds at random and shuffle them. */
  /*---------------------------------------------------------------*/
  array2 = g_array_new( FALSE, FALSE, sizeof( HRD_herd_t* ) );
  /* Fill the array with dummy data.  It will be replaced in the function gsl_ran_choose(). */
  herd = NULL;
  for( i = 0; i < n_to_select; ++i )
    g_array_append_val( array2, herd );
  selected_herds = (HRD_herd_t**) array2->data;

  gsl_ran_choose( RAN_generator_as_gsl( rng ), selected_herds, n_to_select, eligible_herds, neligible_herds, sizeof( HRD_herd_t* ) );
  gsl_ran_shuffle( RAN_generator_as_gsl( rng ), selected_herds, n_to_select, sizeof( HRD_herd_t* ) );



  /* Set the randomly selected units to their new disease states, based
     on the numbers that should be in each state as determined above. */
  /*------------------------------------------------------------------*/
  h = 0;
  for( i= 0; i < initial_state_numbers[Latent]; ++i ){
    selected_herds[h]->initial_status = Latent;
    selected_herds[h]->status = Latent;
    ++h;
  }
  for( i= 0; i < initial_state_numbers[InfectiousSubclinical]; ++i ){
    selected_herds[h]->initial_status = InfectiousSubclinical;
    selected_herds[h]->status = InfectiousSubclinical;
    ++h;
  }
  for( i= 0; i < initial_state_numbers[InfectiousClinical]; ++i ){
    selected_herds[h]->initial_status = InfectiousClinical;
    selected_herds[h]->status = InfectiousClinical;
    ++h;
  }
  for( i= 0; i < initial_state_numbers[NaturallyImmune]; ++i ){
    selected_herds[h]->initial_status = NaturallyImmune;
    selected_herds[h]->status = NaturallyImmune;
    ++h;
  }
  for( i= 0; i < initial_state_numbers[VaccineImmune]; ++i ){
    selected_herds[h]->initial_status = VaccineImmune;
    selected_herds[h]->status = VaccineImmune;
    ++h;
  }

  /* Clean up. */
  /*-----------*/
  g_array_free( array1, FALSE );
  g_array_free( array2, FALSE );
  g_free( eligible_herds );
  g_free( selected_herds );
}

