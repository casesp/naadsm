/** @file scorecard.h
 * Administrative
 *
 * Symbols from this module begin with HSC_.
 *
 * @author Anthony Schwickerath <Drew.Schwickerath@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80526-8117<br>
 *   USA
 *
 * @version 0.1
 * @date April 2009
 *
 * Copyright &copy; Colorado State University, 2009
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef SCORECARD_H
#define SCORECARD_H

#include <stdio.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#include <glib.h>



/** The authorities' information for one herd. */
typedef struct
{
  gboolean is_detected_as_diseased;
  int day_detected_as_diseased;

  gboolean is_detected_as_dead;
  int day_detected_as_dead;
  
  int min_next_vaccination_day;
  
  int first_destruction_queue_day;
}
HSC_scorecard_t;



/* Prototypes. */

HSC_scorecard_t *HSC_new_scorecard (void);
void HSC_free_scorecard (HSC_scorecard_t *);

char *HSC_scorecard_to_string (HSC_scorecard_t *);
int HSC_fprintf_scorecard (FILE *, HSC_scorecard_t *);
#define HSC_printf_scorecard(S) HSC_fprintf_scorecard(stdout,S)

#define HSC_herd_is_detected_as_diseased(S) (((HSC_scorecard_t *)(S))->is_detected_as_diseased)
#define HSC_herd_is_detected_as_dead(S) (((HSC_scorecard_t *)(S))->is_detected_as_dead)
#define HSC_herd_day_detected_as_diseased(S) (((HSC_scorecard_t *)(S))->day_detected_as_diseased)
#define HSC_herd_day_detected_as_dead(S) (((HSC_scorecard_t *)(S))->day_detected_as_dead)

#define HSC_herd_min_next_vaccination_day(S) (((HSC_scorecard_t *)(S))->min_next_vaccination_day)

#define HSC_herd_first_destruction_queue_day(S) (((HSC_scorecard_t *)(S))->first_destruction_queue_day)

void HSC_scorecard_reset (HSC_scorecard_t *);

gboolean HSC_record_detection_as_diseased (HSC_scorecard_t *, int day);
gboolean HSC_record_detection_as_dead (HSC_scorecard_t *, int day);

void HSC_record_next_vaccination_day (HSC_scorecard_t *, int next_vaccination_day); 
void HSC_record_first_destruction_queue_day (HSC_scorecard_t *, int day);

#endif /* !SCORECARD_H */
