/** @file scorecard.c
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


#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <unistd.h>
#include <stdio.h>
#include "scorecard.h"

#if STDC_HEADERS
#  include <stdlib.h>
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_CTYPE_H
#  include <ctype.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#if HAVE_ERRNO_H
#  include <errno.h>
#endif

#define EPSILON 0.001

#include <naadsm.h>



#ifndef WIN_DLL
#ifndef COGRID
/* This line causes problems on Windows, but seems to be unnecessary. */
extern FILE *stdin;
#endif
#endif



HSC_scorecard_t *
HSC_new_scorecard (void)
{
  HSC_scorecard_t *scorecard = NULL;

  scorecard = g_new(HSC_scorecard_t, 1);
  HSC_scorecard_reset(scorecard);

  return scorecard;
}



void
HSC_free_scorecard (HSC_scorecard_t *scorecard)
{
  if (scorecard != NULL)
    {
      g_free (scorecard);
    }
}



char *
HSC_scorecard_to_string (HSC_scorecard_t *scorecard)
{
  GString *s;
  char *chararray;

  s = g_string_new ("<scorecard ");
  if (scorecard->is_detected_as_diseased)
    g_string_append_printf (s, "detected as diseased (day = %d)\n", scorecard->day_detected_as_diseased);
  else
    g_string_append_printf (s, "not detected as diseased\n");
  if (scorecard->is_detected_as_dead)
    g_string_append_printf (s, "detected as dead (day = %d)\n", scorecard->day_detected_as_dead);
  else
    g_string_append_printf (s, "not detected as dead\n");

  g_string_append_printf (s, "min next vaccination day = %d\n", scorecard->min_next_vaccination_day);

  g_string_append_printf (s, "first destruction queue day = %d\n", scorecard->first_destruction_queue_day);

  g_string_append_printf (s, ">");

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



int
HSC_fprintf_scorecard (FILE *stream, HSC_scorecard_t *scorecard)
{
  char *s = NULL;
  int nchars_written = 0;

#if DEBUG
  g_debug ("----- ENTER HSC_fprintf_scorecard");
#endif

  if (!stream)
    stream = stdout;

  s = HSC_scorecard_to_string (scorecard);
  nchars_written = fprintf (stream, "%s", s);
  g_free (s);

#if DEBUG
  g_debug ("----- EXIT HSC_fprintf_scorecard");
#endif

  return nchars_written;
}



void
HSC_scorecard_reset (HSC_scorecard_t *scorecard)
{
  scorecard->is_detected_as_diseased = FALSE;
  scorecard->day_detected_as_diseased = -1;

  scorecard->is_detected_as_dead = FALSE;
  scorecard->day_detected_as_dead = -1;
  
  scorecard->min_next_vaccination_day = -1;
  
  scorecard->first_destruction_queue_day = -1;
}



gboolean
HSC_record_detection_as_diseased (HSC_scorecard_t *scorecard, int day)
{
  gboolean result = FALSE;

  if (!scorecard->is_detected_as_diseased)
    {
      scorecard->is_detected_as_diseased = TRUE;
      scorecard->day_detected_as_diseased = day;
      result = TRUE;
    }

  return result;
}



gboolean
HSC_record_detection_as_dead (HSC_scorecard_t *scorecard, int day)
{
  gboolean result = FALSE;

  if (!scorecard->is_detected_as_dead)
    {
      scorecard->is_detected_as_dead = TRUE;
      scorecard->day_detected_as_dead = day;
      result = TRUE;
    }

  return result;
}



void
HSC_record_next_vaccination_day (HSC_scorecard_t *scorecard, int next_vaccination_day) 
{
  scorecard->min_next_vaccination_day = next_vaccination_day;
} 

void 
HSC_record_first_destruction_queue_day (HSC_scorecard_t *scorecard, int day)
{
  scorecard->first_destruction_queue_day = day; 
}

/* end of file scorecard.c */
