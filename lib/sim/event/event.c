/** @file event.c
 * Functions for creating, destroying, printing, and manipulating events.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date March 2003
 *
 * Copyright &copy; University of Guelph, 2003-2009
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

/* To avoid name clashes when dlpreopening multiple modules that have the same
 * global symbols (interface).  See sec. 18.4 of "GNU Autoconf, Automake, and
 * Libtool". */
#define EVT_free_event_as_GFunc event_LTX_EVT_free_event_as_GFunc

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#include "naadsm.h"
#include "event.h"
#include "reporting.h"
#include "scorecard.h"
#include "model_util.h"



extern const char *HRD_status_name[];



/**
 * Names for the events of interest, terminated with a NULL sentinel.
 *
 * @sa EVT_event_type_t
 */
const char *EVT_event_type_name[] = {
  "BeforeAnySimulations",
  "BeforeEachSimulation",
  "DeclarationOfExposureCauses",
  "DeclarationOfDetectionMeans",
  "DeclarationOfExamReasons",
  "DeclarationOfTestReasons",
  "DeclarationOfVaccinationReasons",
  "DeclarationOfDestructionReasons",
  "DeclarationOfOutputs",
  "NewDay", "Exposure", "Infection", "Detection",
  "PublicAnnouncement", "Exam", "AttemptToTrace", "TraceResult", "Test",
  "TestResult",
  "RequestForVaccination", "CommitmentToVaccinate", "VaccinationCanceled",
  "Vaccination", "RequestForDestruction",
  "CommitmentToDestroy",
  "DestructionCanceled",
  "Destruction",
  "RequestForZoneFocus", "EndOfDay", "LastDay",
  "Midnight",
  "UnitStateChange",
  NULL
};



/**
 * Creates a new "before any simulations" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_before_any_simulations_event (void)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_BeforeAnySimulations;
  return event;
}



/**
 * Returns a text representation of a "before any simulations" event.
 *
 * @return a string.
 */
char *
EVT_before_any_simulations_event_to_string (void)
{
  return g_strdup ("<Before any simulations event>");
}



/**
 * Creates a new "before each simulation" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_before_each_simulation_event (void)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_BeforeEachSimulation;
  return event;
}



/**
 * Returns a text representation of a "before each simulation" event.
 *
 * @return a string.
 */
char *
EVT_before_each_simulation_event_to_string (void)
{
  return g_strdup ("<Before each simulation event>");
}



/**
 * Creates a new "declaration of exposure causes" event.
 *
 * @param causes an array of ordinary C strings giving the causes of exposures
 *   a model may create.  The pointer to the array is copied so the strings and
 *   the array structure itself should not be freed after calling this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_exposure_causes_event (GPtrArray * causes)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfExposureCauses;
  event->u.declaration_of_exposure_causes.causes = causes;
  return event;
}



/**
 * Returns a text representation of a declaration of exposure causes event.
 *
 * @param event a declaration of exposure causes event.
 * @return a string.
 */
char *
EVT_declaration_of_exposure_causes_event_to_string (EVT_declaration_of_exposure_causes_event_t *
                                                    event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of exposure causes event\n  causes=");
  for (i = 0; i < event->causes->len; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (event->causes, i));
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "declaration of detection means" event.
 *
 * @param means an array of ordinary C strings giving the means of detections
 *   a model may create.  The pointer to the array is copied so the strings and
 *   the array structure itself should not be freed after calling this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_detection_means_event (GPtrArray * means)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfDetectionMeans;
  event->u.declaration_of_detection_means.means = means;
  return event;
}



/**
 * Returns a text representation of a declaration of detection means event.
 *
 * @param event a declaration of detection means event.
 * @return a string.
 */
char *
EVT_declaration_of_detection_means_event_to_string (EVT_declaration_of_detection_means_event_t *
                                                    event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of detection means event\n  means=");
  for (i = 0; i < event->means->len; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (event->means, i));
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "declaration of exam reasons" event.
 *
 * @param reasons an array of ordinary C strings giving the reasons for which
 *   a module may request exams.  The pointer to the array is copied so the
 *   strings and the array structure itself should not be freed after calling
 *   this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_exam_reasons_event (GPtrArray * reasons)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfExamReasons;
  event->u.declaration_of_exam_reasons.reasons = reasons;
  return event;
}



/**
 * Returns a text representation of a declaration of exam reasons event.
 *
 * @param event a declaration of exam reasons event.
 * @return a string.
 */
char *
EVT_declaration_of_exam_reasons_event_to_string (EVT_declaration_of_exam_reasons_event_t * event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of exam reasons event\n  reasons=");
  for (i = 0; i < event->reasons->len; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (event->reasons, i));
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}


/**
 * Creates a new "declaration of test reasons" event.
 *
 * @param reasons an array of ordinary C strings giving the reasons for which
 *   a module may request tests.  The pointer to the array is copied so the
 *   strings and the array structure itself should not be freed after calling
 *   this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_test_reasons_event (GPtrArray * reasons)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfTestReasons;
  event->u.declaration_of_test_reasons.reasons = reasons;
  return event;
}



/**
 * Returns a text representation of a declaration of test reasons event.
 *
 * @param event a declaration of test reasons event.
 * @return a string.
 */
char *
EVT_declaration_of_test_reasons_event_to_string (EVT_declaration_of_test_reasons_event_t * event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of test reasons event\n  reasons=");
  for (i = 0; i < event->reasons->len; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (event->reasons, i));
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "declaration of vaccination reasons" event.
 *
 * @param reasons an array of ordinary C strings giving the reasons for which
 *   a model may request vaccinations.  The pointer to the array is copied so
 *   the strings and the array structure itself should not be freed after
 *   calling this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_vaccination_reasons_event (GPtrArray * reasons)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfVaccinationReasons;
  event->u.declaration_of_vaccination_reasons.reasons = reasons;
  return event;
}



/**
 * Returns a text representation of a declaration of vaccination reasons event.
 *
 * @param event a declaration of vaccination reasons event.
 * @return a string.
 */
char *
EVT_declaration_of_vaccination_reasons_event_to_string
  (EVT_declaration_of_vaccination_reasons_event_t * event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of vaccination reasons event\n  reasons=");
  for (i = 0; i < event->reasons->len; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (event->reasons, i));
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "declaration of destruction reasons" event.
 *
 * @param reasons an array of ordinary C strings giving the reasons for which
 *   a model may request destructions.  The pointer to the array is copied so
 *   the strings and the array structure itself should not be freed after
 *   calling this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_destruction_reasons_event (GPtrArray * reasons)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfDestructionReasons;
  event->u.declaration_of_destruction_reasons.reasons = reasons;
  return event;
}



/**
 * Returns a text representation of a declaration of destruction reasons event.
 *
 * @param event a declaration of destruction reasons event.
 * @return a string.
 */
char *
EVT_declaration_of_destruction_reasons_event_to_string
  (EVT_declaration_of_destruction_reasons_event_t * event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of destruction reasons event\n  reasons=");
  for (i = 0; i < event->reasons->len; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            (char *) g_ptr_array_index (event->reasons, i));
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "declaration of outputs" event.
 *
 * @param outputs an array of pointers to RPT_reporting_t objects.  A new
 *   GPtrArray is created, so the array structure can be freed after calling
 *   this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_outputs_event (GPtrArray * outputs)
{
  EVT_event_t *event;
  guint n, i;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfOutputs;
  event->u.declaration_of_outputs.outputs = g_ptr_array_new();
  n = outputs->len;
  for (i = 0; i < n; i++)
    g_ptr_array_add (event->u.declaration_of_outputs.outputs,
                     g_ptr_array_index (outputs, i));
  return event;
}



/**
 * Returns a text representation of a declaration of outputs event.
 *
 * @param event a declaration of outputs event.
 * @return a string.
 */
char *
EVT_declaration_of_outputs_event_to_string (EVT_declaration_of_outputs_event_t * event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of outputs event\n  outputs=");
  for (i = 0; i < event->outputs->len; i++)
    g_string_append_printf (s, i == 0 ? "\"%s\"" : ",\"%s\"",
                            ((RPT_reporting_t *) g_ptr_array_index (event->outputs, i))->name);
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "new day" event.
 *
 * @param day the day of the simulation.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_new_day_event (int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_NewDay;
  event->u.new_day.day = day;
  return event;
}



/**
 * Returns a text representation of a new day event.
 *
 * @param event a new day event.
 * @return a string.
 */
char *
EVT_new_day_event_to_string (EVT_new_day_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<New day event day=%hu>", event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "exposure" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_exposure_event (HRD_herd_t * exposing_herd, HRD_herd_t * exposed_herd,
                        int day, const char *cause, gboolean traceable,
                        gboolean adequate)
{
  EVT_event_t *event;
  
  event = g_new (EVT_event_t, 1);
  event->type = EVT_Exposure;
  event->u.exposure.exposing_herd = exposing_herd;
  event->u.exposure.exposed_herd = exposed_herd;
  event->u.exposure.day = day;
  event->u.exposure.cause = cause;
  event->u.exposure.traceable = traceable;
  event->u.exposure.traced = FALSE;
  event->u.exposure.adequate = adequate;
  /* The following three items cause this to be a normal infection, one that
   * starts from the start rather than being added to the simulation in-
   * progress. */
  event->u.exposure.override_initial_state = Latent;
  event->u.exposure.override_days_in_state = 0;
  event->u.exposure.override_days_left_in_state = 0;
  return event;
}



/**
 * Returns a text representation of an exposure event.
 *
 * @param event an exposure event.
 * @return a string.
 */
char *
EVT_exposure_event_to_string (EVT_exposure_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  if (event->exposing_herd == NULL)
    g_string_sprintf (s, "<Exposure event unit=\"%s\"",
                      event->exposed_herd->official_id);
  else
    g_string_sprintf (s, "<Exposure event units=\"%s\"->\"%s\"",
                      event->exposing_herd->official_id,
                      event->exposed_herd->official_id);
  g_string_append_printf (s, " (%s) day=%hu adequate=%i traceable=%i",
                          NAADSM_contact_type_name[event->contact_type],
                          event->day, event->adequate, event->traceable);
  if (event->override_initial_state > Susceptible)
    {
      g_string_append_printf (s, "\n start %s", HRD_status_name[event->override_initial_state]);

      if (event->override_days_in_state > 0)
        g_string_append_printf (s, " %i days elapsed", event->override_days_in_state);

      if (event->override_days_left_in_state > 0)
        g_string_append_printf (s, " %i days left", event->override_days_left_in_state);
    }
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "infection" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_infection_event (HRD_herd_t * herd, int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Infection;
  event->u.infection.herd = herd;
  event->u.infection.day = day;

  return event;
}



/**
 * Returns a text representation of an infection event.
 *
 * @param event an infection event.
 * @return a string.
 */
char *
EVT_infection_event_to_string (EVT_infection_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Infection event unit=\"%s\" day=%hu>",
                    event->herd->official_id, event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "detection" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_detection_event (HRD_herd_t * herd, int day,
                         NAADSM_detection_reason means,
                         const char * means_as_text,
                         NAADSM_test_result test_result)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Detection;
  event->u.detection.herd = herd;
  event->u.detection.day = day;
  event->u.detection.means = means;
  event->u.detection.means_as_text = means_as_text;
  event->u.detection.test_result = test_result;
  return event;
}



/**
 * Returns a text representation of a detection event.
 *
 * @param event a detection event.
 * @return a string.
 */
char *
EVT_detection_event_to_string (EVT_detection_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Detection event unit=\"%s\" day=%hu>", event->herd->official_id,
                    event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "public announcement" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_public_announcement_event (int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_PublicAnnouncement;
  event->u.public_announcement.day = day;
  return event;
}



/**
 * Returns a text representation of a public announcement event.
 *
 * @param event a public announcement event.
 * @return a string.
 */
char *
EVT_public_announcement_event_to_string (EVT_public_announcement_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Public announcement event day=%hu>", event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "exam" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_exam_event (HRD_herd_t * herd, int day, char * reason,
                    double detection_multiplier, gboolean test_if_no_signs)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Exam;
  event->u.exam.herd = herd;
  event->u.exam.day = day;
  event->u.exam.reason = reason;
  event->u.exam.detection_multiplier = detection_multiplier;
  event->u.exam.test_if_no_signs = test_if_no_signs;
  return event;
}



/**
 * Returns a text representation of an exam event.
 *
 * @param event an exam event.
 * @return a string.
 */
char *
EVT_exam_event_to_string (EVT_exam_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Exam event unit=\"%s\" day=%hu detection multiplier=%g test if no signs=%s>",
                    event->herd->official_id, event->day,
                    event->detection_multiplier,
                    event->test_if_no_signs ? "yes" : "no");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "attempt to trace" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_attempt_to_trace_event (HRD_herd_t * herd, int day,
                                NAADSM_contact_type contact_type,
                                NAADSM_trace_direction direction,
                                int trace_period)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_AttemptToTrace;
  event->u.attempt_to_trace.herd = herd;
  event->u.attempt_to_trace.day = day;
  event->u.attempt_to_trace.contact_type = contact_type;
  event->u.attempt_to_trace.direction = direction;
  event->u.attempt_to_trace.trace_period = trace_period;
  return event;
}



/**
 * Returns a text representation of an attempt to trace event.
 *
 * @param event an attempt to trace event.
 * @return a string.
 */
char *
EVT_attempt_to_trace_event_to_string (EVT_attempt_to_trace_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Attempt to trace event unit=\"%s\" (%s) day=%hu %s direction=%s>",
                    event->herd->official_id,
                    event->herd->production_type_name,
                    event->day,
                    NAADSM_contact_type_abbrev[event->contact_type],
                    NAADSM_trace_direction_abbrev[event->direction]);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "trace result" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_trace_result_event (HRD_herd_t * exposing_herd,
                            HRD_herd_t * exposed_herd,
                            NAADSM_contact_type contact_type,
                            NAADSM_trace_direction direction,
                            int day, int initiated_day, gboolean traced)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_TraceResult;
  event->u.trace_result.exposing_herd = exposing_herd;
  event->u.trace_result.exposed_herd = exposed_herd;
  event->u.trace_result.contact_type = contact_type;
  event->u.trace_result.direction = direction;
  event->u.trace_result.day = day;
  event->u.trace_result.initiated_day = initiated_day;
  event->u.trace_result.traced = traced;
  return event;
}



/**
 * Returns a text representation of a trace result event.
 *
 * @param event a trace result event.
 * @return a string.
 */
char *
EVT_trace_result_event_to_string (EVT_trace_result_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Trace result event units=\"%s\" (%s)->\"%s\" (%s) day=%hu initiated_day=%hu %s direction=%s trace=%s>",
                    event->exposing_herd->official_id,
                    event->exposing_herd->production_type_name,
                    event->exposed_herd->official_id,
                    event->exposed_herd->production_type_name,
                    event->day,
                    event->initiated_day,
                    NAADSM_contact_type_abbrev[event->contact_type],
                    NAADSM_trace_direction_abbrev[event->direction],
                    event->traced == TRUE ? "succeeded" : "failed");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "test" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_test_event (HRD_herd_t * herd, int day, char * reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Test;
  event->u.test.herd = herd;
  event->u.test.day = day;
  event->u.test.reason = reason;
  return event;
}



/**
 * Returns a text representation of a test event.
 *
 * @param event a test event.
 * @return a string.
 */
char *
EVT_test_event_to_string (EVT_test_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Test event unit=\"%s\" day=%hu>",
		    event->herd->official_id, event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "test result" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_test_result_event (HRD_herd_t * herd, int day,
                           gboolean positive, gboolean correct, char *reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_TestResult;
  event->u.test_result.herd = herd;
  event->u.test_result.day = day;
  event->u.test_result.positive = positive;
  event->u.test_result.correct = correct;
  event->u.test_result.reason = reason;
  return event;
}



/**
 * Returns a text representation of a test result event.
 *
 * @param event a test result event.
 * @return a string.
 */
char *
EVT_test_result_event_to_string (EVT_test_result_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Test result event unit=\"%s\" day=%hu %s %s>",
                    event->herd->official_id, event->day,
                    event->correct ? "true" : "false",
                    event->positive ? "positive" : "negative");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "request for vaccination" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_vaccination_event (HRD_herd_t * herd,
                                       int day,
                                       char *reason,
                                       int priority,
                                       gboolean cancel_on_detection,
                                       int min_days_before_next)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForVaccination;
  event->u.request_for_vaccination.herd = herd;
  event->u.request_for_vaccination.day = day;
  event->u.request_for_vaccination.reason = reason;
  event->u.request_for_vaccination.priority = priority;
  event->u.request_for_vaccination.cancel_on_detection = cancel_on_detection;
  event->u.request_for_vaccination.min_days_before_next = min_days_before_next;
  event->u.request_for_vaccination.day_commitment_made = 0; /* default */
  return event;
}



/**
 * Returns a text representation of a request for vaccination event.
 *
 * @param event a request for vaccination event.
 * @return a string.
 */
char *EVT_request_for_vaccination_event_to_string (EVT_request_for_vaccination_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Request for vaccination event unit=\"%s\" day=%hu priority=%hu cancel on detection=%s>",
                    event->herd->official_id, event->day, event->priority,
                    event->cancel_on_detection ? "yes" : "no");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "commitment to vaccinate" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_commitment_to_vaccinate_event (HRD_herd_t * herd, int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_CommitmentToVaccinate;
  event->u.commitment_to_vaccinate.herd = herd;
  event->u.commitment_to_vaccinate.day = day;
  return event;
}



/**
 * Returns a text representation of a commitment to vaccinate event.
 *
 * @param event a commitment to vaccinate event.
 * @return a string.
 */
char *EVT_commitment_to_vaccinate_event_to_string (EVT_commitment_to_vaccinate_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Commitment to vaccinate event unit=\"%s\" day=%hu>",
                    event->herd->official_id, event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "vaccination canceled" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_vaccination_canceled_event (HRD_herd_t * herd, int day,
                                    int day_commitment_made)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_VaccinationCanceled;
  event->u.vaccination_canceled.herd = herd;
  event->u.vaccination_canceled.day = day;
  event->u.vaccination_canceled.day_commitment_made = day_commitment_made;
  return event;
}



/**
 * Returns a text representation of a vaccination canceled event.
 *
 * @param event a vaccination canceled event.
 * @return a string.
 */
char *
EVT_vaccination_canceled_event_to_string (EVT_vaccination_canceled_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Vaccination canceled event unit=\"%s\" day=%hu>",
                    event->herd->official_id, event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}




/**
 * Creates a new "vaccination" event for setting a herd's immunity state
 * in-progress.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_inprogress_immunity_event (HRD_herd_t * herd,
                                   int day, char *reason,
                                   HRD_status_t start_in_state,
                                   int days_in_state, int days_left_in_state)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Vaccination;
  event->u.vaccination.herd = herd;
  event->u.vaccination.day = day;
  event->u.vaccination.reason = reason;
  event->u.vaccination.day_commitment_made = day;
  event->u.vaccination.override_initial_state = start_in_state;
  event->u.vaccination.override_days_in_state = days_in_state;
  event->u.vaccination.override_days_left_in_state = days_left_in_state;
  event->u.vaccination.fast_forward = 0;

  return event;
}



/**
 * Creates a new "vaccination" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_vaccination_event (HRD_herd_t * herd, int day,
                           const char *reason,
                           int day_commitment_made)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Vaccination;
  event->u.vaccination.herd = herd;
  event->u.vaccination.day = day;
  event->u.vaccination.reason = (char *) reason;
  event->u.vaccination.day_commitment_made = day_commitment_made;
  /* The following four items cause this to be a normal immunity, one that
   * starts from the start rather than being added to the simulation in-
   * progress. */
  event->u.vaccination.override_initial_state = Susceptible;
  event->u.vaccination.override_days_in_state = 0;
  event->u.vaccination.override_days_left_in_state = 0;
  event->u.vaccination.fast_forward = 0;
  return event;
}



/**
 * Returns a text representation of a vaccination event.
 *
 * @param event a vaccination event.
 * @return a string.
 */
char *
EVT_vaccination_event_to_string (EVT_vaccination_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Vaccination event unit=\"%s\" day=%hu",
                    event->herd->official_id, event->day);
  if (event->override_initial_state == VaccineImmune)
    {
      g_string_append_printf (s, "\n start %s", HRD_status_name[event->override_initial_state]);

      if (event->override_days_in_state > 0)
        g_string_append_printf (s, " %i days elapsed", event->override_days_in_state);

      if (event->override_days_left_in_state > 0)
        g_string_append_printf (s, " %i days left", event->override_days_left_in_state);
    }
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "request for destruction" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_destruction_event (HRD_herd_t * herd,
                                       int day,
                                       char *reason, int priority)
{
  EVT_event_t *event;
  HSC_scorecard_t *scorecard;
  int first_queue_day;
  
  scorecard = naadsm_get_or_create_scorecard( herd );

  if( -1 == scorecard->first_destruction_queue_day )
    HSC_record_first_destruction_queue_day( scorecard, day );
  
  first_queue_day = scorecard->first_destruction_queue_day;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForDestruction;
  event->u.request_for_destruction.herd = herd;
  event->u.request_for_destruction.day = first_queue_day;
  event->u.request_for_destruction.reason = reason;
  event->u.request_for_destruction.priority = priority;
  event->u.request_for_destruction.day_commitment_made = 0; /* default */
  return event;
}



/**
 * Returns a text representation of a request for destruction event.
 *
 * @param event a request for destruction event.
 * @return a string.
 */
char *EVT_request_for_destruction_event_to_string (EVT_request_for_destruction_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<Request for destruction event unit=\"%s\" day=%hu priority=%hu>",
                    event->herd->official_id, event->day, event->priority);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "commitment to destroy" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_commitment_to_destroy_event (HRD_herd_t * herd, int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_CommitmentToDestroy;
  event->u.commitment_to_destroy.herd = herd;
  event->u.commitment_to_destroy.day = day;
  return event;
}



/**
 * Returns a text representation of a commitment to destroy event.
 *
 * @param event a commitment to destroy event.
 * @return a string.
 */
char *
EVT_commitment_to_destroy_event_to_string (EVT_commitment_to_destroy_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Commitment to destroy event unit=\"%s\" day=%hu>",
                    event->herd->official_id, event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "destruction canceled" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_destruction_canceled_event (HRD_herd_t * herd, int day,
                                    int day_commitment_made)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DestructionCanceled;
  event->u.destruction_canceled.herd = herd;
  event->u.destruction_canceled.day = day;
  event->u.destruction_canceled.day_commitment_made = day_commitment_made;
  return event;
}



/**
 * Returns a text representation of a destruction canceled event.
 *
 * @param event a destruction canceled event.
 * @return a string.
 */
char *
EVT_destruction_canceled_event_to_string (EVT_destruction_canceled_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Destruction canceled event unit=\"%s\" day=%hu>",
                    event->herd->official_id, event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "destruction" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_destruction_event (HRD_herd_t * herd, int day, char *reason,
                           int day_commitment_made)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Destruction;
  event->u.destruction.herd = herd;
  event->u.destruction.day = day;
  event->u.destruction.reason = reason;
  event->u.destruction.day_commitment_made = day_commitment_made;
  return event;
}



/**
 * Returns a text representation of a destruction event.
 *
 * @param event a destruction event.
 * @return a string.
 */
char *
EVT_destruction_event_to_string (EVT_destruction_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Destruction event unit=\"%s\" day=%hu>", event->herd->official_id,
                    event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "request for zone focus" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_zone_focus_event (HRD_herd_t * herd, int day, char *reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForZoneFocus;
  event->u.request_for_zone_focus.herd = herd;
  event->u.request_for_zone_focus.day = day;
  event->u.request_for_zone_focus.reason = reason;
  return event;
}



/**
 * Returns a text representation of a request for zone focus event.
 *
 * @param event a request for zone focus event.
 * @return a string.
 */
char *
EVT_request_for_zone_focus_event_to_string (EVT_request_for_zone_focus_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<Request for zone focus event unit=\"%s\" day=%hu>",
                    event->herd->official_id, event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "end of day" event.
 *
 * @param day the day of the simulation.
 * @param done whether the simulation is over or not.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_end_of_day_event (int day, gboolean done)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_EndOfDay;
  event->u.end_of_day.day = day;
  event->u.end_of_day.done = done;
  return event;
}



/**
 * Returns a text representation of an end of day event.
 *
 * @param event an end of day event.
 * @return a string.
 */
char *
EVT_end_of_day_event_to_string (EVT_end_of_day_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<End of day event day=%hu>", event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "last day" event.
 *
 * @param day the day of the simulation.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_last_day_event (int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_LastDay;
  event->u.last_day.day = day;
  return event;
}



/**
 * Returns a text representation of a last day event.
 *
 * @param event a last day event.
 * @return a string.
 */
char *
EVT_last_day_event_to_string (EVT_last_day_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Last day event day=%hu>", event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "midnight" event.
 *
 * @param day the day of the simulation.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_midnight_event (int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Midnight;
  event->u.midnight.day = day;
  return event;
}



/**
 * Returns a text representation of a midnight event.
 *
 * @param event a midnight event.
 * @return a string.
 */
char *
EVT_midnight_event_to_string (EVT_midnight_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Midnight event day=%hu>", event->day);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "unit state change" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_unit_state_change_event (HRD_herd_t * herd,
                                 HRD_status_t old_state,
                                 HRD_status_t new_state,
                                 int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_UnitStateChange;
  event->u.unit_state_change.herd = herd;
  event->u.unit_state_change.old_state = old_state;
  event->u.unit_state_change.new_state = new_state;
  event->u.unit_state_change.day = day;

  return event;
}



/**
 * Returns a text representation of a unit state change event.
 *
 * @param event a unit state change event.
 * @return a string.
 */
char *
EVT_unit_state_change_event_to_string (EVT_unit_state_change_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Unit state change event unit=\"%s\" %s->%s day=%i>",
                    event->herd->official_id,
                    HRD_status_name[event->old_state],
                    HRD_status_name[event->new_state],
                    event->day);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Deletes an event from memory.
 *
 * @param event an event.
 */
void
EVT_free_event (EVT_event_t * event)
{
  if (event == NULL)
    return;

  switch (event->type)
    {
    case EVT_BeforeAnySimulations:
    case EVT_BeforeEachSimulation:
    case EVT_NewDay:
    case EVT_Exposure:
    case EVT_Infection:
    case EVT_Detection:
    case EVT_PublicAnnouncement:
    case EVT_Exam:
    case EVT_AttemptToTrace:
    case EVT_TraceResult:
    case EVT_Test:
    case EVT_TestResult:
    case EVT_RequestForVaccination:
    case EVT_CommitmentToVaccinate:
    case EVT_VaccinationCanceled:
    case EVT_Vaccination:
    case EVT_RequestForDestruction:
    case EVT_CommitmentToDestroy:
    case EVT_DestructionCanceled:
    case EVT_Destruction:
    case EVT_RequestForZoneFocus:
    case EVT_EndOfDay:
    case EVT_LastDay:
    case EVT_Midnight:
    case EVT_UnitStateChange:
      /* No dynamically-allocated parts to free. */
      break;
    case EVT_DeclarationOfExposureCauses:
      /* Note that we do not free the C strings in the array of exposure
       * causes, because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_exposure_causes.causes, TRUE);
      break;
    case EVT_DeclarationOfDetectionMeans:
      /* Note that we do not free the C strings in the array of detection means,
       * because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_detection_means.means, TRUE);
      break;
    case EVT_DeclarationOfExamReasons:
      /* Note that we do not free the C strings in the array of exam reasons,
       * because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_exam_reasons.reasons, TRUE);
      break;
    case EVT_DeclarationOfTestReasons:
      /* Note that we do not free the C strings in the array of test reasons,
       * because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_test_reasons.reasons, TRUE);
      break;
    case EVT_DeclarationOfVaccinationReasons:
      /* Note that we do not free the C strings in the array of vaccination
       * reasons, because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_vaccination_reasons.reasons, TRUE);
      break;
    case EVT_DeclarationOfDestructionReasons:
      /* Note that we do not free the C strings in the array of destruction
       * reasons, because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_destruction_reasons.reasons, TRUE);
      break;
    case EVT_DeclarationOfOutputs:
      /* Note that we free the GPtrArray structure that holds the list of
       * reporting variables, but we do not free the reporting variables
       * themselves.  We assume that will be done when the module owning those
       * reporting variables is freed. */
      g_ptr_array_free (event->u.declaration_of_outputs.outputs, TRUE);
      break;
    case EVT_NEVENT_TYPES:
      /* This avoids a compiler warning on Windows. */
      break;
    }
  g_free (event);
}



/**
 * Makes a deep copy of an event.
 *
 * @param event an event.
 * @return a deep copy of the event.
 */
EVT_event_t *
EVT_clone_event (EVT_event_t * event)
{
  EVT_event_t *clone;

  if (event == NULL)
    return NULL;

  switch (event->type)
    {
    case EVT_Exposure:
      {
        EVT_exposure_event_t *e;
        e = &(event->u.exposure);
        clone = EVT_new_exposure_event (e->exposing_herd, e->exposed_herd,
                                        e->day, e->cause, e->traceable, e->adequate);
        clone->u.exposure.contact_type = e->contact_type;
        clone->u.exposure.override_initial_state = e->override_initial_state;
        clone->u.exposure.override_days_in_state = e->override_days_in_state;
        clone->u.exposure.override_days_left_in_state = e->override_days_left_in_state;
        break;
      }
    case EVT_RequestForVaccination:
      {
        EVT_request_for_vaccination_event_t *e;
        e = &(event->u.request_for_vaccination);
        clone = EVT_new_request_for_vaccination_event (e->herd, e->day,
                                                       e->reason, e->priority,
                                                       e->cancel_on_detection,
                                                       e->min_days_before_next);
        break;
      }
    case EVT_Vaccination:
      {
        EVT_vaccination_event_t *e;
        e = &(event->u.vaccination);
        clone = EVT_new_vaccination_event (e->herd, e->day, e->reason, e->day_commitment_made);
        clone->u.vaccination.override_initial_state = e->override_initial_state;
        clone->u.vaccination.override_days_in_state = e->override_days_in_state;
        clone->u.vaccination.override_days_left_in_state = e->override_days_left_in_state;
        clone->u.vaccination.fast_forward = e->fast_forward;
        break;
      }
    case EVT_RequestForDestruction:
      {
        EVT_request_for_destruction_event_t *e;
        e = &(event->u.request_for_destruction);
        clone = EVT_new_request_for_destruction_event (e->herd, e->day, e->reason, e->priority);
        break;
      }
    case EVT_Destruction:
      {
        EVT_destruction_event_t *e;
        e = &(event->u.destruction);
        clone = EVT_new_destruction_event (e->herd, e->day, e->reason, e->day_commitment_made);
        break;
      }
    default:
      g_assert_not_reached ();
    }

  return clone;
}



/**
 * Wraps EVT_free_event so that it can be used in GLib calls.
 *
 * @param data a pointer to an EVT_event_t structure, but cast to a gpointer.
 * @param user_data not used, pass NULL.
 */
void
EVT_free_event_as_GFunc (gpointer data, gpointer user_data)
{
  EVT_free_event ((EVT_event_t *) data);
}



/**
 * Returns a text representation of an event.
 *
 * @param event an event.
 * @return a string.
 */
char *
EVT_event_to_string (EVT_event_t * event)
{
  char *s;

  switch (event->type)
    {
    case EVT_BeforeAnySimulations:
      s = EVT_before_any_simulations_event_to_string ();
      break;
    case EVT_BeforeEachSimulation:
      s = EVT_before_each_simulation_event_to_string ();
      break;
    case EVT_DeclarationOfExposureCauses:
      s =
        EVT_declaration_of_exposure_causes_event_to_string (&
                                                            (event->u.
                                                             declaration_of_exposure_causes));
      break;
    case EVT_DeclarationOfDetectionMeans:
      s =
        EVT_declaration_of_detection_means_event_to_string (&(event->u.declaration_of_detection_means));
      break;
    case EVT_DeclarationOfExamReasons:
      s =
        EVT_declaration_of_exam_reasons_event_to_string (&(event->u.declaration_of_exam_reasons));
      break;
    case EVT_DeclarationOfTestReasons:
      s =
        EVT_declaration_of_test_reasons_event_to_string (&(event->u.declaration_of_test_reasons));
      break;
    case EVT_DeclarationOfVaccinationReasons:
      s =
        EVT_declaration_of_vaccination_reasons_event_to_string (&
                                                                (event->u.
                                                                 declaration_of_vaccination_reasons));
      break;
    case EVT_DeclarationOfDestructionReasons:
      s =
        EVT_declaration_of_destruction_reasons_event_to_string (&
                                                                (event->u.
                                                                 declaration_of_destruction_reasons));
      break;
    case EVT_DeclarationOfOutputs:
      s = EVT_declaration_of_outputs_event_to_string (&(event->u.declaration_of_outputs));
      break;
    case EVT_NewDay:
      s = EVT_new_day_event_to_string (&(event->u.new_day));
      break;
    case EVT_Exposure:
      s = EVT_exposure_event_to_string (&(event->u.exposure));
      break;
    case EVT_Infection:
      s = EVT_infection_event_to_string (&(event->u.infection));
      break;
    case EVT_Detection:
      s = EVT_detection_event_to_string (&(event->u.detection));
      break;
    case EVT_PublicAnnouncement:
      s = EVT_public_announcement_event_to_string (&(event->u.public_announcement));
      break;
    case EVT_Exam:
      s = EVT_exam_event_to_string (&(event->u.exam));
      break;
    case EVT_AttemptToTrace:
      s = EVT_attempt_to_trace_event_to_string (&(event->u.attempt_to_trace));
      break;
    case EVT_TraceResult:
      s = EVT_trace_result_event_to_string (&(event->u.trace_result));
      break;
    case EVT_Test:
      s = EVT_test_event_to_string (&(event->u.test));
      break;
    case EVT_TestResult:
      s = EVT_test_result_event_to_string (&(event->u.test_result));
      break;
    case EVT_RequestForVaccination:
      s = EVT_request_for_vaccination_event_to_string (&(event->u.request_for_vaccination));
      break;
    case EVT_CommitmentToVaccinate:
      s = EVT_commitment_to_vaccinate_event_to_string (&(event->u.commitment_to_vaccinate));
      break;
    case EVT_VaccinationCanceled:
      s = EVT_vaccination_canceled_event_to_string (&(event->u.vaccination_canceled));
      break;
    case EVT_Vaccination:
      s = EVT_vaccination_event_to_string (&(event->u.vaccination));
      break;
    case EVT_RequestForDestruction:
      s = EVT_request_for_destruction_event_to_string (&(event->u.request_for_destruction));
      break;
    case EVT_CommitmentToDestroy:
      s = EVT_commitment_to_destroy_event_to_string (&(event->u.commitment_to_destroy));
      break;
    case EVT_DestructionCanceled:
      s = EVT_destruction_canceled_event_to_string (&(event->u.destruction_canceled));
      break;
    case EVT_Destruction:
      s = EVT_destruction_event_to_string (&(event->u.destruction));
      break;
    case EVT_RequestForZoneFocus:
      s = EVT_request_for_zone_focus_event_to_string (&(event->u.request_for_zone_focus));
      break;
    case EVT_EndOfDay:
      s = EVT_end_of_day_event_to_string (&(event->u.end_of_day));
      break;
    case EVT_LastDay:
      s = EVT_last_day_event_to_string (&(event->u.last_day));
      break;
    case EVT_Midnight:
      s = EVT_midnight_event_to_string (&(event->u.midnight));
      break;
    case EVT_UnitStateChange:
      s = EVT_unit_state_change_event_to_string (&(event->u.unit_state_change));
      break;
    default:
      g_assert_not_reached ();
    }

  return s;
}



/**
 * Prints an event.
 *
 * @param stream a stream to write to.  If NULL, defaults to stdout.
 * @param event an event.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
EVT_fprintf_event (FILE * stream, EVT_event_t * event)
{
  char *s;
  int nchars_written = 0;

  s = EVT_event_to_string (event);
  nchars_written = fprintf (stream ? stream : stdout, "%s", s);
  free (s);
  return nchars_written;
}



/**
 * Creates a new, empty event queue.
 *
 * @return a pointer to a newly-created event queue.
 */
EVT_event_queue_t *
EVT_new_event_queue (void)
{
  EVT_event_queue_t *queue;
  
  queue = g_new(EVT_event_queue_t, 1);
  queue->current_wave = g_ptr_array_new();
  queue->next_wave = g_ptr_array_new();
  return queue;
}



/**
 * Retrieves the next event from an event queue.  Returns NULL if the queue is
 * empty.
 *
 * @param queue an event queue.
 * @return an event.
 */
EVT_event_t *
EVT_event_dequeue (EVT_event_queue_t *queue, RAN_gen_t *rng)
{
  EVT_event_t *event;
  GPtrArray *tmp;

  if (queue->current_wave->len == 0)
    {
#if DEBUG
      g_debug ("next wave");
#endif
      tmp = queue->current_wave;
      queue->current_wave = queue->next_wave;
      queue->next_wave = tmp;
    }  
  if (queue->current_wave->len == 0)
    event = NULL;
  else
    {
      guint i;
      /* Pick one randomly */
      i = (guint) floor (RAN_num(rng) * (queue->current_wave->len));
      event = (EVT_event_t *) g_ptr_array_remove_index_fast (queue->current_wave, i);
    }

  return event;
}



/**
 * Deletes an event queue from memory.
 *
 * @param queue an event queue.
 */
void
EVT_free_event_queue (EVT_event_queue_t * queue)
{
  if (queue == NULL)
    return;

  g_ptr_array_foreach (queue->current_wave, EVT_free_event_as_GFunc, NULL);
  g_ptr_array_free (queue->current_wave, TRUE);
  g_ptr_array_foreach (queue->next_wave, EVT_free_event_as_GFunc, NULL);
  g_ptr_array_free (queue->next_wave, TRUE);
  g_free (queue);
}



/**
 * Returns a text representation of an event queue.
 *
 * @param queue an event queue.
 * @return a string.
 */
char *
EVT_event_queue_to_string (EVT_event_queue_t * queue)
{
  guint i, n;                  /* iterator over events in the list */
  EVT_event_t *event;
  GString *s;
  char *substring, *chararray;

  s = g_string_new ("<event queue (starting with next)=\n");

  n = queue->current_wave->len;
  for (i = 0; i < n; i++)
    {
      event = (EVT_event_t *) g_ptr_array_index (queue->current_wave, i);
      substring = EVT_event_to_string (event);
      g_string_append (s, substring);
      g_string_append_c (s, '\n');
      g_free (substring);
    }
  n = queue->next_wave->len;
  for (i = 0; i < n; i++)
    {
      event = (EVT_event_t *) g_ptr_array_index (queue->next_wave, i);
      substring = EVT_event_to_string (event);
      g_string_append (s, substring);
      g_string_append_c (s, '\n');
      g_free (substring);
    }
  g_string_append (s, ">");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Prints an event queue.
 *
 * @param stream a stream to write to.
 * @param queue an event queue.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
EVT_fprintf_event_queue (FILE * stream, EVT_event_queue_t * queue)
{
  char *s;
  int nchars_written;

  s = EVT_event_queue_to_string (queue);
  nchars_written = fprintf (stream, "%s", s);
  free (s);
  return nchars_written;
}

/* end of file event.c */
