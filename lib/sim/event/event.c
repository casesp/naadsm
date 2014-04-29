/** @file event.c
 * Functions for creating, destroying, printing, and manipulating events.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date March 2003
 *
 * Copyright &copy; University of Guelph, 2003-2008
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when dlpreopening multiple modules that have the same
 * global symbols (interface).  See sec. 18.4 of "GNU Autoconf, Automake, and
 * Libtool". */
#define EVT_free_event_as_GFunc event_LTX_EVT_free_event_as_GFunc

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#include "event.h"



extern const char *HRD_status_name[];



/**
 * Names for the events of interest, terminated with a NULL sentinel.
 *
 * @sa EVT_event_type_t
 */
const char *EVT_event_type_name[] = {
  "RequestForExposureCauses", "DeclarationOfExposureCauses",
  "RequestForInfectionCauses", "DeclarationOfInfectionCauses",
  "RequestForVaccinationReasons", "DeclarationOfVaccinationReasons",
  "RequestForVaccineDelay", "DeclarationOfVaccineDelay",
  "RequestForDestructionReasons", "DeclarationOfDestructionReasons",
  "NewDay", "Exposure", "AttemptToInfect", "Infection", "Detection",
  "PublicAnnouncement", "AttemptToTrace", "TraceResult",
  "RequestForVaccination", "CommitmentToVaccinate", "AttemptToVaccinate",
  "Vaccination", "RequestForDestruction", "CommitmentToDestroy",
  "AttemptToDestroy", "Destruction", "RequestForZoneFocus", "EndOfDay",
  "LastDay", NULL
};



/**
 * Names for the ways one herd may contact another, terminated with a NULL
 * sentinel.
 *
 * @sa EVT_contact_type_t
 */
const char *EVT_contact_type_name[] = {
  "Unknown", "Direct Contact", "Indirect Contact", NULL
};



/**
 * Creates a new "request for exposure causes" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_exposure_causes_event (void)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForExposureCauses;
  return event;
}



/**
 * Returns a text representation of a request for exposure causes event.
 *
 * @param event a request for exposure causes event.
 * @return a string.
 */
char *
EVT_request_for_exposure_causes_event_to_string (EVT_request_for_exposure_causes_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new ("<Request for exposure causes event>");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
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
 * Creates a new "request for infection causes" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_infection_causes_event (void)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForInfectionCauses;
  return event;
}



/**
 * Returns a text representation of a request for infection causes event.
 *
 * @param event a request for infection causes event.
 * @return a string.
 */
char *
EVT_request_for_infection_causes_event_to_string (EVT_request_for_infection_causes_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new ("<Request for infection causes event>");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "declaration of infection causes" event.
 *
 * @param causes an array of ordinary C strings giving the causes of infections
 *   a model may create.  The pointer to the array is copied so the strings and
 *   the array structure itself should not be freed after calling this function.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_infection_causes_event (GPtrArray * causes)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfInfectionCauses;
  event->u.declaration_of_infection_causes.causes = causes;
  return event;
}



/**
 * Returns a text representation of a declaration of infection causes event.
 *
 * @param event a declaration of infection causes event.
 * @return a string.
 */
char *
EVT_declaration_of_infection_causes_event_to_string (EVT_declaration_of_infection_causes_event_t *
                                                     event)
{
  GString *s;
  char *chararray;
  int i;

  s = g_string_new ("<Declaration of infection causes event\n  causes=");
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
 * Creates a new "request for vaccination reasons" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_vaccination_reasons_event (void)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForVaccinationReasons;
  return event;
}



/**
 * Returns a text representation of a request for vaccination reasons event.
 *
 * @param event a request for vaccination reasons event.
 * @return a string.
 */
char *
EVT_request_for_vaccination_reasons_event_to_string (EVT_request_for_vaccination_reasons_event_t *
                                                     event)
{
  GString *s;
  char *chararray;

  s = g_string_new ("<Request for vaccination reasons event>");
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
 * Creates a new "request for vaccine delay" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_vaccine_delay_event (void)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForVaccineDelay;
  return event;
}



/**
 * Returns a text representation of a request for vaccine delay event.
 *
 * @param event a request for vaccine delay event.
 * @return a string.
 */
char *
EVT_request_for_vaccine_delay_event_to_string (EVT_request_for_vaccine_delay_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new ("<Request for vaccine delay event>");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "declaration of vaccine delay" event.
 *
 * @param production_type the production type this delay applies to.
 * @param production_type_name the production type as a string.
 * @param delay the number of days between being vaccinated and becoming
 *   immune.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_declaration_of_vaccine_delay_event (HRD_production_type_t production_type,
                                            char * production_type_name,
                                            unsigned short int delay)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_DeclarationOfVaccineDelay;
  event->u.declaration_of_vaccine_delay.production_type = production_type;
  event->u.declaration_of_vaccine_delay.production_type_name = production_type_name;
  event->u.declaration_of_vaccine_delay.delay = delay;
  return event;
}



/**
 * Returns a text representation of a declaration of vaccine delay event.
 *
 * @param event a declaration of vaccine delay event.
 * @return a string.
 */
char *
EVT_declaration_of_vaccine_delay_event_to_string
  (EVT_declaration_of_vaccine_delay_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_printf (s, "<Declaration of vaccine delay event for \"%s\" delay=%hu>",
                   event->production_type_name, event->delay);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "request for destruction reasons" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_request_for_destruction_reasons_event (void)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForDestructionReasons;
  return event;
}



/**
 * Returns a text representation of a request for destruction reasons event.
 *
 * @param event a request for destruction reasons event.
 * @return a string.
 */
char *
EVT_request_for_destruction_reasons_event_to_string (EVT_request_for_destruction_reasons_event_t *
                                                     event)
{
  GString *s;
  char *chararray;

  s = g_string_new ("<Request for destruction reasons event>");
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
 * Creates a new "new day" event.
 *
 * @param day the day of the simulation.
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_new_day_event (unsigned short int day)
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
                        unsigned short int day, char *cause, gboolean traceable)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Exposure;
  event->u.exposure.exposing_herd = exposing_herd;
  event->u.exposure.exposed_herd = exposed_herd;
  event->u.exposure.day = day;
  event->u.exposure.cause = cause;
  event->u.exposure.traceable = traceable;
  event->u.exposure.contact_type = UnknownContact;
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
  g_string_sprintf (s, "<Exposure event units=\"%s\"->\"%s\" (%s) day=%hu traceable=%i>",
                    event->exposing_herd->official_id,
                    event->exposed_herd->official_id,
                    EVT_contact_type_name[event->contact_type], event->day, event->traceable);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "attempt to infect" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_attempt_to_infect_event (HRD_herd_t * infecting_herd,
                                 HRD_herd_t * infected_herd, unsigned short int day, char *cause)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_AttemptToInfect;
  event->u.attempt_to_infect.infecting_herd = infecting_herd;
  event->u.attempt_to_infect.infected_herd = infected_herd;
  event->u.attempt_to_infect.day = day;
  event->u.attempt_to_infect.cause = cause;
  /* The following two items cause this to be a normal infection, one that
   * starts from the start rather than being added to the simulation in-
   * progress. */
  event->u.attempt_to_infect.override_initial_state = Latent;
  event->u.attempt_to_infect.override_days_left_in_state = -1;

  return event;
}



/**
 * Creates a new "attempt to infect" event for setting a herd's disease state
 * in-progress.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_inprogress_infection_event (HRD_herd_t * infecting_herd,
                                    HRD_herd_t * infected_herd,
                                    unsigned short int day, char *cause,
                                    HRD_status_t start_in_state, int days_left_in_state)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_AttemptToInfect;
  event->u.attempt_to_infect.infecting_herd = infecting_herd;
  event->u.attempt_to_infect.infected_herd = infected_herd;
  event->u.attempt_to_infect.day = day;
  event->u.attempt_to_infect.cause = cause;
  event->u.attempt_to_infect.override_initial_state = start_in_state;
  event->u.attempt_to_infect.override_days_left_in_state = days_left_in_state;

  return event;
}



/**
 * Returns a text representation of an attempt to infect event.
 *
 * @param event an attempt to infect event.
 * @return a string.
 */
char *
EVT_attempt_to_infect_event_to_string (EVT_attempt_to_infect_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Attempt to infect event unit=\"%s\" day=%hu",
                    event->infected_herd->official_id, event->day);
  if (event->override_initial_state > Susceptible)
    {
      g_string_append_printf (s, "\n start %s", HRD_status_name[event->override_initial_state]);

      if (event->override_days_left_in_state > 0)
        g_string_append_printf (s, " (%i days left) ", event->override_days_left_in_state);
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
EVT_new_infection_event (HRD_herd_t * infecting_herd, HRD_herd_t * infected_herd,
                         unsigned short int day, char *cause)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Infection;
  event->u.infection.infecting_herd = infecting_herd;
  event->u.infection.infected_herd = infected_herd;
  event->u.infection.day = day;
  event->u.infection.cause = cause;

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
  g_string_sprintf (s, "<Infection event unit=\"%s\" day=%hu",
                    event->infected_herd->official_id, event->day);
  if (event->override_initial_state > Susceptible)
    {
      g_string_append_printf (s, "\n start %s", HRD_status_name[event->override_initial_state]);

      if (event->override_days_left_in_state > 0)
        g_string_append_printf (s, " (%i days left) ", event->override_days_left_in_state);
    }
  g_string_append_c (s, '>');

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
EVT_new_detection_event (HRD_herd_t * herd, unsigned short int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Detection;
  event->u.detection.herd = herd;
  event->u.detection.day = day;
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
EVT_new_public_announcement_event (unsigned short int day)
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
 * Creates a new "attempt to trace" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_attempt_to_trace_event (HRD_herd_t * herd, unsigned short int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_AttemptToTrace;
  event->u.attempt_to_trace.herd = herd;
  event->u.attempt_to_trace.day = day;
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
  g_string_sprintf (s, "<Attempt to trace event unit=\"%s\" day=%hu>",
                    event->herd->official_id, event->day);
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
                            EVT_contact_type_t contact_type,
                            unsigned short int day, gboolean traced)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_TraceResult;
  event->u.trace_result.exposing_herd = exposing_herd;
  event->u.trace_result.exposed_herd = exposed_herd;
  event->u.trace_result.contact_type = contact_type;
  event->u.trace_result.day = day;
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
  g_string_sprintf (s, "<Trace result event units=\"%s\"->\"%s\" (%s) day=%hu trace=%s>",
                    event->exposing_herd->official_id,
                    event->exposed_herd->official_id,
                    EVT_contact_type_name[event->contact_type],
                    event->day, event->traced == TRUE ? "succeeded" : "failed");
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
                                       unsigned short int day,
                                       char *reason,
                                       unsigned short int priority,
                                       unsigned short int min_days_before_next)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForVaccination;
  event->u.request_for_vaccination.herd = herd;
  event->u.request_for_vaccination.day = day;
  event->u.request_for_vaccination.reason = reason;
  event->u.request_for_vaccination.priority = priority;
  event->u.request_for_vaccination.min_days_before_next = min_days_before_next;
  event->u.request_for_vaccination.accepted = FALSE;
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
  g_string_sprintf (s, "<Request for vaccination event unit=\"%s\" day=%hu priority=%hu>",
                    event->herd->official_id, event->day, event->priority);
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
EVT_new_commitment_to_vaccinate_event (HRD_herd_t * herd, unsigned short int day)
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
 * Creates a new "attempt to vaccinate" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_attempt_to_vaccinate_event (HRD_herd_t * herd, unsigned short int day, char *reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_AttemptToVaccinate;
  event->u.attempt_to_vaccinate.herd = herd;
  event->u.attempt_to_vaccinate.day = day;
  event->u.attempt_to_vaccinate.reason = reason;
  /* The following two items cause this to be a normal immunity, one that
   * starts from the start rather than being added to the simulation in-
   * progress. */
  event->u.attempt_to_vaccinate.override_initial_state = Susceptible;
  event->u.attempt_to_vaccinate.override_days_left_in_state = -1;

  return event;
}



/**
 * Creates a new "attempt to vaccinate" event for setting a herd's immunity
 * state in-progress.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_inprogress_immunity_event (HRD_herd_t * herd,
                                   unsigned short int day, char *reason,
                                   HRD_status_t start_in_state, int days_left_in_state)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_AttemptToVaccinate;
  event->u.attempt_to_vaccinate.herd = herd;
  event->u.attempt_to_vaccinate.day = day;
  event->u.attempt_to_vaccinate.reason = reason;
  event->u.attempt_to_vaccinate.override_initial_state = start_in_state;
  event->u.attempt_to_vaccinate.override_days_left_in_state = days_left_in_state;

  return event;
}



/**
 * Returns a text representation of an attempt to vaccinate event.
 *
 * @param event an attempt to vaccinate event.
 * @return a string.
 */
char *
EVT_attempt_to_vaccinate_event_to_string (EVT_attempt_to_vaccinate_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Attempt to vaccinate event unit=\"%s\" day=%hu",
                    event->herd->official_id, event->day);
  if (event->override_initial_state == VaccineImmune)
    {
      g_string_append_printf (s, "\n start %s", HRD_status_name[event->override_initial_state]);

      if (event->override_days_left_in_state > 0)
        g_string_append_printf (s, " (%i days left) ", event->override_days_left_in_state);
    }
  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new "vaccination" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_vaccination_event (HRD_herd_t * herd, unsigned short int day, char *reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Vaccination;
  event->u.vaccination.herd = herd;
  event->u.vaccination.day = day;
  event->u.vaccination.reason = reason;
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

      if (event->override_days_left_in_state > 0)
        g_string_append_printf (s, " (%i days left) ", event->override_days_left_in_state);
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
                                       unsigned short int day,
                                       char *reason, unsigned short int priority)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForDestruction;
  event->u.request_for_destruction.herd = herd;
  event->u.request_for_destruction.day = day;
  event->u.request_for_destruction.reason = reason;
  event->u.request_for_destruction.priority = priority;
  event->u.request_for_destruction.accepted = FALSE;
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
EVT_new_commitment_to_destroy_event (HRD_herd_t * herd, unsigned short int day)
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
 * Creates a new "attempt to destroy" event.
 *
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_attempt_to_destroy_event (HRD_herd_t * herd, unsigned short int day, char *reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_AttemptToDestroy;
  event->u.attempt_to_destroy.herd = herd;
  event->u.attempt_to_destroy.day = day;
  event->u.attempt_to_destroy.reason = reason;
  return event;
}



/**
 * Returns a text representation of an attempt to destroy event.
 *
 * @param event an attempt to destroy event.
 * @return a string.
 */
char *
EVT_attempt_to_destroy_event_to_string (EVT_attempt_to_destroy_event_t * event)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Attempt to destroy event unit=\"%s\" day=%hu reason=\"%s\">",
                    event->herd->official_id, event->day, event->reason);
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
EVT_new_destruction_event (HRD_herd_t * herd, unsigned short int day, char *reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_Destruction;
  event->u.destruction.herd = herd;
  event->u.destruction.day = day;
  event->u.destruction.reason = reason;
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
EVT_new_request_for_zone_focus_event (HRD_herd_t * herd, unsigned short int day, char *reason)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_RequestForZoneFocus;
  event->u.request_for_destruction.herd = herd;
  event->u.request_for_destruction.day = day;
  event->u.request_for_destruction.reason = reason;
  event->u.request_for_destruction.accepted = FALSE;
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
 * @return a pointer to a newly-created EVT_event_t structure.
 */
EVT_event_t *
EVT_new_end_of_day_event (unsigned short int day)
{
  EVT_event_t *event;

  event = g_new (EVT_event_t, 1);
  event->type = EVT_EndOfDay;
  event->u.end_of_day.day = day;
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
EVT_new_last_day_event (unsigned short int day)
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
    case EVT_RequestForExposureCauses:
    case EVT_RequestForInfectionCauses:
    case EVT_RequestForVaccinationReasons:
    case EVT_RequestForVaccineDelay:
    case EVT_DeclarationOfVaccineDelay:
    case EVT_RequestForDestructionReasons:
    case EVT_NewDay:
    case EVT_Exposure:
    case EVT_AttemptToInfect:
    case EVT_Infection:
    case EVT_Detection:
    case EVT_PublicAnnouncement:
    case EVT_AttemptToTrace:
    case EVT_TraceResult:
    case EVT_RequestForVaccination:
    case EVT_CommitmentToVaccinate:
    case EVT_AttemptToVaccinate:
    case EVT_Vaccination:
    case EVT_RequestForDestruction:
    case EVT_CommitmentToDestroy:
    case EVT_AttemptToDestroy:
    case EVT_Destruction:
    case EVT_RequestForZoneFocus:
    case EVT_EndOfDay:
    case EVT_LastDay:
      /* No dynamically-allocated parts to free. */
      break;
    case EVT_DeclarationOfExposureCauses:
      /* Note that we do not free the C strings in the array of exposure
       * causes, because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_exposure_causes.causes, TRUE);
      break;
    case EVT_DeclarationOfInfectionCauses:
      /* Note that we do not free the C strings in the array of infection
       * causes, because we assume they are static strings. */
      g_ptr_array_free (event->u.declaration_of_infection_causes.causes, TRUE);
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
                                        e->day, e->cause, e->traceable);
        clone->u.exposure.contact_type = e->contact_type;
        break;
      }
    case EVT_AttemptToInfect:
      {
        EVT_attempt_to_infect_event_t *e;
        e = &(event->u.attempt_to_infect);
        clone = EVT_new_attempt_to_infect_event (e->infecting_herd,
                                                 e->infected_herd, e->day, e->cause);
        clone->u.attempt_to_infect.override_initial_state = e->override_initial_state;
        clone->u.attempt_to_infect.override_days_left_in_state = e->override_days_left_in_state;
        break;
      }
    case EVT_RequestForVaccination:
      {
        EVT_request_for_vaccination_event_t *e;
        e = &(event->u.request_for_vaccination);
        clone = EVT_new_request_for_vaccination_event (e->herd, e->day,
                                                       e->reason, e->priority,
                                                       e->min_days_before_next);
        clone->u.request_for_vaccination.accepted = e->accepted;
        break;
      }
    case EVT_AttemptToVaccinate:
      {
        EVT_attempt_to_vaccinate_event_t *e;
        e = &(event->u.attempt_to_vaccinate);
        clone = EVT_new_attempt_to_vaccinate_event (e->herd, e->day, e->reason);
        clone->u.attempt_to_vaccinate.override_initial_state = e->override_initial_state;
        clone->u.attempt_to_vaccinate.override_days_left_in_state = e->override_days_left_in_state;
        break;
      }
    case EVT_RequestForDestruction:
      {
        EVT_request_for_destruction_event_t *e;
        e = &(event->u.request_for_destruction);
        clone = EVT_new_request_for_destruction_event (e->herd, e->day, e->reason, e->priority);
        clone->u.request_for_destruction.accepted = e->accepted;
        break;
      }
    case EVT_AttemptToDestroy:
      {
        EVT_attempt_to_destroy_event_t *e;
        e = &(event->u.attempt_to_destroy);
        clone = EVT_new_attempt_to_destroy_event (e->herd, e->day, e->reason);
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
    case EVT_RequestForExposureCauses:
      s = EVT_request_for_exposure_causes_event_to_string (&(event->u.request_for_exposure_causes));
      break;
    case EVT_DeclarationOfExposureCauses:
      s =
        EVT_declaration_of_exposure_causes_event_to_string (&
                                                            (event->u.
                                                             declaration_of_exposure_causes));
      break;
    case EVT_RequestForInfectionCauses:
      s =
        EVT_request_for_infection_causes_event_to_string (&(event->u.request_for_infection_causes));
      break;
    case EVT_DeclarationOfInfectionCauses:
      s =
        EVT_declaration_of_infection_causes_event_to_string (&
                                                             (event->u.
                                                              declaration_of_infection_causes));
      break;
    case EVT_RequestForVaccinationReasons:
      s =
        EVT_request_for_vaccination_reasons_event_to_string (&
                                                             (event->u.
                                                              request_for_vaccination_reasons));
      break;
    case EVT_DeclarationOfVaccinationReasons:
      s =
        EVT_declaration_of_vaccination_reasons_event_to_string (&
                                                                (event->u.
                                                                 declaration_of_vaccination_reasons));
      break;
    case EVT_RequestForVaccineDelay:
      s =
        EVT_request_for_vaccine_delay_event_to_string (&(event->u.request_for_vaccine_delay));
      break;
    case EVT_DeclarationOfVaccineDelay:
      s =
        EVT_declaration_of_vaccine_delay_event_to_string (&(event->u.declaration_of_vaccine_delay));
      break;
    case EVT_RequestForDestructionReasons:
      s =
        EVT_request_for_destruction_reasons_event_to_string (&
                                                             (event->u.
                                                              request_for_destruction_reasons));
      break;
    case EVT_DeclarationOfDestructionReasons:
      s =
        EVT_declaration_of_destruction_reasons_event_to_string (&
                                                                (event->u.
                                                                 declaration_of_destruction_reasons));
      break;
    case EVT_NewDay:
      s = EVT_new_day_event_to_string (&(event->u.new_day));
      break;
    case EVT_Exposure:
      s = EVT_exposure_event_to_string (&(event->u.exposure));
      break;
    case EVT_AttemptToInfect:
      s = EVT_attempt_to_infect_event_to_string (&(event->u.attempt_to_infect));
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
    case EVT_AttemptToTrace:
      s = EVT_attempt_to_trace_event_to_string (&(event->u.attempt_to_trace));
      break;
    case EVT_TraceResult:
      s = EVT_trace_result_event_to_string (&(event->u.trace_result));
      break;
    case EVT_RequestForVaccination:
      s = EVT_request_for_vaccination_event_to_string (&(event->u.request_for_vaccination));
      break;
    case EVT_CommitmentToVaccinate:
      s = EVT_commitment_to_vaccinate_event_to_string (&(event->u.commitment_to_vaccinate));
      break;
    case EVT_AttemptToVaccinate:
      s = EVT_attempt_to_vaccinate_event_to_string (&(event->u.attempt_to_vaccinate));
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
    case EVT_AttemptToDestroy:
      s = EVT_attempt_to_destroy_event_to_string (&(event->u.attempt_to_destroy));
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
 * Deletes an event queue from memory.
 *
 * @param queue an event queue.
 */
void
EVT_free_event_queue (EVT_event_queue_t * queue)
{
  if (queue == NULL)
    return;

  g_queue_foreach (queue, EVT_free_event_as_GFunc, NULL);
  g_queue_free (queue);
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
  GList *iter;                  /* iterator over events in the list */
  EVT_event_t *event;
  GString *s;
  char *substring, *chararray;

  s = g_string_new ("<event queue (starting with next)=\n");

  for (iter = g_queue_peek_head_link (queue); iter != NULL; iter = g_list_next (iter))
    {
      event = (EVT_event_t *) (iter->data);
      substring = EVT_event_to_string (event);
      g_string_append (s, substring);
      g_string_append_c (s, '\n');
      free (substring);
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
