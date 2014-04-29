/** @file event.h
 * Events that can occur in the simulation.
 *
 * One goal of this system is to be able to treat sub-models as interchangeable,
 * mix-and-match parts.  We use an Observer or Publish-Subscribe system where
 * sub-models announce any changes they make to herds (events) and listen for
 * events announced by other sub-models.
 *
 * @image html events.png
 * @image latex events.eps "" width=4in
 *
 * Symbols from this module begin with EVT_.
 *
 * @sa event_manager.h
 * @sa model.h
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

#ifndef EVENT_H
#define EVENT_H

#include <herd.h>
#include <stdio.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#include <glib.h>



#define EVT_NEVENT_TYPES 29

/** Events of interest. */
typedef enum
{
  EVT_RequestForExposureCauses, EVT_DeclarationOfExposureCauses,
  EVT_RequestForInfectionCauses, EVT_DeclarationOfInfectionCauses,
  EVT_RequestForVaccinationReasons, EVT_DeclarationOfVaccinationReasons,
  EVT_RequestForVaccineDelay, EVT_DeclarationOfVaccineDelay,
  EVT_RequestForDestructionReasons, EVT_DeclarationOfDestructionReasons,
  EVT_NewDay, EVT_Exposure, EVT_AttemptToInfect, EVT_Infection,
  EVT_Detection, EVT_PublicAnnouncement, EVT_AttemptToTrace, EVT_TraceResult,
  EVT_RequestForVaccination, EVT_CommitmentToVaccinate, EVT_AttemptToVaccinate,
  EVT_Vaccination, EVT_RequestForDestruction, EVT_CommitmentToDestroy,
  EVT_AttemptToDestroy, EVT_Destruction, EVT_RequestForZoneFocus, EVT_EndOfDay,
  EVT_LastDay
}
EVT_event_type_t;

extern const char *EVT_event_type_name[];

#define EVT_NCONTACT_TYPES 3

/**
 * Ways one herd may contact another.  Direct contact means the movement of
 * animals; indirect contact means the movement of potentially infected things
 * (fomites), e.g., equipment, vehicles, bedding, or people.
 */
typedef enum
{
  UnknownContact, DirectContact, IndirectContact
}
EVT_contact_type_t;



/**
 * A "request for exposure causes" event.  This event signals that any models
 * that can cause exposures must declare the causes(s) they will state for the
 * infections.
 */
typedef struct
{
  int dummy; /**< to avoid a "struct has no members" warning */
}
EVT_request_for_exposure_causes_event_t;



/**
 * A "declaration of exposure causes" event.  Models that can cause exposures
 * use this event to communicate the causes(s) they will state for the
 * exposures, so that other models may initialize counters, etc.
 */
typedef struct
{
  GPtrArray *causes; /**< array of pointers to ordinary C strings */
}
EVT_declaration_of_exposure_causes_event_t;



/**
 * A "request for infection causes" event.  This event signals that any models
 * that can cause infections must declare the causes(s) they will state for the
 * infections.
 */
typedef struct
{
  int dummy; /**< to avoid a "struct has no members" warning */
}
EVT_request_for_infection_causes_event_t;



/**
 * A "declaration of infection causes" event.  Models that can cause infections
 * use this event to communicate the causes(s) they will state for the
 * infections, so that other models may initialize counters, etc.
 */
typedef struct
{
  GPtrArray *causes; /**< array of pointers to ordinary C strings */
}
EVT_declaration_of_infection_causes_event_t;



/**
 * A "request for vaccination reasons" event.  This event signals that any
 * models that can request vaccinations must declare the reason(s) they will
 * supply for the requests.
 */
typedef struct
{
  int dummy; /**< to avoid a "struct has no members" warning */
}
EVT_request_for_vaccination_reasons_event_t;



/**
 * A "declaration of vaccination reasons" event.  Models that can request
 * vaccinations use this event to communicate the reason(s) they will supply
 * for the requests, so that other models may initialize counters, etc.
 */
typedef struct
{
  GPtrArray *reasons; /**< array of pointers to ordinary C strings */
}
EVT_declaration_of_vaccination_reasons_event_t;



/**
 * A "request for vaccine delay" event.  This event signals instances of the
 * vaccine module to declare how long the delay to vaccine immunity is for
 * their production type.  This bit of information is needed in the conflict
 * resolver module to handle one special case.
 */
typedef struct
{
  int dummy; /**< to avoid a "struct has no members" warning */
}
EVT_request_for_vaccine_delay_event_t;



/**
 * A "declaration of vaccine delay" event.  The vaccine module uses this event
 * to communicate how long the delay to vaccine immunity is for their
 * production type.  This bit of information is needed in the conflict resolver
 * module to handle one special case.
 */
typedef struct
{
  HRD_production_type_t production_type;
  char *production_type_name;
  unsigned short int delay;
}
EVT_declaration_of_vaccine_delay_event_t;



/**
 * A "request for destruction reasons" event.  This event signals that any
 * models that can request destructions must declare the reason(s) they will
 * supply for the requests.
 */
typedef struct
{
  int dummy; /**< to avoid a "struct has no members" warning */
}
EVT_request_for_destruction_reasons_event_t;



/**
 * A "declaration of destruction reasons" event.  Models that can request
 * destructions use this event to communicate the reason(s) they will supply
 * for the requests, so that other models may initialize counters, etc.
 */
typedef struct
{
  GPtrArray *reasons; /**< array of pointers to ordinary C strings */
}
EVT_declaration_of_destruction_reasons_event_t;



/** A "new day" event. */
typedef struct
{
  unsigned short int day; /**< day of the simulation */
}
EVT_new_day_event_t;



/** An "exposure" event. */
typedef struct
{
  HRD_herd_t *exposing_herd;
  HRD_herd_t *exposed_herd;
  unsigned short int day;       /**< day of the simulation */
  EVT_contact_type_t contact_type;
  char *cause; /**< name of the model that created the event */
  gboolean traceable;
}
EVT_exposure_event_t;



/** An "attempt to infect" event. */
typedef struct
{
  HRD_herd_t *infecting_herd;
  HRD_herd_t *infected_herd;
  unsigned short int day; /**< day of the simulation */
  char *cause; /**< name of the model that created the event */
  HRD_status_t override_initial_state; /**< when using an infection event to
    specify an in-progress infection, set this to the herd's state (Latent,
    InfectiousSubclinical, or InfectiousClinical). */
  int override_days_left_in_state; /**< when using an infection event to
    specify an in-progress infection, use a non-zero value here to give the
    number of days until the herd transitions to the next state.  A zero value
    means that the number should be chosen from the probability distributions
    given in the disease model parameters. */
}
EVT_attempt_to_infect_event_t;



/** An "infection" event. */
typedef struct
{
  HRD_herd_t *infecting_herd;
  HRD_herd_t *infected_herd;
  unsigned short int day; /**< day of the simulation */
  char *cause; /**< name of the model that created the event */
  HRD_status_t override_initial_state; /**< when using an infection event to
    specify an in-progress infection, set this to the herd's state (Latent,
    InfectiousSubclinical, or InfectiousClinical). */
  int override_days_left_in_state; /**< when using an infection event to
    specify an in-progress infection, use a non-zero value here to give the
    number of days until the herd transitions to the next state.  A zero value
    means that the number should be chosen from the probability distributions
    given in the disease model parameters. */
}
EVT_infection_event_t;



/** A "detection" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day; /**< day of the simulation */
}
EVT_detection_event_t;



/** A "public announcement" event. */
typedef struct
{
  unsigned short int day;
}
EVT_public_announcement_event_t;



/** An "attempt to trace" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
}
EVT_attempt_to_trace_event_t;



/** A "trace result" event. */
typedef struct
{
  HRD_herd_t *exposing_herd;
  HRD_herd_t *exposed_herd;
  EVT_contact_type_t contact_type;
  unsigned short int day;
  gboolean traced;
}
EVT_trace_result_event_t;



/** A "request for vaccination" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
  unsigned short int priority;
  char *reason; /**< why vaccination was requested */
  unsigned short int min_days_before_next;
  gboolean accepted;
}
EVT_request_for_vaccination_event_t;



/** A "commitment to vaccinate" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
}
EVT_commitment_to_vaccinate_event_t;



/** An "attempt to vaccinate" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
  char *reason; /**< why vaccination was requested */
  HRD_status_t override_initial_state; /**< when using a vaccination event to
    specify in-progress immunity, set this to VaccineImmune. */
  int override_days_left_in_state; /**< when using a vaccination event to
    specify in-progress immunity, use a non-zero value here to give the number
    of days until the herd transitions to the next state.  A zero value means
    that the number should be chosen from the probability distribution given
    in the disease model parameters. */
}
EVT_attempt_to_vaccinate_event_t;



/** A "vaccination" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
  char *reason; /**< why vaccination was requested */
  HRD_status_t override_initial_state; /**< when using a vaccination event to
    specify in-progress immunity, set this to VaccineImmune. */
  int override_days_left_in_state; /**< when using a vaccination event to
    specify in-progress immunity, use a non-zero value here to give the number
    of days until the herd transitions to the next state.  A zero value means
    that the number should be chosen from the probability distribution given
    in the disease model parameters. */
}
EVT_vaccination_event_t;



/** A "request for destruction" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
  unsigned short int priority;
  char *reason; /**< why destruction was requested */
  gboolean accepted;
}
EVT_request_for_destruction_event_t;



/** A "commitment to destroy" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
}
EVT_commitment_to_destroy_event_t;



/** An "attempt to destroy" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
  char *reason; /**< why destruction was requested */
}
EVT_attempt_to_destroy_event_t;



/** A "destruction" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
  char *reason; /**< why destruction was requested */
}
EVT_destruction_event_t;



/** A "request for zone focus" event. */
typedef struct
{
  HRD_herd_t *herd;
  unsigned short int day;
  char *reason; /**< why a zone focus was requested */
  gboolean accepted;
}
EVT_request_for_zone_focus_event_t;



/**
 * An "end of day" event.  Only the conflict resolver module should ever
 * respond to this event, otherwise the "end of day" could create more
 * potentially conflicting events and we'd need a second conflict resolver...
 */
typedef struct
{
  unsigned short int day; /**< day of the simulation */
}
EVT_end_of_day_event_t;



/**
 * A "last day" event.  This event is specifically for alerting modules to
 * compute the values of output variables that were only requested for the last
 * day of output.
 */
typedef struct
{
  unsigned short int day; /**< day of the simulation */
}
EVT_last_day_event_t;



/** A supertype for all events. */
typedef struct
{
  EVT_event_type_t type;
  union
  {
    EVT_request_for_exposure_causes_event_t request_for_exposure_causes;
    EVT_declaration_of_exposure_causes_event_t declaration_of_exposure_causes;
    EVT_request_for_infection_causes_event_t request_for_infection_causes;
    EVT_declaration_of_infection_causes_event_t declaration_of_infection_causes;
    EVT_request_for_vaccination_reasons_event_t request_for_vaccination_reasons;
    EVT_declaration_of_vaccination_reasons_event_t declaration_of_vaccination_reasons;
    EVT_request_for_vaccine_delay_event_t request_for_vaccine_delay;
    EVT_declaration_of_vaccine_delay_event_t declaration_of_vaccine_delay;
    EVT_request_for_destruction_reasons_event_t request_for_destruction_reasons;
    EVT_declaration_of_destruction_reasons_event_t declaration_of_destruction_reasons;
    EVT_new_day_event_t new_day;
    EVT_exposure_event_t exposure;
    EVT_attempt_to_infect_event_t attempt_to_infect;
    EVT_infection_event_t infection;
    EVT_detection_event_t detection;
    EVT_public_announcement_event_t public_announcement;
    EVT_attempt_to_trace_event_t attempt_to_trace;
    EVT_trace_result_event_t trace_result;
    EVT_request_for_vaccination_event_t request_for_vaccination;
    EVT_commitment_to_vaccinate_event_t commitment_to_vaccinate;
    EVT_attempt_to_vaccinate_event_t attempt_to_vaccinate;
    EVT_vaccination_event_t vaccination;
    EVT_request_for_destruction_event_t request_for_destruction;
    EVT_commitment_to_destroy_event_t commitment_to_destroy;
    EVT_attempt_to_destroy_event_t attempt_to_destroy;
    EVT_destruction_event_t destruction;
    EVT_request_for_zone_focus_event_t request_for_zone_focus;
    EVT_end_of_day_event_t end_of_day;
    EVT_last_day_event_t last_day;
  }
  u;
}
EVT_event_t;



/** A queue of events. */
typedef GQueue EVT_event_queue_t;



/* Prototypes. */
EVT_event_t *EVT_new_request_for_exposure_causes_event (void);
EVT_event_t *EVT_new_declaration_of_exposure_causes_event (GPtrArray * causes);
EVT_event_t *EVT_new_request_for_infection_causes_event (void);
EVT_event_t *EVT_new_declaration_of_infection_causes_event (GPtrArray * causes);
EVT_event_t *EVT_new_request_for_vaccination_reasons_event (void);
EVT_event_t *EVT_new_declaration_of_vaccination_reasons_event (GPtrArray * reasons);
EVT_event_t *EVT_new_request_for_vaccine_delay_event (void);
EVT_event_t *EVT_new_declaration_of_vaccine_delay_event (HRD_production_type_t,
                                                         char * production_type_name,
                                                         unsigned short int);
EVT_event_t *EVT_new_request_for_destruction_reasons_event (void);
EVT_event_t *EVT_new_declaration_of_destruction_reasons_event (GPtrArray * reasons);
EVT_event_t *EVT_new_new_day_event (unsigned short int day);
EVT_event_t *EVT_new_exposure_event (HRD_herd_t * exposing_herd,
                                     HRD_herd_t * exposed_herd,
                                     unsigned short int day, char *cause, gboolean traceable);
EVT_event_t *EVT_new_attempt_to_infect_event (HRD_herd_t * infecting_herd,
                                              HRD_herd_t * infected_herd,
                                              unsigned short int day, char *cause);
EVT_event_t *EVT_new_inprogress_infection_event (HRD_herd_t * infecting_herd,
                                                 HRD_herd_t * infected_herd,
                                                 unsigned short int day,
                                                 char *cause,
                                                 HRD_status_t start_in_state,
                                                 int days_left_in_state);
EVT_event_t *EVT_new_infection_event (HRD_herd_t * infecting_herd,
                                      HRD_herd_t * infected_herd,
                                      unsigned short int day, char *cause);
EVT_event_t *EVT_new_detection_event (HRD_herd_t *, unsigned short int day);
EVT_event_t *EVT_new_public_announcement_event (unsigned short int day);
EVT_event_t *EVT_new_attempt_to_trace_event (HRD_herd_t *, unsigned short int day);
EVT_event_t *EVT_new_trace_result_event (HRD_herd_t * exposing_herd,
                                         HRD_herd_t * exposed_herd,
                                         EVT_contact_type_t contact_type,
                                         unsigned short int day, gboolean traced);
EVT_event_t *EVT_new_request_for_vaccination_event (HRD_herd_t *,
                                                    unsigned short int day,
                                                    char *reason,
                                                    unsigned short int priority,
                                                    unsigned short int min_days_before_next);
EVT_event_t *EVT_new_commitment_to_vaccinate_event (HRD_herd_t *, unsigned short int day);
EVT_event_t *EVT_new_attempt_to_vaccinate_event (HRD_herd_t *,
                                                 unsigned short int day, char *reason);
EVT_event_t *EVT_new_inprogress_immunity_event (HRD_herd_t * herd,
                                                unsigned short int day,
                                                char *cause,
                                                HRD_status_t start_in_state,
                                                int days_left_in_state);
EVT_event_t *EVT_new_vaccination_event (HRD_herd_t *, unsigned short int day, char *reason);
EVT_event_t *EVT_new_request_for_destruction_event (HRD_herd_t *,
                                                    unsigned short int day,
                                                    char *reason, unsigned short int priority);
EVT_event_t *EVT_new_commitment_to_destroy_event (HRD_herd_t *, unsigned short int day);
EVT_event_t *EVT_new_attempt_to_destroy_event (HRD_herd_t *, unsigned short int day, char *reason);
EVT_event_t *EVT_new_destruction_event (HRD_herd_t *, unsigned short int day, char *reason);
EVT_event_t *EVT_new_request_for_zone_focus_event (HRD_herd_t *,
                                                   unsigned short int day, char *reason);
EVT_event_t *EVT_new_end_of_day_event (unsigned short int day);
EVT_event_t *EVT_new_last_day_event (unsigned short int day);

void EVT_free_event (EVT_event_t *);
EVT_event_t *EVT_clone_event (EVT_event_t *);
char *EVT_event_to_string (EVT_event_t *);
int EVT_fprintf_event (FILE *, EVT_event_t *);

#define EVT_printf_event(Q) EVT_fprintf_event(stdout,Q)

/**
 * Creates a new, empty event queue.
 *
 * @return a pointer to a newly-created event queue.
 */
#define EVT_new_event_queue g_queue_new

/**
 * Adds an event to an event queue.
 *
 * @param Q an event queue.
 * @param E an event.
 */
#define EVT_event_enqueue(Q,E) g_queue_push_tail(Q,E)

/**
 * Retrieves the next event from an event queue.  Returns NULL if the queue is
 * empty.
 *
 * @param Q an event queue.
 * @return an event.
 */
#define EVT_event_dequeue(Q) g_queue_pop_head(Q)

/**
 * Returns whether an event queue is empty.
 *
 * @param Q an event queue.
 * @return TRUE if the queue is empty, FALSE otherwise.
 */
#define EVT_event_queue_is_empty(Q) g_queue_is_empty(Q)

void EVT_free_event_queue (EVT_event_queue_t *);
char *EVT_event_queue_to_string (EVT_event_queue_t *);
int EVT_fprintf_event_queue (FILE *, EVT_event_queue_t *);

#define EVT_printf_event_queue(Q) EVT_fprintf_event_queue(stdout,Q)

#endif /* !EVENT_H */
