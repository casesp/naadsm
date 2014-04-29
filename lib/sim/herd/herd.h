/** @file herd.h
 * State information about a herd of animals.
 *
 * A herd contains one production type and has a size, location (latitude and
 * longitude), state, and prevalence.  Sub-models may read these data fields,
 * but they should modify a herd only through the functions HRD_infect(),
 * HRD_vaccinate(), HRD_destroy(), HRD_quarantine(), and HRD_lift_quarantine().
 * The first three functions correspond to the action labels on the
 * <a href="herd_8h.html#a42">herd state-transition diagram</a>.
 *
 * Because the events in one simulation day should be considered to happen
 * simultaneously, and because different sub-models may try to make conflicting
 * changes to a herd, the functions named above do not change a herd
 * immediately.  Instead, the request for a change is stored, and conflicts
 * between the change requests are resolved before any changes are applied.
 * See HRD_step() for details.
 *
 * Symbols from this module begin with HRD_.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date January 2003
 *
 * Copyright &copy; University of Guelph, 2003-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef HERD_H
#define HERD_H

#include <stdio.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#include "rel_chart.h"
#include <glib.h>
#include <rTreeIndex.h>



/**
 * Production types.
 */
typedef unsigned int HRD_production_type_t;



/**
 * Number of possible states (with respect to a disease) for a herd.
 *
 * @sa HRD_status_t
 */
#define HRD_NSTATES 7

/**
 * Possible states (with respect to a disease) for a herd.  The diagram below
 * is based on one from Mark Schoenbaum's presentation to the North American
 * Animal Health Committee's (NAAHC) Emergency Management Working Group at the
 * Disease Spread Modeling Workshop, Fort Collins, CO July 9-11, 2002.  The
 * single Infectious state has been split into two.
 *
 * @image html state-transition.png
 * @image latex state-transition.eps width=4in
 *
 * @sa HRD_valid_transition
 */
typedef enum
{
  Susceptible, Latent, InfectiousSubclinical, InfectiousClinical,
  NaturallyImmune, VaccineImmune, Destroyed
}
HRD_status_t;



/**
 * Number of actions/changes that can be made to a herd.
 *
 * @sa HRD_change_request_type_t
 */
#define HRD_NCHANGE_REQUEST_TYPES 5

/**
 * Actions/changes that can be made to a herd.
 */
typedef enum
{
  Infect, Vaccinate, Quarantine, LiftQuarantine, Destroy
}
HRD_change_request_type_t;



/** A request to infect a herd. */
typedef struct
{
  unsigned short int latent_period;
  unsigned short int infectious_subclinical_period;
  unsigned short int infectious_clinical_period;
  unsigned short int immunity_period;
}
HRD_infect_change_request_t;



/** A request to vaccinate a herd. */
typedef struct
{
  unsigned short int delay;
  unsigned short int immunity_period;
}
HRD_vaccinate_change_request_t;



/** A request to quarantine a herd. */
typedef struct
{
  int dummy;
}
HRD_quarantine_change_request_t;



/** A request to lift the quarantine on a herd. */
typedef struct
{
  int dummy;
}
HRD_lift_quarantine_change_request_t;



/** A request to destroy a herd. */
typedef struct
{
  int dummy;
}
HRD_destroy_change_request_t;



/** A supertype for all change requests. */
typedef struct
{
  HRD_change_request_type_t type;
  union
  {
    HRD_infect_change_request_t infect;
    HRD_vaccinate_change_request_t vaccinate;
    HRD_quarantine_change_request_t quarantine;
    HRD_lift_quarantine_change_request_t lift_quarantine;
    HRD_destroy_change_request_t destroy;
  }
  u;
}
HRD_change_request_t;



/** Type of a herd's identifier. */
typedef char *HRD_id_t;


/** Notification for the GUI that a herd's status has changed,
  * or when something else happens to a herd.
 */
typedef struct
{
  unsigned int index;
  HRD_status_t status;          /* an integer */
  int success;                  /* Used with attempted traces */
  char *msg;
}
HRD_update_t;


/** Notification for the GUI that an exposure has occurred.
 * if an "attempt to infect" event is generated, the attempt
 * is considered successful.
 *
 * Note that exposures may preceed infection by some period
 * of time.
*/
typedef struct
{
  unsigned int src_index;
  HRD_status_t src_status;      /* an integer */
  unsigned int dest_index;
  HRD_status_t dest_status;     /* an integer */
  int success;
  char *exposure_method;        /* A for airborne, D for direct contact, I for indirect contact */
}
HRD_expose_t;


/** Notification for the GUI that a herd's zone designation has changed. */
typedef struct
{
  unsigned int herd_index;
  unsigned int zone_level;  
}
HRD_zone_t;


/** Complete state information for a herd. */
typedef struct
{
  unsigned int index;           /**< position in a herd list */
  HRD_production_type_t production_type;
  char *production_type_name;
  HRD_id_t official_id;         /**< arbitrary identifier string */
  unsigned int size;            /**< number of animals */
  float lat;                    /**< latitude (degrees) */
  float lon;                    /**< longitude (degrees) */
  HRD_status_t status;
  HRD_status_t initial_status;
  unsigned short int days_left_in_initial_status;
  double prevalence;

  /* Remaining fields should be considered private. */

  gboolean quarantined;
  unsigned short int days_in_status;

  gboolean in_vaccine_cycle;
  unsigned short int immunity_start_countdown;
  unsigned short int immunity_end_countdown;

  gboolean in_disease_cycle;
  unsigned short int day_in_disease_cycle;
  unsigned short int infectious_start_countdown;
  unsigned short int clinical_start_countdown;
  REL_chart_t *prevalence_curve;

  GSList *change_requests;
}
HRD_herd_t;



/** A list of herds. */
typedef struct
{
  GArray *list; /**< Each item is a HRD_herd_t structure. */
  GPtrArray *production_type_names; /**< Each pointer is to a regular C string. */
  struct Node *spatial_index;
  struct Rect limits; /**< the minimum and maximum lats and longs. */
  double oriented_rect[8]; /**< a minimum-area oriented rectangle around the
    herds. */
  double xaxis_length, yaxis_length; /**< the length in km of the sides of the
    minimum-area oriented rectangle. */
  double short_axis_length; /**< xaxis_length or yaxis_length, whichever is
    less. */
}
HRD_herd_list_t;



/* Prototypes. */

HRD_herd_list_t *HRD_new_herd_list (void);
HRD_herd_list_t *HRD_load_herd_list (const char *filename);
HRD_herd_list_t *HRD_load_herd_list_from_stream (FILE *stream, const char *filename);
void HRD_free_herd_list (HRD_herd_list_t *);
unsigned int HRD_herd_list_append (HRD_herd_list_t *, HRD_herd_t *);

/**
 * Returns the number of herds in a herd list.
 *
 * @param H a herd list.
 * @return the number of herds in the list.
 */
#define HRD_herd_list_length(H) (H->list->len)

/**
 * Returns the ith herd in a herd list.
 *
 * @param H a herd list.
 * @param I the index of the herd to retrieve.
 * @return the ith herd.
 */
#define HRD_herd_list_get(H,I) (&g_array_index(H->list,HRD_herd_t,I))

unsigned int HRD_herd_list_get_by_status (HRD_herd_list_t *, HRD_status_t, HRD_herd_t ***);
void HRD_herd_list_get_bounding_box (HRD_herd_list_t *, double *corners);
char *HRD_herd_list_to_string (HRD_herd_list_t *);
int HRD_printf_herd_list (HRD_herd_list_t *);
int HRD_fprintf_herd_list (FILE *, HRD_herd_list_t *);
char *HRD_herd_list_summary_to_string (HRD_herd_list_t *);
char *HRD_herd_list_prevalence_to_string (HRD_herd_list_t *, unsigned int day);
int HRD_printf_herd_list_summary (HRD_herd_list_t *);
int HRD_fprintf_herd_list_summary (FILE *, HRD_herd_list_t *);

HRD_herd_t *HRD_new_herd (HRD_production_type_t, char *production_type_name,
                          unsigned int size, float lat, float lon);
void HRD_free_herd (HRD_herd_t *, gboolean free_segment);
char *HRD_herd_to_string (HRD_herd_t *);
int HRD_fprintf_herd (FILE *, HRD_herd_t *);

#define HRD_printf_herd(H) HRD_fprintf_herd(stdout,H)

void HRD_reset (HRD_herd_t *);
void HRD_step (HRD_herd_t *);
void HRD_infect (HRD_herd_t *, unsigned short int latent_period,
                 unsigned short int infectious_subclinical_period,
                 unsigned short int infectious_clinical_period, unsigned short int immunity_period);
void HRD_vaccinate (HRD_herd_t *, unsigned short int delay, unsigned short int immunity_period);
void HRD_quarantine (HRD_herd_t *);
void HRD_lift_quarantine (HRD_herd_t *);
void HRD_destroy (HRD_herd_t *);

#endif /* !HERD_H */
