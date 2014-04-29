/** @file herd.c
 * Functions for creating, destroying, printing, and manipulating herds.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 *
  * @author Shaun Case <Shaun.Case@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   College of Veterinary Medicine and Biomedical Sciences<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80523<br>
 *   USA
 *
 * Copyright &copy; University of Guelph, 2003-2010
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * @todo Take SCEW out of the Makefile.
 */


#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* To avoid name clashes when dlpreopening multiple modules that have the same
 * global symbols (interface).  See sec. 18.4 of "GNU Autoconf, Automake, and
 * Libtool". */
#define free_as_GFunc herd_LTX_free_as_GFunc

#include <unistd.h>
#include <stdio.h>
#include "herd.h"
#include <expat.h>
/* Expat 1.95 has this constant on my Debian system, but not on Hammerhead's
 * Red Hat system.  ?? */
#ifndef XML_STATUS_ERROR
#  define XML_STATUS_ERROR 0
#endif

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

#if !HAVE_ROUND && HAVE_RINT
#  define round rint
#endif

/* Temporary fix -- "round" and "rint" are in the math library on Red Hat 7.3,
 * but they're #defined so AC_CHECK_FUNCS doesn't find them. */
double round (double x);

/* Temporary fix -- missing from math header file? */
double trunc (double);

#if HAVE_ERRNO_H
#  include <errno.h>
#endif

#define EPSILON 0.001

#include <replace.h>

#include <naadsm.h>


#ifndef WIN_DLL
#ifndef COGRID
/* This line causes problems on Windows, but seems to be unnecessary. */
extern FILE *stdin;
#endif
#endif



/** A request to infect a herd. */
typedef struct
{
  int latent_period;
  int infectious_subclinical_period;
  int infectious_clinical_period;
  int immunity_period;
  gboolean fatal;
  unsigned int fast_forward;
}
HRD_infect_change_request_t;



/** A request to vaccinate a herd. */
typedef struct
{
  int delay;
  int immunity_period;
  unsigned int fast_forward;
}
HRD_vaccinate_change_request_t;



/**
 * A table of all valid state transitions.
 *
 * @sa HRD_status_t
 */
  const gboolean HRD_valid_transition[][HRD_NSTATES] = {
    {FALSE, TRUE,  TRUE,  TRUE,  FALSE, TRUE,  TRUE,  FALSE}, /* Susceptible -> Latent, InfectiousSubclinical, InfectiousClinical, VaccineImmune or Destroyed */
    {FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  FALSE}, /* Latent -> InfectiousSubclinical, InfectiousClinical or Destroyed */
    {FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE}, /* InfectiousSubclinical -> InfectiousClinical or Destroyed */
    {FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE,  TRUE},  /* InfectiousClinical -> NaturallyImmune, DeadFromDisease, or Destroyed */
    {TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE}, /* NaturallyImmune -> Susceptible or Destroyed */
    {TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE}, /* VaccineImmune -> Susceptible or Destroyed */
    {FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE}, /* Destroyed -> <<emptyset>> */
    {FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE}, /* DeadFromDisease -> Destroyed */
  };

/**
 * Names for the possible states (with respect to a disease) for a herd,
 * terminated with a NULL sentinel.
 *
 * @sa HRD_status_t
 */
const char *HRD_status_name[] = {
  "Susceptible", "Latent", "Infectious Subclinical", "Infectious Clinical",
  "Naturally Immune", "Vaccine Immune", "Destroyed", "Dead from Disease",
  NULL
};

const char *HRD_status_abbrev[] = {
  "Susc", "Lat", "Subc", "Clin", "NImm", "VImm", "Dest", "Dead", NULL
};

const char HRD_status_letter_code[] = {
  'S', 'L', 'B', 'C', 'N', 'V', 'D', 'X', '\0'
};



/**
 * Names for the fields in a herd structure, terminated with a NULL sentinel.
 *
 * @sa HRD_herd_t
 */
const char *HRD_herd_field_name[] = {
  "ProductionType", "HerdSize", "Lat", "Lon", "Status", NULL
};



/**
 * Wraps free so that it can be used in GLib calls.
 *
 * @param data a pointer to anything, cast to a gpointer.
 * @param user_data not used, pass NULL.
 */
void
free_as_GFunc (gpointer data, gpointer user_data)
{
  free (data);
}



/**
 * Changes the state of a herd.  This function checks if the transition is
 * valid.
 *
 * @param herd a herd.
 * @param new_state the new state.
 * @param infectious_herds a list of infectious herds, which may change as a
 *   result of this operation.
 */
void
HRD_change_state (HRD_herd_t * herd, HRD_status_t new_state,
                  GHashTable *infectious_herds)
{
  HRD_status_t state;

  state = herd->status;  
  
  if (HRD_valid_transition[state][new_state])
    {    
      herd->status = new_state;
      herd->days_in_status = 0;

      switch( new_state )
      {
        case Susceptible:
          HRD_remove_herd_from_infectious_list( herd, infectious_herds );
        break;

        case Latent:
        case InfectiousSubclinical:
        case InfectiousClinical:
          HRD_add_herd_to_infectious_list( herd, infectious_herds );
        break;

        case NaturallyImmune:
        case VaccineImmune:
          HRD_remove_herd_from_infectious_list( herd, infectious_herds );
        break;

        case Destroyed:
        case DeadFromDisease:
          /*  HRD_remove_herd_from_suscpetible_rtree( herd );  */
          HRD_remove_herd_from_infectious_list( herd, infectious_herds );
        break;
      };

#if DEBUG
      g_debug ("unit \"%s\" is now %s", herd->official_id, HRD_status_name[herd->status]);
#endif
    }
  else
    {
      ;
#if DEBUG
      g_debug ("%s->%s transition for unit \"%s\" was not possible",
               HRD_status_name[state], HRD_status_name[new_state], herd->official_id);                 
#endif
    }
}



/**
 * Creates a new infection change request.
 *
 * @param latent_period the number of days to spend latent.
 * @param infectious_subclinical_period the number of days to spend infectious
 *   without visible signs.
 * @param infectious_clinical_period the number of days to spend infectious
 *   with visible signs.
 * @param immunity_period how many days the herd's natural immunity lasts
 *   after recovery.
 * @param fatal whether the herd will die of the disease.
 * @return a pointer to a newly-created HRD_change_request_t structure.
 */
HRD_infect_change_request_t *
HRD_new_infect_change_request (int latent_period,
                               int infectious_subclinical_period,
                               int infectious_clinical_period,
                               int immunity_period,
                               gboolean fatal,
                               unsigned int fast_forward)
{
  HRD_infect_change_request_t *request;

  request = g_new (HRD_infect_change_request_t, 1);
  request->latent_period = latent_period;
  request->infectious_subclinical_period = infectious_subclinical_period;
  request->infectious_clinical_period = infectious_clinical_period;
  request->immunity_period = immunity_period;
  request->fatal = fatal;
  request->fast_forward = fast_forward;
  return request;
}



/**
 * Carries out an infection change request.
 *
 * @param herd a herd.
 * @param request an infection change request.
 * @param infectious_herds a list of infectious herds, which may change as a
 *   result of this operation.
 */
void
HRD_apply_infect_change_request (HRD_herd_t * herd,
                                 HRD_infect_change_request_t * request,
                                 GHashTable * infectious_herds)
{
  int infectious_start_day, clinical_start_day,
    immunity_start_day, immunity_end_day;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_infect_change_request");
#endif

  if (herd->status != Susceptible)
    goto end;

  /* If the herd has been vaccinated but has not yet developed immunity, cancel
   * the progress of the vaccine. */
  herd->in_vaccine_cycle = FALSE;

  herd->in_disease_cycle = TRUE;
  herd->day_in_disease_cycle = request->fast_forward;
  herd->disease_is_fatal = request->fatal;

#if DEBUG
  if (!request->fatal)
    g_debug ("requested disease progression = day %u in (%hu,%hu,%hu,%hu)",
             request->fast_forward,
             request->latent_period, request->infectious_subclinical_period,
             request->infectious_clinical_period, request->immunity_period);
  else
    g_debug ("requested disease progression = day %u in (%hu,%hu,%hu,die)",
             request->fast_forward,
             request->latent_period, request->infectious_subclinical_period,
             request->infectious_clinical_period);
#endif

  /* Compute the day for each state transition. */
  infectious_start_day = request->latent_period;
  clinical_start_day = infectious_start_day + request->infectious_subclinical_period;
  immunity_start_day = clinical_start_day + request->infectious_clinical_period;
  immunity_end_day = immunity_start_day + request->immunity_period;

  /* Advance the countdowns if the day_in_disease_cycle has been set. */
  if (herd->day_in_disease_cycle >= immunity_end_day)
    {
      if (request->fatal)
        {
          HRD_change_state (herd, Latent, infectious_herds);
          HRD_change_state (herd, InfectiousClinical, infectious_herds);
          herd->infectious_start_countdown = -1;
          herd->clinical_start_countdown = -1;
          herd->immunity_start_countdown = 0;
        }
      else
        herd->in_disease_cycle = FALSE;
    }
  else if (herd->day_in_disease_cycle >= immunity_start_day)
    {
      HRD_change_state (herd, Latent, infectious_herds);
      HRD_change_state (herd, InfectiousClinical, infectious_herds);
      HRD_change_state (herd, NaturallyImmune, infectious_herds);
      herd->days_in_status = herd->day_in_disease_cycle - immunity_start_day;
      herd->infectious_start_countdown = -1;
      herd->clinical_start_countdown = -1;
      herd->immunity_start_countdown = -1;
      herd->immunity_end_countdown = immunity_end_day - herd->day_in_disease_cycle;
    }
  else if (herd->day_in_disease_cycle >= clinical_start_day)
    {
      HRD_change_state (herd, Latent, infectious_herds);
      HRD_change_state (herd, InfectiousClinical, infectious_herds);
      herd->days_in_status = herd->day_in_disease_cycle - clinical_start_day;
      herd->infectious_start_countdown = -1;
      herd->clinical_start_countdown = -1;
      herd->immunity_start_countdown = immunity_start_day - herd->day_in_disease_cycle;
      herd->immunity_end_countdown = immunity_end_day - herd->day_in_disease_cycle;
    }
  else if (herd->day_in_disease_cycle >= infectious_start_day)
    {
      HRD_change_state (herd, Latent, infectious_herds);
      HRD_change_state (herd, InfectiousSubclinical, infectious_herds);
      herd->days_in_status = herd->day_in_disease_cycle - infectious_start_day;
      herd->infectious_start_countdown = -1;
      herd->clinical_start_countdown = clinical_start_day - herd->day_in_disease_cycle;
      herd->immunity_start_countdown = immunity_start_day - herd->day_in_disease_cycle;
      herd->immunity_end_countdown = immunity_end_day - herd->day_in_disease_cycle;
    }
  else
    {
      HRD_change_state (herd, Latent, infectious_herds);
      herd->days_in_status = herd->day_in_disease_cycle;
      herd->infectious_start_countdown = infectious_start_day - herd->day_in_disease_cycle;
      herd->clinical_start_countdown = clinical_start_day - herd->day_in_disease_cycle;
      herd->immunity_start_countdown = immunity_start_day - herd->day_in_disease_cycle;
      herd->immunity_end_countdown = immunity_end_day - herd->day_in_disease_cycle;
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "infectious start countdown=%i",
         herd->infectious_start_countdown);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "clinical start countdown=%i",
         herd->clinical_start_countdown);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "immunity start countdown=%i",
         herd->immunity_start_countdown);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "immunity end countdown=%i",
         herd->immunity_end_countdown);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "in disease cycle=%s",
         herd->in_disease_cycle ? "true" : "false");
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "day in disease cycle=%i",
         herd->day_in_disease_cycle);
#endif

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_apply_infect_change_request");
#endif
  return;
}



/**
 * Creates a new vaccination change request.
 *
 * @param delay the number of days before the immunity begins.
 * @param immunity_period the number of days the immunity lasts.
 * @param fast_forward how many days to advance into the progression of
 *   immunity.  A value of 0 means do not fast forward.
 * @return a pointer to a newly-created HRD_change_request_t structure.
 */
HRD_vaccinate_change_request_t *
HRD_new_vaccinate_change_request (int delay, int immunity_period,
                                  unsigned int fast_forward)
{
  HRD_vaccinate_change_request_t *request;

  request = g_new (HRD_vaccinate_change_request_t, 1);
  request->delay = delay;
  request->immunity_period = immunity_period;
  request->fast_forward = fast_forward;
  return request;
}



/**
 * Carries out a vaccination change request.
 *
 * @param herd a herd.
 * @param request a vaccination change request.
 * @param infectious_herds a list of infectious herds.
 */
void
HRD_apply_vaccinate_change_request (HRD_herd_t * herd,
                                    HRD_vaccinate_change_request_t * request,
                                    GHashTable * infectious_herds)
{
  unsigned int delay;
  int immunity_start_day, immunity_end_day;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_vaccinate_change_request");
#endif

  /* If the herd is Susceptible and not already in the vaccine cycle, then we
   * start the vaccine cycle (i.e. delayed transition to Vaccine Immune). */
  if (herd->status == Susceptible && !herd->in_vaccine_cycle)
    {
      /* Compute the day for each state transition. */
      immunity_start_day = request->delay;
      immunity_end_day = immunity_start_day + request->immunity_period;
      herd->in_vaccine_cycle = TRUE;

      /* Advance the countdowns if fast_foward has been set. */
      if (request->fast_forward >= immunity_end_day)
        {
          herd->in_vaccine_cycle = FALSE;
        }
      else if (request->fast_forward >= immunity_start_day)
        {
          HRD_change_state (herd, VaccineImmune, infectious_herds);
          herd->days_in_status = request->fast_forward - immunity_start_day;
          herd->immunity_start_countdown = -1;
          herd->immunity_end_countdown = immunity_end_day - request->fast_forward;
        }
      else
        {
          herd->immunity_start_countdown = immunity_start_day - request->fast_forward;
          herd->immunity_end_countdown = immunity_end_day - request->fast_forward;
        }
    }
  /* If the herd is already Vaccine Immune, we re-set the time left for the
   * immunity according to the new parameter. */
  else if (herd->status == VaccineImmune)
    {
      delay = request->immunity_period;
      herd->immunity_end_countdown = delay;
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_apply_vaccinate_change_request");
#endif
  return;
}



/**
 * Carries out a destruction change request.
 *
 * @param herd a herd.
 * @param infectious_herds a list of infectious herds, which may change as a
 *   result of this operation.
 */
void
HRD_apply_destroy_change_request (HRD_herd_t * herd,
                                  GHashTable *infectious_herds)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_destroy_change_request");
#endif

  herd->in_vaccine_cycle = FALSE;
  herd->in_disease_cycle = FALSE;

  HRD_change_state (herd, Destroyed, infectious_herds);
  
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_apply_destroy_change_request");
#endif
}



/**
 * Removes all change requests from a herd.
 *
 * @param herd a herd.
 */
void
HRD_herd_clear_change_requests (HRD_herd_t * herd)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_herd_clear_change_requests");
#endif

  g_free (herd->infect_change_request); /* safe even if NULL */
  herd->infect_change_request = NULL;
  g_free (herd->vaccinate_change_request); /* safe even if NULL */
  herd->vaccinate_change_request = NULL;
  herd->destroy_change_request = FALSE;
  herd->quarantine_change_request = FALSE;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_herd_clear_change_requests");
#endif
}



void
HRD_herd_set_latitude (HRD_herd_t * herd, double lat)
{
  if (lat < -90)
    {
      g_warning ("latitude %g is out of bounds, setting to -90", lat);
      herd->latitude = -90;
    }
  else if (lat > 90)
    {
      g_warning ("latitude %g is out of bounds, setting to 90", lat);
      herd->latitude = 90;
    }
  else
    herd->latitude = lat;
}



void
HRD_herd_set_longitude (HRD_herd_t * herd, double lon)
{
  while (lon < -180)
    lon += 360;
  while (lon > 180)
    lon -= 360;
  herd->longitude = lon;
}



/**
 * Creates a new herd structure.
 *
 * @param production_type type of animals.
 * @param production_type_name type of animals.
 * @param size number of animals.
 * @param x x-coordinate of the herd's location.
 * @param y y-coordinate of the herd's location.
 * @return a pointer to a newly-created, initialized HRD_herd_t structure.
 */
HRD_herd_t *
HRD_new_herd (HRD_production_type_t production_type,
              char *production_type_name, unsigned int size, double x, double y)
{
  HRD_herd_t *herd;

  herd = g_new (HRD_herd_t, 1);

  herd->index = 0;
  herd->official_id = NULL;
  herd->production_type = production_type;
  herd->production_type_name = production_type_name;
  if (size < 1)
    {
      g_warning ("unit cannot have zero size, setting to 1");
      herd->size = 1;
    }
  else
    herd->size = size;
  herd->x = x;
  herd->y = y;
  herd->may_be_initially_infected = FALSE;
  herd->status = herd->initial_status = Susceptible;
  herd->days_in_status = 0;
  herd->days_in_initial_status = 0;
  herd->days_left_in_initial_status = 0;
  herd->quarantined = FALSE;
  herd->prevalence = 0;
  herd->prevalence_infectious = 0;
  herd->extra_data = NULL;

  herd->in_vaccine_cycle = FALSE;
  herd->in_disease_cycle = FALSE;
  herd->prevalence_curve = NULL;
  herd->prevalence_infectious_curve = NULL;

  herd->infect_change_request = NULL;
  herd->vaccinate_change_request = NULL;
  herd->quarantine_change_request = FALSE;
  herd->destroy_change_request = FALSE;

  return herd;
}



/**
 * Converts latitude and longitude to x and y coordinates on a map.
 *
 * @param herd a herd.
 * @param projection a map projection.  If NULL, the longitude will be copied
 *   unchanged to x and the latitude to y.  Otherwise, x and y will be filled
 *   in with projected coordinates.
 */
void
HRD_herd_project (HRD_herd_t * herd, projPJ projection)
{
  projUV p;

  if (projection == NULL)
    {
      herd->x = herd->longitude;
      herd->y = herd->latitude;
    }
  else
    {
      p.u = herd->longitude * DEG_TO_RAD;
      p.v = herd->latitude * DEG_TO_RAD;
      p = pj_fwd (p, projection);
      herd->x = p.u;
      herd->y = p.v;
    }
#if DEBUG
  g_debug ("unit \"%s\" lat,lon %.3f,%.3f -> x,y %.1f,%.1f",
           herd->official_id, herd->latitude, herd->longitude,
           herd->x, herd->y);
#endif
  return;
}



/**
 * Converts x and y coordinates on a map to latitude and longitude.
 *
 * @param herd a herd.
 * @param projection a map projection.  If NULL, the x-coordinate will be
 *   copied to longitude (and will be restricted to the range -180 to 180
 *   inclusive if necessary) and the y-coordinate will be copied to latitude
 *   (all will be restricted to the range -90 to 90 if necessary).
 */
void
HRD_herd_unproject (HRD_herd_t * herd, projPJ projection)
{
  projUV p;

  if (projection == NULL)
    {
      HRD_herd_set_longitude (herd, herd->x);
      HRD_herd_set_latitude (herd, herd->y);
    }
  else
    {
	  p.u = herd->x;
	  p.v = herd->y;
	  p = pj_inv (p, projection);
      HRD_herd_set_longitude (herd, p.u * RAD_TO_DEG);
      HRD_herd_set_latitude (herd, p.v * RAD_TO_DEG);
    }
#if DEBUG
  g_debug ("unit \"%s\" x,y %.1f,%.1f -> lat,lon %.3f,%.3f",
           herd->official_id, herd->x, herd->y,
           herd->latitude, herd->longitude);
#endif
  return;
}



/**
 * A special structure for passing a partially completed herd list to Expat's
 * tag handler functions.
 */
typedef struct
{
  HRD_herd_list_t *herds;
  HRD_herd_t *herd;
  GString *s; /**< for gathering character data */
  char *filename; /**< for reporting the XML file's name in errors */
  XML_Parser parser; /**< for reporting the line number in errors */
  gboolean list_has_latlon; /**< TRUE if we have found a unit in the file with
    its location given as latitude and longitude. */
  gboolean list_has_xy; /**< TRUE if we have found a unit in the file with its
    location given as x and y coordinates.  list_has_latlon and list_has_xy
    cannot both be true. */
  gboolean unit_has_lat; /**< TRUE if we have found a latitude element in the
    unit currently being read, FALSE otherwise. */
  gboolean unit_has_lon; /**< TRUE if we have found a longitude element in the
    unit currently being read, FALSE otherwise. */
  gboolean unit_has_x; /**< TRUE if we have found an x-coordinate element in
    the unit currently being read, FALSE otherwise. */
  gboolean unit_has_y; /**< TRUE if we have found a y-coordinate element in the
    unit currently being read, FALSE otherwise. */
}
HRD_partial_herd_list_t;



/**
 * Character data handler for an Expat herd file parser.  Accumulates the
 * complete text for an XML element (which may come in pieces).
 *
 * @param userData a pointer to a HRD_partial_herd_list_t structure, cast to a
 *   void pointer.
 * @param s complete or partial character data from an XML element.
 * @param len the length of the character data.
 */
static void
charData (void *userData, const XML_Char * s, int len)
{
  HRD_partial_herd_list_t *partial;

  partial = (HRD_partial_herd_list_t *) userData;
  g_string_append_len (partial->s, s, len);
}



/**
 * Start element handler for an Expat herd file parser.  Creates a new herd
 * when it encounters a \<herd\> tag.
 *
 * @param userData a pointer to a HRD_partial_herd_list_t structure, cast to a
 *   void pointer.
 * @param name the tag's name.
 * @param atts the tag's attributes.
 */
static void
startElement (void *userData, const char *name, const char **atts)
{
  HRD_partial_herd_list_t *partial;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "encountered start tag for \"%s\"", name);
#endif

  partial = (HRD_partial_herd_list_t *) userData;
  if (strcmp (name, "herds") == 0)
    {
      partial->list_has_latlon = partial->list_has_xy = FALSE;
    }
  if (strcmp (name, "herd") == 0)
    {
      partial->herd = HRD_new_herd (0, NULL, 1, 0, 0);
      partial->unit_has_lat = partial->unit_has_lon = FALSE;
      partial->unit_has_x = partial->unit_has_y = FALSE;
    }
  return;
}



/**
 * End element handler for an Expat herd file parser.
 *
 * When it encounters an \</id\>, \</production-type\>, \</size\>,
 * \</latitude\>, \</longitude\>, or \</status\> tag, it fills in the
 * corresponding field in the herd most recently created by startElement and
 * clears the character data buffer.  This function issues a warning and fills
 * in a reasonable default value when fields are missing or invalid.
 *
 * When it encounters a \</herd\> tag, it adds the just-completed herd to the
 * herd list.
 *
 * @param userData a pointer to a HRD_partial_herd_list_t structure, cast to a
 *   void pointer.
 * @param name the tag's name.
 */
static void
endElement (void *userData, const char *name)
{
  HRD_partial_herd_list_t *partial;
  char *filename;
  XML_Parser parser;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "encountered end tag for \"%s\"", name);
#endif

  partial = (HRD_partial_herd_list_t *) userData;
  filename = partial->filename;
  parser = partial->parser;

  /* id tag */

  if (strcmp (name, "id") == 0)
    {
      char *tmp;
      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "  accumulated string (Expat encoding) = \"%s\"", tmp);
#endif
      /* Expat stores the text as UTF-8.  Convert to ISO-8859-1. */
      partial->herd->official_id = g_convert_with_fallback (tmp, -1, "ISO-8859-1", "UTF-8", "?", NULL, NULL, NULL);
      g_assert (partial->herd->official_id != NULL);
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* production-type tag */

  else if (strcmp (name, "production-type") == 0)
    {
      GPtrArray *production_type_names;
      char *tmp, *tmp2;
      int i;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "  accumulated string (Expat encoding) = \"%s\"", tmp);
#endif
      /* Expat stores the text as UTF-8.  Convert to ISO-8859-1. */
      tmp2 = g_convert_with_fallback (tmp, -1, "ISO-8859-1", "UTF-8", "?", NULL, NULL, NULL);
      g_assert (tmp2 != NULL);
      g_free (tmp);
      production_type_names = partial->herds->production_type_names;

      for (i = 0; i < production_type_names->len; i++)
        {
          if (strcasecmp (tmp2, g_ptr_array_index (production_type_names, i)) == 0)
            break;
        }
      if (i == production_type_names->len)
        {
          /* We haven't encountered this production type before; add its name to
           * the list. */
          g_ptr_array_add (production_type_names, tmp2);
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "  adding new production type \"%s\"", tmp2);
#endif
        }
      else
        g_free (tmp2);

      partial->herd->production_type = i;

      partial->herd->production_type_name = g_ptr_array_index (production_type_names, i);
      g_string_truncate (partial->s, 0);
    }

  /* size tag */

  else if (strcmp (name, "size") == 0)
    {
      long int size;
      char *tmp, *endptr;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      errno = 0;
      size = strtol (tmp, &endptr, 0);
      if (tmp[0] == '\0')
        {
          g_warning ("size missing on line %lu of %s, setting to 1",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          size = 1;
        }
      else if (errno == ERANGE || errno == EINVAL)
        {
          g_warning ("size is too large a number (\"%s\") on line %lu of %s, setting to 1",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          size = 1;
          errno = 0;
        }
      else if (*endptr != '\0')
        {
          g_warning ("size is not a number (\"%s\") on line %lu of %s, setting to 1",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          size = 1;
        }
      else if (size < 1)
        {
          g_warning ("size cannot be less than 1 (\"%s\") on line %lu of %s, setting to 1",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          size = 1;
        }
      partial->herd->size = (unsigned int) size;
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* latitude tag */

  else if (strcmp (name, "latitude") == 0)
    {
      double lat;
      char *tmp, *endptr;

      if (partial->list_has_xy)
        g_error ("cannot mix lat/lon and x/y locations on line %lu of %s",
                 (unsigned long) XML_GetCurrentLineNumber (parser), filename);
      partial->list_has_latlon = TRUE;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      lat = strtod (tmp, &endptr);
      if (tmp[0] == '\0')
        {
          g_warning ("latitude missing on line %lu of %s, setting to 0",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          lat = 0;
        }
      else if (errno == ERANGE)
        {
          g_warning ("latitude is too large a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          lat = 0;
          errno = 0;
        }
      else if (endptr == tmp)
        {
          g_warning ("latitude is not a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          lat = 0;
        }
      HRD_herd_set_latitude (partial->herd, lat);
      partial->unit_has_lat = TRUE;
      /* If we have latitude and longitude and a projection, fill in x and y. */
      if (partial->unit_has_lat && partial->unit_has_lon && partial->herds->projection != NULL)
        HRD_herd_project (partial->herd, partial->herds->projection);
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* longitude tag */

  else if (strcmp (name, "longitude") == 0)
    {
      double lon;
      char *tmp, *endptr;

      if (partial->list_has_xy)
        g_error ("cannot mix lat/lon and x/y locations on line %lu of %s",
                 (unsigned long) XML_GetCurrentLineNumber (parser), filename);
      partial->list_has_latlon = TRUE;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      lon = strtod (tmp, &endptr);
      if (tmp[0] == '\0')
        {
          g_warning ("longitude missing on line %lu of %s, setting to 0",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          lon = 0;
        }
      else if (errno == ERANGE)
        {
          g_warning ("longitude is too large a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          lon = 0;
          errno = 0;
        }
      else if (endptr == tmp)
        {
          g_warning ("longitude is not a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          lon = 0;
        }
      HRD_herd_set_longitude (partial->herd, lon);
      partial->unit_has_lon = TRUE;
      /* If we have latitude and longitude and a projection, fill in x and y. */
      if (partial->unit_has_lat && partial->unit_has_lon && partial->herds->projection != NULL)
        HRD_herd_project (partial->herd, partial->herds->projection);
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* x tag */

  else if (strcmp (name, "x") == 0)
    {
      double x;
      char *tmp, *endptr;

      if (partial->list_has_latlon)
        g_error ("cannot mix lat/lon and x/y locations on line %lu of %s",
                 (unsigned long) XML_GetCurrentLineNumber (parser), filename);
      partial->list_has_xy = TRUE;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_debug ("  accumulated string = \"%s\"", tmp);
#endif

      x = strtod (tmp, &endptr);
      if (tmp[0] == '\0')
        {
          g_warning ("x-coordinate missing on line %lu of %s, setting to 0",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          x = 0;
        }
      else if (errno == ERANGE)
        {
          g_warning ("x-coordinate is too large a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          x = 0;
          errno = 0;
        }
      else if (endptr == tmp)
        {
          g_warning ("x-coordinate is not a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          x = 0;
        }
      partial->herd->x = x;
      partial->unit_has_x = TRUE;
      /* If we have x and y and a projection, fill in latitude and longitude. */
      if (partial->unit_has_x && partial->unit_has_y && partial->herds->projection != NULL)
        HRD_herd_unproject (partial->herd, partial->herds->projection);
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* y tag */

  else if (strcmp (name, "y") == 0)
    {
      double y;
      char *tmp, *endptr;

      if (partial->list_has_latlon)
        g_error ("cannot mix lat/lon and x/y locations on line %lu of %s",
                 (unsigned long) XML_GetCurrentLineNumber (parser), filename);
      partial->list_has_xy = TRUE;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_debug ("  accumulated string = \"%s\"", tmp);
#endif

      y = strtod (tmp, &endptr);
      if (tmp[0] == '\0')
        {
          g_warning ("y-coordinate missing on line %lu of %s, setting to 0",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          y = 0;
        }
      else if (errno == ERANGE)
        {
          g_warning ("y-coordinate is too large a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          y = 0;
          errno = 0;
        }
      else if (endptr == tmp)
        {
          g_warning ("y-coordinate is not a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          y = 0;
        }
      partial->herd->y = y;
      partial->unit_has_y = TRUE;
      /* If we have x and y and a projection, fill in latitude and longitude. */
      if (partial->unit_has_x && partial->unit_has_y && partial->herds->projection != NULL)
        HRD_herd_unproject (partial->herd, partial->herds->projection);
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* may-be-initially-infected tag */

  else if ( 0 == strcmp (name, "may-be-initially-infected") )
    {
      char *tmp;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      partial->herd->may_be_initially_infected = ( 0 == strcasecmp( tmp, "true" ) );

      /* In this case, the actual initial status will be set elsewhere. */
      partial->herd->status = partial->herd->initial_status = Susceptible;
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }


  /* status tag */

  else if (strcmp (name, "status") == 0)
    {
      HRD_status_t status;
      char *tmp, *endptr;

      /* According to the XML Schema, status is allowed to be a numeric code or
       * a string. */
      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      if (tmp[0] == '\0')
        {
          g_warning ("status missing on line %lu of %s, setting to Susceptible",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);

          status = Susceptible;
        }
      else if (isdigit (tmp[0]))
        {
          /* If it starts with a number, assume it is a numeric code. */
          status = (HRD_status_t) strtol (tmp, &endptr, 0);
          if (errno == EINVAL || errno == ERANGE || *endptr != '\0' || status < 0
              || status >= HRD_NSTATES)
            {
              g_warning
                ("\"%s\" is not a valid numeric status code on line %lu of %s, setting to 0 (Susceptible)",
                 tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
              status = Susceptible;
            }
        }
      else if (strcasecmp (tmp, "S") == 0
               || strcasecmp (tmp, "Susceptible") == 0)
        status = Susceptible;
      else if (strcasecmp (tmp, "L") == 0
               || strcasecmp (tmp, "Latent") == 0)
        status = Latent;
      else if (strcasecmp (tmp, "B") == 0
               || strcasecmp (tmp, "Infectious Subclinical") == 0
               || strcasecmp (tmp, "InfectiousSubclinical") == 0)
        status = InfectiousSubclinical;
      else if (strcasecmp (tmp, "C") == 0
               || strcasecmp (tmp, "Infectious Clinical") == 0
               || strcasecmp (tmp, "InfectiousClinical") == 0)
        status = InfectiousClinical;
      else if (strcasecmp (tmp, "N") == 0
               || strcasecmp (tmp, "Naturally Immune") == 0
               || strcasecmp (tmp, "NaturallyImmune") == 0)
        status = NaturallyImmune;
      else if (strcasecmp (tmp, "V") == 0
               || strcasecmp (tmp, "Vaccine Immune") == 0
               || strcasecmp (tmp, "VaccineImmune") == 0)
        status = VaccineImmune;
      else if (strcasecmp (tmp, "D") == 0
               || strcasecmp (tmp, "Destroyed") == 0)
        status = Destroyed;
      else if (strcasecmp (tmp, "X") == 0
               || strcasecmp (tmp, "Dead from Disease") == 0
               || strcasecmp (tmp, "DeadFromDisease") == 0)
        status = DeadFromDisease;
      else
        {
          g_warning ("\"%s\" is not a valid unit state on line %lu of %s, setting to Susceptible",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          status = Susceptible;
        }
      partial->herd->status = partial->herd->initial_status = status;

      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* days-in-status tag */

  else if (strcmp (name, "days-in-status") == 0)
    {
      long int days;
      char *tmp, *endptr;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      errno = 0;
      days = strtol (tmp, &endptr, 0);
      if (tmp[0] == '\0')
        {
          g_warning ("days-in-status missing on line %lu of %s, setting to 0",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (errno == ERANGE || errno == EINVAL)
        {
          g_warning
            ("days-in-status is too large a number (\"%s\") on line %lu of %s, setting to 0",
             tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
          errno = 0;
        }
      else if (*endptr != '\0')
        {
          g_warning ("days-in-status is not a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (days < 0)
        {
          g_warning
            ("days-in-status cannot be negative (\"%s\") on line %lu of %s, setting to 0", tmp,
             (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      partial->herd->days_in_initial_status = (int) days;
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* days-left-in-status tag */

  else if (strcmp (name, "days-left-in-status") == 0)
    {
      long int days;
      char *tmp, *endptr;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      errno = 0;
      days = strtol (tmp, &endptr, 0);
      if (tmp[0] == '\0')
        {
          g_warning ("days-left-in-status missing on line %lu of %s, setting to 0",
                     (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (errno == ERANGE || errno == EINVAL)
        {
          g_warning
            ("days-left-in-status is too large a number (\"%s\") on line %lu of %s, setting to 0",
             tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
          errno = 0;
        }
      else if (*endptr != '\0')
        {
          g_warning ("days-left-in-status is not a number (\"%s\") on line %lu of %s, setting to 0",
                     tmp, (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (days < 0)
        {
          g_warning
            ("days-left-in-status cannot be negative (\"%s\") on line %lu of %s, setting to 0", tmp,
             (unsigned long) XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      partial->herd->days_left_in_initial_status = (int) days;
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* herd tag */

  else if (strcmp (name, "herd") == 0)
    {
#ifdef FIX_ME                   // FIX ME: the function call below causes the app to crash
#if DEBUG
      char *s;
      s = HRD_herd_to_string (partial->herd);   // FIX ME: This function fails.
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "completed herd =\n%s", s);
      g_free (s);
#endif
#endif
      HRD_herd_list_append (partial->herds, partial->herd);
      HRD_free_herd (partial->herd, FALSE);
    }

  /* PROJ4 tag */

  else if (strcmp (name, "PROJ4") == 0)
    {
      projPJ projection;
      char *tmp;
#if DEBUG
      char *s;
#endif

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_debug ("  accumulated string = \"%s\"", tmp);
#endif
      projection = pj_init_plus (tmp);
      if (!projection)
        {
          g_error ("could not create map projection object: %s", pj_strerrno(pj_errno));
        }
#if DEBUG
      s = pj_get_def (projection, 0);
      g_debug ("projection = %s", s);
      free (s);
#endif
      partial->herds->projection = projection;
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }
}



/**
 * Returns a text representation of a herd.
 *
 * @param herd a herd.
 * @return a string.
 */
char *
HRD_herd_to_string (HRD_herd_t * herd)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<%s herd id=%s size=%u x=%g y=%g",
                    herd->production_type_name, herd->official_id, herd->size, herd->x, herd->y);

  /* Print the status, plus days left if applicable. */
  g_string_append_printf (s, "\n %s", HRD_status_name[herd->status]);
  if (herd->days_left_in_initial_status > 0)
    g_string_append_printf (s, " (%hu days left) ", herd->days_left_in_initial_status);

  g_string_append_c (s, '>');

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Prints a herd to a stream.
 *
 * @param stream an output stream to write to.  If NULL, defaults to stdout.
 * @param herd a herd.
 * @return the number of characters written.
 */
int
HRD_fprintf_herd (FILE * stream, HRD_herd_t * herd)
{
  char *s;
  int nchars_written;

  s = HRD_herd_to_string (herd);
  nchars_written = fprintf (stream ? stream : stdout, "%s", s);
  free (s);
  return nchars_written;
}



/**
 * Deletes a herd structure from memory.  Does not free the production type
 * name string.
 *
 * @param herd a herd.
 * @param free_segment if TRUE, also frees the dynamically-allocated parts of
 *   the herd structure.
 */
void
HRD_free_herd (HRD_herd_t * herd, gboolean free_segment)
{
  if (free_segment == TRUE)
    {
      g_free (herd->official_id);
      HRD_herd_clear_change_requests (herd);
      /* We do not free the prevalence chart, because it is assumed to belong
       * to the disease module. */
    }
  g_free (herd);
}



/**
 * Creates a new, empty herd list.
 *
 * @return a pointer to a newly-created, empty HRD_herd_list_t structure.
 */
HRD_herd_list_t *
HRD_new_herd_list (void)
{
  HRD_herd_list_t *herds;

  herds = g_new (HRD_herd_list_t, 1);
  herds->list = g_array_new (FALSE, FALSE, sizeof (HRD_herd_t));
  herds->production_type_names = g_ptr_array_new ();
  herds->projection = NULL;

  return herds;
}



/**
 * Deletes a herd list from memory.
 *
 * @param herds a herd list.
 */
void
HRD_free_herd_list (HRD_herd_list_t * herds)
{
  HRD_herd_t *herd;
  unsigned int nherds;
  int i;                        /* loop counter */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_free_herd_list");
#endif

  if (herds == NULL)
    goto end;

  /* Free the dynamic parts of each herd structure. */
  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      g_free (herd->official_id);
      g_free (herd->infect_change_request); /* safe even if NULL */
      g_free (herd->vaccinate_change_request); /* safe even if NULL */
    }

  /* Free the herd structures. */
  g_array_free (herds->list, TRUE);

  /* Free the production type names. */
  for (i = 0; i < herds->production_type_names->len; i++)
    g_free (g_ptr_array_index (herds->production_type_names, i));
  g_ptr_array_free (herds->production_type_names, TRUE);

  /* Free the projection. */
  if (herds->projection != NULL)
    pj_free (herds->projection);

  /* Finally, free the herd list structure. */
  g_free (herds);

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_free_herd_list");
#endif
  return;
}



/**
 * Converts the latitude and longitude values to x and y coordinates on a map.
 *
 * @param herds a herd list.
 * @param projection a map projection.  If NULL, the longitudes will be copied
 *   unchanged to x-coordinates and the latitudes to y-coordinates.
 */
void
HRD_herd_list_project (HRD_herd_list_t * herds, projPJ projection)
{
  unsigned int nherds, i;

#if DEBUG
  g_debug ("----- ENTER HRD_herd_list_project");
#endif

  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    {
      HRD_herd_project (HRD_herd_list_get (herds, i), projection);
    }

#if DEBUG
  g_debug ("----- EXIT HRD_herd_list_project");
#endif

  return;
}



/**
 * Loads a herd list from a file.  Use HRD_herd_list_project() to convert the
 * lats and lons to a flat map.  Also, a bounding rectangle has not been
 * computed; use either HRD_herd_list_unoriented_bounding_box() or
 * HRD_herd_list_oriented_bounding_box to fill that in.
 *
 * @param filename a file name.
 * @return a herd list.
 */
HRD_herd_list_t *
HRD_load_herd_list (const char *filename)
{
  FILE *fp;
  HRD_herd_list_t *herds;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_load_herd_list");
#endif

  fp = fopen (filename, "r");
  if (fp == NULL)
    {
      g_error ("could not open file \"%s\": %s", filename, strerror (errno));
    }
  herds = HRD_load_herd_list_from_stream (fp, filename);

  fclose (fp);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_load_herd_list");
#endif
  return herds;
}



/**
 * Loads a herd list from an open stream.
 *
 * @param stream a stream.  If NULL, defaults to stdin.  This function does not
 *   close the stream; that is the caller's responsibility.
 * @param filename a file name, if known, for reporting in error messages.  Use
 *   NULL if the file name is not known.
 * @return a herd list.
 */
HRD_herd_list_t *
HRD_load_herd_list_from_stream (FILE * stream, const char *filename)
{
  HRD_herd_list_t *herds;
  HRD_partial_herd_list_t to_pass;
  XML_Parser parser;            /* to read the file */
  int xmlerr;
  char *linebuf = NULL;
  size_t bufsize = 0;
  ssize_t linelen;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_load_herd_list_from_stream");
#endif

  if (stream == NULL)
    stream = stdin;
  if (filename == NULL)
    filename = "input";

  herds = HRD_new_herd_list ();

  parser = XML_ParserCreate (NULL);
  if (parser == NULL)
    {
      g_warning ("failed to create parser for reading file of units");
      goto end;
    }

  to_pass.herds = herds;
  to_pass.herd = NULL;
  to_pass.s = g_string_new (NULL);
  to_pass.filename = (char *)filename;
  to_pass.parser = parser;

  XML_SetUserData (parser, &to_pass);
  XML_SetElementHandler (parser, startElement, endElement);
  XML_SetCharacterDataHandler (parser, charData);

  while (1)
    {
      linelen = getline (&linebuf, &bufsize, stream);
      if (linelen == -1)
        {
          xmlerr = XML_Parse (parser, NULL, 0, 1);
          if (xmlerr == XML_STATUS_ERROR)
            {
              g_error ("%s at line %lu in %s",
                       XML_ErrorString (XML_GetErrorCode (parser)),
                       (unsigned long) XML_GetCurrentLineNumber (parser), filename);
            }
          break;
        }
      xmlerr = XML_Parse (parser, linebuf, linelen, 0);
      if (xmlerr == XML_STATUS_ERROR)
        {
          g_error ("%s at line %lu in %s",
                   XML_ErrorString (XML_GetErrorCode (parser)),
                   (unsigned long) XML_GetCurrentLineNumber (parser), filename);
        }
    }

  /* Clean up. */
  XML_ParserFree (parser);
  g_string_free (to_pass.s, TRUE);
  free (linebuf);

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_load_herd_list_from_stream");
#endif
  return herds;
}



/**
 * Appends a herd to a herd list.  NB: The contents of the herd structure are
 * shallow-copied into an array, so you may free the herd structure <em>but not
 * its dynamically-allocated children</em> after adding it to a herd list.
 *
 * @param herds a herd list.
 * @param herd a herd.
 * @return the new length of the herd list.
 */
unsigned int
HRD_herd_list_append (HRD_herd_list_t * herds, HRD_herd_t * herd)
{
  GArray *list;
  unsigned int new_length;

  list = herds->list;
  g_array_append_val (list, *herd);
  new_length = HRD_herd_list_length (herds);

  /* Now make the pointer point to the copy in the herd list. */
  herd = &g_array_index (list, HRD_herd_t, new_length - 1);

  /* Set the list index number for the herd. */
  herd->index = new_length - 1;

  return new_length;
}



/**
 * Returns a text string containing a herd list.
 *
 * @param herds a herd list.
 * @return a string.
 */
char *
HRD_herd_list_to_string (HRD_herd_list_t * herds)
{
  GString *s;
  char *substring, *chararray;
  unsigned int nherds;
  unsigned int i;               /* loop counter */

  s = g_string_new (NULL);

  nherds = HRD_herd_list_length (herds);
  if (nherds > 0)
    {
      substring = HRD_herd_to_string (HRD_herd_list_get (herds, 0));
      g_string_assign (s, substring);
      g_free (substring);
      for (i = 1; i < nherds; i++)
        {
          substring = HRD_herd_to_string (HRD_herd_list_get (herds, i));
          g_string_append_printf (s, "\n%s", substring);
          g_free (substring);
        }
    }
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Prints a herd list to a stream.
 *
 * @param stream an output stream to write to.  If NULL, defaults to stdout.
 * @param herds a herd list.
 * @return the number of characters written.
 */
int
HRD_fprintf_herd_list (FILE * stream, HRD_herd_list_t * herds)
{
  char *s;
  int nchars_written;

  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_fprintf_herd_list");

  if (!stream)
    stream = stdout;

  s = HRD_herd_list_to_string (herds);
  nchars_written = fprintf (stream, "%s", s);
  free (s);

  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_fprintf_herd_list");

  return nchars_written;
}



/**
 * Returns a text string giving the state of each herd.
 *
 * @param herds a herd list.
 * @return a string.
 */
char *
HRD_herd_list_summary_to_string (HRD_herd_list_t * herds)
{
  GString *s;
  char *chararray;
  unsigned int nherds;          /* number of herds */
  unsigned int i;               /* loop counter */

  nherds = HRD_herd_list_length (herds);
  s = g_string_new (NULL);
  g_string_sprintf (s, "%i", HRD_herd_list_get (herds, 0)->status);
  for (i = 1; i < nherds; i++)
    g_string_sprintfa (s, " %i", HRD_herd_list_get (herds, i)->status);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}


/**
 * Returns a text string giving the prevalence of each infected herd.
 *
 * @param herds a herd list.
 * @param day the simulation day for which the prevalence is being recorded.
 * @return a string.
 */
char *
HRD_herd_list_prevalence_to_string (HRD_herd_list_t * herds, unsigned int day)
{
  GString *s;
  char *chararray;
  unsigned int nherds;          /* number of herds */
  unsigned int i;               /* loop counter */
  gboolean first_infected_found;
  int herd_status;

  first_infected_found = FALSE;
  nherds = HRD_herd_list_length (herds);
  s = g_string_new (NULL);


  for (i = 0; i < nherds; i++)
    {
      herd_status = HRD_herd_list_get (herds, i)->status;

      if ((Latent == herd_status)
          || (InfectiousSubclinical == herd_status) || (InfectiousClinical == herd_status))
        {
          if (FALSE == first_infected_found)
            {
              first_infected_found = TRUE;

              g_string_sprintf (s, "%i, %s, s%is, %f",  /* The second and third "s"'s in the string look
                                                         * funny, but they're there for a reason. */
                                day,
                                HRD_herd_list_get (herds, i)->official_id,
                                herd_status, HRD_herd_list_get (herds, i)->prevalence);
            }
          else
            {
              g_string_sprintfa (s, "\r\n%i, %s, s%is, %f",     /* The second and third "s"'s in the string look
                                                                 * funny, but they're there for a reason. */
                                 day,
                                 HRD_herd_list_get (herds, i)->official_id,
                                 herd_status, HRD_herd_list_get (herds, i)->prevalence);
            }
        }
    }

  if (FALSE == first_infected_found)
    g_string_sprintf (s, "%i, (No infected units)", day);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}


/**
 * Prints the state of each herd.
 *
 * @param herds a herd list.
 * @return the number of characters written.
 */
int
HRD_printf_herd_list_summary (HRD_herd_list_t * herds)
{
  int nchars_written;

  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_printf_herd_list_summary");

  nchars_written = HRD_fprintf_herd_list_summary (stdout, herds);

  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_printf_herd_list_summary");

  return nchars_written;
}



/**
 * Prints the state of each herd to a stream.
 *
 * @param stream an output stream to write to.  If NULL, defaults to stdout.
 * @param herds a herd list.
 * @return the number of characters written.
 */
int
HRD_fprintf_herd_list_summary (FILE * stream, HRD_herd_list_t * herds)
{
  char *s;
  int nchars_written;

  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_fprintf_herd_list_summary");

  if (!stream)
    stream = stdout;

  s = HRD_herd_list_summary_to_string (herds);
  nchars_written = fprintf (stream, "%s", s);
  free (s);

  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_fprintf_herd_list_summary");

  return nchars_written;
}



/**
 * Populates an array with counts of animals in each state inside a unit.
 *
 * @param herd a unit.
 * @param counts pointer to an array of HRD_NSTATES unsigned integers.
 * @return nothing.  Side effect: the passed array will be populated with
 *   counts of animals in each state.
 */
void
HRD_num_animals_in_states (HRD_herd_t * herd)
{
  unsigned int *counts;
  unsigned int i;

#if DEBUG
  g_debug ("----- ENTER HRD_num_animals_in_states");
#endif

  counts = herd->num_animals_in_state;

  for (i = 0; i < HRD_NSTATES; i++)
    counts[i] = 0;
  switch (herd->status)
    {
    /* These 5 states are unaffected by prevalence. */
    case Susceptible:
    case NaturallyImmune:
    case VaccineImmune:
    case Destroyed:
    case DeadFromDisease:
      counts[herd->status] = herd->size;
      break;
    /* The counts for the remaining states will depend on whether the unit
     * has no prevalence curves (v3.0), just an infected prevalence curve
     * (v3.1), just an infectious prevalence curve (allowed in v4.0), or
     * both prevalence curves (v4.0). */
    default:
      if (herd->prevalence_infectious_curve == NULL)
        {
          if (herd->prevalence_curve == NULL)
            {
              /* No prevalence curves are defined. */
#if DEBUG
              g_debug ("no prevalence curves");
#endif
              counts[herd->status] = herd->size;
            }
          else
            {
              /* Just the infected prevalence curve is defined. */
              unsigned int num_susceptible, num_infected;
#if DEBUG
              g_debug ("infected prevalence curve only");
#endif
              num_infected = MAX (1, (unsigned int) round (herd->prevalence * herd->size));
              num_susceptible = herd->size - num_infected;
              counts[Susceptible] = num_susceptible;
              counts[herd->status] = num_infected;
            }
        }
      else
        {
          unsigned int num_susceptible, num_latent, num_infectious;

          if (herd->prevalence_curve == NULL)
            {
              /* Just the infectious prevalence curve is defined.  We
               * don't know how many animals are Latent, but if the
               * unit-level state is Latent, there must be at least 1. */
#if DEBUG
              g_debug ("infectious prevalence curve only");
#endif
              num_latent = (herd->status == Latent ? 1 : 0);
              num_infectious = MIN (herd->size - num_latent, (unsigned int) round (herd->prevalence_infectious * herd->size));
              num_susceptible = herd->size - num_latent - num_infectious;
              counts[Susceptible] = num_susceptible;
              counts[Latent] = num_latent;
              if (herd->status == Latent)
                counts[InfectiousSubclinical] = num_infectious;
              else
                counts[herd->status] = num_infectious;
            }
          else
            {
              /* Both the infected and infectious prevalence curves are
               * defined. */
              double difference;
#if DEBUG
              g_debug ("infected and infectious prevalence curves");
#endif
              difference = herd->prevalence - herd->prevalence_infectious;
              g_assert (difference >= 0);

              /* If the unit-level state is Latent, assume that any
               * infectious animals are Infectious Subclinical. */
              if (herd->status == Latent)
                {
                  num_latent = MAX (1, (unsigned int) round (difference * herd->size));
                  num_infectious = MIN (herd->size - num_latent, (unsigned int) round (herd->prevalence_infectious * herd->size));
                  num_susceptible = herd->size - num_latent - num_infectious;
                  counts[Susceptible] = num_susceptible;
                  counts[Latent] = num_latent;
                  counts[InfectiousSubclinical] = num_infectious;
                }
              else
                {
                  num_infectious = MAX (1, (unsigned int) round (herd->prevalence_infectious * herd->size));
                  num_latent = MIN (herd->size - num_infectious, (unsigned int) round (difference * herd->size));
                  num_susceptible = herd->size - num_latent - num_infectious;
                  counts[Susceptible] = num_susceptible;
                  counts[Latent] = num_latent;
                  counts[herd->status] = num_infectious;
                }
           }
        } /* end of 4 cases covering which prevalence curves are defined. */
    } /* end of switch statement for the unit-level state. */

#if DEBUG
  g_debug ("----- EXIT HRD_num_animals_in_states");
#endif

  return;
}



/**
 * Resets a herd to alive, Susceptible, and not quarantined.
 *
 * @param herd a herd.
 */
void
HRD_reset (HRD_herd_t * herd)
{
  herd->status = Susceptible;
  herd->days_in_status = 0;
  HRD_num_animals_in_states (herd);
  herd->quarantined = FALSE;
  herd->in_vaccine_cycle = FALSE;
  herd->in_disease_cycle = FALSE;
  HRD_herd_clear_change_requests (herd);
}



/**
 * Advances a herd's status by one time step (day).
 *
 * This function is called <em>before</em> any sub-models that may be operating.
 * It carries out changes or delayed transitions that the models may have set.
 *
 * This function resolves conflicts among changes set by sub-models.  For
 * example, with sub-models operating largely independently, it may be possible
 * for a herd to be infected in the morning, vaccinated at noon, and
 * destroyed later in the day!
 *
 * The conflict resolution rules implemented here say:
 * <ol>
 *   <li>
 *     Orders to quarantine are processed first.
 *   <li>
 *     Infection, vaccination, and destruction are processed next.  If both
 *     infection and vaccination, both infection and destruction, both
 *     vaccination and destruction, or all three are pending, the order in
 *     which they happen is chosen randomly.  If more than one disease spread
 *     sub-model has caused an infection, one cause and its associated
 *     parameters (latent period, etc.) is chosen randomly.  Similarly, if more
 *     than one sub-model has requestion destruction or vaccination, one
 *     reason for the action is chosen randomly.
 *   <li>
 *     Biological processes happening inside the animals (e.g., the natural
 *     progression of the disease or the process of gaining immunity from a
 *     vaccine) are processed last.
 * </ol>
 *
 * @param herd a herd.
 * @param rng a random number generator.
 * @return the herd's new state.
 */
HRD_status_t
HRD_step (HRD_herd_t * herd, RAN_gen_t * rng, GHashTable *infectious_herds)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_step");
#endif

  herd->days_in_status++;

  if (herd->quarantine_change_request)
    herd->quarantined = TRUE;

  if (herd->destroy_change_request)
    HRD_apply_destroy_change_request (herd, infectious_herds);

  /* If the herd has been both infected and vaccinated, randomly choose which
   * to apply first. */
  if (herd->infect_change_request)
    {
      if (herd->vaccinate_change_request)
        {
          /* Had both infect and vaccinate requests */
          double r;
          r = RAN_num (rng);
          if (r < 0.5)
            {
              HRD_apply_infect_change_request (herd, herd->infect_change_request, infectious_herds);
              HRD_apply_vaccinate_change_request (herd, herd->vaccinate_change_request, infectious_herds);
            }              
          else
            {
              HRD_apply_vaccinate_change_request (herd, herd->vaccinate_change_request, infectious_herds);
              HRD_apply_infect_change_request (herd, herd->infect_change_request, infectious_herds);
            }              
        }
      else
        /* Had just an infect request */
        HRD_apply_infect_change_request (herd, herd->infect_change_request, infectious_herds);
    }
  else if (herd->vaccinate_change_request)
    /* Had just a vaccinate request */
    HRD_apply_vaccinate_change_request (herd, herd->vaccinate_change_request, infectious_herds);
  HRD_herd_clear_change_requests (herd);

  /* Take any delayed transitions. */
  if (herd->in_vaccine_cycle)
    {
      if (herd->immunity_start_countdown-- == 0)
        HRD_change_state (herd, VaccineImmune, infectious_herds);
      if (herd->immunity_end_countdown-- == 0)
        {
          HRD_change_state (herd, Susceptible, infectious_herds);
          herd->in_vaccine_cycle = FALSE;
        }
    }

  if (herd->in_disease_cycle)
    {
      if (herd->immunity_start_countdown > 0)
        {
          if (herd->prevalence_curve == NULL)
            {
              herd->prevalence = 1;
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "prevalence = 1");
#endif
            }
          else
            {
              herd->prevalence = REL_chart_lookup ((0.5 + herd->day_in_disease_cycle) /
                                                   (herd->day_in_disease_cycle +
                                                    herd->immunity_start_countdown),
                                                   herd->prevalence_curve);
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "prevalence = lookup((%hu+0.5)/(%hu+%hu))=%g",
                     herd->day_in_disease_cycle,
                     herd->day_in_disease_cycle, herd->immunity_start_countdown, herd->prevalence);
#endif
            }
          if (herd->prevalence_infectious_curve == NULL)
            {
              herd->prevalence_infectious = 1;
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "prevalence_infectious = 1");
#endif
            }
          else
            {
              herd->prevalence_infectious =
                REL_chart_lookup ((0.5 + herd->day_in_disease_cycle) /
                                  (herd->day_in_disease_cycle +
                                   herd->immunity_start_countdown),
                                  herd->prevalence_infectious_curve);
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "prevalence_infectious = lookup((%hu+0.5)/(%hu+%hu))=%g",
                     herd->day_in_disease_cycle,
                     herd->day_in_disease_cycle, herd->immunity_start_countdown, herd->prevalence_infectious);
#endif
            }
        }
      else
        {
          herd->prevalence = 0;
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "prevalence = 0");
#endif
          herd->prevalence_infectious = 0;
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "prevalence_infectious = 0");
#endif
        }

      herd->day_in_disease_cycle++;
      if (herd->infectious_start_countdown-- == 0)
        HRD_change_state (herd, InfectiousSubclinical, infectious_herds);
      if (herd->clinical_start_countdown-- == 0)
        HRD_change_state (herd, InfectiousClinical, infectious_herds);
      if (herd->immunity_start_countdown-- == 0)
        {
          if (herd->disease_is_fatal)
            {
              HRD_change_state (herd, DeadFromDisease, infectious_herds);
              herd->immunity_end_countdown = -1;
              herd->in_disease_cycle = FALSE;
            }
          else
            HRD_change_state (herd, NaturallyImmune, infectious_herds);
        }
      if (herd->immunity_end_countdown-- == 0)
        {
          HRD_change_state (herd, Susceptible, infectious_herds);
          herd->in_disease_cycle = FALSE;
        }   
    }

  /* Estimate counts of animals in each state. */
  HRD_num_animals_in_states (herd);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_step");
#endif
  return herd->status;
}



/**
 * Infects a herd with a disease.
 *
 * @param herd the herd to be infected.
 * @param latent_period the number of days to spend latent.
 * @param infectious_subclinical_period the number of days to spend infectious
 *   without visible signs.
 * @param infectious_clinical_period the number of days to spend infectious
 *   with visible signs.
 * @param immunity_period how many days the herd's natural immunity lasts
 *   after recovery
 * @param fatal whether the herd will die of the disease
 */
void
HRD_infect (HRD_herd_t * herd,
            int latent_period,
            int infectious_subclinical_period,
            int infectious_clinical_period,
            int immunity_period,
            gboolean fatal,
            unsigned int day_in_disease_cycle)
{
  if (herd->infect_change_request == NULL)
    herd->infect_change_request = HRD_new_infect_change_request
                                  (latent_period, infectious_subclinical_period,
                                   infectious_clinical_period, immunity_period,
                                   fatal, day_in_disease_cycle);
}



/**
 * Vaccinates a herd against a disease.
 *
 * @param herd the herd to be vaccinated.
 * @param delay the number of days before immunity begins.
 * @param immunity_period the number of days the immunity lasts.
 * @param fast_forward how many days to advance into the progression of
 *   immunity.  A value of 0 means do not fast forward.
 */
void
HRD_vaccinate (HRD_herd_t * herd, int delay, int immunity_period,
               unsigned int fast_forward)
{
  if (herd->vaccinate_change_request == NULL)
    herd->vaccinate_change_request = HRD_new_vaccinate_change_request
                                     (delay, immunity_period, fast_forward);
}



/**
 * Quarantines a herd.
 *
 * @param herd the herd to be quarantined.
 */
void
HRD_quarantine (HRD_herd_t * herd)
{
  herd->quarantine_change_request = TRUE;
}



/**
 * Destroys a herd.
 *
 * @param herd the herd to be destroyed.
 */
void
HRD_destroy (HRD_herd_t * herd)
{
  herd->destroy_change_request = TRUE;
}



/**
 * Returns the number of animals in a given state inside a unit.
 *
 * @param herd a unit.
 * @param state_of_interest a state.
 * @return how many animals in the unit are in that state.
 */
unsigned int
HRD_num_animals_in_state (HRD_herd_t * herd, HRD_status_t state_of_interest)
{
  unsigned int num;
  
  if (herd == NULL)
    num = 0;
  else
    num = herd->num_animals_in_state[state_of_interest];
  
  return num;
}



/**
 * Returns whether a unit is infectious.
 *
 * @param herd a unit.
 * @return TRUE if the unit is infectious, FALSE if not.
 */
gboolean
HRD_is_infectious (HRD_herd_t * herd)
{
  return (HRD_num_animals_in_state(herd, InfectiousClinical) > 0
          || HRD_num_animals_in_state(herd, InfectiousSubclinical) > 0);
}



/**
 * Returns whether a unit is infected.
 *
 * @param herd a unit.
 * @return TRUE if the unit is infected, FALSE if not.
 */
gboolean
HRD_is_infected (HRD_herd_t * herd)
{
  return (HRD_is_infectious (herd) || HRD_num_animals_in_state(herd, Latent) > 0);
}



/**
 * Removes a herd from the infectious list.
 *
 * @param herd the herd to be removed.
 */
void
HRD_remove_herd_from_infectious_list( HRD_herd_t *herd,
                                      GHashTable *infectious_herds )
{
  if ( ( herd != NULL ) && ( infectious_herds != NULL ) )
    g_hash_table_remove( infectious_herds, GUINT_TO_POINTER(herd->index) );
}



/**
 * Adds a herd to the infectious list.
 *
 * @param herd the herd to be added.
 */
void
HRD_add_herd_to_infectious_list( HRD_herd_t *herd,
                                 GHashTable *infectious_herds )
{
  if ( ( herd != NULL ) && ( infectious_herds != NULL ) )
  {
    if ( g_hash_table_lookup( infectious_herds, GUINT_TO_POINTER(herd->index) ) == NULL )
      g_hash_table_insert( infectious_herds, GUINT_TO_POINTER(herd->index), (gpointer)herd );
  };
}

/* end of file herd.c */
