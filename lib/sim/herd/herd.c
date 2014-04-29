/** @file herd.c
 * Functions for creating, destroying, printing, and manipulating herds.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date January 2003
 *
 * Copyright &copy; University of Guelph, 2003-2008
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
#include <sprng.h>
#include "wml.h"
#include "2dch.h"
#include "gis.h"
#include <gsl/gsl_math.h>

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

/* Temporary fix -- missing from math header file? */
double trunc (double);

#if HAVE_ERRNO_H
#  include <errno.h>
#endif

#define EPSILON 0.001

#ifndef _WIN32
/* This line causes problems on Windows, but seems to be unnecessary. */
extern FILE *stdin;
#endif

/* 
herd.c needs access to the functions defined in guilib.h,
even when compiled as a *nix executable (in which case, 
the functions defined will all be NULL). 
*/
#include "guilib.h"

/**
 * A table of all valid state transitions.
 *
 * @sa HRD_status_t
 */
const gboolean HRD_valid_transition[][HRD_NSTATES] = {
  {FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE}, /* Susceptible -> Latent, InfectiousSubclinical, InfectiousClinical, VaccineImmune or Destroyed */
  {FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE},       /* Latent -> InfectiousSubclinical, InfectiousClinical or Destroyed */
  {FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE},      /* InfectiousSubclinical -> InfectiousClinical or Destroyed */
  {FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE},      /* InfectiousClinical -> NaturallyImmune or Destroyed */
  {TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE},      /* NaturallyImmune -> Susceptible or Destroyed */
  {TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE},      /* VaccineImmune -> Susceptible or Destroyed */
  {FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE},    /* Destroyed -> <<emptyset>> */
};



/**
 * Names for the possible states (with respect to a disease) for a herd,
 * terminated with a NULL sentinel.
 *
 * @sa HRD_status_t
 */
const char *HRD_status_name[] = {
  "Susceptible", "Latent", "Infectious Subclinical", "Infectious Clinical",
  "Naturally Immune", "Vaccine Immune", "Destroyed", NULL
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
 */
void
HRD_change_state (HRD_herd_t * herd, HRD_status_t new_state)
{
  HRD_status_t state;
  HRD_update_t update;

  state = herd->status;
  if (HRD_valid_transition[state][new_state])
    {
      herd->status = new_state;
      herd->days_in_status = 0;

      if (NULL != guilib_change_herd_state)
        {
          update.index = herd->index;
          update.status = herd->status;
          update.success = 2;   /* Unused */
          guilib_change_herd_state (update);

        }

#if INFO
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "unit \"%s\" is now %s", herd->official_id,
             HRD_status_name[herd->status]);
#endif
    }
  else
    {
      ;
#if INFO
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,
             "%s->%s transition for unit \"%s\" was not possible",
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
 * @return a pointer to a newly-created HRD_change_request_t structure.
 */
HRD_change_request_t *
HRD_new_infect_change_request (unsigned short int latent_period,
                               unsigned short int
                               infectious_subclinical_period,
                               unsigned short int infectious_clinical_period,
                               unsigned short int immunity_period)
{
  HRD_change_request_t *request;

  request = g_new (HRD_change_request_t, 1);
  request->type = Infect;
  request->u.infect.latent_period = latent_period;
  request->u.infect.infectious_subclinical_period = infectious_subclinical_period;
  request->u.infect.infectious_clinical_period = infectious_clinical_period;
  request->u.infect.immunity_period = immunity_period;
  return request;
}



/**
 * Carries out an infection change request.
 *
 * @param herd a herd.
 * @param request an infection change request.
 */
void
HRD_apply_infect_change_request (HRD_herd_t * herd, HRD_infect_change_request_t * request)
{
  unsigned int delay;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_infect_change_request");
#endif

  if (herd->status != Susceptible)
    goto end;

  /* If the herd has been vaccinated but has not yet developed immunity, cancel
   * the progress of the vaccine. */
  herd->in_vaccine_cycle = FALSE;

  HRD_change_state (herd, Latent);

  delay = request->latent_period;
  herd->infectious_start_countdown = delay;
  if (delay == 0)
    HRD_change_state (herd, InfectiousSubclinical);

  delay += request->infectious_subclinical_period;
  herd->clinical_start_countdown = delay;
  if (delay == 0)
    HRD_change_state (herd, InfectiousClinical);

  delay += request->infectious_clinical_period;
  herd->immunity_start_countdown = delay;
  if (delay == 0)
    HRD_change_state (herd, NaturallyImmune);

  delay += request->immunity_period;
  herd->immunity_end_countdown = delay;
  if (delay == 0)
    HRD_change_state (herd, Susceptible);

  herd->in_disease_cycle = (delay > 0);
  herd->day_in_disease_cycle = 0;

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
 * @return a pointer to a newly-created HRD_change_request_t structure.
 */
HRD_change_request_t *
HRD_new_vaccinate_change_request (unsigned short int delay, unsigned short int immunity_period)
{
  HRD_change_request_t *request;

  request = g_new (HRD_change_request_t, 1);
  request->type = Vaccinate;
  request->u.vaccinate.delay = delay;
  request->u.vaccinate.immunity_period = immunity_period;
  return request;
}



/**
 * Carries out a vaccination change request.
 *
 * @param herd a herd.
 * @param request a vaccination change request.
 */
void
HRD_apply_vaccinate_change_request (HRD_herd_t * herd, HRD_vaccinate_change_request_t * request)
{
  unsigned int delay;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_vaccinate_change_request");
#endif

  /* If the herd is Susceptible and not already in the vaccine cycle, then we
   * start the vaccine cycle (i.e. delayed transition to Vaccine Immune). */
  if (herd->status == Susceptible && !herd->in_vaccine_cycle)
    {
      delay = request->delay;
      herd->immunity_start_countdown = delay;
      if (delay == 0)
        HRD_change_state (herd, VaccineImmune);

      delay += request->immunity_period;
      herd->immunity_end_countdown = delay;
      if (delay == 0)
        HRD_change_state (herd, Susceptible);

      herd->in_vaccine_cycle = (delay > 0);
    }
  /* If the herd is already Vaccine Immune, we re-set the time left for the
   * immunity according to the new parameter. */
  else if (herd->status == VaccineImmune)
    {
      delay = request->immunity_period;
      herd->immunity_end_countdown = delay;
      if (delay == 0)
        HRD_change_state (herd, Susceptible);

      herd->in_vaccine_cycle = (delay > 0);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_apply_vaccinate_change_request");
#endif
  return;
}



/**
 * Creates a new quarantine change request.
 *
 * @return a pointer to a newly-created HRD_change_request_t structure.
 */
HRD_change_request_t *
HRD_new_quarantine_change_request (void)
{
  HRD_change_request_t *request;

  request = g_new (HRD_change_request_t, 1);
  request->type = Quarantine;
  return request;
}



/**
 * Carries out a quarantine change request.
 *
 * @param herd a herd.
 * @param request a quarantine change request.
 */
void
HRD_apply_quarantine_change_request (HRD_herd_t * herd, HRD_quarantine_change_request_t * request)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_quarantine_change_request");
#endif

  herd->quarantined = TRUE;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_apply_quarantine_change_request");
#endif
}



/**
 * Creates a new lift quarantine change request.
 *
 * @return a pointer to a newly-created HRD_change_request_t structure.
 */
HRD_change_request_t *
HRD_new_lift_quarantine_change_request (void)
{
  HRD_change_request_t *request;

  request = g_new (HRD_change_request_t, 1);
  request->type = LiftQuarantine;
  return request;
}



/**
 * Carries out a lift quarantine change request.
 *
 * @param herd a herd.
 * @param request a lift quarantine change request.
 */
void
HRD_apply_lift_quarantine_change_request (HRD_herd_t * herd,
                                          HRD_lift_quarantine_change_request_t * request)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_lift_quarantine_change_request");
#endif

  herd->quarantined = FALSE;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_apply_lift_quarantine_change_request");
#endif
}



/**
 * Creates a new destruction change request.
 *
 * @return a pointer to a newly-created HRD_change_request_t structure.
 */
HRD_change_request_t *
HRD_new_destroy_change_request (void)
{
  HRD_change_request_t *request;

  request = g_new (HRD_change_request_t, 1);
  request->type = Destroy;
  return request;
}



/**
 * Carries out a destruction change request.
 *
 * @param herd a herd.
 * @param request a destruction change request.
 */
void
HRD_apply_destroy_change_request (HRD_herd_t * herd, HRD_destroy_change_request_t * request)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_apply_destroy_change_request");
#endif

  herd->in_vaccine_cycle = FALSE;
  herd->in_disease_cycle = FALSE;

  HRD_change_state (herd, Destroyed);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_apply_destroy_change_request");
#endif
}



/**
 * Carries out a change request.
 *
 * @param herd a herd.
 * @param request a change request.
 */
void
HRD_apply_change_request (HRD_herd_t * herd, HRD_change_request_t * request)
{
  switch (request->type)
    {
    case Infect:
      HRD_apply_infect_change_request (herd, &(request->u.infect));
      break;
    case Vaccinate:
      HRD_apply_vaccinate_change_request (herd, &(request->u.vaccinate));
      break;
    case Quarantine:
      HRD_apply_quarantine_change_request (herd, &(request->u.quarantine));
      break;
    case LiftQuarantine:
      HRD_apply_lift_quarantine_change_request (herd, &(request->u.lift_quarantine));
      break;
    case Destroy:
      HRD_apply_destroy_change_request (herd, &(request->u.destroy));
      break;
    default:
      g_assert_not_reached ();
    }
}



/**
 * Registers a request for a change to a herd.
 */
void
HRD_herd_add_change_request (HRD_herd_t * herd, HRD_change_request_t * request)
{
  herd->change_requests = g_slist_append (herd->change_requests, request);
}



/**
 * Deletes a change request structure from memory.
 *
 * @param request a change request.
 */
void
HRD_free_change_request (HRD_change_request_t * request)
{
  g_free (request);
}



/**
 * Wraps HRD_free_change_request so that it can be used in GLib calls.
 *
 * @param data a pointer to a HRD_change_request_t structure, but cast to a
 *   gpointer.
 * @param user_data not used, pass NULL.
 */
void
HRD_free_change_request_as_GFunc (gpointer data, gpointer user_data)
{
  HRD_free_change_request ((HRD_change_request_t *) data);
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

  g_slist_foreach (herd->change_requests, HRD_free_change_request_as_GFunc, NULL);
  g_slist_free (herd->change_requests);
  herd->change_requests = NULL;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_herd_clear_change_requests");
#endif
}



void
HRD_herd_set_latitude (HRD_herd_t * herd, float lat)
{
  if (lat < -90)
    {
      g_warning ("latitude %g is out of bounds, setting to -90", lat);
      herd->lat = -90;
    }
  else if (lat > 90)
    {
      g_warning ("latitude %g is out of bounds, setting to 90", lat);
      herd->lat = 90;
    }
  else
    herd->lat = lat;
}



void
HRD_herd_set_longitude (HRD_herd_t * herd, float lon)
{
  while (lon < -180)
    lon += 360;
  while (lon > 180)
    lon -= 360;
  herd->lon = lon;
}



/**
 * Creates a new herd structure.
 *
 * @param production_type type of animals.
 * @param production_type_name type of animals.
 * @param size number of animals.
 * @param lat latitude component (degrees) of the herd's location.  -90 <=
 *   <i>lat</i> <= 90.
 * @param lon longitude component (degrees) of the herd's location.  -180 <=
 *   <i>lon</i> <= 180.
 * @return a pointer to a newly-created, initialized HRD_herd_t structure.
 */
HRD_herd_t *
HRD_new_herd (HRD_production_type_t production_type,
              char *production_type_name, unsigned int size, float lat, float lon)
{
  HRD_herd_t *herd;

  herd = g_new (HRD_herd_t, 1);

  herd->index = 0;
  herd->official_id = NULL;
  herd->production_type = production_type;
  herd->production_type_name = production_type_name;
  herd->size = size;
  HRD_herd_set_latitude (herd, lat);
  HRD_herd_set_longitude (herd, lon);
  herd->status = herd->initial_status = Susceptible;
  herd->days_in_status = 0;
  herd->days_left_in_initial_status = 0;
  herd->quarantined = FALSE;
  herd->prevalence = 0;

  herd->in_vaccine_cycle = FALSE;
  herd->in_disease_cycle = FALSE;
  herd->prevalence_curve = NULL;
  herd->change_requests = NULL;

  return herd;
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
  if (strcmp (name, "herd") == 0)
    partial->herd = HRD_new_herd (0, NULL, 0, 0, 0);
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
          g_warning ("size missing on line %d of %s, setting to 1",
                     XML_GetCurrentLineNumber (parser), filename);
          size = 1;
        }
      else if (errno == ERANGE || errno == EINVAL)
        {
          g_warning ("size is too large a number (\"%s\") on line %d of %s, setting to 1",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          size = 1;
          errno = 0;
        }
      else if (*endptr != '\0')
        {
          g_warning ("size is not a number (\"%s\") on line %d of %s, setting to 1",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          size = 1;
        }
      else if (size < 0)
        {
          g_warning ("size cannot be negative (\"%s\") on line %d of %s, setting to 1",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          size = 1;
        }
      partial->herd->size = (unsigned int) size;
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* latitude tag */

  else if (strcmp (name, "latitude") == 0)
    {
      float lat;
      char *tmp, *endptr;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      lat = (float) strtod (tmp, &endptr);
      if (tmp[0] == '\0')
        {
          g_warning ("latitude missing on line %d of %s, setting to 0",
                     XML_GetCurrentLineNumber (parser), filename);
          lat = 0;
        }
      else if (errno == ERANGE)
        {
          g_warning ("latitude is too large a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          lat = 0;
          errno = 0;
        }
      else if (endptr == tmp)
        {
          g_warning ("latitude is not a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          lat = 0;
        }
      HRD_herd_set_latitude (partial->herd, lat);
      g_free (tmp);
      g_string_truncate (partial->s, 0);
    }

  /* longitude tag */

  else if (strcmp (name, "longitude") == 0)
    {
      float lon;
      char *tmp, *endptr;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "  accumulated string = \"%s\"", tmp);
#endif

      lon = (float) strtod (tmp, &endptr);
      if (tmp[0] == '\0')
        {
          g_warning ("longitude missing on line %d of %s, setting to 0",
                     XML_GetCurrentLineNumber (parser), filename);
          lon = 0;
        }
      else if (errno == ERANGE)
        {
          g_warning ("longitude is too large a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          lon = 0;
          errno = 0;
        }
      else if (endptr == tmp)
        {
          g_warning ("longitude is not a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          lon = 0;
        }
      HRD_herd_set_longitude (partial->herd, lon);
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
          g_warning ("status missing on line %d of %s, setting to Susceptible",
                     XML_GetCurrentLineNumber (parser), filename);
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
                ("\"%s\" is not a valid numeric status code on line %d of %s, setting to 0 (Susceptible)",
                 tmp, XML_GetCurrentLineNumber (parser), filename);
              status = Susceptible;
            }
        }
      else if (strcasecmp (tmp, "S") == 0 || strcasecmp (tmp, "Susceptible") == 0)
        status = Susceptible;
      else if (strcasecmp (tmp, "L") == 0
               || strcasecmp (tmp, "Latent") == 0 || strcasecmp (tmp, "Incubating") == 0)
        status = Latent;
      else if (strcasecmp (tmp, "B") == 0
               || strcasecmp (tmp, "Infectious Subclinical") == 0
               || strcasecmp (tmp, "InfectiousSubclinical") == 0
               || strcasecmp (tmp, "Inapparent Shedding") == 0
               || strcasecmp (tmp, "InapparentShedding") == 0)
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
               || strcasecmp (tmp, "Vaccine Immune") == 0 || strcasecmp (tmp, "VaccineImmune") == 0)
        status = VaccineImmune;
      else if (strcasecmp (tmp, "D") == 0
               || strcasecmp (tmp, "Dead") == 0 || strcasecmp (tmp, "Destroyed") == 0)
        status = Destroyed;
      else
        {
          g_warning ("\"%s\" is not a valid unit state on line %d of %s, setting to Susceptible",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          status = Susceptible;
        }
      partial->herd->status = partial->herd->initial_status = status;

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
          g_warning ("days-left-in-status missing on line %d of %s, setting to 0",
                     XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (errno == ERANGE || errno == EINVAL)
        {
          g_warning
            ("days-left-in-status is too large a number (\"%s\") on line %d of %s, setting to 0",
             tmp, XML_GetCurrentLineNumber (parser), filename);
          days = 0;
          errno = 0;
        }
      else if (*endptr != '\0')
        {
          g_warning ("days-left-in-status is not a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (days < 0)
        {
          g_warning
            ("days-left-in-status cannot be negative (\"%s\") on line %d of %s, setting to 0", tmp,
             XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      partial->herd->days_left_in_initial_status = (unsigned short int) days;
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
      free (s);
#endif
#endif
      HRD_herd_list_append (partial->herds, partial->herd);
      HRD_free_herd (partial->herd, FALSE);
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
  g_string_sprintf (s, "<%s herd id=%s size=%u lat=%f lon=%f",
                    herd->production_type_name, herd->official_id,
                    herd->size, herd->lat, herd->lon);

  /* Print the status, plus days left if applicable. */
  g_string_append_printf (s, "\n %s", HRD_status_name[herd->status]);
  if (herd->days_left_in_initial_status > 0)
    g_string_append_printf (s, " (%hu days left) ", herd->days_left_in_initial_status);

  /* Print delayed transitions. */
#if 0
  for (iter = herd->delayed_transitions; iter != NULL; iter = g_list_next (iter))
    {
      transition = (HRD_delayed_transition_t *) (iter->data);
      substring = HRD_delayed_transition_to_string (transition);
      g_string_sprintfa (s, "\n %s", substring);
      free (substring);
    }
#endif
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
  herds->spatial_index = RTreeNewIndex ();

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
      g_slist_foreach (herd->change_requests, HRD_free_change_request_as_GFunc, NULL);
    }

  /* Free the herd structures. */
  g_array_free (herds->list, TRUE);

  /* Free the production type names. */
  for (i = 0; i < herds->production_type_names->len; i++)
    free (g_ptr_array_index (herds->production_type_names, i));
  g_ptr_array_free (herds->production_type_names, TRUE);

  /* Free the spatial index. */
  RTreeDeleteIndex (herds->spatial_index);

  /* Finally, free the herd list structure. */
  g_free (herds);

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_free_herd_list");
#endif
  return;
}



/**
 * Calculates a minimum-area oriented rectangle bounding the herds.
 *
 * @param herds a herd list.
 */
void
HRD_herd_list_compute_bounding_box (HRD_herd_list_t * herds)
{
  double *latlon = NULL, *p;
  unsigned int nherds;
  HRD_herd_t *herd;
  gboolean all_x_same, all_y_same;
  double **hull;
  unsigned int hull_npoints = 0;
  WML_Vector2 *hull2 = NULL;
  WML_Box2 *minbox = NULL;
  WML_Vector2 corner[4];
  unsigned int i;               /* loop counter */
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_herd_list_compute_bounding_box");
#endif

  nherds = HRD_herd_list_length (herds);
  if (nherds == 0)
    goto end;

  /* Deal with 2 special cases first. */
  if (nherds == 1)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "only 1 unit: bounding box is a point");
#endif
      herd = HRD_herd_list_get (herds, 0);
      for (i = 0; i < 4; i++)
        {
          herds->oriented_rect[2 * i] = herd->lat;
          herds->oriented_rect[2 * i + 1] = herd->lon;
        }
      goto end;
    }

  if (nherds == 2)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "only 2 units: bounding box is a line");
#endif
      herd = HRD_herd_list_get (herds, 0);
      for (i = 0; i < 2; i++)
        {
          herds->oriented_rect[2 * i] = herd->lat;
          herds->oriented_rect[2 * i + 1] = herd->lon;
        }
      herd = HRD_herd_list_get (herds, 1);
      for (i = 2; i < 4; i++)
        {
          herds->oriented_rect[2 * i] = herd->lat;
          herds->oriented_rect[2 * i + 1] = herd->lon;
        }
      goto end;
    }

  /* Copy the positions of the herds into a temporary array. */
  latlon = g_new (double, nherds * 2);
  for (i = 0, p = latlon; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      *p++ = (double) herd->lat;
      *p++ = (double) herd->lon;
    }

  /* Get the convex hull around the locations. */
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "getting convex hull around %u units", nherds);
#endif
  hull = g_new (double *, nherds + 1);
  all_x_same = all_y_same = TRUE;
  for (i = 0; i < nherds; i++)
    {
      hull[i] = latlon + 2 * i;
      if (i > 0)
        {
          if (all_x_same && gsl_fcmp (hull[i][1], hull[i - 1][1], EPSILON) != 0)
            all_x_same = FALSE;
          if (all_y_same && gsl_fcmp (hull[i][0], hull[i - 1][0], EPSILON) != 0)
            all_y_same = FALSE;
        }
    }
  if (all_x_same && all_y_same)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "all %u units have the same location: bounding box is a point", nherds);
#endif
      herd = HRD_herd_list_get (herds, 0);
      for (i = 0; i < 4; i++)
        {
          herds->oriented_rect[2 * i] = herd->lat;
          herds->oriented_rect[2 * i + 1] = herd->lon;
        }
      goto end;
    }
  else if (all_x_same)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "all %u units have the same longitude: bounding box is a line", nherds);
#endif
      for (i = 0; i < 4; i++)
        herds->oriented_rect[2 * i + 1] = herds->limits.boundary[1];
      herds->oriented_rect[0] = herds->oriented_rect[2] = herds->limits.boundary[0];
      herds->oriented_rect[4] = herds->oriented_rect[6] = herds->limits.boundary[2];
      goto end;
    }
  else if (all_y_same)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "all %u units have the same latitude: bounding box is a line", nherds);
#endif
      for (i = 0; i < 4; i++)
        herds->oriented_rect[2 * i] = herds->limits.boundary[0];
      herds->oriented_rect[1] = herds->oriented_rect[3] = herds->limits.boundary[1];
      herds->oriented_rect[5] = herds->oriented_rect[7] = herds->limits.boundary[3];
      goto end;
    }

  hull_npoints = ch2d (hull, nherds);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "convex hull has %u points", hull_npoints);
#endif

  /* Get the minimum-area oriented box around the hull.  First we copy the
   * latlon array into an array of WML_Vector2 objects, since that's what the
   * function expects. */
  hull2 = g_new (WML_Vector2, hull_npoints);
  for (i = 0; i < hull_npoints; i++)
    {
      hull2[i].X = hull[i][1];
      hull2[i].Y = hull[i][0];
    }
  g_free (hull);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "getting minimum-area box around hull");
#endif
  /* FIXME: replace with the O(n log n) algorithm once it's working. */
  minbox = WML_MinBoxOrderNSqr (hull_npoints, hull2);
  WML_Box2_ComputeVertices (minbox, corner);

  /* Copy the corners into the herd list. */
  for (i = 0; i < 4; i++)
    {
      herds->oriented_rect[2 * i] = corner[i].Y;
      herds->oriented_rect[2 * i + 1] = corner[i].X;
    }
#if DEBUG
  s = g_string_new ("minimum-area oriented rectangle (lat,lon): [");
  for (i = 0; i < 4; i++)
    {
      if (i > 0)
        g_string_append_c (s, ',');
      g_string_append_printf (s, "(%g,%g)", corner[i].Y, corner[i].X);
    }
  g_string_append_c (s, ']');
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif

  /* Finally, compute the edge lengths for the bounding rectangle. */
  herds->xaxis_length = (GIS_local_distance (corner[0].Y, corner[0].X,
                                             corner[3].Y, corner[3].X) +
                         GIS_local_distance (corner[1].Y, corner[1].X,
                                             corner[2].Y, corner[2].X)) / 2.0;
  herds->yaxis_length = (GIS_local_distance (corner[0].Y, corner[0].X,
                                             corner[1].Y, corner[1].X) +
                         GIS_local_distance (corner[2].Y, corner[2].X,
                                             corner[3].Y, corner[3].X)) / 2.0;
  herds->short_axis_length = MIN (herds->xaxis_length, herds->yaxis_length);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "side lengths (km) = %g,%g",
         herds->xaxis_length, herds->yaxis_length);
#endif

end:
  /* Clean up. */
  if (latlon != NULL)
    g_free (latlon);
  if (hull2 != NULL)
    g_free (hull2);
  if (minbox != NULL)
    WML_free_Box2 (minbox);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_herd_list_compute_bounding_box");
#endif
  return;
}



/**
 * Loads a herd list from a file.
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
    g_error ("could not open file \"%s\": %s", filename, strerror (errno));
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
  to_pass.filename = filename;
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
            g_error ("%s at line %d in %s",
                     XML_ErrorString (XML_GetErrorCode (parser)),
                     XML_GetCurrentLineNumber (parser), filename);
          break;
        }
      xmlerr = XML_Parse (parser, linebuf, linelen, 0);
      if (xmlerr == XML_STATUS_ERROR)
        g_error ("%s at line %d in %s",
                 XML_ErrorString (XML_GetErrorCode (parser)),
                 XML_GetCurrentLineNumber (parser), filename);
    }

  HRD_herd_list_compute_bounding_box (herds);

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
  struct Rect rect;

  list = herds->list;
  g_array_append_val (list, *herd);
  new_length = HRD_herd_list_length (herds);

  /* Now make the pointer point to the copy in the herd list. */
  herd = &g_array_index (list, HRD_herd_t, new_length - 1);

  /* Set the list index number for the herd. */
  herd->index = new_length - 1;

  /* Insert the herd into the spatial index. */
  rect.boundary[0] = herd->lon;
  rect.boundary[1] = herd->lat;
  rect.boundary[2] = herd->lon;
  rect.boundary[3] = herd->lat;
  /* Notice the rectangle ID is the herd index +1, since rectangle IDs in the
   * R-tree start from 1. */
#if !defined(USE_RTREE) || USE_RTREE != 0
  RTreeInsertRect (&rect, new_length, &(herds->spatial_index), 0);
#endif

  /* Keep track of the boundaries of the study area. */
  if (new_length == 1)
    {
      herds->limits.boundary[0] = herd->lon;
      herds->limits.boundary[1] = herd->lat;
      herds->limits.boundary[2] = herd->lon;
      herds->limits.boundary[3] = herd->lat;
    }
  else
    {
      if (herd->lon < herds->limits.boundary[0])
        herds->limits.boundary[0] = herd->lon;
      else if (herd->lon > herds->limits.boundary[2])
        herds->limits.boundary[2] = herd->lon;

      if (herd->lat < herds->limits.boundary[1])
        herds->limits.boundary[1] = herd->lat;
      else if (herd->lat > herds->limits.boundary[3])
        herds->limits.boundary[3] = herd->lat;
    }

  return new_length;
}



/**
 * Returns the herds with a given status.
 *
 * @param herds a herd list.
 * @param status the desired status.
 * @param list a location in which to store the address of a list of pointers
 *   to herds.
 * @return the number of herds with the given status.
 */
unsigned int
HRD_herd_list_get_by_status (HRD_herd_list_t * herds, HRD_status_t status, HRD_herd_t *** list)
{
  unsigned int nherds;
  HRD_herd_t *herd;
  unsigned int count = 0;
  unsigned int i;

  /* Count the herds with the given status. */
  nherds = HRD_herd_list_length (herds);
  for (i = 0; i < nherds; i++)
    if (HRD_herd_list_get (herds, i)->status == status)
      count++;

  if (count == 0)
    (*list) = NULL;
  else
    {
      /* Allocate and fill the array. */
      *list = g_new (HRD_herd_t *, count);
      count = 0;
      for (i = 0; i < nherds; i++)
        {
          herd = HRD_herd_list_get (herds, i);
          if (herd->status == status)
            (*list)[count++] = herd;
        }
    }
  return count;
}



/**
 * Returns the minimum-area oriented rectangle bounding the herds.  Note that
 * this information will be available only if you used HRD_load_herd_list() to
 * create the herd list.
 *
 * @param herds a herd list.
 * @param corners a location in which to store the corners of the bounding
 *   rectangle.  They are stored in the format lat1,lon1,lat2,lon2,...
 */
void
HRD_herd_list_get_bounding_box (HRD_herd_list_t * herds, double *corners)
{
  unsigned int i;

  for (i = 0; i < 8; i++)
    corners[i] = herds->oriented_rect[i];
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
      free (substring);
      for (i = 1; i < nherds; i++)
        {
          substring = HRD_herd_to_string (HRD_herd_list_get (herds, i));
          g_string_append_printf (s, "\n%s", substring);
          free (substring);
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
       
      if( (Latent == herd_status)
          || (InfectiousSubclinical == herd_status) 
          || (InfectiousClinical == herd_status) ) 
        {
          if( FALSE == first_infected_found )
            {
              first_infected_found = TRUE;
              
              g_string_sprintf (
                s, "%i, %s, s%is, %f", /* The second and third "s"'s in the string look  
                                            * funny, but they're there for a reason. */
                day,
                HRD_herd_list_get (herds, i)->official_id, 
                herd_status,
                HRD_herd_list_get (herds, i)->prevalence 
              );  
            }
          else
            {
              g_string_sprintfa (
                s, "\r\n%i, %s, s%is, %f", /* The second and third "s"'s in the string look  
                                            * funny, but they're there for a reason. */
                day,
                HRD_herd_list_get (herds, i)->official_id, 
                herd_status,
                HRD_herd_list_get (herds, i)->prevalence 
              );
            }
        }
    }

  if( FALSE == first_infected_found )
    g_string_sprintf( s, "%i, (No infected units)", day );  

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
 * Resets a herd to alive, Susceptible, and not quarantined.
 *
 * @param herd a herd.
 */
void
HRD_reset (HRD_herd_t * herd)
{
  herd->status = Susceptible;
  herd->days_in_status = 0;
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
 *     Orders to quarantine or lift a quarantine are processed first.  An order
 *     to quarantine overrides one or more orders to lift quarantine.
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
 */
void
HRD_step (HRD_herd_t * herd)
{
  GSList *iter;
  HRD_change_request_t *request;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER HRD_step");
#endif

  herd->days_in_status++;

  /* Apply requested changes in the order in which they occur. */

  for (iter = herd->change_requests; iter != NULL; iter = g_slist_next (iter))
    {
      request = (HRD_change_request_t *) (iter->data);
      HRD_apply_change_request (herd, request);
    }
  HRD_herd_clear_change_requests (herd);

  /* Quarantine doesn't conflict with anything, but an order to quarantine
   * trumps an order to lift quarantine. */

  /* Take any delayed transitions. */
  if (herd->in_vaccine_cycle)
    {
      if (herd->immunity_start_countdown-- == 0)
        HRD_change_state (herd, VaccineImmune);
      if (herd->immunity_end_countdown-- == 0)
        {
          HRD_change_state (herd, Susceptible);
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
        }
      else
        {
          herd->prevalence = 0;
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "prevalence = 0");
#endif
        }

      herd->day_in_disease_cycle++;
      if (herd->infectious_start_countdown-- == 0)
        HRD_change_state (herd, InfectiousSubclinical);
      if (herd->clinical_start_countdown-- == 0)
        HRD_change_state (herd, InfectiousClinical);
      if (herd->immunity_start_countdown-- == 0)
        HRD_change_state (herd, NaturallyImmune);
      if (herd->immunity_end_countdown-- == 0)
        {
          HRD_change_state (herd, Susceptible);
          herd->in_disease_cycle = FALSE;
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT HRD_step");
#endif
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
 */
void
HRD_infect (HRD_herd_t * herd,
            unsigned short int latent_period,
            unsigned short int infectious_subclinical_period,
            unsigned short int infectious_clinical_period, unsigned short int immunity_period)
{
  HRD_herd_add_change_request (herd,
                               HRD_new_infect_change_request
                               (latent_period, infectious_subclinical_period,
                                infectious_clinical_period, immunity_period));
}



/**
 * Vaccinates a herd against a disease.
 *
 * @param herd the herd to be vaccinated.
 * @param delay the number of days before immunity begins.
 * @param immunity_period the number of days the immunity lasts.
 */
void
HRD_vaccinate (HRD_herd_t * herd, unsigned short int delay, unsigned short int immunity_period)
{
  HRD_herd_add_change_request (herd, HRD_new_vaccinate_change_request (delay, immunity_period));
}



/**
 * Quarantines a herd.
 *
 * @param herd the herd to be quarantined.
 */
void
HRD_quarantine (HRD_herd_t * herd)
{
  HRD_herd_add_change_request (herd, HRD_new_quarantine_change_request ());
}



/**
 * Lifts a quarantine on a herd.
 *
 * @param herd the herd to have its quarantine lifted.
 */
void
HRD_lift_quarantine (HRD_herd_t * herd)
{
  HRD_herd_add_change_request (herd, HRD_new_lift_quarantine_change_request ());
}



/**
 * Destroys a herd.
 *
 * @param herd the herd to be destroyed.
 */
void
HRD_destroy (HRD_herd_t * herd)
{
  HRD_herd_add_change_request (herd, HRD_new_destroy_change_request ());
}

/* end of file herd.c */
