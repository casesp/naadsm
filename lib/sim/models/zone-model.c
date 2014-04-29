/** @file zone-model.c
 * Parameters that describe a zone.  Specifically,
 * <ul>
 *   <li> the zone's name
 *   <li> the zone's radius
 *   <li> the zone's surveillance level, where level 1 should correspond to the
 *     smallest radius and probably the most intensive surveillance, level 2
 *     should correspond to the second-smallest radius and probably the
 *     second-most intensive surveillance, and so on.
 * </ul>
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date October 2004
 *
 * Copyright &copy; University of Guelph, 2004-2008
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
#define interface_version zone_model_LTX_interface_version
#define new zone_model_LTX_new
#define run zone_model_LTX_run
#define reset zone_model_LTX_reset
#define events_listened_for zone_model_LTX_events_listened_for
#define is_listening_for zone_model_LTX_is_listening_for
#define has_pending_actions zone_model_LTX_has_pending_actions
#define has_pending_infections zone_model_LTX_has_pending_infections
#define to_string zone_model_LTX_to_string
#define local_printf zone_model_LTX_printf
#define local_fprintf zone_model_LTX_fprintf
#define local_free zone_model_LTX_free
#define handle_new_day_event zone_model_LTX_handle_new_day_event

#include "model.h"
#include "gis.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#include "guilib.h"

#include "zone-model.h"

extern const char *RPT_frequency_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "zone-model"

#define MODEL_DESCRIPTION "\
A component to describe a zone.\n\
\n\
Neil Harvey <neilharvey@canada.com>\n\
v0.1 October 2004\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 0
EVT_event_type_t events_created[] = { 0 };

#define NEVENTS_LISTENED_FOR 1
EVT_event_type_t events_listened_for[] = { EVT_NewDay };



/* Specialized information for this model. */
typedef struct
{
  ZON_zone_t *zone;
  RPT_reporting_t *num_fragments;
  RPT_reporting_t *num_holes_filled;
  RPT_reporting_t *cumul_num_holes_filled;
}
local_data_t;



/**
 * Responds to a new day event by updating the reporting variables.
 *
 * @param self the model.
 * @param event a new day event.
 */
void
handle_new_day_event (struct ergadm_model_t_ *self, EVT_new_day_event_t * event)
{
  local_data_t *local_data;
  ZON_zone_t *zone;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  zone = local_data->zone;

  if (local_data->num_fragments->frequency != RPT_never)
    RPT_reporting_set_integer1 (local_data->num_fragments,
                                g_queue_get_length (zone->fragments), zone->name);
  if (local_data->num_holes_filled->frequency != RPT_never)
    RPT_reporting_set_integer1 (local_data->num_holes_filled, zone->nholes_filled, zone->name);
  if (local_data->cumul_num_holes_filled->frequency != RPT_never)
    RPT_reporting_add_integer1 (local_data->cumul_num_holes_filled,
                                zone->nholes_filled, zone->name);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
}



/**
 * Runs this model.
 *
 * @param self the model.
 * @param herds a list of herds.
 * @param zones a list of zones.
 * @param event the event that caused the model to run.
 * @param rng a random number generator.
 * @param queue for any new events the model creates.
 */
void
run (struct ergadm_model_t_ *self, HRD_herd_list_t * herds,
     ZON_zone_list_t * zones, EVT_event_t * event, RAN_gen_t * rng, EVT_event_queue_t * queue)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER run (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "ENTER run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }
  
  switch (event->type)
    {
    case EVT_NewDay:
      handle_new_day_event (self, &(event->u.new_day));
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT run (%s)", MODEL_NAME);
#endif
  if( NULL != guilib_printf ) {
    char guilog[1024];
    sprintf( guilog, "EXIT run %s", MODEL_NAME); 
    //guilib_printf( guilog );
  }
}



/**
 * Resets this model after a simulation run.
 *
 * @param self the model.
 */
void
reset (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER reset (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);
  RPT_reporting_zero (local_data->num_fragments);
  RPT_reporting_zero (local_data->num_holes_filled);
  RPT_reporting_zero (local_data->cumul_num_holes_filled);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT reset (%s)", MODEL_NAME);
#endif
}



/**
 * Reports whether this model is listening for a given event type.
 *
 * @param self the model.
 * @param event_type an event type.
 * @return TRUE if the model is listening for the event type.
 */
gboolean
is_listening_for (struct ergadm_model_t_ *self, EVT_event_type_t event_type)
{
  int i;

  for (i = 0; i < self->nevents_listened_for; i++)
    if (self->events_listened_for[i] == event_type)
      return TRUE;
  return FALSE;
}



/**
 * Reports whether this model has any pending actions to carry out.
 *
 * @param self the model.
 * @return TRUE if the model has pending actions.
 */
gboolean
has_pending_actions (struct ergadm_model_t_ * self)
{
  return FALSE;
}



/**
 * Reports whether this model has any pending infections to cause.
 *
 * @param self the model.
 * @return TRUE if the model has pending infections.
 */
gboolean
has_pending_infections (struct ergadm_model_t_ * self)
{
  return FALSE;
}



/**
 * Returns a text representation of this model.
 *
 * @param self the model.
 * @return a string.
 */
char *
to_string (struct ergadm_model_t_ *self)
{
  GString *s;
  char *chararray;
  local_data_t *local_data;
  ZON_zone_t *zone;

  local_data = (local_data_t *) (self->model_data);
  s = g_string_new (NULL);
  zone = local_data->zone;
  g_string_sprintf (s, "<%s \"%s\" level=%i radius=%.2f >", MODEL_NAME,
                    zone->name, zone->level, zone->radius);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Prints this model to a stream.
 *
 * @param stream a stream to write to.
 * @param self the model.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_fprintf (FILE * stream, struct ergadm_model_t_ *self)
{
  char *s;
  int nchars_written;

  s = to_string (self);
  nchars_written = fprintf (stream, "%s", s);
  free (s);
  return nchars_written;
}



/**
 * Prints this model.
 *
 * @param self the model.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
local_printf (struct ergadm_model_t_ *self)
{
  return local_fprintf (stdout, self);
}



/**
 * Frees this model.  Does not free the production type names.
 *
 * @param self the model.
 */
void
local_free (struct ergadm_model_t_ *self)
{
  local_data_t *local_data;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER free (%s)", MODEL_NAME);
#endif

  /* Free the dynamically-allocated parts. */
  local_data = (local_data_t *) (self->model_data);
  RPT_free_reporting (local_data->num_fragments, TRUE);
  RPT_free_reporting (local_data->num_holes_filled, TRUE);
  RPT_free_reporting (local_data->cumul_num_holes_filled, TRUE);
  g_free (local_data);
  g_ptr_array_free (self->outputs, TRUE);
  g_free (self);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT free (%s)", MODEL_NAME);
#endif
}



/**
 * Returns the version of the interface this model conforms to.
 */
char *
interface_version (void)
{
  return MODEL_INTERFACE_VERSION;
}



/**
 * Returns a new zone model.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *m;
  local_data_t *local_data;
  scew_element const *e, **ee;
  unsigned int noutputs;
  RPT_reporting_t *output;
  const XML_Char *variable_name;
  gboolean success;
  int i, j;
  char *name;
  int level;
  double radius;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER new (%s)", MODEL_NAME);
#endif

  m = g_new (ergadm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  m->name = MODEL_NAME;
  m->description = MODEL_DESCRIPTION;
  m->events_created = events_created;
  m->nevents_created = NEVENTS_CREATED;
  m->events_listened_for = events_listened_for;
  m->nevents_listened_for = NEVENTS_LISTENED_FOR;
  m->outputs = g_ptr_array_sized_new (3);
  m->model_data = local_data;
  m->run = run;
  m->reset = reset;
  m->is_listening_for = is_listening_for;
  m->has_pending_actions = has_pending_actions;
  m->has_pending_infections = has_pending_infections;
  m->to_string = to_string;
  m->printf = local_printf;
  m->fprintf = local_fprintf;
  m->free = local_free;

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  e = scew_element_by_name (params, "name");
  if (e != NULL)
    {
      /* Expat stores the text as UTF-8.  Convert to ISO-8859-1. */
      name = g_convert_with_fallback (PAR_get_text (e), -1, "ISO-8859-1", "UTF-8", "?", NULL, NULL, NULL);
      g_assert (name != NULL);
    }
  else
    name = g_strdup ("unnamed zone");

  e = scew_element_by_name (params, "level");
  if (e != NULL)
    {
      level = PAR_get_unitless (e, &success);
      if (success == FALSE)
        {
          /* This value will be re-assigned later, when the zone is added to
           * the zone list. */
          level = -1;
        }
    }
  else
    level = -1;

  e = scew_element_by_name (params, "radius");
  if (e != NULL)
    {
      radius = PAR_get_length (e, &success);
      if (success == FALSE)
        {
          g_warning ("setting zone radius to 0");
          radius = 0;
        }
      /* Radius must be positive. */
      if (radius < 0)
        {
          g_warning ("zone radius cannot be negative, setting to 0");
          radius = 0;
        }
    }
  else
    {
      g_warning ("zone radius missing, setting to 0");
      radius = 0;
    }

  local_data->zone = ZON_new_zone (name, level, radius);
  free (name);

  local_data->num_fragments = RPT_new_reporting ("num-fragments", NULL, RPT_group, RPT_never, TRUE);
  local_data->num_holes_filled =
    RPT_new_reporting ("num-holes-filled", NULL, RPT_group, RPT_never, FALSE);
  local_data->cumul_num_holes_filled =
    RPT_new_reporting ("cumulative-num-holes-filled", NULL, RPT_group, RPT_never, TRUE);
  g_ptr_array_add (m->outputs, local_data->num_fragments);
  g_ptr_array_add (m->outputs, local_data->num_holes_filled);
  g_ptr_array_add (m->outputs, local_data->cumul_num_holes_filled);

  /* Set the reporting frequency for the output variables. */
  ee = scew_element_list (params, "output", &noutputs);
#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i output variables", noutputs);
#endif
  for (i = 0; i < noutputs; i++)
    {
      e = ee[i];
      variable_name = scew_element_contents (scew_element_by_name (e, "variable-name"));
      /* Do the outputs include a variable with this name? */
      for (j = 0; j < m->outputs->len; j++)
        {
          output = (RPT_reporting_t *) g_ptr_array_index (m->outputs, j);
          if (strcmp (output->name, variable_name) == 0)
            break;
        }
      if (j == m->outputs->len)
        g_warning ("no output variable named \"%s\", ignoring", variable_name);
      else
        {
          RPT_reporting_set_frequency (output,
                                       RPT_string_to_frequency (scew_element_contents
                                                                (scew_element_by_name
                                                                 (e, "frequency"))));
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "report \"%s\" %s", variable_name,
                 RPT_frequency_name[output->frequency]);
#endif
        }
    }
  free (ee);

  /* Use the following heuristic to decide whether to use the R-tree index in
   * searches: if the ratio of the diameter of the zone to the short axis of
   * the oriented bounding rectangle around the herds is <= 0.25, use the
   * R-tree. */

  /* For debugging purposes, you can #define USE_RTREE to 0 to never use the
   * spatial index, or 1 to always use it. */
#if defined(USE_RTREE)
  local_data->zone->use_rtree_index = USE_RTREE;
#else
  if (herds->short_axis_length > 0)
    local_data->zone->use_rtree_index = 0.25 / 2 * herds->short_axis_length;
  else
    local_data->zone->use_rtree_index = 0;
#endif

  /* Add the zone object to the zone list. */
  ZON_zone_list_append (zones, local_data->zone);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return m;
}

char *
zone_model_interface_version (void)
{
  return interface_version ();
}

ergadm_model_t *
zone_model_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file zone-model.c */
