/** @file zone-monitor.c
 * Tracks the shape and area of zones, number of units in each zone, and number
 * of animal-days spent in each zone.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date Feb 2007
 *
 * Copyright &copy; University of Guelph, 2007
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
#define interface_version zone_monitor_LTX_interface_version
#define new zone_monitor_LTX_new
#define run zone_monitor_LTX_run
#define reset zone_monitor_LTX_reset
#define events_listened_for zone_monitor_LTX_events_listened_for
#define is_listening_for zone_monitor_LTX_is_listening_for
#define has_pending_actions zone_monitor_LTX_has_pending_actions
#define has_pending_infections zone_monitor_LTX_has_pending_infections
#define to_string zone_monitor_LTX_to_string
#define local_printf zone_monitor_LTX_printf
#define local_fprintf zone_monitor_LTX_fprintf
#define local_free zone_monitor_LTX_free
#define handle_new_day_event zone_monitor_LTX_handle_new_day_event
#define handle_last_day_event zone_monitor_LTX_handle_last_day_event
#define events_created zone_monitor_LTX_events_created

#include "model.h"
#include "gis.h"
#include "guilib.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#include "zone-monitor.h"

extern const char *RPT_frequency_name[];

/** This must match an element name in the DTD. */
#define MODEL_NAME "zone-monitor"

#define MODEL_DESCRIPTION "\
A module to track the shape and area of zones.\n\
\n\
Neil Harvey <neilharvey@gmail.com>\n\
v0.1 Feb 2007\
"

#define MODEL_INTERFACE_VERSION "0.93"



#define NEVENTS_CREATED 0
EVT_event_type_t events_created[] = { 0 };

#define NEVENTS_LISTENED_FOR 2
EVT_event_type_t events_listened_for[] = { EVT_NewDay, EVT_LastDay };



/* Specialized information for this model. */
typedef struct
{
  int nzones;
  RPT_reporting_t *shape;
  RPT_reporting_t *area;
  RPT_reporting_t *num_separate_areas;
  RPT_reporting_t *num_units;
  RPT_reporting_t *num_animal_days_by_prodtype;
}
local_data_t;



/**
 * Creates a representation of a polygon in OpenGIS Well-Known Text (WKT)
 * format.
 */
GString *
polygon_to_wkt (gpc_polygon * poly)
{
  GString *s;
  int num_contours, num_vertices;
  int i, j;
  gpc_vertex_list *contour;
  gpc_vertex *vertex;

  s = g_string_new ("POLYGON(");
  num_contours = poly->num_contours;
  for (i = 0; i < num_contours; i++)
    {
      g_string_append_c (s, '(');
      contour = &(poly->contour[i]);
      num_vertices = contour->num_vertices;
      for (j = 0; j < num_vertices; j++)
        {
          vertex = &(contour->vertex[j]);
          g_string_append_printf (s, "%g %g,", vertex->x, vertex->y);
        }
      /* Repeat the initial point to close the contour, as required by the
       * WKT format. */
      vertex = &(contour->vertex[0]);
      g_string_append_printf (s, "%g %g)", vertex->x, vertex->y);
    }
  g_string_append_c (s, ')');

  return s;
}



/**
 * Responds to a new day event by updating the reporting variables.
 *
 * @param self the model.
 * @param herds a list of herds.
 * @param zones a list of zones.
 * @param event a new day event.
 */
void
handle_new_day_event (struct ergadm_model_t_ *self, HRD_herd_list_t * herds,
                      ZON_zone_list_t * zones, EVT_new_day_event_t * event)
{
  local_data_t *local_data;
  gboolean shape_due, area_due, num_areas_due, num_units_due;
  int i;
  ZON_zone_t *zone, *next_smaller_zone;
  GString *s;
  double area;
  unsigned int nherds;
  HRD_herd_t *herd;
  char *drill_down_list[3] = { NULL, NULL, NULL };

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_new_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  shape_due = RPT_reporting_due (local_data->shape, event->day);
  area_due = RPT_reporting_due (local_data->area, event->day);
  num_areas_due = RPT_reporting_due (local_data->num_separate_areas, event->day);
  num_units_due = RPT_reporting_due (local_data->num_units, event->day);

  for (i = 0; i < local_data->nzones; i++)
    {
      zone = ZON_zone_list_get (zones, i);

      if (shape_due)
        {
          s = polygon_to_wkt (zone->poly);
          RPT_reporting_set_text1 (local_data->shape, s->str, zone->name);
          /* The string was copied so it can be freed. */
          g_string_free (s, TRUE);
        }

      if (area_due)
        {
          area = ZON_update_area (zone);
          RPT_reporting_set_real1 (local_data->area, area, zone->name);
        }

      if (num_areas_due)
        RPT_reporting_set_integer1 (local_data->num_separate_areas,
                                    zone->poly->num_contours, zone->name);
    }

  /* In the loop above, the area of each zone polygon was computed.  But since
   * zones are nested inside of each other, that's not exactly what we want:
   * we want the area displayed for an "outer" zone to exclude the area of the
   * smaller "inner" zones.  So we do that computation here. */
  if (area_due)
    {
      /* Start with the next-to-last zone, because the last one is the
       * "background" zone. */
      for (i = local_data->nzones - 2; i > 0; i--)
        {
          zone = ZON_zone_list_get (zones, i);
          next_smaller_zone = ZON_zone_list_get (zones, i - 1);
          zone->area -= next_smaller_zone->area;
          RPT_reporting_set_real1 (local_data->area, zone->area, zone->name);

          if (NULL != guilib_record_zone_area)
            guilib_record_zone_area (zone->level, zone->area);
        }

      /* Don't forget to report the smallest zone to the GUI! */
      if (NULL != guilib_record_zone_area)
        {
          zone = ZON_zone_list_get (zones, 0);
          guilib_record_zone_area (zone->level, zone->area);
        }
    }

  if (local_data->num_animal_days_by_prodtype->frequency != RPT_never || num_units_due)
    {
      nherds = zones->membership_length;
      for (i = 0; i < local_data->nzones; i++)
        {
          zone = ZON_zone_list_get (zones, i);
          RPT_reporting_set_integer1 (local_data->num_units, 0, zone->name);
        }
      for (i = 0; i < nherds; i++)
        {
          zone = zones->membership[i]->parent;
          RPT_reporting_add_integer1 (local_data->num_units, 1, zone->name);
          herd = HRD_herd_list_get (herds, i);
          if (herd->status != Destroyed)
            {
              drill_down_list[0] = herd->production_type_name;
              drill_down_list[1] = zone->name;
              RPT_reporting_add_integer (local_data->num_animal_days_by_prodtype, herd->size,
                                         drill_down_list);
            }
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_new_day_event (%s)", MODEL_NAME);
#endif
}



/**
 * Responds to a last day event by computing output variables that are only
 * needed on the last day.
 *
 * @param self the model.
 * @param zones a list of zones.
 * @param event a last day event.
 */
void
handle_last_day_event (struct ergadm_model_t_ *self, ZON_zone_list_t * zones,
                       EVT_new_day_event_t * event)
{
  local_data_t *local_data;
  gboolean skip_shape, skip_area, skip_num_areas, skip_num_units;
  int i;
  ZON_zone_t *zone, *next_smaller_zone;
  GString *s;
  double area;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER handle_last_day_event (%s)", MODEL_NAME);
#endif

  local_data = (local_data_t *) (self->model_data);

  /* Some of the output variables are computationally intensive.  If they're
   * reported "never", or if they were already computed today by
   * handle_new_day_event, don't bother to compute them. */
  skip_shape = local_data->shape->frequency == RPT_never
    || RPT_reporting_due (local_data->shape, event->day);
  skip_area = local_data->area->frequency == RPT_never
    || RPT_reporting_due (local_data->area, event->day);
  skip_num_areas = local_data->num_separate_areas->frequency == RPT_never
    || RPT_reporting_due (local_data->num_separate_areas, event->day);
  skip_num_units = local_data->num_units->frequency == RPT_never
    || RPT_reporting_due (local_data->num_units, event->day);

  /* We don't have to worry about num_animal_days_by_prodtype because, unless
   * it's set to be reported "never", it will be updated every day by
   * handle_new_day_event. */

  for (i = 0; i < local_data->nzones; i++)
    {
      zone = ZON_zone_list_get (zones, i);

      if (!skip_shape)
        {
          s = polygon_to_wkt (zone->poly);
          RPT_reporting_set_text1 (local_data->shape, s->str, zone->name);
          /* The string was copied so it can be freed. */
          g_string_free (s, TRUE);
        }

      if (!skip_area)
        {
          area = ZON_update_area (zone);
          RPT_reporting_set_real1 (local_data->area, area, zone->name);
        }

      if (!skip_num_areas)
        RPT_reporting_set_integer1 (local_data->num_separate_areas,
                                    zone->poly->num_contours, zone->name);
    }

  if (!skip_area)
    {
      /* Start with the next-to-last zone, because the last one is the
       * "background" zone. */
      for (i = local_data->nzones - 2; i > 0; i--)
        {
          zone = ZON_zone_list_get (zones, i);
          next_smaller_zone = ZON_zone_list_get (zones, i - 1);
          zone->area -= next_smaller_zone->area;
          RPT_reporting_set_real1 (local_data->area, zone->area, zone->name);
        }
    }

  if (!skip_num_units)
    {
      for (i = 0; i < local_data->nzones; i++)
        {
          zone = ZON_zone_list_get (zones, i);
          RPT_reporting_set_integer1 (local_data->num_units, 0, zone->name);
        }
      for (i = 0; i < zones->membership_length; i++)
        {
          zone = zones->membership[i]->parent;
          RPT_reporting_add_integer1 (local_data->num_units, 1, zone->name);
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT handle_last_day_event (%s)", MODEL_NAME);
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

  switch (event->type)
    {
    case EVT_NewDay:
      handle_new_day_event (self, herds, zones, &(event->u.new_day));
      break;
    case EVT_LastDay:
      handle_last_day_event (self, zones, &(event->u.last_day));
      break;
    default:
      g_error
        ("%s has received a %s event, which it does not listen for.  This should never happen.  Please contact the developer.",
         MODEL_NAME, EVT_event_type_name[event->type]);
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT run (%s)", MODEL_NAME);
#endif
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
  RPT_reporting_zero (local_data->shape);
  RPT_reporting_zero (local_data->area);
  RPT_reporting_zero (local_data->num_separate_areas);
  RPT_reporting_zero (local_data->num_units);
  RPT_reporting_zero (local_data->num_animal_days_by_prodtype);

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

  s = g_string_new (NULL);
  g_string_sprintf (s, "<%s>", MODEL_NAME);

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
  RPT_free_reporting (local_data->shape, TRUE);
  RPT_free_reporting (local_data->area, TRUE);
  RPT_free_reporting (local_data->num_separate_areas, TRUE);
  RPT_free_reporting (local_data->num_units, TRUE);
  RPT_free_reporting (local_data->num_animal_days_by_prodtype, TRUE);
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
 * Returns a new zone monitor.
 */
ergadm_model_t *
new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  ergadm_model_t *self;
  local_data_t *local_data;
  scew_element const *e, **ee;
  unsigned int noutputs;
  RPT_reporting_t *output;
  const XML_Char *variable_name;
  int i, j;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER new (%s)", MODEL_NAME);
#endif

  self = g_new (ergadm_model_t, 1);
  local_data = g_new (local_data_t, 1);

  self->name = MODEL_NAME;
  self->description = MODEL_DESCRIPTION;
  self->events_created = events_created;
  self->nevents_created = NEVENTS_CREATED;
  self->events_listened_for = events_listened_for;
  self->nevents_listened_for = NEVENTS_LISTENED_FOR;
  self->outputs = g_ptr_array_sized_new (5);
  self->model_data = local_data;
  self->run = run;
  self->reset = reset;
  self->is_listening_for = is_listening_for;
  self->has_pending_actions = has_pending_actions;
  self->has_pending_infections = has_pending_infections;
  self->to_string = to_string;
  self->printf = local_printf;
  self->fprintf = local_fprintf;
  self->free = local_free;

  /* Make sure the right XML subtree was sent. */
  g_assert (strcmp (scew_element_name (params), MODEL_NAME) == 0);

  local_data->shape = RPT_new_reporting ("zone-shape", NULL, RPT_group, RPT_never, TRUE);
  local_data->area = RPT_new_reporting ("zone-area", NULL, RPT_group, RPT_never, TRUE);
  local_data->num_separate_areas =
    RPT_new_reporting ("num-separate-areas", NULL, RPT_group, RPT_never, TRUE);
  local_data->num_units = RPT_new_reporting ("num-units-in-zone", NULL, RPT_group, RPT_never, TRUE);
  local_data->num_animal_days_by_prodtype =
    RPT_new_reporting ("num-animal-days-in-zone-by-production-type", NULL, RPT_group, RPT_never,
                       TRUE);
  g_ptr_array_add (self->outputs, local_data->shape);
  g_ptr_array_add (self->outputs, local_data->area);
  g_ptr_array_add (self->outputs, local_data->num_separate_areas);
  g_ptr_array_add (self->outputs, local_data->num_units);
  g_ptr_array_add (self->outputs, local_data->num_animal_days_by_prodtype);

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
      for (j = 0; j < self->outputs->len; j++)
        {
          output = (RPT_reporting_t *) g_ptr_array_index (self->outputs, j);
          if (strcmp (output->name, variable_name) == 0)
            break;
        }
      if (j == self->outputs->len)
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

  local_data->nzones = ZON_zone_list_length (zones);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT new (%s)", MODEL_NAME);
#endif

  return self;
}

char *
zone_monitor_interface_version (void)
{
  return interface_version ();
}

ergadm_model_t *
zone_monitor_new (scew_element * params, HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  return new (params, herds, zones);
}

/* end of file zone-monitor.c */
