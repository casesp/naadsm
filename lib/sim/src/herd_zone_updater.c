/** @file herd_zone_updater.c
 * Functions for updating the assignment of herds to zones.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date October 2004
 *
 * Copyright &copy; University of Guelph, 2006-2007
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "herd_zone_updater.h"
#include "gis.h"
#include "guilib.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#define EPSILON 0.001



/**
 * Gets the minimum and maximum x- and y- coordinates of a polygon contour.
 *
 * @param contour a contour.
 * @param coord a location in which to store the minimum x, minimum y, maximum
 *   x, and maximum y coordinates, in that order.
 */
void
gpc_contour_get_boundary (gpc_vertex_list * contour, double *coord)
{
  double minx, maxx, miny, maxy;
  gpc_vertex *vertex;
  int i;

  vertex = contour->vertex;
  minx = maxx = vertex->x;
  miny = maxy = vertex->y;
  vertex++;

  for (i = 1; i < contour->num_vertices; i++, vertex++)
    {
      if (vertex->x < minx)
        minx = vertex->x;
      else if (vertex->x > maxx)
        maxx = vertex->x;
      if (vertex->y < miny)
        miny = vertex->y;
      else if (vertex->y > maxy)
        maxy = vertex->y;
    }

  coord[0] = minx;
  coord[1] = miny;
  coord[2] = maxx;
  coord[3] = maxy;
}



/**
 * Reports whether a polygon contour contains a given point.
 *
 * @param contour a contour.
 * @return TRUE if the contour contains the point, FALSE otherwise.
 */
gboolean
gpc_contour_contains_point (gpc_vertex_list * contour, double x, double y)
{
  int npol;
  int i, j, c = 0;
  gpc_vertex *v;

  npol = contour->num_vertices;
  v = contour->vertex;
  for (i = 0, j = npol - 1; i < npol; j = i++)
    {
      if ((((v[i].y <= y) && (y < v[j].y)) ||
           ((v[j].y <= y) && (y < v[i].y))) &&
          (x < (v[j].x - v[i].x) * (y - v[i].y) / (v[j].y - v[i].y) + v[i].x))

        c = !c;
    }
  return c;
}



/**
 * Special structure for use with the callback functions below.
 */
typedef struct
{
  double lat, lon;
  HRD_herd_list_t *herds;
  HRD_herd_t *herd;
  ZON_zone_list_t *zones;
  ZON_zone_fragment_t **fragment_containing_focus;
  gpc_vertex_list *hole;
  ZON_zone_fragment_t *hole_fragment;
#if DEBUG
  int *n_search_hits;
  int zone_index;
#endif
} callback_t;



/**
 * Check whether the herd's distance to the focus is within the radius
 * assigned to each zone.  If so, update the herd-to-zone assignment.
 */
void
check_circle_and_rezone (callback_t * callback_data, HRD_herd_t * herd, gboolean update_zones)
{
  double distance_sq;
  ZON_zone_list_t *zones;
  unsigned int nzones;
  ZON_zone_t *zone;
  ZON_zone_fragment_t *current_fragment;
  int current_level;
  int i;
  HRD_zone_t zone_update;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER check_circle_and_rezone");
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "checking unit \"%s\"", herd->official_id);
#endif

  distance_sq = GIS_local_distance_sq (herd->lat, herd->lon,
                                       callback_data->lat, callback_data->lon);
  /* Check if the distance is within the radius of any of the zone rings,
   * starting with the smallest ring.  Skip the last zone in the list because
   * it will be the "background" zone. */
  zones = callback_data->zones;
  nzones = ZON_zone_list_length (zones);
  for (i = 0; i < nzones - 1; i++)
    {
      zone = ZON_zone_list_get (zones, i);
      if (distance_sq - zone->radius_sq <= EPSILON)
        {
          /* The herd is inside this zone ring. */
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "unit \"%s\" is inside the radius (%.2g km) for zone \"%s\" (level %i)",
                 herd->official_id, zone->radius, zone->name, zone->level);

          callback_data->n_search_hits[i]++;
#endif
          /* update_zones is ignored, unless testing and debugging are being conducted. */
#if DEBUG
          if( TRUE == update_zones )
            {
#endif
              /* Find out the surveillance level of the zone the herd is currently
               * in.  If the herd is currently in a lower-priority level (the level
               * number is higher), update the herd's zone membership. */
              current_fragment = zones->membership[herd->index];
              current_level = current_fragment->parent->level;
              if (current_level > zone->level)
                {
#if DEBUG
                  s = g_string_new (NULL);
                  g_string_printf (s, "unit \"%s\" was in zone \"%s\" (level %i)",
                                   herd->official_id, current_fragment->parent->name, current_level);
#endif
                  zones->membership[herd->index] = callback_data->fragment_containing_focus[i];

                  if( NULL != guilib_record_zone_change )
                    {
                      zone_update.herd_index = herd->index;
                      zone_update.zone_level = zone->level;

                      guilib_record_zone_change( zone_update );
                    }
#if DEBUG
                  g_string_append_printf (s, ", now in zone \"%s\" (level %i)",
                                          zone->name, zone->level);
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
                  g_string_free (s, TRUE);
#endif
                }
              else
                {
#if DEBUG
                  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                         "unit \"%s\" remains in size \"%s\" (level %i)",
                         herd->official_id, current_fragment->parent->name, current_level);
#endif
                }
#if DEBUG
            }
#endif
          break;
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT check_circle_and_rezone");
#endif

  return;
}



/**
 * Check whether the herd's location is inside the given polygon.  If so,
 * update the herd-to-zone assignment.
 */
void
check_poly_and_rezone (callback_t * callback_data, HRD_herd_t * herd, gboolean update_zones)
{
  gpc_vertex_list *poly;
  ZON_zone_fragment_t *current_fragment;
  ZON_zone_t *zone;
  ZON_zone_list_t *zones;
  int current_level;
  HRD_zone_t zone_update;
#if DEBUG
  GString *s;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER check_poly_and_rezone");
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "checking unit \"%s\"", herd->official_id);
#endif

  poly = callback_data->hole;

  /* Check if the herd's location is inside the polygon. */
  if (gpc_contour_contains_point (poly, herd->lon, herd->lat))
    {
      zone = callback_data->hole_fragment->parent;
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
             "unit \"%s\" is inside the hole in zone \"%s\" (level %i)",
             herd->official_id, zone->name, zone->level);

      callback_data->n_search_hits[callback_data->zone_index]++;
#endif

      /* update_zones is ignored, unless testing and debugging are being conducted. */
#if DEBUG
      if( TRUE == update_zones )
        {
#endif
          /* Find out the surveillance level of the zone the herd is currently in.
           * If the herd is currently in a lower-priority level (the level number
           * number is higher), update the herd's zone membership. */
          zones = callback_data->zones;
          current_fragment = zones->membership[herd->index];
          current_level = current_fragment->parent->level;
          if (current_level > zone->level)
            {
#if DEBUG
              s = g_string_new (NULL);
              g_string_printf (s, "unit \"%s\" was in zone \"%s\" (level %i)",
                               herd->official_id, current_fragment->parent->name, current_level);
#endif
              zones->membership[herd->index] = callback_data->hole_fragment;

              if( NULL != guilib_record_zone_change )
                {
                  zone_update.herd_index = herd->index;
                  zone_update.zone_level = zone->level;

                  guilib_record_zone_change( zone_update );
                }

#if DEBUG
              g_string_append_printf (s, ", now in zone \"%s\" (level %i)", zone->name, zone->level);
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
              g_string_free (s, TRUE);
#endif
            }
#if DEBUG
        }
#endif
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT check_poly_and_rezone");
#endif

  return;
}



int
callback_circle (int id, void *arg)
{
  callback_t *callback_data;
  HRD_herd_t *herd;

  callback_data = (callback_t *) arg;
  /* Because herd indices start at 0, and R-Tree rectangle IDs start at 1. */
  herd = HRD_herd_list_get (callback_data->herds, id - 1);
  check_circle_and_rezone (callback_data, herd, TRUE);

  /* A return value of 0 would mean that the spatial search should stop early
   * (before all herds in the search rectangle were visited).  We don't want
   * that, so return 1. */
  return 1;
}



int
callback_poly (int id, void *arg)
{
  callback_t *callback_data;
  HRD_herd_t *herd;

  callback_data = (callback_t *) arg;
  /* Because herd indices start at 0, and R-Tree rectangle IDs start at 1. */
  herd = HRD_herd_list_get (callback_data->herds, id - 1);
  check_poly_and_rezone (callback_data, herd, TRUE);

  /* A return value of 0 would mean that the spatial search should stop early
   * (before all herds in the search rectangle were visited).  We don't want
   * that, so return 1. */
  return 1;
}


/**
 * Adds a focus to the zone(s) and updates the zone membership of each herd.
 *
 * @param herds the list of herds.
 * @param zones the list of zones.
 */
void
ergadm_update_herd_zones (HRD_herd_list_t * herds, ZON_zone_list_t * zones)
{
  unsigned int nzones, nherds;
  double lat, lon;
  ZON_pending_focus_t *pending_focus;
  unsigned int herd_index;
  callback_t callback_data;
  struct Rect search_rect;      /* for narrowing down radius searches using the
                                   R-tree (spatial index) */
  double mult;                  /* to account for latitude */
  ZON_zone_t *zone;
  double distance;
  gpc_polygon **holes;
  int nholes;
  gpc_vertex_list *hole;
  double boundary[4];
  double hole_size;
  int i, j;

#if DEBUG
  int report_array_size;
  int *nRTreeHitsCircle;
  int *nRTreeHitsPoly;
  int *nIteratorHitsCircle;
  int *nIteratorHitsPoly;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_update_herd_zones");
#endif

  /* If there is just a "background" zone, we have nothing to do. */
  nzones = ZON_zone_list_length (zones);
  if (nzones == 1)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "there is only a background zone, nothing to do");
#endif
      goto end;
    }

#if DEBUG
  /* For debugging purposes, these arrays can be used to count the number of 
   * search hits produced by each mechanism. */
  report_array_size = nzones - 1;
  nRTreeHitsCircle = (int*)malloc( sizeof( int ) * report_array_size );
  memset( nRTreeHitsCircle, 0, sizeof( int ) * report_array_size );

  nRTreeHitsPoly = (int*)malloc( sizeof( int ) * report_array_size );
  memset( nRTreeHitsPoly, 0, sizeof( int ) * report_array_size );

  nIteratorHitsCircle = (int*)malloc( sizeof( int ) * report_array_size );
  memset( nIteratorHitsCircle, 0, sizeof( int ) * report_array_size );

  nIteratorHitsPoly = (int*)malloc( sizeof( int ) * report_array_size );
  memset( nIteratorHitsPoly, 0, sizeof( int ) * report_array_size );
#endif

  while (!g_queue_is_empty (zones->pending_foci))
    {
      pending_focus = (ZON_pending_focus_t *) g_queue_pop_head (zones->pending_foci);
      lon = pending_focus->x;
      lat = pending_focus->y;
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "focus to add at lat=%g, lon=%g", lat, lon);
#endif
      g_free (pending_focus);

      /* Update the shape of each zone, and get pointers to the zone fragments
       * in which the focus lies. */
      callback_data.fragment_containing_focus = g_new (ZON_zone_fragment_t *, nzones);
      holes = g_new0 (gpc_polygon *, nzones);
      for (i = 0; i < nzones; i++)
        {
          zone = ZON_zone_list_get (zones, i);
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "adding focus to zone \"%s\" (level %i)", zone->name, zone->level);
#endif
          callback_data.fragment_containing_focus[i] =
            ZON_zone_add_focus (zone, lon, lat, &holes[i]);
        }

      /* Update the assignments of herds to zone fragments. */
      /* First, draw the circles around the focus. */
      callback_data.lat = lat;
      callback_data.lon = lon;
      callback_data.herds = herds;
      callback_data.zones = zones;
      
      /* Find the distances to other units. */
      if (zones->use_rtree_index)
        {
#if DEBUG
          callback_data.n_search_hits = nRTreeHitsCircle;
#endif
          /* The search area should be larger than the radius of the largest 
           * zone.  Remember that the largest zone is the next-to-last item in
           * the zone list.
           */
          distance = ZON_zone_list_get (zones, nzones - 2)->radius_as_degrees;
          mult = 1.0 / cos (DEG2RAD * lat);
          search_rect.boundary[0] = lon - (distance * mult) - EPSILON;
          search_rect.boundary[1] = lat - distance - EPSILON;
          search_rect.boundary[2] = lon + (distance * mult) + EPSILON;
          search_rect.boundary[3] = lat + distance + EPSILON;

          RTreeSearch (herds->spatial_index, &search_rect, callback_circle, &callback_data);
        }
      else
        {
#if DEBUG
          callback_data.n_search_hits = nIteratorHitsCircle;
#endif
          nherds = HRD_herd_list_length (herds);
          for (herd_index = 0; herd_index < nherds; herd_index++)
            check_circle_and_rezone (&callback_data, HRD_herd_list_get (herds, herd_index), TRUE);
        }

      /* Next, fill in any "holes", starting with the zone with the highest
       * priority. */
      for (i = 0; i < nzones; i++)
        {
          if (holes[i] == NULL)
            continue;
          zone = ZON_zone_list_get (zones, i);
          nholes = holes[i]->num_contours;
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "filling in %i hole(s) in zone \"%s\" (level %i)",
                 nholes, zone->name, zone->level);

          callback_data.zone_index = i;
#endif

          for (j = 0; j < nholes; j++)
            {
              hole = &(holes[i]->contour[j]);
              gpc_contour_get_boundary (hole, boundary);
              hole_size = MIN (boundary[2] - boundary[0], boundary[3] - boundary[1]);
              callback_data.hole = hole;
              callback_data.hole_fragment = callback_data.fragment_containing_focus[i];

              if (hole_size / 2 <= zone->use_rtree_index)
                {
#if DEBUG
                  callback_data.n_search_hits = nRTreeHitsPoly;
#endif
                  search_rect.boundary[0] = (float) boundary[0];
                  search_rect.boundary[1] = (float) boundary[1];
                  search_rect.boundary[2] = (float) boundary[2];
                  search_rect.boundary[3] = (float) boundary[3];
                  RTreeSearch (herds->spatial_index, &search_rect, callback_poly, &callback_data);
                }
              else
                {
#if DEBUG
                  callback_data.n_search_hits = nIteratorHitsPoly;
#endif
                  nherds = HRD_herd_list_length (herds);
                  for (herd_index = 0; herd_index < nherds; herd_index++)
                    check_poly_and_rezone (&callback_data, HRD_herd_list_get (herds, herd_index), TRUE);
                }
            }                   /* end of loop over holes */
        }                       /* end of loop over zones */

      /* Clean up. */
      g_free (callback_data.fragment_containing_focus);
      for (i = 0; i < nzones; i++)
        {
          if (holes[i] == NULL)
            continue;
          else if (holes[i]->num_contours == 0)
            /* gpc_free_polygon doesn't like polygons with zero contours! */
            g_free (holes[i]);
          else
            gpc_free_polygon (holes[i]);
        }
      g_free (holes);
    }                           /* end of loop over pending foci */

#if DEBUG
  if( NULL != guilib_report_search_hits )
    {
      for( i = 0; i < report_array_size; ++i )
        guilib_report_search_hits( ZON_zone_list_get( zones, i )->level,
                                    nRTreeHitsCircle[i], nIteratorHitsCircle[i],
                                    nRTreeHitsPoly[i], nIteratorHitsPoly[i] );
    }

  free( nRTreeHitsCircle );
  free( nRTreeHitsPoly );
  free( nIteratorHitsCircle );
  free( nIteratorHitsPoly );
#endif

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_update_herd_zones");
#endif

  return;
}



/* end of file herd_zone_updater.c */
