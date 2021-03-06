/** @file zone.h
 *
 * Zones are areas of differing surveillance and control policies.  The text
 * below gives the rules for how zones work; for notes on how they are
 * implemented in code, see zone.c.
 *
 * There can be an arbitrary number of zones, each with a name.
 *
 * @image html surv_levels.png
 * @image latex surv_levels.eps
 *
 * The basic form of a zone is a circle around a unit.  Areas outside the
 * circle also constitute a zone, with the lowest surveillance level.
 *
 * @image html basic_circle.png
 * @image latex basic_circle.eps
 *
 * Higher levels of surveillance correspond to smaller zones.
 *
 * @image html ordered_circles.png
 * @image latex ordered_circles.eps
 *
 * Overlapping foci of the same zone merge.
 *
 * @image html adjacent_zones.png
 * @image latex adjacent_zones.eps
 *
 * Zones with lower surveillance levels are absorbed when enclosed by a zone of
 * a higher surveillance level.  (The "no donuts" rule.)
 *
 * @image html enclosure.png
 * @image latex enclosure.eps
 *
 * Note that adding a focus can join existing physically separated areas of the
 * same zone and create more than one donut-hole.
 *
 * @image html enclosure2.png
 * @image latex enclosure2.eps
 *
 * If a focus is removed from a zone, the zone takes the shape it would have
 * had were that focus never included.  (The "no bites out of Mickey's head"
 * rule.)
 *
 * @image html split_zones.png
 * @image latex split_zones.eps
 *
 * @section movement-and-zones Movement and zones
 *
 * Movement inside a zone may be allowed, but movement between physically
 * separated foci of the same zone is not.
 *
 * @image html movement_inside.png
 * @image latex movement_inside.eps
 *
 * Movement from a zone to an adjacent zone of a higher surveillance level is
 * allowed, but not vice-versa.
 *
 * @image html lower_to_higher.png
 * @image latex lower_to_higher.eps
 *
 * Movement that would cross a zone of a higher surveillance level "as the crow
 * flies" but where a "detour" exists, is allowed.
 *
 * @image html detour.png
 * @image latex detour.eps
 *
 * Symbols from this module begin with ZON_.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date October 2004
 *
 * Copyright &copy; University of Guelph, 2004-2007
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef ZONE_H
#define ZONE_H

#include <stdio.h>
#include <gpcl/gpc.h>

#include <glib.h>



/** A zone. */
typedef struct
{
  char *name;
  int level;
  double radius;
  double radius_sq;
  double radius_as_degrees;
  double use_rtree_index;
  GArray *foci; /**< Unordered array of foci.  Each focus is a gpc_vertex
    structure. */
  gpc_polygon *poly; /**< the (possibly multi-contour) polygon */
  GQueue *fragments; /**< The fragments (contours) making up the zone.  It is
    safe to keep a pointer to a particular fragment since fragments will not
    disappear during a simulation.  If one fragment merges with another, both
    fragments will continue to exist, and will simply point to the same contour
    in <i>poly</i>.  You should use ZON_same_zone() and ZON_same_fragment() to
    safely compare zone fragments. */
  double area;
  unsigned int nholes_filled;
}
ZON_zone_t;



/** A zone "fragment". */
typedef struct
{
#if DEBUG
  int id; /**< a numeric identifier for debugging purposes */
#endif
  ZON_zone_t *parent; /**< the zone this fragment is part of */
  int contour; /**< the index of the sub-polygon in the parent structure */
  gpc_vertex sample; /**< a point (any point) that is known to be inside the
    fragment.  Used to tell which post-merge contour corresponds to which
    pre-merge contour. */
}
ZON_zone_fragment_t;



typedef struct
{
  double x, y;
}
ZON_pending_focus_t;



/** A list of zones. */
typedef struct
{
  GPtrArray *list;
  ZON_zone_fragment_t **membership; /**< A list with 1 item per herd.  Each
    item is a pointer to the zone fragment that herd is in.  The pointers are
    never null, because even herds that are not inside a zone focus count as
    being in the "background" zone. */
  unsigned int membership_length; /**< Length of the membership array. */
  gboolean use_rtree_index;
  GQueue *pending_foci; /**< A list of foci that have yet to be added.  Each
    item in the queue will be a ZON_pending_focus struct.  Because the events
    in one simulation day should be considered to happen simultaneously,
    changes to a zone are not processed mid-day; instead, they are stored and
    applied all at once later. */
}
ZON_zone_list_t;



/* Prototypes. */

/* Functions for lists of zone objects. */

ZON_zone_list_t *ZON_new_zone_list (unsigned int membership_length);
unsigned int ZON_zone_list_append (ZON_zone_list_t *, ZON_zone_t *);

/**
 * Returns the number of zones in a zone list.
 *
 * @param Z a zone list.
 * @return the number of zones in the list.
 */
#define ZON_zone_list_length(Z) (Z->list->len)

/**
 * Returns the ith zone in a zone list.
 *
 * @param Z a zone list.
 * @param I the index of the zone to retrieve.
 * @return the ith zone.
 */
#define ZON_zone_list_get(Z,I) ((ZON_zone_t*)g_ptr_array_index(Z->list,I))

void ZON_zone_list_reset (ZON_zone_list_t *);
void ZON_zone_list_add_focus (ZON_zone_list_t *, double x, double y);

ZON_zone_fragment_t *ZON_zone_list_get_background (ZON_zone_list_t *);

char *ZON_zone_list_to_string (ZON_zone_list_t *);
int ZON_fprintf_zone_list (FILE *, ZON_zone_list_t *);

#define ZON_printf_zone_list(Z) ZON_fprintf_zone_list(stdout,Z)

void ZON_free_zone_list (ZON_zone_list_t *);

/* Functions for zone objects. */

ZON_zone_t *ZON_new_zone (char *, int level, double radius);
void ZON_free_zone (ZON_zone_t *, gboolean free_segment);
char *ZON_zone_to_string (ZON_zone_t *);
int ZON_fprintf_zone (FILE *, ZON_zone_t *);

#define ZON_printf_zone(Z) ZON_fprintf_zone(stdout,Z)

void ZON_reset (ZON_zone_t *);
ZON_zone_fragment_t *ZON_zone_add_focus (ZON_zone_t *, double x, double y, gpc_polygon ** holes);
gboolean ZON_zone_contains (ZON_zone_t *, double x, double y);
gboolean ZON_same_zone (ZON_zone_fragment_t *, ZON_zone_fragment_t *);
gboolean ZON_same_fragment (ZON_zone_fragment_t *, ZON_zone_fragment_t *);
double ZON_update_area (ZON_zone_t *);

#define ZON_level(F) (F->parent->level)

#endif /* !ZONE_H */
