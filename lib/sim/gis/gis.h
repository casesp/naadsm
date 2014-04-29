/** @file gis.h
 * Interface for gis.c.
 *
 * Symbols from this module begin with GIS_.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date May 2003
 *
 * Copyright &copy; University of Guelph, 2003-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifndef GIS_H
#define GIS_H

/*  These symbols make it possible to use functions defined in this file
*   in a Windows DLL.  If DLL_EXPORTS and DLL_IMPORTS are undefined
*   (e.g. in typical *nix builds), the symbol DLL_API is ignored.  In Windows
*   builds, defining DLL_EXPORTS will make these functions available in a DLL.
*   If DLL_IMPORTS is defined, a C program may use the functions from the DLL.
*/
#if defined(DLL_EXPORTS)
#	define DLL_API __declspec( dllexport )
#elif defined(DLL_IMPORTS)
# define DLL_API __declspec( dllimport )
#else
# define DLL_API
#endif


#include <gpcl/gpc.h>
#include <glib.h>

#define GIS_EARTH_RADIUS 6378.137 /**< equatorial radius in km, WGS84 */
#define GIS_DEGREE_DISTANCE 111.31949 /**< 1/360th of equatorial circumference, in km */
#define GIS_DEGREE_DISTANCE_SQ 12392.029 /**< square of above */
#define DEG2RAD 0.01745329



/* Prototypes. */
DLL_API double GIS_great_circle_distance (double lat1, double lon1, double lat2, double lon2);
DLL_API double GIS_local_distance (double lat1, double lon1, double lat2, double lon2);
DLL_API double GIS_local_distance_sq (double lat1, double lon1, double lat2, double lon2);
DLL_API double GIS_simple_distance (double lat1, double lon1, double lat2, double lon2);
DLL_API double GIS_simple_distance_sq (double lat1, double lon1, double lat2, double lon2);

double GIS_heading (double lat1, double lon1, double lat2, double lon2);
double GIS_local_heading (double lat1, double lon1, double lat2, double lon2);
double GIS_simple_heading (double lat1, double lon1, double lat2, double lon2);

gboolean GIS_point_in_polygon (gpc_polygon * poly, float x, float y);
gboolean GIS_point_in_contour (gpc_vertex_list * poly, float x, float y);
double GIS_polygon_area (gpc_polygon * poly);
double GIS_simple_latlon_polygon_area (gpc_polygon * poly);
double GIS_local_latlon_polygon_area (gpc_polygon * poly);

#endif /* !GIS_H */
