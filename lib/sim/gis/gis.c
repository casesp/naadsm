/** @file gis.c
 * Functions for geographical calculations.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gis.h>

#if HAVE_MATH_H
#  include <math.h>
#endif

#include <gsl/gsl_math.h>
#include <gsl/gsl_sf_trig.h>

#define EPSILON 0.00001
#define RAD2DEG 57.2957795
#define d_acos(x) (fabs (x) >= 1.0 ? ((x) < 0.0 ? M_PI : 0.0) : acos (x))



/**
 * Returns the great-circle distance in km between points 1 and 2.  Latitudes
 * and longitudes must be given in degrees.  Latitudes must be between -90 (at
 * the south pole) and +90 (at the north pole), inclusive.  Longitudes must be
 * between -180 (west is negative) and +180 (east is positive), inclusive.
 * Taken from the Generic Mapping Tools (GMT) 3.4.2 by P. Wessel and W.H.F.
 * Smith.
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the distance (in km) between points 1 and 2.
 */
double
GIS_great_circle_distance (double lat1, double lon1, double lat2, double lon2)
{
  double C, a, b, c;
  double cosC, cosa, cosb, cosc;
  double sina, sinb;

  if ((lat1 == lat2) && (lon1 == lon2))
    return 0.0;

  a = DEG2RAD * (90.0 - lat2);
  b = DEG2RAD * (90.0 - lat1);

  C = DEG2RAD * (lon2 - lon1);

  sina = sin (a);
  cosa = cos (a);
  sinb = sin (b);
  cosb = cos (b);
  cosC = cos (C);

  cosc = cosa * cosb + sina * sinb * cosC;
  if (cosc < -1.0)
    c = M_PI;
  else if (cosc > 1)
    c = 0.0;
  else
    c = d_acos (cosc);

  return (c * GIS_EARTH_RADIUS);
}



/**
 * Returns the distance in km between points 1 and 2 using a local, flat-Earth
 * approximation.  The calculation is
 *
 * <i>y</i> = <i>lat2</i> - <i>lat1</i><br>
 * <i>x</i> = (<i>lon2</i> - <i>lon1</i>) \htmlonly &times; \endhtmlonly
 * \latexonly $\times$ \endlatexonly cos (<i>lat1</i>)<br>
 * <i>d</i> = \f$\frac{c}{360} \sqrt{x^2 + y^2}\f$
 *
 * where <i>c</i> is the Earth's circumference at the equator.
 *
 * Latitudes and longitudes must be given in degrees.  Latitudes must be
 * between -90 (at the south pole) and +90 (at the north pole), inclusive.
 * Longitudes must be between -180 (west is negative) and +180 (east is
 * positive), inclusive.  The calculation is from the Aviation Formulary by Ed
 * Williams (http://williams.best.vwh.net/avform.htm) using the spherical-Earth
 * case.
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the distance (in km) between points 1 and 2.
 */
double
GIS_local_distance (double lat1, double lon1, double lat2, double lon2)
{
  double dlat, dlon;
  double x;

  dlat = lat2 - lat1;
  dlon = fabs (lon2 - lon1);
  /* Handle the case where the shortest line between the points crosses the
   * +180/-180 line. */
  if (dlon > 180)
    dlon = 360 - dlon;

  x = dlon * cos (DEG2RAD * lat1);
  return sqrt (x * x + dlat * dlat) * GIS_DEGREE_DISTANCE;
}



/**
 * Returns the square of the distance in km between points 1 and 2 using a
 * local, flat-Earth approximation.  The calculation is
 *
 * <i>y</i> = <i>lat2</i> - <i>lat1</i><br>
 * <i>x</i> = (<i>lon2</i> - <i>lon1</i>) \htmlonly &times; \endhtmlonly
 * \latexonly $\times$ \endlatexonly cos (<i>lat1</i>)<br>
 * <i>d</i><sup>2</sup> = \f$(\frac{c}{360})^2 \times (x^2 + y^2)\f$
 *
 * where <i>c</i> is the Earth's circumference at the equator.
 *
 * Latitudes and longitudes must be given in degrees.  Latitudes must be
 * between -90 (at the south pole) and +90 (at the north pole), inclusive.
 * Longitudes must be between -180 (west is negative) and +180 (east is
 * positive), inclusive.  The calculation is from the Aviation Formulary by Ed
 * Williams (http://williams.best.vwh.net/avform.htm) using the spherical-Earth
 * case.
 *
 * This function may be useful if you want to compare computed distances to a
 * fixed threshold.  By comparing this function's output to the square of the
 * threshold instead, you can avoid taking square roots.
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the distance (in km) between points 1 and 2.
 */
double
GIS_local_distance_sq (double lat1, double lon1, double lat2, double lon2)
{
  double dlat, dlon;
  double x;

  dlat = lat2 - lat1;
  dlon = fabs (lon2 - lon1);
  /* Handle the case where the shortest line between the points crosses the
   * +180/-180 line. */
  if (dlon > 180)
    dlon = 360 - dlon;

  x = dlon * cos (DEG2RAD * lat1);
  return (x * x + dlat * dlat) * GIS_DEGREE_DISTANCE_SQ;
}



/**
 * Returns the distance in km between points 1 and 2 assuming a flat, square
 * grid where 1 degree of latitude or longitude equals 1/360th of the Earth's
 * circumference at the equator.  Latitudes and longitudes must be given in
 * degrees.  Latitudes must be between -90 (at the south pole) and +90 (at the
 * north pole), inclusive.  Longitudes must be between -180 (west is negative)
 * and +180 (east is positive), inclusive.
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the distance (in km) between points 1 and 2.
 */
double
GIS_simple_distance (double lat1, double lon1, double lat2, double lon2)
{
  double dlat, dlon;

  dlat = lat2 - lat1;
  dlon = fabs (lon2 - lon1);
  /* Handle the case where the shortest line between the points crosses the
   * +180/-180 line. */
  if (dlon > 180)
    dlon = 360 - dlon;

  return sqrt (dlat * dlat + dlon * dlon) * GIS_DEGREE_DISTANCE;
}



/**
 * Returns the square of the distance in km between points 1 and 2 assuming a
 * flat, square grid where 1 degree of latitude or longitude equals 1/360th of
 * the Earth's circumference at the equator.  Latitudes and longitudes must be
 * given in degrees.  Latitudes must be between -90 (at the south pole) and +90
 * (at the north pole), inclusive.  Longitudes must be between -180 (west is
 * negative) and +180 (east is positive), inclusive.
 *
 * This function may be useful if you want to compare computed distances to a
 * fixed threshold.  By comparing this function's output to the square of the
 * threshold instead, you can avoid taking square roots.
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the distance (in km) between points 1 and 2.
 */
double
GIS_simple_distance_sq (double lat1, double lon1, double lat2, double lon2)
{
  double dlat, dlon;

  dlat = lat2 - lat1;
  dlon = fabs (lon2 - lon1);
  /* Handle the case where the shortest line between the points crosses the
   * +180/-180 line. */
  if (dlon > 180)
    dlon = 360 - dlon;

  return (dlat * dlat + dlon * dlon) * GIS_DEGREE_DISTANCE_SQ;
}



/**
 * Returns the initial heading in degrees from point 1 to point 2.  Latitudes
 * and longitudes must be given in degrees.  Latitudes must be between -90 (at
 * the south pole) and +90 (at the north pole), inclusive.  Longitudes must be
 * between -180 (west is negative) and +180 (east is positive), inclusive.  The
 * calculation is from the Aviation Formulary by Ed Williams
 * (http://williams.best.vwh.net/avform.htm).
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the initial heading (in degrees) from point 1 to point 2.
 */
double
GIS_heading (double lat1, double lon1, double lat2, double lon2)
{
  double heading;

  /* Convert to radians.  Also flip the longitude's sign, because the Aviation
   * Formulary follows the unusual convention that west is positive (for the
   * "convenience" of North American readers). */
  lat1 = DEG2RAD * lat1;
  lon1 = DEG2RAD * -lon1;
  lat2 = DEG2RAD * lat2;
  lon2 = DEG2RAD * -lon2;

  /* Special case for poles. */
  if (cos (lat1) < EPSILON)
    heading = (lat1 > 0) ? M_PI : M_2_PI;
  else
    heading =
      gsl_sf_angle_restrict_pos (atan2
                                 (sin (lon1 - lon2) * cos (lat2),
                                  cos (lat1) * sin (lat2) -
                                  sin (lat1) * cos (lat2) * cos (lon1 - lon2)));

  return RAD2DEG * heading;
}



/**
 * Returns the heading in degrees from point 1 to point 2 using a local,
 * flat-Earth approximation.
 *
 * The calculation is
 *
 * <i>y</i> = <i>lat2</i> - <i>lat1</i><br>
 * <i>x</i> = (<i>lon2</i> - <i>lon1</i>) \htmlonly &times; \endhtmlonly
 * \latexonly $\times$ \endlatexonly cos (<i>lat1</i>)<br>
 * <i>h</i> = atan2 (<i>x</i>,<i>y</i>)
 *
 * where atan2 is a special kind of inverse tangent that returns an angle in
 * the proper quadrant and handles the special case where the horizontal value
 * is 0.
 *
 * Latitudes and longitudes must be given in degrees.  Latitudes must be
 * between -90 (at the south pole) and +90 (at the north pole), inclusive.
 * Longitudes must be between -180 (west is negative) and +180 (east is
 * positive), inclusive.  The calculation is from the Aviation Formulary by Ed
 * Williams (http://williams.best.vwh.net/avform.htm) using the spherical-Earth
 * case.
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the heading (in degrees) from point 1 to point 2.
 */
double
GIS_local_heading (double lat1, double lon1, double lat2, double lon2)
{
  double dlat, dlon;
  double x;
  double heading;

  dlat = lat2 - lat1;
  dlon = lon2 - lon1;
  /* Handle the case where the shortest line between the points crosses the
   * +180/-180 line. */
  if (dlon > 180)
    dlon = dlon - 360;
  else if (dlon < -180)
    dlon = 360 + dlon;

  /* Flip the order of atan2's arguments to rotate the coordinate system from
   * 0 = E, + = counter-clockwise (the trig way) to 0 = N, + = clockwise (the
   * navigation/surveying way). */
  x = dlon * cos (DEG2RAD * lat1);
  heading = atan2 (x, dlat) * RAD2DEG;

  /* atan2's return values are in the range -pi to pi; make sure this function
   * returns values in [0,360). */
  if (heading < 0)
    heading += 360;

  return heading;
}



/**
 * Returns the heading in degrees from point 1 to point 2 assuming a flat,
 * square grid where 1 degree of latitude or longitude equals 1/360th of the
 * Earth's circumference at the equator.  Latitudes and longitudes must be
 * given in degrees.  Latitudes must be between -90 (at the south pole) and +90
 * (at the north pole), inclusive.  Longitudes must be between -180 (west is
 * negative) and +180 (east is positive), inclusive.
 *
 * @param lat1 latitude of point 1, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon1 longitude of point 1, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon1</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @param lat2 latitude of point 2, in degrees.  -90 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lat2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 90.
 * @param lon2 longitude of point 2, in degrees.  -180 \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly <i>lon2</i> \htmlonly &le; \endhtmlonly \latexonly $\le$ \endlatexonly 180.
 * @return the heading (in degrees) from point 1 to point 2.
 */
double
GIS_simple_heading (double lat1, double lon1, double lat2, double lon2)
{
  double dlat, dlon;
  double heading;

  dlat = lat2 - lat1;
  dlon = lon2 - lon1;
  /* Handle the case where the shortest line between the points crosses the
   * +180/-180 line. */
  if (dlon > 180)
    dlon = dlon - 360;
  else if (dlon < -180)
    dlon = 360 + dlon;

  /* Flip the order of atan2's arguments to rotate the coordinate system from
   * 0 = E, + = counter-clockwise (the trig way) to 0 = N, + = clockwise (the
   * navigation/surveying way). */
  heading = atan2 (dlon, dlat) * RAD2DEG;

  /* atan2's return values are in the range -pi to pi; make sure this function
   * returns values in [0,360). */
  if (heading < 0)
    heading += 360;

  return heading;
}



/**
 * Tells whether the given point is inside the given polygon contour.
 * Following the terminology used in the General Polygon Clipper library, a
 * "polygon" may consist of more than one separated "contour".  This function
 * has not been tested on self-intersecting contours.
 *
 * This function is copyright 1970-2003 Wm. Randolph Franklin.  A page with
 * information about this function can be found at
 *
 * http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
 *
 * From the page:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimers.
 * 2. Redistributions in binary form must reproduce the above copyright notice
 *    in the documentation and/or other materials provided with the
 *    distribution.
 * 3. The name of Wm. Randolph Franklin may not be used to endorse or promote
 *    products derived from this Software without specific prior written
 *    permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE. 
 *
 * This function has been modified to accept the polygon in the format used by
 * the General Polygon Clipper library.
 *
 * @param poly the polygon contour.
 * @param x the x-coordinate of the point.
 * @param y the y-coordinate of the point.
 * @return TRUE if the point is inside the contour, FALSE otherwise.
 */
gboolean
GIS_point_in_contour (gpc_vertex_list * poly, float x, float y)
{
  int npol;
  int i, j;
  gboolean c = FALSE;
  gpc_vertex *v;

  npol = poly->num_vertices;
  v = poly->vertex;
  c = FALSE;
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
 * Tells whether the given point is inside the given polygon.  Following the
 * terminology used in the General Polygon Clipper library, a "polygon" may
 * consist of more than one separated "contour".  This function has not been
 * tested on self-intersecting contours or polygons with holes.
 *
 * @param poly the polygon.
 * @param x the x-coordinate of the point.
 * @param y the y-coordinate of the point.
 * @return TRUE if the point is inside the polygon, FALSE otherwise.
 */
gboolean
GIS_point_in_polygon (gpc_polygon * poly, float x, float y)
{
  int i;
  gboolean c = FALSE;

  for (i = 0; i < poly->num_contours; i++)
    if (c = GIS_point_in_contour (&(poly->contour[i]), x, y))
      break;

  return c;
}



/**
 * Returns the area of a polygon.  The x- and y-coordinates are assumed to be
 * on a square grid, so this function isn't directly usable for lat-lon points.
 *
 * @param poly a polygon.
 * @return the area of the polygon.
 */
double
GIS_polygon_area (gpc_polygon * poly)
{
  double poly_area, contour_area;
  int n, i, j;
  gpc_vertex_list *contour;
  gpc_vertex *v;

  poly_area = 0;

  if (poly == NULL)
    goto end;

  /* Add the areas of each sub-polygon or "contour" */
  for (i = 0; i < poly->num_contours; i++)
    {
      contour = &(poly->contour[i]);
      n = contour->num_vertices;
      /* Zero vertices (empty contour), one vertex (single point), or two
       * vertices (line) count for zero area.  The contour must be at least a
       * triangle before we bother to calculate the area. */
      if (n < 3)
        continue;

      contour_area = 0;
      v = contour->vertex;

      for (j = 0; j < n - 1; j++)
        {
          contour_area += (v[j].x * v[j + 1].y - v[j + 1].x * v[j].y);
        }
      /* final point */
      contour_area += (v[j].x * v[0].y - v[0].x * v[j].y);

      poly_area += fabs (0.5 * contour_area);
    }

end:
  return poly_area;
}



/**
 * Returns the area of a polygon in square km.  The x-coordinates of the points
 * are taken as longitude, the y-coordinates as latitude, in degrees.  The
 * calculation assumes a flat, square grid where 1 degree of latitude or
 * longitude equals 1/360th of the Earth's circumference at the equator.
 *
 * @param poly a polygon.
 * @return the area (in square km) of the polygon.
 */
double
GIS_simple_latlon_polygon_area (gpc_polygon * poly)
{
  /* FIXME: what about polygons that cross the -180/+180 line? */

  return GIS_polygon_area (poly) * GIS_DEGREE_DISTANCE_SQ;
}



/**
 * Returns the area of a polygon in square km.  The x-coordinates of the points
 * are taken as longitude, the y-coordinates as latitude, in degrees.  The
 * calculation attempts to take converging meridians into account.
 *
 * If we find the bounding rectangle around a polygon, and we treat the lat-lon
 * grid as a square grid, the area of the bounding rectangle is <i>w</i>
 * \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly <i>h</i>.
 * We can try to account for converging meridians by turning the bounding
 * rectangle into a trapezoid, where the top and bottom edges shrink as the
 * meridians converge at higher latitude values (this is a northern hemisphere
 * example).  The area of this "bounding trapezoid" is <i>h</i>
 * (<i>w</i> \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 * cos(<i>lat<sub>min</sub></i>) +
 * <i>w</i> \htmlonly &times; \endhtmlonly \latexonly $\times$ \endlatexonly
 * cos (<i>lat<sub>max</sub></i>)) / 2.
 *
 * The ratio of the area of the trapezoid to the area of the rectangle is
 * (cos(<i>lat<sub>min</sub></i>) + cos (<i>lat<sub>max</sub></i>)) / 2.
 * This function calls GIS_simple_latlon_polygon_area to compute the polygon
 * area assuming a square grid, then multiplies by that ratio.
 *
 * @image html latitude_and_area.png "Areas of the rectangle and trapezoid"
 * @image latex latitude_and_area.eps "Areas of the rectangle and trapezoid" width=4in
 *
 * @param poly a polygon.
 * @return the area (in square km) of the polygon.
 */
double
GIS_local_latlon_polygon_area (gpc_polygon * poly)
{
  double area;
  double lat, min_lat, max_lat;
  gboolean min_max_initialized;
  double simple_area, multiplier;
  int n, i, j;
  gpc_vertex_list *contour;
  gpc_vertex *v;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER GIS_local_latlon_polygon_area");
#endif

  /* FIXME: what about polygons that cross the -180/+180 line? */

  if (poly == NULL)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "null polygon, area = 0");
#endif
      area = 0;
      goto end;
    }

  /* We want to find the minimum and maximum y (latitude) values.  Initialize
   * both values to the latitude of the first point.  Because there may be zero
   * contours, or there may be contours with zero points, we may have to look
   * through a few contours to find that first point. */
  min_max_initialized = FALSE;
  for (i = 0; i < poly->num_contours; i++)
    {
      contour = &(poly->contour[i]);
      n = contour->num_vertices;
      if (n == 0)
        continue;

      v = contour->vertex;
      min_lat = max_lat = v[0].y;
      min_max_initialized = TRUE;
      break;
    }
  if (!min_max_initialized)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "no points, area = 0");
#endif
      area = 0;
      goto end;
    }

  /* Now find the minimum and maximum latitudes. */
  for (i = 0; i < poly->num_contours; i++)
    {
      contour = &(poly->contour[i]);
      n = contour->num_vertices;
      v = contour->vertex;

      for (j = 0; j < n; j++)
        {
          lat = v[j].y;
          if (lat < min_lat)
            min_lat = lat;
          else if (lat > max_lat)
            max_lat = lat;
        }
    }
  simple_area = GIS_simple_latlon_polygon_area (poly);
  multiplier = 0.5 * (cos (DEG2RAD * min_lat) + cos (DEG2RAD * max_lat));
  area = simple_area * multiplier;
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "minlat = %g, maxlat = %g", min_lat, max_lat);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "multiplier = (cos(%g)+cos(%g))/2 = %g", min_lat, max_lat,
         multiplier);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "area = %g * %g = %g", simple_area, multiplier, area);
#endif

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT GIS_local_latlon_polygon_area");
#endif

  return area;
}

/* end of file gis.c */
