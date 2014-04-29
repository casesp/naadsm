/** @file prob_dist.c
 * Functions for creating, destroying, printing, and getting values and
 * variates from probability distributions.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @author Aaron Reeves <Aaron.Reeves@colostate.edu><br>
 *   Animal Population Health Institute<br>
 *   Colorado State University<br>
 *   Fort Collins, CO 80526-8117<br>
 *   USA
 * @version 0.1
 * @date February 2003
 *
 * Copyright &copy; University of Guelph, 2003-2008
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "prob_dist.h"

#if HAVE_MATH_H
#  include <math.h>
#endif

#include <glib.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_integration.h>

#define EPSILON 0.00001
#define MAX_BISECTION_ITER 300



/**
 * Names for the probability distributions, terminated with a NULL sentinel.
 *
 * @sa PDF_dist_type_t
 */
const char *PDF_dist_type_name[] = {
  "Point", "Uniform", "Triangular", "Piecewise", "Histogram", "Gaussian",
  "Poisson", "Beta", "Gamma", "Weibull", "Exponential", "Pearson5", "Logistic",
  "LogLogistic", "Lognormal", NULL
};



/**
 * Creates a new point distribution (one that always returns the same value).
 *
 * @param value the number that the distribution will always return.
 * @return a pointer to a newly-created PDF_dist_t structure, or NULL if there
 *   wasn't enough memory to allocate one.
 */
PDF_dist_t *
PDF_new_point_dist (double value)
{
  PDF_dist_t *dist;
  PDF_point_dist_t *t;          /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_point_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Point;
  dist->has_inf_lower_tail = dist->has_inf_upper_tail = FALSE;
  t = &(dist->u.point);
  t->value = value;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_point_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a point distribution.
 *
 * @param dist a point distribution.
 * @return a string.
 */
char *
PDF_point_dist_to_string (PDF_point_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<point probability distribution\n value=%.2g>", dist->value);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new uniform (flat) distribution with parameters as illustrated
 * below.
 *
 * @image html uniform.png
 * @image latex uniform.eps width=5cm
 *
 * @return a pointer to a newly-created PDF_dist_t structure, or NULL if there
 *   wasn't enough memory to allocate one.
 */
PDF_dist_t *
PDF_new_uniform_dist (double a, double b)
{
  PDF_dist_t *dist;
  PDF_uniform_dist_t *t;        /* part specific to this distribution */
  double tmp;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_uniform_dist");
#endif

  /* If a and b are the same, return a point distribution instead. */
  if (gsl_fcmp (a, b, EPSILON) == 0)
    {
      dist = PDF_new_point_dist (a);
    }
  else
    {
      dist = g_new (PDF_dist_t, 1);
      dist->type = PDF_Uniform;
      dist->has_inf_lower_tail = dist->has_inf_upper_tail = FALSE;
      t = &(dist->u.uniform);

      /* Swap a and b if they're in the wrong order. */
      if (b < a)
        {
          tmp = a;
          a = b;
          b = tmp;
        }
      t->a = a;
      t->b = b;
      t->range = b - a;
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_uniform_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a uniform (flat) distribution.
 *
 * @param dist a uniform distribution.
 * @return a string.
 */
char *
PDF_uniform_dist_to_string (PDF_uniform_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<uniform (flat) probability distribution\n from %.2g to %.2g>",
                    dist->a, dist->b);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Computes the cumulative area <= \a x for a uniform (flat) distribution.
 *
 * @param x
 * @param dist a uniform distribution.
 */
double
uniform_cdf (double x, PDF_uniform_dist_t * dist)
{
  if (x < dist->a)
    return 0;
  else if (x > dist->b)
    return 1;
  else
    return (x - dist->a) / dist->range;
}



/**
 * The inverse cumulative distribution function for a uniform (flat)
 * distribution.
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a uniform distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
uniform_inverse_cdf (double area, PDF_uniform_dist_t * dist)
{
  return dist->range * area + dist->a;
}



/**
 * Creates a new triangular distribution with parameters as illustrated below.
 * If <i>a</i>, <i>c</i>, and <i>b</i> are given in the wrong order, they will
 * be re-arranged.  If <i>a</i>, <i>c</i>, and <i>b</i> are all the same value,
 * a "point" distribution is returned instead of a triangular distribution.
 *
 * @image html triangular.png
 * @image latex triangular.eps width=5cm
 *
 * @param a the minimum.
 * @param c the mode.
 * @param b the maximum.
 * @return a pointer to a newly-created PDF_dist_t structure, or NULL if there
 *   wasn't enough memory to allocate one.
 */
PDF_dist_t *
PDF_new_triangular_dist (double a, double c, double b)
{
  PDF_dist_t *dist;
  PDF_triangular_dist_t *t;     /* part specific to this distribution */
  gboolean out_of_order = FALSE;
  double tmp;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_triangular_dist");
#endif

  if (gsl_fcmp (a, b, EPSILON) == 0 && gsl_fcmp (b, c, EPSILON) == 0)
    {
      dist = PDF_new_point_dist (a);
      goto end;
    }

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Triangular;
  dist->has_inf_lower_tail = dist->has_inf_upper_tail = FALSE;
  t = &(dist->u.triangular);

  /* Re-order the parameters if they're given out of order. */
  if (a > c)
    {
      tmp = a;
      a = c;
      c = tmp;
      out_of_order = TRUE;
    }
  if (c > b)
    {
      tmp = c;
      c = b;
      b = tmp;
      out_of_order = TRUE;
    }
  if (a > c)
    {
      tmp = a;
      a = c;
      c = tmp;
      out_of_order = TRUE;
    }
  if (out_of_order)
    g_warning ("parameters were rearranged so that a <= c <= b");

  t->a = a;
  t->b = b;
  t->c = c;
  t->P = (c - a) / (b - a);
  t->one_minus_P = 1 - t->P;
  t->range = b - a;
  t->width_1 = c - a;
  t->width_2 = b - c;
  t->rw1 = t->range * t->width_1;
  t->rw2 = t->range * t->width_2;

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_triangular_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a triangular distribution.
 *
 * @param dist a triangular distribution.
 * @return a string.
 */
char *
PDF_triangular_dist_to_string (PDF_triangular_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<triangular probability distribution\n a=%.2g, c=%.2g, b=%.2g\n P=%.2g>",
                    dist->a, dist->c, dist->b, dist->P);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}




/**
 * Computes the probability density p(x) at x for a triangular distribution.
 *
 * @param x
 * @param dist a triangular distribution.
 */
double
triangular_pdf (double x, PDF_triangular_dist_t * dist)
{
  if (x <= dist->a)
    return 0;
  else if (x < dist->c)
    return 2 * (x - dist->a) / dist->rw1;
  else if (x < dist->b)
    return 2 * (dist->b - x) / dist->rw2;
  else
    return 0;
}



/**
 * Computes the cumulative area <= \a x for a triangular distribution.
 *
 * @param x
 * @param dist a triangular distribution.
 */
double
triangular_cdf (double x, PDF_triangular_dist_t * dist)
{
  double tmp;

  if (x <= dist->a)
    return 0;
  else if (x < dist->c)
    {
      tmp = x - dist->a;
      return tmp * tmp / dist->rw1;
    }
  else if (x < dist->b)
    {
      tmp = dist->b - x;
      return 1 - tmp * tmp / dist->rw2;
    }
  else
    return 1;
}


/**
 * The inverse cumulative distribution function for a triangular distribution.
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a triangular distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
triangular_inverse_cdf (double area, PDF_triangular_dist_t * dist)
{
  double x1, x2, x;
  int nsolutions;

  if (area < dist->P)
    {
      nsolutions =
        gsl_poly_solve_quadratic (1, -2 * dist->a,
                                  (dist->a * dist->a) - area * dist->rw1, &x1, &x2);
      if (nsolutions == 1)
        x = x1;
      else
        {
          /* check which one is the solution */
          x = ((x1 >= dist->a) && (x1 <= dist->c) ? x1 : x2);
        }
    }
  else
    {
      nsolutions =
        gsl_poly_solve_quadratic (1, -2 * dist->b,
                                  (dist->b * dist->b) - (1 - area) * dist->rw2, &x1, &x2);
      if (nsolutions == 1)
        x = x1;
      else
        {
          /* check which one is the solution */
          x = ((x1 >= dist->c) && (x1 <= dist->b) ? x1 : x2);
        }
    }

  return x;
}


/**
 * Computes the probability density p(x) at x for a triangular distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param min the minimum value used to define a triangular PDF.
 * @param mode the mode used to define a triangular PDF.
 * @param max the maximum value used to define a triangular PDF.
 * @return the probability density p(x) 
 */
DLL_API double
ar_triangular_pdf (double x, double min, double mode, double max)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_triangular_dist (min, mode, max);
  result = triangular_pdf (x, &(dist->u.triangular));
  PDF_free_dist (dist);
  return result;
}


/**
 * Computes the cumulative area <= \a x for a triangular distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param min the minimum value used to define a triangular PDF.
 * @param mode the mode used to define a triangular PDF.
 * @param max the maximum value used to define a triangular PDF.
 * @return the cumulative area <= \a x 
 */
DLL_API double
ar_triangular_cdf (double x, double min, double mode, double max)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_triangular_dist (min, mode, max);
  result = triangular_cdf (x, &(dist->u.triangular));
  PDF_free_dist (dist);
  return result;
}



/**
 * The inverse cumulative distribution function for a triangular distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param area 0 <= \a area <= 1.
 * @param min the minimum value used to define a triangular PDF.
 * @param mode the mode used to define a triangular PDF.
 * @param max the maximum value used to define a triangular PDF.
 * @return the value at which the cumulative distribution function = \a area.
 */
DLL_API double
ar_triangular_inverse_cdf (double area, double min, double mode, double max)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_triangular_dist (min, mode, max);
  result = triangular_inverse_cdf (area, &(dist->u.triangular));
  PDF_free_dist (dist);
  return result;
}



/**
 * Returns a random variate from a triangular distribution.
 *
 * @param dist a triangular distribution.
 * @param rng a random number generator.
 */
double
ran_triangular (PDF_triangular_dist_t * dist, RAN_gen_t * rng)
{
  double r;

  r = RAN_num (rng);
  if (r <= dist->P)
    return dist->a + dist->width_1 * sqrt (r / dist->P);
  else
    return dist->b - dist->width_2 * sqrt ((1 - r) / dist->one_minus_P);
}



/**
 * Fills in the cumulative distribution at each x-coordinate for a
 * PDF_piecewise_dist_t structure.
 *
 * @param a a piecewise distribution.
 */
double
calc_cumulative_for_piecewise (PDF_piecewise_dist_t * a)
{
  double width;
  unsigned int i;               /* loop counter */

  a->cumul[0] = 0;
  for (i = 1; i < a->n; i++)
    {
      width = a->x[i] - a->x[i - 1];
      a->cumul[i] = a->cumul[i - 1] + ((a->y[i - 1] + a->y[i]) / 2) * width;
      a->slope[i - 1] = (a->y[i] - a->y[i - 1]) / width;
    }
  return a->cumul[a->n - 1];
}



/**
 * Creates a new piecewise distribution.  The distribution is built from
 * triangles and trapezoids, and is defined by a set of points, as shown in the
 * example below.
 *
 * @image html piecewise.png
 * @image latex piecewise.eps width=5cm
 *
 * @param n the number of points on the curve.  \a n >= 1.
 * @param xy an array containing the points on the curve, as
 *   {<i>x</i><sub>0</sub>, <i>y</i><sub>0</sub>, <i>x</i><sub>1</sub>,
 *   <i>y</i><sub>1</sub>, <i>x</i><sub>2</sub>, <i>y</i><sub>2</sub>, ...}
 *   The <i>x</i>-coordinates must be strictly increasing.  The first and last
 *   <i>y</i>-coordinates must be 0.  The others must be positive or 0, and at
 *   least one of them must be positive.  The <i>y</i>-coordinates will be
 *   scaled if necessary to make the area under the curve equal 1.  The values
 *   from xy are copied, so the array can be freed if desired after calling
 *   this function.
 * @return a pointer to a newly-created PDF_dist_t structure, or NULL if there
 *   wasn't enough memory to allocate one.  If \a n = 1, this function will
 *   return a "point" distribution.
 */
PDF_dist_t *
PDF_new_piecewise_dist (unsigned int n, double *xy)
{
  PDF_dist_t *dist;
  PDF_piecewise_dist_t *p;      /* part specific to this distribution */
  unsigned int i;               /* loop counter */
  gboolean positive_y;
  double total;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_piecewise_dist");
#endif

  g_assert (n >= 1);

  if (n == 1)
    dist = PDF_new_point_dist (xy[1]);
  else
    {
      dist = g_new (PDF_dist_t, 1);
      dist->type = PDF_Piecewise;
      dist->has_inf_lower_tail = dist->has_inf_upper_tail = FALSE;
      p = &(dist->u.piecewise);
      p->x = g_new (double, n);
      p->y = g_new (double, n);
      p->cumul = g_new (double, n);
      p->slope = g_new (double, n - 1);

      /* Copy the coordinate list. */
      p->n = n;
      positive_y = FALSE;
      for (i = 0; i < n; i++)
        {
          p->x[i] = *xy++;
          if (i > 0)
            {
              g_assert (p->x[i] > p->x[i - 1]);
            }
          p->y[i] = *xy++;
          if (i == 0 || i == n - 1)
            g_assert (p->y[i] == 0);
          else
            {
              g_assert (p->y[i] >= 0);
              if (p->y[i] > 0)
                positive_y = TRUE;
            }
        }
      g_assert (positive_y == TRUE);
      p->first_x = p->x[0];
      p->last_x = p->x[n - 1];

      /* Calculate the cumulative probability at each x-coordinate.  If the
       * total is not 1, scale the y-coordinates to make it so. */
      total = calc_cumulative_for_piecewise (p);
      if (gsl_fcmp (total, 1, EPSILON) != 0)
        {
          for (i = 1; i < n - 1; i++)
            p->y[i] /= total;
          calc_cumulative_for_piecewise (p);
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_piecewise_dist");
#endif

  return dist;
}



/**
 * Deletes the dynamically-allocated parts of piecewise distribution from
 * memory.
 *
 * @param dist a piecewise distribution.
 */
void
PDF_free_piecewise_dist (PDF_piecewise_dist_t * dist)
{
  g_free (dist->x);
  g_free (dist->y);
  g_free (dist->slope);
  g_free (dist->cumul);
}



/**
 * Returns a text representation of a piecewise distribution.
 *
 * @param dist a piecewise distribution.
 * @return a string.
 */
char *
PDF_piecewise_dist_to_string (PDF_piecewise_dist_t * dist)
{
  GString *s;
  char *chararray;
  int i;                        /* loop counter */

  s = g_string_new ("<piecewise probability distribution\n points={");
  for (i = 0; i < dist->n; i++)
    g_string_sprintfa (s, i > 0 ? ", (%.2g,%.2g)" : "(%.2g,%.2g)", dist->x[i], dist->y[i]);
  g_string_append (s, "}\n cumulative={");
  for (i = 0; i < dist->n; i++)
    g_string_sprintfa (s, i > 0 ? ", %.2g" : "%.2g", dist->cumul[i]);
  g_string_append (s, "}>");
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Computes the probability density p(x) at x for a piecewise distribution.
 *
 * @param x
 * @param dist a piecewise distribution.
 */
double
piecewise_pdf (double x, PDF_piecewise_dist_t * dist)
{
  unsigned int seg;             /* the segment in which x lies */
  unsigned int lo, hi;          /* for a binary search */

  if (x <= dist->first_x || x >= dist->last_x)
    return 0.0;

  /* Find the segment in which x lies (binary search). */
  lo = 0;
  hi = dist->n;
  while (hi - lo > 1)
    {
      seg = (lo + hi) / 2;
      if (x >= dist->x[seg])
        lo = seg;
      else
        hi = seg;
    }

  /* Linear interpolation. */
  x -= dist->x[lo];
  return dist->y[lo] + x * dist->slope[lo];
}



/**
 * Computes the cumulative area <= \a x for a piecewise distribution.
 *
 * @param x
 * @param a a piecewise distribution.
 */
double
piecewise_cdf (double x, PDF_piecewise_dist_t * a)
{
  unsigned int seg;             /* the segment in which x lies */
  unsigned int lo, hi;          /* for a binary search */
  double y;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER piecewise_cdf");
#endif

  if (x <= a->first_x)
    return 0.0;
  if (x >= a->last_x)
    return 1.0;

  /* Find the segment in which x lies (binary search). */
  lo = 0;
  hi = a->n;
  while (hi - lo > 1)
    {
      seg = (lo + hi) / 2;
      if (x >= a->x[seg])
        lo = seg;
      else
        hi = seg;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "x (%g) is in segment %u", x, lo);
#endif

  x -= a->x[lo];
  y = a->y[lo] + x * a->slope[lo];

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT piecewise_cdf");
#endif

  return a->cumul[lo] + x * ((y + a->y[lo]) / 2);
}



/**
 * The inverse cumulative distribution function for a piecewise distribution.
 * In some cases this function cannot give a meaningful answer.  In the example
 * below, the area of the left triangle is 0.5.  The cumulative area is 0.5
 * over the range [3,4].  So this function may return any number in that range
 * if asked for the value at which the area = 0.5.
 *
 * @image html piecewise_flat.png
 * @image latex piecewise_flat.eps width=5cm
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a piecewise distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
piecewise_inverse_cdf (double area, PDF_piecewise_dist_t * dist)
{
  unsigned int seg;             /* the segment in which area lies */
  unsigned int lo, hi;          /* for a binary search */
  double x, y, slope;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER piecewise_inverse_cdf");
#endif

  if (area <= 0.0)
    return dist->first_x;
  if (area >= 1.0)
    return dist->last_x;

  /* Find the segment in which area lies (binary search). */
  lo = 0;
  hi = dist->n;
  while (hi - lo > 1)
    {
      seg = (lo + hi) / 2;
      if (area >= dist->cumul[seg])
        lo = seg;
      else
        hi = seg;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "area (%g) is in segment %u", area, lo);
#endif

  area -= dist->cumul[lo];
  y = dist->y[lo];
  slope = dist->slope[lo];
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "for trapezoid calculation area=%g, h1=%g, slope=%g", area, y, slope);
#endif

  if (gsl_fcmp (slope, 0, EPSILON) == 0)
    {
      /* easy case. */
      x = area / y;
    }
  else
    {
      /* this case requires solving a quadratic equation. */
      double x1, x2, max_x;
      int nsolutions;

      nsolutions = gsl_poly_solve_quadratic (slope / 2, y, -area, &x1, &x2);
      if (nsolutions == 1)
        x = x1;
      else
        {
          /* check which one is the solution */
          max_x = dist->x[lo + 1] - dist->x[lo];
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "max_x=%g, choices=(%g,%g)\n", max_x, x1, x2);
#endif
          x = ((x1 >= 0) && (x1 <= max_x) ? x1 : x2);
        }
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "solution=%g", x + dist->x[lo]);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT piecewise_inverse_cdf");
#endif

  return x + dist->x[lo];
}



/**
 * Returns a random variate from a piecewise distribution.
 *
 * @param dist a piecewise distribution.
 * @param rng a random number generator.
 */
double
ran_piecewise (PDF_piecewise_dist_t * dist, RAN_gen_t * rng)
{
  double r;

  while (1)
    {
      r = dist->first_x + RAN_num (rng) * (dist->last_x - dist->first_x);
      if (RAN_num (rng) <= piecewise_pdf (r, dist))
        break;
    }
  return r;
}



/**
 * Creates a new histogram distribution.
 *
 * @param histo a histogram.  This function copies the histogram, so the
 *   original may be freed or modified afterward.
 * @return a pointer to a newly-created PDF_dist_t structure, or NULL if there
 *   wasn't enough memory to allocate one.
 */
PDF_dist_t *
PDF_new_histogram_dist (gsl_histogram * histo)
{
  PDF_dist_t *dist;
  PDF_histogram_dist_t *t;      /* part specific to this distribution */
  gsl_histogram_pdf *pdf;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_histogram_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Histogram;
  dist->has_inf_lower_tail = dist->has_inf_upper_tail = FALSE;
  t = &(dist->u.histogram);
  t->histo = gsl_histogram_clone (histo);
  g_assert (t->histo != NULL);

  /* Scale the histogram so that its bin values sum to 1. */
  gsl_histogram_scale (t->histo, 1.0 / gsl_histogram_sum (histo));

  /* Allocate a GSL histogram probability distribution struct, which comes with
   * a sampling function. */
  pdf = gsl_histogram_pdf_alloc (gsl_histogram_bins (histo));
  g_assert (pdf != NULL);
  gsl_histogram_pdf_init (pdf, histo);

  t->pdf = pdf;
  t->first_x = gsl_histogram_min (histo);
  t->last_x = gsl_histogram_max (histo);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_histogram_dist");
#endif

  return dist;
}



/**
 * Deletes the dynamically-allocated parts of a histogram distribution from
 * memory.
 *
 * @param dist a histogram distribution.
 */
void
PDF_free_histogram_dist (PDF_histogram_dist_t * dist)
{
  gsl_histogram_pdf_free (dist->pdf);
  gsl_histogram_free (dist->histo);
}



/**
 * Returns a text representation of a histogram distribution.
 *
 * @param dist a histogram distribution.
 * @return a string.
 */
char *
PDF_histogram_dist_to_string (PDF_histogram_dist_t * dist)
{
  GString *s;
  size_t bins;
  double lower, upper;
  int i;                        /* loop counter */
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<histogram probability distribution\n %u bins",
                    gsl_histogram_bins (dist->histo));
  bins = gsl_histogram_bins (dist->histo);
  for (i = 0; i < bins; i++)
    {
      gsl_histogram_get_range (dist->histo, i, &lower, &upper);
      g_string_sprintfa (s, "\n bin #%i [%g,%g) %g", i, lower, upper,
                         gsl_histogram_get (dist->histo, i));
    }
  g_string_append_c (s, '>');
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Computes the probability density p(x) at x for a histogram distribution.
 *
 * @param x
 * @param dist a histogram distribution.
 */
double
histogram_pdf (double x, PDF_histogram_dist_t * dist)
{
  size_t i;

  if (x < dist->first_x || x >= dist->last_x)
    return 0.0;

  gsl_histogram_find (dist->histo, x, &i);
  return gsl_histogram_get (dist->histo, i);
}



/**
 * Computes the cumulative area <= \a x for a histogram distribution.
 *
 * @param x
 * @param dist a histogram distribution.
 */
double
histogram_cdf (double x, PDF_histogram_dist_t * dist)
{
  size_t bin;
  double lower, upper;
  double frac;
  double area;

  if (x < dist->first_x)
    area = 0.0;
  else if (x >= dist->last_x)
    area = 1.0;
  else
    {
      gsl_histogram_find (dist->histo, x, &bin);
      gsl_histogram_get_range (dist->histo, bin, &lower, &upper);
      frac = (x - lower) / (upper - lower);
      area = dist->pdf->sum[bin] + frac * (dist->pdf->sum[bin + 1] - dist->pdf->sum[bin]);
    }
  return area;
}



/**
 * The inverse cumulative distribution function for a histogram distribution.
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a histogram distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
histogram_inverse_cdf (double area, PDF_histogram_dist_t * dist)
{
  unsigned int seg;             /* the segment in which area lies */
  unsigned int lo, hi;          /* for a binary search */
  double x, y;
  double lower, upper;

  if (area <= 0.0)
    return dist->first_x;
  if (area >= 1.0)
    return dist->last_x;

  /* Find the segment in which area lies (binary search). */
  lo = 0;
  hi = gsl_histogram_bins (dist->histo);
  while (hi - lo > 1)
    {
      seg = (lo + hi) / 2;
      if (area >= dist->pdf->sum[seg])
        lo = seg;
      else
        hi = seg;
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "area (%g) is in bin %u", area, lo);
#endif

  area -= dist->pdf->sum[lo];
  y = gsl_histogram_get (dist->histo, lo);
  x = area / y;
  gsl_histogram_get_range (dist->histo, lo, &lower, &upper);
  return x + lower;
}



/**
 * Returns a random variate from a histogram distribution.
 *
 * @param dist a histogram distribution.
 * @param rng a random number generator.
 */
double
ran_histogram (PDF_histogram_dist_t * dist, RAN_gen_t * rng)
{
  return gsl_histogram_pdf_sample (dist->pdf, RAN_num (rng));
}



/**
 * Creates a new Gaussian distribution with parameters as illustrated below.
 *
 * @image html gaussian.png
 * @image latex gaussian.eps width=5cm
 *
 * @param mu the mean.
 * @param sigma the standard deviation.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_gaussian_dist (double mu, double sigma)
{
  PDF_dist_t *dist;
  PDF_gaussian_dist_t *t;       /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_gaussian_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Gaussian;
  dist->has_inf_lower_tail = dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.gaussian);
  t->mu = mu;
  t->sigma = sigma;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_gaussian_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a Gaussian distribution.
 *
 * @param dist a Gaussian distribution.
 * @return a string.
 */
char *
PDF_gaussian_dist_to_string (PDF_gaussian_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<Gaussian probability distribution\n mean=%.2g, std dev=%.2g>",
                    dist->mu, dist->sigma);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new Poisson distribution with parameters as illustrated below.
 *
 * @image html poisson.png
 * @image latex poisson.eps width=5cm
 *
 * @param mu the mean.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_poisson_dist (double mu)
{
  PDF_dist_t *dist;
  PDF_poisson_dist_t *t;        /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_poisson_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Poisson;
  dist->has_inf_lower_tail = FALSE;
  dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.poisson);
  t->mu = mu;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_poisson_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a Poisson distribution.
 *
 * @param dist a Poisson distribution.
 * @return a string.
 */
char *
PDF_poisson_dist_to_string (PDF_poisson_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<Poisson probability distribution\n mean=%.2g>", dist->mu);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Computes the cumulative area <= \a x for a Poisson distribution.  From 
 * Bowerman, Nolty, and Scheuer, "Calculation of the Poisson Cumulative
 * Distribution Function", IEEE Transactions on Reliability, vol. 32 no. 2,
 * June 1990.
 *
 * @param x
 * @param dist a Poisson distribution.
 * @return the area under the distribution curve to the left of \a x.
 */
double
poisson_cdf (double x, PDF_poisson_dist_t * dist)
{
  double BRK_POINT = 1, ADJ_FACTOR = 350.0;
  long n = (long) x, i, num_of_adjusts;
  double mu, cumprob, sum, old_factor;

  if (x < 0)
    return 0;

  num_of_adjusts = 0;
  sum = old_factor = 1.0;
  mu = dist->mu;

  for (i = 1; i <= n; i++)
    {
      old_factor = mu / i * old_factor;
      sum = sum + old_factor;
      if (sum > BRK_POINT)
        {
          old_factor = old_factor / exp (ADJ_FACTOR);
          sum = sum / exp (ADJ_FACTOR);
          num_of_adjusts++;
        }
    }

  cumprob = exp (num_of_adjusts * ADJ_FACTOR - mu) * sum;

  return cumprob;
}



/**
 * Computes the probability density p(x) at x for a Poisson distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param mean the mean used to define a Poisson distribution.
 * @return the probability density p(x) 
 */
DLL_API double
ar_poisson_pdf (double x, double mean)
{
  return gsl_ran_poisson_pdf ((unsigned int) x, mean);
}



/**
 * Computes the cumulative area <= \a x for a Poisson distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param mean the mean used to define a Poisson distribution.
 * @return the cumulative area <= \a x 
 */
DLL_API double
ar_poisson_cdf (double x, double mean)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_poisson_dist (mean);
  result = poisson_cdf (x, &(dist->u.poisson));
  PDF_free_dist (dist);
  return result;
}



/**
 * Creates a new beta distribution with parameters as illustrated below.
 *
 * @image html beta.png
 * @image latex beta.eps width=5cm
 *
 * @param alpha the alpha parameter.
 * @param beta the beta parameter.
 * @param location how many units left (-) or right (+) to shift the
 *   distribution.
 * @param scale the upper bound of the distribution.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_beta_dist (double alpha, double beta, double location, double scale)
{
  PDF_dist_t *dist;
  PDF_beta_dist_t *t;           /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_beta_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Beta;
  dist->has_inf_lower_tail = dist->has_inf_upper_tail = FALSE;
  t = &(dist->u.beta);
  t->alpha = alpha;
  t->beta = beta;
  t->location = location;
  t->scale = scale;
  t->width = scale - location;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_beta_dist");
#endif

  return dist;
}



/**
 * Creates a new beta distribution with parameters estimated from a minimum,
 * maximum, and mode.
 *
 * The formulas used to estimate the alpha and beta parameters are:
 * \f[d = \frac{min + 4 \times mode + max}{6}\f]
 * \f[\alpha = [ 6 \left[ \frac{d-min}{max-min} \right] \f]
 * \f[\beta = [ 6 \left[ \frac{max-d}{max-min} \right] \f]
 *
 *
 * These calculations follow A Concise Summary of @@RISK Probability
 * Distribution Functions (2002, Palisade Corporation).
 *
 * @image html betapert.png
 * @image latex betapert.eps width=5cm
 *
 * @param min the minimum.
 * @param mode the mode.
 * @param max the maximum.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_beta_pert_dist (double min, double mode, double max)
{
  PDF_dist_t *dist;
  PDF_beta_dist_t *t;           /* part specific to this distribution */
  double d, v;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_beta_pert_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Beta;
  dist->has_inf_lower_tail = dist->has_inf_upper_tail = FALSE;
  t = &(dist->u.beta);
  d = (min + 4 * mode + max) / 6;
  v = (max - min) * (max - min) / 36;

  /*
     // These calculations follow the @RISK documentation
   */
  t->alpha = 6 * ((d - min) / (max - min));
  t->beta = 6 * ((max - d) / (max - min));

  /*
     // These calculations follow an online course reader by R.E. Davis at
     // http://www.cob.sjsu.edu/facstaff/davis_r/courses/QBAreader/QBAtoc.html
     //
     // \f[d = \frac{min + 4 \times mode + max}{6}\f]
     // \f[\sigma^2 = \frac{(max-min)^2}{36} \f]
     // \f[
     //   \alpha = \left( \frac{d-min}{max-min} \right)
     //     \left[ \frac{(d-min)(max-d)}{\sigma^2} - 1 \right]
     // \f]
     // \f[
     //   \beta = \left( \frac{max-d}{d-min} \right) \alpha
     // \f]

     t->alpha = (d - min) / (max - min) * ((d - min) * (max - d) / v - 1);
     t->beta = (max - d) / (d - min) * t->alpha;
   */
  t->location = min;
  t->scale = max;
  t->width = max - min;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_beta_pert_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a beta distribution.
 *
 * @param dist a beta distribution.
 * @return a string.
 */
char *
PDF_beta_dist_to_string (PDF_beta_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<beta probability distribution\n alpha=%.3g beta=%.3g\n location=%.2g scale=%.2g>",
                    dist->alpha, dist->beta, dist->location, dist->scale);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Computes the cumulative area <= \a x for a beta distribution.
 *
 * @param x
 * @param dist a beta distribution.
 */
double
beta_cdf (double x, PDF_beta_dist_t * dist)
{
  double result;

  result = (x < dist->location) ? 0 :
    gsl_cdf_beta_P ((x - dist->location) / dist->width, dist->alpha, dist->beta);
  return result;
}



/**
 * The inverse cumulative distribution function for a beta distribution.  It
 * finds the answer iteratively using the bisection method.
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a beta distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
beta_inverse_cdf (double area, PDF_beta_dist_t * dist)
{
  int iter;
  double x_lo, x_hi, x, a;

  if (area == 0)
    return dist->location;

  x_lo = dist->location;
  x_hi = dist->scale;
  x = (x_hi - x_lo) / 2;        /* starting guess */
  iter = 0;
  do
    {
      iter++;
      a = beta_cdf (x, dist);
      if (fabs (a - area) < EPSILON)
        {
          /* We've found a good approximation to the answer. */
          break;
        }
      if (a < area)
        {
          /* We're to the left of the proper x.  Move x to the right. */
          x_lo = x;
          x = x_lo + (x_hi - x_lo) / 2;
        }
      else
        {
          /* We're to the right of the proper x.  Move x to the left. */
          x_hi = x;
          x = x_lo + (x_hi - x_lo) / 2;
        }
    }
  while (iter < MAX_BISECTION_ITER);

#ifndef NO_MODEL_LIBS
  if (iter == MAX_BISECTION_ITER)
    g_error ("Unable to compute beta inverse CDF after %i iterations",
             MAX_BISECTION_ITER);
#endif

  return x;
}



/**
 * Computes the probability density p(x) at x for a beta distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param alpha parameter used to define a beta PDF.
 * @param beta parameter used to define a beta PDF.
 * @param location the minimum value used to define a beta PDF.
 * @param scale the maximum value used to define a beta PDF.
 * @return the probability density p(x) 
 */
DLL_API double
ar_beta_pdf (double x, double alpha, double beta, double location, double scale)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_beta_dist (alpha, beta, location, scale);

  /* This returns the probability density for the "unit" beta, with width = 1 */
  result =
    gsl_ran_beta_pdf ((x - dist->u.beta.location) / dist->u.beta.width, dist->u.beta.alpha,
                      dist->u.beta.beta);

  /* For non-unit beta functions, this correction is necessary */
  result = result / dist->u.beta.width;

  return result;
}



/**
 * Computes the cumulative area <= \a x for a beta distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param alpha parameter used to define a beta PDF.
 * @param beta parameter used to define a beta PDF.
 * @param location the minimum value used to define a beta PDF.
 * @param scale the maximum value used to define a beta PDF.
 * @return the cumulative area <= \a x 
 */
DLL_API double
ar_beta_cdf (double x, double alpha, double beta, double location, double scale)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_beta_dist (alpha, beta, location, scale);
  result = beta_cdf (x, &(dist->u.beta));
  PDF_free_dist (dist);

  return result;
}


/**
 * The inverse cumulative distribution function for a beta distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param area 0 <= \a area <= 1.
 * @param alpha1 parameter used to define a beta PDF.
 * @param alpha2 parameter used to define a beta PDF.
 * @param min the minimum value used to define a beta PDF.
 * @param max the maximum value used to define a beta PDF.
 * @return the value at which the cumulative distribution function = \a area.
 */
DLL_API double
ar_beta_inverse_cdf (double area, double alpha1, double alpha2, double min, double max)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_beta_dist (alpha1, alpha2, min, max);
  result = beta_inverse_cdf (area, &(dist->u.beta));
  PDF_free_dist (dist);
  return result;
}



/**
 * Computes the probability density p(x) at x for a betaPERT distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param min the minimum value used to define a betaPERT PDF.
 * @param mode the mode used to define a betaPERT PDF.
 * @param max the maximum value used to define a betaPERT PDF.
 * @return the probability density p(x) 
 */
DLL_API double
ar_beta_pert_pdf (double x, double min, double mode, double max)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_beta_pert_dist (min, mode, max);
  result =
    ar_beta_pdf (x, dist->u.beta.alpha, dist->u.beta.beta, dist->u.beta.location,
                 dist->u.beta.scale);
  PDF_free_dist (dist);
  return result;
}


/**
 * Computes the cumulative area <= \a x for a betaPERT distribution. 
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param min the minimum value used to define a betaPERT PDF.
 * @param mode the mode used to define a betaPERT PDF.
 * @param max the maximum value used to define a betaPERT PDF.
 * @return the cumulative area <= \a x 
 */
DLL_API double
ar_beta_pert_cdf (double x, double min, double mode, double max)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_beta_pert_dist (min, mode, max);
  result = beta_cdf (x, &(dist->u.beta));
  PDF_free_dist (dist);
  return result;
}



/**
 * The inverse cumulative distribution function for a betaPERT distribution. 
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param area 0 <= \a area <= 1.
 * @param min the minimum value used to define a betaPERT PDF.
 * @param mode the mode used to define a betaPERT PDF.
 * @param max the maximum value used to define a betaPERT PDF.
 * @return the value at which the cumulative distribution function = \a area.
 */
DLL_API double
ar_beta_pert_inverse_cdf (double area, double min, double mode, double max)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_beta_pert_dist (min, mode, max);
  result = beta_inverse_cdf (area, &(dist->u.beta));
  PDF_free_dist (dist);
  return result;
}



/**
 * Creates a new gamma distribution with parameters as illustrated below.
 *
 * @image html gamma.png
 * @image latex gamma.eps width=5cm
 *
 * @param alpha the alpha parameter.
 * @param beta the beta parameter.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_gamma_dist (double alpha, double beta)
{
  PDF_dist_t *dist;
  PDF_gamma_dist_t *t;          /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_gamma_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Gamma;
  dist->has_inf_lower_tail = FALSE;
  dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.gamma);
  t->alpha = alpha;
  t->beta = beta;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_gamma_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a gamma distribution.
 *
 * @param dist a gamma distribution.
 * @return a string.
 */
char *
PDF_gamma_dist_to_string (PDF_gamma_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<gamma probability distribution\n alpha=%.3g beta=%.3g>",
                    dist->alpha, dist->beta);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new Weibull distribution with parameters as illustrated below.
 *
 * @image html weibull.png
 * @image latex weibull.eps width=5cm
 *
 * @param alpha the exponent parameter.
 * @param beta the scale parameter.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_weibull_dist (double alpha, double beta)
{
  PDF_dist_t *dist;
  PDF_weibull_dist_t *t;        /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_weibull_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Weibull;
  dist->has_inf_lower_tail = FALSE;
  dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.weibull);
  t->alpha = alpha;
  t->beta = beta;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_weibull_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a Weibull distribution.
 *
 * @param dist a Weibull distribution.
 * @return a string.
 */
char *
PDF_weibull_dist_to_string (PDF_weibull_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<Weibull probability distribution\n alpha=%.2g beta=%.2g>",
                    dist->alpha, dist->beta);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new exponential distribution with parameters as illustrated below.
 *
 * @image html exponential.png
 * @image latex exponential.eps width=5cm
 *
 * @param mean the mean.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_exponential_dist (double mean)
{
  PDF_dist_t *dist;
  PDF_exponential_dist_t *t;    /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_exponential_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Exponential;
  dist->has_inf_lower_tail = TRUE;
  dist->has_inf_upper_tail = FALSE;
  t = &(dist->u.exponential);
  t->mean = mean;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_exponential_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of an exponential distribution.
 *
 * @param dist an exponential distribution.
 * @return a string.
 */
char *
PDF_exponential_dist_to_string (PDF_exponential_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<exponential probability distribution\n mean=%.2g>", dist->mean);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new Pearson Type V distribution with parameters as illustrated
 * below.
 *
 * @image html pearson.png
 * @image latex pearson.eps width=5cm.
 *
 * @param alpha the alpha parameter.
 * @param beta the beta parameter.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_pearson5_dist (double alpha, double beta)
{
  PDF_dist_t *dist;
  PDF_pearson5_dist_t *t;       /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_pearson5_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Pearson5;
  dist->has_inf_lower_tail = FALSE;
  dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.pearson5);
  t->alpha = alpha;
  t->beta = beta;
  t->one_over_beta = 1.0 / beta;
  t->loggamma_alpha = gsl_sf_lngamma (alpha);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_pearson5_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a Pearson Type V distribution.
 *
 * @param dist a Pearson Type V distribution.
 * @return a string.
 */
char *
PDF_pearson5_dist_to_string (PDF_pearson5_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<Pearson Type V probability distribution\n alpha=%.2g beta=%.2g>",
                    dist->alpha, dist->beta);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Computes the probability density p(x) at x for a Pearson Type V
 * distribution.
 *
 * @param x
 * @param dist a Pearson Type V distribution.
 */
double
pearson5_pdf (double x, PDF_pearson5_dist_t * dist)
{
  double a, b;

  if (x <= 0)
    return 0;

  a = dist->alpha;
  b = dist->beta;
  return exp (-b / x - dist->loggamma_alpha - (a + 1) * log (x / b)) / b;
}



/**
 * The function pearson5_pdf cast to a gsl_function.
 */
double
pearson5_pdf_as_gsl_function (double x, void *params)
{
  return pearson5_pdf (x, (PDF_pearson5_dist_t *) params);
}



/**
 * Computes the cumulative area <= \a x for a Pearson Type V distribution.
 * Uses numerical integration.
 *
 * @param x
 * @param dist a Pearson Type V distribution.
 */
double
pearson5_cdf (double x, PDF_pearson5_dist_t * dist)
{
  gsl_function f;
  double result;
  int err;
  double abserr;
  size_t dummy;

  if (x <= 0)
    return 0;

  f.function = pearson5_pdf_as_gsl_function;
  f.params = (void *) dist;

  err = gsl_integration_qng (&f, 0, x, EPSILON, 1, &result, &abserr, &dummy);
  return result;
}



/**
 * The inverse cumulative distribution function for a Pearson Type V
 * distribution.  It finds the answer iteratively using the bisection method.
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a Pearson Type V distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
pearson5_inverse_cdf (double area, PDF_pearson5_dist_t * dist)
{
  int iter;
  double x_lo, x_hi, x, a;
  gboolean hibound_found;

  if (area == 0)
    return 0;

  x_lo = 0;
  x = dist->beta / (dist->alpha - 1);   /* starting guess = mean */
  hibound_found = FALSE;
  iter = 0;
  do
    {
      iter++;
      a = pearson5_cdf (x, dist);
      if (fabs (a - area) < EPSILON)
        {
          /* We've found a good approximation to the answer. */
          break;
        }
      if (a < area)
        {
          /* We're to the left of the proper x.  Move x to the right. */
          x_lo = x;
          if (hibound_found)
            x = x_lo + (x_hi - x_lo) / 2;
          else
            x = 2 * x;
        }
      else
        {
          /* We're to the right of the proper x.  Move x to the left. */
          if (!hibound_found)
            hibound_found = TRUE;
          x_hi = x;
          x = x_lo + (x_hi - x_lo) / 2;
        }
    }
  while (iter < MAX_BISECTION_ITER);

#ifndef NO_MODEL_LIBS
  if (iter == MAX_BISECTION_ITER)
    g_error ("Unable to compute Pearson Type V inverse CDF after %i iterations",
             MAX_BISECTION_ITER);
#endif

  return x;
}


/*
 * Computes the probability density p(x) at x for a Pearson Type V
 * distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param alpha the alpha parameter of the distribution.
 * @param beta the beta parameter of the distribution.
 * @return the probability density p(x) 
 */
DLL_API double
ar_pearson5_pdf (double x, double alpha, double beta)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_pearson5_dist (alpha, beta);
  result = pearson5_pdf (x, &(dist->u.pearson5));
  PDF_free_dist (dist);
  return result;
}



/*
 * Computes the cumulative area <= \a x for a Pearson Type V
 * distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param alpha the alpha parameter of the distribution.
 * @param beta the beta parameter of the distribution.
 * @return the cumulative area <= \a x 
 */
DLL_API double
ar_pearson5_cdf (double x, double alpha, double beta)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_pearson5_dist (alpha, beta);
  result = pearson5_cdf (x, &(dist->u.pearson5));
  PDF_free_dist (dist);
  return result;
}



/*
 * The inverse cumulative distribution function for a Pearson Type V
 * distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param area 0 <= \a area <= 1.
 * @param alpha the alpha parameter of the distribution.
 * @param beta the beta parameter of the distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
DLL_API double
ar_pearson5_inverse_cdf (double area, double alpha, double beta)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_pearson5_dist (alpha, beta);
  result = pearson5_inverse_cdf (area, &(dist->u.pearson5));
  PDF_free_dist (dist);
  return result;
}



/**
 * Creates a new logistic distribution with parameters as illustrated below.
 *
 * @image html logistic.png
 * @image latex logistic.eps width=5cm.
 *
 * @param location the location parameter.
 * @param scale the scale parameter.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_logistic_dist (double location, double scale)
{
  PDF_dist_t *dist;
  PDF_logistic_dist_t *t;       /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_logistic_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Logistic;
  dist->has_inf_lower_tail = TRUE;
  dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.logistic);
  t->location = location;
  t->scale = scale;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_logistic_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a logistic distribution.
 *
 * @param dist a logistic distribution.
 * @return a string.
 */
char *
PDF_logistic_dist_to_string (PDF_logistic_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<logistic probability distribution\n location=%.2g scale=%.2g>",
                    dist->location, dist->scale);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Creates a new loglogistic (a.k.a. Fisk) distribution with parameters as
 * illustrated below.
 *
 * @image html loglogistic.png
 * @image latex loglogistic.eps width=5cm.
 *
 * @param location the location parameter.
 * @param scale the scale parameter.
 * @param shape the scale parameter.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_loglogistic_dist (double location, double scale, double shape)
{
  PDF_dist_t *dist;
  PDF_loglogistic_dist_t *t;    /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_loglogistic_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_LogLogistic;
  dist->has_inf_lower_tail = FALSE;
  dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.loglogistic);
  t->location = location;
  t->scale = scale;
  t->shape = shape;
  t->shape_over_scale = shape / scale;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_loglogistic_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a loglogistic distribution.
 *
 * @param dist a loglogistic distribution.
 * @return a string.
 */
char *
PDF_loglogistic_dist_to_string (PDF_loglogistic_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<loglogistic probability distribution\n location=%.2g scale=%.2g shape=%.2g>",
                    dist->location, dist->scale, dist->shape);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Computes the probability density p(x) at x for a loglogistic distribution.
 *
 * @param x
 * @param dist a loglogistic distribution.
 */
double
loglogistic_pdf (double x, PDF_loglogistic_dist_t * dist)
{
  double A, B, C, tmp1, tmp2;

  if (x <= dist->location)
    return 0;

  A = dist->location;
  B = dist->scale;
  C = dist->shape;

  tmp1 = (x - A) / B;
  tmp2 = 1 + pow (tmp1, C);

  return dist->shape_over_scale * pow (tmp1, C - 1) / (tmp2 * tmp2);
}



/**
 * Computes the cumulative area <= \a x for a loglogistic distribution.
 *
 * @param x
 * @param dist a loglogistic distribution.
 */
double
loglogistic_cdf (double x, PDF_loglogistic_dist_t * dist)
{
  double A, B, C;

  if (x <= dist->location)
    return 0;

  A = dist->location;
  B = dist->scale;
  C = dist->shape;

  return 1 / (1 + pow ((x - A) / B, -C));
}



/**
 * The inverse cumulative distribution function for a loglogistic distribution.
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a loglogistic distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
loglogistic_inverse_cdf (double area, PDF_loglogistic_dist_t * dist)
{
  double A, B, C;

  A = dist->location;
  B = dist->scale;
  C = dist->shape;

  return B * pow (area / (1 - area), 1 / C) + A;
}



/**
 * Returns a random variate from a loglogistic distribution.
 *
 * @param dist a loglogistic distribution.
 * @param rng a random number generator.
 */
double
ran_loglogistic (PDF_loglogistic_dist_t * dist, RAN_gen_t * rng)
{
  double A, B, C, r;

  A = dist->location;
  B = dist->scale;
  C = dist->shape;

  r = RAN_num (rng);
  return A + B * pow (r / (1 - r), 1 / C);
}



/**
 * Computes the probability density p(x) at x for a loglogistic distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param location the location parameter of the distribution.
 * @param scale the scale parameter of the distribution.
 * @param shape the scale parameter of the distribution.
 * @return the probability density p(x) 
 */
DLL_API double
ar_loglogistic_pdf (double x, double location, double scale, double shape)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_loglogistic_dist (location, scale, shape);
  result = loglogistic_pdf (x, &(dist->u.loglogistic));
  PDF_free_dist (dist);
  return result;
}



/**
 * Computes the cumulative area <= \a x for a loglogistic distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param x
 * @param location the location parameter of the distribution.
 * @param scale the scale parameter of the distribution.
 * @param shape the scale parameter of the distribution.
 * @return the cumulative area <= \a x 
 */
DLL_API double
ar_loglogistic_cdf (double x, double location, double scale, double shape)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_loglogistic_dist (location, scale, shape);
  result = loglogistic_cdf (x, &(dist->u.loglogistic));
  PDF_free_dist (dist);
  return result;
}



/**
 * The inverse cumulative distribution function for a loglogistic distribution.
 * This form of the function is primarily intended to be called from a 
 * library.
 *
 * @param area 0 <= \a area <= 1.
 * @param location the location parameter of the distribution.
 * @param scale the scale parameter of the distribution.
 * @param shape the scale parameter of the distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
DLL_API double
ar_loglogistic_inverse_cdf (double area, double location, double scale, double shape)
{
  PDF_dist_t *dist;
  double result;

  dist = PDF_new_loglogistic_dist (location, scale, shape);
  result = loglogistic_inverse_cdf (area, &(dist->u.loglogistic));
  PDF_free_dist (dist);
  return result;
}



/**
 * Creates a new lognormal distribution with parameters as illustrated below.
 *
 * @image html lognormal.png
 * @image latex lognormal.eps width=5cm.
 *
 * @param zeta the zeta parameter.
 * @param sigma the sigma parameter.
 * @return a pointer to a newly-created PDF_dist_t structure.
 */
PDF_dist_t *
PDF_new_lognormal_dist (double zeta, double sigma)
{
  PDF_dist_t *dist;
  PDF_lognormal_dist_t *t;      /* part specific to this distribution */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PDF_new_lognormal_dist");
#endif

  dist = g_new (PDF_dist_t, 1);
  dist->type = PDF_Lognormal;
  dist->has_inf_lower_tail = FALSE;
  dist->has_inf_upper_tail = TRUE;
  t = &(dist->u.lognormal);
  t->zeta = zeta;
  t->sigma = sigma;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PDF_new_lognormal_dist");
#endif

  return dist;
}



/**
 * Returns a text representation of a lognormal distribution.
 *
 * @param dist a lognormal distribution.
 * @return a string.
 */
char *
PDF_lognormal_dist_to_string (PDF_lognormal_dist_t * dist)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s,
                    "<lognormal probability distribution\n zeta=%.2g sigma=%.2g>",
                    dist->zeta, dist->sigma);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Deletes a distribution from memory.
 *
 * @param dist a distribution.
 */
void
PDF_free_dist (PDF_dist_t * dist)
{
  if (dist == NULL)
    return;

  switch (dist->type)
    {
    case PDF_Piecewise:
      PDF_free_piecewise_dist (&(dist->u.piecewise));
      break;
    case PDF_Histogram:
      PDF_free_histogram_dist (&(dist->u.histogram));
      break;
    default:
      /* No dynamically-allocated parts to free. */
      break;
    }
  g_free (dist);
}



/**
 * Makes a deep copy of a distribution.
 *
 * @param dist a distribution.
 * @return a deep copy of the distribution.
 */
PDF_dist_t *
PDF_clone_dist (PDF_dist_t * dist)
{
  PDF_dist_t *clone;

  if (dist == NULL)
    return NULL;

  switch (dist->type)
    {
    case PDF_Point:
      {
        PDF_point_dist_t *d;
        d = &(dist->u.point);
        clone = PDF_new_point_dist (d->value);
        break;
      }
    case PDF_Uniform:
      {
        PDF_uniform_dist_t *d;
        d = &(dist->u.uniform);
        clone = PDF_new_uniform_dist (d->a, d->b);
        break;
      }
    case PDF_Triangular:
      {
        PDF_triangular_dist_t *d;
        d = &(dist->u.triangular);
        clone = PDF_new_triangular_dist (d->a, d->c, d->b);
        break;
      }
    case PDF_Piecewise:
      {
        PDF_piecewise_dist_t *d;
        double *xy;
        unsigned int n, i;

        d = &(dist->u.piecewise);
        /* Copy the x-y coordinates from the existing distribution. */
        n = d->n;
        xy = g_new (double, 2 * n);
        for (i = 0; i < n; i++)
          {
            xy[2 * i] = d->x[i];
            xy[2 * i + 1] = d->y[i];
          }
        clone = PDF_new_piecewise_dist (n, xy);
        g_free (xy);
        break;
      }
    case PDF_Histogram:
      {
        PDF_histogram_dist_t *d;
        d = &(dist->u.histogram);
        clone = PDF_new_histogram_dist (d->histo);
        break;
      }
    case PDF_Gaussian:
      {
        PDF_gaussian_dist_t *d;
        d = &(dist->u.gaussian);
        clone = PDF_new_gaussian_dist (d->mu, d->sigma);
        break;
      }
    case PDF_Poisson:
      {
        PDF_poisson_dist_t *d;
        d = &(dist->u.poisson);
        clone = PDF_new_poisson_dist (d->mu);
        break;
      }
    case PDF_Beta:
      {
        PDF_beta_dist_t *d;
        d = &(dist->u.beta);
        clone = PDF_new_beta_dist (d->alpha, d->beta, d->location, d->scale);
        break;
      }
    case PDF_Gamma:
      {
        PDF_gamma_dist_t *d;
        d = &(dist->u.gamma);
        clone = PDF_new_gamma_dist (d->alpha, d->beta);
        break;
      }
    case PDF_Weibull:
      {
        PDF_weibull_dist_t *d;
        d = &(dist->u.weibull);
        clone = PDF_new_weibull_dist (d->alpha, d->beta);
        break;
      }
    case PDF_Exponential:
      {
        PDF_exponential_dist_t *d;
        d = &(dist->u.exponential);
        clone = PDF_new_exponential_dist (d->mean);
        break;
      }
    case PDF_Pearson5:
      {
        PDF_pearson5_dist_t *d;
        d = &(dist->u.pearson5);
        clone = PDF_new_pearson5_dist (d->alpha, d->beta);
        break;
      }
    case PDF_Logistic:
      {
        PDF_logistic_dist_t *d;
        d = &(dist->u.logistic);
        clone = PDF_new_logistic_dist (d->location, d->scale);
        break;
      }
    case PDF_LogLogistic:
      {
        PDF_loglogistic_dist_t *d;
        d = &(dist->u.loglogistic);
        clone = PDF_new_loglogistic_dist (d->location, d->scale, d->shape);
        break;
      }
    case PDF_Lognormal:
      {
        PDF_lognormal_dist_t *d;
        d = &(dist->u.lognormal);
        clone = PDF_new_lognormal_dist (d->zeta, d->sigma);
        break;
      }
    default:
      g_assert_not_reached ();
    }

  return clone;
}



/**
 * Returns a text representation of a distribution.
 *
 * @param dist a distribution.
 * @return a string.
 */
char *
PDF_dist_to_string (PDF_dist_t * dist)
{
  char *s;

  switch (dist->type)
    {
    case PDF_Point:
      s = PDF_point_dist_to_string (&(dist->u.point));
      break;
    case PDF_Uniform:
      s = PDF_uniform_dist_to_string (&(dist->u.uniform));
      break;
    case PDF_Triangular:
      s = PDF_triangular_dist_to_string (&(dist->u.triangular));
      break;
    case PDF_Piecewise:
      s = PDF_piecewise_dist_to_string (&(dist->u.piecewise));
      break;
    case PDF_Histogram:
      s = PDF_histogram_dist_to_string (&(dist->u.histogram));
      break;
    case PDF_Gaussian:
      s = PDF_gaussian_dist_to_string (&(dist->u.gaussian));
      break;
    case PDF_Poisson:
      s = PDF_poisson_dist_to_string (&(dist->u.poisson));
      break;
    case PDF_Beta:
      s = PDF_beta_dist_to_string (&(dist->u.beta));
      break;
    case PDF_Gamma:
      s = PDF_gamma_dist_to_string (&(dist->u.gamma));
      break;
    case PDF_Weibull:
      s = PDF_weibull_dist_to_string (&(dist->u.weibull));
      break;
    case PDF_Exponential:
      s = PDF_exponential_dist_to_string (&(dist->u.exponential));
      break;
    case PDF_Pearson5:
      s = PDF_pearson5_dist_to_string (&(dist->u.pearson5));
      break;
    case PDF_Logistic:
      s = PDF_logistic_dist_to_string (&(dist->u.logistic));
      break;
    case PDF_LogLogistic:
      s = PDF_loglogistic_dist_to_string (&(dist->u.loglogistic));
      break;
    case PDF_Lognormal:
      s = PDF_lognormal_dist_to_string (&(dist->u.lognormal));
      break;
    default:
      g_assert_not_reached ();
    }

  return s;
}



/**
 * Prints a distribution to a stream.
 *
 * @param stream a stream to write to.  If NULL, defaults to stdout.
 * @param dist a distribution.
 * @return the number of characters printed (not including the trailing '\\0').
 */
int
PDF_fprintf_dist (FILE * stream, PDF_dist_t * dist)
{
  char *s;
  int nchars_written;

  s = PDF_dist_to_string (dist);
  nchars_written = fprintf (stream ? stream : stdout, "%s", s);
  free (s);
  return nchars_written;
}



/**
 * Returns a random variate from a distribution.
 *
 * @param dist a distribution.
 * @param rng a random number generator.
 * @return a random number drawn from \a dist.
 */
double
PDF_random (PDF_dist_t * dist, RAN_gen_t * rng)
{
  double r;

  switch (dist->type)
    {
    case PDF_Triangular:
      r = ran_triangular (&(dist->u.triangular), rng);
      break;
    case PDF_Piecewise:
      r = ran_piecewise (&(dist->u.piecewise), rng);
      break;
    case PDF_Uniform:
      r = gsl_ran_flat (RAN_generator_as_gsl (rng), dist->u.uniform.a, dist->u.uniform.b);
      break;
    case PDF_Histogram:
      r = ran_histogram (&(dist->u.histogram), rng);
      break;
    case PDF_Gaussian:
      r = gsl_ran_gaussian (RAN_generator_as_gsl (rng), dist->u.gaussian.sigma);
      r += dist->u.gaussian.mu;
      break;
    case PDF_Poisson:
      r = gsl_ran_poisson (RAN_generator_as_gsl (rng), dist->u.poisson.mu);
      break;
    case PDF_Point:
      r = dist->u.point.value;
      break;
    case PDF_Beta:
      r = gsl_ran_beta (RAN_generator_as_gsl (rng), dist->u.beta.alpha, dist->u.beta.beta);
      r = r * dist->u.beta.width + dist->u.beta.location;
      break;
    case PDF_Gamma:
      r = gsl_ran_gamma (RAN_generator_as_gsl (rng), dist->u.gamma.alpha, dist->u.gamma.beta);
      break;
    case PDF_Weibull:
      /* 
         The order in which the GSL functions for Weibull distributions accept
         parameters is different from most other references.  For this reason,
         it may appear that these parameters are given in the wrong order.  
       */
      r = gsl_ran_weibull (RAN_generator_as_gsl (rng), dist->u.weibull.beta, dist->u.weibull.alpha);
      break;
    case PDF_Exponential:
      r = gsl_ran_exponential (RAN_generator_as_gsl (rng), dist->u.exponential.mean);
      break;
    case PDF_Pearson5:
      r = 1.0 / gsl_ran_gamma (RAN_generator_as_gsl (rng),
                               dist->u.pearson5.alpha, dist->u.pearson5.one_over_beta);
      break;
    case PDF_Logistic:
      r = gsl_ran_logistic (RAN_generator_as_gsl (rng), dist->u.logistic.scale);
      r += dist->u.logistic.location;
      break;
    case PDF_LogLogistic:
      r = ran_loglogistic (&(dist->u.loglogistic), rng);
      break;
    case PDF_Lognormal:
      r = gsl_ran_lognormal (RAN_generator_as_gsl (rng),
                             dist->u.lognormal.zeta, dist->u.lognormal.sigma);
      break;
    default:
      g_assert_not_reached ();
    }

  return r;
}



/**
 * Computes the probability density p(x) at x for a distribution.
 *
 * @param x
 * @param dist a distribution.
 * @return p(x).
 */
double
PDF_pdf (double x, PDF_dist_t * dist)
{
  double p;

  switch (dist->type)
    {
    case PDF_Triangular:
      p = triangular_pdf (x, &(dist->u.triangular));
      break;
    case PDF_Piecewise:
      p = piecewise_pdf (x, &(dist->u.piecewise));
      break;
    case PDF_Uniform:
      p = gsl_ran_flat_pdf (x, dist->u.uniform.a, dist->u.uniform.b);
      break;
    case PDF_Histogram:
      p = histogram_pdf (x, &(dist->u.histogram));
      break;
    case PDF_Gaussian:
      p = gsl_ran_gaussian_pdf (x - dist->u.gaussian.mu, dist->u.gaussian.sigma);
      break;
    case PDF_Poisson:
      p = gsl_ran_poisson_pdf ((unsigned int) x, dist->u.poisson.mu);
      break;
    case PDF_Point:
      p = (gsl_fcmp (x, dist->u.point.value, EPSILON) == 0) ? 1 : 0;
      break;
    case PDF_Beta:
      /* This returns the probability density for the "unit" beta, with width = 1 */
      p = gsl_ran_beta_pdf ((x - dist->u.beta.location) / dist->u.beta.width,
                            dist->u.beta.alpha, dist->u.beta.beta);

      /* For non-unit beta PDFs, this transformation is necessary */
      p = p / dist->u.beta.width;
      break;
    case PDF_Gamma:
      p = gsl_ran_gamma_pdf (x, dist->u.gamma.alpha, dist->u.gamma.beta);
      break;
    case PDF_Weibull:
      /* 
         The order in which the GSL functions for Weibull distributions accept
         parameters is different from most other references.  For this reason,
         it may appear that these parameters are given in the wrong order.  
       */
      p = gsl_ran_weibull_pdf (x, dist->u.weibull.beta, dist->u.weibull.alpha);
      break;
    case PDF_Exponential:
      p = gsl_ran_exponential_pdf (x, dist->u.exponential.mean);
      break;
    case PDF_Pearson5:
      p = pearson5_pdf (x, &(dist->u.pearson5));
      break;
    case PDF_Logistic:
      p = gsl_ran_logistic_pdf (x - dist->u.logistic.location, dist->u.logistic.scale);
      break;
    case PDF_LogLogistic:
      p = loglogistic_pdf (x, &(dist->u.loglogistic));
      break;
    case PDF_Lognormal:
      p = gsl_ran_lognormal_pdf (x, dist->u.lognormal.zeta, dist->u.lognormal.sigma);
      break;
    default:
      g_assert_not_reached ();
    }
  return p;
}



/**
 * Computes the cumulative area <= \a x for a distribution.
 *
 * @param x
 * @param dist a distribution.
 * @return the area under the distribution curve to the left of \a x.
 */
double
PDF_cdf (double x, PDF_dist_t * dist)
{
  double c;

  switch (dist->type)
    {
    case PDF_Triangular:
      c = triangular_cdf (x, &(dist->u.triangular));
      break;
    case PDF_Piecewise:
      c = piecewise_cdf (x, &(dist->u.piecewise));
      break;
    case PDF_Uniform:
      c = uniform_cdf (x, &(dist->u.uniform));
      break;
    case PDF_Histogram:
      c = histogram_cdf (x, &(dist->u.histogram));
      break;
    case PDF_Gaussian:
      c = gsl_cdf_gaussian_P (x - dist->u.gaussian.mu, dist->u.gaussian.sigma);
      break;
    case PDF_Poisson:
      c = poisson_cdf (x, &(dist->u.poisson));
      break;
    case PDF_Point:
      c = (x < dist->u.point.value) ? 0 : 1;
      break;
    case PDF_Beta:
      c = beta_cdf (x, &(dist->u.beta));
      break;
    case PDF_Gamma:
      c = gsl_cdf_gamma_P (x, dist->u.gamma.alpha, dist->u.gamma.beta);
      break;
    case PDF_Weibull:
      /* 
         The order in which the GSL functions for Weibull distributions accept
         parameters is different from most other references.  For this reason,
         it may appear that these parameters are given in the wrong order.  
       */
      c = (x < 0) ? 0 : gsl_cdf_weibull_P (x, dist->u.weibull.beta, dist->u.weibull.alpha);
      break;
    case PDF_Exponential:
      c = (x < 0) ? 0 : gsl_cdf_exponential_P (x, dist->u.exponential.mean);
      break;
    case PDF_Pearson5:
      c = pearson5_cdf (x, &(dist->u.pearson5));
      break;
    case PDF_Logistic:
      c = gsl_cdf_logistic_P (x - dist->u.logistic.location, dist->u.logistic.scale);
      break;
    case PDF_LogLogistic:
      c = loglogistic_cdf (x, &(dist->u.loglogistic));
      break;
    case PDF_Lognormal:
      c = (x < 0) ? 0 : gsl_cdf_lognormal_P (x, dist->u.lognormal.zeta, dist->u.lognormal.sigma);
      break;
    default:
      g_assert_not_reached ();
    }
  return c;
}



/**
 * The inverse cumulative distribution function.
 *
 * @param area 0 <= \a area <= 1.
 * @param dist a distribution.
 * @return the value at which the cumulative distribution function = \a area.
 */
double
PDF_inverse_cdf (double area, PDF_dist_t * dist)
{
  double c;

  g_assert (0 <= area && area <= 1);

  switch (dist->type)
    {
    case PDF_Triangular:
      c = triangular_inverse_cdf (area, &(dist->u.triangular));
      break;
    case PDF_Piecewise:
      c = piecewise_inverse_cdf (area, &(dist->u.piecewise));
      break;
    case PDF_Uniform:
      c = uniform_inverse_cdf (area, &(dist->u.uniform));
      break;
    case PDF_Histogram:
      c = histogram_inverse_cdf (area, &(dist->u.histogram));
      break;
    case PDF_Gaussian:
      c = gsl_cdf_gaussian_Pinv (area, dist->u.gaussian.sigma) + dist->u.gaussian.mu;
      break;
    case PDF_Point:
      c = dist->u.point.value;
      break;
    case PDF_Beta:
      c = beta_inverse_cdf (area, &(dist->u.beta));
      break;
    case PDF_Gamma:
      c = gsl_cdf_gamma_Pinv (area, dist->u.gamma.alpha, dist->u.gamma.beta);
      break;
    case PDF_Weibull:
      /* 
         The order in which the GSL functions for Weibull distributions accept
         parameters is different from most other references.  For this reason,
         it may appear that these parameters are given in the wrong order.  
       */
      c = gsl_cdf_weibull_Pinv (area, dist->u.weibull.beta, dist->u.weibull.alpha);
      break;
    case PDF_Exponential:
      c = gsl_cdf_exponential_Pinv (area, dist->u.exponential.mean);
      break;
    case PDF_Pearson5:
      c = pearson5_inverse_cdf (area, &(dist->u.pearson5));
      break;
    case PDF_Logistic:
      c = gsl_cdf_logistic_Pinv (area, dist->u.logistic.scale) + dist->u.logistic.location;
      break;
    case PDF_LogLogistic:
      c = loglogistic_inverse_cdf (area, &(dist->u.loglogistic));
      break;
    case PDF_Lognormal:
      c = gsl_cdf_lognormal_Pinv (area, dist->u.lognormal.zeta, dist->u.lognormal.sigma);
      break;
    default:
      g_assert_not_reached ();
    }
  return c;
}

/* end of file prob_dist.c */
