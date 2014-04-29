/** @file parameter.c
 * Functions for retrieving simulation control parameters.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date March 2003
 *
 * Copyright &copy; University of Guelph, 2003-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "parameter.h"
#include <glib.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#if HAVE_STRING_H
#  include <string.h>
#endif

#if HAVE_ERRNO_H
#  include <errno.h>
#endif



/**
 * Retrieves a value for a real-valued parameter.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a real-valued parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the parameter value.  If the conversion did not succeed, this value
 *   is undefined.
 */
double
PAR_get_real (PAR_parameter_t * param, gboolean * success)
{
  scew_element *e;
  double x;
  const char *text;
  char *endptr;

  e = scew_element_by_name (param, "value");
  if (e)
    {
      text = scew_element_contents (e);
      x = strtod (text, &endptr);
      *success = !(text[0] == '\0' || errno == ERANGE || endptr == text);
    }
  else
    {
      g_warning ("parameter %s is missing a \"value\" element", scew_element_name (param));
      *success = FALSE;
    }
  return x;
}



/**
 * Retrieves a length value.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a length parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the length.  If the conversion did not succeed, this value is
 *   undefined.
 */
double
PAR_get_length (PAR_parameter_t * param, gboolean * success)
{
  double x;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_length");
#endif

  x = PAR_get_real (param, success);
  if (*success == FALSE)
    g_warning ("missing or invalid length parameter");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_length");
#endif

  return x;
}



/**
 * Retrieves a time value.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a time parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the time.  If the conversion did not succeed, this value is
 *   undefined.
 */
double
PAR_get_time (PAR_parameter_t * param, gboolean * success)
{
  double x;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_time");
#endif

  x = PAR_get_real (param, success);
  if (*success == FALSE)
    g_warning ("missing or invalid time parameter");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_time");
#endif

  return x;
}



/**
 * Retrieves an angle value.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param an angle parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the angle, in radians.  If the conversion did not succeed, this
 *   value is undefined.
 */
double
PAR_get_angle (PAR_parameter_t * param, gboolean * success)
{
  double x;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_angle");
#endif

  x = PAR_get_real (param, success);
  if (*success == FALSE)
    g_warning ("missing or invalid angle parameter");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_angle");
#endif

  return x;
}



/**
 * Retrieves a frequency value.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a frequency parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the frequency.  If the conversion did not succeed, this value is
 *   undefined.
 */
double
PAR_get_frequency (PAR_parameter_t * param, gboolean * success)
{
  double x;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_frequency");
#endif

  x = PAR_get_real (param, success);
  if (*success == FALSE)
    g_warning ("missing or invalid frequency parameter");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_frequency");
#endif

  return x;
}



/**
 * Retrieves a probability value.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a probability parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the probability in [0,1].  If the conversion did not succeed, this
 *   value is undefined.
 */
double
PAR_get_probability (PAR_parameter_t * param, gboolean * success)
{
  double x;
  const char *text;
  char *endptr;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_probability");
#endif

  text = scew_element_contents (param);
  x = strtod (text, &endptr);
  *success = !(text[0] == '\0' || errno == ERANGE || endptr == text || x < 0 || x > 1);
  if (*success == FALSE)
    g_warning ("missing or invalid probability parameter");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_probability");
#endif

  return x;
}



/**
 * Retrieves a money or cost value.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a money parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the amount.  If the conversion did not succeed, this value is
 *   undefined.
 */
double
PAR_get_money (PAR_parameter_t * param, gboolean * success)
{
  double x;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_money");
#endif

  x = PAR_get_real (param, success);
  if (*success == FALSE)
    g_warning ("missing or invalid money parameter");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_money");
#endif

  return x;
}



/**
 * Retrieves a probability distribution function.
 *
 * @param param a probability distribution function parameter.
 * @return a probability distribution function object.
 */
PDF_dist_t *
PAR_get_PDF (PAR_parameter_t * param)
{
  PDF_dist_t *dist;
  scew_element *e;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_PDF");
#endif

  /* Find out what kind of distribution it is. */
  e = scew_element_by_name (param, "point");
  if (e)
    {
      double value;

      errno = 0;
      value = strtod (scew_element_contents (e), NULL);
      if (errno == ERANGE)
        g_error ("point distribution parameter \"%s\" is not a number", scew_element_contents (e));
      dist = PDF_new_point_dist (value);
      goto end;
    }
  e = scew_element_by_name (param, "uniform");
  if (e)
    {
      double a, b;

      errno = 0;
      a = strtod (scew_element_contents (scew_element_by_name (e, "a")), NULL);
      g_assert (errno != ERANGE);
      b = strtod (scew_element_contents (scew_element_by_name (e, "b")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_uniform_dist (a, b);
      goto end;
    }
  e = scew_element_by_name (param, "triangular");
  if (e)
    {
      double a, c, b;

      errno = 0;
      a = strtod (scew_element_contents (scew_element_by_name (e, "a")), NULL);
      g_assert (errno != ERANGE);
      c = strtod (scew_element_contents (scew_element_by_name (e, "c")), NULL);
      g_assert (errno != ERANGE);
      b = strtod (scew_element_contents (scew_element_by_name (e, "b")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_triangular_dist (a, c, b);
      goto end;
    }
  e = scew_element_by_name (param, "gaussian");
  if (e)
    {
      double mean, stddev;

      errno = 0;
      mean = strtod (scew_element_contents (scew_element_by_name (e, "mean")), NULL);
      g_assert (errno != ERANGE);
      stddev = strtod (scew_element_contents (scew_element_by_name (e, "stddev")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_gaussian_dist (mean, stddev);
      goto end;
    }
  e = scew_element_by_name (param, "poisson");
  if (e)
    {
      double mean;

      errno = 0;
      mean = strtod (scew_element_contents (scew_element_by_name (e, "mean")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_poisson_dist (mean);
      goto end;
    }
  e = scew_element_by_name (param, "beta");
  if (e)
    {
      double alpha, beta, location, scale;

      errno = 0;
      alpha = strtod (scew_element_contents (scew_element_by_name (e, "alpha")), NULL);
      g_assert (errno != ERANGE);
      beta = strtod (scew_element_contents (scew_element_by_name (e, "beta")), NULL);
      g_assert (errno != ERANGE);
      location = strtod (scew_element_contents (scew_element_by_name (e, "location")), NULL);
      g_assert (errno != ERANGE);
      scale = strtod (scew_element_contents (scew_element_by_name (e, "scale")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_beta_dist (alpha, beta, location, scale);
      goto end;
    }
  e = scew_element_by_name (param, "beta-pert");
  if (e)
    {
      double min, mode, max;

      errno = 0;
      min = strtod (scew_element_contents (scew_element_by_name (e, "min")), NULL);
      g_assert (errno != ERANGE);
      mode = strtod (scew_element_contents (scew_element_by_name (e, "mode")), NULL);
      g_assert (errno != ERANGE);
      max = strtod (scew_element_contents (scew_element_by_name (e, "max")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_beta_pert_dist (min, mode, max);
      goto end;
    }
  e = scew_element_by_name (param, "gamma");
  if (e)
    {
      double alpha, beta;

      errno = 0;
      alpha = strtod (scew_element_contents (scew_element_by_name (e, "alpha")), NULL);
      g_assert (errno != ERANGE);
      beta = strtod (scew_element_contents (scew_element_by_name (e, "beta")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_gamma_dist (alpha, beta);
      goto end;
    }
  e = scew_element_by_name (param, "weibull");
  if (e)
    {
      double alpha, beta;

      errno = 0;
      alpha = strtod (scew_element_contents (scew_element_by_name (e, "alpha")), NULL);
      g_assert (errno != ERANGE);
      beta = strtod (scew_element_contents (scew_element_by_name (e, "beta")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_weibull_dist (alpha, beta);
      goto end;
    }
  e = scew_element_by_name (param, "exponential");
  if (e)
    {
      double mean;

      errno = 0;
      mean = strtod (scew_element_contents (scew_element_by_name (e, "mean")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_exponential_dist (mean);
      goto end;
    }
  e = scew_element_by_name (param, "pearson5");
  if (e)
    {
      double alpha, beta;

      errno = 0;
      alpha = strtod (scew_element_contents (scew_element_by_name (e, "alpha")), NULL);
      g_assert (errno != ERANGE);
      beta = strtod (scew_element_contents (scew_element_by_name (e, "beta")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_pearson5_dist (alpha, beta);
      goto end;
    }
  e = scew_element_by_name (param, "logistic");
  if (e)
    {
      double location, scale;

      errno = 0;
      location = strtod (scew_element_contents (scew_element_by_name (e, "location")), NULL);
      g_assert (errno != ERANGE);
      scale = strtod (scew_element_contents (scew_element_by_name (e, "scale")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_logistic_dist (location, scale);
      goto end;
    }
  e = scew_element_by_name (param, "loglogistic");
  if (e)
    {
      double location, scale, shape;

      errno = 0;
      location = strtod (scew_element_contents (scew_element_by_name (e, "location")), NULL);
      g_assert (errno != ERANGE);
      scale = strtod (scew_element_contents (scew_element_by_name (e, "scale")), NULL);
      g_assert (errno != ERANGE);
      shape = strtod (scew_element_contents (scew_element_by_name (e, "shape")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_loglogistic_dist (location, scale, shape);
      goto end;
    }
  e = scew_element_by_name (param, "lognormal");
  if (e)
    {
      double zeta, sigma;

      errno = 0;
      zeta = strtod (scew_element_contents (scew_element_by_name (e, "zeta")), NULL);
      g_assert (errno != ERANGE);
      sigma = strtod (scew_element_contents (scew_element_by_name (e, "sigma")), NULL);
      g_assert (errno != ERANGE);
      dist = PDF_new_lognormal_dist (zeta, sigma);
      goto end;
    }
  e = scew_element_by_name (param, "piecewise");
  if (e)
    {
      scew_element **ee;
      unsigned int npoints;
      unsigned int i;
      double x, y, *xy;

      ee = scew_element_list (e, "value", &npoints);
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%u points", npoints);
#endif

      /* Copy the x,y values from the DOM tree into an array. */
      xy = g_new (double, 2 * npoints);

      errno = 0;
      for (i = 0; i < npoints; i++)
        {
          x = strtod (scew_element_contents (ee[i]), NULL);
          g_assert (errno != ERANGE);
          xy[2 * i] = x;
        }
      free (ee);
      ee = scew_element_list (e, "p", &npoints);
      for (i = 0; i < npoints; i++)
        {
          y = strtod (scew_element_contents (ee[i]), NULL);
          g_assert (errno != ERANGE);
          xy[2 * i + 1] = y;
        }
      dist = PDF_new_piecewise_dist (npoints, xy);
      g_free (xy);
      free (ee);
      goto end;
    }

  g_assert_not_reached ();

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_PDF");
#endif

  return dist;
}



/**
 * Retrieves a relationship chart.
 *
 * @param param a relationship chart parameter.
 * @return a relationship chart object.
 */
REL_chart_t *
PAR_get_relationship_chart (PAR_parameter_t * param)
{
  REL_chart_t *chart;
  scew_element **ee;
  unsigned int npoints;
  double *x, *y;
  unsigned int i;               /* loop counter */
  unsigned int index;
  double value;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_relationship_chart");
#endif

  ee = scew_element_list (param, "value", &npoints);
  npoints /= 2;
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%u points", npoints);
#endif

  /* Copy the x,y values from the DOM tree into arrays. */
  x = g_new (double, npoints);
  y = g_new (double, npoints);

  index = 0;
  for (i = 0; i < npoints; i++)
    {
      errno = 0;
      value = strtod (scew_element_contents (ee[index++]), NULL);
      g_assert (errno != ERANGE);
      x[i] = value;
      value = strtod (scew_element_contents (ee[index++]), NULL);
      g_assert (errno != ERANGE);
      y[i] = value;
    }
  chart = REL_new_chart (x, y, npoints);
  g_free (y);
  g_free (x);
  free (ee);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_relationship_chart");
#endif

  return chart;
}



/**
 * Retrieves a boolean.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a boolean parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the value.  If the conversion did not succeed, this value is
 *   undefined.
 */
gboolean
PAR_get_boolean (PAR_parameter_t * param, gboolean * success)
{
  gboolean x;
  XML_Char const *element_text;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_boolean");
#endif

  element_text = scew_element_contents (param);
  *success = TRUE;
  if (strcmp (element_text, "true") == 0 || strcmp (element_text, "1") == 0)
    x = TRUE;
  else if (strcmp (element_text, "false") == 0 || strcmp (element_text, "0") == 0)
    x = FALSE;
  else
    {
      *success = FALSE;
      g_warning ("missing or invalid boolean parameter");
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_boolean");
#endif

  return x;
}



/**
 * Retrieves a generic unitless value.
 *
 * Side effect: upon return, the location indicated by <i>success</i> contains
 * FALSE if the parameter was missing, out of range, or otherwise invalid, and
 * TRUE otherwise.
 *
 * @param param a numeric parameter.
 * @param success a location in which to store a success or failure flag.
 * @return the value.  If the conversion did not succeed, this value is
 *   undefined.
 */
double
PAR_get_unitless (PAR_parameter_t * param, gboolean * success)
{
  double x;
  const char *text;
  char *endptr;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_unitless");
#endif

  text = scew_element_contents (param);
  x = strtod (text, &endptr);
  *success = !(text[0] == '\0' || errno == ERANGE || endptr == text);
  if (*success == FALSE)
    g_warning ("missing or invalid unitless parameter");

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_unitless");
#endif

  return x;
}



/**
 * Retrieves a text value.  The text is copied, so the original PAR_parameter_t
 * object may be freed after calling this function.
 *
 * @param param a text parameter.
 * @return the text.
 */
char *
PAR_get_text (PAR_parameter_t * param)
{
  const char *element_text;
  char *text;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER PAR_get_text");
#endif

  element_text = scew_element_contents (param);
  text = g_strdup (element_text);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT PAR_get_text");
#endif

  return text;
}

/* end of file parameter.c */
