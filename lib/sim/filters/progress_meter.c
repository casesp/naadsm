/** @file progress_meter.c
 * This program reads simulation log file and estimates how much of the
 * simulation is complete.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date July 2004
 *
 * Copyright &copy; University of Guelph, 2004-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <expat.h>
/* Expat 1.95 has this constant on my Debian system, but not on Hammerhead's
 * Red Hat system.  ?? */
#ifndef XML_STATUS_ERROR
#  define XML_STATUS_ERROR 0
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <regex.h>
#include <glib.h>
#include <popt.h>

#if STDC_HEADERS
#  include <string.h>
#elif HAVE_STRINGS_H
#  include <strings.h>
#endif

#if HAVE_ERRNO_H
#  include <errno.h>
#endif



/**
 * A log handler that simply discards messages.  "Info" and "debug" level
 * messages are directed to this at low verbosity levels.
 */
void
silent_log_handler (const gchar * log_domain, GLogLevelFlags log_level,
                    const gchar * message, gpointer user_data)
{
  ;
}



/**
 * A special structure for passing partially completed information to Expat's
 * tag handler functions.
 */
typedef struct
{
  unsigned int num_days, num_runs;
  GString *s; /**< for gathering character data */
  char *filename; /**< for reporting the XML file's name in errors */
  XML_Parser parser; /**< for reporting the line number in errors */
}
partial_info_t;



/**
 * Character data handler for an Expat parser.  Accumulates the complete text
 * for an XML element (which may come in pieces).
 *
 * @param userData a pointer to a partial_info_t structure, cast to a void
 *   pointer.
 * @param s complete or partial character data from an XML element.
 * @param len the length of the character data.
 */
static void
charData (void *userData, const XML_Char * s, int len)
{
  partial_info_t *partial;

  partial = (partial_info_t *) userData;
  g_string_append_len (partial->s, s, len);
}



/**
 * Start element handler for an Expat parser.  Does nothing.
 *
 * @param userData a pointer to a partial_info_t structure, cast to a void
 *   pointer.
 * @param name the tag's name.
 * @param atts the tag's attributes.
 */
static void
startElement (void *userData, const char *name, const char **atts)
{
  return;
}



/**
 * End element handler for an Expat parser.
 *
 * When it encounters a \</num-days\> or \</num-runs\> tag, it records the
 * corresponding field.  It also clears the character data buffer after any end
 * tag.
 *
 * @param userData a pointer to a partial_info_t structure, cast to a void
 *   pointer.
 * @param name the tag's name.
 */
static void
endElement (void *userData, const char *name)
{
  partial_info_t *partial;
  char *filename;
  XML_Parser parser;
  char *tmp, *endptr;

  partial = (partial_info_t *) userData;
  filename = partial->filename;
  parser = partial->parser;

  /* num-days tag */

  if (strcmp (name, "num-days") == 0)
    {
      long int days;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);

      days = strtol (tmp, &endptr, 0);
      if (tmp[0] == '\0')
        {
          g_warning ("num-days missing on line %d of %s, setting to 0",
                     XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (errno == ERANGE || errno == EINVAL)
        {
          g_warning ("num-days is too large a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          days = 0;
          errno = 0;
        }
      else if (*endptr != '\0')
        {
          g_warning ("num-days is not a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      else if (days < 0)
        {
          g_warning ("num-days cannot be negative (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          days = 0;
        }
      partial->num_days = (unsigned int) days;
      g_free (tmp);
    }

  /* num-runs tag */

  else if (strcmp (name, "num-runs") == 0)
    {
      long int runs;

      tmp = g_strdup (partial->s->str);
      g_strstrip (tmp);

      runs = strtol (tmp, &endptr, 0);
      if (tmp[0] == '\0')
        {
          g_warning ("num-runs missing on line %d of %s, setting to 0",
                     XML_GetCurrentLineNumber (parser), filename);
          runs = 0;
        }
      else if (errno == ERANGE || errno == EINVAL)
        {
          g_warning ("num-runs is too large a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          runs = 0;
          errno = 0;
        }
      else if (*endptr != '\0')
        {
          g_warning ("num-runs is not a number (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          runs = 0;
        }
      else if (runs < 0)
        {
          g_warning ("num-runs cannot be negative (\"%s\") on line %d of %s, setting to 0",
                     tmp, XML_GetCurrentLineNumber (parser), filename);
          runs = 0;
        }
      partial->num_runs = (unsigned int) runs;
      g_free (tmp);
    }

  g_string_truncate (partial->s, 0);
}



/**
 * Returns the number of runs and the the maximum number of days in a
 * simulation from a parameter file.
 */
void
num_runs_days (const char *filename, unsigned int *num_runs, unsigned int *num_days)
{
  partial_info_t to_pass;
  XML_Parser parser;            /* to read the file */
  int xmlerr;
  FILE *fp;
  char *line = NULL;
  size_t bufsize = 0;
  ssize_t len;

  parser = XML_ParserCreate (NULL);
  if (parser == NULL)
    {
      g_warning ("failed to create parser for reading parameter file");
      goto end;
    }

  to_pass.num_days = 0;
  to_pass.num_runs = 0;
  to_pass.s = g_string_new (NULL);
  to_pass.filename = filename;
  to_pass.parser = parser;

  XML_SetUserData (parser, &to_pass);
  XML_SetElementHandler (parser, startElement, endElement);
  XML_SetCharacterDataHandler (parser, charData);

  fp = fopen (filename, "r");
  g_assert (fp != NULL);
  while (1)
    {
      len = getline (&line, &bufsize, fp);
      if (len == -1)
        {
          xmlerr = XML_Parse (parser, NULL, 0, 1);
          if (xmlerr == XML_STATUS_ERROR)
            g_error ("%s at line %d in %s",
                     XML_ErrorString (XML_GetErrorCode (parser)),
                     XML_GetCurrentLineNumber (parser), filename);
          break;
        }
      xmlerr = XML_Parse (parser, line, len, 0);
      if (xmlerr == XML_STATUS_ERROR)
        g_error ("%s at line %d in %s",
                 XML_ErrorString (XML_GetErrorCode (parser)),
                 XML_GetCurrentLineNumber (parser), filename);
    }
  fclose (fp);

  *num_runs = to_pass.num_runs;
  *num_days = to_pass.num_days;

  /* Clean up. */
  XML_ParserFree (parser);
  g_string_free (to_pass.s, TRUE);
  free (line);

end:
  return;
}



int
main (int argc, char *argv[])
{
  unsigned int runs_desired, max_days;
  regex_t linepat;              /* A regular expression to match node/run info lines. */
  regmatch_t match[3];
  int regex_error_code;
  char *regex_error;
  char *line = NULL;            /* An input buffer. */
  size_t bufsize = 0;
  ssize_t len;
  GArray *current_run;          /* The current run number for each node. */
  GArray *current_day;          /* The current day for each node. */
  GArray *days;                 /* The number of days each simulation lasted. */
  char *endptr;
  unsigned int node, run, day;
  int diff;
  double runs_completed;
  int i;                        /* loop counter */
  double mean_days;

  num_runs_days (argv[1], &runs_desired, &max_days);
#if DEBUG
  printf ("runs_desired = %u\n", runs_desired);
  printf ("max_days = %u\n", max_days);
#endif

  /* Create a regular expression for matching out the lines we're interested
   * in. */
  regex_error_code = regcomp (&linepat,
                              "^node ([[:digit:]]+) run ([[:digit:]]+)", REG_EXTENDED | REG_ICASE);
  if (regex_error_code)
    {
      len = regerror (regex_error_code, &linepat, NULL, 0);
      regex_error = g_new (char, len);
      regerror (regex_error_code, &linepat, regex_error, len);
      g_error ("linepat did not compile: %s", regex_error);
    }

  current_run = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);
  current_day = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);
  days = g_array_new (FALSE, TRUE, sizeof (unsigned int));

  while (1)
    {
      len = getline (&line, &bufsize, stdin);
      if (len == -1)
        break;

      if (regexec (&linepat, line, 3, match, 0) != 0)
        continue;

      endptr = line + match[1].rm_eo;
      node = (unsigned int) strtoul (line + match[1].rm_so, &endptr, 10);

      /* If this is a node we haven't seen before, expand the tracking lists to
       * include it. */
      diff = node + 1 - current_run->len;
      if (diff > 0)
        g_array_set_size (current_run, node + 1);
      diff = node + 1 - current_day->len;
      if (diff > 0)
        g_array_set_size (current_day, node + 1);

      endptr = line + match[2].rm_eo;
      run = (unsigned int) strtoul (line + match[2].rm_so, &endptr, 10);

      /* If this is a different run number than the last one we saw for this
       * node, we have switched to a new simulation on this node. */
      if (run > g_array_index (current_run, unsigned int, node))
        {
          day = g_array_index (current_day, unsigned int, node);
          g_array_append_val (days, day);
          g_array_index (current_run, unsigned int, node) = run;
          g_array_index (current_day, unsigned int, node) = 1;
        }
      else
        {
          g_array_index (current_day, unsigned int, node)++;
        }
    }

  /* Now we are finished reading the file. */
#if DEBUG
  printf ("current_run = [");
  for (i = 0; i < current_run->len; i++)
    printf (i == 0 ? "%u" : ",%u", g_array_index (current_run, unsigned int, i));
  printf ("]\n");
  printf ("current_day = [");
  for (i = 0; i < current_day->len; i++)
    printf (i == 0 ? "%u" : ",%u", g_array_index (current_day, unsigned int, i));
  printf ("]\n");
  printf ("days = [");
  for (i = 0; i < days->len; i++)
    printf (i == 0 ? "%u" : ",%u", g_array_index (days, unsigned int, i));
  printf ("]\n");
#endif

  /* The current run number will be 0 when a node is working on its first
   * simulation, 1 when it's working on its second, etc.  So the number of runs
   * runs completed is simply the sum of the current run number for all
   * nodes. */
  runs_completed = 0;
  for (i = 0; i < current_run->len; i++)
    runs_completed += g_array_index (current_run, unsigned int, i);
  /* Also add how far each node is in its current run.  To estimate this, use
   * the average number of days past simulations have run.  If no simulations
   * have completed, use the maximum number of days a simulation is allowed to
   * run. */
  if (days->len == 0)
    mean_days = max_days;
  else
    {
      mean_days = max_days;     /* meanDays = float (sum (days)) / len (days) */
    }
  for (i = 0; i < current_day->len; i++)
    runs_completed += MIN (1, g_array_index (current_day, unsigned int, i) / mean_days);

  printf ("simulations completed = %g of %u (%.1f%%)\n",
          runs_completed, runs_desired, 100.0 * runs_completed / runs_desired);
#if DEBUG
  printf ("average number of days per simulation = ");
  if (days->len == 0)
    printf ("N/A\n");
  else
    printf ("%.0f\n", mean_days);
#endif

  /* Clean up. */
  regfree (&linepat);
  g_array_free (current_run, TRUE);
  g_array_free (current_day, TRUE);
  g_array_free (days, TRUE);

  return EXIT_SUCCESS;
}

/* end of file progress_meter.c */
