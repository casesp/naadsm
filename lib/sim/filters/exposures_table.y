%{
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <popt.h>
#include "herd.h"
#include "event.h"
#include "reporting.h"
#include <stdio.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_sort.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#if HAVE_STRING_H
#  include <string.h>
#endif

/* #define DEBUG 1 */

/** @file filters/exposures_table.c
 * A filter that turns SHARCSpread output into a table of exposures and
 * infections.
 *
 * Call it as
 *
 * <code>exposures_table_filter HERD-FILE < LOG-FILE</code>
 *
 * The exposures table is written to standard output in comma-separated values
 * format.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date August 2004
 *
 * Copyright &copy; University of Guelph, 2004-2007
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#define YYERROR_VERBOSE
#define BUFFERSIZE 2048

/* Prototype mysteriously not in <stdio.h> like the manpage says */
int snprintf (char *str, size_t size, const char *format, ...);

/* int yydebug = 1; must also compile with --debug to use this */
char errmsg[BUFFERSIZE];

typedef struct
{
  unsigned int run;
  unsigned int day;
  EVT_event_t event;
} run_day_event_triple_t;

HRD_herd_list_t *herds;
GPtrArray *causes; /**< Causes of exposure and infection.  Each item in the
  list is a unique string.  Events stored in the array output_values reference
  the strings in this list. */
GPtrArray *exposures; /**< Each item in the list is GArray of run-day-event
  triples for one node. */
GPtrArray *infections; /**< Each item in the list is GArray of run-day-event
  triples for one node. */
int infections_only;
unsigned int current_node;
GArray *current_run; /**< The most recent run number we have seen in the output from each node. */
GArray *current_day; /**< The most recent day we have seen in the output from each node. */



/**
 * Wraps GLib's g_array_free function so that it can be used as the
 * GDestroyNotify function in a Keyed Data List.
 */
void
g_array_free_as_GDestroyNotify (gpointer data)
{
  g_array_free ((GArray *) data, TRUE);
}



/**
 * Wraps free so that it can be used with a Pointer Array's foreach function.
 */
void
free_as_GFunc (gpointer data, gpointer user_data)
{
  free ((void *) data);
}




/**
 * Wraps GLib's g_string_free function so that it can be used with a Pointer
 * Array's foreach function.
 */
void
g_string_free_as_GFunc (gpointer data, gpointer user_data)
{
  g_string_free ((GString *) data, TRUE);
}



/**
 * Wraps g_ascii_strcasecmp so that it can be used as a GCompareFunc to
 * sort a Pointer Array of GStrings.
 */
gint
g_ascii_strcasecmp_as_GCompareFunc (gconstpointer a, gconstpointer b)
{
  char *s1, *s2;

  s1 = (*((GString **) a))->str;
  s2 = (*((GString **) b))->str;
  return g_ascii_strcasecmp (s1, s2);  
}



/**
 * Adds all the values stored in an RPT_reporting_t structure to the master
 * list of outputs.  This function is typed as a GDataForeachFunc so that
 * it can easily be called recursively on the sub-variables.
 *
 * @param key_id use 0.
 * @param data an output variable, cast to a gpointer.
 * @param user_data the output variable's name as seen so far.  Used when
 *   recursively drilling down into sub-variables.  Use NULL for the top-level
 *   call.
 */
void
add_values (GQuark key_id, gpointer data, gpointer user_data)
{
  RPT_reporting_t *reporting;
  GArray *values;
  char *s; /* For building temporary strings. */
  gchar **tokens, **iter;
  gchar **tokens2;
  unsigned int herd_index;

  run_day_event_triple_t tmp;
  unsigned int i;

  reporting = (RPT_reporting_t *) data;

  /* The reporting variable is one level deep; the categories are the causes
   * of infection. */

  /* If this is an "exposures" or "infections" variable, recurse into its
   * sub-categories. */
  if (user_data == NULL) /* will be false when in a sub-category */
    {
      if (strcmp (reporting->name, "infections") == 0
	  || (!infections_only && strcmp (reporting->name, "exposures") == 0))
	{
#if DEBUG
	  s = RPT_reporting_value_to_string (reporting, NULL);
	  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
		 "found %s variable, value = %s", reporting->name, s);
	  free (s);
#endif
	  g_datalist_foreach ((GData **) (&reporting->data), add_values, reporting->name);
	}
      goto end;
    }

  /* If this is a sub-category of an "exposures" or "infections" variable,
   * we want to add the individual events to the list.  We get the individual
   * events from the variable's value, which is a comma-separated list of
   * source->target pairs. */
  s = RPT_reporting_get_text (reporting, NULL);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "s = \"%s\"", s);
#endif
  if (strlen (s) == 0)
    goto end;

  if (strcmp ((char *)user_data, "infections") == 0)
    values = (GArray *) g_ptr_array_index (infections, current_node);
  else if (strcmp ((char *)user_data, "exposures") == 0)
    values = (GArray *) g_ptr_array_index (exposures, current_node);

  /* Set the run number, day number, and cause, which will be the same for all
   * of the events. */
  tmp.run = g_array_index (current_run, unsigned int, current_node);
  tmp.day = g_array_index (current_day, unsigned int, current_node);
  for (i = 0; i < causes->len; i++)
    if (strcmp (reporting->name, (char *) g_ptr_array_index (causes, i)) == 0)
      break;
  if (i < causes->len)
    tmp.event.u.exposure.cause = g_ptr_array_index (causes, i);
  else
    {
      /* This is a cause we haven't encountered before, add it to the list. */
      tmp.event.u.exposure.cause = g_strdup (reporting->name);
      g_ptr_array_add (causes, tmp.event.u.exposure.cause);
    }

  /* Split the text on commas. */
  tokens = g_strsplit (s, ",", 0);
  for (iter = tokens; *iter != NULL; iter++)
    {
      /* This chunk of text will be a source & target herd pair, separated by
       * a '->'.  Split the text at the '->'. */
      tokens2 = g_strsplit (*iter, "->", 0);
      /* If there is only one token, there was just a target herd (this happens
       * for herds that are infected before the simulation starts). */
      if (tokens2[1] == NULL)
	{
	  tmp.event.u.exposure.exposing_herd = NULL;
	  herd_index = strtol (tokens2[0], NULL, 10);
	  tmp.event.u.exposure.exposed_herd = HRD_herd_list_get (herds, herd_index);
	}
      else
	{
	  herd_index = strtol (tokens2[0], NULL, 10);
	  tmp.event.u.exposure.exposing_herd = HRD_herd_list_get (herds, herd_index);
	  herd_index = strtol (tokens2[1], NULL, 10);
	  tmp.event.u.exposure.exposed_herd = HRD_herd_list_get (herds, herd_index);
	}
      g_strfreev (tokens2);
      g_array_append_val (values, tmp);
    }
  g_strfreev (tokens);

end:
  return;
}



/**
 * Prints the stored events for one node.
 */
void
print_values (unsigned int node)
{
  unsigned int i, j;
  GArray *exposure_events, *infection_events;
  run_day_event_triple_t *tmp;
  unsigned int day;
  gboolean done;
  EVT_exposure_event_t *event;
  static unsigned int run = 1; /* A run identifier than increases with each
    call to this function.  This is distinct from the per-node run numbers
    stored in the run-day-event triples. */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
	 "printing stored events for node %u", node);
#endif

  exposure_events = (GArray *) g_ptr_array_index (exposures, node);
  infection_events = (GArray *) g_ptr_array_index (infections, node);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "# of recorded exposure events = %u", exposure_events->len);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
         "# of recorded infection events = %u", infection_events->len);
#endif  
  i = j = 0;
  done = FALSE;
  for (day = 1; !done; day++)
    {
      /* Print all exposures that happened on this day. */
      while (1)
        {
	  if (i >= exposure_events->len)
	    break;
	  tmp = &g_array_index (exposure_events, run_day_event_triple_t, i);
	  if (tmp->day != day)
	    break;
	  event = &(tmp->event.u.exposure);
	  printf ("%u,%u,Exposure,%s,%s,%s,%g,%g,%s,%s,%g,%g\n",
		  run, tmp->day, event->cause,
		  event->exposing_herd->official_id,
		  event->exposing_herd->production_type_name,
		  event->exposing_herd->lat,
		  event->exposing_herd->lon,
		  event->exposed_herd->official_id,
		  event->exposed_herd->production_type_name,
		  event->exposed_herd->lat,
		  event->exposed_herd->lon);
	  i++;
	}

      /* Print all infections that happened on this day. */
      while (1)
        {
	  if (j >= infection_events->len)
	    break;
	  tmp = &g_array_index (infection_events, run_day_event_triple_t, j);
	  if (tmp->day != day)
	    break;
	  event = &(tmp->event.u.exposure);
	  if (event->exposing_herd == NULL)
	    printf ("%u,%u,Infection,%s,,,,,%s,%s,%g,%g\n",
		    run, tmp->day, event->cause,
		    event->exposed_herd->official_id,
		    event->exposed_herd->production_type_name,
		    event->exposed_herd->lat,
		    event->exposed_herd->lon);
	  else
	    printf ("%u,%u,Infection,%s,%s,%s,%g,%g,%s,%s,%g,%g\n",
		    run, tmp->day, event->cause,
		    event->exposing_herd->official_id,
		    event->exposing_herd->production_type_name,
		    event->exposing_herd->lat,
		    event->exposing_herd->lon,
		    event->exposed_herd->official_id,
		    event->exposed_herd->production_type_name,
		    event->exposed_herd->lat,
		    event->exposed_herd->lon);
	  j++;
	}

      done = (i >= exposure_events->len && j >= infection_events->len);
    }

  run++;

  return;
}



/**
 * Clears the stored events for one node.
 */
void
clear_values (unsigned int node)
{
  g_array_set_size ((GArray *) g_ptr_array_index (exposures, node), 0);
  g_array_set_size ((GArray *) g_ptr_array_index (infections, node), 0);
}

%}

%union {
  int ival;
  float fval;
  char *sval;
  RPT_reporting_t *rval;
  GSList *lval;
}

%token NODE RUN DAY POLYGON
%token COMMA COLON EQ LBRACE RBRACE LPAREN RPAREN DQUOTE NEWLINE
%token <ival> INT
%token <fval> FLOAT
%token <sval> VARNAME STRING
%type <rval> value subvar
%type <lval> subvars
%%
output_lines :
    output_lines output_line
    { }
  | output_line
    { }
  ;

output_line:
    tracking_line NEWLINE data_line NEWLINE
    { }
  ;

tracking_line:
    NODE INT RUN INT
    {
      unsigned int node = $2;
      unsigned int run = $4;
      GArray *new_list;

      /* If we haven't seen output from this node before, we need to extend
       * the tracking lists for current run and current day, and create new
       * GArrays to hold events for the new node. */
      if ((node + 1) > current_run->len)
        {
	  g_array_set_size (current_run, node + 1);

	  g_array_set_size (current_day, node + 1);
	  g_array_index (current_day, unsigned int, node) = 1;

	  g_ptr_array_set_size (exposures, node + 1);
	  /* Initialize the new entry to an empty GArray. */
	  new_list = g_array_new (FALSE, FALSE, sizeof (run_day_event_triple_t));
	  g_ptr_array_index (exposures, node) = new_list;

	  g_ptr_array_set_size (infections, node + 1);
	  /* Initialize the new entry to an empty GArray. */
	  new_list = g_array_new (FALSE, FALSE, sizeof (run_day_event_triple_t));
	  g_ptr_array_index (infections, node) = new_list;
	}
      else
	{
	  /* Since output from a single node is sequential, when we see
	   * that the run number has changed, we know the output from one
	   * Monte Carlo trial is over.  So we print the values, clear the
	   * data for that node, and reset the day for that node. */
	  if (run != g_array_index (current_run, unsigned int, node))
            {
	      print_values (node);
	      clear_values (node);
	      g_array_index (current_run, unsigned int, node) = run;
	      g_array_index (current_day, unsigned int, node) = 1;
	    }
	  else
	    g_array_index (current_day, unsigned int, node) ++;
	}
      current_node = node;
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
	     "node %u now on run %u, day %u\n",
	     node, run, g_array_index (current_day, unsigned int, node));
#endif
    }
  ;

data_line:
    state_codes vars
    { }
  | state_codes
    { }
  | vars
    { }
  |
    { }
  ;

state_codes:
    state_codes INT
    { }
  | INT
    { }
  ;

vars:
    vars var
    { }
  | var
    { }
  ;       

var:
    VARNAME EQ value
    {
      if ($3 != NULL)
	{
	  $3->name = $1;

	  /* At this point we have built, in a bottom-up fashion, one complete
	   * output variable, stored in an RPT_reporting_t structure.  So we have a
	   * tree-style represention of
	   * 
	   * infections={'Direct Contact':'1->2','Airborne':'1->3'}
	   *
	   * What we want for the summary table is something like:
	   *
	   * day 1,Direct Contact,1,2
	   * day 1,Airborne,1,3
	   *
	   * So we call a function that drills down into the RPT_reporting_t
	   * structure and adds whatever sub-variables it finds, with their values,
	   * to the master list of outputs.
	   */
	  add_values (0, $3, NULL);
	  RPT_free_reporting ($3, TRUE);
	}
      else
        g_free ($1);
    }
  ;

value:
    INT
    {
      $$ = NULL;
    }
  | FLOAT
    {
      $$ = NULL;
    }
  | STRING
    {
      $$ = RPT_new_reporting (NULL, NULL, RPT_text, RPT_never, FALSE);
      RPT_reporting_set_text ($$, $1, NULL);
      /* The string token's value is set with a g_strndup, so we need to free
       * it after copying it into the RPT_reporting structure. */
      g_free ($1);
    }
  | POLYGON LPAREN RPAREN
    {
      $$ = NULL;
    }
  | POLYGON LPAREN contours RPAREN
    {
      $$ = NULL;
    }
  | LBRACE RBRACE
    {
      $$ = NULL;
    }
  | LBRACE subvars RBRACE
    {
      GSList *iter;

      $$ = RPT_new_reporting (NULL, NULL, RPT_group, RPT_never, FALSE);
      for (iter = $2; iter != NULL; iter = g_slist_next (iter))
	RPT_reporting_splice ($$, (RPT_reporting_t *) (iter->data));

      /* Now that the sub-variables have been attached to a newly-created group
       * reporting variable, free the linked list structure that contained
       * them. */
      g_slist_free ($2);
    }
  ;

subvars:
    subvars COMMA subvar
    {
      $$ = g_slist_append ($1, $3);
    }
  | subvar
    {
      /* Initialize a linked list of subvars. */
      $$ = g_slist_append (NULL, $1);
    }
  ;

subvar:
    STRING COLON value
    {
      $$ = $3;
      if ($$ != NULL)
        $$->name = $1;
      else
        g_free ($1);
    }
  ;

/* The parser contains rules for parsing polygons, because they may appear in
 * the output, but we do nothing with them. */

contours:
    contours contour
    { }
  | contour
    { }
  ;

contour:
    LPAREN coords RPAREN
    { }
  ;

coords:
    coords COMMA coord
    { }
  | coord
    { }
  ;

coord:
    FLOAT FLOAT
    { }
  | FLOAT INT
    { }
  | INT FLOAT
    { }
  | INT INT
    { }
  ;

%%
extern FILE *yyin;
extern int yylineno, tokenpos;
extern char linebuf[];

/* Simple yyerror from _lex & yacc_ by Levine, Mason & Brown. */
int
yyerror (char *s, int fatal)
{
  fprintf (stderr, "Error in output (line %d): %s:\n%s\n", yylineno, s, linebuf);
  fprintf (stderr, "%*s\n", 1+tokenpos, "^");
  if (fatal) exit (EXIT_FAILURE);
  return 0;
}



/**
 * A log handler that simply discards messages.
 */
void
silent_log_handler (const gchar * log_domain, GLogLevelFlags log_level,
		    const gchar * message, gpointer user_data)
{
  ;
}



int
main (int argc, char *argv[])
{
  poptContext option;
  const char *herd_file = NULL;
  int verbosity = 0;
  unsigned int nherds;
  unsigned int nnodes, i;
  struct poptOption options[3];

  /* Get the command-line options and arguments.  There should be one command-
   * line argument, the name of the herd file. */
  options[0].longName = "verbosity";
  options[0].shortName = 'V';
  options[0].argInfo = POPT_ARG_INT;
  options[0].arg = &verbosity;
  options[0].val = 0;
  options[0].descrip = "Message verbosity level (0 = simulation output only, 1 = + informational messages, 2 = + all debugging output)";
  options[0].argDescrip = "verbosity";

  options[1].longName = "infections-only";
  options[1].shortName = 'i';
  options[1].argInfo = POPT_ARG_NONE;
  options[1].arg = &infections_only;
  options[1].val = 0;
  options[1].descrip = "Use this flag to output only infections, not exposures";
  options[1].argDescrip = "infections-only";

  options[2].longName = NULL;
  options[2].shortName = '\0';
  options[2].argInfo = 0;
  options[2].arg =  NULL;
  options[2].val = 0;
  options[2].descrip = NULL;
  options[2].argDescrip = NULL;

  infections_only = 0;
  option = poptGetContext (NULL, argc, (const char **)argv, options, 0);
  poptGetNextOpt (option);
  herd_file = poptGetArg (option);
  poptFreeContext (option);

  if (herd_file == NULL)
    g_error ("Need the name of a herd file.");

  /* Set the verbosity level. */
  if (verbosity < 2)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
    }
  if (verbosity < 1)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "verbosity = %i", verbosity);
#endif

  herds = HRD_load_herd_list (herd_file);
  nherds = HRD_herd_list_length (herds);

#if INFO
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i units read", nherds);
#endif
  if (nherds == 0)
    g_error ("no units in file %s", herd_file);

  /* Print the table header. */
  printf ("Run,Day,Type,Reason,Source ID,Production type,Lat,Lon,Recipient ID,Production type,Lat,Lon\n");

  causes = g_ptr_array_new ();
  exposures = g_ptr_array_new ();
  infections = g_ptr_array_new ();
  current_run = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);
  current_day = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);
    
  yyin = stdin;
  while (!feof(yyin))
    yyparse();

  /* Some output values may have been printed during the parse.  Print out any
   * remaining values. */
  nnodes = current_run->len;
  for (i = 0; i < nnodes; i++)
    {
      print_values (i);
      clear_values (i);
    }

  /* Clean up. */
  HRD_free_herd_list (herds);
  g_ptr_array_foreach (causes, free_as_GFunc, NULL);
  g_ptr_array_free (causes, TRUE);
  g_array_free (current_run, TRUE);
  g_array_free (current_day, TRUE);
  g_ptr_array_free (exposures, TRUE);
  g_ptr_array_free (infections, TRUE);

  return EXIT_SUCCESS;
}

/* end of file exposures_table.y */
