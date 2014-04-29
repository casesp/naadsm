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

/** @file filters/apparent_events_table.c
 * A filter that turns SHARCSpread output into a table of detections,
 * vaccinations and destructions.
 *
 * Call it as
 *
 * <code>apparent_events_table_filter HERD-FILE < LOG-FILE</code>
 *
 * The apparent events table is written to standard output in comma-separated
 * values format.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date January 2005
 *
 * Copyright &copy; University of Guelph, 2005-2007
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
GPtrArray *reasons; /**< Reasons for detection, vaccination and destruction.
  Each item in the list is a unique string.  Events stored in the array
  output_values reference the strings in this list. */
GPtrArray *detections; /**< Each item in the list is GArray of run-day-event
  triples for one node. */
GPtrArray *vaccinations; /**< Each item in the list is GArray of run-day-event
  triples for one node. */
GPtrArray *destructions; /**< Each item in the list is GArray of run-day-event
  triples for one node. */
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
  unsigned int herd_index;

  run_day_event_triple_t tmp;
  unsigned int i;

  reporting = (RPT_reporting_t *) data;

  /* The reporting variable is one level deep; the categories are the reasons
   * for detection/vaccination/destruction. */

  /* If this is a "detections", "vaccinations", or "destructions" variable,
   * recurse into its sub-categories. */
  if (user_data == NULL) /* will be false when in a sub-category */
    {
      if (strcmp (reporting->name, "detections") == 0
	  || strcmp (reporting->name, "vaccinations") == 0
	  || strcmp (reporting->name, "destructions") == 0)
	{
#if DEBUG
	  s = RPT_reporting_value_to_string (reporting, NULL);
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "found relevant variable, value = %s", s);
	  free (s);
#endif
          if (reporting->type == RPT_group)
	    {
	      g_datalist_foreach ((GData **) (&reporting->data), add_values, reporting->name);
	      goto end;
	    }
	}
      else
        {
	  goto end;
        }
    }

  /* If this is a sub-category of a "detections", "vaccinations", or
   * "destructions" variable, we want to add the individual units to the list.
   * We get the individual units from the variable's value, which is a comma-
   * separated list of unit indices. */
  s = RPT_reporting_get_text (reporting, NULL);
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "s = \"%s\"", s);
#endif
  if (strlen (s) == 0)
    goto end;

  if (strcmp (user_data ? (char *)user_data : reporting->name, "detections") == 0)
    values = (GArray *) g_ptr_array_index (detections, current_node);
  else if (strcmp (user_data ? (char *)user_data : reporting->name, "vaccinations") == 0)
    values = (GArray *) g_ptr_array_index (vaccinations, current_node);
  else if (strcmp (user_data ? (char *)user_data : reporting->name, "destructions") == 0)
    values = (GArray *) g_ptr_array_index (destructions, current_node);

  /* Set the run number, day number, and reason, which will be the same for all
   * of the events.  Note that we store data in a destruction event structure
   * regardless of what type of event it really is. */
  tmp.run = g_array_index (current_run, unsigned int, current_node);
  tmp.day = g_array_index (current_day, unsigned int, current_node);
  for (i = 0; i < reasons->len; i++)
    if (strcmp (reporting->name, (char *) g_ptr_array_index (reasons, i)) == 0)
      break;
  if (i < reasons->len)
    tmp.event.u.destruction.reason = g_ptr_array_index (reasons, i);
  else
    {
      /* This is a reason we haven't encountered before, add it to the list. */
      tmp.event.u.destruction.reason = g_strdup (reporting->name);
      g_ptr_array_add (reasons, tmp.event.u.destruction.reason);
    }

  /* Split the text on commas. */
  tokens = g_strsplit (s, ",", 0);
  for (iter = tokens; *iter != NULL; iter++)
    {
      /* This chunk of text will be a herd index. */
      herd_index = strtol (*iter, NULL, 10);
      tmp.event.u.destruction.herd = HRD_herd_list_get (herds, herd_index);
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
  unsigned int i, j, k;
  GArray *detection_events, *vaccination_events, *destruction_events;
  run_day_event_triple_t *tmp;
  unsigned int day;
  gboolean done;
  EVT_destruction_event_t *event;
  static unsigned int run = 1; /* A run identifier than increases with each
    call to this function.  This is distinct from the per-node run numbers
    stored in the run-day-event triples. */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "printing stored events for node %u", node);
#endif

  detection_events = (GArray *) g_ptr_array_index (detections, node);
  vaccination_events = (GArray *) g_ptr_array_index (vaccinations, node);
  destruction_events = (GArray *) g_ptr_array_index (destructions, node);
  
  i = j = k = 0;
  done = FALSE;
  for (day = 1; !done; day++)
    {
      /* Print all detections that happened on this day. */
      while (1)
        {
	  if (i >= detection_events->len)
	    break;
	  tmp = &g_array_index (detection_events, run_day_event_triple_t, i);
	  if (tmp->day != day)
	    break;
	  event = &(tmp->event.u.destruction);
	  printf ("%u,%u,Detection,,%s,%s,%u,%g,%g\n",
		  run, tmp->day,
		  event->herd->official_id,
		  event->herd->production_type_name,
		  event->herd->size,
		  event->herd->lat,
		  event->herd->lon);
	  i++;
	}

      /* Print all vaccinations that happened on this day. */
      while (1)
        {
	  if (j >= vaccination_events->len)
	    break;
	  tmp = &g_array_index (vaccination_events, run_day_event_triple_t, j);
	  if (tmp->day != day)
	    break;
	  event = &(tmp->event.u.destruction);
	  printf ("%u,%u,Vaccination,%s,%s,%s,%u,%g,%g\n",
		  run, tmp->day, event->reason,
		  event->herd->official_id,
		  event->herd->production_type_name,
		  event->herd->size,
		  event->herd->lat,
		  event->herd->lon);
	  j++;
	}

      /* Print all destructions that happened on this day. */
      while (1)
        {
	  if (k >= destruction_events->len)
	    break;
	  tmp = &g_array_index (destruction_events, run_day_event_triple_t, k);
	  if (tmp->day != day)
	    break;
	  event = &(tmp->event.u.destruction);
	  printf ("%u,%u,Destruction,%s,%s,%s,%u,%g,%g\n",
		  run, tmp->day, event->reason,
		  event->herd->official_id,
		  event->herd->production_type_name,
		  event->herd->size,
		  event->herd->lat,
		  event->herd->lon);
	  k++;
	}

      done = (i >= detection_events->len
	      && j >= vaccination_events->len
	      && k >= destruction_events->len);
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
  g_array_set_size ((GArray *) g_ptr_array_index (detections, node), 0);
  g_array_set_size ((GArray *) g_ptr_array_index (vaccinations, node), 0);
  g_array_set_size ((GArray *) g_ptr_array_index (destructions, node), 0);
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

	  g_ptr_array_set_size (detections, node + 1);
	  /* Initialize the new entry to an empty GArray. */
	  new_list = g_array_new (FALSE, FALSE, sizeof (run_day_event_triple_t));
	  g_ptr_array_index (detections, node) = new_list;

	  g_ptr_array_set_size (vaccinations, node + 1);
	  /* Initialize the new entry to an empty GArray. */
	  new_list = g_array_new (FALSE, FALSE, sizeof (run_day_event_triple_t));
	  g_ptr_array_index (vaccinations, node) = new_list;

	  g_ptr_array_set_size (destructions, node + 1);
	  /* Initialize the new entry to an empty GArray. */
	  new_list = g_array_new (FALSE, FALSE, sizeof (run_day_event_triple_t));
	  g_ptr_array_index (destructions, node) = new_list;
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
	     "node %u now on run %u, day %u",
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
	   * destructions={'reported diseased':'1','trace out':'2'}
	   *
	   * What we want for the summary table is something like:
	   *
	   * run 1,day 1,reported diseased,1
	   * run 1,day 1,trace out,2
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
  struct poptOption options[2];

  /* Get the command-line options and arguments.  There should be one command-
   * line argument, the name of the herd file. */
  options[0].longName = "verbosity";
  options[0].shortName = 'V';
  options[0].argInfo = POPT_ARG_INT;
  options[0].arg = &verbosity;
  options[0].val = 0;
  options[0].descrip = "Message verbosity level (0 = simulation output only, 1 = + informational messages, 2 = + all debugging output)";
  options[0].argDescrip = "verbosity";

  options[1].longName = NULL;
  options[1].shortName = '\0';
  options[1].argInfo = 0;
  options[1].arg = NULL;
  options[1].val = 0;
  options[1].descrip = NULL;
  options[1].argDescrip = NULL;

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
  printf ("Run,Day,Type,Reason,ID,Production type,Size,Lat,Lon\n");

  reasons = g_ptr_array_new ();
  detections = g_ptr_array_new ();
  vaccinations = g_ptr_array_new ();
  destructions = g_ptr_array_new ();
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
  g_ptr_array_foreach (reasons, free_as_GFunc, NULL);
  g_ptr_array_free (reasons, TRUE);
  g_array_free (current_run, TRUE);
  g_array_free (current_day, TRUE);
  g_ptr_array_free (detections, TRUE);
  g_ptr_array_free (vaccinations, TRUE);
  g_ptr_array_free (destructions, TRUE);

  return EXIT_SUCCESS;
}

/* end of file apparent_events_table.y */
