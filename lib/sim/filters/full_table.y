%{
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <popt.h>
#include "reporting.h"
#include <stdio.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_sort.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#include <assert.h>

/** @file filters/full_table.c
 * A filter that turns SHARCSpread output into a table.
 *
 * Call it as
 *
 * <code>table_filter LOG-FILE</code>
 *
 * The table is written to standard output in comma-separated values format.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date May 2004
 *
 * Copyright &copy; University of Guelph, 2004-2007
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

/** @page filters Output filters
 * The chart below shows the workflow for generating output on the
 * supercomputer.  Input files are orange.  Output files are blue for tables,
 * green for plots and images, and purple for Arcview GIS files.  Temporary
 * intermediate files are grey.
 *
 * @image html filters_flowchart.png
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
  double value;
  GString *svalue;
} run_day_value_triple_t;

GPtrArray *output_names; /**< Names of output variables.  The names are
  GStrings. */
GPtrArray *output_values; /**< The output variable values.  Each item in the
  list is for one node.  The items are Keyed Data Lists that associate a
  variable name with a GArray of run-day-value triples. */
unsigned int current_node;
GArray *current_run; /**< The most recent run number we have seen in the output from each node. */
GArray *current_day; /**< The most recent day we have seen in the output from each node. */
int pass;



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
 * Adds a pointer to the pointer array, but only if <i>data</i> does not
 * already exist in <i>array</i>.  The new data is compared to existing entries
 * using <i>compare_func</i> which should be a qsort()-style comparison
 * function (returns -1 for first arg is less than second arg, 0 for equal, 1
 * if first arg is greater than second arg).  The array will grow in size
 * automatically if necessary.
 *
 * This function mimics g_ptr_array_add from the GLib library, except it
 * inserts the new data only if it doesn't already exist.
 *
 * @param array a GPtrArray.
 * @param compare_func a comparison function.
 * @param data the pointer to add.
 */
void
g_ptr_array_add_unique (GPtrArray *array, gpointer data,
                        GCompareFunc compare_func)
{
  unsigned int i, n;
  gboolean done, found;

  /* The list is assumed to be unsorted, so a linear search is needed.  Scan
   * backwards from the end of the list.  The rationale for this is that if
   * similar data appear together, you will probably find the match to the new
   * data close to the end of the list. */
  found = FALSE;
  n = array->len;
  for (i = n-1, done = (n == 0); !done; i--)
    {
      done = (i == 0);
      if (compare_func (&data, &g_ptr_array_index (array, i)) == 0)
        found = done = TRUE;
    }

  if (!found)
    g_ptr_array_add (array, data);
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
  GString *name_so_far, *name;
  GArray *values;
  GData **node_output_values;
  gboolean new_name = FALSE;
  run_day_value_triple_t tmp;
  unsigned int nnames;
#if DEBUG
  GString *s;
  unsigned int i;
#endif

  reporting = (RPT_reporting_t *) data;
  name_so_far = (GString *) user_data;  

  /* Find the variable's name. */
  if (name_so_far == NULL)
    name = g_string_new (reporting->name);
  else
    {
      name = g_string_new (name_so_far->str);
      g_string_append_printf (name, ":%s", reporting->name);
    }

  if (reporting->type == RPT_group)
    {
      g_datalist_foreach ((GData **) (&reporting->data), add_values, name);
      goto end;
    }

  switch (pass)
    {
    case 1:
      /* In the first pass, we're just gathering output variable names.  So if
       * the list doesn't already contain this variable, add it. */
      nnames = output_names->len;
      g_ptr_array_add_unique (output_names, name, g_ascii_strcasecmp_as_GCompareFunc);
      if (output_names->len > nnames)
	new_name = TRUE;
      break;

    case 2:
      /* In the second pass, we're gathering output variable values. */
      tmp.run = g_array_index (current_run, unsigned int, current_node);
      tmp.day = g_array_index (current_day, unsigned int, current_node);
      if (reporting->type == RPT_integer)
	{
	  tmp.value = (double)(*((long *)(reporting->data)));
	  tmp.svalue = NULL;
	}
      else if (reporting->type == RPT_real)
	{
	  tmp.value = *((double *)(reporting->data));
	  tmp.svalue = NULL;
	}
      else if (reporting->type == RPT_text)
	{
	  tmp.value = 0;
	  tmp.svalue = g_string_new (((GString *)reporting->data)->str);
	}

      node_output_values = (GData **)(&g_ptr_array_index (output_values, current_node));
      values = (GArray *) (g_datalist_get_data (node_output_values, name->str));

      /* If no values have been recorded for this node and variable before,
       * create a new list. */
      if (values == NULL)
	{
	  values = g_array_new (FALSE, FALSE, sizeof (run_day_value_triple_t));
	  g_datalist_set_data_full (node_output_values, name->str, values,
				    g_array_free_as_GDestroyNotify);
	  g_array_append_val (values, tmp);
	}	  
      /* If values have been recorded for this node and variable, check the
       * most recent one.  If it is for the current day, add to it; otherwise,
       * record the value as new. */
      else
        {
	  if (g_array_index (values, run_day_value_triple_t, values->len - 1).run == tmp.run
	      && g_array_index (values, run_day_value_triple_t, values->len - 1).day == tmp.day)
	    {
	      if (tmp.svalue == NULL)
		g_array_index (values, run_day_value_triple_t, values->len - 1).value += tmp.value;
	      else
		{
		  g_string_append_printf (g_array_index (values, run_day_value_triple_t, values->len - 1).svalue, ",%s", tmp.svalue->str);
		  g_string_free (tmp.svalue, TRUE);
		}
	    }
	  else
	    g_array_append_val (values, tmp);
        }
#if DEBUG
      s = g_string_new (NULL);
      g_string_printf (s, "values for \"%s\" now = [", name->str);
      for (i = 0; i < values->len; i++)
        g_string_append_printf (s, i == 0 ? "%g" : ",%g",
		g_array_index (values, run_day_value_triple_t, i).value);
      g_string_append_c (s, ']');
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, s->str);
      g_string_free (s, TRUE);
#endif
      break;

    default:
      g_assert_not_reached();
    }

end:
  if (!new_name)
    g_string_free (name, TRUE);
  return;
}



/**
 * Prints the stored output values for one node.
 */
void
print_values (unsigned int node)
{
  unsigned int nnames, i;
  char *s;
  GData **node_output_values;
  GArray **columns;
  GArray *column;
  unsigned int *indexes;
  unsigned int index;
  run_day_value_triple_t *tmp;
  unsigned int day;
  gboolean done = FALSE;
  static unsigned int run = 1; /* A run identifier than increases with each
    call to this function.  This is distinct from the per-node run numbers
    stored in the run-day-event triples. */

  nnames = output_names->len;

  /* For convenience during parsing, the output value lists are stored in a
   * Keyed Data list, keyed by output variable name.  For printing, it would be
   * more convenient to have the output value lists stored in an array, ordered
   * by output variable name.  So we construct that array here. */
  node_output_values = (GData **)(&g_ptr_array_index (output_values, node));
  columns = g_new (GArray *, nnames);
  for (i = 0; i < nnames; i++)
    {
      s = ((GString *) g_ptr_array_index (output_names, i))->str;
      columns[i] = (GArray *) (g_datalist_get_data (node_output_values, s));
    }

  /* Create an array of indexes into the output value lists.  This is needed
   * because not every output variable is reported on every day. */
  indexes = g_new0 (unsigned int, nnames);
  for (day = 1; !done; day++) /* outer loop = rows = days */
    {
      printf ("%u,%u", run, day);
      done = TRUE;
      for (i = 0; i < nnames; i++) /* inner loop = columns = variable names */
	{
	  column = columns[i];
	  if (column == NULL)
	    {
	      printf (",");
	      continue;
	    }
          index = indexes[i];
          if (index >= column->len)
	    {
	      printf (",");
	      continue;
	    }
	  tmp = &g_array_index (column, run_day_value_triple_t, index);
	  if (tmp->day > day)
	    {
	      printf (",");
	      done = FALSE;
	      continue;
	    }	  
	  if (tmp->svalue == NULL)
	    printf (",%g", tmp->value);
	  else
	    printf (",\"%s\"", tmp->svalue->str);
	  if (++index < column->len)
	    done = FALSE;
	  indexes[i] = index;
        }
      printf ("\n");
    }

  run++;

  /* Clean up. */
  g_free (columns);
  g_free (indexes);
}



/**
 * Clears the stored output values for one node.
 */
void
clear_values (unsigned int node)
{
  GData **node_output_values;

  node_output_values = (GData **)(&g_ptr_array_index (output_values, node));
  g_datalist_clear (node_output_values);
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
%type <sval> contours contour coords coord
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
      GData *new_list;

      if (pass == 2)
	{
	  /* If we haven't seen output from this node before, we need to extend
	   * the tracking lists for current run and current day, and create a
	   * new Keyed Data List to hold output values for the new node. */
	  if ((node + 1) > current_run->len)
            {
	      g_array_set_size (current_run, node + 1);

	      g_array_set_size (current_day, node + 1);
	      g_array_index (current_day, unsigned int, node) = 1;

	      g_ptr_array_set_size (output_values, node + 1);
	      /* Initialize the new entry to an empty Keyed Data List. */
	      g_datalist_init (&new_list);
	      g_ptr_array_index (output_values, node) = new_list;
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
	   * tree-style represention of something like, for example,
	   * 
	   * num-units-in-each-state={'Susceptible':20,'Latent':10}
	   *
	   * What we want for the summary table is something like:
	   *
	   * num-units-in-each-state:Susceptible = 20, ...
	   * num-units-in-each-state:Latent = 10, ...
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
      $$ = RPT_new_reporting (NULL, NULL, RPT_integer, RPT_never, FALSE);
      RPT_reporting_set_integer ($$, $1, NULL);
    }
  | FLOAT
    {
      $$ = RPT_new_reporting (NULL, NULL, RPT_real, RPT_never, FALSE);
      RPT_reporting_set_real ($$, $1, NULL);
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
      $$ = RPT_new_reporting (NULL, NULL, RPT_text, RPT_never, FALSE);
      RPT_reporting_set_text ($$, "POLYGON()", NULL);
    }
  | POLYGON LPAREN contours RPAREN
    {
      char *s;
      $$ = RPT_new_reporting (NULL, NULL, RPT_text, RPT_never, FALSE);
      s = g_strdup_printf ("POLYGON(%s)", $3);
      g_free ($3);
      RPT_reporting_set_text ($$, s, NULL);
      g_free (s);
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

contours:
    contours contour
    {
      $$ = g_strdup_printf ("%s %s", $1, $2);
      g_free ($1);
      g_free ($2);
    }
  | contour
    {
      $$ = $1;
    }
  ;

contour:
    LPAREN coords RPAREN
    {
      $$ = g_strdup_printf ("(%s)", $2);
      g_free ($2);
    }
  ;

coords:
    coords COMMA coord
    {
      $$ = g_strdup_printf ("%s,%s", $1, $3);
      g_free ($1);
      g_free ($3);
    }
  | coord
    {
      $$ = $1;
    }
  ;

coord:
    FLOAT FLOAT
    {
      $$ = g_strdup_printf ("%g %g", $1, $2);
    }
  | FLOAT INT
    {
      $$ = g_strdup_printf ("%g %i", $1, $2);
    }
  | INT FLOAT
    {
      $$ = g_strdup_printf ("%i %g", $1, $2);
    }
  | INT INT
    {
      $$ = g_strdup_printf ("%i %i", $1, $2);
    }
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
  const char *log_file = NULL;
  int verbosity = 0;
  unsigned int nnodes, i;
  struct poptOption options[2];

  /* Get the command-line options and arguments.  There should be one command-
   * line argument, the name of the simulation output file. */
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
  log_file = poptGetArg (option);
  poptFreeContext (option);

  if (log_file == NULL)
    g_error ("Need the name of a simulation output file.");

  /* Set the verbosity level. */
  if (verbosity < 2)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
    }
  if (verbosity < 1)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("reporting", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "verbosity = %i", verbosity);
#endif

  /* First pass to find all the output variable names.  We need them to print
   * the table header. */
  output_names = g_ptr_array_new ();

  yyin = fopen (log_file, "r");
  pass = 1;
  while (!feof(yyin))
    yyparse();

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "Done 1st pass");
#endif

  printf ("Run,Day");
  for (i = 0; i < output_names->len; i++)
    printf (",%s", ((GString *) g_ptr_array_index (output_names,i))->str);
  printf ("\n");	      
  fflush (stdout);

  /* Second pass to find and print the output variable values. */
  output_values = g_ptr_array_new ();
  current_run = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);
  current_day = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);

  fseek (yyin, 0L, SEEK_SET);
  yyrestart (yyin);
  pass = 2;
  while (!feof(yyin))
    yyparse();

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "Done 2nd pass");
#endif

  /* Some output values may have been printed during the parse.  Print out any
   * remaining values. */
  nnodes = current_run->len;
  for (i = 0; i < nnodes; i++)
    {
      print_values (i);
      clear_values (i);
    }

  /* Clean up. */
  g_array_free (current_run, TRUE);
  g_array_free (current_day, TRUE);
  g_ptr_array_foreach (output_names, g_string_free_as_GFunc, NULL);
  g_ptr_array_free (output_names, TRUE);
  g_ptr_array_free (output_values, TRUE);

  return EXIT_SUCCESS;
}

/* end of file full_table.y */
