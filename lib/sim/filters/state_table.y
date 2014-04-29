%{
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <popt.h>
#include "herd.h"
#include <stdio.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#include <assert.h>

/** @file filters/state_table.c
 * A filter that turns SHARCSpread output into a table of herd states.
 *
 * Call it as
 *
 * <code>state_table_filter < LOG-FILE</code>
 *
 * The table is written to standard output in comma-separated values format.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date January 2005
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
  GArray *state_list;
} run_day_value_triple_t;

GPtrArray *unit_states; /**< The herd states.  Each item in the list is for one
  node.  The items are GArrays of run-day-statelist triples. */
unsigned int current_node;
GArray *current_run; /**< The most recent run number we have seen in the output from each node. */
GArray *current_day; /**< The most recent day we have seen in the output from each node. */
gboolean header_printed;


/**
 * Prints the stored output values for one node.
 */
void
print_values (unsigned int node)
{
  GArray *tmp;
  GArray *state_list;
  unsigned int ndays, nherds;
  unsigned int day, i;
  static unsigned int run = 1; /* A run identifier than increases with each
    call to this function.  This is distinct from the per-node run numbers
    stored in the run-day-event triples. */

  /* Get the data for this node. */
  tmp = (GArray *) g_ptr_array_index (unit_states, node);

  /* If the header line for the table hasn't been printed yet, do so now. */
  if (!header_printed && tmp->len > 0)
    {
      printf ("Run,Day");
      state_list = g_array_index (tmp, run_day_value_triple_t, 0).state_list;
      nherds = state_list->len;
      for (i = 0; i < nherds; i++) /* inner loop = columns = herds */
        printf (",%i", i);
      printf ("\n");
      header_printed = TRUE;
    }

  ndays = tmp->len;
  for (day = 0; day < ndays; day++) /* outer loop = rows = days */
    {
      printf ("%u,%u", run, day + 1);
      state_list = g_array_index (tmp, run_day_value_triple_t, day).state_list;
      nherds = state_list->len;
      for (i = 0; i < nherds; i++) /* inner loop = columns = herds */
        printf (",%i", g_array_index (state_list, HRD_status_t, i));
      printf ("\n");
    }

  run++;
}



/**
 * Clears the stored output values for one node.
 */
void
clear_values (unsigned int node)
{
  GArray *tmp;
  unsigned int i, n;

  /* Get the data for this node. */
  tmp = (GArray *) g_ptr_array_index (unit_states, node);

  /* Free the dynamic part of each element. */
  n = tmp->len;
  for (i = 0; i < n; i++)
    g_array_free (g_array_index (tmp, run_day_value_triple_t, i).state_list, TRUE);

  /* Truncate this array. */
  g_array_set_size (tmp, 0);
}

%}

%union {
  int ival;
  float fval;
  char *sval;
  GArray *lval;
}

%token NODE RUN DAY POLYGON
%token COMMA COLON EQ LBRACE RBRACE LPAREN RPAREN DQUOTE NEWLINE
%token <ival> INT
%token <fval> FLOAT
%token <sval> VARNAME STRING
%type <lval> state_codes
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
       * the tracking lists for current run and current day, and create a
       * new Keyed Data List to hold output values for the new node. */
      if ((node + 1) > current_run->len)
        {
	  g_array_set_size (current_run, node + 1);

	  g_array_set_size (current_day, node + 1);
	  g_array_index (current_day, unsigned int, node) = 1;

	  g_ptr_array_set_size (unit_states, node + 1);
	  /* Initialize the new entry to an empty GArray. */
	  new_list = g_array_new (FALSE, FALSE, sizeof(run_day_value_triple_t));
	  g_ptr_array_index (unit_states, node) = new_list;
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
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "node %u now on run %u, day %u",
             node, run, g_array_index (current_day, unsigned int, node));
#endif
    }
  ;

data_line:
    state_codes vars
    {
      run_day_value_triple_t tmp;
      GArray *states;
      
      /* Add the line of state codes for this node. */
      tmp.run = g_array_index (current_run, unsigned int, current_node);
      tmp.day = g_array_index (current_day, unsigned int, current_node);
      tmp.state_list = $1;
      
      states = (GArray *) g_ptr_array_index (unit_states, current_node);
      g_array_append_val (states, tmp);
    }
  | state_codes
    {
      run_day_value_triple_t tmp;
      GArray *states;
      
      /* Add the line of state codes for this node. */
      tmp.run = g_array_index (current_run, unsigned int, current_node);
      tmp.day = g_array_index (current_day, unsigned int, current_node);
      tmp.state_list = $1;
      
      states = (GArray *) g_ptr_array_index (unit_states, current_node);
      g_array_append_val (states, tmp);    
    }
  | vars
    { }
  |
    { }
  ;

state_codes:
    state_codes INT
    {
      $$ = $1;
      g_array_append_val ($$, $2);
    }
  | INT
    {
      /* Initialize an array of state codes. */
      $$ = g_array_new (FALSE, FALSE, sizeof(HRD_status_t));
      g_array_append_val ($$, $1);
    }
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
      /* The varname token's value is set with a g_strdup, so we need to free
       * it. */
      g_free ($1);
    }
  ;

value:
    INT
    { }
  | FLOAT
    { }
  | STRING
    {
      /* The string token's value is set with a g_strndup, so we need to free
       * it. */
      g_free ($1);
    }
  | POLYGON LPAREN RPAREN
    { }
  | POLYGON LPAREN contours RPAREN
    { }
  | LBRACE RBRACE
    { }
  | LBRACE subvars RBRACE
    { }
  ;

subvars:
    subvars COMMA subvar
    { }
  | subvar
    { }
  ;

subvar:
    STRING COLON value
    {
      /* The string token's value is set with a g_strndup, so we need to free
       * it. */
      g_free ($1);
    }
  ;

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
  int verbosity = 0;
  unsigned int nnodes, i;
  struct poptOption options[2];

  /* Get the command-line options and arguments. */
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
  poptFreeContext (option);

  /* Set the verbosity level. */
  if (verbosity < 2)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_LEVEL_DEBUG, silent_log_handler, NULL);
    }
  if (verbosity < 1)
    {
      g_log_set_handler (NULL, G_LOG_LEVEL_INFO, silent_log_handler, NULL);
      g_log_set_handler ("herd", G_LOG_LEVEL_INFO, silent_log_handler, NULL);
    }
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "verbosity = %i", verbosity);
#endif

  /* First pass to find all the output variable names.  We need them to print
   * the table header. */
  unit_states = g_ptr_array_new ();
  current_run = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);
  current_day = g_array_sized_new (FALSE, TRUE, sizeof (unsigned int), 1);

  /* We can't print the header line until we know how many herds there are. */
  header_printed = FALSE;

  /* Call the parser to fill in the unit_states array. */
  if (yyin == NULL)
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
  g_array_free (current_run, TRUE);
  g_array_free (current_day, TRUE);
  /*
  g_ptr_array_foreach (output_names, g_string_free_as_GFunc, NULL);
  g_ptr_array_free (unit_states, TRUE);
  */
  
  return EXIT_SUCCESS;
}

/* end of file state_table.y */
