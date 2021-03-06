/** @file smherds2xml/main.c
 * A utility that converts SpreadModel 2.14 herd status snapshot files to XML.
 * Send the snapshot file to standard input; the XML is written to standard
 * output.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date January 2004
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

#include <popt.h>
#include "herd.h"

extern FILE *yyin;              /* defined in scanner */
int yyparse (void);             /* defined in parser */
extern HRD_herd_list_t *herds;  /* defined in parser */

extern const char *HRD_status_name[];



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



int
main (int argc, char *argv[])
{
  poptContext option;
  int verbosity = 0;
  const char *herd_file = NULL;
  unsigned int nherds;
  HRD_herd_t *herd;
  int i;                        /* loop counter */
  char *summary;
  struct poptOption options[3];

  options[0].longName = "herd-file";
  options[0].shortName = 'h';
  options[0].argInfo = POPT_ARG_STRING;
  options[0].arg = &herd_file;
  options[0].val = 0;
  options[0].descrip = "SpreadModel 2.14 herd file";
  options[0].argDescrip = "herd-file";

  options[1].longName = "verbosity";
  options[1].shortName = 'V';
  options[1].argInfo = POPT_ARG_INT;
  options[1].arg = &verbosity;
  options[1].val = 0;
  options[1].descrip =
    "Message verbosity level (0 = simulation output only, 1 = + informational messages, 2 = + all debugging output)";
  options[1].argDescrip = "verbosity";

  options[2].longName = NULL;
  options[2].shortName = '\0';
  options[2].argInfo = 0;
  options[2].arg = NULL;
  options[2].val = 0;
  options[2].descrip = NULL;
  options[2].argDescrip = NULL;

  option = poptGetContext (NULL, argc, (const char **) argv, options, 0);
  poptGetNextOpt (option);

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

  /* Get the list of herds. */
  if (herd_file)
    yyin = fopen (herd_file, "r");
  else if (yyin == NULL)
    yyin = stdin;
  while (!feof (yyin))
    yyparse ();
  if (yyin != stdin)
    fclose (yyin);
  nherds = HRD_herd_list_length (herds);

  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO, "%i herds read", nherds);
#if DEBUG
  summary = HRD_herd_list_to_string (herds);
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "\n%s", summary);
  free (summary);
#endif

  printf ("<herds>\n");
  for (i = 0; i < nherds; i++)
    {
      herd = HRD_herd_list_get (herds, i);
      printf ("  <herd>\n");
      printf ("    <production-type></production-type>\n");
      printf ("    <size>%u</size>\n", herd->size);
      printf ("    <location>\n");
      printf ("      <latitude>%g</latitude>\n", herd->lat);
      printf ("      <longitude>%g</longitude>\n", herd->lon);
      printf ("    </location>\n");
      printf ("    <status>%s</status>\n", HRD_status_name[herd->status]);
      printf ("  </herd>\n");
    }
  printf ("</herds>\n");

  /* Clean up. */
  HRD_free_herd_list (herds);

  return EXIT_SUCCESS;
}

/* end of file main.c */
