/** @file g_strsplit_regexp.c
 * Splits a string into pieces, using the given delimter regular expression.
 *
 * This function mimics g_strsplit from the GLib library, except it specifies
 * the delimiter as a regular expression instead of an ordinary string.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date March 2004
 *
 * Copyright &copy; University of Guelph, 2004-2006
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include "g_strsplit_regexp.h"
#include <regex.h>



/**
 * Splits a string into a maximum of <i>max_tokens</i> pieces, using the given
 * delimter regular expression <i>delimiter_regexp</i>.
 *
 * This function mimics g_strsplit from the GLib library, except it specifies
 * the delimiter as a regular expression instead of an ordinary string.
 *
 * @param string a string to split.
 * @param delimiter_regexp a string which specifies the places at which to
 *   split the string.
 * @param max_tokens the maximum number of pieces to split <i>string</i> into.
 *   If this is less than 1, the string is split completely.
 * @return a newly-allocated NULL-terminated array of strings. Use g_strfreev()
 *   to free it.
 */
gchar **
g_strsplit_regexp (const gchar * string, const gchar * delimiter_regexp, gint max_tokens)
{
  regex_t regex;
  GPtrArray *tmp_tokens;
  gchar **tokens;
  int errcode;
  size_t errlength;
  char *errmsg;
  regmatch_t match;
  const gchar *current_start;

  errcode = regcomp (&regex, delimiter_regexp, REG_EXTENDED);
  /* If the regular expression failed to compile, issue a warning and return
   * the whole string as one big token. */
  if (errcode != 0)
    {
      errlength = regerror (errcode, &regex, NULL, 0);
      errmsg = g_new (char, errlength);
      regerror (errcode, &regex, errmsg, errlength);
      g_warning
        ("regular expression did not compile because: %s\nreturning string not split", errmsg);
      g_free (errmsg);
      tokens = g_new (char *, 2);
      tokens[0] = g_strdup (string);
      /* Null-terminate the array of strings. */
      tokens[1] = NULL;
    }
  else
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "regular expression compiled");
#endif
      current_start = string;
      tmp_tokens = g_ptr_array_new ();
      while (1)
        {
          errcode = regexec (&regex, current_start, 1, &match, 0);
          if (errcode == REG_NOMATCH)
            {
              /* There are no more matches; the remainder of the string will
               * become the final token. */
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "no more matches, final token is \"%s\"", current_start);
#endif
              g_ptr_array_add (tmp_tokens, g_strdup (current_start));
              break;
            }
          g_ptr_array_add (tmp_tokens, g_strndup (current_start, match.rm_so));
#if DEBUG
          g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                 "match at %i-%i, text before = \"%s\"", match.rm_so,
                 match.rm_eo, (char *) (g_ptr_array_index (tmp_tokens, tmp_tokens->len - 1)));
#endif
          current_start += match.rm_eo;
        }
      /* Null-terminate the array of strings. */
      g_ptr_array_add (tmp_tokens, NULL);
      tokens = (gchar **) (tmp_tokens->pdata);
      g_ptr_array_free (tmp_tokens, FALSE);
    }

  regfree (&regex);
  return tokens;
}

/* end of file g_strsplit_regexp.c */
