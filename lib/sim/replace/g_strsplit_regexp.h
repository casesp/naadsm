/** @file g_strsplit_regexp.h
 * Interface for g_strsplit_regexp.c
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

#ifndef G_STRSPLIT_REGEXP_H
#define G_STRSPLIT_REGEXP_H

#include <glib.h>



/* Prototypes. */
gchar **g_strsplit_regexp (const gchar * string, const gchar * delimiter, gint max_tokens);

#endif /* !G_STRSPLIT_REGEXP_H */
