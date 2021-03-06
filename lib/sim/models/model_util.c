/** @file model_util.c
 * Helper functions for sub-models.
 *
 * @author Neil Harvey <neilharvey@gmail.com><br>
 *   Grid Computing Research Group<br>
 *   Department of Computing & Information Science, University of Guelph<br>
 *   Guelph, ON N1G 2W1<br>
 *   CANADA
 * @version 0.1
 * @date October 2004
 *
 * Copyright &copy; University of Guelph, 2004-2008
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "model_util.h"
#include "gis.h"

#if STDC_HEADERS
#  include <string.h>
#endif

#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#define EPSILON 0.001



/**
 *
 */
gboolean *
ergadm_read_prodtype_attribute (scew_element * params,
                                char *attr_name, GPtrArray * production_type_names)
{
  gboolean *flags;
  unsigned int nprod_types;
  scew_attribute *attr;
  XML_Char const *attr_text;
  gchar **tokens;
  gchar **iter;
  gchar *tmp;                   /* for converting between encodings */
  int i;                        /* loop counter */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_read_prodtype_attribute");
#endif

  nprod_types = production_type_names->len;
  flags = g_new0 (gboolean, nprod_types);

  attr = scew_attribute_by_name (params, attr_name);
  /* If the "production-type" attribute is missing, assume the parameters apply
   * to all production types. */
  if (attr == NULL)
    {
      for (i = 0; i < nprod_types; i++)
        flags[i] = TRUE;
    }
  else
    {
      attr_text = scew_attribute_value (attr);
      /* If the "production-type" attribute is blank, assume the parameters
       * apply to all production types. */
      if (strlen (attr_text) == 0)
        for (i = 0; i < nprod_types; i++)
          flags[i] = TRUE;
      else
        {
          /* Split up the text at commas. */
          tokens = g_strsplit (attr_text, ",", 0);
          for (iter = tokens; *iter != NULL; iter++)
            {
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
                     "token (Expat encoding) = \"%s\"", *iter);
#endif
              /* Expat stores the text as UTF-8.  Convert to ISO-8859-1. */
              tmp = g_convert_with_fallback (*iter, -1, "ISO-8859-1", "UTF-8", "?", NULL, NULL, NULL);
              g_assert (tmp != NULL);
              for (i = 0; i < nprod_types; i++)
                if (strcasecmp (tmp, g_ptr_array_index (production_type_names, i)) == 0)
                  break;
              if (i == nprod_types)
                g_warning ("there are no \"%s\" units", tmp);
              else
                flags[i] = TRUE;
              g_free (tmp);
            }
          g_strfreev (tokens);
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_read_prodtype_attribute");
#endif

  return flags;
}



/**
 *
 */
gboolean *
ergadm_read_zone_attribute (scew_element * params, ZON_zone_list_t * zones)
{
  gboolean *flags;
  unsigned int nzones;
  scew_attribute *attr;
  XML_Char const *attr_text;
  gchar **tokens;
  gchar **iter;
  int i;                        /* loop counter */

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_read_zone_attribute");
#endif

  nzones = ZON_zone_list_length (zones);
  flags = g_new0 (gboolean, nzones);

  attr = scew_attribute_by_name (params, "zone");
  /* If the "zone" attribute is missing, assume the parameters apply in all
   * zones. */
  if (attr == NULL)
    {
      for (i = 0; i < nzones; i++)
        flags[i] = TRUE;
    }
  else
    {
      attr_text = scew_attribute_value (attr);
      /* If the "zone" attribute is blank, assume the parameters apply in all
       * zones. */
      if (strlen (attr_text) == 0)
        for (i = 0; i < nzones; i++)
          flags[i] = TRUE;
      else
        {
          /* Split up the text at commas. */
          tokens = g_strsplit (attr_text, ",", 0);
          for (iter = tokens; *iter != NULL; iter++)
            {
#if DEBUG
              g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "token = \"%s\"", *iter);
#endif
              for (i = 0; i < nzones; i++)
                if (strcasecmp (*iter, ZON_zone_list_get (zones, i)->name) == 0)
                  break;
              if (i == nzones)
                g_warning ("there is no zone named \"%s\"", *iter);
              else
                flags[i] = TRUE;
            }
          g_strfreev (tokens);
        }
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_read_zone_attribute");
#endif

  return flags;
}



/**
 * Extends a rotating array.
 *
 * There are several places in the model where we want to delay actions.  It is
 * useful to have an array in which you can easily store events that you want
 * to retrieve 1 day from now, 2 days, etc.  An index "rotates" through the
 * array, so an event that is to happen in 1 day is stored 1 place ahead of the
 * rotating index, an event that is to happen in 2 days is stored 2 places
 * ahead of the rotating index, etc.  Sometimes you don't know in advance how
 * many places ahead you will need, and so you need to extend a rotating array.
 *
 * The array "rotates" so there are additional steps beyond simply extending
 * the array.  The first figure below shows the case when there is room at the
 * end of the newly-extended array for all items before the rotating index.
 * So given an array of length <i>n</i> (1), we (2) add null entries to the
 * end, (3) copy the items from before the rotating index to the new space at
 * the end, and (4) overwrite the spaces before the rotating index with null
 * entries.
 *
 * The second figure shows the case when there is not room at the end of the
 * extended array for all items before the rotating index.  Given an array of
 * length <i>n</i> (1), we (2) add null entries to the end, (3) copy as many
 * items as we can from before the rotating index to the new space at the end,
 * (4) copy the remaining entries before the rotating index to the start of the
 * array, and (5) overwrite the spaces before the rotating index with null
 * entries.
 *
 * @image html extend_array_1.png "Extending the array when there is room at the end for all items before the index"
 * @image latex extend_array_1.eps "Extending the array when there is room at the end for all items before the index" width=4in
 *
 * @image html extend_array_2.png "Extending the array when there is not room at the end for all items before the index"
 * @image latex extend_array_2.eps "Extending the array when there is not room at the end for all items before the index" width=4in
 *
 * @param array the pending results array
 * @param length the new length
 * @param index the current location of the rotating index
 */
void
ergadm_extend_rotating_array (GPtrArray * array, unsigned int length, unsigned int index)
{
  unsigned int old_length, diff;
  unsigned int i;
#if DEBUG
  GString *s;
  GQueue *q;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER ergadm_extend_rotating_array");
#endif

  old_length = array->len;
  if (old_length >= length)
    goto end;

#if DEBUG
  s = g_string_new ("old array = [");
  for (i = 0; i < old_length; i++)
    {
      if (i > 0)
        g_string_append_c (s, ',');
      if (i == index)
        g_string_append_c (s, '(');
      q = (GQueue *) g_ptr_array_index (array, i);
      if (q == NULL)
        g_string_append_c (s, 'x');
      else
        g_string_append_printf (s, "%u", g_queue_get_length (q));
      if (i == index)
        g_string_append_c (s, ')');
    }
  g_string_append_c (s, ']');
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif

  g_ptr_array_set_size (array, length);

  /* Move as many items as possible from the start of the list to the new end
   * of the list. */
  diff = length - old_length;
  for (i = 0; i < diff; i++)
    if (i < index)
      {
#if DEBUG
        g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,
               "copying item at pos %u to pos %u", i, old_length + i);
#endif
        g_ptr_array_index (array, old_length + i) = g_ptr_array_index (array, i);
      }
    else
      {
#if DEBUG
        g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "adding empty item at pos %u", old_length + i);
#endif
        g_ptr_array_index (array, old_length + i) = g_queue_new ();
      }

  /* If there are still items sitting just before the rotating index, move them
   * down to the start of the list. */
  for (i = 0; i + diff < index; i++)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "copying item at pos %u to pos %u", i + diff, i);
#endif
      g_ptr_array_index (array, i) = g_ptr_array_index (array, i + diff);
    }
  for (; i < index; i++)
    {
#if DEBUG
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "adding empty item at pos %u", i);
#endif
      g_ptr_array_index (array, i) = g_queue_new ();
    }

#if DEBUG
  s = g_string_new ("new array = [");
  for (i = 0; i < length; i++)
    {
      if (i > 0)
        g_string_append_c (s, ',');
      if (i == index)
        g_string_append_c (s, '(');
      q = (GQueue *) g_ptr_array_index (array, i);
      if (q == NULL)
        g_string_append_c (s, 'x');
      else
        g_string_append_printf (s, "%u", g_queue_get_length (q));
      if (i == index)
        g_string_append_c (s, ')');
    }
  g_string_append_c (s, ']');
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT ergadm_extend_rotating_array");
#endif

  return;
}

/* end of file model_util.c */
