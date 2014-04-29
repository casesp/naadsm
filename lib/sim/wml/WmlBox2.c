/** @file WmlBox2.c
 * Functions for 2-dimensional oriented rectangles.
 *
 * The Wild Magic Library (WML) is written by David H. Eberly.  The WML code
 * used here has been converted to C.  The entire WML source code is not here;
 * only enough parts to provide some needed functions.
 *
 * Magic Software, Inc.<br>
 * http://www.magic-software.com<br>
 * http://www.wild-magic.com<br>
 * Copyright &copy; 2004.  All Rights Reserved
 *
 * The Wild Magic Library (WML) source code is supplied under the terms of
 * the license agreement http://www.magic-software.com/License/WildMagic.pdf
 * and may not be copied or disclosed except in accordance with the terms of
 * that agreement.
 *
 * The WML license states, in part, "The Software may be used, edited,
 * modified, copied, and distributed by you for commercial products provided
 * that such products are not intended to wrap The Software solely for the
 * purposes of selling it as if it were your own product.  The intent of this
 * clause is that you use The Software, in part or in whole, to assist you in
 * building your own original products."
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "wml/WmlBox2.h"
#include <glib.h>

#if STDC_HEADERS
#  include <stdlib.h>
#endif



/**
 * Creates a new Box.  No initialization for efficiency.
 *
 * @return a newly-allocated Box object.
 */
WML_Box2 *
WML_new_Box2 (void)
{
  WML_Box2 *self;

  self = g_new (WML_Box2, 1);

  return self;
}



/**
 * Creates a deep copy of an existing Box.
 *
 * @param other the box to copy.
 * @return a newly-allocated Box object.
 */
WML_Box2 *
WML_clone_Box2 (WML_Box2 * other)
{
  WML_Box2 *self;
  int i;

  self = g_new (WML_Box2, 1);
  WML_Vector2_assign (WML_Box2_Center (self), WML_Box2_Center (other));
  for (i = 0; i < 2; i++)
    {
      WML_Vector2_assign (WML_Box2_Axis (self, i), WML_Box2_Axis (other, i));
      self->m_afExtent[i] = other->m_afExtent[i];
    }

  return self;
}



/**
 * Returns a text representation of a Box.
 *
 * @param self a box.
 * @return a string.
 */
char *
WML_Box2_to_string (WML_Box2 * self)
{
  GString *s;
  char *chararray, *substring;

  s = g_string_new ("<Box2");

  substring = WML_Vector2_to_string (&self->m_kCenter);
  g_string_append_printf (s, " center=%s", substring);
  free (substring);

  substring = WML_Vector2_to_string (&self->m_akAxis[0]);
  g_string_append_printf (s, " 1st axis=%s", substring);
  free (substring);

  substring = WML_Vector2_to_string (&self->m_akAxis[1]);
  g_string_append_printf (s, " 2nd axis=%s", substring);
  free (substring);

  g_string_append_printf (s, " extents=<%g,%g>>", self->m_afExtent[0], self->m_afExtent[1]);

  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Deletes a Box from memory.
 *
 * @param self a box.
 */
void
WML_free_Box2 (WML_Box2 * self)
{
  if (self != NULL)
    g_free (self);
}




/**
 * Returns the <i>i</i>th axis of a Box.
 *
 * @param self a box.
 * @param i the axis to choose, either 0 or 1.
 * @return the <i>i</i>th axis.
 */
WML_Vector2 *
WML_Box2_Axis (WML_Box2 * self, int i)
{
  g_assert (0 <= i && i < 2);
  return &self->m_akAxis[i];
}



/**
 * Returns the axes of a Box.
 *
 * @param self a box.
 * @return the axes (a 2-element array).
 */
WML_Vector2 *
WML_Box2_Axes (WML_Box2 * self)
{
  return self->m_akAxis;
}



/**
 * Returns the <i>i</i>th extent of a Box.
 *
 * @param self a box.
 * @param i the extent to choose, either 0 or 1.
 * @return the <i>i</i>th extent.
 */
double
WML_Box2_Extent (WML_Box2 * self, int i)
{
  g_assert (0 <= i && i < 2);
  return self->m_afExtent[i];
}



/**
 * Returns the extents of a Box.
 *
 * @param self a box.
 * @return the extents (a 2-element array).
 */
double *
WML_Box2_Extents (WML_Box2 * self)
{
  return self->m_afExtent;
}



/**
 * Finds the vertices of a Box.
 *
 * @param self a box.
 * @param akVertex an array of 4 Vectors in which to store the vertices.
 */
void
WML_Box2_ComputeVertices (WML_Box2 * self, WML_Vector2 akVertex[4])
{
  WML_Vector2 akEAxis[2];

  akEAxis[0] = WML_Vector2_mul (&self->m_akAxis[0], self->m_afExtent[0]);
  akEAxis[1] = WML_Vector2_mul (&self->m_akAxis[1], self->m_afExtent[1]);

  WML_Vector2_assign (&akVertex[0], &self->m_kCenter);
  WML_Vector2_sub_inplace (&akVertex[0], &akEAxis[0]);
  WML_Vector2_sub_inplace (&akVertex[0], &akEAxis[1]);

  WML_Vector2_assign (&akVertex[1], &self->m_kCenter);
  WML_Vector2_add_inplace (&akVertex[1], &akEAxis[0]);
  WML_Vector2_sub_inplace (&akVertex[1], &akEAxis[1]);

  WML_Vector2_assign (&akVertex[2], &self->m_kCenter);
  WML_Vector2_add_inplace (&akVertex[2], &akEAxis[0]);
  WML_Vector2_add_inplace (&akVertex[2], &akEAxis[1]);

  WML_Vector2_assign (&akVertex[3], &self->m_kCenter);
  WML_Vector2_sub_inplace (&akVertex[3], &akEAxis[0]);
  WML_Vector2_add_inplace (&akVertex[3], &akEAxis[1]);
}

/* end of file WmlBox2.c */
