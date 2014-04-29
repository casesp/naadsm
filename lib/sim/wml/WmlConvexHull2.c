/** @file WmlConvexHull2.c
 * Functions for finding convex hulls of 2-D points.
 *
 * The Wild Magic Library (WML) is written by David H. Eberly.  The WML code
 * used here has been converted to C.  The entire WML source code is not here;
 * only enough parts to provide some needed functions.
 *
 * Magic Software, Inc.<br>
 * http://www.magic-software.com<br>
 * http://www.wild-magic.com<br>
 * Copyright &copy; 2003.  All Rights Reserved
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

#if STDC_HEADERS
#  include <stdlib.h>
#endif

#if HAVE_STRING_H
#  include <string.h>
#endif

#include "wml/WmlConvexHull2.h"

#define COLLINEAR_EPSILON 1e-06



/* for sorting */
typedef struct
{
  WML_Vector2 m_kV;
  int m_iIndex;
}
WML_SortedVertex;



/**
 * Returns a text representation of a SortedVertex object.
 *
 * @param self a sorted vertex.
 * @return a string.
 */
char *
WML_SortedVertex_to_string (WML_SortedVertex * self)
{
  GString *s;
  char *substring, *chararray;

  s = g_string_new (NULL);
  substring = WML_Vector2_to_string (&(self->m_kV));
  g_string_sprintf (s, "%s#%i", substring, self->m_iIndex);
  free (substring);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Assigns one SortedVertex object to another.
 *
 * @param self the destination sorted vertex.
 * @param rkSV the source sorted vertex.
 * @return the destination sorted vertex.
 */
WML_SortedVertex *
WML_SortedVertex_assign (WML_SortedVertex * self, WML_SortedVertex * rkSV)
{
  WML_Vector2_assign (&(self->m_kV), &(rkSV->m_kV));
  self->m_iIndex = rkSV->m_iIndex;
  return self;
}



/**
 * Tests whether two SortedVertex objects are equal.
 *
 * @param self a sorted vertex.
 * @param rkSV a sorted vertex.
 * @return TRUE if the vertices are identical; FALSE otherwise.
 */
gboolean
WML_SortedVertex_eq (WML_SortedVertex * self, WML_SortedVertex * rkSV)
{
  return WML_Vector2_eq (&(self->m_kV), &(rkSV->m_kV));
}



/**
 * Tests whether one SortedVertex object is less than another.
 *
 * @param self a sorted vertex.
 * @param rkSV a sorted vertex.
 * @return TRUE if <i>self</i> < <i>rkSV</i>; FALSE otherwise.
 */
gboolean
WML_SortedVertex_lt (WML_SortedVertex * self, WML_SortedVertex * rkSV)
{
  if (self->m_kV.X < rkSV->m_kV.X)
    return TRUE;
  if (self->m_kV.X > rkSV->m_kV.X)
    return FALSE;
  return self->m_kV.Y < rkSV->m_kV.Y;
}



/**
 * A comparison function for SortedVertex objects suitable for GArray's sort
 * function.
 *
 * @param a a sorted vertex, cast to a gconstpointer.
 * @param b a sorted vertex, cast to a gconstpointer.
 * @return -1 if a < b, 0 if a = b, or 1 if a > b.
 */
gint
WML_SortedVertex_cmp (gconstpointer a, gconstpointer b)
{
  WML_SortedVertex *sv1, *sv2;

  sv1 = (WML_SortedVertex *) a;
  sv2 = (WML_SortedVertex *) b;

  if (WML_SortedVertex_eq (sv1, sv2))
    return 0;
  else if (WML_SortedVertex_lt (sv1, sv2))
    return -1;
  else
    return 1;
}



/**
 * test if point is contained by hull.
 */
gboolean
WML_ConvexHull2_ContainsPoint (WML_ConvexHull2 * self, WML_Vector2 * rkP)
{
  int i0, i1;
  WML_Vector2 *rkV0, *rkV1;
  WML_Vector2 kDir, kNormal, tmp;

  for (i1 = 0, i0 = self->m_iHQuantity - 1; i1 < self->m_iHQuantity; i0 = i1++)
    {
      rkV0 = &(self->m_akVertex[self->m_aiHIndex[i0]]);
      rkV1 = &(self->m_akVertex[self->m_aiHIndex[i1]]);
      kDir = WML_Vector2_sub (rkV1, rkV0);
      kNormal = WML_Vector2_Perp (&kDir);       /* outer normal */

      tmp = WML_Vector2_sub (rkP, rkV0);
      if (WML_Vector2_Dot (&kNormal, &tmp) > 0.0)
        return FALSE;
    }

  return TRUE;
}



/**
 *
 */
WML_OrderType
WML_CollinearTest (WML_Vector2 * rkP, WML_Vector2 * rkQ0, WML_Vector2 * rkQ1)
{
  WML_Vector2 kD, kA;
  double fDdD, fAdA, fDet, fRelative, fDdA;

  kD = WML_Vector2_sub (rkQ1, rkQ0);
  kA = WML_Vector2_sub (rkP, rkQ0);
  fDdD = WML_Vector2_Dot (&kD, &kD);
  fAdA = WML_Vector2_Dot (&kA, &kA);
  fDet = WML_Vector2_Kross (&kD, &kA);
  fRelative = fDet * fDet - COLLINEAR_EPSILON * fDdD * fAdA;

  if (fRelative > 0.0)
    {
      if (fDet > 0.0)
        {
          /* points form counterclockwise triangle <P,Q0,Q1> */
          return WML_ORDER_POSITIVE;
        }
      else if (fDet < 0.0)
        {
          /* points form clockwise triangle <P,Q1,Q0> */
          return WML_ORDER_NEGATIVE;
        }
    }

  /* P is on line of <Q0,Q1> */
  fDdA = WML_Vector2_Dot (&kD, &kA);
  if (fDdA < 0.0)
    {
      /* order is <P,Q0,Q1> */
      return WML_ORDER_COLLINEAR_LEFT;
    }

  if (fDdA > fDdD)
    {
      /* order is <Q0,Q1,P> */
      return WML_ORDER_COLLINEAR_RIGHT;
    }

  /* order is <Q0,P,Q1> */
  return WML_ORDER_COLLINEAR_CONTAIN;
}



void
WML_MergeLinear (WML_ConvexHull2 * self, WML_SortedVertex * rkP)
{
  WML_SortedVertex kCopy;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER WML_MergeLinear");
#endif

  switch (WML_CollinearTest
          (&(rkP->m_kV),
           &(g_array_index (self->m_kHull, WML_SortedVertex, 0).m_kV),
           &(g_array_index (self->m_kHull, WML_SortedVertex, 1).m_kV)))
    {
    case WML_ORDER_POSITIVE:
      {
        /* merged hull is <P,Q0,Q1> */
        self->m_iHullType = WML_HULL_PLANAR;
        WML_SortedVertex_assign (&kCopy, &g_array_index (self->m_kHull, WML_SortedVertex, 1));
        g_array_append_val (self->m_kHull, kCopy);
        WML_SortedVertex_assign (&g_array_index
                                 (self->m_kHull, WML_SortedVertex, 1),
                                 &g_array_index (self->m_kHull, WML_SortedVertex, 0));
        WML_SortedVertex_assign (&g_array_index (self->m_kHull, WML_SortedVertex, 0), rkP);
        break;
      }
    case WML_ORDER_NEGATIVE:
      {
        /* merged hull is <P,Q1,Q0> */
        self->m_iHullType = WML_HULL_PLANAR;
        WML_SortedVertex_assign (&kCopy, &g_array_index (self->m_kHull, WML_SortedVertex, 0));
        g_array_append_val (self->m_kHull, kCopy);
        WML_SortedVertex_assign (&g_array_index (self->m_kHull, WML_SortedVertex, 0), rkP);
        break;
      }
    case WML_ORDER_COLLINEAR_LEFT:
      /* linear order is <P,Q0,Q1>, merged hull is <P,Q1> */
      WML_SortedVertex_assign (&g_array_index (self->m_kHull, WML_SortedVertex, 0), rkP);
      break;
    case WML_ORDER_COLLINEAR_RIGHT:
      /* linear order is <Q0,Q1,P>, merged hull is <Q0,P> */
      WML_SortedVertex_assign (&g_array_index (self->m_kHull, WML_SortedVertex, 1), rkP);
      break;
      /* case WML_ORDER_COLLINEAR_CONTAIN:  linear order is <Q0,P,Q1>, no change */
    }

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT WML_MergeLinear");
#endif

  return;
}



void
WML_MergePlanar (WML_ConvexHull2 * self, WML_SortedVertex * rkP)
{
  int iSize;
  int i, iU, iL;
  WML_OrderType iCT;
  GArray *kTmpHull = NULL, *tmp;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER WML_MergePlanar");
#endif

  iSize = self->m_kHull->len;

  /* search counterclockwise for last visible vertex */
  for (iU = 0, i = 1; iU < iSize; iU = i++)
    {
      if (i == iSize)
        i = 0;

      iCT =
        WML_CollinearTest (&(rkP->m_kV),
                           &(g_array_index
                             (self->m_kHull, WML_SortedVertex, iU).m_kV),
                           &(g_array_index (self->m_kHull, WML_SortedVertex, i).m_kV));
      if (iCT == WML_ORDER_NEGATIVE)
        continue;
      if (iCT == WML_ORDER_POSITIVE || iCT == WML_ORDER_COLLINEAR_LEFT)
        break;

      /* iCT == WML_ORDER_COLLINEAR_CONTAIN || iCT == WML_ORDER_COLLINEAR_RIGHT */
      goto end;
    }
  g_assert (iU < iSize);

  /* search clockwise for last visible vertex */
  for (iL = 0, i = iSize - 1; i >= 0; iL = i--)
    {
      iCT =
        WML_CollinearTest (&(rkP->m_kV),
                           &(g_array_index
                             (self->m_kHull, WML_SortedVertex, i).m_kV),
                           &(g_array_index (self->m_kHull, WML_SortedVertex, iL).m_kV));
      if (iCT == WML_ORDER_NEGATIVE)
        continue;
      if (iCT == WML_ORDER_POSITIVE || iCT == WML_ORDER_COLLINEAR_RIGHT)
        break;

      /* iCT == WML_ORDER_COLLINEAR_CONTAIN || iCT == WML_ORDER_COLLINEAR_LEFT */
      goto end;
    }
  g_assert (i >= 0);

  if (iU == iL)
    {
      /* This probably occurs when CollinearTest should report collinearity,
       * but does not.  If it does occur, and you care about this code
       * block not occurring, try increasing the size of the collinear
       * epsilon.  When this block does occur, the conclusion is that the
       * input point is collinear with an edge of the hull, so just return. */
      goto end;
    }

  /* construct the counterclockwise-ordered merged-hull vertices */
  kTmpHull = g_array_new (FALSE, FALSE, sizeof (WML_SortedVertex));
  g_array_append_val (kTmpHull, *rkP);
  while (TRUE)
    {
      g_array_append_val (kTmpHull, g_array_index (self->m_kHull, WML_SortedVertex, iU));
      if (iU == iL)
        break;

      if (++iU == iSize)
        iU = 0;
    }
  g_assert (kTmpHull->len > 2);

  tmp = self->m_kHull;
  self->m_kHull = kTmpHull;
  g_array_free (tmp, TRUE);

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT WML_MergePlanar");
#endif

  return;
}



/**
 * incremental hull
 */
void
WML_ByIncremental (WML_ConvexHull2 * self)
{
  /* In C++ this variable is a vector of SortedVertex objects. */
  GArray *kSVArray = NULL;
  int i;
  WML_SortedVertex sv;
#if DEBUG
  GString *s;
  char *substring;
  int j;
#endif

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER WML_ByIncremental");
#endif

  /* Sort by x-component and store in contiguous array.  The sort is
   * O(N log N). */
  kSVArray = g_array_sized_new (FALSE, FALSE, sizeof (WML_SortedVertex), self->m_iVQuantity);
  for (i = 0; i < self->m_iVQuantity; i++)
    {
      sv.m_kV = self->m_akVertex[i];
      sv.m_iIndex = i;
      g_array_append_val (kSVArray, sv);
    }
#if DEBUG
  s = g_string_new ("vertices (unsorted):\n");
  for (i = 0; i < self->m_iVQuantity; i++)
    {
      substring = WML_SortedVertex_to_string (&g_array_index (kSVArray, WML_SortedVertex, i));
      g_string_sprintfa (s, (i > 0) ? ", %s" : "%s", substring);
      free (substring);
    }
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif
  g_array_sort (kSVArray, WML_SortedVertex_cmp);
#if DEBUG
  s = g_string_new ("sorted by x:\n");
  for (i = 0; i < self->m_iVQuantity; i++)
    {
      substring = WML_SortedVertex_to_string (&g_array_index (kSVArray, WML_SortedVertex, i));
      g_string_sprintfa (s, (i > 0) ? ", %s" : "%s", substring);
      free (substring);
    }
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif

  /* remove duplicate points */
  i = self->m_iVQuantity - 1;
  while (i > 0)
    {
      if (WML_SortedVertex_eq
          (&g_array_index (kSVArray, WML_SortedVertex, i),
           &g_array_index (kSVArray, WML_SortedVertex, i - 1)))
        g_array_remove_index (kSVArray, i);
      i--;
    }
#if DEBUG
  s = g_string_new ("with duplicates removed:\n");
  for (i = 0; i < kSVArray->len; i++)
    {
      substring = WML_SortedVertex_to_string (&g_array_index (kSVArray, WML_SortedVertex, i));
      g_string_sprintfa (s, (i > 0) ? ", %s" : "%s", substring);
      free (substring);
    }
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif

  /* Compute convex hull incrementally.  The first and second vertices in
   * the hull are managed separately until at least one triangle is formed.
   * At that time an array is used to store the hull in counterclockwise
   * order. */
  self->m_iHullType = WML_HULL_POINT;

  self->m_kHull = g_array_new (FALSE, FALSE, sizeof (WML_SortedVertex));
  g_array_append_val (self->m_kHull, g_array_index (kSVArray, WML_SortedVertex, 0));
#if DEBUG
  s = g_string_new ("hull (start):\n");
  for (i = 0; i < self->m_kHull->len; i++)
    {
      substring = WML_SortedVertex_to_string (&g_array_index (self->m_kHull, WML_SortedVertex, i));
      g_string_sprintfa (s, (i > 0) ? ", %s" : "%s", substring);
      free (substring);
    }
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
  g_string_free (s, TRUE);
#endif
  for (i = 1; i < kSVArray->len; i++)
    {
      switch (self->m_iHullType)
        {
        case WML_HULL_POINT:
          self->m_iHullType = WML_HULL_LINEAR;
          g_array_append_val (self->m_kHull, g_array_index (kSVArray, WML_SortedVertex, i));
          break;
        case WML_HULL_LINEAR:
          WML_MergeLinear (self, &g_array_index (kSVArray, WML_SortedVertex, i));
          break;
        case WML_HULL_PLANAR:
          WML_MergePlanar (self, &g_array_index (kSVArray, WML_SortedVertex, i));
          break;
        }
#if DEBUG
      s = g_string_new (NULL);
      substring = WML_SortedVertex_to_string (&g_array_index (kSVArray, WML_SortedVertex, i));
      g_string_sprintf (s, "hull (after considering %s):\n", substring);
      free (substring);
      for (j = 0; j < self->m_kHull->len; j++)
        {
          substring =
            WML_SortedVertex_to_string (&g_array_index (self->m_kHull, WML_SortedVertex, j));
          g_string_sprintfa (s, (j > 0) ? ", %s" : "%s", substring);
          free (substring);
        }
      g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", s->str);
      g_string_free (s, TRUE);
#endif
    }

  /* construct index array for ordered vertices of convex hull */
  self->m_iHQuantity = self->m_kHull->len;
  self->m_aiHIndex = g_new (int, self->m_iHQuantity);
  for (i = 0; i < self->m_iHQuantity; i++)
    self->m_aiHIndex[i] = g_array_index (self->m_kHull, WML_SortedVertex, i).m_iIndex;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT WML_ByIncremental");
#endif

  return;
}



void
WML_RemoveCollinear (WML_ConvexHull2 * self)
{
  GArray *kHull = NULL;
  int i0, i1, i2;
  WML_OrderType iCT;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER WML_RemoveCollinear");
#endif

  if (self->m_iHQuantity <= 2)
    goto end;

  kHull = g_array_sized_new (FALSE, FALSE, sizeof (int), self->m_iHQuantity);
  for (i0 = self->m_iHQuantity - 1, i1 = 0, i2 = 1; i1 < self->m_iHQuantity;)
    {
      iCT = WML_CollinearTest (&(self->m_akVertex[self->m_aiHIndex[i0]]),
                               &(self->m_akVertex[self->m_aiHIndex[i1]]),
                               &(self->m_akVertex[self->m_aiHIndex[i2]]));

      if (iCT == WML_ORDER_POSITIVE || iCT == WML_ORDER_NEGATIVE)
        {
          /* points are not collinear */
          g_array_append_val (kHull, self->m_aiHIndex[i1]);
        }

      i0 = i1++;
      if (++i2 == self->m_iHQuantity)
        i2 = 0;
    }

  /* construct index array for ordered vertices of convex hull */
  self->m_iHQuantity = kHull->len;
  g_free (self->m_aiHIndex);
  self->m_aiHIndex = g_new (int, self->m_iHQuantity);
  memcpy (self->m_aiHIndex, kHull->data, self->m_iHQuantity * sizeof (int));

  g_array_free (kHull, TRUE);

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT WML_RemoveCollinear");
#endif

  return;
}



/**
 * Constructor.  ConvexHull2 does not take ownership of the input array.  The
 * application is responsible for deleting it.
 *
 * @param iVQuantity the number of points.
 * @param akVertex the points.
 * @param bIncremental if TRUE, the convex hull is computed by the
 *   "incremental" method; otherwise, the convex hull is computer by the
 *   "divide and conquer" method.
 * @returns a newly-allocated convex hull object.
 */
WML_ConvexHull2 *
WML_new_ConvexHull2 (int iVQuantity, WML_Vector2 * akVertex, gboolean bIncremental)
{
  WML_ConvexHull2 *self;

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER WML_new_ConvexHull2");
#endif

  self = g_new (WML_ConvexHull2, 1);

  self->m_iVQuantity = iVQuantity;
  self->m_akVertex = akVertex;

  if (bIncremental)
    WML_ByIncremental (self);
  else
    {
      /* Implementation to be ported when or if I get around it to. */
      /*
         WML_ByDivideAndConquer (self);
       */
      ;
    }

  if (self->m_iHQuantity >= 3)
    WML_RemoveCollinear (self);

#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT WML_new_ConvexHull2");
#endif

  return self;
}



/**
 * Deletes a convex hull from memory.
 *
 * @param self a convex hull.
 */
void
WML_free_ConvexHull2 (WML_ConvexHull2 * self)
{
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- ENTER WML_free_ConvexHull2");
#endif

  if (self == NULL)
    goto end;

  g_free (self->m_aiHIndex);
  g_free (self);

end:
#if DEBUG
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "----- EXIT WML_free_ConvexHull2");
#endif

  return;
}



/**
 * Returns the number of points on a convex hull
 *
 * @param self a convex hull.
 * @return the number of points on the convex hull.
 */
int
WML_ConvexHull2_GetQuantity (WML_ConvexHull2 * self)
{
  return self->m_iHQuantity;
}



/**
 * Returns the indices of the points that are on the convex hull.
 *
 * @param self a convex hull.
 * @return a list of point indices.
 */
int *
WML_ConvexHull2_GetIndices (WML_ConvexHull2 * self)
{
  return self->m_aiHIndex;
}

/* end of file WmlConvexHull2.c */
