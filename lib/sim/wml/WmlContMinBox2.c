/** @file WmlContMinBox2.c
 * Functions for finding minimum-area rectangles around 2-D points.
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

#include "wml/WmlContMinBox2.h"
#include <math.h>

#define F_NONE 0
#define F_LEFT 1
#define F_RIGHT 2
#define F_BOTTOM 3
#define F_TOP 4

extern const WML_Vector2 UNIT_X;
extern const WML_Vector2 UNIT_Y;



/**
 * Compute minimum area oriented box containing the specified points.  The
 * algorithm uses the rotating calipers method.  NOTE.  The input points must
 * form a convex polygon and be in counterclockwise order.
 *
 * @param iQuantity the number of points.
 * @param akPoint the points.
 * @return the minimum-area box.
 */
WML_Box2 *
WML_MinBox (int iQuantity, WML_Vector2 * akPoint)
{
  int iQuantityM1;
  WML_Vector2 *akEdge;
  gboolean *abVisited;
  int i;
  double fXMin, fXMax, fYMin, fYMax;
  int iLIndex, iRIndex, iBIndex, iTIndex;
  WML_Box2 *kBox;
  double fAreaDiv4, fMinAreaDiv4;
  WML_Vector2 kU, kV, kTmp, tmp;
  gboolean bDone;
  int iFlag;
  double fDot, fMaxDot;
  double fExtent0, fExtent1;

  /* The input points are V[0] through V[N-1] and are assumed to be the
   * vertices of a convex polygon that are counterclockwise ordered.  The
   * input points must not contain three consecutive collinear points. */

  /* Unit-length edge directions of convex polygon.  These could be
   * precomputed and passed to this routine if the application requires it. */
  iQuantityM1 = iQuantity - 1;
  akEdge = g_new (WML_Vector2, iQuantity);
  abVisited = g_new (gboolean, iQuantity);

  for (i = 0; i < iQuantityM1; i++)
    {
      WML_Vector2_assign (&akEdge[i], &akPoint[i + 1]);
      WML_Vector2_sub_inplace (&akEdge[i], &akPoint[i]);

      WML_Vector2_Normalize (&akEdge[i]);
      abVisited[i] = FALSE;
    }
  WML_Vector2_assign (&akEdge[iQuantityM1], &akPoint[0]);
  WML_Vector2_sub_inplace (&akEdge[iQuantityM1], &akPoint[iQuantityM1]);

  WML_Vector2_Normalize (&akEdge[iQuantityM1]);
  abVisited[iQuantityM1] = FALSE;

  /* Find the smallest axis-aligned box containing the points.  Keep track
   * of the extremum indices, L (left), R (right), B (bottom), and T (top)
   * so that the following constraints are met:
   *   V[L].X() <= V[i].X() for all i and V[(L+1)%N].X() > V[L].X()
   *   V[R].X() >= V[i].X() for all i and V[(R+1)%N].X() < V[R].X()
   *   V[B].Y() <= V[i].Y() for all i and V[(B+1)%N].Y() > V[B].Y()
   *   V[T].Y() >= V[i].Y() for all i and V[(T+1)%N].Y() < V[R].Y() */

  fXMin = fXMax = akPoint[0].X;
  fYMin = fYMax = akPoint[0].Y;
  iLIndex = iRIndex = iBIndex = iTIndex = 0;
  for (i = 1; i < iQuantity; i++)
    {
      if (akPoint[i].X <= fXMin)
        {
          fXMin = akPoint[i].X;
          iLIndex = i;
        }
      else if (akPoint[i].X >= fXMax)
        {
          fXMax = akPoint[i].X;
          iRIndex = i;
        }

      if (akPoint[i].Y <= fYMin)
        {
          fYMin = akPoint[i].Y;
          iBIndex = i;
        }
      else if (akPoint[i].Y >= fYMax)
        {
          fYMax = akPoint[i].Y;
          iTIndex = i;
        }
    }

  /* wrap-around tests to ensure the constraints mentioned above */
  if (akPoint[0].X <= fXMin)
    {
      fXMin = akPoint[0].X;
      iLIndex = 0;
    }
  else if (akPoint[0].X >= fXMax)
    {
      fXMax = akPoint[0].X;
      iRIndex = 0;
    }

  if (akPoint[0].Y <= fYMin)
    {
      fYMin = akPoint[0].Y;
      iBIndex = 0;
    }
  else if (akPoint[0].Y >= fYMax)
    {
      fYMax = akPoint[0].Y;
      iTIndex = 0;
    }

  /* dimensions of axis-aligned box (extents store width and height for now) */
  kBox = WML_new_Box2 ();
  WML_Box2_Center (kBox)->X = 0.5 * (fXMin + fXMax);
  WML_Box2_Center (kBox)->Y = 0.5 * (fYMin + fYMax);
  WML_Vector2_assign (WML_Box2_Axis (kBox, 0), &UNIT_X);
  WML_Vector2_assign (WML_Box2_Axis (kBox, 1), &UNIT_Y);
  kBox->m_afExtent[0] = 0.5 * (fXMax - fXMin);
  kBox->m_afExtent[1] = 0.5 * (fYMax - fYMin);
  fMinAreaDiv4 = WML_Box2_Extent (kBox, 0) * WML_Box2_Extent (kBox, 1);

  /* rotating calipers algorithm */
  WML_Vector2_assign (&kU, &UNIT_X);
  WML_Vector2_assign (&kV, &UNIT_Y);

  bDone = FALSE;
  while (!bDone)
    {
      /* determine edge that forms smallest angle with current box edges */
      iFlag = F_NONE;
      fMaxDot = 0;

      fDot = WML_Vector2_Dot (&kU, &akEdge[iBIndex]);
      if (fDot > fMaxDot)
        {
          fMaxDot = fDot;
          iFlag = F_BOTTOM;
        }

      fDot = WML_Vector2_Dot (&kV, &akEdge[iRIndex]);
      if (fDot > fMaxDot)
        {
          fMaxDot = fDot;
          iFlag = F_RIGHT;
        }

      WML_Vector2_negate (&kU);
      fDot = WML_Vector2_Dot (&kU, &akEdge[iTIndex]);
      if (fDot > fMaxDot)
        {
          fMaxDot = fDot;
          iFlag = F_TOP;
        }

      WML_Vector2_negate (&kV);
      fDot = WML_Vector2_Dot (&kV, &akEdge[iLIndex]);
      if (fDot > fMaxDot)
        {
          fMaxDot = fDot;
          iFlag = F_LEFT;
        }

      switch (iFlag)
        {
        case F_BOTTOM:
          if (abVisited[iBIndex])
            bDone = TRUE;
          else
            {
              /* compute box axes with E[B] as an edge */
              WML_Vector2_assign (&kU, &akEdge[iBIndex]);
              WML_Vector2_assign (&kU, &kV);
              WML_Vector2_negate (&kV);
              kV = WML_Vector2_Perp (&kV);

              /* mark edge visited and rotate the calipers */
              abVisited[iBIndex] = TRUE;
              if (++iBIndex == iQuantity)
                iBIndex = 0;
            }
          break;
        case F_RIGHT:
          if (abVisited[iRIndex])
            bDone = TRUE;
          else
            {
              /* compute dimensions of box with E[R] as an edge */
              WML_Vector2_assign (&kV, &akEdge[iRIndex]);
              kU = WML_Vector2_Perp (&kV);

              /* mark edge visited and rotate the calipers */
              abVisited[iRIndex] = TRUE;
              if (++iRIndex == iQuantity)
                iRIndex = 0;
            }
          break;
        case F_TOP:
          if (abVisited[iTIndex])
            bDone = TRUE;
          else
            {
              /* compute dimensions of box with E[T] as an edge */
              WML_Vector2_assign (&kU, &akEdge[iTIndex]);
              WML_Vector2_negate (&kU);
              WML_Vector2_assign (&kV, &kU);
              WML_Vector2_negate (&kV);
              kV = WML_Vector2_Perp (&kV);

              /* mark edge visited and rotate the calipers */
              abVisited[iTIndex] = TRUE;
              if (++iTIndex == iQuantity)
                iTIndex = 0;
            }
          break;
        case F_LEFT:
          if (abVisited[iLIndex])
            bDone = TRUE;
          else
            {
              /* compute dimensions of box with E[L] as an edge */
              WML_Vector2_assign (&kV, &akEdge[iLIndex]);
              WML_Vector2_negate (&kV);
              kU = WML_Vector2_Perp (&kV);

              /* mark edge visited and rotate the calipers */
              abVisited[iLIndex] = TRUE;
              if (++iLIndex == iQuantity)
                iLIndex = 0;
            }
          break;
        case F_NONE:
          /* polygon is a rectangle */
          bDone = TRUE;
          break;
        }

      tmp = WML_Vector2_sub (&akPoint[iRIndex], &akPoint[iLIndex]);
      fExtent0 = 0.5 * WML_Vector2_Dot (&kU, &tmp);
      tmp = WML_Vector2_sub (&akPoint[iTIndex], &akPoint[iBIndex]);
      fExtent1 = 0.5 * WML_Vector2_Dot (&kV, &tmp);
      fAreaDiv4 = fExtent0 * fExtent1;
      if (fAreaDiv4 < fMinAreaDiv4)
        {
          fMinAreaDiv4 = fAreaDiv4;
          WML_Vector2_assign (WML_Box2_Axis (kBox, 0), &kU);
          WML_Vector2_assign (WML_Box2_Axis (kBox, 1), &kV);
          kBox->m_afExtent[0] = fExtent0;
          kBox->m_afExtent[1] = fExtent1;

          /* compute box center */
          kTmp = WML_Vector2_add (&akPoint[iTIndex], &akPoint[iBIndex]);
          WML_Vector2_mul_inplace (&kTmp, 0.5);
          WML_Vector2_sub_inplace (&kTmp, &akPoint[iLIndex]);

          WML_Vector2_assign (WML_Box2_Center (kBox), &akPoint[iLIndex]);
          tmp = WML_Vector2_mul (WML_Box2_Axis (kBox, 0), fExtent0);
          WML_Vector2_add_inplace (WML_Box2_Center (kBox), &tmp);
          tmp = WML_Vector2_mul (WML_Box2_Axis (kBox, 1),
                                 WML_Vector2_Dot (WML_Box2_Axis (kBox, 1), &kTmp));
          WML_Vector2_add_inplace (WML_Box2_Center (kBox), &tmp);
        }
    }

  g_free (abVisited);
  g_free (akEdge);
  return kBox;
}



/**
 * The slower method for computing the minimum area oriented box that does not
 * maintain the extremal points supporting the box (like rotating calipers
 * does).  The input points must also form a convex polygon, but the order may
 * be counterclockwise or clockwise.
 *
 * @param iQuantity the number of points.
 * @param akPoint the points.
 * @return the minimum-area box.
 */
WML_Box2 *
WML_MinBoxOrderNSqr (int iQuantity, WML_Vector2 * akPoint)
{
  double fMinAreaDiv4 = 3.40282347e+38F;
  WML_Box2 *kBox;
  int i1, i0;
  WML_Vector2 kU0, kU1;
  double fS0, fS1, fT0, fT1;
  int j;
  WML_Vector2 kDiff;
  double fTest;
  double fExtent0, fExtent1, fAreaDiv4;
  WML_Vector2 tmp;

  kBox = WML_new_Box2 ();
  for (i1 = 0, i0 = iQuantity - 1; i1 < iQuantity; i0 = i1, i1++)
    {
      kU0 = WML_Vector2_sub (&akPoint[i1], &akPoint[i0]);
      WML_Vector2_Normalize (&kU0);

      WML_Vector2_assign (&kU1, &kU0);
      WML_Vector2_negate (&kU1);
      kU1 = WML_Vector2_Perp (&kU1);

      fS0 = 0;
      fT0 = 0;
      fS1 = 0;
      fT1 = 0;
      for (j = 1; j < iQuantity; j++)
        {
          kDiff = WML_Vector2_sub (&akPoint[j], &akPoint[0]);
          fTest = WML_Vector2_Dot (&kU0, &kDiff);
          if (fTest < fS0)
            fS0 = fTest;
          else if (fTest > fS1)
            fS1 = fTest;

          fTest = WML_Vector2_Dot (&kU1, &kDiff);
          if (fTest < fT0)
            fT0 = fTest;
          else if (fTest > fT1)
            fT1 = fTest;
        }

      fExtent0 = 0.5 * (fS1 - fS0);
      fExtent1 = 0.5 * (fT1 - fT0);
      fAreaDiv4 = fExtent0 * fExtent1;
      if (fAreaDiv4 < fMinAreaDiv4)
        {
          fMinAreaDiv4 = fAreaDiv4;
          WML_Vector2_assign (WML_Box2_Axis (kBox, 0), &kU0);
          WML_Vector2_assign (WML_Box2_Axis (kBox, 1), &kU1);
          kBox->m_afExtent[0] = fExtent0;
          kBox->m_afExtent[1] = fExtent1;

          WML_Vector2_assign (WML_Box2_Center (kBox), &akPoint[0]);

          WML_Vector2_assign (&tmp, &kU0);
          WML_Vector2_mul_inplace (&tmp, 0.5 * (fS0 + fS1));
          WML_Vector2_add_inplace (WML_Box2_Center (kBox), &tmp);

          WML_Vector2_assign (&tmp, &kU1);
          WML_Vector2_mul_inplace (&tmp, 0.5 * (fT0 + fT1));
          WML_Vector2_add_inplace (WML_Box2_Center (kBox), &tmp);
        }
    }

  return kBox;
}

/* end of file WmlContMinBox2.c */
