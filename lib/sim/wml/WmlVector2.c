/** @file WmlVector2.c
 * Functions for 2-dimensional vectors.
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
#  include <string.h>
#endif

#include "wml/WmlVector2.h"
#include <glib.h>

#if HAVE_MATH_H
#  include <math.h>
#endif

#define EPSILON 1e-06



/* special vectors */
const WML_Vector2 ZERO = { 0.0, 0.0 };
const WML_Vector2 UNIT_X = { 1.0, 0.0 };
const WML_Vector2 UNIT_Y = { 0.0, 1.0 };



/**
 * Creates a new Vector.
 *
 * @param fX the <i>x</i>-coordinate.
 * @param fY the <i>y</i>-coordinate.
 * @return a newly-allocated Vector object.
 */
WML_Vector2 *
WML_new_Vector2 (double fX, double fY)
{
  WML_Vector2 *self;

  self = g_new (WML_Vector2, 1);

  self->X = fX;
  self->Y = fY;

  return self;
}



/**
 * Creates a new Vector.
 *
 * @param rkV a Vector.
 * @return a newly-allocated Vector object that is a copy of <i>rkV</i>.
 */
WML_Vector2 *
WML_new_Vector2v (const WML_Vector2 * rkV)
{
  WML_Vector2 *self;

  self = g_new (WML_Vector2, 1);

  memcpy (self, rkV, sizeof (WML_Vector2));

  return self;
}



/**
 * Returns a text representation of a Vector.
 *
 * @param self a vector.
 * @return a string.
 */
char *
WML_Vector2_to_string (WML_Vector2 * self)
{
  GString *s;
  char *chararray;

  s = g_string_new (NULL);
  g_string_sprintf (s, "<%g,%g>", self->X, self->Y);
  /* don't return the wrapper object */
  chararray = s->str;
  g_string_free (s, FALSE);
  return chararray;
}



/**
 * Deletes a Vector from memory.
 *
 * @param self a vector.
 */
void
WML_free_Vector2 (WML_Vector2 * self)
{
  if (self == NULL)
    return;

  g_free (self);
}



/**
 * Assigns one Vector to another.
 *
 * @param self the destination vector.
 * @param rkV the source vector.
 * @return the destination vector.
 */
WML_Vector2 *
WML_Vector2_assign (WML_Vector2 * self, WML_Vector2 * rkV)
{
  memcpy (self, rkV, sizeof (WML_Vector2));
  return self;
}



/**
 * Tests whether two Vectors are equal.
 *
 * @param self a vector.
 * @param rkV a vector.
 * @return TRUE if the vectors are identical; FALSE otherwise.
 */
gboolean
WML_Vector2_eq (WML_Vector2 * self, WML_Vector2 * rkV)
{
  return memcmp (self, rkV, sizeof (WML_Vector2)) == 0;
}



/**
 * Adds one vector to another.
 *
 * @param self the first vector.
 * @param rkV the second vector.
 * @return <i>self</i> + <i>rkV</i>.
 */
WML_Vector2
WML_Vector2_add (WML_Vector2 * self, WML_Vector2 * rkV)
{
  WML_Vector2 kSum;

  kSum.X = self->X + rkV->X;
  kSum.Y = self->Y + rkV->Y;
  return kSum;
}



/**
 * Adds one vector from another, leaving the result in the first vector.
 *
 * @param self the first vector.
 * @param rkV the second vector.
 */
void
WML_Vector2_add_inplace (WML_Vector2 * self, WML_Vector2 * rkV)
{
  self->X += rkV->X;
  self->Y += rkV->Y;
}




/**
 * Subtracts one vector from another.
 *
 * @param self the first vector.
 * @param rkV the second vector.
 * @return <i>self</i> - <i>rkV</i>.
 */
WML_Vector2
WML_Vector2_sub (WML_Vector2 * self, WML_Vector2 * rkV)
{
  WML_Vector2 kDiff;

  kDiff.X = self->X - rkV->X;
  kDiff.Y = self->Y - rkV->Y;
  return kDiff;
}



/**
 * Subtracts one vector from another, leaving the result in the first vector.
 *
 * @param self the first vector.
 * @param rkV the second vector.
 */
void
WML_Vector2_sub_inplace (WML_Vector2 * self, WML_Vector2 * rkV)
{
  self->X -= rkV->X;
  self->Y -= rkV->Y;
}



/**
 * Multiplies a vector by a scalar.
 *
 * @param self the first vector.
 * @param m the number to scale by.
 * @return <i>self</i> * <i>m</i>.
 */
WML_Vector2
WML_Vector2_mul (WML_Vector2 * self, double m)
{
  WML_Vector2 kProd;

  kProd.X = self->X * m;
  kProd.Y = self->Y * m;
  return kProd;
}



/**
 * Multiplies a vector by a scalar, leaving the result in the vector.
 *
 * @param self a vector.
 * @param m the number to scale by.
 */
void
WML_Vector2_mul_inplace (WML_Vector2 * self, double m)
{
  self->X *= m;
  self->Y *= m;
}



/**
 * Returns a vector perpendicular to the given one.
 *
 * @param self a vector (<i>x</i>,<i>y</i>).
 * @return the vector (<i>y</i>,-<i>x</i>).
 */
WML_Vector2
WML_Vector2_Perp (WML_Vector2 * self)
{
  WML_Vector2 kPerp;

  kPerp.X = self->Y;
  kPerp.Y = -(self->X);
  return kPerp;
}



/**
 * Finds the length of a vector.
 *
 * @param self a vector.
 * @return the length.
 */
double
WML_Vector2_Length (WML_Vector2 * self)
{
  double fSqrLen = 0.0;

  fSqrLen = self->X * self->X + self->Y * self->Y;
  return sqrt (fSqrLen);
}



/**
 * Finds the squared length of a vector.
 *
 * @param self a vector.
 * @return the squared length.
 */
double
WML_Vector2_SquaredLength (WML_Vector2 * self)
{
  double fSqrLen = 0.0;

  fSqrLen = self->X * self->X + self->Y * self->Y;
  return fSqrLen;
}



/**
 * Finds the dot product of two vectors.
 *
 * @param self the first vector.
 * @param rkV the second vector.
 * @return <i>self</i> \htmlonly &middot; \endhtmlonly \latexonly $\dot$ \endlatexonly <i>rkV</i>.
 */
double
WML_Vector2_Dot (WML_Vector2 * self, WML_Vector2 * rkV)
{
  return self->X * rkV->X + self->Y * rkV->Y;
}



/**
 * Normalizes a vector.
 *
 * @param self a vector.
 * @return the length of the vector before normalization.
 */
double
WML_Vector2_Normalize (WML_Vector2 * self)
{
  double fLength, fInvLength;

  fLength = WML_Vector2_Length (self);
  if (fLength > EPSILON)
    {
      fInvLength = 1.0 / fLength;
      self->X *= fInvLength;
      self->Y *= fInvLength;
    }
  else
    {
      fLength = 0;
      self->X = self->Y = 0;
    }

  return fLength;
}



/**
 * Finds the cross product of two vectors.
 *
 * @param self the first vector.
 * @param rkV the second vector.
 * @return Cross((x,y,0),(V.x,V.y,0)) = x*V.y - y*V.x.
 */
double
WML_Vector2_Kross (WML_Vector2 * self, WML_Vector2 * rkV)
{
  return self->X * rkV->Y - self->Y * rkV->X;
}



/*
template <class Real>
Vector2<Real> Vector2<Real>::UnitPerp () const
{
    Vector2 kPerp(m_afTuple[1],-m_afTuple[0]);
    kPerp.Normalize();
    return kPerp;
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Vector2<Real>::Cross (const Vector2&) const
{
    return Vector2(m_afTuple[1],-m_afTuple[0]);
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Vector2<Real>::UnitCross (const Vector2&) const
{
    Vector2 kPerp(m_afTuple[1],-m_afTuple[0]);
    kPerp.Normalize();
    return kPerp;
}
//----------------------------------------------------------------------------
template <class Real>
void Vector2<Real>::Orthonormalize (Vector2& rkU, Vector2& rkV)
{
    // If the input vectors are v0 and v1, then the Gram-Schmidt
    // orthonormalization produces vectors u0 and u1 as follows,
    //
    //   u0 = v0/|v0|
    //   u1 = (v1-(u0*v1)u0)/|v1-(u0*v1)u0|
    //
    // where |A| indicates length of vector A and A*B indicates dot
    // product of vectors A and B.

    // compute u0
    rkU.Normalize();

    // compute u1
    Real fDot0 = rkU.Dot(rkV); 
    rkV -= fDot0*rkU;
    rkV.Normalize();
}
//----------------------------------------------------------------------------
template <class Real>
void Vector2<Real>::GenerateOrthonormalBasis (Vector2& rkU, Vector2& rkV,
    bool bUnitLengthV)
{
    if ( !bUnitLengthV )
        rkV.Normalize();

    rkU = rkV.Perp();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Vector<2,float>;

#ifdef WML_USING_VC6
template WML_ITEM Vector<2,float> operator* (float,
    const Vector<2,float>&);
#else
template WML_ITEM Vector<2,float> operator*<2,float> (float,
    const Vector<2,float>&);
#endif

template class WML_ITEM Vector2<float>;
const Vector2f Vector2f::ZERO(0.0f,0.0f);
const Vector2f Vector2f::UNIT_X(1.0f,0.0f);
const Vector2f Vector2f::UNIT_Y(0.0f,1.0f);

template class WML_ITEM Vector<2,double>;

#ifdef WML_USING_VC6
template WML_ITEM Vector<2,double> operator* (double,
    const Vector<2,double>&);
#else
template WML_ITEM Vector<2,double> operator*<2,double> (double,
    const Vector<2,double>&);
#endif

template class WML_ITEM Vector2<double>;
}
*/

/* end of file WmlVector2.c */
