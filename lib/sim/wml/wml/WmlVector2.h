/** @file WmlVector2.h
 * 2-Dimensional vectors.
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

#ifndef WMLVECTOR2_H
#define WMLVECTOR2_H

/* #include "WmlVector.h" */
#include <glib.h>



/** A two-dimensional vector. */
typedef struct
{
  double X, Y;
}
WML_Vector2;



/* Prototypes */

WML_Vector2 *WML_new_Vector2 (double fX, double fY);
WML_Vector2 *WML_new_Vector2v (const WML_Vector2 * rkV);
char *WML_Vector2_to_string (WML_Vector2 *);
void WML_free_Vector2 (WML_Vector2 *);

gboolean WML_Vector2_eq (WML_Vector2 *, WML_Vector2 *);
WML_Vector2 WML_Vector2_add (WML_Vector2 *, WML_Vector2 *);
WML_Vector2 WML_Vector2_sub (WML_Vector2 *, WML_Vector2 *);
WML_Vector2 WML_Vector2_mul (WML_Vector2 *, double);
void WML_Vector2_add_inplace (WML_Vector2 *, WML_Vector2 *);
void WML_Vector2_sub_inplace (WML_Vector2 *, WML_Vector2 *);
void WML_Vector2_mul_inplace (WML_Vector2 *, double);
#define WML_Vector2_negate(V) WML_Vector2_mul_inplace(V,-1)
WML_Vector2 *WML_Vector2_assign (WML_Vector2 * self, WML_Vector2 * rkV);

WML_Vector2 WML_Vector2_Perp (WML_Vector2 *);
double WML_Vector2_Length (WML_Vector2 *);
double WML_Vector2_SquaredLength (WML_Vector2 *);
double WML_Vector2_Dot (WML_Vector2 *, WML_Vector2 *);
double WML_Vector2_Normalize (WML_Vector2 *);
double WML_Vector2_Kross (WML_Vector2 *, WML_Vector2 *);



/*
class WML_ITEM Vector2 : public Vector<2,Real>
{
public:


    // returns (y,-x)/sqrt(x*x+y*y)
    Vector2 UnitPerp () const;

    // NOTE.  These exist to support template code that takes vectors of
    // dimensions 2 or 3.  In 2D, the input vector is ignored, but allows
    // the correct signature matching in the template code.
    Vector2 Cross (const Vector2& rkV) const;
    Vector2 UnitCross (const Vector2& rkV) const;

    // Gram-Schmidt orthonormalization.  Take linearly independent vectors U
    // and V and compute an orthonormal set (unit length, mutually
    // perpendicular).
    static void Orthonormalize (Vector2& rkU, Vector2& rkV);

    // Input V must be initialized to a nonzero vector, output is {U,V}, an
    // orthonormal basis.  A hint is provided about whether or not V is
    // already unit length.
    static void GenerateOrthonormalBasis (Vector2& rkU, Vector2& rkV,
        bool bUnitLengthV);
};
*/

#endif /* !WMLVECTOR2_H */
