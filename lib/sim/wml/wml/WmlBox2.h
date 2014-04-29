/** @file WmlBox2.h
 * 2-Dimensional boxes.
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

#ifndef WMLBOX2_H
#define WMLBOX2_H

#include "WmlVector2.h"



/** A two-dimensional box. */
typedef struct
{
  WML_Vector2 m_kCenter;
  WML_Vector2 m_akAxis[2];
  double m_afExtent[2];
}
WML_Box2;



/* Prototypes */
WML_Box2 *WML_new_Box2 (void);
WML_Box2 *WML_clone_Box2 (WML_Box2 *);
char *WML_Box2_to_string (WML_Box2 *);
void WML_free_Box2 (WML_Box2 *);

/**
 * Returns the center of a Box.
 *
 * @param B a box.
 * @return the center.
 */
#define WML_Box2_Center(B) (&(B)->m_kCenter)

WML_Vector2 *WML_Box2_Axis (WML_Box2 *, int i);
WML_Vector2 *WML_Box2_Axes (WML_Box2 *);
double WML_Box2_Extent (WML_Box2 *, int i);
double *WML_Box2_Extents (WML_Box2 *);
void WML_Box2_ComputeVertices (WML_Box2 *, WML_Vector2 akVertex[4]);

#endif /* !WMLBOX2_H */
