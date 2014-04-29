/** @file WmlContPointInPolygon2.h
 * Interface for WmlContPointInPolygon2.c.
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

#ifndef WMLCONTPOINTINPOLYGON2_H
#define WMLCONTPOINTINPOLYGON2_H

/* Given a polygon as an ordered list of vertices (x[i],y[i]) for 0 <= i < N
 * and a test point (xt,yt), return 'true' if (xt,yt) is in the polygon and
 * 'false' if it is not.  All queries require that the number of vertices
 * satisfies N >= 3. */

#include "WmlVector2.h"
#include <glib.h>

/* Prototypes */

/* general polygons */
gboolean WML_PointInPolygon (int iQuantity, WML_Vector2 * akV, WML_Vector2 * rkP);

/* Algorithms for convex polygons.  The input polygons must have vertices in
 * counterclockwise order. */

/* O(N) algorithm */
gboolean WML_PointInConvexOrderN (int iQuantity, WML_Vector2 * akV, WML_Vector2 * rkP);

/* O(log N) algorithm, uses bisection and recursion */
gboolean WML_PointInConvexOrderLogN (int iQuantity, WML_Vector2 * akV, WML_Vector2 * rkP);

/* O(log N) algorithm but hard-coded for the specified size.  The number at
 * the end of the function name is the number of vertices in the convex
 * polygon. */
gboolean WML_PointInConvex4 (WML_Vector2 * akV, WML_Vector2 * rkP);

#endif /* !WMLCONTPOINTINPOLYGON2_H */
