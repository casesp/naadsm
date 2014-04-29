/** @file WmlContMinBox2.h
 * Interface for WmlContMinBox2.c.
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

#ifndef WMLCONTMINBOX2_H
#define WMLCONTMINBOX2_H

#include "WmlBox2.h"



/* Prototypes. */
WML_Box2 *WML_MinBox (int iQuantity, WML_Vector2 * akPoint);
WML_Box2 *WML_MinBoxOrderNSqr (int iQuantity, WML_Vector2 * akPoint);

#endif /* !WMLCONTMINBOX2_H */
