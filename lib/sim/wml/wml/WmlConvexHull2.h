/** @file WmlConvexHull2.h
 * Convex hulls of 2-D points.
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

#ifndef WMLCONVEXHULL2_H
#define WMLCONVEXHULL2_H

#include "WmlVector2.h"
#include <glib.h>

/** hull dimensions */
typedef enum
{
  WML_HULL_POINT,
  WML_HULL_LINEAR,
  WML_HULL_PLANAR
}
WML_HullType;



/** for collinearity tests */
typedef enum
{
  WML_ORDER_POSITIVE,
  WML_ORDER_NEGATIVE,
  WML_ORDER_COLLINEAR_LEFT,
  WML_ORDER_COLLINEAR_RIGHT,
  WML_ORDER_COLLINEAR_CONTAIN
}
WML_OrderType;



typedef struct
{
  /* hull stored in counterclockwise order */
  /* vertex information */
  int m_iVQuantity;
  WML_Vector2 *m_akVertex;

  /* indices for ordered vertices of hull */
  int m_iHQuantity;
  int *m_aiHIndex;

  /* hull information */
  WML_HullType m_iHullType;
  GArray *m_kHull;

}
WML_ConvexHull2;



/* Prototypes */

/* Construction and destruction.  ConvexHull2 does not take ownership
 * of the input array.  The application is responsible for deleting it. */
WML_ConvexHull2 *WML_new_ConvexHull2 (int iVQuantity,
                                      WML_Vector2 * akVertex, gboolean bIncremental);
void WML_free_ConvexHull2 (WML_ConvexHull2 *);

int WML_ConvexHull2_GetQuantity (WML_ConvexHull2 *);
int *WML_ConvexHull2_GetIndices (WML_ConvexHull2 *);
gboolean WML_ConvexHull2_ContainsPoint (WML_ConvexHull2 *, WML_Vector2 *);


/*
template <class Real>
class WML_ITEM ConvexHull2
{
public:

    // The 'collinear epsilon' is used to test if three points P0, P1, and P2
    // are collinear.  If A = P1-P0 and B = P2-P0, the points are collinear
    // in theory if d = A.x*B.y-A.y*B.x = 0.  For numerical robustness, the
    // test is implemented as |d|^2 <= e*|A|^2*|B|^2 where e is the collinear
    // epsilon.  The idea is that d = |Cross((A,0),(B,0))| = |A|*|B|*|sin(t)|
    // where t is the angle between A and B.  Therefore, the comparison is
    // really |sin(t)|^2 <= e, a relative error test.  The default e = 1e-06.
    static Real COLLINEAR_EPSILON;

protected:
    // two different methods to compute convex hull
    void ByDivideAndConquer ();
    void ByIncremental ();

    // remove collinear points on hull
    void RemoveCollinear ();


    typedef typename std::vector<SortedVertex> SVArray;



    int CollinearTest (const Vector2<Real>& rkP, const Vector2<Real>& rkQ0,
        const Vector2<Real>& rkQ1) const;

    // construct convex hull using divide-and-conquer
    void GetHull (int i0, int i1, const SVArray& rkSVArray, SVArray& rkHull);
    void Merge (SVArray& rkLHull, SVArray& rkRHull, SVArray& rkHull);
    void MergeLinear (const SortedVertex& rkP, SVArray& rkHull);
    void GetTangent (const SVArray& rkLHull, const SVArray& rkRHull,
        int& riL, int& riR);

    // construct convex hull incrementally
    void MergeLinear (const SortedVertex& rkP);
    void MergePlanar (const SortedVertex& rkP);


};

typedef ConvexHull2<float> ConvexHull2f;
typedef ConvexHull2<double> ConvexHull2d;
*/

#endif /* !WMLCONVEXHULL2_H */
