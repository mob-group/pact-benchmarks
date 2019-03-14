*> \brief \b AB_SLAPY2 returns sqrt(x2+y2).
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_SLAPY2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_SLAPY2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_SLAPY2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_SLAPY2.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       REAL             FUNCTION AB_SLAPY2( X, Y )
*
*       .. Scalar Arguments ..
*       REAL               X, Y
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
*> overflow.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] X
*> \verbatim
*>          X is REAL
*> \endverbatim
*>
*> \param[in] Y
*> \verbatim
*>          Y is REAL
*>          X and Y specify the values x and y.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date June 2017
*
*> \ingroup OTHERauxiliary
*
*  =====================================================================
      REAL             FUNCTION AB_SLAPY2( X, Y )
*
*  -- LAPACK auxiliary routine (version 3.7.1) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2017
*
*     .. Scalar Arguments ..
      REAL               X, Y
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      REAL               W, XABS, YABS, Z
      LOGICAL            X_IS_NAN, Y_IS_NAN
*     ..
*     .. External Functions ..
      LOGICAL            AB_SISNAN
      EXTERNAL           AB_SISNAN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     ..
*     .. Executable Statements ..
*
      X_IS_NAN = AB_SISNAN( X )
      Y_IS_NAN = AB_SISNAN( Y )
      IF ( X_IS_NAN ) AB_SLAPY2 = X
      IF ( Y_IS_NAN ) AB_SLAPY2 = Y
*
      IF ( .NOT.( X_IS_NAN.OR.Y_IS_NAN ) ) THEN
         XABS = ABS( X )
         YABS = ABS( Y )
         W = MAX( XABS, YABS )
         Z = MIN( XABS, YABS )
         IF( Z.EQ.ZERO ) THEN
            AB_SLAPY2 = W
         ELSE
            AB_SLAPY2 = W*SQRT( ONE+( Z / W )**2 )
         END IF
      END IF
      RETURN
*
*     End of AB_SLAPY2
*
      END
