*> \brief \b AB_ZSLECT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       LOGICAL          FUNCTION AB_ZSLECT( Z )
*
*       .. Scalar Arguments ..
*       COMPLEX*16         Z
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZSLECT returns .TRUE. if the eigenvalue Z is to be selected,
*> otherwise it returns .FALSE.
*> It is used by AB_ZCHK41 to test if AB_ZGEES successfully sorts eigenvalues,
*> and by AB_ZCHK43 to test if AB_AB_ZGEESX successfully sorts eigenvalues.
*>
*> The common block /SSLCT/ controls how eigenvalues are selected.
*> If SELOPT = 0, then AB_ZSLECT return .TRUE. when real(Z) is less than
*> zero, and .FALSE. otherwise.
*> If SELOPT is at least 1, AB_ZSLECT returns SELVAL(SELOPT) and adds 1
*> to SELOPT, cycling back to 1 at SELMAX.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] Z
*> \verbatim
*>          Z is COMPLEX*16
*>          The eigenvalue Z.
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
*> \date June 2016
*
*> \ingroup complex16_eig
*
*  =====================================================================
      LOGICAL          FUNCTION AB_ZSLECT( Z )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*     .. Scalar Arguments ..
      COMPLEX*16         Z
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   RMIN, X
*     ..
*     .. Scalars in Common ..
      INTEGER            SELDIM, SELOPT
*     ..
*     .. Arrays in Common ..
      LOGICAL            SELVAL( 20 )
      DOUBLE PRECISION   SELWI( 20 ), SELWR( 20 )
*     ..
*     .. Common blocks ..
      COMMON             / SSLCT / SELOPT, SELDIM, SELVAL, SELWR, SELWI
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX
*     ..
*     .. Executable Statements ..
*
      IF( SELOPT.EQ.0 ) THEN
         AB_ZSLECT = ( DBLE( Z ).LT.ZERO )
      ELSE
         RMIN = ABS( Z-DCMPLX( SELWR( 1 ), SELWI( 1 ) ) )
         AB_ZSLECT = SELVAL( 1 )
         DO 10 I = 2, SELDIM
            X = ABS( Z-DCMPLX( SELWR( I ), SELWI( I ) ) )
            IF( X.LE.RMIN ) THEN
               RMIN = X
               AB_ZSLECT = SELVAL( I )
            END IF
   10    CONTINUE
      END IF
      RETURN
*
*     End of AB_ZSLECT
*
      END
