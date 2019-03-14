*> \brief \b AB_ZLCTSX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       LOGICAL          FUNCTION AB_ZLCTSX( ALPHA, BETA )
*
*       .. Scalar Arguments ..
*       COMPLEX*16         ALPHA, BETA
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> This function is used to determine what eigenvalues will be
*> selected.  If this is part of the test driver AB_ZDRGSX, do not
*> change the code UNLESS you are testing input examples and not
*> using the built-in examples.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] ALPHA
*> \verbatim
*>          ALPHA is COMPLEX*16
*> \endverbatim
*>
*> \param[in] BETA
*> \verbatim
*>          BETA is COMPLEX*16
*>
*>          parameters to decide whether the pair (ALPHA, BETA) is
*>          selected.
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
*> \date December 2016
*
*> \ingroup complex16_eig
*
*  =====================================================================
      LOGICAL          FUNCTION AB_ZLCTSX( ALPHA, BETA )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      COMPLEX*16         ALPHA, BETA
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
*     DOUBLE PRECISION               ZERO
*     PARAMETER          ( ZERO = 0.0E+0 )
*     COMPLEX*16            CZERO
*     PARAMETER          ( CZERO = ( 0.0E+0, 0.0E+0 ) )
*     ..
*     .. Scalars in Common ..
      LOGICAL            FS
      INTEGER            I, M, MPLUSN, N
*     ..
*     .. Common blocks ..
      COMMON             / MN / M, N, MPLUSN, I, FS
*     ..
*     .. Save statement ..
      SAVE
*     ..
*     .. Executable Statements ..
*
      IF( FS ) THEN
         I = I + 1
         IF( I.LE.M ) THEN
            AB_ZLCTSX = .FALSE.
         ELSE
            AB_ZLCTSX = .TRUE.
         END IF
         IF( I.EQ.MPLUSN ) THEN
            FS = .FALSE.
            I = 0
         END IF
      ELSE
         I = I + 1
         IF( I.LE.N ) THEN
            AB_ZLCTSX = .TRUE.
         ELSE
            AB_ZLCTSX = .FALSE.
         END IF
         IF( I.EQ.MPLUSN ) THEN
            FS = .TRUE.
            I = 0
         END IF
      END IF
*
*      IF( BETA.EQ.CZERO ) THEN
*         AB_ZLCTSX = ( DBLE( ALPHA ).GT.ZERO )
*      ELSE
*         AB_ZLCTSX = ( DBLE( ALPHA/BETA ).GT.ZERO )
*      END IF
*
      RETURN
*
*     End of AB_ZLCTSX
*
      END
