*> \brief \b DGET10
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE DGET10( M, N, A, LDA, B, LDB, WORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, LDB, M, N
*       DOUBLE PRECISION   RESULT
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DGET10 compares two matrices A and B and computes the ratio
*> RESULT = norm( A - B ) / ( norm(A) * M * EPS )
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrices A and B.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrices A and B.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          The m by n matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[in] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension (LDB,N)
*>          The m by n matrix B.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (M)
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is DOUBLE PRECISION
*>          RESULT = norm( A - B ) / ( norm(A) * M * EPS )
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
*> \ingroup double_eig
*
*  =====================================================================
      SUBROUTINE DGET10( M, N, A, LDA, B, LDB, WORK, RESULT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDB, M, N
      DOUBLE PRECISION   RESULT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J
      DOUBLE PRECISION   ANORM, EPS, UNFL, WNORM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DASUM, AB_DLAMCH, DLANGE
      EXTERNAL           DASUM, AB_DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 ) THEN
         RESULT = ZERO
         RETURN
      END IF
*
      UNFL = AB_DLAMCH( 'Safe minimum' )
      EPS = AB_DLAMCH( 'Precision' )
*
      WNORM = ZERO
      DO 10 J = 1, N
         CALL DCOPY( M, A( 1, J ), 1, WORK, 1 )
         CALL DAXPY( M, -ONE, B( 1, J ), 1, WORK, 1 )
         WNORM = MAX( WNORM, DASUM( N, WORK, 1 ) )
   10 CONTINUE
*
      ANORM = MAX( DLANGE( '1', M, N, A, LDA, WORK ), UNFL )
*
      IF( ANORM.GT.WNORM ) THEN
         RESULT = ( WNORM / ANORM ) / ( M*EPS )
      ELSE
         IF( ANORM.LT.ONE ) THEN
            RESULT = ( MIN( WNORM, M*ANORM ) / ANORM ) / ( M*EPS )
         ELSE
            RESULT = MIN( WNORM / ANORM, DBLE( M ) ) / ( M*EPS )
         END IF
      END IF
*
      RETURN
*
*     End of DGET10
*
      END
