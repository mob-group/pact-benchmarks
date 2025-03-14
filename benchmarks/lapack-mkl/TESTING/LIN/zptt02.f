*> \brief \b ZPTT02
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE ZPTT02( UPLO, N, NRHS, D, E, X, LDX, B, LDB, RESID )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            LDB, LDX, N, NRHS
*       DOUBLE PRECISION   RESID
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   D( * )
*       COMPLEX*16         B( LDB, * ), E( * ), X( LDX, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> ZPTT02 computes the residual for the solution to a symmetric
*> tridiagonal system of equations:
*>    RESID = norm(B - A*X) / (norm(A) * norm(X) * EPS),
*> where EPS is the machine epsilon.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the superdiagonal or the subdiagonal of the
*>          tridiagonal matrix A is stored.
*>          = 'U':  E is the superdiagonal of A
*>          = 'L':  E is the subdiagonal of A
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGTER
*>          The order of the matrix A.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrices B and X.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in] D
*> \verbatim
*>          D is DOUBLE PRECISION array, dimension (N)
*>          The n diagonal elements of the tridiagonal matrix A.
*> \endverbatim
*>
*> \param[in] E
*> \verbatim
*>          E is COMPLEX*16 array, dimension (N-1)
*>          The (n-1) subdiagonal elements of the tridiagonal matrix A.
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is COMPLEX*16 array, dimension (LDX,NRHS)
*>          The n by nrhs matrix of solution vectors X.
*> \endverbatim
*>
*> \param[in] LDX
*> \verbatim
*>          LDX is INTEGER
*>          The leading dimension of the array X.  LDX >= max(1,N).
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is COMPLEX*16 array, dimension (LDB,NRHS)
*>          On entry, the n by nrhs matrix of right hand side vectors B.
*>          On exit, B is overwritten with the difference B - A*X.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[out] RESID
*> \verbatim
*>          RESID is DOUBLE PRECISION
*>          norm(B - A*X) / (norm(A) * norm(X) * EPS)
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
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE ZPTT02( UPLO, N, NRHS, D, E, X, LDX, B, LDB, RESID )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDB, LDX, N, NRHS
      DOUBLE PRECISION   RESID
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * )
      COMPLEX*16         B( LDB, * ), E( * ), X( LDX, * )
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
      DOUBLE PRECISION   ANORM, BNORM, EPS, XNORM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH, DZASUM, ZLANHT
      EXTERNAL           AB_DLAMCH, DZASUM, ZLANHT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZLAPTM
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
*     Compute the 1-norm of the tridiagonal matrix A.
*
      ANORM = ZLANHT( '1', N, D, E )
*
*     Exit with RESID = 1/EPS if ANORM = 0.
*
      EPS = AB_DLAMCH( 'Epsilon' )
      IF( ANORM.LE.ZERO ) THEN
         RESID = ONE / EPS
         RETURN
      END IF
*
*     Compute B - A*X.
*
      CALL ZLAPTM( UPLO, N, NRHS, -ONE, D, E, X, LDX, ONE, B, LDB )
*
*     Compute the maximum over the number of right hand sides of
*        norm(B - A*X) / ( norm(A) * norm(X) * EPS ).
*
      RESID = ZERO
      DO 10 J = 1, NRHS
         BNORM = DZASUM( N, B( 1, J ), 1 )
         XNORM = DZASUM( N, X( 1, J ), 1 )
         IF( XNORM.LE.ZERO ) THEN
            RESID = ONE / EPS
         ELSE
            RESID = MAX( RESID, ( ( BNORM / ANORM ) / XNORM ) / EPS )
         END IF
   10 CONTINUE
*
      RETURN
*
*     End of ZPTT02
*
      END
