*> \brief \b AB_ZGTT02
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZGTT02( TRANS, N, NRHS, DL, D, DU, X, LDX, B, LDB,
*                          RESID )
*
*       .. Scalar Arguments ..
*       CHARACTER          TRANS
*       INTEGER            LDB, LDX, N, NRHS
*       DOUBLE PRECISION   RESID
*       ..
*       .. Array Arguments ..
*       COMPLEX*16         B( LDB, * ), D( * ), DL( * ), DU( * ),
*      $                   X( LDX, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZGTT02 computes the residual for the solution to a tridiagonal
*> system of equations:
*>    RESID = norm(B - op(A)*X) / (norm(A) * norm(X) * EPS),
*> where EPS is the machine epsilon.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER
*>          Specifies the form of the residual.
*>          = 'N':  B - A * X     (No transpose)
*>          = 'T':  B - A**T * X  (Transpose)
*>          = 'C':  B - A**H * X  (Conjugate transpose)
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGTER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrices B and X.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in] DL
*> \verbatim
*>          DL is COMPLEX*16 array, dimension (N-1)
*>          The (n-1) sub-diagonal elements of A.
*> \endverbatim
*>
*> \param[in] D
*> \verbatim
*>          D is COMPLEX*16 array, dimension (N)
*>          The diagonal elements of A.
*> \endverbatim
*>
*> \param[in] DU
*> \verbatim
*>          DU is COMPLEX*16 array, dimension (N-1)
*>          The (n-1) super-diagonal elements of A.
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is COMPLEX*16 array, dimension (LDX,NRHS)
*>          The computed solution vectors X.
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
*>          On entry, the right hand side vectors for the system of
*>          linear equations.
*>          On exit, B is overwritten with the difference B - op(A)*X.
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
*>          norm(B - op(A)*X) / (norm(A) * norm(X) * EPS)
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
      SUBROUTINE AB_ZGTT02( TRANS, N, NRHS, DL, D, DU, X, LDX, B, LDB,
     $                   RESID )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            LDB, LDX, N, NRHS
      DOUBLE PRECISION   RESID
*     ..
*     .. Array Arguments ..
      COMPLEX*16         B( LDB, * ), D( * ), DL( * ), DU( * ),
     $                   X( LDX, * )
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
      LOGICAL            AB_LSAME
      DOUBLE PRECISION   AB_DLAMCH, AB_DZASUM, AB_ZLANGT
      EXTERNAL           AB_LSAME, AB_DLAMCH, AB_DZASUM, AB_ZLANGT
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ZLAGTM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0 or NRHS = 0
*
      RESID = ZERO
      IF( N.LE.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
*     Compute the maximum over the number of right hand sides of
*        norm(B - op(A)*X) / ( norm(A) * norm(X) * EPS ).
*
      IF( AB_LSAME( TRANS, 'N' ) ) THEN
         ANORM = AB_ZLANGT( '1', N, DL, D, DU )
      ELSE
         ANORM = AB_ZLANGT( 'I', N, DL, D, DU )
      END IF
*
*     Exit with RESID = 1/EPS if ANORM = 0.
*
      EPS = AB_DLAMCH( 'Epsilon' )
      IF( ANORM.LE.ZERO ) THEN
         RESID = ONE / EPS
         RETURN
      END IF
*
*     Compute B - op(A)*X.
*
      CALL AB_ZLAGTM( TRANS, N, NRHS, -ONE, DL, D, DU, X, LDX, ONE, B,
     $             LDB )
*
      DO 10 J = 1, NRHS
         BNORM = AB_DZASUM( N, B( 1, J ), 1 )
         XNORM = AB_DZASUM( N, X( 1, J ), 1 )
         IF( XNORM.LE.ZERO ) THEN
            RESID = ONE / EPS
         ELSE
            RESID = MAX( RESID, ( ( BNORM / ANORM ) / XNORM ) / EPS )
         END IF
   10 CONTINUE
*
      RETURN
*
*     End of AB_ZGTT02
*
      END
