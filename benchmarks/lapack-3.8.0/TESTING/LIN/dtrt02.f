*> \brief \b DTRT02
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE DTRT02( UPLO, TRANS, DIAG, N, NRHS, A, LDA, X, LDX, B,
*                          LDB, WORK, RESID )
*
*       .. Scalar Arguments ..
*       CHARACTER          DIAG, TRANS, UPLO
*       INTEGER            LDA, LDB, LDX, N, NRHS
*       DOUBLE PRECISION   RESID
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * ),
*      $                   X( LDX, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DTRT02 computes the residual for the computed solution to a
*> triangular system of linear equations  A*x = b  or  A'*x = b.
*> Here A is a triangular matrix, A' is the transpose of A, and x and b
*> are N by NRHS matrices.  The test ratio is the maximum over the
*> number of right hand sides of
*>    norm(b - op(A)*x) / ( norm(op(A)) * norm(x) * EPS ),
*> where op(A) denotes A or A' and EPS is the machine epsilon.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the matrix A is upper or lower triangular.
*>          = 'U':  Upper triangular
*>          = 'L':  Lower triangular
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>          Specifies the operation applied to A.
*>          = 'N':  A *x = b  (No transpose)
*>          = 'T':  A'*x = b  (Transpose)
*>          = 'C':  A'*x = b  (Conjugate transpose = Transpose)
*> \endverbatim
*>
*> \param[in] DIAG
*> \verbatim
*>          DIAG is CHARACTER*1
*>          Specifies whether or not the matrix A is unit triangular.
*>          = 'N':  Non-unit triangular
*>          = 'U':  Unit triangular
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrices X and B.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          The triangular matrix A.  If UPLO = 'U', the leading n by n
*>          upper triangular part of the array A contains the upper
*>          triangular matrix, and the strictly lower triangular part of
*>          A is not referenced.  If UPLO = 'L', the leading n by n lower
*>          triangular part of the array A contains the lower triangular
*>          matrix, and the strictly upper triangular part of A is not
*>          referenced.  If DIAG = 'U', the diagonal elements of A are
*>          also not referenced and are assumed to be 1.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is DOUBLE PRECISION array, dimension (LDX,NRHS)
*>          The computed solution vectors for the system of linear
*>          equations.
*> \endverbatim
*>
*> \param[in] LDX
*> \verbatim
*>          LDX is INTEGER
*>          The leading dimension of the array X.  LDX >= max(1,N).
*> \endverbatim
*>
*> \param[in] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
*>          The right hand side vectors for the system of linear
*>          equations.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (N)
*> \endverbatim
*>
*> \param[out] RESID
*> \verbatim
*>          RESID is DOUBLE PRECISION
*>          The maximum over the number of right hand sides of
*>          norm(op(A)*x - b) / ( norm(op(A)) * norm(x) * EPS ).
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE DTRT02( UPLO, TRANS, DIAG, N, NRHS, A, LDA, X, LDX, B,
     $                   LDB, WORK, RESID )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, TRANS, UPLO
      INTEGER            LDA, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RESID
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * ),
     $                   X( LDX, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J
      DOUBLE PRECISION   ANORM, BNORM, EPS, XNORM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DASUM, AB_DLAMCH, DLANTR
      EXTERNAL           LSAME, DASUM, AB_DLAMCH, DLANTR
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DTRMV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0 or NRHS = 0
*
      IF( N.LE.0 .OR. NRHS.LE.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
*     Compute the 1-norm of A or A'.
*
      IF( LSAME( TRANS, 'N' ) ) THEN
         ANORM = DLANTR( '1', UPLO, DIAG, N, N, A, LDA, WORK )
      ELSE
         ANORM = DLANTR( 'I', UPLO, DIAG, N, N, A, LDA, WORK )
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
*     Compute the maximum over the number of right hand sides of
*        norm(op(A)*x - b) / ( norm(op(A)) * norm(x) * EPS )
*
      RESID = ZERO
      DO 10 J = 1, NRHS
         CALL DCOPY( N, X( 1, J ), 1, WORK, 1 )
         CALL DTRMV( UPLO, TRANS, DIAG, N, A, LDA, WORK, 1 )
         CALL DAXPY( N, -ONE, B( 1, J ), 1, WORK, 1 )
         BNORM = DASUM( N, WORK, 1 )
         XNORM = DASUM( N, X( 1, J ), 1 )
         IF( XNORM.LE.ZERO ) THEN
            RESID = ONE / EPS
         ELSE
            RESID = MAX( RESID, ( ( BNORM / ANORM ) / XNORM ) / EPS )
         END IF
   10 CONTINUE
*
      RETURN
*
*     End of DTRT02
*
      END
