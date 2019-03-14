*> \brief \b AB_DSYT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_DSYT01_AA( UPLO, N, A, LDA, AFAC, LDAFAC, IPIV, C, LDC,
*                             RWORK, RESID )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            LDA, LDAFAC, LDC, N
*       DOUBLE PRECISION   RESID
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       DOUBLE PRECISION   A( LDA, * ), AFAC( LDAFAC, * ), C( LDC, * ),
*      $                   RWORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DSYT01 reconstructs a symmetric indefinite matrix A from its
*> block L*D*L' or U*D*U' factorization and computes the residual
*>    norm( C - A ) / ( N * norm(A) * EPS ),
*> where C is the reconstructed matrix and EPS is the machine epsilon.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the upper or lower triangular part of the
*>          symmetric matrix A is stored:
*>          = 'U':  Upper triangular
*>          = 'L':  Lower triangular
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of rows and columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          The original symmetric matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N)
*> \endverbatim
*>
*> \param[in] AFAC
*> \verbatim
*>          AFAC is DOUBLE PRECISION array, dimension (LDAFAC,N)
*>          The factored form of the matrix A.  AFAC contains the block
*>          diagonal matrix D and the multipliers used to obtain the
*>          factor L or U from the block L*D*L' or U*D*U' factorization
*>          as computed by AB_DSYTRF.
*> \endverbatim
*>
*> \param[in] LDAFAC
*> \verbatim
*>          LDAFAC is INTEGER
*>          The leading dimension of the array AFAC.  LDAFAC >= max(1,N).
*> \endverbatim
*>
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          The pivot indices from AB_DSYTRF.
*> \endverbatim
*>
*> \param[out] C
*> \verbatim
*>          C is DOUBLE PRECISION array, dimension (LDC,N)
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the array C.  LDC >= max(1,N).
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension (N)
*> \endverbatim
*>
*> \param[out] RESID
*> \verbatim
*>          RESID is DOUBLE PRECISION
*>          If UPLO = 'L', norm(L*D*L' - A) / ( N * norm(A) * EPS )
*>          If UPLO = 'U', norm(U*D*U' - A) / ( N * norm(A) * EPS )
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
*  @precisions fortran d -> z c
*
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_AB_DSYT01_AA( UPLO, N, A, LDA, AFAC, LDAFAC, IPIV, C
     $,
     $                         LDC, RWORK, RESID )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDAFAC, LDC, N
      DOUBLE PRECISION   RESID
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), AFAC( LDAFAC, * ), C( LDC, * ),
     $                   RWORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   ANORM, EPS
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANSY
      EXTERNAL           AB_LSAME, AB_DLAMCH, AB_DLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DLASET, AB_DLAVSY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0.
*
      IF( N.LE.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
*     Determine EPS and the norm of A.
*
      EPS = AB_DLAMCH( 'Epsilon' )
      ANORM = AB_DLANSY( '1', UPLO, N, A, LDA, RWORK )
*
*     Initialize C to the tridiagonal matrix T.
*
      CALL AB_DLASET( 'Full', N, N, ZERO, ZERO, C, LDC )
      CALL AB_DLACPY( 'F', 1, N, AFAC( 1, 1 ), LDAFAC+1, C( 1, 1 ), LDC+
     $1 )
      IF( N.GT.1 ) THEN
         IF( AB_LSAME( UPLO, 'U' ) ) THEN
            CALL AB_DLACPY( 'F', 1, N-1, AFAC( 1, 2 ), LDAFAC+1, C( 1, 2
     $ ),
     $                   LDC+1 )
            CALL AB_DLACPY( 'F', 1, N-1, AFAC( 1, 2 ), LDAFAC+1, C( 2, 1
     $ ),
     $                   LDC+1 )
         ELSE
            CALL AB_DLACPY( 'F', 1, N-1, AFAC( 2, 1 ), LDAFAC+1, C( 1, 2
     $ ),
     $                   LDC+1 )
            CALL AB_DLACPY( 'F', 1, N-1, AFAC( 2, 1 ), LDAFAC+1, C( 2, 1
     $ ),
     $                   LDC+1 )
         ENDIF
*
*        Call AB_DTRMM to form the product U' * D (or L * D ).
*
         IF( AB_LSAME( UPLO, 'U' ) ) THEN
            CALL AB_DTRMM( 'Left', UPLO, 'Transpose', 'Unit', N-1, N,
     $                  ONE, AFAC( 1, 2 ), LDAFAC, C( 2, 1 ), LDC )
         ELSE
            CALL AB_DTRMM( 'Left', UPLO, 'No transpose', 'Unit', N-1, N,
     $                  ONE, AFAC( 2, 1 ), LDAFAC, C( 2, 1 ), LDC )
         END IF
*
*        Call AB_DTRMM again to multiply by U (or L ).
*
         IF( AB_LSAME( UPLO, 'U' ) ) THEN
            CALL AB_DTRMM( 'Right', UPLO, 'No transpose', 'Unit', N, N-1
     $,
     $                  ONE, AFAC( 1, 2 ), LDAFAC, C( 1, 2 ), LDC )
         ELSE
            CALL AB_DTRMM( 'Right', UPLO, 'Transpose', 'Unit', N, N-1,
     $                  ONE, AFAC( 2, 1 ), LDAFAC, C( 1, 2 ), LDC )
         END IF
      ENDIF
*
*     Apply symmetric pivots
*
      DO J = N, 1, -1
         I = IPIV( J )
         IF( I.NE.J )
     $      CALL AB_DSWAP( N, C( J, 1 ), LDC, C( I, 1 ), LDC )
      END DO
      DO J = N, 1, -1
         I = IPIV( J )
         IF( I.NE.J )
     $      CALL AB_DSWAP( N, C( 1, J ), 1, C( 1, I ), 1 )
      END DO
*
*
*     Compute the difference  C - A .
*
      IF( AB_LSAME( UPLO, 'U' ) ) THEN
         DO J = 1, N
            DO I = 1, J
               C( I, J ) = C( I, J ) - A( I, J )
            END DO
         END DO
      ELSE
         DO J = 1, N
            DO I = J, N
               C( I, J ) = C( I, J ) - A( I, J )
            END DO
         END DO
      END IF
*
*     Compute norm( C - A ) / ( N * norm(A) * EPS )
*
      RESID = AB_DLANSY( '1', UPLO, N, C, LDC, RWORK )
*
      IF( ANORM.LE.ZERO ) THEN
         IF( RESID.NE.ZERO )
     $      RESID = ONE / EPS
      ELSE
         RESID = ( ( RESID / DBLE( N ) ) / ANORM ) / EPS
      END IF
*
      RETURN
*
*     End of AB_DSYT01
*
      END
