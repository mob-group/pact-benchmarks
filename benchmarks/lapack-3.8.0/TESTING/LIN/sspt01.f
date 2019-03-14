*> \brief \b AB_SSPT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SSPT01( UPLO, N, A, AFAC, IPIV, C, LDC, RWORK, RESID )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            LDC, N
*       REAL               RESID
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       REAL               A( * ), AFAC( * ), C( LDC, * ), RWORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SSPT01 reconstructs a symmetric indefinite packed matrix A from its
*> block L*D*L' or U*D*U' factorization and computes the residual
*>      norm( C - A ) / ( N * norm(A) * EPS ),
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
*>          A is REAL array, dimension (N*(N+1)/2)
*>          The original symmetric matrix A, stored as a packed
*>          triangular matrix.
*> \endverbatim
*>
*> \param[in] AFAC
*> \verbatim
*>          AFAC is REAL array, dimension (N*(N+1)/2)
*>          The factored form of the matrix A, stored as a packed
*>          triangular matrix.  AFAC contains the block diagonal matrix D
*>          and the multipliers used to obtain the factor L or U from the
*>          block L*D*L' or U*D*U' factorization as computed by AB_SSPTRF.
*> \endverbatim
*>
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          The pivot indices from AB_SSPTRF.
*> \endverbatim
*>
*> \param[out] C
*> \verbatim
*>          C is REAL array, dimension (LDC,N)
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
*>          RWORK is REAL array, dimension (N)
*> \endverbatim
*>
*> \param[out] RESID
*> \verbatim
*>          RESID is REAL
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SSPT01( UPLO, N, A, AFAC, IPIV, C, LDC, RWORK, RESID
     $ )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDC, N
      REAL               RESID
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      REAL               A( * ), AFAC( * ), C( LDC, * ), RWORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J, JC
      REAL               ANORM, EPS
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      REAL               AB_SLAMCH, AB_SLANSP, AB_SLANSY
      EXTERNAL           AB_LSAME, AB_SLAMCH, AB_SLANSP, AB_SLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_SLAVSP, AB_SLASET
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          REAL
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
      EPS = AB_SLAMCH( 'Epsilon' )
      ANORM = AB_SLANSP( '1', UPLO, N, A, RWORK )
*
*     Initialize C to the identity matrix.
*
      CALL AB_SLASET( 'Full', N, N, ZERO, ONE, C, LDC )
*
*     Call AB_SLAVSP to form the product D * U' (or D * L' ).
*
      CALL AB_SLAVSP( UPLO, 'Transpose', 'Non-unit', N, N, AFAC, IPIV, C
     $,
     $             LDC, INFO )
*
*     Call AB_SLAVSP again to multiply by U ( or L ).
*
      CALL AB_SLAVSP( UPLO, 'No transpose', 'Unit', N, N, AFAC, IPIV, C,
     $             LDC, INFO )
*
*     Compute the difference  C - A .
*
      IF( AB_LSAME( UPLO, 'U' ) ) THEN
         JC = 0
         DO 20 J = 1, N
            DO 10 I = 1, J
               C( I, J ) = C( I, J ) - A( JC+I )
   10       CONTINUE
            JC = JC + J
   20    CONTINUE
      ELSE
         JC = 1
         DO 40 J = 1, N
            DO 30 I = J, N
               C( I, J ) = C( I, J ) - A( JC+I-J )
   30       CONTINUE
            JC = JC + N - J + 1
   40    CONTINUE
      END IF
*
*     Compute norm( C - A ) / ( N * norm(A) * EPS )
*
      RESID = AB_SLANSY( '1', UPLO, N, C, LDC, RWORK )
*
      IF( ANORM.LE.ZERO ) THEN
         IF( RESID.NE.ZERO )
     $      RESID = ONE / EPS
      ELSE
         RESID = ( ( RESID / REAL( N ) ) / ANORM ) / EPS
      END IF
*
      RETURN
*
*     End of AB_SSPT01
*
      END
