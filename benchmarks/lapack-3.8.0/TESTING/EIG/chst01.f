*> \brief \b AB_CHST01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CHST01( N, ILO, IHI, A, LDA, H, LDH, Q, LDQ, WORK,
*                          LWORK, RWORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            IHI, ILO, LDA, LDH, LDQ, LWORK, N
*       ..
*       .. Array Arguments ..
*       REAL               RESULT( 2 ), RWORK( * )
*       COMPLEX            A( LDA, * ), H( LDH, * ), Q( LDQ, * ),
*      $                   WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CHST01 tests the reduction of a general matrix A to upper Hessenberg
*> form:  A = Q*H*Q'.  Two test ratios are computed;
*>
*> RESULT(1) = norm( A - Q*H*Q' ) / ( norm(A) * N * EPS )
*> RESULT(2) = norm( I - Q'*Q ) / ( N * EPS )
*>
*> The matrix Q is assumed to be given explicitly as it would be
*> following AB_CGEHRD + AB_CUNGHR.
*>
*> In this version, ILO and IHI are not used, but they could be used
*> to save some work if this is desired.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] ILO
*> \verbatim
*>          ILO is INTEGER
*> \endverbatim
*>
*> \param[in] IHI
*> \verbatim
*>          IHI is INTEGER
*>
*>          A is assumed to be upper triangular in rows and columns
*>          1:ILO-1 and IHI+1:N, so Q differs from the identity only in
*>          rows and columns ILO+1:IHI.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX array, dimension (LDA,N)
*>          The original n by n matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in] H
*> \verbatim
*>          H is COMPLEX array, dimension (LDH,N)
*>          The upper Hessenberg matrix H from the reduction A = Q*H*Q'
*>          as computed by AB_CGEHRD.  H is assumed to be zero below the
*>          first subdiagonal.
*> \endverbatim
*>
*> \param[in] LDH
*> \verbatim
*>          LDH is INTEGER
*>          The leading dimension of the array H.  LDH >= max(1,N).
*> \endverbatim
*>
*> \param[in] Q
*> \verbatim
*>          Q is COMPLEX array, dimension (LDQ,N)
*>          The orthogonal matrix Q from the reduction A = Q*H*Q' as
*>          computed by AB_CGEHRD + AB_CUNGHR.
*> \endverbatim
*>
*> \param[in] LDQ
*> \verbatim
*>          LDQ is INTEGER
*>          The leading dimension of the array Q.  LDQ >= max(1,N).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX array, dimension (LWORK)
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The length of the array WORK.  LWORK >= 2*N*N.
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is REAL array, dimension (N)
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is REAL array, dimension (2)
*>          RESULT(1) = norm( A - Q*H*Q' ) / ( norm(A) * N * EPS )
*>          RESULT(2) = norm( I - Q'*Q ) / ( N * EPS )
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
*> \ingroup complex_eig
*
*  =====================================================================
      SUBROUTINE AB_CHST01( N, ILO, IHI, A, LDA, H, LDH, Q, LDQ, WORK,
     $                   LWORK, RWORK, RESULT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            IHI, ILO, LDA, LDH, LDQ, LWORK, N
*     ..
*     .. Array Arguments ..
      REAL               RESULT( 2 ), RWORK( * )
      COMPLEX            A( LDA, * ), H( LDH, * ), Q( LDQ, * ),
     $                   WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            LDWORK
      REAL               ANORM, EPS, OVFL, SMLNUM, UNFL, WNORM
*     ..
*     .. External Functions ..
      REAL               AB_CLANGE, AB_SLAMCH
      EXTERNAL           AB_CLANGE, AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGEMM, AB_CLACPY, AB_CUNT01, AB_SLABAD
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         RESULT( 1 ) = ZERO
         RESULT( 2 ) = ZERO
         RETURN
      END IF
*
      UNFL = AB_SLAMCH( 'Safe minimum' )
      EPS = AB_SLAMCH( 'Precision' )
      OVFL = ONE / UNFL
      CALL AB_SLABAD( UNFL, OVFL )
      SMLNUM = UNFL*N / EPS
*
*     Test 1:  Compute norm( A - Q*H*Q' ) / ( norm(A) * N * EPS )
*
*     Copy A to WORK
*
      LDWORK = MAX( 1, N )
      CALL AB_CLACPY( ' ', N, N, A, LDA, WORK, LDWORK )
*
*     Compute Q*H
*
      CALL AB_CGEMM( 'No transpose', 'No transpose', N, N, N, CMPLX( ONE
     $ ),
     $            Q, LDQ, H, LDH, CMPLX( ZERO ), WORK( LDWORK*N+1 ),
     $            LDWORK )
*
*     Compute A - Q*H*Q'
*
      CALL AB_CGEMM( 'No transpose', 'Conjugate transpose', N, N, N,
     $            CMPLX( -ONE ), WORK( LDWORK*N+1 ), LDWORK, Q, LDQ,
     $            CMPLX( ONE ), WORK, LDWORK )
*
      ANORM = MAX( AB_CLANGE( '1', N, N, A, LDA, RWORK ), UNFL )
      WNORM = AB_CLANGE( '1', N, N, WORK, LDWORK, RWORK )
*
*     Note that RESULT(1) cannot overflow and is bounded by 1/(N*EPS)
*
      RESULT( 1 ) = MIN( WNORM, ANORM ) / MAX( SMLNUM, ANORM*EPS ) / N
*
*     Test 2:  Compute norm( I - Q'*Q ) / ( N * EPS )
*
      CALL AB_CUNT01( 'Columns', N, N, Q, LDQ, WORK, LWORK, RWORK,
     $             RESULT( 2 ) )
*
      RETURN
*
*     End of AB_CHST01
*
      END
