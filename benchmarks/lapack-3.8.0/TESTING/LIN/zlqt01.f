*> \brief \b AB_ZLQT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZLQT01( M, N, A, AF, Q, L, LDA, TAU, WORK, LWORK,
*                          RWORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   RESULT( * ), RWORK( * )
*       COMPLEX*16         A( LDA, * ), AF( LDA, * ), L( LDA, * ),
*      $                   Q( LDA, * ), TAU( * ), WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZLQT01 tests AB_AB_ZGELQF, which computes the LQ factorization of an m-by-n
*> matrix A, and partially tests AB_ZUNGLQ which forms the n-by-n
*> orthogonal matrix Q.
*>
*> AB_ZLQT01 compares L with A*Q', and checks that Q is orthogonal.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          The m-by-n matrix A.
*> \endverbatim
*>
*> \param[out] AF
*> \verbatim
*>          AF is COMPLEX*16 array, dimension (LDA,N)
*>          Details of the LQ factorization of A, as returned by AB_AB_ZGELQF.
*>          See AB_AB_ZGELQF for further details.
*> \endverbatim
*>
*> \param[out] Q
*> \verbatim
*>          Q is COMPLEX*16 array, dimension (LDA,N)
*>          The n-by-n orthogonal matrix Q.
*> \endverbatim
*>
*> \param[out] L
*> \verbatim
*>          L is COMPLEX*16 array, dimension (LDA,max(M,N))
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the arrays A, AF, Q and L.
*>          LDA >= max(M,N).
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is COMPLEX*16 array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors, as returned
*>          by AB_AB_ZGELQF.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (LWORK)
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension (max(M,N))
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is DOUBLE PRECISION array, dimension (2)
*>          The test ratios:
*>          RESULT(1) = norm( L - A*Q' ) / ( N * norm(A) * EPS )
*>          RESULT(2) = norm( I - Q*Q' ) / ( N * EPS )
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
      SUBROUTINE AB_ZLQT01( M, N, A, AF, Q, L, LDA, TAU, WORK, LWORK,
     $                   RWORK, RESULT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   RESULT( * ), RWORK( * )
      COMPLEX*16         A( LDA, * ), AF( LDA, * ), L( LDA, * ),
     $                   Q( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         ROGUE
      PARAMETER          ( ROGUE = ( -1.0D+10, -1.0D+10 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, MINMN
      DOUBLE PRECISION   ANORM, EPS, RESID
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH, AB_ZLANGE, AB_ZLANSY
      EXTERNAL           AB_DLAMCH, AB_ZLANGE, AB_ZLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_AB_ZGELQF, AB_ZGEMM, AB_AB_ZHERK, AB_ZLACPY,
     $ AB_ZLASET, AB_ZUNGLQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, MAX, MIN
*     ..
*     .. Scalars in Common ..
      CHARACTER*32       SRNAMT
*     ..
*     .. Common blocks ..
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      MINMN = MIN( M, N )
      EPS = AB_DLAMCH( 'Epsilon' )
*
*     Copy the matrix A to the array AF.
*
      CALL AB_ZLACPY( 'Full', M, N, A, LDA, AF, LDA )
*
*     Factorize the matrix A in the array AF.
*
      SRNAMT = 'AB_AB_ZGELQF'
      CALL AB_AB_ZGELQF( M, N, AF, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy details of Q
*
      CALL AB_ZLASET( 'Full', N, N, ROGUE, ROGUE, Q, LDA )
      IF( N.GT.1 )
     $   CALL AB_ZLACPY( 'Upper', M, N-1, AF( 1, 2 ), LDA, Q( 1, 2 ), LD
     $A )
*
*     Generate the n-by-n matrix Q
*
      SRNAMT = 'AB_ZUNGLQ'
      CALL AB_ZUNGLQ( N, N, MINMN, Q, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy L
*
      CALL AB_ZLASET( 'Full', M, N, DCMPLX( ZERO ), DCMPLX( ZERO ), L,
     $             LDA )
      CALL AB_ZLACPY( 'Lower', M, N, AF, LDA, L, LDA )
*
*     Compute L - A*Q'
*
      CALL AB_ZGEMM( 'No transpose', 'Conjugate transpose', M, N, N,
     $            DCMPLX( -ONE ), A, LDA, Q, LDA, DCMPLX( ONE ), L,
     $            LDA )
*
*     Compute norm( L - Q'*A ) / ( N * norm(A) * EPS ) .
*
      ANORM = AB_ZLANGE( '1', M, N, A, LDA, RWORK )
      RESID = AB_ZLANGE( '1', M, N, L, LDA, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = ( ( RESID / DBLE( MAX( 1, N ) ) ) / ANORM ) / EPS
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute I - Q*Q'
*
      CALL AB_ZLASET( 'Full', N, N, DCMPLX( ZERO ), DCMPLX( ONE ), L, LD
     $A )
      CALL AB_AB_ZHERK( 'Upper', 'No transpose', N, N, -ONE, Q, LDA, ONE
     $, L,
     $            LDA )
*
*     Compute norm( I - Q*Q' ) / ( N * EPS ) .
*
      RESID = AB_ZLANSY( '1', 'Upper', N, L, LDA, RWORK )
*
      RESULT( 2 ) = ( RESID / DBLE( MAX( 1, N ) ) ) / EPS
*
      RETURN
*
*     End of AB_ZLQT01
*
      END
