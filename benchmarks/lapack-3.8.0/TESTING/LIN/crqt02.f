*> \brief \b AB_CRQT02
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CRQT02( M, N, K, A, AF, Q, R, LDA, TAU, WORK, LWORK,
*                          RWORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            K, LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       REAL               RESULT( * ), RWORK( * )
*       COMPLEX            A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
*      $                   R( LDA, * ), TAU( * ), WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CRQT02 tests AB_CUNGRQ, which generates an m-by-n matrix Q with
*> orthonornmal rows that is defined as the product of k elementary
*> reflectors.
*>
*> Given the RQ factorization of an m-by-n matrix A, AB_CRQT02 generates
*> the orthogonal matrix Q defined by the factorization of the last k
*> rows of A; it compares R(m-k+1:m,n-m+1:n) with
*> A(m-k+1:m,1:n)*Q(n-m+1:n,1:n)', and checks that the rows of Q are
*> orthonormal.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix Q to be generated.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix Q to be generated.
*>          N >= M >= 0.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of elementary reflectors whose product defines the
*>          matrix Q. M >= K >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX array, dimension (LDA,N)
*>          The m-by-n matrix A which was factorized by AB_CRQT01.
*> \endverbatim
*>
*> \param[in] AF
*> \verbatim
*>          AF is COMPLEX array, dimension (LDA,N)
*>          Details of the RQ factorization of A, as returned by AB_CGERQF.
*>          See AB_CGERQF for further details.
*> \endverbatim
*>
*> \param[out] Q
*> \verbatim
*>          Q is COMPLEX array, dimension (LDA,N)
*> \endverbatim
*>
*> \param[out] R
*> \verbatim
*>          R is COMPLEX array, dimension (LDA,M)
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the arrays A, AF, Q and L. LDA >= N.
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is COMPLEX array, dimension (M)
*>          The scalar factors of the elementary reflectors corresponding
*>          to the RQ factorization in AF.
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
*>          The dimension of the array WORK.
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is REAL array, dimension (M)
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is REAL array, dimension (2)
*>          The test ratios:
*>          RESULT(1) = norm( R - A*Q' ) / ( N * norm(A) * EPS )
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CRQT02( M, N, K, A, AF, Q, R, LDA, TAU, WORK, LWORK,
     $                   RWORK, RESULT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      REAL               RESULT( * ), RWORK( * )
      COMPLEX            A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
     $                   R( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      COMPLEX            ROGUE
      PARAMETER          ( ROGUE = ( -1.0E+10, -1.0E+10 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO
      REAL               ANORM, EPS, RESID
*     ..
*     .. External Functions ..
      REAL               AB_CLANGE, AB_CLANSY, AB_SLAMCH
      EXTERNAL           AB_CLANGE, AB_CLANSY, AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGEMM, AB_AB_CHERK, AB_CLACPY, AB_CLASET, AB
     $_CUNGRQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MAX, REAL
*     ..
*     .. Scalars in Common ..
      CHARACTER*32       SRNAMT
*     ..
*     .. Common blocks ..
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         RESULT( 1 ) = ZERO
         RESULT( 2 ) = ZERO
         RETURN
      END IF
*
      EPS = AB_SLAMCH( 'Epsilon' )
*
*     Copy the last k rows of the factorization to the array Q
*
      CALL AB_CLASET( 'Full', M, N, ROGUE, ROGUE, Q, LDA )
      IF( K.LT.N )
     $   CALL AB_CLACPY( 'Full', K, N-K, AF( M-K+1, 1 ), LDA,
     $                Q( M-K+1, 1 ), LDA )
      IF( K.GT.1 )
     $   CALL AB_CLACPY( 'Lower', K-1, K-1, AF( M-K+2, N-K+1 ), LDA,
     $                Q( M-K+2, N-K+1 ), LDA )
*
*     Generate the last n rows of the matrix Q
*
      SRNAMT = 'AB_CUNGRQ'
      CALL AB_CUNGRQ( M, N, K, Q, LDA, TAU( M-K+1 ), WORK, LWORK, INFO )
*
*     Copy R(m-k+1:m,n-m+1:n)
*
      CALL AB_CLASET( 'Full', K, M, CMPLX( ZERO ), CMPLX( ZERO ),
     $             R( M-K+1, N-M+1 ), LDA )
      CALL AB_CLACPY( 'Upper', K, K, AF( M-K+1, N-K+1 ), LDA,
     $             R( M-K+1, N-K+1 ), LDA )
*
*     Compute R(m-k+1:m,n-m+1:n) - A(m-k+1:m,1:n) * Q(n-m+1:n,1:n)'
*
      CALL AB_CGEMM( 'No transpose', 'Conjugate transpose', K, M, N,
     $            CMPLX( -ONE ), A( M-K+1, 1 ), LDA, Q, LDA,
     $            CMPLX( ONE ), R( M-K+1, N-M+1 ), LDA )
*
*     Compute norm( R - A*Q' ) / ( N * norm(A) * EPS ) .
*
      ANORM = AB_CLANGE( '1', K, N, A( M-K+1, 1 ), LDA, RWORK )
      RESID = AB_CLANGE( '1', K, M, R( M-K+1, N-M+1 ), LDA, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = ( ( RESID / REAL( MAX( 1, N ) ) ) / ANORM ) / EPS
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute I - Q*Q'
*
      CALL AB_CLASET( 'Full', M, M, CMPLX( ZERO ), CMPLX( ONE ), R, LDA 
     $)
      CALL AB_AB_CHERK( 'Upper', 'No transpose', M, N, -ONE, Q, LDA, ONE
     $, R,
     $            LDA )
*
*     Compute norm( I - Q*Q' ) / ( N * EPS ) .
*
      RESID = AB_CLANSY( '1', 'Upper', M, R, LDA, RWORK )
*
      RESULT( 2 ) = ( RESID / REAL( MAX( 1, N ) ) ) / EPS
*
      RETURN
*
*     End of AB_CRQT02
*
      END
