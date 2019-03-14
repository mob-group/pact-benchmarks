*> \brief \b AB_AB_CQRT01P
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_CQRT01P( M, N, A, AF, Q, R, LDA, TAU, WORK, LWORK,
*                          RWORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, LWORK, M, N
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
*> AB_AB_CQRT01P tests AB_AB_AB_CGEQRFP, which computes the QR factorization of an m-by-n
*> matrix A, and partially tests AB_CUNGQR which forms the m-by-m
*> orthogonal matrix Q.
*>
*> AB_AB_CQRT01P compares R with Q'*A, and checks that Q is orthogonal.
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
*>          A is COMPLEX array, dimension (LDA,N)
*>          The m-by-n matrix A.
*> \endverbatim
*>
*> \param[out] AF
*> \verbatim
*>          AF is COMPLEX array, dimension (LDA,N)
*>          Details of the QR factorization of A, as returned by AB_AB_AB_CGEQRFP.
*>          See AB_AB_AB_CGEQRFP for further details.
*> \endverbatim
*>
*> \param[out] Q
*> \verbatim
*>          Q is COMPLEX array, dimension (LDA,M)
*>          The m-by-m orthogonal matrix Q.
*> \endverbatim
*>
*> \param[out] R
*> \verbatim
*>          R is COMPLEX array, dimension (LDA,max(M,N))
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the arrays A, AF, Q and R.
*>          LDA >= max(M,N).
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is COMPLEX array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors, as returned
*>          by AB_AB_AB_CGEQRFP.
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
*>          RESULT(1) = norm( R - Q'*A ) / ( M * norm(A) * EPS )
*>          RESULT(2) = norm( I - Q'*Q ) / ( M * EPS )
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
      SUBROUTINE AB_AB_CQRT01P( M, N, A, AF, Q, R, LDA, TAU, WORK, LWORK
     $,
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
      INTEGER            INFO, MINMN
      REAL               ANORM, EPS, RESID
*     ..
*     .. External Functions ..
      REAL               AB_CLANGE, AB_CLANSY, AB_SLAMCH
      EXTERNAL           AB_CLANGE, AB_CLANSY, AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGEMM, AB_AB_AB_CGEQRFP, AB_AB_CHERK, AB_CLA
     $CPY, AB_CLASET, AB_CUNGQR
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MAX, MIN, REAL
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
      EPS = AB_SLAMCH( 'Epsilon' )
*
*     Copy the matrix A to the array AF.
*
      CALL AB_CLACPY( 'Full', M, N, A, LDA, AF, LDA )
*
*     Factorize the matrix A in the array AF.
*
      SRNAMT = 'AB_AB_AB_CGEQRFP'
      CALL AB_AB_AB_CGEQRFP( M, N, AF, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy details of Q
*
      CALL AB_CLASET( 'Full', M, M, ROGUE, ROGUE, Q, LDA )
      CALL AB_CLACPY( 'Lower', M-1, N, AF( 2, 1 ), LDA, Q( 2, 1 ), LDA )
*
*     Generate the m-by-m matrix Q
*
      SRNAMT = 'AB_CUNGQR'
      CALL AB_CUNGQR( M, M, MINMN, Q, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy R
*
      CALL AB_CLASET( 'Full', M, N, CMPLX( ZERO ), CMPLX( ZERO ), R, LDA
     $ )
      CALL AB_CLACPY( 'Upper', M, N, AF, LDA, R, LDA )
*
*     Compute R - Q'*A
*
      CALL AB_CGEMM( 'Conjugate transpose', 'No transpose', M, N, M,
     $            CMPLX( -ONE ), Q, LDA, A, LDA, CMPLX( ONE ), R, LDA )
*
*     Compute norm( R - Q'*A ) / ( M * norm(A) * EPS ) .
*
      ANORM = AB_CLANGE( '1', M, N, A, LDA, RWORK )
      RESID = AB_CLANGE( '1', M, N, R, LDA, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = ( ( RESID / REAL( MAX( 1, M ) ) ) / ANORM ) / EPS
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute I - Q'*Q
*
      CALL AB_CLASET( 'Full', M, M, CMPLX( ZERO ), CMPLX( ONE ), R, LDA 
     $)
      CALL AB_AB_CHERK( 'Upper', 'Conjugate transpose', M, M, -ONE, Q, L
     $DA,
     $            ONE, R, LDA )
*
*     Compute norm( I - Q'*Q ) / ( M * EPS ) .
*
      RESID = AB_CLANSY( '1', 'Upper', M, R, LDA, RWORK )
*
      RESULT( 2 ) = ( RESID / REAL( MAX( 1, M ) ) ) / EPS
*
      RETURN
*
*     End of AB_AB_CQRT01P
*
      END
