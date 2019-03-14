*> \brief \b AB_SRQT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SRQT01( M, N, A, AF, Q, R, LDA, TAU, WORK, LWORK,
*                          RWORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       REAL               A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
*      $                   R( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
*      $                   WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SRQT01 tests AB_AB_SGERQF, which computes the RQ factorization of an m-by-n
*> matrix A, and partially tests AB_SORGRQ which forms the n-by-n
*> orthogonal matrix Q.
*>
*> AB_SRQT01 compares R with A*Q', and checks that Q is orthogonal.
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
*>          A is REAL array, dimension (LDA,N)
*>          The m-by-n matrix A.
*> \endverbatim
*>
*> \param[out] AF
*> \verbatim
*>          AF is REAL array, dimension (LDA,N)
*>          Details of the RQ factorization of A, as returned by AB_AB_SGERQF.
*>          See AB_AB_SGERQF for further details.
*> \endverbatim
*>
*> \param[out] Q
*> \verbatim
*>          Q is REAL array, dimension (LDA,N)
*>          The n-by-n orthogonal matrix Q.
*> \endverbatim
*>
*> \param[out] R
*> \verbatim
*>          R is REAL array, dimension (LDA,max(M,N))
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
*>          TAU is REAL array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors, as returned
*>          by AB_AB_SGERQF.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is REAL array, dimension (LWORK)
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
*>          RWORK is REAL array, dimension (max(M,N))
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SRQT01( M, N, A, AF, Q, R, LDA, TAU, WORK, LWORK,
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
      REAL               A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
     $                   R( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      REAL               ROGUE
      PARAMETER          ( ROGUE = -1.0E+10 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, MINMN
      REAL               ANORM, EPS, RESID
*     ..
*     .. External Functions ..
      REAL               AB_SLAMCH, AB_SLANGE, AB_SLANSY
      EXTERNAL           AB_SLAMCH, AB_SLANGE, AB_SLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_SGEMM, AB_AB_SGERQF, AB_SLACPY, AB_SLASET, A
     $B_SORGRQ, AB_AB_SSYRK
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, REAL
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
      CALL AB_SLACPY( 'Full', M, N, A, LDA, AF, LDA )
*
*     Factorize the matrix A in the array AF.
*
      SRNAMT = 'AB_AB_SGERQF'
      CALL AB_AB_SGERQF( M, N, AF, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy details of Q
*
      CALL AB_SLASET( 'Full', N, N, ROGUE, ROGUE, Q, LDA )
      IF( M.LE.N ) THEN
         IF( M.GT.0 .AND. M.LT.N )
     $      CALL AB_SLACPY( 'Full', M, N-M, AF, LDA, Q( N-M+1, 1 ), LDA 
     $)
         IF( M.GT.1 )
     $      CALL AB_SLACPY( 'Lower', M-1, M-1, AF( 2, N-M+1 ), LDA,
     $                   Q( N-M+2, N-M+1 ), LDA )
      ELSE
         IF( N.GT.1 )
     $      CALL AB_SLACPY( 'Lower', N-1, N-1, AF( M-N+2, 1 ), LDA,
     $                   Q( 2, 1 ), LDA )
      END IF
*
*     Generate the n-by-n matrix Q
*
      SRNAMT = 'AB_SORGRQ'
      CALL AB_SORGRQ( N, N, MINMN, Q, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy R
*
      CALL AB_SLASET( 'Full', M, N, ZERO, ZERO, R, LDA )
      IF( M.LE.N ) THEN
         IF( M.GT.0 )
     $      CALL AB_SLACPY( 'Upper', M, M, AF( 1, N-M+1 ), LDA,
     $                   R( 1, N-M+1 ), LDA )
      ELSE
         IF( M.GT.N .AND. N.GT.0 )
     $      CALL AB_SLACPY( 'Full', M-N, N, AF, LDA, R, LDA )
         IF( N.GT.0 )
     $      CALL AB_SLACPY( 'Upper', N, N, AF( M-N+1, 1 ), LDA,
     $                   R( M-N+1, 1 ), LDA )
      END IF
*
*     Compute R - A*Q'
*
      CALL AB_SGEMM( 'No transpose', 'Transpose', M, N, N, -ONE, A, LDA,
     $ Q,
     $            LDA, ONE, R, LDA )
*
*     Compute norm( R - Q'*A ) / ( N * norm(A) * EPS ) .
*
      ANORM = AB_SLANGE( '1', M, N, A, LDA, RWORK )
      RESID = AB_SLANGE( '1', M, N, R, LDA, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = ( ( RESID / REAL( MAX( 1, N ) ) ) / ANORM ) / EPS
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute I - Q*Q'
*
      CALL AB_SLASET( 'Full', N, N, ZERO, ONE, R, LDA )
      CALL AB_AB_SSYRK( 'Upper', 'No transpose', N, N, -ONE, Q, LDA, ONE
     $, R,
     $            LDA )
*
*     Compute norm( I - Q*Q' ) / ( N * EPS ) .
*
      RESID = AB_SLANSY( '1', 'Upper', N, R, LDA, RWORK )
*
      RESULT( 2 ) = ( RESID / REAL( MAX( 1, N ) ) ) / EPS
*
      RETURN
*
*     End of AB_SRQT01
*
      END
