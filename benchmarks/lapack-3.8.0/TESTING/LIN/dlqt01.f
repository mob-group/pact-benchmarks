*> \brief \b AB_DLQT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DLQT01( M, N, A, AF, Q, L, LDA, TAU, WORK, LWORK,
*                          RWORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), L( LDA, * ),
*      $                   Q( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
*      $                   WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DLQT01 tests AB_AB_DGELQF, which computes the LQ factorization of an m-by-n
*> matrix A, and partially tests AB_DORGLQ which forms the n-by-n
*> orthogonal matrix Q.
*>
*> AB_DLQT01 compares L with A*Q', and checks that Q is orthogonal.
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
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          The m-by-n matrix A.
*> \endverbatim
*>
*> \param[out] AF
*> \verbatim
*>          AF is DOUBLE PRECISION array, dimension (LDA,N)
*>          Details of the LQ factorization of A, as returned by AB_AB_DGELQF.
*>          See AB_AB_DGELQF for further details.
*> \endverbatim
*>
*> \param[out] Q
*> \verbatim
*>          Q is DOUBLE PRECISION array, dimension (LDA,N)
*>          The n-by-n orthogonal matrix Q.
*> \endverbatim
*>
*> \param[out] L
*> \verbatim
*>          L is DOUBLE PRECISION array, dimension (LDA,max(M,N))
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
*>          TAU is DOUBLE PRECISION array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors, as returned
*>          by AB_AB_DGELQF.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (LWORK)
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DLQT01( M, N, A, AF, Q, L, LDA, TAU, WORK, LWORK,
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
      DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), L( LDA, * ),
     $                   Q( LDA, * ), RESULT( * ), RWORK( * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   ROGUE
      PARAMETER          ( ROGUE = -1.0D+10 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, MINMN
      DOUBLE PRECISION   ANORM, EPS, RESID
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANGE, AB_DLANSY
      EXTERNAL           AB_DLAMCH, AB_DLANGE, AB_DLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_AB_DGELQF, AB_DGEMM, AB_DLACPY, AB_DLASET, A
     $B_DORGLQ, AB_AB_DSYRK
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
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
      CALL AB_DLACPY( 'Full', M, N, A, LDA, AF, LDA )
*
*     Factorize the matrix A in the array AF.
*
      SRNAMT = 'AB_AB_DGELQF'
      CALL AB_AB_DGELQF( M, N, AF, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy details of Q
*
      CALL AB_DLASET( 'Full', N, N, ROGUE, ROGUE, Q, LDA )
      IF( N.GT.1 )
     $   CALL AB_DLACPY( 'Upper', M, N-1, AF( 1, 2 ), LDA, Q( 1, 2 ), LD
     $A )
*
*     Generate the n-by-n matrix Q
*
      SRNAMT = 'AB_DORGLQ'
      CALL AB_DORGLQ( N, N, MINMN, Q, LDA, TAU, WORK, LWORK, INFO )
*
*     Copy L
*
      CALL AB_DLASET( 'Full', M, N, ZERO, ZERO, L, LDA )
      CALL AB_DLACPY( 'Lower', M, N, AF, LDA, L, LDA )
*
*     Compute L - A*Q'
*
      CALL AB_DGEMM( 'No transpose', 'Transpose', M, N, N, -ONE, A, LDA,
     $ Q,
     $            LDA, ONE, L, LDA )
*
*     Compute norm( L - Q'*A ) / ( N * norm(A) * EPS ) .
*
      ANORM = AB_DLANGE( '1', M, N, A, LDA, RWORK )
      RESID = AB_DLANGE( '1', M, N, L, LDA, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = ( ( RESID / DBLE( MAX( 1, N ) ) ) / ANORM ) / EPS
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute I - Q*Q'
*
      CALL AB_DLASET( 'Full', N, N, ZERO, ONE, L, LDA )
      CALL AB_AB_DSYRK( 'Upper', 'No transpose', N, N, -ONE, Q, LDA, ONE
     $, L,
     $            LDA )
*
*     Compute norm( I - Q*Q' ) / ( N * EPS ) .
*
      RESID = AB_DLANSY( '1', 'Upper', N, L, LDA, RWORK )
*
      RESULT( 2 ) = ( RESID / DBLE( MAX( 1, N ) ) ) / EPS
*
      RETURN
*
*     End of AB_DLQT01
*
      END
