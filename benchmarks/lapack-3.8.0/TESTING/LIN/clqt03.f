*> \brief \b AB_CLQT03
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CLQT03( M, N, K, AF, C, CC, Q, LDA, TAU, WORK, LWORK,
*                          RWORK, RESULT )
*
*       .. Scalar Arguments ..
*       INTEGER            K, LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       REAL               RESULT( * ), RWORK( * )
*       COMPLEX            AF( LDA, * ), C( LDA, * ), CC( LDA, * ),
*      $                   Q( LDA, * ), TAU( * ), WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CLQT03 tests AB_CUNMLQ, which computes Q*C, Q'*C, C*Q or C*Q'.
*>
*> AB_CLQT03 compares the results of a call to AB_CUNMLQ with the results of
*> forming Q explicitly by a call to AB_CUNGLQ and then performing matrix
*> multiplication by a call to AB_CGEMM.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows or columns of the matrix C; C is n-by-m if
*>          Q is applied from the left, or m-by-n if Q is applied from
*>          the right.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the orthogonal matrix Q.  N >= 0.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of elementary reflectors whose product defines the
*>          orthogonal matrix Q.  N >= K >= 0.
*> \endverbatim
*>
*> \param[in] AF
*> \verbatim
*>          AF is COMPLEX array, dimension (LDA,N)
*>          Details of the LQ factorization of an m-by-n matrix, as
*>          returned by AB_AB_CGELQF. See AB_AB_CGELQF for further details.
*> \endverbatim
*>
*> \param[out] C
*> \verbatim
*>          C is COMPLEX array, dimension (LDA,N)
*> \endverbatim
*>
*> \param[out] CC
*> \verbatim
*>          CC is COMPLEX array, dimension (LDA,N)
*> \endverbatim
*>
*> \param[out] Q
*> \verbatim
*>          Q is COMPLEX array, dimension (LDA,N)
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the arrays AF, C, CC, and Q.
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is COMPLEX array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors corresponding
*>          to the LQ factorization in AF.
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
*>          The length of WORK.  LWORK must be at least M, and should be
*>          M*NB, where NB is the blocksize for this environment.
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is REAL array, dimension (M)
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is REAL array, dimension (4)
*>          The test ratios compare two techniques for multiplying a
*>          random matrix C by an n-by-n orthogonal matrix Q.
*>          RESULT(1) = norm( Q*C - Q*C )  / ( N * norm(C) * EPS )
*>          RESULT(2) = norm( C*Q - C*Q )  / ( N * norm(C) * EPS )
*>          RESULT(3) = norm( Q'*C - Q'*C )/ ( N * norm(C) * EPS )
*>          RESULT(4) = norm( C*Q' - C*Q' )/ ( N * norm(C) * EPS )
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
      SUBROUTINE AB_CLQT03( M, N, K, AF, C, CC, Q, LDA, TAU, WORK, LWORK
     $,
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
      COMPLEX            AF( LDA, * ), C( LDA, * ), CC( LDA, * ),
     $                   Q( LDA, * ), TAU( * ), WORK( LWORK )
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
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, ISIDE, ITRANS, J, MC, NC
      REAL               CNORM, EPS, RESID
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      REAL               AB_CLANGE, AB_SLAMCH
      EXTERNAL           AB_LSAME, AB_CLANGE, AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGEMM, AB_CLACPY, AB_CLARNV, AB_CLASET, AB_C
     $UNGLQ, AB_CUNMLQ
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
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
*     .. Data statements ..
      DATA               ISEED / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
      EPS = AB_SLAMCH( 'Epsilon' )
*
*     Copy the first k rows of the factorization to the array Q
*
      CALL AB_CLASET( 'Full', N, N, ROGUE, ROGUE, Q, LDA )
      CALL AB_CLACPY( 'Upper', K, N-1, AF( 1, 2 ), LDA, Q( 1, 2 ), LDA )
*
*     Generate the n-by-n matrix Q
*
      SRNAMT = 'AB_CUNGLQ'
      CALL AB_CUNGLQ( N, N, K, Q, LDA, TAU, WORK, LWORK, INFO )
*
      DO 30 ISIDE = 1, 2
         IF( ISIDE.EQ.1 ) THEN
            SIDE = 'L'
            MC = N
            NC = M
         ELSE
            SIDE = 'R'
            MC = M
            NC = N
         END IF
*
*        Generate MC by NC matrix C
*
         DO 10 J = 1, NC
            CALL AB_CLARNV( 2, ISEED, MC, C( 1, J ) )
   10    CONTINUE
         CNORM = AB_CLANGE( '1', MC, NC, C, LDA, RWORK )
         IF( CNORM.EQ.ZERO )
     $      CNORM = ONE
*
         DO 20 ITRANS = 1, 2
            IF( ITRANS.EQ.1 ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'C'
            END IF
*
*           Copy C
*
            CALL AB_CLACPY( 'Full', MC, NC, C, LDA, CC, LDA )
*
*           Apply Q or Q' to C
*
            SRNAMT = 'AB_CUNMLQ'
            CALL AB_CUNMLQ( SIDE, TRANS, MC, NC, K, AF, LDA, TAU, CC, LD
     $A,
     $                   WORK, LWORK, INFO )
*
*           Form explicit product and subtract
*
            IF( AB_LSAME( SIDE, 'L' ) ) THEN
               CALL AB_CGEMM( TRANS, 'No transpose', MC, NC, MC,
     $                     CMPLX( -ONE ), Q, LDA, C, LDA, CMPLX( ONE ),
     $                     CC, LDA )
            ELSE
               CALL AB_CGEMM( 'No transpose', TRANS, MC, NC, NC,
     $                     CMPLX( -ONE ), C, LDA, Q, LDA, CMPLX( ONE ),
     $                     CC, LDA )
            END IF
*
*           Compute error in the difference
*
            RESID = AB_CLANGE( '1', MC, NC, CC, LDA, RWORK )
            RESULT( ( ISIDE-1 )*2+ITRANS ) = RESID /
     $         ( REAL( MAX( 1, N ) )*CNORM*EPS )
*
   20    CONTINUE
   30 CONTINUE
*
      RETURN
*
*     End of AB_CLQT03
*
      END
