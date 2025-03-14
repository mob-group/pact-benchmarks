*> \brief \b STSQR01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE STSQR01(TSSW, M,N, MB, NB, RESULT)
*
*       .. Scalar Arguments ..
*       INTEGER M, N, MB
*       .. Return values ..
*       REAL  RESULT(6)
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DTSQR01 tests DGEQR , DGELQ, DGEMLQ and DGEMQR.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] TSSW
*> \verbatim
*>          TSSW is CHARACTER
*>          'TS' for testing tall skinny QR
*>               and anything else for testing short wide LQ
*> \endverbatim
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          Number of rows in test matrix.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          Number of columns in test matrix.
*> \endverbatim
*> \param[in] MB
*> \verbatim
*>          MB is INTEGER
*>          Number of row in row block in test matrix.
*> \endverbatim
*>
*> \param[in] NB
*> \verbatim
*>          NB is INTEGER
*>          Number of columns in column block test matrix.
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is REAL array, dimension (6)
*>          Results of each of the six tests below.
*>
*>          RESULT(1) = | A - Q R | or | A - L Q |
*>          RESULT(2) = | I - Q^H Q | or | I - Q Q^H |
*>          RESULT(3) = | Q C - Q C |
*>          RESULT(4) = | Q^H C - Q^H C |
*>          RESULT(5) = | C Q - C Q |
*>          RESULT(6) = | C Q^H - C Q^H |
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
*> \date April 2012
*
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE STSQR01(TSSW, M, N, MB, NB, RESULT)
      IMPLICIT NONE
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     April 2012
*
*     .. Scalar Arguments ..
      CHARACTER         TSSW
      INTEGER           M, N, MB, NB
*     .. Return values ..
      REAL              RESULT(6)
*
*  =====================================================================
*
*     ..
*     .. Local allocatable arrays
      REAL, ALLOCATABLE :: AF(:,:), Q(:,:),
     $  R(:,:), RWORK(:), WORK( : ), T(:),
     $  CF(:,:), DF(:,:), A(:,:), C(:,:), D(:,:), LQ(:,:)
*
*     .. Parameters ..
      REAL     ONE, ZERO
      PARAMETER( ZERO = 0.0, ONE = 1.0 )
*     ..
*     .. Local Scalars ..
      LOGICAL TESTZEROS, TS
      INTEGER INFO, J, K, L, LWORK, TSIZE, MNB
      REAL   ANORM, EPS, RESID, CNORM, DNORM
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
      REAL               TQUERY( 5 ), WORKQUERY
*     ..
*     .. External Functions ..
      REAL     AB_SLAMCH, SLANGE, SLANSY
      LOGICAL  LSAME
      INTEGER  ILAENV
      EXTERNAL AB_SLAMCH, SLARNV, SLANGE, SLANSY, LSAME, ILAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC  MAX, MIN
*     .. Scalars in Common ..
      CHARACTER*32       srnamt
*     ..
*     .. Common blocks ..
      COMMON             / srnamc / srnamt
*     ..
*     .. Data statements ..
      DATA ISEED / 1988, 1989, 1990, 1991 /
*
*     TEST TALL SKINNY OR SHORT WIDE
*
      TS = LSAME(TSSW, 'TS')
*
*     TEST MATRICES WITH HALF OF MATRIX BEING ZEROS
*
      TESTZEROS = .FALSE.
*
      EPS = AB_SLAMCH( 'Epsilon' )
      K = MIN(M,N)
      L = MAX(M,N,1)
      MNB = MAX ( MB, NB)
      LWORK = MAX(3,L)*MNB
*
*     Dynamically allocate local arrays
*
      ALLOCATE ( A(M,N), AF(M,N), Q(L,L), R(M,L), RWORK(L),
     $           C(M,N), CF(M,N),
     $           D(N,M), DF(N,M), LQ(L,N) )
*
*     Put random numbers into A and copy to AF
*
      DO J=1,N
         CALL SLARNV( 2, ISEED, M, A( 1, J ) )
      END DO
      IF (TESTZEROS) THEN
         IF (M.GE.4) THEN
            DO J=1,N
               CALL SLARNV( 2, ISEED, M/2, A( M/4, J ) )
            END DO
         END IF
      END IF
      CALL SLACPY( 'Full', M, N, A, M, AF, M )
*
      IF (TS) THEN
*
*     Factor the matrix A in the array AF.
*
      CALL SGEQR( M, N, AF, M, TQUERY, -1, WORKQUERY, -1, INFO )
      TSIZE = INT( TQUERY( 1 ) )
      LWORK = INT( WORKQUERY )
      CALL SGEMQR( 'L', 'N', M, M, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMQR( 'L', 'N', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMQR( 'L', 'T', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMQR( 'R', 'N', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMQR( 'R', 'T', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      ALLOCATE ( T( TSIZE ) )
      ALLOCATE ( WORK( LWORK ) )
      srnamt = 'SGEQR'
      CALL SGEQR( M, N, AF, M, T, TSIZE, WORK, LWORK, INFO )
*
*     Generate the m-by-m matrix Q
*
      CALL SLASET( 'Full', M, M, ZERO, ONE, Q, M )
      srnamt = 'SGEMQR'
      CALL SGEMQR( 'L', 'N', M, M, K, AF, M, T, TSIZE, Q, M,
     $              WORK, LWORK, INFO )
*
*     Copy R
*
      CALL SLASET( 'Full', M, N, ZERO, ZERO, R, M )
      CALL SLACPY( 'Upper', M, N, AF, M, R, M )
*
*     Compute |R - Q'*A| / |A| and store in RESULT(1)
*
      CALL SGEMM( 'T', 'N', M, N, M, -ONE, Q, M, A, M, ONE, R, M )
      ANORM = SLANGE( '1', M, N, A, M, RWORK )
      RESID = SLANGE( '1', M, N, R, M, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = RESID / (EPS*MAX(1,M)*ANORM)
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute |I - Q'*Q| and store in RESULT(2)
*
      CALL SLASET( 'Full', M, M, ZERO, ONE, R, M )
      CALL SSYRK( 'U', 'C', M, M, -ONE, Q, M, ONE, R, M )
      RESID = SLANSY( '1', 'Upper', M, R, M, RWORK )
      RESULT( 2 ) = RESID / (EPS*MAX(1,M))
*
*     Generate random m-by-n matrix C and a copy CF
*
      DO J=1,N
         CALL SLARNV( 2, ISEED, M, C( 1, J ) )
      END DO
      CNORM = SLANGE( '1', M, N, C, M, RWORK)
      CALL SLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as Q*C
*
      srnamt = 'DGEQR'
      CALL SGEMQR( 'L', 'N', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |Q*C - Q*C| / |C|
*
      CALL SGEMM( 'N', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = SLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 3 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 3 ) = ZERO
      END IF
*
*     Copy C into CF again
*
      CALL SLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as QT*C
*
      srnamt = 'DGEQR'
      CALL SGEMQR( 'L', 'T', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |QT*C - QT*C| / |C|
*
      CALL SGEMM( 'T', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = SLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 4 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 4 ) = ZERO
      END IF
*
*     Generate random n-by-m matrix D and a copy DF
*
      DO J=1,M
         CALL SLARNV( 2, ISEED, N, D( 1, J ) )
      END DO
      DNORM = SLANGE( '1', N, M, D, N, RWORK)
      CALL SLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*Q
*
      srnamt = 'DGEQR'
      CALL SGEMQR( 'R', 'N', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |D*Q - D*Q| / |D|
*
      CALL SGEMM( 'N', 'N', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = SLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 5 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 5 ) = ZERO
      END IF
*
*     Copy D into DF again
*
      CALL SLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*QT
*
      CALL SGEMQR( 'R', 'T', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |D*QT - D*QT| / |D|
*
      CALL SGEMM( 'N', 'T', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = SLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 6 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 6 ) = ZERO
      END IF
*
*     Short and wide
*
      ELSE
      CALL SGELQ( M, N, AF, M, TQUERY, -1, WORKQUERY, -1, INFO )
      TSIZE = INT( TQUERY( 1 ) )
      LWORK = INT( WORKQUERY )
      CALL SGEMLQ( 'R', 'N', N, N, K, AF, M, TQUERY, TSIZE, Q, N,
     $              WORKQUERY, -1, INFO )
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMLQ( 'L', 'N', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMLQ( 'L', 'T', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMLQ( 'R', 'N', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL SGEMLQ( 'R', 'T', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      ALLOCATE ( T( TSIZE ) )
      ALLOCATE ( WORK( LWORK ) )
      srnamt = 'SGELQ'
      CALL SGELQ( M, N, AF, M, T, TSIZE, WORK, LWORK, INFO )
*
*
*     Generate the n-by-n matrix Q
*
      CALL SLASET( 'Full', N, N, ZERO, ONE, Q, N )
      srnamt = 'SGEMLQ'
      CALL SGEMLQ( 'R', 'N', N, N, K, AF, M, T, TSIZE, Q, N,
     $              WORK, LWORK, INFO )
*
*     Copy R
*
      CALL SLASET( 'Full', M, N, ZERO, ZERO, LQ, L )
      CALL SLACPY( 'Lower', M, N, AF, M, LQ, L )
*
*     Compute |L - A*Q'| / |A| and store in RESULT(1)
*
      CALL SGEMM( 'N', 'T', M, N, N, -ONE, A, M, Q, N, ONE, LQ, L )
      ANORM = SLANGE( '1', M, N, A, M, RWORK )
      RESID = SLANGE( '1', M, N, LQ, L, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = RESID / (EPS*MAX(1,N)*ANORM)
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute |I - Q'*Q| and store in RESULT(2)
*
      CALL SLASET( 'Full', N, N, ZERO, ONE, LQ, L )
      CALL SSYRK( 'U', 'C', N, N, -ONE, Q, N, ONE, LQ, L )
      RESID = SLANSY( '1', 'Upper', N, LQ, L, RWORK )
      RESULT( 2 ) = RESID / (EPS*MAX(1,N))
*
*     Generate random m-by-n matrix C and a copy CF
*
      DO J=1,M
         CALL SLARNV( 2, ISEED, N, D( 1, J ) )
      END DO
      DNORM = SLANGE( '1', N, M, D, N, RWORK)
      CALL SLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to C as Q*C
*
      CALL SGEMLQ( 'L', 'N', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |Q*D - Q*D| / |D|
*
      CALL SGEMM( 'N', 'N', N, M, N, -ONE, Q, N, D, N, ONE, DF, N )
      RESID = SLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 3 ) = RESID / (EPS*MAX(1,N)*DNORM)
      ELSE
         RESULT( 3 ) = ZERO
      END IF
*
*     Copy D into DF again
*
      CALL SLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as QT*D
*
      CALL SGEMLQ( 'L', 'T', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |QT*D - QT*D| / |D|
*
      CALL SGEMM( 'T', 'N', N, M, N, -ONE, Q, N, D, N, ONE, DF, N )
      RESID = SLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 4 ) = RESID / (EPS*MAX(1,N)*DNORM)
      ELSE
         RESULT( 4 ) = ZERO
      END IF
*
*     Generate random n-by-m matrix D and a copy DF
*
      DO J=1,N
         CALL SLARNV( 2, ISEED, M, C( 1, J ) )
      END DO
      CNORM = SLANGE( '1', M, N, C, M, RWORK)
      CALL SLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as C*Q
*
      CALL SGEMLQ( 'R', 'N', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |C*Q - C*Q| / |C|
*
      CALL SGEMM( 'N', 'N', M, N, N, -ONE, C, M, Q, N, ONE, CF, M )
      RESID = SLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 5 ) = RESID / (EPS*MAX(1,N)*CNORM)
      ELSE
         RESULT( 5 ) = ZERO
      END IF
*
*     Copy C into CF again
*
      CALL SLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to D as D*QT
*
      CALL SGEMLQ( 'R', 'T', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |C*QT - C*QT| / |C|
*
      CALL SGEMM( 'N', 'T', M, N, N, -ONE, C, M, Q, N, ONE, CF, M )
      RESID = SLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 6 ) = RESID / (EPS*MAX(1,N)*CNORM)
      ELSE
         RESULT( 6 ) = ZERO
      END IF
*
      END IF
*
*     Deallocate all arrays
*
      DEALLOCATE ( A, AF, Q, R, RWORK, WORK, T, C, D, CF, DF)
*
      RETURN
      END
