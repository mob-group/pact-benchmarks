*> \brief \b AB_CTSQR01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CTSQR01(TSSW, M,N, MB, NB, RESULT)
*
*       .. Scalar Arguments ..
*       INTEGER M, N, MB
*       .. Return values ..
*       REAL RESULT(6)
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DTSQR01 tests AB_DGEQR , AB_DGELQ, AB_DGEMLQ and AB_DGEMQR.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] TSSW
*> \verbatim
*>          TSSW is CHARACTER
*>          'TS' for testing tall skinny QR
*>               and anything ELSE for testing short wide LQ
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
*  =====================================================================
      SUBROUTINE AB_CTSQR01(TSSW, M, N, MB, NB, RESULT)
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
      COMPLEX, ALLOCATABLE :: AF(:,:), Q(:,:),
     $  R(:,:), RWORK(:), WORK( : ), T(:),
     $  CF(:,:), DF(:,:), A(:,:), C(:,:), D(:,:), LQ(:,:)
*
*     .. Parameters ..
      REAL ZERO
      COMPLEX ONE, CZERO
      PARAMETER( ZERO = 0.0, ONE = (1.0,0.0), CZERO=(0.0,0.0) )
*     ..
*     .. Local Scalars ..
      LOGICAL TESTZEROS, TS
      INTEGER INFO, J, K, L, LWORK, TSIZE, MNB
      REAL    ANORM, EPS, RESID, CNORM, DNORM
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
      COMPLEX            TQUERY( 5 ), WORKQUERY
*     ..
*     .. External Functions ..
      REAL     AB_SLAMCH, AB_CLANGE, AB_CLANSY
      LOGICAL  AB_LSAME
      INTEGER  AB_ILAENV
      EXTERNAL AB_SLAMCH, AB_CLANGE, AB_CLANSY, AB_LSAME, AB_ILAENV
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
      TS = AB_LSAME(TSSW, 'TS')
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
         CALL AB_CLARNV( 2, ISEED, M, A( 1, J ) )
      END DO
      IF (TESTZEROS) THEN
         IF (M.GE.4) THEN
            DO J=1,N
               CALL AB_CLARNV( 2, ISEED, M/2, A( M/4, J ) )
            END DO
         END IF
      END IF
      CALL AB_CLACPY( 'Full', M, N, A, M, AF, M )
*
      IF (TS) THEN
*
*     Factor the matrix A in the array AF.
*
      CALL AB_CGEQR( M, N, AF, M, TQUERY, -1, WORKQUERY, -1, INFO )
      TSIZE = INT( TQUERY( 1 ) )
      LWORK = INT( WORKQUERY )
      CALL AB_CGEMQR( 'L', 'N', M, M, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMQR( 'L', 'N', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMQR( 'L', 'C', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMQR( 'R', 'N', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMQR( 'R', 'C', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      ALLOCATE ( T( TSIZE ) )
      ALLOCATE ( WORK( LWORK ) )
      srnamt = 'AB_CGEQR'
      CALL AB_CGEQR( M, N, AF, M, T, TSIZE, WORK, LWORK, INFO )
*
*     Generate the m-by-m matrix Q
*
      CALL AB_CLASET( 'Full', M, M, CZERO, ONE, Q, M )
      srnamt = 'AB_CGEMQR'
      CALL AB_CGEMQR( 'L', 'N', M, M, K, AF, M, T, TSIZE, Q, M,
     $              WORK, LWORK, INFO )
*
*     Copy R
*
      CALL AB_CLASET( 'Full', M, N, CZERO, CZERO, R, M )
      CALL AB_CLACPY( 'Upper', M, N, AF, M, R, M )
*
*     Compute |R - Q'*A| / |A| and store in RESULT(1)
*
      CALL AB_CGEMM( 'C', 'N', M, N, M, -ONE, Q, M, A, M, ONE, R, M )
      ANORM = AB_CLANGE( '1', M, N, A, M, RWORK )
      RESID = AB_CLANGE( '1', M, N, R, M, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = RESID / (EPS*MAX(1,M)*ANORM)
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute |I - Q'*Q| and store in RESULT(2)
*
      CALL AB_CLASET( 'Full', M, M, CZERO, ONE, R, M )
      CALL AB_AB_CHERK( 'U', 'C', M, M, REAL(-ONE), Q, M, REAL(ONE), R, 
     $M )
      RESID = AB_CLANSY( '1', 'Upper', M, R, M, RWORK )
      RESULT( 2 ) = RESID / (EPS*MAX(1,M))
*
*     Generate random m-by-n matrix C and a copy CF
*
      DO J=1,N
         CALL AB_CLARNV( 2, ISEED, M, C( 1, J ) )
      END DO
      CNORM = AB_CLANGE( '1', M, N, C, M, RWORK)
      CALL AB_CLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as Q*C
*
      srnamt = 'AB_CGEMQR'
      CALL AB_CGEMQR( 'L', 'N', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |Q*C - Q*C| / |C|
*
      CALL AB_CGEMM( 'N', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = AB_CLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 3 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 3 ) = ZERO
      END IF
*
*     Copy C into CF again
*
      CALL AB_CLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as QT*C
*
      srnamt = 'AB_CGEMQR'
      CALL AB_CGEMQR( 'L', 'C', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |QT*C - QT*C| / |C|
*
      CALL AB_CGEMM( 'C', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = AB_CLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 4 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 4 ) = ZERO
      END IF
*
*     Generate random n-by-m matrix D and a copy DF
*
      DO J=1,M
         CALL AB_CLARNV( 2, ISEED, N, D( 1, J ) )
      END DO
      DNORM = AB_CLANGE( '1', N, M, D, N, RWORK)
      CALL AB_CLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*Q
*
      srnamt = 'AB_CGEMQR'
      CALL AB_CGEMQR( 'R', 'N', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |D*Q - D*Q| / |D|
*
      CALL AB_CGEMM( 'N', 'N', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = AB_CLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 5 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 5 ) = ZERO
      END IF
*
*     Copy D into DF again
*
      CALL AB_CLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*QT
*
      CALL AB_CGEMQR( 'R', 'C', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |D*QT - D*QT| / |D|
*
      CALL AB_CGEMM( 'N', 'C', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = AB_CLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 6 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 6 ) = ZERO
      END IF
*
*     Short and wide
*
      ELSE
      CALL AB_CGELQ( M, N, AF, M, TQUERY, -1, WORKQUERY, -1, INFO )
      TSIZE = INT( TQUERY( 1 ) )
      LWORK = INT( WORKQUERY )
      CALL AB_CGEMLQ( 'R', 'N', N, N, K, AF, M, TQUERY, TSIZE, Q, N,
     $              WORKQUERY, -1, INFO )
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMLQ( 'L', 'N', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMLQ( 'L', 'C', N, M, K, AF, M, TQUERY, TSIZE, DF, N,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMLQ( 'R', 'N', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      CALL AB_CGEMLQ( 'R', 'C', M, N, K, AF, M, TQUERY, TSIZE, CF, M,
     $             WORKQUERY, -1, INFO)
      LWORK = MAX( LWORK, INT( WORKQUERY ) )
      ALLOCATE ( T( TSIZE ) )
      ALLOCATE ( WORK( LWORK ) )
      srnamt = 'AB_CGELQ'
      CALL AB_CGELQ( M, N, AF, M, T, TSIZE, WORK, LWORK, INFO )
*
*
*     Generate the n-by-n matrix Q
*
      CALL AB_CLASET( 'Full', N, N, CZERO, ONE, Q, N )
      srnamt = 'AB_CGEMLQ'
      CALL AB_CGEMLQ( 'R', 'N', N, N, K, AF, M, T, TSIZE, Q, N,
     $              WORK, LWORK, INFO )
*
*     Copy R
*
      CALL AB_CLASET( 'Full', M, N, CZERO, CZERO, LQ, L )
      CALL AB_CLACPY( 'Lower', M, N, AF, M, LQ, L )
*
*     Compute |L - A*Q'| / |A| and store in RESULT(1)
*
      CALL AB_CGEMM( 'N', 'C', M, N, N, -ONE, A, M, Q, N, ONE, LQ, L )
      ANORM = AB_CLANGE( '1', M, N, A, M, RWORK )
      RESID = AB_CLANGE( '1', M, N, LQ, L, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = RESID / (EPS*MAX(1,N)*ANORM)
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute |I - Q'*Q| and store in RESULT(2)
*
      CALL AB_CLASET( 'Full', N, N, CZERO, ONE, LQ, L )
      CALL AB_AB_CHERK( 'U', 'C', N, N, REAL(-ONE), Q, N, REAL(ONE), LQ,
     $ L)
      RESID = AB_CLANSY( '1', 'Upper', N, LQ, L, RWORK )
      RESULT( 2 ) = RESID / (EPS*MAX(1,N))
*
*     Generate random m-by-n matrix C and a copy CF
*
      DO J=1,M
         CALL AB_CLARNV( 2, ISEED, N, D( 1, J ) )
      END DO
      DNORM = AB_CLANGE( '1', N, M, D, N, RWORK)
      CALL AB_CLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to C as Q*C
*
      CALL AB_CGEMLQ( 'L', 'N', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |Q*D - Q*D| / |D|
*
      CALL AB_CGEMM( 'N', 'N', N, M, N, -ONE, Q, N, D, N, ONE, DF, N )
      RESID = AB_CLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 3 ) = RESID / (EPS*MAX(1,N)*DNORM)
      ELSE
         RESULT( 3 ) = ZERO
      END IF
*
*     Copy D into DF again
*
      CALL AB_CLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as QT*D
*
      CALL AB_CGEMLQ( 'L', 'C', N, M, K, AF, M, T, TSIZE, DF, N,
     $             WORK, LWORK, INFO)
*
*     Compute |QT*D - QT*D| / |D|
*
      CALL AB_CGEMM( 'C', 'N', N, M, N, -ONE, Q, N, D, N, ONE, DF, N )
      RESID = AB_CLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 4 ) = RESID / (EPS*MAX(1,N)*DNORM)
      ELSE
         RESULT( 4 ) = ZERO
      END IF
*
*     Generate random n-by-m matrix D and a copy DF
*
      DO J=1,N
         CALL AB_CLARNV( 2, ISEED, M, C( 1, J ) )
      END DO
      CNORM = AB_CLANGE( '1', M, N, C, M, RWORK)
      CALL AB_CLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as C*Q
*
      CALL AB_CGEMLQ( 'R', 'N', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |C*Q - C*Q| / |C|
*
      CALL AB_CGEMM( 'N', 'N', M, N, N, -ONE, C, M, Q, N, ONE, CF, M )
      RESID = AB_CLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 5 ) = RESID / (EPS*MAX(1,N)*CNORM)
      ELSE
         RESULT( 5 ) = ZERO
      END IF
*
*     Copy C into CF again
*
      CALL AB_CLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to D as D*QT
*
      CALL AB_CGEMLQ( 'R', 'C', M, N, K, AF, M, T, TSIZE, CF, M,
     $             WORK, LWORK, INFO)
*
*     Compute |C*QT - C*QT| / |C|
*
      CALL AB_CGEMM( 'N', 'C', M, N, N, -ONE, C, M, Q, N, ONE, CF, M )
      RESID = AB_CLANGE( '1', M, N, CF, M, RWORK )
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
