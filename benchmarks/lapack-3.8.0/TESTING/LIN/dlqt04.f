*> \brief \b AB_DLQT04
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DLQT04(M,N,NB,RESULT)
*
*       .. Scalar Arguments ..
*       INTEGER M, N, NB, LDT
*       .. Return values ..
*       DOUBLE PRECISION RESULT(6)
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DLQT04 tests AB_AB_DGELQT and AB_AB_DGEMLQT.
*> \endverbatim
*
*  Arguments:
*  ==========
*
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
*>
*> \param[in] NB
*> \verbatim
*>          NB is INTEGER
*>          Block size of test matrix.  NB <= Min(M,N).
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is DOUBLE PRECISION array, dimension (6)
*>          Results of each of the six tests below.
*>
*>          RESULT(1) = | A - L Q |
*>          RESULT(2) = | I - Q Q^H |
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
      SUBROUTINE AB_DLQT04(M,N,NB,RESULT)
      IMPLICIT NONE
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     April 2012
*
*     .. Scalar Arguments ..
      INTEGER M, N, NB, LDT
*     .. Return values ..
      DOUBLE PRECISION RESULT(6)
*
*  =====================================================================
*
*     ..
*     .. Local allocatable arrays
      DOUBLE PRECISION, ALLOCATABLE :: AF(:,:), Q(:,:),
     $  L(:,:), RWORK(:), WORK( : ), T(:,:),
     $  CF(:,:), DF(:,:), A(:,:), C(:,:), D(:,:)
*
*     .. Parameters ..
      DOUBLE PRECISION ONE, ZERO
      PARAMETER( ZERO = 0.0, ONE = 1.0 )
*     ..
*     .. Local Scalars ..
      INTEGER INFO, J, K, LL, LWORK
      DOUBLE PRECISION   ANORM, EPS, RESID, CNORM, DNORM
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION AB_DLAMCH, AB_DLANGE, AB_DLANSY
      LOGICAL  AB_LSAME
      EXTERNAL AB_DLAMCH, AB_DLANGE, AB_DLANSY, AB_LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC  MAX, MIN
*     ..
*     .. Data statements ..
      DATA ISEED / 1988, 1989, 1990, 1991 /
*
      EPS = AB_DLAMCH( 'Epsilon' )
      K = MIN(M,N)
      LL = MAX(M,N)
      LWORK = MAX(2,LL)*MAX(2,LL)*NB
*
*     Dynamically allocate local arrays
*
      ALLOCATE ( A(M,N), AF(M,N), Q(N,N), L(LL,N), RWORK(LL),
     $           WORK(LWORK), T(NB,N), C(M,N), CF(M,N),
     $           D(N,M), DF(N,M) )
*
*     Put random numbers into A and copy to AF
*
      LDT=NB
      DO J=1,N
         CALL AB_DLARNV( 2, ISEED, M, A( 1, J ) )
      END DO
      CALL AB_DLACPY( 'Full', M, N, A, M, AF, M )
*
*     Factor the matrix A in the array AF.
*
      CALL AB_AB_DGELQT( M, N, NB, AF, M, T, LDT, WORK, INFO )
*
*     Generate the n-by-n matrix Q
*
      CALL AB_DLASET( 'Full', N, N, ZERO, ONE, Q, N )
      CALL AB_AB_DGEMLQT( 'R', 'N', N, N, K, NB, AF, M, T, LDT, Q, N,
     $              WORK, INFO )
*
*     Copy R
*
      CALL AB_DLASET( 'Full', M, N, ZERO, ZERO, L, LL )
      CALL AB_DLACPY( 'Lower', M, N, AF, M, L, LL )
*
*     Compute |L - A*Q'| / |A| and store in RESULT(1)
*
      CALL AB_DGEMM( 'N', 'T', M, N, N, -ONE, A, M, Q, N, ONE, L, LL )
      ANORM = AB_DLANGE( '1', M, N, A, M, RWORK )
      RESID = AB_DLANGE( '1', M, N, L, LL, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = RESID / (EPS*MAX(1,M)*ANORM)
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute |I - Q'*Q| and store in RESULT(2)
*
      CALL AB_DLASET( 'Full', N, N, ZERO, ONE, L, LL )
      CALL AB_AB_DSYRK( 'U', 'C', N, N, -ONE, Q, N, ONE, L, LL )
      RESID = AB_DLANSY( '1', 'Upper', N, L, LL, RWORK )
      RESULT( 2 ) = RESID / (EPS*MAX(1,N))
*
*     Generate random m-by-n matrix C and a copy CF
*
      DO J=1,M
         CALL AB_DLARNV( 2, ISEED, N, D( 1, J ) )
      END DO
      DNORM = AB_DLANGE( '1', N, M, D, N, RWORK)
      CALL AB_DLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to C as Q*C
*
      CALL AB_AB_DGEMLQT( 'L', 'N', N, M, K, NB, AF, M, T, NB, DF, N,
     $             WORK, INFO)
*
*     Compute |Q*D - Q*D| / |D|
*
      CALL AB_DGEMM( 'N', 'N', N, M, N, -ONE, Q, N, D, N, ONE, DF, N )
      RESID = AB_DLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 3 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 3 ) = ZERO
      END IF
*
*     Copy D into DF again
*
      CALL AB_DLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as QT*D
*
      CALL AB_AB_DGEMLQT( 'L', 'T', N, M, K, NB, AF, M, T, NB, DF, N,
     $             WORK, INFO)
*
*     Compute |QT*D - QT*D| / |D|
*
      CALL AB_DGEMM( 'T', 'N', N, M, N, -ONE, Q, N, D, N, ONE, DF, N )
      RESID = AB_DLANGE( '1', N, M, DF, N, RWORK )
      IF( DNORM.GT.ZERO ) THEN
         RESULT( 4 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 4 ) = ZERO
      END IF
*
*     Generate random n-by-m matrix D and a copy DF
*
      DO J=1,N
         CALL AB_DLARNV( 2, ISEED, M, C( 1, J ) )
      END DO
      CNORM = AB_DLANGE( '1', M, N, C, M, RWORK)
      CALL AB_DLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as C*Q
*
      CALL AB_AB_DGEMLQT( 'R', 'N', M, N, K, NB, AF, M, T, NB, CF, M,
     $             WORK, INFO)
*
*     Compute |C*Q - C*Q| / |C|
*
      CALL AB_DGEMM( 'N', 'N', M, N, N, -ONE, C, M, Q, N, ONE, CF, M )
      RESID = AB_DLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 5 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 5 ) = ZERO
      END IF
*
*     Copy C into CF again
*
      CALL AB_DLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to D as D*QT
*
      CALL AB_AB_DGEMLQT( 'R', 'T', M, N, K, NB, AF, M, T, NB, CF, M,
     $             WORK, INFO)
*
*     Compute |C*QT - C*QT| / |C|
*
      CALL AB_DGEMM( 'N', 'T', M, N, N, -ONE, C, M, Q, N, ONE, CF, M )
      RESID = AB_DLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 6 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 6 ) = ZERO
      END IF
*
*     Deallocate all arrays
*
      DEALLOCATE ( A, AF, Q, L, RWORK, WORK, T, C, D, CF, DF)
*
      RETURN
      END

