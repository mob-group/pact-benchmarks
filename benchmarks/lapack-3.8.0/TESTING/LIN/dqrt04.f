*> \brief \b AB_DQRT04
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DQRT04(M,N,NB,RESULT)
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
*> AB_DQRT04 tests AB_AB_DGEQRT and AB_AB_DGEMQRT.
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
*>          RESULT(1) = | A - Q R |
*>          RESULT(2) = | I - Q^H Q |
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
      SUBROUTINE AB_DQRT04(M,N,NB,RESULT)
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
     $  R(:,:), RWORK(:), WORK( : ), T(:,:),
     $  CF(:,:), DF(:,:), A(:,:), C(:,:), D(:,:)
*
*     .. Parameters ..
      DOUBLE PRECISION ONE, ZERO
      PARAMETER( ZERO = 0.0, ONE = 1.0 )
*     ..
*     .. Local Scalars ..
      INTEGER INFO, J, K, L, LWORK
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
      L = MAX(M,N)
      LWORK = MAX(2,L)*MAX(2,L)*NB
*
*     Dynamically allocate local arrays
*
      ALLOCATE ( A(M,N), AF(M,N), Q(M,M), R(M,L), RWORK(L),
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
      CALL AB_AB_DGEQRT( M, N, NB, AF, M, T, LDT, WORK, INFO )
*
*     Generate the m-by-m matrix Q
*
      CALL AB_DLASET( 'Full', M, M, ZERO, ONE, Q, M )
      CALL AB_AB_DGEMQRT( 'R', 'N', M, M, K, NB, AF, M, T, LDT, Q, M,
     $              WORK, INFO )
*
*     Copy R
*
      CALL AB_DLASET( 'Full', M, N, ZERO, ZERO, R, M )
      CALL AB_DLACPY( 'Upper', M, N, AF, M, R, M )
*
*     Compute |R - Q'*A| / |A| and store in RESULT(1)
*
      CALL AB_DGEMM( 'T', 'N', M, N, M, -ONE, Q, M, A, M, ONE, R, M )
      ANORM = AB_DLANGE( '1', M, N, A, M, RWORK )
      RESID = AB_DLANGE( '1', M, N, R, M, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = RESID / (EPS*MAX(1,M)*ANORM)
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute |I - Q'*Q| and store in RESULT(2)
*
      CALL AB_DLASET( 'Full', M, M, ZERO, ONE, R, M )
      CALL AB_AB_DSYRK( 'U', 'C', M, M, -ONE, Q, M, ONE, R, M )
      RESID = AB_DLANSY( '1', 'Upper', M, R, M, RWORK )
      RESULT( 2 ) = RESID / (EPS*MAX(1,M))
*
*     Generate random m-by-n matrix C and a copy CF
*
      DO J=1,N
         CALL AB_DLARNV( 2, ISEED, M, C( 1, J ) )
      END DO
      CNORM = AB_DLANGE( '1', M, N, C, M, RWORK)
      CALL AB_DLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as Q*C
*
      CALL AB_AB_DGEMQRT( 'L', 'N', M, N, K, NB, AF, M, T, NB, CF, M,
     $             WORK, INFO)
*
*     Compute |Q*C - Q*C| / |C|
*
      CALL AB_DGEMM( 'N', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = AB_DLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 3 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 3 ) = ZERO
      END IF
*
*     Copy C into CF again
*
      CALL AB_DLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as QT*C
*
      CALL AB_AB_DGEMQRT( 'L', 'T', M, N, K, NB, AF, M, T, NB, CF, M,
     $             WORK, INFO)
*
*     Compute |QT*C - QT*C| / |C|
*
      CALL AB_DGEMM( 'T', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = AB_DLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 4 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 4 ) = ZERO
      END IF
*
*     Generate random n-by-m matrix D and a copy DF
*
      DO J=1,M
         CALL AB_DLARNV( 2, ISEED, N, D( 1, J ) )
      END DO
      DNORM = AB_DLANGE( '1', N, M, D, N, RWORK)
      CALL AB_DLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*Q
*
      CALL AB_AB_DGEMQRT( 'R', 'N', N, M, K, NB, AF, M, T, NB, DF, N,
     $             WORK, INFO)
*
*     Compute |D*Q - D*Q| / |D|
*
      CALL AB_DGEMM( 'N', 'N', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = AB_DLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 5 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 5 ) = ZERO
      END IF
*
*     Copy D into DF again
*
      CALL AB_DLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*QT
*
      CALL AB_AB_DGEMQRT( 'R', 'T', N, M, K, NB, AF, M, T, NB, DF, N,
     $             WORK, INFO)
*
*     Compute |D*QT - D*QT| / |D|
*
      CALL AB_DGEMM( 'N', 'T', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = AB_DLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 6 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 6 ) = ZERO
      END IF
*
*     Deallocate all arrays
*
      DEALLOCATE ( A, AF, Q, R, RWORK, WORK, T, C, D, CF, DF)
*
      RETURN
      END

