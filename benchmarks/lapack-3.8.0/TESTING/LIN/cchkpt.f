*> \brief \b AB_CCHKPT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CCHKPT( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR,
*                          A, D, E, B, X, XACT, WORK, RWORK, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NN, NNS, NOUT
*       REAL               THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            NSVAL( * ), NVAL( * )
*       REAL               D( * ), RWORK( * )
*       COMPLEX            A( * ), B( * ), E( * ), WORK( * ), X( * ),
*      $                   XACT( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CCHKPT tests AB_CPTTRF, -TRS, -RFS, and -CON
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] DOTYPE
*> \verbatim
*>          DOTYPE is LOGICAL array, dimension (NTYPES)
*>          The matrix types to be used for testing.  Matrices of type j
*>          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*>          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*> \endverbatim
*>
*> \param[in] NN
*> \verbatim
*>          NN is INTEGER
*>          The number of values of N contained in the vector NVAL.
*> \endverbatim
*>
*> \param[in] NVAL
*> \verbatim
*>          NVAL is INTEGER array, dimension (NN)
*>          The values of the matrix dimension N.
*> \endverbatim
*>
*> \param[in] NNS
*> \verbatim
*>          NNS is INTEGER
*>          The number of values of NRHS contained in the vector NSVAL.
*> \endverbatim
*>
*> \param[in] NSVAL
*> \verbatim
*>          NSVAL is INTEGER array, dimension (NNS)
*>          The values of the number of right hand sides NRHS.
*> \endverbatim
*>
*> \param[in] THRESH
*> \verbatim
*>          THRESH is REAL
*>          The threshold value for the test ratios.  A result is
*>          included in the output file if RESULT >= THRESH.  To have
*>          every test ratio printed, use THRESH = 0.
*> \endverbatim
*>
*> \param[in] TSTERR
*> \verbatim
*>          TSTERR is LOGICAL
*>          Flag that indicates whether error exits are to be tested.
*> \endverbatim
*>
*> \param[out] A
*> \verbatim
*>          A is COMPLEX array, dimension (NMAX*2)
*> \endverbatim
*>
*> \param[out] D
*> \verbatim
*>          D is REAL array, dimension (NMAX*2)
*> \endverbatim
*>
*> \param[out] E
*> \verbatim
*>          E is COMPLEX array, dimension (NMAX*2)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is COMPLEX array, dimension (NMAX*NSMAX)
*>          where NSMAX is the largest entry in NSVAL.
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is COMPLEX array, dimension (NMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] XACT
*> \verbatim
*>          XACT is COMPLEX array, dimension (NMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX array, dimension
*>                      (NMAX*max(3,NSMAX))
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is REAL array, dimension
*>                      (max(NMAX,2*NSMAX))
*> \endverbatim
*>
*> \param[in] NOUT
*> \verbatim
*>          NOUT is INTEGER
*>          The unit number for output.
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
      SUBROUTINE AB_CCHKPT( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   A, D, E, B, X, XACT, WORK, RWORK, NOUT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NN, NNS, NOUT
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            NSVAL( * ), NVAL( * )
      REAL               D( * ), RWORK( * )
      COMPLEX            A( * ), B( * ), E( * ), WORK( * ), X( * ),
     $                   XACT( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 12 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 7 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ZEROT
      CHARACTER          DIST, TYPE, UPLO
      CHARACTER*3        PATH
      INTEGER            I, IA, IMAT, IN, INFO, IRHS, IUPLO, IX, IZERO,
     $                   J, K, KL, KU, LDA, MODE, N, NERRS, NFAIL,
     $                   NIMAT, NRHS, NRUN
      REAL               AINVNM, ANORM, COND, DMAX, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      CHARACTER          UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
      COMPLEX            Z( 3 )
*     ..
*     .. External Functions ..
      INTEGER            AB_ISAMAX
      REAL               AB_CLANHT, AB_SCASUM, AB_SGET06
      EXTERNAL           AB_ISAMAX, AB_CLANHT, AB_SCASUM, AB_SGET06
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAERH, AB_ALAHD, AB_ALASUM, AB_CCOPY, AB_CE
     $RRGT, AB_CGET04,
     $                   AB_CLACPY, AB_CLAPTM, AB_CLARNV, AB_CLATB4, AB_
     $CLATMS, AB_CPTCON,
     $                   AB_CPTRFS, AB_CPTT01, AB_CPTT02, AB_CPTT05, AB_
     $CPTTRF, AB_CPTTRS,
     $                   AB_CAB_SSCAL, AB_SCOPY, AB_SLARNV, AB_SSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, REAL
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 0, 0, 0, 1 / , UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'PT'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL AB_CERRGT( PATH, NOUT )
      INFOT = 0
*
      DO 120 IN = 1, NN
*
*        Do for each value of N in NVAL.
*
         N = NVAL( IN )
         LDA = MAX( 1, N )
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         DO 110 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( N.GT.0 .AND. .NOT.DOTYPE( IMAT ) )
     $         GO TO 110
*
*           Set up parameters with AB_CLATB4.
*
            CALL AB_CLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                   COND, DIST )
*
            ZEROT = IMAT.GE.8 .AND. IMAT.LE.10
            IF( IMAT.LE.6 ) THEN
*
*              Type 1-6:  generate a Hermitian tridiagonal matrix of
*              known condition number in lower triangular band storage.
*
               SRNAMT = 'AB_CLATMS'
               CALL AB_CLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE, CON
     $D,
     $                      ANORM, KL, KU, 'B', A, 2, WORK, INFO )
*
*              Check the error code from AB_CLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL AB_ALAERH( PATH, 'AB_CLATMS', INFO, 0, ' ', N, N,
     $ KL,
     $                         KU, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 110
               END IF
               IZERO = 0
*
*              Copy the matrix to D and E.
*
               IA = 1
               DO 20 I = 1, N - 1
                  D( I ) = REAL( A( IA ) )
                  E( I ) = A( IA+1 )
                  IA = IA + 2
   20          CONTINUE
               IF( N.GT.0 )
     $            D( N ) = REAL( A( IA ) )
            ELSE
*
*              Type 7-12:  generate a diagonally dominant matrix with
*              unknown condition number in the vectors D and E.
*
               IF( .NOT.ZEROT .OR. .NOT.DOTYPE( 7 ) ) THEN
*
*                 Let E be complex, D real, with values from [-1,1].
*
                  CALL AB_SLARNV( 2, ISEED, N, D )
                  CALL AB_CLARNV( 2, ISEED, N-1, E )
*
*                 Make the tridiagonal matrix diagonally dominant.
*
                  IF( N.EQ.1 ) THEN
                     D( 1 ) = ABS( D( 1 ) )
                  ELSE
                     D( 1 ) = ABS( D( 1 ) ) + ABS( E( 1 ) )
                     D( N ) = ABS( D( N ) ) + ABS( E( N-1 ) )
                     DO 30 I = 2, N - 1
                        D( I ) = ABS( D( I ) ) + ABS( E( I ) ) +
     $                           ABS( E( I-1 ) )
   30                CONTINUE
                  END IF
*
*                 Scale D and E so the maximum element is ANORM.
*
                  IX = AB_ISAMAX( N, D, 1 )
                  DMAX = D( IX )
                  CALL AB_SSCAL( N, ANORM / DMAX, D, 1 )
                  CALL AB_CAB_SSCAL( N-1, ANORM / DMAX, E, 1 )
*
               ELSE IF( IZERO.GT.0 ) THEN
*
*                 Reuse the last matrix by copying back the zeroed out
*                 elements.
*
                  IF( IZERO.EQ.1 ) THEN
                     D( 1 ) = Z( 2 )
                     IF( N.GT.1 )
     $                  E( 1 ) = Z( 3 )
                  ELSE IF( IZERO.EQ.N ) THEN
                     E( N-1 ) = Z( 1 )
                     D( N ) = Z( 2 )
                  ELSE
                     E( IZERO-1 ) = Z( 1 )
                     D( IZERO ) = Z( 2 )
                     E( IZERO ) = Z( 3 )
                  END IF
               END IF
*
*              For types 8-10, set one row and column of the matrix to
*              zero.
*
               IZERO = 0
               IF( IMAT.EQ.8 ) THEN
                  IZERO = 1
                  Z( 2 ) = D( 1 )
                  D( 1 ) = ZERO
                  IF( N.GT.1 ) THEN
                     Z( 3 ) = E( 1 )
                     E( 1 ) = ZERO
                  END IF
               ELSE IF( IMAT.EQ.9 ) THEN
                  IZERO = N
                  IF( N.GT.1 ) THEN
                     Z( 1 ) = E( N-1 )
                     E( N-1 ) = ZERO
                  END IF
                  Z( 2 ) = D( N )
                  D( N ) = ZERO
               ELSE IF( IMAT.EQ.10 ) THEN
                  IZERO = ( N+1 ) / 2
                  IF( IZERO.GT.1 ) THEN
                     Z( 1 ) = E( IZERO-1 )
                     Z( 3 ) = E( IZERO )
                     E( IZERO-1 ) = ZERO
                     E( IZERO ) = ZERO
                  END IF
                  Z( 2 ) = D( IZERO )
                  D( IZERO ) = ZERO
               END IF
            END IF
*
            CALL AB_SCOPY( N, D, 1, D( N+1 ), 1 )
            IF( N.GT.1 )
     $         CALL AB_CCOPY( N-1, E, 1, E( N+1 ), 1 )
*
*+    TEST 1
*           Factor A as L*D*L' and compute the ratio
*              norm(L*D*L' - A) / (n * norm(A) * EPS )
*
            CALL AB_CPTTRF( N, D( N+1 ), E( N+1 ), INFO )
*
*           Check error code from AB_CPTTRF.
*
            IF( INFO.NE.IZERO ) THEN
               CALL AB_ALAERH( PATH, 'AB_CPTTRF', INFO, IZERO, ' ', N, N
     $, -1,
     $                      -1, -1, IMAT, NFAIL, NERRS, NOUT )
               GO TO 110
            END IF
*
            IF( INFO.GT.0 ) THEN
               RCONDC = ZERO
               GO TO 100
            END IF
*
            CALL AB_CPTT01( N, D, E, D( N+1 ), E( N+1 ), WORK,
     $                   RESULT( 1 ) )
*
*           Print the test ratio if greater than or equal to THRESH.
*
            IF( RESULT( 1 ).GE.THRESH ) THEN
               IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $            CALL AB_ALAHD( NOUT, PATH )
               WRITE( NOUT, FMT = 9999 )N, IMAT, 1, RESULT( 1 )
               NFAIL = NFAIL + 1
            END IF
            NRUN = NRUN + 1
*
*           Compute RCONDC = 1 / (norm(A) * norm(inv(A))
*
*           Compute norm(A).
*
            ANORM = AB_CLANHT( '1', N, D, E )
*
*           Use AB_CPTTRS to solve for one column at a time of inv(A),
*           computing the maximum column sum as we go.
*
            AINVNM = ZERO
            DO 50 I = 1, N
               DO 40 J = 1, N
                  X( J ) = ZERO
   40          CONTINUE
               X( I ) = ONE
               CALL AB_CPTTRS( 'Lower', N, 1, D( N+1 ), E( N+1 ), X, LDA
     $,
     $                      INFO )
               AINVNM = MAX( AINVNM, AB_SCASUM( N, X, 1 ) )
   50       CONTINUE
            RCONDC = ONE / MAX( ONE, ANORM*AINVNM )
*
            DO 90 IRHS = 1, NNS
               NRHS = NSVAL( IRHS )
*
*           Generate NRHS random solution vectors.
*
               IX = 1
               DO 60 J = 1, NRHS
                  CALL AB_CLARNV( 2, ISEED, N, XACT( IX ) )
                  IX = IX + LDA
   60          CONTINUE
*
               DO 80 IUPLO = 1, 2
*
*              Do first for UPLO = 'U', then for UPLO = 'L'.
*
                  UPLO = UPLOS( IUPLO )
*
*              Set the right hand side.
*
                  CALL AB_CLAPTM( UPLO, N, NRHS, ONE, D, E, XACT, LDA,
     $                         ZERO, B, LDA )
*
*+    TEST 2
*              Solve A*x = b and compute the residual.
*
                  CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
                  CALL AB_CPTTRS( UPLO, N, NRHS, D( N+1 ), E( N+1 ), X,
     $                         LDA, INFO )
*
*              Check error code from AB_CPTTRS.
*
                  IF( INFO.NE.0 )
     $               CALL AB_ALAERH( PATH, 'AB_CPTTRS', INFO, 0, UPLO, N
     $, N,
     $                            -1, -1, NRHS, IMAT, NFAIL, NERRS,
     $                            NOUT )
*
                  CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA )
                  CALL AB_CPTT02( UPLO, N, NRHS, D, E, X, LDA, WORK, LDA
     $,
     $                         RESULT( 2 ) )
*
*+    TEST 3
*              Check solution from generated exact solution.
*
                  CALL AB_CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                         RESULT( 3 ) )
*
*+    TESTS 4, 5, and 6
*              Use iterative refinement to improve the solution.
*
                  SRNAMT = 'AB_CPTRFS'
                  CALL AB_CPTRFS( UPLO, N, NRHS, D, E, D( N+1 ), E( N+1 
     $),
     $                         B, LDA, X, LDA, RWORK, RWORK( NRHS+1 ),
     $                         WORK, RWORK( 2*NRHS+1 ), INFO )
*
*              Check error code from AB_CPTRFS.
*
                  IF( INFO.NE.0 )
     $               CALL AB_ALAERH( PATH, 'AB_CPTRFS', INFO, 0, UPLO, N
     $, N,
     $                            -1, -1, NRHS, IMAT, NFAIL, NERRS,
     $                            NOUT )
*
                  CALL AB_CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                         RESULT( 4 ) )
                  CALL AB_CPTT05( N, NRHS, D, E, B, LDA, X, LDA, XACT, L
     $DA,
     $                         RWORK, RWORK( NRHS+1 ), RESULT( 5 ) )
*
*              Print information about the tests that did not pass the
*              threshold.
*
                  DO 70 K = 2, 6
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL AB_ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9998 )UPLO, N, NRHS, IMAT,
     $                     K, RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
   70             CONTINUE
                  NRUN = NRUN + 5
*
   80          CONTINUE
   90       CONTINUE
*
*+    TEST 7
*           Estimate the reciprocal of the condition number of the
*           matrix.
*
  100       CONTINUE
            SRNAMT = 'AB_CPTCON'
            CALL AB_CPTCON( N, D( N+1 ), E( N+1 ), ANORM, RCOND, RWORK,
     $                   INFO )
*
*           Check error code from AB_CPTCON.
*
            IF( INFO.NE.0 )
     $         CALL AB_ALAERH( PATH, 'AB_CPTCON', INFO, 0, ' ', N, N, -1
     $, -1,
     $                      -1, IMAT, NFAIL, NERRS, NOUT )
*
            RESULT( 7 ) = AB_SGET06( RCOND, RCONDC )
*
*           Print the test ratio if greater than or equal to THRESH.
*
            IF( RESULT( 7 ).GE.THRESH ) THEN
               IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $            CALL AB_ALAHD( NOUT, PATH )
               WRITE( NOUT, FMT = 9999 )N, IMAT, 7, RESULT( 7 )
               NFAIL = NFAIL + 1
            END IF
            NRUN = NRUN + 1
  110    CONTINUE
  120 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' N =', I5, ', type ', I2, ', test ', I2, ', ratio = ',
     $      G12.5 )
 9998 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', NRHS =', I3,
     $        ', type ', I2, ', test ', I2, ', ratio = ', G12.5 )
      RETURN
*
*     End of AB_CCHKPT
*
      END
