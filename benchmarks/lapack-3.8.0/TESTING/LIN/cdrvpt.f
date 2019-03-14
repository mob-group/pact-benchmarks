*> \brief \b AB_CDRVPT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CDRVPT( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, A, D,
*                          E, B, X, XACT, WORK, RWORK, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NN, NOUT, NRHS
*       REAL               THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            NVAL( * )
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
*> AB_CDRVPT tests AB_CPTSV and -SVX.
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
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand side vectors to be generated for
*>          each linear system.
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
*>          B is COMPLEX array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is COMPLEX array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] XACT
*> \verbatim
*>          XACT is COMPLEX array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX array, dimension
*>                      (NMAX*max(3,NRHS))
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is REAL array, dimension (NMAX+2*NRHS)
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
      SUBROUTINE AB_CDRVPT( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, A, D
     $,
     $                   E, B, X, XACT, WORK, RWORK, NOUT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NN, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            NVAL( * )
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
      PARAMETER          ( NTESTS = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ZEROT
      CHARACTER          DIST, FACT, TYPE
      CHARACTER*3        PATH
      INTEGER            I, IA, IFACT, IMAT, IN, INFO, IX, IZERO, J, K,
     $                   K1, KL, KU, LDA, MODE, N, NERRS, NFAIL, NIMAT,
     $                   NRUN, NT
      REAL               AINVNM, ANORM, COND, DMAX, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS ), Z( 3 )
*     ..
*     .. External Functions ..
      INTEGER            AB_ISAMAX
      REAL               AB_CLANHT, AB_SCASUM, AB_SGET06
      EXTERNAL           AB_ISAMAX, AB_CLANHT, AB_SCASUM, AB_SGET06
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALADHD, AB_ALAERH, AB_ALASVM, AB_CCOPY, AB_C
     $ERRVX, AB_CGET04,
     $                   AB_CLACPY, AB_CLAPTM, AB_CLARNV, AB_CLASET, AB_
     $CLATB4, AB_CLATMS,
     $                   AB_CPTSV, AB_AB_CPTSVX, AB_CPTT01, AB_CPTT02, A
     $B_CPTT05, AB_CPTTRF,
     $                   AB_CPTTRS, AB_CAB_SSCAL, AB_SCOPY, AB_SLARNV, A
     $B_SSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, CMPLX, MAX
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
      DATA               ISEEDY / 0, 0, 0, 1 /
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
     $   CALL AB_CERRVX( PATH, NOUT )
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
*              Type 1-6:  generate a symmetric tridiagonal matrix of
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
                  D( I ) = A( IA )
                  E( I ) = A( IA+1 )
                  IA = IA + 2
   20          CONTINUE
               IF( N.GT.0 )
     $            D( N ) = A( IA )
            ELSE
*
*              Type 7-12:  generate a diagonally dominant matrix with
*              unknown condition number in the vectors D and E.
*
               IF( .NOT.ZEROT .OR. .NOT.DOTYPE( 7 ) ) THEN
*
*                 Let D and E have values from [-1,1].
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
                  IF( N.GT.1 )
     $               CALL AB_CAB_SSCAL( N-1, ANORM / DMAX, E, 1 )
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
                     E( IZERO-1 ) = ZERO
                     Z( 3 ) = E( IZERO )
                     E( IZERO ) = ZERO
                  END IF
                  Z( 2 ) = D( IZERO )
                  D( IZERO ) = ZERO
               END IF
            END IF
*
*           Generate NRHS random solution vectors.
*
            IX = 1
            DO 40 J = 1, NRHS
               CALL AB_CLARNV( 2, ISEED, N, XACT( IX ) )
               IX = IX + LDA
   40       CONTINUE
*
*           Set the right hand side.
*
            CALL AB_CLAPTM( 'Lower', N, NRHS, ONE, D, E, XACT, LDA, ZERO
     $,
     $                   B, LDA )
*
            DO 100 IFACT = 1, 2
               IF( IFACT.EQ.1 ) THEN
                  FACT = 'F'
               ELSE
                  FACT = 'N'
               END IF
*
*              Compute the condition number for comparison with
*              the value returned by AB_AB_CPTSVX.
*
               IF( ZEROT ) THEN
                  IF( IFACT.EQ.1 )
     $               GO TO 100
                  RCONDC = ZERO
*
               ELSE IF( IFACT.EQ.1 ) THEN
*
*                 Compute the 1-norm of A.
*
                  ANORM = AB_CLANHT( '1', N, D, E )
*
                  CALL AB_SCOPY( N, D, 1, D( N+1 ), 1 )
                  IF( N.GT.1 )
     $               CALL AB_CCOPY( N-1, E, 1, E( N+1 ), 1 )
*
*                 Factor the matrix A.
*
                  CALL AB_CPTTRF( N, D( N+1 ), E( N+1 ), INFO )
*
*                 Use AB_CPTTRS to solve for one column at a time of
*                 inv(A), computing the maximum column sum as we go.
*
                  AINVNM = ZERO
                  DO 60 I = 1, N
                     DO 50 J = 1, N
                        X( J ) = ZERO
   50                CONTINUE
                     X( I ) = ONE
                     CALL AB_CPTTRS( 'Lower', N, 1, D( N+1 ), E( N+1 ), 
     $X,
     $                            LDA, INFO )
                     AINVNM = MAX( AINVNM, AB_SCASUM( N, X, 1 ) )
   60             CONTINUE
*
*                 Compute the 1-norm condition number of A.
*
                  IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                     RCONDC = ONE
                  ELSE
                     RCONDC = ( ONE / ANORM ) / AINVNM
                  END IF
               END IF
*
               IF( IFACT.EQ.2 ) THEN
*
*                 --- Test AB_CPTSV --
*
                  CALL AB_SCOPY( N, D, 1, D( N+1 ), 1 )
                  IF( N.GT.1 )
     $               CALL AB_CCOPY( N-1, E, 1, E( N+1 ), 1 )
                  CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
*                 Factor A as L*D*L' and solve the system A*X = B.
*
                  SRNAMT = 'AB_CPTSV '
                  CALL AB_CPTSV( N, NRHS, D( N+1 ), E( N+1 ), X, LDA,
     $                        INFO )
*
*                 Check error code from AB_CPTSV .
*
                  IF( INFO.NE.IZERO )
     $               CALL AB_ALAERH( PATH, 'AB_CPTSV ', INFO, IZERO, ' '
     $, N,
     $                            N, 1, 1, NRHS, IMAT, NFAIL, NERRS,
     $                            NOUT )
                  NT = 0
                  IF( IZERO.EQ.0 ) THEN
*
*                    Check the factorization by computing the ratio
*                       norm(L*D*L' - A) / (n * norm(A) * EPS )
*
                     CALL AB_CPTT01( N, D, E, D( N+1 ), E( N+1 ), WORK,
     $                            RESULT( 1 ) )
*
*                    Compute the residual in the solution.
*
                     CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA 
     $)
                     CALL AB_CPTT02( 'Lower', N, NRHS, D, E, X, LDA, WOR
     $K,
     $                            LDA, RESULT( 2 ) )
*
*                    Check solution from generated exact solution.
*
                     CALL AB_CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                            RESULT( 3 ) )
                     NT = 3
                  END IF
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  DO 70 K = 1, NT
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL AB_ALADHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9999 )'AB_CPTSV ', N, IMAT, K
     $,
     $                     RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
   70             CONTINUE
                  NRUN = NRUN + NT
               END IF
*
*              --- Test AB_AB_CPTSVX ---
*
               IF( IFACT.GT.1 ) THEN
*
*                 Initialize D( N+1:2*N ) and E( N+1:2*N ) to zero.
*
                  DO 80 I = 1, N - 1
                     D( N+I ) = ZERO
                     E( N+I ) = ZERO
   80             CONTINUE
                  IF( N.GT.0 )
     $               D( N+N ) = ZERO
               END IF
*
               CALL AB_CLASET( 'Full', N, NRHS, CMPLX( ZERO ),
     $                      CMPLX( ZERO ), X, LDA )
*
*              Solve the system and compute the condition number and
*              error bounds using AB_AB_CPTSVX.
*
               SRNAMT = 'AB_AB_CPTSVX'
               CALL AB_AB_CPTSVX( FACT, N, NRHS, D, E, D( N+1 ), E( N+1 
     $), B,
     $                      LDA, X, LDA, RCOND, RWORK, RWORK( NRHS+1 ),
     $                      WORK, RWORK( 2*NRHS+1 ), INFO )
*
*              Check the error code from AB_AB_CPTSVX.
*
               IF( INFO.NE.IZERO )
     $            CALL AB_ALAERH( PATH, 'AB_AB_CPTSVX', INFO, IZERO, FAC
     $T, N, N,
     $                         1, 1, NRHS, IMAT, NFAIL, NERRS, NOUT )
               IF( IZERO.EQ.0 ) THEN
                  IF( IFACT.EQ.2 ) THEN
*
*                    Check the factorization by computing the ratio
*                       norm(L*D*L' - A) / (n * norm(A) * EPS )
*
                     K1 = 1
                     CALL AB_CPTT01( N, D, E, D( N+1 ), E( N+1 ), WORK,
     $                            RESULT( 1 ) )
                  ELSE
                     K1 = 2
                  END IF
*
*                 Compute the residual in the solution.
*
                  CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA )
                  CALL AB_CPTT02( 'Lower', N, NRHS, D, E, X, LDA, WORK,
     $                         LDA, RESULT( 2 ) )
*
*                 Check solution from generated exact solution.
*
                  CALL AB_CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                         RESULT( 3 ) )
*
*                 Check error bounds from iterative refinement.
*
                  CALL AB_CPTT05( N, NRHS, D, E, B, LDA, X, LDA, XACT, L
     $DA,
     $                         RWORK, RWORK( NRHS+1 ), RESULT( 4 ) )
               ELSE
                  K1 = 6
               END IF
*
*              Check the reciprocal of the condition number.
*
               RESULT( 6 ) = AB_SGET06( RCOND, RCONDC )
*
*              Print information about the tests that did not pass
*              the threshold.
*
               DO 90 K = K1, 6
                  IF( RESULT( K ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL AB_ALADHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9998 )'AB_AB_CPTSVX', FACT, N, I
     $MAT,
     $                  K, RESULT( K )
                     NFAIL = NFAIL + 1
                  END IF
   90          CONTINUE
               NRUN = NRUN + 7 - K1
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( 1X, A, ', N =', I5, ', type ', I2, ', test ', I2,
     $      ', ratio = ', G12.5 )
 9998 FORMAT( 1X, A, ', FACT=''', A1, ''', N =', I5, ', type ', I2,
     $      ', test ', I2, ', ratio = ', G12.5 )
      RETURN
*
*     End of AB_CDRVPT
*
      END
