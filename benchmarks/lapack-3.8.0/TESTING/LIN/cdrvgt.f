*> \brief \b AB_CDRVGT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CDRVGT( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, A, AF,
*                          B, X, XACT, WORK, RWORK, IWORK, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NN, NOUT, NRHS
*       REAL               THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            IWORK( * ), NVAL( * )
*       REAL               RWORK( * )
*       COMPLEX            A( * ), AF( * ), B( * ), WORK( * ), X( * ),
*      $                   XACT( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CDRVGT tests AB_CGTSV and -SVX.
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
*>          The number of right hand sides, NRHS >= 0.
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
*>          A is COMPLEX array, dimension (NMAX*4)
*> \endverbatim
*>
*> \param[out] AF
*> \verbatim
*>          AF is COMPLEX array, dimension (NMAX*4)
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
*> \param[out] IWORK
*> \verbatim
*>          IWORK is INTEGER array, dimension (2*NMAX)
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
      SUBROUTINE AB_CDRVGT( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, A, A
     $F,
     $                   B, X, XACT, WORK, RWORK, IWORK, NOUT )
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
      INTEGER            IWORK( * ), NVAL( * )
      REAL               RWORK( * )
      COMPLEX            A( * ), AF( * ), B( * ), WORK( * ), X( * ),
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
      LOGICAL            TRFCON, ZEROT
      CHARACTER          DIST, FACT, TRANS, TYPE
      CHARACTER*3        PATH
      INTEGER            I, IFACT, IMAT, IN, INFO, ITRAN, IX, IZERO, J,
     $                   K, K1, KL, KOFF, KU, LDA, M, MODE, N, NERRS,
     $                   NFAIL, NIMAT, NRUN, NT
      REAL               AINVNM, ANORM, ANORMI, ANORMO, COND, RCOND,
     $                   RCONDC, RCONDI, RCONDO
*     ..
*     .. Local Arrays ..
      CHARACTER          TRANSS( 3 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS ), Z( 3 )
*     ..
*     .. External Functions ..
      REAL               AB_CLANGT, AB_SCASUM, AB_SGET06
      EXTERNAL           AB_CLANGT, AB_SCASUM, AB_SGET06
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALADHD, AB_ALAERH, AB_ALASVM, AB_CCOPY, AB_C
     $ERRVX, AB_CGET04,
     $                   AB_CGTSV, AB_AB_CGTSVX, AB_CGTT01, AB_CGTT02, A
     $B_CGTT05, AB_CGTTRF,
     $                   AB_CGTTRS, AB_CLACPY, AB_CLAGTM, AB_CLARNV, AB_
     $CLASET, AB_CLATB4,
     $                   AB_CLATMS, AB_CAB_SSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MAX
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
      DATA               ISEEDY / 0, 0, 0, 1 / , TRANSS / 'N', 'T',
     $                   'C' /
*     ..
*     .. Executable Statements ..
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'GT'
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
      DO 140 IN = 1, NN
*
*        Do for each value of N in NVAL.
*
         N = NVAL( IN )
         M = MAX( N-1, 0 )
         LDA = MAX( 1, N )
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         DO 130 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 130
*
*           Set up parameters with AB_CLATB4.
*
            CALL AB_CLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                   COND, DIST )
*
            ZEROT = IMAT.GE.8 .AND. IMAT.LE.10
            IF( IMAT.LE.6 ) THEN
*
*              Types 1-6:  generate matrices of known condition number.
*
               KOFF = MAX( 2-KU, 3-MAX( 1, N ) )
               SRNAMT = 'AB_CLATMS'
               CALL AB_CLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE, CON
     $D,
     $                      ANORM, KL, KU, 'Z', AF( KOFF ), 3, WORK,
     $                      INFO )
*
*              Check the error code from AB_CLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL AB_ALAERH( PATH, 'AB_CLATMS', INFO, 0, ' ', N, N,
     $ KL,
     $                         KU, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 130
               END IF
               IZERO = 0
*
               IF( N.GT.1 ) THEN
                  CALL AB_CCOPY( N-1, AF( 4 ), 3, A, 1 )
                  CALL AB_CCOPY( N-1, AF( 3 ), 3, A( N+M+1 ), 1 )
               END IF
               CALL AB_CCOPY( N, AF( 2 ), 3, A( M+1 ), 1 )
            ELSE
*
*              Types 7-12:  generate tridiagonal matrices with
*              unknown condition numbers.
*
               IF( .NOT.ZEROT .OR. .NOT.DOTYPE( 7 ) ) THEN
*
*                 Generate a matrix with elements from [-1,1].
*
                  CALL AB_CLARNV( 2, ISEED, N+2*M, A )
                  IF( ANORM.NE.ONE )
     $               CALL AB_CAB_SSCAL( N+2*M, ANORM, A, 1 )
               ELSE IF( IZERO.GT.0 ) THEN
*
*                 Reuse the last matrix by copying back the zeroed out
*                 elements.
*
                  IF( IZERO.EQ.1 ) THEN
                     A( N ) = Z( 2 )
                     IF( N.GT.1 )
     $                  A( 1 ) = Z( 3 )
                  ELSE IF( IZERO.EQ.N ) THEN
                     A( 3*N-2 ) = Z( 1 )
                     A( 2*N-1 ) = Z( 2 )
                  ELSE
                     A( 2*N-2+IZERO ) = Z( 1 )
                     A( N-1+IZERO ) = Z( 2 )
                     A( IZERO ) = Z( 3 )
                  END IF
               END IF
*
*              If IMAT > 7, set one column of the matrix to 0.
*
               IF( .NOT.ZEROT ) THEN
                  IZERO = 0
               ELSE IF( IMAT.EQ.8 ) THEN
                  IZERO = 1
                  Z( 2 ) = A( N )
                  A( N ) = ZERO
                  IF( N.GT.1 ) THEN
                     Z( 3 ) = A( 1 )
                     A( 1 ) = ZERO
                  END IF
               ELSE IF( IMAT.EQ.9 ) THEN
                  IZERO = N
                  Z( 1 ) = A( 3*N-2 )
                  Z( 2 ) = A( 2*N-1 )
                  A( 3*N-2 ) = ZERO
                  A( 2*N-1 ) = ZERO
               ELSE
                  IZERO = ( N+1 ) / 2
                  DO 20 I = IZERO, N - 1
                     A( 2*N-2+I ) = ZERO
                     A( N-1+I ) = ZERO
                     A( I ) = ZERO
   20             CONTINUE
                  A( 3*N-2 ) = ZERO
                  A( 2*N-1 ) = ZERO
               END IF
            END IF
*
            DO 120 IFACT = 1, 2
               IF( IFACT.EQ.1 ) THEN
                  FACT = 'F'
               ELSE
                  FACT = 'N'
               END IF
*
*              Compute the condition number for comparison with
*              the value returned by AB_AB_CGTSVX.
*
               IF( ZEROT ) THEN
                  IF( IFACT.EQ.1 )
     $               GO TO 120
                  RCONDO = ZERO
                  RCONDI = ZERO
*
               ELSE IF( IFACT.EQ.1 ) THEN
                  CALL AB_CCOPY( N+2*M, A, 1, AF, 1 )
*
*                 Compute the 1-norm and infinity-norm of A.
*
                  ANORMO = AB_CLANGT( '1', N, A, A( M+1 ), A( N+M+1 ) )
                  ANORMI = AB_CLANGT( 'I', N, A, A( M+1 ), A( N+M+1 ) )
*
*                 Factor the matrix A.
*
                  CALL AB_CGTTRF( N, AF, AF( M+1 ), AF( N+M+1 ),
     $                         AF( N+2*M+1 ), IWORK, INFO )
*
*                 Use AB_CGTTRS to solve for one column at a time of
*                 inv(A), computing the maximum column sum as we go.
*
                  AINVNM = ZERO
                  DO 40 I = 1, N
                     DO 30 J = 1, N
                        X( J ) = ZERO
   30                CONTINUE
                     X( I ) = ONE
                     CALL AB_CGTTRS( 'No transpose', N, 1, AF, AF( M+1 )
     $,
     $                            AF( N+M+1 ), AF( N+2*M+1 ), IWORK, X,
     $                            LDA, INFO )
                     AINVNM = MAX( AINVNM, AB_SCASUM( N, X, 1 ) )
   40             CONTINUE
*
*                 Compute the 1-norm condition number of A.
*
                  IF( ANORMO.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                     RCONDO = ONE
                  ELSE
                     RCONDO = ( ONE / ANORMO ) / AINVNM
                  END IF
*
*                 Use AB_CGTTRS to solve for one column at a time of
*                 inv(A'), computing the maximum column sum as we go.
*
                  AINVNM = ZERO
                  DO 60 I = 1, N
                     DO 50 J = 1, N
                        X( J ) = ZERO
   50                CONTINUE
                     X( I ) = ONE
                     CALL AB_CGTTRS( 'Conjugate transpose', N, 1, AF,
     $                            AF( M+1 ), AF( N+M+1 ), AF( N+2*M+1 ),
     $                            IWORK, X, LDA, INFO )
                     AINVNM = MAX( AINVNM, AB_SCASUM( N, X, 1 ) )
   60             CONTINUE
*
*                 Compute the infinity-norm condition number of A.
*
                  IF( ANORMI.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                     RCONDI = ONE
                  ELSE
                     RCONDI = ( ONE / ANORMI ) / AINVNM
                  END IF
               END IF
*
               DO 110 ITRAN = 1, 3
                  TRANS = TRANSS( ITRAN )
                  IF( ITRAN.EQ.1 ) THEN
                     RCONDC = RCONDO
                  ELSE
                     RCONDC = RCONDI
                  END IF
*
*                 Generate NRHS random solution vectors.
*
                  IX = 1
                  DO 70 J = 1, NRHS
                     CALL AB_CLARNV( 2, ISEED, N, XACT( IX ) )
                     IX = IX + LDA
   70             CONTINUE
*
*                 Set the right hand side.
*
                  CALL AB_CLAGTM( TRANS, N, NRHS, ONE, A, A( M+1 ),
     $                         A( N+M+1 ), XACT, LDA, ZERO, B, LDA )
*
                  IF( IFACT.EQ.2 .AND. ITRAN.EQ.1 ) THEN
*
*                    --- Test AB_CGTSV  ---
*
*                    Solve the system using Gaussian elimination with
*                    partial pivoting.
*
                     CALL AB_CCOPY( N+2*M, A, 1, AF, 1 )
                     CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
                     SRNAMT = 'AB_CGTSV '
                     CALL AB_CGTSV( N, NRHS, AF, AF( M+1 ), AF( N+M+1 ),
     $ X,
     $                           LDA, INFO )
*
*                    Check error code from AB_CGTSV .
*
                     IF( INFO.NE.IZERO )
     $                  CALL AB_ALAERH( PATH, 'AB_CGTSV ', INFO, IZERO, 
     $' ',
     $                               N, N, 1, 1, NRHS, IMAT, NFAIL,
     $                               NERRS, NOUT )
                     NT = 1
                     IF( IZERO.EQ.0 ) THEN
*
*                       Check residual of computed solution.
*
                        CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, WORK,
     $                               LDA )
                        CALL AB_CGTT02( TRANS, N, NRHS, A, A( M+1 ),
     $                               A( N+M+1 ), X, LDA, WORK, LDA,
     $                               RESULT( 2 ) )
*
*                       Check solution from generated exact solution.
*
                        CALL AB_CGET04( N, NRHS, X, LDA, XACT, LDA, RCON
     $DC,
     $                               RESULT( 3 ) )
                        NT = 3
                     END IF
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     DO 80 K = 2, NT
                        IF( RESULT( K ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           WRITE( NOUT, FMT = 9999 )'AB_CGTSV ', N, IMAT
     $,
     $                        K, RESULT( K )
                           NFAIL = NFAIL + 1
                        END IF
   80                CONTINUE
                     NRUN = NRUN + NT - 1
                  END IF
*
*                 --- Test AB_AB_CGTSVX ---
*
                  IF( IFACT.GT.1 ) THEN
*
*                    Initialize AF to zero.
*
                     DO 90 I = 1, 3*N - 2
                        AF( I ) = ZERO
   90                CONTINUE
                  END IF
                  CALL AB_CLASET( 'Full', N, NRHS, CMPLX( ZERO ),
     $                         CMPLX( ZERO ), X, LDA )
*
*                 Solve the system and compute the condition number and
*                 error bounds using AB_AB_CGTSVX.
*
                  SRNAMT = 'AB_AB_CGTSVX'
                  CALL AB_AB_CGTSVX( FACT, TRANS, N, NRHS, A, A( M+1 ),
     $                         A( N+M+1 ), AF, AF( M+1 ), AF( N+M+1 ),
     $                         AF( N+2*M+1 ), IWORK, B, LDA, X, LDA,
     $                         RCOND, RWORK, RWORK( NRHS+1 ), WORK,
     $                         RWORK( 2*NRHS+1 ), INFO )
*
*                 Check the error code from AB_AB_CGTSVX.
*
                  IF( INFO.NE.IZERO )
     $               CALL AB_ALAERH( PATH, 'AB_AB_CGTSVX', INFO, IZERO,
     $                            FACT // TRANS, N, N, 1, 1, NRHS, IMAT,
     $                            NFAIL, NERRS, NOUT )
*
                  IF( IFACT.GE.2 ) THEN
*
*                    Reconstruct matrix from factors and compute
*                    residual.
*
                     CALL AB_CGTT01( N, A, A( M+1 ), A( N+M+1 ), AF,
     $                            AF( M+1 ), AF( N+M+1 ), AF( N+2*M+1 ),
     $                            IWORK, WORK, LDA, RWORK, RESULT( 1 ) )
                     K1 = 1
                  ELSE
                     K1 = 2
                  END IF
*
                  IF( INFO.EQ.0 ) THEN
                     TRFCON = .FALSE.
*
*                    Check residual of computed solution.
*
                     CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA 
     $)
                     CALL AB_CGTT02( TRANS, N, NRHS, A, A( M+1 ),
     $                            A( N+M+1 ), X, LDA, WORK, LDA,
     $                            RESULT( 2 ) )
*
*                    Check solution from generated exact solution.
*
                     CALL AB_CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                            RESULT( 3 ) )
*
*                    Check the error bounds from iterative refinement.
*
                     CALL AB_CGTT05( TRANS, N, NRHS, A, A( M+1 ),
     $                            A( N+M+1 ), B, LDA, X, LDA, XACT, LDA,
     $                            RWORK, RWORK( NRHS+1 ), RESULT( 4 ) )
                     NT = 5
                  END IF
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  DO 100 K = K1, NT
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL AB_ALADHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9998 )'AB_AB_CGTSVX', FACT, T
     $RANS,
     $                     N, IMAT, K, RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
  100             CONTINUE
*
*                 Check the reciprocal of the condition number.
*
                  RESULT( 6 ) = AB_SGET06( RCOND, RCONDC )
                  IF( RESULT( 6 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL AB_ALADHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9998 )'AB_AB_CGTSVX', FACT, TRAN
     $S, N,
     $                  IMAT, K, RESULT( K )
                     NFAIL = NFAIL + 1
                  END IF
                  NRUN = NRUN + NT - K1 + 2
*
  110          CONTINUE
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( 1X, A, ', N =', I5, ', type ', I2, ', test ', I2,
     $      ', ratio = ', G12.5 )
 9998 FORMAT( 1X, A, ', FACT=''', A1, ''', TRANS=''', A1, ''', N =',
     $      I5, ', type ', I2, ', test ', I2, ', ratio = ', G12.5 )
      RETURN
*
*     End of AB_CDRVGT
*
      END
