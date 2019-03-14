*> \brief \b AB_SCHKPO
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SCHKPO( DOTYPE, NN, NVAL, NNB, NBVAL, NNS, NSVAL,
*                          THRESH, TSTERR, NMAX, A, AFAC, AINV, B, X,
*                          XACT, WORK, RWORK, IWORK, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NMAX, NN, NNB, NNS, NOUT
*       REAL               THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            IWORK( * ), NBVAL( * ), NSVAL( * ), NVAL( * )
*       REAL               A( * ), AFAC( * ), AINV( * ), B( * ),
*      $                   RWORK( * ), WORK( * ), X( * ), XACT( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SCHKPO tests AB_SPOTRF, -TRI, -TRS, -RFS, and -CON
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
*> \param[in] NNB
*> \verbatim
*>          NNB is INTEGER
*>          The number of values of NB contained in the vector NBVAL.
*> \endverbatim
*>
*> \param[in] NBVAL
*> \verbatim
*>          NBVAL is INTEGER array, dimension (NBVAL)
*>          The values of the blocksize NB.
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
*> \param[in] NMAX
*> \verbatim
*>          NMAX is INTEGER
*>          The maximum value permitted for N, used in dimensioning the
*>          work arrays.
*> \endverbatim
*>
*> \param[out] A
*> \verbatim
*>          A is REAL array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AFAC
*> \verbatim
*>          AFAC is REAL array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AINV
*> \verbatim
*>          AINV is REAL array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is REAL array, dimension (NMAX*NSMAX)
*>          where NSMAX is the largest entry in NSVAL.
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is REAL array, dimension (NMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] XACT
*> \verbatim
*>          XACT is REAL array, dimension (NMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is REAL array, dimension
*>                      (NMAX*max(3,NSMAX))
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is REAL array, dimension
*>                      (max(NMAX,2*NSMAX))
*> \endverbatim
*>
*> \param[out] IWORK
*> \verbatim
*>          IWORK is INTEGER array, dimension (NMAX)
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SCHKPO( DOTYPE, NN, NVAL, NNB, NBVAL, NNS, NSVAL,
     $                   THRESH, TSTERR, NMAX, A, AFAC, AINV, B, X,
     $                   XACT, WORK, RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NNB, NNS, NOUT
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NBVAL( * ), NSVAL( * ), NVAL( * )
      REAL               A( * ), AFAC( * ), AINV( * ), B( * ),
     $                   RWORK( * ), WORK( * ), X( * ), XACT( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 9 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 8 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ZEROT
      CHARACTER          DIST, TYPE, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, IMAT, IN, INB, INFO, IOFF, IRHS, IUPLO,
     $                   IZERO, K, KL, KU, LDA, MODE, N, NB, NERRS,
     $                   NFAIL, NIMAT, NRHS, NRUN
      REAL               ANORM, CNDNUM, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      CHARACTER          UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      REAL               AB_SGET06, AB_SLANSY
      EXTERNAL           AB_SGET06, AB_SLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAERH, AB_ALAHD, AB_ALASUM, AB_SERRPO, AB_S
     $GET04, AB_SLACPY,
     $                   AB_SLARHS, AB_SLATB4, AB_SLATMS, AB_SPOCON, AB_
     $SPORFS, AB_SPOT01,
     $                   AB_SPOT02, AB_SPOT03, AB_SPOT05, AB_SPOTRF, AB_
     $SPOTRI, AB_SPOTRS,
     $                   AB_XLAENV
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
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Single precision'
      PATH( 2: 3 ) = 'PO'
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
     $   CALL AB_SERRPO( PATH, NOUT )
      INFOT = 0
      CALL AB_XLAENV( 2, 2 )
*
*     Do for each value of N in NVAL
*
      DO 120 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         IZERO = 0
         DO 110 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 110
*
*           Skip types 3, 4, or 5 if the matrix size is too small.
*
            ZEROT = IMAT.GE.3 .AND. IMAT.LE.5
            IF( ZEROT .AND. N.LT.IMAT-2 )
     $         GO TO 110
*
*           Do first for UPLO = 'U', then for UPLO = 'L'
*
            DO 100 IUPLO = 1, 2
               UPLO = UPLOS( IUPLO )
*
*              Set up parameters with AB_SLATB4 and generate a test matrix
*              with AB_SLATMS.
*
               CALL AB_SLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MO
     $DE,
     $                      CNDNUM, DIST )
*
               SRNAMT = 'AB_SLATMS'
               CALL AB_SLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE,
     $                      CNDNUM, ANORM, KL, KU, UPLO, A, LDA, WORK,
     $                      INFO )
*
*              Check error code from AB_SLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL AB_ALAERH( PATH, 'AB_SLATMS', INFO, 0, UPLO, N, N
     $, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 100
               END IF
*
*              For types 3-5, zero one row and column of the matrix to
*              test that INFO is returned correctly.
*
               IF( ZEROT ) THEN
                  IF( IMAT.EQ.3 ) THEN
                     IZERO = 1
                  ELSE IF( IMAT.EQ.4 ) THEN
                     IZERO = N
                  ELSE
                     IZERO = N / 2 + 1
                  END IF
                  IOFF = ( IZERO-1 )*LDA
*
*                 Set row and column IZERO of A to 0.
*
                  IF( IUPLO.EQ.1 ) THEN
                     DO 20 I = 1, IZERO - 1
                        A( IOFF+I ) = ZERO
   20                CONTINUE
                     IOFF = IOFF + IZERO
                     DO 30 I = IZERO, N
                        A( IOFF ) = ZERO
                        IOFF = IOFF + LDA
   30                CONTINUE
                  ELSE
                     IOFF = IZERO
                     DO 40 I = 1, IZERO - 1
                        A( IOFF ) = ZERO
                        IOFF = IOFF + LDA
   40                CONTINUE
                     IOFF = IOFF - IZERO
                     DO 50 I = IZERO, N
                        A( IOFF+I ) = ZERO
   50                CONTINUE
                  END IF
               ELSE
                  IZERO = 0
               END IF
*
*              Do for each value of NB in NBVAL
*
               DO 90 INB = 1, NNB
                  NB = NBVAL( INB )
                  CALL AB_XLAENV( 1, NB )
*
*                 Compute the L*L' or U'*U factorization of the matrix.
*
                  CALL AB_SLACPY( UPLO, N, N, A, LDA, AFAC, LDA )
                  SRNAMT = 'AB_SPOTRF'
                  CALL AB_SPOTRF( UPLO, N, AFAC, LDA, INFO )
*
*                 Check error code from AB_SPOTRF.
*
                  IF( INFO.NE.IZERO ) THEN
                     CALL AB_ALAERH( PATH, 'AB_SPOTRF', INFO, IZERO, UPL
     $O, N,
     $                            N, -1, -1, NB, IMAT, NFAIL, NERRS,
     $                            NOUT )
                     GO TO 90
                  END IF
*
*                 Skip the tests if INFO is not 0.
*
                  IF( INFO.NE.0 )
     $               GO TO 90
*
*+    TEST 1
*                 Reconstruct matrix from factors and compute residual.
*
                  CALL AB_SLACPY( UPLO, N, N, AFAC, LDA, AINV, LDA )
                  CALL AB_SPOT01( UPLO, N, A, LDA, AINV, LDA, RWORK,
     $                         RESULT( 1 ) )
*
*+    TEST 2
*                 Form the inverse and compute the residual.
*
                  CALL AB_SLACPY( UPLO, N, N, AFAC, LDA, AINV, LDA )
                  SRNAMT = 'AB_SPOTRI'
                  CALL AB_SPOTRI( UPLO, N, AINV, LDA, INFO )
*
*                 Check error code from AB_SPOTRI.
*
                  IF( INFO.NE.0 )
     $               CALL AB_ALAERH( PATH, 'AB_SPOTRI', INFO, 0, UPLO, N
     $, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL AB_SPOT03( UPLO, N, A, LDA, AINV, LDA, WORK, LDA,
     $                         RWORK, RCONDC, RESULT( 2 ) )
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  DO 60 K = 1, 2
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL AB_ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9999 )UPLO, N, NB, IMAT, K,
     $                     RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
   60             CONTINUE
                  NRUN = NRUN + 2
*
*                 Skip the rest of the tests unless this is the first
*                 blocksize.
*
                  IF( INB.NE.1 )
     $               GO TO 90
*
                  DO 80 IRHS = 1, NNS
                     NRHS = NSVAL( IRHS )
*
*+    TEST 3
*                 Solve and compute residual for A * X = B .
*
                     SRNAMT = 'AB_SLARHS'
                     CALL AB_SLARHS( PATH, XTYPE, UPLO, ' ', N, N, KL, K
     $U,
     $                            NRHS, A, LDA, XACT, LDA, B, LDA,
     $                            ISEED, INFO )
                     CALL AB_SLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
                     SRNAMT = 'AB_SPOTRS'
                     CALL AB_SPOTRS( UPLO, N, NRHS, AFAC, LDA, X, LDA,
     $                            INFO )
*
*                 Check error code from AB_SPOTRS.
*
                     IF( INFO.NE.0 )
     $                  CALL AB_ALAERH( PATH, 'AB_SPOTRS', INFO, 0, UPLO
     $, N,
     $                               N, -1, -1, NRHS, IMAT, NFAIL,
     $                               NERRS, NOUT )
*
                     CALL AB_SLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA 
     $)
                     CALL AB_SPOT02( UPLO, N, NRHS, A, LDA, X, LDA, WORK
     $,
     $                            LDA, RWORK, RESULT( 3 ) )
*
*+    TEST 4
*                 Check solution from generated exact solution.
*
                     CALL AB_SGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                            RESULT( 4 ) )
*
*+    TESTS 5, 6, and 7
*                 Use iterative refinement to improve the solution.
*
                     SRNAMT = 'AB_SPORFS'
                     CALL AB_SPORFS( UPLO, N, NRHS, A, LDA, AFAC, LDA, B
     $,
     $                            LDA, X, LDA, RWORK, RWORK( NRHS+1 ),
     $                            WORK, IWORK, INFO )
*
*                 Check error code from AB_SPORFS.
*
                     IF( INFO.NE.0 )
     $                  CALL AB_ALAERH( PATH, 'AB_SPORFS', INFO, 0, UPLO
     $, N,
     $                               N, -1, -1, NRHS, IMAT, NFAIL,
     $                               NERRS, NOUT )
*
                     CALL AB_SGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                            RESULT( 5 ) )
                     CALL AB_SPOT05( UPLO, N, NRHS, A, LDA, B, LDA, X, L
     $DA,
     $                            XACT, LDA, RWORK, RWORK( NRHS+1 ),
     $                            RESULT( 6 ) )
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     DO 70 K = 3, 7
                        IF( RESULT( K ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALAHD( NOUT, PATH )
                           WRITE( NOUT, FMT = 9998 )UPLO, N, NRHS,
     $                        IMAT, K, RESULT( K )
                           NFAIL = NFAIL + 1
                        END IF
   70                CONTINUE
                     NRUN = NRUN + 5
   80             CONTINUE
*
*+    TEST 8
*                 Get an estimate of RCOND = 1/CNDNUM.
*
                  ANORM = AB_SLANSY( '1', UPLO, N, A, LDA, RWORK )
                  SRNAMT = 'AB_SPOCON'
                  CALL AB_SPOCON( UPLO, N, AFAC, LDA, ANORM, RCOND, WORK
     $,
     $                         IWORK, INFO )
*
*                 Check error code from AB_SPOCON.
*
                  IF( INFO.NE.0 )
     $               CALL AB_ALAERH( PATH, 'AB_SPOCON', INFO, 0, UPLO, N
     $, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  RESULT( 8 ) = AB_SGET06( RCOND, RCONDC )
*
*                 Print the test ratio if it is .GE. THRESH.
*
                  IF( RESULT( 8 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL AB_ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9997 )UPLO, N, IMAT, 8,
     $                  RESULT( 8 )
                     NFAIL = NFAIL + 1
                  END IF
                  NRUN = NRUN + 1
   90          CONTINUE
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', NB =', I4, ', type ',
     $      I2, ', test ', I2, ', ratio =', G12.5 )
 9998 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', NRHS=', I3, ', type ',
     $      I2, ', test(', I2, ') =', G12.5 )
 9997 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ',', 10X, ' type ', I2,
     $      ', test(', I2, ') =', G12.5 )
      RETURN
*
*     End of AB_SCHKPO
*
      END
