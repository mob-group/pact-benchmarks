*> \brief \b AB_DCHKPB
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DCHKPB( DOTYPE, NN, NVAL, NNB, NBVAL, NNS, NSVAL,
*                          THRESH, TSTERR, NMAX, A, AFAC, AINV, B, X,
*                          XACT, WORK, RWORK, IWORK, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NMAX, NN, NNB, NNS, NOUT
*       DOUBLE PRECISION   THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            IWORK( * ), NBVAL( * ), NSVAL( * ), NVAL( * )
*       DOUBLE PRECISION   A( * ), AFAC( * ), AINV( * ), B( * ),
*      $                   RWORK( * ), WORK( * ), X( * ), XACT( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DCHKPB tests AB_DPBTRF, -TRS, -RFS, and -CON.
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
*>          THRESH is DOUBLE PRECISION
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
*>          A is DOUBLE PRECISION array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AFAC
*> \verbatim
*>          AFAC is DOUBLE PRECISION array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AINV
*> \verbatim
*>          AINV is DOUBLE PRECISION array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension (NMAX*NSMAX)
*>          where NSMAX is the largest entry in NSVAL.
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is DOUBLE PRECISION array, dimension (NMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] XACT
*> \verbatim
*>          XACT is DOUBLE PRECISION array, dimension (NMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension
*>                      (NMAX*max(3,NSMAX))
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DCHKPB( DOTYPE, NN, NVAL, NNB, NBVAL, NNS, NSVAL,
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
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NBVAL( * ), NSVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), AFAC( * ), AINV( * ), B( * ),
     $                   RWORK( * ), WORK( * ), X( * ), XACT( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NTYPES, NTESTS
      PARAMETER          ( NTYPES = 8, NTESTS = 7 )
      INTEGER            NBW
      PARAMETER          ( NBW = 4 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ZEROT
      CHARACTER          DIST, PACKIT, TYPE, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, I1, I2, IKD, IMAT, IN, INB, INFO, IOFF,
     $                   IRHS, IUPLO, IW, IZERO, K, KD, KL, KOFF, KU,
     $                   LDA, LDAB, MODE, N, NB, NERRS, NFAIL, NIMAT,
     $                   NKD, NRHS, NRUN
      DOUBLE PRECISION   AINVNM, ANORM, CNDNUM, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 ), KDVAL( NBW )
      DOUBLE PRECISION   RESULT( NTESTS )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DGET06, AB_DLANGE, AB_DLANSB
      EXTERNAL           AB_DGET06, AB_DLANGE, AB_DLANSB
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAERH, AB_ALAHD, AB_ALASUM, AB_DCOPY, AB_DE
     $RRPO, AB_DGET04,
     $                   AB_DLACPY, AB_DLARHS, AB_DLASET, AB_DLATB4, AB_
     $DLATMS, AB_DPBCON,
     $                   AB_DPBRFS, AB_DPBT01, AB_DPBT02, AB_DPBT05, AB_
     $DPBTRF, AB_DPBTRS,
     $                   AB_DSWAP, AB_XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'PB'
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
     $   CALL AB_DERRPO( PATH, NOUT )
      INFOT = 0
      CALL AB_XLAENV( 2, 2 )
      KDVAL( 1 ) = 0
*
*     Do for each value of N in NVAL
*
      DO 90 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
*
*        Set limits on the number of loop iterations.
*
         NKD = MAX( 1, MIN( N, 4 ) )
         NIMAT = NTYPES
         IF( N.EQ.0 )
     $      NIMAT = 1
*
         KDVAL( 2 ) = N + ( N+1 ) / 4
         KDVAL( 3 ) = ( 3*N-1 ) / 4
         KDVAL( 4 ) = ( N+1 ) / 4
*
         DO 80 IKD = 1, NKD
*
*           Do for KD = 0, (5*N+1)/4, (3N-1)/4, and (N+1)/4. This order
*           makes it easier to skip redundant values for small values
*           of N.
*
            KD = KDVAL( IKD )
            LDAB = KD + 1
*
*           Do first for UPLO = 'U', then for UPLO = 'L'
*
            DO 70 IUPLO = 1, 2
               KOFF = 1
               IF( IUPLO.EQ.1 ) THEN
                  UPLO = 'U'
                  KOFF = MAX( 1, KD+2-N )
                  PACKIT = 'Q'
               ELSE
                  UPLO = 'L'
                  PACKIT = 'B'
               END IF
*
               DO 60 IMAT = 1, NIMAT
*
*                 Do the tests only if DOTYPE( IMAT ) is true.
*
                  IF( .NOT.DOTYPE( IMAT ) )
     $               GO TO 60
*
*                 Skip types 2, 3, or 4 if the matrix size is too small.
*
                  ZEROT = IMAT.GE.2 .AND. IMAT.LE.4
                  IF( ZEROT .AND. N.LT.IMAT-1 )
     $               GO TO 60
*
                  IF( .NOT.ZEROT .OR. .NOT.DOTYPE( 1 ) ) THEN
*
*                    Set up parameters with AB_DLATB4 and generate a test
*                    matrix with AB_DLATMS.
*
                     CALL AB_DLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANO
     $RM,
     $                            MODE, CNDNUM, DIST )
*
                     SRNAMT = 'AB_DLATMS'
                     CALL AB_DLATMS( N, N, DIST, ISEED, TYPE, RWORK, MOD
     $E,
     $                            CNDNUM, ANORM, KD, KD, PACKIT,
     $                            A( KOFF ), LDAB, WORK, INFO )
*
*                    Check error code from AB_DLATMS.
*
                     IF( INFO.NE.0 ) THEN
                        CALL AB_ALAERH( PATH, 'AB_DLATMS', INFO, 0, UPLO
     $, N,
     $                               N, KD, KD, -1, IMAT, NFAIL, NERRS,
     $                               NOUT )
                        GO TO 60
                     END IF
                  ELSE IF( IZERO.GT.0 ) THEN
*
*                    Use the same matrix for types 3 and 4 as for type
*                    2 by copying back the zeroed out column,
*
                     IW = 2*LDA + 1
                     IF( IUPLO.EQ.1 ) THEN
                        IOFF = ( IZERO-1 )*LDAB + KD + 1
                        CALL AB_DCOPY( IZERO-I1, WORK( IW ), 1,
     $                              A( IOFF-IZERO+I1 ), 1 )
                        IW = IW + IZERO - I1
                        CALL AB_DCOPY( I2-IZERO+1, WORK( IW ), 1,
     $                              A( IOFF ), MAX( LDAB-1, 1 ) )
                     ELSE
                        IOFF = ( I1-1 )*LDAB + 1
                        CALL AB_DCOPY( IZERO-I1, WORK( IW ), 1,
     $                              A( IOFF+IZERO-I1 ),
     $                              MAX( LDAB-1, 1 ) )
                        IOFF = ( IZERO-1 )*LDAB + 1
                        IW = IW + IZERO - I1
                        CALL AB_DCOPY( I2-IZERO+1, WORK( IW ), 1,
     $                              A( IOFF ), 1 )
                     END IF
                  END IF
*
*                 For types 2-4, zero one row and column of the matrix
*                 to test that INFO is returned correctly.
*
                  IZERO = 0
                  IF( ZEROT ) THEN
                     IF( IMAT.EQ.2 ) THEN
                        IZERO = 1
                     ELSE IF( IMAT.EQ.3 ) THEN
                        IZERO = N
                     ELSE
                        IZERO = N / 2 + 1
                     END IF
*
*                    Save the zeroed out row and column in WORK(*,3)
*
                     IW = 2*LDA
                     DO 20 I = 1, MIN( 2*KD+1, N )
                        WORK( IW+I ) = ZERO
   20                CONTINUE
                     IW = IW + 1
                     I1 = MAX( IZERO-KD, 1 )
                     I2 = MIN( IZERO+KD, N )
*
                     IF( IUPLO.EQ.1 ) THEN
                        IOFF = ( IZERO-1 )*LDAB + KD + 1
                        CALL AB_DSWAP( IZERO-I1, A( IOFF-IZERO+I1 ), 1,
     $                              WORK( IW ), 1 )
                        IW = IW + IZERO - I1
                        CALL AB_DSWAP( I2-IZERO+1, A( IOFF ),
     $                              MAX( LDAB-1, 1 ), WORK( IW ), 1 )
                     ELSE
                        IOFF = ( I1-1 )*LDAB + 1
                        CALL AB_DSWAP( IZERO-I1, A( IOFF+IZERO-I1 ),
     $                              MAX( LDAB-1, 1 ), WORK( IW ), 1 )
                        IOFF = ( IZERO-1 )*LDAB + 1
                        IW = IW + IZERO - I1
                        CALL AB_DSWAP( I2-IZERO+1, A( IOFF ), 1,
     $                              WORK( IW ), 1 )
                     END IF
                  END IF
*
*                 Do for each value of NB in NBVAL
*
                  DO 50 INB = 1, NNB
                     NB = NBVAL( INB )
                     CALL AB_XLAENV( 1, NB )
*
*                    Compute the L*L' or U'*U factorization of the band
*                    matrix.
*
                     CALL AB_DLACPY( 'Full', KD+1, N, A, LDAB, AFAC, LDA
     $B )
                     SRNAMT = 'AB_DPBTRF'
                     CALL AB_DPBTRF( UPLO, N, KD, AFAC, LDAB, INFO )
*
*                    Check error code from AB_DPBTRF.
*
                     IF( INFO.NE.IZERO ) THEN
                        CALL AB_ALAERH( PATH, 'AB_DPBTRF', INFO, IZERO, 
     $UPLO,
     $                               N, N, KD, KD, NB, IMAT, NFAIL,
     $                               NERRS, NOUT )
                        GO TO 50
                     END IF
*
*                    Skip the tests if INFO is not 0.
*
                     IF( INFO.NE.0 )
     $                  GO TO 50
*
*+    TEST 1
*                    Reconstruct matrix from factors and compute
*                    residual.
*
                     CALL AB_DLACPY( 'Full', KD+1, N, AFAC, LDAB, AINV,
     $                            LDAB )
                     CALL AB_DPBT01( UPLO, N, KD, A, LDAB, AINV, LDAB,
     $                            RWORK, RESULT( 1 ) )
*
*                    Print the test ratio if it is .GE. THRESH.
*
                     IF( RESULT( 1 ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL AB_ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9999 )UPLO, N, KD, NB, IMAT,
     $                     1, RESULT( 1 )
                        NFAIL = NFAIL + 1
                     END IF
                     NRUN = NRUN + 1
*
*                    Only do other tests if this is the first blocksize.
*
                     IF( INB.GT.1 )
     $                  GO TO 50
*
*                    Form the inverse of A so we can get a good estimate
*                    of RCONDC = 1/(norm(A) * norm(inv(A))).
*
                     CALL AB_DLASET( 'Full', N, N, ZERO, ONE, AINV, LDA 
     $)
                     SRNAMT = 'AB_DPBTRS'
                     CALL AB_DPBTRS( UPLO, N, KD, N, AFAC, LDAB, AINV, L
     $DA,
     $                            INFO )
*
*                    Compute RCONDC = 1/(norm(A) * norm(inv(A))).
*
                     ANORM = AB_DLANSB( '1', UPLO, N, KD, A, LDAB, RWORK
     $ )
                     AINVNM = AB_DLANGE( '1', N, N, AINV, LDA, RWORK )
                     IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                        RCONDC = ONE
                     ELSE
                        RCONDC = ( ONE / ANORM ) / AINVNM
                     END IF
*
                     DO 40 IRHS = 1, NNS
                        NRHS = NSVAL( IRHS )
*
*+    TEST 2
*                    Solve and compute residual for A * X = B.
*
                        SRNAMT = 'AB_DLARHS'
                        CALL AB_DLARHS( PATH, XTYPE, UPLO, ' ', N, N, KD
     $,
     $                               KD, NRHS, A, LDAB, XACT, LDA, B,
     $                               LDA, ISEED, INFO )
                        CALL AB_DLACPY( 'Full', N, NRHS, B, LDA, X, LDA 
     $)
*
                        SRNAMT = 'AB_DPBTRS'
                        CALL AB_DPBTRS( UPLO, N, KD, NRHS, AFAC, LDAB, X
     $,
     $                               LDA, INFO )
*
*                    Check error code from AB_DPBTRS.
*
                        IF( INFO.NE.0 )
     $                     CALL AB_ALAERH( PATH, 'AB_DPBTRS', INFO, 0, U
     $PLO,
     $                                  N, N, KD, KD, NRHS, IMAT, NFAIL,
     $                                  NERRS, NOUT )
*
                        CALL AB_DLACPY( 'Full', N, NRHS, B, LDA, WORK,
     $                               LDA )
                        CALL AB_DPBT02( UPLO, N, KD, NRHS, A, LDAB, X, L
     $DA,
     $                               WORK, LDA, RWORK, RESULT( 2 ) )
*
*+    TEST 3
*                    Check solution from generated exact solution.
*
                        CALL AB_DGET04( N, NRHS, X, LDA, XACT, LDA, RCON
     $DC,
     $                               RESULT( 3 ) )
*
*+    TESTS 4, 5, and 6
*                    Use iterative refinement to improve the solution.
*
                        SRNAMT = 'AB_DPBRFS'
                        CALL AB_DPBRFS( UPLO, N, KD, NRHS, A, LDAB, AFAC
     $,
     $                               LDAB, B, LDA, X, LDA, RWORK,
     $                               RWORK( NRHS+1 ), WORK, IWORK,
     $                               INFO )
*
*                    Check error code from AB_DPBRFS.
*
                        IF( INFO.NE.0 )
     $                     CALL AB_ALAERH( PATH, 'AB_DPBRFS', INFO, 0, U
     $PLO,
     $                                  N, N, KD, KD, NRHS, IMAT, NFAIL,
     $                                  NERRS, NOUT )
*
                        CALL AB_DGET04( N, NRHS, X, LDA, XACT, LDA, RCON
     $DC,
     $                               RESULT( 4 ) )
                        CALL AB_DPBT05( UPLO, N, KD, NRHS, A, LDAB, B, L
     $DA,
     $                               X, LDA, XACT, LDA, RWORK,
     $                               RWORK( NRHS+1 ), RESULT( 5 ) )
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 30 K = 2, 6
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL AB_ALAHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9998 )UPLO, N, KD,
     $                           NRHS, IMAT, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   30                   CONTINUE
                        NRUN = NRUN + 5
   40                CONTINUE
*
*+    TEST 7
*                    Get an estimate of RCOND = 1/CNDNUM.
*
                     SRNAMT = 'AB_DPBCON'
                     CALL AB_DPBCON( UPLO, N, KD, AFAC, LDAB, ANORM, RCO
     $ND,
     $                            WORK, IWORK, INFO )
*
*                    Check error code from AB_DPBCON.
*
                     IF( INFO.NE.0 )
     $                  CALL AB_ALAERH( PATH, 'AB_DPBCON', INFO, 0, UPLO
     $, N,
     $                               N, KD, KD, -1, IMAT, NFAIL, NERRS,
     $                               NOUT )
*
                     RESULT( 7 ) = AB_DGET06( RCOND, RCONDC )
*
*                    Print the test ratio if it is .GE. THRESH.
*
                     IF( RESULT( 7 ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL AB_ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9997 )UPLO, N, KD, IMAT, 7,
     $                     RESULT( 7 )
                        NFAIL = NFAIL + 1
                     END IF
                     NRUN = NRUN + 1
   50             CONTINUE
   60          CONTINUE
   70       CONTINUE
   80    CONTINUE
   90 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' UPLO=''', A1, ''', N=', I5, ', KD=', I5, ', NB=', I4,
     $      ', type ', I2, ', test ', I2, ', ratio= ', G12.5 )
 9998 FORMAT( ' UPLO=''', A1, ''', N=', I5, ', KD=', I5, ', NRHS=', I3,
     $      ', type ', I2, ', test(', I2, ') = ', G12.5 )
 9997 FORMAT( ' UPLO=''', A1, ''', N=', I5, ', KD=', I5, ',', 10X,
     $      ' type ', I2, ', test(', I2, ') = ', G12.5 )
      RETURN
*
*     End of AB_DCHKPB
*
      END
