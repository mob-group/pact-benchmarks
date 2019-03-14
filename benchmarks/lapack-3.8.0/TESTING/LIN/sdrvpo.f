*> \brief \b AB_SDRVPO
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SDRVPO( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX,
*                          A, AFAC, ASAV, B, BSAV, X, XACT, S, WORK,
*                          RWORK, IWORK, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NMAX, NN, NOUT, NRHS
*       REAL               THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            IWORK( * ), NVAL( * )
*       REAL               A( * ), AFAC( * ), ASAV( * ), B( * ),
*      $                   BSAV( * ), RWORK( * ), S( * ), WORK( * ),
*      $                   X( * ), XACT( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SDRVPO tests the driver routines AB_SPOSV and -SVX.
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
*> \param[out] ASAV
*> \verbatim
*>          ASAV is REAL array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is REAL array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] BSAV
*> \verbatim
*>          BSAV is REAL array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is REAL array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] XACT
*> \verbatim
*>          XACT is REAL array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is REAL array, dimension (NMAX)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is REAL array, dimension
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
      SUBROUTINE AB_SDRVPO( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX
     $,
     $                   A, AFAC, ASAV, B, BSAV, X, XACT, S, WORK,
     $                   RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NVAL( * )
      REAL               A( * ), AFAC( * ), ASAV( * ), B( * ),
     $                   BSAV( * ), RWORK( * ), S( * ), WORK( * ),
     $                   X( * ), XACT( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 9 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            EQUIL, NOFACT, PREFAC, ZEROT
      CHARACTER          DIST, EQUED, FACT, TYPE, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, IEQUED, IFACT, IMAT, IN, INFO, IOFF, IUPLO,
     $                   IZERO, K, K1, KL, KU, LDA, MODE, N, NB, NBMIN,
     $                   NERRS, NFACT, NFAIL, NIMAT, NRUN, NT
      REAL               AINVNM, AMAX, ANORM, CNDNUM, RCOND, RCONDC,
     $                   ROLDC, SCOND
*     ..
*     .. Local Arrays ..
      CHARACTER          EQUEDS( 2 ), FACTS( 3 ), UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      REAL               AB_SGET06, AB_SLANSY
      EXTERNAL           AB_LSAME, AB_SGET06, AB_SLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALADHD, AB_ALAERH, AB_ALASVM, AB_SERRVX, AB_
     $SGET04, AB_SLACPY,
     $                   AB_SLAQSY, AB_SLARHS, AB_SLASET, AB_SLATB4, AB_
     $SLATMS, AB_SPOEQU,
     $                   AB_SPOSV, AB_AB_SPOSVX, AB_SPOT01, AB_SPOT02, A
     $B_SPOT05, AB_SPOTRF,
     $                   AB_SPOTRI, AB_XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
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
      DATA               UPLOS / 'U', 'L' /
      DATA               FACTS / 'F', 'N', 'E' /
      DATA               EQUEDS / 'N', 'Y' /
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
     $   CALL AB_SERRVX( PATH, NOUT )
      INFOT = 0
*
*     Set the block size and minimum block size for testing.
*
      NB = 1
      NBMIN = 2
      CALL AB_XLAENV( 1, NB )
      CALL AB_XLAENV( 2, NBMIN )
*
*     Do for each value of N in NVAL
*
      DO 130 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         DO 120 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 120
*
*           Skip types 3, 4, or 5 if the matrix size is too small.
*
            ZEROT = IMAT.GE.3 .AND. IMAT.LE.5
            IF( ZEROT .AND. N.LT.IMAT-2 )
     $         GO TO 120
*
*           Do first for UPLO = 'U', then for UPLO = 'L'
*
            DO 110 IUPLO = 1, 2
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
                  GO TO 110
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
*              Save a copy of the matrix A in ASAV.
*
               CALL AB_SLACPY( UPLO, N, N, A, LDA, ASAV, LDA )
*
               DO 100 IEQUED = 1, 2
                  EQUED = EQUEDS( IEQUED )
                  IF( IEQUED.EQ.1 ) THEN
                     NFACT = 3
                  ELSE
                     NFACT = 1
                  END IF
*
                  DO 90 IFACT = 1, NFACT
                     FACT = FACTS( IFACT )
                     PREFAC = AB_LSAME( FACT, 'F' )
                     NOFACT = AB_LSAME( FACT, 'N' )
                     EQUIL = AB_LSAME( FACT, 'E' )
*
                     IF( ZEROT ) THEN
                        IF( PREFAC )
     $                     GO TO 90
                        RCONDC = ZERO
*
                     ELSE IF( .NOT.AB_LSAME( FACT, 'N' ) ) THEN
*
*                       Compute the condition number for comparison with
*                       the value returned by AB_AB_SPOSVX (FACT = 'N' reuses
*                       the condition number from the previous iteration
*                       with FACT = 'F').
*
                        CALL AB_SLACPY( UPLO, N, N, ASAV, LDA, AFAC, LDA
     $ )
                        IF( EQUIL .OR. IEQUED.GT.1 ) THEN
*
*                          Compute row and column scale factors to
*                          equilibrate the matrix A.
*
                           CALL AB_SPOEQU( N, AFAC, LDA, S, SCOND, AMAX,
     $                                  INFO )
                           IF( INFO.EQ.0 .AND. N.GT.0 ) THEN
                              IF( IEQUED.GT.1 )
     $                           SCOND = ZERO
*
*                             Equilibrate the matrix.
*
                              CALL AB_SLAQSY( UPLO, N, AFAC, LDA, S, SCO
     $ND,
     $                                     AMAX, EQUED )
                           END IF
                        END IF
*
*                       Save the condition number of the
*                       non-equilibrated system for use in AB_SGET04.
*
                        IF( EQUIL )
     $                     ROLDC = RCONDC
*
*                       Compute the 1-norm of A.
*
                        ANORM = AB_SLANSY( '1', UPLO, N, AFAC, LDA, RWOR
     $K )
*
*                       Factor the matrix A.
*
                        CALL AB_SPOTRF( UPLO, N, AFAC, LDA, INFO )
*
*                       Form the inverse of A.
*
                        CALL AB_SLACPY( UPLO, N, N, AFAC, LDA, A, LDA )
                        CALL AB_SPOTRI( UPLO, N, A, LDA, INFO )
*
*                       Compute the 1-norm condition number of A.
*
                        AINVNM = AB_SLANSY( '1', UPLO, N, A, LDA, RWORK 
     $)
                        IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                           RCONDC = ONE
                        ELSE
                           RCONDC = ( ONE / ANORM ) / AINVNM
                        END IF
                     END IF
*
*                    Restore the matrix A.
*
                     CALL AB_SLACPY( UPLO, N, N, ASAV, LDA, A, LDA )
*
*                    Form an exact solution and set the right hand side.
*
                     SRNAMT = 'AB_SLARHS'
                     CALL AB_SLARHS( PATH, XTYPE, UPLO, ' ', N, N, KL, K
     $U,
     $                            NRHS, A, LDA, XACT, LDA, B, LDA,
     $                            ISEED, INFO )
                     XTYPE = 'C'
                     CALL AB_SLACPY( 'Full', N, NRHS, B, LDA, BSAV, LDA 
     $)
*
                     IF( NOFACT ) THEN
*
*                       --- Test AB_SPOSV  ---
*
*                       Compute the L*L' or U'*U factorization of the
*                       matrix and solve the system.
*
                        CALL AB_SLACPY( UPLO, N, N, A, LDA, AFAC, LDA )
                        CALL AB_SLACPY( 'Full', N, NRHS, B, LDA, X, LDA 
     $)
*
                        SRNAMT = 'AB_SPOSV '
                        CALL AB_SPOSV( UPLO, N, NRHS, AFAC, LDA, X, LDA,
     $                              INFO )
*
*                       Check error code from AB_SPOSV .
*
                        IF( INFO.NE.IZERO ) THEN
                           CALL AB_ALAERH( PATH, 'AB_SPOSV ', INFO, IZER
     $O,
     $                                  UPLO, N, N, -1, -1, NRHS, IMAT,
     $                                  NFAIL, NERRS, NOUT )
                           GO TO 70
                        ELSE IF( INFO.NE.0 ) THEN
                           GO TO 70
                        END IF
*
*                       Reconstruct matrix from factors and compute
*                       residual.
*
                        CALL AB_SPOT01( UPLO, N, A, LDA, AFAC, LDA, RWOR
     $K,
     $                               RESULT( 1 ) )
*
*                       Compute residual of the computed solution.
*
                        CALL AB_SLACPY( 'Full', N, NRHS, B, LDA, WORK,
     $                               LDA )
                        CALL AB_SPOT02( UPLO, N, NRHS, A, LDA, X, LDA,
     $                               WORK, LDA, RWORK, RESULT( 2 ) )
*
*                       Check solution from generated exact solution.
*
                        CALL AB_SGET04( N, NRHS, X, LDA, XACT, LDA, RCON
     $DC,
     $                               RESULT( 3 ) )
                        NT = 3
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 60 K = 1, NT
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL AB_ALADHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9999 )'AB_SPOSV ', UPLO
     $,
     $                           N, IMAT, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   60                   CONTINUE
                        NRUN = NRUN + NT
   70                   CONTINUE
                     END IF
*
*                    --- Test AB_AB_SPOSVX ---
*
                     IF( .NOT.PREFAC )
     $                  CALL AB_SLASET( UPLO, N, N, ZERO, ZERO, AFAC, LD
     $A )
                     CALL AB_SLASET( 'Full', N, NRHS, ZERO, ZERO, X, LDA
     $ )
                     IF( IEQUED.GT.1 .AND. N.GT.0 ) THEN
*
*                       Equilibrate the matrix if FACT='F' and
*                       EQUED='Y'.
*
                        CALL AB_SLAQSY( UPLO, N, A, LDA, S, SCOND, AMAX,
     $                               EQUED )
                     END IF
*
*                    Solve the system and compute the condition number
*                    and error bounds using AB_AB_SPOSVX.
*
                     SRNAMT = 'AB_AB_SPOSVX'
                     CALL AB_AB_SPOSVX( FACT, UPLO, N, NRHS, A, LDA, AFA
     $C,
     $                            LDA, EQUED, S, B, LDA, X, LDA, RCOND,
     $                            RWORK, RWORK( NRHS+1 ), WORK, IWORK,
     $                            INFO )
*
*                    Check the error code from AB_AB_SPOSVX.
*
                     IF( INFO.NE.IZERO ) THEN
                        CALL AB_ALAERH( PATH, 'AB_AB_SPOSVX', INFO, IZER
     $O,
     $                               FACT // UPLO, N, N, -1, -1, NRHS,
     $                               IMAT, NFAIL, NERRS, NOUT )
                        GO TO 90
                     END IF
*
                     IF( INFO.EQ.0 ) THEN
                        IF( .NOT.PREFAC ) THEN
*
*                          Reconstruct matrix from factors and compute
*                          residual.
*
                           CALL AB_SPOT01( UPLO, N, A, LDA, AFAC, LDA,
     $                                  RWORK( 2*NRHS+1 ), RESULT( 1 ) )
                           K1 = 1
                        ELSE
                           K1 = 2
                        END IF
*
*                       Compute residual of the computed solution.
*
                        CALL AB_SLACPY( 'Full', N, NRHS, BSAV, LDA, WORK
     $,
     $                               LDA )
                        CALL AB_SPOT02( UPLO, N, NRHS, ASAV, LDA, X, LDA
     $,
     $                               WORK, LDA, RWORK( 2*NRHS+1 ),
     $                               RESULT( 2 ) )
*
*                       Check solution from generated exact solution.
*
                        IF( NOFACT .OR. ( PREFAC .AND. AB_LSAME( EQUED,
     $                      'N' ) ) ) THEN
                           CALL AB_SGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  RCONDC, RESULT( 3 ) )
                        ELSE
                           CALL AB_SGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  ROLDC, RESULT( 3 ) )
                        END IF
*
*                       Check the error bounds from iterative
*                       refinement.
*
                        CALL AB_SPOT05( UPLO, N, NRHS, ASAV, LDA, B, LDA
     $,
     $                               X, LDA, XACT, LDA, RWORK,
     $                               RWORK( NRHS+1 ), RESULT( 4 ) )
                     ELSE
                        K1 = 6
                     END IF
*
*                    Compare RCOND from AB_AB_SPOSVX with the computed value
*                    in RCONDC.
*
                     RESULT( 6 ) = AB_SGET06( RCOND, RCONDC )
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     DO 80 K = K1, 6
                        IF( RESULT( K ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           IF( PREFAC ) THEN
                              WRITE( NOUT, FMT = 9997 )'AB_AB_SPOSVX', F
     $ACT,
     $                           UPLO, N, EQUED, IMAT, K, RESULT( K )
                           ELSE
                              WRITE( NOUT, FMT = 9998 )'AB_AB_SPOSVX', F
     $ACT,
     $                           UPLO, N, IMAT, K, RESULT( K )
                           END IF
                           NFAIL = NFAIL + 1
                        END IF
   80                CONTINUE
                     NRUN = NRUN + 7 - K1
   90             CONTINUE
  100          CONTINUE
  110       CONTINUE
  120    CONTINUE
  130 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( 1X, A, ', UPLO=''', A1, ''', N =', I5, ', type ', I1,
     $      ', test(', I1, ')=', G12.5 )
 9998 FORMAT( 1X, A, ', FACT=''', A1, ''', UPLO=''', A1, ''', N=', I5,
     $      ', type ', I1, ', test(', I1, ')=', G12.5 )
 9997 FORMAT( 1X, A, ', FACT=''', A1, ''', UPLO=''', A1, ''', N=', I5,
     $      ', EQUED=''', A1, ''', type ', I1, ', test(', I1, ') =',
     $      G12.5 )
      RETURN
*
*     End of AB_SDRVPO
*
      END
