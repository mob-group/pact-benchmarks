*> \brief \b AB_DDRVPB
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DDRVPB( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX,
*                          A, AFAC, ASAV, B, BSAV, X, XACT, S, WORK,
*                          RWORK, IWORK, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NMAX, NN, NOUT, NRHS
*       DOUBLE PRECISION   THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            IWORK( * ), NVAL( * )
*       DOUBLE PRECISION   A( * ), AFAC( * ), ASAV( * ), B( * ),
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
*> AB_DDRVPB tests the driver routines AB_DPBSV and -SVX.
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
*> \param[out] ASAV
*> \verbatim
*>          ASAV is DOUBLE PRECISION array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] BSAV
*> \verbatim
*>          BSAV is DOUBLE PRECISION array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is DOUBLE PRECISION array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] XACT
*> \verbatim
*>          XACT is DOUBLE PRECISION array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is DOUBLE PRECISION array, dimension (NMAX)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension
*>                      (NMAX*max(3,NRHS))
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension (NMAX+2*NRHS)
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
      SUBROUTINE AB_DDRVPB( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX
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
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), AFAC( * ), ASAV( * ), B( * ),
     $                   BSAV( * ), RWORK( * ), S( * ), WORK( * ),
     $                   X( * ), XACT( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NTYPES, NTESTS
      PARAMETER          ( NTYPES = 8, NTESTS = 6 )
      INTEGER            NBW
      PARAMETER          ( NBW = 4 )
*     ..
*     .. Local Scalars ..
      LOGICAL            EQUIL, NOFACT, PREFAC, ZEROT
      CHARACTER          DIST, EQUED, FACT, PACKIT, TYPE, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, I1, I2, IEQUED, IFACT, IKD, IMAT, IN, INFO,
     $                   IOFF, IUPLO, IW, IZERO, K, K1, KD, KL, KOFF,
     $                   KU, LDA, LDAB, MODE, N, NB, NBMIN, NERRS,
     $                   NFACT, NFAIL, NIMAT, NKD, NRUN, NT
      DOUBLE PRECISION   AINVNM, AMAX, ANORM, CNDNUM, RCOND, RCONDC,
     $                   ROLDC, SCOND
*     ..
*     .. Local Arrays ..
      CHARACTER          EQUEDS( 2 ), FACTS( 3 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 ), KDVAL( NBW )
      DOUBLE PRECISION   RESULT( NTESTS )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      DOUBLE PRECISION   AB_DGET06, AB_DLANGE, AB_DLANSB
      EXTERNAL           AB_LSAME, AB_DGET06, AB_DLANGE, AB_DLANSB
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALADHD, AB_ALAERH, AB_ALASVM, AB_DCOPY, AB_D
     $ERRVX, AB_DGET04,
     $                   AB_DLACPY, AB_DLAQSB, AB_DLARHS, AB_DLASET, AB_
     $DLATB4, AB_DLATMS,
     $                   AB_DPBEQU, AB_DPBSV, AB_AB_DPBSVX, AB_DPBT01, A
     $B_DPBT02, AB_DPBT05,
     $                   AB_DPBTRF, AB_DPBTRS, AB_DSWAP, AB_XLAENV
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
      DATA               FACTS / 'F', 'N', 'E' /
      DATA               EQUEDS / 'N', 'Y' /
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
     $   CALL AB_DERRVX( PATH, NOUT )
      INFOT = 0
      KDVAL( 1 ) = 0
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
      DO 110 IN = 1, NN
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
         DO 100 IKD = 1, NKD
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
            DO 90 IUPLO = 1, 2
               KOFF = 1
               IF( IUPLO.EQ.1 ) THEN
                  UPLO = 'U'
                  PACKIT = 'Q'
                  KOFF = MAX( 1, KD+2-N )
               ELSE
                  UPLO = 'L'
                  PACKIT = 'B'
               END IF
*
               DO 80 IMAT = 1, NIMAT
*
*                 Do the tests only if DOTYPE( IMAT ) is true.
*
                  IF( .NOT.DOTYPE( IMAT ) )
     $               GO TO 80
*
*                 Skip types 2, 3, or 4 if the matrix size is too small.
*
                  ZEROT = IMAT.GE.2 .AND. IMAT.LE.4
                  IF( ZEROT .AND. N.LT.IMAT-1 )
     $               GO TO 80
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
     $                               N, -1, -1, -1, IMAT, NFAIL, NERRS,
     $                               NOUT )
                        GO TO 80
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
*                 Save a copy of the matrix A in ASAV.
*
                  CALL AB_DLACPY( 'Full', KD+1, N, A, LDAB, ASAV, LDAB )
*
                  DO 70 IEQUED = 1, 2
                     EQUED = EQUEDS( IEQUED )
                     IF( IEQUED.EQ.1 ) THEN
                        NFACT = 3
                     ELSE
                        NFACT = 1
                     END IF
*
                     DO 60 IFACT = 1, NFACT
                        FACT = FACTS( IFACT )
                        PREFAC = AB_LSAME( FACT, 'F' )
                        NOFACT = AB_LSAME( FACT, 'N' )
                        EQUIL = AB_LSAME( FACT, 'E' )
*
                        IF( ZEROT ) THEN
                           IF( PREFAC )
     $                        GO TO 60
                           RCONDC = ZERO
*
                        ELSE IF( .NOT.AB_LSAME( FACT, 'N' ) ) THEN
*
*                          Compute the condition number for comparison
*                          with the value returned by AB_AB_DPBSVX (FACT =
*                          'N' reuses the condition number from the
*                          previous iteration with FACT = 'F').
*
                           CALL AB_DLACPY( 'Full', KD+1, N, ASAV, LDAB,
     $                                  AFAC, LDAB )
                           IF( EQUIL .OR. IEQUED.GT.1 ) THEN
*
*                             Compute row and column scale factors to
*                             equilibrate the matrix A.
*
                              CALL AB_DPBEQU( UPLO, N, KD, AFAC, LDAB, S
     $,
     $                                     SCOND, AMAX, INFO )
                              IF( INFO.EQ.0 .AND. N.GT.0 ) THEN
                                 IF( IEQUED.GT.1 )
     $                              SCOND = ZERO
*
*                                Equilibrate the matrix.
*
                                 CALL AB_DLAQSB( UPLO, N, KD, AFAC, LDAB
     $,
     $                                        S, SCOND, AMAX, EQUED )
                              END IF
                           END IF
*
*                          Save the condition number of the
*                          non-equilibrated system for use in AB_DGET04.
*
                           IF( EQUIL )
     $                        ROLDC = RCONDC
*
*                          Compute the 1-norm of A.
*
                           ANORM = AB_DLANSB( '1', UPLO, N, KD, AFAC, LD
     $AB,
     $                             RWORK )
*
*                          Factor the matrix A.
*
                           CALL AB_DPBTRF( UPLO, N, KD, AFAC, LDAB, INFO
     $ )
*
*                          Form the inverse of A.
*
                           CALL AB_DLASET( 'Full', N, N, ZERO, ONE, A,
     $                                  LDA )
                           SRNAMT = 'AB_DPBTRS'
                           CALL AB_DPBTRS( UPLO, N, KD, N, AFAC, LDAB, A
     $,
     $                                  LDA, INFO )
*
*                          Compute the 1-norm condition number of A.
*
                           AINVNM = AB_DLANGE( '1', N, N, A, LDA, RWORK 
     $)
                           IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                              RCONDC = ONE
                           ELSE
                              RCONDC = ( ONE / ANORM ) / AINVNM
                           END IF
                        END IF
*
*                       Restore the matrix A.
*
                        CALL AB_DLACPY( 'Full', KD+1, N, ASAV, LDAB, A,
     $                               LDAB )
*
*                       Form an exact solution and set the right hand
*                       side.
*
                        SRNAMT = 'AB_DLARHS'
                        CALL AB_DLARHS( PATH, XTYPE, UPLO, ' ', N, N, KD
     $,
     $                               KD, NRHS, A, LDAB, XACT, LDA, B,
     $                               LDA, ISEED, INFO )
                        XTYPE = 'C'
                        CALL AB_DLACPY( 'Full', N, NRHS, B, LDA, BSAV,
     $                               LDA )
*
                        IF( NOFACT ) THEN
*
*                          --- Test AB_DPBSV  ---
*
*                          Compute the L*L' or U'*U factorization of the
*                          matrix and solve the system.
*
                           CALL AB_DLACPY( 'Full', KD+1, N, A, LDAB, AFA
     $C,
     $                                  LDAB )
                           CALL AB_DLACPY( 'Full', N, NRHS, B, LDA, X,
     $                                  LDA )
*
                           SRNAMT = 'AB_DPBSV '
                           CALL AB_DPBSV( UPLO, N, KD, NRHS, AFAC, LDAB,
     $ X,
     $                                 LDA, INFO )
*
*                          Check error code from AB_DPBSV .
*
                           IF( INFO.NE.IZERO ) THEN
                              CALL AB_ALAERH( PATH, 'AB_DPBSV ', INFO, I
     $ZERO,
     $                                     UPLO, N, N, KD, KD, NRHS,
     $                                     IMAT, NFAIL, NERRS, NOUT )
                              GO TO 40
                           ELSE IF( INFO.NE.0 ) THEN
                              GO TO 40
                           END IF
*
*                          Reconstruct matrix from factors and compute
*                          residual.
*
                           CALL AB_DPBT01( UPLO, N, KD, A, LDAB, AFAC,
     $                                  LDAB, RWORK, RESULT( 1 ) )
*
*                          Compute residual of the computed solution.
*
                           CALL AB_DLACPY( 'Full', N, NRHS, B, LDA, WORK
     $,
     $                                  LDA )
                           CALL AB_DPBT02( UPLO, N, KD, NRHS, A, LDAB, X
     $,
     $                                  LDA, WORK, LDA, RWORK,
     $                                  RESULT( 2 ) )
*
*                          Check solution from generated exact solution.
*
                           CALL AB_DGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  RCONDC, RESULT( 3 ) )
                           NT = 3
*
*                          Print information about the tests that did
*                          not pass the threshold.
*
                           DO 30 K = 1, NT
                              IF( RESULT( K ).GE.THRESH ) THEN
                                 IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                              CALL AB_ALADHD( NOUT, PATH )
                                 WRITE( NOUT, FMT = 9999 )'AB_DPBSV ',
     $                              UPLO, N, KD, IMAT, K, RESULT( K )
                                 NFAIL = NFAIL + 1
                              END IF
   30                      CONTINUE
                           NRUN = NRUN + NT
   40                      CONTINUE
                        END IF
*
*                       --- Test AB_AB_DPBSVX ---
*
                        IF( .NOT.PREFAC )
     $                     CALL AB_DLASET( 'Full', KD+1, N, ZERO, ZERO,
     $                                  AFAC, LDAB )
                        CALL AB_DLASET( 'Full', N, NRHS, ZERO, ZERO, X,
     $                               LDA )
                        IF( IEQUED.GT.1 .AND. N.GT.0 ) THEN
*
*                          Equilibrate the matrix if FACT='F' and
*                          EQUED='Y'
*
                           CALL AB_DLAQSB( UPLO, N, KD, A, LDAB, S, SCON
     $D,
     $                                  AMAX, EQUED )
                        END IF
*
*                       Solve the system and compute the condition
*                       number and error bounds using AB_AB_DPBSVX.
*
                        SRNAMT = 'AB_AB_DPBSVX'
                        CALL AB_AB_DPBSVX( FACT, UPLO, N, KD, NRHS, A, L
     $DAB,
     $                               AFAC, LDAB, EQUED, S, B, LDA, X,
     $                               LDA, RCOND, RWORK, RWORK( NRHS+1 ),
     $                               WORK, IWORK, INFO )
*
*                       Check the error code from AB_AB_DPBSVX.
*
                        IF( INFO.NE.IZERO ) THEN
                           CALL AB_ALAERH( PATH, 'AB_AB_DPBSVX', INFO, I
     $ZERO,
     $                                  FACT // UPLO, N, N, KD, KD,
     $                                  NRHS, IMAT, NFAIL, NERRS, NOUT )
                           GO TO 60
                        END IF
*
                        IF( INFO.EQ.0 ) THEN
                           IF( .NOT.PREFAC ) THEN
*
*                             Reconstruct matrix from factors and
*                             compute residual.
*
                              CALL AB_DPBT01( UPLO, N, KD, A, LDAB, AFAC
     $,
     $                                     LDAB, RWORK( 2*NRHS+1 ),
     $                                     RESULT( 1 ) )
                              K1 = 1
                           ELSE
                              K1 = 2
                           END IF
*
*                          Compute residual of the computed solution.
*
                           CALL AB_DLACPY( 'Full', N, NRHS, BSAV, LDA,
     $                                  WORK, LDA )
                           CALL AB_DPBT02( UPLO, N, KD, NRHS, ASAV, LDAB
     $,
     $                                  X, LDA, WORK, LDA,
     $                                  RWORK( 2*NRHS+1 ), RESULT( 2 ) )
*
*                          Check solution from generated exact solution.
*
                           IF( NOFACT .OR. ( PREFAC .AND. AB_LSAME( EQUE
     $D,
     $                         'N' ) ) ) THEN
                              CALL AB_DGET04( N, NRHS, X, LDA, XACT, LDA
     $,
     $                                     RCONDC, RESULT( 3 ) )
                           ELSE
                              CALL AB_DGET04( N, NRHS, X, LDA, XACT, LDA
     $,
     $                                     ROLDC, RESULT( 3 ) )
                           END IF
*
*                          Check the error bounds from iterative
*                          refinement.
*
                           CALL AB_DPBT05( UPLO, N, KD, NRHS, ASAV, LDAB
     $,
     $                                  B, LDA, X, LDA, XACT, LDA,
     $                                  RWORK, RWORK( NRHS+1 ),
     $                                  RESULT( 4 ) )
                        ELSE
                           K1 = 6
                        END IF
*
*                       Compare RCOND from AB_AB_DPBSVX with the computed
*                       value in RCONDC.
*
                        RESULT( 6 ) = AB_DGET06( RCOND, RCONDC )
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 50 K = K1, 6
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL AB_ALADHD( NOUT, PATH )
                              IF( PREFAC ) THEN
                                 WRITE( NOUT, FMT = 9997 )'AB_AB_DPBSVX'
     $,
     $                              FACT, UPLO, N, KD, EQUED, IMAT, K,
     $                              RESULT( K )
                              ELSE
                                 WRITE( NOUT, FMT = 9998 )'AB_AB_DPBSVX'
     $,
     $                              FACT, UPLO, N, KD, IMAT, K,
     $                              RESULT( K )
                              END IF
                              NFAIL = NFAIL + 1
                           END IF
   50                   CONTINUE
                        NRUN = NRUN + 7 - K1
   60                CONTINUE
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( 1X, A, ', UPLO=''', A1, ''', N =', I5, ', KD =', I5,
     $      ', type ', I1, ', test(', I1, ')=', G12.5 )
 9998 FORMAT( 1X, A, '( ''', A1, ''', ''', A1, ''', ', I5, ', ', I5,
     $      ', ... ), type ', I1, ', test(', I1, ')=', G12.5 )
 9997 FORMAT( 1X, A, '( ''', A1, ''', ''', A1, ''', ', I5, ', ', I5,
     $      ', ... ), EQUED=''', A1, ''', type ', I1, ', test(', I1,
     $      ')=', G12.5 )
      RETURN
*
*     End of AB_DDRVPB
*
      END
