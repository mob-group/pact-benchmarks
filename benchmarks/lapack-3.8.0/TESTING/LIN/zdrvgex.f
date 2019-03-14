*> \brief \b AB_ZDRVGEX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZDRVGE( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX,
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
*       DOUBLE PRECISION   RWORK( * ), S( * )
*       COMPLEX*16         A( * ), AFAC( * ), ASAV( * ), B( * ),
*      $                   BSAV( * ), WORK( * ), X( * ), XACT( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZDRVGE tests the driver routines AB_ZGESV, -SVX, and -SVXX.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_ZDRVGE.f defines this subroutine.
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
*>          The values of the matrix column dimension N.
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
*>          A is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AFAC
*> \verbatim
*>          AFAC is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] ASAV
*> \verbatim
*>          ASAV is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is COMPLEX*16 array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] BSAV
*> \verbatim
*>          BSAV is COMPLEX*16 array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] X
*> \verbatim
*>          X is COMPLEX*16 array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] XACT
*> \verbatim
*>          XACT is COMPLEX*16 array, dimension (NMAX*NRHS)
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is DOUBLE PRECISION array, dimension (2*NMAX)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension
*>                      (NMAX*max(3,NRHS))
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension (2*NRHS+NMAX)
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
*> \date April 2012
*
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE AB_ZDRVGE( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX
     $,
     $                   A, AFAC, ASAV, B, BSAV, X, XACT, S, WORK,
     $                   RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     April 2012
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NOUT, NRHS
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NVAL( * )
      DOUBLE PRECISION   RWORK( * ), S( * )
      COMPLEX*16         A( * ), AFAC( * ), ASAV( * ), B( * ),
     $                   BSAV( * ), WORK( * ), X( * ), XACT( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 11 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 7 )
      INTEGER            NTRAN
      PARAMETER          ( NTRAN = 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            EQUIL, NOFACT, PREFAC, TRFCON, ZEROT
      CHARACTER          DIST, EQUED, FACT, TRANS, TYPE, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, IEQUED, IFACT, IMAT, IN, INFO, IOFF, ITRAN,
     $                   IZERO, K, K1, KL, KU, LDA, LWORK, MODE, N, NB,
     $                   NBMIN, NERRS, NFACT, NFAIL, NIMAT, NRUN, NT,
     $                   N_ERR_BNDS
      DOUBLE PRECISION   AINVNM, AMAX, ANORM, ANORMI, ANORMO, CNDNUM,
     $                   COLCND, RCOND, RCONDC, RCONDI, RCONDO, ROLDC,
     $                   ROLDI, ROLDO, ROWCND, RPVGRW, RPVGRW_SVXX
*     ..
*     .. Local Arrays ..
      CHARACTER          EQUEDS( 4 ), FACTS( 3 ), TRANSS( NTRAN )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      DOUBLE PRECISION   RDUM( 1 ), RESULT( NTESTS ), BERR( NRHS ),
     $                   ERRBNDS_N( NRHS, 3 ), ERRBNDS_C( NRHS, 3 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      DOUBLE PRECISION   AB_DGET06, AB_DLAMCH, AB_ZLANGE, AB_ZLANTR, AB_
     $ZLA_GERPVGRW
      EXTERNAL           AB_LSAME, AB_DGET06, AB_DLAMCH, AB_ZLANGE, AB_Z
     $LANTR,
     $                   AB_ZLA_GERPVGRW
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALADHD, AB_ALAERH, AB_ALASVM, AB_XLAENV, AB_
     $ZERRVX, AB_ZGEEQU,
     $                   AB_ZGESV, AB_AB_ZGESVX, AB_ZGET01, AB_ZGET02, A
     $B_ZGET04, AB_ZGET07,
     $                   AB_ZGETRF, AB_ZGETRI, AB_ZLACPY, AB_ZLAQGE, AB_
     $ZLARHS, AB_ZLASET,
     $                   AB_ZLATB4, AB_ZLATMS, AB_AB_AB_ZGESVXX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DCMPLX, MAX, DBLE, DIMAG
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
      DATA               TRANSS / 'N', 'T', 'C' /
      DATA               FACTS / 'F', 'N', 'E' /
      DATA               EQUEDS / 'N', 'R', 'C', 'B' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Zomplex precision'
      PATH( 2: 3 ) = 'GE'
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
     $   CALL AB_ZERRVX( PATH, NOUT )
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
      DO 90 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         DO 80 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 80
*
*           Skip types 5, 6, or 7 if the matrix size is too small.
*
            ZEROT = IMAT.GE.5 .AND. IMAT.LE.7
            IF( ZEROT .AND. N.LT.IMAT-4 )
     $         GO TO 80
*
*           Set up parameters with AB_ZLATB4 and generate a test matrix
*           with AB_ZLATMS.
*
            CALL AB_ZLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                   CNDNUM, DIST )
            RCONDC = ONE / CNDNUM
*
            SRNAMT = 'AB_ZLATMS'
            CALL AB_ZLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE, CNDNUM
     $,
     $                   ANORM, KL, KU, 'No packing', A, LDA, WORK,
     $                   INFO )
*
*           Check error code from AB_ZLATMS.
*
            IF( INFO.NE.0 ) THEN
               CALL AB_ALAERH( PATH, 'AB_ZLATMS', INFO, 0, ' ', N, N, -1
     $, -1,
     $                      -1, IMAT, NFAIL, NERRS, NOUT )
               GO TO 80
            END IF
*
*           For types 5-7, zero one or more columns of the matrix to
*           test that INFO is returned correctly.
*
            IF( ZEROT ) THEN
               IF( IMAT.EQ.5 ) THEN
                  IZERO = 1
               ELSE IF( IMAT.EQ.6 ) THEN
                  IZERO = N
               ELSE
                  IZERO = N / 2 + 1
               END IF
               IOFF = ( IZERO-1 )*LDA
               IF( IMAT.LT.7 ) THEN
                  DO 20 I = 1, N
                     A( IOFF+I ) = ZERO
   20             CONTINUE
               ELSE
                  CALL AB_ZLASET( 'Full', N, N-IZERO+1, DCMPLX( ZERO ),
     $                         DCMPLX( ZERO ), A( IOFF+1 ), LDA )
               END IF
            ELSE
               IZERO = 0
            END IF
*
*           Save a copy of the matrix A in ASAV.
*
            CALL AB_ZLACPY( 'Full', N, N, A, LDA, ASAV, LDA )
*
            DO 70 IEQUED = 1, 4
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
     $                  GO TO 60
                     RCONDO = ZERO
                     RCONDI = ZERO
*
                  ELSE IF( .NOT.NOFACT ) THEN
*
*                    Compute the condition number for comparison with
*                    the value returned by AB_AB_ZGESVX (FACT = 'N' reuses
*                    the condition number from the previous iteration
*                    with FACT = 'F').
*
                     CALL AB_ZLACPY( 'Full', N, N, ASAV, LDA, AFAC, LDA 
     $)
                     IF( EQUIL .OR. IEQUED.GT.1 ) THEN
*
*                       Compute row and column scale factors to
*                       equilibrate the matrix A.
*
                        CALL AB_ZGEEQU( N, N, AFAC, LDA, S, S( N+1 ),
     $                               ROWCND, COLCND, AMAX, INFO )
                        IF( INFO.EQ.0 .AND. N.GT.0 ) THEN
                           IF( AB_LSAME( EQUED, 'R' ) ) THEN
                              ROWCND = ZERO
                              COLCND = ONE
                           ELSE IF( AB_LSAME( EQUED, 'C' ) ) THEN
                              ROWCND = ONE
                              COLCND = ZERO
                           ELSE IF( AB_LSAME( EQUED, 'B' ) ) THEN
                              ROWCND = ZERO
                              COLCND = ZERO
                           END IF
*
*                          Equilibrate the matrix.
*
                           CALL AB_ZLAQGE( N, N, AFAC, LDA, S, S( N+1 ),
     $                                  ROWCND, COLCND, AMAX, EQUED )
                        END IF
                     END IF
*
*                    Save the condition number of the non-equilibrated
*                    system for use in AB_ZGET04.
*
                     IF( EQUIL ) THEN
                        ROLDO = RCONDO
                        ROLDI = RCONDI
                     END IF
*
*                    Compute the 1-norm and infinity-norm of A.
*
                     ANORMO = AB_ZLANGE( '1', N, N, AFAC, LDA, RWORK )
                     ANORMI = AB_ZLANGE( 'I', N, N, AFAC, LDA, RWORK )
*
*                    Factor the matrix A.
*
                     CALL AB_ZGETRF( N, N, AFAC, LDA, IWORK, INFO )
*
*                    Form the inverse of A.
*
                     CALL AB_ZLACPY( 'Full', N, N, AFAC, LDA, A, LDA )
                     LWORK = NMAX*MAX( 3, NRHS )
                     CALL AB_ZGETRI( N, A, LDA, IWORK, WORK, LWORK, INFO
     $ )
*
*                    Compute the 1-norm condition number of A.
*
                     AINVNM = AB_ZLANGE( '1', N, N, A, LDA, RWORK )
                     IF( ANORMO.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                        RCONDO = ONE
                     ELSE
                        RCONDO = ( ONE / ANORMO ) / AINVNM
                     END IF
*
*                    Compute the infinity-norm condition number of A.
*
                     AINVNM = AB_ZLANGE( 'I', N, N, A, LDA, RWORK )
                     IF( ANORMI.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                        RCONDI = ONE
                     ELSE
                        RCONDI = ( ONE / ANORMI ) / AINVNM
                     END IF
                  END IF
*
                  DO 50 ITRAN = 1, NTRAN
*
*                    Do for each value of TRANS.
*
                     TRANS = TRANSS( ITRAN )
                     IF( ITRAN.EQ.1 ) THEN
                        RCONDC = RCONDO
                     ELSE
                        RCONDC = RCONDI
                     END IF
*
*                    Restore the matrix A.
*
                     CALL AB_ZLACPY( 'Full', N, N, ASAV, LDA, A, LDA )
*
*                    Form an exact solution and set the right hand side.
*
                     SRNAMT = 'AB_ZLARHS'
                     CALL AB_ZLARHS( PATH, XTYPE, 'Full', TRANS, N, N, K
     $L,
     $                            KU, NRHS, A, LDA, XACT, LDA, B, LDA,
     $                            ISEED, INFO )
                     XTYPE = 'C'
                     CALL AB_ZLACPY( 'Full', N, NRHS, B, LDA, BSAV, LDA 
     $)
*
                     IF( NOFACT .AND. ITRAN.EQ.1 ) THEN
*
*                       --- Test AB_ZGESV  ---
*
*                       Compute the LU factorization of the matrix and
*                       solve the system.
*
                        CALL AB_ZLACPY( 'Full', N, N, A, LDA, AFAC, LDA 
     $)
                        CALL AB_ZLACPY( 'Full', N, NRHS, B, LDA, X, LDA 
     $)
*
                        SRNAMT = 'AB_ZGESV '
                        CALL AB_ZGESV( N, NRHS, AFAC, LDA, IWORK, X, LDA
     $,
     $                              INFO )
*
*                       Check error code from AB_ZGESV .
*
                        IF( INFO.NE.IZERO )
     $                     CALL AB_ALAERH( PATH, 'AB_ZGESV ', INFO, IZER
     $O,
     $                                  ' ', N, N, -1, -1, NRHS, IMAT,
     $                                  NFAIL, NERRS, NOUT )
*
*                       Reconstruct matrix from factors and compute
*                       residual.
*
                        CALL AB_ZGET01( N, N, A, LDA, AFAC, LDA, IWORK,
     $                               RWORK, RESULT( 1 ) )
                        NT = 1
                        IF( IZERO.EQ.0 ) THEN
*
*                          Compute residual of the computed solution.
*
                           CALL AB_ZLACPY( 'Full', N, NRHS, B, LDA, WORK
     $,
     $                                  LDA )
                           CALL AB_ZGET02( 'No transpose', N, N, NRHS, A
     $,
     $                                  LDA, X, LDA, WORK, LDA, RWORK,
     $                                  RESULT( 2 ) )
*
*                          Check solution from generated exact solution.
*
                           CALL AB_ZGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  RCONDC, RESULT( 3 ) )
                           NT = 3
                        END IF
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 30 K = 1, NT
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL AB_ALADHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9999 )'AB_ZGESV ', N,
     $                           IMAT, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   30                   CONTINUE
                        NRUN = NRUN + NT
                     END IF
*
*                    --- Test AB_AB_ZGESVX ---
*
                     IF( .NOT.PREFAC )
     $                  CALL AB_ZLASET( 'Full', N, N, DCMPLX( ZERO ),
     $                               DCMPLX( ZERO ), AFAC, LDA )
                     CALL AB_ZLASET( 'Full', N, NRHS, DCMPLX( ZERO ),
     $                            DCMPLX( ZERO ), X, LDA )
                     IF( IEQUED.GT.1 .AND. N.GT.0 ) THEN
*
*                       Equilibrate the matrix if FACT = 'F' and
*                       EQUED = 'R', 'C', or 'B'.
*
                        CALL AB_ZLAQGE( N, N, A, LDA, S, S( N+1 ), ROWCN
     $D,
     $                               COLCND, AMAX, EQUED )
                     END IF
*
*                    Solve the system and compute the condition number
*                    and error bounds using AB_AB_ZGESVX.
*
                     SRNAMT = 'AB_AB_ZGESVX'
                     CALL AB_AB_ZGESVX( FACT, TRANS, N, NRHS, A, LDA, AF
     $AC,
     $                            LDA, IWORK, EQUED, S, S( N+1 ), B,
     $                            LDA, X, LDA, RCOND, RWORK,
     $                            RWORK( NRHS+1 ), WORK,
     $                            RWORK( 2*NRHS+1 ), INFO )
*
*                    Check the error code from AB_AB_ZGESVX.
*
                     IF( INFO.NE.IZERO )
     $                  CALL AB_ALAERH( PATH, 'AB_AB_ZGESVX', INFO, IZER
     $O,
     $                               FACT // TRANS, N, N, -1, -1, NRHS,
     $                               IMAT, NFAIL, NERRS, NOUT )
*
*                    Compare RWORK(2*NRHS+1) from AB_AB_ZGESVX with the
*                    computed reciprocal pivot growth factor RPVGRW
*
                     IF( INFO.NE.0 ) THEN
                        RPVGRW = AB_ZLANTR( 'M', 'U', 'N', INFO, INFO,
     $                           AFAC, LDA, RDUM )
                        IF( RPVGRW.EQ.ZERO ) THEN
                           RPVGRW = ONE
                        ELSE
                           RPVGRW = AB_ZLANGE( 'M', N, INFO, A, LDA,
     $                              RDUM ) / RPVGRW
                        END IF
                     ELSE
                        RPVGRW = AB_ZLANTR( 'M', 'U', 'N', N, N, AFAC, L
     $DA,
     $                           RDUM )
                        IF( RPVGRW.EQ.ZERO ) THEN
                           RPVGRW = ONE
                        ELSE
                           RPVGRW = AB_ZLANGE( 'M', N, N, A, LDA, RDUM )
     $ /
     $                              RPVGRW
                        END IF
                     END IF
                     RESULT( 7 ) = ABS( RPVGRW-RWORK( 2*NRHS+1 ) ) /
     $                             MAX( RWORK( 2*NRHS+1 ), RPVGRW ) /
     $                             AB_DLAMCH( 'E' )
*
                     IF( .NOT.PREFAC ) THEN
*
*                       Reconstruct matrix from factors and compute
*                       residual.
*
                        CALL AB_ZGET01( N, N, A, LDA, AFAC, LDA, IWORK,
     $                               RWORK( 2*NRHS+1 ), RESULT( 1 ) )
                        K1 = 1
                     ELSE
                        K1 = 2
                     END IF
*
                     IF( INFO.EQ.0 ) THEN
                        TRFCON = .FALSE.
*
*                       Compute residual of the computed solution.
*
                        CALL AB_ZLACPY( 'Full', N, NRHS, BSAV, LDA, WORK
     $,
     $                               LDA )
                        CALL AB_ZGET02( TRANS, N, N, NRHS, ASAV, LDA, X,
     $                               LDA, WORK, LDA, RWORK( 2*NRHS+1 ),
     $                               RESULT( 2 ) )
*
*                       Check solution from generated exact solution.
*
                        IF( NOFACT .OR. ( PREFAC .AND. AB_LSAME( EQUED,
     $                      'N' ) ) ) THEN
                           CALL AB_ZGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  RCONDC, RESULT( 3 ) )
                        ELSE
                           IF( ITRAN.EQ.1 ) THEN
                              ROLDC = ROLDO
                           ELSE
                              ROLDC = ROLDI
                           END IF
                           CALL AB_ZGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  ROLDC, RESULT( 3 ) )
                        END IF
*
*                       Check the error bounds from iterative
*                       refinement.
*
                        CALL AB_ZGET07( TRANS, N, NRHS, ASAV, LDA, B, LD
     $A,
     $                               X, LDA, XACT, LDA, RWORK, .TRUE.,
     $                               RWORK( NRHS+1 ), RESULT( 4 ) )
                     ELSE
                        TRFCON = .TRUE.
                     END IF
*
*                    Compare RCOND from AB_AB_ZGESVX with the computed value
*                    in RCONDC.
*
                     RESULT( 6 ) = AB_DGET06( RCOND, RCONDC )
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     IF( .NOT.TRFCON ) THEN
                        DO 40 K = K1, NTESTS
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL AB_ALADHD( NOUT, PATH )
                              IF( PREFAC ) THEN
                                 WRITE( NOUT, FMT = 9997 )'AB_AB_ZGESVX'
     $,
     $                              FACT, TRANS, N, EQUED, IMAT, K,
     $                              RESULT( K )
                              ELSE
                                 WRITE( NOUT, FMT = 9998 )'AB_AB_ZGESVX'
     $,
     $                              FACT, TRANS, N, IMAT, K, RESULT( K )
                              END IF
                              NFAIL = NFAIL + 1
                           END IF
   40                   CONTINUE
                        NRUN = NRUN + 7 - K1
                     ELSE
                        IF( RESULT( 1 ).GE.THRESH .AND. .NOT.PREFAC )
     $                       THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           IF( PREFAC ) THEN
                              WRITE( NOUT, FMT = 9997 )'AB_AB_ZGESVX', F
     $ACT,
     $                           TRANS, N, EQUED, IMAT, 1, RESULT( 1 )
                           ELSE
                              WRITE( NOUT, FMT = 9998 )'AB_AB_ZGESVX', F
     $ACT,
     $                           TRANS, N, IMAT, 1, RESULT( 1 )
                           END IF
                           NFAIL = NFAIL + 1
                           NRUN = NRUN + 1
                        END IF
                        IF( RESULT( 6 ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           IF( PREFAC ) THEN
                              WRITE( NOUT, FMT = 9997 )'AB_AB_ZGESVX', F
     $ACT,
     $                           TRANS, N, EQUED, IMAT, 6, RESULT( 6 )
                           ELSE
                              WRITE( NOUT, FMT = 9998 )'AB_AB_ZGESVX', F
     $ACT,
     $                           TRANS, N, IMAT, 6, RESULT( 6 )
                           END IF
                           NFAIL = NFAIL + 1
                           NRUN = NRUN + 1
                        END IF
                        IF( RESULT( 7 ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           IF( PREFAC ) THEN
                              WRITE( NOUT, FMT = 9997 )'AB_AB_ZGESVX', F
     $ACT,
     $                           TRANS, N, EQUED, IMAT, 7, RESULT( 7 )
                           ELSE
                              WRITE( NOUT, FMT = 9998 )'AB_AB_ZGESVX', F
     $ACT,
     $                           TRANS, N, IMAT, 7, RESULT( 7 )
                           END IF
                           NFAIL = NFAIL + 1
                           NRUN = NRUN + 1
                        END IF
*
                     END IF
*
*                    --- Test AB_AB_AB_ZGESVXX ---
*
*                    Restore the matrices A and B.
*

                     CALL AB_ZLACPY( 'Full', N, N, ASAV, LDA, A, LDA )
                     CALL AB_ZLACPY( 'Full', N, NRHS, BSAV, LDA, B, LDA 
     $)

                     IF( .NOT.PREFAC )
     $                  CALL AB_ZLASET( 'Full', N, N, ZERO, ZERO, AFAC,
     $                               LDA )
                     CALL AB_ZLASET( 'Full', N, NRHS, ZERO, ZERO, X, LDA
     $ )
                     IF( IEQUED.GT.1 .AND. N.GT.0 ) THEN
*
*                       Equilibrate the matrix if FACT = 'F' and
*                       EQUED = 'R', 'C', or 'B'.
*
                        CALL AB_ZLAQGE( N, N, A, LDA, S, S( N+1 ), ROWCN
     $D,
     $                               COLCND, AMAX, EQUED )
                     END IF
*
*                    Solve the system and compute the condition number
*                    and error bounds using AB_AB_AB_ZGESVXX.
*
                     SRNAMT = 'AB_AB_AB_ZGESVXX'
                     N_ERR_BNDS = 3
                     CALL AB_AB_AB_ZGESVXX( FACT, TRANS, N, NRHS, A, LDA
     $, AFAC,
     $                    LDA, IWORK, EQUED, S, S( N+1 ), B, LDA, X,
     $                    LDA, RCOND, RPVGRW_SVXX, BERR, N_ERR_BNDS,
     $                    ERRBNDS_N, ERRBNDS_C, 0, ZERO, WORK,
     $                    RWORK, INFO )
*
*                    Check the error code from AB_AB_AB_ZGESVXX.
*
                     IF( INFO.EQ.N+1 ) GOTO 50
                     IF( INFO.NE.IZERO ) THEN
                        CALL AB_ALAERH( PATH, 'AB_AB_AB_ZGESVXX', INFO, 
     $IZERO,
     $                               FACT // TRANS, N, N, -1, -1, NRHS,
     $                               IMAT, NFAIL, NERRS, NOUT )
                        GOTO 50
                     END IF
*
*                    Compare rpvgrw_svxx from AB_AB_AB_ZGESVXX with the computed
*                    reciprocal pivot growth factor RPVGRW
*

                     IF ( INFO .GT. 0 .AND. INFO .LT. N+1 ) THEN
                        RPVGRW = AB_ZLA_GERPVGRW
     $                               (N, INFO, A, LDA, AFAC, LDA)
                     ELSE
                        RPVGRW = AB_ZLA_GERPVGRW
     $                               (N, N, A, LDA, AFAC, LDA)
                     ENDIF

                     RESULT( 7 ) = ABS( RPVGRW-rpvgrw_svxx ) /
     $                             MAX( rpvgrw_svxx, RPVGRW ) /
     $                             AB_DLAMCH( 'E' )
*
                     IF( .NOT.PREFAC ) THEN
*
*                       Reconstruct matrix from factors and compute
*                       residual.
*
                        CALL AB_ZGET01( N, N, A, LDA, AFAC, LDA, IWORK,
     $                               RWORK( 2*NRHS+1 ), RESULT( 1 ) )
                        K1 = 1
                     ELSE
                        K1 = 2
                     END IF
*
                     IF( INFO.EQ.0 ) THEN
                        TRFCON = .FALSE.
*
*                       Compute residual of the computed solution.
*
                        CALL AB_ZLACPY( 'Full', N, NRHS, BSAV, LDA, WORK
     $,
     $                               LDA )
                        CALL AB_ZGET02( TRANS, N, N, NRHS, ASAV, LDA, X,
     $                               LDA, WORK, LDA, RWORK( 2*NRHS+1 ),
     $                               RESULT( 2 ) )
*
*                       Check solution from generated exact solution.
*
                        IF( NOFACT .OR. ( PREFAC .AND. AB_LSAME( EQUED,
     $                      'N' ) ) ) THEN
                           CALL AB_ZGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  RCONDC, RESULT( 3 ) )
                        ELSE
                           IF( ITRAN.EQ.1 ) THEN
                              ROLDC = ROLDO
                           ELSE
                              ROLDC = ROLDI
                           END IF
                           CALL AB_ZGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  ROLDC, RESULT( 3 ) )
                        END IF
                     ELSE
                        TRFCON = .TRUE.
                     END IF
*
*                    Compare RCOND from AB_AB_AB_ZGESVXX with the computed value
*                    in RCONDC.
*
                     RESULT( 6 ) = AB_DGET06( RCOND, RCONDC )
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     IF( .NOT.TRFCON ) THEN
                        DO 45 K = K1, NTESTS
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL AB_ALADHD( NOUT, PATH )
                              IF( PREFAC ) THEN
                                 WRITE( NOUT, FMT = 9997 )'AB_AB_AB_ZGES
     $VXX',
     $                              FACT, TRANS, N, EQUED, IMAT, K,
     $                              RESULT( K )
                              ELSE
                                 WRITE( NOUT, FMT = 9998 )'AB_AB_AB_ZGES
     $VXX',
     $                              FACT, TRANS, N, IMAT, K, RESULT( K )
                              END IF
                              NFAIL = NFAIL + 1
                           END IF
 45                     CONTINUE
                        NRUN = NRUN + 7 - K1
                     ELSE
                        IF( RESULT( 1 ).GE.THRESH .AND. .NOT.PREFAC )
     $                       THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           IF( PREFAC ) THEN
                              WRITE( NOUT, FMT = 9997 )'AB_AB_AB_ZGESVXX
     $', FACT,
     $                           TRANS, N, EQUED, IMAT, 1, RESULT( 1 )
                           ELSE
                              WRITE( NOUT, FMT = 9998 )'AB_AB_AB_ZGESVXX
     $', FACT,
     $                           TRANS, N, IMAT, 1, RESULT( 1 )
                           END IF
                           NFAIL = NFAIL + 1
                           NRUN = NRUN + 1
                        END IF
                        IF( RESULT( 6 ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           IF( PREFAC ) THEN
                              WRITE( NOUT, FMT = 9997 )'AB_AB_AB_ZGESVXX
     $', FACT,
     $                           TRANS, N, EQUED, IMAT, 6, RESULT( 6 )
                           ELSE
                              WRITE( NOUT, FMT = 9998 )'AB_AB_AB_ZGESVXX
     $', FACT,
     $                           TRANS, N, IMAT, 6, RESULT( 6 )
                           END IF
                           NFAIL = NFAIL + 1
                           NRUN = NRUN + 1
                        END IF
                        IF( RESULT( 7 ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           IF( PREFAC ) THEN
                              WRITE( NOUT, FMT = 9997 )'AB_AB_AB_ZGESVXX
     $', FACT,
     $                           TRANS, N, EQUED, IMAT, 7, RESULT( 7 )
                           ELSE
                              WRITE( NOUT, FMT = 9998 )'AB_AB_AB_ZGESVXX
     $', FACT,
     $                           TRANS, N, IMAT, 7, RESULT( 7 )
                           END IF
                           NFAIL = NFAIL + 1
                           NRUN = NRUN + 1
                        END IF
*
                     END IF
*
   50             CONTINUE
   60          CONTINUE
   70       CONTINUE
   80    CONTINUE
   90 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*

*     Test Error Bounds for AB_AB_AB_ZGESVXX

      CALL AB_ZEBCHVXX(THRESH, PATH)

 9999 FORMAT( 1X, A, ', N =', I5, ', type ', I2, ', test(', I2, ') =',
     $      G12.5 )
 9998 FORMAT( 1X, A, ', FACT=''', A1, ''', TRANS=''', A1, ''', N=', I5,
     $      ', type ', I2, ', test(', I1, ')=', G12.5 )
 9997 FORMAT( 1X, A, ', FACT=''', A1, ''', TRANS=''', A1, ''', N=', I5,
     $      ', EQUED=''', A1, ''', type ', I2, ', test(', I1, ')=',
     $      G12.5 )
      RETURN
*
*     End of AB_ZDRVGE
*
      END
