*> \brief \b AB_AB_CDRVSY_AA
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_CDRVSY_AA( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX,
*                             A, AFAC, AINV, B, X, XACT, WORK, RWORK, IWORK,
*                             NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NMAX, NN, NOUT, NRHS
*       REAL               THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            IWORK( * ), NVAL( * )
*       REAL               RWORK( * )
*       COMPLEX            A( * ), AFAC( * ), AINV( * ), B( * ),
*      $                   WORK( * ), X( * ), XACT( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_AB_CDRVSY_AA tests the driver routine AB_AB_CSYSV_AA.
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
*> \param[out] AINV
*> \verbatim
*>          AINV is REAL array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is REAL array, dimension (NMAX*NRHS)
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
*> \param[out] WORK
*> \verbatim
*>          WORK is REAL array, dimension (NMAX*max(2,NRHS))
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
*> \date November 2017
*
*  @generated from LIN/AB_AB_DDRVSY_AA.f, fortran d -> c, Thu Nov 17 12:14:51 2016
*
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_AB_CDRVSY_AA( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR
     $,
     $                      NMAX, A, AFAC, AINV, B, X, XACT, WORK,
     $                      RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NVAL( * )
      REAL               RWORK( * )
      COMPLEX            A( * ), AFAC( * ), AINV( * ), B( * ),
     $                   WORK( * ), X( * ), XACT( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      COMPLEX            CZERO
      PARAMETER          ( CZERO = 0.0E+0 )
      INTEGER            NTYPES, NTESTS
      PARAMETER          ( NTYPES = 10, NTESTS = 3 )
      INTEGER            NFACT
      PARAMETER          ( NFACT = 2 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ZEROT
      CHARACTER          DIST, FACT, TYPE, UPLO, XTYPE
      CHARACTER*3        MATPATH, PATH
      INTEGER            I, I1, I2, IFACT, IMAT, IN, INFO, IOFF, IUPLO,
     $                   IZERO, J, K, KL, KU, LDA, LWORK, MODE, N,
     $                   NB, NBMIN, NERRS, NFAIL, NIMAT, NRUN, NT
      REAL               ANORM, CNDNUM
*     ..
*     .. Local Arrays ..
      CHARACTER          FACTS( NFACT ), UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL   RESULT( NTESTS )
*     ..
*     .. External Functions ..
      REAL               AB_DGET06, AB_CLANSY
      EXTERNAL           AB_DGET06, AB_CLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALADHD, AB_ALAERH, AB_ALASVM, AB_CERRVX, AB_
     $CGET04, AB_CLACPY,
     $                   AB_CLARHS, AB_CLASET, AB_CLATB4, AB_CLATMS, AB_
     $CSYT02,
     $                   AB_AB_CSYSV_AA, AB_AB_CSYT01_AA, AB_AB_CSYTRF_A
     $A, AB_XLAENV
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
      INTRINSIC          MAX, MIN
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               UPLOS / 'U', 'L' / , FACTS / 'F', 'N' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
*     Test path
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'SA'
*
*     Path to generate matrices
*
      MATPATH( 1: 1 ) = 'Complex precision'
      MATPATH( 2: 3 ) = 'SY'
*
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
*     Set the block size and minimum block size for testing.
*
      NB = 1
      NBMIN = 2
      CALL AB_XLAENV( 1, NB )
      CALL AB_XLAENV( 2, NBMIN )
*
*     Do for each value of N in NVAL
*
      DO 180 IN = 1, NN
         N = NVAL( IN )
         LWORK = MAX( 3*N-2, N*(1+NB) )
         LWORK = MAX( LWORK, 1 )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         DO 170 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 170
*
*           Skip types 3, 4, 5, or 6 if the matrix size is too small.
*
            ZEROT = IMAT.GE.3 .AND. IMAT.LE.6
            IF( ZEROT .AND. N.LT.IMAT-2 )
     $         GO TO 170
*
*           Do first for UPLO = 'U', then for UPLO = 'L'
*
            DO 160 IUPLO = 1, 2
               UPLO = UPLOS( IUPLO )
*
*              Set up parameters with AB_CLATB4 and generate a test matrix
*              with AB_CLATMS.
*
               CALL AB_CLATB4( MATPATH, IMAT, N, N, TYPE, KL, KU, ANORM,
     $                      MODE, CNDNUM, DIST )
*
               SRNAMT = 'AB_CLATMS'
               CALL AB_CLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE,
     $                      CNDNUM, ANORM, KL, KU, UPLO, A, LDA, WORK,
     $                      INFO )
*
*              Check error code from AB_CLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL AB_ALAERH( PATH, 'AB_CLATMS', INFO, 0, UPLO, N, N
     $, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 160
               END IF
*
*              For types 3-6, zero one or more rows and columns of the
*              matrix to test that INFO is returned correctly.
*
               IF( ZEROT ) THEN
                  IF( IMAT.EQ.3 ) THEN
                     IZERO = 1
                  ELSE IF( IMAT.EQ.4 ) THEN
                     IZERO = N
                  ELSE
                     IZERO = N / 2 + 1
                  END IF
*
                  IF( IMAT.LT.6 ) THEN
*
*                    Set row and column IZERO to zero.
*
                     IF( IUPLO.EQ.1 ) THEN
                        IOFF = ( IZERO-1 )*LDA
                        DO 20 I = 1, IZERO - 1
                           A( IOFF+I ) = CZERO
   20                   CONTINUE
                        IOFF = IOFF + IZERO
                        DO 30 I = IZERO, N
                           A( IOFF ) = CZERO
                           IOFF = IOFF + LDA
   30                   CONTINUE
                     ELSE
                        IOFF = IZERO
                        DO 40 I = 1, IZERO - 1
                           A( IOFF ) = CZERO
                           IOFF = IOFF + LDA
   40                   CONTINUE
                        IOFF = IOFF - IZERO
                        DO 50 I = IZERO, N
                           A( IOFF+I ) = CZERO
   50                   CONTINUE
                     END IF
                  ELSE
                     IOFF = 0
                     IF( IUPLO.EQ.1 ) THEN
*
*                       Set the first IZERO rows and columns to zero.
*
                        DO 70 J = 1, N
                           I2 = MIN( J, IZERO )
                           DO 60 I = 1, I2
                              A( IOFF+I ) = CZERO
   60                      CONTINUE
                           IOFF = IOFF + LDA
   70                   CONTINUE
                        IZERO = 1
                     ELSE
*
*                       Set the last IZERO rows and columns to zero.
*
                        DO 90 J = 1, N
                           I1 = MAX( J, IZERO )
                           DO 80 I = I1, N
                              A( IOFF+I ) = CZERO
   80                      CONTINUE
                           IOFF = IOFF + LDA
   90                   CONTINUE
                     END IF
                  END IF
               ELSE
                  IZERO = 0
               END IF
*
               DO 150 IFACT = 1, NFACT
*
*                 Do first for FACT = 'F', then for other values.
*
                  FACT = FACTS( IFACT )
*
*                 Form an exact solution and set the right hand side.
*
                  SRNAMT = 'AB_CLARHS'
                  CALL AB_CLARHS( MATPATH, XTYPE, UPLO, ' ', N, N, KL, K
     $U,
     $                         NRHS, A, LDA, XACT, LDA, B, LDA, ISEED,
     $                         INFO )
                  XTYPE = 'C'
*
*                 --- Test AB_AB_CSYSV_AA  ---
*
                  IF( IFACT.EQ.2 ) THEN
                     CALL AB_CLACPY( UPLO, N, N, A, LDA, AFAC, LDA )
                     CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
*                    Factor the matrix and solve the system using AB_AB_CSYSV_AA.
*
                     SRNAMT = 'AB_AB_CSYSV_AA'
                     CALL AB_AB_CSYSV_AA( UPLO, N, NRHS, AFAC, LDA, IWOR
     $K,
     $                                 X, LDA, WORK, LWORK, INFO )
*
*                    Adjust the expected value of INFO to account for
*                    pivoting.
*
                     IF( IZERO.GT.0 ) THEN
                        J = 1
                        K = IZERO
  100                   CONTINUE
                        IF( J.EQ.K ) THEN
                           K = IWORK( J )
                        ELSE IF( IWORK( J ).EQ.K ) THEN
                           K = J
                        END IF
                        IF( J.LT.K ) THEN
                           J = J + 1
                           GO TO 100
                        END IF
                     ELSE
                        K = 0
                     END IF
*
*                    Check error code from AB_AB_CSYSV_AA .
*
                     IF( INFO.NE.K ) THEN
                        CALL AB_ALAERH( PATH, 'AB_AB_CSYSV_AA ', INFO, K
     $,
     $                               UPLO, N, N, -1, -1, NRHS,
     $                               IMAT, NFAIL, NERRS, NOUT )
                        GO TO 120
                     ELSE IF( INFO.NE.0 ) THEN
                        GO TO 120
                     END IF
*
*                    Reconstruct matrix from factors and compute
*                    residual.
*
                     CALL AB_AB_CSYT01_AA( UPLO, N, A, LDA, AFAC, LDA,
     $                               IWORK, AINV, LDA, RWORK,
     $                               RESULT( 1 ) )
*
*                    Compute residual of the computed solution.
*
                     CALL AB_CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA 
     $)
                     CALL AB_CSYT02( UPLO, N, NRHS, A, LDA, X, LDA, WORK
     $,
     $                            LDA, RWORK, RESULT( 2 ) )
                     NT = 2
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     DO 110 K = 1, NT
                        IF( RESULT( K ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL AB_ALADHD( NOUT, PATH )
                           WRITE( NOUT, FMT = 9999 )'AB_AB_CSYSV_AA ',
     $                        UPLO, N, IMAT, K, RESULT( K )
                           NFAIL = NFAIL + 1
                        END IF
  110                CONTINUE
                     NRUN = NRUN + NT
  120                CONTINUE
                  END IF
*
  150          CONTINUE
*
  160       CONTINUE
  170    CONTINUE
  180 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( 1X, A, ', UPLO=''', A1, ''', N =', I5, ', type ', I2,
     $      ', test ', I2, ', ratio =', G12.5 )
      RETURN
*
*     End of AB_AB_CDRVSY_AA
*
      END
