*> \brief \b AB_ZCKGQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZCKGQR( NM, MVAL, NP, PVAL, NN, NVAL, NMATS, ISEED,
*                          THRESH, NMAX, A, AF, AQ, AR, TAUA, B, BF, BZ,
*                          BT, BWK, TAUB, WORK, RWORK, NIN, NOUT, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, NIN, NM, NMATS, NMAX, NN, NOUT, NP
*       DOUBLE PRECISION   THRESH
*       ..
*       .. Array Arguments ..
*       INTEGER            ISEED( 4 ), MVAL( * ), NVAL( * ), PVAL( * )
*       DOUBLE PRECISION   RWORK( * )
*       COMPLEX*16         A( * ), AF( * ), AQ( * ), AR( * ), B( * ),
*      $                   BF( * ), BT( * ), BWK( * ), BZ( * ), TAUA( * ),
*      $                   TAUB( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZCKGQR tests
*> AB_ZGGQRF: GQR factorization for N-by-M matrix A and N-by-P matrix B,
*> AB_ZGGRQF: GRQ factorization for M-by-N matrix A and P-by-N matrix B.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] NM
*> \verbatim
*>          NM is INTEGER
*>          The number of values of M contained in the vector MVAL.
*> \endverbatim
*>
*> \param[in] MVAL
*> \verbatim
*>          MVAL is INTEGER array, dimension (NM)
*>          The values of the matrix row(column) dimension M.
*> \endverbatim
*>
*> \param[in] NP
*> \verbatim
*>          NP is INTEGER
*>          The number of values of P contained in the vector PVAL.
*> \endverbatim
*>
*> \param[in] PVAL
*> \verbatim
*>          PVAL is INTEGER array, dimension (NP)
*>          The values of the matrix row(column) dimension P.
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
*>          The values of the matrix column(row) dimension N.
*> \endverbatim
*>
*> \param[in] NMATS
*> \verbatim
*>          NMATS is INTEGER
*>          The number of matrix types to be tested for each combination
*>          of matrix dimensions.  If NMATS >= NTYPES (the maximum
*>          number of matrix types), then all the different types are
*>          generated for testing.  If NMATS < NTYPES, another input line
*>          is read to get the numbers of the matrix types to be used.
*> \endverbatim
*>
*> \param[in,out] ISEED
*> \verbatim
*>          ISEED is INTEGER array, dimension (4)
*>          On entry, the seed of the random number generator.  The array
*>          elements should be between 0 and 4095, otherwise they will be
*>          reduced mod 4096, and ISEED(4) must be odd.
*>          On exit, the next seed in the random number sequence after
*>          all the test matrices have been generated.
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
*> \param[in] NMAX
*> \verbatim
*>          NMAX is INTEGER
*>          The maximum value permitted for M or N, used in dimensioning
*>          the work arrays.
*> \endverbatim
*>
*> \param[out] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AF
*> \verbatim
*>          AF is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AQ
*> \verbatim
*>          AQ is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] AR
*> \verbatim
*>          AR is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] TAUA
*> \verbatim
*>          TAUA is COMPLEX*16 array, dimension (NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] BF
*> \verbatim
*>          BF is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] BZ
*> \verbatim
*>          BZ is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] BT
*> \verbatim
*>          BT is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] BWK
*> \verbatim
*>          BWK is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] TAUB
*> \verbatim
*>          TAUB is COMPLEX*16 array, dimension (NMAX)
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (NMAX*NMAX)
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension (NMAX)
*> \endverbatim
*>
*> \param[in] NIN
*> \verbatim
*>          NIN is INTEGER
*>          The unit number for input.
*> \endverbatim
*>
*> \param[in] NOUT
*> \verbatim
*>          NOUT is INTEGER
*>          The unit number for output.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0 :  successful exit
*>          > 0 :  If AB_ZLATMS returns an error code, the absolute value
*>                 of it is returned.
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
*> \ingroup complex16_eig
*
*  =====================================================================
      SUBROUTINE AB_ZCKGQR( NM, MVAL, NP, PVAL, NN, NVAL, NMATS, ISEED,
     $                   THRESH, NMAX, A, AF, AQ, AR, TAUA, B, BF, BZ,
     $                   BT, BWK, TAUB, WORK, RWORK, NIN, NOUT, INFO )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            INFO, NIN, NM, NMATS, NMAX, NN, NOUT, NP
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 ), MVAL( * ), NVAL( * ), PVAL( * )
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         A( * ), AF( * ), AQ( * ), AR( * ), B( * ),
     $                   BF( * ), BT( * ), BWK( * ), BZ( * ), TAUA( * ),
     $                   TAUB( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 7 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 8 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRSTT
      CHARACTER          DISTA, DISTB, TYPE
      CHARACTER*3        PATH
      INTEGER            I, IINFO, IM, IMAT, IN, IP, KLA, KLB, KUA, KUB,
     $                   LDA, LDB, LWORK, M, MODEA, MODEB, N, NFAIL,
     $                   NRUN, NT, P
      DOUBLE PRECISION   ANORM, BNORM, CNDNMA, CNDNMB
*     ..
*     .. Local Arrays ..
      LOGICAL            DOTYPE( NTYPES )
      DOUBLE PRECISION   RESULT( NTESTS )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_AB_ALAHDG, AB_ALAREQ, AB_ALASUM, AB_DLATB9, 
     $AB_ZGQRTS, AB_ZGRQTS,
     $                   AB_ZLATMS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
*     Initialize constants.
*
      PATH( 1: 3 ) = 'GQR'
      INFO = 0
      NRUN = 0
      NFAIL = 0
      FIRSTT = .TRUE.
      CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
      LDA = NMAX
      LDB = NMAX
      LWORK = NMAX*NMAX
*
*     Do for each value of M in MVAL.
*
      DO 60 IM = 1, NM
         M = MVAL( IM )
*
*        Do for each value of P in PVAL.
*
         DO 50 IP = 1, NP
            P = PVAL( IP )
*
*           Do for each value of N in NVAL.
*
            DO 40 IN = 1, NN
               N = NVAL( IN )
*
               DO 30 IMAT = 1, NTYPES
*
*                 Do the tests only if DOTYPE( IMAT ) is true.
*
                  IF( .NOT.DOTYPE( IMAT ) )
     $               GO TO 30
*
*                 Test AB_ZGGRQF
*
*                 Set up parameters with AB_DLATB9 and generate test
*                 matrices A and B with AB_ZLATMS.
*
                  CALL AB_DLATB9( 'GRQ', IMAT, M, P, N, TYPE, KLA, KUA,
     $                         KLB, KUB, ANORM, BNORM, MODEA, MODEB,
     $                         CNDNMA, CNDNMB, DISTA, DISTB )
*
                  CALL AB_ZLATMS( M, N, DISTA, ISEED, TYPE, RWORK, MODEA
     $,
     $                         CNDNMA, ANORM, KLA, KUA, 'No packing', A,
     $                         LDA, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
                  CALL AB_ZLATMS( P, N, DISTB, ISEED, TYPE, RWORK, MODEB
     $,
     $                         CNDNMB, BNORM, KLB, KUB, 'No packing', B,
     $                         LDB, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
                  NT = 4
*
                  CALL AB_ZGRQTS( M, P, N, A, AF, AQ, AR, LDA, TAUA, B, 
     $BF,
     $                         BZ, BT, BWK, LDB, TAUB, WORK, LWORK,
     $                         RWORK, RESULT )
*
*                 Print information about the tests that did not
*                 pass the threshold.
*
                  DO 10 I = 1, NT
                     IF( RESULT( I ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. FIRSTT ) THEN
                           FIRSTT = .FALSE.
                           CALL AB_AB_ALAHDG( NOUT, 'GRQ' )
                        END IF
                        WRITE( NOUT, FMT = 9998 )M, P, N, IMAT, I,
     $                     RESULT( I )
                        NFAIL = NFAIL + 1
                     END IF
   10             CONTINUE
                  NRUN = NRUN + NT
*
*                 Test AB_ZGGQRF
*
*                 Set up parameters with AB_DLATB9 and generate test
*                 matrices A and B with AB_ZLATMS.
*
                  CALL AB_DLATB9( 'GQR', IMAT, M, P, N, TYPE, KLA, KUA,
     $                         KLB, KUB, ANORM, BNORM, MODEA, MODEB,
     $                         CNDNMA, CNDNMB, DISTA, DISTB )
*
                  CALL AB_ZLATMS( N, M, DISTA, ISEED, TYPE, RWORK, MODEA
     $,
     $                         CNDNMA, ANORM, KLA, KUA, 'No packing', A,
     $                         LDA, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
                  CALL AB_ZLATMS( N, P, DISTB, ISEED, TYPE, RWORK, MODEA
     $,
     $                         CNDNMA, BNORM, KLB, KUB, 'No packing', B,
     $                         LDB, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
                  NT = 4
*
                  CALL AB_ZGQRTS( N, M, P, A, AF, AQ, AR, LDA, TAUA, B, 
     $BF,
     $                         BZ, BT, BWK, LDB, TAUB, WORK, LWORK,
     $                         RWORK, RESULT )
*
*                 Print information about the tests that did not
*                 pass the threshold.
*
                  DO 20 I = 1, NT
                     IF( RESULT( I ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. FIRSTT ) THEN
                           FIRSTT = .FALSE.
                           CALL AB_AB_ALAHDG( NOUT, PATH )
                        END IF
                        WRITE( NOUT, FMT = 9997 )N, M, P, IMAT, I,
     $                     RESULT( I )
                        NFAIL = NFAIL + 1
                     END IF
   20             CONTINUE
                  NRUN = NRUN + NT
*
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASUM( PATH, NOUT, NFAIL, NRUN, 0 )
*
 9999 FORMAT( ' AB_ZLATMS in AB_ZCKGQR:    INFO = ', I5 )
 9998 FORMAT( ' M=', I4, ' P=', I4, ', N=', I4, ', type ', I2,
     $      ', test ', I2, ', ratio=', G13.6 )
 9997 FORMAT( ' N=', I4, ' M=', I4, ', P=', I4, ', type ', I2,
     $      ', test ', I2, ', ratio=', G13.6 )
      RETURN
*
*     End of AB_ZCKGQR
*
      END
