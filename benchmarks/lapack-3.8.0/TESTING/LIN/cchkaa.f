*> \brief \b CCHKAA
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       PROGRAM CCHKAA
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> CCHKAA is the main test program for the COMPLEX linear equation
*> routines.
*>
*> The program must be driven by a short data file. The first 15 records
*> (not including the first comment  line) specify problem dimensions
*> and program options using list-directed input. The remaining lines
*> specify the LAPACK test paths and the number of matrix types to use
*> in testing.  An annotated example of a data file can be obtained by
*> deleting the first 3 characters from the following 42 lines:
*> Data file for testing COMPLEX LAPACK linear equation routines
*> 7                      Number of values of M
*> 0 1 2 3 5 10 16        Values of M (row dimension)
*> 7                      Number of values of N
*> 0 1 2 3 5 10 16        Values of N (column dimension)
*> 1                      Number of values of NRHS
*> 2                      Values of NRHS (number of right hand sides)
*> 5                      Number of values of NB
*> 1 3 3 3 20             Values of NB (the blocksize)
*> 1 0 5 9 1              Values of NX (crossover point)
*> 3                      Number of values of RANK
*> 30 50 90               Values of rank (as a % of N)
*> 30.0                   Threshold value of test ratio
*> T                      Put T to test the LAPACK routines
*> T                      Put T to test the driver routines
*> T                      Put T to test the error exits
*> CGE   11               List types on next line if 0 < NTYPES < 11
*> CGB    8               List types on next line if 0 < NTYPES <  8
*> CGT   12               List types on next line if 0 < NTYPES < 12
*> CPO    9               List types on next line if 0 < NTYPES <  9
*> CPO    9               List types on next line if 0 < NTYPES <  9
*> CPP    9               List types on next line if 0 < NTYPES <  9
*> CPB    8               List types on next line if 0 < NTYPES <  8
*> CPT   12               List types on next line if 0 < NTYPES < 12
*> CHE   10               List types on next line if 0 < NTYPES < 10
*> CHR   10               List types on next line if 0 < NTYPES < 10
*> CHK   10               List types on next line if 0 < NTYPES < 10
*> CHA   10               List types on next line if 0 < NTYPES < 10
*> CH2   10               List types on next line if 0 < NTYPES < 10
*> CSA   11               List types on next line if 0 < NTYPES < 10
*> CS2   11               List types on next line if 0 < NTYPES < 10
*> CHP   10               List types on next line if 0 < NTYPES < 10
*> CSY   11               List types on next line if 0 < NTYPES < 11
*> CSK   11               List types on next line if 0 < NTYPES < 11
*> CSR   11               List types on next line if 0 < NTYPES < 11
*> CSP   11               List types on next line if 0 < NTYPES < 11
*> CTR   18               List types on next line if 0 < NTYPES < 18
*> CTP   18               List types on next line if 0 < NTYPES < 18
*> CTB   17               List types on next line if 0 < NTYPES < 17
*> CQR    8               List types on next line if 0 < NTYPES <  8
*> CRQ    8               List types on next line if 0 < NTYPES <  8
*> CLQ    8               List types on next line if 0 < NTYPES <  8
*> CQL    8               List types on next line if 0 < NTYPES <  8
*> CQP    6               List types on next line if 0 < NTYPES <  6
*> CTZ    3               List types on next line if 0 < NTYPES <  3
*> CLS    6               List types on next line if 0 < NTYPES <  6
*> CEQ
*> CQT
*> CQX
*> \endverbatim
*
*  Parameters:
*  ==========
*
*> \verbatim
*>  NMAX    INTEGER
*>          The maximum allowable value for M and N.
*>
*>  MAXIN   INTEGER
*>          The number of different values that can be used for each of
*>          M, N, NRHS, NB, NX and RANK
*>
*>  MAXRHS  INTEGER
*>          The maximum number of right hand sides
*>
*>  MATMAX  INTEGER
*>          The maximum number of matrix types to use for testing
*>
*>  NIN     INTEGER
*>          The unit number for input
*>
*>  NOUT    INTEGER
*>          The unit number for output
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
*> \ingroup complex_lin
*
*  =====================================================================
      PROGRAM CCHKAA
*
*  -- LAPACK test routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX
      PARAMETER          ( NMAX = 132 )
      INTEGER            MAXIN
      PARAMETER          ( MAXIN = 12 )
      INTEGER            MAXRHS
      PARAMETER          ( MAXRHS = 16 )
      INTEGER            MATMAX
      PARAMETER          ( MATMAX = 30 )
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            KDMAX
      PARAMETER          ( KDMAX = NMAX+( NMAX+1 ) / 4 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FATAL, TSTCHK, TSTDRV, TSTERR
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        PATH
      CHARACTER*10       INTSTR
      CHARACTER*72       ALINE
      INTEGER            I, IC, J, K, LA, LAFAC, LDA, NB, NM, NMATS, NN,
     $                   NNB, NNB2, NNS, NRHS, NTYPES, NRANK,
     $                   VERS_MAJOR, VERS_MINOR, VERS_PATCH
      REAL               EPS, S1, S2, THREQ, THRESH
*     ..
*     .. Local Arrays ..
      LOGICAL            DOTYPE( MATMAX )
      INTEGER            IWORK( 25*NMAX ), MVAL( MAXIN ),
     $                   NBVAL( MAXIN ), NBVAL2( MAXIN ),
     $                   NSVAL( MAXIN ), NVAL( MAXIN ), NXVAL( MAXIN ),
     $                   RANKVAL( MAXIN ), PIV( NMAX )
      REAL               RWORK( 150*NMAX+2*MAXRHS ), S( 2*NMAX )
      COMPLEX            A( ( KDMAX+1 )*NMAX, 7 ), B( NMAX*MAXRHS, 4 ),
     $                   E( NMAX ), WORK( NMAX, NMAX+MAXRHS+10 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME, AB_AB_LSAMEN
      REAL               AB_SECOND, AB_SLAMCH
      EXTERNAL           AB_LSAME, AB_AB_LSAMEN, AB_SECOND, AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAREQ, AB_AB_CCHKEQ, AB_CCHKGB, AB_CCHKGE, 
     $AB_CCHKGT, AB_CCHKHE,
     $                   AB_AB_CCHKHE_ROOK, AB_AB_CCHKHE_RK, AB_AB_CCHKH
     $E_AA, AB_CCHKLQ,
     $                   AB_CCHKPB,AB_CCHKPO, AB_CCHKPS, AB_CCHKPP, AB_C
     $CHKPT, AB_CCHKQ3,
     $                   AB_CCHKQL, AB_CCHKQR, AB_CCHKRQ, AB_CCHKSP, AB_
     $CCHKSY,
     $                   AB_AB_CCHKSY_ROOK, AB_AB_CCHKSY_RK, AB_AB_CCHKS
     $Y_AA, AB_CCHKTB, 
     $                   AB_CCHKTP, AB_CCHKTR, AB_CCHKTZ, AB_CDRVGB, AB_
     $CDRVGE, AB_CDRVGT,
     $                   AB_CDRVHE, AB_AB_CDRVHE_ROOK, AB_AB_CDRVHE_RK, 
     $AB_AB_CDRVHE_AA, 
     $                   AB_CDRVHP, AB_CDRVLS, AB_CDRVPB, AB_CDRVPO, AB_
     $CDRVPP, AB_CDRVPT,
     $                   AB_CDRVSP, AB_CDRVSY, AB_AB_CDRVSY_ROOK, AB_AB_
     $CDRVSY_RK,
     $                   AB_AB_CDRVSY_AA, AB_ILAVER, AB_AB_CCHKQRT, AB_A
     $B_AB_CCHKQRTP
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Arrays in Common ..
      INTEGER            IPARMS( 100 )
*     ..
*     .. Common blocks ..
      COMMON             / CLAENV / IPARMS
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               THREQ / 2.0 / , INTSTR / '0123456789' /
*     ..
*     .. Executable Statements ..
*
      S1 = AB_SECOND( )
      LDA = NMAX
      FATAL = .FALSE.
*
*     Read a dummy line.
*
      READ( NIN, FMT = * )
*
*     Report values of parameters.
*
      CALL AB_ILAVER( VERS_MAJOR, VERS_MINOR, VERS_PATCH )
      WRITE( NOUT, FMT = 9994 ) VERS_MAJOR, VERS_MINOR, VERS_PATCH
*
*     Read the values of M
*
      READ( NIN, FMT = * )NM
      IF( NM.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NM ', NM, 1
         NM = 0
         FATAL = .TRUE.
      ELSE IF( NM.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NM ', NM, MAXIN
         NM = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( MVAL( I ), I = 1, NM )
      DO 10 I = 1, NM
         IF( MVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' M  ', MVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( MVAL( I ).GT.NMAX ) THEN
            WRITE( NOUT, FMT = 9995 )' M  ', MVAL( I ), NMAX
            FATAL = .TRUE.
         END IF
   10 CONTINUE
      IF( NM.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'M   ', ( MVAL( I ), I = 1, NM )
*
*     Read the values of N
*
      READ( NIN, FMT = * )NN
      IF( NN.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NN ', NN, 1
         NN = 0
         FATAL = .TRUE.
      ELSE IF( NN.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NN ', NN, MAXIN
         NN = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( NVAL( I ), I = 1, NN )
      DO 20 I = 1, NN
         IF( NVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' N  ', NVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( NVAL( I ).GT.NMAX ) THEN
            WRITE( NOUT, FMT = 9995 )' N  ', NVAL( I ), NMAX
            FATAL = .TRUE.
         END IF
   20 CONTINUE
      IF( NN.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'N   ', ( NVAL( I ), I = 1, NN )
*
*     Read the values of NRHS
*
      READ( NIN, FMT = * )NNS
      IF( NNS.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NNS', NNS, 1
         NNS = 0
         FATAL = .TRUE.
      ELSE IF( NNS.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NNS', NNS, MAXIN
         NNS = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( NSVAL( I ), I = 1, NNS )
      DO 30 I = 1, NNS
         IF( NSVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )'NRHS', NSVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( NSVAL( I ).GT.MAXRHS ) THEN
            WRITE( NOUT, FMT = 9995 )'NRHS', NSVAL( I ), MAXRHS
            FATAL = .TRUE.
         END IF
   30 CONTINUE
      IF( NNS.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'NRHS', ( NSVAL( I ), I = 1, NNS )
*
*     Read the values of NB
*
      READ( NIN, FMT = * )NNB
      IF( NNB.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )'NNB ', NNB, 1
         NNB = 0
         FATAL = .TRUE.
      ELSE IF( NNB.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )'NNB ', NNB, MAXIN
         NNB = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( NBVAL( I ), I = 1, NNB )
      DO 40 I = 1, NNB
         IF( NBVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' NB ', NBVAL( I ), 0
            FATAL = .TRUE.
         END IF
   40 CONTINUE
      IF( NNB.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'NB  ', ( NBVAL( I ), I = 1, NNB )
*
*     Set NBVAL2 to be the set of unique values of NB
*
      NNB2 = 0
      DO 60 I = 1, NNB
         NB = NBVAL( I )
         DO 50 J = 1, NNB2
            IF( NB.EQ.NBVAL2( J ) )
     $         GO TO 60
   50    CONTINUE
         NNB2 = NNB2 + 1
         NBVAL2( NNB2 ) = NB
   60 CONTINUE
*
*     Read the values of NX
*
      READ( NIN, FMT = * )( NXVAL( I ), I = 1, NNB )
      DO 70 I = 1, NNB
         IF( NXVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' NX ', NXVAL( I ), 0
            FATAL = .TRUE.
         END IF
   70 CONTINUE
      IF( NNB.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'NX  ', ( NXVAL( I ), I = 1, NNB )
*
*     Read the values of RANKVAL
*
      READ( NIN, FMT = * )NRANK
      IF( NN.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NRANK ', NRANK, 1
         NRANK = 0
         FATAL = .TRUE.
      ELSE IF( NN.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NRANK ', NRANK, MAXIN
         NRANK = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( RANKVAL( I ), I = 1, NRANK )
      DO I = 1, NRANK
         IF( RANKVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' RANK  ', RANKVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( RANKVAL( I ).GT.100 ) THEN
            WRITE( NOUT, FMT = 9995 )' RANK  ', RANKVAL( I ), 100
            FATAL = .TRUE.
         END IF
      END DO
      IF( NRANK.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'RANK % OF N',
     $   ( RANKVAL( I ), I = 1, NRANK )
*
*     Read the threshold value for the test ratios.
*
      READ( NIN, FMT = * )THRESH
      WRITE( NOUT, FMT = 9992 )THRESH
*
*     Read the flag that indicates whether to test the LAPACK routines.
*
      READ( NIN, FMT = * )TSTCHK
*
*     Read the flag that indicates whether to test the driver routines.
*
      READ( NIN, FMT = * )TSTDRV
*
*     Read the flag that indicates whether to test the error exits.
*
      READ( NIN, FMT = * )TSTERR
*
      IF( FATAL ) THEN
         WRITE( NOUT, FMT = 9999 )
         STOP
      END IF
*
*     Calculate and print the machine dependent constants.
*
      EPS = AB_SLAMCH( 'Underflow threshold' )
      WRITE( NOUT, FMT = 9991 )'underflow', EPS
      EPS = AB_SLAMCH( 'Overflow threshold' )
      WRITE( NOUT, FMT = 9991 )'overflow ', EPS
      EPS = AB_SLAMCH( 'Epsilon' )
      WRITE( NOUT, FMT = 9991 )'precision', EPS
      WRITE( NOUT, FMT = * )
      NRHS = NSVAL( 1 )
*
   80 CONTINUE
*
*     Read a test path and the number of matrix types to use.
*
      READ( NIN, FMT = '(A72)', END = 140 )ALINE
      PATH = ALINE( 1: 3 )
      NMATS = MATMAX
      I = 3
   90 CONTINUE
      I = I + 1
      IF( I.GT.72 )
     $   GO TO 130
      IF( ALINE( I: I ).EQ.' ' )
     $   GO TO 90
      NMATS = 0
  100 CONTINUE
      C1 = ALINE( I: I )
      DO 110 K = 1, 10
         IF( C1.EQ.INTSTR( K: K ) ) THEN
            IC = K - 1
            GO TO 120
         END IF
  110 CONTINUE
      GO TO 130
  120 CONTINUE
      NMATS = NMATS*10 + IC
      I = I + 1
      IF( I.GT.72 )
     $   GO TO 130
      GO TO 100
  130 CONTINUE
      C1 = PATH( 1: 1 )
      C2 = PATH( 2: 3 )
*
*     Check first character for correct precision.
*
      IF( .NOT.AB_LSAME( C1, 'Complex precision' ) ) THEN
         WRITE( NOUT, FMT = 9990 )PATH
*
      ELSE IF( NMATS.LE.0 ) THEN
*
*        Check for a positive number of tests requested.
*
         WRITE( NOUT, FMT = 9989 )PATH
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        GE:  general matrices
*
         NTYPES = 11
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKGE( DOTYPE, NM, MVAL, NN, NVAL, NNB2, NBVAL2, NN
     $S,
     $                   NSVAL, THRESH, TSTERR, LDA, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), B( 1, 1 ), B( 1, 2 ),
     $                   B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVGE( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), B( 1, 4 ), S, WORK,
     $                   RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        GB:  general banded matrices
*
         LA = ( 2*KDMAX+1 )*NMAX
         LAFAC = ( 3*KDMAX+1 )*NMAX
         NTYPES = 8
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKGB( DOTYPE, NM, MVAL, NN, NVAL, NNB2, NBVAL2, NN
     $S,
     $                   NSVAL, THRESH, TSTERR, A( 1, 1 ), LA,
     $                   A( 1, 3 ), LAFAC, B( 1, 1 ), B( 1, 2 ),
     $                   B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVGB( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR,
     $                   A( 1, 1 ), LA, A( 1, 3 ), LAFAC, A( 1, 6 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), B( 1, 4 ), S,
     $                   WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        GT:  general tridiagonal matrices
*
         NTYPES = 12
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKGT( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ),
     $                   B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVGT( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR,
     $                   A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ),
     $                   B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        PO:  positive definite matrices
*
         NTYPES = 9
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKPO( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, NSVAL,
     $                   THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                   A( 1, 3 ), B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                   WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVPO( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), B( 1, 4 ), S, WORK,
     $                   RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PS' ) ) THEN
*
*        PS:  positive semi-definite matrices
*
         NTYPES = 9
*
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKPS( DOTYPE, NN, NVAL, NNB2, NBVAL2, NRANK,
     $                   RANKVAL, THRESH, TSTERR, LDA, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), PIV, WORK, RWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        PP:  positive definite packed matrices
*
         NTYPES = 9
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKPP( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK, RWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVPP( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), B( 1, 4 ), S, WORK,
     $                   RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        PB:  positive definite banded matrices
*
         NTYPES = 8
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKPB( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, NSVAL,
     $                   THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                   A( 1, 3 ), B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                   WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVPB( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), B( 1, 4 ), S, WORK,
     $                   RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        PT:  positive definite tridiagonal matrices
*
         NTYPES = 12
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKPT( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   A( 1, 1 ), S, A( 1, 2 ), B( 1, 1 ), B( 1, 2 ),
     $                   B( 1, 3 ), WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVPT( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR,
     $                   A( 1, 1 ), S, A( 1, 2 ), B( 1, 1 ), B( 1, 2 ),
     $                   B( 1, 3 ), WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        HE:  Hermitian indefinite matrices,
*             with partial (Bunch-Kaufman) pivoting algorithm
*
         NTYPES = 10
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKHE( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, NSVAL,
     $                   THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                   A( 1, 3 ), B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                   WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVHE( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), WORK, RWORK, IWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HR' ) ) THEN
*
*        HR:  Hermitian indefinite matrices,
*             with bounded Bunch-Kaufman (rook) pivoting algorithm
*
         NTYPES = 10
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKHE_ROOK(DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, 
     $NSVAL,
     $                       THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                       A( 1, 3 ), B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                       WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_CDRVHE_ROOK( DOTYPE, NN, NVAL, NRHS, THRESH, TSTE
     $RR,
     $                        LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                        B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK,
     $                        RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HK' ) ) THEN
*
*        HK:  Hermitian indefinite matrices,
*             with bounded Bunch-Kaufman (rook) pivoting algorithm,
*             differnet matrix storage format than HR path version.
*
         NTYPES = 10
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKHE_RK( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, N
     $SVAL,
     $                      THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                      E, A( 1, 3 ), B( 1, 1 ), B( 1, 2 ),
     $                      B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_CDRVHE_RK( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR
     $,
     $                      LDA, A( 1, 1 ), A( 1, 2 ), E, A( 1, 3 ),
     $                      B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK,
     $                      RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HA' ) ) THEN
*
*        HA:  Hermitian matrices,
*             Aasen Algorithm
*
         NTYPES = 10
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKHE_AA( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS,
     $                      NSVAL, THRESH, TSTERR, LDA,
     $                      A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                      B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                      WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_CDRVHE_AA( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR
     $,
     $                      LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                           B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                      WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'H2' ) ) THEN
*
*        H2:  Hermitian matrices,
*             with partial (Aasen's) pivoting algorithm
*
         NTYPES = 10
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_AB_CCHKHE_AA_2STAGE( DOTYPE, NN, NVAL, NNB2, NBVA
     $L2,
     $                         NNS, NSVAL, THRESH, TSTERR, LDA,
     $                         A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                         B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                         WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_AB_CDRVHE_AA_2STAGE(
     $                         DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR,
     $                         LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                              B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                         WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        HP:  Hermitian indefinite packed matrices,
*             with partial (Bunch-Kaufman) pivoting algorithm
*
         NTYPES = 10
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKHP( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK, RWORK,
     $                   IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVHP( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), WORK, RWORK, IWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        SY:  symmetric indefinite matrices,
*             with partial (Bunch-Kaufman) pivoting algorithm
*
         NTYPES = 11
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKSY( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, NSVAL,
     $                   THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                   A( 1, 3 ), B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                   WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVSY( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), WORK, RWORK, IWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        SR:  symmetric indefinite matrices,
*             with bounded Bunch-Kaufman (rook) pivoting algorithm
*
         NTYPES = 11
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKSY_ROOK(DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, 
     $NSVAL,
     $                       THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                       A( 1, 3 ), B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                       WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_CDRVSY_ROOK( DOTYPE, NN, NVAL, NRHS, THRESH, TSTE
     $RR,
     $                        LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                        B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK,
     $                        RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SK' ) ) THEN
*
*        SK:  symmetric indefinite matrices,
*             with bounded Bunch-Kaufman (rook) pivoting algorithm,
*             differnet matrix storage format than SR path version.
*
         NTYPES = 11
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKSY_RK( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, N
     $SVAL,
     $                      THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                      E, A( 1, 3 ), B( 1, 1 ), B( 1, 2 ),
     $                      B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_CDRVSY_RK( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR
     $,
     $                      LDA, A( 1, 1 ), A( 1, 2 ), E, A( 1, 3 ),
     $                      B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK,
     $                      RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SA' ) ) THEN
*
*        SA:  symmetric indefinite matrices with Aasen's algorithm,
*
         NTYPES = 11
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKSY_AA( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, N
     $SVAL,
     $                      THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                      A( 1, 3 ), B( 1, 1 ), B( 1, 2 ),
     $                      B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_CDRVSY_AA( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR
     $,
     $                      LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                      B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK,
     $                      RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'S2' ) ) THEN
*
*        S2:  symmetric indefinite matrices with Aasen's algorithm
*             2 stage
*
         NTYPES = 11
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_AB_AB_CCHKSY_AA_2STAGE( DOTYPE, NN, NVAL, NNB2, NBVA
     $L2, NNS,
     $                      NSVAL, THRESH, TSTERR, LDA,
     $                      A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                      B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                      WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_AB_AB_CDRVSY_AA_2STAGE(
     $                      DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR,
     $                      LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                      B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK,
     $                      RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        SP:  symmetric indefinite packed matrices,
*             with partial (Bunch-Kaufman) pivoting algorithm
*
         NTYPES = 11
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKSP( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   LDA, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK, RWORK,
     $                   IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVSP( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), WORK, RWORK, IWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TR' ) ) THEN
*
*        TR:  triangular matrices
*
         NTYPES = 18
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKTR( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, NSVAL,
     $                   THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), WORK, RWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        TP:  triangular packed matrices
*
         NTYPES = 18
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKTP( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   LDA, A( 1, 1 ), A( 1, 2 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        TB:  triangular banded matrices
*
         NTYPES = 17
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKTB( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR
     $,
     $                   LDA, A( 1, 1 ), A( 1, 2 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'QR' ) ) THEN
*
*        QR:  QR factorization
*
         NTYPES = 8
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKQR( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVA
     $L,
     $                   NRHS, THRESH, TSTERR, NMAX, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), B( 1, 4 ),
     $                   WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'LQ' ) ) THEN
*
*        LQ:  LQ factorization
*
         NTYPES = 8
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKLQ( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVA
     $L,
     $                   NRHS, THRESH, TSTERR, NMAX, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), B( 1, 4 ),
     $                   WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'QL' ) ) THEN
*
*        QL:  QL factorization
*
         NTYPES = 8
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKQL( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVA
     $L,
     $                   NRHS, THRESH, TSTERR, NMAX, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), B( 1, 4 ),
     $                   WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'RQ' ) ) THEN
*
*        RQ:  RQ factorization
*
         NTYPES = 8
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKRQ( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVA
     $L,
     $                   NRHS, THRESH, TSTERR, NMAX, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), B( 1, 4 ),
     $                   WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'EQ' ) ) THEN
*
*        EQ:  Equilibration routines for general and positive definite
*             matrices (THREQ should be between 2 and 10)
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKEQ( THREQ, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TZ' ) ) THEN
*
*        TZ:  Trapezoidal matrix
*
         NTYPES = 3
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKTZ( DOTYPE, NM, MVAL, NN, NVAL, THRESH, TSTERR,
     $                   A( 1, 1 ), A( 1, 2 ), S( 1 ),
     $                   B( 1, 1 ), WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'QP' ) ) THEN
*
*        QP:  QR factorization with pivoting
*
         NTYPES = 6
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKQ3( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVA
     $L,
     $                   THRESH, A( 1, 1 ), A( 1, 2 ), S( 1 ),
     $                   B( 1, 1 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        LS:  Least squares drivers
*
         NTYPES = 6
         CALL AB_ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTDRV ) THEN
            CALL AB_CDRVLS( DOTYPE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
     $                   NBVAL, NXVAL, THRESH, TSTERR, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   S( 1 ), S( NMAX+1 ), NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'QT' ) ) THEN
*
*        QT:  QRT routines for general matrices
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKQRT( THRESH, TSTERR, NM, MVAL, NN, NVAL, NNB,
     $                    NBVAL, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'QX' ) ) THEN
*
*        QX:  QRT routines for triangular-pentagonal matrices
*
         IF( TSTCHK ) THEN
            CALL AB_AB_AB_CCHKQRTP( THRESH, TSTERR, NM, MVAL, NN, NVAL, 
     $NNB,
     $                     NBVAL, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TQ' ) ) THEN
*
*        TQ:  LQT routines for general matrices
*
         IF( TSTCHK ) THEN
            CALL AB_AB_CCHKLQT( THRESH, TSTERR, NM, MVAL, NN, NVAL, NNB,
     $                    NBVAL, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'XQ' ) ) THEN
*
*        XQ:  LQT routines for triangular-pentagonal matrices
*
         IF( TSTCHK ) THEN
            CALL AB_AB_AB_CCHKLQTP( THRESH, TSTERR, NM, MVAL, NN, NVAL, 
     $NNB,
     $                     NBVAL, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TS' ) ) THEN
*
*        TS:  QR routines for tall-skinny matrices
*
         IF( TSTCHK ) THEN
            CALL AB_CCHKTSQR( THRESH, TSTERR, NM, MVAL, NN, NVAL, NNB,
     $                     NBVAL, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE
*
         WRITE( NOUT, FMT = 9990 )PATH
      END IF
*
*     Go back to get another input line.
*
      GO TO 80
*
*     Branch to this line when the last record is read.
*
  140 CONTINUE
      CLOSE ( NIN )
      S2 = AB_SECOND( )
      WRITE( NOUT, FMT = 9998 )
      WRITE( NOUT, FMT = 9997 )S2 - S1
*
 9999 FORMAT( / ' Execution not attempted due to input errors' )
 9998 FORMAT( / ' End of tests' )
 9997 FORMAT( ' Total time used = ', F12.2, ' AB_SECONDs', / )
 9996 FORMAT( ' Invalid input value: ', A4, '=', I6, '; must be >=',
     $      I6 )
 9995 FORMAT( ' Invalid input value: ', A4, '=', I6, '; must be <=',
     $      I6 )
 9994 FORMAT( ' Tests of the COMPLEX LAPACK routines ',
     $      / ' LAPACK VERSION ', I1, '.', I1, '.', I1,
     $      / / ' The following parameter values will be used:' )
 9993 FORMAT( 4X, A4, ':  ', 10I6, / 11X, 10I6 )
 9992 FORMAT( / ' Routines pass computational tests if test ratio is ',
     $      'less than', F8.2, / )
 9991 FORMAT( ' Relative machine ', A, ' is taken to be', E16.6 )
 9990 FORMAT( / 1X, A3, ':  Unrecognized path name' )
 9989 FORMAT( / 1X, A3, ' routines were not tested' )
 9988 FORMAT( / 1X, A3, ' driver routines were not tested' )
*
*     End of CCHKAA
*
      END
