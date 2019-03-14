*> \brief \b AB_AB_CCHKEC
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_CCHKEC( THRESH, TSTERR, NIN, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NIN, NOUT
*       REAL               THRESH
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_AB_CCHKEC tests eigen- condition estimation routines
*>        AB_CTRSYL, AB_CTREXC, AB_CTRSNA, AB_CTRSEN
*>
*> In all cases, the routine runs through a fixed set of numerical
*> examples, subjects them to various tests, and compares the test
*> results to a threshold THRESH. In addition, AB_CTRSNA and AB_CTRSEN are
*> tested by reading in precomputed examples from a file (on input unit
*> NIN).  Output is written to output unit NOUT.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] THRESH
*> \verbatim
*>          THRESH is REAL
*>          Threshold for residual tests.  A computed test ratio passes
*>          the threshold if it is less than THRESH.
*> \endverbatim
*>
*> \param[in] TSTERR
*> \verbatim
*>          TSTERR is LOGICAL
*>          Flag that indicates whether error exits are to be tested.
*> \endverbatim
*>
*> \param[in] NIN
*> \verbatim
*>          NIN is INTEGER
*>          The logical unit number for input.
*> \endverbatim
*>
*> \param[in] NOUT
*> \verbatim
*>          NOUT is INTEGER
*>          The logical unit number for output.
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
*> \ingroup complex_eig
*
*  =====================================================================
      SUBROUTINE AB_AB_CCHKEC( THRESH, TSTERR, NIN, NOUT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NIN, NOUT
      REAL               THRESH
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            OK
      CHARACTER*3        PATH
      INTEGER            KTREXC, KTRSEN, KTRSNA, KTRSYL, LTREXC, LTRSYL,
     $                   NTESTS, NTREXC, NTRSYL
      REAL               EPS, RTREXC, RTRSYL, SFMIN
*     ..
*     .. Local Arrays ..
      INTEGER            LTRSEN( 3 ), LTRSNA( 3 ), NTRSEN( 3 ),
     $                   NTRSNA( 3 )
      REAL               RTRSEN( 3 ), RTRSNA( 3 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CERREC, AB_CGET35, AB_CGET36, AB_CGET37, AB_
     $CGET38
*     ..
*     .. External Functions ..
      REAL               AB_SLAMCH
      EXTERNAL           AB_SLAMCH
*     ..
*     .. Executable Statements ..
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'EC'
      EPS = AB_SLAMCH( 'P' )
      SFMIN = AB_SLAMCH( 'S' )
      WRITE( NOUT, FMT = 9994 )
      WRITE( NOUT, FMT = 9993 )EPS, SFMIN
      WRITE( NOUT, FMT = 9992 )THRESH
*
*     Test error exits if TSTERR is .TRUE.
*
      IF( TSTERR )
     $   CALL AB_CERREC( PATH, NOUT )
*
      OK = .TRUE.
      CALL AB_CGET35( RTRSYL, LTRSYL, NTRSYL, KTRSYL, NIN )
      IF( RTRSYL.GT.THRESH ) THEN
         OK = .FALSE.
         WRITE( NOUT, FMT = 9999 )RTRSYL, LTRSYL, NTRSYL, KTRSYL
      END IF
*
      CALL AB_CGET36( RTREXC, LTREXC, NTREXC, KTREXC, NIN )
      IF( RTREXC.GT.THRESH .OR. NTREXC.GT.0 ) THEN
         OK = .FALSE.
         WRITE( NOUT, FMT = 9998 )RTREXC, LTREXC, NTREXC, KTREXC
      END IF
*
      CALL AB_CGET37( RTRSNA, LTRSNA, NTRSNA, KTRSNA, NIN )
      IF( RTRSNA( 1 ).GT.THRESH .OR. RTRSNA( 2 ).GT.THRESH .OR.
     $    NTRSNA( 1 ).NE.0 .OR. NTRSNA( 2 ).NE.0 .OR. NTRSNA( 3 ).NE.0 )
     $     THEN
         OK = .FALSE.
         WRITE( NOUT, FMT = 9997 )RTRSNA, LTRSNA, NTRSNA, KTRSNA
      END IF
*
      CALL AB_CGET38( RTRSEN, LTRSEN, NTRSEN, KTRSEN, NIN )
      IF( RTRSEN( 1 ).GT.THRESH .OR. RTRSEN( 2 ).GT.THRESH .OR.
     $    NTRSEN( 1 ).NE.0 .OR. NTRSEN( 2 ).NE.0 .OR. NTRSEN( 3 ).NE.0 )
     $     THEN
         OK = .FALSE.
         WRITE( NOUT, FMT = 9996 )RTRSEN, LTRSEN, NTRSEN, KTRSEN
      END IF
*
      NTESTS = KTRSYL + KTREXC + KTRSNA + KTRSEN
      IF( OK )
     $   WRITE( NOUT, FMT = 9995 )PATH, NTESTS
*
 9999 FORMAT( ' Error in AB_CTRSYL: RMAX =', E12.3, / ' LMAX = ', I8,
     $      ' NINFO=', I8, ' KNT=', I8 )
 9998 FORMAT( ' Error in AB_CTREXC: RMAX =', E12.3, / ' LMAX = ', I8,
     $      ' NINFO=', I8, ' KNT=', I8 )
 9997 FORMAT( ' Error in AB_CTRSNA: RMAX =', 3E12.3, / ' LMAX = ',
     $      3I8, ' NINFO=', 3I8, ' KNT=', I8 )
 9996 FORMAT( ' Error in AB_CTRSEN: RMAX =', 3E12.3, / ' LMAX = ',
     $      3I8, ' NINFO=', 3I8, ' KNT=', I8 )
 9995 FORMAT( / 1X, 'All tests for ', A3,
     $      ' routines passed the threshold ( ', I6, ' tests run)' )
 9994 FORMAT( ' Tests of the Nonsymmetric eigenproblem condition',
     $      ' estimation routines', / ' AB_CTRSYL, AB_CTREXC, AB_CTRSNA,
     $ AB_CTRSEN',
     $      / )
 9993 FORMAT( ' Relative machine precision (EPS) = ', E16.6,
     $      / ' Safe minimum (SFMIN)             = ', E16.6, / )
 9992 FORMAT( ' Routines pass computational tests if test ratio is ',
     $      'less than', F8.2, / / )
      RETURN
*
*     End of AB_AB_CCHKEC
*
      END
