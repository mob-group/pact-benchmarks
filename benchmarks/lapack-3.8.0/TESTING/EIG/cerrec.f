*> \brief \b AB_CERREC
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERREC( PATH, NUNIT )
*
*       .. Scalar Arguments ..
*       CHARACTER*3        PATH
*       INTEGER            NUNIT
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CERREC tests the error exits for the routines for eigen- condition
*> estimation for REAL matrices:
*>    AB_CTRSYL, AB_CTREXC, AB_CTRSNA and AB_CTRSEN.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] PATH
*> \verbatim
*>          PATH is CHARACTER*3
*>          The LAPACK path name for the routines to be tested.
*> \endverbatim
*>
*> \param[in] NUNIT
*> \verbatim
*>          NUNIT is INTEGER
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
*> \ingroup complex_eig
*
*  =====================================================================
      SUBROUTINE AB_CERREC( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 4, LW = NMAX*( NMAX+2 ) )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E0, ZERO = 0.0E0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IFST, ILST, INFO, J, M, NT
      REAL               SCALE
*     ..
*     .. Local Arrays ..
      LOGICAL            SEL( NMAX )
      REAL               RW( LW ), S( NMAX ), SEP( NMAX )
      COMPLEX            A( NMAX, NMAX ), B( NMAX, NMAX ),
     $                   C( NMAX, NMAX ), WORK( LW ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_CTREXC, AB_CTRSEN, AB_CTRSNA, AB_
     $CTRSYL
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      OK = .TRUE.
      NT = 0
*
*     Initialize A, B and SEL
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = ZERO
            B( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, NMAX
         A( I, I ) = ONE
         SEL( I ) = .TRUE.
   30 CONTINUE
*
*     Test AB_CTRSYL
*
      SRNAMT = 'AB_CTRSYL'
      INFOT = 1
      CALL AB_CTRSYL( 'X', 'N', 1, 0, 0, A, 1, B, 1, C, 1, SCALE, INFO )
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTRSYL( 'N', 'X', 1, 0, 0, A, 1, B, 1, C, 1, SCALE, INFO )
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CTRSYL( 'N', 'N', 0, 0, 0, A, 1, B, 1, C, 1, SCALE, INFO )
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CTRSYL( 'N', 'N', 1, -1, 0, A, 1, B, 1, C, 1, SCALE, INFO 
     $)
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CTRSYL( 'N', 'N', 1, 0, -1, A, 1, B, 1, C, 1, SCALE, INFO 
     $)
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CTRSYL( 'N', 'N', 1, 2, 0, A, 1, B, 1, C, 2, SCALE, INFO )
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_CTRSYL( 'N', 'N', 1, 0, 2, A, 1, B, 1, C, 1, SCALE, INFO )
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_CTRSYL( 'N', 'N', 1, 2, 0, A, 2, B, 1, C, 1, SCALE, INFO )
      CALL AB_CHKXER( 'AB_CTRSYL', INFOT, NOUT, LERR, OK )
      NT = NT + 8
*
*     Test AB_CTREXC
*
      SRNAMT = 'AB_CTREXC'
      IFST = 1
      ILST = 1
      INFOT = 1
      CALL AB_CTREXC( 'X', 1, A, 1, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTREXC( 'N', -1, A, 1, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      INFOT = 4
      ILST = 2
      CALL AB_CTREXC( 'N', 2, A, 1, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_CTREXC( 'V', 2, A, 2, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      INFOT = 7
      IFST = 0
      ILST = 1
      CALL AB_CTREXC( 'V', 1, A, 1, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      INFOT = 7
      IFST = 2
      CALL AB_CTREXC( 'V', 1, A, 1, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      INFOT = 8
      IFST = 1
      ILST = 0
      CALL AB_CTREXC( 'V', 1, A, 1, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      INFOT = 8
      ILST = 2
      CALL AB_CTREXC( 'V', 1, A, 1, B, 1, IFST, ILST, INFO )
      CALL AB_CHKXER( 'AB_CTREXC', INFOT, NOUT, LERR, OK )
      NT = NT + 8
*
*     Test AB_CTRSNA
*
      SRNAMT = 'AB_CTRSNA'
      INFOT = 1
      CALL AB_CTRSNA( 'X', 'A', SEL, 0, A, 1, B, 1, C, 1, S, SEP, 1, M,
     $             WORK, 1, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTRSNA( 'B', 'X', SEL, 0, A, 1, B, 1, C, 1, S, SEP, 1, M,
     $             WORK, 1, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CTRSNA( 'B', 'A', SEL, -1, A, 1, B, 1, C, 1, S, SEP, 1, M,
     $             WORK, 1, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_CTRSNA( 'V', 'A', SEL, 2, A, 1, B, 1, C, 1, S, SEP, 2, M,
     $             WORK, 2, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CTRSNA( 'B', 'A', SEL, 2, A, 2, B, 1, C, 2, S, SEP, 2, M,
     $             WORK, 2, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CTRSNA( 'B', 'A', SEL, 2, A, 2, B, 2, C, 1, S, SEP, 2, M,
     $             WORK, 2, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 13
      CALL AB_CTRSNA( 'B', 'A', SEL, 1, A, 1, B, 1, C, 1, S, SEP, 0, M,
     $             WORK, 1, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 13
      CALL AB_CTRSNA( 'B', 'S', SEL, 2, A, 2, B, 2, C, 2, S, SEP, 1, M,
     $             WORK, 1, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      INFOT = 16
      CALL AB_CTRSNA( 'B', 'A', SEL, 2, A, 2, B, 2, C, 2, S, SEP, 2, M,
     $             WORK, 1, RW, INFO )
      CALL AB_CHKXER( 'AB_CTRSNA', INFOT, NOUT, LERR, OK )
      NT = NT + 9
*
*     Test AB_CTRSEN
*
      SEL( 1 ) = .FALSE.
      SRNAMT = 'AB_CTRSEN'
      INFOT = 1
      CALL AB_CTRSEN( 'X', 'N', SEL, 0, A, 1, B, 1, X, M, S( 1 ), SEP( 1
     $ ),
     $             WORK, 1, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTRSEN( 'N', 'X', SEL, 0, A, 1, B, 1, X, M, S( 1 ), SEP( 1
     $ ),
     $             WORK, 1, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CTRSEN( 'N', 'N', SEL, -1, A, 1, B, 1, X, M, S( 1 ),
     $             SEP( 1 ), WORK, 1, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_CTRSEN( 'N', 'N', SEL, 2, A, 1, B, 1, X, M, S( 1 ), SEP( 1
     $ ),
     $             WORK, 2, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CTRSEN( 'N', 'V', SEL, 2, A, 2, B, 1, X, M, S( 1 ), SEP( 1
     $ ),
     $             WORK, 1, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      INFOT = 14
      CALL AB_CTRSEN( 'N', 'V', SEL, 2, A, 2, B, 2, X, M, S( 1 ), SEP( 1
     $ ),
     $             WORK, 0, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      INFOT = 14
      CALL AB_CTRSEN( 'E', 'V', SEL, 3, A, 3, B, 3, X, M, S( 1 ), SEP( 1
     $ ),
     $             WORK, 1, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      INFOT = 14
      CALL AB_CTRSEN( 'V', 'V', SEL, 3, A, 3, B, 3, X, M, S( 1 ), SEP( 1
     $ ),
     $             WORK, 3, INFO )
      CALL AB_CHKXER( 'AB_CTRSEN', INFOT, NOUT, LERR, OK )
      NT = NT + 8
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH, NT
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits (',
     $      I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
      RETURN
*
*     End of AB_CERREC
*
      END
