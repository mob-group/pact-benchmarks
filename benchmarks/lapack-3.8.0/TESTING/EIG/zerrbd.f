*> \brief \b AB_ZERRBD
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRBD( PATH, NUNIT )
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
*> AB_ZERRBD tests the error exits for AB_ZGEBRD, AB_ZUNGBR, AB_ZUNMBR, and AB_ZBDSQR.
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
*> \ingroup complex16_eig
*
*  =====================================================================
      SUBROUTINE AB_ZERRBD( PATH, NUNIT )
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
      PARAMETER          ( NMAX = 4, LW = NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, NT
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   D( NMAX ), E( NMAX ), RW( 4*NMAX )
      COMPLEX*16         A( NMAX, NMAX ), TP( NMAX ), TQ( NMAX ),
     $                   U( NMAX, NMAX ), V( NMAX, NMAX ), W( LW )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_ZBDSQR, AB_ZGEBRD, AB_ZUNGBR, AB_
     $ZUNMBR
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
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
   20 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits of the SVD routines.
*
      IF( AB_AB_LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        AB_ZGEBRD
*
         SRNAMT = 'AB_ZGEBRD'
         INFOT = 1
         CALL AB_ZGEBRD( -1, 0, A, 1, D, E, TQ, TP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGEBRD( 0, -1, A, 1, D, E, TQ, TP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGEBRD( 2, 1, A, 1, D, E, TQ, TP, W, 2, INFO )
         CALL AB_CHKXER( 'AB_ZGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGEBRD( 2, 1, A, 2, D, E, TQ, TP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGEBRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_ZUNGBR
*
         SRNAMT = 'AB_ZUNGBR'
         INFOT = 1
         CALL AB_ZUNGBR( '/', 0, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZUNGBR( 'Q', -1, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUNGBR( 'Q', 0, -1, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUNGBR( 'Q', 0, 1, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUNGBR( 'Q', 1, 0, 1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUNGBR( 'P', 1, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUNGBR( 'P', 0, 1, 1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZUNGBR( 'Q', 0, 0, -1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZUNGBR( 'Q', 2, 1, 1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZUNGBR( 'Q', 2, 2, 1, A, 2, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGBR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_ZUNMBR
*
         SRNAMT = 'AB_ZUNMBR'
         INFOT = 1
         CALL AB_ZUNMBR( '/', 'L', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZUNMBR( 'Q', '/', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUNMBR( 'Q', 'L', '/', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZUNMBR( 'Q', 'L', 'C', -1, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZUNMBR( 'Q', 'L', 'C', 0, -1, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZUNMBR( 'Q', 'L', 'C', 0, 0, -1, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZUNMBR( 'Q', 'L', 'C', 2, 0, 0, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZUNMBR( 'Q', 'R', 'C', 0, 2, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZUNMBR( 'P', 'L', 'C', 2, 0, 2, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZUNMBR( 'P', 'R', 'C', 0, 2, 2, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZUNMBR( 'Q', 'R', 'C', 2, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZUNMBR( 'Q', 'L', 'C', 0, 2, 0, A, 1, TQ, U, 1, W, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZUNMBR( 'Q', 'R', 'C', 2, 0, 0, A, 1, TQ, U, 2, W, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMBR', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_ZBDSQR
*
         SRNAMT = 'AB_ZBDSQR'
         INFOT = 1
         CALL AB_ZBDSQR( '/', 0, 0, 0, 0, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZBDSQR( 'U', -1, 0, 0, 0, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZBDSQR( 'U', 0, -1, 0, 0, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZBDSQR( 'U', 0, 0, -1, 0, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZBDSQR( 'U', 0, 0, 0, -1, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZBDSQR( 'U', 2, 1, 0, 0, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZBDSQR( 'U', 0, 0, 2, 0, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZBDSQR( 'U', 2, 0, 0, 1, D, E, V, 1, U, 1, A, 1, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZBDSQR', INFOT, NOUT, LERR, OK )
         NT = NT + 8
      END IF
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
     $        I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $        'exits ***' )
*
      RETURN
*
*     End of AB_ZERRBD
*
      END
