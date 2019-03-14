*> \brief \b AB_ZERRGT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRGT( PATH, NUNIT )
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
*> AB_ZERRGT tests the error exits for the COMPLEX*16 tridiagonal
*> routines.
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
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE AB_ZERRGT( PATH, NUNIT )
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
      INTEGER            NMAX
      PARAMETER          ( NMAX = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO
      DOUBLE PRECISION   ANORM, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   D( NMAX ), DF( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RW( NMAX )
      COMPLEX*16         B( NMAX ), DL( NMAX ), DLF( NMAX ), DU( NMAX ),
     $                   DU2( NMAX ), DUF( NMAX ), E( NMAX ),
     $                   EF( NMAX ), W( NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZGTCON, AB_ZGTRFS, AB_
     $ZGTTRF, AB_ZGTTRS,
     $                   AB_ZPTCON, AB_ZPTRFS, AB_ZPTTRF, AB_ZPTTRS
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
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
      DO 10 I = 1, NMAX
         D( I ) = 1.D0
         E( I ) = 2.D0
         DL( I ) = 3.D0
         DU( I ) = 4.D0
   10 CONTINUE
      ANORM = 1.0D0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        Test error exits for the general tridiagonal routines.
*
*        AB_ZGTTRF
*
         SRNAMT = 'AB_ZGTTRF'
         INFOT = 1
         CALL AB_ZGTTRF( -1, DL, E, DU, DU2, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZGTTRS
*
         SRNAMT = 'AB_ZGTTRS'
         INFOT = 1
         CALL AB_ZGTTRS( '/', 0, 0, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGTTRS( 'N', -1, 0, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGTTRS( 'N', 0, -1, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGTTRS( 'N', 2, 1, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZGTRFS
*
         SRNAMT = 'AB_ZGTRFS'
         INFOT = 1
         CALL AB_ZGTRFS( '/', 0, 0, DL, E, DU, DLF, EF, DUF, DU2, IP, B,
     $ 1,
     $                X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGTRFS( 'N', -1, 0, DL, E, DU, DLF, EF, DUF, DU2, IP, B
     $,
     $                1, X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGTRFS( 'N', 0, -1, DL, E, DU, DLF, EF, DUF, DU2, IP, B
     $,
     $                1, X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZGTRFS( 'N', 2, 1, DL, E, DU, DLF, EF, DUF, DU2, IP, B,
     $ 1,
     $                X, 2, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_ZGTRFS( 'N', 2, 1, DL, E, DU, DLF, EF, DUF, DU2, IP, B,
     $ 2,
     $                X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZGTCON
*
         SRNAMT = 'AB_ZGTCON'
         INFOT = 1
         CALL AB_ZGTCON( '/', 0, DL, E, DU, DU2, IP, ANORM, RCOND, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGTCON( 'I', -1, DL, E, DU, DU2, IP, ANORM, RCOND, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGTCON( 'I', 0, DL, E, DU, DU2, IP, -ANORM, RCOND, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGTCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        Test error exits for the positive definite tridiagonal
*        routines.
*
*        AB_ZPTTRF
*
         SRNAMT = 'AB_ZPTTRF'
         INFOT = 1
         CALL AB_ZPTTRF( -1, D, E, INFO )
         CALL AB_CHKXER( 'AB_ZPTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZPTTRS
*
         SRNAMT = 'AB_ZPTTRS'
         INFOT = 1
         CALL AB_ZPTTRS( '/', 1, 0, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPTTRS( 'U', -1, 0, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPTTRS( 'U', 0, -1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZPTTRS( 'U', 2, 1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZPTRFS
*
         SRNAMT = 'AB_ZPTRFS'
         INFOT = 1
         CALL AB_ZPTRFS( '/', 1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPTRFS( 'U', -1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W
     $,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPTRFS( 'U', 0, -1, D, E, DF, EF, B, 1, X, 1, R1, R2, W
     $,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZPTRFS( 'U', 2, 1, D, E, DF, EF, B, 1, X, 2, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZPTRFS( 'U', 2, 1, D, E, DF, EF, B, 2, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZPTCON
*
         SRNAMT = 'AB_ZPTCON'
         INFOT = 1
         CALL AB_ZPTCON( -1, D, E, ANORM, RCOND, RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPTCON( 0, D, E, -ANORM, RCOND, RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRGT
*
      END
