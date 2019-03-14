*> \brief \b AB_CERRGT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRGT( PATH, NUNIT )
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
*> AB_CERRGT tests the error exits for the COMPLEX tridiagonal
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CERRGT( PATH, NUNIT )
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
      REAL               ANORM, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               D( NMAX ), DF( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RW( NMAX )
      COMPLEX            B( NMAX ), DL( NMAX ), DLF( NMAX ), DU( NMAX ),
     $                   DU2( NMAX ), DUF( NMAX ), E( NMAX ),
     $                   EF( NMAX ), W( NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CGTCON, AB_CGTRFS, AB_CGTTRF, AB_
     $CGTTRS, AB_CHKXER,
     $                   AB_CPTCON, AB_CPTRFS, AB_CPTTRF, AB_CPTTRS
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
         D( I ) = 1.
         E( I ) = 2.
         DL( I ) = 3.
         DU( I ) = 4.
   10 CONTINUE
      ANORM = 1.0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        Test error exits for the general tridiagonal routines.
*
*        AB_CGTTRF
*
         SRNAMT = 'AB_CGTTRF'
         INFOT = 1
         CALL AB_CGTTRF( -1, DL, E, DU, DU2, IP, INFO )
         CALL AB_CHKXER( 'AB_CGTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CGTTRS
*
         SRNAMT = 'AB_CGTTRS'
         INFOT = 1
         CALL AB_CGTTRS( '/', 0, 0, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGTTRS( 'N', -1, 0, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGTTRS( 'N', 0, -1, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CGTTRS( 'N', 2, 1, DL, E, DU, DU2, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CGTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CGTRFS
*
         SRNAMT = 'AB_CGTRFS'
         INFOT = 1
         CALL AB_CGTRFS( '/', 0, 0, DL, E, DU, DLF, EF, DUF, DU2, IP, B,
     $ 1,
     $                X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGTRFS( 'N', -1, 0, DL, E, DU, DLF, EF, DUF, DU2, IP, B
     $,
     $                1, X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGTRFS( 'N', 0, -1, DL, E, DU, DLF, EF, DUF, DU2, IP, B
     $,
     $                1, X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CGTRFS( 'N', 2, 1, DL, E, DU, DLF, EF, DUF, DU2, IP, B,
     $ 1,
     $                X, 2, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_CGTRFS( 'N', 2, 1, DL, E, DU, DLF, EF, DUF, DU2, IP, B,
     $ 2,
     $                X, 1, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CGTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CGTCON
*
         SRNAMT = 'AB_CGTCON'
         INFOT = 1
         CALL AB_CGTCON( '/', 0, DL, E, DU, DU2, IP, ANORM, RCOND, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGTCON( 'I', -1, DL, E, DU, DU2, IP, ANORM, RCOND, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CGTCON( 'I', 0, DL, E, DU, DU2, IP, -ANORM, RCOND, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGTCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        Test error exits for the positive definite tridiagonal
*        routines.
*
*        AB_CPTTRF
*
         SRNAMT = 'AB_CPTTRF'
         INFOT = 1
         CALL AB_CPTTRF( -1, D, E, INFO )
         CALL AB_CHKXER( 'AB_CPTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CPTTRS
*
         SRNAMT = 'AB_CPTTRS'
         INFOT = 1
         CALL AB_CPTTRS( '/', 1, 0, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPTTRS( 'U', -1, 0, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPTTRS( 'U', 0, -1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CPTTRS( 'U', 2, 1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CPTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CPTRFS
*
         SRNAMT = 'AB_CPTRFS'
         INFOT = 1
         CALL AB_CPTRFS( '/', 1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPTRFS( 'U', -1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W
     $,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPTRFS( 'U', 0, -1, D, E, DF, EF, B, 1, X, 1, R1, R2, W
     $,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CPTRFS( 'U', 2, 1, D, E, DF, EF, B, 1, X, 2, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CPTRFS( 'U', 2, 1, D, E, DF, EF, B, 2, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CPTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CPTCON
*
         SRNAMT = 'AB_CPTCON'
         INFOT = 1
         CALL AB_CPTCON( -1, D, E, ANORM, RCOND, RW, INFO )
         CALL AB_CHKXER( 'AB_CPTCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPTCON( 0, D, E, -ANORM, RCOND, RW, INFO )
         CALL AB_CHKXER( 'AB_CPTCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRGT
*
      END
