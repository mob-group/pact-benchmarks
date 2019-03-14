*> \brief \b AB_DERRGT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRGT( PATH, NUNIT )
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
*> AB_DERRGT tests the error exits for the DOUBLE PRECISION tridiagonal
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DERRGT( PATH, NUNIT )
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
      INTEGER            INFO
      DOUBLE PRECISION   ANORM, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX )
      DOUBLE PRECISION   B( NMAX ), C( NMAX ), CF( NMAX ), D( NMAX ),
     $                   DF( NMAX ), E( NMAX ), EF( NMAX ), F( NMAX ),
     $                   R1( NMAX ), R2( NMAX ), W( NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DGTCON, AB_DGTRFS, AB_
     $DGTTRF, AB_DGTTRS,
     $                   AB_DPTCON, AB_DPTRFS, AB_DPTTRF, AB_DPTTRS
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
      D( 1 ) = 1.D0
      D( 2 ) = 2.D0
      DF( 1 ) = 1.D0
      DF( 2 ) = 2.D0
      E( 1 ) = 3.D0
      E( 2 ) = 4.D0
      EF( 1 ) = 3.D0
      EF( 2 ) = 4.D0
      ANORM = 1.0D0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        Test error exits for the general tridiagonal routines.
*
*        AB_DGTTRF
*
         SRNAMT = 'AB_DGTTRF'
         INFOT = 1
         CALL AB_DGTTRF( -1, C, D, E, F, IP, INFO )
         CALL AB_CHKXER( 'AB_DGTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DGTTRS
*
         SRNAMT = 'AB_DGTTRS'
         INFOT = 1
         CALL AB_DGTTRS( '/', 0, 0, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGTTRS( 'N', -1, 0, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGTTRS( 'N', 0, -1, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DGTTRS( 'N', 2, 1, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DGTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DGTRFS
*
         SRNAMT = 'AB_DGTRFS'
         INFOT = 1
         CALL AB_DGTRFS( '/', 0, 0, C, D, E, CF, DF, EF, F, IP, B, 1, X,
     $ 1,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGTRFS( 'N', -1, 0, C, D, E, CF, DF, EF, F, IP, B, 1, X
     $,
     $                1, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGTRFS( 'N', 0, -1, C, D, E, CF, DF, EF, F, IP, B, 1, X
     $,
     $                1, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DGTRFS( 'N', 2, 1, C, D, E, CF, DF, EF, F, IP, B, 1, X,
     $ 2,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_DGTRFS( 'N', 2, 1, C, D, E, CF, DF, EF, F, IP, B, 2, X,
     $ 1,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DGTCON
*
         SRNAMT = 'AB_DGTCON'
         INFOT = 1
         CALL AB_DGTCON( '/', 0, C, D, E, F, IP, ANORM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGTCON( 'I', -1, C, D, E, F, IP, ANORM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DGTCON( 'I', 0, C, D, E, F, IP, -ANORM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGTCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        Test error exits for the positive definite tridiagonal
*        routines.
*
*        AB_DPTTRF
*
         SRNAMT = 'AB_DPTTRF'
         INFOT = 1
         CALL AB_DPTTRF( -1, D, E, INFO )
         CALL AB_CHKXER( 'AB_DPTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DPTTRS
*
         SRNAMT = 'AB_DPTTRS'
         INFOT = 1
         CALL AB_DPTTRS( -1, 0, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPTTRS( 0, -1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPTTRS( 2, 1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DPTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DPTRFS
*
         SRNAMT = 'AB_DPTRFS'
         INFOT = 1
         CALL AB_DPTRFS( -1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W, INF
     $O )
         CALL AB_CHKXER( 'AB_DPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPTRFS( 0, -1, D, E, DF, EF, B, 1, X, 1, R1, R2, W, INF
     $O )
         CALL AB_CHKXER( 'AB_DPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DPTRFS( 2, 1, D, E, DF, EF, B, 1, X, 2, R1, R2, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_DPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DPTRFS( 2, 1, D, E, DF, EF, B, 2, X, 1, R1, R2, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_DPTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DPTCON
*
         SRNAMT = 'AB_DPTCON'
         INFOT = 1
         CALL AB_DPTCON( -1, D, E, ANORM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_DPTCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPTCON( 0, D, E, -ANORM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_DPTCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRGT
*
      END
