*> \brief \b AB_SERRGT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRGT( PATH, NUNIT )
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
*> AB_SERRGT tests the error exits for the REAL tridiagonal
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SERRGT( PATH, NUNIT )
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
      REAL               ANORM, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX )
      REAL               B( NMAX ), C( NMAX ), CF( NMAX ), D( NMAX ),
     $                   DF( NMAX ), E( NMAX ), EF( NMAX ), F( NMAX ),
     $                   R1( NMAX ), R2( NMAX ), W( NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_SGTCON, AB_SGTRFS, AB_
     $SGTTRF, AB_SGTTRS,
     $                   AB_SPTCON, AB_SPTRFS, AB_SPTTRF, AB_SPTTRS
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
      D( 1 ) = 1.
      D( 2 ) = 2.
      DF( 1 ) = 1.
      DF( 2 ) = 2.
      E( 1 ) = 3.
      E( 2 ) = 4.
      EF( 1 ) = 3.
      EF( 2 ) = 4.
      ANORM = 1.0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        Test error exits for the general tridiagonal routines.
*
*        AB_SGTTRF
*
         SRNAMT = 'AB_SGTTRF'
         INFOT = 1
         CALL AB_SGTTRF( -1, C, D, E, F, IP, INFO )
         CALL AB_CHKXER( 'AB_SGTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SGTTRS
*
         SRNAMT = 'AB_SGTTRS'
         INFOT = 1
         CALL AB_SGTTRS( '/', 0, 0, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_SGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGTTRS( 'N', -1, 0, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_SGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGTTRS( 'N', 0, -1, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_SGTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SGTTRS( 'N', 2, 1, C, D, E, F, IP, X, 1, INFO )
         CALL AB_CHKXER( 'AB_SGTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_SGTRFS
*
         SRNAMT = 'AB_SGTRFS'
         INFOT = 1
         CALL AB_SGTRFS( '/', 0, 0, C, D, E, CF, DF, EF, F, IP, B, 1, X,
     $ 1,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGTRFS( 'N', -1, 0, C, D, E, CF, DF, EF, F, IP, B, 1, X
     $,
     $                1, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGTRFS( 'N', 0, -1, C, D, E, CF, DF, EF, F, IP, B, 1, X
     $,
     $                1, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_SGTRFS( 'N', 2, 1, C, D, E, CF, DF, EF, F, IP, B, 1, X,
     $ 2,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_SGTRFS( 'N', 2, 1, C, D, E, CF, DF, EF, F, IP, B, 2, X,
     $ 1,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_SGTCON
*
         SRNAMT = 'AB_SGTCON'
         INFOT = 1
         CALL AB_SGTCON( '/', 0, C, D, E, F, IP, ANORM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGTCON( 'I', -1, C, D, E, F, IP, ANORM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGTCON', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SGTCON( 'I', 0, C, D, E, F, IP, -ANORM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGTCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        Test error exits for the positive definite tridiagonal
*        routines.
*
*        AB_SPTTRF
*
         SRNAMT = 'AB_SPTTRF'
         INFOT = 1
         CALL AB_SPTTRF( -1, D, E, INFO )
         CALL AB_CHKXER( 'AB_SPTTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SPTTRS
*
         SRNAMT = 'AB_SPTTRS'
         INFOT = 1
         CALL AB_SPTTRS( -1, 0, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_SPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPTTRS( 0, -1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_SPTTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPTTRS( 2, 1, D, E, X, 1, INFO )
         CALL AB_CHKXER( 'AB_SPTTRS', INFOT, NOUT, LERR, OK )
*
*        AB_SPTRFS
*
         SRNAMT = 'AB_SPTRFS'
         INFOT = 1
         CALL AB_SPTRFS( -1, 0, D, E, DF, EF, B, 1, X, 1, R1, R2, W, INF
     $O )
         CALL AB_CHKXER( 'AB_SPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPTRFS( 0, -1, D, E, DF, EF, B, 1, X, 1, R1, R2, W, INF
     $O )
         CALL AB_CHKXER( 'AB_SPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SPTRFS( 2, 1, D, E, DF, EF, B, 1, X, 2, R1, R2, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_SPTRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SPTRFS( 2, 1, D, E, DF, EF, B, 2, X, 1, R1, R2, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_SPTRFS', INFOT, NOUT, LERR, OK )
*
*        AB_SPTCON
*
         SRNAMT = 'AB_SPTCON'
         INFOT = 1
         CALL AB_SPTCON( -1, D, E, ANORM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_SPTCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPTCON( 0, D, E, -ANORM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_SPTCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRGT
*
      END
