*> \brief \b AB_ZERRTR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRTR( PATH, NUNIT )
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
*> AB_ZERRTR tests the error exits for the COMPLEX*16 triangular routines.
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
      SUBROUTINE AB_ZERRTR( PATH, NUNIT )
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
      DOUBLE PRECISION   RCOND, SCALE
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   R1( NMAX ), R2( NMAX ), RW( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), B( NMAX ), W( NMAX ),
     $                   X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZLATBS, AB_ZLATPS, AB_
     $ZLATRS, AB_ZTBCON,
     $                   AB_ZTBRFS, AB_ZTBTRS, AB_ZTPCON, AB_ZTPRFS, AB_
     $ZTPTRI, AB_ZTPTRS,
     $                   AB_ZTRCON, AB_ZTRRFS, AB_ZTRTI2, AB_ZTRTRI, AB_
     $ZTRTRS
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
      A( 1, 1 ) = 1.D0
      A( 1, 2 ) = 2.D0
      A( 2, 2 ) = 3.D0
      A( 2, 1 ) = 4.D0
      OK = .TRUE.
*
*     Test error exits for the general triangular routines.
*
      IF( AB_AB_LSAMEN( 2, C2, 'TR' ) ) THEN
*
*        AB_ZTRTRI
*
         SRNAMT = 'AB_ZTRTRI'
         INFOT = 1
         CALL AB_ZTRTRI( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTRTRI( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTRTRI( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTRTRI( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRI', INFOT, NOUT, LERR, OK )
*
*        AB_ZTRTI2
*
         SRNAMT = 'AB_ZTRTI2'
         INFOT = 1
         CALL AB_ZTRTI2( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTRTI2( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTRTI2( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTRTI2( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTI2', INFOT, NOUT, LERR, OK )
*
*
*        AB_ZTRTRS
*
         SRNAMT = 'AB_ZTRTRS'
         INFOT = 1
         CALL AB_ZTRTRS( '/', 'N', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTRTRS( 'U', '/', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTRTRS( 'U', 'N', '/', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTRTRS( 'U', 'N', 'N', -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTRTRS( 'U', 'N', 'N', 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
*
*        AB_ZTRRFS
*
         SRNAMT = 'AB_ZTRRFS'
         INFOT = 1
         CALL AB_ZTRRFS( '/', 'N', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTRRFS( 'U', '/', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTRRFS( 'U', 'N', '/', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTRRFS( 'U', 'N', 'N', -1, 0, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTRRFS( 'U', 'N', 'N', 0, -1, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZTRRFS( 'U', 'N', 'N', 2, 1, A, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZTRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 1, X, 2, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZTRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 2, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZTRCON
*
         SRNAMT = 'AB_ZTRCON'
         INFOT = 1
         CALL AB_ZTRCON( '/', 'U', 'N', 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTRCON( '1', '/', 'N', 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTRCON( '1', 'U', '/', 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTRCON( '1', 'U', 'N', -1, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZTRCON( '1', 'U', 'N', 2, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTRCON', INFOT, NOUT, LERR, OK )
*
*        AB_ZLATRS
*
         SRNAMT = 'AB_ZLATRS'
         INFOT = 1
         CALL AB_ZLATRS( '/', 'N', 'N', 'N', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZLATRS( 'U', '/', 'N', 'N', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZLATRS( 'U', 'N', '/', 'N', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZLATRS( 'U', 'N', 'N', '/', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZLATRS( 'U', 'N', 'N', 'N', -1, A, 1, X, SCALE, RW, INF
     $O )
         CALL AB_CHKXER( 'AB_ZLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZLATRS( 'U', 'N', 'N', 'N', 2, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZLATRS', INFOT, NOUT, LERR, OK )
*
*     Test error exits for the packed triangular routines.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        AB_ZTPTRI
*
         SRNAMT = 'AB_ZTPTRI'
         INFOT = 1
         CALL AB_ZTPTRI( '/', 'N', 0, A, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTPTRI( 'U', '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTPTRI( 'U', 'N', -1, A, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_ZTPTRS
*
         SRNAMT = 'AB_ZTPTRS'
         INFOT = 1
         CALL AB_ZTPTRS( '/', 'N', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTPTRS( 'U', '/', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTPTRS( 'U', 'N', '/', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTPTRS( 'U', 'N', 'N', -1, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTPTRS( 'U', 'N', 'N', 0, -1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZTPTRS( 'U', 'N', 'N', 2, 1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZTPRFS
*
         SRNAMT = 'AB_ZTPRFS'
         INFOT = 1
         CALL AB_ZTPRFS( '/', 'N', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTPRFS( 'U', '/', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTPRFS( 'U', 'N', '/', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTPRFS( 'U', 'N', 'N', -1, 0, A, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTPRFS( 'U', 'N', 'N', 0, -1, A, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZTPRFS( 'U', 'N', 'N', 2, 1, A, B, 1, X, 2, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZTPRFS( 'U', 'N', 'N', 2, 1, A, B, 2, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZTPCON
*
         SRNAMT = 'AB_ZTPCON'
         INFOT = 1
         CALL AB_ZTPCON( '/', 'U', 'N', 0, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTPCON( '1', '/', 'N', 0, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTPCON( '1', 'U', '/', 0, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTPCON( '1', 'U', 'N', -1, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTPCON', INFOT, NOUT, LERR, OK )
*
*        AB_ZLATPS
*
         SRNAMT = 'AB_ZLATPS'
         INFOT = 1
         CALL AB_ZLATPS( '/', 'N', 'N', 'N', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_ZLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZLATPS( 'U', '/', 'N', 'N', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_ZLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZLATPS( 'U', 'N', '/', 'N', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_ZLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZLATPS( 'U', 'N', 'N', '/', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_ZLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZLATPS( 'U', 'N', 'N', 'N', -1, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_ZLATPS', INFOT, NOUT, LERR, OK )
*
*     Test error exits for the banded triangular routines.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        AB_ZTBTRS
*
         SRNAMT = 'AB_ZTBTRS'
         INFOT = 1
         CALL AB_ZTBTRS( '/', 'N', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTBTRS( 'U', '/', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTBTRS( 'U', 'N', '/', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTBTRS( 'U', 'N', 'N', -1, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTBTRS( 'U', 'N', 'N', 0, -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZTBTRS( 'U', 'N', 'N', 0, 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZTBTRS( 'U', 'N', 'N', 2, 1, 1, A, 1, X, 2, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZTBTRS( 'U', 'N', 'N', 2, 0, 1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_ZTBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZTBRFS
*
         SRNAMT = 'AB_ZTBRFS'
         INFOT = 1
         CALL AB_ZTBRFS( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTBRFS( 'U', '/', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTBRFS( 'U', 'N', '/', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTBRFS( 'U', 'N', 'N', -1, 0, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTBRFS( 'U', 'N', 'N', 0, -1, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZTBRFS( 'U', 'N', 'N', 0, 0, -1, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 1, B, 2, X, 2, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 1, X, 2, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 2, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZTBCON
*
         SRNAMT = 'AB_ZTBCON'
         INFOT = 1
         CALL AB_ZTBCON( '/', 'U', 'N', 0, 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTBCON( '1', '/', 'N', 0, 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTBCON( '1', 'U', '/', 0, 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTBCON( '1', 'U', 'N', -1, 0, A, 1, RCOND, W, RW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTBCON( '1', 'U', 'N', 0, -1, A, 1, RCOND, W, RW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZTBCON( '1', 'U', 'N', 2, 1, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTBCON', INFOT, NOUT, LERR, OK )
*
*        AB_ZLATBS
*
         SRNAMT = 'AB_ZLATBS'
         INFOT = 1
         CALL AB_ZLATBS( '/', 'N', 'N', 'N', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZLATBS( 'U', '/', 'N', 'N', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZLATBS( 'U', 'N', '/', 'N', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZLATBS( 'U', 'N', 'N', '/', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZLATBS( 'U', 'N', 'N', 'N', -1, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZLATBS( 'U', 'N', 'N', 'N', 1, -1, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZLATBS( 'U', 'N', 'N', 'N', 2, 1, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZLATBS', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRTR
*
      END
