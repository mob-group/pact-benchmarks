*> \brief \b AB_CERRTR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRTR( PATH, NUNIT )
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
*> AB_CERRTR tests the error exits for the COMPLEX triangular routines.
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
      SUBROUTINE AB_CERRTR( PATH, NUNIT )
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
      REAL               RCOND, SCALE
*     ..
*     .. Local Arrays ..
      REAL               R1( NMAX ), R2( NMAX ), RW( NMAX )
      COMPLEX            A( NMAX, NMAX ), B( NMAX ), W( NMAX ),
     $                   X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_CLATBS, AB_CLATPS, AB_
     $CLATRS, AB_CTBCON,
     $                   AB_CTBRFS, AB_CTBTRS, AB_CTPCON, AB_CTPRFS, AB_
     $CTPTRI, AB_CTPTRS,
     $                   AB_CTRCON, AB_CTRRFS, AB_CTRTI2, AB_CTRTRI, AB_
     $CTRTRS
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
      A( 1, 1 ) = 1.
      A( 1, 2 ) = 2.
      A( 2, 2 ) = 3.
      A( 2, 1 ) = 4.
      OK = .TRUE.
*
*     Test error exits for the general triangular routines.
*
      IF( AB_AB_LSAMEN( 2, C2, 'TR' ) ) THEN
*
*        AB_CTRTRI
*
         SRNAMT = 'AB_CTRTRI'
         INFOT = 1
         CALL AB_CTRTRI( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTRTRI( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTRTRI( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTRTRI( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRI', INFOT, NOUT, LERR, OK )
*
*        AB_CTRTI2
*
         SRNAMT = 'AB_CTRTI2'
         INFOT = 1
         CALL AB_CTRTI2( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTRTI2( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTRTI2( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTRTI2( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTI2', INFOT, NOUT, LERR, OK )
*
*
*        AB_CTRTRS
*
         SRNAMT = 'AB_CTRTRS'
         INFOT = 1
         CALL AB_CTRTRS( '/', 'N', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTRTRS( 'U', '/', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTRTRS( 'U', 'N', '/', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTRTRS( 'U', 'N', 'N', -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTRTRS( 'U', 'N', 'N', 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
*
*        AB_CTRRFS
*
         SRNAMT = 'AB_CTRRFS'
         INFOT = 1
         CALL AB_CTRRFS( '/', 'N', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTRRFS( 'U', '/', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTRRFS( 'U', 'N', '/', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTRRFS( 'U', 'N', 'N', -1, 0, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTRRFS( 'U', 'N', 'N', 0, -1, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CTRRFS( 'U', 'N', 'N', 2, 1, A, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CTRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 1, X, 2, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CTRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 2, X, 1, R1, R2, 
     $W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTRRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CTRCON
*
         SRNAMT = 'AB_CTRCON'
         INFOT = 1
         CALL AB_CTRCON( '/', 'U', 'N', 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTRCON( '1', '/', 'N', 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTRCON( '1', 'U', '/', 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTRCON( '1', 'U', 'N', -1, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CTRCON( '1', 'U', 'N', 2, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTRCON', INFOT, NOUT, LERR, OK )
*
*        AB_CLATRS
*
         SRNAMT = 'AB_CLATRS'
         INFOT = 1
         CALL AB_CLATRS( '/', 'N', 'N', 'N', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_CLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CLATRS( 'U', '/', 'N', 'N', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_CLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CLATRS( 'U', 'N', '/', 'N', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_CLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CLATRS( 'U', 'N', 'N', '/', 0, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_CLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CLATRS( 'U', 'N', 'N', 'N', -1, A, 1, X, SCALE, RW, INF
     $O )
         CALL AB_CHKXER( 'AB_CLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CLATRS( 'U', 'N', 'N', 'N', 2, A, 1, X, SCALE, RW, INFO
     $ )
         CALL AB_CHKXER( 'AB_CLATRS', INFOT, NOUT, LERR, OK )
*
*     Test error exits for the packed triangular routines.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        AB_CTPTRI
*
         SRNAMT = 'AB_CTPTRI'
         INFOT = 1
         CALL AB_CTPTRI( '/', 'N', 0, A, INFO )
         CALL AB_CHKXER( 'AB_CTPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTPTRI( 'U', '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_CTPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTPTRI( 'U', 'N', -1, A, INFO )
         CALL AB_CHKXER( 'AB_CTPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_CTPTRS
*
         SRNAMT = 'AB_CTPTRS'
         INFOT = 1
         CALL AB_CTPTRS( '/', 'N', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTPTRS( 'U', '/', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTPTRS( 'U', 'N', '/', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTPTRS( 'U', 'N', 'N', -1, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTPTRS( 'U', 'N', 'N', 0, -1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CTPTRS( 'U', 'N', 'N', 2, 1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CTPRFS
*
         SRNAMT = 'AB_CTPRFS'
         INFOT = 1
         CALL AB_CTPRFS( '/', 'N', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTPRFS( 'U', '/', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTPRFS( 'U', 'N', '/', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTPRFS( 'U', 'N', 'N', -1, 0, A, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTPRFS( 'U', 'N', 'N', 0, -1, A, B, 1, X, 1, R1, R2, W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CTPRFS( 'U', 'N', 'N', 2, 1, A, B, 1, X, 2, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CTPRFS( 'U', 'N', 'N', 2, 1, A, B, 2, X, 1, R1, R2, W, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CTPCON
*
         SRNAMT = 'AB_CTPCON'
         INFOT = 1
         CALL AB_CTPCON( '/', 'U', 'N', 0, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTPCON( '1', '/', 'N', 0, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTPCON( '1', 'U', '/', 0, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTPCON( '1', 'U', 'N', -1, A, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTPCON', INFOT, NOUT, LERR, OK )
*
*        AB_CLATPS
*
         SRNAMT = 'AB_CLATPS'
         INFOT = 1
         CALL AB_CLATPS( '/', 'N', 'N', 'N', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_CLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CLATPS( 'U', '/', 'N', 'N', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_CLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CLATPS( 'U', 'N', '/', 'N', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_CLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CLATPS( 'U', 'N', 'N', '/', 0, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_CLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CLATPS( 'U', 'N', 'N', 'N', -1, A, X, SCALE, RW, INFO )
         CALL AB_CHKXER( 'AB_CLATPS', INFOT, NOUT, LERR, OK )
*
*     Test error exits for the banded triangular routines.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        AB_CTBTRS
*
         SRNAMT = 'AB_CTBTRS'
         INFOT = 1
         CALL AB_CTBTRS( '/', 'N', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTBTRS( 'U', '/', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTBTRS( 'U', 'N', '/', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTBTRS( 'U', 'N', 'N', -1, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTBTRS( 'U', 'N', 'N', 0, -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CTBTRS( 'U', 'N', 'N', 0, 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CTBTRS( 'U', 'N', 'N', 2, 1, 1, A, 1, X, 2, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CTBTRS( 'U', 'N', 'N', 2, 0, 1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_CTBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CTBRFS
*
         SRNAMT = 'AB_CTBRFS'
         INFOT = 1
         CALL AB_CTBRFS( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTBRFS( 'U', '/', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTBRFS( 'U', 'N', '/', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTBRFS( 'U', 'N', 'N', -1, 0, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTBRFS( 'U', 'N', 'N', 0, -1, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CTBRFS( 'U', 'N', 'N', 0, 0, -1, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 1, B, 2, X, 2, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 1, X, 2, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 2, X, 1, R1, R
     $2,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CTBCON
*
         SRNAMT = 'AB_CTBCON'
         INFOT = 1
         CALL AB_CTBCON( '/', 'U', 'N', 0, 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTBCON( '1', '/', 'N', 0, 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTBCON( '1', 'U', '/', 0, 0, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTBCON( '1', 'U', 'N', -1, 0, A, 1, RCOND, W, RW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTBCON( '1', 'U', 'N', 0, -1, A, 1, RCOND, W, RW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CTBCON( '1', 'U', 'N', 2, 1, A, 1, RCOND, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTBCON', INFOT, NOUT, LERR, OK )
*
*        AB_CLATBS
*
         SRNAMT = 'AB_CLATBS'
         INFOT = 1
         CALL AB_CLATBS( '/', 'N', 'N', 'N', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CLATBS( 'U', '/', 'N', 'N', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CLATBS( 'U', 'N', '/', 'N', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CLATBS( 'U', 'N', 'N', '/', 0, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CLATBS( 'U', 'N', 'N', 'N', -1, 0, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CLATBS( 'U', 'N', 'N', 'N', 1, -1, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CLATBS( 'U', 'N', 'N', 'N', 2, 1, A, 1, X, SCALE, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CLATBS', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRTR
*
      END
