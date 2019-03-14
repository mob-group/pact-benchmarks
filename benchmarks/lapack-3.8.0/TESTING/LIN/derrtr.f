*> \brief \b AB_DERRTR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRTR( PATH, NUNIT )
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
*> AB_DERRTR tests the error exits for the DOUBLE PRECISION triangular
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
      SUBROUTINE AB_DERRTR( PATH, NUNIT )
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
      INTEGER            IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), B( NMAX ), R1( NMAX ),
     $                   R2( NMAX ), W( NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DLATBS, AB_DLATPS, AB_
     $DLATRS, AB_DTBCON,
     $                   AB_DTBRFS, AB_DTBTRS, AB_DTPCON, AB_DTPRFS, AB_
     $DTPTRI, AB_DTPTRS,
     $                   AB_DTRCON, AB_DTRRFS, AB_DTRTI2, AB_DTRTRI, AB_
     $DTRTRS
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
      IF( AB_AB_LSAMEN( 2, C2, 'TR' ) ) THEN
*
*        Test error exits for the general triangular routines.
*
*        AB_DTRTRI
*
         SRNAMT = 'AB_DTRTRI'
         INFOT = 1
         CALL AB_DTRTRI( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTRTRI( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTRTRI( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTRTRI( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRI', INFOT, NOUT, LERR, OK )
*
*        AB_DTRTI2
*
         SRNAMT = 'AB_DTRTI2'
         INFOT = 1
         CALL AB_DTRTI2( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTRTI2( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTRTI2( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTRTI2( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTI2', INFOT, NOUT, LERR, OK )
*
*        AB_DTRTRS
*
         SRNAMT = 'AB_DTRTRS'
         INFOT = 1
         CALL AB_DTRTRS( '/', 'N', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTRTRS( 'U', '/', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTRTRS( 'U', 'N', '/', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTRTRS( 'U', 'N', 'N', -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTRTRS( 'U', 'N', 'N', 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DTRTRS( 'U', 'N', 'N', 2, 1, A, 1, X, 2, INFO )
         CALL AB_CHKXER( 'AB_DTRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DTRTRS( 'U', 'N', 'N', 2, 1, A, 2, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTRTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DTRRFS
*
         SRNAMT = 'AB_DTRRFS'
         INFOT = 1
         CALL AB_DTRRFS( '/', 'N', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTRRFS( 'U', '/', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTRRFS( 'U', 'N', '/', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTRRFS( 'U', 'N', 'N', -1, 0, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTRRFS( 'U', 'N', 'N', 0, -1, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DTRRFS( 'U', 'N', 'N', 2, 1, A, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DTRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 1, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DTRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 2, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTRRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DTRCON
*
         SRNAMT = 'AB_DTRCON'
         INFOT = 1
         CALL AB_DTRCON( '/', 'U', 'N', 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTRCON( '1', '/', 'N', 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTRCON( '1', 'U', '/', 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTRCON( '1', 'U', 'N', -1, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTRCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DTRCON( '1', 'U', 'N', 2, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTRCON', INFOT, NOUT, LERR, OK )
*
*        AB_DLATRS
*
         SRNAMT = 'AB_DLATRS'
         INFOT = 1
         CALL AB_DLATRS( '/', 'N', 'N', 'N', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_DLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DLATRS( 'U', '/', 'N', 'N', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_DLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DLATRS( 'U', 'N', '/', 'N', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_DLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DLATRS( 'U', 'N', 'N', '/', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_DLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DLATRS( 'U', 'N', 'N', 'N', -1, A, 1, X, SCALE, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_DLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DLATRS( 'U', 'N', 'N', 'N', 2, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_DLATRS', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        Test error exits for the packed triangular routines.
*
*        AB_DTPTRI
*
         SRNAMT = 'AB_DTPTRI'
         INFOT = 1
         CALL AB_DTPTRI( '/', 'N', 0, A, INFO )
         CALL AB_CHKXER( 'AB_DTPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTPTRI( 'U', '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_DTPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTPTRI( 'U', 'N', -1, A, INFO )
         CALL AB_CHKXER( 'AB_DTPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_DTPTRS
*
         SRNAMT = 'AB_DTPTRS'
         INFOT = 1
         CALL AB_DTPTRS( '/', 'N', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTPTRS( 'U', '/', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTPTRS( 'U', 'N', '/', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTPTRS( 'U', 'N', 'N', -1, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTPTRS( 'U', 'N', 'N', 0, -1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DTPTRS( 'U', 'N', 'N', 2, 1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DTPRFS
*
         SRNAMT = 'AB_DTPRFS'
         INFOT = 1
         CALL AB_DTPRFS( '/', 'N', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTPRFS( 'U', '/', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTPRFS( 'U', 'N', '/', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTPRFS( 'U', 'N', 'N', -1, 0, A, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTPRFS( 'U', 'N', 'N', 0, -1, A, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DTPRFS( 'U', 'N', 'N', 2, 1, A, B, 1, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DTPRFS( 'U', 'N', 'N', 2, 1, A, B, 2, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DTPCON
*
         SRNAMT = 'AB_DTPCON'
         INFOT = 1
         CALL AB_DTPCON( '/', 'U', 'N', 0, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTPCON( '1', '/', 'N', 0, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTPCON( '1', 'U', '/', 0, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTPCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTPCON( '1', 'U', 'N', -1, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTPCON', INFOT, NOUT, LERR, OK )
*
*        AB_DLATPS
*
         SRNAMT = 'AB_DLATPS'
         INFOT = 1
         CALL AB_DLATPS( '/', 'N', 'N', 'N', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_DLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DLATPS( 'U', '/', 'N', 'N', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_DLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DLATPS( 'U', 'N', '/', 'N', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_DLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DLATPS( 'U', 'N', 'N', '/', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_DLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DLATPS( 'U', 'N', 'N', 'N', -1, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_DLATPS', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        Test error exits for the banded triangular routines.
*
*        AB_DTBTRS
*
         SRNAMT = 'AB_DTBTRS'
         INFOT = 1
         CALL AB_DTBTRS( '/', 'N', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTBTRS( 'U', '/', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTBTRS( 'U', 'N', '/', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTBTRS( 'U', 'N', 'N', -1, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTBTRS( 'U', 'N', 'N', 0, -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DTBTRS( 'U', 'N', 'N', 0, 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DTBTRS( 'U', 'N', 'N', 2, 1, 1, A, 1, X, 2, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DTBTRS( 'U', 'N', 'N', 2, 0, 1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_DTBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DTBRFS
*
         SRNAMT = 'AB_DTBRFS'
         INFOT = 1
         CALL AB_DTBRFS( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTBRFS( 'U', '/', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTBRFS( 'U', 'N', '/', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTBRFS( 'U', 'N', 'N', -1, 0, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTBRFS( 'U', 'N', 'N', 0, -1, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DTBRFS( 'U', 'N', 'N', 0, 0, -1, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 1, B, 2, X, 2, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 1, X, 2, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_DTBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 2, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DTBCON
*
         SRNAMT = 'AB_DTBCON'
         INFOT = 1
         CALL AB_DTBCON( '/', 'U', 'N', 0, 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTBCON( '1', '/', 'N', 0, 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DTBCON( '1', 'U', '/', 0, 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTBCON( '1', 'U', 'N', -1, 0, A, 1, RCOND, W, IW, INFO 
     $)
         CALL AB_CHKXER( 'AB_DTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DTBCON( '1', 'U', 'N', 0, -1, A, 1, RCOND, W, IW, INFO 
     $)
         CALL AB_CHKXER( 'AB_DTBCON', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DTBCON( '1', 'U', 'N', 2, 1, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DTBCON', INFOT, NOUT, LERR, OK )
*
*        AB_DLATBS
*
         SRNAMT = 'AB_DLATBS'
         INFOT = 1
         CALL AB_DLATBS( '/', 'N', 'N', 'N', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DLATBS( 'U', '/', 'N', 'N', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DLATBS( 'U', 'N', '/', 'N', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DLATBS( 'U', 'N', 'N', '/', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DLATBS( 'U', 'N', 'N', 'N', -1, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DLATBS( 'U', 'N', 'N', 'N', 1, -1, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DLATBS( 'U', 'N', 'N', 'N', 2, 1, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DLATBS', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRTR
*
      END
