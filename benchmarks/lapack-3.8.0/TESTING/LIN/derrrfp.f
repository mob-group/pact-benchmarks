*> \brief \b AB_DERRRFP
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRRFP( NUNIT )
*
*       .. Scalar Arguments ..
*       INTEGER            NUNIT
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DERRRFP tests the error exits for the DOUBLE PRECISION driver routines
*> for solving linear systems of equations.
*>
*> AB_DDRVRFP tests the DOUBLE PRECISION LAPACK RFP routines:
*>     AB_DTFSM, AB_DTFTRI, AB_DSFRK, AB_DTFTTP, AB_DTFTTR, AB_DPFTRF, AB_DPFTRS, AB_DTPTTF,
*>     AB_DTPTTR, AB_DTRTTF, and AB_DTRTTP
*> \endverbatim
*
*  Arguments:
*  ==========
*
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
      SUBROUTINE AB_DERRRFP( NUNIT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            NUNIT
*     ..
*
*  =====================================================================
*
*     ..
*     .. Local Scalars ..
      INTEGER            INFO
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   A( 1, 1), B( 1, 1)
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_DTFSM, AB_DTFTRI, AB_DSFRK, AB_DT
     $FTTP, AB_DTFTTR,
     +                   AB_DPFTRI, AB_DPFTRF, AB_DPFTRS, AB_DTPTTF, AB_
     $DTPTTR, AB_DTRTTF,
     +                   AB_DTRTTP
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
      A( 1, 1 ) = 1.0D+0
      B( 1, 1 ) = 1.0D+0
      ALPHA     = 1.0D+0
      BETA      = 1.0D+0
*
      SRNAMT = 'AB_DPFTRF'
      INFOT = 1
      CALL AB_DPFTRF( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_DPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DPFTRF( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_DPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DPFTRF( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_DPFTRF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DPFTRS'
      INFOT = 1
      CALL AB_DPFTRS( '/', 'U', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_DPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DPFTRS( 'N', '/', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_DPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DPFTRS( 'N', 'U', -1, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_DPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DPFTRS( 'N', 'U', 0, -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_DPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DPFTRS( 'N', 'U', 0, 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_DPFTRS', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DPFTRI'
      INFOT = 1
      CALL AB_DPFTRI( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_DPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DPFTRI( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_DPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DPFTRI( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_DPFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTFSM '
      INFOT = 1
      CALL AB_DTFSM( '/', 'L', 'U', 'T', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTFSM( 'N', '/', 'U', 'T', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DTFSM( 'N', 'L', '/', 'T', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DTFSM( 'N', 'L', 'U', '/', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DTFSM( 'N', 'L', 'U', 'T', '/', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_DTFSM( 'N', 'L', 'U', 'T', 'U', -1, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DTFSM( 'N', 'L', 'U', 'T', 'U', 0, -1, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_DTFSM( 'N', 'L', 'U', 'T', 'U', 0, 0, ALPHA, A, B, 0 )
      CALL AB_CHKXER( 'AB_DTFSM ', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTFTRI'
      INFOT = 1
      CALL AB_DTFTRI( '/', 'L', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_DTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTFTRI( 'N', '/', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_DTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DTFTRI( 'N', 'L', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_DTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DTFTRI( 'N', 'L', 'N', -1, A, INFO )
      CALL AB_CHKXER( 'AB_DTFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTFTTR'
      INFOT = 1
      CALL AB_DTFTTR( '/', 'U', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_DTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTFTTR( 'N', '/', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_DTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DTFTTR( 'N', 'U', -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_DTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_DTFTTR( 'N', 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_DTFTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTRTTF'
      INFOT = 1
      CALL AB_DTRTTF( '/', 'U', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_DTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTRTTF( 'N', '/', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_DTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DTRTTF( 'N', 'U', -1, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_DTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DTRTTF( 'N', 'U', 0, A, 0, B, INFO )
      CALL AB_CHKXER( 'AB_DTRTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTFTTP'
      INFOT = 1
      CALL AB_DTFTTP( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_DTFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTFTTP( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_DTFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DTFTTP( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_DTFTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTPTTF'
      INFOT = 1
      CALL AB_DTPTTF( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_DTPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTPTTF( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_DTPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DTPTTF( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_DTPTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTRTTP'
      INFOT = 1
      CALL AB_DTRTTP( '/', 0, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_DTRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTRTTP( 'U', -1, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_DTRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DTRTTP( 'U', 0, A, 0,  B, INFO )
      CALL AB_CHKXER( 'AB_DTRTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DTPTTR'
      INFOT = 1
      CALL AB_DTPTTR( '/', 0, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_DTPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DTPTTR( 'U', -1, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_DTPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DTPTTR( 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_DTPTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_DSFRK '
      INFOT = 1
      CALL AB_DSFRK( '/', 'U', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_DSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DSFRK( 'N', '/', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_DSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DSFRK( 'N', 'U', '/', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_DSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DSFRK( 'N', 'U', 'N', -1, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_DSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DSFRK( 'N', 'U', 'N', 0, -1, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_DSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DSFRK( 'N', 'U', 'N', 0, 0, ALPHA, A, 0, BETA, B )
      CALL AB_CHKXER( 'AB_DSFRK ', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )
      ELSE
         WRITE( NOUT, FMT = 9998 )
      END IF
*
 9999 FORMAT( 1X, 'DOUBLE PRECISION RFP routines passed the tests of ',
     $        'the error exits' )
 9998 FORMAT( ' *** RFP routines failed the tests of the error ',
     $        'exits ***' )
      RETURN
*
*     End of AB_DERRRFP
*
      END
