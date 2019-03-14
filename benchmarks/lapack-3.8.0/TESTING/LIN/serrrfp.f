*> \brief \b AB_SERRRFP
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRRFP( NUNIT )
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
*> AB_SERRRFP tests the error exits for the REAL driver routines
*> for solving linear systems of equations.
*>
*> AB_SDRVRFP tests the REAL LAPACK RFP routines:
*>     AB_STFSM, AB_STFTRI, AB_SSFRK, AB_STFTTP, AB_STFTTR, AB_SPFTRF, AB_SPFTRS, AB_STPTTF,
*>     AB_STPTTR, AB_STRTTF, and AB_STRTTP
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SERRRFP( NUNIT )
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
      REAL               ALPHA, BETA
*     ..
*     .. Local Arrays ..
      REAL               A( 1, 1), B( 1, 1)
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_STFSM, AB_STFTRI, AB_SSFRK, AB_ST
     $FTTP, AB_STFTTR,
     +                   AB_SPFTRI, AB_SPFTRF, AB_SPFTRS, AB_STPTTF, AB_
     $STPTTR, AB_STRTTF,
     +                   AB_STRTTP
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
      A( 1, 1 ) = 1.0E+0
      B( 1, 1 ) = 1.0E+0
      ALPHA     = 1.0E+0
      BETA      = 1.0E+0
*
      SRNAMT = 'AB_SPFTRF'
      INFOT = 1
      CALL AB_SPFTRF( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_SPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SPFTRF( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_SPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SPFTRF( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_SPFTRF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_SPFTRS'
      INFOT = 1
      CALL AB_SPFTRS( '/', 'U', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_SPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SPFTRS( 'N', '/', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_SPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SPFTRS( 'N', 'U', -1, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_SPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_SPFTRS( 'N', 'U', 0, -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_SPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SPFTRS( 'N', 'U', 0, 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_SPFTRS', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_SPFTRI'
      INFOT = 1
      CALL AB_SPFTRI( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_SPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SPFTRI( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_SPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SPFTRI( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_SPFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STFSM '
      INFOT = 1
      CALL AB_STFSM( '/', 'L', 'U', 'T', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STFSM( 'N', '/', 'U', 'T', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_STFSM( 'N', 'L', '/', 'T', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_STFSM( 'N', 'L', 'U', '/', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_STFSM( 'N', 'L', 'U', 'T', '/', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_STFSM( 'N', 'L', 'U', 'T', 'U', -1, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_STFSM( 'N', 'L', 'U', 'T', 'U', 0, -1, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_STFSM( 'N', 'L', 'U', 'T', 'U', 0, 0, ALPHA, A, B, 0 )
      CALL AB_CHKXER( 'AB_STFSM ', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STFTRI'
      INFOT = 1
      CALL AB_STFTRI( '/', 'L', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_STFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STFTRI( 'N', '/', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_STFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_STFTRI( 'N', 'L', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_STFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_STFTRI( 'N', 'L', 'N', -1, A, INFO )
      CALL AB_CHKXER( 'AB_STFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STFTTR'
      INFOT = 1
      CALL AB_STFTTR( '/', 'U', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_STFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STFTTR( 'N', '/', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_STFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_STFTTR( 'N', 'U', -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_STFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_STFTTR( 'N', 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_STFTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STRTTF'
      INFOT = 1
      CALL AB_STRTTF( '/', 'U', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_STRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STRTTF( 'N', '/', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_STRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_STRTTF( 'N', 'U', -1, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_STRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_STRTTF( 'N', 'U', 0, A, 0, B, INFO )
      CALL AB_CHKXER( 'AB_STRTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STFTTP'
      INFOT = 1
      CALL AB_STFTTP( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_STFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STFTTP( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_STFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_STFTTP( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_STFTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STPTTF'
      INFOT = 1
      CALL AB_STPTTF( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_STPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STPTTF( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_STPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_STPTTF( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_STPTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STRTTP'
      INFOT = 1
      CALL AB_STRTTP( '/', 0, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_STRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STRTTP( 'U', -1, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_STRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_STRTTP( 'U', 0, A, 0,  B, INFO )
      CALL AB_CHKXER( 'AB_STRTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_STPTTR'
      INFOT = 1
      CALL AB_STPTTR( '/', 0, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_STPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_STPTTR( 'U', -1, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_STPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_STPTTR( 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_STPTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_SSFRK '
      INFOT = 1
      CALL AB_SSFRK( '/', 'U', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_SSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SSFRK( 'N', '/', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_SSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SSFRK( 'N', 'U', '/', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_SSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_SSFRK( 'N', 'U', 'N', -1, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_SSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SSFRK( 'N', 'U', 'N', 0, -1, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_SSFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_SSFRK( 'N', 'U', 'N', 0, 0, ALPHA, A, 0, BETA, B )
      CALL AB_CHKXER( 'AB_SSFRK ', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )
      ELSE
         WRITE( NOUT, FMT = 9998 )
      END IF
*
 9999 FORMAT( 1X, 'REAL RFP routines passed the tests of ',
     $        'the error exits' )
 9998 FORMAT( ' *** RFP routines failed the tests of the error ',
     $        'exits ***' )
      RETURN
*
*     End of AB_SERRRFP
*
      END
