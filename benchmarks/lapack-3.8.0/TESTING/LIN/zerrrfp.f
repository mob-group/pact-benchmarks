*> \brief \b AB_ZERRRFP
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRRFP( NUNIT )
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
*> AB_ZERRRFP tests the error exits for the COMPLEX*16 driver routines
*> for solving linear systems of equations.
*>
*> AB_ZDRVRFP tests the COMPLEX*16 LAPACK RFP routines:
*>     AB_ZTFSM, AB_ZTFTRI, AB_ZHFRK, AB_ZTFTTP, AB_ZTFTTR, AB_ZPFTRF, AB_ZPFTRS, AB_ZTPTTF,
*>     AB_ZTPTTR, AB_ZTRTTF, and AB_ZTRTTP
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
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE AB_ZERRRFP( NUNIT )
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
      COMPLEX*16         CALPHA
*     ..
*     .. Local Arrays ..
      COMPLEX*16         A( 1, 1), B( 1, 1)
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_ZTFSM, AB_ZTFTRI, AB_ZHFRK, AB_ZT
     $FTTP, AB_ZTFTTR,
     +                   AB_ZPFTRI, AB_ZPFTRF, AB_ZPFTRS, AB_ZTPTTF, AB_
     $ZTPTTR, AB_ZTRTTF,
     +                   AB_ZTRTTP
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCMPLX
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      OK = .TRUE.
      A( 1, 1 ) = DCMPLX( 1.0D0 , 1.0D0  )
      B( 1, 1 ) = DCMPLX( 1.0D0 , 1.0D0  )
      ALPHA     = 1.0D0
      CALPHA    = DCMPLX( 1.0D0 , 1.0D0  )
      BETA      = 1.0D0
*
      SRNAMT = 'AB_ZPFTRF'
      INFOT = 1
      CALL AB_ZPFTRF( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZPFTRF( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZPFTRF( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZPFTRS'
      INFOT = 1
      CALL AB_ZPFTRS( '/', 'U', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZPFTRS( 'N', '/', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZPFTRS( 'N', 'U', -1, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZPFTRS( 'N', 'U', 0, -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZPFTRS( 'N', 'U', 0, 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRS', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZPFTRI'
      INFOT = 1
      CALL AB_ZPFTRI( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZPFTRI( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZPFTRI( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_ZPFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTFSM '
      INFOT = 1
      CALL AB_ZTFSM( '/', 'L', 'U', 'C', 'U', 0, 0, CALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTFSM( 'N', '/', 'U', 'C', 'U', 0, 0, CALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZTFSM( 'N', 'L', '/', 'C', 'U', 0, 0, CALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZTFSM( 'N', 'L', 'U', '/', 'U', 0, 0, CALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZTFSM( 'N', 'L', 'U', 'C', '/', 0, 0, CALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_ZTFSM( 'N', 'L', 'U', 'C', 'U', -1, 0, CALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZTFSM( 'N', 'L', 'U', 'C', 'U', 0, -1, CALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_ZTFSM( 'N', 'L', 'U', 'C', 'U', 0, 0, CALPHA, A, B, 0 )
      CALL AB_CHKXER( 'AB_ZTFSM ', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTFTRI'
      INFOT = 1
      CALL AB_ZTFTRI( '/', 'L', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_ZTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTFTRI( 'N', '/', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_ZTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZTFTRI( 'N', 'L', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_ZTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZTFTRI( 'N', 'L', 'N', -1, A, INFO )
      CALL AB_CHKXER( 'AB_ZTFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTFTTR'
      INFOT = 1
      CALL AB_ZTFTTR( '/', 'U', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_ZTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTFTTR( 'N', '/', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_ZTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZTFTTR( 'N', 'U', -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_ZTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_ZTFTTR( 'N', 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_ZTFTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTRTTF'
      INFOT = 1
      CALL AB_ZTRTTF( '/', 'U', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_ZTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTRTTF( 'N', '/', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_ZTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZTRTTF( 'N', 'U', -1, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_ZTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZTRTTF( 'N', 'U', 0, A, 0, B, INFO )
      CALL AB_CHKXER( 'AB_ZTRTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTFTTP'
      INFOT = 1
      CALL AB_ZTFTTP( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_ZTFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTFTTP( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_ZTFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZTFTTP( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_ZTFTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTPTTF'
      INFOT = 1
      CALL AB_ZTPTTF( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_ZTPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTPTTF( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_ZTPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZTPTTF( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_ZTPTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTRTTP'
      INFOT = 1
      CALL AB_ZTRTTP( '/', 0, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_ZTRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTRTTP( 'U', -1, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_ZTRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZTRTTP( 'U', 0, A, 0,  B, INFO )
      CALL AB_CHKXER( 'AB_ZTRTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZTPTTR'
      INFOT = 1
      CALL AB_ZTPTTR( '/', 0, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_ZTPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZTPTTR( 'U', -1, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_ZTPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZTPTTR( 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_ZTPTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_ZHFRK '
      INFOT = 1
      CALL AB_ZHFRK( '/', 'U', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_ZHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZHFRK( 'N', '/', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_ZHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZHFRK( 'N', 'U', '/', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_ZHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZHFRK( 'N', 'U', 'N', -1, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_ZHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZHFRK( 'N', 'U', 'N', 0, -1, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_ZHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_ZHFRK( 'N', 'U', 'N', 0, 0, ALPHA, A, 0, BETA, B )
      CALL AB_CHKXER( 'AB_ZHFRK ', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )
      ELSE
         WRITE( NOUT, FMT = 9998 )
      END IF
*
 9999 FORMAT( 1X, 'COMPLEX*16 RFP routines passed the tests of the ',
     $        'error exits' )
 9998 FORMAT( ' *** RFP routines failed the tests of the error ',
     $        'exits ***' )
      RETURN
*
*     End of AB_ZERRRFP
*
      END
