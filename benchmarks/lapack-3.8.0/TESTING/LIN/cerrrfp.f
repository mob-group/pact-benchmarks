*> \brief \b AB_CERRRFP
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRRFP( NUNIT )
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
*> AB_CERRRFP tests the error exits for the COMPLEX driver routines
*> for solving linear systems of equations.
*>
*> AB_CDRVRFP tests the COMPLEX LAPACK RFP routines:
*>     AB_CTFSM, AB_CTFTRI, AB_CHFRK, AB_CTFTTP, AB_CTFTTR, AB_CPFTRF, AB_CPFTRS, AB_CTPTTF,
*>     AB_CTPTTR, AB_CTRTTF, and AB_CTRTTP
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CERRRFP( NUNIT )
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
      COMPLEX            ALPHA, BETA
*     ..
*     .. Local Arrays ..
      COMPLEX            A( 1, 1), B( 1, 1)
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_CTFSM, AB_CTFTRI, AB_CHFRK, AB_CT
     $FTTP, AB_CTFTTR,
     +                   AB_CPFTRI, AB_CPFTRF, AB_CPFTRS, AB_CTPTTF, AB_
     $CTPTTR, AB_CTRTTF,
     +                   AB_CTRTTP
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      OK = .TRUE.
      A( 1, 1 ) = CMPLX( 1.D0 , 1.D0  )
      B( 1, 1 ) = CMPLX( 1.D0 , 1.D0  )
      ALPHA     = CMPLX( 1.D0 , 1.D0  )
      BETA      = CMPLX( 1.D0 , 1.D0  )
*
      SRNAMT = 'AB_CPFTRF'
      INFOT = 1
      CALL AB_CPFTRF( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_CPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CPFTRF( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_CPFTRF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CPFTRF( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_CPFTRF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CPFTRS'
      INFOT = 1
      CALL AB_CPFTRS( '/', 'U', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_CPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CPFTRS( 'N', '/', 0, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_CPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CPFTRS( 'N', 'U', -1, 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_CPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CPFTRS( 'N', 'U', 0, -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_CPFTRS', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CPFTRS( 'N', 'U', 0, 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_CPFTRS', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CPFTRI'
      INFOT = 1
      CALL AB_CPFTRI( '/', 'U', 0, A, INFO )
      CALL AB_CHKXER( 'AB_CPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CPFTRI( 'N', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_CPFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CPFTRI( 'N', 'U', -1, A, INFO )
      CALL AB_CHKXER( 'AB_CPFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTFSM '
      INFOT = 1
      CALL AB_CTFSM( '/', 'L', 'U', 'C', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTFSM( 'N', '/', 'U', 'C', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CTFSM( 'N', 'L', '/', 'C', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CTFSM( 'N', 'L', 'U', '/', 'U', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CTFSM( 'N', 'L', 'U', 'C', '/', 0, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_CTFSM( 'N', 'L', 'U', 'C', 'U', -1, 0, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CTFSM( 'N', 'L', 'U', 'C', 'U', 0, -1, ALPHA, A, B, 1 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_CTFSM( 'N', 'L', 'U', 'C', 'U', 0, 0, ALPHA, A, B, 0 )
      CALL AB_CHKXER( 'AB_CTFSM ', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTFTRI'
      INFOT = 1
      CALL AB_CTFTRI( '/', 'L', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_CTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTFTRI( 'N', '/', 'N', 0, A, INFO )
      CALL AB_CHKXER( 'AB_CTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CTFTRI( 'N', 'L', '/', 0, A, INFO )
      CALL AB_CHKXER( 'AB_CTFTRI', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CTFTRI( 'N', 'L', 'N', -1, A, INFO )
      CALL AB_CHKXER( 'AB_CTFTRI', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTFTTR'
      INFOT = 1
      CALL AB_CTFTTR( '/', 'U', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_CTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTFTTR( 'N', '/', 0, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_CTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CTFTTR( 'N', 'U', -1, A, B, 1, INFO )
      CALL AB_CHKXER( 'AB_CTFTTR', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_CTFTTR( 'N', 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_CTFTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTRTTF'
      INFOT = 1
      CALL AB_CTRTTF( '/', 'U', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_CTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTRTTF( 'N', '/', 0, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_CTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CTRTTF( 'N', 'U', -1, A, 1, B, INFO )
      CALL AB_CHKXER( 'AB_CTRTTF', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CTRTTF( 'N', 'U', 0, A, 0, B, INFO )
      CALL AB_CHKXER( 'AB_CTRTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTFTTP'
      INFOT = 1
      CALL AB_CTFTTP( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_CTFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTFTTP( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_CTFTTP', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CTFTTP( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_CTFTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTPTTF'
      INFOT = 1
      CALL AB_CTPTTF( '/', 'U', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_CTPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTPTTF( 'N', '/', 0, A, B, INFO )
      CALL AB_CHKXER( 'AB_CTPTTF', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CTPTTF( 'N', 'U', -1, A, B, INFO )
      CALL AB_CHKXER( 'AB_CTPTTF', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTRTTP'
      INFOT = 1
      CALL AB_CTRTTP( '/', 0, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_CTRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTRTTP( 'U', -1, A, 1,  B, INFO )
      CALL AB_CHKXER( 'AB_CTRTTP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CTRTTP( 'U', 0, A, 0,  B, INFO )
      CALL AB_CHKXER( 'AB_CTRTTP', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CTPTTR'
      INFOT = 1
      CALL AB_CTPTTR( '/', 0, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_CTPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CTPTTR( 'U', -1, A, B, 1,  INFO )
      CALL AB_CHKXER( 'AB_CTPTTR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CTPTTR( 'U', 0, A, B, 0, INFO )
      CALL AB_CHKXER( 'AB_CTPTTR', INFOT, NOUT, LERR, OK )
*
      SRNAMT = 'AB_CHFRK '
      INFOT = 1
      CALL AB_CHFRK( '/', 'U', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_CHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CHFRK( 'N', '/', 'N', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_CHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CHFRK( 'N', 'U', '/', 0, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_CHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CHFRK( 'N', 'U', 'N', -1, 0, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_CHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CHFRK( 'N', 'U', 'N', 0, -1, ALPHA, A, 1, BETA, B )
      CALL AB_CHKXER( 'AB_CHFRK ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CHFRK( 'N', 'U', 'N', 0, 0, ALPHA, A, 0, BETA, B )
      CALL AB_CHKXER( 'AB_CHFRK ', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )
      ELSE
         WRITE( NOUT, FMT = 9998 )
      END IF
*
 9999 FORMAT( 1X, 'COMPLEX RFP routines passed the tests of the ',
     $        'error exits' )
 9998 FORMAT( ' *** RFP routines failed the tests of the error ',
     $        'exits ***' )
      RETURN
*
*     End of AB_CERRRFP
*
      END
