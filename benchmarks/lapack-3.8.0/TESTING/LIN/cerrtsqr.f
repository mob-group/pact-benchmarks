*> \brief \b AB_CERRTSQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRTSQR( PATH, NUNIT )
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
*> AB_CERRTSQR tests the error exits for the COMPLEX routines
*> that use the TSQR decomposition of a general matrix.
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
*> \author Univ. of Colorado Zenver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_CERRTSQR( PATH, NUNIT )
      IMPLICIT NONE
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
      INTEGER            I, INFO, J, NB
*     ..
*     .. Local Arrays ..
      COMPLEX            A( NMAX, NMAX ), T( NMAX, NMAX ), W( NMAX ),
     $                   C( NMAX, NMAX ), TAU(NMAX)
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_CGEQR,
     $                   AB_CGEMQR, AB_CGELQ, AB_CGEMLQ
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
*     .. Intrinsic Functions ..
      INTRINSIC          REAL
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
*
*     Set the variables to innocuous values.
*
      DO J = 1, NMAX
         DO I = 1, NMAX
            A( I, J ) = 1.E0 / CMPLX( REAL( I+J ), 0.E0 )
            C( I, J ) = 1.E0 / CMPLX( REAL( I+J ), 0.E0 )
            T( I, J ) = 1.E0 / CMPLX( REAL( I+J ), 0.E0 )
         END DO
         W( J ) = 0.E0
      END DO
      OK = .TRUE.
*
*     Error exits for TS factorization
*
*     AB_CGEQR
*
      SRNAMT = 'AB_CGEQR'
      INFOT = 1
      CALL AB_CGEQR( -1, 0, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGEQR( 0, -1, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGEQR( 1, 1, A, 0, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_CGEQR( 3, 2, A, 3, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CGEQR( 3, 2, A, 3, TAU, 8, W, 0, INFO )
      CALL AB_CHKXER( 'AB_CGEQR', INFOT, NOUT, LERR, OK )
*
*     AB_CGEMQR
*
      TAU(1)=1
      TAU(2)=1
      SRNAMT = 'AB_CGEMQR'
      NB=1
      INFOT = 1
      CALL AB_CGEMQR( '/', 'N', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGEMQR( 'L', '/', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CGEMQR( 'L', 'N', -1, 0, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGEMQR( 'L', 'N', 0, -1, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CGEMQR( 'L', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CGEMQR( 'R', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CGEMQR( 'L', 'N', 2, 1, 0, A, 0, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_CGEMQR( 'R', 'N', 2, 2, 1, A, 2, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_CGEMQR( 'L', 'N', 2, 2, 1, A, 2, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_CGEMQR( 'L', 'N', 2, 1, 1, A, 2, TAU, 6, C, 0, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 13
      CALL AB_CGEMQR( 'L', 'N', 2, 2, 1, A, 2, TAU, 6, C, 2, W, 0,INFO)
      CALL AB_CHKXER( 'AB_CGEMQR', INFOT, NOUT, LERR, OK )
*
*     AB_CGELQ
*
      SRNAMT = 'AB_CGELQ'
      INFOT = 1
      CALL AB_CGELQ( -1, 0, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGELQ( 0, -1, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGELQ( 1, 1, A, 0, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_CGELQ( 2, 3, A, 3, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CGELQ( 2, 3, A, 3, TAU, 8, W, 0, INFO )
      CALL AB_CHKXER( 'AB_CGELQ', INFOT, NOUT, LERR, OK )
*
*     AB_CGEMLQ
*
      TAU(1)=1
      TAU(2)=1
      SRNAMT = 'AB_CGEMLQ'
      NB=1
      INFOT = 1
      CALL AB_CGEMLQ( '/', 'N', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGEMLQ( 'L', '/', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CGEMLQ( 'L', 'N', -1, 0, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGEMLQ( 'L', 'N', 0, -1, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CGEMLQ( 'L', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CGEMLQ( 'R', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CGEMLQ( 'L', 'N', 1, 2, 0, A, 0, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_CGEMLQ( 'R', 'N', 2, 2, 1, A, 1, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_CGEMLQ( 'L', 'N', 2, 2, 1, A, 1, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_CGEMLQ( 'L', 'N', 1, 2, 1, A, 1, TAU, 6, C, 0, W, 1,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 13
      CALL AB_CGEMLQ( 'L', 'N', 2, 2, 1, A, 2, TAU, 6, C, 2, W, 0,INFO)
      CALL AB_CHKXER( 'AB_CGEMLQ', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRTSQR
*
      END
