*> \brief \b AB_DERRQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRQR( PATH, NUNIT )
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
*> AB_DERRQR tests the error exits for the DOUBLE PRECISION routines
*> that use the QR decomposition of a general matrix.
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
      SUBROUTINE AB_DERRQR( PATH, NUNIT )
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
      INTEGER            I, INFO, J
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_DGEQR2, AB_AB_AB_DG
     $EQR2P, AB_AB_DGEQRF,
     $                   AB_AB_AB_DGEQRFP, AB_AB_DGEQRS, AB_DORG2R, AB_D
     $ORGQR, AB_DORM2R,
     $                   AB_DORMQR
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
      INTRINSIC          DBLE
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
            AF( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
         B( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
   20 CONTINUE
      OK = .TRUE.
*
*     Error exits for QR factorization
*
*     AB_AB_DGEQRF
*
      SRNAMT = 'AB_AB_DGEQRF'
      INFOT = 1
      CALL AB_AB_DGEQRF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGEQRF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_DGEQRF( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_DGEQRF( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_DGEQRFP
*
      SRNAMT = 'AB_AB_AB_DGEQRFP'
      INFOT = 1
      CALL AB_AB_AB_DGEQRFP( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_DGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_DGEQRFP( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_DGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_DGEQRFP( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_DGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_AB_DGEQRFP( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_DGEQRFP', INFOT, NOUT, LERR, OK )
*
*     AB_AB_DGEQR2
*
      SRNAMT = 'AB_AB_DGEQR2'
      INFOT = 1
      CALL AB_AB_DGEQR2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGEQR2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_DGEQR2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQR2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_DGEQR2P
*
      SRNAMT = 'AB_AB_AB_DGEQR2P'
      INFOT = 1
      CALL AB_AB_AB_DGEQR2P( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_DGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_DGEQR2P( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_DGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_DGEQR2P( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_DGEQR2P', INFOT, NOUT, LERR, OK )
*
*     AB_AB_DGEQRS
*
      SRNAMT = 'AB_AB_DGEQRS'
      INFOT = 1
      CALL AB_AB_DGEQRS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGEQRS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGEQRS( 1, 2, 0, A, 2, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_DGEQRS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_DGEQRS( 2, 1, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_DGEQRS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_DGEQRS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGEQRS', INFOT, NOUT, LERR, OK )
*
*     AB_DORGQR
*
      SRNAMT = 'AB_DORGQR'
      INFOT = 1
      CALL AB_DORGQR( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGQR( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGQR( 1, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_DORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGQR( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGQR( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORGQR( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_DORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DORGQR( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQR', INFOT, NOUT, LERR, OK )
*
*     AB_DORG2R
*
      SRNAMT = 'AB_DORG2R'
      INFOT = 1
      CALL AB_DORG2R( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORG2R( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORG2R( 1, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORG2R( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORG2R( 2, 1, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORG2R( 2, 1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2R', INFOT, NOUT, LERR, OK )
*
*     AB_DORMQR
*
      SRNAMT = 'AB_DORMQR'
      INFOT = 1
      CALL AB_DORMQR( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORMQR( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORMQR( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORMQR( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMQR( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMQR( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMQR( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMQR( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMQR( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORMQR( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMQR( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMQR( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQR', INFOT, NOUT, LERR, OK )
*
*     AB_DORM2R
*
      SRNAMT = 'AB_DORM2R'
      INFOT = 1
      CALL AB_DORM2R( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORM2R( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORM2R( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORM2R( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORM2R( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORM2R( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORM2R( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORM2R( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORM2R( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORM2R( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2R', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRQR
*
      END
