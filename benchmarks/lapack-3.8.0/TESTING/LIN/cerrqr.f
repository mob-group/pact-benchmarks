*> \brief \b AB_CERRQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRQR( PATH, NUNIT )
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
*> AB_CERRQR tests the error exits for the COMPLEX routines
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CERRQR( PATH, NUNIT )
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
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_AB_CGEQR2, AB_AB_AB_CGEQR2P, AB_A
     $B_CGEQRF, AB_AB_AB_CGEQRFP,
     $                   AB_AB_CGEQRS, AB_CHKXER, AB_CUNG2R, AB_CUNGQR, 
     $AB_CUNM2R,
     $                   AB_CUNMQR
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
      INTRINSIC          CMPLX, REAL
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
            A( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
            AF( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
   10    CONTINUE
         B( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
   20 CONTINUE
      OK = .TRUE.
*
*     Error exits for QR factorization
*
*     AB_AB_CGEQRF
*
      SRNAMT = 'AB_AB_CGEQRF'
      INFOT = 1
      CALL AB_AB_CGEQRF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_CGEQRF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_CGEQRF( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_CGEQRF( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_CGEQRFP
*
      SRNAMT = 'AB_AB_AB_CGEQRFP'
      INFOT = 1
      CALL AB_AB_AB_CGEQRFP( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_CGEQRFP( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_CGEQRFP( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_AB_CGEQRFP( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRFP', INFOT, NOUT, LERR, OK )
*
*     AB_AB_CGEQR2
*
      SRNAMT = 'AB_AB_CGEQR2'
      INFOT = 1
      CALL AB_AB_CGEQR2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_CGEQR2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_CGEQR2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQR2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_CGEQR2P
*
      SRNAMT = 'AB_AB_AB_CGEQR2P'
      INFOT = 1
      CALL AB_AB_AB_CGEQR2P( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_CGEQR2P( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_CGEQR2P( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQR2P', INFOT, NOUT, LERR, OK )
*
*     AB_AB_CGEQRS
*
      SRNAMT = 'AB_AB_CGEQRS'
      INFOT = 1
      CALL AB_AB_CGEQRS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_CGEQRS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_CGEQRS( 1, 2, 0, A, 2, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_CGEQRS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_CGEQRS( 2, 1, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_CGEQRS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_CGEQRS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRS', INFOT, NOUT, LERR, OK )
*
*     AB_CUNGQR
*
      SRNAMT = 'AB_CUNGQR'
      INFOT = 1
      CALL AB_CUNGQR( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGQR( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGQR( 1, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_CUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGQR( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGQR( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNGQR( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_CUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CUNGQR( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQR', INFOT, NOUT, LERR, OK )
*
*     AB_CUNG2R
*
      SRNAMT = 'AB_CUNG2R'
      INFOT = 1
      CALL AB_CUNG2R( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNG2R( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNG2R( 1, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNG2R( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNG2R( 2, 1, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNG2R( 2, 1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2R', INFOT, NOUT, LERR, OK )
*
*     AB_CUNMQR
*
      SRNAMT = 'AB_CUNMQR'
      INFOT = 1
      CALL AB_CUNMQR( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNMQR( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNMQR( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CUNMQR( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMQR( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMQR( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMQR( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMQR( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMQR( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CUNMQR( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_CUNMQR( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_CUNMQR( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQR', INFOT, NOUT, LERR, OK )
*
*     AB_CUNM2R
*
      SRNAMT = 'AB_CUNM2R'
      INFOT = 1
      CALL AB_CUNM2R( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNM2R( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNM2R( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CUNM2R( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNM2R( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNM2R( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNM2R( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNM2R( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNM2R( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CUNM2R( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2R', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRQR
*
      END
