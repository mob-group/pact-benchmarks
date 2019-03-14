*> \brief \b AB_ZERRQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRQR( PATH, NUNIT )
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
*> AB_ZERRQR tests the error exits for the COMPLEX*16 routines
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
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE AB_ZERRQR( PATH, NUNIT )
*
*  -- LAPACK test routine ((version 3.7.0) --
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
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_ZGEQR2, AB_AB_AB_ZG
     $EQR2P, AB_AB_ZGEQRF,
     $                   AB_AB_AB_ZGEQRFP, AB_AB_ZGEQRS, AB_ZUNG2R, AB_Z
     $UNGQR, AB_ZUNM2R,
     $                   AB_ZUNMQR
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
      INTRINSIC          DBLE, DCMPLX
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
            A( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                  -1.D0 / DBLE( I+J ) )
            AF( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                   -1.D0 / DBLE( I+J ) )
   10    CONTINUE
         B( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
   20 CONTINUE
      OK = .TRUE.
*
*     Error exits for QR factorization
*
*     AB_AB_ZGEQRF
*
      SRNAMT = 'AB_AB_ZGEQRF'
      INFOT = 1
      CALL AB_AB_ZGEQRF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGEQRF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_ZGEQRF( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_ZGEQRF( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_ZGEQRFP
*
      SRNAMT = 'AB_AB_AB_ZGEQRFP'
      INFOT = 1
      CALL AB_AB_AB_ZGEQRFP( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_ZGEQRFP( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_ZGEQRFP( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_AB_ZGEQRFP( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRFP', INFOT, NOUT, LERR, OK )
*
*     AB_AB_ZGEQR2
*
      SRNAMT = 'AB_AB_ZGEQR2'
      INFOT = 1
      CALL AB_AB_ZGEQR2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGEQR2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_ZGEQR2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQR2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_ZGEQR2P
*
      SRNAMT = 'AB_AB_AB_ZGEQR2P'
      INFOT = 1
      CALL AB_AB_AB_ZGEQR2P( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_ZGEQR2P( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_ZGEQR2P( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQR2P', INFOT, NOUT, LERR, OK )
*
*     AB_AB_ZGEQRS
*
      SRNAMT = 'AB_AB_ZGEQRS'
      INFOT = 1
      CALL AB_AB_ZGEQRS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGEQRS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGEQRS( 1, 2, 0, A, 2, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_ZGEQRS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_ZGEQRS( 2, 1, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_ZGEQRS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_ZGEQRS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRS', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNGQR
*
      SRNAMT = 'AB_ZUNGQR'
      INFOT = 1
      CALL AB_ZUNGQR( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGQR( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGQR( 1, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGQR( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGQR( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNGQR( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQR', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_ZUNGQR( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQR', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNG2R
*
      SRNAMT = 'AB_ZUNG2R'
      INFOT = 1
      CALL AB_ZUNG2R( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNG2R( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNG2R( 1, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNG2R( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNG2R( 2, 1, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNG2R( 2, 1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2R', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNMQR
*
      SRNAMT = 'AB_ZUNMQR'
      INFOT = 1
      CALL AB_ZUNMQR( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNMQR( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNMQR( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNMQR( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMQR( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMQR( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMQR( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMQR( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMQR( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNMQR( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMQR( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMQR( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQR', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNM2R
*
      SRNAMT = 'AB_ZUNM2R'
      INFOT = 1
      CALL AB_ZUNM2R( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNM2R( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNM2R( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNM2R( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNM2R( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNM2R( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNM2R( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNM2R( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNM2R( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNM2R( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2R', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRQR
*
      END
