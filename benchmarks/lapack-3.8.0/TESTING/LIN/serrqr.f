*> \brief \b AB_SERRQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRQR( PATH, NUNIT )
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
*> AB_SERRQR tests the error exits for the REAL routines
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SERRQR( PATH, NUNIT )
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
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_SGEQR2, AB_AB_AB_SG
     $EQR2P, AB_AB_SGEQRF,
     $                   AB_AB_AB_SGEQRFP, AB_AB_SGEQRS, AB_SORG2R, AB_S
     $ORGQR, AB_SORM2R,
     $                   AB_SORMQR
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
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1. / REAL( I+J )
            AF( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
         B( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
   20 CONTINUE
      OK = .TRUE.
*
*     Error exits for QR factorization
*
*     AB_AB_SGEQRF
*
      SRNAMT = 'AB_AB_SGEQRF'
      INFOT = 1
      CALL AB_AB_SGEQRF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGEQRF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_SGEQRF( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_SGEQRF( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_SGEQRFP
*
      SRNAMT = 'AB_AB_AB_SGEQRFP'
      INFOT = 1
      CALL AB_AB_AB_SGEQRFP( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_SGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_SGEQRFP( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_SGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_SGEQRFP( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_SGEQRFP', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_AB_SGEQRFP( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_SGEQRFP', INFOT, NOUT, LERR, OK )
*
*     AB_AB_SGEQR2
*
      SRNAMT = 'AB_AB_SGEQR2'
      INFOT = 1
      CALL AB_AB_SGEQR2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGEQR2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_SGEQR2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQR2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_SGEQR2P
*
      SRNAMT = 'AB_AB_AB_SGEQR2P'
      INFOT = 1
      CALL AB_AB_AB_SGEQR2P( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_SGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_SGEQR2P( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_SGEQR2P', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_SGEQR2P( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_SGEQR2P', INFOT, NOUT, LERR, OK )
*
*     AB_AB_SGEQRS
*
      SRNAMT = 'AB_AB_SGEQRS'
      INFOT = 1
      CALL AB_AB_SGEQRS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGEQRS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGEQRS( 1, 2, 0, A, 2, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_SGEQRS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_SGEQRS( 2, 1, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_SGEQRS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_SGEQRS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGEQRS', INFOT, NOUT, LERR, OK )
*
*     AB_SORGQR
*
      SRNAMT = 'AB_SORGQR'
      INFOT = 1
      CALL AB_SORGQR( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORGQR( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORGQR( 1, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_SORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORGQR( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORGQR( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORGQR( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_SORGQR', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_SORGQR( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGQR', INFOT, NOUT, LERR, OK )
*
*     AB_SORG2R
*
      SRNAMT = 'AB_SORG2R'
      INFOT = 1
      CALL AB_SORG2R( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORG2R( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORG2R( 1, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORG2R( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORG2R( 2, 1, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORG2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORG2R( 2, 1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORG2R', INFOT, NOUT, LERR, OK )
*
*     AB_SORMQR
*
      SRNAMT = 'AB_SORMQR'
      INFOT = 1
      CALL AB_SORMQR( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORMQR( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORMQR( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_SORMQR( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMQR( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMQR( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMQR( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORMQR( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORMQR( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_SORMQR( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_SORMQR( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_SORMQR( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMQR', INFOT, NOUT, LERR, OK )
*
*     AB_SORM2R
*
      SRNAMT = 'AB_SORM2R'
      INFOT = 1
      CALL AB_SORM2R( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORM2R( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORM2R( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_SORM2R( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORM2R( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORM2R( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORM2R( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORM2R( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORM2R( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_SORM2R( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORM2R', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRQR
*
      END
