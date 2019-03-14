*> \brief \b AB_ZERRLQ
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRLQ( PATH, NUNIT )
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
*> AB_ZERRLQ tests the error exits for the COMPLEX*16 routines
*> that use the LQ decomposition of a general matrix.
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
      SUBROUTINE AB_ZERRLQ( PATH, NUNIT )
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
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_ZGELQ2, AB_AB_ZGELQ
     $F, AB_AB_ZGELQS, AB_ZUNGL2,
     $                   AB_ZUNGLQ, AB_ZUNML2, AB_ZUNMLQ
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
*     Error exits for LQ factorization
*
*     AB_AB_ZGELQF
*
      SRNAMT = 'AB_AB_ZGELQF'
      INFOT = 1
      CALL AB_AB_ZGELQF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGELQF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_ZGELQF( 2, 1, A, 1, B, W, 2, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_ZGELQF( 2, 1, A, 2, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_ZGELQ2
*
      SRNAMT = 'AB_AB_ZGELQ2'
      INFOT = 1
      CALL AB_AB_ZGELQ2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQ2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGELQ2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQ2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_ZGELQ2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQ2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_ZGELQS
*
      SRNAMT = 'AB_AB_ZGELQS'
      INFOT = 1
      CALL AB_AB_ZGELQS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGELQS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGELQS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_ZGELQS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_ZGELQS( 2, 2, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_ZGELQS( 1, 2, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_ZGELQS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGELQS', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNGLQ
*
      SRNAMT = 'AB_ZUNGLQ'
      INFOT = 1
      CALL AB_ZUNGLQ( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGLQ( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGLQ( 2, 1, 0, A, 2, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZUNGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGLQ( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGLQ( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNGLQ( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZUNGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_ZUNGLQ( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGLQ', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNGL2
*
      SRNAMT = 'AB_ZUNGL2'
      INFOT = 1
      CALL AB_ZUNGL2( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGL2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGL2( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGL2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGL2( 2, 1, 0, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGL2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGL2( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGL2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGL2( 1, 1, 2, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGL2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNGL2( 2, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGL2', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNMLQ
*
      SRNAMT = 'AB_ZUNMLQ'
      INFOT = 1
      CALL AB_ZUNMLQ( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNMLQ( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNMLQ( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNMLQ( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMLQ( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMLQ( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMLQ( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMLQ( 'L', 'N', 2, 0, 2, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMLQ( 'R', 'N', 0, 2, 2, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNMLQ( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMLQ( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMLQ( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMLQ', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNML2
*
      SRNAMT = 'AB_ZUNML2'
      INFOT = 1
      CALL AB_ZUNML2( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNML2( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNML2( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNML2( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNML2( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNML2( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNML2( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNML2( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNML2( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNML2( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNML2', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRLQ
*
      END
