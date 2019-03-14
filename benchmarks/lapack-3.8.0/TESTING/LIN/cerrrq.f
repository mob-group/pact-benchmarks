*> \brief \b AB_CERRRQ
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRRQ( PATH, NUNIT )
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
*> AB_CERRRQ tests the error exits for the COMPLEX routines
*> that use the RQ decomposition of a general matrix.
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
      SUBROUTINE AB_CERRRQ( PATH, NUNIT )
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
      EXTERNAL           AB_ALAESM, AB_CGERQ2, AB_CGERQF, AB_CGERQS, AB_
     $CHKXER, AB_CUNGR2,
     $                   AB_CUNGRQ, AB_CUNMR2, AB_CUNMRQ
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
*     Error exits for RQ factorization
*
*     AB_CGERQF
*
      SRNAMT = 'AB_CGERQF'
      INFOT = 1
      CALL AB_CGERQF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGERQF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGERQF( 2, 1, A, 1, B, W, 2, INFO )
      CALL AB_CHKXER( 'AB_CGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CGERQF( 2, 1, A, 2, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQF', INFOT, NOUT, LERR, OK )
*
*     AB_CGERQ2
*
      SRNAMT = 'AB_CGERQ2'
      INFOT = 1
      CALL AB_CGERQ2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_CGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGERQ2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_CGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGERQ2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_CGERQ2', INFOT, NOUT, LERR, OK )
*
*     AB_CGERQS
*
      SRNAMT = 'AB_CGERQS'
      INFOT = 1
      CALL AB_CGERQS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGERQS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGERQS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CGERQS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CGERQS( 2, 2, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CGERQS( 2, 2, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CGERQS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGERQS', INFOT, NOUT, LERR, OK )
*
*     AB_CUNGRQ
*
      SRNAMT = 'AB_CUNGRQ'
      INFOT = 1
      CALL AB_CUNGRQ( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGRQ( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGRQ( 2, 1, 0, A, 2, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_CUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGRQ( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGRQ( 1, 2, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNGRQ( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_CUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CUNGRQ( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGRQ', INFOT, NOUT, LERR, OK )
*
*     AB_CUNGR2
*
      SRNAMT = 'AB_CUNGR2'
      INFOT = 1
      CALL AB_CUNGR2( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGR2( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGR2( 2, 1, 0, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGR2( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGR2( 1, 2, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNGR2( 2, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNGR2', INFOT, NOUT, LERR, OK )
*
*     AB_CUNMRQ
*
      SRNAMT = 'AB_CUNMRQ'
      INFOT = 1
      CALL AB_CUNMRQ( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNMRQ( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNMRQ( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CUNMRQ( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMRQ( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMRQ( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMRQ( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMRQ( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMRQ( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CUNMRQ( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_CUNMRQ( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_CUNMRQ( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMRQ', INFOT, NOUT, LERR, OK )
*
*     AB_CUNMR2
*
      SRNAMT = 'AB_CUNMR2'
      INFOT = 1
      CALL AB_CUNMR2( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNMR2( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNMR2( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CUNMR2( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMR2( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMR2( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMR2( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMR2( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMR2( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CUNMR2( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNMR2', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRRQ
*
      END
