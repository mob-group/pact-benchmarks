*> \brief \b AB_ZERRRQ
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRRQ( PATH, NUNIT )
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
*> AB_ZERRRQ tests the error exits for the COMPLEX*16 routines
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
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE AB_ZERRRQ( PATH, NUNIT )
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
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZGERQ2, AB_ZGERQF, AB_
     $ZGERQS, AB_ZUNGR2,
     $                   AB_ZUNGRQ, AB_ZUNMR2, AB_ZUNMRQ
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
*     Error exits for RQ factorization
*
*     AB_ZGERQF
*
      SRNAMT = 'AB_ZGERQF'
      INFOT = 1
      CALL AB_ZGERQF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGERQF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZGERQF( 2, 1, A, 1, B, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZGERQF( 2, 1, A, 2, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQF', INFOT, NOUT, LERR, OK )
*
*     AB_ZGERQ2
*
      SRNAMT = 'AB_ZGERQ2'
      INFOT = 1
      CALL AB_ZGERQ2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_ZGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGERQ2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_ZGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZGERQ2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_ZGERQ2', INFOT, NOUT, LERR, OK )
*
*     AB_ZGERQS
*
      SRNAMT = 'AB_ZGERQS'
      INFOT = 1
      CALL AB_ZGERQS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGERQS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGERQS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZGERQS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZGERQS( 2, 2, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_ZGERQS( 2, 2, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZGERQS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGERQS', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNGRQ
*
      SRNAMT = 'AB_ZUNGRQ'
      INFOT = 1
      CALL AB_ZUNGRQ( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGRQ( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGRQ( 2, 1, 0, A, 2, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGRQ( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGRQ( 1, 2, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNGRQ( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZUNGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_ZUNGRQ( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGRQ', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNGR2
*
      SRNAMT = 'AB_ZUNGR2'
      INFOT = 1
      CALL AB_ZUNGR2( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGR2( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGR2( 2, 1, 0, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGR2( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGR2( 1, 2, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNGR2( 2, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNGR2', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNMRQ
*
      SRNAMT = 'AB_ZUNMRQ'
      INFOT = 1
      CALL AB_ZUNMRQ( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNMRQ( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNMRQ( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNMRQ( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMRQ( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMRQ( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMRQ( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMRQ( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMRQ( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNMRQ( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMRQ( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMRQ( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMRQ', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNMR2
*
      SRNAMT = 'AB_ZUNMR2'
      INFOT = 1
      CALL AB_ZUNMR2( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNMR2( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNMR2( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNMR2( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMR2( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMR2( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMR2( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMR2( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMR2( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNMR2( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNMR2', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRRQ
*
      END
