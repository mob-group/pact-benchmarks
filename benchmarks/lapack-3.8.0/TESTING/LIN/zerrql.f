*> \brief \b AB_ZERRQL
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRQL( PATH, NUNIT )
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
*> AB_ZERRQL tests the error exits for the COMPLEX*16 routines
*> that use the QL decomposition of a general matrix.
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
      SUBROUTINE AB_ZERRQL( PATH, NUNIT )
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
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZGEQL2, AB_ZGEQLF, AB_
     $ZGEQLS, AB_ZUNG2L,
     $                   AB_ZUNGQL, AB_ZUNM2L, AB_ZUNMQL
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
*     Error exits for QL factorization
*
*     AB_ZGEQLF
*
      SRNAMT = 'AB_ZGEQLF'
      INFOT = 1
      CALL AB_ZGEQLF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGEQLF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZGEQLF( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZGEQLF( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLF', INFOT, NOUT, LERR, OK )
*
*     AB_ZGEQL2
*
      SRNAMT = 'AB_ZGEQL2'
      INFOT = 1
      CALL AB_ZGEQL2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_ZGEQL2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGEQL2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_ZGEQL2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZGEQL2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_ZGEQL2', INFOT, NOUT, LERR, OK )
*
*     AB_ZGEQLS
*
      SRNAMT = 'AB_ZGEQLS'
      INFOT = 1
      CALL AB_ZGEQLS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGEQLS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZGEQLS( 1, 2, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZGEQLS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZGEQLS( 2, 1, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_ZGEQLS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZGEQLS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZGEQLS', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNGQL
*
      SRNAMT = 'AB_ZUNGQL'
      INFOT = 1
      CALL AB_ZUNGQL( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGQL( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNGQL( 1, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGQL( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNGQL( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNGQL( 2, 1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_ZUNGQL( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNGQL', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNG2L
*
      SRNAMT = 'AB_ZUNG2L'
      INFOT = 1
      CALL AB_ZUNG2L( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNG2L( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNG2L( 1, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNG2L( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNG2L( 2, 1, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNG2L( 2, 1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNG2L', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNMQL
*
      SRNAMT = 'AB_ZUNMQL'
      INFOT = 1
      CALL AB_ZUNMQL( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNMQL( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNMQL( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNMQL( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMQL( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMQL( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNMQL( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMQL( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNMQL( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNMQL( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMQL( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_ZUNMQL( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_ZUNMQL', INFOT, NOUT, LERR, OK )
*
*     AB_ZUNM2L
*
      SRNAMT = 'AB_ZUNM2L'
      INFOT = 1
      CALL AB_ZUNM2L( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_ZUNM2L( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_ZUNM2L( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_ZUNM2L( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNM2L( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNM2L( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_ZUNM2L( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNM2L( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_ZUNM2L( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_ZUNM2L( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_ZUNM2L', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRQL
*
      END
