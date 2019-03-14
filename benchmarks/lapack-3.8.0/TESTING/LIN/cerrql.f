*> \brief \b AB_CERRQL
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRQL( PATH, NUNIT )
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
*> AB_CERRQL tests the error exits for the COMPLEX routines
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CERRQL( PATH, NUNIT )
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
      EXTERNAL           AB_ALAESM, AB_CGEQL2, AB_CGEQLF, AB_CGEQLS, AB_
     $CHKXER, AB_CUNG2L,
     $                   AB_CUNGQL, AB_CUNM2L, AB_CUNMQL
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
*     Error exits for QL factorization
*
*     AB_CGEQLF
*
      SRNAMT = 'AB_CGEQLF'
      INFOT = 1
      CALL AB_CGEQLF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGEQLF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGEQLF( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CGEQLF( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLF', INFOT, NOUT, LERR, OK )
*
*     AB_CGEQL2
*
      SRNAMT = 'AB_CGEQL2'
      INFOT = 1
      CALL AB_CGEQL2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_CGEQL2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGEQL2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_CGEQL2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CGEQL2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_CGEQL2', INFOT, NOUT, LERR, OK )
*
*     AB_CGEQLS
*
      SRNAMT = 'AB_CGEQLS'
      INFOT = 1
      CALL AB_CGEQLS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGEQLS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CGEQLS( 1, 2, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CGEQLS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CGEQLS( 2, 1, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CGEQLS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CGEQLS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CGEQLS', INFOT, NOUT, LERR, OK )
*
*     AB_CUNGQL
*
      SRNAMT = 'AB_CUNGQL'
      INFOT = 1
      CALL AB_CUNGQL( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGQL( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNGQL( 1, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_CUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGQL( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNGQL( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNGQL( 2, 1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQL', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_CUNGQL( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNGQL', INFOT, NOUT, LERR, OK )
*
*     AB_CUNG2L
*
      SRNAMT = 'AB_CUNG2L'
      INFOT = 1
      CALL AB_CUNG2L( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNG2L( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNG2L( 1, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNG2L( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNG2L( 2, 1, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNG2L( 2, 1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_CUNG2L', INFOT, NOUT, LERR, OK )
*
*     AB_CUNMQL
*
      SRNAMT = 'AB_CUNMQL'
      INFOT = 1
      CALL AB_CUNMQL( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNMQL( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNMQL( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CUNMQL( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMQL( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMQL( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNMQL( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMQL( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNMQL( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CUNMQL( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_CUNMQL( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_CUNMQL( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_CUNMQL', INFOT, NOUT, LERR, OK )
*
*     AB_CUNM2L
*
      SRNAMT = 'AB_CUNM2L'
      INFOT = 1
      CALL AB_CUNM2L( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_CUNM2L( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_CUNM2L( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_CUNM2L( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNM2L( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNM2L( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_CUNM2L( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNM2L( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_CUNM2L( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_CUNM2L( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_CUNM2L', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRQL
*
      END
