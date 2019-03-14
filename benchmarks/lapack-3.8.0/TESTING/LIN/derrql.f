*> \brief \b AB_DERRQL
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRQL( PATH, NUNIT )
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
*> AB_DERRQL tests the error exits for the DOUBLE PRECISION routines
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DERRQL( PATH, NUNIT )
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
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DGEQL2, AB_DGEQLF, AB_
     $DGEQLS, AB_DORG2L,
     $                   AB_DORGQL, AB_DORM2L, AB_DORMQL
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
*     Error exits for QL factorization
*
*     AB_DGEQLF
*
      SRNAMT = 'AB_DGEQLF'
      INFOT = 1
      CALL AB_DGEQLF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGEQLF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DGEQLF( 2, 1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DGEQLF( 1, 2, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLF', INFOT, NOUT, LERR, OK )
*
*     AB_DGEQL2
*
      SRNAMT = 'AB_DGEQL2'
      INFOT = 1
      CALL AB_DGEQL2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_DGEQL2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGEQL2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_DGEQL2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DGEQL2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_DGEQL2', INFOT, NOUT, LERR, OK )
*
*     AB_DGEQLS
*
      SRNAMT = 'AB_DGEQLS'
      INFOT = 1
      CALL AB_DGEQLS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGEQLS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGEQLS( 1, 2, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DGEQLS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DGEQLS( 2, 1, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DGEQLS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DGEQLS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQLS', INFOT, NOUT, LERR, OK )
*
*     AB_DORGQL
*
      SRNAMT = 'AB_DORGQL'
      INFOT = 1
      CALL AB_DORGQL( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGQL( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGQL( 1, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_DORGQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGQL( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGQL( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORGQL( 2, 1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQL', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DORGQL( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGQL', INFOT, NOUT, LERR, OK )
*
*     AB_DORG2L
*
      SRNAMT = 'AB_DORG2L'
      INFOT = 1
      CALL AB_DORG2L( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORG2L( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORG2L( 1, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORG2L( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORG2L( 2, 1, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORG2L( 2, 1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORG2L', INFOT, NOUT, LERR, OK )
*
*     AB_DORMQL
*
      SRNAMT = 'AB_DORMQL'
      INFOT = 1
      CALL AB_DORMQL( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORMQL( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORMQL( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORMQL( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMQL( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMQL( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMQL( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMQL( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMQL( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORMQL( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMQL( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMQL( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMQL', INFOT, NOUT, LERR, OK )
*
*     AB_DORM2L
*
      SRNAMT = 'AB_DORM2L'
      INFOT = 1
      CALL AB_DORM2L( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORM2L( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORM2L( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORM2L( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORM2L( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORM2L( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORM2L( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORM2L( 'L', 'N', 2, 1, 0, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORM2L( 'R', 'N', 1, 2, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORM2L( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORM2L', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRQL
*
      END
