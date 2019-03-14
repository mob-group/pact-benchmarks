*> \brief \b AB_AB_SERRLQT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_SERRLQT( PATH, NUNIT )
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
*> AB_AB_DERRLQT tests the error exits for the DOUBLE PRECISION routines
*> that use the LQT decomposition of a general matrix.
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
      SUBROUTINE AB_AB_SERRLQT( PATH, NUNIT )
      IMPLICIT NONE
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
      REAL               A( NMAX, NMAX ), T( NMAX, NMAX ), W( NMAX ),
     $                   C( NMAX, NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_SGELQT3, AB_AB_SGEL
     $QT,
     $                   AB_AB_SGEMLQT
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
      DO J = 1, NMAX
         DO I = 1, NMAX
            A( I, J ) = 1.D0 / REAL( I+J )
            C( I, J ) = 1.D0 / REAL( I+J )
            T( I, J ) = 1.D0 / REAL( I+J )
         END DO
         W( J ) = 0.D0
      END DO
      OK = .TRUE.
*
*     Error exits for LQT factorization
*
*     AB_AB_SGELQT
*
      SRNAMT = 'AB_AB_SGELQT'
      INFOT = 1
      CALL AB_AB_SGELQT( -1, 0, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGELQT( 0, -1, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_SGELQT( 0, 0, 0, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_SGELQT( 2, 1, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_SGELQT( 2, 2, 2, A, 2, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT', INFOT, NOUT, LERR, OK )
*
*     AB_AB_SGELQT3
*
      SRNAMT = 'AB_AB_SGELQT3'
      INFOT = 1
      CALL AB_AB_SGELQT3( -1, 0, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT3', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGELQT3( 0, -1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT3', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_SGELQT3( 2, 2, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT3', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_SGELQT3( 2, 2, A, 2, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGELQT3', INFOT, NOUT, LERR, OK )
*
*     AB_AB_SGEMLQT
*
      SRNAMT = 'AB_AB_SGEMLQT'
      INFOT = 1
      CALL AB_AB_SGEMLQT( '/', 'N', 0, 0, 0, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGEMLQT( 'L', '/', 0, 0, 0, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_SGEMLQT( 'L', 'N', -1, 0, 0, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_SGEMLQT( 'L', 'N', 0, -1, 0, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_SGEMLQT( 'L', 'N', 0, 0, -1, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_SGEMLQT( 'R', 'N', 0, 0, -1, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_SGEMLQT( 'L', 'N', 0, 0, 0, 0, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_SGEMLQT( 'R', 'N', 2, 2, 2, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_SGEMLQT( 'L', 'N', 2, 2, 2, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_SGEMLQT( 'R', 'N', 1, 1, 1, 1, A, 1, T, 0, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_AB_SGEMLQT( 'L', 'N', 1, 1, 1, 1, A, 1, T, 1, C, 0, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_SGEMLQT', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_AB_SERRLQT
*
      END
