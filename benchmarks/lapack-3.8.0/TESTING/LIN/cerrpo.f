*> \brief \b AB_CERRPO
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRPO( PATH, NUNIT )
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
*> AB_CERRPO tests the error exits for the COMPLEX routines
*> for Hermitian positive definite matrices.
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
      SUBROUTINE AB_CERRPO( PATH, NUNIT )
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
      PARAMETER          ( NMAX = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J
      REAL               ANRM, RCOND
*     ..
*     .. Local Arrays ..
      REAL               R( NMAX ), R1( NMAX ), R2( NMAX )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_CPBCON, AB_CPBEQU, AB_
     $CPBRFS, AB_CPBTF2,
     $                   AB_CPBTRF, AB_CPBTRS, AB_CPOCON, AB_CPOEQU, AB_
     $CPORFS, AB_CPOTF2,
     $                   AB_CPOTRF, AB_CPOTRI, AB_CPOTRS, AB_CPPCON, AB_
     $CPPEQU, AB_CPPRFS,
     $                   AB_CPPTRF, AB_CPPTRI, AB_CPPTRS
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
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
            AF( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
   10    CONTINUE
         B( J ) = 0.
         R1( J ) = 0.
         R2( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
   20 CONTINUE
      ANRM = 1.
      OK = .TRUE.
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite matrix.
*
      IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        AB_CPOTRF
*
         SRNAMT = 'AB_CPOTRF'
         INFOT = 1
         CALL AB_CPOTRF( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPOTRF( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPOTRF( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CPOTF2
*
         SRNAMT = 'AB_CPOTF2'
         INFOT = 1
         CALL AB_CPOTF2( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPOTF2( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPOTF2( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTF2', INFOT, NOUT, LERR, OK )
*
*        AB_CPOTRI
*
         SRNAMT = 'AB_CPOTRI'
         INFOT = 1
         CALL AB_CPOTRI( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPOTRI( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPOTRI( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRI', INFOT, NOUT, LERR, OK )
*
*        AB_CPOTRS
*
         SRNAMT = 'AB_CPOTRS'
         INFOT = 1
         CALL AB_CPOTRS( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPOTRS( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPOTRS( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPOTRS( 'U', 2, 1, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CPOTRS( 'U', 2, 1, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CPORFS
*
         SRNAMT = 'AB_CPORFS'
         INFOT = 1
         CALL AB_CPORFS( '/', 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPORFS( 'U', -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPORFS( 'U', 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPORFS( 'U', 2, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CPORFS( 'U', 2, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CPORFS( 'U', 2, 1, A, 2, AF, 2, B, 1, X, 2, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CPORFS( 'U', 2, 1, A, 2, AF, 2, B, 2, X, 1, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPORFS', INFOT, NOUT, LERR, OK )
*
*        AB_CPOCON
*
         SRNAMT = 'AB_CPOCON'
         INFOT = 1
         CALL AB_CPOCON( '/', 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPOCON( 'U', -1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPOCON( 'U', 2, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPOCON( 'U', 1, A, 1, -ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPOCON', INFOT, NOUT, LERR, OK )
*
*        AB_CPOEQU
*
         SRNAMT = 'AB_CPOEQU'
         INFOT = 1
         CALL AB_CPOEQU( -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPOEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPOEQU( 2, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPOEQU', INFOT, NOUT, LERR, OK )
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite packed matrix.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        AB_CPPTRF
*
         SRNAMT = 'AB_CPPTRF'
         INFOT = 1
         CALL AB_CPPTRF( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_CPPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPPTRF( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_CPPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CPPTRI
*
         SRNAMT = 'AB_CPPTRI'
         INFOT = 1
         CALL AB_CPPTRI( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_CPPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPPTRI( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_CPPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_CPPTRS
*
         SRNAMT = 'AB_CPPTRS'
         INFOT = 1
         CALL AB_CPPTRS( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPPTRS( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPPTRS( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPPTRS( 'U', 2, 1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CPPRFS
*
         SRNAMT = 'AB_CPPRFS'
         INFOT = 1
         CALL AB_CPPRFS( '/', 0, 0, A, AF, B, 1, X, 1, R1, R2, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_CPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPPRFS( 'U', -1, 0, A, AF, B, 1, X, 1, R1, R2, W, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPPRFS( 'U', 0, -1, A, AF, B, 1, X, 1, R1, R2, W, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_CPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CPPRFS( 'U', 2, 1, A, AF, B, 1, X, 2, R1, R2, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_CPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CPPRFS( 'U', 2, 1, A, AF, B, 2, X, 1, R1, R2, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_CPPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CPPCON
*
         SRNAMT = 'AB_CPPCON'
         INFOT = 1
         CALL AB_CPPCON( '/', 0, A, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPPCON( 'U', -1, A, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPPCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPPCON( 'U', 1, A, -ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPPCON', INFOT, NOUT, LERR, OK )
*
*        AB_CPPEQU
*
         SRNAMT = 'AB_CPPEQU'
         INFOT = 1
         CALL AB_CPPEQU( '/', 0, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPPEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPPEQU( 'U', -1, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPPEQU', INFOT, NOUT, LERR, OK )
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite band matrix.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        AB_CPBTRF
*
         SRNAMT = 'AB_CPBTRF'
         INFOT = 1
         CALL AB_CPBTRF( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPBTRF( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPBTRF( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPBTRF( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CPBTF2
*
         SRNAMT = 'AB_CPBTF2'
         INFOT = 1
         CALL AB_CPBTF2( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPBTF2( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPBTF2( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPBTF2( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_CPBTRS
*
         SRNAMT = 'AB_CPBTRS'
         INFOT = 1
         CALL AB_CPBTRS( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPBTRS( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPBTRS( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPBTRS( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPBTRS( 'U', 2, 1, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CPBTRS( 'U', 2, 0, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CPBRFS
*
         SRNAMT = 'AB_CPBRFS'
         INFOT = 1
         CALL AB_CPBRFS( '/', 0, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPBRFS( 'U', -1, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPBRFS( 'U', 1, -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPBRFS( 'U', 0, 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPBRFS( 'U', 2, 1, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CPBRFS( 'U', 2, 1, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 1, X, 2, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 2, X, 1, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CPBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CPBCON
*
         SRNAMT = 'AB_CPBCON'
         INFOT = 1
         CALL AB_CPBCON( '/', 0, 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPBCON( 'U', -1, 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPBCON( 'U', 1, -1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPBCON( 'U', 2, 1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPBCON( 'U', 1, 0, A, 1, -ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CPBCON', INFOT, NOUT, LERR, OK )
*
*        AB_CPBEQU
*
         SRNAMT = 'AB_CPBEQU'
         INFOT = 1
         CALL AB_CPBEQU( '/', 0, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPBEQU( 'U', -1, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPBEQU( 'U', 1, -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPBEQU( 'U', 2, 1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CPBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRPO
*
      END
