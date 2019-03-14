*> \brief \b AB_DERRPO
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRPO( PATH, NUNIT )
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
*> AB_DERRPO tests the error exits for the DOUBLE PRECISION routines
*> for symmetric positive definite matrices.
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
      SUBROUTINE AB_DERRPO( PATH, NUNIT )
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
      DOUBLE PRECISION   ANRM, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   R1( NMAX ), R2( NMAX ), W( 3*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DPBCON, AB_DPBEQU, AB_
     $DPBRFS, AB_DPBTF2,
     $                   AB_DPBTRF, AB_DPBTRS, AB_DPOCON, AB_DPOEQU, AB_
     $DPORFS, AB_DPOTF2,
     $                   AB_DPOTRF, AB_DPOTRI, AB_DPOTRS, AB_DPPCON, AB_
     $DPPEQU, AB_DPPRFS,
     $                   AB_DPPTRF, AB_DPPTRI, AB_DPPTRS
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
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
            AF( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
         B( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         IW( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        Test error exits of the routines that use the Cholesky
*        decomposition of a symmetric positive definite matrix.
*
*        AB_DPOTRF
*
         SRNAMT = 'AB_DPOTRF'
         INFOT = 1
         CALL AB_DPOTRF( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPOTRF( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPOTRF( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DPOTF2
*
         SRNAMT = 'AB_DPOTF2'
         INFOT = 1
         CALL AB_DPOTF2( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPOTF2( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPOTF2( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTF2', INFOT, NOUT, LERR, OK )
*
*        AB_DPOTRI
*
         SRNAMT = 'AB_DPOTRI'
         INFOT = 1
         CALL AB_DPOTRI( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPOTRI( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPOTRI( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRI', INFOT, NOUT, LERR, OK )
*
*        AB_DPOTRS
*
         SRNAMT = 'AB_DPOTRS'
         INFOT = 1
         CALL AB_DPOTRS( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPOTRS( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPOTRS( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DPOTRS( 'U', 2, 1, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_DPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DPOTRS( 'U', 2, 1, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DPORFS
*
         SRNAMT = 'AB_DPORFS'
         INFOT = 1
         CALL AB_DPORFS( '/', 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPORFS( 'U', -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPORFS( 'U', 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DPORFS( 'U', 2, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DPORFS( 'U', 2, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DPORFS( 'U', 2, 1, A, 2, AF, 2, B, 1, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DPORFS( 'U', 2, 1, A, 2, AF, 2, B, 2, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPORFS', INFOT, NOUT, LERR, OK )
*
*        AB_DPOCON
*
         SRNAMT = 'AB_DPOCON'
         INFOT = 1
         CALL AB_DPOCON( '/', 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPOCON( 'U', -1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPOCON( 'U', 2, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPOCON', INFOT, NOUT, LERR, OK )
*
*        AB_DPOEQU
*
         SRNAMT = 'AB_DPOEQU'
         INFOT = 1
         CALL AB_DPOEQU( -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPOEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPOEQU( 2, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPOEQU', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        Test error exits of the routines that use the Cholesky
*        decomposition of a symmetric positive definite packed matrix.
*
*        AB_DPPTRF
*
         SRNAMT = 'AB_DPPTRF'
         INFOT = 1
         CALL AB_DPPTRF( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_DPPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPPTRF( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_DPPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DPPTRI
*
         SRNAMT = 'AB_DPPTRI'
         INFOT = 1
         CALL AB_DPPTRI( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_DPPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPPTRI( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_DPPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_DPPTRS
*
         SRNAMT = 'AB_DPPTRS'
         INFOT = 1
         CALL AB_DPPTRS( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPPTRS( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPPTRS( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPPTRS( 'U', 2, 1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DPPRFS
*
         SRNAMT = 'AB_DPPRFS'
         INFOT = 1
         CALL AB_DPPRFS( '/', 0, 0, A, AF, B, 1, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPPRFS( 'U', -1, 0, A, AF, B, 1, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPPRFS( 'U', 0, -1, A, AF, B, 1, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DPPRFS( 'U', 2, 1, A, AF, B, 1, X, 2, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DPPRFS( 'U', 2, 1, A, AF, B, 2, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DPPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DPPCON
*
         SRNAMT = 'AB_DPPCON'
         INFOT = 1
         CALL AB_DPPCON( '/', 0, A, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPPCON( 'U', -1, A, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPPCON', INFOT, NOUT, LERR, OK )
*
*        AB_DPPEQU
*
         SRNAMT = 'AB_DPPEQU'
         INFOT = 1
         CALL AB_DPPEQU( '/', 0, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPPEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPPEQU( 'U', -1, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPPEQU', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        Test error exits of the routines that use the Cholesky
*        decomposition of a symmetric positive definite band matrix.
*
*        AB_DPBTRF
*
         SRNAMT = 'AB_DPBTRF'
         INFOT = 1
         CALL AB_DPBTRF( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPBTRF( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPBTRF( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DPBTRF( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DPBTF2
*
         SRNAMT = 'AB_DPBTF2'
         INFOT = 1
         CALL AB_DPBTF2( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPBTF2( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPBTF2( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DPBTF2( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_DPBTRS
*
         SRNAMT = 'AB_DPBTRS'
         INFOT = 1
         CALL AB_DPBTRS( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPBTRS( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPBTRS( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPBTRS( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPBTRS( 'U', 2, 1, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DPBTRS( 'U', 2, 0, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DPBRFS
*
         SRNAMT = 'AB_DPBRFS'
         INFOT = 1
         CALL AB_DPBRFS( '/', 0, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPBRFS( 'U', -1, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPBRFS( 'U', 1, -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPBRFS( 'U', 0, 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPBRFS( 'U', 2, 1, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DPBRFS( 'U', 2, 1, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 1, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_DPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 2, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_DPBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DPBCON
*
         SRNAMT = 'AB_DPBCON'
         INFOT = 1
         CALL AB_DPBCON( '/', 0, 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPBCON( 'U', -1, 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPBCON( 'U', 1, -1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DPBCON( 'U', 2, 1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DPBCON', INFOT, NOUT, LERR, OK )
*
*        AB_DPBEQU
*
         SRNAMT = 'AB_DPBEQU'
         INFOT = 1
         CALL AB_DPBEQU( '/', 0, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPBEQU( 'U', -1, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPBEQU( 'U', 1, -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DPBEQU( 'U', 2, 1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DPBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRPO
*
      END
