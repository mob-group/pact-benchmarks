*> \brief \b AB_SERRGE
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRGE( PATH, NUNIT )
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
*> AB_SERRGE tests the error exits for the REAL routines
*> for general matrices.
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
      SUBROUTINE AB_SERRGE( PATH, NUNIT )
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
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 4, LW = 3*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J
      REAL               ANRM, CCOND, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX )
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   R1( NMAX ), R2( NMAX ), W( LW ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_SGBCON, AB_SGBEQU, AB_
     $SGBRFS, AB_SGBTF2,
     $                   AB_SGBTRF, AB_SGBTRS, AB_SGECON, AB_SGEEQU, AB_
     $AB_SGERFS, AB_SGETF2,
     $                   AB_SGETRF, AB_SGETRI, AB_SGETRS
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
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1. / REAL( I+J )
            AF( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
         B( J ) = 0.
         R1( J ) = 0.
         R2( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
         IP( J ) = J
         IW( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        Test error exits of the routines that use the LU decomposition
*        of a general matrix.
*
*        AB_SGETRF
*
         SRNAMT = 'AB_SGETRF'
         INFOT = 1
         CALL AB_SGETRF( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGETRF( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGETRF( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGETRF', INFOT, NOUT, LERR, OK )
*
*        AB_SGETF2
*
         SRNAMT = 'AB_SGETF2'
         INFOT = 1
         CALL AB_SGETF2( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGETF2( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGETF2( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGETF2', INFOT, NOUT, LERR, OK )
*
*        AB_SGETRI
*
         SRNAMT = 'AB_SGETRI'
         INFOT = 1
         CALL AB_SGETRI( -1, A, 1, IP, W, LW, INFO )
         CALL AB_CHKXER( 'AB_SGETRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGETRI( 2, A, 1, IP, W, LW, INFO )
         CALL AB_CHKXER( 'AB_SGETRI', INFOT, NOUT, LERR, OK )
*
*        AB_SGETRS
*
         SRNAMT = 'AB_SGETRS'
         INFOT = 1
         CALL AB_SGETRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGETRS( 'N', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGETRS( 'N', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SGETRS( 'N', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_SGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SGETRS( 'N', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGETRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SGERFS
*
         SRNAMT = 'AB_AB_SGERFS'
         INFOT = 1
         CALL AB_AB_SGERFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SGERFS( 'N', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SGERFS( 'N', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SGERFS( 'N', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SGERFS( 'N', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGERFS', INFOT, NOUT, LERR, OK )
*
*        AB_SGECON
*
         SRNAMT = 'AB_SGECON'
         INFOT = 1
         CALL AB_SGECON( '/', 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGECON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGECON( '1', -1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGECON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGECON( '1', 2, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGECON', INFOT, NOUT, LERR, OK )
*
*        AB_SGEEQU
*
         SRNAMT = 'AB_SGEEQU'
         INFOT = 1
         CALL AB_SGEEQU( -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGEEQU( 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGEEQU( 2, 2, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SGEEQU', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        Test error exits of the routines that use the LU decomposition
*        of a general band matrix.
*
*        AB_SGBTRF
*
         SRNAMT = 'AB_SGBTRF'
         INFOT = 1
         CALL AB_SGBTRF( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGBTRF( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGBTRF( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGBTRF( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SGBTRF( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SGBTF2
*
         SRNAMT = 'AB_SGBTF2'
         INFOT = 1
         CALL AB_SGBTF2( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGBTF2( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGBTF2( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGBTF2( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SGBTF2( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_SGBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_SGBTRS
*
         SRNAMT = 'AB_SGBTRS'
         INFOT = 1
         CALL AB_SGBTRS( '/', 0, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGBTRS( 'N', -1, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGBTRS( 'N', 1, -1, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGBTRS( 'N', 1, 0, -1, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SGBTRS( 'N', 1, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SGBTRS( 'N', 2, 1, 1, 1, A, 3, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SGBTRS( 'N', 2, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_SGBRFS
*
         SRNAMT = 'AB_SGBRFS'
         INFOT = 1
         CALL AB_SGBRFS( '/', 0, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGBRFS( 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGBRFS( 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGBRFS( 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SGBRFS( 'N', 1, 0, 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SGBRFS( 'N', 2, 1, 1, 1, A, 2, AF, 4, IP, B, 2, X, 2, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SGBRFS( 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, B, 2, X, 2, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_SGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 1, X, 2, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_SGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 2, X, 1, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SGBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_SGBCON
*
         SRNAMT = 'AB_SGBCON'
         INFOT = 1
         CALL AB_SGBCON( '/', 0, 0, 0, A, 1, IP, ANRM, RCOND, W, IW, INF
     $O )
         CALL AB_CHKXER( 'AB_SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGBCON( '1', -1, 0, 0, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGBCON( '1', 1, -1, 0, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGBCON( '1', 1, 0, -1, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SGBCON( '1', 2, 1, 1, A, 3, IP, ANRM, RCOND, W, IW, INF
     $O )
         CALL AB_CHKXER( 'AB_SGBCON', INFOT, NOUT, LERR, OK )
*
*        AB_SGBEQU
*
         SRNAMT = 'AB_SGBEQU'
         INFOT = 1
         CALL AB_SGBEQU( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGBEQU( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGBEQU( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGBEQU( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SGBEQU( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_SGBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRGE
*
      END
