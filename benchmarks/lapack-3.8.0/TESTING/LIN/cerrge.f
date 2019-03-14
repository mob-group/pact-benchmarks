*> \brief \b AB_CERRGE
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRGE( PATH, NUNIT )
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
*> AB_CERRGE tests the error exits for the COMPLEX routines
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CERRGE( PATH, NUNIT )
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
      REAL               ANRM, CCOND, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               R( NMAX ), R1( NMAX ), R2( NMAX )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CGBCON, AB_CGBEQU, AB_CGBRFS, AB_
     $CGBTF2, AB_CGBTRF,
     $                   AB_CGBTRS, AB_CGECON, AB_CGEEQU, AB_CGERFS, AB_
     $CGETF2, AB_CGETRF,
     $                   AB_CGETRI, AB_CGETRS, AB_CHKXER
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
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
*     Test error exits of the routines that use the LU decomposition
*     of a general matrix.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        AB_CGETRF
*
         SRNAMT = 'AB_CGETRF'
         INFOT = 1
         CALL AB_CGETRF( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGETRF( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGETRF( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGETRF', INFOT, NOUT, LERR, OK )
*
*        AB_CGETF2
*
         SRNAMT = 'AB_CGETF2'
         INFOT = 1
         CALL AB_CGETF2( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGETF2( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGETF2( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGETF2', INFOT, NOUT, LERR, OK )
*
*        AB_CGETRI
*
         SRNAMT = 'AB_CGETRI'
         INFOT = 1
         CALL AB_CGETRI( -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGETRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGETRI( 2, A, 1, IP, W, 2, INFO )
         CALL AB_CHKXER( 'AB_CGETRI', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CGETRI( 2, A, 2, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGETRI', INFOT, NOUT, LERR, OK )
*
*        AB_CGETRS
*
         SRNAMT = 'AB_CGETRS'
         INFOT = 1
         CALL AB_CGETRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGETRS( 'N', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGETRS( 'N', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGETRS( 'N', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CGETRS( 'N', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGETRS', INFOT, NOUT, LERR, OK )
*
*        AB_CGERFS
*
         SRNAMT = 'AB_CGERFS'
         INFOT = 1
         CALL AB_CGERFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGERFS( 'N', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, R2
     $,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_CGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGERFS( 'N', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1, R2
     $,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_CGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGERFS( 'N', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGERFS( 'N', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_CGERFS', INFOT, NOUT, LERR, OK )
*
*        AB_CGECON
*
         SRNAMT = 'AB_CGECON'
         INFOT = 1
         CALL AB_CGECON( '/', 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGECON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGECON( '1', -1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGECON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGECON( '1', 2, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGECON', INFOT, NOUT, LERR, OK )
*
*        AB_CGEEQU
*
         SRNAMT = 'AB_CGEEQU'
         INFOT = 1
         CALL AB_CGEEQU( -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGEEQU( 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGEEQU( 2, 2, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_CGEEQU', INFOT, NOUT, LERR, OK )
*
*     Test error exits of the routines that use the LU decomposition
*     of a general band matrix.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        AB_CGBTRF
*
         SRNAMT = 'AB_CGBTRF'
         INFOT = 1
         CALL AB_CGBTRF( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGBTRF( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGBTRF( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGBTRF( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CGBTRF( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CGBTF2
*
         SRNAMT = 'AB_CGBTF2'
         INFOT = 1
         CALL AB_CGBTF2( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGBTF2( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGBTF2( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGBTF2( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CGBTF2( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_CGBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_CGBTRS
*
         SRNAMT = 'AB_CGBTRS'
         INFOT = 1
         CALL AB_CGBTRS( '/', 0, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGBTRS( 'N', -1, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGBTRS( 'N', 1, -1, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGBTRS( 'N', 1, 0, -1, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGBTRS( 'N', 1, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGBTRS( 'N', 2, 1, 1, 1, A, 3, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CGBTRS( 'N', 2, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_CGBRFS
*
         SRNAMT = 'AB_CGBRFS'
         INFOT = 1
         CALL AB_CGBRFS( '/', 0, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGBRFS( 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGBRFS( 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGBRFS( 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGBRFS( 'N', 1, 0, 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGBRFS( 'N', 2, 1, 1, 1, A, 2, AF, 4, IP, B, 2, X, 2, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CGBRFS( 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, B, 2, X, 2, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 1, X, 2, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_CGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 2, X, 1, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_CGBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CGBCON
*
         SRNAMT = 'AB_CGBCON'
         INFOT = 1
         CALL AB_CGBCON( '/', 0, 0, 0, A, 1, IP, ANRM, RCOND, W, R, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGBCON( '1', -1, 0, 0, A, 1, IP, ANRM, RCOND, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGBCON( '1', 1, -1, 0, A, 1, IP, ANRM, RCOND, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGBCON( '1', 1, 0, -1, A, 1, IP, ANRM, RCOND, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CGBCON( '1', 2, 1, 1, A, 3, IP, ANRM, RCOND, W, R, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGBCON', INFOT, NOUT, LERR, OK )
*
*        AB_CGBEQU
*
         SRNAMT = 'AB_CGBEQU'
         INFOT = 1
         CALL AB_CGBEQU( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGBEQU( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGBEQU( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGBEQU( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CGBEQU( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRGE
*
      END
