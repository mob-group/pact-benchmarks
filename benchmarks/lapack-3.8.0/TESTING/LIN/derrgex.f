*> \brief \b AB_DERRGEX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRGE( PATH, NUNIT )
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
*> AB_DERRGE tests the error exits for the DOUBLE PRECISION routines
*> for general matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_DERRGE.f defines this subroutine.
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
      SUBROUTINE AB_DERRGE( PATH, NUNIT )
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
      CHARACTER          EQ
      CHARACTER*2        C2
      INTEGER            I, INFO, J, N_ERR_BNDS, NPARAMS
      DOUBLE PRECISION   ANRM, CCOND, RCOND, BERR
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   W( LW ), X( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DGBCON, AB_DGBEQU, AB_
     $DGBRFS, AB_DGBTF2,
     $                   AB_DGBTRF, AB_DGBTRS, AB_DGECON, AB_DGEEQU, AB_
     $AB_DGERFS, AB_DGETF2,
     $                   AB_DGETRF, AB_DGETRI, AB_DGETRS, AB_AB_DGEEQUB,
     $ AB_AB_AB_DGERFSX,
     $                   AB_AB_DGBEQUB, AB_AB_DGBRFSX
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
         C( J ) = 0.D0
         R( J ) = 0.D0
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
*        AB_DGETRF
*
         SRNAMT = 'AB_DGETRF'
         INFOT = 1
         CALL AB_DGETRF( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGETRF( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGETRF( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGETRF', INFOT, NOUT, LERR, OK )
*
*        AB_DGETF2
*
         SRNAMT = 'AB_DGETF2'
         INFOT = 1
         CALL AB_DGETF2( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGETF2( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGETF2( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGETF2', INFOT, NOUT, LERR, OK )
*
*        AB_DGETRI
*
         SRNAMT = 'AB_DGETRI'
         INFOT = 1
         CALL AB_DGETRI( -1, A, 1, IP, W, LW, INFO )
         CALL AB_CHKXER( 'AB_DGETRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGETRI( 2, A, 1, IP, W, LW, INFO )
         CALL AB_CHKXER( 'AB_DGETRI', INFOT, NOUT, LERR, OK )
*
*        AB_DGETRS
*
         SRNAMT = 'AB_DGETRS'
         INFOT = 1
         CALL AB_DGETRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGETRS( 'N', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGETRS( 'N', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGETRS( 'N', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_DGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DGETRS( 'N', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGETRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGERFS
*
         SRNAMT = 'AB_AB_DGERFS'
         INFOT = 1
         CALL AB_AB_DGERFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGERFS( 'N', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGERFS( 'N', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DGERFS( 'N', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DGERFS( 'N', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGERFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_DGERFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_AB_DGERFSX'
         INFOT = 1
         CALL AB_AB_AB_DGERFSX( '/', EQ, 0, 0, A, 1, AF, 1, IP, R, C, B,
     $ 1, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         EQ = '/'
         CALL AB_AB_AB_DGERFSX( 'N', EQ, 2, 1, A, 1, AF, 2, IP, R, C, B,
     $ 2, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         EQ = 'R'
         CALL AB_AB_AB_DGERFSX( 'N', EQ, -1, 0, A, 1, AF, 1, IP, R, C, B
     $, 1, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_DGERFSX( 'N', EQ, 0, -1, A, 1, AF, 1, IP, R, C, B
     $, 1, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_DGERFSX( 'N', EQ, 2, 1, A, 1, AF, 2, IP, R, C, B,
     $ 2, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_DGERFSX( 'N', EQ, 2, 1, A, 2, AF, 1, IP, R, C, B,
     $ 2, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'C'
         CALL AB_AB_AB_DGERFSX( 'N', EQ, 2, 1, A, 2, AF, 2, IP, R, C, B,
     $ 1, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_DGERFSX( 'N', EQ, 2, 1, A, 2, AF, 2, IP, R, C, B,
     $ 2, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGERFSX', INFOT, NOUT, LERR, OK )
*
*        AB_DGECON
*
         SRNAMT = 'AB_DGECON'
         INFOT = 1
         CALL AB_DGECON( '/', 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGECON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGECON( '1', -1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGECON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGECON( '1', 2, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGECON', INFOT, NOUT, LERR, OK )
*
*        AB_DGEEQU
*
         SRNAMT = 'AB_DGEEQU'
         INFOT = 1
         CALL AB_DGEEQU( -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEEQU( 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGEEQU( 2, 2, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_DGEEQU', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGEEQUB
*
         SRNAMT = 'AB_AB_DGEEQUB'
         INFOT = 1
         CALL AB_AB_DGEEQUB( -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_DGEEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGEEQUB( 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_DGEEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGEEQUB( 2, 2, A, 1, R1, R2, RCOND, CCOND, ANRM, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_DGEEQUB', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        Test error exits of the routines that use the LU decomposition
*        of a general band matrix.
*
*        AB_DGBTRF
*
         SRNAMT = 'AB_DGBTRF'
         INFOT = 1
         CALL AB_DGBTRF( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGBTRF( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGBTRF( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGBTRF( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGBTRF( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DGBTF2
*
         SRNAMT = 'AB_DGBTF2'
         INFOT = 1
         CALL AB_DGBTF2( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGBTF2( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGBTF2( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGBTF2( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGBTF2( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_DGBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_DGBTRS
*
         SRNAMT = 'AB_DGBTRS'
         INFOT = 1
         CALL AB_DGBTRS( '/', 0, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGBTRS( 'N', -1, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGBTRS( 'N', 1, -1, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGBTRS( 'N', 1, 0, -1, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGBTRS( 'N', 1, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DGBTRS( 'N', 2, 1, 1, 1, A, 3, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_DGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DGBTRS( 'N', 2, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_DGBRFS
*
         SRNAMT = 'AB_DGBRFS'
         INFOT = 1
         CALL AB_DGBRFS( '/', 0, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGBRFS( 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGBRFS( 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGBRFS( 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGBRFS( 'N', 1, 0, 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DGBRFS( 'N', 2, 1, 1, 1, A, 2, AF, 4, IP, B, 2, X, 2, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DGBRFS( 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, B, 2, X, 2, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_DGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 1, X, 2, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_DGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 2, X, 1, R
     $1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DGBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGBRFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_DGBRFSX'
         INFOT = 1
         CALL AB_AB_DGBRFSX( '/', EQ, 0, 0, 0, 0, A, 1, AF, 1, IP, R, C,
     $ B, 1,
     $        X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS,  W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         EQ = '/'
         CALL AB_AB_DGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 1, AF, 2, IP, R, C,
     $ B, 2,
     $        X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         EQ = 'R'
         CALL AB_AB_DGBRFSX( 'N', EQ, -1, 1, 1, 0, A, 1, AF, 1, IP, R, C
     $, B,
     $        1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         EQ = 'R'
         CALL AB_AB_DGBRFSX( 'N', EQ, 2, -1, 1, 1, A, 3, AF, 4, IP, R, C
     $, B,
     $        1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         EQ = 'R'
         CALL AB_AB_DGBRFSX( 'N', EQ, 2, 1, -1, 1, A, 3, AF, 4, IP, R, C
     $, B,
     $        1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DGBRFSX( 'N', EQ, 0, 0, 0, -1, A, 1, AF, 1, IP, R, C
     $, B,
     $        1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 1, AF, 2, IP, R, C,
     $ B,
     $        2, X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 3, AF, 3, IP, R, C,
     $ B, 2,
     $        X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'C'
         CALL AB_AB_DGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 3, AF, 5, IP, R, C,
     $ B,
     $        1, X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_DGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 3, AF, 5, IP, R, C,
     $ B, 2,
     $        X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBRFSX', INFOT, NOUT, LERR, OK )
*
*        AB_DGBCON
*
         SRNAMT = 'AB_DGBCON'
         INFOT = 1
         CALL AB_DGBCON( '/', 0, 0, 0, A, 1, IP, ANRM, RCOND, W, IW, INF
     $O )
         CALL AB_CHKXER( 'AB_DGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGBCON( '1', -1, 0, 0, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGBCON( '1', 1, -1, 0, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGBCON( '1', 1, 0, -1, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGBCON( '1', 2, 1, 1, A, 3, IP, ANRM, RCOND, W, IW, INF
     $O )
         CALL AB_CHKXER( 'AB_DGBCON', INFOT, NOUT, LERR, OK )
*
*        AB_DGBEQU
*
         SRNAMT = 'AB_DGBEQU'
         INFOT = 1
         CALL AB_DGBEQU( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGBEQU( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGBEQU( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGBEQU( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGBEQU( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_DGBEQU', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGBEQUB
*
         SRNAMT = 'AB_AB_DGBEQUB'
         INFOT = 1
         CALL AB_AB_DGBEQUB( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGBEQUB( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGBEQUB( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGBEQUB( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DGBEQUB( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANR
     $M,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGBEQUB', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRGE
*
      END
