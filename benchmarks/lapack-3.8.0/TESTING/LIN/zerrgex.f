*> \brief \b AB_ZERRGEX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRGE( PATH, NUNIT )
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
*> AB_ZERRGE tests the error exits for the COMPLEX*16 routines
*> for general matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_ZERRGE.f defines this subroutine.
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
      SUBROUTINE AB_ZERRGE( PATH, NUNIT )
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
      CHARACTER          EQ
      CHARACTER*2        C2
      INTEGER            I, INFO, J, N_ERR_BNDS, NPARAMS
      DOUBLE PRECISION   ANRM, CCOND, RCOND, BERR
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   R( NMAX ), R1( NMAX ), R2( NMAX ), CS( NMAX ),
     $                   RS( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZGBCON, AB_ZGBEQU, AB_
     $ZGBRFS, AB_ZGBTF2,
     $                   AB_ZGBTRF, AB_ZGBTRS, AB_ZGECON, AB_ZGEEQU, AB_
     $ZGERFS, AB_ZGETF2,
     $                   AB_ZGETRF, AB_ZGETRI, AB_ZGETRS, AB_AB_ZGEEQUB,
     $ AB_AB_ZGERFSX,
     $                   AB_AB_ZGBEQUB, AB_AB_ZGBRFSX
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
      C2 = PATH( 2: 3 )
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
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         CS( J ) = 0.D0
         RS( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
*     Test error exits of the routines that use the LU decomposition
*     of a general matrix.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        AB_ZGETRF
*
         SRNAMT = 'AB_ZGETRF'
         INFOT = 1
         CALL AB_ZGETRF( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGETRF( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGETRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGETRF( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGETRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZGETF2
*
         SRNAMT = 'AB_ZGETF2'
         INFOT = 1
         CALL AB_ZGETF2( -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGETF2( 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGETF2( 2, 1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGETF2', INFOT, NOUT, LERR, OK )
*
*        AB_ZGETRI
*
         SRNAMT = 'AB_ZGETRI'
         INFOT = 1
         CALL AB_ZGETRI( -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGETRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGETRI( 2, A, 1, IP, W, 2, INFO )
         CALL AB_CHKXER( 'AB_ZGETRI', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGETRI( 2, A, 2, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGETRI', INFOT, NOUT, LERR, OK )
*
*        AB_ZGETRS
*
         SRNAMT = 'AB_ZGETRS'
         INFOT = 1
         CALL AB_ZGETRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGETRS( 'N', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGETRS( 'N', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGETRS( 'N', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZGETRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGETRS( 'N', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGETRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZGERFS
*
         SRNAMT = 'AB_ZGERFS'
         INFOT = 1
         CALL AB_ZGERFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGERFS( 'N', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, R2
     $,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGERFS( 'N', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1, R2
     $,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGERFS( 'N', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGERFS( 'N', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZGERFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZGERFS( 'N', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZGERFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGERFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_ZGERFSX'
         INFOT = 1
         CALL AB_AB_ZGERFSX( '/', EQ, 0, 0, A, 1, AF, 1, IP, RS, CS, B, 
     $1, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         EQ = '/'
         CALL AB_AB_ZGERFSX( 'N', EQ, 2, 1, A, 1, AF, 2, IP, RS, CS, B, 
     $2, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         EQ = 'R'
         CALL AB_AB_ZGERFSX( 'N', EQ, -1, 0, A, 1, AF, 1, IP, RS, CS, B,
     $ 1, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGERFSX( 'N', EQ, 0, -1, A, 1, AF, 1, IP, RS, CS, B,
     $ 1, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGERFSX( 'N', EQ, 2, 1, A, 1, AF, 2, IP, RS, CS, B, 
     $2, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZGERFSX( 'N', EQ, 2, 1, A, 2, AF, 1, IP, RS, CS, B, 
     $2, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'C'
         CALL AB_AB_ZGERFSX( 'N', EQ, 2, 1, A, 2, AF, 2, IP, RS, CS, B, 
     $1, X,
     $        2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGERFSX( 'N', EQ, 2, 1, A, 2, AF, 2, IP, RS, CS, B, 
     $2, X,
     $        1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C,
     $        NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGERFSX', INFOT, NOUT, LERR, OK )
*
*        AB_ZGECON
*
         SRNAMT = 'AB_ZGECON'
         INFOT = 1
         CALL AB_ZGECON( '/', 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGECON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGECON( '1', -1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGECON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGECON( '1', 2, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGECON', INFOT, NOUT, LERR, OK )
*
*        AB_ZGEEQU
*
         SRNAMT = 'AB_ZGEEQU'
         INFOT = 1
         CALL AB_ZGEEQU( -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGEEQU( 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZGEEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGEEQU( 2, 2, A, 1, R1, R2, RCOND, CCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZGEEQU', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGEEQUB
*
         SRNAMT = 'AB_AB_ZGEEQUB'
         INFOT = 1
         CALL AB_AB_ZGEEQUB( -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZGEEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGEEQUB( 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZGEEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGEEQUB( 2, 2, A, 1, R1, R2, RCOND, CCOND, ANRM, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_ZGEEQUB', INFOT, NOUT, LERR, OK )
*
*     Test error exits of the routines that use the LU decomposition
*     of a general band matrix.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        AB_ZGBTRF
*
         SRNAMT = 'AB_ZGBTRF'
         INFOT = 1
         CALL AB_ZGBTRF( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGBTRF( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGBTRF( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGBTRF( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGBTRF( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZGBTF2
*
         SRNAMT = 'AB_ZGBTF2'
         INFOT = 1
         CALL AB_ZGBTF2( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGBTF2( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGBTF2( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGBTF2( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGBTF2( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL AB_CHKXER( 'AB_ZGBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_ZGBTRS
*
         SRNAMT = 'AB_ZGBTRS'
         INFOT = 1
         CALL AB_ZGBTRS( '/', 0, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGBTRS( 'N', -1, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGBTRS( 'N', 1, -1, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGBTRS( 'N', 1, 0, -1, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGBTRS( 'N', 1, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGBTRS( 'N', 2, 1, 1, 1, A, 3, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGBTRS( 'N', 2, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZGBRFS
*
         SRNAMT = 'AB_ZGBRFS'
         INFOT = 1
         CALL AB_ZGBRFS( '/', 0, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGBRFS( 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGBRFS( 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGBRFS( 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGBRFS( 'N', 1, 0, 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, 
     $R1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGBRFS( 'N', 2, 1, 1, 1, A, 2, AF, 4, IP, B, 2, X, 2, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZGBRFS( 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, B, 2, X, 2, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 1, X, 2, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_ZGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 2, X, 1, R
     $1,
     $                R2, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZGBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGBRFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_ZGBRFSX'
         INFOT = 1
         CALL AB_AB_ZGBRFSX( '/', EQ, 0, 0, 0, 0, A, 1, AF, 1, IP, RS, C
     $S, B,
     $                1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS,  W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         EQ = '/'
         CALL AB_AB_ZGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 1, AF, 2, IP, RS, C
     $S, B,
     $                2, X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         EQ = 'R'
         CALL AB_AB_ZGBRFSX( 'N', EQ, -1, 1, 1, 0, A, 1, AF, 1, IP, RS, 
     $CS, B,
     $                1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         EQ = 'R'
         CALL AB_AB_ZGBRFSX( 'N', EQ, 2, -1, 1, 1, A, 3, AF, 4, IP, RS, 
     $CS, B,
     $                1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         EQ = 'R'
         CALL AB_AB_ZGBRFSX( 'N', EQ, 2, 1, -1, 1, A, 3, AF, 4, IP, RS, 
     $CS, B,
     $                1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGBRFSX( 'N', EQ, 0, 0, 0, -1, A, 1, AF, 1, IP, RS, 
     $CS, B,
     $                1, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 1, AF, 2, IP, RS, C
     $S, B,
     $                2, X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 3, AF, 3, IP, RS, C
     $S, B,
     $                2, X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'C'
         CALL AB_AB_ZGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 3, AF, 5, IP, RS, C
     $S, B,
     $                1, X, 2, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGBRFSX( 'N', EQ, 2, 1, 1, 1, A, 3, AF, 5, IP, RS, C
     $S, B,
     $                2, X, 1, RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBRFSX', INFOT, NOUT, LERR, OK )
*
*        AB_ZGBCON
*
         SRNAMT = 'AB_ZGBCON'
         INFOT = 1
         CALL AB_ZGBCON( '/', 0, 0, 0, A, 1, IP, ANRM, RCOND, W, R, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGBCON( '1', -1, 0, 0, A, 1, IP, ANRM, RCOND, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_ZGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGBCON( '1', 1, -1, 0, A, 1, IP, ANRM, RCOND, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_ZGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGBCON( '1', 1, 0, -1, A, 1, IP, ANRM, RCOND, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_ZGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGBCON( '1', 2, 1, 1, A, 3, IP, ANRM, RCOND, W, R, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGBCON', INFOT, NOUT, LERR, OK )
*
*        AB_ZGBEQU
*
         SRNAMT = 'AB_ZGBEQU'
         INFOT = 1
         CALL AB_ZGBEQU( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGBEQU( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGBEQU( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGBEQU( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGBEQU( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGBEQU', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGBEQUB
*
         SRNAMT = 'AB_AB_ZGBEQUB'
         INFOT = 1
         CALL AB_AB_ZGBEQUB( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGBEQUB( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGBEQUB( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGBEQUB( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, AN
     $RM,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGBEQUB( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANR
     $M,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBEQUB', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRGE
*
      END
