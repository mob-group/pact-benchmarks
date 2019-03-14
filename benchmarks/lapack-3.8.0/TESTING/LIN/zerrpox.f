*> \brief \b AB_ZERRPOX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRPO( PATH, NUNIT )
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
*> AB_ZERRPO tests the error exits for the COMPLEX*16 routines
*> for Hermitian positive definite matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_ZERRPO.f defines this subroutine.
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
      SUBROUTINE AB_ZERRPO( PATH, NUNIT )
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
      DOUBLE PRECISION   ANRM, RCOND, BERR
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   S( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   ERR_BNDS_N( NMAX, 3 ), ERR_BNDS_C( NMAX, 3 ),
     $                   PARAMS( 1 )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZPBCON, AB_ZPBEQU, AB_
     $ZPBRFS, AB_ZPBTF2,
     $                   AB_ZPBTRF, AB_ZPBTRS, AB_ZPOCON, AB_ZPOEQU, AB_
     $ZPORFS, AB_ZPOTF2,
     $                   AB_ZPOTRF, AB_ZPOTRI, AB_ZPOTRS, AB_ZPPCON, AB_
     $ZPPEQU, AB_ZPPRFS,
     $                   AB_ZPPTRF, AB_ZPPTRI, AB_ZPPTRS, AB_AB_ZPOEQUB,
     $ AB_AB_ZPORFSX
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
         S( J ) = 0.D0
   20 CONTINUE
      ANRM = 1.D0
      OK = .TRUE.
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite matrix.
*
      IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        AB_ZPOTRF
*
         SRNAMT = 'AB_ZPOTRF'
         INFOT = 1
         CALL AB_ZPOTRF( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPOTRF( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPOTRF( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZPOTF2
*
         SRNAMT = 'AB_ZPOTF2'
         INFOT = 1
         CALL AB_ZPOTF2( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPOTF2( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPOTF2( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTF2', INFOT, NOUT, LERR, OK )
*
*        AB_ZPOTRI
*
         SRNAMT = 'AB_ZPOTRI'
         INFOT = 1
         CALL AB_ZPOTRI( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPOTRI( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPOTRI( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRI', INFOT, NOUT, LERR, OK )
*
*        AB_ZPOTRS
*
         SRNAMT = 'AB_ZPOTRS'
         INFOT = 1
         CALL AB_ZPOTRS( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPOTRS( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPOTRS( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPOTRS( 'U', 2, 1, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZPOTRS( 'U', 2, 1, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZPORFS
*
         SRNAMT = 'AB_ZPORFS'
         INFOT = 1
         CALL AB_ZPORFS( '/', 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPORFS( 'U', -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPORFS( 'U', 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPORFS( 'U', 2, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZPORFS( 'U', 2, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZPORFS( 'U', 2, 1, A, 2, AF, 2, B, 1, X, 2, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZPORFS( 'U', 2, 1, A, 2, AF, 2, B, 2, X, 1, R1, R2, W, 
     $R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPORFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZPORFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_ZPORFSX'
         INFOT = 1
         CALL AB_AB_ZPORFSX( '/', EQ, 0, 0, A, 1, AF, 1, S, B, 1, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZPORFSX( 'U', "/", -1, 0, A, 1, AF, 1, S, B, 1, X, 1
     $,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
         EQ = 'N'
         INFOT = 3
         CALL AB_AB_ZPORFSX( 'U', EQ, -1, 0, A, 1, AF, 1, S, B, 1, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZPORFSX( 'U', EQ, 0, -1, A, 1, AF, 1, S, B, 1, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZPORFSX( 'U', EQ, 2, 1, A, 1, AF, 2, S, B, 2, X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZPORFSX( 'U', EQ, 2, 1, A, 2, AF, 1, S, B, 2, X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZPORFSX( 'U', EQ, 2, 1, A, 2, AF, 2, S, B, 1, X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZPORFSX( 'U', EQ, 2, 1, A, 2, AF, 2, S, B, 2, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPORFSX', INFOT, NOUT, LERR, OK )
*
*        AB_ZPOCON
*
         SRNAMT = 'AB_ZPOCON'
         INFOT = 1
         CALL AB_ZPOCON( '/', 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPOCON( 'U', -1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPOCON( 'U', 2, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPOCON( 'U', 1, A, 1, -ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPOCON', INFOT, NOUT, LERR, OK )
*
*        AB_ZPOEQU
*
         SRNAMT = 'AB_ZPOEQU'
         INFOT = 1
         CALL AB_ZPOEQU( -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPOEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPOEQU( 2, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPOEQU', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZPOEQUB
*
         SRNAMT = 'AB_AB_ZPOEQUB'
         INFOT = 1
         CALL AB_AB_ZPOEQUB( -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZPOEQUB( 2, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOEQUB', INFOT, NOUT, LERR, OK )
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite packed matrix.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        AB_ZPPTRF
*
         SRNAMT = 'AB_ZPPTRF'
         INFOT = 1
         CALL AB_ZPPTRF( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPPTRF( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZPPTRI
*
         SRNAMT = 'AB_ZPPTRI'
         INFOT = 1
         CALL AB_ZPPTRI( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPPTRI( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_ZPPTRS
*
         SRNAMT = 'AB_ZPPTRS'
         INFOT = 1
         CALL AB_ZPPTRS( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPPTRS( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPPTRS( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPPTRS( 'U', 2, 1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZPPRFS
*
         SRNAMT = 'AB_ZPPRFS'
         INFOT = 1
         CALL AB_ZPPRFS( '/', 0, 0, A, AF, B, 1, X, 1, R1, R2, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_ZPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPPRFS( 'U', -1, 0, A, AF, B, 1, X, 1, R1, R2, W, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPPRFS( 'U', 0, -1, A, AF, B, 1, X, 1, R1, R2, W, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZPPRFS( 'U', 2, 1, A, AF, B, 1, X, 2, R1, R2, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_ZPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZPPRFS( 'U', 2, 1, A, AF, B, 2, X, 1, R1, R2, W, R, INF
     $O )
         CALL AB_CHKXER( 'AB_ZPPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZPPCON
*
         SRNAMT = 'AB_ZPPCON'
         INFOT = 1
         CALL AB_ZPPCON( '/', 0, A, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPPCON( 'U', -1, A, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPPCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPPCON( 'U', 1, A, -ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPPCON', INFOT, NOUT, LERR, OK )
*
*        AB_ZPPEQU
*
         SRNAMT = 'AB_ZPPEQU'
         INFOT = 1
         CALL AB_ZPPEQU( '/', 0, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPPEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPPEQU( 'U', -1, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPPEQU', INFOT, NOUT, LERR, OK )
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite band matrix.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        AB_ZPBTRF
*
         SRNAMT = 'AB_ZPBTRF'
         INFOT = 1
         CALL AB_ZPBTRF( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPBTRF( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPBTRF( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPBTRF( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZPBTF2
*
         SRNAMT = 'AB_ZPBTF2'
         INFOT = 1
         CALL AB_ZPBTF2( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPBTF2( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPBTF2( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPBTF2( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_ZPBTRS
*
         SRNAMT = 'AB_ZPBTRS'
         INFOT = 1
         CALL AB_ZPBTRS( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPBTRS( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPBTRS( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPBTRS( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPBTRS( 'U', 2, 1, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZPBTRS( 'U', 2, 0, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_ZPBRFS
*
         SRNAMT = 'AB_ZPBRFS'
         INFOT = 1
         CALL AB_ZPBRFS( '/', 0, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPBRFS( 'U', -1, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPBRFS( 'U', 1, -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPBRFS( 'U', 0, 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPBRFS( 'U', 2, 1, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZPBRFS( 'U', 2, 1, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 1, X, 2, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 2, X, 1, R1, R2, 
     $W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_ZPBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZPBCON
*
         SRNAMT = 'AB_ZPBCON'
         INFOT = 1
         CALL AB_ZPBCON( '/', 0, 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPBCON( 'U', -1, 0, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPBCON( 'U', 1, -1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPBCON( 'U', 2, 1, A, 1, ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPBCON( 'U', 1, 0, A, 1, -ANRM, RCOND, W, R, INFO )
         CALL AB_CHKXER( 'AB_ZPBCON', INFOT, NOUT, LERR, OK )
*
*        AB_ZPBEQU
*
         SRNAMT = 'AB_ZPBEQU'
         INFOT = 1
         CALL AB_ZPBEQU( '/', 0, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPBEQU( 'U', -1, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPBEQU( 'U', 1, -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPBEQU( 'U', 2, 1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_ZPBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRPO
*
      END
