*> \brief \b AB_SERRPOX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRPO( PATH, NUNIT )
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
*> AB_SERRPO tests the error exits for the REAL routines
*> for symmetric positive definite matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_SERRPO.f defines this subroutine.
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
      SUBROUTINE AB_SERRPO( PATH, NUNIT )
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
      REAL               ANRM, RCOND, BERR
*     ..
*     .. Local Arrays ..
      INTEGER            IW( NMAX )
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   R1( NMAX ), R2( NMAX ), W( 3*NMAX ), X( NMAX ),
     $                   S( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_SPBCON, AB_SPBEQU, AB_
     $SPBRFS, AB_SPBTF2,
     $                   AB_SPBTRF, AB_SPBTRS, AB_SPOCON, AB_SPOEQU, AB_
     $SPORFS, AB_SPOTF2,
     $                   AB_SPOTRF, AB_SPOTRI, AB_SPOTRS, AB_SPPCON, AB_
     $SPPEQU, AB_SPPRFS,
     $                   AB_SPPTRF, AB_SPPTRI, AB_SPPTRS, AB_AB_SPOEQUB,
     $ AB_AB_SPORFSX
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
         S( J ) = 0.
         IW( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        Test error exits of the routines that use the Cholesky
*        decomposition of a symmetric positive definite matrix.
*
*        AB_SPOTRF
*
         SRNAMT = 'AB_SPOTRF'
         INFOT = 1
         CALL AB_SPOTRF( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPOTRF( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPOTRF( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SPOTF2
*
         SRNAMT = 'AB_SPOTF2'
         INFOT = 1
         CALL AB_SPOTF2( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPOTF2( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPOTF2( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTF2', INFOT, NOUT, LERR, OK )
*
*        AB_SPOTRI
*
         SRNAMT = 'AB_SPOTRI'
         INFOT = 1
         CALL AB_SPOTRI( '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPOTRI( 'U', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPOTRI( 'U', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRI', INFOT, NOUT, LERR, OK )
*
*        AB_SPOTRS
*
         SRNAMT = 'AB_SPOTRS'
         INFOT = 1
         CALL AB_SPOTRS( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPOTRS( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPOTRS( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SPOTRS( 'U', 2, 1, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_SPOTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SPOTRS( 'U', 2, 1, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOTRS', INFOT, NOUT, LERR, OK )
*
*        AB_SPORFS
*
         SRNAMT = 'AB_SPORFS'
         INFOT = 1
         CALL AB_SPORFS( '/', 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPORFS( 'U', -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPORFS( 'U', 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SPORFS( 'U', 2, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SPORFS( 'U', 2, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SPORFS( 'U', 2, 1, A, 2, AF, 2, B, 1, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPORFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_SPORFS( 'U', 2, 1, A, 2, AF, 2, B, 2, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPORFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SPORFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_SPORFSX'
         INFOT = 1
         CALL AB_AB_SPORFSX( '/', EQ, 0, 0, A, 1, AF, 1, S, B, 1, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SPORFSX( 'U', "/", -1, 0, A, 1, AF, 1, S, B, 1, X, 1
     $,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
         EQ = 'N'
         INFOT = 3
         CALL AB_AB_SPORFSX( 'U', EQ, -1, 0, A, 1, AF, 1, S, B, 1, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SPORFSX( 'U', EQ, 0, -1, A, 1, AF, 1, S, B, 1, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SPORFSX( 'U', EQ, 2, 1, A, 1, AF, 2, S, B, 2, X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SPORFSX( 'U', EQ, 2, 1, A, 2, AF, 1, S, B, 2, X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SPORFSX( 'U', EQ, 2, 1, A, 2, AF, 2, S, B, 1, X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_SPORFSX( 'U', EQ, 2, 1, A, 2, AF, 2, S, B, 2, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPORFSX', INFOT, NOUT, LERR, OK )
*
*        AB_SPOCON
*
         SRNAMT = 'AB_SPOCON'
         INFOT = 1
         CALL AB_SPOCON( '/', 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPOCON( 'U', -1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPOCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPOCON( 'U', 2, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPOCON', INFOT, NOUT, LERR, OK )
*
*        AB_SPOEQU
*
         SRNAMT = 'AB_SPOEQU'
         INFOT = 1
         CALL AB_SPOEQU( -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPOEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPOEQU( 2, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPOEQU', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SPOEQUB
*
         SRNAMT = 'AB_AB_SPOEQUB'
         INFOT = 1
         CALL AB_AB_SPOEQUB( -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOEQUB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SPOEQUB( 2, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOEQUB', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        Test error exits of the routines that use the Cholesky
*        decomposition of a symmetric positive definite packed matrix.
*
*        AB_SPPTRF
*
         SRNAMT = 'AB_SPPTRF'
         INFOT = 1
         CALL AB_SPPTRF( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_SPPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPPTRF( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_SPPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SPPTRI
*
         SRNAMT = 'AB_SPPTRI'
         INFOT = 1
         CALL AB_SPPTRI( '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_SPPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPPTRI( 'U', -1, A, INFO )
         CALL AB_CHKXER( 'AB_SPPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_SPPTRS
*
         SRNAMT = 'AB_SPPTRS'
         INFOT = 1
         CALL AB_SPPTRS( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPPTRS( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPPTRS( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPPTRS( 'U', 2, 1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_SPPRFS
*
         SRNAMT = 'AB_SPPRFS'
         INFOT = 1
         CALL AB_SPPRFS( '/', 0, 0, A, AF, B, 1, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPPRFS( 'U', -1, 0, A, AF, B, 1, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPPRFS( 'U', 0, -1, A, AF, B, 1, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SPPRFS( 'U', 2, 1, A, AF, B, 1, X, 2, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SPPRFS( 'U', 2, 1, A, AF, B, 2, X, 1, R1, R2, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SPPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_SPPCON
*
         SRNAMT = 'AB_SPPCON'
         INFOT = 1
         CALL AB_SPPCON( '/', 0, A, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPPCON( 'U', -1, A, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPPCON', INFOT, NOUT, LERR, OK )
*
*        AB_SPPEQU
*
         SRNAMT = 'AB_SPPEQU'
         INFOT = 1
         CALL AB_SPPEQU( '/', 0, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPPEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPPEQU( 'U', -1, A, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPPEQU', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        Test error exits of the routines that use the Cholesky
*        decomposition of a symmetric positive definite band matrix.
*
*        AB_SPBTRF
*
         SRNAMT = 'AB_SPBTRF'
         INFOT = 1
         CALL AB_SPBTRF( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPBTRF( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPBTRF( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SPBTRF( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SPBTF2
*
         SRNAMT = 'AB_SPBTF2'
         INFOT = 1
         CALL AB_SPBTF2( '/', 0, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPBTF2( 'U', -1, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPBTF2( 'U', 1, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SPBTF2( 'U', 2, 1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTF2', INFOT, NOUT, LERR, OK )
*
*        AB_SPBTRS
*
         SRNAMT = 'AB_SPBTRS'
         INFOT = 1
         CALL AB_SPBTRS( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPBTRS( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPBTRS( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPBTRS( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPBTRS( 'U', 2, 1, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SPBTRS( 'U', 2, 0, 1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_SPBRFS
*
         SRNAMT = 'AB_SPBRFS'
         INFOT = 1
         CALL AB_SPBRFS( '/', 0, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPBRFS( 'U', -1, 0, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPBRFS( 'U', 1, -1, 0, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPBRFS( 'U', 0, 0, -1, A, 1, AF, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPBRFS( 'U', 2, 1, 1, A, 1, AF, 2, B, 2, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SPBRFS( 'U', 2, 1, 1, A, 2, AF, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 1, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_SPBRFS( 'U', 2, 0, 1, A, 1, AF, 1, B, 2, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_SPBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_SPBCON
*
         SRNAMT = 'AB_SPBCON'
         INFOT = 1
         CALL AB_SPBCON( '/', 0, 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPBCON( 'U', -1, 0, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPBCON( 'U', 1, -1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SPBCON( 'U', 2, 1, A, 1, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SPBCON', INFOT, NOUT, LERR, OK )
*
*        AB_SPBEQU
*
         SRNAMT = 'AB_SPBEQU'
         INFOT = 1
         CALL AB_SPBEQU( '/', 0, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPBEQU( 'U', -1, 0, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPBEQU( 'U', 1, -1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SPBEQU( 'U', 2, 1, A, 1, R1, RCOND, ANRM, INFO )
         CALL AB_CHKXER( 'AB_SPBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRPO
*
      END
