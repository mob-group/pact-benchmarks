*> \brief \b AB_DERRSYX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRSY( PATH, NUNIT )
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
*> AB_DERRSY tests the error exits for the DOUBLE PRECISION routines
*> for symmetric indefinite matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_DERRSY.f defines this subroutine.
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
      SUBROUTINE AB_DERRSY( PATH, NUNIT )
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
      INTEGER            IP( NMAX ), IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX ), R1( NMAX ), R2( NMAX ), W( 3*NMAX ),
     $                   X( NMAX ), S( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DSPCON, AB_AB_DSPRFS, 
     $AB_DSPTRF, AB_DSPTRI,
     $                   AB_DSPTRS, AB_DSYCON, AB_AB_DSYCON_3, AB_AB_DSY
     $CON_ROOK, AB_AB_DSYRFS,
     $                   AB_DSYTF2, AB_AB_DSYTF2_RK, AB_AB_DSYTF2_ROOK, 
     $AB_DSYTRF,
     $                   AB_AB_DSYTRF_RK, AB_AB_DSYTRF_ROOK, AB_DSYTRI, 
     $AB_AB_DSYTRI_3,
     $                   AB_AB_AB_DSYTRI_3X, AB_AB_DSYTRI_ROOK, AB_AB_DS
     $YTRI2, AB_AB_AB_DSYTRI2X,
     $                   AB_DSYTRS, AB_AB_DSYTRS_3, AB_AB_DSYTRS_ROOK, A
     $B_AB_AB_DSYRFSX
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
         E( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         S( J ) = 0.D0
         IP( J ) = J
         IW( J ) = J
   20 CONTINUE
      ANRM = 1.0D0
      RCOND = 1.0D0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with patrial
*        (Bunch-Kaufman) pivoting.
*
*        AB_DSYTRF
*
         SRNAMT = 'AB_DSYTRF'
         INFOT = 1
         CALL AB_DSYTRF( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRF( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSYTRF( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_DSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSYTRF( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_DSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSYTRF( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_DSYTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DSYTF2
*
         SRNAMT = 'AB_DSYTF2'
         INFOT = 1
         CALL AB_DSYTF2( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTF2( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSYTF2( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_DSYTF2', INFOT, NOUT, LERR, OK )
*
*        AB_DSYTRI
*
         SRNAMT = 'AB_DSYTRI'
         INFOT = 1
         CALL AB_DSYTRI( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_DSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRI( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_DSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSYTRI( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_DSYTRI', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYTRI2
*
         SRNAMT = 'AB_AB_DSYTRI2'
         INFOT = 1
         CALL AB_AB_DSYTRI2( '/', 0, A, 1, IP, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRI2( 'U', -1, A, 1, IP, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYTRI2( 'U', 2, A, 1, IP, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI2', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_DSYTRI2X
*
         SRNAMT = 'AB_AB_AB_DSYTRI2X'
         INFOT = 1
         CALL AB_AB_AB_DSYTRI2X( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_DSYTRI2X( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_DSYTRI2X( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYTRI2X', INFOT, NOUT, LERR, OK )
*
*        AB_DSYTRS
*
         SRNAMT = 'AB_DSYTRS'
         INFOT = 1
         CALL AB_DSYTRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRS( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSYTRS( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DSYTRS( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_DSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DSYTRS( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYRFS
*
         SRNAMT = 'AB_AB_DSYRFS'
         INFOT = 1
         CALL AB_AB_DSYRFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYRFS( 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYRFS( 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYRFS( 'U', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSYRFS( 'U', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYRFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_DSYRFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_AB_DSYRFSX'
         INFOT = 1
         CALL AB_AB_AB_DSYRFSX( '/', EQ, 0, 0, A, 1, AF, 1, IP, S, B, 1,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_DSYRFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
         EQ = 'N'
         INFOT = 3
         CALL AB_AB_AB_DSYRFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_DSYRFSX( 'U', EQ, 0, -1, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_DSYRFSX( 'U', EQ, 2, 1, A, 1, AF, 2, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_DSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 1, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_DSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 1,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_DSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 2,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYRFSX', INFOT, NOUT, LERR, OK )
*
*        AB_DSYCON
*
         SRNAMT = 'AB_DSYCON'
         INFOT = 1
         CALL AB_DSYCON( '/', 0, A, 1, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYCON( 'U', -1, A, 1, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSYCON( 'U', 2, A, 1, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSYCON( 'U', 1, A, 1, IP, -1.0D0, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSYCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting.
*
*        AB_AB_DSYTRF_ROOK
*
         SRNAMT = 'AB_AB_DSYTRF_ROOK'
         INFOT = 1
         CALL AB_AB_DSYTRF_ROOK( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRF_ROOK( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYTRF_ROOK( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSYTRF_ROOK( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSYTRF_ROOK( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYTF2_ROOK
*
         SRNAMT = 'AB_AB_DSYTF2_ROOK'
         INFOT = 1
         CALL AB_AB_DSYTF2_ROOK( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTF2_ROOK( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYTF2_ROOK( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTF2_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYTRI_ROOK
*
         SRNAMT = 'AB_AB_DSYTRI_ROOK'
         INFOT = 1
         CALL AB_AB_DSYTRI_ROOK( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRI_ROOK( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYTRI_ROOK( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYTRS_ROOK
*
         SRNAMT = 'AB_AB_DSYTRS_ROOK'
         INFOT = 1
         CALL AB_AB_DSYTRS_ROOK( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRS_ROOK( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYTRS_ROOK( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYTRS_ROOK( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYTRS_ROOK( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYCON_ROOK
*
         SRNAMT = 'AB_AB_DSYCON_ROOK'
         INFOT = 1
         CALL AB_AB_DSYCON_ROOK( '/', 0, A, 1, IP, ANRM, RCOND, W, IW, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYCON_ROOK( 'U', -1, A, 1, IP, ANRM, RCOND, W, IW, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYCON_ROOK( 'U', 2, A, 1, IP, ANRM, RCOND, W, IW, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DSYCON_ROOK( 'U', 1, A, 1, IP, -1.0D0, RCOND, W, IW,
     $ INFO)
         CALL AB_CHKXER( 'AB_AB_DSYCON_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SK' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
*        AB_AB_DSYTRF_RK
*
         SRNAMT = 'AB_AB_DSYTRF_RK'
         INFOT = 1
         CALL AB_AB_DSYTRF_RK( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRF_RK( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYTRF_RK( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYTRF_RK( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYTRF_RK( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRF_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYTF2_RK
*
         SRNAMT = 'AB_AB_DSYTF2_RK'
         INFOT = 1
         CALL AB_AB_DSYTF2_RK( '/', 0, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTF2_RK( 'U', -1, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYTF2_RK( 'U', 2, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTF2_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYTRI_3
*
         SRNAMT = 'AB_AB_DSYTRI_3'
         INFOT = 1
         CALL AB_AB_DSYTRI_3( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRI_3( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYTRI_3( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYTRI_3( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYTRI_3( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRI_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_DSYTRI_3X
*
         SRNAMT = 'AB_AB_AB_DSYTRI_3X'
         INFOT = 1
         CALL AB_AB_AB_DSYTRI_3X( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_DSYTRI_3X( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_DSYTRI_3X( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYTRI_3X', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYTRS_3
*
         SRNAMT = 'AB_AB_DSYTRS_3'
         INFOT = 1
         CALL AB_AB_DSYTRS_3( '/', 0, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRS_3( 'U', -1, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYTRS_3( 'U', 0, -1, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYTRS_3( 'U', 2, 1, A, 1, E, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSYTRS_3( 'U', 2, 1, A, 2, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRS_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYCON_3
*
         SRNAMT = 'AB_AB_DSYCON_3'
         INFOT = 1
         CALL AB_AB_DSYCON_3( '/', 0, A, 1,  E, IP, ANRM, RCOND, W, IW,
     $                   INFO )
         CALL AB_CHKXER( 'AB_AB_DSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYCON_3( 'U', -1, A, 1, E, IP, ANRM, RCOND, W, IW,
     $                   INFO )
         CALL AB_CHKXER( 'AB_AB_DSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYCON_3( 'U', 2, A, 1, E, IP, ANRM, RCOND, W, IW,
     $                   INFO )
         CALL AB_CHKXER( 'AB_AB_DSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSYCON_3( 'U', 1, A, 1, E, IP, -1.0D0, RCOND, W, IW,
     $                   INFO)
         CALL AB_CHKXER( 'AB_AB_DSYCON_3', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite packed matrix with patrial
*        (Bunch-Kaufman) pivoting.
*
*        AB_DSPTRF
*
         SRNAMT = 'AB_DSPTRF'
         INFOT = 1
         CALL AB_DSPTRF( '/', 0, A, IP, INFO )
         CALL AB_CHKXER( 'AB_DSPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSPTRF( 'U', -1, A, IP, INFO )
         CALL AB_CHKXER( 'AB_DSPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_DSPTRI
*
         SRNAMT = 'AB_DSPTRI'
         INFOT = 1
         CALL AB_DSPTRI( '/', 0, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_DSPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSPTRI( 'U', -1, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_DSPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_DSPTRS
*
         SRNAMT = 'AB_DSPTRS'
         INFOT = 1
         CALL AB_DSPTRS( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSPTRS( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSPTRS( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSPTRS( 'U', 2, 1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSPRFS
*
         SRNAMT = 'AB_AB_DSPRFS'
         INFOT = 1
         CALL AB_AB_DSPRFS( '/', 0, 0, A, AF, IP, B, 1, X, 1, R1, R2, W,
     $ IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSPRFS( 'U', -1, 0, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSPRFS( 'U', 0, -1, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSPRFS( 'U', 2, 1, A, AF, IP, B, 1, X, 2, R1, R2, W,
     $ IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSPRFS( 'U', 2, 1, A, AF, IP, B, 2, X, 1, R1, R2, W,
     $ IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_DSPCON
*
         SRNAMT = 'AB_DSPCON'
         INFOT = 1
         CALL AB_DSPCON( '/', 0, A, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSPCON( 'U', -1, A, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DSPCON( 'U', 1, A, IP, -1.0D0, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSPCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRSY
*
      END
