*> \brief \b AB_SERRSYX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRSY( PATH, NUNIT )
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
*> AB_SERRSY tests the error exits for the REAL routines
*> for symmetric indefinite matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_SERRSY.f defines this subroutine.
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
      SUBROUTINE AB_SERRSY( PATH, NUNIT )
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
      INTEGER            IP( NMAX ), IW( NMAX )
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX ), R1( NMAX ), R2( NMAX ), W( 3*NMAX ),
     $                   X( NMAX ), S( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_SSPCON, AB_AB_SSPRFS, 
     $AB_SSPTRF, AB_SSPTRI,
     $                   AB_SSPTRS, AB_SSYCON, AB_AB_SSYCON_3, AB_AB_SSY
     $CON_ROOK, AB_AB_SSYRFS,
     $                   AB_SSYTF2, AB_AB_SSYTF2_RK, AB_AB_SSYTF2_ROOK, 
     $AB_SSYTRF,
     $                   AB_AB_SSYTRF_RK, AB_AB_SSYTRF_ROOK, AB_SSYTRI, 
     $AB_AB_SSYTRI_3,
     $                   AB_AB_AB_SSYTRI_3X, AB_AB_SSYTRI_ROOK, AB_AB_SS
     $YTRI2, AB_AB_AB_SSYTRI2X,
     $                   AB_SSYTRS, AB_AB_SSYTRS_3, AB_AB_SSYTRS_ROOK, A
     $B_AB_AB_SSYRFSX
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
         B( J ) = 0.E+0
         E( J ) = 0.E+0
         R1( J ) = 0.E+0
         R2( J ) = 0.E+0
         W( J ) = 0.E+0
         X( J ) = 0.E+0
         IP( J ) = J
         IW( J ) = J
   20 CONTINUE
      ANRM = 1.0
      RCOND = 1.0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with patrial
*        (Bunch-Kaufman) pivoting.
*
*        AB_SSYTRF
*
         SRNAMT = 'AB_SSYTRF'
         INFOT = 1
         CALL AB_SSYTRF( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRF( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSYTRF( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_SSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSYTRF( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_SSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSYTRF( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_SSYTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SSYTF2
*
         SRNAMT = 'AB_SSYTF2'
         INFOT = 1
         CALL AB_SSYTF2( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTF2( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSYTF2( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_SSYTF2', INFOT, NOUT, LERR, OK )
*
*        AB_SSYTRI
*
         SRNAMT = 'AB_SSYTRI'
         INFOT = 1
         CALL AB_SSYTRI( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_SSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRI( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_SSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSYTRI( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_SSYTRI', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYTRI2
*
         SRNAMT = 'AB_AB_SSYTRI2'
         INFOT = 1
         CALL AB_AB_SSYTRI2( '/', 0, A, 1, IP, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRI2( 'U', -1, A, 1, IP, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYTRI2( 'U', 2, A, 1, IP, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSYTRI', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_SSYTRI2X
*
         SRNAMT = 'AB_AB_AB_SSYTRI2X'
         INFOT = 1
         CALL AB_AB_AB_SSYTRI2X( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_SSYTRI2X( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_SSYTRI2X( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYTRI2X', INFOT, NOUT, LERR, OK )
*
*        AB_SSYTRS
*
         SRNAMT = 'AB_SSYTRS'
         INFOT = 1
         CALL AB_SSYTRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRS( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSYTRS( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SSYTRS( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_SSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SSYTRS( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYRFS
*
         SRNAMT = 'AB_AB_SSYRFS'
         INFOT = 1
         CALL AB_AB_SSYRFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYRFS( 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYRFS( 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYRFS( 'U', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSYRFS( 'U', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYRFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_SSYRFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_AB_SSYRFSX'
         INFOT = 1
         CALL AB_AB_AB_SSYRFSX( '/', EQ, 0, 0, A, 1, AF, 1, IP, S, B, 1,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_SSYRFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
         EQ = 'N'
         INFOT = 3
         CALL AB_AB_AB_SSYRFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_SSYRFSX( 'U', EQ, 0, -1, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_SSYRFSX( 'U', EQ, 2, 1, A, 1, AF, 2, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_SSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 1, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_SSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 1,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_SSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 2,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYRFSX', INFOT, NOUT, LERR, OK )
*
*        AB_SSYCON
*
         SRNAMT = 'AB_SSYCON'
         INFOT = 1
         CALL AB_SSYCON( '/', 0, A, 1, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYCON( 'U', -1, A, 1, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSYCON( 'U', 2, A, 1, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSYCON( 'U', 1, A, 1, IP, -1.0, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSYCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting.
*
*        AB_AB_SSYTRF_ROOK
*
         SRNAMT = 'AB_AB_SSYTRF_ROOK'
         INFOT = 1
         CALL AB_AB_SSYTRF_ROOK( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRF_ROOK( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYTRF_ROOK( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSYTRF_ROOK( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSYTRF_ROOK( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYTF2_ROOK
*
         SRNAMT = 'AB_AB_SSYTF2_ROOK'
         INFOT = 1
         CALL AB_AB_SSYTF2_ROOK( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTF2_ROOK( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYTF2_ROOK( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTF2_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYTRI_ROOK
*
         SRNAMT = 'AB_AB_SSYTRI_ROOK'
         INFOT = 1
         CALL AB_AB_SSYTRI_ROOK( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRI_ROOK( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYTRI_ROOK( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYTRS_ROOK
*
         SRNAMT = 'AB_AB_SSYTRS_ROOK'
         INFOT = 1
         CALL AB_AB_SSYTRS_ROOK( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRS_ROOK( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYTRS_ROOK( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYTRS_ROOK( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYTRS_ROOK( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYCON_ROOK
*
         SRNAMT = 'AB_AB_SSYCON_ROOK'
         INFOT = 1
         CALL AB_AB_SSYCON_ROOK( '/', 0, A, 1, IP, ANRM, RCOND, W, IW, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYCON_ROOK( 'U', -1, A, 1, IP, ANRM, RCOND, W, IW, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYCON_ROOK( 'U', 2, A, 1, IP, ANRM, RCOND, W, IW, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SSYCON_ROOK( 'U', 1, A, 1, IP, -1.0, RCOND, W, IW, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSYCON_ROOK', INFOT, NOUT, LERR, OK )
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
*        AB_AB_SSYTRF_RK
*
         SRNAMT = 'AB_AB_SSYTRF_RK'
         INFOT = 1
         CALL AB_AB_SSYTRF_RK( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRF_RK( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYTRF_RK( 'U', 2, A, 1, E, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYTRF_RK( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYTRF_RK( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRF_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYTF2_RK
*
         SRNAMT = 'AB_AB_SSYTF2_RK'
         INFOT = 1
         CALL AB_AB_SSYTF2_RK( '/', 0, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTF2_RK( 'U', -1, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYTF2_RK( 'U', 2, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTF2_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYTRI_3
*
         SRNAMT = 'AB_AB_SSYTRI_3'
         INFOT = 1
         CALL AB_AB_SSYTRI_3( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRI_3( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYTRI_3( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYTRI_3( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYTRI_3( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRI_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_SSYTRI_3X
*
         SRNAMT = 'AB_AB_AB_SSYTRI_3X'
         INFOT = 1
         CALL AB_AB_AB_SSYTRI_3X( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_SSYTRI_3X( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_SSYTRI_3X( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYTRI_3X', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYTRS_3
*
         SRNAMT = 'AB_AB_SSYTRS_3'
         INFOT = 1
         CALL AB_AB_SSYTRS_3( '/', 0, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRS_3( 'U', -1, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYTRS_3( 'U', 0, -1, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYTRS_3( 'U', 2, 1, A, 1, E, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSYTRS_3( 'U', 2, 1, A, 2, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRS_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYCON_3
*
         SRNAMT = 'AB_AB_SSYCON_3'
         INFOT = 1
         CALL AB_AB_SSYCON_3( '/', 0, A, 1,  E, IP, ANRM, RCOND, W, IW,
     $                   INFO )
         CALL AB_CHKXER( 'AB_AB_SSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYCON_3( 'U', -1, A, 1, E, IP, ANRM, RCOND, W, IW,
     $                   INFO )
         CALL AB_CHKXER( 'AB_AB_SSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYCON_3( 'U', 2, A, 1, E, IP, ANRM, RCOND, W, IW,
     $                   INFO )
         CALL AB_CHKXER( 'AB_AB_SSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSYCON_3( 'U', 1, A, 1, E, IP, -1.0E0, RCOND, W, IW,
     $                   INFO)
         CALL AB_CHKXER( 'AB_AB_SSYCON_3', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite packed matrix with patrial
*        (Bunch-Kaufman) pivoting.
*
*        AB_SSPTRF
*
         SRNAMT = 'AB_SSPTRF'
         INFOT = 1
         CALL AB_SSPTRF( '/', 0, A, IP, INFO )
         CALL AB_CHKXER( 'AB_SSPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSPTRF( 'U', -1, A, IP, INFO )
         CALL AB_CHKXER( 'AB_SSPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_SSPTRI
*
         SRNAMT = 'AB_SSPTRI'
         INFOT = 1
         CALL AB_SSPTRI( '/', 0, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_SSPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSPTRI( 'U', -1, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_SSPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_SSPTRS
*
         SRNAMT = 'AB_SSPTRS'
         INFOT = 1
         CALL AB_SSPTRS( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSPTRS( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSPTRS( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSPTRS( 'U', 2, 1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSPRFS
*
         SRNAMT = 'AB_AB_SSPRFS'
         INFOT = 1
         CALL AB_AB_SSPRFS( '/', 0, 0, A, AF, IP, B, 1, X, 1, R1, R2, W,
     $ IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSPRFS( 'U', -1, 0, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSPRFS( 'U', 0, -1, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSPRFS( 'U', 2, 1, A, AF, IP, B, 1, X, 2, R1, R2, W,
     $ IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSPRFS( 'U', 2, 1, A, AF, IP, B, 2, X, 1, R1, R2, W,
     $ IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_SSPCON
*
         SRNAMT = 'AB_SSPCON'
         INFOT = 1
         CALL AB_SSPCON( '/', 0, A, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSPCON( 'U', -1, A, IP, ANRM, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SSPCON( 'U', 1, A, IP, -1.0, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSPCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRSY
*
      END
