*> \brief \b AB_ZERRHEX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRHE( PATH, NUNIT )
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
*> AB_ZERRHE tests the error exits for the COMPLEX*16 routines
*> for Hermitian indefinite matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_ZERRHE.f defines this subroutine.
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
      SUBROUTINE AB_ZERRHE( PATH, NUNIT )
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
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   S( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX ), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZHECON, AB_AB_ZHECON_3
     $, AB_AB_ZHECON_ROOK,
     $                   AB_AB_ZHERFS, AB_ZHETF2, AB_AB_ZHETF2_RK, AB_AB
     $_ZHETF2_ROOK, AB_ZHETRF,
     $                   AB_AB_ZHETRF_RK, AB_AB_ZHETRF_ROOK, AB_ZHETRI, 
     $AB_AB_ZHETRI_3,
     $                   AB_AB_AB_ZHETRI_3X, AB_AB_ZHETRI_ROOK, AB_AB_ZH
     $ETRI2, AB_AB_AB_ZHETRI2X,
     $                   AB_ZHETRS, AB_AB_ZHETRS_3, AB_AB_ZHETRS_ROOK, A
     $B_ZHPCON,
     $                   AB_AB_ZHPRFS, AB_ZHPTRF, AB_ZHPTRI, AB_ZHPTRS, 
     $AB_AB_AB_ZHERFSX
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
         E( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         S( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      ANRM = 1.0D0
      OK = .TRUE.
*
*     Test error exits of the routines that use factorization
*     of a Hermitian indefinite matrix with patrial
*     (Bunch-Kaufman) diagonal pivoting method.
*
      IF( AB_AB_LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        AB_ZHETRF
*
         SRNAMT = 'AB_ZHETRF'
         INFOT = 1
         CALL AB_ZHETRF( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRF( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHETRF( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_ZHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZHETRF( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_ZHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZHETRF( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_ZHETRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZHETF2
*
         SRNAMT = 'AB_ZHETF2'
         INFOT = 1
         CALL AB_ZHETF2( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZHETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETF2( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZHETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHETF2( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZHETF2', INFOT, NOUT, LERR, OK )
*
*        AB_ZHETRI
*
         SRNAMT = 'AB_ZHETRI'
         INFOT = 1
         CALL AB_ZHETRI( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZHETRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRI( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZHETRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHETRI( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZHETRI', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHETRI2
*
         SRNAMT = 'AB_AB_ZHETRI2'
         INFOT = 1
         CALL AB_AB_ZHETRI2( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRI2( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHETRI2( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI2', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZHETRI2X
*
         SRNAMT = 'AB_AB_AB_ZHETRI2X'
         INFOT = 1
         CALL AB_AB_AB_ZHETRI2X( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHETRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZHETRI2X( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHETRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZHETRI2X( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHETRI2X', INFOT, NOUT, LERR, OK )
*
*        AB_ZHETRS
*
         SRNAMT = 'AB_ZHETRS'
         INFOT = 1
         CALL AB_ZHETRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRS( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHETRS( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZHETRS( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZHETRS( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHERFS
*
         SRNAMT = 'AB_AB_ZHERFS'
         INFOT = 1
         CALL AB_AB_ZHERFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHERFS( 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHERFS( 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHERFS( 'U', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHERFS( 'U', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHERFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZHERFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHERFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZHERFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_AB_ZHERFSX'
         INFOT = 1
         CALL AB_AB_AB_ZHERFSX( '/', EQ, 0, 0, A, 1, AF, 1, IP, S, B, 1,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZHERFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
         EQ = 'N'
         INFOT = 3
         CALL AB_AB_AB_ZHERFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZHERFSX( 'U', EQ, 0, -1, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_ZHERFSX( 'U', EQ, 2, 1, A, 1, AF, 2, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_ZHERFSX( 'U', EQ, 2, 1, A, 2, AF, 1, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_ZHERFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 1,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_ZHERFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 2,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHERFSX', INFOT, NOUT, LERR, OK )
*
*        AB_ZHECON
*
         SRNAMT = 'AB_ZHECON'
         INFOT = 1
         CALL AB_ZHECON( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZHECON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHECON( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZHECON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHECON( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZHECON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZHECON( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZHECON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HR' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a Hermitian indefinite matrix with rook
*        (bounded Bunch-Kaufman) diagonal pivoting method.
*
*        AB_AB_ZHETRF_ROOK
*
         SRNAMT = 'AB_AB_ZHETRF_ROOK'
         INFOT = 1
         CALL AB_AB_ZHETRF_ROOK( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRF_ROOK( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHETRF_ROOK( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHETRF_ROOK( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHETRF_ROOK( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHETF2_ROOK
*
         SRNAMT = 'AB_AB_ZHETF2_ROOK'
         INFOT = 1
         CALL AB_AB_ZHETF2_ROOK( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETF2_ROOK( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHETF2_ROOK( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETF2_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHETRI_ROOK
*
         SRNAMT = 'AB_AB_ZHETRI_ROOK'
         INFOT = 1
         CALL AB_AB_ZHETRI_ROOK( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRI_ROOK( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHETRI_ROOK( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHETRS_ROOK
*
         SRNAMT = 'AB_AB_ZHETRS_ROOK'
         INFOT = 1
         CALL AB_AB_ZHETRS_ROOK( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRS_ROOK( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHETRS_ROOK( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHETRS_ROOK( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHETRS_ROOK( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHECON_ROOK
*
         SRNAMT = 'AB_AB_ZHECON_ROOK'
         INFOT = 1
         CALL AB_AB_ZHECON_ROOK( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZHECON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHECON_ROOK( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHECON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHECON_ROOK( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZHECON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZHECON_ROOK( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHECON_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HK' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
*        AB_AB_ZHETRF_RK
*
         SRNAMT = 'AB_AB_ZHETRF_RK'
         INFOT = 1
         CALL AB_AB_ZHETRF_RK( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRF_RK( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHETRF_RK( 'U', 2, A, 1, E, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHETRF_RK( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHETRF_RK( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRF_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHETF2_RK
*
         SRNAMT = 'AB_AB_ZHETF2_RK'
         INFOT = 1
         CALL AB_AB_ZHETF2_RK( '/', 0, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETF2_RK( 'U', -1, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHETF2_RK( 'U', 2, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETF2_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHETRI_3
*
         SRNAMT = 'AB_AB_ZHETRI_3'
         INFOT = 1
         CALL AB_AB_ZHETRI_3( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRI_3( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHETRI_3( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHETRI_3( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHETRI_3( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRI_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZHETRI_3X
*
         SRNAMT = 'AB_AB_AB_ZHETRI_3X'
         INFOT = 1
         CALL AB_AB_AB_ZHETRI_3X( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHETRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZHETRI_3X( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHETRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZHETRI_3X( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHETRI_3X', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHETRS_3
*
         SRNAMT = 'AB_AB_ZHETRS_3'
         INFOT = 1
         CALL AB_AB_ZHETRS_3( '/', 0, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRS_3( 'U', -1, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHETRS_3( 'U', 0, -1, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHETRS_3( 'U', 2, 1, A, 1, E, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHETRS_3( 'U', 2, 1, A, 2, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRS_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHECON_3
*
         SRNAMT = 'AB_AB_ZHECON_3'
         INFOT = 1
         CALL AB_AB_ZHECON_3( '/', 0, A, 1,  E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHECON_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHECON_3( 'U', -1, A, 1, E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHECON_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHECON_3( 'U', 2, A, 1, E, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZHECON_3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHECON_3( 'U', 1, A, 1, E, IP, -1.0D0, RCOND, W, INF
     $O)
         CALL AB_CHKXER( 'AB_AB_ZHECON_3', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a Hermitian indefinite packed matrix with patrial
*        (Bunch-Kaufman) diagonal pivoting method.
*
*        AB_ZHPTRF
*
         SRNAMT = 'AB_ZHPTRF'
         INFOT = 1
         CALL AB_ZHPTRF( '/', 0, A, IP, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHPTRF( 'U', -1, A, IP, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZHPTRI
*
         SRNAMT = 'AB_ZHPTRI'
         INFOT = 1
         CALL AB_ZHPTRI( '/', 0, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHPTRI( 'U', -1, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_ZHPTRS
*
         SRNAMT = 'AB_ZHPTRS'
         INFOT = 1
         CALL AB_ZHPTRS( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHPTRS( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHPTRS( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZHPTRS( 'U', 2, 1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHPRFS
*
         SRNAMT = 'AB_AB_ZHPRFS'
         INFOT = 1
         CALL AB_AB_ZHPRFS( '/', 0, 0, A, AF, IP, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHPRFS( 'U', -1, 0, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHPRFS( 'U', 0, -1, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHPRFS( 'U', 2, 1, A, AF, IP, B, 1, X, 2, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHPRFS( 'U', 2, 1, A, AF, IP, B, 2, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZHPCON
*
         SRNAMT = 'AB_ZHPCON'
         INFOT = 1
         CALL AB_ZHPCON( '/', 0, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZHPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHPCON( 'U', -1, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZHPCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZHPCON( 'U', 1, A, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZHPCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRHE
*
      END
