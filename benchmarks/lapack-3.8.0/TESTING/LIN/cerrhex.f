*> \brief \b AB_CERRHEX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRHE( PATH, NUNIT )
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
*> AB_CERRHE tests the error exits for the COMPLEX routines
*> for Hermitian indefinite matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_CERRHE.f defines this subroutine.
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
      SUBROUTINE AB_CERRHE( PATH, NUNIT )
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
      REAL               ANRM, RCOND, BERR
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   S( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX ), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHECON, AB_AB_CHECON_3, AB_AB_CHE
     $CON_ROOK, AB_AB_CHERFS,
     $                   AB_CHETF2, AB_AB_CHETF2_RK, AB_AB_CHETF2_ROOK, 
     $AB_CHETRF,
     $                   AB_AB_CHETRF_RK, AB_AB_CHETRF_ROOK, AB_CHETRI, 
     $AB_AB_CHETRI_3,
     $                   AB_AB_AB_CHETRI_3X, AB_AB_CHETRI_ROOK, AB_AB_CH
     $ETRI2, AB_AB_AB_CHETRI2X,
     $                   AB_CHETRS, AB_AB_CHETRS_3, AB_AB_CHETRS_ROOK, A
     $B_CHKXER, AB_CHPCON,
     $                   AB_AB_CHPRFS, AB_CHPTRF, AB_CHPTRI, AB_CHPTRS, 
     $AB_AB_AB_CHERFSX
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
         B( J ) = 0.E+0
         E( J ) = 0.E+0
         R1( J ) = 0.E+0
         R2( J ) = 0.E+0
         W( J ) = 0.E+0
         X( J ) = 0.E+0
         IP( J ) = J
   20 CONTINUE
      ANRM = 1.0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a Hermitian indefinite matrix with patrial
*        (Bunch-Kaufman) diagonal pivoting method.
*
*        AB_CHETRF
*
         SRNAMT = 'AB_CHETRF'
         INFOT = 1
         CALL AB_CHETRF( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRF( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHETRF( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_CHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CHETRF( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_CHETRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CHETRF( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_CHETRF', INFOT, NOUT, LERR, OK )
*
*        AB_CHETF2
*
         SRNAMT = 'AB_CHETF2'
         INFOT = 1
         CALL AB_CHETF2( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CHETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETF2( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CHETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHETF2( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CHETF2', INFOT, NOUT, LERR, OK )
*
*        AB_CHETRI
*
         SRNAMT = 'AB_CHETRI'
         INFOT = 1
         CALL AB_CHETRI( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CHETRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRI( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CHETRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHETRI( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CHETRI', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHETRI2
*
         SRNAMT = 'AB_AB_CHETRI2'
         INFOT = 1
         CALL AB_AB_CHETRI2( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRI2( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHETRI2( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI2', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CHETRI2X
*
         SRNAMT = 'AB_AB_AB_CHETRI2X'
         INFOT = 1
         CALL AB_AB_AB_CHETRI2X( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHETRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CHETRI2X( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHETRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CHETRI2X( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHETRI2X', INFOT, NOUT, LERR, OK )
*
*        AB_CHETRS
*
         SRNAMT = 'AB_CHETRS'
         INFOT = 1
         CALL AB_CHETRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRS( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHETRS( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CHETRS( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CHETRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CHETRS( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHERFS
*
         SRNAMT = 'AB_AB_CHERFS'
         INFOT = 1
         CALL AB_AB_CHERFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHERFS( 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_CHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHERFS( 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_CHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHERFS( 'U', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHERFS( 'U', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHERFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CHERFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CHERFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CHERFS', INFOT, NOUT, LERR, OK )
*
*        AB_CHECON
*
         SRNAMT = 'AB_CHECON'
         INFOT = 1
         CALL AB_CHECON( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CHECON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHECON( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CHECON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHECON( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CHECON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CHECON( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CHECON', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CHERFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_AB_CHERFSX'
         INFOT = 1
         CALL AB_AB_AB_CHERFSX( '/', EQ, 0, 0, A, 1, AF, 1, IP, S, B, 1,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CHERFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
         EQ = 'N'
         INFOT = 3
         CALL AB_AB_AB_CHERFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CHERFSX( 'U', EQ, 0, -1, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_CHERFSX( 'U', EQ, 2, 1, A, 1, AF, 2, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_CHERFSX( 'U', EQ, 2, 1, A, 2, AF, 1, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_CHERFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 1,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_CHERFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 2,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHERFSX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HR' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a Hermitian indefinite matrix with rook
*        (bounded Bunch-Kaufman) diagonal pivoting method.
*
*        AB_AB_CHETRF_ROOK
*
         SRNAMT = 'AB_AB_CHETRF_ROOK'
         INFOT = 1
         CALL AB_AB_CHETRF_ROOK( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRF_ROOK( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHETRF_ROOK( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHETRF_ROOK( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHETRF_ROOK( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHETF2_ROOK
*
         SRNAMT = 'AB_AB_CHETF2_ROOK'
         INFOT = 1
         CALL AB_AB_CHETF2_ROOK( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETF2_ROOK( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHETF2_ROOK( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETF2_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHETRI_ROOK
*
         SRNAMT = 'AB_AB_CHETRI_ROOK'
         INFOT = 1
         CALL AB_AB_CHETRI_ROOK( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRI_ROOK( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHETRI_ROOK( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHETRS_ROOK
*
         SRNAMT = 'AB_AB_CHETRS_ROOK'
         INFOT = 1
         CALL AB_AB_CHETRS_ROOK( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRS_ROOK( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHETRS_ROOK( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHETRS_ROOK( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHETRS_ROOK( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHECON_ROOK
*
         SRNAMT = 'AB_AB_CHECON_ROOK'
         INFOT = 1
         CALL AB_AB_CHECON_ROOK( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CHECON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHECON_ROOK( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHECON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHECON_ROOK( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CHECON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CHECON_ROOK( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHECON_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HK' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a Hermitian indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
*        AB_AB_CHETRF_RK
*
         SRNAMT = 'AB_AB_CHETRF_RK'
         INFOT = 1
         CALL AB_AB_CHETRF_RK( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRF_RK( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHETRF_RK( 'U', 2, A, 1, E, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHETRF_RK( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHETRF_RK( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRF_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHETF2_RK
*
         SRNAMT = 'AB_AB_CHETF2_RK'
         INFOT = 1
         CALL AB_AB_CHETF2_RK( '/', 0, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETF2_RK( 'U', -1, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHETF2_RK( 'U', 2, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETF2_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHETRI_3
*
         SRNAMT = 'AB_AB_CHETRI_3'
         INFOT = 1
         CALL AB_AB_CHETRI_3( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRI_3( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHETRI_3( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHETRI_3( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHETRI_3( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRI_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CHETRI_3X
*
         SRNAMT = 'AB_AB_AB_CHETRI_3X'
         INFOT = 1
         CALL AB_AB_AB_CHETRI_3X( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHETRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CHETRI_3X( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHETRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CHETRI_3X( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHETRI_3X', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHETRS_3
*
         SRNAMT = 'AB_AB_CHETRS_3'
         INFOT = 1
         CALL AB_AB_CHETRS_3( '/', 0, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRS_3( 'U', -1, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHETRS_3( 'U', 0, -1, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHETRS_3( 'U', 2, 1, A, 1, E, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHETRS_3( 'U', 2, 1, A, 2, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRS_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHECON_3
*
         SRNAMT = 'AB_AB_CHECON_3'
         INFOT = 1
         CALL AB_AB_CHECON_3( '/', 0, A, 1,  E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHECON_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHECON_3( 'U', -1, A, 1, E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHECON_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHECON_3( 'U', 2, A, 1, E, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CHECON_3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHECON_3( 'U', 1, A, 1, E, IP, -1.0E0, RCOND, W, INF
     $O)
         CALL AB_CHKXER( 'AB_AB_CHECON_3', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HP' ) ) THEN
*
*     Test error exits of the routines that use factorization
*     of a Hermitian indefinite packed matrix with patrial
*     (Bunch-Kaufman) diagonal pivoting method.
*
*        AB_CHPTRF
*
         SRNAMT = 'AB_CHPTRF'
         INFOT = 1
         CALL AB_CHPTRF( '/', 0, A, IP, INFO )
         CALL AB_CHKXER( 'AB_CHPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHPTRF( 'U', -1, A, IP, INFO )
         CALL AB_CHKXER( 'AB_CHPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CHPTRI
*
         SRNAMT = 'AB_CHPTRI'
         INFOT = 1
         CALL AB_CHPTRI( '/', 0, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CHPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHPTRI( 'U', -1, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CHPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_CHPTRS
*
         SRNAMT = 'AB_CHPTRS'
         INFOT = 1
         CALL AB_CHPTRS( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHPTRS( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHPTRS( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CHPTRS( 'U', 2, 1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHPRFS
*
         SRNAMT = 'AB_AB_CHPRFS'
         INFOT = 1
         CALL AB_AB_CHPRFS( '/', 0, 0, A, AF, IP, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHPRFS( 'U', -1, 0, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHPRFS( 'U', 0, -1, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHPRFS( 'U', 2, 1, A, AF, IP, B, 1, X, 2, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHPRFS( 'U', 2, 1, A, AF, IP, B, 2, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CHPCON
*
         SRNAMT = 'AB_CHPCON'
         INFOT = 1
         CALL AB_CHPCON( '/', 0, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CHPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHPCON( 'U', -1, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CHPCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CHPCON( 'U', 1, A, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CHPCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRHE
*
      END
