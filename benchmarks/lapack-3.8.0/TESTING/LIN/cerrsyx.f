*> \brief \b AB_CERRSYX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRSY( PATH, NUNIT )
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
*> AB_CERRSY tests the error exits for the COMPLEX routines
*> for symmetric indefinite matrices.
*>
*> Note that this file is used only when the XBLAS are available,
*> otherwise AB_CERRSY.f defines this subroutine.
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
      SUBROUTINE AB_CERRSY( PATH, NUNIT )
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
      INTEGER            IP( NMAX )
      REAL               R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   S( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_CSPCON, AB_AB_CSPRFS, 
     $AB_CSPTRF, AB_CSPTRI,
     $                   AB_CSPTRS, AB_CSYCON, AB_AB_CSYRFS, AB_CSYTF2, 
     $AB_CSYTRF, AB_CSYTRI,
     $                   AB_AB_CSYTRI2, AB_CSYTRS, AB_AB_AB_CSYRFSX, AB_
     $AB_CSYCON_ROOK,
     $                   AB_AB_CSYTF2_ROOK, AB_AB_CSYTRF_ROOK, AB_AB_CSY
     $TRI_ROOK,
     $                   AB_AB_CSYTRS_ROOK
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
         B( J ) = 0.E0
         E( J ) = 0.E0
         R1( J ) = 0.E0
         R2( J ) = 0.E0
         W( J ) = 0.E0
         X( J ) = 0.E0
         IP( J ) = J
   20 CONTINUE
      ANRM = 1.0
      OK = .TRUE.

      IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with patrial
*        (Bunch-Kaufman) diagonal pivoting method.
*
*        AB_CSYTRF
*
         SRNAMT = 'AB_CSYTRF'
         INFOT = 1
         CALL AB_CSYTRF( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSYTRF( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CSYTRF( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_CSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CSYTRF( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_CSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CSYTRF( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_CSYTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CSYTF2
*
         SRNAMT = 'AB_CSYTF2'
         INFOT = 1
         CALL AB_CSYTF2( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSYTF2( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CSYTF2( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_CSYTF2', INFOT, NOUT, LERR, OK )
*
*        AB_CSYTRI
*
         SRNAMT = 'AB_CSYTRI'
         INFOT = 1
         CALL AB_CSYTRI( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSYTRI( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CSYTRI( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CSYTRI', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYTRI2
*
         SRNAMT = 'AB_AB_CSYTRI2'
         INFOT = 1
         CALL AB_AB_CSYTRI2( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTRI2( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYTRI2( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI2', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CSYTRI2X
*
         SRNAMT = 'AB_AB_AB_CSYTRI2X'
         INFOT = 1
         CALL AB_AB_AB_CSYTRI2X( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CSYTRI2X( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CSYTRI2X( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYTRI2X', INFOT, NOUT, LERR, OK )
*
*        AB_CSYTRS
*
         SRNAMT = 'AB_CSYTRS'
         INFOT = 1
         CALL AB_CSYTRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSYTRS( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CSYTRS( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CSYTRS( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CSYTRS( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYRFS
*
         SRNAMT = 'AB_AB_CSYRFS'
         INFOT = 1
         CALL AB_AB_CSYRFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYRFS( 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSYRFS( 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CSYRFS( 'U', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CSYRFS( 'U', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYRFS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CSYRFSX
*
         N_ERR_BNDS = 3
         NPARAMS = 0
         SRNAMT = 'AB_AB_AB_CSYRFSX'
         INFOT = 1
         CALL AB_AB_AB_CSYRFSX( '/', EQ, 0, 0, A, 1, AF, 1, IP, S, B, 1,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CSYRFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
         EQ = 'N'
         INFOT = 3
         CALL AB_AB_AB_CSYRFSX( 'U', EQ, -1, 0, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CSYRFSX( 'U', EQ, 0, -1, A, 1, AF, 1, IP, S, B, 1
     $, X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_CSYRFSX( 'U', EQ, 2, 1, A, 1, AF, 2, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_CSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 1, IP, S, B, 2,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_CSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 1,
     $ X, 2,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_CSYRFSX( 'U', EQ, 2, 1, A, 2, AF, 2, IP, S, B, 2,
     $ X, 1,
     $        RCOND, BERR, N_ERR_BNDS, ERR_BNDS_N, ERR_BNDS_C, NPARAMS,
     $        PARAMS, W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYRFSX', INFOT, NOUT, LERR, OK )
*
*        AB_CSYCON
*
         SRNAMT = 'AB_CSYCON'
         INFOT = 1
         CALL AB_CSYCON( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSYCON( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CSYCON( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CSYCON( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CSYCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) diagonal pivoting method.
*
*        AB_AB_CSYTRF_ROOK
*
         SRNAMT = 'AB_AB_CSYTRF_ROOK'
         INFOT = 1
         CALL AB_AB_CSYTRF_ROOK( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTRF_ROOK( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYTRF_ROOK( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CSYTRF_ROOK( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CSYTRF_ROOK( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYTF2_ROOK
*
         SRNAMT = 'AB_AB_CSYTF2_ROOK'
         INFOT = 1
         CALL AB_AB_CSYTF2_ROOK( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTF2_ROOK( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYTF2_ROOK( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTF2_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYTRI_ROOK
*
         SRNAMT = 'AB_AB_CSYTRI_ROOK'
         INFOT = 1
         CALL AB_AB_CSYTRI_ROOK( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTRI_ROOK( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYTRI_ROOK( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYTRS_ROOK
*
         SRNAMT = 'AB_AB_CSYTRS_ROOK'
         INFOT = 1
         CALL AB_AB_CSYTRS_ROOK( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTRS_ROOK( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSYTRS_ROOK( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CSYTRS_ROOK( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSYTRS_ROOK( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYCON_ROOK
*
         SRNAMT = 'AB_AB_CSYCON_ROOK'
         INFOT = 1
         CALL AB_AB_CSYCON_ROOK( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYCON_ROOK( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYCON_ROOK( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CSYCON_ROOK( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CSYCON_ROOK', INFOT, NOUT, LERR, OK )
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
*        AB_AB_CSYTRF_RK
*
         SRNAMT = 'AB_AB_CSYTRF_RK'
         INFOT = 1
         CALL AB_AB_CSYTRF_RK( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTRF_RK( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYTRF_RK( 'U', 2, A, 1, E, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSYTRF_RK( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSYTRF_RK( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRF_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYTF2_RK
*
         SRNAMT = 'AB_AB_CSYTF2_RK'
         INFOT = 1
         CALL AB_AB_CSYTF2_RK( '/', 0, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTF2_RK( 'U', -1, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYTF2_RK( 'U', 2, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTF2_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYTRI_3
*
         SRNAMT = 'AB_AB_CSYTRI_3'
         INFOT = 1
         CALL AB_AB_CSYTRI_3( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTRI_3( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYTRI_3( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSYTRI_3( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSYTRI_3( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRI_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CSYTRI_3X
*
         SRNAMT = 'AB_AB_AB_CSYTRI_3X'
         INFOT = 1
         CALL AB_AB_AB_CSYTRI_3X( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CSYTRI_3X( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CSYTRI_3X( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYTRI_3X', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYTRS_3
*
         SRNAMT = 'AB_AB_CSYTRS_3'
         INFOT = 1
         CALL AB_AB_CSYTRS_3( '/', 0, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYTRS_3( 'U', -1, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSYTRS_3( 'U', 0, -1, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CSYTRS_3( 'U', 2, 1, A, 1, E, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CSYTRS_3( 'U', 2, 1, A, 2, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYTRS_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYCON_3
*
         SRNAMT = 'AB_AB_CSYCON_3'
         INFOT = 1
         CALL AB_AB_CSYCON_3( '/', 0, A, 1,  E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYCON_3( 'U', -1, A, 1, E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYCON_3( 'U', 2, A, 1, E, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CSYCON_3( 'U', 1, A, 1, E, IP, -1.0E0, RCOND, W, INF
     $O)
         CALL AB_CHKXER( 'AB_AB_CSYCON_3', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite packed matrix with patrial
*        (Bunch-Kaufman) diagonal pivoting method.
*
*        AB_CSPTRF
*
         SRNAMT = 'AB_CSPTRF'
         INFOT = 1
         CALL AB_CSPTRF( '/', 0, A, IP, INFO )
         CALL AB_CHKXER( 'AB_CSPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSPTRF( 'U', -1, A, IP, INFO )
         CALL AB_CHKXER( 'AB_CSPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_CSPTRI
*
         SRNAMT = 'AB_CSPTRI'
         INFOT = 1
         CALL AB_CSPTRI( '/', 0, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CSPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSPTRI( 'U', -1, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_CSPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_CSPTRS
*
         SRNAMT = 'AB_CSPTRS'
         INFOT = 1
         CALL AB_CSPTRS( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSPTRS( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CSPTRS( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CSPTRS( 'U', 2, 1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSPRFS
*
         SRNAMT = 'AB_AB_CSPRFS'
         INFOT = 1
         CALL AB_AB_CSPRFS( '/', 0, 0, A, AF, IP, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSPRFS( 'U', -1, 0, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSPRFS( 'U', 0, -1, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSPRFS( 'U', 2, 1, A, AF, IP, B, 1, X, 2, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CSPRFS( 'U', 2, 1, A, AF, IP, B, 2, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CSPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_CSPCON
*
         SRNAMT = 'AB_CSPCON'
         INFOT = 1
         CALL AB_CSPCON( '/', 0, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSPCON( 'U', -1, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CSPCON( 'U', 1, A, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_CSPCON', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRSY
*
      END
