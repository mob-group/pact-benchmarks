*> \brief \b AB_ZERRVXX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRVX( PATH, NUNIT )
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
*> AB_ZERRVX tests the error exits for the COMPLEX*16 driver routines
*> for solving linear systems of equations.
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
      SUBROUTINE AB_ZERRVX( PATH, NUNIT )
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
      REAL               ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          EQ
      CHARACTER*2        C2
      INTEGER            I, INFO, J, N_ERR_BNDS, NPARAMS
      DOUBLE PRECISION   RCOND, RPVGRW, BERR
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RF( NMAX ), RW( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX ), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_ZGBSV, AB_AB_ZGBSVX, AB_ZGESV, AB
     $_AB_ZGESVX, AB_ZGTSV,
     $                   AB_AB_ZGTSVX, AB_ZHESV, AB_AB_ZHESV_RK, AB_AB_Z
     $HESV_ROOK, AB_AB_ZHESVX,
     $                   AB_ZHPSV, AB_AB_ZHPSVX, AB_ZPBSV, AB_AB_ZPBSVX,
     $ AB_ZPOSV, AB_AB_ZPOSVX,
     $                   AB_ZPPSV, AB_AB_ZPPSVX, AB_ZPTSV, AB_AB_ZPTSVX,
     $ AB_ZSPSV, AB_AB_ZSPSVX,
     $                   AB_ZSYSV, AB_AB_ZSYSV_RK, AB_AB_ZSYSV_ROOK, AB_
     $AB_ZSYSVX, AB_AB_AB_ZGESVXX,
     $                   AB_AB_AB_ZSYSVXX, AB_AB_AB_ZPOSVXX, AB_AB_AB_ZH
     $ESVXX, AB_AB_AB_ZGBSVXX
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
         C( J ) = 0.D0
         R( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      EQ = ' '
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        AB_ZGESV
*
         SRNAMT = 'AB_ZGESV '
         INFOT = 1
         CALL AB_ZGESV( -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGESV( 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGESV( 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGESV( 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGESV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGESVX
*
         SRNAMT = 'AB_AB_ZGESVX'
         INFOT = 1
         CALL AB_AB_ZGESVX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGESVX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGESVX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGESVX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZGESVX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_ZGESVX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL AB_AB_ZGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL AB_AB_ZGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_ZGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 1,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_ZGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZGESVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_ZGESVXX'
         INFOT = 1
         CALL AB_AB_AB_ZGESVXX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B,
     $                1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZGESVXX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B,
     $                1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_ZGESVXX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R,
     $ C, B,
     $                1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZGESVXX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R,
     $ C, B,
     $                1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_ZGESVXX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, 
     $C, B,
     $                2, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_ZGESVXX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, 
     $C, B,
     $                2, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_AB_ZGESVXX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B,
     $                1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL AB_AB_AB_ZGESVXX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B,
     $                1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL AB_AB_AB_ZGESVXX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B,
     $                1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_ZGESVXX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, 
     $C, B,
     $                1, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_AB_ZGESVXX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, 
     $C, B,
     $                2, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        AB_ZGBSV
*
         SRNAMT = 'AB_ZGBSV '
         INFOT = 1
         CALL AB_ZGBSV( -1, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGBSV( 1, -1, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGBSV( 1, 0, -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGBSV( 0, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGBSV( 1, 1, 1, 0, A, 3, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZGBSV( 2, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGBSVX
*
         SRNAMT = 'AB_AB_ZGBSVX'
         INFOT = 1
         CALL AB_AB_ZGBSVX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGBSVX( 'N', '/', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGBSVX( 'N', 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGBSVX( 'N', 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGBSVX( 'N', 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGBSVX( 'N', 'N', 0, 0, 0, -1, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZGBSVX( 'N', 'N', 1, 1, 1, 0, A, 2, AF, 4, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZGBSVX( 'N', 'N', 1, 1, 1, 0, A, 3, AF, 3, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL AB_AB_ZGBSVX( 'F', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL AB_AB_ZGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL AB_AB_ZGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_ZGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGBSVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZGBSVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_ZGBSVXX'
         INFOT = 1
         CALL AB_AB_AB_ZGBSVXX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZGBSVXX( 'N', '/', 0, 1, 1, 0, A, 1, AF, 1, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', -1, 1, 1, 0, A, 1, AF, 1, IP, 
     $EQ, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', 2, -1, 1, 0, A, 1, AF, 1, IP, 
     $EQ,
     $                R, C, B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', 2, 1, -1, 0, A, 1, AF, 1, IP, 
     $EQ,
     $                R, C, B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', 0, 1, 1, -1, A, 1, AF, 1, IP, 
     $EQ, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 2, AF, 2, IP, E
     $Q, R, C,
     $                B, 2, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, E
     $Q, R, C,
     $                B, 2, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL AB_AB_AB_ZGBSVXX( 'F', 'N', 0, 1, 1, 0, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL AB_AB_AB_ZGBSVXX( 'F', 'N', 1, 1, 1, 0, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL AB_AB_AB_ZGBSVXX( 'F', 'N', 1, 1, 1, 0, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_AB_ZGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 2, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGBSVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        AB_ZGTSV
*
         SRNAMT = 'AB_ZGTSV '
         INFOT = 1
         CALL AB_ZGTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZGTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGTSVX
*
         SRNAMT = 'AB_AB_ZGTSVX'
         INFOT = 1
         CALL AB_AB_ZGTSVX( '/', 'N', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGTSVX( 'N', '/', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGTSVX( 'N', 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGTSVX( 'N', 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_ZGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_ZGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HR' ) ) THEN
*
*        AB_AB_ZHESV_ROOK
*
         SRNAMT = 'AB_AB_ZHESV_ROOK'
         INFOT = 1
         CALL AB_AB_ZHESV_ROOK( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHESV_ROOK( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHESV_ROOK( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHESV_ROOK( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        AB_ZPOSV
*
         SRNAMT = 'AB_ZPOSV '
         INFOT = 1
         CALL AB_ZPOSV( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPOSV( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPOSV( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZPOSV( 'U', 2, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZPOSV( 'U', 2, 0, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPOSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZPOSVX
*
         SRNAMT = 'AB_AB_ZPOSVX'
         INFOT = 1
         CALL AB_AB_ZPOSVX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZPOSVX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZPOSVX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZPOSVX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZPOSVX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZPOSVX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_ZPOSVX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_ZPOSVX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1, X,
     $ 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_ZPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPOSVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZPOSVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_ZPOSVXX'
         INFOT = 1
         CALL AB_AB_AB_ZPOSVXX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZPOSVXX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_ZPOSVXX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 
     $1, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZPOSVXX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 
     $1, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_ZPOSVXX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2
     $, X, 2,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_ZPOSVXX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2
     $, X, 2,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_AB_ZPOSVXX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_AB_ZPOSVXX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_ZPOSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1
     $, X, 2,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_ZPOSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZPOSVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        AB_ZPPSV
*
         SRNAMT = 'AB_ZPPSV '
         INFOT = 1
         CALL AB_ZPPSV( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPPSV( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPPSV( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPPSV( 'U', 2, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZPPSVX
*
         SRNAMT = 'AB_AB_ZPPSVX'
         INFOT = 1
         CALL AB_AB_ZPPSVX( '/', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZPPSVX( 'N', '/', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZPPSVX( 'N', 'U', -1, 0, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZPPSVX( 'N', 'U', 0, -1, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         EQ = '/'
         CALL AB_AB_ZPPSVX( 'F', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         EQ = 'Y'
         CALL AB_AB_ZPPSVX( 'F', 'U', 1, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 1, X, 2, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 2, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        AB_ZPBSV
*
         SRNAMT = 'AB_ZPBSV '
         INFOT = 1
         CALL AB_ZPBSV( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPBSV( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZPBSV( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZPBSV( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPBSV( 'U', 1, 1, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZPBSV( 'U', 2, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZPBSVX
*
         SRNAMT = 'AB_AB_ZPBSVX'
         INFOT = 1
         CALL AB_AB_ZPBSVX( '/', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZPBSVX( 'N', '/', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZPBSVX( 'N', 'U', -1, 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZPBSVX( 'N', 'U', 1, -1, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZPBSVX( 'N', 'U', 0, 0, -1, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZPBSVX( 'N', 'U', 1, 1, 0, A, 1, AF, 2, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZPBSVX( 'N', 'U', 1, 1, 0, A, 2, AF, 1, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_ZPBSVX( 'F', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'Y'
         CALL AB_AB_ZPBSVX( 'F', 'U', 1, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 2,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        AB_ZPTSV
*
         SRNAMT = 'AB_ZPTSV '
         INFOT = 1
         CALL AB_ZPTSV( -1, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPTSV( 0, -1, R, A( 1, 1 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPTSV( 2, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZPTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZPTSVX
*
         SRNAMT = 'AB_AB_ZPTSVX'
         INFOT = 1
         CALL AB_AB_ZPTSVX( '/', 0, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 
     $1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZPTSVX( 'N', -1, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B,
     $ 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZPTSVX( 'N', 0, -1, R, A( 1, 1 ), RF, AF( 1, 1 ), B,
     $ 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 
     $1, X,
     $                2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 
     $2, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZPTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        AB_ZHESV
*
         SRNAMT = 'AB_ZHESV '
         INFOT = 1
         CALL AB_ZHESV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHESV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHESV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZHESV( 'U', 2, 0, A, 1, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZHESV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZHESV( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZHESV( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_ZHESV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHESVX
*
         SRNAMT = 'AB_AB_ZHESVX'
         INFOT = 1
         CALL AB_AB_ZHESVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHESVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHESVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZHESVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZHESVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_ZHESVXX'
         INFOT = 1
         CALL AB_AB_AB_ZHESVXX( '/', 'U', 0, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZHESVXX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_ZHESVXX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, EQ, C,
     $ B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZHESVXX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, EQ, C,
     $ B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_ZHESVXX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, EQ, C, 
     $B, 2, X,
     $                2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_ZHESVXX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, EQ, C, 
     $B, 2, X,
     $                2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C,  NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_AB_ZHESVXX( 'F', 'U', 0, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_AB_ZHESVXX( 'F', 'U', 1, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_ZHESVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, C, 
     $B, 1, X,
     $                2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_ZHESVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, C, 
     $B, 2, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHESVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HR' ) ) THEN
*
*        AB_AB_ZHESV_ROOK
*
         SRNAMT = 'AB_AB_ZHESV_ROOK'
         INFOT = 1
         CALL AB_AB_ZHESV_ROOK( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHESV_ROOK( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHESV_ROOK( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHESV_ROOK( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHESV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHESV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HK' ) ) THEN
*
*        AB_AB_ZSYSV_RK
*
*        Test error exits of the driver that uses factorization
*        of a Hermitian indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
         SRNAMT = 'AB_AB_ZHESV_RK'
         INFOT = 1
         CALL AB_AB_ZHESV_RK( '/', 0, 0, A, 1, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHESV_RK( 'U', -1, 0, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHESV_RK( 'U', 0, -1, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHESV_RK( 'U', 2, 0, A, 1, E, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHESV_RK( 'U', 2, 0, A, 2, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHESV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHESV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, -2, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZHESV_RK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        AB_ZHPSV
*
         SRNAMT = 'AB_ZHPSV '
         INFOT = 1
         CALL AB_ZHPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZHPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZHPSVX
*
         SRNAMT = 'AB_AB_ZHPSVX'
         INFOT = 1
         CALL AB_AB_ZHPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        AB_ZSYSV
*
         SRNAMT = 'AB_ZSYSV '
         INFOT = 1
         CALL AB_ZSYSV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSYSV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZSYSV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZSYSV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_ZSYSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYSVX
*
         SRNAMT = 'AB_AB_ZSYSVX'
         INFOT = 1
         CALL AB_AB_ZSYSVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYSVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSYSVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYSVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZSYSVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZSYSVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_ZSYSVXX'
         INFOT = 1
         EQ = 'N'
         CALL AB_AB_AB_ZSYSVXX( '/', 'U', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZSYSVXX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_ZSYSVXX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, EQ, R,
     $ B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         EQ = '/'
         CALL AB_AB_AB_ZSYSVXX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, EQ, R,
     $ B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         EQ = 'Y'
         INFOT = 6
         CALL AB_AB_AB_ZSYSVXX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_ZSYSVXX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_ZSYSVXX( 'F', 'U', 2, 0, A, 2, AF, 2, IP, 'A', R,
     $ B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ='Y'
         CALL AB_AB_AB_ZSYSVXX( 'F', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ='Y'
         R(1) = -ONE
         CALL AB_AB_AB_ZSYSVXX( 'F', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
	     INFOT = 13
         EQ = 'N'
         CALL AB_AB_AB_ZSYSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 1, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_ZSYSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYSVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        AB_AB_ZSYSV_ROOK
*
         SRNAMT = 'AB_AB_ZSYSV_ROOK'
         INFOT = 1
         CALL AB_AB_ZSYSV_ROOK( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYSV_ROOK( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSYSV_ROOK( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYSV_ROOK( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SK' ) ) THEN
*
*        AB_AB_ZSYSV_RK
*
*        Test error exits of the driver that uses factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
         SRNAMT = 'AB_AB_ZSYSV_RK'
         INFOT = 1
         CALL AB_AB_ZSYSV_RK( '/', 0, 0, A, 1, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYSV_RK( 'U', -1, 0, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSYSV_RK( 'U', 0, -1, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZSYSV_RK( 'U', 2, 0, A, 1, E, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZSYSV_RK( 'U', 2, 0, A, 2, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, -2, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZSYSV_RK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        AB_ZSPSV
*
         SRNAMT = 'AB_ZSPSV '
         INFOT = 1
         CALL AB_ZSPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZSPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZSPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSPSVX
*
         SRNAMT = 'AB_AB_ZSPSVX'
         INFOT = 1
         CALL AB_AB_ZSPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPSVX', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' drivers passed the tests of the error exits' )
 9998 FORMAT( ' *** ', A3, ' drivers failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of AB_ZERRVX
*
      END
