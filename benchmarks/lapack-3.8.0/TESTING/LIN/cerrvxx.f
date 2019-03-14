*> \brief \b AB_CERRVXX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRVX( PATH, NUNIT )
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
*> AB_CERRVX tests the error exits for the COMPLEX driver routines
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CERRVX( PATH, NUNIT )
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
      PARAMETER          ( ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          EQ
      CHARACTER*2        C2
      INTEGER            I, INFO, J, N_ERR_BNDS, NPARAMS
      REAL               RCOND, RPVGRW, BERR
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RF( NMAX ), RW( NMAX ), ERR_BNDS_N( NMAX, 3 ),
     $                   ERR_BNDS_C( NMAX, 3 ), PARAMS( 1 )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX ), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGBSV, AB_AB_CGBSVX, AB_CGESV, AB_AB_CGESVX,
     $ AB_CGTSV, AB_AB_CGTSVX,
     $                   AB_CHESV, AB_AB_CHESV_RK, AB_AB_CHESV_ROOK, AB_
     $AB_CHESVX, AB_CHKXER,
     $                   AB_CHPSV, AB_AB_CHPSVX, AB_CPBSV, AB_AB_CPBSVX,
     $ AB_CPOSV, AB_AB_CPOSVX,
     $                   AB_CPPSV, AB_AB_CPPSVX, AB_CPTSV, AB_AB_CPTSVX,
     $ AB_CSPSV, AB_AB_CSPSVX,
     $                   AB_CSYSV, AB_AB_CSYSV_RK, AB_AB_CSYSV_ROOK, AB_
     $AB_CSYSVX, AB_AB_AB_CGESVXX,
     $                   AB_AB_AB_CPOSVXX, AB_AB_AB_CSYSVXX, AB_AB_AB_CH
     $ESVXX, AB_AB_AB_CGBSVXX
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
         E( J ) = 0E+0
         R1( J ) = 0.E+0
         R2( J ) = 0.E+0
         W( J ) = 0.E+0
         X( J ) = 0.E+0
         C( J ) = 0.E+0
         R( J ) = 0.E+0
         IP( J ) = J
   20 CONTINUE
      EQ = ' '
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        AB_CGESV
*
         SRNAMT = 'AB_CGESV '
         INFOT = 1
         CALL AB_CGESV( -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGESV( 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGESV( 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGESV( 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGESV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CGESVX
*
         SRNAMT = 'AB_AB_CGESVX'
         INFOT = 1
         CALL AB_AB_CGESVX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGESVX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CGESVX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CGESVX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CGESVX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_CGESVX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL AB_AB_CGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL AB_AB_CGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_CGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 1,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_CGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGESVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CGESVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_CGESVXX'
         INFOT = 1
         CALL AB_AB_AB_CGESVXX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B, 1,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CGESVXX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B, 1,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_CGESVXX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R,
     $ C, B, 1,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CGESVXX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R,
     $ C, B, 1,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_CGESVXX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, 
     $C, B, 2,
     $                X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_CGESVXX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, 
     $C, B, 2,
     $                X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_AB_CGESVXX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B, 1,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL AB_AB_AB_CGESVXX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B, 1,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL AB_AB_AB_CGESVXX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, 
     $C, B, 1,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_CGESVXX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, 
     $C, B, 1,
     $                X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_AB_CGESVXX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, 
     $C, B, 2,
     $                X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGESVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        AB_CGBSV
*
         SRNAMT = 'AB_CGBSV '
         INFOT = 1
         CALL AB_CGBSV( -1, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGBSV( 1, -1, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGBSV( 1, 0, -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGBSV( 0, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CGBSV( 1, 1, 1, 0, A, 3, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CGBSV( 2, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CGBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CGBSVX
*
         SRNAMT = 'AB_AB_CGBSVX'
         INFOT = 1
         CALL AB_AB_CGBSVX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGBSVX( 'N', '/', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGBSVX( 'N', 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CGBSVX( 'N', 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGBSVX( 'N', 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CGBSVX( 'N', 'N', 0, 0, 0, -1, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CGBSVX( 'N', 'N', 1, 1, 1, 0, A, 2, AF, 4, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CGBSVX( 'N', 'N', 1, 1, 1, 0, A, 3, AF, 3, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL AB_AB_CGBSVX( 'F', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL AB_AB_CGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL AB_AB_CGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_CGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGBSVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CGBSVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_CGBSVXX'
         INFOT = 1
         CALL AB_AB_AB_CGBSVXX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CGBSVXX( 'N', '/', 0, 1, 1, 0, A, 1, AF, 1, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', -1, 1, 1, 0, A, 1, AF, 1, IP, 
     $EQ, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', 2, -1, 1, 0, A, 1, AF, 1, IP, 
     $EQ,
     $                R, C, B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', 2, 1, -1, 0, A, 1, AF, 1, IP, 
     $EQ,
     $                R, C, B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', 0, 1, 1, -1, A, 1, AF, 1, IP, 
     $EQ, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 2, AF, 2, IP, E
     $Q, R, C,
     $                B, 2, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, E
     $Q, R, C,
     $                B, 2, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL AB_AB_AB_CGBSVXX( 'F', 'N', 0, 1, 1, 0, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL AB_AB_AB_CGBSVXX( 'F', 'N', 1, 1, 1, 0, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL AB_AB_AB_CGBSVXX( 'F', 'N', 1, 1, 1, 0, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 1, X, 2, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_AB_CGBSVXX( 'N', 'N', 2, 1, 1, 1, A, 3, AF, 4, IP, E
     $Q, R, C,
     $                B, 2, X, 1, RCOND, RPVGRW, BERR, N_ERR_BNDS,
     $                ERR_BNDS_N, ERR_BNDS_C, NPARAMS, PARAMS, W, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CGBSVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        AB_CGTSV
*
         SRNAMT = 'AB_CGTSV '
         INFOT = 1
         CALL AB_CGTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_CGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_CGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CGTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CGTSVX
*
         SRNAMT = 'AB_AB_CGTSVX'
         INFOT = 1
         CALL AB_AB_CGTSVX( '/', 'N', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGTSVX( 'N', '/', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGTSVX( 'N', 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CGTSVX( 'N', 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_CGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_CGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        AB_CPOSV
*
         SRNAMT = 'AB_CPOSV '
         INFOT = 1
         CALL AB_CPOSV( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPOSV( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPOSV( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CPOSV( 'U', 2, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CPOSV( 'U', 2, 0, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPOSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CPOSVX
*
         SRNAMT = 'AB_AB_CPOSVX'
         INFOT = 1
         CALL AB_AB_CPOSVX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CPOSVX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CPOSVX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CPOSVX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CPOSVX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CPOSVX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_CPOSVX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_CPOSVX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1, X,
     $ 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_CPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2, X,
     $ 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPOSVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CPOSVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_CPOSVXX'
         INFOT = 1
         CALL AB_AB_AB_CPOSVXX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CPOSVXX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_CPOSVXX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 
     $1, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CPOSVXX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 
     $1, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_CPOSVXX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2
     $, X, 2,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_CPOSVXX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2
     $, X, 2,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_AB_CPOSVXX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_AB_CPOSVXX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_CPOSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1
     $, X, 2,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_CPOSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2
     $, X, 1,
     $                RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CPOSVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        AB_CPPSV
*
         SRNAMT = 'AB_CPPSV '
         INFOT = 1
         CALL AB_CPPSV( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPPSV( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPPSV( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPPSV( 'U', 2, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CPPSVX
*
         SRNAMT = 'AB_AB_CPPSVX'
         INFOT = 1
         CALL AB_AB_CPPSVX( '/', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CPPSVX( 'N', '/', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CPPSVX( 'N', 'U', -1, 0, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CPPSVX( 'N', 'U', 0, -1, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         EQ = '/'
         CALL AB_AB_CPPSVX( 'F', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         EQ = 'Y'
         CALL AB_AB_CPPSVX( 'F', 'U', 1, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 1, X, 2, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 2, X, 1, RC
     $OND,
     $                R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        AB_CPBSV
*
         SRNAMT = 'AB_CPBSV '
         INFOT = 1
         CALL AB_CPBSV( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPBSV( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CPBSV( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CPBSV( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPBSV( 'U', 1, 1, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CPBSV( 'U', 2, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CPBSVX
*
         SRNAMT = 'AB_AB_CPBSVX'
         INFOT = 1
         CALL AB_AB_CPBSVX( '/', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CPBSVX( 'N', '/', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CPBSVX( 'N', 'U', -1, 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CPBSVX( 'N', 'U', 1, -1, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CPBSVX( 'N', 'U', 0, 0, -1, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CPBSVX( 'N', 'U', 1, 1, 0, A, 1, AF, 2, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CPBSVX( 'N', 'U', 1, 1, 0, A, 2, AF, 1, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_CPBSVX( 'F', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'Y'
         CALL AB_AB_CPBSVX( 'F', 'U', 1, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 2,
     $ X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        AB_CPTSV
*
         SRNAMT = 'AB_CPTSV '
         INFOT = 1
         CALL AB_CPTSV( -1, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPTSV( 0, -1, R, A( 1, 1 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPTSV( 2, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_CPTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CPTSVX
*
         SRNAMT = 'AB_AB_CPTSVX'
         INFOT = 1
         CALL AB_AB_CPTSVX( '/', 0, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 
     $1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CPTSVX( 'N', -1, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B,
     $ 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CPTSVX( 'N', 0, -1, R, A( 1, 1 ), RF, AF( 1, 1 ), B,
     $ 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 
     $1, X,
     $                2, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 
     $2, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CPTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        AB_CHESV
*
         SRNAMT = 'AB_CHESV '
         INFOT = 1
         CALL AB_CHESV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHESV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHESV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CHESV( 'U', 2, 0, A, 1, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CHESV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CHESV( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CHESV( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_CHESV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHESVX
*
         SRNAMT = 'AB_AB_CHESVX'
         INFOT = 1
         CALL AB_AB_CHESVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHESVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHESVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CHESVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHESVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CHESVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_CHESVXX'
         INFOT = 1
         CALL AB_AB_AB_CHESVXX( '/', 'U', 0, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CHESVXX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_CHESVXX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, EQ, C,
     $ B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_CHESVXX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, EQ, C,
     $ B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_AB_CHESVXX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, EQ, C, 
     $B, 2, X,
     $                2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_CHESVXX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, EQ, C, 
     $B, 2, X,
     $                2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_AB_CHESVXX( 'F', 'U', 0, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_AB_CHESVXX( 'F', 'U', 1, 0, A, 1, AF, 1, IP, EQ, C, 
     $B, 1, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_CHESVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, C, 
     $B, 1, X,
     $                2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_AB_CHESVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, C, 
     $B, 2, X,
     $                1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $                ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHESVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HR' ) ) THEN
*
*        AB_AB_CHESV_ROOK
*
         SRNAMT = 'AB_AB_CHESV_ROOK'
         INFOT = 1
         CALL AB_AB_CHESV_ROOK( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHESV_ROOK( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHESV_ROOK( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHESV_ROOK( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHESV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHESV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HK' ) ) THEN
*
*        AB_AB_CHESV_RK
*
*        Test error exits of the driver that uses factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
         SRNAMT = 'AB_AB_CHESV_RK'
         INFOT = 1
         CALL AB_AB_CHESV_RK( '/', 0, 0, A, 1, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHESV_RK( 'U', -1, 0, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHESV_RK( 'U', 0, -1, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHESV_RK( 'U', 2, 0, A, 1, E, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHESV_RK( 'U', 2, 0, A, 2, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHESV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CHESV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHESV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, -2, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CHESV_RK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        AB_CHPSV
*
         SRNAMT = 'AB_CHPSV '
         INFOT = 1
         CALL AB_CHPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CHPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CHPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CHPSVX
*
         SRNAMT = 'AB_AB_CHPSVX'
         INFOT = 1
         CALL AB_AB_CHPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        AB_CSYSV
*
         SRNAMT = 'AB_CSYSV '
         INFOT = 1
         CALL AB_CSYSV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSYSV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CSYSV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CSYSV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_CSYSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSYSVX
*
         SRNAMT = 'AB_AB_CSYSVX'
         INFOT = 1
         CALL AB_AB_CSYSVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYSVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSYSVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSYSVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CSYSVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSVX', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_CSYSVXX
*
         N_ERR_BNDS = 3
         NPARAMS = 1
         SRNAMT = 'AB_AB_AB_CSYSVXX'
         INFOT = 1
         EQ = 'N'
         CALL AB_AB_AB_CSYSVXX( '/', 'U', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_CSYSVXX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, 
     $B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C,  NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_CSYSVXX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, EQ, R,
     $ B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         EQ = '/'
         CALL AB_AB_AB_CSYSVXX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, EQ, R,
     $ B, 1, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         EQ = 'Y'
         INFOT = 6
         CALL AB_AB_AB_CSYSVXX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_CSYSVXX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_CSYSVXX( 'F', 'U', 2, 0, A, 2, AF, 2, IP, 'A', R,
     $ B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ='Y'
         CALL AB_AB_AB_CSYSVXX( 'F', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ='Y'
         R(1) = -ONE
         CALL AB_AB_AB_CSYSVXX( 'F', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'N'
         CALL AB_AB_AB_CSYSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 1, X,
     $        2, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_CSYSVXX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, EQ, R, 
     $B, 2, X,
     $        1, RCOND, RPVGRW, BERR, N_ERR_BNDS, ERR_BNDS_N,
     $        ERR_BNDS_C, NPARAMS, PARAMS, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CSYSVXX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        AB_AB_CSYSV_ROOK
*
         SRNAMT = 'AB_AB_CSYSV_ROOK'
         INFOT = 1
         CALL AB_AB_CSYSV_ROOK( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYSV_ROOK( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSYSV_ROOK( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CSYSV_ROOK( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SK' ) ) THEN
*
*        AB_AB_CSYSV_RK
*
*        Test error exits of the driver that uses factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
         SRNAMT = 'AB_AB_CSYSV_RK'
         INFOT = 1
         CALL AB_AB_CSYSV_RK( '/', 0, 0, A, 1, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSYSV_RK( 'U', -1, 0, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSYSV_RK( 'U', 0, -1, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CSYSV_RK( 'U', 2, 0, A, 1, E, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CSYSV_RK( 'U', 2, 0, A, 2, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, -2, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CSYSV_RK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        AB_CSPSV
*
         SRNAMT = 'AB_CSPSV '
         INFOT = 1
         CALL AB_CSPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CSPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CSPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_CSPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CSPSVX
*
         SRNAMT = 'AB_AB_CSPSVX'
         INFOT = 1
         CALL AB_AB_CSPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CSPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CSPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CSPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND
     $, R1,
     $                R2, W, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CSPSVX', INFOT, NOUT, LERR, OK )
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
*     End of AB_CERRVX
*
      END
