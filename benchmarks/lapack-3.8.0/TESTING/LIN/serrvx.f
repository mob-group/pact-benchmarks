*> \brief \b AB_SERRVX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRVX( PATH, NUNIT )
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
*> AB_SERRVX tests the error exits for the REAL driver routines
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
*> \date November 2017
*
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SERRVX( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
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
      INTEGER            I, INFO, J
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX )
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   C( NMAX ), E( NMAX ), R( NMAX ), R1( NMAX ),
     $                   R2( NMAX ), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_SGBSV, AB_AB_SGBSVX, AB_SGESV, AB
     $_AB_SGESVX, AB_SGTSV,
     $                   AB_AB_SGTSVX, AB_SPBSV, AB_AB_SPBSVX, AB_SPOSV,
     $ AB_AB_SPOSVX, AB_SPPSV,
     $                   AB_AB_SPPSVX, AB_SPTSV, AB_AB_SPTSVX, AB_SSPSV,
     $ AB_AB_SSPSVX, AB_SSYSV,
     $                   AB_AB_SSYSV_AA, AB_AB_SSYSV_RK, AB_AB_SSYSV_ROO
     $K, AB_AB_SSYSVX,
     $                   AB_AB_AB_SSYSV_AA_2STAGE
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
         C( J ) = 0.E+0
         R( J ) = 0.E+0
         IP( J ) = J
   20 CONTINUE
      EQ = ' '
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        AB_SGESV
*
         SRNAMT = 'AB_SGESV '
         INFOT = 1
         CALL AB_SGESV( -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGESV( 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGESV( 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_SGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SGESV( 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGESV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SGESVX
*
         SRNAMT = 'AB_AB_SGESVX'
         INFOT = 1
         CALL AB_AB_SGESVX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SGESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SGESVX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SGESVX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SGESVX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SGESVX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_SGESVX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL AB_AB_SGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL AB_AB_SGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_SGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 1,
     $                X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_SGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGESVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        AB_SGBSV
*
         SRNAMT = 'AB_SGBSV '
         INFOT = 1
         CALL AB_SGBSV( -1, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGBSV( 1, -1, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGBSV( 1, 0, -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGBSV( 0, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SGBSV( 1, 1, 1, 0, A, 3, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SGBSV( 2, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SGBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SGBSVX
*
         SRNAMT = 'AB_AB_SGBSVX'
         INFOT = 1
         CALL AB_AB_SGBSVX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SGBSVX( 'N', '/', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SGBSVX( 'N', 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SGBSVX( 'N', 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SGBSVX( 'N', 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SGBSVX( 'N', 'N', 0, 0, 0, -1, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SGBSVX( 'N', 'N', 1, 1, 1, 0, A, 2, AF, 4, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SGBSVX( 'N', 'N', 1, 1, 1, 0, A, 3, AF, 3, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL AB_AB_SGBSVX( 'F', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL AB_AB_SGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL AB_AB_SGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_SGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_SGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 2, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        AB_SGTSV
*
         SRNAMT = 'AB_SGTSV '
         INFOT = 1
         CALL AB_SGTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_SGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_SGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SGTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SGTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SGTSVX
*
         SRNAMT = 'AB_AB_SGTSVX'
         INFOT = 1
         CALL AB_AB_SGTSVX( '/', 'N', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SGTSVX( 'N', '/', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SGTSVX( 'N', 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SGTSVX( 'N', 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_SGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_SGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 2, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SGTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        AB_SPOSV
*
         SRNAMT = 'AB_SPOSV '
         INFOT = 1
         CALL AB_SPOSV( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPOSV( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPOSV( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SPOSV( 'U', 2, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_SPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SPOSV( 'U', 2, 0, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPOSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SPOSVX
*
         SRNAMT = 'AB_AB_SPOSVX'
         INFOT = 1
         CALL AB_AB_SPOSVX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SPOSVX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SPOSVX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SPOSVX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SPOSVX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SPOSVX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_SPOSVX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_SPOSVX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1, X,
     $ 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_SPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPOSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        AB_SPPSV
*
         SRNAMT = 'AB_SPPSV '
         INFOT = 1
         CALL AB_SPPSV( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPPSV( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPPSV( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPPSV( 'U', 2, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SPPSVX
*
         SRNAMT = 'AB_AB_SPPSVX'
         INFOT = 1
         CALL AB_AB_SPPSVX( '/', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SPPSVX( 'N', '/', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SPPSVX( 'N', 'U', -1, 0, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SPPSVX( 'N', 'U', 0, -1, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         EQ = '/'
         CALL AB_AB_SPPSVX( 'F', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         EQ = 'Y'
         CALL AB_AB_SPPSVX( 'F', 'U', 1, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 1, X, 2, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 2, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        AB_SPBSV
*
         SRNAMT = 'AB_SPBSV '
         INFOT = 1
         CALL AB_SPBSV( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPBSV( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SPBSV( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SPBSV( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPBSV( 'U', 1, 1, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_SPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SPBSV( 'U', 2, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SPBSVX
*
         SRNAMT = 'AB_AB_SPBSVX'
         INFOT = 1
         CALL AB_AB_SPBSVX( '/', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SPBSVX( 'N', '/', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SPBSVX( 'N', 'U', -1, 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SPBSVX( 'N', 'U', 1, -1, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SPBSVX( 'N', 'U', 0, 0, -1, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SPBSVX( 'N', 'U', 1, 1, 0, A, 1, AF, 2, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SPBSVX( 'N', 'U', 1, 1, 0, A, 2, AF, 1, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_SPBSVX( 'F', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'Y'
         CALL AB_AB_SPBSVX( 'F', 'U', 1, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_SPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_SPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 2,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SPBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        AB_SPTSV
*
         SRNAMT = 'AB_SPTSV '
         INFOT = 1
         CALL AB_SPTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_SPTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SPTSVX
*
         SRNAMT = 'AB_AB_SPTSVX'
         INFOT = 1
         CALL AB_AB_SPTSVX( '/', 0, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 ),
     $                AF( 1, 2 ), B, 1, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SPTSVX( 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 )
     $,
     $                AF( 1, 2 ), B, 1, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SPTSVX( 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 )
     $,
     $                AF( 1, 2 ), B, 1, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SPTSVX( 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 ),
     $                AF( 1, 2 ), B, 1, X, 2, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SPTSVX( 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 ),
     $                AF( 1, 2 ), B, 2, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_SPTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        AB_SSYSV
*
         SRNAMT = 'AB_SSYSV '
         INFOT = 1
         CALL AB_SSYSV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYSV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSYSV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SSYSV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_SSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_SSYSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSYSVX
*
         SRNAMT = 'AB_AB_SSYSVX'
         INFOT = 1
         CALL AB_AB_SSYSVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYSVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYSVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYSVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SSYSVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYSVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_SSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_SSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSVX', INFOT, NOUT, LERR, OK )
*
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        AB_AB_SSYSV_ROOK
*
         SRNAMT = 'AB_AB_SSYSV_ROOK'
         INFOT = 1
         CALL AB_AB_SSYSV_ROOK( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYSV_ROOK( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYSV_ROOK( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYSV_ROOK( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SK' ) ) THEN
*
*        AB_AB_SSYSV_RK
*
*        Test error exits of the driver that uses factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
         SRNAMT = 'AB_AB_SSYSV_RK'
         INFOT = 1
         CALL AB_AB_SSYSV_RK( '/', 0, 0, A, 1, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYSV_RK( 'U', -1, 0, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_SSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYSV_RK( 'U', 0, -1, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_SSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYSV_RK( 'U', 2, 0, A, 1, E, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSYSV_RK( 'U', 2, 0, A, 2, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, -2, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_SSYSV_RK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SA' ) ) THEN
*
*        AB_AB_SSYSV_AA
*
         SRNAMT = 'AB_AB_SSYSV_AA'
         INFOT = 1
         CALL AB_AB_SSYSV_AA( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_AA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYSV_AA( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_AA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYSV_AA( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_AA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYSV_AA( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYSV_AA', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'S2' ) ) THEN
*
*        AB_AB_DSYSV_AASEN_2STAGE
*
         SRNAMT = 'AB_AB_AB_SSYSV_AA_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_SSYSV_AA_2STAGE( '/', 0, 0, A, 1, A, 1, IP, IP, B
     $, 1,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 2
         CALL AB_AB_AB_SSYSV_AA_2STAGE( 'U', -1, 0, A, 1, A, 1, IP, IP, 
     $B, 1,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 3
         CALL AB_AB_AB_SSYSV_AA_2STAGE( 'U', 0, -1, A, 1, A, 1, IP, IP, 
     $B, 1,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 5
         CALL AB_AB_AB_SSYSV_AA_2STAGE( 'U', 2, 1, A, 1, A, 1, IP, IP, B
     $, 1,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 11
         CALL AB_AB_AB_SSYSV_AA_2STAGE( 'U', 2, 1, A, 2, A, 2, IP, IP, B
     $, 1,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 7
         CALL AB_AB_AB_SSYSV_AA_2STAGE( 'U', 2, 1, A, 2, A, 1, IP, IP, B
     $, 2,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        AB_SSPSV
*
         SRNAMT = 'AB_SSPSV '
         INFOT = 1
         CALL AB_SSPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_SSPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SSPSVX
*
         SRNAMT = 'AB_AB_SSPSVX'
         INFOT = 1
         CALL AB_AB_SSPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPSVX', INFOT, NOUT, LERR, OK )
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
*     End of AB_SERRVX
*
      END
