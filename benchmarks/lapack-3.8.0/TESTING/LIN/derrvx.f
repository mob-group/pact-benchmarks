*> \brief \b AB_DERRVX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRVX( PATH, NUNIT )
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
*> AB_DERRVX tests the error exits for the DOUBLE PRECISION driver routines
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DERRVX( PATH, NUNIT )
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
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   C( NMAX ), E( NMAX ),  R( NMAX ), R1( NMAX ),
     $                   R2( NMAX ), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_DGBSV, AB_AB_DGBSVX, AB_DGESV, AB
     $_AB_DGESVX, AB_DGTSV,
     $                   AB_AB_DGTSVX, AB_DPBSV, AB_AB_DPBSVX, AB_DPOSV,
     $ AB_AB_DPOSVX, AB_DPPSV,
     $                   AB_AB_DPPSVX, AB_DPTSV, AB_AB_DPTSVX, AB_DSPSV,
     $ AB_AB_DSPSVX, AB_DSYSV,
     $                   AB_AB_DSYSV_AA, AB_AB_DSYSV_RK, AB_AB_DSYSV_ROO
     $K, AB_AB_DSYSVX,
     $                   AB_AB_AB_DSYSV_AA_2STAGE
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
         B( J ) = 0.D+0
         E( J ) = 0.D+0
         R1( J ) = 0.D+0
         R2( J ) = 0.D+0
         W( J ) = 0.D+0
         X( J ) = 0.D+0
         C( J ) = 0.D+0
         R( J ) = 0.D+0
         IP( J ) = J
   20 CONTINUE
      EQ = ' '
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        AB_DGESV
*
         SRNAMT = 'AB_DGESV '
         INFOT = 1
         CALL AB_DGESV( -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGESV( 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGESV( 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_DGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DGESV( 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGESV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGESVX
*
         SRNAMT = 'AB_AB_DGESVX'
         INFOT = 1
         CALL AB_AB_DGESVX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGESVX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGESVX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R, C, 
     $B, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DGESVX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DGESVX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, C, B
     $, 2,
     $                X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_DGESVX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL AB_AB_DGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL AB_AB_DGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B
     $, 1,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_DGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 1,
     $                X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_DGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B
     $, 2,
     $                X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        AB_DGBSV
*
         SRNAMT = 'AB_DGBSV '
         INFOT = 1
         CALL AB_DGBSV( -1, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGBSV( 1, -1, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGBSV( 1, 0, -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGBSV( 0, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGBSV( 1, 1, 1, 0, A, 3, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DGBSV( 2, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DGBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGBSVX
*
         SRNAMT = 'AB_AB_DGBSVX'
         INFOT = 1
         CALL AB_AB_DGBSVX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGBSVX( 'N', '/', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGBSVX( 'N', 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGBSVX( 'N', 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DGBSVX( 'N', 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DGBSVX( 'N', 'N', 0, 0, 0, -1, A, 1, AF, 1, IP, EQ, 
     $R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DGBSVX( 'N', 'N', 1, 1, 1, 0, A, 2, AF, 4, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DGBSVX( 'N', 'N', 1, 1, 1, 0, A, 3, AF, 3, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL AB_AB_DGBSVX( 'F', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL AB_AB_DGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL AB_AB_DGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_DGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 1, X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_DGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R
     $, C,
     $                B, 2, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        AB_DGTSV
*
         SRNAMT = 'AB_DGTSV '
         INFOT = 1
         CALL AB_DGTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DGTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DGTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGTSVX
*
         SRNAMT = 'AB_AB_DGTSVX'
         INFOT = 1
         CALL AB_AB_DGTSVX( '/', 'N', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGTSVX( 'N', '/', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGTSVX( 'N', 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGTSVX( 'N', 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1,
     $ 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_DGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 2, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_DGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 
     $3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 2, X, 1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        AB_DPOSV
*
         SRNAMT = 'AB_DPOSV '
         INFOT = 1
         CALL AB_DPOSV( '/', 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPOSV( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPOSV( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DPOSV( 'U', 2, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_DPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DPOSV( 'U', 2, 0, A, 2, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPOSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DPOSVX
*
         SRNAMT = 'AB_AB_DPOSVX'
         INFOT = 1
         CALL AB_AB_DPOSVX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DPOSVX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DPOSVX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DPOSVX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 1, X
     $, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DPOSVX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DPOSVX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2, X,
     $ 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL AB_AB_DPOSVX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL AB_AB_DPOSVX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1, X,
     $ 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_DPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2, X,
     $ 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPOSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        AB_DPPSV
*
         SRNAMT = 'AB_DPPSV '
         INFOT = 1
         CALL AB_DPPSV( '/', 0, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPPSV( 'U', -1, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPPSV( 'U', 0, -1, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPPSV( 'U', 2, 0, A, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DPPSVX
*
         SRNAMT = 'AB_AB_DPPSVX'
         INFOT = 1
         CALL AB_AB_DPPSVX( '/', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DPPSVX( 'N', '/', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DPPSVX( 'N', 'U', -1, 0, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DPPSVX( 'N', 'U', 0, -1, A, AF, EQ, C, B, 1, X, 1, R
     $COND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         EQ = '/'
         CALL AB_AB_DPPSVX( 'F', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         EQ = 'Y'
         CALL AB_AB_DPPSVX( 'F', 'U', 1, 0, A, AF, EQ, C, B, 1, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 1, X, 2, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 2, X, 1, RC
     $OND,
     $                R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        AB_DPBSV
*
         SRNAMT = 'AB_DPBSV '
         INFOT = 1
         CALL AB_DPBSV( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPBSV( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DPBSV( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DPBSV( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPBSV( 'U', 1, 1, 0, A, 1, B, 2, INFO )
         CALL AB_CHKXER( 'AB_DPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DPBSV( 'U', 2, 0, 0, A, 1, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPBSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DPBSVX
*
         SRNAMT = 'AB_AB_DPBSVX'
         INFOT = 1
         CALL AB_AB_DPBSVX( '/', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DPBSVX( 'N', '/', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DPBSVX( 'N', 'U', -1, 0, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DPBSVX( 'N', 'U', 1, -1, 0, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DPBSVX( 'N', 'U', 0, 0, -1, A, 1, AF, 1, EQ, C, B, 1
     $, X,
     $                1, RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DPBSVX( 'N', 'U', 1, 1, 0, A, 1, AF, 2, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DPBSVX( 'N', 'U', 1, 1, 0, A, 2, AF, 1, EQ, C, B, 2,
     $ X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL AB_AB_DPBSVX( 'F', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'Y'
         CALL AB_AB_DPBSVX( 'F', 'U', 1, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_DPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 1,
     $ X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_DPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 2,
     $ X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DPBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        AB_DPTSV
*
         SRNAMT = 'AB_DPTSV '
         INFOT = 1
         CALL AB_DPTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), B, 1, INFO )
         CALL AB_CHKXER( 'AB_DPTSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DPTSVX
*
         SRNAMT = 'AB_AB_DPTSVX'
         INFOT = 1
         CALL AB_AB_DPTSVX( '/', 0, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 ),
     $                AF( 1, 2 ), B, 1, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DPTSVX( 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 )
     $,
     $                AF( 1, 2 ), B, 1, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DPTSVX( 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 )
     $,
     $                AF( 1, 2 ), B, 1, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DPTSVX( 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 ),
     $                AF( 1, 2 ), B, 1, X, 2, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DPTSVX( 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), AF( 1, 1 ),
     $                AF( 1, 2 ), B, 2, X, 1, RCOND, R1, R2, W, INFO )
         CALL AB_CHKXER( 'AB_AB_DPTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        AB_DSYSV
*
         SRNAMT = 'AB_DSYSV '
         INFOT = 1
         CALL AB_DSYSV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYSV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSYSV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DSYSV( 'U', 2, 0, A, 1, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DSYSV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_DSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DSYSV( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_DSYSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSYSVX
*
         SRNAMT = 'AB_AB_DSYSVX'
         INFOT = 1
         CALL AB_AB_DSYSVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYSVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYSVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYSVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1
     $,
     $                RCOND, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DSYSVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYSVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_DSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_DSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        AB_AB_DSYSV_ROOK
*
         SRNAMT = 'AB_AB_DSYSV_ROOK'
         INFOT = 1
         CALL AB_AB_DSYSV_ROOK( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYSV_ROOK( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYSV_ROOK( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYSV_ROOK( 'U', 2, 0, A, 1, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYSV_ROOK( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYSV_ROOK( 'U', 0, 0, A, 1, IP, B, 1, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SK' ) ) THEN
*
*        AB_AB_DSYSV_RK
*
*        Test error exits of the driver that uses factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
         SRNAMT = 'AB_AB_DSYSV_RK'
         INFOT = 1
         CALL AB_AB_DSYSV_RK( '/', 0, 0, A, 1, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYSV_RK( 'U', -1, 0, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_DSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYSV_RK( 'U', 0, -1, A, 1, E, IP, B, 1, W, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_DSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYSV_RK( 'U', 2, 0, A, 1, E, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSYSV_RK( 'U', 2, 0, A, 2, E, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYSV_RK', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSYSV_RK( 'U', 0, 0, A, 1, E, IP, B, 1, W, -2, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_DSYSV_RK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SA' ) ) THEN
*
*        AB_AB_DSYSV_AA
*
        SRNAMT = 'AB_AB_DSYSV_AA'
        INFOT = 1
        CALL AB_AB_DSYSV_AA( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
        CALL AB_CHKXER( 'AB_AB_DSYSV_AA', INFOT, NOUT, LERR, OK )
        INFOT = 2
        CALL AB_AB_DSYSV_AA( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
        CALL AB_CHKXER( 'AB_AB_DSYSV_AA', INFOT, NOUT, LERR, OK )
        INFOT = 3
        CALL AB_AB_DSYSV_AA( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
        CALL AB_CHKXER( 'AB_AB_DSYSV_AA', INFOT, NOUT, LERR, OK )
        INFOT = 8
        CALL AB_AB_DSYSV_AA( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
        CALL AB_CHKXER( 'AB_AB_DSYSV_AA', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'S2' ) ) THEN
*
*        AB_AB_DSYSV_AASEN_2STAGE
*
         SRNAMT = 'AB_AB_AB_DSYSV_AA_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_DSYSV_AA_2STAGE( '/', 0, 0, A, 1, A, 1, IP, IP, B
     $, 1, 
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 2
         CALL AB_AB_AB_DSYSV_AA_2STAGE( 'U', -1, 0, A, 1, A, 1, IP, IP, 
     $B, 1, 
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 3
         CALL AB_AB_AB_DSYSV_AA_2STAGE( 'U', 0, -1, A, 1, A, 1, IP, IP, 
     $B, 1, 
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 5
         CALL AB_AB_AB_DSYSV_AA_2STAGE( 'U', 2, 1, A, 1, A, 1, IP, IP, B
     $, 1,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 11
         CALL AB_AB_AB_DSYSV_AA_2STAGE( 'U', 2, 1, A, 2, A, 2, IP, IP, B
     $, 1,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
         INFOT = 7
         CALL AB_AB_AB_DSYSV_AA_2STAGE( 'U', 2, 1, A, 2, A, 1, IP, IP, B
     $, 2,
     $                         W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYSV_AA_2STAGE', INFOT, NOUT, LERR, 
     $OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        AB_DSPSV
*
         SRNAMT = 'AB_DSPSV '
         INFOT = 1
         CALL AB_DSPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_DSPSV ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DSPSVX
*
         SRNAMT = 'AB_AB_DSPSVX'
         INFOT = 1
         CALL AB_AB_DSPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCON
     $D, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND
     $, R1,
     $                R2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPSVX', INFOT, NOUT, LERR, OK )
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
*     End of AB_DERRVX
*
      END
