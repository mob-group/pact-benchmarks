*> \brief \b AB_SERRST
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRST( PATH, NUNIT )
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
*> AB_SERRST tests the error exits for AB_SSYTRD, AB_SORGTR, AB_SORMTR, AB_SSPTRD,
*> AB_SOPGTR, AB_SOPMTR, AB_SSTEQR, AB_SSTERF, AB_SSTEBZ, AB_SSTEIN, AB_SPTEQR, AB_SSBTRD,
*> AB_SSYEV, AB_AB_SSYEVX, AB_AB_SSYEVD, AB_SSBEV, AB_AB_SSBEVX, AB_AB_SSBEVD,
*> AB_SSPEV, AB_AB_SSPEVX, AB_AB_SSPEVD, AB_SSTEV, AB_AB_SSTEVX, AB_AB_SSTEVD, and AB_SSTEDC.
*> AB_AB_AB_SSYEVD_2STAGE, AB_AB_AB_SSYEVR_2STAGE, AB_AB_AB_SSYEVX_2STAGE,
*> AB_AB_SSYEV_2STAGE, AB_AB_SSBEV_2STAGE, AB_AB_AB_SSBEVD_2STAGE,
*> AB_AB_AB_SSBEVX_2STAGE, AB_AB_SSYTRD_2STAGE, AB_AB_SSYTRD_SY2SB,
*> AB_SSYTRD_SB2ST
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
*> \ingroup single_eig
*
*  =====================================================================
      SUBROUTINE AB_SERRST( PATH, NUNIT )
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
*     NMAX has to be at least 3 or LIW may be too small
*     .. Parameters ..
      INTEGER            NMAX, LIW, LW
      PARAMETER          ( NMAX = 3, LIW = 12*NMAX, LW = 20*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, M, N, NSPLIT, NT
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW )
      REAL               A( NMAX, NMAX ), C( NMAX, NMAX ), D( NMAX ),
     $                   E( NMAX ), Q( NMAX, NMAX ), R( NMAX ),
     $                   TAU( NMAX ), W( LW ), X( NMAX ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_SOPGTR, AB_SOPMTR, AB_SORGTR, AB_
     $SORMTR, AB_SPTEQR,
     $                   AB_SSBEV, AB_AB_SSBEVD, AB_AB_SSBEVX, AB_SSBTRD
     $, AB_SSPEV, AB_AB_SSPEVD,
     $                   AB_AB_SSPEVX, AB_SSPTRD, AB_SSTEBZ, AB_SSTEDC, 
     $AB_SSTEIN, AB_SSTEQR,
     $                   AB_SSTERF, AB_SSTEV, AB_AB_SSTEVD, AB_AB_SSTEVR
     $, AB_AB_SSTEVX, AB_SSYEV,
     $                   AB_AB_SSYEVD, AB_AB_SSYEVR, AB_AB_SSYEVX, AB_SS
     $YTRD,
     $                   AB_AB_AB_SSYEVD_2STAGE, AB_AB_AB_SSYEVR_2STAGE,
     $ AB_AB_AB_SSYEVX_2STAGE,
     $                   AB_AB_SSYEV_2STAGE, AB_AB_SSBEV_2STAGE, AB_AB_A
     $B_SSBEVD_2STAGE,
     $                   AB_AB_AB_SSBEVX_2STAGE, AB_AB_SSYTRD_2STAGE, AB
     $_AB_SSYTRD_SY2SB,
     $                   AB_SSYTRD_SB2ST
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
   10    CONTINUE
   20 CONTINUE
      DO 30 J = 1, NMAX
         D( J ) = REAL( J )
         E( J ) = 0.0
         I1( J ) = J
         I2( J ) = J
         TAU( J ) = 1.
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
      IF( AB_AB_LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        AB_SSYTRD
*
         SRNAMT = 'AB_SSYTRD'
         INFOT = 1
         CALL AB_SSYTRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSYTRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SSYTRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_SSYTRD_2STAGE
*
         SRNAMT = 'AB_AB_SSYTRD_2STAGE'
         INFOT = 1
         CALL AB_AB_SSYTRD_2STAGE( '/', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_SSYTRD_2STAGE( 'H', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRD_2STAGE( 'N', '/', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYTRD_2STAGE( 'N', 'U', -1, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYTRD_2STAGE( 'N', 'U', 2, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYTRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SSYTRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_AB_SSYTRD_SY2SB
*
         SRNAMT = 'AB_AB_SSYTRD_SY2SB'
         INFOT = 1
         CALL AB_AB_SSYTRD_SY2SB( '/', 0, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYTRD_SY2SB( 'U', -1, 0, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYTRD_SY2SB( 'U', 0, -1, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYTRD_SY2SB( 'U', 2, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSYTRD_SY2SB( 'U', 0, 2, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYTRD_SY2SB( 'U', 0, 0, A, 1, C, 1, TAU, W, 0, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_SSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_SSYTRD_SB2ST
*
         SRNAMT = 'AB_SSYTRD_SB2ST'
         INFOT = 1
         CALL AB_SSYTRD_SB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRD_SB2ST( 'Y', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRD_SB2ST( 'Y', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSYTRD_SB2ST( 'Y', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSYTRD_SB2ST( 'Y', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SSYTRD_SB2ST( 'Y', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSYTRD_SB2ST( 'Y', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_SSYTRD_SB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_SSYTRD_SB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_SORGTR
*
         SRNAMT = 'AB_SORGTR'
         INFOT = 1
         CALL AB_SORGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SORGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SORGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SORGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_SORMTR
*
         SRNAMT = 'AB_SORMTR'
         INFOT = 1
         CALL AB_SORMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SORMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SORMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SORMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SORMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SORMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SORMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SORMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_SORMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_SORMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_SORMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_SSPTRD
*
         SRNAMT = 'AB_SSPTRD'
         INFOT = 1
         CALL AB_SSPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_SSPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_SSPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        AB_SOPGTR
*
         SRNAMT = 'AB_SOPGTR'
         INFOT = 1
         CALL AB_SOPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SOPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SOPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_SOPMTR
*
         SRNAMT = 'AB_SOPMTR'
         INFOT = 1
         CALL AB_SOPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SOPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SOPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SOPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SOPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SOPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SOPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_SPTEQR
*
         SRNAMT = 'AB_SPTEQR'
         INFOT = 1
         CALL AB_SPTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SPTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SPTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_SSTEBZ
*
         SRNAMT = 'AB_SSTEBZ'
         INFOT = 1
         CALL AB_SSTEBZ( '/', 'E', 0, 0.0, 1.0, 1, 0, 0.0, D, E, M, NSPL
     $IT,
     $                X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSTEBZ( 'A', '/', 0, 0.0, 0.0, 0, 0, 0.0, D, E, M, NSPL
     $IT,
     $                X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSTEBZ( 'A', 'E', -1, 0.0, 0.0, 0, 0, 0.0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SSTEBZ( 'V', 'E', 0, 0.0, 0.0, 0, 0, 0.0, D, E, M, NSPL
     $IT,
     $                X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSTEBZ( 'I', 'E', 0, 0.0, 0.0, 0, 0, 0.0, D, E, M, NSPL
     $IT,
     $                X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSTEBZ( 'I', 'E', 1, 0.0, 0.0, 2, 1, 0.0, D, E, M, NSPL
     $IT,
     $                X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSTEBZ( 'I', 'E', 1, 0.0, 0.0, 1, 0, 0.0, D, E, M, NSPL
     $IT,
     $                X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSTEBZ( 'I', 'E', 1, 0.0, 0.0, 1, 2, 0.0, D, E, M, NSPL
     $IT,
     $                X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_SSTEBZ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_SSTEIN
*
         SRNAMT = 'AB_SSTEIN'
         INFOT = 1
         CALL AB_SSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_SSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_SSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_SSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_SSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_SSTEQR
*
         SRNAMT = 'AB_SSTEQR'
         INFOT = 1
         CALL AB_SSTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_SSTERF
*
         SRNAMT = 'AB_SSTERF'
         INFOT = 1
         CALL AB_SSTERF( -1, D, E, INFO )
         CALL AB_CHKXER( 'AB_SSTERF', INFOT, NOUT, LERR, OK )
         NT = NT + 1
*
*        AB_SSTEDC
*
         SRNAMT = 'AB_SSTEDC'
         INFOT = 1
         CALL AB_SSTEDC( '/', 0, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSTEDC( 'N', -1, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSTEDC( 'V', 2, D, E, Z, 1, W, 23, IW, 28, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SSTEDC( 'N', 1, D, E, Z, 1, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SSTEDC( 'I', 2, D, E, Z, 2, W, 0, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SSTEDC( 'V', 2, D, E, Z, 2, W, 0, IW, 28, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SSTEDC( 'N', 1, D, E, Z, 1, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SSTEDC( 'I', 2, D, E, Z, 2, W, 19, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SSTEDC( 'V', 2, D, E, Z, 2, W, 23, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_SSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_SSTEVD
*
         SRNAMT = 'AB_AB_SSTEVD'
         INFOT = 1
         CALL AB_AB_SSTEVD( '/', 0, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSTEVD( 'N', -1, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SSTEVD( 'V', 2, D, E, Z, 1, W, 19, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSTEVD( 'N', 1, D, E, Z, 1, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSTEVD( 'V', 2, D, E, Z, 2, W, 12, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSTEVD( 'N', 0, D, E, Z, 1, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSTEVD( 'V', 2, D, E, Z, 2, W, 19, IW, 11, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_SSTEV
*
         SRNAMT = 'AB_SSTEV '
         INFOT = 1
         CALL AB_SSTEV( '/', 0, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSTEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSTEV( 'N', -1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSTEV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSTEV( 'V', 2, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSTEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_AB_SSTEVX
*
         SRNAMT = 'AB_AB_SSTEVX'
         INFOT = 1
         CALL AB_AB_SSTEVX( '/', 'A', 0, D, E, 0.0, 0.0, 0, 0, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSTEVX( 'N', '/', 0, D, E, 0.0, 1.0, 1, 0, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSTEVX( 'N', 'A', -1, D, E, 0.0, 0.0, 0, 0, 0.0, M, 
     $X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSTEVX( 'N', 'V', 1, D, E, 0.0, 0.0, 0, 0, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSTEVX( 'N', 'I', 1, D, E, 0.0, 0.0, 0, 0, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSTEVX( 'N', 'I', 1, D, E, 0.0, 0.0, 2, 1, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSTEVX( 'N', 'I', 2, D, E, 0.0, 0.0, 2, 1, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSTEVX( 'N', 'I', 1, D, E, 0.0, 0.0, 1, 2, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_SSTEVX( 'V', 'A', 2, D, E, 0.0, 0.0, 0, 0, 0.0, M, X
     $, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_SSTEVR
*
         N = 1
         SRNAMT = 'AB_AB_SSTEVR'
         INFOT = 1
         CALL AB_AB_SSTEVR( '/', 'A', 0, D, E, 0.0, 0.0, 1, 1, 0.0, M, R
     $, Z,
     $                1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSTEVR( 'V', '/', 0, D, E, 0.0, 0.0, 1, 1, 0.0, M, R
     $, Z,
     $                1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSTEVR( 'V', 'A', -1, D, E, 0.0, 0.0, 1, 1, 0.0, M, 
     $R, Z,
     $                1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSTEVR( 'V', 'V', 1, D, E, 0.0, 0.0, 1, 1, 0.0, M, R
     $, Z,
     $                1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSTEVR( 'V', 'I', 1, D, E, 0.0, 0.0, 0, 1, 0.0, M, W
     $, Z,
     $                1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         N = 2
         CALL AB_AB_SSTEVR( 'V', 'I', 2, D, E, 0.0, 0.0, 2, 1, 0.0, M, W
     $, Z,
     $                1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 14
         N = 1
         CALL AB_AB_SSTEVR( 'V', 'I', 1, D, E, 0.0, 0.0, 1, 1, 0.0, M, W
     $, Z,
     $                0, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_SSTEVR( 'V', 'I', 1, D, E, 0.0, 0.0, 1, 1, 0.0, M, W
     $, Z,
     $                1, IW, X, 20*N-1, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 19
         CALL AB_AB_SSTEVR( 'V', 'I', 1, D, E, 0.0, 0.0, 1, 1, 0.0, M, W
     $, Z,
     $                1, IW, X, 20*N, IW( 2*N+1 ), 10*N-1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSTEVR', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_SSYEVD
*
         SRNAMT = 'AB_AB_SSYEVD'
         INFOT = 1
         CALL AB_AB_SSYEVD( '/', 'U', 0, A, 1, X, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYEVD( 'N', '/', 0, A, 1, X, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYEVD( 'N', 'U', -1, A, 1, X, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYEVD( 'N', 'U', 2, A, 1, X, W, 3, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYEVD( 'N', 'U', 1, A, 1, X, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYEVD( 'N', 'U', 2, A, 2, X, W, 4, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYEVD( 'V', 'U', 2, A, 2, X, W, 20, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYEVD( 'N', 'U', 1, A, 1, X, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYEVD( 'N', 'U', 2, A, 2, X, W, 5, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYEVD( 'V', 'U', 2, A, 2, X, W, 27, IW, 11, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_AB_SSYEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_SSYEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_SSYEVD_2STAGE( '/', 'U', 0, A, 1, X, W, 1, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_SSYEVD_2STAGE( 'V', 'U', 0, A, 1, X, W, 1, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_SSYEVD_2STAGE( 'N', '/', 0, A, 1, X, W, 1, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_SSYEVD_2STAGE( 'N', 'U', -1, A, 1, X, W, 1, IW, 1
     $, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_SSYEVD_2STAGE( 'N', 'U', 2, A, 1, X, W, 3, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_SSYEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 0, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_SSYEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 4, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 8
*         CALL AB_AB_AB_SSYEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 20, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_SSYEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 1, IW, 0,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_SSYEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 25, IW, 0
     $, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 10
*         CALL AB_AB_AB_SSYEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 27, IW, 11, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_SSYEVD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_SSYEVR
*
         SRNAMT = 'AB_AB_SSYEVR'
         N = 1
         INFOT = 1
         CALL AB_AB_SSYEVR( '/', 'A', 'U', 0, A, 1, 0.0, 0.0, 1, 1, 0.0,
     $ M, R,
     $                Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYEVR( 'V', '/', 'U', 0, A, 1, 0.0, 0.0, 1, 1, 0.0,
     $ M, R,
     $                Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYEVR( 'V', 'A', '/', -1, A, 1, 0.0, 0.0, 1, 1, 0.0
     $, M,
     $                R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSYEVR( 'V', 'A', 'U', -1, A, 1, 0.0, 0.0, 1, 1, 0.0
     $, M,
     $                R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SSYEVR( 'V', 'A', 'U', 2, A, 1, 0.0, 0.0, 1, 1, 0.0,
     $ M, R,
     $                Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYEVR( 'V', 'V', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 0, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 10
*
         CALL AB_AB_SSYEVR( 'V', 'I', 'U', 2, A, 2, 0.0E0, 0.0E0, 2, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_SSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 0, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_SSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 26*N-1, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_SSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N-1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVR', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_AB_SSYEVR_2STAGE
*
         SRNAMT = 'AB_AB_AB_SSYEVR_2STAGE'
         N = 1
         INFOT = 1
         CALL AB_AB_AB_SSYEVR_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_SSYEVR_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'A', '/', -1, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 0, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0E0, 0.0E0, 2, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 0, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 18
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 0, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 20
         CALL AB_AB_AB_SSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 1, 1, 0.0E0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 12
*
*        AB_SSYEV
*
         SRNAMT = 'AB_SSYEV '
         INFOT = 1
         CALL AB_SSYEV( '/', 'U', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYEV( 'N', '/', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSYEV( 'N', 'U', -1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SSYEV( 'N', 'U', 2, A, 1, X, W, 3, INFO )
         CALL AB_CHKXER( 'AB_SSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SSYEV( 'N', 'U', 1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        AB_AB_SSYEV_2STAGE
*
         SRNAMT = 'AB_AB_SSYEV_2STAGE '
         INFOT = 1
         CALL AB_AB_SSYEV_2STAGE( '/', 'U', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_SSYEV_2STAGE( 'V', 'U', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYEV_2STAGE( 'N', '/', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYEV_2STAGE( 'N', 'U', -1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSYEV_2STAGE( 'N', 'U', 2, A, 1, X, W, 3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYEV_2STAGE( 'N', 'U', 1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_SSYEVX
*
         SRNAMT = 'AB_AB_SSYEVX'
         INFOT = 1
         CALL AB_AB_SSYEVX( '/', 'A', 'U', 0, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSYEVX( 'N', '/', 'U', 0, A, 1, 0.0, 1.0, 1, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSYEVX( 'N', 'A', '/', 0, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_SSYEVX( 'N', 'A', 'U', -1, A, 1, 0.0, 0.0, 0, 0, 0.0
     $, M,
     $                X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SSYEVX( 'N', 'A', 'U', 2, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSYEVX( 'N', 'V', 'U', 1, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSYEVX( 'N', 'I', 'U', 1, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSYEVX( 'N', 'I', 'U', 1, A, 1, 0.0, 0.0, 2, 1, 0.0,
     $ M, X,
     $                Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYEVX( 'N', 'I', 'U', 2, A, 2, 0.0, 0.0, 2, 1, 0.0,
     $ M, X,
     $                Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_SSYEVX( 'N', 'I', 'U', 1, A, 1, 0.0, 0.0, 1, 2, 0.0,
     $ M, X,
     $                Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_SSYEVX( 'V', 'A', 'U', 2, A, 2, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_SSYEVX( 'V', 'A', 'U', 1, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSYEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_AB_AB_SSYEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_SSYEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_SSYEVX_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                 0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_SSYEVX_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                 0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0E0, 1.0E0, 1, 0, 0.0E0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'A', '/', 0, A, 1,
     $                0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 2, 1, 0.0E0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0E0, 0.0E0, 2, 1, 0.0E0,
     $                M, X, Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 1, 2, 0.0E0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'A', 'U', 2, A, 2,
     $                0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 0, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 17
         CALL AB_AB_AB_SSYEVX_2STAGE( 'N', 'A', 'U', 1, A, 1,
     $                0.0E0, 0.0E0, 0, 0, 0.0E0,
     $                M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 13
*
*        AB_AB_SSPEVD
*
         SRNAMT = 'AB_AB_SSPEVD'
         INFOT = 1
         CALL AB_AB_SSPEVD( '/', 'U', 0, A, X, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSPEVD( 'N', '/', 0, A, X, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSPEVD( 'N', 'U', -1, A, X, Z, 1, W, 1, IW, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSPEVD( 'V', 'U', 2, A, X, Z, 1, W, 23, IW, 12, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSPEVD( 'N', 'U', 1, A, X, Z, 1, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSPEVD( 'N', 'U', 2, A, X, Z, 1, W, 3, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSPEVD( 'V', 'U', 2, A, X, Z, 2, W, 16, IW, 12, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSPEVD( 'N', 'U', 2, A, X, Z, 1, W, 4, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSPEVD( 'V', 'U', 2, A, X, Z, 2, W, 23, IW, 11, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_SSPEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_SSPEV
*
         SRNAMT = 'AB_SSPEV '
         INFOT = 1
         CALL AB_SSPEV( '/', 'U', 0, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_SSPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSPEV( 'N', '/', 0, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_SSPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSPEV( 'N', 'U', -1, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_SSPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSPEV( 'V', 'U', 2, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_SSPEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_SSPEVX
*
         SRNAMT = 'AB_AB_SSPEVX'
         INFOT = 1
         CALL AB_AB_SSPEVX( '/', 'A', 'U', 0, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSPEVX( 'N', '/', 'U', 0, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSPEVX( 'N', 'A', '/', 0, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_SSPEVX( 'N', 'A', 'U', -1, A, 0.0, 0.0, 0, 0, 0.0, M
     $, X,
     $                Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSPEVX( 'N', 'V', 'U', 1, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSPEVX( 'N', 'I', 'U', 1, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_SSPEVX( 'N', 'I', 'U', 1, A, 0.0, 0.0, 2, 1, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSPEVX( 'N', 'I', 'U', 2, A, 0.0, 0.0, 2, 1, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSPEVX( 'N', 'I', 'U', 1, A, 0.0, 0.0, 1, 2, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_SSPEVX( 'V', 'A', 'U', 2, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSPEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*     Test error exits for the SB path.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SB' ) ) THEN
*
*        AB_SSBTRD
*
         SRNAMT = 'AB_SSBTRD'
         INFOT = 1
         CALL AB_SSBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SSBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_SSYTRD_SB2ST
*
         SRNAMT = 'AB_SSYTRD_SB2ST'
         INFOT = 1
         CALL AB_SSYTRD_SB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRD_SB2ST( 'N', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSYTRD_SB2ST( 'N', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSYTRD_SB2ST( 'N', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSYTRD_SB2ST( 'N', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SSYTRD_SB2ST( 'N', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SSYTRD_SB2ST( 'N', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_SSYTRD_SB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_SSYTRD_SB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_SSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_SSBEVD
*
         SRNAMT = 'AB_AB_SSBEVD'
         INFOT = 1
         CALL AB_AB_SSBEVD( '/', 'U', 0, 0, A, 1, X, Z, 1, W, 1, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSBEVD( 'N', '/', 0, 0, A, 1, X, Z, 1, W, 1, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSBEVD( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSBEVD( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SSBEVD( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 4, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 1, W, 25, IW, 12
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 0, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSBEVD( 'N', 'U', 2, 0, A, 1, X, Z, 1, W, 3, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSBEVD( 'V', 'U', 2, 0, A, 1, X, Z, 2, W, 18, IW, 12
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_SSBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, IW, 0, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_SSBEVD( 'V', 'U', 2, 0, A, 1, X, Z, 2, W, 25, IW, 11
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_AB_SSBEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_SSBEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_SSBEVD_2STAGE( '/', 'U', 0, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_SSBEVD_2STAGE( 'V', 'U', 0, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_SSBEVD_2STAGE( 'N', '/', 0, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_SSBEVD_2STAGE( 'N', 'U', -1, 0, A, 1, X, Z, 1, W,
     $                                         1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_SSBEVD_2STAGE( 'N', 'U', 0, -1, A, 1, X, Z, 1, W,
     $                                         1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_SSBEVD_2STAGE( 'N', 'U', 2, 1, A, 1, X, Z, 1, W,
     $                                        4, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 9
*         CALL AB_AB_AB_SSBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 1, W,
*     $                                      25, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_SSBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1, W,
     $                                        0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 11
         CALL AB_AB_AB_SSBEVD_2STAGE( 'N', 'U', 2, 0, A, 1, X, Z, 1, W,
     $                                        3, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 11
*         CALL AB_AB_AB_SSBEVD_2STAGE( 'V', 'U', 2, 0, A, 1, X, Z, 2, W,
*     $                                      18, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_AB_SSBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 13
*         CALL AB_AB_AB_SSBEVD_2STAGE( 'V', 'U', 2, 0, A, 1, X, Z, 2, W,
*     $                                      25, IW, 11, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_SSBEVD_2STAGE', INFOT, NOUT, LERR, OK )
*         NT = NT + 12
         NT = NT + 9
*
*        AB_SSBEV
*
         SRNAMT = 'AB_SSBEV '
         INFOT = 1
         CALL AB_SSBEV( '/', 'U', 0, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SSBEV( 'N', '/', 0, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SSBEV( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SSBEV( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SSBEV( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SSBEV( 'V', 'U', 2, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_SSBEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_SSBEV_2STAGE
*
         SRNAMT = 'AB_AB_SSBEV_2STAGE '
         INFOT = 1
         CALL AB_AB_SSBEV_2STAGE( '/', 'U', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_SSBEV_2STAGE( 'V', 'U', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSBEV_2STAGE( 'N', '/', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSBEV_2STAGE( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, 0, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSBEV_2STAGE( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, 0, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_SSBEV_2STAGE( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSBEV_2STAGE( 'N', 'U', 2, 0, A, 1, X, Z, 0, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSBEV_2STAGE( 'N', 'U', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_AB_SSBEVX
*
         SRNAMT = 'AB_AB_SSBEVX'
         INFOT = 1
         CALL AB_AB_SSBEVX( '/', 'A', 'U', 0, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SSBEVX( 'N', '/', 'U', 0, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SSBEVX( 'N', 'A', '/', 0, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_SSBEVX( 'N', 'A', 'U', -1, 0, A, 1, Q, 1, 0.0, 0.0, 
     $0, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SSBEVX( 'N', 'A', 'U', 0, -1, A, 1, Q, 1, 0.0, 0.0, 
     $0, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SSBEVX( 'N', 'A', 'U', 2, 1, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_SSBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 2, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_SSBEVX( 'N', 'V', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SSBEVX( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SSBEVX( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 2
     $, 1,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_SSBEVX( 'N', 'I', 'U', 2, 0, A, 1, Q, 1, 0.0, 0.0, 2
     $, 1,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_SSBEVX( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 1
     $, 2,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_SSBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 2, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_SSBEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_AB_AB_SSBEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_SSBEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_SSBEVX_2STAGE( '/', 'A', 'U', 0, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_SSBEVX_2STAGE( 'V', 'A', 'U', 0, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', '/', 'U', 0, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'A', '/', 0, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'A', 'U', -1, 0, A, 1, Q, 1, 
     $0.0E0,
     $           0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'A', 'U', 0, -1, A, 1, Q, 1, 
     $0.0E0,
     $           0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 7
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'A', 'U', 2, 1, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 9
*         CALL AB_AB_AB_SSBEVX_2STAGE( 'V', 'A', 'U', 2, 0, A, 1, Q, 1, 0.0E0,
*     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 2, W, 0, IW, I3, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'V', 'U', 1, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 2, 1, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'I', 'U', 2, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 2, 1, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0
     $.0E0,
     $          0.0E0, 1, 2, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 18
*         CALL AB_AB_AB_SSBEVX_2STAGE( 'V', 'A', 'U', 2, 0, A, 1, Q, 2, 0.0E0,
*     $          0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_AB_SSBEVX_2STAGE( 'N', 'A', 'U', 0, 0, A, 1, Q, 1, 0
     $.0E0,
     $           0.0E0, 0, 0, 0.0E0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_SSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         NT = NT + 15
         NT = NT + 13
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH, NT
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits',
     $      ' (', I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of AB_SERRST
*
      END
