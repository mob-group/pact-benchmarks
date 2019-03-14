*> \brief \b AB_DERRST
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRST( PATH, NUNIT )
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
*> AB_DERRST tests the error exits for AB_DSYTRD, AB_DORGTR, AB_DORMTR, AB_DSPTRD,
*> AB_DOPGTR, AB_DOPMTR, AB_DSTEQR, AB_SSTERF, AB_SSTEBZ, AB_SSTEIN, AB_DPTEQR, AB_DSBTRD,
*> AB_DSYEV, AB_AB_SSYEVX, AB_AB_SSYEVD, AB_DSBEV, AB_AB_SSBEVX, AB_AB_SSBEVD,
*> AB_DSPEV, AB_AB_SSPEVX, AB_AB_SSPEVD, AB_DSTEV, AB_AB_SSTEVX, AB_AB_SSTEVD, and AB_SSTEDC.
*> AB_AB_AB_DSYEVD_2STAGE, AB_AB_AB_DSYEVR_2STAGE, AB_AB_AB_DSYEVX_2STAGE,
*> AB_AB_DSYEV_2STAGE, AB_AB_DSBEV_2STAGE, AB_AB_AB_DSBEVD_2STAGE,
*> AB_AB_AB_DSBEVX_2STAGE, AB_AB_DSYTRD_2STAGE, AB_AB_DSYTRD_SY2SB,
*> AB_DSYTRD_SB2ST
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
*> \ingroup double_eig
*
*  =====================================================================
      SUBROUTINE AB_DERRST( PATH, NUNIT )
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
      DOUBLE PRECISION   A( NMAX, NMAX ), C( NMAX, NMAX ), D( NMAX ),
     $                   E( NMAX ), Q( NMAX, NMAX ), R( NMAX ),
     $                   TAU( NMAX ), W( LW ), X( NMAX ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_DOPGTR, AB_DOPMTR, AB_DORGTR, AB_
     $DORMTR, AB_DPTEQR,
     $                   AB_DSBEV, AB_AB_DSBEVD, AB_AB_DSBEVX, AB_DSBTRD
     $, AB_DSPEV, AB_AB_DSPEVD,
     $                   AB_AB_DSPEVX, AB_DSPTRD, AB_DSTEBZ, AB_DSTEDC, 
     $AB_DSTEIN, AB_DSTEQR,
     $                   AB_DSTERF, AB_DSTEV, AB_AB_DSTEVD, AB_AB_DSTEVR
     $, AB_AB_DSTEVX, AB_DSYEV,
     $                   AB_AB_DSYEVD, AB_AB_DSYEVR, AB_AB_DSYEVX, AB_DS
     $YTRD,
     $                   AB_AB_AB_DSYEVD_2STAGE, AB_AB_AB_DSYEVR_2STAGE,
     $ AB_AB_AB_DSYEVX_2STAGE,
     $                   AB_AB_DSYEV_2STAGE, AB_AB_DSBEV_2STAGE, AB_AB_A
     $B_DSBEVD_2STAGE,
     $                   AB_AB_AB_DSBEVX_2STAGE, AB_AB_DSYTRD_2STAGE, AB
     $_AB_DSYTRD_SY2SB,
     $                   AB_DSYTRD_SB2ST
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
   10    CONTINUE
   20 CONTINUE
      DO 30 J = 1, NMAX
         D( J ) = DBLE( J )
         E( J ) = 0.0D0
         I1( J ) = J
         I2( J ) = J
         TAU( J ) = 1.D0
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
      IF( AB_AB_LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        AB_DSYTRD
*
         SRNAMT = 'AB_DSYTRD'
         INFOT = 1
         CALL AB_DSYTRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSYTRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DSYTRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_DSYTRD_2STAGE
*
         SRNAMT = 'AB_AB_DSYTRD_2STAGE'
         INFOT = 1
         CALL AB_AB_DSYTRD_2STAGE( '/', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_DSYTRD_2STAGE( 'H', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRD_2STAGE( 'N', '/', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYTRD_2STAGE( 'N', 'U', -1, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYTRD_2STAGE( 'N', 'U', 2, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYTRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DSYTRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_AB_DSYTRD_SY2SB
*
         SRNAMT = 'AB_AB_DSYTRD_SY2SB'
         INFOT = 1
         CALL AB_AB_DSYTRD_SY2SB( '/', 0, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYTRD_SY2SB( 'U', -1, 0, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYTRD_SY2SB( 'U', 0, -1, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYTRD_SY2SB( 'U', 2, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSYTRD_SY2SB( 'U', 0, 2, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYTRD_SY2SB( 'U', 0, 0, A, 1, C, 1, TAU, W, 0, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_DSYTRD_SY2SB', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_DSYTRD_SB2ST
*
         SRNAMT = 'AB_DSYTRD_SB2ST'
         INFOT = 1
         CALL AB_DSYTRD_SB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRD_SB2ST( 'Y', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRD_SB2ST( 'Y', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSYTRD_SB2ST( 'Y', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSYTRD_SB2ST( 'Y', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DSYTRD_SB2ST( 'Y', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSYTRD_SB2ST( 'Y', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DSYTRD_SB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DSYTRD_SB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_DORGTR
*
         SRNAMT = 'AB_DORGTR'
         INFOT = 1
         CALL AB_DORGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DORGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DORGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DORGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_DORMTR
*
         SRNAMT = 'AB_DORMTR'
         INFOT = 1
         CALL AB_DORMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DORMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DORMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DORMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DORMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DORMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DORMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DORMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_DORMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_DORMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_DORMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_DSPTRD
*
         SRNAMT = 'AB_DSPTRD'
         INFOT = 1
         CALL AB_DSPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_DSPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_DSPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        AB_DOPGTR
*
         SRNAMT = 'AB_DOPGTR'
         INFOT = 1
         CALL AB_DOPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DOPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DOPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_DOPMTR
*
         SRNAMT = 'AB_DOPMTR'
         INFOT = 1
         CALL AB_DOPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DOPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DOPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DOPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DOPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DOPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DOPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_DPTEQR
*
         SRNAMT = 'AB_DPTEQR'
         INFOT = 1
         CALL AB_DPTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DPTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DPTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_DSTEBZ
*
         SRNAMT = 'AB_DSTEBZ'
         INFOT = 1
         CALL AB_DSTEBZ( '/', 'E', 0, 0.0D0, 1.0D0, 1, 0, 0.0D0, D, E, M
     $,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSTEBZ( 'A', '/', 0, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, M
     $,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSTEBZ( 'A', 'E', -1, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, 
     $M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DSTEBZ( 'V', 'E', 0, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, M
     $,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSTEBZ( 'I', 'E', 0, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, M
     $,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSTEBZ( 'I', 'E', 1, 0.0D0, 0.0D0, 2, 1, 0.0D0, D, E, M
     $,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSTEBZ( 'I', 'E', 1, 0.0D0, 0.0D0, 1, 0, 0.0D0, D, E, M
     $,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSTEBZ( 'I', 'E', 1, 0.0D0, 0.0D0, 1, 2, 0.0D0, D, E, M
     $,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL AB_CHKXER( 'AB_DSTEBZ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_DSTEIN
*
         SRNAMT = 'AB_DSTEIN'
         INFOT = 1
         CALL AB_DSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_DSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_DSTEQR
*
         SRNAMT = 'AB_DSTEQR'
         INFOT = 1
         CALL AB_DSTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_DSTERF
*
         SRNAMT = 'AB_DSTERF'
         INFOT = 1
         CALL AB_DSTERF( -1, D, E, INFO )
         CALL AB_CHKXER( 'AB_DSTERF', INFOT, NOUT, LERR, OK )
         NT = NT + 1
*
*        AB_DSTEDC
*
         SRNAMT = 'AB_DSTEDC'
         INFOT = 1
         CALL AB_DSTEDC( '/', 0, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSTEDC( 'N', -1, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSTEDC( 'V', 2, D, E, Z, 1, W, 23, IW, 28, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DSTEDC( 'N', 1, D, E, Z, 1, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DSTEDC( 'I', 2, D, E, Z, 2, W, 0, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DSTEDC( 'V', 2, D, E, Z, 2, W, 0, IW, 28, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DSTEDC( 'N', 1, D, E, Z, 1, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DSTEDC( 'I', 2, D, E, Z, 2, W, 19, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DSTEDC( 'V', 2, D, E, Z, 2, W, 23, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_DSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_DSTEVD
*
         SRNAMT = 'AB_AB_DSTEVD'
         INFOT = 1
         CALL AB_AB_DSTEVD( '/', 0, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSTEVD( 'N', -1, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DSTEVD( 'V', 2, D, E, Z, 1, W, 19, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSTEVD( 'N', 1, D, E, Z, 1, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSTEVD( 'V', 2, D, E, Z, 2, W, 12, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSTEVD( 'N', 0, D, E, Z, 1, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSTEVD( 'V', 2, D, E, Z, 2, W, 19, IW, 11, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_DSTEV
*
         SRNAMT = 'AB_DSTEV '
         INFOT = 1
         CALL AB_DSTEV( '/', 0, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSTEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSTEV( 'N', -1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSTEV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSTEV( 'V', 2, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSTEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_AB_DSTEVX
*
         SRNAMT = 'AB_AB_DSTEVX'
         INFOT = 1
         CALL AB_AB_DSTEVX( '/', 'A', 0, D, E, 0.0D0, 0.0D0, 0, 0, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSTEVX( 'N', '/', 0, D, E, 0.0D0, 1.0D0, 1, 0, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSTEVX( 'N', 'A', -1, D, E, 0.0D0, 0.0D0, 0, 0, 0.0D
     $0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSTEVX( 'N', 'V', 1, D, E, 0.0D0, 0.0D0, 0, 0, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSTEVX( 'N', 'I', 1, D, E, 0.0D0, 0.0D0, 0, 0, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSTEVX( 'N', 'I', 1, D, E, 0.0D0, 0.0D0, 2, 1, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSTEVX( 'N', 'I', 2, D, E, 0.0D0, 0.0D0, 2, 1, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSTEVX( 'N', 'I', 1, D, E, 0.0D0, 0.0D0, 1, 2, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_DSTEVX( 'V', 'A', 2, D, E, 0.0D0, 0.0D0, 0, 0, 0.0D0
     $, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_DSTEVR
*
         N = 1
         SRNAMT = 'AB_AB_DSTEVR'
         INFOT = 1
         CALL AB_AB_DSTEVR( '/', 'A', 0, D, E, 0.0D0, 0.0D0, 1, 1, 0.0D0
     $, M,
     $                R, Z, 1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSTEVR( 'V', '/', 0, D, E, 0.0D0, 0.0D0, 1, 1, 0.0D0
     $, M,
     $                R, Z, 1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSTEVR( 'V', 'A', -1, D, E, 0.0D0, 0.0D0, 1, 1, 0.0D
     $0, M,
     $                R, Z, 1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSTEVR( 'V', 'V', 1, D, E, 0.0D0, 0.0D0, 1, 1, 0.0D0
     $, M,
     $                R, Z, 1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSTEVR( 'V', 'I', 1, D, E, 0.0D0, 0.0D0, 0, 1, 0.0D0
     $, M,
     $                W, Z, 1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         N = 2
         CALL AB_AB_DSTEVR( 'V', 'I', 2, D, E, 0.0D0, 0.0D0, 2, 1, 0.0D0
     $, M,
     $                W, Z, 1, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 14
         N = 1
         CALL AB_AB_DSTEVR( 'V', 'I', 1, D, E, 0.0D0, 0.0D0, 1, 1, 0.0D0
     $, M,
     $                W, Z, 0, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_DSTEVR( 'V', 'I', 1, D, E, 0.0D0, 0.0D0, 1, 1, 0.0D0
     $, M,
     $                W, Z, 1, IW, X, 20*N-1, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         INFOT = 19
         CALL AB_AB_DSTEVR( 'V', 'I', 1, D, E, 0.0D0, 0.0D0, 1, 1, 0.0D0
     $, M,
     $                W, Z, 1, IW, X, 20*N, IW( 2*N+1 ), 10*N-1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSTEVR', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_DSYEVD
*
         SRNAMT = 'AB_AB_DSYEVD'
         INFOT = 1
         CALL AB_AB_DSYEVD( '/', 'U', 0, A, 1, X, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYEVD( 'N', '/', 0, A, 1, X, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYEVD( 'N', 'U', -1, A, 1, X, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYEVD( 'N', 'U', 2, A, 1, X, W, 3, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYEVD( 'N', 'U', 1, A, 1, X, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYEVD( 'N', 'U', 2, A, 2, X, W, 4, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYEVD( 'V', 'U', 2, A, 2, X, W, 20, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYEVD( 'N', 'U', 1, A, 1, X, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYEVD( 'N', 'U', 2, A, 2, X, W, 5, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYEVD( 'V', 'U', 2, A, 2, X, W, 27, IW, 11, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_AB_DSYEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_DSYEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_DSYEVD_2STAGE( '/', 'U', 0, A, 1, X, W, 1, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_DSYEVD_2STAGE( 'V', 'U', 0, A, 1, X, W, 1, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_DSYEVD_2STAGE( 'N', '/', 0, A, 1, X, W, 1, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_DSYEVD_2STAGE( 'N', 'U', -1, A, 1, X, W, 1, IW, 1
     $, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_DSYEVD_2STAGE( 'N', 'U', 2, A, 1, X, W, 3, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_DSYEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 0, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_DSYEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 4, IW, 1,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 8
*         CALL AB_AB_AB_DSYEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 20, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_DSYEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 1, IW, 0,
     $ INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_DSYEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 25, IW, 0
     $, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 10
*         CALL AB_AB_AB_DSYEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 27, IW, 11, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_DSYEVD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_DSYEVR
*
         SRNAMT = 'AB_AB_DSYEVR'
         N = 1
         INFOT = 1
         CALL AB_AB_DSYEVR( '/', 'A', 'U', 0, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYEVR( 'V', '/', 'U', 0, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYEVR( 'V', 'A', '/', -1, A, 1, 0.0D0, 0.0D0, 1, 1,
     $                0.0D0, M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSYEVR( 'V', 'A', 'U', -1, A, 1, 0.0D0, 0.0D0, 1, 1,
     $                0.0D0, M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DSYEVR( 'V', 'A', 'U', 2, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYEVR( 'V', 'V', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 10
*
         CALL AB_AB_DSYEVR( 'V', 'I', 'U', 2, A, 2, 0.0D0, 0.0D0, 2, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_DSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 0, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_DSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N-1, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_DSYEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N-1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVR', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_AB_DSYEVR_2STAGE
*
         SRNAMT = 'AB_AB_AB_DSYEVR_2STAGE'
         N = 1
         INFOT = 1
         CALL AB_AB_AB_DSYEVR_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_DSYEVR_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'A', '/', -1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 2, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 0, IW, Q, 26*N, IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 18
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 0, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 20
         CALL AB_AB_AB_DSYEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, IW( 2*N+1 ), 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 12
*
*        AB_DSYEV
*
         SRNAMT = 'AB_DSYEV '
         INFOT = 1
         CALL AB_DSYEV( '/', 'U', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYEV( 'N', '/', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSYEV( 'N', 'U', -1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DSYEV( 'N', 'U', 2, A, 1, X, W, 3, INFO )
         CALL AB_CHKXER( 'AB_DSYEV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DSYEV( 'N', 'U', 1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        AB_AB_DSYEV_2STAGE
*
         SRNAMT = 'AB_AB_DSYEV_2STAGE '
         INFOT = 1
         CALL AB_AB_DSYEV_2STAGE( '/', 'U', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_DSYEV_2STAGE( 'V', 'U', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYEV_2STAGE( 'N', '/', 0, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYEV_2STAGE( 'N', 'U', -1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSYEV_2STAGE( 'N', 'U', 2, A, 1, X, W, 3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYEV_2STAGE( 'N', 'U', 1, A, 1, X, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_DSYEVX
*
         SRNAMT = 'AB_AB_DSYEVX'
         INFOT = 1
         CALL AB_AB_DSYEVX( '/', 'A', 'U', 0, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSYEVX( 'N', '/', 'U', 0, A, 1, 0.0D0, 1.0D0, 1, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSYEVX( 'N', 'A', '/', 0, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_DSYEVX( 'N', 'A', 'U', -1, A, 1, 0.0D0, 0.0D0, 0, 0,
     $                0.0D0, M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DSYEVX( 'N', 'A', 'U', 2, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSYEVX( 'N', 'V', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSYEVX( 'N', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSYEVX( 'N', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 2, 1, 
     $0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYEVX( 'N', 'I', 'U', 2, A, 2, 0.0D0, 0.0D0, 2, 1, 
     $0.0D0,
     $                M, X, Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_DSYEVX( 'N', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 2, 
     $0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_DSYEVX( 'V', 'A', 'U', 2, A, 2, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_DSYEVX( 'V', 'A', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSYEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_AB_AB_DSYEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_DSYEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_DSYEVX_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                 0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_DSYEVX_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                 0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0D0, 1.0D0, 1, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'A', '/', 0, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 2, 1, 0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 2, 1, 0.0D0,
     $                M, X, Z, 1, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 2, 0.0D0,
     $                M, X, Z, 1, W, 8, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'A', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 0, W, 16, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 17
         CALL AB_AB_AB_DSYEVX_2STAGE( 'N', 'A', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSYEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 13
*
*        AB_AB_DSPEVD
*
         SRNAMT = 'AB_AB_DSPEVD'
         INFOT = 1
         CALL AB_AB_DSPEVD( '/', 'U', 0, A, X, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSPEVD( 'N', '/', 0, A, X, Z, 1, W, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSPEVD( 'N', 'U', -1, A, X, Z, 1, W, 1, IW, 1, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSPEVD( 'V', 'U', 2, A, X, Z, 1, W, 23, IW, 12, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSPEVD( 'N', 'U', 1, A, X, Z, 1, W, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSPEVD( 'N', 'U', 2, A, X, Z, 1, W, 3, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSPEVD( 'V', 'U', 2, A, X, Z, 2, W, 16, IW, 12, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSPEVD( 'N', 'U', 2, A, X, Z, 1, W, 4, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSPEVD( 'V', 'U', 2, A, X, Z, 2, W, 23, IW, 11, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_DSPEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_DSPEV
*
         SRNAMT = 'AB_DSPEV '
         INFOT = 1
         CALL AB_DSPEV( '/', 'U', 0, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_DSPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSPEV( 'N', '/', 0, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_DSPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSPEV( 'N', 'U', -1, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_DSPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSPEV( 'V', 'U', 2, A, W, Z, 1, X, INFO )
         CALL AB_CHKXER( 'AB_DSPEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_DSPEVX
*
         SRNAMT = 'AB_AB_DSPEVX'
         INFOT = 1
         CALL AB_AB_DSPEVX( '/', 'A', 'U', 0, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSPEVX( 'N', '/', 'U', 0, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSPEVX( 'N', 'A', '/', 0, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_DSPEVX( 'N', 'A', 'U', -1, A, 0.0D0, 0.0D0, 0, 0, 0.
     $0D0,
     $                M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSPEVX( 'N', 'V', 'U', 1, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSPEVX( 'N', 'I', 'U', 1, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_DSPEVX( 'N', 'I', 'U', 1, A, 0.0D0, 0.0D0, 2, 1, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSPEVX( 'N', 'I', 'U', 2, A, 0.0D0, 0.0D0, 2, 1, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSPEVX( 'N', 'I', 'U', 1, A, 0.0D0, 0.0D0, 1, 2, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_DSPEVX( 'V', 'A', 'U', 2, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSPEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*     Test error exits for the SB path.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SB' ) ) THEN
*
*        AB_DSBTRD
*
         SRNAMT = 'AB_DSBTRD'
         INFOT = 1
         CALL AB_DSBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DSBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_DSYTRD_SB2ST
*
         SRNAMT = 'AB_DSYTRD_SB2ST'
         INFOT = 1
         CALL AB_DSYTRD_SB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRD_SB2ST( 'N', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSYTRD_SB2ST( 'N', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSYTRD_SB2ST( 'N', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSYTRD_SB2ST( 'N', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DSYTRD_SB2ST( 'N', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DSYTRD_SB2ST( 'N', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DSYTRD_SB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DSYTRD_SB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_DSYTRD_SB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_DSBEVD
*
         SRNAMT = 'AB_AB_DSBEVD'
         INFOT = 1
         CALL AB_AB_DSBEVD( '/', 'U', 0, 0, A, 1, X, Z, 1, W, 1, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSBEVD( 'N', '/', 0, 0, A, 1, X, Z, 1, W, 1, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSBEVD( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSBEVD( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DSBEVD( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 4, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 1, W, 25, IW, 12
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 0, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSBEVD( 'N', 'U', 2, 0, A, 1, X, Z, 1, W, 3, IW, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSBEVD( 'V', 'U', 2, 0, A, 1, X, Z, 2, W, 18, IW, 12
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_DSBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, IW, 0, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_DSBEVD( 'V', 'U', 2, 0, A, 1, X, Z, 2, W, 25, IW, 11
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_AB_DSBEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_DSBEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_DSBEVD_2STAGE( '/', 'U', 0, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_DSBEVD_2STAGE( 'V', 'U', 0, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_DSBEVD_2STAGE( 'N', '/', 0, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_DSBEVD_2STAGE( 'N', 'U', -1, 0, A, 1, X, Z, 1, W,
     $                                         1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_DSBEVD_2STAGE( 'N', 'U', 0, -1, A, 1, X, Z, 1, W,
     $                                         1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_DSBEVD_2STAGE( 'N', 'U', 2, 1, A, 1, X, Z, 1, W,
     $                                        4, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 9
*         CALL AB_AB_AB_DSBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 1, W,
*     $                                      25, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_DSBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1, W,
     $                                        0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 11
         CALL AB_AB_AB_DSBEVD_2STAGE( 'N', 'U', 2, 0, A, 1, X, Z, 1, W,
     $                                        3, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 11
*         CALL AB_AB_AB_DSBEVD_2STAGE( 'V', 'U', 2, 0, A, 1, X, Z, 2, W,
*     $                                      18, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_AB_DSBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1, W,
     $                                        1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 13
*         CALL AB_AB_AB_DSBEVD_2STAGE( 'V', 'U', 2, 0, A, 1, X, Z, 2, W,
*     $                                      25, IW, 11, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_DSBEVD_2STAGE', INFOT, NOUT, LERR, OK )
*         NT = NT + 12
         NT = NT + 9
*
*        AB_DSBEV
*
         SRNAMT = 'AB_DSBEV '
         INFOT = 1
         CALL AB_DSBEV( '/', 'U', 0, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DSBEV( 'N', '/', 0, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DSBEV( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DSBEV( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DSBEV( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DSBEV( 'V', 'U', 2, 0, A, 1, X, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_DSBEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_DSBEV_2STAGE
*
         SRNAMT = 'AB_AB_DSBEV_2STAGE '
         INFOT = 1
         CALL AB_AB_DSBEV_2STAGE( '/', 'U', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_DSBEV_2STAGE( 'V', 'U', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSBEV_2STAGE( 'N', '/', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSBEV_2STAGE( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, 0, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSBEV_2STAGE( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, 0, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DSBEV_2STAGE( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSBEV_2STAGE( 'N', 'U', 2, 0, A, 1, X, Z, 0, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSBEV_2STAGE( 'N', 'U', 0, 0, A, 1, X, Z, 1, W, 0, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DSBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_AB_DSBEVX
*
         SRNAMT = 'AB_AB_DSBEVX'
         INFOT = 1
         CALL AB_AB_DSBEVX( '/', 'A', 'U', 0, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DSBEVX( 'N', '/', 'U', 0, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DSBEVX( 'N', 'A', '/', 0, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DSBEVX( 'N', 'A', 'U', -1, 0, A, 1, Q, 1, 0.0D0, 0.0
     $D0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DSBEVX( 'N', 'A', 'U', 0, -1, A, 1, Q, 1, 0.0D0, 0.0
     $D0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DSBEVX( 'N', 'A', 'U', 2, 1, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DSBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 2, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DSBEVX( 'N', 'V', 'U', 1, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DSBEVX( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DSBEVX( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 2,
     $                1, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_DSBEVX( 'N', 'I', 'U', 2, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 2,
     $                1, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_DSBEVX( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 1,
     $                2, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_DSBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 2, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_DSBEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_AB_AB_DSBEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_DSBEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_DSBEVX_2STAGE( '/', 'A', 'U', 0, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_DSBEVX_2STAGE( 'V', 'A', 'U', 0, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', '/', 'U', 0, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'A', '/', 0, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'A', 'U', -1, 0, A, 1, Q, 1, 
     $0.0D0,
     $           0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'A', 'U', 0, -1, A, 1, Q, 1, 
     $0.0D0,
     $           0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 7
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'A', 'U', 2, 1, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 9
*         CALL AB_AB_AB_DSBEVX_2STAGE( 'V', 'A', 'U', 2, 0, A, 1, Q, 1, 0.0D0,
*     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 2, W, 0, IW, I3, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'V', 'U', 1, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 2, 1, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'I', 'U', 2, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 2, 1, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1, 0
     $.0D0,
     $          0.0D0, 1, 2, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 18
*         CALL AB_AB_AB_DSBEVX_2STAGE( 'V', 'A', 'U', 2, 0, A, 1, Q, 2, 0.0D0,
*     $          0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_AB_DSBEVX_2STAGE( 'N', 'A', 'U', 0, 0, A, 1, Q, 1, 0
     $.0D0,
     $           0.0D0, 0, 0, 0.0D0, M, X, Z, 1, W, 0, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DSBEVX_2STAGE', INFOT, NOUT, LERR, OK
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
*     End of AB_DERRST
*
      END
