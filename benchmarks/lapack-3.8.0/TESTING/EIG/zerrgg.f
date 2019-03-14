*> \brief \b AB_ZERRGG
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRGG( PATH, NUNIT )
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
*> AB_ZERRGG tests the error exits for AB_ZGGES, AB_AB_ZGGESX, AB_ZGGEV, AB_AB_ZGGEVX,
*> AB_AB_ZGGES3, AB_AB_ZGGEV3, AB_ZGGGLM, AB_ZGGHRD, AB_ZGGAB_LSE, AB_ZGGQRF, AB_ZGGRQF,
*> AB_AB_ZGGSVD3, AB_AB_ZGGSVP3, AB_ZHGEQZ, AB_ZTGEVC, AB_ZTGEXC, AB_ZTGSEN, AB_ZTGSJA,
*> AB_ZTGSNA, AB_ZTGSYL, and ZUNCSD.
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
*> \date June 2016
*
*> \ingroup complex16_eig
*
*  =====================================================================
      SUBROUTINE AB_ZERRGG( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 3, LW = 6*NMAX )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            DUMMYK, DUMMYL, I, IFST, IHI, ILO, ILST, INFO,
     $                   J, M, NCYCLE, NT, SDIM, LWORK
      DOUBLE PRECISION   ANRM, BNRM, DIF, SCALE, TOLA, TOLB
*     ..
*     .. Local Arrays ..
      LOGICAL            BW( NMAX ), SEL( NMAX )
      INTEGER            IW( LW ), IDUM(NMAX)
      DOUBLE PRECISION   LS( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RCE( NMAX ), RCV( NMAX ), RS( NMAX ), RW( LW )
      COMPLEX*16         A( NMAX, NMAX ), ALPHA( NMAX ),
     $                   B( NMAX, NMAX ), BETA( NMAX ), Q( NMAX, NMAX ),
     $                   TAU( NMAX ), U( NMAX, NMAX ), V( NMAX, NMAX ),
     $                   W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN, AB_ZLCTES, AB_ZLCTSX
      EXTERNAL           AB_AB_LSAMEN, AB_ZLCTES, AB_ZLCTSX
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_ZGGES,  AB_AB_ZGGESX, AB_ZGGEV,  
     $AB_AB_ZGGEVX, AB_ZGGGLM,
     $                   AB_ZGGHRD, AB_ZGGAB_LSE, AB_ZGGQRF, AB_ZGGRQF,
     $                   AB_ZHGEQZ, AB_ZTGEVC, AB_ZTGEXC, AB_ZTGSEN, AB_
     $ZTGSJA, AB_ZTGSNA,
     $                   AB_ZTGSYL, ZUNCSD, AB_AB_ZGGES3, AB_AB_ZGGEV3, 
     $AB_ZGGHD3,
     $                   AB_AB_ZGGSVD3, AB_AB_ZGGSVP3
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
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         SEL( J ) = .TRUE.
         DO 10 I = 1, NMAX
            A( I, J ) = ZERO
            B( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, NMAX
         A( I, I ) = ONE
         B( I, I ) = ONE
   30 CONTINUE
      OK = .TRUE.
      TOLA = 1.0D0
      TOLB = 1.0D0
      IFST = 1
      ILST = 1
      NT = 0
      LWORK = 1
*
*     Test error exits for the GG path.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GG' ) ) THEN
*
*        AB_ZGGHRD
*
         SRNAMT = 'AB_ZGGHRD'
         INFOT = 1
         CALL AB_ZGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_ZGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_ZGGHD3
*
         SRNAMT = 'AB_ZGGHD3'
         INFOT = 1
         CALL AB_ZGGHD3( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGHD3( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGHD3( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, W, 
     $LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGGHD3( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGHD3( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGGHD3( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZGGHD3( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZGGHD3( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZGGHD3( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGHD3', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_ZHGEQZ
*
         SRNAMT = 'AB_ZHGEQZ'
         INFOT = 1
         CALL AB_ZHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, ALPHA, BET
     $A,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_ZHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_ZHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_ZTGEVC
*
         SRNAMT = 'AB_ZTGEVC'
         INFOT = 1
         CALL AB_ZTGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M
     $,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZTGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZTGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZTGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_ZTGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        AB_AB_ZGGSVD3
*
         SRNAMT = 'AB_AB_ZGGSVD3'
         INFOT = 1
         CALL AB_AB_ZGGSVD3( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGGSVD3( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGGSVD3( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGGSVD3( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A,
     $ 1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGGSVD3( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A,
     $ 1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGGSVD3( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A,
     $ 1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZGGSVD3( 'N', 'N', 'N', 2, 1, 1, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZGGSVD3( 'N', 'N', 'N', 1, 1, 2, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_ZGGSVD3( 'U', 'N', 'N', 2, 2, 2, DUMMYK, DUMMYL, A, 
     $2, B,
     $                 2, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZGGSVD3( 'N', 'V', 'N', 2, 2, 2, DUMMYK, DUMMYL, A, 
     $2, B,
     $                 2, R1, R2, U, 2, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_ZGGSVD3( 'N', 'N', 'Q', 2, 2, 2, DUMMYK, DUMMYL, A, 
     $2, B,
     $                 2, R1, R2, U, 2, V, 2, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVD3', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_ZGGSVP3
*
         SRNAMT = 'AB_AB_ZGGSVP3'
         INFOT = 1
         CALL AB_AB_ZGGSVP3( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGGSVP3( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGGSVP3( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGGSVP3( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, 
     $TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGGSVP3( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, 
     $TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGGSVP3( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, 
     $TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZGGSVP3( 'N', 'N', 'N', 2, 1, 1, A, 1, B, 1, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZGGSVP3( 'N', 'N', 'N', 1, 2, 1, A, 1, B, 1, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_ZGGSVP3( 'U', 'N', 'N', 2, 2, 2, A, 2, B, 2, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZGGSVP3( 'N', 'V', 'N', 2, 2, 2, A, 2, B, 2, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 2, V, 1, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_ZGGSVP3( 'N', 'N', 'Q', 2, 2, 2, A, 2, B, 2, TOLA, T
     $OLB,
     $                DUMMYK, DUMMYL, U, 2, V, 2, Q, 1, IW, RW, TAU, W,
     $                LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGSVP3', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_ZTGSJA
*
         SRNAMT = 'AB_ZTGSJA'
         INFOT = 1
         CALL AB_ZTGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, 
     $B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, 
     $B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZTGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, 
     $B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_ZTGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_ZTGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL AB_ZTGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_ZTGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        AB_ZGGGLM
*
         SRNAMT = 'AB_ZGGGLM'
         INFOT = 1
         CALL AB_ZGGGLM( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGGLM( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGGLM( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGGLM( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGGLM( 1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGGLM( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGGGLM( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZGGGLM( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGGLM', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the AB_LSE path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'AB_LSE' ) ) THEN
*
*        AB_ZGGAB_LSE
*
         SRNAMT = 'AB_ZGGAB_LSE'
         INFOT = 1
         CALL AB_ZGGAB_LSE( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGAB_LSE( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGAB_LSE( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGAB_LSE( 0, 0, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGAB_LSE( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGAB_LSE( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGGAB_LSE( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZGGAB_LSE( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGGAB_LSE', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the CSD path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'CSD' ) ) THEN
*
*        ZUNCSD
*
         SRNAMT = 'ZUNCSD'
         INFOT = 7
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 -1, 0, 0, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, -1, 0, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, -1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, -1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, -1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, -1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, -1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 26
         CALL ZUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 -1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'ZUNCSD', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        AB_ZGGQRF
*
         SRNAMT = 'AB_ZGGQRF'
         INFOT = 1
         CALL AB_ZGGQRF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGQRF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGQRF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGQRF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGGQRF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZGGQRF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_ZGGRQF
*
         SRNAMT = 'AB_ZGGRQF'
         INFOT = 1
         CALL AB_ZGGRQF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGRQF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGRQF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGRQF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGGRQF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZGGRQF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGGRQF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*     Test error exits for the ZGS, ZGV, ZGX, and ZXV paths.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'ZGS' ) .OR.
     $         AB_AB_LSAMEN( 3, PATH, 'ZGV' ) .OR.
     $         AB_AB_LSAMEN( 3, PATH, 'ZGX' ) .OR. AB_AB_LSAMEN( 3, PATH
     $, 'ZXV' ) )
     $          THEN
*
*        AB_ZGGES
*
         SRNAMT = 'AB_ZGGES '
         INFOT = 1
         CALL AB_ZGGES( '/', 'N', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGES( 'N', '/', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGES( 'N', 'V', '/', AB_ZLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGES( 'N', 'V', 'S', AB_ZLCTES, -1, A, 1, B, 1, SDIM, 
     $ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGGES( 'N', 'V', 'S', AB_ZLCTES, 1, A, 0, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZGGES( 'N', 'V', 'S', AB_ZLCTES, 1, A, 1, B, 0, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_ZGGES( 'N', 'V', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 0, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_ZGGES( 'V', 'V', 'S', AB_ZLCTES, 2, A, 2, B, 2, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_ZGGES( 'N', 'V', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 0, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_ZGGES( 'V', 'V', 'S', AB_ZLCTES, 2, A, 2, B, 2, SDIM, A
     $LPHA,
     $               BETA, Q, 2, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_ZGGES( 'V', 'V', 'S', AB_ZLCTES, 2, A, 2, B, 2, SDIM, A
     $LPHA,
     $               BETA, Q, 2, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_ZGGES ', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_ZGGES3
*
         SRNAMT = 'AB_AB_ZGGES3'
         INFOT = 1
         CALL AB_AB_ZGGES3( '/', 'N', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGGES3( 'N', '/', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGGES3( 'N', 'V', '/', AB_ZLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGGES3( 'N', 'V', 'S', AB_ZLCTES, -1, A, 1, B, 1, SD
     $IM,
     $                ALPHA, BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGGES3( 'N', 'V', 'S', AB_ZLCTES, 1, A, 0, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZGGES3( 'N', 'V', 'S', AB_ZLCTES, 1, A, 1, B, 0, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_ZGGES3( 'N', 'V', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 0, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_ZGGES3( 'V', 'V', 'S', AB_ZLCTES, 2, A, 2, B, 2, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_ZGGES3( 'N', 'V', 'S', AB_ZLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 0, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_ZGGES3( 'V', 'V', 'S', AB_ZLCTES, 2, A, 2, B, 2, SDI
     $M, ALPHA,
     $                BETA, Q, 2, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZGGES3( 'V', 'V', 'S', AB_ZLCTES, 2, A, 2, B, 2, SDI
     $M, ALPHA,
     $                BETA, Q, 2, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGES3', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_ZGGESX
*
         SRNAMT = 'AB_AB_ZGGESX'
         INFOT = 1
         CALL AB_AB_ZGGESX( '/', 'N', 'S', AB_ZLCTSX, 'N', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGGESX( 'N', '/', 'S', AB_ZLCTSX, 'N', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGGESX( 'V', 'V', '/', AB_ZLCTSX, 'N', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, '/', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', -1, A, 1, B, 
     $1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', 1, A, 0, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', 1, A, 1, B, 0
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 0, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', 2, A, 2, B, 2
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 0, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', 2, A, 2, B, 2
     $, SDIM,
     $                ALPHA, BETA, Q, 2, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'B', 2, A, 2, B, 2
     $, SDIM,
     $                ALPHA, BETA, Q, 2, U, 2, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL AB_AB_ZGGESX( 'V', 'V', 'S', AB_ZLCTSX, 'V', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 32, RW, IW,
     $                0, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGESX', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_ZGGEV
*
         SRNAMT = 'AB_ZGGEV '
         INFOT = 1
         CALL AB_ZGGEV( '/', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGGEV( 'N', '/', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGGEV( 'V', 'V', -1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 
     $1,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGGEV( 'V', 'V', 1, A, 0, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGGEV( 'V', 'V', 1, A, 1, B, 0, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZGGEV( 'N', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 0, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZGGEV( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 1, U, 2
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZGGEV( 'V', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, U, 0
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZGGEV( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_ZGGEV( 'V', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZGGEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_ZGGEV3
*
         SRNAMT = 'AB_AB_ZGGEV3'
         INFOT = 1
         CALL AB_AB_ZGGEV3( '/', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGGEV3( 'N', '/', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGGEV3( 'V', 'V', -1, A, 1, B, 1, ALPHA, BETA, Q, 1,
     $ U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGGEV3( 'V', 'V', 1, A, 0, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGGEV3( 'V', 'V', 1, A, 1, B, 0, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZGGEV3( 'N', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 0, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZGGEV3( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 1, 
     $U, 2,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZGGEV3( 'V', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, 
     $U, 0,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZGGEV3( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGGEV3( 'V', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEV3', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_ZGGEVX
*
         SRNAMT = 'AB_AB_ZGGEVX'
         INFOT = 1
         CALL AB_AB_ZGGEVX( '/', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGGEVX( 'N', '/', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGGEVX( 'N', 'N', '/', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGGEVX( 'N', 'N', 'N', '/', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGGEVX( 'N', 'N', 'N', 'N', -1, A, 1, B, 1, ALPHA, B
     $ETA,
     $                Q, 1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE,
     $                RCV, W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGGEVX( 'N', 'N', 'N', 'N', 1, A, 0, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 0, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                0, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZGGEVX( 'N', 'V', 'N', 'N', 2, A, 2, B, 2, ALPHA, BE
     $TA, Q,
     $                1, U, 2, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 0, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGGEVX( 'N', 'N', 'V', 'N', 2, A, 2, B, 2, ALPHA, BE
     $TA, Q,
     $                2, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 25
         CALL AB_AB_ZGGEVX( 'N', 'N', 'V', 'N', 2, A, 2, B, 2, ALPHA, BE
     $TA, Q,
     $                2, U, 2, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 0, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGGEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_ZTGEXC
*
         SRNAMT = 'AB_ZTGEXC'
         INFOT = 3
         CALL AB_ZTGEXC( .TRUE., .TRUE., -1, A, 1, B, 1, Q, 1, Z, 1, IFS
     $T,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTGEXC( .TRUE., .TRUE., 1, A, 0, B, 1, Q, 1, Z, 1, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZTGEXC( .TRUE., .TRUE., 1, A, 1, B, 0, Q, 1, Z, 1, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZTGEXC( .FALSE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, 
     $IFST,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZTGEXC( .TRUE., .FALSE., 1, A, 1, B, 1, Q, 1, Z, 0, 
     $IFST,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_ZTGEXC', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_ZTGSEN
*
         SRNAMT = 'AB_ZTGSEN'
         INFOT = 1
         CALL AB_ZTGSEN( -1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZTGSEN( 1, .TRUE., .TRUE., SEL, -1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 0, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 0, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 0, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 0, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL AB_ZTGSEN( 3, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, -5, IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL AB_ZTGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL AB_ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL AB_ZTGSEN( 5, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_ZTGSEN', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_ZTGSNA
*
         SRNAMT = 'AB_ZTGSNA'
         INFOT = 1
         CALL AB_ZTGSNA( '/', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTGSNA( 'B', '/', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTGSNA( 'B', 'A', SEL, -1, A, 1, B, 1, Q, 1, U, 1, R1, 
     $R2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZTGSNA( 'B', 'A', SEL, 1, A, 0, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZTGSNA( 'B', 'A', SEL, 1, A, 1, B, 0, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 0, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 0, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                0, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 0, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSNA', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_ZTGSYL
*
         SRNAMT = 'AB_ZTGSYL'
         INFOT = 1
         CALL AB_ZTGSYL( '/', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZTGSYL( 'N', -1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z,
     $ 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZTGSYL( 'N', 0, 0, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZTGSYL( 'N', 0, 1, 0, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZTGSYL( 'N', 0, 1, 1, A, 0, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZTGSYL( 'N', 0, 1, 1, A, 1, B, 0, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 0, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 0, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 0, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $0,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_ZTGSYL( 'N', 1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_ZTGSYL( 'N', 2, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_ZTGSYL', INFOT, NOUT, LERR, OK )
         NT = NT + 12
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
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits (',
     $      I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of AB_ZERRGG
*
      END
