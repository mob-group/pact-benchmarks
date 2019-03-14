*> \brief \b AB_CERRGG
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRGG( PATH, NUNIT )
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
*> AB_CERRGG tests the error exits for AB_CGGES, AB_AB_CGGESX, AB_CGGEV, AB_AB_CGGEVX,
*> AB_AB_CGGES3, AB_AB_CGGEV3, AB_CGGGLM, AB_CGGHRD, AB_CGGAB_LSE, AB_CGGQRF, AB_CGGRQF,
*> AB_AB_CGGSVD3, AB_AB_CGGSVP3, AB_CHGEQZ, AB_CTGEVC, AB_CTGEXC, AB_CTGSEN, AB_CTGSJA,
*> AB_CTGSNA, AB_CTGSYL, and CUNCSD.
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
*> \ingroup complex_eig
*
*  =====================================================================
      SUBROUTINE AB_CERRGG( PATH, NUNIT )
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
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            DUMMYK, DUMMYL, I, IFST, IHI, ILO, ILST, INFO,
     $                   J, M, NCYCLE, NT, SDIM, LWORK
      REAL               ANRM, BNRM, DIF, SCALE, TOLA, TOLB
*     ..
*     .. Local Arrays ..
      LOGICAL            BW( NMAX ), SEL( NMAX )
      INTEGER            IW( LW ), IDUM(NMAX)
      REAL               LS( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RCE( NMAX ), RCV( NMAX ), RS( NMAX ), RW( LW )
      COMPLEX            A( NMAX, NMAX ), ALPHA( NMAX ),
     $                   B( NMAX, NMAX ), BETA( NMAX ), Q( NMAX, NMAX ),
     $                   TAU( NMAX ), U( NMAX, NMAX ), V( NMAX, NMAX ),
     $                   W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_CLCTES, AB_CLCTSX, AB_AB_LSAMEN
      EXTERNAL           AB_CLCTES, AB_CLCTSX, AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGGES, AB_AB_CGGESX, AB_CGGEV, AB_AB_CGGEVX,
     $ AB_CGGGLM, AB_CGGHRD,
     $                   AB_CGGAB_LSE, AB_CGGQRF, AB_CGGRQF, AB_CHGEQZ,
     $                   AB_CHKXER, AB_CTGEVC, AB_CTGEXC, AB_CTGSEN, AB_
     $CTGSJA, AB_CTGSNA,
     $                   AB_CTGSYL, CUNCSD, AB_AB_CGGES3, AB_AB_CGGEV3, 
     $AB_CGGHD3,
     $                   AB_AB_CGGSVD3, AB_AB_CGGSVP3
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
      TOLA = 1.0E0
      TOLB = 1.0E0
      IFST = 1
      ILST = 1
      NT = 0
      LWORK = 1
*
*     Test error exits for the GG path.
*
      IF( AB_AB_LSAMEN( 2, C2, 'GG' ) ) THEN
*
*        AB_CGGHRD
*
         SRNAMT = 'AB_CGGHRD'
         INFOT = 1
         CALL AB_CGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_CGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_CGGHD3
*
         SRNAMT = 'AB_CGGHD3'
         INFOT = 1
         CALL AB_CGGHD3( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGHD3( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGHD3( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, W, 
     $LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGGHD3( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGHD3( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGGHD3( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CGGHD3( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CGGHD3( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CGGHD3( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGHD3', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_CHGEQZ
*
         SRNAMT = 'AB_CHGEQZ'
         INFOT = 1
         CALL AB_CHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, ALPHA, BET
     $A,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_CHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_CHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, ALPHA, BETA
     $,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_CTGEVC
*
         SRNAMT = 'AB_CTGEVC'
         INFOT = 1
         CALL AB_CTGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M
     $,
     $                W, RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CTGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CTGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M,
     $ W,
     $                RW, INFO )
         CALL AB_CHKXER( 'AB_CTGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        AB_AB_CGGSVD3
*
         SRNAMT = 'AB_AB_CGGSVD3'
         INFOT = 1
         CALL AB_AB_CGGSVD3( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGGSVD3( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGGSVD3( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CGGSVD3( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A,
     $ 1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGGSVD3( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A,
     $ 1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CGGSVD3( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A,
     $ 1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CGGSVD3( 'N', 'N', 'N', 2, 1, 1, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CGGSVD3( 'N', 'N', 'N', 1, 1, 2, DUMMYK, DUMMYL, A, 
     $1, B,
     $                 1, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_CGGSVD3( 'U', 'N', 'N', 2, 2, 2, DUMMYK, DUMMYL, A, 
     $2, B,
     $                 2, R1, R2, U, 1, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CGGSVD3( 'N', 'V', 'N', 2, 2, 2, DUMMYK, DUMMYL, A, 
     $2, B,
     $                 2, R1, R2, U, 2, V, 1, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_CGGSVD3( 'N', 'N', 'Q', 2, 2, 2, DUMMYK, DUMMYL, A, 
     $2, B,
     $                 2, R1, R2, U, 2, V, 2, Q, 1, W, LWORK, RW, IDUM,
     $                 INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVD3', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_CGGSVP3
*
         SRNAMT = 'AB_AB_CGGSVP3'
         INFOT = 1
         CALL AB_AB_CGGSVP3( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGGSVP3( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGGSVP3( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CGGSVP3( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, 
     $TOLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGGSVP3( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, 
     $TOLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CGGSVP3( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, 
     $TOLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CGGSVP3( 'N', 'N', 'N', 2, 1, 1, A, 1, B, 1, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CGGSVP3( 'N', 'N', 'N', 1, 2, 1, A, 1, B, 1, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_CGGSVP3( 'U', 'N', 'N', 2, 2, 2, A, 2, B, 2, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CGGSVP3( 'N', 'V', 'N', 2, 2, 2, A, 2, B, 2, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 2, V, 1, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_CGGSVP3( 'N', 'N', 'Q', 2, 2, 2, A, 2, B, 2, TOLA, T
     $OLB,
     $                 DUMMYK, DUMMYL, U, 2, V, 2, Q, 1, IW, RW, TAU, W,
     $                 LWORK, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGSVP3', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_CTGSJA
*
         SRNAMT = 'AB_CTGSJA'
         INFOT = 1
         CALL AB_CTGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, 
     $B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, 
     $B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CTGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, 
     $B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_CTGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_CTGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL AB_CTGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B
     $,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO )
         CALL AB_CHKXER( 'AB_CTGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        AB_CGGGLM
*
         SRNAMT = 'AB_CGGGLM'
         INFOT = 1
         CALL AB_CGGGLM( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGGLM( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGGLM( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGGLM( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGGLM( 1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGGLM( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGGGLM( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CGGGLM( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGGLM', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the AB_LSE path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'AB_LSE' ) ) THEN
*
*        AB_CGGAB_LSE
*
         SRNAMT = 'AB_CGGAB_LSE'
         INFOT = 1
         CALL AB_CGGAB_LSE( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGAB_LSE( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGAB_LSE( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, L
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGAB_LSE( 0, 0, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGAB_LSE( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGAB_LSE( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGGAB_LSE( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CGGAB_LSE( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CGGAB_LSE', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the CSD path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'CSD' ) ) THEN
*
*        CUNCSD
*
         SRNAMT = 'CUNCSD'
         INFOT = 7
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 -1, 0, 0, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, -1, 0, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, -1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, -1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, -1, A, 1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, -1, A, 1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, -1, A,
     $                 1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         INFOT = 26
         CALL CUNCSD( 'Y', 'Y', 'Y', 'Y', 'N', 'N',
     $                 1, 1, 1, A, 1, A,
     $                 1, A, 1, A, 1, RS,
     $                 A, 1, A, 1, A, 1, A,
     $                 -1, W, LW, RW, LW, IW, INFO )
         CALL AB_CHKXER( 'CUNCSD', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        AB_CGGQRF
*
         SRNAMT = 'AB_CGGQRF'
         INFOT = 1
         CALL AB_CGGQRF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGQRF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGQRF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGQRF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CGGQRF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CGGQRF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_CGGRQF
*
         SRNAMT = 'AB_CGGRQF'
         INFOT = 1
         CALL AB_CGGRQF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGRQF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGRQF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO 
     $)
         CALL AB_CHKXER( 'AB_CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGRQF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CGGRQF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL AB_CHKXER( 'AB_CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CGGRQF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGGRQF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*     Test error exits for the CGS, CGV, CGX, and CXV paths.
*
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'CGS' ) .OR.
     $         AB_AB_LSAMEN( 3, PATH, 'CGV' ) .OR.
     $         AB_AB_LSAMEN( 3, PATH, 'CGX' ) .OR. AB_AB_LSAMEN( 3, PATH
     $, 'CXV' ) )
     $          THEN
*
*        AB_CGGES
*
         SRNAMT = 'AB_CGGES '
         INFOT = 1
         CALL AB_CGGES( '/', 'N', 'S', AB_CLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGES( 'N', '/', 'S', AB_CLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGES( 'N', 'V', '/', AB_CLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGES( 'N', 'V', 'S', AB_CLCTES, -1, A, 1, B, 1, SDIM, 
     $ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGGES( 'N', 'V', 'S', AB_CLCTES, 1, A, 0, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CGGES( 'N', 'V', 'S', AB_CLCTES, 1, A, 1, B, 0, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_CGGES( 'N', 'V', 'S', AB_CLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 0, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_CGGES( 'V', 'V', 'S', AB_CLCTES, 2, A, 2, B, 2, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_CGGES( 'N', 'V', 'S', AB_CLCTES, 1, A, 1, B, 1, SDIM, A
     $LPHA,
     $               BETA, Q, 1, U, 0, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_CGGES( 'V', 'V', 'S', AB_CLCTES, 2, A, 2, B, 2, SDIM, A
     $LPHA,
     $               BETA, Q, 2, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_CGGES( 'V', 'V', 'S', AB_CLCTES, 2, A, 2, B, 2, SDIM, A
     $LPHA,
     $               BETA, Q, 2, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_CGGES ', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_CGGES3
*
         SRNAMT = 'AB_AB_CGGES3'
         INFOT = 1
         CALL AB_AB_CGGES3( '/', 'N', 'S', AB_CLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGGES3( 'N', '/', 'S', AB_CLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGGES3( 'N', 'V', '/', AB_CLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGGES3( 'N', 'V', 'S', AB_CLCTES, -1, A, 1, B, 1, SD
     $IM,
     $                ALPHA, BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CGGES3( 'N', 'V', 'S', AB_CLCTES, 1, A, 0, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CGGES3( 'N', 'V', 'S', AB_CLCTES, 1, A, 1, B, 0, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_CGGES3( 'N', 'V', 'S', AB_CLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 0, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_CGGES3( 'V', 'V', 'S', AB_CLCTES, 2, A, 2, B, 2, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_CGGES3( 'N', 'V', 'S', AB_CLCTES, 1, A, 1, B, 1, SDI
     $M, ALPHA,
     $                BETA, Q, 1, U, 0, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_CGGES3( 'V', 'V', 'S', AB_CLCTES, 2, A, 2, B, 2, SDI
     $M, ALPHA,
     $                BETA, Q, 2, U, 1, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CGGES3( 'V', 'V', 'S', AB_CLCTES, 2, A, 2, B, 2, SDI
     $M, ALPHA,
     $                BETA, Q, 2, U, 2, W, 1, RW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGES3', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_CGGESX
*
         SRNAMT = 'AB_AB_CGGESX'
         INFOT = 1
         CALL AB_AB_CGGESX( '/', 'N', 'S', AB_CLCTSX, 'N', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGGESX( 'N', '/', 'S', AB_CLCTSX, 'N', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGGESX( 'V', 'V', '/', AB_CLCTSX, 'N', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, '/', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', -1, A, 1, B, 
     $1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', 1, A, 0, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', 1, A, 1, B, 0
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 0, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', 2, A, 2, B, 2
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 0, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', 2, A, 2, B, 2
     $, SDIM,
     $                ALPHA, BETA, Q, 2, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'B', 2, A, 2, B, 2
     $, SDIM,
     $                ALPHA, BETA, Q, 2, U, 2, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL AB_AB_CGGESX( 'V', 'V', 'S', AB_CLCTSX, 'V', 1, A, 1, B, 1
     $, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 32, RW, IW,
     $                0, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGESX', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_CGGEV
*
         SRNAMT = 'AB_CGGEV '
         INFOT = 1
         CALL AB_CGGEV( '/', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGGEV( 'N', '/', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGGEV( 'V', 'V', -1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 
     $1,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CGGEV( 'V', 'V', 1, A, 0, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CGGEV( 'V', 'V', 1, A, 1, B, 0, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CGGEV( 'N', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 0, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CGGEV( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 1, U, 2
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CGGEV( 'V', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, U, 0
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CGGEV( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_CGGEV( 'V', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1
     $,
     $               W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CGGEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_CGGEV3
*
         SRNAMT = 'AB_AB_CGGEV3'
         INFOT = 1
         CALL AB_AB_CGGEV3( '/', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGGEV3( 'N', '/', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGGEV3( 'V', 'V', -1, A, 1, B, 1, ALPHA, BETA, Q, 1,
     $ U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGGEV3( 'V', 'V', 1, A, 0, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CGGEV3( 'V', 'V', 1, A, 1, B, 0, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CGGEV3( 'N', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 0, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CGGEV3( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 1, 
     $U, 2,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CGGEV3( 'V', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, 
     $U, 0,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CGGEV3( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CGGEV3( 'V', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, 
     $U, 1,
     $                W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEV3', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_CGGEVX
*
         SRNAMT = 'AB_AB_CGGEVX'
         INFOT = 1
         CALL AB_AB_CGGEVX( '/', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGGEVX( 'N', '/', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGGEVX( 'N', 'N', '/', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CGGEVX( 'N', 'N', 'N', '/', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGGEVX( 'N', 'N', 'N', 'N', -1, A, 1, B, 1, ALPHA, B
     $ETA,
     $                Q, 1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE,
     $                RCV, W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CGGEVX( 'N', 'N', 'N', 'N', 1, A, 0, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 0, ALPHA, BE
     $TA, Q,
     $                1, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                0, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CGGEVX( 'N', 'V', 'N', 'N', 2, A, 2, B, 2, ALPHA, BE
     $TA, Q,
     $                1, U, 2, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BE
     $TA, Q,
     $                1, U, 0, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CGGEVX( 'N', 'N', 'V', 'N', 2, A, 2, B, 2, ALPHA, BE
     $TA, Q,
     $                2, U, 1, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 1, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 25
         CALL AB_AB_CGGEVX( 'N', 'N', 'V', 'N', 2, A, 2, B, 2, ALPHA, BE
     $TA, Q,
     $                2, U, 2, ILO, IHI, LS, RS, ANRM, BNRM, RCE, RCV,
     $                W, 0, RW, IW, BW, INFO )
         CALL AB_CHKXER( 'AB_AB_CGGEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_CTGEXC
*
         SRNAMT = 'AB_CTGEXC'
         INFOT = 3
         CALL AB_CTGEXC( .TRUE., .TRUE., -1, A, 1, B, 1, Q, 1, Z, 1, IFS
     $T,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTGEXC( .TRUE., .TRUE., 1, A, 0, B, 1, Q, 1, Z, 1, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CTGEXC( .TRUE., .TRUE., 1, A, 1, B, 0, Q, 1, Z, 1, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CTGEXC( .FALSE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, 
     $IFST,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CTGEXC( .TRUE., .FALSE., 1, A, 1, B, 1, Q, 1, Z, 0, 
     $IFST,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST
     $,
     $                ILST, INFO )
         CALL AB_CHKXER( 'AB_CTGEXC', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_CTGSEN
*
         SRNAMT = 'AB_CTGSEN'
         INFOT = 1
         CALL AB_CTGSEN( -1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CTGSEN( 1, .TRUE., .TRUE., SEL, -1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 0, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 0, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 0, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 0, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL AB_CTGSEN( 3, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, -5, IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL AB_CTGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL AB_CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL AB_CTGSEN( 5, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_CTGSEN', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_CTGSNA
*
         SRNAMT = 'AB_CTGSNA'
         INFOT = 1
         CALL AB_CTGSNA( '/', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTGSNA( 'B', '/', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTGSNA( 'B', 'A', SEL, -1, A, 1, B, 1, Q, 1, U, 1, R1, 
     $R2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CTGSNA( 'B', 'A', SEL, 1, A, 0, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CTGSNA( 'B', 'A', SEL, 1, A, 1, B, 0, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 0, U, 1, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 0, R1, R
     $2,
     $                1, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                0, M, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R
     $2,
     $                1, M, W, 0, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSNA', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_CTGSYL
*
         SRNAMT = 'AB_CTGSYL'
         INFOT = 1
         CALL AB_CTGSYL( '/', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CTGSYL( 'N', -1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z,
     $ 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CTGSYL( 'N', 0, 0, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CTGSYL( 'N', 0, 1, 0, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CTGSYL( 'N', 0, 1, 1, A, 0, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CTGSYL( 'N', 0, 1, 1, A, 1, B, 0, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 0, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 0, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 0, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $0,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_CTGSYL( 'N', 1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_CTGSYL( 'N', 2, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 
     $1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_CTGSYL', INFOT, NOUT, LERR, OK )
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
*     End of AB_CERRGG
*
      END
