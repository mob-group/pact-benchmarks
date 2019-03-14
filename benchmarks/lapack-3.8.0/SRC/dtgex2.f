*> \brief \b AB_DTGEX2 swaps adjacent diagonal blocks in an upper (quasi) triangular matrix pair by an orthogonal equivalence transformation.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_DTGEX2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_DTGEX2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_DTGEX2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_DTGEX2.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DTGEX2( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
*                          LDZ, J1, N1, N2, WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       LOGICAL            WANTQ, WANTZ
*       INTEGER            INFO, J1, LDA, LDB, LDQ, LDZ, LWORK, N, N1, N2
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
*      $                   WORK( * ), Z( LDZ, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DTGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)
*> of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair
*> (A, B) by an orthogonal equivalence transformation.
*>
*> (A, B) must be in generalized real Schur canonical form (as returned
*> by AB_DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
*> diagonal blocks. B is upper triangular.
*>
*> Optionally, the matrices Q and Z of generalized Schur vectors are
*> updated.
*>
*>        Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
*>        Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
*>
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] WANTQ
*> \verbatim
*>          WANTQ is LOGICAL
*>          .TRUE. : update the left transformation matrix Q;
*>          .FALSE.: do not update Q.
*> \endverbatim
*>
*> \param[in] WANTZ
*> \verbatim
*>          WANTZ is LOGICAL
*>          .TRUE. : update the right transformation matrix Z;
*>          .FALSE.: do not update Z.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrices A and B. N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimensions (LDA,N)
*>          On entry, the matrix A in the pair (A, B).
*>          On exit, the updated matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A. LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimensions (LDB,N)
*>          On entry, the matrix B in the pair (A, B).
*>          On exit, the updated matrix B.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B. LDB >= max(1,N).
*> \endverbatim
*>
*> \param[in,out] Q
*> \verbatim
*>          Q is DOUBLE PRECISION array, dimension (LDQ,N)
*>          On entry, if WANTQ = .TRUE., the orthogonal matrix Q.
*>          On exit, the updated matrix Q.
*>          Not referenced if WANTQ = .FALSE..
*> \endverbatim
*>
*> \param[in] LDQ
*> \verbatim
*>          LDQ is INTEGER
*>          The leading dimension of the array Q. LDQ >= 1.
*>          If WANTQ = .TRUE., LDQ >= N.
*> \endverbatim
*>
*> \param[in,out] Z
*> \verbatim
*>          Z is DOUBLE PRECISION array, dimension (LDZ,N)
*>          On entry, if WANTZ =.TRUE., the orthogonal matrix Z.
*>          On exit, the updated matrix Z.
*>          Not referenced if WANTZ = .FALSE..
*> \endverbatim
*>
*> \param[in] LDZ
*> \verbatim
*>          LDZ is INTEGER
*>          The leading dimension of the array Z. LDZ >= 1.
*>          If WANTZ = .TRUE., LDZ >= N.
*> \endverbatim
*>
*> \param[in] J1
*> \verbatim
*>          J1 is INTEGER
*>          The index to the first block (A11, B11). 1 <= J1 <= N.
*> \endverbatim
*>
*> \param[in] N1
*> \verbatim
*>          N1 is INTEGER
*>          The order of the first block (A11, B11). N1 = 0, 1 or 2.
*> \endverbatim
*>
*> \param[in] N2
*> \verbatim
*>          N2 is INTEGER
*>          The order of the second block (A22, B22). N2 = 0, 1 or 2.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK)).
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          LWORK >=  MAX( 1, N*(N2+N1), (N2+N1)*(N2+N1)*2 )
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>            =0: Successful exit
*>            >0: If INFO = 1, the transformed matrix (A, B) would be
*>                too far from generalized Schur form; the blocks are
*>                not swapped and (A, B) and (Q, Z) are unchanged.
*>                The problem of swapping is too ill-conditioned.
*>            <0: If INFO = -16: LWORK is too small. Appropriate value
*>                for LWORK is returned in WORK(1).
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
*> \ingroup doubleGEauxiliary
*
*> \par Further Details:
*  =====================
*>
*>  In the current code both weak and strong stability tests are
*>  performed. The user can omit the strong stability test by changing
*>  the internal logical parameter WANDS to .FALSE.. See ref. [2] for
*>  details.
*
*> \par Contributors:
*  ==================
*>
*>     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
*>     Umea University, S-901 87 Umea, Sweden.
*
*> \par References:
*  ================
*>
*> \verbatim
*>
*>  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
*>      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
*>      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
*>      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
*>
*>  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
*>      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
*>      Estimation: Theory, Algorithms and Software,
*>      Report UMINF - 94.04, Department of Computing Science, Umea
*>      University, S-901 87 Umea, Sweden, 1994. Also as LAPACK Working
*>      Note 87. To appear in Numerical Algorithms, 1996.
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE AB_DTGEX2( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
     $                   LDZ, J1, N1, N2, WORK, LWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      LOGICAL            WANTQ, WANTZ
      INTEGER            INFO, J1, LDA, LDB, LDQ, LDZ, LWORK, N, N1, N2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     $                   WORK( * ), Z( LDZ, * )
*     ..
*
*  =====================================================================
*  Replaced various illegal calls to AB_DCOPY by calls to AB_DLASET, or by DO
*  loops. Sven Hammarling, 1/5/02.
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   TWENTY
      PARAMETER          ( TWENTY = 2.0D+01 )
      INTEGER            LDST
      PARAMETER          ( LDST = 4 )
      LOGICAL            WANDS
      PARAMETER          ( WANDS = .TRUE. )
*     ..
*     .. Local Scalars ..
      LOGICAL            DTRONG, WEAK
      INTEGER            I, IDUM, LINFO, M
      DOUBLE PRECISION   BQRA21, BRQA21, DDUM, DNORM, AB_DSCALE, DSUM, E
     $PS,
     $                   F, G, SA, SB, SCALE, SMLNUM, SS, THRESH, WS
*     ..
*     .. Local Arrays ..
      INTEGER            IWORK( LDST )
      DOUBLE PRECISION   AI( 2 ), AR( 2 ), BE( 2 ), IR( LDST, LDST ),
     $                   IRCOP( LDST, LDST ), LI( LDST, LDST ),
     $                   LICOP( LDST, LDST ), S( LDST, LDST ),
     $                   SCPY( LDST, LDST ), T( LDST, LDST ),
     $                   TAUL( LDST ), TAUR( LDST ), TCPY( LDST, LDST )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DGEMM, AB_DGEQR2, AB_DGERQ2, AB_DLACPY, AB_D
     $LAGV2, AB_DLARTG,
     $                   AB_DLASET, AB_DLASSQ, AB_DORG2R, AB_DORGR2, AB_
     $DORM2R, AB_DORMR2,
     $                   AB_DROT, AB_DSCAL, AB_DTGSY2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     Quick return if possible
*
      IF( N.LE.1 .OR. N1.LE.0 .OR. N2.LE.0 )
     $   RETURN
      IF( N1.GT.N .OR. ( J1+N1 ).GT.N )
     $   RETURN
      M = N1 + N2
      IF( LWORK.LT.MAX( 1, N*M, M*M*2 ) ) THEN
         INFO = -16
         WORK( 1 ) = MAX( 1, N*M, M*M*2 )
         RETURN
      END IF
*
      WEAK = .FALSE.
      DTRONG = .FALSE.
*
*     Make a local copy of selected block
*
      CALL AB_DLASET( 'Full', LDST, LDST, ZERO, ZERO, LI, LDST )
      CALL AB_DLASET( 'Full', LDST, LDST, ZERO, ZERO, IR, LDST )
      CALL AB_DLACPY( 'Full', M, M, A( J1, J1 ), LDA, S, LDST )
      CALL AB_DLACPY( 'Full', M, M, B( J1, J1 ), LDB, T, LDST )
*
*     Compute threshold for testing acceptance of swapping.
*
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      AB_DSCALE = ZERO
      DSUM = ONE
      CALL AB_DLACPY( 'Full', M, M, S, LDST, WORK, M )
      CALL AB_DLASSQ( M*M, WORK, 1, AB_DSCALE, DSUM )
      CALL AB_DLACPY( 'Full', M, M, T, LDST, WORK, M )
      CALL AB_DLASSQ( M*M, WORK, 1, AB_DSCALE, DSUM )
      DNORM = AB_DSCALE*SQRT( DSUM )
*
*     THRES has been changed from
*        THRESH = MAX( TEN*EPS*SA, SMLNUM )
*     to
*        THRESH = MAX( TWENTY*EPS*SA, SMLNUM )
*     on 04/01/10.
*     "Bug" reported by Ondra Kamenik, confirmed by Julie Langou, fixed by
*     Jim Demmel and Guillaume Revy. See forum post 1783.
*
      THRESH = MAX( TWENTY*EPS*DNORM, SMLNUM )
*
      IF( M.EQ.2 ) THEN
*
*        CASE 1: Swap 1-by-1 and 1-by-1 blocks.
*
*        Compute orthogonal QL and RQ that swap 1-by-1 and 1-by-1 blocks
*        using Givens rotations and perform the swap tentatively.
*
         F = S( 2, 2 )*T( 1, 1 ) - T( 2, 2 )*S( 1, 1 )
         G = S( 2, 2 )*T( 1, 2 ) - T( 2, 2 )*S( 1, 2 )
         SB = ABS( T( 2, 2 ) )
         SA = ABS( S( 2, 2 ) )
         CALL AB_DLARTG( F, G, IR( 1, 2 ), IR( 1, 1 ), DDUM )
         IR( 2, 1 ) = -IR( 1, 2 )
         IR( 2, 2 ) = IR( 1, 1 )
         CALL AB_DROT( 2, S( 1, 1 ), 1, S( 1, 2 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         CALL AB_DROT( 2, T( 1, 1 ), 1, T( 1, 2 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         IF( SA.GE.SB ) THEN
            CALL AB_DLARTG( S( 1, 1 ), S( 2, 1 ), LI( 1, 1 ), LI( 2, 1 )
     $,
     $                   DDUM )
         ELSE
            CALL AB_DLARTG( T( 1, 1 ), T( 2, 1 ), LI( 1, 1 ), LI( 2, 1 )
     $,
     $                   DDUM )
         END IF
         CALL AB_DROT( 2, S( 1, 1 ), LDST, S( 2, 1 ), LDST, LI( 1, 1 ),
     $              LI( 2, 1 ) )
         CALL AB_DROT( 2, T( 1, 1 ), LDST, T( 2, 1 ), LDST, LI( 1, 1 ),
     $              LI( 2, 1 ) )
         LI( 2, 2 ) = LI( 1, 1 )
         LI( 1, 2 ) = -LI( 2, 1 )
*
*        Weak stability test:
*           |S21| + |T21| <= O(EPS * F-norm((S, T)))
*
         WS = ABS( S( 2, 1 ) ) + ABS( T( 2, 1 ) )
         WEAK = WS.LE.THRESH
         IF( .NOT.WEAK )
     $      GO TO 70
*
         IF( WANDS ) THEN
*
*           Strong stability test:
*             F-norm((A-QL**T*S*QR, B-QL**T*T*QR)) <= O(EPS*F-norm((A,B)))
*
            CALL AB_DLACPY( 'Full', M, M, A( J1, J1 ), LDA, WORK( M*M+1 
     $),
     $                   M )
            CALL AB_DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, S, LDST, ZE
     $RO,
     $                  WORK, M )
            CALL AB_DGEMM( 'N', 'T', M, M, M, -ONE, WORK, M, IR, LDST, O
     $NE,
     $                  WORK( M*M+1 ), M )
            AB_DSCALE = ZERO
            DSUM = ONE
            CALL AB_DLASSQ( M*M, WORK( M*M+1 ), 1, AB_DSCALE, DSUM )
*
            CALL AB_DLACPY( 'Full', M, M, B( J1, J1 ), LDB, WORK( M*M+1 
     $),
     $                   M )
            CALL AB_DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, T, LDST, ZE
     $RO,
     $                  WORK, M )
            CALL AB_DGEMM( 'N', 'T', M, M, M, -ONE, WORK, M, IR, LDST, O
     $NE,
     $                  WORK( M*M+1 ), M )
            CALL AB_DLASSQ( M*M, WORK( M*M+1 ), 1, AB_DSCALE, DSUM )
            SS = AB_DSCALE*SQRT( DSUM )
            DTRONG = SS.LE.THRESH
            IF( .NOT.DTRONG )
     $         GO TO 70
         END IF
*
*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and
*               (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)).
*
         CALL AB_DROT( J1+1, A( 1, J1 ), 1, A( 1, J1+1 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         CALL AB_DROT( J1+1, B( 1, J1 ), 1, B( 1, J1+1 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         CALL AB_DROT( N-J1+1, A( J1, J1 ), LDA, A( J1+1, J1 ), LDA,
     $              LI( 1, 1 ), LI( 2, 1 ) )
         CALL AB_DROT( N-J1+1, B( J1, J1 ), LDB, B( J1+1, J1 ), LDB,
     $              LI( 1, 1 ), LI( 2, 1 ) )
*
*        Set  N1-by-N2 (2,1) - blocks to ZERO.
*
         A( J1+1, J1 ) = ZERO
         B( J1+1, J1 ) = ZERO
*
*        Accumulate transformations into Q and Z if requested.
*
         IF( WANTZ )
     $      CALL AB_DROT( N, Z( 1, J1 ), 1, Z( 1, J1+1 ), 1, IR( 1, 1 ),
     $                 IR( 2, 1 ) )
         IF( WANTQ )
     $      CALL AB_DROT( N, Q( 1, J1 ), 1, Q( 1, J1+1 ), 1, LI( 1, 1 ),
     $                 LI( 2, 1 ) )
*
*        Exit with INFO = 0 if swap was successfully performed.
*
         RETURN
*
      ELSE
*
*        CASE 2: Swap 1-by-1 and 2-by-2 blocks, or 2-by-2
*                and 2-by-2 blocks.
*
*        Solve the generalized Sylvester equation
*                 S11 * R - L * S22 = SCALE * S12
*                 T11 * R - L * T22 = SCALE * T12
*        for R and L. Solutions in LI and IR.
*
         CALL AB_DLACPY( 'Full', N1, N2, T( 1, N1+1 ), LDST, LI, LDST )
         CALL AB_DLACPY( 'Full', N1, N2, S( 1, N1+1 ), LDST,
     $                IR( N2+1, N1+1 ), LDST )
         CALL AB_DTGSY2( 'N', 0, N1, N2, S, LDST, S( N1+1, N1+1 ), LDST,
     $                IR( N2+1, N1+1 ), LDST, T, LDST, T( N1+1, N1+1 ),
     $                LDST, LI, LDST, SCALE, DSUM, AB_DSCALE, IWORK, IDU
     $M,
     $                LINFO )
*
*        Compute orthogonal matrix QL:
*
*                    QL**T * LI = [ TL ]
*                                 [ 0  ]
*        where
*                    LI =  [      -L              ]
*                          [ SCALE * identity(N2) ]
*
         DO 10 I = 1, N2
            CALL AB_DSCAL( N1, -ONE, LI( 1, I ), 1 )
            LI( N1+I, I ) = SCALE
   10    CONTINUE
         CALL AB_DGEQR2( M, N2, LI, LDST, TAUL, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL AB_DORG2R( M, M, N2, LI, LDST, TAUL, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Compute orthogonal matrix RQ:
*
*                    IR * RQ**T =   [ 0  TR],
*
*         where IR = [ SCALE * identity(N1), R ]
*
         DO 20 I = 1, N1
            IR( N2+I, I ) = SCALE
   20    CONTINUE
         CALL AB_DGERQ2( N1, M, IR( N2+1, 1 ), LDST, TAUR, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL AB_DORGR2( M, M, N1, IR, LDST, TAUR, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Perform the swapping tentatively:
*
         CALL AB_DGEMM( 'T', 'N', M, M, M, ONE, LI, LDST, S, LDST, ZERO,
     $               WORK, M )
         CALL AB_DGEMM( 'N', 'T', M, M, M, ONE, WORK, M, IR, LDST, ZERO,
     $ S,
     $               LDST )
         CALL AB_DGEMM( 'T', 'N', M, M, M, ONE, LI, LDST, T, LDST, ZERO,
     $               WORK, M )
         CALL AB_DGEMM( 'N', 'T', M, M, M, ONE, WORK, M, IR, LDST, ZERO,
     $ T,
     $               LDST )
         CALL AB_DLACPY( 'F', M, M, S, LDST, SCPY, LDST )
         CALL AB_DLACPY( 'F', M, M, T, LDST, TCPY, LDST )
         CALL AB_DLACPY( 'F', M, M, IR, LDST, IRCOP, LDST )
         CALL AB_DLACPY( 'F', M, M, LI, LDST, LICOP, LDST )
*
*        Triangularize the B-part by an RQ factorization.
*        Apply transformation (from left) to A-part, giving S.
*
         CALL AB_DGERQ2( M, M, T, LDST, TAUR, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL AB_DORMR2( 'R', 'T', M, M, M, T, LDST, TAUR, S, LDST, WORK
     $,
     $                LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL AB_DORMR2( 'L', 'N', M, M, M, T, LDST, TAUR, IR, LDST, WOR
     $K,
     $                LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Compute F-norm(S21) in BRQA21. (T21 is 0.)
*
         AB_DSCALE = ZERO
         DSUM = ONE
         DO 30 I = 1, N2
            CALL AB_DLASSQ( N1, S( N2+1, I ), 1, AB_DSCALE, DSUM )
   30    CONTINUE
         BRQA21 = AB_DSCALE*SQRT( DSUM )
*
*        Triangularize the B-part by a QR factorization.
*        Apply transformation (from right) to A-part, giving S.
*
         CALL AB_DGEQR2( M, M, TCPY, LDST, TAUL, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL AB_DORM2R( 'L', 'T', M, M, M, TCPY, LDST, TAUL, SCPY, LDST
     $,
     $                WORK, INFO )
         CALL AB_DORM2R( 'R', 'N', M, M, M, TCPY, LDST, TAUL, LICOP, LDS
     $T,
     $                WORK, INFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Compute F-norm(S21) in BQRA21. (T21 is 0.)
*
         AB_DSCALE = ZERO
         DSUM = ONE
         DO 40 I = 1, N2
            CALL AB_DLASSQ( N1, SCPY( N2+1, I ), 1, AB_DSCALE, DSUM )
   40    CONTINUE
         BQRA21 = AB_DSCALE*SQRT( DSUM )
*
*        Decide which method to use.
*          Weak stability test:
*             F-norm(S21) <= O(EPS * F-norm((S, T)))
*
         IF( BQRA21.LE.BRQA21 .AND. BQRA21.LE.THRESH ) THEN
            CALL AB_DLACPY( 'F', M, M, SCPY, LDST, S, LDST )
            CALL AB_DLACPY( 'F', M, M, TCPY, LDST, T, LDST )
            CALL AB_DLACPY( 'F', M, M, IRCOP, LDST, IR, LDST )
            CALL AB_DLACPY( 'F', M, M, LICOP, LDST, LI, LDST )
         ELSE IF( BRQA21.GE.THRESH ) THEN
            GO TO 70
         END IF
*
*        Set lower triangle of B-part to zero
*
         CALL AB_DLASET( 'Lower', M-1, M-1, ZERO, ZERO, T(2,1), LDST )
*
         IF( WANDS ) THEN
*
*           Strong stability test:
*              F-norm((A-QL*S*QR**T, B-QL*T*QR**T)) <= O(EPS*F-norm((A,B)))
*
            CALL AB_DLACPY( 'Full', M, M, A( J1, J1 ), LDA, WORK( M*M+1 
     $),
     $                   M )
            CALL AB_DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, S, LDST, ZE
     $RO,
     $                  WORK, M )
            CALL AB_DGEMM( 'N', 'N', M, M, M, -ONE, WORK, M, IR, LDST, O
     $NE,
     $                  WORK( M*M+1 ), M )
            AB_DSCALE = ZERO
            DSUM = ONE
            CALL AB_DLASSQ( M*M, WORK( M*M+1 ), 1, AB_DSCALE, DSUM )
*
            CALL AB_DLACPY( 'Full', M, M, B( J1, J1 ), LDB, WORK( M*M+1 
     $),
     $                   M )
            CALL AB_DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, T, LDST, ZE
     $RO,
     $                  WORK, M )
            CALL AB_DGEMM( 'N', 'N', M, M, M, -ONE, WORK, M, IR, LDST, O
     $NE,
     $                  WORK( M*M+1 ), M )
            CALL AB_DLASSQ( M*M, WORK( M*M+1 ), 1, AB_DSCALE, DSUM )
            SS = AB_DSCALE*SQRT( DSUM )
            DTRONG = ( SS.LE.THRESH )
            IF( .NOT.DTRONG )
     $         GO TO 70
*
         END IF
*
*        If the swap is accepted ("weakly" and "strongly"), apply the
*        transformations and set N1-by-N2 (2,1)-block to zero.
*
         CALL AB_DLASET( 'Full', N1, N2, ZERO, ZERO, S(N2+1,1), LDST )
*
*        copy back M-by-M diagonal block starting at index J1 of (A, B)
*
         CALL AB_DLACPY( 'F', M, M, S, LDST, A( J1, J1 ), LDA )
         CALL AB_DLACPY( 'F', M, M, T, LDST, B( J1, J1 ), LDB )
         CALL AB_DLASET( 'Full', LDST, LDST, ZERO, ZERO, T, LDST )
*
*        Standardize existing 2-by-2 blocks.
*
         CALL AB_DLASET( 'Full', M, M, ZERO, ZERO, WORK, M )
         WORK( 1 ) = ONE
         T( 1, 1 ) = ONE
         IDUM = LWORK - M*M - 2
         IF( N2.GT.1 ) THEN
            CALL AB_DLAGV2( A( J1, J1 ), LDA, B( J1, J1 ), LDB, AR, AI, 
     $BE,
     $                   WORK( 1 ), WORK( 2 ), T( 1, 1 ), T( 2, 1 ) )
            WORK( M+1 ) = -WORK( 2 )
            WORK( M+2 ) = WORK( 1 )
            T( N2, N2 ) = T( 1, 1 )
            T( 1, 2 ) = -T( 2, 1 )
         END IF
         WORK( M*M ) = ONE
         T( M, M ) = ONE
*
         IF( N1.GT.1 ) THEN
            CALL AB_DLAGV2( A( J1+N2, J1+N2 ), LDA, B( J1+N2, J1+N2 ), L
     $DB,
     $                   TAUR, TAUL, WORK( M*M+1 ), WORK( N2*M+N2+1 ),
     $                   WORK( N2*M+N2+2 ), T( N2+1, N2+1 ),
     $                   T( M, M-1 ) )
            WORK( M*M ) = WORK( N2*M+N2+1 )
            WORK( M*M-1 ) = -WORK( N2*M+N2+2 )
            T( M, M ) = T( N2+1, N2+1 )
            T( M-1, M ) = -T( M, M-1 )
         END IF
         CALL AB_DGEMM( 'T', 'N', N2, N1, N2, ONE, WORK, M, A( J1, J1+N2
     $ ),
     $               LDA, ZERO, WORK( M*M+1 ), N2 )
         CALL AB_DLACPY( 'Full', N2, N1, WORK( M*M+1 ), N2, A( J1, J1+N2
     $ ),
     $                LDA )
         CALL AB_DGEMM( 'T', 'N', N2, N1, N2, ONE, WORK, M, B( J1, J1+N2
     $ ),
     $               LDB, ZERO, WORK( M*M+1 ), N2 )
         CALL AB_DLACPY( 'Full', N2, N1, WORK( M*M+1 ), N2, B( J1, J1+N2
     $ ),
     $                LDB )
         CALL AB_DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, WORK, M, ZERO,
     $               WORK( M*M+1 ), M )
         CALL AB_DLACPY( 'Full', M, M, WORK( M*M+1 ), M, LI, LDST )
         CALL AB_DGEMM( 'N', 'N', N2, N1, N1, ONE, A( J1, J1+N2 ), LDA,
     $               T( N2+1, N2+1 ), LDST, ZERO, WORK, N2 )
         CALL AB_DLACPY( 'Full', N2, N1, WORK, N2, A( J1, J1+N2 ), LDA )
         CALL AB_DGEMM( 'N', 'N', N2, N1, N1, ONE, B( J1, J1+N2 ), LDB,
     $               T( N2+1, N2+1 ), LDST, ZERO, WORK, N2 )
         CALL AB_DLACPY( 'Full', N2, N1, WORK, N2, B( J1, J1+N2 ), LDB )
         CALL AB_DGEMM( 'T', 'N', M, M, M, ONE, IR, LDST, T, LDST, ZERO,
     $               WORK, M )
         CALL AB_DLACPY( 'Full', M, M, WORK, M, IR, LDST )
*
*        Accumulate transformations into Q and Z if requested.
*
         IF( WANTQ ) THEN
            CALL AB_DGEMM( 'N', 'N', N, M, M, ONE, Q( 1, J1 ), LDQ, LI,
     $                  LDST, ZERO, WORK, N )
            CALL AB_DLACPY( 'Full', N, M, WORK, N, Q( 1, J1 ), LDQ )
*
         END IF
*
         IF( WANTZ ) THEN
            CALL AB_DGEMM( 'N', 'N', N, M, M, ONE, Z( 1, J1 ), LDZ, IR,
     $                  LDST, ZERO, WORK, N )
            CALL AB_DLACPY( 'Full', N, M, WORK, N, Z( 1, J1 ), LDZ )
*
         END IF
*
*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and
*                (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)).
*
         I = J1 + M
         IF( I.LE.N ) THEN
            CALL AB_DGEMM( 'T', 'N', M, N-I+1, M, ONE, LI, LDST,
     $                  A( J1, I ), LDA, ZERO, WORK, M )
            CALL AB_DLACPY( 'Full', M, N-I+1, WORK, M, A( J1, I ), LDA )
            CALL AB_DGEMM( 'T', 'N', M, N-I+1, M, ONE, LI, LDST,
     $                  B( J1, I ), LDB, ZERO, WORK, M )
            CALL AB_DLACPY( 'Full', M, N-I+1, WORK, M, B( J1, I ), LDB )
         END IF
         I = J1 - 1
         IF( I.GT.0 ) THEN
            CALL AB_DGEMM( 'N', 'N', I, M, M, ONE, A( 1, J1 ), LDA, IR,
     $                  LDST, ZERO, WORK, I )
            CALL AB_DLACPY( 'Full', I, M, WORK, I, A( 1, J1 ), LDA )
            CALL AB_DGEMM( 'N', 'N', I, M, M, ONE, B( 1, J1 ), LDB, IR,
     $                  LDST, ZERO, WORK, I )
            CALL AB_DLACPY( 'Full', I, M, WORK, I, B( 1, J1 ), LDB )
         END IF
*
*        Exit with INFO = 0 if swap was successfully performed.
*
         RETURN
*
      END IF
*
*     Exit with INFO = 1 if swap was rejected.
*
   70 CONTINUE
*
      INFO = 1
      RETURN
*
*     End of AB_DTGEX2
*
      END
