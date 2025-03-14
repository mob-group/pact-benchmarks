*
*  Definition:
*  ===========
*
*      SUBROUTINE AB_ZLAMSWLQ( SIDE, TRANS, M, N, K, MB, NB, A, LDA, T,
*     $                LDT, C, LDC, WORK, LWORK, INFO )
*
*
*     .. Scalar Arguments ..
*      CHARACTER         SIDE, TRANS
*      INTEGER           INFO, LDA, M, N, K, MB, NB, LDT, LWORK, LDC
*     ..
*     .. Array Arguments ..
*      COMPLEX*16        A( LDA, * ), WORK( * ), C(LDC, * ),
*     $                  T( LDT, * )
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    ZLAMQRTS overwrites the general real M-by-N matrix C with
*>
*>
*>                    SIDE = 'L'     SIDE = 'R'
*>    TRANS = 'N':      Q * C          C * Q
*>    TRANS = 'C':      Q**H * C       C * Q**H
*>    where Q is a real orthogonal matrix defined as the product of blocked
*>    elementary reflectors computed by short wide LQ
*>    factorization (AB_ZLASWLQ)
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>          = 'L': apply Q or Q**H from the Left;
*>          = 'R': apply Q or Q**H from the Right.
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>          = 'N':  No transpose, apply Q;
*>          = 'C':  Conjugate Transpose, apply Q**H.
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix C.  M >=0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix C. N >= M.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of elementary reflectors whose product defines
*>          the matrix Q.
*>          M >= K >= 0;
*>
*> \endverbatim
*> \param[in] MB
*> \verbatim
*>          MB is INTEGER
*>          The row block size to be used in the blocked QR.
*>          M >= MB >= 1
*> \endverbatim
*>
*> \param[in] NB
*> \verbatim
*>          NB is INTEGER
*>          The column block size to be used in the blocked QR.
*>          NB > M.
*> \endverbatim
*>
*> \param[in] NB
*> \verbatim
*>          NB is INTEGER
*>          The block size to be used in the blocked QR.
*>                MB > M.
*>
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension
*>                               (LDA,M) if SIDE = 'L',
*>                               (LDA,N) if SIDE = 'R'
*>          The i-th row must contain the vector which defines the blocked
*>          elementary reflector H(i), for i = 1,2,...,k, as returned by
*>          AB_ZLASWLQ in the first k rows of its array argument A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.
*>          If SIDE = 'L', LDA >= max(1,M);
*>          if SIDE = 'R', LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in] T
*> \verbatim
*>          T is COMPLEX*16 array, dimension
*>          ( M * Number of blocks(CEIL(N-K/NB-K)),
*>          The blocked upper triangular block reflectors stored in compact form
*>          as a sequence of upper triangular blocks.  See below
*>          for further details.
*> \endverbatim
*>
*> \param[in] LDT
*> \verbatim
*>          LDT is INTEGER
*>          The leading dimension of the array T.  LDT >= MB.
*> \endverbatim
*>
*> \param[in,out] C
*> \verbatim
*>          C is COMPLEX*16 array, dimension (LDC,N)
*>          On entry, the M-by-N matrix C.
*>          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the array C. LDC >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>         (workspace) COMPLEX*16 array, dimension (MAX(1,LWORK))
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          If SIDE = 'L', LWORK >= max(1,NB) * MB;
*>          if SIDE = 'R', LWORK >= max(1,M) * MB.
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by AB_XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
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
*> \par Further Details:
*  =====================
*>
*> \verbatim
*> Short-Wide LQ (SWLQ) performs LQ by a sequence of orthogonal transformations,
*> representing Q as a product of other orthogonal matrices
*>   Q = Q(1) * Q(2) * . . . * Q(k)
*> where each Q(i) zeros out upper diagonal entries of a block of NB rows of A:
*>   Q(1) zeros out the upper diagonal entries of rows 1:NB of A
*>   Q(2) zeros out the bottom MB-N rows of rows [1:M,NB+1:2*NB-M] of A
*>   Q(3) zeros out the bottom MB-N rows of rows [1:M,2*NB-M+1:3*NB-2*M] of A
*>   . . .
*>
*> Q(1) is computed by GELQT, which represents Q(1) by Householder vectors
*> stored under the diagonal of rows 1:MB of A, and by upper triangular
*> block reflectors, stored in array T(1:LDT,1:N).
*> For more information see Further Details in GELQT.
*>
*> Q(i) for i>1 is computed by TPLQT, which represents Q(i) by Householder vectors
*> stored in columns [(i-1)*(NB-M)+M+1:i*(NB-M)+M] of A, and by upper triangular
*> block reflectors, stored in array T(1:LDT,(i-1)*M+1:i*M).
*> The last Q(k) may use fewer rows.
*> For more information see Further Details in TPQRT.
*>
*> For more details of the overall algorithm, see the description of
*> Sequential TSQR in Section 2.2 of [1].
*>
*> [1] “Communication-Optimal Parallel and Sequential QR and LU Factorizations,”
*>     J. Demmel, L. Grigori, M. Hoemmen, J. Langou,
*>     SIAM J. Sci. Comput, vol. 34, no. 1, 2012
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE AB_ZLAMSWLQ( SIDE, TRANS, M, N, K, MB, NB, A, LDA, T,
     $    LDT, C, LDC, WORK, LWORK, INFO )
*
*  -- LAPACK computational routine (version 3.7.1) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2017
*
*     .. Scalar Arguments ..
      CHARACTER         SIDE, TRANS
      INTEGER           INFO, LDA, M, N, K, MB, NB, LDT, LWORK, LDC
*     ..
*     .. Array Arguments ..
      COMPLEX*16        A( LDA, * ), WORK( * ), C(LDC, * ),
     $      T( LDT, * )
*     ..
*
* =====================================================================
*
*     ..
*     .. Local Scalars ..
      LOGICAL    LEFT, RIGHT, TRAN, NOTRAN, LQUERY
      INTEGER    I, II, KK, LW, CTR
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      EXTERNAL           AB_LSAME
*     .. External Subroutines ..
      EXTERNAL    AB_ZTPMLQT, AB_ZGEMLQT, AB_XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      LQUERY  = LWORK.LT.0
      NOTRAN  = AB_LSAME( TRANS, 'N' )
      TRAN    = AB_LSAME( TRANS, 'C' )
      LEFT    = AB_LSAME( SIDE, 'L' )
      RIGHT   = AB_LSAME( SIDE, 'R' )
      IF (LEFT) THEN
        LW = N * MB
      ELSE
        LW = M * MB
      END IF
*
      INFO = 0
      IF( .NOT.LEFT .AND. .NOT.RIGHT ) THEN
         INFO = -1
      ELSE IF( .NOT.TRAN .AND. .NOT.NOTRAN ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
        INFO = -3
      ELSE IF( N.LT.0 ) THEN
        INFO = -4
      ELSE IF( K.LT.0 ) THEN
        INFO = -5
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
        INFO = -9
      ELSE IF( LDT.LT.MAX( 1, MB) ) THEN
        INFO = -11
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -13
      ELSE IF(( LWORK.LT.MAX(1,LW)).AND.(.NOT.LQUERY)) THEN
        INFO = -15
      END IF
*
      IF( INFO.NE.0 ) THEN
        CALL AB_XERBLA( 'AB_ZLAMSWLQ', -INFO )
        WORK(1) = LW
        RETURN
      ELSE IF (LQUERY) THEN
        WORK(1) = LW
        RETURN
      END IF
*
*     Quick return if possible
*
      IF( MIN(M,N,K).EQ.0 ) THEN
        RETURN
      END IF
*
      IF((NB.LE.K).OR.(NB.GE.MAX(M,N,K))) THEN
        CALL AB_ZGEMLQT( SIDE, TRANS, M, N, K, MB, A, LDA,
     $        T, LDT, C, LDC, WORK, INFO)
        RETURN
      END IF
*
      IF(LEFT.AND.TRAN) THEN
*
*         Multiply Q to the last block of C
*
          KK = MOD((M-K),(NB-K))
          CTR = (M-K)/(NB-K)
*
          IF (KK.GT.0) THEN
            II=M-KK+1
            CALL AB_ZTPMLQT('L','C',KK , N, K, 0, MB, A(1,II), LDA,
     $        T(1,CTR*K+1), LDT, C(1,1), LDC,
     $        C(II,1), LDC, WORK, INFO )
          ELSE
            II=M+1
          END IF
*
          DO I=II-(NB-K),NB+1,-(NB-K)
*
*         Multiply Q to the current block of C (1:M,I:I+NB)
*
            CTR = CTR - 1
            CALL AB_ZTPMLQT('L','C',NB-K , N, K, 0,MB, A(1,I), LDA,
     $          T(1,CTR*K+1),LDT, C(1,1), LDC,
     $          C(I,1), LDC, WORK, INFO )

          END DO
*
*         Multiply Q to the first block of C (1:M,1:NB)
*
          CALL AB_ZGEMLQT('L','C',NB , N, K, MB, A(1,1), LDA, T
     $              ,LDT ,C(1,1), LDC, WORK, INFO )
*
      ELSE IF (LEFT.AND.NOTRAN) THEN
*
*         Multiply Q to the first block of C
*
         KK = MOD((M-K),(NB-K))
         II=M-KK+1
         CTR = 1
         CALL AB_ZGEMLQT('L','N',NB , N, K, MB, A(1,1), LDA, T
     $              ,LDT ,C(1,1), LDC, WORK, INFO )
*
         DO I=NB+1,II-NB+K,(NB-K)
*
*         Multiply Q to the current block of C (I:I+NB,1:N)
*
          CALL AB_ZTPMLQT('L','N',NB-K , N, K, 0,MB, A(1,I), LDA,
     $         T(1, CTR * K + 1), LDT, C(1,1), LDC,
     $         C(I,1), LDC, WORK, INFO )
          CTR = CTR + 1
*
         END DO
         IF(II.LE.M) THEN
*
*         Multiply Q to the last block of C
*
          CALL AB_ZTPMLQT('L','N',KK , N, K, 0, MB, A(1,II), LDA,
     $        T(1, CTR * K + 1), LDT, C(1,1), LDC,
     $        C(II,1), LDC, WORK, INFO )
*
         END IF
*
      ELSE IF(RIGHT.AND.NOTRAN) THEN
*
*         Multiply Q to the last block of C
*
          KK = MOD((N-K),(NB-K))
          CTR = (N-K)/(NB-K)
          IF (KK.GT.0) THEN
            II=N-KK+1
            CALL AB_ZTPMLQT('R','N',M , KK, K, 0, MB, A(1, II), LDA,
     $        T(1, CTR * K + 1), LDT, C(1,1), LDC,
     $        C(1,II), LDC, WORK, INFO )
          ELSE
            II=N+1
          END IF
*
          DO I=II-(NB-K),NB+1,-(NB-K)
*
*         Multiply Q to the current block of C (1:M,I:I+MB)
*
          CTR = CTR - 1
          CALL AB_ZTPMLQT('R','N', M, NB-K, K, 0, MB, A(1, I), LDA,
     $        T(1, CTR * K + 1), LDT, C(1,1), LDC,
     $        C(1,I), LDC, WORK, INFO )

          END DO
*
*         Multiply Q to the first block of C (1:M,1:MB)
*
          CALL AB_ZGEMLQT('R','N',M , NB, K, MB, A(1,1), LDA, T
     $            ,LDT ,C(1,1), LDC, WORK, INFO )
*
      ELSE IF (RIGHT.AND.TRAN) THEN
*
*       Multiply Q to the first block of C
*
         KK = MOD((N-K),(NB-K))
         II=N-KK+1
         CALL AB_ZGEMLQT('R','C',M , NB, K, MB, A(1,1), LDA, T
     $            ,LDT ,C(1,1), LDC, WORK, INFO )
         CTR = 1
*
         DO I=NB+1,II-NB+K,(NB-K)
*
*         Multiply Q to the current block of C (1:M,I:I+MB)
*
          CALL AB_ZTPMLQT('R','C',M , NB-K, K, 0,MB, A(1,I), LDA,
     $       T(1,CTR *K+1), LDT, C(1,1), LDC,
     $       C(1,I), LDC, WORK, INFO )
          CTR = CTR + 1
*
         END DO
         IF(II.LE.N) THEN
*
*       Multiply Q to the last block of C
*
          CALL AB_ZTPMLQT('R','C',M , KK, K, 0,MB, A(1,II), LDA,
     $      T(1, CTR * K + 1),LDT, C(1,1), LDC,
     $      C(1,II), LDC, WORK, INFO )
*
         END IF
*
      END IF
*
      WORK(1) = LW
      RETURN
*
*     End of AB_ZLAMSWLQ
*
      END
