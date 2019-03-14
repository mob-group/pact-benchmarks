*> \brief \b AB_ZLATDF uses the LU factorization of the n-by-n matrix computed by AB_SGETC2 and computes a contribution to the reciprocal Dif-estimate.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZLATDF + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZLATDF.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZLATDF.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZLATDF.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZLATDF( IJOB, N, Z, LDZ, RHS, RDSUM, RAB_DSCAL, IPIV,
*                          JPIV )
*
*       .. Scalar Arguments ..
*       INTEGER            IJOB, LDZ, N
*       DOUBLE PRECISION   RAB_DSCAL, RDSUM
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * ), JPIV( * )
*       COMPLEX*16         RHS( * ), Z( LDZ, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZLATDF computes the contribution to the reciprocal Dif-estimate
*> by solving for x in Z * x = b, where b is chosen such that the norm
*> of x is as large as possible. It is assumed that LU decomposition
*> of Z has been computed by AB_ZGETC2. On entry RHS = f holds the
*> contribution from earlier solved sub-systems, and on return RHS = x.
*>
*> The factorization of Z returned by AB_ZGETC2 has the form
*> Z = P * L * U * Q, where P and Q are permutation matrices. L is lower
*> triangular with unit diagonal elements and U is upper triangular.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] IJOB
*> \verbatim
*>          IJOB is INTEGER
*>          IJOB = 2: First compute an approximative null-vector e
*>              of Z using AB_ZGECON, e is normalized and solve for
*>              Zx = +-e - f with the sign giving the greater value of
*>              2-norm(x).  About 5 times as expensive as Default.
*>          IJOB .ne. 2: Local look ahead strategy where
*>              all entries of the r.h.s. b is chosen as either +1 or
*>              -1.  Default.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix Z.
*> \endverbatim
*>
*> \param[in] Z
*> \verbatim
*>          Z is COMPLEX*16 array, dimension (LDZ, N)
*>          On entry, the LU part of the factorization of the n-by-n
*>          matrix Z computed by AB_ZGETC2:  Z = P * L * U * Q
*> \endverbatim
*>
*> \param[in] LDZ
*> \verbatim
*>          LDZ is INTEGER
*>          The leading dimension of the array Z.  LDA >= max(1, N).
*> \endverbatim
*>
*> \param[in,out] RHS
*> \verbatim
*>          RHS is COMPLEX*16 array, dimension (N).
*>          On entry, RHS contains contributions from other subsystems.
*>          On exit, RHS contains the solution of the subsystem with
*>          entries according to the value of IJOB (see above).
*> \endverbatim
*>
*> \param[in,out] RDSUM
*> \verbatim
*>          RDSUM is DOUBLE PRECISION
*>          On entry, the sum of squares of computed contributions to
*>          the Dif-estimate under computation by AB_ZTGSYL, where the
*>          scaling factor RAB_DSCAL (see below) has been factored out.
*>          On exit, the corresponding sum of squares updated with the
*>          contributions from the current sub-system.
*>          If TRANS = 'T' RDSUM is not touched.
*>          NOTE: RDSUM only makes sense when AB_ZTGSY2 is called by AB_CTGSYL.
*> \endverbatim
*>
*> \param[in,out] RAB_DSCAL
*> \verbatim
*>          RAB_DSCAL is DOUBLE PRECISION
*>          On entry, scaling factor used to prevent overflow in RDSUM.
*>          On exit, RAB_DSCAL is updated w.r.t. the current contributions
*>          in RDSUM.
*>          If TRANS = 'T', RAB_DSCAL is not touched.
*>          NOTE: RAB_DSCAL only makes sense when AB_ZTGSY2 is called by
*>          AB_ZTGSYL.
*> \endverbatim
*>
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N).
*>          The pivot indices; for 1 <= i <= N, row i of the
*>          matrix has been interchanged with row IPIV(i).
*> \endverbatim
*>
*> \param[in] JPIV
*> \verbatim
*>          JPIV is INTEGER array, dimension (N).
*>          The pivot indices; for 1 <= j <= N, column j of the
*>          matrix has been interchanged with column JPIV(j).
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
*> \ingroup complex16OTHERauxiliary
*
*> \par Further Details:
*  =====================
*>
*>  This routine is a further developed implementation of algorithm
*>  BSOLVE in [1] using complete pivoting in the LU factorization.
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
*>   [1]   Bo Kagstrom and Lars Westin,
*>         Generalized Schur Methods with Condition Estimators for
*>         Solving the Generalized Sylvester Equation, IEEE Transactions
*>         on Automatic Control, Vol. 34, No. 7, July 1989, pp 745-751.
*>\n
*>   [2]   Peter Poromaa,
*>         On Efficient and Robust Estimators for the Separation
*>         between two Regular Matrix Pairs with Applications in
*>         Condition Estimation. Report UMINF-95.05, Department of
*>         Computing Science, Umea University, S-901 87 Umea, Sweden,
*>         1995.
*
*  =====================================================================
      SUBROUTINE AB_ZLATDF( IJOB, N, Z, LDZ, RHS, RDSUM, RAB_DSCAL, IPIV
     $,
     $                   JPIV )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*     .. Scalar Arguments ..
      INTEGER            IJOB, LDZ, N
      DOUBLE PRECISION   RAB_DSCAL, RDSUM
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), JPIV( * )
      COMPLEX*16         RHS( * ), Z( LDZ, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXDIM
      PARAMETER          ( MAXDIM = 2 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CONE
      PARAMETER          ( CONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J, K
      DOUBLE PRECISION   RTEMP, SCALE, SMINU, SPLUS
      COMPLEX*16         BM, BP, PMONE, TEMP
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   RWORK( MAXDIM )
      COMPLEX*16         WORK( 4*MAXDIM ), XM( MAXDIM ), XP( MAXDIM )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ZAXPY, AB_ZCOPY, AB_ZGECON, AB_ZGESC2, AB_ZL
     $ASSQ, AB_ZLASWP,
     $                   AB_ZSCAL
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DZASUM
      COMPLEX*16         AB_ZDOTC
      EXTERNAL           AB_DZASUM, AB_ZDOTC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( IJOB.NE.2 ) THEN
*
*        Apply permutations IPIV to RHS
*
         CALL AB_ZLASWP( 1, RHS, LDZ, 1, N-1, IPIV, 1 )
*
*        Solve for L-part choosing RHS either to +1 or -1.
*
         PMONE = -CONE
         DO 10 J = 1, N - 1
            BP = RHS( J ) + CONE
            BM = RHS( J ) - CONE
            SPLUS = ONE
*
*           Lockahead for L- part RHS(1:N-1) = +-1
*           SPLUS and SMIN computed more efficiently than in BSOLVE[1].
*
            SPLUS = SPLUS + DBLE( AB_ZDOTC( N-J, Z( J+1, J ), 1, Z( J+1,
     $              J ), 1 ) )
            SMINU = DBLE( AB_ZDOTC( N-J, Z( J+1, J ), 1, RHS( J+1 ), 1 )
     $ )
            SPLUS = SPLUS*DBLE( RHS( J ) )
            IF( SPLUS.GT.SMINU ) THEN
               RHS( J ) = BP
            ELSE IF( SMINU.GT.SPLUS ) THEN
               RHS( J ) = BM
            ELSE
*
*              In this case the updating sums are equal and we can
*              choose RHS(J) +1 or -1. The first time this happens we
*              choose -1, thereafter +1. This is a simple way to get
*              good estimates of matrices like Byers well-known example
*              (see [1]). (Not done in BSOLVE.)
*
               RHS( J ) = RHS( J ) + PMONE
               PMONE = CONE
            END IF
*
*           Compute the remaining r.h.s.
*
            TEMP = -RHS( J )
            CALL AB_ZAXPY( N-J, TEMP, Z( J+1, J ), 1, RHS( J+1 ), 1 )
   10    CONTINUE
*
*        Solve for U- part, lockahead for RHS(N) = +-1. This is not done
*        In BSOLVE and will hopefully give us a better estimate because
*        any ill-conditioning of the original matrix is transfered to U
*        and not to L. U(N, N) is an approximation to sigma_min(LU).
*
         CALL AB_ZCOPY( N-1, RHS, 1, WORK, 1 )
         WORK( N ) = RHS( N ) + CONE
         RHS( N ) = RHS( N ) - CONE
         SPLUS = ZERO
         SMINU = ZERO
         DO 30 I = N, 1, -1
            TEMP = CONE / Z( I, I )
            WORK( I ) = WORK( I )*TEMP
            RHS( I ) = RHS( I )*TEMP
            DO 20 K = I + 1, N
               WORK( I ) = WORK( I ) - WORK( K )*( Z( I, K )*TEMP )
               RHS( I ) = RHS( I ) - RHS( K )*( Z( I, K )*TEMP )
   20       CONTINUE
            SPLUS = SPLUS + ABS( WORK( I ) )
            SMINU = SMINU + ABS( RHS( I ) )
   30    CONTINUE
         IF( SPLUS.GT.SMINU )
     $      CALL AB_ZCOPY( N, WORK, 1, RHS, 1 )
*
*        Apply the permutations JPIV to the computed solution (RHS)
*
         CALL AB_ZLASWP( 1, RHS, LDZ, 1, N-1, JPIV, -1 )
*
*        Compute the sum of squares
*
         CALL AB_ZLASSQ( N, RHS, 1, RAB_DSCAL, RDSUM )
         RETURN
      END IF
*
*     ENTRY IJOB = 2
*
*     Compute approximate nullvector XM of Z
*
      CALL AB_ZGECON( 'I', N, Z, LDZ, ONE, RTEMP, WORK, RWORK, INFO )
      CALL AB_ZCOPY( N, WORK( N+1 ), 1, XM, 1 )
*
*     Compute RHS
*
      CALL AB_ZLASWP( 1, XM, LDZ, 1, N-1, IPIV, -1 )
      TEMP = CONE / SQRT( AB_ZDOTC( N, XM, 1, XM, 1 ) )
      CALL AB_ZSCAL( N, TEMP, XM, 1 )
      CALL AB_ZCOPY( N, XM, 1, XP, 1 )
      CALL AB_ZAXPY( N, CONE, RHS, 1, XP, 1 )
      CALL AB_ZAXPY( N, -CONE, XM, 1, RHS, 1 )
      CALL AB_ZGESC2( N, Z, LDZ, RHS, IPIV, JPIV, SCALE )
      CALL AB_ZGESC2( N, Z, LDZ, XP, IPIV, JPIV, SCALE )
      IF( AB_DZASUM( N, XP, 1 ).GT.AB_DZASUM( N, RHS, 1 ) )
     $   CALL AB_ZCOPY( N, XP, 1, RHS, 1 )
*
*     Compute the sum of squares
*
      CALL AB_ZLASSQ( N, RHS, 1, RAB_DSCAL, RDSUM )
      RETURN
*
*     End of AB_ZLATDF
*
      END
