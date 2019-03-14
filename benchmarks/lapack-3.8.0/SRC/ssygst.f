*> \brief \b AB_SSYGST
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_SSYGST + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_SSYGST.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_SSYGST.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_SSYGST.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, ITYPE, LDA, LDB, N
*       ..
*       .. Array Arguments ..
*       REAL               A( LDA, * ), B( LDB, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SSYGST reduces a real symmetric-definite generalized eigenproblem
*> to standard form.
*>
*> If ITYPE = 1, the problem is A*x = lambda*B*x,
*> and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
*>
*> If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
*> B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T*A*L.
*>
*> B must have been previously factorized as U**T*U or L*L**T by AB_SPOTRF.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] ITYPE
*> \verbatim
*>          ITYPE is INTEGER
*>          = 1: compute inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T);
*>          = 2 or 3: compute U*A*U**T or L**T*A*L.
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangle of A is stored and B is factored as
*>                  U**T*U;
*>          = 'L':  Lower triangle of A is stored and B is factored as
*>                  L*L**T.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrices A and B.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is REAL array, dimension (LDA,N)
*>          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*>          N-by-N upper triangular part of A contains the upper
*>          triangular part of the matrix A, and the strictly lower
*>          triangular part of A is not referenced.  If UPLO = 'L', the
*>          leading N-by-N lower triangular part of A contains the lower
*>          triangular part of the matrix A, and the strictly upper
*>          triangular part of A is not referenced.
*>
*>          On exit, if INFO = 0, the transformed matrix, stored in the
*>          same format as A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in] B
*> \verbatim
*>          B is REAL array, dimension (LDB,N)
*>          The triangular factor from the Cholesky factorization of B,
*>          as returned by AB_SPOTRF.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
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
*> \date December 2016
*
*> \ingroup realSYcomputational
*
*  =====================================================================
      SUBROUTINE AB_SSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, ITYPE, LDA, LDB, N
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, HALF
      PARAMETER          ( ONE = 1.0, HALF = 0.5 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            K, KB, NB
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_SSYGS2, AB_SSYMM, AB_AB_AB_SSYR2K, AB_STRMM,
     $ AB_STRSM, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ILAENV
      EXTERNAL           AB_LSAME, AB_ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = AB_LSAME( UPLO, 'U' )
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.UPPER .AND. .NOT.AB_LSAME( UPLO, 'L' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_SSYGST', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Determine the block size for this environment.
*
      NB = AB_ILAENV( 1, 'AB_SSYGST', UPLO, N, -1, -1, -1 )
*
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
*
*        Use unblocked code
*
         CALL AB_SSYGS2( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
      ELSE
*
*        Use blocked code
*
         IF( ITYPE.EQ.1 ) THEN
            IF( UPPER ) THEN
*
*              Compute inv(U**T)*A*inv(U)
*
               DO 10 K = 1, N, NB
                  KB = MIN( N-K+1, NB )
*
*                 Update the upper triangle of A(k:n,k:n)
*
                  CALL AB_SSYGS2( ITYPE, UPLO, KB, A( K, K ), LDA,
     $                         B( K, K ), LDB, INFO )
                  IF( K+KB.LE.N ) THEN
                     CALL AB_STRSM( 'Left', UPLO, 'Transpose', 'Non-unit
     $',
     $                           KB, N-K-KB+1, ONE, B( K, K ), LDB,
     $                           A( K, K+KB ), LDA )
                     CALL AB_SSYMM( 'Left', UPLO, KB, N-K-KB+1, -HALF,
     $                           A( K, K ), LDA, B( K, K+KB ), LDB, ONE,
     $                           A( K, K+KB ), LDA )
                     CALL AB_AB_AB_SSYR2K( UPLO, 'Transpose', N-K-KB+1, 
     $KB, -ONE,
     $                            A( K, K+KB ), LDA, B( K, K+KB ), LDB,
     $                            ONE, A( K+KB, K+KB ), LDA )
                     CALL AB_SSYMM( 'Left', UPLO, KB, N-K-KB+1, -HALF,
     $                           A( K, K ), LDA, B( K, K+KB ), LDB, ONE,
     $                           A( K, K+KB ), LDA )
                     CALL AB_STRSM( 'Right', UPLO, 'No transpose',
     $                           'Non-unit', KB, N-K-KB+1, ONE,
     $                           B( K+KB, K+KB ), LDB, A( K, K+KB ),
     $                           LDA )
                  END IF
   10          CONTINUE
            ELSE
*
*              Compute inv(L)*A*inv(L**T)
*
               DO 20 K = 1, N, NB
                  KB = MIN( N-K+1, NB )
*
*                 Update the lower triangle of A(k:n,k:n)
*
                  CALL AB_SSYGS2( ITYPE, UPLO, KB, A( K, K ), LDA,
     $                         B( K, K ), LDB, INFO )
                  IF( K+KB.LE.N ) THEN
                     CALL AB_STRSM( 'Right', UPLO, 'Transpose', 'Non-uni
     $t',
     $                           N-K-KB+1, KB, ONE, B( K, K ), LDB,
     $                           A( K+KB, K ), LDA )
                     CALL AB_SSYMM( 'Right', UPLO, N-K-KB+1, KB, -HALF,
     $                           A( K, K ), LDA, B( K+KB, K ), LDB, ONE,
     $                           A( K+KB, K ), LDA )
                     CALL AB_AB_AB_SSYR2K( UPLO, 'No transpose', N-K-KB+
     $1, KB,
     $                            -ONE, A( K+KB, K ), LDA, B( K+KB, K ),
     $                            LDB, ONE, A( K+KB, K+KB ), LDA )
                     CALL AB_SSYMM( 'Right', UPLO, N-K-KB+1, KB, -HALF,
     $                           A( K, K ), LDA, B( K+KB, K ), LDB, ONE,
     $                           A( K+KB, K ), LDA )
                     CALL AB_STRSM( 'Left', UPLO, 'No transpose',
     $                           'Non-unit', N-K-KB+1, KB, ONE,
     $                           B( K+KB, K+KB ), LDB, A( K+KB, K ),
     $                           LDA )
                  END IF
   20          CONTINUE
            END IF
         ELSE
            IF( UPPER ) THEN
*
*              Compute U*A*U**T
*
               DO 30 K = 1, N, NB
                  KB = MIN( N-K+1, NB )
*
*                 Update the upper triangle of A(1:k+kb-1,1:k+kb-1)
*
                  CALL AB_STRMM( 'Left', UPLO, 'No transpose', 'Non-unit
     $',
     $                        K-1, KB, ONE, B, LDB, A( 1, K ), LDA )
                  CALL AB_SSYMM( 'Right', UPLO, K-1, KB, HALF, A( K, K )
     $,
     $                        LDA, B( 1, K ), LDB, ONE, A( 1, K ), LDA )
                  CALL AB_AB_AB_SSYR2K( UPLO, 'No transpose', K-1, KB, O
     $NE,
     $                         A( 1, K ), LDA, B( 1, K ), LDB, ONE, A,
     $                         LDA )
                  CALL AB_SSYMM( 'Right', UPLO, K-1, KB, HALF, A( K, K )
     $,
     $                        LDA, B( 1, K ), LDB, ONE, A( 1, K ), LDA )
                  CALL AB_STRMM( 'Right', UPLO, 'Transpose', 'Non-unit',
     $                        K-1, KB, ONE, B( K, K ), LDB, A( 1, K ),
     $                        LDA )
                  CALL AB_SSYGS2( ITYPE, UPLO, KB, A( K, K ), LDA,
     $                         B( K, K ), LDB, INFO )
   30          CONTINUE
            ELSE
*
*              Compute L**T*A*L
*
               DO 40 K = 1, N, NB
                  KB = MIN( N-K+1, NB )
*
*                 Update the lower triangle of A(1:k+kb-1,1:k+kb-1)
*
                  CALL AB_STRMM( 'Right', UPLO, 'No transpose', 'Non-uni
     $t',
     $                        KB, K-1, ONE, B, LDB, A( K, 1 ), LDA )
                  CALL AB_SSYMM( 'Left', UPLO, KB, K-1, HALF, A( K, K ),
     $                        LDA, B( K, 1 ), LDB, ONE, A( K, 1 ), LDA )
                  CALL AB_AB_AB_SSYR2K( UPLO, 'Transpose', K-1, KB, ONE,
     $                         A( K, 1 ), LDA, B( K, 1 ), LDB, ONE, A,
     $                         LDA )
                  CALL AB_SSYMM( 'Left', UPLO, KB, K-1, HALF, A( K, K ),
     $                        LDA, B( K, 1 ), LDB, ONE, A( K, 1 ), LDA )
                  CALL AB_STRMM( 'Left', UPLO, 'Transpose', 'Non-unit', 
     $KB,
     $                        K-1, ONE, B( K, K ), LDB, A( K, 1 ), LDA )
                  CALL AB_SSYGS2( ITYPE, UPLO, KB, A( K, K ), LDA,
     $                         B( K, K ), LDB, INFO )
   40          CONTINUE
            END IF
         END IF
      END IF
      RETURN
*
*     End of AB_SSYGST
*
      END
