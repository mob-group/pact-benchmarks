*> \brief \b AB_ZHETRS_AA
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZHETRS_AA + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZHETRS_aa.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZHETRS_aa.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZHETRS_aa.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZHETRS_AA( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,
*                             WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            N, NRHS, LDA, LDB, LWORK, INFO
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       COMPLEX*16         A( LDA, * ), B( LDB, * ), WORK( * )
*       ..
*
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZHETRS_AA solves a system of linear equations A*X = B with a complex
*> hermitian matrix A using the factorization A = U*T*U**H or
*> A = L*T*L**T computed by AB_ZHETRF_AA.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the details of the factorization are stored
*>          as an upper or lower triangular matrix.
*>          = 'U':  Upper triangular, form is A = U*T*U**H;
*>          = 'L':  Lower triangular, form is A = L*T*L**H.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrix B.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          Details of factors computed by AB_ZHETRF_AA.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          Details of the interchanges as computed by AB_ZHETRF_AA.
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is COMPLEX*16 array, dimension (LDB,NRHS)
*>          On entry, the right hand side matrix B.
*>          On exit, the solution matrix X.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[in] WORK
*> \verbatim
*>          WORK is DOUBLE array, dimension (MAX(1,LWORK))
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER, LWORK >= MAX(1,3*N-2).
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
*> \date November 2017
*
*> \ingroup complex16HEcomputational
*
*  =====================================================================
      SUBROUTINE AB_ZHETRS_AA( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,
     $                      WORK, LWORK, INFO )
*
*  -- LAPACK computational routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
      IMPLICIT NONE
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            N, NRHS, LDA, LDB, LWORK, INFO
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  =====================================================================
*
      COMPLEX*16         ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER
      INTEGER            K, KP, LWKOPT
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      EXTERNAL           AB_LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ZGTSV, AB_ZSWAP, AB_ZTRSM, AB_ZLACGV, AB_ZLA
     $CPY, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      UPPER = AB_LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.UPPER .AND. .NOT.AB_LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.MAX( 1, 3*N-2 ) .AND. .NOT.LQUERY ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_ZHETRS_AA', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         LWKOPT = (3*N-2)
         WORK( 1 ) = LWKOPT
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Solve A*X = B, where A = U*T*U**T.
*
*        Pivot, P**T * B
*
         DO K = 1, N
            KP = IPIV( K )
            IF( KP.NE.K )
     $          CALL AB_ZSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
         END DO
*
*        Compute (U \P**T * B) -> B    [ (U \P**T * B) ]
*
         CALL AB_ZTRSM('L', 'U', 'C', 'U', N-1, NRHS, ONE, A( 1, 2 ), LD
     $A,
     $               B( 2, 1 ), LDB)
*
*        Compute T \ B -> B   [ T \ (U \P**T * B) ]
*
         CALL AB_ZLACPY( 'F', 1, N, A(1, 1), LDA+1, WORK(N), 1)
         IF( N.GT.1 ) THEN
             CALL AB_ZLACPY( 'F', 1, N-1, A( 1, 2 ), LDA+1, WORK( 2*N ),
     $ 1)
             CALL AB_ZLACPY( 'F', 1, N-1, A( 1, 2 ), LDA+1, WORK( 1 ), 1
     $)
             CALL AB_ZLACGV( N-1, WORK( 1 ), 1 )
         END IF
         CALL AB_ZGTSV(N, NRHS, WORK(1), WORK(N), WORK(2*N), B, LDB,
     $              INFO)
*
*        Compute (U**T \ B) -> B   [ U**T \ (T \ (U \P**T * B) ) ]
*
         CALL AB_ZTRSM( 'L', 'U', 'N', 'U', N-1, NRHS, ONE, A( 1, 2 ), L
     $DA,
     $               B(2, 1), LDB)
*
*        Pivot, P * B  [ P * (U**T \ (T \ (U \P**T * B) )) ]
*
         DO K = N, 1, -1
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL AB_ZSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
         END DO
*
      ELSE
*
*        Solve A*X = B, where A = L*T*L**T.
*
*        Pivot, P**T * B
*
         DO K = 1, N
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL AB_ZSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
         END DO
*
*        Compute (L \P**T * B) -> B    [ (L \P**T * B) ]
*
         CALL AB_ZTRSM( 'L', 'L', 'N', 'U', N-1, NRHS, ONE, A( 2, 1 ), L
     $DA,
     $               B(2, 1), LDB)
*
*        Compute T \ B -> B   [ T \ (L \P**T * B) ]
*
         CALL AB_ZLACPY( 'F', 1, N, A(1, 1), LDA+1, WORK(N), 1)
         IF( N.GT.1 ) THEN
             CALL AB_ZLACPY( 'F', 1, N-1, A( 2, 1 ), LDA+1, WORK( 1 ), 1
     $)
             CALL AB_ZLACPY( 'F', 1, N-1, A( 2, 1 ), LDA+1, WORK( 2*N ),
     $ 1)
             CALL AB_ZLACGV( N-1, WORK( 2*N ), 1 )
         END IF
         CALL AB_ZGTSV(N, NRHS, WORK(1), WORK(N), WORK(2*N), B, LDB,
     $              INFO)
*
*        Compute (L**T \ B) -> B   [ L**T \ (T \ (L \P**T * B) ) ]
*
         CALL AB_ZTRSM( 'L', 'L', 'C', 'U', N-1, NRHS, ONE, A( 2, 1 ), L
     $DA,
     $              B( 2, 1 ), LDB)
*
*        Pivot, P * B  [ P * (L**T \ (T \ (L \P**T * B) )) ]
*
         DO K = N, 1, -1
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL AB_ZSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
         END DO
*
      END IF
*
      RETURN
*
*     End of AB_ZHETRS_AA
*
      END
