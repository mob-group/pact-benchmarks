*> \brief \b AB_CQPT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       REAL             FUNCTION AB_CQPT01( M, N, K, A, AF, LDA, TAU, JPVT,
*                        WORK, LWORK )
*
*       .. Scalar Arguments ..
*       INTEGER            K, LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       INTEGER            JPVT( * )
*       COMPLEX            A( LDA, * ), AF( LDA, * ), TAU( * ),
*      $                   WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CQPT01 tests the QR-factorization with pivoting of a matrix A.  The
*> array AF contains the (possibly partial) QR-factorization of A, where
*> the upper triangle of AF(1:k,1:k) is a partial triangular factor,
*> the entries below the diagonal in the first k columns are the
*> HousehoAB_LDEr vectors, and the rest of AF contains a partially updated
*> matrix.
*>
*> This function returns ||A*P - Q*R||/(||norm(A)||*eps*M)
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrices A and AF.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrices A and AF.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of columns of AF that have been reduced
*>          to upper triangular form.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is COMPLEX array, dimension (LDA, N)
*>          The original matrix A.
*> \endverbatim
*>
*> \param[in] AF
*> \verbatim
*>          AF is COMPLEX array, dimension (LDA,N)
*>          The (possibly partial) output of AB_CGEQPF.  The upper triangle
*>          of AF(1:k,1:k) is a partial triangular factor, the entries
*>          below the diagonal in the first k columns are the HousehoAB_LDEr
*>          vectors, and the rest of AF contains a partially updated
*>          matrix.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the arrays A and AF.
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is COMPLEX array, dimension (K)
*>          Details of the HousehoAB_LDEr transformations as returned by
*>          AB_CGEQPF.
*> \endverbatim
*>
*> \param[in] JPVT
*> \verbatim
*>          JPVT is INTEGER array, dimension (N)
*>          Pivot information as returned by AB_CGEQPF.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX array, dimension (LWORK)
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The length of the array WORK.  LWORK >= M*N+N.
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
*> \ingroup complex_lin
*
*  =====================================================================
      REAL             FUNCTION AB_CQPT01( M, N, K, A, AF, LDA, TAU, JPV
     $T,
     $                 WORK, LWORK )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      COMPLEX            A( LDA, * ), AF( LDA, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
      REAL               NORMA
*     ..
*     .. Local Arrays ..
      REAL               RWORK( 1 )
*     ..
*     .. External Functions ..
      REAL               AB_CLANGE, AB_SLAMCH
      EXTERNAL           AB_CLANGE, AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CAXPY, AB_CCOPY, AB_CUNMQR, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MAX, MIN, REAL
*     ..
*     .. Executable Statements ..
*
      AB_CQPT01 = ZERO
*
*     Test if there is enough workspace
*
      IF( LWORK.LT.M*N+N ) THEN
         CALL AB_XERBLA( 'AB_CQPT01', 10 )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      NORMA = AB_CLANGE( 'One-norm', M, N, A, LDA, RWORK )
*
      DO 30 J = 1, K
         DO 10 I = 1, MIN( J, M )
            WORK( ( J-1 )*M+I ) = AF( I, J )
   10    CONTINUE
         DO 20 I = J + 1, M
            WORK( ( J-1 )*M+I ) = ZERO
   20    CONTINUE
   30 CONTINUE
      DO 40 J = K + 1, N
         CALL AB_CCOPY( M, AF( 1, J ), 1, WORK( ( J-1 )*M+1 ), 1 )
   40 CONTINUE
*
      CALL AB_CUNMQR( 'Left', 'No transpose', M, N, K, AF, LDA, TAU, WOR
     $K,
     $             M, WORK( M*N+1 ), LWORK-M*N, INFO )
*
      DO 50 J = 1, N
*
*        Compare i-th column of QR and jpvt(i)-th column of A
*
         CALL AB_CAXPY( M, CMPLX( -ONE ), A( 1, JPVT( J ) ), 1,
     $               WORK( ( J-1 )*M+1 ), 1 )
   50 CONTINUE
*
      AB_CQPT01 = AB_CLANGE( 'One-norm', M, N, WORK, M, RWORK ) /
     $         ( REAL( MAX( M, N ) )*AB_SLAMCH( 'Epsilon' ) )
      IF( NORMA.NE.ZERO )
     $   AB_CQPT01 = AB_CQPT01 / NORMA
*
      RETURN
*
*     End of AB_CQPT01
*
      END
