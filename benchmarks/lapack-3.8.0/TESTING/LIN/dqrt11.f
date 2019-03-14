*> \brief \b AB_DQRT11
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       DOUBLE PRECISION FUNCTION AB_DQRT11( M, K, A, LDA, TAU, WORK, LWORK )
*
*       .. Scalar Arguments ..
*       INTEGER            K, LDA, LWORK, M
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DQRT11 computes the test ratio
*>
*>       || Q'*Q - I || / (eps * m)
*>
*> where the orthogonal matrix Q is represented as a product of
*> elementary transformations.  Each transformation has the form
*>
*>    H(k) = I - tau(k) v(k) v(k)'
*>
*> where tau(k) is stored in TAU(k) and v(k) is an m-vector of the form
*> [ 0 ... 0 1 x(k) ]', where x(k) is a vector of length m-k stored
*> in A(k+1:m,k).
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of columns of A whose subdiagonal entries
*>          contain information about orthogonal transformations.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,K)
*>          The (possibly partial) output of a QR reduction routine.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (K)
*>          The scaling factors tau for the elementary transformations as
*>          computed by the QR factorization routine.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (LWORK)
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The length of the array WORK.  LWORK >= M*M + M.
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
*> \ingroup double_lin
*
*  =====================================================================
      DOUBLE PRECISION FUNCTION AB_DQRT11( M, K, A, LDA, TAU, WORK, LWOR
     $K )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            K, LDA, LWORK, M
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, J
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANGE
      EXTERNAL           AB_DLAMCH, AB_DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DLASET, AB_DORM2R, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   RDUMMY( 1 )
*     ..
*     .. Executable Statements ..
*
      AB_DQRT11 = ZERO
*
*     Test for sufficient workspace
*
      IF( LWORK.LT.M*M+M ) THEN
         CALL AB_XERBLA( 'AB_DQRT11', 7 )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 )
     $   RETURN
*
      CALL AB_DLASET( 'Full', M, M, ZERO, ONE, WORK, M )
*
*     Form Q
*
      CALL AB_DORM2R( 'Left', 'No transpose', M, M, K, A, LDA, TAU, WORK
     $,
     $             M, WORK( M*M+1 ), INFO )
*
*     Form Q'*Q
*
      CALL AB_DORM2R( 'Left', 'Transpose', M, M, K, A, LDA, TAU, WORK, M
     $,
     $             WORK( M*M+1 ), INFO )
*
      DO 10 J = 1, M
         WORK( ( J-1 )*M+J ) = WORK( ( J-1 )*M+J ) - ONE
   10 CONTINUE
*
      AB_DQRT11 = AB_DLANGE( 'One-norm', M, M, WORK, M, RDUMMY ) /
     $         ( DBLE( M )*AB_DLAMCH( 'Epsilon' ) )
*
      RETURN
*
*     End of AB_DQRT11
*
      END
