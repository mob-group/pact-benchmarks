*> \brief \b AB_ZLATRZ factors an upper trapezoidal matrix by means of unitary transformations.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZLATRZ + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZLATRZ.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZLATRZ.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZLATRZ.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZLATRZ( M, N, L, A, LDA, TAU, WORK )
*
*       .. Scalar Arguments ..
*       INTEGER            L, LDA, M, N
*       ..
*       .. Array Arguments ..
*       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZLATRZ factors the M-by-(M+L) complex upper trapezoidal matrix
*> [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z by means
*> of unitary transformations, where  Z is an (M+L)-by-(M+L) unitary
*> matrix and, R and A1 are M-by-M upper triangular matrices.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] L
*> \verbatim
*>          L is INTEGER
*>          The number of columns of the matrix A containing the
*>          meaningful part of the Householder vectors. N-M >= L >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          On entry, the leading M-by-N upper trapezoidal part of the
*>          array A must contain the matrix to be factorized.
*>          On exit, the leading M-by-M upper triangular part of A
*>          contains the upper triangular matrix R, and elements N-L+1 to
*>          N of the first M rows of A, with the array TAU, represent the
*>          unitary matrix Z as a product of M elementary reflectors.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is COMPLEX*16 array, dimension (M)
*>          The scalar factors of the elementary reflectors.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (M)
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
*> \ingroup complex16OTHERcomputational
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The factorization is obtained by Householder's method.  The kth
*>  transformation matrix, Z( k ), which is used to introduce zeros into
*>  the ( m - k + 1 )th row of A, is given in the form
*>
*>     Z( k ) = ( I     0   ),
*>              ( 0  T( k ) )
*>
*>  where
*>
*>     T( k ) = I - tau*u( k )*u( k )**H,   u( k ) = (   1    ),
*>                                                 (   0    )
*>                                                 ( z( k ) )
*>
*>  tau is a scalar and z( k ) is an l element vector. tau and z( k )
*>  are chosen to annihilate the elements of the kth row of A2.
*>
*>  The scalar tau is returned in the kth element of TAU and the vector
*>  u( k ) in the kth row of A2, such that the elements of z( k ) are
*>  in  a( k, l + 1 ), ..., a( k, n ). The elements of R are returned in
*>  the upper triangular part of A1.
*>
*>  Z is given by
*>
*>     Z =  Z( 1 ) * Z( 2 ) * ... * Z( m ).
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE AB_ZLATRZ( M, N, L, A, LDA, TAU, WORK )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            L, LDA, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ZLACGV, AB_ZLARFG, AB_ZLARZ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.EQ.0 ) THEN
         RETURN
      ELSE IF( M.EQ.N ) THEN
         DO 10 I = 1, N
            TAU( I ) = ZERO
   10    CONTINUE
         RETURN
      END IF
*
      DO 20 I = M, 1, -1
*
*        Generate elementary reflector H(i) to annihilate
*        [ A(i,i) A(i,n-l+1:n) ]
*
         CALL AB_ZLACGV( L, A( I, N-L+1 ), LDA )
         ALPHA = DCONJG( A( I, I ) )
         CALL AB_ZLARFG( L+1, ALPHA, A( I, N-L+1 ), LDA, TAU( I ) )
         TAU( I ) = DCONJG( TAU( I ) )
*
*        Apply H(i) to A(1:i-1,i:n) from the right
*
         CALL AB_ZLARZ( 'Right', I-1, N-I+1, L, A( I, N-L+1 ), LDA,
     $               DCONJG( TAU( I ) ), A( 1, I ), LDA, WORK )
         A( I, I ) = DCONJG( ALPHA )
*
   20 CONTINUE
*
      RETURN
*
*     End of AB_ZLATRZ
*
      END
