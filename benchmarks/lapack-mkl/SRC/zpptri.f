*> \brief \b AB_ZPPTRI
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZPPTRI + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZPPTRI.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZPPTRI.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZPPTRI.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZPPTRI( UPLO, N, AP, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, N
*       ..
*       .. Array Arguments ..
*       COMPLEX*16         AP( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZPPTRI computes the inverse of a complex Hermitian positive definite
*> matrix A using the Cholesky factorization A = U**H*U or A = L*L**H
*> computed by AB_ZPPTRF.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangular factor is stored in AP;
*>          = 'L':  Lower triangular factor is stored in AP.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] AP
*> \verbatim
*>          AP is COMPLEX*16 array, dimension (N*(N+1)/2)
*>          On entry, the triangular factor U or L from the Cholesky
*>          factorization A = U**H*U or A = L*L**H, packed columnwise as
*>          a linear array.  The j-th column of U or L is stored in the
*>          array AP as follows:
*>          if UPLO = 'U', AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;
*>          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.
*>
*>          On exit, the upper or lower triangle of the (Hermitian)
*>          inverse of A, overwriting the input factor U or L.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*>          > 0:  if INFO = i, the (i,i) element of the factor U or L is
*>                zero, and the inverse could not be computed.
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
*  =====================================================================
      SUBROUTINE AB_ZPPTRI( UPLO, N, AP, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         AP( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J, JC, JJ, JJN
      DOUBLE PRECISION   AJJ
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      COMPLEX*16         AB_ZDOTC
      EXTERNAL           AB_LSAME, AB_ZDOTC
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_XERBLA, AB_ZDSCAL, AB_ZHPR, AB_ZTPMV, AB_ZTP
     $TRI
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = AB_LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.AB_LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_ZPPTRI', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Invert the triangular Cholesky factor U or L.
*
      CALL AB_ZTPTRI( UPLO, 'Non-unit', N, AP, INFO )
      IF( INFO.GT.0 )
     $   RETURN
      IF( UPPER ) THEN
*
*        Compute the product inv(U) * inv(U)**H.
*
         JJ = 0
         DO 10 J = 1, N
            JC = JJ + 1
            JJ = JJ + J
            IF( J.GT.1 )
     $         CALL AB_ZHPR( 'Upper', J-1, ONE, AP( JC ), 1, AP )
            AJJ = AP( JJ )
            CALL AB_ZDSCAL( J, AJJ, AP( JC ), 1 )
   10    CONTINUE
*
      ELSE
*
*        Compute the product inv(L)**H * inv(L).
*
         JJ = 1
         DO 20 J = 1, N
            JJN = JJ + N - J + 1
            AP( JJ ) = DBLE( AB_ZDOTC( N-J+1, AP( JJ ), 1, AP( JJ ), 1 )
     $ )
            IF( J.LT.N )
     $         CALL AB_ZTPMV( 'Lower', 'Conjugate transpose', 'Non-unit'
     $,
     $                     N-J, AP( JJN ), AP( JJ+1 ), 1 )
            JJ = JJN
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of AB_ZPPTRI
*
      END
