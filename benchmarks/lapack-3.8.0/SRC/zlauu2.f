*> \brief \b AB_ZLAUU2 computes the product UUH or LHL, where U and L are upper or lower triangular matrices (unblocked algorithm).
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZLAUU2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZLAUU2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZLAUU2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZLAUU2.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZLAUU2( UPLO, N, A, LDA, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, LDA, N
*       ..
*       .. Array Arguments ..
*       COMPLEX*16         A( LDA, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZLAUU2 computes the product U * U**H or L**H * L, where the triangular
*> factor U or L is stored in the upper or lower triangular part of
*> the array A.
*>
*> If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
*> overwriting the factor U in A.
*> If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
*> overwriting the factor L in A.
*>
*> This is the unblocked form of the algorithm, calling Level 2 BLAS.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the triangular factor stored in the array A
*>          is upper or lower triangular:
*>          = 'U':  Upper triangular
*>          = 'L':  Lower triangular
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the triangular factor U or L.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          On entry, the triangular factor U or L.
*>          On exit, if UPLO = 'U', the upper triangle of A is
*>          overwritten with the upper triangle of the product U * U**H;
*>          if UPLO = 'L', the lower triangle of A is overwritten with
*>          the lower triangle of the product L**H * L.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit
*>          < 0: if INFO = -k, the k-th argument had an illegal value
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
*> \ingroup complex16OTHERauxiliary
*
*  =====================================================================
      SUBROUTINE AB_ZLAUU2( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I
      DOUBLE PRECISION   AII
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      COMPLEX*16         AB_ZDOTC
      EXTERNAL           AB_LSAME, AB_ZDOTC
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_XERBLA, AB_ZDSCAL, AB_ZGEMV, AB_ZLACGV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, MAX
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
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_ZLAUU2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Compute the product U * U**H.
*
         DO 10 I = 1, N
            AII = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = AII*AII + DBLE( AB_ZDOTC( N-I, A( I, I+1 ), L
     $DA,
     $                     A( I, I+1 ), LDA ) )
               CALL AB_ZLACGV( N-I, A( I, I+1 ), LDA )
               CALL AB_ZGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 )
     $,
     $                     LDA, A( I, I+1 ), LDA, DCMPLX( AII ),
     $                     A( 1, I ), 1 )
               CALL AB_ZLACGV( N-I, A( I, I+1 ), LDA )
            ELSE
               CALL AB_ZDSCAL( I, AII, A( 1, I ), 1 )
            END IF
   10    CONTINUE
*
      ELSE
*
*        Compute the product L**H * L.
*
         DO 20 I = 1, N
            AII = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = AII*AII + DBLE( AB_ZDOTC( N-I, A( I+1, I ), 1
     $,
     $                     A( I+1, I ), 1 ) )
               CALL AB_ZLACGV( I-1, A( I, 1 ), LDA )
               CALL AB_ZGEMV( 'Conjugate transpose', N-I, I-1, ONE,
     $                     A( I+1, 1 ), LDA, A( I+1, I ), 1,
     $                     DCMPLX( AII ), A( I, 1 ), LDA )
               CALL AB_ZLACGV( I-1, A( I, 1 ), LDA )
            ELSE
               CALL AB_ZDSCAL( I, AII, A( I, 1 ), LDA )
            END IF
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of AB_ZLAUU2
*
      END
