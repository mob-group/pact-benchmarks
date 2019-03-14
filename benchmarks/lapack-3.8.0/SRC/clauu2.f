*> \brief \b AB_CLAUU2 computes the product UUH or LHL, where U and L are upper or lower triangular matrices (unblocked algorithm).
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_CLAUU2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_CLAUU2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_CLAUU2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_CLAUU2.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CLAUU2( UPLO, N, A, LDA, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, LDA, N
*       ..
*       .. Array Arguments ..
*       COMPLEX            A( LDA, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CLAUU2 computes the product U * U**H or L**H * L, where the triangular
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
*>          A is COMPLEX array, dimension (LDA,N)
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
*> \ingroup complexOTHERauxiliary
*
*  =====================================================================
      SUBROUTINE AB_CLAUU2( UPLO, N, A, LDA, INFO )
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
      COMPLEX            A( LDA, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX            ONE
      PARAMETER          ( ONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I
      REAL               AII
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      COMPLEX            AB_CDOTC
      EXTERNAL           AB_LSAME, AB_CDOTC
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGEMV, AB_CLACGV, AB_CAB_SSCAL, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MAX, REAL
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
         CALL AB_XERBLA( 'AB_CLAUU2', -INFO )
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
               A( I, I ) = AII*AII + REAL( AB_CDOTC( N-I, A( I, I+1 ), L
     $DA,
     $                     A( I, I+1 ), LDA ) )
               CALL AB_CLACGV( N-I, A( I, I+1 ), LDA )
               CALL AB_CGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 )
     $,
     $                     LDA, A( I, I+1 ), LDA, CMPLX( AII ),
     $                     A( 1, I ), 1 )
               CALL AB_CLACGV( N-I, A( I, I+1 ), LDA )
            ELSE
               CALL AB_CAB_SSCAL( I, AII, A( 1, I ), 1 )
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
               A( I, I ) = AII*AII + REAL( AB_CDOTC( N-I, A( I+1, I ), 1
     $,
     $                     A( I+1, I ), 1 ) )
               CALL AB_CLACGV( I-1, A( I, 1 ), LDA )
               CALL AB_CGEMV( 'Conjugate transpose', N-I, I-1, ONE,
     $                     A( I+1, 1 ), LDA, A( I+1, I ), 1,
     $                     CMPLX( AII ), A( I, 1 ), LDA )
               CALL AB_CLACGV( I-1, A( I, 1 ), LDA )
            ELSE
               CALL AB_CAB_SSCAL( I, AII, A( I, 1 ), LDA )
            END IF
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of AB_CLAUU2
*
      END
