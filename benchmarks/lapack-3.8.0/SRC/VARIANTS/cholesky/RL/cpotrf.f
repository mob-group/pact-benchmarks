C> \brief \b AB_CPOTRF VARIANT: right looking block version of the algor
     $ithm, calling Level 3 BLAS.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CPOTRF ( UPLO, N, A, LDA, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, LDA, N
*       ..
*       .. Array Arguments ..
*       COMPLEX            A( LDA, * )
*       ..
*
*  Purpose
*  =======
*
C>\details \b Purpose:
C>\verbatim
C>
C> AB_CPOTRF computes the Cholesky factorization of a real Hermitian
C> positive definite matrix A.
C>
C> The factorization has the form
C>    A = U**H * U,  if UPLO = 'U', or
C>    A = L  * L**H,  if UPLO = 'L',
C> where U is an upper triangular matrix and L is lower triangular.
C>
C> This is the right looking block version of the algorithm, calling Lev
     $el 3 BLAS.
C>
C>\endverbatim
*
*  Arguments:
*  ==========
*
C> \param[in] UPLO
C> \verbatim
C>          UPLO is CHARACTER*1
C>          = 'U':  Upper triangle of A is stored;
C>          = 'L':  Lower triangle of A is stored.
C> \endverbatim
C>
C> \param[in] N
C> \verbatim
C>          N is INTEGER
C>          The order of the matrix A.  N >= 0.
C> \endverbatim
C>
C> \param[in,out] A
C> \verbatim
C>          A is COMPLEX array, dimension (LDA,N)
C>          On entry, the Hermitian matrix A.  If UPLO = 'U', the leadin
     $g
C>          N-by-N upper triangular part of A contains the upper
C>          triangular part of the matrix A, and the strictly lower
C>          triangular part of A is not referenced.  If UPLO = 'L', the
C>          leading N-by-N lower triangular part of A contains the lower
C>          triangular part of the matrix A, and the strictly upper
C>          triangular part of A is not referenced.
C> \endverbatim
C> \verbatim
C>          On exit, if INFO = 0, the factor U or L from the Cholesky
C>          factorization A = U**H*U or A = L*L**H.
C> \endverbatim
C>
C> \param[in] LDA
C> \verbatim
C>          LDA is INTEGER
C>          The leading dimension of the array A.  LDA >= max(1,N).
C> \endverbatim
C>
C> \param[out] INFO
C> \verbatim
C>          INFO is INTEGER
C>          = 0:  successful exit
C>          < 0:  if INFO = -i, the i-th argument had an illegal value
C>          > 0:  if INFO = i, the leading minor of order i is not
C>                positive definite, and the factorization could not be
C>                completed.
C> \endverbatim
C>
*
*  Authors:
*  ========
*
C> \author Univ. of Tennessee
C> \author Univ. of California Berkeley
C> \author Univ. of Colorado Denver
C> \author NAG Ltd.
*
C> \date December 2016
*
C> \ingroup variantsPOcomputational
*
*  =====================================================================
      SUBROUTINE AB_CPOTRF ( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK computational routine (version 3.1) --
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
      REAL               ONE
      COMPLEX            CONE
      PARAMETER          ( ONE = 1.0E+0, CONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J, JB, NB
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ILAENV
      EXTERNAL           AB_LSAME, AB_ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGEMM, AB_CPOTF2, AB_CHERK, AB_CTRSM, AB_XER
     $BLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
         CALL AB_XERBLA( 'AB_CPOTRF', -INFO )
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
      NB = AB_ILAENV( 1, 'AB_CPOTRF', UPLO, N, -1, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
*
*        Use unblocked code.
*
         CALL AB_CPOTF2( UPLO, N, A, LDA, INFO )
      ELSE
*
*        Use blocked code.
*
         IF( UPPER ) THEN
*
*           Compute the Cholesky factorization A = U'*U.
*
            DO 10 J = 1, N, NB
*
*              Update and factorize the current diagonal block and test
*              for non-positive-definiteness.
*
               JB = MIN( NB, N-J+1 )

               CALL AB_CPOTF2( 'Upper', JB, A( J, J ), LDA, INFO )

               IF( INFO.NE.0 )
     $            GO TO 30

               IF( J+JB.LE.N ) THEN
*
*                 Updating the trailing submatrix.
*
                  CALL AB_CTRSM( 'Left', 'Upper', 'Conjugate Transpose',
     $                        'Non-unit', JB, N-J-JB+1, CONE, A( J, J ),
     $                        LDA, A( J, J+JB ), LDA )
                  CALL AB_CHERK( 'Upper', 'Conjugate transpose', N-J-JB+
     $1,
     $                        JB, -ONE, A( J, J+JB ), LDA,
     $                        ONE, A( J+JB, J+JB ), LDA )
               END IF
   10       CONTINUE
*
         ELSE
*
*           Compute the Cholesky factorization A = L*L'.
*
            DO 20 J = 1, N, NB
*
*              Update and factorize the current diagonal block and test
*              for non-positive-definiteness.
*
               JB = MIN( NB, N-J+1 )

               CALL AB_CPOTF2( 'Lower', JB, A( J, J ), LDA, INFO )

               IF( INFO.NE.0 )
     $            GO TO 30

               IF( J+JB.LE.N ) THEN
*
*                Updating the trailing submatrix.
*
                 CALL AB_CTRSM( 'Right', 'Lower', 'Conjugate Transpose',
     $                       'Non-unit', N-J-JB+1, JB, CONE, A( J, J ),
     $                       LDA, A( J+JB, J ), LDA )

                 CALL AB_CHERK( 'Lower', 'No Transpose', N-J-JB+1, JB,
     $                       -ONE, A( J+JB, J ), LDA,
     $                       ONE, A( J+JB, J+JB ), LDA )
               END IF
   20       CONTINUE
         END IF
      END IF
      GO TO 40
*
   30 CONTINUE
      INFO = INFO + J - 1
*
   40 CONTINUE
      RETURN
*
*     End of AB_CPOTRF
*
      END
