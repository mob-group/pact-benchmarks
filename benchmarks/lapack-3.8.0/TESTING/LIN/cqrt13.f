*> \brief \b AB_CQRT13
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CQRT13( SCALE, M, N, A, LDA, NORMA, ISEED )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, M, N, SCALE
*       REAL               NORMA
*       ..
*       .. Array Arguments ..
*       INTEGER            ISEED( 4 )
*       COMPLEX            A( LDA, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CQRT13 generates a full-rank matrix that may be scaled to have large
*> or small norm.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SCALE
*> \verbatim
*>          SCALE is INTEGER
*>          SCALE = 1: normally scaled matrix
*>          SCALE = 2: matrix scaled up
*>          SCALE = 3: matrix scaled down
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of A.
*> \endverbatim
*>
*> \param[out] A
*> \verbatim
*>          A is COMPLEX array, dimension (LDA,N)
*>          The M-by-N matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.
*> \endverbatim
*>
*> \param[out] NORMA
*> \verbatim
*>          NORMA is REAL
*>          The one-norm of A.
*> \endverbatim
*>
*> \param[in,out] ISEED
*> \verbatim
*>          ISEED is integer array, dimension (4)
*>          Seed for random number generator
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
      SUBROUTINE AB_CQRT13( SCALE, M, N, A, LDA, NORMA, ISEED )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            LDA, M, N, SCALE
      REAL               NORMA
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      COMPLEX            A( LDA, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, J
      REAL               BIGNUM, SMLNUM
*     ..
*     .. External Functions ..
      REAL               AB_CLANGE, AB_SCASUM, AB_SLAMCH
      EXTERNAL           AB_CLANGE, AB_SCASUM, AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CLARNV, AB_CLASCL, AB_SLABAD
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL, SIGN
*     ..
*     .. Local Arrays ..
      REAL               DUMMY( 1 )
*     ..
*     .. Executable Statements ..
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
*     benign matrix
*
      DO 10 J = 1, N
         CALL AB_CLARNV( 2, ISEED, M, A( 1, J ) )
         IF( J.LE.M ) THEN
            A( J, J ) = A( J, J ) + CMPLX( SIGN( AB_SCASUM( M, A( 1, J )
     $,
     $                  1 ), REAL( A( J, J ) ) ) )
         END IF
   10 CONTINUE
*
*     scaled versions
*
      IF( SCALE.NE.1 ) THEN
         NORMA = AB_CLANGE( 'Max', M, N, A, LDA, DUMMY )
         SMLNUM = AB_SLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
         CALL AB_SLABAD( SMLNUM, BIGNUM )
         SMLNUM = SMLNUM / AB_SLAMCH( 'Epsilon' )
         BIGNUM = ONE / SMLNUM
*
         IF( SCALE.EQ.2 ) THEN
*
*           matrix scaled up
*
            CALL AB_CLASCL( 'General', 0, 0, NORMA, BIGNUM, M, N, A, LDA
     $,
     $                   INFO )
         ELSE IF( SCALE.EQ.3 ) THEN
*
*           matrix scaled down
*
            CALL AB_CLASCL( 'General', 0, 0, NORMA, SMLNUM, M, N, A, LDA
     $,
     $                   INFO )
         END IF
      END IF
*
      NORMA = AB_CLANGE( 'One-norm', M, N, A, LDA, DUMMY )
      RETURN
*
*     End of AB_CQRT13
*
      END
