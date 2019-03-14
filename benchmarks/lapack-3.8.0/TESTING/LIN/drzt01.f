*> \brief \b AB_DRZT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       DOUBLE PRECISION FUNCTION AB_DRZT01( M, N, A, AF, LDA, TAU, WORK,
*                        LWORK )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), TAU( * ),
*      $                   WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DRZT01 returns
*>      || A - R*Q || / ( M * eps * ||A|| )
*> for an upper trapezoidal A that was factored with AB_DTZRZF.
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
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          The original upper trapezoidal M by N matrix A.
*> \endverbatim
*>
*> \param[in] AF
*> \verbatim
*>          AF is DOUBLE PRECISION array, dimension (LDA,N)
*>          The output of AB_DTZRZF for input matrix A.
*>          The lower triangle is not referenced.
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
*>          TAU is DOUBLE PRECISION array, dimension (M)
*>          Details of the HousehoAB_LDEr transformations as returned by
*>          AB_DTZRZF.
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
*>          The length of the array WORK.  LWORK >= m*n + m*nb.
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
      DOUBLE PRECISION FUNCTION AB_DRZT01( M, N, A, AF, LDA, TAU, WORK,
     $                 LWORK )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), AF( LDA, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
      DOUBLE PRECISION   NORMA
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   RWORK( 1 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANGE
      EXTERNAL           AB_DLAMCH, AB_DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DAXPY, AB_DLASET, AB_DORMRZ, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Executable Statements ..
*
      AB_DRZT01 = ZERO
*
      IF( LWORK.LT.M*N+M ) THEN
         CALL AB_XERBLA( 'AB_DRZT01', 8 )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      NORMA = AB_DLANGE( 'One-norm', M, N, A, LDA, RWORK )
*
*     Copy upper triangle R
*
      CALL AB_DLASET( 'Full', M, N, ZERO, ZERO, WORK, M )
      DO 20 J = 1, M
         DO 10 I = 1, J
            WORK( ( J-1 )*M+I ) = AF( I, J )
   10    CONTINUE
   20 CONTINUE
*
*     R = R * P(1) * ... *P(m)
*
      CALL AB_DORMRZ( 'Right', 'No tranpose', M, N, M, N-M, AF, LDA, TAU
     $,
     $             WORK, M, WORK( M*N+1 ), LWORK-M*N, INFO )
*
*     R = R - A
*
      DO 30 I = 1, N
         CALL AB_DAXPY( M, -ONE, A( 1, I ), 1, WORK( ( I-1 )*M+1 ), 1 )
   30 CONTINUE
*
      AB_DRZT01 = AB_DLANGE( 'One-norm', M, N, WORK, M, RWORK )
*
      AB_DRZT01 = AB_DRZT01 / ( AB_DLAMCH( 'Epsilon' )*DBLE( MAX( M, N )
     $ ) )
      IF( NORMA.NE.ZERO )
     $   AB_DRZT01 = AB_DRZT01 / NORMA
*
      RETURN
*
*     End of AB_DRZT01
*
      END
