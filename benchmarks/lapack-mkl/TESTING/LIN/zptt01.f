*> \brief \b ZPTT01
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE ZPTT01( N, D, E, DF, EF, WORK, RESID )
*
*       .. Scalar Arguments ..
*       INTEGER            N
*       DOUBLE PRECISION   RESID
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   D( * ), DF( * )
*       COMPLEX*16         E( * ), EF( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> ZPTT01 reconstructs a tridiagonal matrix A from its L*D*L'
*> factorization and computes the residual
*>    norm(L*D*L' - A) / ( n * norm(A) * EPS ),
*> where EPS is the machine epsilon.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGTER
*>          The order of the matrix A.
*> \endverbatim
*>
*> \param[in] D
*> \verbatim
*>          D is DOUBLE PRECISION array, dimension (N)
*>          The n diagonal elements of the tridiagonal matrix A.
*> \endverbatim
*>
*> \param[in] E
*> \verbatim
*>          E is COMPLEX*16 array, dimension (N-1)
*>          The (n-1) subdiagonal elements of the tridiagonal matrix A.
*> \endverbatim
*>
*> \param[in] DF
*> \verbatim
*>          DF is DOUBLE PRECISION array, dimension (N)
*>          The n diagonal elements of the factor L from the L*D*L'
*>          factorization of A.
*> \endverbatim
*>
*> \param[in] EF
*> \verbatim
*>          EF is COMPLEX*16 array, dimension (N-1)
*>          The (n-1) subdiagonal elements of the factor L from the
*>          L*D*L' factorization of A.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (2*N)
*> \endverbatim
*>
*> \param[out] RESID
*> \verbatim
*>          RESID is DOUBLE PRECISION
*>          norm(L*D*L' - A) / (n * norm(A) * EPS)
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
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE ZPTT01( N, D, E, DF, EF, WORK, RESID )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            N
      DOUBLE PRECISION   RESID
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DF( * )
      COMPLEX*16         E( * ), EF( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   ANORM, EPS
      COMPLEX*16         DE
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH
      EXTERNAL           AB_DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCONJG, MAX
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
      EPS = AB_DLAMCH( 'Epsilon' )
*
*     Construct the difference L*D*L' - A.
*
      WORK( 1 ) = DF( 1 ) - D( 1 )
      DO 10 I = 1, N - 1
         DE = DF( I )*EF( I )
         WORK( N+I ) = DE - E( I )
         WORK( 1+I ) = DE*DCONJG( EF( I ) ) + DF( I+1 ) - D( I+1 )
   10 CONTINUE
*
*     Compute the 1-norms of the tridiagonal matrices A and WORK.
*
      IF( N.EQ.1 ) THEN
         ANORM = D( 1 )
         RESID = ABS( WORK( 1 ) )
      ELSE
         ANORM = MAX( D( 1 )+ABS( E( 1 ) ), D( N )+ABS( E( N-1 ) ) )
         RESID = MAX( ABS( WORK( 1 ) )+ABS( WORK( N+1 ) ),
     $           ABS( WORK( N ) )+ABS( WORK( 2*N-1 ) ) )
         DO 20 I = 2, N - 1
            ANORM = MAX( ANORM, D( I )+ABS( E( I ) )+ABS( E( I-1 ) ) )
            RESID = MAX( RESID, ABS( WORK( I ) )+ABS( WORK( N+I-1 ) )+
     $              ABS( WORK( N+I ) ) )
   20    CONTINUE
      END IF
*
*     Compute norm(L*D*L' - A) / (n * norm(A) * EPS)
*
      IF( ANORM.LE.ZERO ) THEN
         IF( RESID.NE.ZERO )
     $      RESID = ONE / EPS
      ELSE
         RESID = ( ( RESID / DBLE( N ) ) / ANORM ) / EPS
      END IF
*
      RETURN
*
*     End of ZPTT01
*
      END
