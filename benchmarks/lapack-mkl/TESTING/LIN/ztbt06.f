*> \brief \b ZTBT06
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE ZTBT06( RCOND, RCONDC, UPLO, DIAG, N, KD, AB, LDAB,
*                          RWORK, RAT )
*
*       .. Scalar Arguments ..
*       CHARACTER          DIAG, UPLO
*       INTEGER            KD, LDAB, N
*       DOUBLE PRECISION   RAT, RCOND, RCONDC
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   RWORK( * )
*       COMPLEX*16         AB( LDAB, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> ZTBT06 computes a test ratio comparing RCOND (the reciprocal
*> condition number of a triangular matrix A) and RCONDC, the estimate
*> computed by ZTBCON.  Information about the triangular matrix A is
*> used if one estimate is zero and the other is non-zero to decide if
*> underflow in the estimate is justified.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] RCOND
*> \verbatim
*>          RCOND is DOUBLE PRECISION
*>          The estimate of the reciprocal condition number obtained by
*>          forming the explicit inverse of the matrix A and computing
*>          RCOND = 1/( norm(A) * norm(inv(A)) ).
*> \endverbatim
*>
*> \param[in] RCONDC
*> \verbatim
*>          RCONDC is DOUBLE PRECISION
*>          The estimate of the reciprocal condition number computed by
*>          ZTBCON.
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER
*>          Specifies whether the matrix A is upper or lower triangular.
*>          = 'U':  Upper triangular
*>          = 'L':  Lower triangular
*> \endverbatim
*>
*> \param[in] DIAG
*> \verbatim
*>          DIAG is CHARACTER
*>          Specifies whether or not the matrix A is unit triangular.
*>          = 'N':  Non-unit triangular
*>          = 'U':  Unit triangular
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] KD
*> \verbatim
*>          KD is INTEGER
*>          The number of superdiagonals or subdiagonals of the
*>          triangular band matrix A.  KD >= 0.
*> \endverbatim
*>
*> \param[in] AB
*> \verbatim
*>          AB is COMPLEX*16 array, dimension (LDAB,N)
*>          The upper or lower triangular band matrix A, stored in the
*>          first kd+1 rows of the array. The j-th column of A is stored
*>          in the j-th column of the array AB as follows:
*>          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*>          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*> \endverbatim
*>
*> \param[in] LDAB
*> \verbatim
*>          LDAB is INTEGER
*>          The leading dimension of the array AB.  LDAB >= KD+1.
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension (N)
*> \endverbatim
*>
*> \param[out] RAT
*> \verbatim
*>          RAT is DOUBLE PRECISION
*>          The test ratio.  If both RCOND and RCONDC are nonzero,
*>             RAT = MAX( RCOND, RCONDC )/MIN( RCOND, RCONDC ) - 1.
*>          If RAT = 0, the two estimates are exactly the same.
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
      SUBROUTINE ZTBT06( RCOND, RCONDC, UPLO, DIAG, N, KD, AB, LDAB,
     $                   RWORK, RAT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            KD, LDAB, N
      DOUBLE PRECISION   RAT, RCOND, RCONDC
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         AB( LDAB, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   ANORM, BIGNUM, EPS, RMAX, RMIN
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH, ZLANTB
      EXTERNAL           AB_DLAMCH, ZLANTB
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
      EPS = AB_DLAMCH( 'Epsilon' )
      RMAX = MAX( RCOND, RCONDC )
      RMIN = MIN( RCOND, RCONDC )
*
*     Do the easy cases first.
*
      IF( RMIN.LT.ZERO ) THEN
*
*        Invalid value for RCOND or RCONDC, return 1/EPS.
*
         RAT = ONE / EPS
*
      ELSE IF( RMIN.GT.ZERO ) THEN
*
*        Both estimates are positive, return RMAX/RMIN - 1.
*
         RAT = RMAX / RMIN - ONE
*
      ELSE IF( RMAX.EQ.ZERO ) THEN
*
*        Both estimates zero.
*
         RAT = ZERO
*
      ELSE
*
*        One estimate is zero, the other is non-zero.  If the matrix is
*        ill-conditioned, return the nonzero estimate multiplied by
*        1/EPS; if the matrix is badly scaled, return the nonzero
*        estimate multiplied by BIGNUM/TMAX, where TMAX is the maximum
*        element in absolute value in A.
*
         BIGNUM = ONE / AB_DLAMCH( 'Safe minimum' )
         ANORM = ZLANTB( 'M', UPLO, DIAG, N, KD, AB, LDAB, RWORK )
*
         RAT = RMAX*( MIN( BIGNUM / MAX( ONE, ANORM ), ONE / EPS ) )
      END IF
*
      RETURN
*
*     End of ZTBT06
*
      END
