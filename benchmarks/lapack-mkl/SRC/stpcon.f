*> \brief \b AB_STPCON
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_STPCON + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_STPCON.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_STPCON.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_STPCON.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_STPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, IWORK,
*                          INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          DIAG, NORM, UPLO
*       INTEGER            INFO, N
*       REAL               RCOND
*       ..
*       .. Array Arguments ..
*       INTEGER            IWORK( * )
*       REAL               AP( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_STPCON estimates the reciprocal of the condition number of a packed
*> triangular matrix A, in either the 1-norm or the infinity-norm.
*>
*> The norm of A is computed and an estimate is obtained for
*> norm(inv(A)), then the reciprocal of the condition number is
*> computed as
*>    RCOND = 1 / ( norm(A) * norm(inv(A)) ).
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] NORM
*> \verbatim
*>          NORM is CHARACTER*1
*>          Specifies whether the 1-norm condition number or the
*>          infinity-norm condition number is required:
*>          = '1' or 'O':  1-norm;
*>          = 'I':         Infinity-norm.
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  A is upper triangular;
*>          = 'L':  A is lower triangular.
*> \endverbatim
*>
*> \param[in] DIAG
*> \verbatim
*>          DIAG is CHARACTER*1
*>          = 'N':  A is non-unit triangular;
*>          = 'U':  A is unit triangular.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] AP
*> \verbatim
*>          AP is REAL array, dimension (N*(N+1)/2)
*>          The upper or lower triangular matrix A, packed columnwise in
*>          a linear array.  The j-th column of A is stored in the array
*>          AP as follows:
*>          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*>          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*>          If DIAG = 'U', the diagonal elements of A are not referenced
*>          and are assumed to be 1.
*> \endverbatim
*>
*> \param[out] RCOND
*> \verbatim
*>          RCOND is REAL
*>          The reciprocal of the condition number of the matrix A,
*>          computed as RCOND = 1/(norm(A) * norm(inv(A))).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is REAL array, dimension (3*N)
*> \endverbatim
*>
*> \param[out] IWORK
*> \verbatim
*>          IWORK is INTEGER array, dimension (N)
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
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
*> \ingroup realOTHERcomputational
*
*  =====================================================================
      SUBROUTINE AB_STPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, IWORK,
     $                   INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORM, UPLO
      INTEGER            INFO, N
      REAL               RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      REAL               AP( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOUNIT, ONENRM, UPPER
      CHARACTER          NORMIN
      INTEGER            IX, KASE, KASE1
      REAL               AINVNM, ANORM, SCALE, SMLNUM, XNORM
*     ..
*     .. Local Arrays ..
      INTEGER            ISAVE( 3 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ISAMAX
      REAL               AB_SLAMCH, AB_SLANTP
      EXTERNAL           AB_LSAME, AB_ISAMAX, AB_SLAMCH, AB_SLANTP
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_SLACN2, AB_SLATPS, AB_SRSCL, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, REAL
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = AB_LSAME( UPLO, 'U' )
      ONENRM = NORM.EQ.'1' .OR. AB_LSAME( NORM, 'O' )
      NOUNIT = AB_LSAME( DIAG, 'N' )
*
      IF( .NOT.ONENRM .AND. .NOT.AB_LSAME( NORM, 'I' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.UPPER .AND. .NOT.AB_LSAME( UPLO, 'L' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOUNIT .AND. .NOT.AB_LSAME( DIAG, 'U' ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_STPCON', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         RCOND = ONE
         RETURN
      END IF
*
      RCOND = ZERO
      SMLNUM = AB_SLAMCH( 'Safe minimum' )*REAL( MAX( 1, N ) )
*
*     Compute the norm of the triangular matrix A.
*
      ANORM = AB_SLANTP( NORM, UPLO, DIAG, N, AP, WORK )
*
*     Continue only if ANORM > 0.
*
      IF( ANORM.GT.ZERO ) THEN
*
*        Estimate the norm of the inverse of A.
*
         AINVNM = ZERO
         NORMIN = 'N'
         IF( ONENRM ) THEN
            KASE1 = 1
         ELSE
            KASE1 = 2
         END IF
         KASE = 0
   10    CONTINUE
         CALL AB_SLACN2( N, WORK( N+1 ), WORK, IWORK, AINVNM, KASE, ISAV
     $E )
         IF( KASE.NE.0 ) THEN
            IF( KASE.EQ.KASE1 ) THEN
*
*              Multiply by inv(A).
*
               CALL AB_SLATPS( UPLO, 'No transpose', DIAG, NORMIN, N, AP
     $,
     $                      WORK, SCALE, WORK( 2*N+1 ), INFO )
            ELSE
*
*              Multiply by inv(A**T).
*
               CALL AB_SLATPS( UPLO, 'Transpose', DIAG, NORMIN, N, AP,
     $                      WORK, SCALE, WORK( 2*N+1 ), INFO )
            END IF
            NORMIN = 'Y'
*
*           Multiply by 1/SCALE if doing so will not cause overflow.
*
            IF( SCALE.NE.ONE ) THEN
               IX = AB_ISAMAX( N, WORK, 1 )
               XNORM = ABS( WORK( IX ) )
               IF( SCALE.LT.XNORM*SMLNUM .OR. SCALE.EQ.ZERO )
     $            GO TO 20
               CALL AB_SRSCL( N, SCALE, WORK, 1 )
            END IF
            GO TO 10
         END IF
*
*        Compute the estimate of the reciprocal condition number.
*
         IF( AINVNM.NE.ZERO )
     $      RCOND = ( ONE / ANORM ) / AINVNM
      END IF
*
   20 CONTINUE
      RETURN
*
*     End of AB_STPCON
*
      END
