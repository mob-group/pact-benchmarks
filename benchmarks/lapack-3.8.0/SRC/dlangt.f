*> \brief \b AB_DLANGT returns the value of the 1-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a general tridiagonal matrix.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_DLANGT + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_DLANGT.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_DLANGT.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_DLANGT.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       DOUBLE PRECISION FUNCTION AB_DLANGT( NORM, N, DL, D, DU )
*
*       .. Scalar Arguments ..
*       CHARACTER          NORM
*       INTEGER            N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   D( * ), DL( * ), DU( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DLANGT  returns the value of the one norm,  or the Frobenius norm, or
*> the  infinity norm,  or the  element of  largest absolute value  of a
*> real tridiagonal matrix A.
*> \endverbatim
*>
*> \return AB_DLANGT
*> \verbatim
*>
*>    AB_DLANGT = ( max(abs(A(i,j))), NORM = 'M' or 'm'
*>             (
*>             ( norm1(A),         NORM = '1', 'O' or 'o'
*>             (
*>             ( normI(A),         NORM = 'I' or 'i'
*>             (
*>             ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
*>
*> where  norm1  denotes the  one norm of a matrix (maximum column sum),
*> normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*> normF  denotes the  Frobenius norm of a matrix (square root of sum of
*> squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] NORM
*> \verbatim
*>          NORM is CHARACTER*1
*>          Specifies the value to be returned in AB_DLANGT as described
*>          above.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.  When N = 0, AB_DLANGT is
*>          set to zero.
*> \endverbatim
*>
*> \param[in] DL
*> \verbatim
*>          DL is DOUBLE PRECISION array, dimension (N-1)
*>          The (n-1) sub-diagonal elements of A.
*> \endverbatim
*>
*> \param[in] D
*> \verbatim
*>          D is DOUBLE PRECISION array, dimension (N)
*>          The diagonal elements of A.
*> \endverbatim
*>
*> \param[in] DU
*> \verbatim
*>          DU is DOUBLE PRECISION array, dimension (N-1)
*>          The (n-1) super-diagonal elements of A.
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
*> \ingroup doubleOTHERauxiliary
*
*  =====================================================================
      DOUBLE PRECISION FUNCTION AB_DLANGT( NORM, N, DL, D, DU )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DL( * ), DU( * )
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
      DOUBLE PRECISION   ANORM, SCALE, SUM, TEMP
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME, AB_DISNAN
      EXTERNAL           AB_LSAME, AB_DISNAN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DLASSQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.0 ) THEN
         ANORM = ZERO
      ELSE IF( AB_LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         ANORM = ABS( D( N ) )
         DO 10 I = 1, N - 1
            IF( ANORM.LT.ABS( DL( I ) ) .OR. AB_DISNAN( ABS( DL( I ) ) )
     $ )
     $           ANORM = ABS(DL(I))
            IF( ANORM.LT.ABS( D( I ) ) .OR. AB_DISNAN( ABS( D( I ) ) ) )
     $           ANORM = ABS(D(I))
            IF( ANORM.LT.ABS( DU( I ) ) .OR. AB_DISNAN (ABS( DU( I ) ) )
     $ )
     $           ANORM = ABS(DU(I))
   10    CONTINUE
      ELSE IF( AB_LSAME( NORM, 'O' ) .OR. NORM.EQ.'1' ) THEN
*
*        Find norm1(A).
*
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = ABS( D( 1 ) )+ABS( DL( 1 ) )
            TEMP = ABS( D( N ) )+ABS( DU( N-1 ) )
            IF( ANORM .LT. TEMP .OR. AB_DISNAN( TEMP ) ) ANORM = TEMP
            DO 20 I = 2, N - 1
               TEMP = ABS( D( I ) )+ABS( DL( I ) )+ABS( DU( I-1 ) )
               IF( ANORM .LT. TEMP .OR. AB_DISNAN( TEMP ) ) ANORM = TEMP
   20       CONTINUE
         END IF
      ELSE IF( AB_LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = ABS( D( 1 ) )+ABS( DU( 1 ) )
            TEMP = ABS( D( N ) )+ABS( DL( N-1 ) )
            IF( ANORM .LT. TEMP .OR. AB_DISNAN( TEMP ) ) ANORM = TEMP
            DO 30 I = 2, N - 1
               TEMP = ABS( D( I ) )+ABS( DU( I ) )+ABS( DL( I-1 ) )
               IF( ANORM .LT. TEMP .OR. AB_DISNAN( TEMP ) ) ANORM = TEMP
   30       CONTINUE
         END IF
      ELSE IF( ( AB_LSAME( NORM, 'F' ) ) .OR. ( AB_LSAME( NORM, 'E' ) ) 
     $) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         CALL AB_DLASSQ( N, D, 1, SCALE, SUM )
         IF( N.GT.1 ) THEN
            CALL AB_DLASSQ( N-1, DL, 1, SCALE, SUM )
            CALL AB_DLASSQ( N-1, DU, 1, SCALE, SUM )
         END IF
         ANORM = SCALE*SQRT( SUM )
      END IF
*
      AB_DLANGT = ANORM
      RETURN
*
*     End of AB_DLANGT
*
      END
