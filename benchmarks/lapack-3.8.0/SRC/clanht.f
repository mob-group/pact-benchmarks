*> \brief \b AB_CLANHT returns the value of the 1-norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value of a complex Hermitian tridiagonal matrix.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_CLANHT + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_CLANHT.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_CLANHT.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_CLANHT.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       REAL             FUNCTION AB_CLANHT( NORM, N, D, E )
*
*       .. Scalar Arguments ..
*       CHARACTER          NORM
*       INTEGER            N
*       ..
*       .. Array Arguments ..
*       REAL               D( * )
*       COMPLEX            E( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CLANHT  returns the value of the one norm,  or the Frobenius norm, or
*> the  infinity norm,  or the  element of  largest absolute value  of a
*> complex Hermitian tridiagonal matrix A.
*> \endverbatim
*>
*> \return AB_CLANHT
*> \verbatim
*>
*>    AB_CLANHT = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*>          Specifies the value to be returned in AB_CLANHT as described
*>          above.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.  When N = 0, AB_CLANHT is
*>          set to zero.
*> \endverbatim
*>
*> \param[in] D
*> \verbatim
*>          D is REAL array, dimension (N)
*>          The diagonal elements of A.
*> \endverbatim
*>
*> \param[in] E
*> \verbatim
*>          E is COMPLEX array, dimension (N-1)
*>          The (n-1) sub-diagonal or super-diagonal elements of A.
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
      REAL             FUNCTION AB_CLANHT( NORM, N, D, E )
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
      REAL               D( * )
      COMPLEX            E( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      REAL               ANORM, SCALE, SUM
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME, AB_SISNAN
      EXTERNAL           AB_LSAME, AB_SISNAN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CLASSQ, AB_SLASSQ
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
            SUM = ABS( D( I ) )
            IF( ANORM .LT. SUM .OR. AB_SISNAN( SUM ) ) ANORM = SUM
            SUM = ABS( E( I ) )
            IF( ANORM .LT. SUM .OR. AB_SISNAN( SUM ) ) ANORM = SUM
   10    CONTINUE
      ELSE IF( AB_LSAME( NORM, 'O' ) .OR. NORM.EQ.'1' .OR.
     $         AB_LSAME( NORM, 'I' ) ) THEN
*
*        Find norm1(A).
*
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = ABS( D( 1 ) )+ABS( E( 1 ) )
            SUM = ABS( E( N-1 ) )+ABS( D( N ) )
            IF( ANORM .LT. SUM .OR. AB_SISNAN( SUM ) ) ANORM = SUM
            DO 20 I = 2, N - 1
               SUM = ABS( D( I ) )+ABS( E( I ) )+ABS( E( I-1 ) )
               IF( ANORM .LT. SUM .OR. AB_SISNAN( SUM ) ) ANORM = SUM
   20       CONTINUE
         END IF
      ELSE IF( ( AB_LSAME( NORM, 'F' ) ) .OR. ( AB_LSAME( NORM, 'E' )
     $ ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         IF( N.GT.1 ) THEN
            CALL AB_CLASSQ( N-1, E, 1, SCALE, SUM )
            SUM = 2*SUM
         END IF
         CALL AB_SLASSQ( N, D, 1, SCALE, SUM )
         ANORM = SCALE*SQRT( SUM )
      END IF
*
      AB_CLANHT = ANORM
      RETURN
*
*     End of AB_CLANHT
*
      END
