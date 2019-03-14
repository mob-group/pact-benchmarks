*> \brief \b AB_CPTEQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_CPTEQR + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_CPTEQR.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_CPTEQR.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_CPTEQR.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          COMPZ
*       INTEGER            INFO, LDZ, N
*       ..
*       .. Array Arguments ..
*       REAL               D( * ), E( * ), WORK( * )
*       COMPLEX            Z( LDZ, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CPTEQR computes all eigenvalues and, optionally, eigenvectors of a
*> symmetric positive definite tridiagonal matrix by first factoring the
*> matrix using AB_SPTTRF and then calling AB_CBDSQR to compute the singular
*> values of the bidiagonal factor.
*>
*> This routine computes the eigenvalues of the positive definite
*> tridiagonal matrix to high relative accuracy.  This means that if the
*> eigenvalues range over many orders of magnitude in size, then the
*> small eigenvalues and corresponding eigenvectors will be computed
*> more accurately than, for example, with the standard QR method.
*>
*> The eigenvectors of a full or band positive definite Hermitian matrix
*> can also be found if AB_CHETRD, AB_CHPTRD, or AB_CHBTRD has been used to
*> reduce this matrix to tridiagonal form.  (The reduction to
*> tridiagonal form, however, may preclude the possibility of obtaining
*> high relative accuracy in the small eigenvalues of the original
*> matrix, if these eigenvalues range over many orders of magnitude.)
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] COMPZ
*> \verbatim
*>          COMPZ is CHARACTER*1
*>          = 'N':  Compute eigenvalues only.
*>          = 'V':  Compute eigenvectors of original Hermitian
*>                  matrix also.  Array Z contains the unitary matrix
*>                  used to reduce the original matrix to tridiagonal
*>                  form.
*>          = 'I':  Compute eigenvectors of tridiagonal matrix also.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] D
*> \verbatim
*>          D is REAL array, dimension (N)
*>          On entry, the n diagonal elements of the tridiagonal matrix.
*>          On normal exit, D contains the eigenvalues, in descending
*>          order.
*> \endverbatim
*>
*> \param[in,out] E
*> \verbatim
*>          E is REAL array, dimension (N-1)
*>          On entry, the (n-1) subdiagonal elements of the tridiagonal
*>          matrix.
*>          On exit, E has been destroyed.
*> \endverbatim
*>
*> \param[in,out] Z
*> \verbatim
*>          Z is COMPLEX array, dimension (LDZ, N)
*>          On entry, if COMPZ = 'V', the unitary matrix used in the
*>          reduction to tridiagonal form.
*>          On exit, if COMPZ = 'V', the orthonormal eigenvectors of the
*>          original Hermitian matrix;
*>          if COMPZ = 'I', the orthonormal eigenvectors of the
*>          tridiagonal matrix.
*>          If INFO > 0 on exit, Z contains the eigenvectors associated
*>          with only the stored eigenvalues.
*>          If  COMPZ = 'N', then Z is not referenced.
*> \endverbatim
*>
*> \param[in] LDZ
*> \verbatim
*>          LDZ is INTEGER
*>          The leading dimension of the array Z.  LDZ >= 1, and if
*>          COMPZ = 'V' or 'I', LDZ >= max(1,N).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is REAL array, dimension (4*N)
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit.
*>          < 0:  if INFO = -i, the i-th argument had an illegal value.
*>          > 0:  if INFO = i, and i is:
*>                <= N  the Cholesky factorization of the matrix could
*>                      not be performed because the i-th principal minor
*>                      was not positive definite.
*>                > N   the SVD algorithm failed to converge;
*>                      if INFO = N+i, i off-diagonal elements of the
*>                      bidiagonal factor did not converge to zero.
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
*> \ingroup complexPTcomputational
*
*  =====================================================================
      SUBROUTINE AB_CPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, N
*     ..
*     .. Array Arguments ..
      REAL               D( * ), E( * ), WORK( * )
      COMPLEX            Z( LDZ, * )
*     ..
*
*  ====================================================================
*
*     .. Parameters ..
      COMPLEX            CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0E+0, 0.0E+0 ),
     $                   CONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      EXTERNAL           AB_LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CBDSQR, AB_CLASET, AB_SPTTRF, AB_XERBLA
*     ..
*     .. Local Arrays ..
      COMPLEX            C( 1, 1 ), VT( 1, 1 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ICOMPZ, NRU
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( AB_LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( AB_LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( AB_LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1,
     $         N ) ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_CPTEQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.GT.0 )
     $      Z( 1, 1 ) = CONE
         RETURN
      END IF
      IF( ICOMPZ.EQ.2 )
     $   CALL AB_CLASET( 'Full', N, N, CZERO, CONE, Z, LDZ )
*
*     Call AB_SPTTRF to factor the matrix.
*
      CALL AB_SPTTRF( N, D, E, INFO )
      IF( INFO.NE.0 )
     $   RETURN
      DO 10 I = 1, N
         D( I ) = SQRT( D( I ) )
   10 CONTINUE
      DO 20 I = 1, N - 1
         E( I ) = E( I )*D( I )
   20 CONTINUE
*
*     Call AB_CBDSQR to compute the singular values/vectors of the
*     bidiagonal factor.
*
      IF( ICOMPZ.GT.0 ) THEN
         NRU = N
      ELSE
         NRU = 0
      END IF
      CALL AB_CBDSQR( 'Lower', N, 0, NRU, 0, D, E, VT, 1, Z, LDZ, C, 1,
     $             WORK, INFO )
*
*     Square the singular values.
*
      IF( INFO.EQ.0 ) THEN
         DO 30 I = 1, N
            D( I ) = D( I )*D( I )
   30    CONTINUE
      ELSE
         INFO = N + INFO
      END IF
*
      RETURN
*
*     End of AB_CPTEQR
*
      END
