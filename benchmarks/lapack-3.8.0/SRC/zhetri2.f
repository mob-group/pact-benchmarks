*> \brief \b AB_ZHETRI2
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZHETRI2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZHETRI2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZHETRI2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZHETRI2.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZHETRI2( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, LDA, LWORK, N
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       COMPLEX*16         A( LDA, * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZHETRI2 computes the inverse of a COMPLEX*16 hermitian indefinite matrix
*> A using the factorization A = U*D*U**T or A = L*D*L**T computed by
*> AB_ZHETRF. AB_ZHETRI2 set the LEADING DIMENSION of the workspace
*> before calling AB_ZHETRI2X that actually computes the inverse.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the details of the factorization are stored
*>          as an upper or lower triangular matrix.
*>          = 'U':  Upper triangular, form is A = U*D*U**T;
*>          = 'L':  Lower triangular, form is A = L*D*L**T.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          On entry, the NB diagonal matrix D and the multipliers
*>          used to obtain the factor U or L as computed by AB_ZHETRF.
*>
*>          On exit, if INFO = 0, the (symmetric) inverse of the original
*>          matrix.  If UPLO = 'U', the upper triangular part of the
*>          inverse is formed and the part of A below the diagonal is not
*>          referenced; if UPLO = 'L' the lower triangular part of the
*>          inverse is formed and the part of A above the diagonal is
*>          not referenced.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          Details of the interchanges and the NB structure of D
*>          as determined by AB_ZHETRF.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (N+NB+1)*(NB+3)
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          WORK is size >= (N+NB+1)*(NB+3)
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>           calculates:
*>              - the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array,
*>              - and no error message related to LWORK is issued by AB_XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit
*>          < 0: if INFO = -i, the i-th argument had an illegal value
*>          > 0: if INFO = i, D(i,i) = 0; the matrix is singular and its
*>               inverse could not be computed.
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
*> \date November 2017
*
*> \ingroup complex16HEcomputational
*
*  =====================================================================
      SUBROUTINE AB_ZHETRI2( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
*
*  -- LAPACK computational routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            UPPER, LQUERY
      INTEGER            MINSIZE, NBMAX
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ILAENV
      EXTERNAL           AB_LSAME, AB_ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ZHETRI2X, AB_ZHETRI, AB_XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = AB_LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
*     Get blocksize
      NBMAX = AB_ILAENV( 1, 'AB_ZHETRF', UPLO, N, -1, -1, -1 )
      IF ( NBMAX .GE. N ) THEN
         MINSIZE = N
      ELSE
         MINSIZE = (N+NBMAX+1)*(NBMAX+3)
      END IF
*
      IF( .NOT.UPPER .AND. .NOT.AB_LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF (LWORK .LT. MINSIZE .AND. .NOT.LQUERY ) THEN
         INFO = -7
      END IF
*
*     Quick return if possible
*
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_ZHETRI2', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         WORK(1)=MINSIZE
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN

      IF( NBMAX .GE. N ) THEN
         CALL AB_ZHETRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
      ELSE
         CALL AB_ZHETRI2X( UPLO, N, A, LDA, IPIV, WORK, NBMAX, INFO )
      END IF
      RETURN
*
*     End of AB_ZHETRI2
*
      END
