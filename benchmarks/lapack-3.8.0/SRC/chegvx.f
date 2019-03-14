*> \brief \b AB_CHEGVX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_CHEGVX + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_CHEGVx.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_CHEGVx.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_CHEGVx.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CHEGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,
*                          VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,
*                          LWORK, RWORK, IWORK, IFAIL, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          JOBZ, RANGE, UPLO
*       INTEGER            IL, INFO, ITYPE, IU, LDA, LDB, LDZ, LWORK, M, N
*       REAL               ABSTOL, VL, VU
*       ..
*       .. Array Arguments ..
*       INTEGER            IFAIL( * ), IWORK( * )
*       REAL               RWORK( * ), W( * )
*       COMPLEX            A( LDA, * ), B( LDB, * ), WORK( * ),
*      $                   Z( LDZ, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CHEGVX computes selected eigenvalues, and optionally, eigenvectors
*> of a complex generalized Hermitian-definite eigenproblem, of the form
*> A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*> B are assumed to be Hermitian and B is also positive definite.
*> Eigenvalues and eigenvectors can be selected by specifying either a
*> range of values or a range of indices for the desired eigenvalues.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] ITYPE
*> \verbatim
*>          ITYPE is INTEGER
*>          Specifies the problem type to be solved:
*>          = 1:  A*x = (lambda)*B*x
*>          = 2:  A*B*x = (lambda)*x
*>          = 3:  B*A*x = (lambda)*x
*> \endverbatim
*>
*> \param[in] JOBZ
*> \verbatim
*>          JOBZ is CHARACTER*1
*>          = 'N':  Compute eigenvalues only;
*>          = 'V':  Compute eigenvalues and eigenvectors.
*> \endverbatim
*>
*> \param[in] RANGE
*> \verbatim
*>          RANGE is CHARACTER*1
*>          = 'A': all eigenvalues will be found.
*>          = 'V': all eigenvalues in the half-open interval (VL,VU]
*>                 will be found.
*>          = 'I': the IL-th through IU-th eigenvalues will be found.
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangles of A and B are stored;
*>          = 'L':  Lower triangles of A and B are stored.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrices A and B.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is COMPLEX array, dimension (LDA, N)
*>          On entry, the Hermitian matrix A.  If UPLO = 'U', the
*>          leading N-by-N upper triangular part of A contains the
*>          upper triangular part of the matrix A.  If UPLO = 'L',
*>          the leading N-by-N lower triangular part of A contains
*>          the lower triangular part of the matrix A.
*>
*>          On exit,  the lower triangle (if UPLO='L') or the upper
*>          triangle (if UPLO='U') of A, including the diagonal, is
*>          destroyed.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is COMPLEX array, dimension (LDB, N)
*>          On entry, the Hermitian matrix B.  If UPLO = 'U', the
*>          leading N-by-N upper triangular part of B contains the
*>          upper triangular part of the matrix B.  If UPLO = 'L',
*>          the leading N-by-N lower triangular part of B contains
*>          the lower triangular part of the matrix B.
*>
*>          On exit, if INFO <= N, the part of B containing the matrix is
*>          overwritten by the triangular factor U or L from the Cholesky
*>          factorization B = U**H*U or B = L*L**H.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[in] VL
*> \verbatim
*>          VL is REAL
*>
*>          If RANGE='V', the lower bound of the interval to
*>          be searched for eigenvalues. VL < VU.
*>          Not referenced if RANGE = 'A' or 'I'.
*> \endverbatim
*>
*> \param[in] VU
*> \verbatim
*>          VU is REAL
*>
*>          If RANGE='V', the upper bound of the interval to
*>          be searched for eigenvalues. VL < VU.
*>          Not referenced if RANGE = 'A' or 'I'.
*> \endverbatim
*>
*> \param[in] IL
*> \verbatim
*>          IL is INTEGER
*>
*>          If RANGE='I', the index of the
*>          smallest eigenvalue to be returned.
*>          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*>          Not referenced if RANGE = 'A' or 'V'.
*> \endverbatim
*>
*> \param[in] IU
*> \verbatim
*>          IU is INTEGER
*>
*>          If RANGE='I', the index of the
*>          largest eigenvalue to be returned.
*>          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*>          Not referenced if RANGE = 'A' or 'V'.
*> \endverbatim
*>
*> \param[in] ABSTOL
*> \verbatim
*>          ABSTOL is REAL
*>          The absolute error tolerance for the eigenvalues.
*>          An approximate eigenvalue is accepted as converged
*>          when it is determined to lie in an interval [a,b]
*>          of width less than or equal to
*>
*>                  ABSTOL + EPS *   max( |a|,|b| ) ,
*>
*>          where EPS is the machine precision.  If ABSTOL is less than
*>          or equal to zero, then  EPS*|T|  will be used in its place,
*>          where |T| is the 1-norm of the tridiagonal matrix obtained
*>          by reducing C to tridiagonal form, where C is the symmetric
*>          matrix of the standard symmetric problem to which the
*>          generalized problem is transformed.
*>
*>          Eigenvalues will be computed most accurately when ABSTOL is
*>          set to twice the underflow threshold 2*SLAMCH('S'), not zero.
*>          If this routine returns with INFO>0, indicating that some
*>          eigenvectors did not converge, try setting ABSTOL to
*>          2*SLAMCH('S').
*> \endverbatim
*>
*> \param[out] M
*> \verbatim
*>          M is INTEGER
*>          The total number of eigenvalues found.  0 <= M <= N.
*>          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*> \endverbatim
*>
*> \param[out] W
*> \verbatim
*>          W is REAL array, dimension (N)
*>          The first M elements contain the selected
*>          eigenvalues in ascending order.
*> \endverbatim
*>
*> \param[out] Z
*> \verbatim
*>          Z is COMPLEX array, dimension (LDZ, max(1,M))
*>          If JOBZ = 'N', then Z is not referenced.
*>          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*>          contain the orthonormal eigenvectors of the matrix A
*>          corresponding to the selected eigenvalues, with the i-th
*>          column of Z holding the eigenvector associated with W(i).
*>          The eigenvectors are normalized as follows:
*>          if ITYPE = 1 or 2, Z**T*B*Z = I;
*>          if ITYPE = 3, Z**T*inv(B)*Z = I.
*>
*>          If an eigenvector fails to converge, then that column of Z
*>          contains the latest approximation to the eigenvector, and the
*>          index of the eigenvector is returned in IFAIL.
*>          Note: the user must ensure that at least max(1,M) columns are
*>          supplied in the array Z; if RANGE = 'V', the exact value of M
*>          is not known in advance and an upper bound must be used.
*> \endverbatim
*>
*> \param[in] LDZ
*> \verbatim
*>          LDZ is INTEGER
*>          The leading dimension of the array Z.  LDZ >= 1, and if
*>          JOBZ = 'V', LDZ >= max(1,N).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The length of the array WORK.  LWORK >= max(1,2*N).
*>          For optimal efficiency, LWORK >= (NB+1)*N,
*>          where NB is the blocksize for AB_CHETRD returned by AB_ILAENV.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by AB_XERBLA.
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is REAL array, dimension (7*N)
*> \endverbatim
*>
*> \param[out] IWORK
*> \verbatim
*>          IWORK is INTEGER array, dimension (5*N)
*> \endverbatim
*>
*> \param[out] IFAIL
*> \verbatim
*>          IFAIL is INTEGER array, dimension (N)
*>          If JOBZ = 'V', then if INFO = 0, the first M elements of
*>          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*>          indices of the eigenvectors that failed to converge.
*>          If JOBZ = 'N', then IFAIL is not referenced.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*>          > 0:  AB_CPOTRF or AB_CHEEVX returned an error code:
*>             <= N:  if INFO = i, AB_CHEEVX failed to converge;
*>                    i eigenvectors failed to converge.  Their indices
*>                    are stored in array IFAIL.
*>             > N:   if INFO = N + i, for 1 <= i <= N, then the leading
*>                    minor of order i of B is not positive definite.
*>                    The factorization of B could not be completed and
*>                    no eigenvalues or eigenvectors were computed.
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
*> \date June 2016
*
*> \ingroup complexHEeigen
*
*> \par Contributors:
*  ==================
*>
*>     Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA
*
*  =====================================================================
      SUBROUTINE AB_CHEGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,
     $                   VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,
     $                   LWORK, RWORK, IWORK, IFAIL, INFO )
*
*  -- LAPACK driver routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, ITYPE, IU, LDA, LDB, LDZ, LWORK, M, N
      REAL               ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      REAL               RWORK( * ), W( * )
      COMPLEX            A( LDA, * ), B( LDB, * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX            CONE
      PARAMETER          ( CONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LQUERY, UPPER, VALEIG, WANTZ
      CHARACTER          TRANS
      INTEGER            LWKOPT, NB
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ILAENV
      EXTERNAL           AB_ILAENV, AB_LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHEEVX, AB_CHEGST, AB_CPOTRF, AB_CTRMM, AB_C
     $TRSM, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = AB_LSAME( JOBZ, 'V' )
      UPPER = AB_LSAME( UPLO, 'U' )
      ALLEIG = AB_LSAME( RANGE, 'A' )
      VALEIG = AB_LSAME( RANGE, 'V' )
      INDEIG = AB_LSAME( RANGE, 'I' )
      LQUERY = ( LWORK.EQ.-1 )
*
      INFO = 0
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTZ .OR. AB_LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -3
      ELSE IF( .NOT.( UPPER .OR. AB_LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -11
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -12
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -13
            END IF
         END IF
      END IF
      IF (INFO.EQ.0) THEN
         IF (LDZ.LT.1 .OR. (WANTZ .AND. LDZ.LT.N)) THEN
            INFO = -18
         END IF
      END IF
*
      IF( INFO.EQ.0 ) THEN
         NB = AB_ILAENV( 1, 'AB_CHETRD', UPLO, N, -1, -1, -1 )
         LWKOPT = MAX( 1, ( NB + 1 )*N )
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.MAX( 1, 2*N ) .AND. .NOT.LQUERY ) THEN
            INFO = -20
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_CHEGVX', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 ) THEN
         RETURN
      END IF
*
*     Form a Cholesky factorization of B.
*
      CALL AB_CPOTRF( UPLO, N, B, LDB, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem and solve.
*
      CALL AB_CHEGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
      CALL AB_CHEEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, ABST
     $OL,
     $             M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL,
     $             INFO )
*
      IF( WANTZ ) THEN
*
*        Backtransform eigenvectors to the original problem.
*
         IF( INFO.GT.0 )
     $      M = INFO - 1
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
*
*           For A*x=(lambda)*B*x and A*B*x=(lambda)*x;
*           backtransform eigenvectors: x = inv(L)**H*y or inv(U)*y
*
            IF( UPPER ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'C'
            END IF
*
            CALL AB_CTRSM( 'Left', UPLO, TRANS, 'Non-unit', N, M, CONE, 
     $B,
     $                  LDB, Z, LDZ )
*
         ELSE IF( ITYPE.EQ.3 ) THEN
*
*           For B*A*x=(lambda)*x;
*           backtransform eigenvectors: x = L*y or U**H*y
*
            IF( UPPER ) THEN
               TRANS = 'C'
            ELSE
               TRANS = 'N'
            END IF
*
            CALL AB_CTRMM( 'Left', UPLO, TRANS, 'Non-unit', N, M, CONE, 
     $B,
     $                  LDB, Z, LDZ )
         END IF
      END IF
*
*     Set WORK(1) to optimal complex workspace size.
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of AB_CHEGVX
*
      END
