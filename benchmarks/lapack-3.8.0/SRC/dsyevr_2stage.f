*> \brief <b> AB_AB_AB_DSYEVR_2STAGE computes the eigenvalues and, optionally, the left and/or right eigenvectors for SY matrices</b>
*
*  @precisions fortran d -> s
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_AB_AB_DSYEVR_2STAGE + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_AB_AB_DSYEVR_2STAGE.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_AB_AB_DSYEVR_2STAGE.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_AB_AB_DSYEVR_2STAGE.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_AB_DSYEVR_2STAGE( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU,
*                          IL, IU, ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK,
*                          LWORK, IWORK, LIWORK, INFO )
*
*       IMPLICIT NONE
*
*       .. Scalar Arguments ..
*       CHARACTER          JOBZ, RANGE, UPLO
*       INTEGER            IL, INFO, IU, LDA, LDZ, LIWORK, LWORK, M, N
*       DOUBLE PRECISION   ABSTOL, VL, VU
*       ..
*       .. Array Arguments ..
*       INTEGER            ISUPPZ( * ), IWORK( * )
*       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * ), Z( LDZ, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_AB_AB_DSYEVR_2STAGE computes selected eigenvalues and, optionally, eigenvectors
*> of a real symmetric matrix A using the 2stage technique for
*> the reduction to tridiagonal.  Eigenvalues and eigenvectors can be
*> selected by specifying either a range of values or a range of
*> indices for the desired eigenvalues.
*>
*> AB_AB_AB_DSYEVR_2STAGE first reduces the matrix A to tridiagonal form T with a call
*> to AB_DSYTRD.  Then, whenever possible, AB_AB_AB_DSYEVR_2STAGE calls AB_DSTEMR to compute
*> the eigenspectrum using Relatively Robust Representations.  AB_DSTEMR
*> computes eigenvalues by the dqds algorithm, while orthogonal
*> eigenvectors are computed from various "good" L D L^T representations
*> (also known as Relatively Robust Representations). Gram-Schmidt
*> orthogonalization is avoided as far as possible. More specifically,
*> the various steps of the algorithm are as follows.
*>
*> For each unreduced block (submatrix) of T,
*>    (a) Compute T - sigma I  = L D L^T, so that L and D
*>        define all the wanted eigenvalues to high relative accuracy.
*>        This means that small relative changes in the entries of D and L
*>        cause only small relative changes in the eigenvalues and
*>        eigenvectors. The standard (unfactored) representation of the
*>        tridiagonal matrix T does not have this property in general.
*>    (b) Compute the eigenvalues to suitable accuracy.
*>        If the eigenvectors are desired, the algorithm attains full
*>        accuracy of the computed eigenvalues only right before
*>        the corresponding vectors have to be computed, see steps c) and d).
*>    (c) For each cluster of close eigenvalues, select a new
*>        shift close to the cluster, find a new factorization, and refine
*>        the shifted eigenvalues to suitable accuracy.
*>    (d) For each eigenvalue with a large enough relative separation compute
*>        the corresponding eigenvector by forming a rank revealing twisted
*>        factorization. Go back to (c) for any clusters that remain.
*>
*> The desired accuracy of the output can be specified by the input
*> parameter ABSTOL.
*>
*> For more details, see AB_DSTEMR's documentation and:
*> - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
*>   to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
*>   Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
*> - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
*>   Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
*>   2004.  Also LAPACK Working Note 154.
*> - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
*>   tridiagonal eigenvalue/eigenvector problem",
*>   Computer Science Division Technical Report No. UCB/CSD-97-971,
*>   UC Berkeley, May 1997.
*>
*>
*> Note 1 : AB_AB_AB_DSYEVR_2STAGE calls AB_DSTEMR when the full spectrum is requested
*> on machines which conform to the ieee-754 floating point standard.
*> AB_AB_AB_DSYEVR_2STAGE calls AB_DSTEBZ and AB_SSTEIN on non-ieee machines and
*> when partial spectrum requests are made.
*>
*> Normal execution of AB_DSTEMR may create NaNs and infinities and
*> hence may abort due to a floating point exception in environments
*> which do not handle NaNs and infinities in the ieee standard default
*> manner.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] JOBZ
*> \verbatim
*>          JOBZ is CHARACTER*1
*>          = 'N':  Compute eigenvalues only;
*>          = 'V':  Compute eigenvalues and eigenvectors.
*>                  Not available in this release.
*> \endverbatim
*>
*> \param[in] RANGE
*> \verbatim
*>          RANGE is CHARACTER*1
*>          = 'A': all eigenvalues will be found.
*>          = 'V': all eigenvalues in the half-open interval (VL,VU]
*>                 will be found.
*>          = 'I': the IL-th through IU-th eigenvalues will be found.
*>          For RANGE = 'V' or 'I' and IU - IL < N - 1, AB_DSTEBZ and
*>          AB_DSTEIN are called
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangle of A is stored;
*>          = 'L':  Lower triangle of A is stored.
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
*>          A is DOUBLE PRECISION array, dimension (LDA, N)
*>          On entry, the symmetric matrix A.  If UPLO = 'U', the
*>          leading N-by-N upper triangular part of A contains the
*>          upper triangular part of the matrix A.  If UPLO = 'L',
*>          the leading N-by-N lower triangular part of A contains
*>          the lower triangular part of the matrix A.
*>          On exit, the lower triangle (if UPLO='L') or the upper
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
*> \param[in] VL
*> \verbatim
*>          VL is DOUBLE PRECISION
*>          If RANGE='V', the lower bound of the interval to
*>          be searched for eigenvalues. VL < VU.
*>          Not referenced if RANGE = 'A' or 'I'.
*> \endverbatim
*>
*> \param[in] VU
*> \verbatim
*>          VU is DOUBLE PRECISION
*>          If RANGE='V', the upper bound of the interval to
*>          be searched for eigenvalues. VL < VU.
*>          Not referenced if RANGE = 'A' or 'I'.
*> \endverbatim
*>
*> \param[in] IL
*> \verbatim
*>          IL is INTEGER
*>          If RANGE='I', the index of the
*>          smallest eigenvalue to be returned.
*>          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*>          Not referenced if RANGE = 'A' or 'V'.
*> \endverbatim
*>
*> \param[in] IU
*> \verbatim
*>          IU is INTEGER
*>          If RANGE='I', the index of the
*>          largest eigenvalue to be returned.
*>          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*>          Not referenced if RANGE = 'A' or 'V'.
*> \endverbatim
*>
*> \param[in] ABSTOL
*> \verbatim
*>          ABSTOL is DOUBLE PRECISION
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
*>          by reducing A to tridiagonal form.
*>
*>          See "Computing Small Singular Values of Bidiagonal Matrices
*>          with Guaranteed High Relative Accuracy," by Demmel and
*>          Kahan, LAPACK Working Note #3.
*>
*>          If high relative accuracy is important, set ABSTOL to
*>          AB_DLAMCH( 'Safe minimum' ).  Doing so will guarantee that
*>          eigenvalues are computed to high relative accuracy when
*>          possible in future releases.  The current code does not
*>          make any guarantees about high relative accuracy, but
*>          future releases will. See J. Barlow and J. Demmel,
*>          "Computing Accurate Eigensystems of Scaled Diagonally
*>          Dominant Matrices", LAPACK Working Note #7, for a discussion
*>          of which matrices define their eigenvalues to high relative
*>          accuracy.
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
*>          W is DOUBLE PRECISION array, dimension (N)
*>          The first M elements contain the selected eigenvalues in
*>          ascending order.
*> \endverbatim
*>
*> \param[out] Z
*> \verbatim
*>          Z is DOUBLE PRECISION array, dimension (LDZ, max(1,M))
*>          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*>          contain the orthonormal eigenvectors of the matrix A
*>          corresponding to the selected eigenvalues, with the i-th
*>          column of Z holding the eigenvector associated with W(i).
*>          If JOBZ = 'N', then Z is not referenced.
*>          Note: the user must ensure that at least max(1,M) columns are
*>          supplied in the array Z; if RANGE = 'V', the exact value of M
*>          is not known in advance and an upper bound must be used.
*>          Supplying N columns is always safe.
*> \endverbatim
*>
*> \param[in] LDZ
*> \verbatim
*>          LDZ is INTEGER
*>          The leading dimension of the array Z.  LDZ >= 1, and if
*>          JOBZ = 'V', LDZ >= max(1,N).
*> \endverbatim
*>
*> \param[out] ISUPPZ
*> \verbatim
*>          ISUPPZ is INTEGER array, dimension ( 2*max(1,M) )
*>          The support of the eigenvectors in Z, i.e., the indices
*>          indicating the nonzero elements in Z. The i-th eigenvector
*>          is nonzero only in elements ISUPPZ( 2*i-1 ) through
*>          ISUPPZ( 2*i ). This is an output of AB_DSTEMR (tridiagonal
*>          matrix). The support of the eigenvectors of A is typically 
*>          1:N because of the orthogonal transformations applied by AB_DORMTR.
*>          Implemented only for RANGE = 'A' or 'I' and IU - IL = N - 1
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.  
*>          If JOBZ = 'N' and N > 1, LWORK must be queried.
*>                                   LWORK = MAX(1, 26*N, dimension) where
*>                                   dimension = max(stage1,stage2) + (KD+1)*N + 5*N
*>                                             = N*KD + N*max(KD+1,FACTOPTNB) 
*>                                               + max(2*KD*KD, KD*NTHREADS) 
*>                                               + (KD+1)*N + 5*N
*>                                   where KD is the blocking size of the reduction,
*>                                   FACTOPTNB is the blocking used by the QR or LQ
*>                                   algorithm, usually FACTOPTNB=128 is a good choice
*>                                   NTHREADS is the number of threads used when
*>                                   openMP compilation is enabled, otherwise =1.
*>          If JOBZ = 'V' and N > 1, LWORK must be queried. Not yet available
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by AB_XERBLA.
*> \endverbatim
*>
*> \param[out] IWORK
*> \verbatim
*>          IWORK is INTEGER array, dimension (MAX(1,LIWORK))
*>          On exit, if INFO = 0, IWORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LIWORK
*> \verbatim
*>          LIWORK is INTEGER
*>          The dimension of the array IWORK.  LIWORK >= max(1,10*N).
*>
*>          If LIWORK = -1, then a workspace query is assumed; the
*>          routine only calculates the optimal size of the IWORK array,
*>          returns this value as the first entry of the IWORK array, and
*>          no error message related to LIWORK is issued by AB_XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*>          > 0:  Internal error
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
*> \ingroup doubleSYeigen
*
*> \par Contributors:
*  ==================
*>
*>     Inderjit Dhillon, IBM Almaden, USA \n
*>     Osni Marques, LBNL/NERSC, USA \n
*>     Ken Stanley, Computer Science Division, University of
*>       California at Berkeley, USA \n
*>     Jason Riedy, Computer Science Division, University of
*>       California at Berkeley, USA \n
*>
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  All details about the 2stage techniques are available in:
*>
*>  Azzam Haidar, Hatem Ltaief, and Jack Dongarra.
*>  Parallel reduction to condensed forms for symmetric eigenvalue problems
*>  using aggregated fine-grained and memory-aware kernels. In Proceedings
*>  of 2011 International Conference for High Performance Computing,
*>  Networking, Storage and Analysis (SC '11), New York, NY, USA,
*>  Article 8 , 11 pages.
*>  http://doi.acm.org/10.1145/2063384.2063394
*>
*>  A. Haidar, J. Kurzak, P. Luszczek, 2013.
*>  An improved parallel singular value algorithm and its implementation 
*>  for multicore hardware, In Proceedings of 2013 International Conference
*>  for High Performance Computing, Networking, Storage and Analysis (SC '13).
*>  Denver, Colorado, USA, 2013.
*>  Article 90, 12 pages.
*>  http://doi.acm.org/10.1145/2503210.2503292
*>
*>  A. Haidar, R. Solca, S. Tomov, T. Schulthess and J. Dongarra.
*>  A novel hybrid CPU-GPU generalized eigensolver for electronic structure 
*>  calculations based on fine-grained memory aware tasks.
*>  International Journal of High Performance Computing Applications.
*>  Volume 28 Issue 2, Pages 196-209, May 2014.
*>  http://hpc.sagepub.com/content/28/2/196 
*>
*> \endverbatim
*
*  =====================================================================
      SUBROUTINE AB_AB_AB_DSYEVR_2STAGE( JOBZ, RANGE, UPLO, N, A, LDA, V
     $L, VU,
     $                   IL, IU, ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK,
     $                   LWORK, IWORK, LIWORK, INFO )
*
      IMPLICIT NONE
*
*  -- LAPACK driver routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, LDA, LDZ, LIWORK, LWORK, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LOWER, LQUERY, VALEIG, WANTZ,
     $                   TRYRAC
      CHARACTER          ORDER
      INTEGER            I, IEEEOK, IINFO, IMAX, INDD, INDDD, INDE,
     $                   INDEE, INDIBL, INDIFL, INDISP, INDIWO, INDTAU,
     $                   INDWK, INDWKN, ISCALE, J, JJ, LIWMIN,
     $                   LLWORK, LLWRKN, LWMIN, NSPLIT,
     $                   LHTRD, LWTRD, KD, IB, INDHOUS
      DOUBLE PRECISION   ABSTLL, ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN,
     $                   SIGMA, SMLNUM, TMP1, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ILAENV, AB_AB_ILAENV2STAGE
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANSY
      EXTERNAL           AB_LSAME, AB_DLAMCH, AB_DLANSY, AB_ILAENV, AB_A
     $B_ILAENV2STAGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DCOPY, AB_DORMTR, AB_DSCAL, AB_DSTEBZ, AB_DS
     $TEMR, AB_DSTEIN,
     $                   AB_DSTERF, AB_DSWAP, AB_AB_DSYTRD_2STAGE, AB_XE
     $RBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      IEEEOK = AB_ILAENV( 10, 'AB_AB_DSYEVR', 'N', 1, 2, 3, 4 )
*
      LOWER = AB_LSAME( UPLO, 'L' )
      WANTZ = AB_LSAME( JOBZ, 'V' )
      ALLEIG = AB_LSAME( RANGE, 'A' )
      VALEIG = AB_LSAME( RANGE, 'V' )
      INDEIG = AB_LSAME( RANGE, 'I' )
*
      LQUERY = ( ( LWORK.EQ.-1 ) .OR. ( LIWORK.EQ.-1 ) )
*
      KD     = AB_AB_ILAENV2STAGE( 1, 'AB_AB_DSYTRD_2STAGE', JOBZ, N, -1
     $, -1, -1 )
      IB     = AB_AB_ILAENV2STAGE( 2, 'AB_AB_DSYTRD_2STAGE', JOBZ, N, KD
     $, -1, -1 )
      LHTRD  = AB_AB_ILAENV2STAGE( 3, 'AB_AB_DSYTRD_2STAGE', JOBZ, N, KD
     $, IB, -1 )
      LWTRD  = AB_AB_ILAENV2STAGE( 4, 'AB_AB_DSYTRD_2STAGE', JOBZ, N, KD
     $, IB, -1 )
      LWMIN  = MAX( 26*N, 5*N + LHTRD + LWTRD )
      LIWMIN = MAX( 1, 10*N )
*
      INFO = 0
      IF( .NOT.( AB_LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LOWER .OR. AB_LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -8
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -9
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -10
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
            INFO = -15
         ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -18
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -20
         END IF
      END IF
*
      IF( INFO.EQ.0 ) THEN
*         NB = AB_ILAENV( 1, 'AB_DSYTRD', UPLO, N, -1, -1, -1 )
*         NB = MAX( NB, AB_ILAENV( 1, 'AB_DORMTR', UPLO, N, -1, -1, -1 ) )
*         LWKOPT = MAX( ( NB+1 )*N, LWMIN )
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_AB_AB_DSYEVR_2STAGE', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( N.EQ.1 ) THEN
         WORK( 1 ) = 7
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = A( 1, 1 )
         ELSE
            IF( VL.LT.A( 1, 1 ) .AND. VU.GE.A( 1, 1 ) ) THEN
               M = 1
               W( 1 ) = A( 1, 1 )
            END IF
         END IF
         IF( WANTZ ) THEN
            Z( 1, 1 ) = ONE
            ISUPPZ( 1 ) = 1
            ISUPPZ( 2 ) = 1
         END IF
         RETURN
      END IF
*
*     Get machine constants.
*
      SAFMIN = AB_DLAMCH( 'Safe minimum' )
      EPS    = AB_DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN   = SQRT( SMLNUM )
      RMAX   = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      ABSTLL = ABSTOL
      IF (VALEIG) THEN
         VLL = VL
         VUU = VU
      END IF
      ANRM = AB_DLANSY( 'M', UPLO, N, A, LDA, WORK )
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            DO 10 J = 1, N
               CALL AB_DSCAL( N-J+1, SIGMA, A( J, J ), 1 )
   10       CONTINUE
         ELSE
            DO 20 J = 1, N
               CALL AB_DSCAL( J, SIGMA, A( 1, J ), 1 )
   20       CONTINUE
         END IF
         IF( ABSTOL.GT.0 )
     $      ABSTLL = ABSTOL*SIGMA
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF

*     Initialize indices into workspaces.  Note: The IWORK indices are
*     used only if AB_DSTERF or AB_DSTEMR fail.

*     WORK(INDTAU:INDTAU+N-1) stores the scalar factors of the
*     elementary reflectors used in AB_DSYTRD.
      INDTAU = 1
*     WORK(INDD:INDD+N-1) stores the tridiagonal's diagonal entries.
      INDD = INDTAU + N
*     WORK(INDE:INDE+N-1) stores the off-diagonal entries of the
*     tridiagonal matrix from AB_DSYTRD.
      INDE = INDD + N
*     WORK(INDDD:INDDD+N-1) is a copy of the diagonal entries over
*     -written by AB_DSTEMR (the AB_DSTERF path copies the diagonal to W).
      INDDD = INDE + N
*     WORK(INDEE:INDEE+N-1) is a copy of the off-diagonal entries over
*     -written while computing the eigenvalues in AB_DSTERF and AB_DSTEMR.
      INDEE = INDDD + N
*     INDHOUS is the starting offset HousehoAB_LDEr storage of stage 2
      INDHOUS = INDEE + N
*     INDWK is the starting offset of the left-over workspace, and
*     LLWORK is the remaining workspace size.
      INDWK  = INDHOUS + LHTRD
      LLWORK = LWORK - INDWK + 1


*     IWORK(INDIBL:INDIBL+M-1) corresponds to IBLOCK in AB_DSTEBZ and
*     stores the block indices of each of the M<=N eigenvalues.
      INDIBL = 1
*     IWORK(INDISP:INDISP+NSPLIT-1) corresponds to ISPLIT in AB_DSTEBZ and
*     stores the starting and finishing indices of each block.
      INDISP = INDIBL + N
*     IWORK(INDIFL:INDIFL+N-1) stores the indices of eigenvectors
*     that corresponding to eigenvectors that fail to converge in
*     AB_DSTEIN.  This information is discarded; if any fail, the driver
*     returns INFO > 0.
      INDIFL = INDISP + N
*     INDIWO is the offset of the remaining integer workspace.
      INDIWO = INDIFL + N

*
*     Call AB_AB_DSYTRD_2STAGE to reduce symmetric matrix to tridiagonal form.
*
*
      CALL AB_AB_DSYTRD_2STAGE( JOBZ, UPLO, N, A, LDA, WORK( INDD ), 
     $                    WORK( INDE ), WORK( INDTAU ), WORK( INDHOUS ),
     $                    LHTRD, WORK( INDWK ), LLWORK, IINFO )
*
*     If all eigenvalues are desired
*     then call AB_DSTERF or AB_DSTEMR and AB_DORMTR.
*
      IF( ( ALLEIG .OR. ( INDEIG .AND. IL.EQ.1 .AND. IU.EQ.N ) ) .AND.
     $    IEEEOK.EQ.1 ) THEN
         IF( .NOT.WANTZ ) THEN
            CALL AB_DCOPY( N, WORK( INDD ), 1, W, 1 )
            CALL AB_DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL AB_DSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL AB_DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL AB_DCOPY( N, WORK( INDD ), 1, WORK( INDDD ), 1 )
*
            IF (ABSTOL .LE. TWO*N*EPS) THEN
               TRYRAC = .TRUE.
            ELSE
               TRYRAC = .FALSE.
            END IF
            CALL AB_DSTEMR( JOBZ, 'A', N, WORK( INDDD ), WORK( INDEE ),
     $                   VL, VU, IL, IU, M, W, Z, LDZ, N, ISUPPZ,
     $                   TRYRAC, WORK( INDWK ), LWORK, IWORK, LIWORK,
     $                   INFO )
*
*
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by AB_DSTEMR.
*
            IF( WANTZ .AND. INFO.EQ.0 ) THEN
               INDWKN = INDE
               LLWRKN = LWORK - INDWKN + 1
               CALL AB_DORMTR( 'L', UPLO, 'N', N, M, A, LDA,
     $                      WORK( INDTAU ), Z, LDZ, WORK( INDWKN ),
     $                      LLWRKN, IINFO )
            END IF
         END IF
*
*
         IF( INFO.EQ.0 ) THEN
*           Everything worked.  Skip AB_DSTEBZ/AB_DSTEIN.  IWORK(:) are
*           undefined.
            M = N
            GO TO 30
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call AB_DSTEBZ and, if eigenvectors are desired, AB_DSTEIN.
*     Also call AB_DSTEBZ and AB_DSTEIN if AB_DSTEMR fails.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF

      CALL AB_DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
     $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
     $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWK ),
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL AB_DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
     $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
     $                WORK( INDWK ), IWORK( INDIWO ), IWORK( INDIFL ),
     $                INFO )
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by AB_DSTEIN.
*
         INDWKN = INDE
         LLWRKN = LWORK - INDWKN + 1
         CALL AB_DORMTR( 'L', UPLO, 'N', N, M, A, LDA, WORK( INDTAU ), Z
     $,
     $                LDZ, WORK( INDWKN ), LLWRKN, IINFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
*  Jump here if AB_DSTEMR/AB_DSTEIN succeeded.
   30 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL AB_DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.  Note: We do not sort the IFAIL portion of IWORK.
*     It may not be initialized (if AB_DSTEMR/AB_DSTEIN succeeded), and we do
*     not return this detailed information to the user.
*
      IF( WANTZ ) THEN
         DO 50 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 40 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   40       CONTINUE
*
            IF( I.NE.0 ) THEN
               W( I ) = W( J )
               W( J ) = TMP1
               CALL AB_DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
            END IF
   50    CONTINUE
      END IF
*
*     Set WORK(1) to optimal workspace size.
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of AB_AB_AB_DSYEVR_2STAGE
*
      END
