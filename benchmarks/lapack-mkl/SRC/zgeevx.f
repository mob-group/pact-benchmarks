*> \brief <b> AB_ZGEEVX computes the eigenvalues and, optionally, the left and/or right eigenvectors for GE matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZGEEVX + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZGEEVx.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZGEEVx.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZGEEVx.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, W, VL,
*                          LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM, RCONDE,
*                          RCONDV, WORK, LWORK, RWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          BALANC, JOBVL, JOBVR, SENSE
*       INTEGER            IHI, ILO, INFO, LDA, LDVL, LDVR, LWORK, N
*       DOUBLE PRECISION   ABNRM
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   RCONDE( * ), RCONDV( * ), RWORK( * ),
*      $                   SCALE( * )
*       COMPLEX*16         A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
*      $                   W( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZGEEVX computes for an N-by-N complex nonsymmetric matrix A, the
*> eigenvalues and, optionally, the left and/or right eigenvectors.
*>
*> Optionally also, it computes a balancing transformation to improve
*> the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*> SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
*> (RCONDE), and reciprocal condition numbers for the right
*> eigenvectors (RCONDV).
*>
*> The right eigenvector v(j) of A satisfies
*>                  A * v(j) = lambda(j) * v(j)
*> where lambda(j) is its eigenvalue.
*> The left eigenvector u(j) of A satisfies
*>               u(j)**H * A = lambda(j) * u(j)**H
*> where u(j)**H denotes the conjugate transpose of u(j).
*>
*> The computed eigenvectors are normalized to have Euclidean norm
*> equal to 1 and largest component real.
*>
*> Balancing a matrix means permuting the rows and columns to make it
*> more nearly upper triangular, and applying a diagonal similarity
*> transformation D * A * D**(-1), where D is a diagonal matrix, to
*> make its rows and columns closer in norm and the condition numbers
*> of its eigenvalues and eigenvectors smaller.  The computed
*> reciprocal condition numbers correspond to the balanced matrix.
*> Permuting rows and columns will not change the condition numbers
*> (in exact arithmetic) but diagonal scaling will.  For further
*> explanation of balancing, see section 4.10.2 of the LAPACK
*> Users' Guide.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] BALANC
*> \verbatim
*>          BALANC is CHARACTER*1
*>          Indicates how the input matrix should be diagonally scaled
*>          and/or permuted to improve the conditioning of its
*>          eigenvalues.
*>          = 'N': Do not diagonally scale or permute;
*>          = 'P': Perform permutations to make the matrix more nearly
*>                 upper triangular. Do not diagonally scale;
*>          = 'S': Diagonally scale the matrix, ie. replace A by
*>                 D*A*D**(-1), where D is a diagonal matrix chosen
*>                 to make the rows and columns of A more equal in
*>                 norm. Do not permute;
*>          = 'B': Both diagonally scale and permute A.
*>
*>          Computed reciprocal condition numbers will be for the matrix
*>          after balancing and/or permuting. Permuting does not change
*>          condition numbers (in exact arithmetic), but balancing does.
*> \endverbatim
*>
*> \param[in] JOBVL
*> \verbatim
*>          JOBVL is CHARACTER*1
*>          = 'N': left eigenvectors of A are not computed;
*>          = 'V': left eigenvectors of A are computed.
*>          If SENSE = 'E' or 'B', JOBVL must = 'V'.
*> \endverbatim
*>
*> \param[in] JOBVR
*> \verbatim
*>          JOBVR is CHARACTER*1
*>          = 'N': right eigenvectors of A are not computed;
*>          = 'V': right eigenvectors of A are computed.
*>          If SENSE = 'E' or 'B', JOBVR must = 'V'.
*> \endverbatim
*>
*> \param[in] SENSE
*> \verbatim
*>          SENSE is CHARACTER*1
*>          Determines which reciprocal condition numbers are computed.
*>          = 'N': None are computed;
*>          = 'E': Computed for eigenvalues only;
*>          = 'V': Computed for right eigenvectors only;
*>          = 'B': Computed for eigenvalues and right eigenvectors.
*>
*>          If SENSE = 'E' or 'B', both left and right eigenvectors
*>          must also be computed (JOBVL = 'V' and JOBVR = 'V').
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A. N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is COMPLEX*16 array, dimension (LDA,N)
*>          On entry, the N-by-N matrix A.
*>          On exit, A has been overwritten.  If JOBVL = 'V' or
*>          JOBVR = 'V', A contains the Schur form of the balanced
*>          version of the matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[out] W
*> \verbatim
*>          W is COMPLEX*16 array, dimension (N)
*>          W contains the computed eigenvalues.
*> \endverbatim
*>
*> \param[out] VL
*> \verbatim
*>          VL is COMPLEX*16 array, dimension (LDVL,N)
*>          If JOBVL = 'V', the left eigenvectors u(j) are stored one
*>          after another in the columns of VL, in the same order
*>          as their eigenvalues.
*>          If JOBVL = 'N', VL is not referenced.
*>          u(j) = VL(:,j), the j-th column of VL.
*> \endverbatim
*>
*> \param[in] LDVL
*> \verbatim
*>          LDVL is INTEGER
*>          The leading dimension of the array VL.  LDVL >= 1; if
*>          JOBVL = 'V', LDVL >= N.
*> \endverbatim
*>
*> \param[out] VR
*> \verbatim
*>          VR is COMPLEX*16 array, dimension (LDVR,N)
*>          If JOBVR = 'V', the right eigenvectors v(j) are stored one
*>          after another in the columns of VR, in the same order
*>          as their eigenvalues.
*>          If JOBVR = 'N', VR is not referenced.
*>          v(j) = VR(:,j), the j-th column of VR.
*> \endverbatim
*>
*> \param[in] LDVR
*> \verbatim
*>          LDVR is INTEGER
*>          The leading dimension of the array VR.  LDVR >= 1; if
*>          JOBVR = 'V', LDVR >= N.
*> \endverbatim
*>
*> \param[out] ILO
*> \verbatim
*>          ILO is INTEGER
*> \endverbatim
*>
*> \param[out] IHI
*> \verbatim
*>          IHI is INTEGER
*>          ILO and IHI are integer values determined when A was
*>          balanced.  The balanced A(i,j) = 0 if I > J and
*>          J = 1,...,ILO-1 or I = IHI+1,...,N.
*> \endverbatim
*>
*> \param[out] SCALE
*> \verbatim
*>          SCALE is DOUBLE PRECISION array, dimension (N)
*>          Details of the permutations and scaling factors applied
*>          when balancing A.  If P(j) is the index of the row and column
*>          interchanged with row and column j, and D(j) is the scaling
*>          factor applied to row and column j, then
*>          SCALE(J) = P(J),    for J = 1,...,ILO-1
*>                   = D(J),    for J = ILO,...,IHI
*>                   = P(J)     for J = IHI+1,...,N.
*>          The order in which the interchanges are made is N to IHI+1,
*>          then 1 to ILO-1.
*> \endverbatim
*>
*> \param[out] ABNRM
*> \verbatim
*>          ABNRM is DOUBLE PRECISION
*>          The one-norm of the balanced matrix (the maximum
*>          of the sum of absolute values of elements of any column).
*> \endverbatim
*>
*> \param[out] RCONDE
*> \verbatim
*>          RCONDE is DOUBLE PRECISION array, dimension (N)
*>          RCONDE(j) is the reciprocal condition number of the j-th
*>          eigenvalue.
*> \endverbatim
*>
*> \param[out] RCONDV
*> \verbatim
*>          RCONDV is DOUBLE PRECISION array, dimension (N)
*>          RCONDV(j) is the reciprocal condition number of the j-th
*>          right eigenvector.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.  If SENSE = 'N' or 'E',
*>          LWORK >= max(1,2*N), and if SENSE = 'V' or 'B',
*>          LWORK >= N*N+2*N.
*>          For good performance, LWORK must generally be larger.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by AB_XERBLA.
*> \endverbatim
*>
*> \param[out] RWORK
*> \verbatim
*>          RWORK is DOUBLE PRECISION array, dimension (2*N)
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value.
*>          > 0:  if INFO = i, the QR algorithm failed to compute all the
*>                eigenvalues, and no eigenvectors or condition numbers
*>                have been computed; elements 1:ILO-1 and i+1:N of W
*>                contain eigenvalues which have converged.
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
*  @precisions fortran z -> c
*
*> \ingroup complex16GEeigen
*
*  =====================================================================
      SUBROUTINE AB_ZGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, W, V
     $L,
     $                   LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM, RCONDE,
     $                   RCONDV, WORK, LWORK, RWORK, INFO )
      implicit none
*
*  -- LAPACK driver routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*     .. Scalar Arguments ..
      CHARACTER          BALANC, JOBVL, JOBVR, SENSE
      INTEGER            IHI, ILO, INFO, LDA, LDVL, LDVR, LWORK, N
      DOUBLE PRECISION   ABNRM
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   RCONDE( * ), RCONDV( * ), RWORK( * ),
     $                   SCALE( * )
      COMPLEX*16         A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   W( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, SCALEA, WANTVL, WANTVR, WNTSNB, WNTSNE,
     $                   WNTSNN, WNTSNV
      CHARACTER          JOB, SIDE
      INTEGER            HSWORK, I, ICOND, IERR, ITAU, IWRK, K,
     $                   LWORK_TREVC, MAXWRK, MINWRK, NOUT
      DOUBLE PRECISION   ANRM, BIGNUM, AB_CSCALE, EPS, SCL, SMLNUM
      COMPLEX*16         TMP
*     ..
*     .. Local Arrays ..
      LOGICAL            SELECT( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DLABAD, AB_DLASCL, AB_XERBLA, AB_ZDSCAL, AB_
     $ZGEBAK, AB_ZGEBAL,
     $                   AB_ZGEHRD, AB_ZHSEQR, AB_ZLACPY, AB_ZLASCL, AB_
     $ZSCAL, AB_ZTREVC3,
     $                   AB_ZTRSNA, AB_ZUNGHR
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_IDAMAX, AB_ILAENV
      DOUBLE PRECISION   AB_DLAMCH, AB_DZNRM2, AB_ZLANGE
      EXTERNAL           AB_LSAME, AB_IDAMAX, AB_ILAENV, AB_DLAMCH, AB_DZNR
     $M2, AB_ZLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, CONJG, AIMAG, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      WANTVL = AB_LSAME( JOBVL, 'V' )
      WANTVR = AB_LSAME( JOBVR, 'V' )
      WNTSNN = AB_LSAME( SENSE, 'N' )
      WNTSNE = AB_LSAME( SENSE, 'E' )
      WNTSNV = AB_LSAME( SENSE, 'V' )
      WNTSNB = AB_LSAME( SENSE, 'B' )
      IF( .NOT.( AB_LSAME( BALANC, 'N' ) .OR. AB_LSAME( BALANC, 'S' ) .O
     $R.
     $    AB_LSAME( BALANC, 'P' ) .OR. AB_LSAME( BALANC, 'B' ) ) ) THEN
         INFO = -1
      ELSE IF( ( .NOT.WANTVL ) .AND. ( .NOT.AB_LSAME( JOBVL, 'N' ) ) ) T
     $HEN
         INFO = -2
      ELSE IF( ( .NOT.WANTVR ) .AND. ( .NOT.AB_LSAME( JOBVR, 'N' ) ) ) T
     $HEN
         INFO = -3
      ELSE IF( .NOT.( WNTSNN .OR. WNTSNE .OR. WNTSNB .OR. WNTSNV ) .OR.
     $         ( ( WNTSNE .OR. WNTSNB ) .AND. .NOT.( WANTVL .AND.
     $         WANTVR ) ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDVL.LT.1 .OR. ( WANTVL .AND. LDVL.LT.N ) ) THEN
         INFO = -10
      ELSE IF( LDVR.LT.1 .OR. ( WANTVR .AND. LDVR.LT.N ) ) THEN
         INFO = -12
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       CWorkspace refers to complex workspace, and RWorkspace to real
*       workspace. NB refers to the optimal block size for the
*       immediately following subroutine, as returned by AB_ILAENV.
*       HSWORK refers to the workspace preferred by AB_ZHSEQR, as
*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
*       the worst case.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            MAXWRK = N + N*AB_ILAENV( 1, 'AB_ZGEHRD', ' ', N, 1, N, 0 )
*
            IF( WANTVL ) THEN
               CALL AB_ZTREVC3( 'L', 'B', SELECT, N, A, LDA,
     $                       VL, LDVL, VR, LDVR,
     $                       N, NOUT, WORK, -1, RWORK, -1, IERR )
               LWORK_TREVC = INT( WORK(1) )
               MAXWRK = MAX( MAXWRK, LWORK_TREVC )
               CALL AB_ZHSEQR( 'S', 'V', N, 1, N, A, LDA, W, VL, LDVL,
     $                WORK, -1, INFO )
            ELSE IF( WANTVR ) THEN
               CALL AB_ZTREVC3( 'R', 'B', SELECT, N, A, LDA,
     $                       VL, LDVL, VR, LDVR,
     $                       N, NOUT, WORK, -1, RWORK, -1, IERR )
               LWORK_TREVC = INT( WORK(1) )
               MAXWRK = MAX( MAXWRK, LWORK_TREVC )
               CALL AB_ZHSEQR( 'S', 'V', N, 1, N, A, LDA, W, VR, LDVR,
     $                WORK, -1, INFO )
            ELSE
               IF( WNTSNN ) THEN
                  CALL AB_ZHSEQR( 'E', 'N', N, 1, N, A, LDA, W, VR, LDVR
     $,
     $                WORK, -1, INFO )
               ELSE
                  CALL AB_ZHSEQR( 'S', 'N', N, 1, N, A, LDA, W, VR, LDVR
     $,
     $                WORK, -1, INFO )
               END IF
            END IF
            HSWORK = INT( WORK(1) )
*
            IF( ( .NOT.WANTVL ) .AND. ( .NOT.WANTVR ) ) THEN
               MINWRK = 2*N
               IF( .NOT.( WNTSNN .OR. WNTSNE ) )
     $            MINWRK = MAX( MINWRK, N*N + 2*N )
               MAXWRK = MAX( MAXWRK, HSWORK )
               IF( .NOT.( WNTSNN .OR. WNTSNE ) )
     $            MAXWRK = MAX( MAXWRK, N*N + 2*N )
            ELSE
               MINWRK = 2*N
               IF( .NOT.( WNTSNN .OR. WNTSNE ) )
     $            MINWRK = MAX( MINWRK, N*N + 2*N )
               MAXWRK = MAX( MAXWRK, HSWORK )
               MAXWRK = MAX( MAXWRK, N + ( N - 1 )*AB_ILAENV( 1, 'AB_ZUN
     $GHR',
     $                       ' ', N, 1, N, -1 ) )
               IF( .NOT.( WNTSNN .OR. WNTSNE ) )
     $            MAXWRK = MAX( MAXWRK, N*N + 2*N )
               MAXWRK = MAX( MAXWRK, 2*N )
            END IF
            MAXWRK = MAX( MAXWRK, MINWRK )
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -20
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_ZGEEVX', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Get machine constants
*
      EPS = AB_DLAMCH( 'P' )
      SMLNUM = AB_DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL AB_DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SQRT( SMLNUM ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ICOND = 0
      ANRM = AB_ZLANGE( 'M', N, N, A, LDA, DUM )
      SCALEA = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         SCALEA = .TRUE.
         AB_CSCALE = SMLNUM
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         SCALEA = .TRUE.
         AB_CSCALE = BIGNUM
      END IF
      IF( SCALEA )
     $   CALL AB_ZLASCL( 'G', 0, 0, ANRM, AB_CSCALE, N, N, A, LDA, IERR 
     $)
*
*     Balance the matrix and compute ABNRM
*
      CALL AB_ZGEBAL( BALANC, N, A, LDA, ILO, IHI, SCALE, IERR )
      ABNRM = AB_ZLANGE( '1', N, N, A, LDA, DUM )
      IF( SCALEA ) THEN
         DUM( 1 ) = ABNRM
         CALL AB_DLASCL( 'G', 0, 0, AB_CSCALE, ANRM, 1, 1, DUM, 1, IERR 
     $)
         ABNRM = DUM( 1 )
      END IF
*
*     Reduce to upper Hessenberg form
*     (CWorkspace: need 2*N, prefer N+N*NB)
*     (RWorkspace: none)
*
      ITAU = 1
      IWRK = ITAU + N
      CALL AB_ZGEHRD( N, ILO, IHI, A, LDA, WORK( ITAU ), WORK( IWRK ),
     $             LWORK-IWRK+1, IERR )
*
      IF( WANTVL ) THEN
*
*        Want left eigenvectors
*        Copy Householder vectors to VL
*
         SIDE = 'L'
         CALL AB_ZLACPY( 'L', N, N, A, LDA, VL, LDVL )
*
*        Generate unitary matrix in VL
*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB)
*        (RWorkspace: none)
*
         CALL AB_ZUNGHR( N, ILO, IHI, VL, LDVL, WORK( ITAU ), WORK( IWRK
     $ ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VL
*        (CWorkspace: need 1, prefer HSWORK (see comments) )
*        (RWorkspace: none)
*
         IWRK = ITAU
         CALL AB_ZHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, W, VL, LDVL,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
         IF( WANTVR ) THEN
*
*           Want left and right eigenvectors
*           Copy Schur vectors to VR
*
            SIDE = 'B'
            CALL AB_ZLACPY( 'F', N, N, VL, LDVL, VR, LDVR )
         END IF
*
      ELSE IF( WANTVR ) THEN
*
*        Want right eigenvectors
*        Copy Householder vectors to VR
*
         SIDE = 'R'
         CALL AB_ZLACPY( 'L', N, N, A, LDA, VR, LDVR )
*
*        Generate unitary matrix in VR
*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB)
*        (RWorkspace: none)
*
         CALL AB_ZUNGHR( N, ILO, IHI, VR, LDVR, WORK( ITAU ), WORK( IWRK
     $ ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VR
*        (CWorkspace: need 1, prefer HSWORK (see comments) )
*        (RWorkspace: none)
*
         IWRK = ITAU
         CALL AB_ZHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, W, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
      ELSE
*
*        Compute eigenvalues only
*        If condition numbers desired, compute Schur form
*
         IF( WNTSNN ) THEN
            JOB = 'E'
         ELSE
            JOB = 'S'
         END IF
*
*        (CWorkspace: need 1, prefer HSWORK (see comments) )
*        (RWorkspace: none)
*
         IWRK = ITAU
         CALL AB_ZHSEQR( JOB, 'N', N, ILO, IHI, A, LDA, W, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
      END IF
*
*     If INFO .NE. 0 from AB_ZHSEQR, then quit
*
      IF( INFO.NE.0 )
     $   GO TO 50
*
      IF( WANTVL .OR. WANTVR ) THEN
*
*        Compute left and/or right eigenvectors
*        (CWorkspace: need 2*N, prefer N + 2*N*NB)
*        (RWorkspace: need N)
*
         CALL AB_ZTREVC3( SIDE, 'B', SELECT, N, A, LDA, VL, LDVL, VR, LD
     $VR,
     $                 N, NOUT, WORK( IWRK ), LWORK-IWRK+1,
     $                 RWORK, N, IERR )
      END IF
*
*     Compute condition numbers if desired
*     (CWorkspace: need N*N+2*N unless SENSE = 'E')
*     (RWorkspace: need 2*N unless SENSE = 'E')
*
      IF( .NOT.WNTSNN ) THEN
         CALL AB_ZTRSNA( SENSE, 'A', SELECT, N, A, LDA, VL, LDVL, VR, LD
     $VR,
     $                RCONDE, RCONDV, N, NOUT, WORK( IWRK ), N, RWORK,
     $                ICOND )
      END IF
*
      IF( WANTVL ) THEN
*
*        Undo balancing of left eigenvectors
*
         CALL AB_ZGEBAK( BALANC, 'L', N, ILO, IHI, SCALE, N, VL, LDVL,
     $                IERR )
*
*        Normalize left eigenvectors and make largest component real
*
         DO 20 I = 1, N
            SCL = ONE / AB_DZNRM2( N, VL( 1, I ), 1 )
            CALL AB_ZDSCAL( N, SCL, VL( 1, I ), 1 )
            DO 10 K = 1, N
               RWORK( K ) = DBLE( VL( K, I ) )**2 +
     $                      AIMAG( VL( K, I ) )**2
   10       CONTINUE
            K = AB_IDAMAX( N, RWORK, 1 )
            TMP = CONJG( VL( K, I ) ) / SQRT( RWORK( K ) )
            CALL AB_ZSCAL( N, TMP, VL( 1, I ), 1 )
            VL( K, I ) = DCMPLX( DBLE( VL( K, I ) ), ZERO )
   20    CONTINUE
      END IF
*
      IF( WANTVR ) THEN
*
*        Undo balancing of right eigenvectors
*
         CALL AB_ZGEBAK( BALANC, 'R', N, ILO, IHI, SCALE, N, VR, LDVR,
     $                IERR )
*
*        Normalize right eigenvectors and make largest component real
*
         DO 40 I = 1, N
            SCL = ONE / AB_DZNRM2( N, VR( 1, I ), 1 )
            CALL AB_ZDSCAL( N, SCL, VR( 1, I ), 1 )
            DO 30 K = 1, N
               RWORK( K ) = DBLE( VR( K, I ) )**2 +
     $                      AIMAG( VR( K, I ) )**2
   30       CONTINUE
            K = AB_IDAMAX( N, RWORK, 1 )
            TMP = CONJG( VR( K, I ) ) / SQRT( RWORK( K ) )
            CALL AB_ZSCAL( N, TMP, VR( 1, I ), 1 )
            VR( K, I ) = DCMPLX( DBLE( VR( K, I ) ), ZERO )
   40    CONTINUE
      END IF
*
*     Undo scaling if necessary
*
   50 CONTINUE
      IF( SCALEA ) THEN
         CALL AB_ZLASCL( 'G', 0, 0, AB_CSCALE, ANRM, N-INFO, 1, W( INFO+
     $1 ),
     $                MAX( N-INFO, 1 ), IERR )
         IF( INFO.EQ.0 ) THEN
            IF( ( WNTSNV .OR. WNTSNB ) .AND. ICOND.EQ.0 )
     $         CALL AB_DLASCL( 'G', 0, 0, AB_CSCALE, ANRM, N, 1, RCONDV,
     $ N,
     $                      IERR )
         ELSE
            CALL AB_ZLASCL( 'G', 0, 0, AB_CSCALE, ANRM, ILO-1, 1, W, N, 
     $IERR )
         END IF
      END IF
*
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of AB_ZGEEVX
*
      END
