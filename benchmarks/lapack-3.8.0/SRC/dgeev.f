*> \brief <b> AB_DGEEV computes the eigenvalues and, optionally, the left and/or right eigenvectors for GE matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_DGEEV + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_DGEEV.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_DGEEV.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_DGEEV.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
*                         LDVR, WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          JOBVL, JOBVR
*       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
*      $                   WI( * ), WORK( * ), WR( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DGEEV computes for an N-by-N real nonsymmetric matrix A, the
*> eigenvalues and, optionally, the left and/or right eigenvectors.
*>
*> The right eigenvector v(j) of A satisfies
*>                  A * v(j) = lambda(j) * v(j)
*> where lambda(j) is its eigenvalue.
*> The left eigenvector u(j) of A satisfies
*>               u(j)**H * A = lambda(j) * u(j)**H
*> where u(j)**H denotes the conjugate-transpose of u(j).
*>
*> The computed eigenvectors are normalized to have Euclidean norm
*> equal to 1 and largest component real.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] JOBVL
*> \verbatim
*>          JOBVL is CHARACTER*1
*>          = 'N': left eigenvectors of A are not computed;
*>          = 'V': left eigenvectors of A are computed.
*> \endverbatim
*>
*> \param[in] JOBVR
*> \verbatim
*>          JOBVR is CHARACTER*1
*>          = 'N': right eigenvectors of A are not computed;
*>          = 'V': right eigenvectors of A are computed.
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
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the N-by-N matrix A.
*>          On exit, A has been overwritten.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[out] WR
*> \verbatim
*>          WR is DOUBLE PRECISION array, dimension (N)
*> \endverbatim
*>
*> \param[out] WI
*> \verbatim
*>          WI is DOUBLE PRECISION array, dimension (N)
*>          WR and WI contain the real and imaginary parts,
*>          respectively, of the computed eigenvalues.  Complex
*>          conjugate pairs of eigenvalues appear consecutively
*>          with the eigenvalue having the positive imaginary part
*>          first.
*> \endverbatim
*>
*> \param[out] VL
*> \verbatim
*>          VL is DOUBLE PRECISION array, dimension (LDVL,N)
*>          If JOBVL = 'V', the left eigenvectors u(j) are stored one
*>          after another in the columns of VL, in the same order
*>          as their eigenvalues.
*>          If JOBVL = 'N', VL is not referenced.
*>          If the j-th eigenvalue is real, then u(j) = VL(:,j),
*>          the j-th column of VL.
*>          If the j-th and (j+1)-st eigenvalues form a complex
*>          conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
*>          u(j+1) = VL(:,j) - i*VL(:,j+1).
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
*>          VR is DOUBLE PRECISION array, dimension (LDVR,N)
*>          If JOBVR = 'V', the right eigenvectors v(j) are stored one
*>          after another in the columns of VR, in the same order
*>          as their eigenvalues.
*>          If JOBVR = 'N', VR is not referenced.
*>          If the j-th eigenvalue is real, then v(j) = VR(:,j),
*>          the j-th column of VR.
*>          If the j-th and (j+1)-st eigenvalues form a complex
*>          conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
*>          v(j+1) = VR(:,j) - i*VR(:,j+1).
*> \endverbatim
*>
*> \param[in] LDVR
*> \verbatim
*>          LDVR is INTEGER
*>          The leading dimension of the array VR.  LDVR >= 1; if
*>          JOBVR = 'V', LDVR >= N.
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
*>          The dimension of the array WORK.  LWORK >= max(1,3*N), and
*>          if JOBVL = 'V' or JOBVR = 'V', LWORK >= 4*N.  For good
*>          performance, LWORK must generally be larger.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by AB_XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value.
*>          > 0:  if INFO = i, the QR algorithm failed to compute all the
*>                eigenvalues, and no eigenvectors have been computed;
*>                elements i+1:N of WR and WI contain eigenvalues which
*>                have converged.
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
*  @precisions fortran d -> s
*
*> \ingroup doubleGEeigen
*
*  =====================================================================
      SUBROUTINE AB_DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR
     $,
     $                  LDVR, WORK, LWORK, INFO )
      implicit none
*
*  -- LAPACK driver routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVL, JOBVR
      INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WI( * ), WORK( * ), WR( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, SCALEA, WANTVL, WANTVR
      CHARACTER          SIDE
      INTEGER            HSWORK, I, IBAL, IERR, IHI, ILO, ITAU, IWRK, K,
     $                   LWORK_TREVC, MAXWRK, MINWRK, NOUT
      DOUBLE PRECISION   ANRM, BIGNUM, CS, AB_CSCALE, EPS, R, SCL, SMLNU
     $M,
     $                   SN
*     ..
*     .. Local Arrays ..
      LOGICAL            SELECT( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DGEBAK, AB_DGEBAL, AB_DGEHRD, AB_DHSEQR, AB_
     $DLABAD, AB_DLACPY,
     $                   AB_DLARTG, AB_DLASCL, AB_DORGHR, AB_DROT, AB_DS
     $CAL, AB_DTREVC3,
     $                   AB_XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_IDAMAX, AB_ILAENV
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANGE, AB_DLAPY2, AB_DNRM2
      EXTERNAL           AB_LSAME, AB_IDAMAX, AB_ILAENV, AB_DLAMCH, AB_DLAN
     $GE, AB_DLAPY2,
     $                   AB_DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      WANTVL = AB_LSAME( JOBVL, 'V' )
      WANTVR = AB_LSAME( JOBVR, 'V' )
      IF( ( .NOT.WANTVL ) .AND. ( .NOT.AB_LSAME( JOBVL, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( ( .NOT.WANTVR ) .AND. ( .NOT.AB_LSAME( JOBVR, 'N' ) ) ) T
     $HEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDVL.LT.1 .OR. ( WANTVL .AND. LDVL.LT.N ) ) THEN
         INFO = -9
      ELSE IF( LDVR.LT.1 .OR. ( WANTVR .AND. LDVR.LT.N ) ) THEN
         INFO = -11
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by AB_ILAENV.
*       HSWORK refers to the workspace preferred by AB_DHSEQR, as
*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
*       the worst case.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            MAXWRK = 2*N + N*AB_ILAENV( 1, 'AB_DGEHRD', ' ', N, 1, N, 0 
     $)
            IF( WANTVL ) THEN
               MINWRK = 4*N
               MAXWRK = MAX( MAXWRK, 2*N + ( N - 1 )*AB_ILAENV( 1,
     $                       'AB_DORGHR', ' ', N, 1, N, -1 ) )
               CALL AB_DHSEQR( 'S', 'V', N, 1, N, A, LDA, WR, WI, VL, LD
     $VL,
     $                      WORK, -1, INFO )
               HSWORK = INT( WORK(1) )
               MAXWRK = MAX( MAXWRK, N + 1, N + HSWORK )
               CALL AB_DTREVC3( 'L', 'B', SELECT, N, A, LDA,
     $                       VL, LDVL, VR, LDVR, N, NOUT,
     $                       WORK, -1, IERR )
               LWORK_TREVC = INT( WORK(1) )
               MAXWRK = MAX( MAXWRK, N + LWORK_TREVC )
               MAXWRK = MAX( MAXWRK, 4*N )
            ELSE IF( WANTVR ) THEN
               MINWRK = 4*N
               MAXWRK = MAX( MAXWRK, 2*N + ( N - 1 )*AB_ILAENV( 1,
     $                       'AB_DORGHR', ' ', N, 1, N, -1 ) )
               CALL AB_DHSEQR( 'S', 'V', N, 1, N, A, LDA, WR, WI, VR, LD
     $VR,
     $                      WORK, -1, INFO )
               HSWORK = INT( WORK(1) )
               MAXWRK = MAX( MAXWRK, N + 1, N + HSWORK )
               CALL AB_DTREVC3( 'R', 'B', SELECT, N, A, LDA,
     $                       VL, LDVL, VR, LDVR, N, NOUT,
     $                       WORK, -1, IERR )
               LWORK_TREVC = INT( WORK(1) )
               MAXWRK = MAX( MAXWRK, N + LWORK_TREVC )
               MAXWRK = MAX( MAXWRK, 4*N )
            ELSE
               MINWRK = 3*N
               CALL AB_DHSEQR( 'E', 'N', N, 1, N, A, LDA, WR, WI, VR, LD
     $VR,
     $                      WORK, -1, INFO )
               HSWORK = INT( WORK(1) )
               MAXWRK = MAX( MAXWRK, N + 1, N + HSWORK )
            END IF
            MAXWRK = MAX( MAXWRK, MINWRK )
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_DGEEV ', -INFO )
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
      ANRM = AB_DLANGE( 'M', N, N, A, LDA, DUM )
      SCALEA = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         SCALEA = .TRUE.
         AB_CSCALE = SMLNUM
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         SCALEA = .TRUE.
         AB_CSCALE = BIGNUM
      END IF
      IF( SCALEA )
     $   CALL AB_DLASCL( 'G', 0, 0, ANRM, AB_CSCALE, N, N, A, LDA, IERR 
     $)
*
*     Balance the matrix
*     (Workspace: need N)
*
      IBAL = 1
      CALL AB_DGEBAL( 'B', N, A, LDA, ILO, IHI, WORK( IBAL ), IERR )
*
*     Reduce to upper Hessenberg form
*     (Workspace: need 3*N, prefer 2*N+N*NB)
*
      ITAU = IBAL + N
      IWRK = ITAU + N
      CALL AB_DGEHRD( N, ILO, IHI, A, LDA, WORK( ITAU ), WORK( IWRK ),
     $             LWORK-IWRK+1, IERR )
*
      IF( WANTVL ) THEN
*
*        Want left eigenvectors
*        Copy Householder vectors to VL
*
         SIDE = 'L'
         CALL AB_DLACPY( 'L', N, N, A, LDA, VL, LDVL )
*
*        Generate orthogonal matrix in VL
*        (Workspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*
         CALL AB_DORGHR( N, ILO, IHI, VL, LDVL, WORK( ITAU ), WORK( IWRK
     $ ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VL
*        (Workspace: need N+1, prefer N+HSWORK (see comments) )
*
         IWRK = ITAU
         CALL AB_DHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, WR, WI, VL, LDVL
     $,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
         IF( WANTVR ) THEN
*
*           Want left and right eigenvectors
*           Copy Schur vectors to VR
*
            SIDE = 'B'
            CALL AB_DLACPY( 'F', N, N, VL, LDVL, VR, LDVR )
         END IF
*
      ELSE IF( WANTVR ) THEN
*
*        Want right eigenvectors
*        Copy Householder vectors to VR
*
         SIDE = 'R'
         CALL AB_DLACPY( 'L', N, N, A, LDA, VR, LDVR )
*
*        Generate orthogonal matrix in VR
*        (Workspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*
         CALL AB_DORGHR( N, ILO, IHI, VR, LDVR, WORK( ITAU ), WORK( IWRK
     $ ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VR
*        (Workspace: need N+1, prefer N+HSWORK (see comments) )
*
         IWRK = ITAU
         CALL AB_DHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, WR, WI, VR, LDVR
     $,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
      ELSE
*
*        Compute eigenvalues only
*        (Workspace: need N+1, prefer N+HSWORK (see comments) )
*
         IWRK = ITAU
         CALL AB_DHSEQR( 'E', 'N', N, ILO, IHI, A, LDA, WR, WI, VR, LDVR
     $,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
      END IF
*
*     If INFO .NE. 0 from AB_DHSEQR, then quit
*
      IF( INFO.NE.0 )
     $   GO TO 50
*
      IF( WANTVL .OR. WANTVR ) THEN
*
*        Compute left and/or right eigenvectors
*        (Workspace: need 4*N, prefer N + N + 2*N*NB)
*
         CALL AB_DTREVC3( SIDE, 'B', SELECT, N, A, LDA, VL, LDVL, VR, LD
     $VR,
     $                 N, NOUT, WORK( IWRK ), LWORK-IWRK+1, IERR )
      END IF
*
      IF( WANTVL ) THEN
*
*        Undo balancing of left eigenvectors
*        (Workspace: need N)
*
         CALL AB_DGEBAK( 'B', 'L', N, ILO, IHI, WORK( IBAL ), N, VL, LDV
     $L,
     $                IERR )
*
*        Normalize left eigenvectors and make largest component real
*
         DO 20 I = 1, N
            IF( WI( I ).EQ.ZERO ) THEN
               SCL = ONE / AB_DNRM2( N, VL( 1, I ), 1 )
               CALL AB_DSCAL( N, SCL, VL( 1, I ), 1 )
            ELSE IF( WI( I ).GT.ZERO ) THEN
               SCL = ONE / AB_DLAPY2( AB_DNRM2( N, VL( 1, I ), 1 ),
     $               AB_DNRM2( N, VL( 1, I+1 ), 1 ) )
               CALL AB_DSCAL( N, SCL, VL( 1, I ), 1 )
               CALL AB_DSCAL( N, SCL, VL( 1, I+1 ), 1 )
               DO 10 K = 1, N
                  WORK( IWRK+K-1 ) = VL( K, I )**2 + VL( K, I+1 )**2
   10          CONTINUE
               K = AB_IDAMAX( N, WORK( IWRK ), 1 )
               CALL AB_DLARTG( VL( K, I ), VL( K, I+1 ), CS, SN, R )
               CALL AB_DROT( N, VL( 1, I ), 1, VL( 1, I+1 ), 1, CS, SN )
               VL( K, I+1 ) = ZERO
            END IF
   20    CONTINUE
      END IF
*
      IF( WANTVR ) THEN
*
*        Undo balancing of right eigenvectors
*        (Workspace: need N)
*
         CALL AB_DGEBAK( 'B', 'R', N, ILO, IHI, WORK( IBAL ), N, VR, LDV
     $R,
     $                IERR )
*
*        Normalize right eigenvectors and make largest component real
*
         DO 40 I = 1, N
            IF( WI( I ).EQ.ZERO ) THEN
               SCL = ONE / AB_DNRM2( N, VR( 1, I ), 1 )
               CALL AB_DSCAL( N, SCL, VR( 1, I ), 1 )
            ELSE IF( WI( I ).GT.ZERO ) THEN
               SCL = ONE / AB_DLAPY2( AB_DNRM2( N, VR( 1, I ), 1 ),
     $               AB_DNRM2( N, VR( 1, I+1 ), 1 ) )
               CALL AB_DSCAL( N, SCL, VR( 1, I ), 1 )
               CALL AB_DSCAL( N, SCL, VR( 1, I+1 ), 1 )
               DO 30 K = 1, N
                  WORK( IWRK+K-1 ) = VR( K, I )**2 + VR( K, I+1 )**2
   30          CONTINUE
               K = AB_IDAMAX( N, WORK( IWRK ), 1 )
               CALL AB_DLARTG( VR( K, I ), VR( K, I+1 ), CS, SN, R )
               CALL AB_DROT( N, VR( 1, I ), 1, VR( 1, I+1 ), 1, CS, SN )
               VR( K, I+1 ) = ZERO
            END IF
   40    CONTINUE
      END IF
*
*     Undo scaling if necessary
*
   50 CONTINUE
      IF( SCALEA ) THEN
         CALL AB_DLASCL( 'G', 0, 0, AB_CSCALE, ANRM, N-INFO, 1, WR( INFO
     $+1 ),
     $                MAX( N-INFO, 1 ), IERR )
         CALL AB_DLASCL( 'G', 0, 0, AB_CSCALE, ANRM, N-INFO, 1, WI( INFO
     $+1 ),
     $                MAX( N-INFO, 1 ), IERR )
         IF( INFO.GT.0 ) THEN
            CALL AB_DLASCL( 'G', 0, 0, AB_CSCALE, ANRM, ILO-1, 1, WR, N,
     $                   IERR )
            CALL AB_DLASCL( 'G', 0, 0, AB_CSCALE, ANRM, ILO-1, 1, WI, N,
     $                   IERR )
         END IF
      END IF
*
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of AB_DGEEV
*
      END
