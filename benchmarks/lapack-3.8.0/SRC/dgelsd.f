*> \brief <b> AB_AB_DGELSD computes the minimum-norm solution to a linear least squares problem for GE matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_AB_DGELSD + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_AB_DGELSD.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_AB_DGELSD.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_AB_DGELSD.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_DGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
*                          WORK, LWORK, IWORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
*       DOUBLE PRECISION   RCOND
*       ..
*       .. Array Arguments ..
*       INTEGER            IWORK( * )
*       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_AB_DGELSD computes the minimum-norm solution to a real linear least
*> squares problem:
*>     minimize 2-norm(| b - A*x |)
*> using the singular value decomposition (SVD) of A. A is an M-by-N
*> matrix which may be rank-deficient.
*>
*> Several right hand side vectors b and solution vectors x can be
*> handled in a single call; they are stored as the columns of the
*> M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*> matrix X.
*>
*> The problem is solved in three steps:
*> (1) Reduce the coefficient matrix A to bidiagonal form with
*>     HousehoAB_LDEr transformations, reducing the original problem
*>     into a "bidiagonal least squares problem" (BLS)
*> (2) Solve the BLS using a divide and conquer approach.
*> (3) Apply back all the HousehoAB_LDEr transformations to solve
*>     the original least squares problem.
*>
*> The effective rank of A is determined by treating as zero those
*> singular values which are less than RCOND times the largest singular
*> value.
*>
*> The divide and conquer algorithm makes very mild assumptions about
*> floating point arithmetic. It will work on machines with a guard
*> digit in add/subtract, or on those binary machines without guard
*> digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*> Cray-2. It could conceivably fail on hexadecimal or decimal machines
*> without guard digits, but we know of none.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of A. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of A. N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrices B and X. NRHS >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the M-by-N matrix A.
*>          On exit, A has been destroyed.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
*>          On entry, the M-by-NRHS right hand side matrix B.
*>          On exit, B is overwritten by the N-by-NRHS solution
*>          matrix X.  If m >= n and RANK = n, the residual
*>          sum-of-squares for the solution in the i-th column is given
*>          by the sum of squares of elements n+1:m in that column.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B. LDB >= max(1,max(M,N)).
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is DOUBLE PRECISION array, dimension (min(M,N))
*>          The singular values of A in decreasing order.
*>          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
*> \endverbatim
*>
*> \param[in] RCOND
*> \verbatim
*>          RCOND is DOUBLE PRECISION
*>          RCOND is used to determine the effective rank of A.
*>          Singular values S(i) <= RCOND*S(1) are treated as zero.
*>          If RCOND < 0, machine precision is used instead.
*> \endverbatim
*>
*> \param[out] RANK
*> \verbatim
*>          RANK is INTEGER
*>          The effective rank of A, i.e., the number of singular values
*>          which are greater than RCOND*S(1).
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
*>          The dimension of the array WORK. LWORK must be at least 1.
*>          The exact minimum amount of workspace needed depends on M,
*>          N and NRHS. As long as LWORK is at least
*>              12*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2,
*>          if M is greater than or equal to N or
*>              12*M + 2*M*SMLSIZ + 8*M*NLVL + M*NRHS + (SMLSIZ+1)**2,
*>          if M is less than N, the code will execute correctly.
*>          SMLSIZ is returned by AB_ILAENV and is equal to the maximum
*>          size of the subproblems at the bottom of the computation
*>          tree (usually about 25), and
*>             NLVL = MAX( 0, INT( LOG_2( MIN( M,N )/(SMLSIZ+1) ) ) + 1 )
*>          For good performance, LWORK should generally be larger.
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
*>          LIWORK >= max(1, 3 * MINMN * NLVL + 11 * MINMN),
*>          where MINMN = MIN( M,N ).
*>          On exit, if INFO = 0, IWORK(1) returns the minimum LIWORK.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value.
*>          > 0:  the algorithm for computing the SVD failed to converge;
*>                if INFO = i, i off-diagonal elements of an intermediate
*>                bidiagonal form did not converge to zero.
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
*> \date June 2017
*
*> \ingroup doubleGEsolve
*
*> \par Contributors:
*  ==================
*>
*>     Ming Gu and Ren-Cang Li, Computer Science Division, University of
*>       California at Berkeley, USA \n
*>     Osni Marques, LBNL/NERSC, USA \n
*
*  =====================================================================
      SUBROUTINE AB_AB_DGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RAN
     $K,
     $                   WORK, LWORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.7.1) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2017
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            IASCL, IBSCL, IE, IL, ITAU, ITAUP, ITAUQ,
     $                   LDWORK, LIWORK, MAXMN, MAXWRK, MINMN, MINWRK,
     $                   MM, MNTHR, NLVL, NWORK, SMLSIZ, WLALSD
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, EPS, SFMIN, SMLNUM
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DGEBRD, AB_AB_DGELQF, AB_AB_DGEQRF, AB_DLABA
     $D, AB_DLACPY, AB_DLALSD,
     $                   AB_DLASCL, AB_DLASET, AB_DORMBR, AB_DORMLQ, AB_
     $DORMQR, AB_XERBLA
*     ..
*     .. External Functions ..
      INTEGER            AB_ILAENV
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANGE
      EXTERNAL           AB_ILAENV, AB_DLAMCH, AB_DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, LOG, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments.
*
      INFO = 0
      MINMN = MIN( M, N )
      MAXMN = MAX( M, N )
      MNTHR = AB_ILAENV( 6, 'AB_AB_DGELSD', ' ', M, N, NRHS, -1 )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, MAXMN ) ) THEN
         INFO = -7
      END IF
*
      SMLSIZ = AB_ILAENV( 9, 'AB_AB_DGELSD', ' ', 0, 0, 0, 0 )
*
*     Compute workspace.
*     (Note: Comments in the code beginning "Workspace:" describe the
*     minimal amount of workspace needed at that point in the code,
*     as well as the preferred amount for good performance.
*     NB refers to the optimal block size for the immediately
*     following subroutine, as returned by AB_ILAENV.)
*
      MINWRK = 1
      LIWORK = 1
      MINMN = MAX( 1, MINMN )
      NLVL = MAX( INT( LOG( DBLE( MINMN ) / DBLE( SMLSIZ+1 ) ) /
     $       LOG( TWO ) ) + 1, 0 )
*
      IF( INFO.EQ.0 ) THEN
         MAXWRK = 0
         LIWORK = 3*MINMN*NLVL + 11*MINMN
         MM = M
         IF( M.GE.N .AND. M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns.
*
            MM = N
            MAXWRK = MAX( MAXWRK, N+N*AB_ILAENV( 1, 'AB_AB_DGEQRF', ' ',
     $ M, N,
     $               -1, -1 ) )
            MAXWRK = MAX( MAXWRK, N+NRHS*
     $               AB_ILAENV( 1, 'AB_DORMQR', 'LT', M, NRHS, N, -1 ) )
         END IF
         IF( M.GE.N ) THEN
*
*           Path 1 - overdetermined or exactly determined.
*
            MAXWRK = MAX( MAXWRK, 3*N+( MM+N )*
     $               AB_ILAENV( 1, 'AB_DGEBRD', ' ', MM, N, -1, -1 ) )
            MAXWRK = MAX( MAXWRK, 3*N+NRHS*
     $               AB_ILAENV( 1, 'AB_DORMBR', 'QLT', MM, NRHS, N, -1 )
     $ )
            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $               AB_ILAENV( 1, 'AB_DORMBR', 'PLN', N, NRHS, N, -1 ) 
     $)
            WLALSD = 9*N+2*N*SMLSIZ+8*N*NLVL+N*NRHS+(SMLSIZ+1)**2
            MAXWRK = MAX( MAXWRK, 3*N+WLALSD )
            MINWRK = MAX( 3*N+MM, 3*N+NRHS, 3*N+WLALSD )
         END IF
         IF( N.GT.M ) THEN
            WLALSD = 9*M+2*M*SMLSIZ+8*M*NLVL+M*NRHS+(SMLSIZ+1)**2
            IF( N.GE.MNTHR ) THEN
*
*              Path 2a - underdetermined, with many more columns
*              than rows.
*
               MAXWRK = M + M*AB_ILAENV( 1, 'AB_AB_DGELQF', ' ', M, N, -
     $1, -1 )
               MAXWRK = MAX( MAXWRK, M*M+4*M+2*M*
     $                  AB_ILAENV( 1, 'AB_DGEBRD', ' ', M, M, -1, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+NRHS*
     $                  AB_ILAENV( 1, 'AB_DORMBR', 'QLT', M, NRHS, M, -1
     $ ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+( M-1 )*
     $                  AB_ILAENV( 1, 'AB_DORMBR', 'PLN', M, NRHS, M, -1
     $ ) )
               IF( NRHS.GT.1 ) THEN
                  MAXWRK = MAX( MAXWRK, M*M+M+M*NRHS )
               ELSE
                  MAXWRK = MAX( MAXWRK, M*M+2*M )
               END IF
               MAXWRK = MAX( MAXWRK, M+NRHS*
     $                  AB_ILAENV( 1, 'AB_DORMLQ', 'LT', N, NRHS, M, -1 
     $) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+WLALSD )
!     XXX: Ensure the Path 2a case below is triggered.  The workspace
!     calculation should use queries for all routines eventually.
               MAXWRK = MAX( MAXWRK,
     $              4*M+M*M+MAX( M, 2*M-4, NRHS, N-3*M ) )
            ELSE
*
*              Path 2 - remaining underdetermined cases.
*
               MAXWRK = 3*M + ( N+M )*AB_ILAENV( 1, 'AB_DGEBRD', ' ', M,
     $ N,
     $                  -1, -1 )
               MAXWRK = MAX( MAXWRK, 3*M+NRHS*
     $                  AB_ILAENV( 1, 'AB_DORMBR', 'QLT', M, NRHS, N, -1
     $ ) )
               MAXWRK = MAX( MAXWRK, 3*M+M*
     $                  AB_ILAENV( 1, 'AB_DORMBR', 'PLN', N, NRHS, M, -1
     $ ) )
               MAXWRK = MAX( MAXWRK, 3*M+WLALSD )
            END IF
            MINWRK = MAX( 3*M+NRHS, 3*M+M, 3*M+WLALSD )
         END IF
         MINWRK = MIN( MINWRK, MAXWRK )
         WORK( 1 ) = MAXWRK
         IWORK( 1 ) = LIWORK

         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_AB_DGELSD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         GO TO 10
      END IF
*
*     Quick return if possible.
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters.
*
      EPS = AB_DLAMCH( 'P' )
      SFMIN = AB_DLAMCH( 'S' )
      SMLNUM = SFMIN / EPS
      BIGNUM = ONE / SMLNUM
      CALL AB_DLABAD( SMLNUM, BIGNUM )
*
*     Scale A if max entry outside range [SMLNUM,BIGNUM].
*
      ANRM = AB_DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM.
*
         CALL AB_DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM.
*
         CALL AB_DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL AB_DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         CALL AB_DLASET( 'F', MINMN, 1, ZERO, ZERO, S, 1 )
         RANK = 0
         GO TO 10
      END IF
*
*     Scale B if max entry outside range [SMLNUM,BIGNUM].
*
      BNRM = AB_DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM.
*
         CALL AB_DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO 
     $)
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM.
*
         CALL AB_DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO 
     $)
         IBSCL = 2
      END IF
*
*     If M < N make sure certain entries of B are zero.
*
      IF( M.LT.N )
     $   CALL AB_DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
*
*     Overdetermined case.
*
      IF( M.GE.N ) THEN
*
*        Path 1 - overdetermined or exactly determined.
*
         MM = M
         IF( M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns.
*
            MM = N
            ITAU = 1
            NWORK = ITAU + N
*
*           Compute A=Q*R.
*           (Workspace: need 2*N, prefer N+N*NB)
*
            CALL AB_AB_DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK )
     $,
     $                   LWORK-NWORK+1, INFO )
*
*           Multiply B by transpose(Q).
*           (Workspace: need N+NRHS, prefer N+NRHS*NB)
*
            CALL AB_DORMQR( 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAU ), 
     $B,
     $                   LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*           Zero out below R.
*
            IF( N.GT.1 ) THEN
               CALL AB_DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA
     $ )
            END IF
         END IF
*
         IE = 1
         ITAUQ = IE + N
         ITAUP = ITAUQ + N
         NWORK = ITAUP + N
*
*        Bidiagonalize R in A.
*        (Workspace: need 3*N+MM, prefer 3*N+(MM+N)*NB)
*
         CALL AB_DGEBRD( MM, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of R.
*        (Workspace: need 3*N+NRHS, prefer 3*N+NRHS*NB)
*
         CALL AB_DORMBR( 'Q', 'L', 'T', MM, NRHS, N, A, LDA, WORK( ITAUQ
     $ ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL AB_DLALSD( 'U', SMLSIZ, N, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of R.
*
         CALL AB_DORMBR( 'P', 'L', 'N', N, NRHS, N, A, LDA, WORK( ITAUP 
     $),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      ELSE IF( N.GE.MNTHR .AND. LWORK.GE.4*M+M*M+
     $         MAX( M, 2*M-4, NRHS, N-3*M, WLALSD ) ) THEN
*
*        Path 2a - underdetermined, with many more columns than rows
*        and sufficient workspace for an efficient algorithm.
*
         LDWORK = M
         IF( LWORK.GE.MAX( 4*M+M*LDA+MAX( M, 2*M-4, NRHS, N-3*M ),
     $       M*LDA+M+M*NRHS, 4*M+M*LDA+WLALSD ) )LDWORK = LDA
         ITAU = 1
         NWORK = M + 1
*
*        Compute A=L*Q.
*        (Workspace: need 2*M, prefer M+M*NB)
*
         CALL AB_AB_DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
         IL = NWORK
*
*        Copy L to WORK(IL), zeroing out above its diagonal.
*
         CALL AB_DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWORK )
         CALL AB_DLASET( 'U', M-1, M-1, ZERO, ZERO, WORK( IL+LDWORK ),
     $                LDWORK )
         IE = IL + LDWORK*M
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         NWORK = ITAUP + M
*
*        Bidiagonalize L in WORK(IL).
*        (Workspace: need M*M+5*M, prefer M*M+4*M+2*M*NB)
*
         CALL AB_DGEBRD( M, M, WORK( IL ), LDWORK, S, WORK( IE ),
     $                WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of L.
*        (Workspace: need M*M+4*M+NRHS, prefer M*M+4*M+NRHS*NB)
*
         CALL AB_DORMBR( 'Q', 'L', 'T', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUQ ), B, LDB, WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL AB_DLALSD( 'U', SMLSIZ, M, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of L.
*
         CALL AB_DORMBR( 'P', 'L', 'N', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUP ), B, LDB, WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Zero out below first M rows of B.
*
         CALL AB_DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
         NWORK = ITAU + M
*
*        Multiply transpose(Q) by B.
*        (Workspace: need M+NRHS, prefer M+NRHS*NB)
*
         CALL AB_DORMLQ( 'L', 'T', N, NRHS, M, A, LDA, WORK( ITAU ), B,
     $                LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      ELSE
*
*        Path 2 - remaining underdetermined cases.
*
         IE = 1
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         NWORK = ITAUP + M
*
*        Bidiagonalize A.
*        (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
         CALL AB_DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors.
*        (Workspace: need 3*M+NRHS, prefer 3*M+NRHS*NB)
*
         CALL AB_DORMBR( 'Q', 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAUQ 
     $),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL AB_DLALSD( 'L', SMLSIZ, M, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of A.
*
         CALL AB_DORMBR( 'P', 'L', 'N', N, NRHS, M, A, LDA, WORK( ITAUP 
     $),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      END IF
*
*     Undo scaling.
*
      IF( IASCL.EQ.1 ) THEN
         CALL AB_DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO 
     $)
         CALL AB_DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL AB_DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO 
     $)
         CALL AB_DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL AB_DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO 
     $)
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL AB_DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO 
     $)
      END IF
*
   10 CONTINUE
      WORK( 1 ) = MAXWRK
      IWORK( 1 ) = LIWORK
      RETURN
*
*     End of AB_AB_DGELSD
*
      END
