*> \brief \b AB_DSTEDC
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_DSTEDC + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_DSTEDC.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_DSTEDC.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_DSTEDC.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
*                          LIWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          COMPZ
*       INTEGER            INFO, LDZ, LIWORK, LWORK, N
*       ..
*       .. Array Arguments ..
*       INTEGER            IWORK( * )
*       DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
*> symmetric tridiagonal matrix using the divide and conquer method.
*> The eigenvectors of a full or band real symmetric matrix can also be
*> found if AB_DSYTRD or AB_DSPTRD or AB_DSBTRD has been used to reduce this
*> matrix to tridiagonal form.
*>
*> This code makes very mild assumptions about floating point
*> arithmetic. It will work on machines with a guard digit in
*> add/subtract, or on those binary machines without guard digits
*> which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
*> It could conceivably fail on hexadecimal or decimal machines
*> without guard digits, but we know of none.  See AB_DLAED3 for details.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] COMPZ
*> \verbatim
*>          COMPZ is CHARACTER*1
*>          = 'N':  Compute eigenvalues only.
*>          = 'I':  Compute eigenvectors of tridiagonal matrix also.
*>          = 'V':  Compute eigenvectors of original dense symmetric
*>                  matrix also.  On entry, Z contains the orthogonal
*>                  matrix used to reduce the original matrix to
*>                  tridiagonal form.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The dimension of the symmetric tridiagonal matrix.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] D
*> \verbatim
*>          D is DOUBLE PRECISION array, dimension (N)
*>          On entry, the diagonal elements of the tridiagonal matrix.
*>          On exit, if INFO = 0, the eigenvalues in ascending order.
*> \endverbatim
*>
*> \param[in,out] E
*> \verbatim
*>          E is DOUBLE PRECISION array, dimension (N-1)
*>          On entry, the subdiagonal elements of the tridiagonal matrix.
*>          On exit, E has been destroyed.
*> \endverbatim
*>
*> \param[in,out] Z
*> \verbatim
*>          Z is DOUBLE PRECISION array, dimension (LDZ,N)
*>          On entry, if COMPZ = 'V', then Z contains the orthogonal
*>          matrix used in the reduction to tridiagonal form.
*>          On exit, if INFO = 0, then if COMPZ = 'V', Z contains the
*>          orthonormal eigenvectors of the original symmetric matrix,
*>          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
*>          of the symmetric tridiagonal matrix.
*>          If  COMPZ = 'N', then Z is not referenced.
*> \endverbatim
*>
*> \param[in] LDZ
*> \verbatim
*>          LDZ is INTEGER
*>          The leading dimension of the array Z.  LDZ >= 1.
*>          If eigenvectors are desired, then LDZ >= max(1,N).
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
*>          If COMPZ = 'N' or N <= 1 then LWORK must be at least 1.
*>          If COMPZ = 'V' and N > 1 then LWORK must be at least
*>                         ( 1 + 3*N + 2*N*lg N + 4*N**2 ),
*>                         where lg( N ) = smallest integer k such
*>                         that 2**k >= N.
*>          If COMPZ = 'I' and N > 1 then LWORK must be at least
*>                         ( 1 + 4*N + N**2 ).
*>          Note that for COMPZ = 'I' or 'V', then if N is less than or
*>          equal to the minimum divide size, usually 25, then LWORK need
*>          only be max(1,2*(N-1)).
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
*>          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*> \endverbatim
*>
*> \param[in] LIWORK
*> \verbatim
*>          LIWORK is INTEGER
*>          The dimension of the array IWORK.
*>          If COMPZ = 'N' or N <= 1 then LIWORK must be at least 1.
*>          If COMPZ = 'V' and N > 1 then LIWORK must be at least
*>                         ( 6 + 6*N + 5*N*lg N ).
*>          If COMPZ = 'I' and N > 1 then LIWORK must be at least
*>                         ( 3 + 5*N ).
*>          Note that for COMPZ = 'I' or 'V', then if N is less than or
*>          equal to the minimum divide size, usually 25, then LIWORK
*>          need only be 1.
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
*>          = 0:  successful exit.
*>          < 0:  if INFO = -i, the i-th argument had an illegal value.
*>          > 0:  The algorithm failed to compute an eigenvalue while
*>                working on the submatrix lying in rows and columns
*>                INFO/(N+1) through mod(INFO,N+1).
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
*> \ingroup auxOTHERcomputational
*
*> \par Contributors:
*  ==================
*>
*> Jeff Rutter, Computer Science Division, University of California
*> at Berkeley, USA \n
*>  Modified by Francoise Tisseur, University of Tennessee
*>
*  =====================================================================
      SUBROUTINE AB_DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK computational routine (version 3.7.1) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2017
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
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
      INTEGER            FINISH, I, ICOMPZ, II, J, K, LGN, LIWMIN,
     $                   LWMIN, M, SMLSIZ, START, STOREZ, STRTRW
      DOUBLE PRECISION   EPS, ORGNRM, P, TINY
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ILAENV
      DOUBLE PRECISION   AB_DLAMCH, AB_DLANST
      EXTERNAL           AB_LSAME, AB_ILAENV, AB_DLAMCH, AB_DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DGEMM, AB_DLACPY, AB_DLAED0, AB_DLASCL, AB_D
     $LASET, AB_DLASRT,
     $                   AB_DSTEQR, AB_DSTERF, AB_DSWAP, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, LOG, MAX, MOD, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
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
      ELSE IF( ( LDZ.LT.1 ) .OR.
     $         ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1, N ) ) ) THEN
         INFO = -6
      END IF
*
      IF( INFO.EQ.0 ) THEN
*
*        Compute the workspace requirements
*
         SMLSIZ = AB_ILAENV( 9, 'AB_DSTEDC', ' ', 0, 0, 0, 0 )
         IF( N.LE.1 .OR. ICOMPZ.EQ.0 ) THEN
            LIWMIN = 1
            LWMIN = 1
         ELSE IF( N.LE.SMLSIZ ) THEN
            LIWMIN = 1
            LWMIN = 2*( N - 1 )
         ELSE
            LGN = INT( LOG( DBLE( N ) )/LOG( TWO ) )
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( ICOMPZ.EQ.1 ) THEN
               LWMIN = 1 + 3*N + 2*N*LGN + 4*N**2
               LIWMIN = 6 + 6*N + 5*N*LGN
            ELSE IF( ICOMPZ.EQ.2 ) THEN
               LWMIN = 1 + 4*N + N**2
               LIWMIN = 3 + 5*N
            END IF
         END IF
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -8
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -10
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_DSTEDC', -INFO )
         RETURN
      ELSE IF (LQUERY) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.NE.0 )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
*
*     If the following conditional clause is removed, then the routine
*     will use the Divide and Conquer routine to compute only the
*     eigenvalues, which requires (3N + 3N**2) real workspace and
*     (2 + 5N + 2N lg(N)) integer workspace.
*     Since on many architectures AB_DSTERF is much faster than any other
*     algorithm for finding eigenvalues only, it is used here
*     as the default. If the conditional clause is removed, then
*     information on the size of workspace needs to be changed.
*
*     If COMPZ = 'N', use AB_DSTERF to compute the eigenvalues.
*
      IF( ICOMPZ.EQ.0 ) THEN
         CALL AB_DSTERF( N, D, E, INFO )
         GO TO 50
      END IF
*
*     If N is smaller than the minimum divide size (SMLSIZ+1), then
*     solve the problem with another solver.
*
      IF( N.LE.SMLSIZ ) THEN
*
         CALL AB_DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
*
      ELSE
*
*        If COMPZ = 'V', the Z matrix must be stored elsewhere for later
*        use.
*
         IF( ICOMPZ.EQ.1 ) THEN
            STOREZ = 1 + N*N
         ELSE
            STOREZ = 1
         END IF
*
         IF( ICOMPZ.EQ.2 ) THEN
            CALL AB_DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
         END IF
*
*        Scale.
*
         ORGNRM = AB_DLANST( 'M', N, D, E )
         IF( ORGNRM.EQ.ZERO )
     $      GO TO 50
*
         EPS = AB_DLAMCH( 'Epsilon' )
*
         START = 1
*
*        while ( START <= N )
*
   10    CONTINUE
         IF( START.LE.N ) THEN
*
*           Let FINISH be the position of the next subdiagonal entry
*           such that E( FINISH ) <= TINY or FINISH = N if no such
*           subdiagonal exists.  The matrix identified by the elements
*           between START and FINISH constitutes an independent
*           sub-problem.
*
            FINISH = START
   20       CONTINUE
            IF( FINISH.LT.N ) THEN
               TINY = EPS*SQRT( ABS( D( FINISH ) ) )*
     $                    SQRT( ABS( D( FINISH+1 ) ) )
               IF( ABS( E( FINISH ) ).GT.TINY ) THEN
                  FINISH = FINISH + 1
                  GO TO 20
               END IF
            END IF
*
*           (Sub) Problem determined.  Compute its size and solve it.
*
            M = FINISH - START + 1
            IF( M.EQ.1 ) THEN
               START = FINISH + 1
               GO TO 10
            END IF
            IF( M.GT.SMLSIZ ) THEN
*
*              Scale.
*
               ORGNRM = AB_DLANST( 'M', M, D( START ), E( START ) )
               CALL AB_DLASCL( 'G', 0, 0, ORGNRM, ONE, M, 1, D( START ),
     $ M,
     $                      INFO )
               CALL AB_DLASCL( 'G', 0, 0, ORGNRM, ONE, M-1, 1, E( START 
     $),
     $                      M-1, INFO )
*
               IF( ICOMPZ.EQ.1 ) THEN
                  STRTRW = 1
               ELSE
                  STRTRW = START
               END IF
               CALL AB_DLAED0( ICOMPZ, N, M, D( START ), E( START ),
     $                      Z( STRTRW, START ), LDZ, WORK( 1 ), N,
     $                      WORK( STOREZ ), IWORK, INFO )
               IF( INFO.NE.0 ) THEN
                  INFO = ( INFO / ( M+1 )+START-1 )*( N+1 ) +
     $                   MOD( INFO, ( M+1 ) ) + START - 1
                  GO TO 50
               END IF
*
*              Scale back.
*
               CALL AB_DLASCL( 'G', 0, 0, ONE, ORGNRM, M, 1, D( START ),
     $ M,
     $                      INFO )
*
            ELSE
               IF( ICOMPZ.EQ.1 ) THEN
*
*                 Since QR won't update a Z matrix which is larger than
*                 the length of D, we must solve the sub-problem in a
*                 workspace and then multiply back into Z.
*
                  CALL AB_DSTEQR( 'I', M, D( START ), E( START ), WORK, 
     $M,
     $                         WORK( M*M+1 ), INFO )
                  CALL AB_DLACPY( 'A', N, M, Z( 1, START ), LDZ,
     $                         WORK( STOREZ ), N )
                  CALL AB_DGEMM( 'N', 'N', N, M, M, ONE,
     $                        WORK( STOREZ ), N, WORK, M, ZERO,
     $                        Z( 1, START ), LDZ )
               ELSE IF( ICOMPZ.EQ.2 ) THEN
                  CALL AB_DSTEQR( 'I', M, D( START ), E( START ),
     $                         Z( START, START ), LDZ, WORK, INFO )
               ELSE
                  CALL AB_DSTERF( M, D( START ), E( START ), INFO )
               END IF
               IF( INFO.NE.0 ) THEN
                  INFO = START*( N+1 ) + FINISH
                  GO TO 50
               END IF
            END IF
*
            START = FINISH + 1
            GO TO 10
         END IF
*
*        endwhile
*
         IF( ICOMPZ.EQ.0 ) THEN
*
*          Use Quick Sort
*
           CALL AB_DLASRT( 'I', N, D, INFO )
*
         ELSE
*
*          Use Selection Sort to minimize swaps of eigenvectors
*
           DO 40 II = 2, N
              I = II - 1
              K = I
              P = D( I )
              DO 30 J = II, N
                 IF( D( J ).LT.P ) THEN
                    K = J
                    P = D( J )
                 END IF
   30         CONTINUE
              IF( K.NE.I ) THEN
                 D( K ) = D( I )
                 D( I ) = P
                 CALL AB_DSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
              END IF
   40      CONTINUE
         END IF
      END IF
*
   50 CONTINUE
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of AB_DSTEDC
*
      END
