*> \brief \b AB_SORMTR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_SORMTR + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_SORMTR.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_SORMTR.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_SORMTR.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,
*                          WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          SIDE, TRANS, UPLO
*       INTEGER            INFO, LDA, LDC, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       REAL               A( LDA, * ), C( LDC, * ), TAU( * ),
*      $                   WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SORMTR overwrites the general real M-by-N matrix C with
*>
*>                 SIDE = 'L'     SIDE = 'R'
*> TRANS = 'N':      Q * C          C * Q
*> TRANS = 'T':      Q**T * C       C * Q**T
*>
*> where Q is a real orthogonal matrix of order nq, with nq = m if
*> SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
*> nq-1 elementary reflectors, as returned by AB_SSYTRD:
*>
*> if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
*>
*> if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>          = 'L': apply Q or Q**T from the Left;
*>          = 'R': apply Q or Q**T from the Right.
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U': Upper triangle of A contains elementary reflectors
*>                 from AB_SSYTRD;
*>          = 'L': Lower triangle of A contains elementary reflectors
*>                 from AB_SSYTRD.
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>          = 'N':  No transpose, apply Q;
*>          = 'T':  Transpose, apply Q**T.
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix C. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix C. N >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is REAL array, dimension
*>                               (LDA,M) if SIDE = 'L'
*>                               (LDA,N) if SIDE = 'R'
*>          The vectors which define the elementary reflectors, as
*>          returned by AB_SSYTRD.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.
*>          LDA >= max(1,M) if SIDE = 'L'; LDA >= max(1,N) if SIDE = 'R'.
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is REAL array, dimension
*>                               (M-1) if SIDE = 'L'
*>                               (N-1) if SIDE = 'R'
*>          TAU(i) must contain the scalar factor of the elementary
*>          reflector H(i), as returned by AB_SSYTRD.
*> \endverbatim
*>
*> \param[in,out] C
*> \verbatim
*>          C is REAL array, dimension (LDC,N)
*>          On entry, the M-by-N matrix C.
*>          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the array C. LDC >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is REAL array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          If SIDE = 'L', LWORK >= max(1,N);
*>          if SIDE = 'R', LWORK >= max(1,M).
*>          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*>          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*>          blocksize.
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
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
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
*> \ingroup realOTHERcomputational
*
*  =====================================================================
      SUBROUTINE AB_SORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC
     $,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS, UPLO
      INTEGER            INFO, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, UPPER
      INTEGER            I1, I2, IINFO, LWKOPT, MI, NI, NB, NQ, NW
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      INTEGER            AB_ILAENV
      EXTERNAL           AB_ILAENV, AB_LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_SORMQL, AB_SORMQR, AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = AB_LSAME( SIDE, 'L' )
      UPPER = AB_LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.AB_LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.UPPER .AND. .NOT.AB_LSAME( UPLO, 'L' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.AB_LSAME( TRANS, 'N' ) .AND. .NOT.AB_LSAME( TRANS
     $, 'T' ) )
     $          THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( UPPER ) THEN
            IF( LEFT ) THEN
               NB = AB_ILAENV( 1, 'AB_SORMQL', SIDE // TRANS, M-1, N, M-
     $1,
     $                      -1 )
            ELSE
               NB = AB_ILAENV( 1, 'AB_SORMQL', SIDE // TRANS, M, N-1, N-
     $1,
     $                      -1 )
            END IF
         ELSE
            IF( LEFT ) THEN
               NB = AB_ILAENV( 1, 'AB_SORMQR', SIDE // TRANS, M-1, N, M-
     $1,
     $                      -1 )
            ELSE
               NB = AB_ILAENV( 1, 'AB_SORMQR', SIDE // TRANS, M, N-1, N-
     $1,
     $                      -1 )
            END IF
         END IF
         LWKOPT = MAX( 1, NW )*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_SORMTR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. NQ.EQ.1 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( LEFT ) THEN
         MI = M - 1
         NI = N
      ELSE
         MI = M
         NI = N - 1
      END IF
*
      IF( UPPER ) THEN
*
*        Q was determined by a call to AB_SSYTRD with UPLO = 'U'
*
         CALL AB_SORMQL( SIDE, TRANS, MI, NI, NQ-1, A( 1, 2 ), LDA, TAU,
     $ C,
     $                LDC, WORK, LWORK, IINFO )
      ELSE
*
*        Q was determined by a call to AB_SSYTRD with UPLO = 'L'
*
         IF( LEFT ) THEN
            I1 = 2
            I2 = 1
         ELSE
            I1 = 1
            I2 = 2
         END IF
         CALL AB_SORMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU,
     $                C( I1, I2 ), LDC, WORK, LWORK, IINFO )
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of AB_SORMTR
*
      END
