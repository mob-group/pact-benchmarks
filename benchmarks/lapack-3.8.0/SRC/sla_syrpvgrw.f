*> \brief \b AB_SLA_SYRPVGRW computes the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric indefinite matrix.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_SLA_SYRPVGRW + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_SLA_SYRPVGRW.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_SLA_SYRPVGRW.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_SLA_SYRPVGRW.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       REAL FUNCTION AB_SLA_SYRPVGRW( UPLO, N, INFO, A, LDA, AF, LDAF, IPIV,
*                                   WORK )
*
*       .. Scalar Arguments ..
*       CHARACTER*1        UPLO
*       INTEGER            N, INFO, LDA, LDAF
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       REAL               A( LDA, * ), AF( LDAF, * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>
*> AB_SLA_SYRPVGRW computes the reciprocal pivot growth factor
*> norm(A)/norm(U). The "max absolute element" norm is used. If this is
*> much less than 1, the stability of the LU factorization of the
*> (equilibrated) matrix A could be poor. This also means that the
*> solution X, estimated condition numbers, and error bounds could be
*> unreliable.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>       = 'U':  Upper triangle of A is stored;
*>       = 'L':  Lower triangle of A is stored.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>     The number of linear equations, i.e., the order of the
*>     matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] INFO
*> \verbatim
*>          INFO is INTEGER
*>     The value of INFO returned from AB_SSYTRF, .i.e., the pivot in
*>     column INFO is exactly 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is REAL array, dimension (LDA,N)
*>     On entry, the N-by-N matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>     The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in] AF
*> \verbatim
*>          AF is REAL array, dimension (LDAF,N)
*>     The block diagonal matrix D and the multipliers used to
*>     obtain the factor U or L as computed by AB_SSYTRF.
*> \endverbatim
*>
*> \param[in] LDAF
*> \verbatim
*>          LDAF is INTEGER
*>     The leading dimension of the array AF.  LDAF >= max(1,N).
*> \endverbatim
*>
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>     Details of the interchanges and the block structure of D
*>     as determined by AB_SSYTRF.
*> \endverbatim
*>
*> \param[in] WORK
*> \verbatim
*>          WORK is REAL array, dimension (2*N)
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
*> \ingroup realSYcomputational
*
*  =====================================================================
      REAL FUNCTION AB_SLA_SYRPVGRW( UPLO, N, INFO, A, LDA, AF, LDAF, IP
     $IV,
     $                            WORK )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO
      INTEGER            N, INFO, LDA, LDAF
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      REAL               A( LDA, * ), AF( LDAF, * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            NCOLS, I, J, K, KP
      REAL               AMAX, UMAX, RPVGRW, TMP
      LOGICAL            UPPER
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. External Functions ..
      EXTERNAL           AB_LSAME
      LOGICAL            AB_LSAME
*     ..
*     .. Executable Statements ..
*
      UPPER = AB_LSAME( 'Upper', UPLO )
      IF ( INFO.EQ.0 ) THEN
         IF ( UPPER ) THEN
            NCOLS = 1
         ELSE
            NCOLS = N
         END IF
      ELSE
         NCOLS = INFO
      END IF

      RPVGRW = 1.0
      DO I = 1, 2*N
         WORK( I ) = 0.0
      END DO
*
*     Find the max magnitude entry of each column of A.  Compute the max
*     for all N columns so we can apply the pivot permutation while
*     looping below.  Assume a full factorization is the common case.
*
      IF ( UPPER ) THEN
         DO J = 1, N
            DO I = 1, J
               WORK( N+I ) = MAX( ABS( A( I, J ) ), WORK( N+I ) )
               WORK( N+J ) = MAX( ABS( A( I, J ) ), WORK( N+J ) )
            END DO
         END DO
      ELSE
         DO J = 1, N
            DO I = J, N
               WORK( N+I ) = MAX( ABS( A( I, J ) ), WORK( N+I ) )
               WORK( N+J ) = MAX( ABS( A( I, J ) ), WORK( N+J ) )
            END DO
         END DO
      END IF
*
*     Now find the max magnitude entry of each column of U or L.  Also
*     permute the magnitudes of A above so they're in the same order as
*     the factor.
*
*     The iteration orders and permutations were copied from AB_SSYTRS.
*     Calls to AB_SSWAP would be severe overkill.
*
      IF ( UPPER ) THEN
         K = N
         DO WHILE ( K .LT. NCOLS .AND. K.GT.0 )
            IF ( IPIV( K ).GT.0 ) THEN
!              1x1 pivot
               KP = IPIV( K )
               IF ( KP .NE. K ) THEN
                  TMP = WORK( N+K )
                  WORK( N+K ) = WORK( N+KP )
                  WORK( N+KP ) = TMP
               END IF
               DO I = 1, K
                  WORK( K ) = MAX( ABS( AF( I, K ) ), WORK( K ) )
               END DO
               K = K - 1
            ELSE
!              2x2 pivot
               KP = -IPIV( K )
               TMP = WORK( N+K-1 )
               WORK( N+K-1 ) = WORK( N+KP )
               WORK( N+KP ) = TMP
               DO I = 1, K-1
                  WORK( K ) = MAX( ABS( AF( I, K ) ), WORK( K ) )
                  WORK( K-1 ) = MAX( ABS( AF( I, K-1 ) ), WORK( K-1 ) )
               END DO
               WORK( K ) = MAX( ABS( AF( K, K ) ), WORK( K ) )
               K = K - 2
            END IF
         END DO
         K = NCOLS
         DO WHILE ( K .LE. N )
            IF ( IPIV( K ).GT.0 ) THEN
               KP = IPIV( K )
               IF ( KP .NE. K ) THEN
                  TMP = WORK( N+K )
                  WORK( N+K ) = WORK( N+KP )
                  WORK( N+KP ) = TMP
               END IF
               K = K + 1
            ELSE
               KP = -IPIV( K )
               TMP = WORK( N+K )
               WORK( N+K ) = WORK( N+KP )
               WORK( N+KP ) = TMP
               K = K + 2
            END IF
         END DO
      ELSE
         K = 1
         DO WHILE ( K .LE. NCOLS )
            IF ( IPIV( K ).GT.0 ) THEN
!              1x1 pivot
               KP = IPIV( K )
               IF ( KP .NE. K ) THEN
                  TMP = WORK( N+K )
                  WORK( N+K ) = WORK( N+KP )
                  WORK( N+KP ) = TMP
               END IF
               DO I = K, N
                  WORK( K ) = MAX( ABS( AF( I, K ) ), WORK( K ) )
               END DO
               K = K + 1
            ELSE
!              2x2 pivot
               KP = -IPIV( K )
               TMP = WORK( N+K+1 )
               WORK( N+K+1 ) = WORK( N+KP )
               WORK( N+KP ) = TMP
               DO I = K+1, N
                  WORK( K ) = MAX( ABS( AF( I, K ) ), WORK( K ) )
                  WORK( K+1 ) = MAX( ABS( AF(I, K+1 ) ), WORK( K+1 ) )
               END DO
               WORK( K ) = MAX( ABS( AF( K, K ) ), WORK( K ) )
               K = K + 2
            END IF
         END DO
         K = NCOLS
         DO WHILE ( K .GE. 1 )
            IF ( IPIV( K ).GT.0 ) THEN
               KP = IPIV( K )
               IF ( KP .NE. K ) THEN
                  TMP = WORK( N+K )
                  WORK( N+K ) = WORK( N+KP )
                  WORK( N+KP ) = TMP
               END IF
               K = K - 1
            ELSE
               KP = -IPIV( K )
               TMP = WORK( N+K )
               WORK( N+K ) = WORK( N+KP )
               WORK( N+KP ) = TMP
               K = K - 2
            ENDIF
         END DO
      END IF
*
*     Compute the *inverse* of the max element growth factor.  Dividing
*     by zero would imply the largest entry of the factor's column is
*     zero.  Than can happen when either the column of A is zero or
*     massive pivots made the factor underflow to zero.  Neither counts
*     as growth in itself, so simply ignore terms with zero
*     denominators.
*
      IF ( UPPER ) THEN
         DO I = NCOLS, N
            UMAX = WORK( I )
            AMAX = WORK( N+I )
            IF ( UMAX /= 0.0 ) THEN
               RPVGRW = MIN( AMAX / UMAX, RPVGRW )
            END IF
         END DO
      ELSE
         DO I = 1, NCOLS
            UMAX = WORK( I )
            AMAX = WORK( N+I )
            IF ( UMAX /= 0.0 ) THEN
               RPVGRW = MIN( AMAX / UMAX, RPVGRW )
            END IF
         END DO
      END IF

      AB_SLA_SYRPVGRW = RPVGRW
      END
