*> \brief \b AB_CSYCONVF_ROOK
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_CSYCONVF_ROOK + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_CSYCONvf_rook.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_CSYCONvf_rook.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_CSYCONvf_rook.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CSYCONVF_ROOK( UPLO, WAY, N, A, LDA, E, IPIV, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO, WAY
*       INTEGER            INFO, LDA, N
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       COMPLEX            A( LDA, * ), E( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*> If parameter WAY = 'C':
*> AB_CSYCONVF_ROOK converts the factorization output format used in
*> AB_CSYTRF_ROOK provided on entry in parameter A into the factorization
*> output format used in AB_CSYTRF_RK (or AB_CSYTRF_BK) that is stored
*> on exit in parameters A and E. IPIV format for AB_CSYTRF_ROOK and
*> AB_CSYTRF_RK (or AB_CSYTRF_BK) is the same and is not converted.
*>
*> If parameter WAY = 'R':
*> AB_CSYCONVF_ROOK performs the conversion in reverse direction, i.e.
*> converts the factorization output format used in AB_CSYTRF_RK
*> (or AB_CSYTRF_BK) provided on entry in parameters A and E into
*> the factorization output format used in AB_CSYTRF_ROOK that is stored
*> on exit in parameter A. IPIV format for AB_CSYTRF_ROOK and
*> AB_CSYTRF_RK (or AB_CSYTRF_BK) is the same and is not converted.
*>
*> AB_CSYCONVF_ROOK can also convert in Hermitian matrix case, i.e. between
*> formats used in AB_CHETRF_ROOK and AB_CHETRF_RK (or AB_CHETRF_BK).
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the details of the factorization are
*>          stored as an upper or lower triangular matrix A.
*>          = 'U':  Upper triangular
*>          = 'L':  Lower triangular
*> \endverbatim
*>
*> \param[in] WAY
*> \verbatim
*>          WAY is CHARACTER*1
*>          = 'C': Convert
*>          = 'R': Revert
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
*>          A is COMPLEX array, dimension (LDA,N)
*>
*>          1) If WAY ='C':
*>
*>          On entry, contains factorization details in format used in
*>          AB_CSYTRF_ROOK:
*>            a) all elements of the symmetric block diagonal
*>               matrix D on the diagonal of A and on superdiagonal
*>               (or subdiagonal) of A, and
*>            b) If UPLO = 'U': multipliers used to obtain factor U
*>               in the superdiagonal part of A.
*>               If UPLO = 'L': multipliers used to obtain factor L
*>               in the superdiagonal part of A.
*>
*>          On exit, contains factorization details in format used in
*>          AB_CSYTRF_RK or AB_CSYTRF_BK:
*>            a) ONLY diagonal elements of the symmetric block diagonal
*>               matrix D on the diagonal of A, i.e. D(k,k) = A(k,k);
*>               (superdiagonal (or subdiagonal) elements of D
*>                are stored on exit in array E), and
*>            b) If UPLO = 'U': factor U in the superdiagonal part of A.
*>               If UPLO = 'L': factor L in the subdiagonal part of A.
*>
*>          2) If WAY = 'R':
*>
*>          On entry, contains factorization details in format used in
*>          AB_CSYTRF_RK or AB_CSYTRF_BK:
*>            a) ONLY diagonal elements of the symmetric block diagonal
*>               matrix D on the diagonal of A, i.e. D(k,k) = A(k,k);
*>               (superdiagonal (or subdiagonal) elements of D
*>                are stored on exit in array E), and
*>            b) If UPLO = 'U': factor U in the superdiagonal part of A.
*>               If UPLO = 'L': factor L in the subdiagonal part of A.
*>
*>          On exit, contains factorization details in format used in
*>          AB_CSYTRF_ROOK:
*>            a) all elements of the symmetric block diagonal
*>               matrix D on the diagonal of A and on superdiagonal
*>               (or subdiagonal) of A, and
*>            b) If UPLO = 'U': multipliers used to obtain factor U
*>               in the superdiagonal part of A.
*>               If UPLO = 'L': multipliers used to obtain factor L
*>               in the superdiagonal part of A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[in,out] E
*> \verbatim
*>          E is COMPLEX array, dimension (N)
*>
*>          1) If WAY ='C':
*>
*>          On entry, just a workspace.
*>
*>          On exit, contains the superdiagonal (or subdiagonal)
*>          elements of the symmetric block diagonal matrix D
*>          with 1-by-1 or 2-by-2 diagonal blocks, where
*>          If UPLO = 'U': E(i) = D(i-1,i), i=2:N, E(1) is set to 0;
*>          If UPLO = 'L': E(i) = D(i+1,i), i=1:N-1, E(N) is set to 0.
*>
*>          2) If WAY = 'R':
*>
*>          On entry, contains the superdiagonal (or subdiagonal)
*>          elements of the symmetric block diagonal matrix D
*>          with 1-by-1 or 2-by-2 diagonal blocks, where
*>          If UPLO = 'U': E(i) = D(i-1,i),i=2:N, E(1) not referenced;
*>          If UPLO = 'L': E(i) = D(i+1,i),i=1:N-1, E(N) not referenced.
*>
*>          On exit, is not changed
*> \endverbatim
*.
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          On entry, details of the interchanges and the block
*>          structure of D as determined:
*>          1) by AB_CSYTRF_ROOK, if WAY ='C';
*>          2) by AB_CSYTRF_RK (or AB_CSYTRF_BK), if WAY ='R'.
*>          The IPIV format is the same for all these routines.
*>
*>          On exit, is not changed.
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
*> \date November 2017
*
*> \ingroup complexSYcomputational
*
*> \par Contributors:
*  ==================
*>
*> \verbatim
*>
*>  November 2017,  Igor Kozachenko,
*>                  Computer Science Division,
*>                  University of California, Berkeley
*>
*> \endverbatim
*  =====================================================================
      SUBROUTINE AB_CSYCONVF_ROOK( UPLO, WAY, N, A, LDA, E, IPIV, INFO )
*
*  -- LAPACK computational routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO, WAY
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX            A( LDA, * ), E( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX            ZERO
      PARAMETER          ( ZERO = ( 0.0E+0, 0.0E+0 ) )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      EXTERNAL           AB_LSAME
*
*     .. External Subroutines ..
      EXTERNAL           AB_CSWAP, AB_XERBLA
*     .. Local Scalars ..
      LOGICAL            UPPER, CONVERT
      INTEGER            I, IP, IP2
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      UPPER = AB_LSAME( UPLO, 'U' )
      CONVERT = AB_LSAME( WAY, 'C' )
      IF( .NOT.UPPER .AND. .NOT.AB_LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.CONVERT .AND. .NOT.AB_LSAME( WAY, 'R' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5

      END IF
      IF( INFO.NE.0 ) THEN
         CALL AB_XERBLA( 'AB_CSYCONVF_ROOK', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Begin A is UPPER
*
         IF ( CONVERT ) THEN
*
*           Convert A (A is upper)
*
*
*           Convert VALUE
*
*           Assign superdiagonal entries of D to array E and zero out
*           corresponding entries in input storage A
*
            I = N
            E( 1 ) = ZERO
            DO WHILE ( I.GT.1 )
               IF( IPIV( I ).LT.0 ) THEN
                  E( I ) = A( I-1, I )
                  E( I-1 ) = ZERO
                  A( I-1, I ) = ZERO
                  I = I - 1
               ELSE
                  E( I ) = ZERO
               END IF
               I = I - 1
            END DO
*
*           Convert PERMUTATIONS
*
*           Apply permutaions to submatrices of upper part of A
*           in factorization order where i decreases from N to 1
*
            I = N
            DO WHILE ( I.GE.1 )
               IF( IPIV( I ).GT.0 ) THEN
*
*                 1-by-1 pivot interchange
*
*                 Swap rows i and IPIV(i) in A(1:i,N-i:N)
*
                  IP = IPIV( I )
                  IF( I.LT.N ) THEN
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( N-I, A( I, I+1 ), LDA,
     $                              A( IP, I+1 ), LDA )
                     END IF
                  END IF
*
               ELSE
*
*                 2-by-2 pivot interchange
*
*                 Swap rows i and IPIV(i) and i-1 and IPIV(i-1)
*                 in A(1:i,N-i:N)
*
                  IP = -IPIV( I )
                  IP2 = -IPIV( I-1 )
                  IF( I.LT.N ) THEN
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( N-I, A( I, I+1 ), LDA,
     $                              A( IP, I+1 ), LDA )
                     END IF
                     IF( IP2.NE.(I-1) ) THEN
                        CALL AB_CSWAP( N-I, A( I-1, I+1 ), LDA,
     $                              A( IP2, I+1 ), LDA )
                     END IF
                  END IF
                  I = I - 1
*
               END IF
               I = I - 1
            END DO
*
         ELSE
*
*           Revert A (A is upper)
*
*
*           Revert PERMUTATIONS
*
*           Apply permutaions to submatrices of upper part of A
*           in reverse factorization order where i increases from 1 to N
*
            I = 1
            DO WHILE ( I.LE.N )
               IF( IPIV( I ).GT.0 ) THEN
*
*                 1-by-1 pivot interchange
*
*                 Swap rows i and IPIV(i) in A(1:i,N-i:N)
*
                  IP = IPIV( I )
                  IF( I.LT.N ) THEN
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( N-I, A( IP, I+1 ), LDA,
     $                              A( I, I+1 ), LDA )
                     END IF
                  END IF
*
               ELSE
*
*                 2-by-2 pivot interchange
*
*                 Swap rows i-1 and IPIV(i-1) and i and IPIV(i)
*                 in A(1:i,N-i:N)
*
                  I = I + 1
                  IP = -IPIV( I )
                  IP2 = -IPIV( I-1 )
                  IF( I.LT.N ) THEN
                     IF( IP2.NE.(I-1) ) THEN
                        CALL AB_CSWAP( N-I, A( IP2, I+1 ), LDA,
     $                              A( I-1, I+1 ), LDA )
                     END IF
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( N-I, A( IP, I+1 ), LDA,
     $                              A( I, I+1 ), LDA )
                     END IF
                  END IF
*
               END IF
               I = I + 1
            END DO
*
*           Revert VALUE
*           Assign superdiagonal entries of D from array E to
*           superdiagonal entries of A.
*
            I = N
            DO WHILE ( I.GT.1 )
               IF( IPIV( I ).LT.0 ) THEN
                  A( I-1, I ) = E( I )
                  I = I - 1
               END IF
               I = I - 1
            END DO
*
*        End A is UPPER
*
         END IF
*
      ELSE
*
*        Begin A is LOWER
*
         IF ( CONVERT ) THEN
*
*           Convert A (A is lower)
*
*
*           Convert VALUE
*           Assign subdiagonal entries of D to array E and zero out
*           corresponding entries in input storage A
*
            I = 1
            E( N ) = ZERO
            DO WHILE ( I.LE.N )
               IF( I.LT.N .AND. IPIV(I).LT.0 ) THEN
                  E( I ) = A( I+1, I )
                  E( I+1 ) = ZERO
                  A( I+1, I ) = ZERO
                  I = I + 1
               ELSE
                  E( I ) = ZERO
               END IF
               I = I + 1
            END DO
*
*           Convert PERMUTATIONS
*
*           Apply permutaions to submatrices of lower part of A
*           in factorization order where i increases from 1 to N
*
            I = 1
            DO WHILE ( I.LE.N )
               IF( IPIV( I ).GT.0 ) THEN
*
*                 1-by-1 pivot interchange
*
*                 Swap rows i and IPIV(i) in A(i:N,1:i-1)
*
                  IP = IPIV( I )
                  IF ( I.GT.1 ) THEN
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( I-1, A( I, 1 ), LDA,
     $                              A( IP, 1 ), LDA )
                     END IF
                  END IF
*
               ELSE
*
*                 2-by-2 pivot interchange
*
*                 Swap rows i and IPIV(i) and i+1 and IPIV(i+1)
*                 in A(i:N,1:i-1)
*
                  IP = -IPIV( I )
                  IP2 = -IPIV( I+1 )
                  IF ( I.GT.1 ) THEN
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( I-1, A( I, 1 ), LDA,
     $                              A( IP, 1 ), LDA )
                     END IF
                     IF( IP2.NE.(I+1) ) THEN
                        CALL AB_CSWAP( I-1, A( I+1, 1 ), LDA,
     $                              A( IP2, 1 ), LDA )
                     END IF
                  END IF
                  I = I + 1
*
               END IF
               I = I + 1
            END DO
*
         ELSE
*
*           Revert A (A is lower)
*
*
*           Revert PERMUTATIONS
*
*           Apply permutaions to submatrices of lower part of A
*           in reverse factorization order where i decreases from N to 1
*
            I = N
            DO WHILE ( I.GE.1 )
               IF( IPIV( I ).GT.0 ) THEN
*
*                 1-by-1 pivot interchange
*
*                 Swap rows i and IPIV(i) in A(i:N,1:i-1)
*
                  IP = IPIV( I )
                  IF ( I.GT.1 ) THEN
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( I-1, A( IP, 1 ), LDA,
     $                              A( I, 1 ), LDA )
                     END IF
                  END IF
*
               ELSE
*
*                 2-by-2 pivot interchange
*
*                 Swap rows i+1 and IPIV(i+1) and i and IPIV(i)
*                 in A(i:N,1:i-1)
*
                  I = I - 1
                  IP = -IPIV( I )
                  IP2 = -IPIV( I+1 )
                  IF ( I.GT.1 ) THEN
                     IF( IP2.NE.(I+1) ) THEN
                        CALL AB_CSWAP( I-1, A( IP2, 1 ), LDA,
     $                              A( I+1, 1 ), LDA )
                     END IF
                     IF( IP.NE.I ) THEN
                        CALL AB_CSWAP( I-1, A( IP, 1 ), LDA,
     $                              A( I, 1 ), LDA )
                     END IF
                  END IF
*
               END IF
               I = I - 1
            END DO
*
*           Revert VALUE
*           Assign subdiagonal entries of D from array E to
*           subgiagonal entries of A.
*
            I = 1
            DO WHILE ( I.LE.N-1 )
               IF( IPIV( I ).LT.0 ) THEN
                  A( I + 1, I ) = E( I )
                  I = I + 1
               END IF
               I = I + 1
            END DO
*
         END IF
*
*        End A is LOWER
*
      END IF

      RETURN
*
*     End of AB_CSYCONVF_ROOK
*
      END
