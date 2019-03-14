*> \brief \b AB_CQRT15
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CQRT15( SCALE, RKSEL, M, N, NRHS, A, LDA, B, LDB, S,
*                          RANK, NORMA, NORMB, ISEED, WORK, LWORK )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, LDB, LWORK, M, N, NRHS, RANK, RKSEL, SCALE
*       REAL               NORMA, NORMB
*       ..
*       .. Array Arguments ..
*       INTEGER            ISEED( 4 )
*       REAL               S( * )
*       COMPLEX            A( LDA, * ), B( LDB, * ), WORK( LWORK )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CQRT15 generates a matrix with full or deficient rank and of various
*> norms.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SCALE
*> \verbatim
*>          SCALE is INTEGER
*>          SCALE = 1: normally scaled matrix
*>          SCALE = 2: matrix scaled up
*>          SCALE = 3: matrix scaled down
*> \endverbatim
*>
*> \param[in] RKSEL
*> \verbatim
*>          RKSEL is INTEGER
*>          RKSEL = 1: full rank matrix
*>          RKSEL = 2: rank-deficient matrix
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of A.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of columns of B.
*> \endverbatim
*>
*> \param[out] A
*> \verbatim
*>          A is COMPLEX array, dimension (LDA,N)
*>          The M-by-N matrix A.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is COMPLEX array, dimension (LDB, NRHS)
*>          A matrix that is in the range space of matrix A.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is REAL array, dimension MIN(M,N)
*>          Singular values of A.
*> \endverbatim
*>
*> \param[out] RANK
*> \verbatim
*>          RANK is INTEGER
*>          number of nonzero singular values of A.
*> \endverbatim
*>
*> \param[out] NORMA
*> \verbatim
*>          NORMA is REAL
*>          one-norm norm of A.
*> \endverbatim
*>
*> \param[out] NORMB
*> \verbatim
*>          NORMB is REAL
*>          one-norm norm of B.
*> \endverbatim
*>
*> \param[in,out] ISEED
*> \verbatim
*>          ISEED is integer array, dimension (4)
*>          seed for random number generator.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX array, dimension (LWORK)
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          length of work space required.
*>          LWORK >= MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M)
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_CQRT15( SCALE, RKSEL, M, N, NRHS, A, LDA, B, LDB, S,
     $                   RANK, NORMA, NORMB, ISEED, WORK, LWORK )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDB, LWORK, M, N, NRHS, RANK, RKSEL, SCALE
      REAL               NORMA, NORMB
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      REAL               S( * )
      COMPLEX            A( LDA, * ), B( LDB, * ), WORK( LWORK )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE, TWO, SVMIN
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0, TWO = 2.0E+0,
     $                   SVMIN = 0.1E+0 )
      COMPLEX            CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0E+0, 0.0E+0 ),
     $                   CONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, J, MN
      REAL               BIGNUM, EPS, SMLNUM, TEMP
*     ..
*     .. Local Arrays ..
      REAL               DUMMY( 1 )
*     ..
*     .. External Functions ..
      REAL               AB_CLANGE, AB_SASUM, AB_SCNRM2, AB_SLAMCH, AB_S
     $LARND
      EXTERNAL           AB_CLANGE, AB_SASUM, AB_SCNRM2, AB_SLAMCH, AB_S
     $LARND
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CGEMM, AB_CLARF, AB_CLARNV, AB_CLAROR, AB_CL
     $ASCL, AB_CLASET,
     $                   AB_CAB_SSCAL, AB_SLABAD, AB_SLAORD, AB_SLASCL, 
     $AB_XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, CMPLX, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      IF( LWORK.LT.MAX( M+MN, MN*NRHS, 2*N+M ) ) THEN
         CALL AB_XERBLA( 'AB_CQRT15', 16 )
         RETURN
      END IF
*
      SMLNUM = AB_SLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SMLNUM
      CALL AB_SLABAD( SMLNUM, BIGNUM )
      EPS = AB_SLAMCH( 'Epsilon' )
      SMLNUM = ( SMLNUM / EPS ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Determine rank and (unscaled) singular values
*
      IF( RKSEL.EQ.1 ) THEN
         RANK = MN
      ELSE IF( RKSEL.EQ.2 ) THEN
         RANK = ( 3*MN ) / 4
         DO 10 J = RANK + 1, MN
            S( J ) = ZERO
   10    CONTINUE
      ELSE
         CALL AB_XERBLA( 'AB_CQRT15', 2 )
      END IF
*
      IF( RANK.GT.0 ) THEN
*
*        Nontrivial case
*
         S( 1 ) = ONE
         DO 30 J = 2, RANK
   20       CONTINUE
            TEMP = AB_SLARND( 1, ISEED )
            IF( TEMP.GT.SVMIN ) THEN
               S( J ) = ABS( TEMP )
            ELSE
               GO TO 20
            END IF
   30    CONTINUE
         CALL AB_SLAORD( 'Decreasing', RANK, S, 1 )
*
*        Generate 'rank' columns of a random orthogonal matrix in A
*
         CALL AB_CLARNV( 2, ISEED, M, WORK )
         CALL AB_CAB_SSCAL( M, ONE / AB_SCNRM2( M, WORK, 1 ), WORK, 1 )
         CALL AB_CLASET( 'Full', M, RANK, CZERO, CONE, A, LDA )
         CALL AB_CLARF( 'Left', M, RANK, WORK, 1, CMPLX( TWO ), A, LDA,
     $               WORK( M+1 ) )
*
*        workspace used: m+mn
*
*        Generate consistent rhs in the range space of A
*
         CALL AB_CLARNV( 2, ISEED, RANK*NRHS, WORK )
         CALL AB_CGEMM( 'No transpose', 'No transpose', M, NRHS, RANK,
     $               CONE, A, LDA, WORK, RANK, CZERO, B, LDB )
*
*        work space used: <= mn *nrhs
*
*        generate (unscaled) matrix A
*
         DO 40 J = 1, RANK
            CALL AB_CAB_SSCAL( M, S( J ), A( 1, J ), 1 )
   40    CONTINUE
         IF( RANK.LT.N )
     $      CALL AB_CLASET( 'Full', M, N-RANK, CZERO, CZERO,
     $                   A( 1, RANK+1 ), LDA )
         CALL AB_CLAROR( 'Right', 'No initialization', M, N, A, LDA, ISE
     $ED,
     $                WORK, INFO )
*
      ELSE
*
*        work space used 2*n+m
*
*        Generate null matrix and rhs
*
         DO 50 J = 1, MN
            S( J ) = ZERO
   50    CONTINUE
         CALL AB_CLASET( 'Full', M, N, CZERO, CZERO, A, LDA )
         CALL AB_CLASET( 'Full', M, NRHS, CZERO, CZERO, B, LDB )
*
      END IF
*
*     Scale the matrix
*
      IF( SCALE.NE.1 ) THEN
         NORMA = AB_CLANGE( 'Max', M, N, A, LDA, DUMMY )
         IF( NORMA.NE.ZERO ) THEN
            IF( SCALE.EQ.2 ) THEN
*
*              matrix scaled up
*
               CALL AB_CLASCL( 'General', 0, 0, NORMA, BIGNUM, M, N, A,
     $                      LDA, INFO )
               CALL AB_SLASCL( 'General', 0, 0, NORMA, BIGNUM, MN, 1, S,
     $                      MN, INFO )
               CALL AB_CLASCL( 'General', 0, 0, NORMA, BIGNUM, M, NRHS, 
     $B,
     $                      LDB, INFO )
            ELSE IF( SCALE.EQ.3 ) THEN
*
*              matrix scaled down
*
               CALL AB_CLASCL( 'General', 0, 0, NORMA, SMLNUM, M, N, A,
     $                      LDA, INFO )
               CALL AB_SLASCL( 'General', 0, 0, NORMA, SMLNUM, MN, 1, S,
     $                      MN, INFO )
               CALL AB_CLASCL( 'General', 0, 0, NORMA, SMLNUM, M, NRHS, 
     $B,
     $                      LDB, INFO )
            ELSE
               CALL AB_XERBLA( 'AB_CQRT15', 1 )
               RETURN
            END IF
         END IF
      END IF
*
      NORMA = AB_SASUM( MN, S, 1 )
      NORMB = AB_CLANGE( 'One-norm', M, NRHS, B, LDB, DUMMY )
*
      RETURN
*
*     End of AB_CQRT15
*
      END
