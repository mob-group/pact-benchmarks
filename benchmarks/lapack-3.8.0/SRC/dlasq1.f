*> \brief \b AB_DLASQ1 computes the singular values of a real square bidiagonal matrix. Used by AB_SBDSQR.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_DLASQ1 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_DLASQ1.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_DLASQ1.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_DLASQ1.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DLASQ1( N, D, E, WORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   D( * ), E( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DLASQ1 computes the singular values of a real N-by-N bidiagonal
*> matrix with diagonal D and off-diagonal E. The singular values
*> are computed to high relative accuracy, in the absence of
*> denormalization, underflow and overflow. The algorithm was first
*> presented in
*>
*> "Accurate singular values and differential qd algorithms" by K. V.
*> Fernando and B. N. Parlett, Numer. Math., Vol-67, No. 2, pp. 191-230,
*> 1994,
*>
*> and the present implementation is described in "An implementation of
*> the dqds Algorithm (Positive Case)", LAPACK Working Note.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>        The number of rows and columns in the matrix. N >= 0.
*> \endverbatim
*>
*> \param[in,out] D
*> \verbatim
*>          D is DOUBLE PRECISION array, dimension (N)
*>        On entry, D contains the diagonal elements of the
*>        bidiagonal matrix whose SVD is desired. On normal exit,
*>        D contains the singular values in decreasing order.
*> \endverbatim
*>
*> \param[in,out] E
*> \verbatim
*>          E is DOUBLE PRECISION array, dimension (N)
*>        On entry, elements E(1:N-1) contain the off-diagonal elements
*>        of the bidiagonal matrix whose SVD is desired.
*>        On exit, E is overwritten.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (4*N)
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>        = 0: successful exit
*>        < 0: if INFO = -i, the i-th argument had an illegal value
*>        > 0: the algorithm failed
*>             = 1, a split was marked by a positive value in E
*>             = 2, current block of Z not diagonalized after 100*N
*>                  iterations (in inner while loop)  On exit D and E
*>                  represent a matrix with the same singular values
*>                  which the calling subroutine could use to finish the
*>                  computation, or even feed back into AB_DLASQ1
*>             = 3, termination criterion of outer while loop not met
*>                  (program created more than N unreduced blocks)
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
*> \ingroup auxOTHERcomputational
*
*  =====================================================================
      SUBROUTINE AB_DLASQ1( N, D, E, WORK, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IINFO
      DOUBLE PRECISION   EPS, SCALE, SAFMIN, SIGMN, SIGMX
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_DCOPY, AB_DLAS2, AB_DLASCL, AB_DLASQ2, AB_AB
     $_DLASRT, AB_XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH
      EXTERNAL           AB_DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL AB_XERBLA( 'AB_DLASQ1', -INFO )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         D( 1 ) = ABS( D( 1 ) )
         RETURN
      ELSE IF( N.EQ.2 ) THEN
         CALL AB_DLAS2( D( 1 ), E( 1 ), D( 2 ), SIGMN, SIGMX )
         D( 1 ) = SIGMX
         D( 2 ) = SIGMN
         RETURN
      END IF
*
*     Estimate the largest singular value.
*
      SIGMX = ZERO
      DO 10 I = 1, N - 1
         D( I ) = ABS( D( I ) )
         SIGMX = MAX( SIGMX, ABS( E( I ) ) )
   10 CONTINUE
      D( N ) = ABS( D( N ) )
*
*     Early return if SIGMX is zero (matrix is already diagonal).
*
      IF( SIGMX.EQ.ZERO ) THEN
         CALL AB_AB_DLASRT( 'D', N, D, IINFO )
         RETURN
      END IF
*
      DO 20 I = 1, N
         SIGMX = MAX( SIGMX, D( I ) )
   20 CONTINUE
*
*     Copy D and E into WORK (in the Z format) and scale (squaring the
*     input data makes scaling by a power of the radix pointless).
*
      EPS = AB_DLAMCH( 'Precision' )
      SAFMIN = AB_DLAMCH( 'Safe minimum' )
      SCALE = SQRT( EPS / SAFMIN )
      CALL AB_DCOPY( N, D, 1, WORK( 1 ), 2 )
      CALL AB_DCOPY( N-1, E, 1, WORK( 2 ), 2 )
      CALL AB_DLASCL( 'G', 0, 0, SIGMX, SCALE, 2*N-1, 1, WORK, 2*N-1,
     $             IINFO )
*
*     Compute the q's and e's.
*
      DO 30 I = 1, 2*N - 1
         WORK( I ) = WORK( I )**2
   30 CONTINUE
      WORK( 2*N ) = ZERO
*
      CALL AB_DLASQ2( N, WORK, INFO )
*
      IF( INFO.EQ.0 ) THEN
         DO 40 I = 1, N
            D( I ) = SQRT( WORK( I ) )
   40    CONTINUE
         CALL AB_DLASCL( 'G', 0, 0, SCALE, SIGMX, N, 1, D, N, IINFO )
      ELSE IF( INFO.EQ.2 ) THEN
*
*     Maximum number of iterations exceeded.  Move data from WORK
*     into D and E so the calling subroutine can try to finish
*
         DO I = 1, N
            D( I ) = SQRT( WORK( 2*I-1 ) )
            E( I ) = SQRT( WORK( 2*I ) )
         END DO
         CALL AB_DLASCL( 'G', 0, 0, SCALE, SIGMX, N, 1, D, N, IINFO )
         CALL AB_DLASCL( 'G', 0, 0, SCALE, SIGMX, N, 1, E, N, IINFO )
      END IF
*
      RETURN
*
*     End of AB_DLASQ1
*
      END
