*> \brief \b AB_AB_ZERRQRT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_ZERRQRT( PATH, NUNIT )
*
*       .. Scalar Arguments ..
*       CHARACTER*3        PATH
*       INTEGER            NUNIT
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_AB_ZERRQRT tests the error exits for the COMPLEX*16 routines
*> that use the QRT decomposition of a general matrix.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] PATH
*> \verbatim
*>          PATH is CHARACTER*3
*>          The LAPACK path name for the routines to be tested.
*> \endverbatim
*>
*> \param[in] NUNIT
*> \verbatim
*>          NUNIT is INTEGER
*>          The unit number for output.
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
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE AB_AB_ZERRQRT( PATH, NUNIT )
      IMPLICIT NONE
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX
      PARAMETER          ( NMAX = 2 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
*     ..
*     .. Local Arrays ..
      COMPLEX*16         A( NMAX, NMAX ), T( NMAX, NMAX ), W( NMAX ),
     $                   C( NMAX, NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_AB_ZGEQRT2, AB_AB_Z
     $GEQRT3, AB_AB_ZGEQRT,
     $                   AB_AB_ZGEMQRT
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
*
*     Set the variables to innocuous values.
*
      DO J = 1, NMAX
         DO I = 1, NMAX
            A( I, J ) = 1.D0 / DCMPLX( DBLE( I+J ), 0.D0 )
            C( I, J ) = 1.D0 / DCMPLX( DBLE( I+J ), 0.D0 )
            T( I, J ) = 1.D0 / DCMPLX( DBLE( I+J ), 0.D0 )
         END DO
         W( J ) = 0.D0
      END DO
      OK = .TRUE.
*
*     Error exits for QRT factorization
*
*     AB_AB_ZGEQRT
*
      SRNAMT = 'AB_AB_ZGEQRT'
      INFOT = 1
      CALL AB_AB_ZGEQRT( -1, 0, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGEQRT( 0, -1, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_ZGEQRT( 0, 0, 0, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_ZGEQRT( 2, 1, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_ZGEQRT( 2, 2, 2, A, 2, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_ZGEQRT2
*
      SRNAMT = 'AB_AB_AB_ZGEQRT2'
      INFOT = 1
      CALL AB_AB_AB_ZGEQRT2( -1, 0, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRT2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_ZGEQRT2( 0, -1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRT2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_ZGEQRT2( 2, 1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRT2', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_AB_ZGEQRT2( 2, 2, A, 2, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_ZGEQRT2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_ZGEQRT3
*
      SRNAMT = 'AB_AB_ZGEQRT3'
      INFOT = 1
      CALL AB_AB_ZGEQRT3( -1, 0, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT3', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGEQRT3( 0, -1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT3', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_ZGEQRT3( 2, 1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT3', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_ZGEQRT3( 2, 2, A, 2, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_ZGEQRT3', INFOT, NOUT, LERR, OK )
*
*     AB_AB_ZGEMQRT
*
      SRNAMT = 'AB_AB_ZGEMQRT'
      INFOT = 1
      CALL AB_AB_ZGEMQRT( '/', 'N', 0, 0, 0, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_ZGEMQRT( 'L', '/', 0, 0, 0, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_ZGEMQRT( 'L', 'N', -1, 0, 0, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_ZGEMQRT( 'L', 'N', 0, -1, 0, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_ZGEMQRT( 'L', 'N', 0, 0, -1, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_ZGEMQRT( 'R', 'N', 0, 0, -1, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_ZGEMQRT( 'L', 'N', 0, 0, 0, 0, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_ZGEMQRT( 'R', 'N', 1, 2, 1, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_ZGEMQRT( 'L', 'N', 2, 1, 1, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_ZGEMQRT( 'R', 'N', 1, 1, 1, 1, A, 1, T, 0, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_AB_ZGEMQRT( 'L', 'N', 1, 1, 1, 1, A, 1, T, 1, C, 0, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_ZGEMQRT', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_AB_ZERRQRT
*
      END
