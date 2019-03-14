*> \brief \b AB_AB_CERRQRT
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_AB_CERRQRT( PATH, NUNIT )
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
*> AB_AB_CERRQRT tests the error exits for the COMPLEX routines
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
*> \ingroup complex_lin
*
*  =====================================================================
      SUBROUTINE AB_AB_CERRQRT( PATH, NUNIT )
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
      COMPLEX            A( NMAX, NMAX ), T( NMAX, NMAX ), W( NMAX ),
     $                   C( NMAX, NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_AB_CGEQRT2, AB_AB_C
     $GEQRT3, AB_AB_CGEQRT,
     $                   AB_AB_CGEMQRT
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
      INTRINSIC          FLOAT, CMPLX
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
            A( I, J ) = 1.0 / CMPLX( FLOAT(I+J), 0.0 )
            C( I, J ) = 1.0 / CMPLX( FLOAT(I+J), 0.0 )
            T( I, J ) = 1.0 / CMPLX( FLOAT(I+J), 0.0 )
         END DO
         W( J ) = 0.0
      END DO
      OK = .TRUE.
*
*     Error exits for QRT factorization
*
*     AB_AB_CGEQRT
*
      SRNAMT = 'AB_AB_CGEQRT'
      INFOT = 1
      CALL AB_AB_CGEQRT( -1, 0, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_CGEQRT( 0, -1, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_CGEQRT( 0, 0, 0, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_CGEQRT( 2, 1, 1, A, 1, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_CGEQRT( 2, 2, 2, A, 2, T, 1, W, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT', INFOT, NOUT, LERR, OK )
*
*     AB_AB_AB_CGEQRT2
*
      SRNAMT = 'AB_AB_AB_CGEQRT2'
      INFOT = 1
      CALL AB_AB_AB_CGEQRT2( -1, 0, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRT2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_AB_CGEQRT2( 0, -1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRT2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_AB_CGEQRT2( 2, 1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRT2', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_AB_CGEQRT2( 2, 2, A, 2, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_AB_CGEQRT2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_CGEQRT3
*
      SRNAMT = 'AB_AB_CGEQRT3'
      INFOT = 1
      CALL AB_AB_CGEQRT3( -1, 0, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT3', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_CGEQRT3( 0, -1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT3', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_CGEQRT3( 2, 1, A, 1, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT3', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_CGEQRT3( 2, 2, A, 2, T, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_CGEQRT3', INFOT, NOUT, LERR, OK )
*
*     AB_AB_CGEMQRT
*
      SRNAMT = 'AB_AB_CGEMQRT'
      INFOT = 1
      CALL AB_AB_CGEMQRT( '/', 'N', 0, 0, 0, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_CGEMQRT( 'L', '/', 0, 0, 0, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_CGEMQRT( 'L', 'N', -1, 0, 0, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_CGEMQRT( 'L', 'N', 0, -1, 0, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_CGEMQRT( 'L', 'N', 0, 0, -1, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_CGEMQRT( 'R', 'N', 0, 0, -1, 1, A, 1, T, 1, C, 1, W, IN
     $FO )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_AB_CGEMQRT( 'L', 'N', 0, 0, 0, 0, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_CGEMQRT( 'R', 'N', 1, 2, 1, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_CGEMQRT( 'L', 'N', 2, 1, 1, 1, A, 1, T, 1, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_CGEMQRT( 'R', 'N', 1, 1, 1, 1, A, 1, T, 0, C, 1, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_AB_CGEMQRT( 'L', 'N', 1, 1, 1, 1, A, 1, T, 1, C, 0, W, INF
     $O )
      CALL AB_CHKXER( 'AB_AB_CGEMQRT', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_AB_CERRQRT
*
      END
