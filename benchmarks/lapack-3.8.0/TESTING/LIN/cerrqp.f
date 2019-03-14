*> \brief \b AB_CERRQP
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRQP( PATH, NUNIT )
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
*> AB_CERRQP tests the error exits for AB_CGEQP3.
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
      SUBROUTINE AB_CERRQP( PATH, NUNIT )
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
      PARAMETER          ( NMAX = 3 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            INFO, LW
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               RW( 2*NMAX )
      COMPLEX            A( NMAX, NMAX ), TAU( NMAX ),
     $                   W( 2*NMAX+3*NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CGEQP3, AB_CHKXER
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
      INTRINSIC          CMPLX
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      C2 = PATH( 2: 3 )
      LW = NMAX + 1
      A( 1, 1 ) = CMPLX( 1.0E+0, -1.0E+0 )
      A( 1, 2 ) = CMPLX( 2.0E+0, -2.0E+0 )
      A( 2, 2 ) = CMPLX( 3.0E+0, -3.0E+0 )
      A( 2, 1 ) = CMPLX( 4.0E+0, -4.0E+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for QR factorization with pivoting
*
      IF( AB_AB_LSAMEN( 2, C2, 'QP' ) ) THEN
*
*        AB_CGEQP3
*
         SRNAMT = 'AB_CGEQP3'
         INFOT = 1
         CALL AB_CGEQP3( -1, 0, A, 1, IP, TAU, W, LW, RW, INFO )
         CALL AB_CHKXER( 'AB_CGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGEQP3( 1, -1, A, 1, IP, TAU, W, LW, RW, INFO )
         CALL AB_CHKXER( 'AB_CGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGEQP3( 2, 3, A, 1, IP, TAU, W, LW, RW, INFO )
         CALL AB_CHKXER( 'AB_CGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CGEQP3( 2, 2, A, 2, IP, TAU, W, LW-10, RW, INFO )
         CALL AB_CHKXER( 'AB_CGEQP3', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRQP
*
      END
