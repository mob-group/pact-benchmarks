*> \brief \b AB_CERRLS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRLS( PATH, NUNIT )
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
*> AB_CERRLS tests the error exits for the COMPLEX least squares
*> driver routines (AB_CGELS, AB_AB_CGELSS, AB_AB_CGELSY, AB_AB_CGELSD).
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
      SUBROUTINE AB_CERRLS( PATH, NUNIT )
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
      CHARACTER*2        C2
      INTEGER            INFO, IRNK
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               RW( NMAX ), S( NMAX )
      COMPLEX            A( NMAX, NMAX ), B( NMAX, NMAX ), W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CGELS, AB_AB_CGELSD, AB_AB_CGELSS
     $, AB_AB_CGELSY, AB_CHKXER
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
*     .. Executable Statements ..
*
      NOUT = NUNIT
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = ( 1.0E+0, 0.0E+0 )
      A( 1, 2 ) = ( 2.0E+0, 0.0E+0 )
      A( 2, 2 ) = ( 3.0E+0, 0.0E+0 )
      A( 2, 1 ) = ( 4.0E+0, 0.0E+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for the least squares driver routines.
*
      IF( AB_AB_LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        AB_CGELS
*
         SRNAMT = 'AB_CGELS '
         INFOT = 1
         CALL AB_CGELS( '/', 0, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CGELS( 'N', -1, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CGELS( 'N', 0, -1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CGELS( 'N', 0, 0, -1, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CGELS( 'N', 2, 0, 0, A, 1, B, 2, W, 2, INFO )
         CALL AB_CHKXER( 'AB_CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CGELS( 'N', 2, 0, 0, A, 2, B, 1, W, 2, INFO )
         CALL AB_CHKXER( 'AB_CGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CGELS( 'N', 1, 1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CGELS ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CGELSS
*
         SRNAMT = 'AB_AB_CGELSS'
         INFOT = 1
         CALL AB_AB_CGELSS( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGELSS( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGELSS( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGELSS( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 2, R
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CGELSS( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 2, R
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CGELSY
*
         SRNAMT = 'AB_AB_CGELSY'
         INFOT = 1
         CALL AB_AB_CGELSY( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGELSY( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGELSY( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGELSY( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, 10,
     $ RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CGELSY( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, 10,
     $ RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CGELSY( 0, 3, 0, A, 1, B, 3, IP, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSY', INFOT, NOUT, LERR, OK )
*
*        AB_AB_CGELSD
*
         SRNAMT = 'AB_AB_CGELSD'
         INFOT = 1
         CALL AB_AB_CGELSD( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $                RW, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CGELSD( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $                RW, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CGELSD( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $                RW, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CGELSD( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 10,
     $                RW, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CGELSD( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 10,
     $                RW, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CGELSD( 2, 2, 1, A, 2, B, 2, S, RCOND, IRNK, W, 1,
     $                RW, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_CGELSD', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_CERRLS
*
      END
