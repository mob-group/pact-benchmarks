*> \brief \b AB_ZERRLS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRLS( PATH, NUNIT )
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
*> AB_ZERRLS tests the error exits for the COMPLEX*16 least squares
*> driver routines (AB_ZGELS, AB_AB_CGELSS, AB_AB_CGELSY, AB_AB_CGELSD).
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
      SUBROUTINE AB_ZERRLS( PATH, NUNIT )
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
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   RW( NMAX ), S( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), B( NMAX, NMAX ), W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZGELS, AB_AB_ZGELSD, A
     $B_AB_ZGELSS, AB_AB_ZGELSY
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
      A( 1, 1 ) = ( 1.0D+0, 0.0D+0 )
      A( 1, 2 ) = ( 2.0D+0, 0.0D+0 )
      A( 2, 2 ) = ( 3.0D+0, 0.0D+0 )
      A( 2, 1 ) = ( 4.0D+0, 0.0D+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for the least squares driver routines.
*
      IF( AB_AB_LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        AB_ZGELS
*
         SRNAMT = 'AB_ZGELS '
         INFOT = 1
         CALL AB_ZGELS( '/', 0, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGELS( 'N', -1, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGELS( 'N', 0, -1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGELS( 'N', 0, 0, -1, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGELS( 'N', 2, 0, 0, A, 1, B, 2, W, 2, INFO )
         CALL AB_CHKXER( 'AB_ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGELS( 'N', 2, 0, 0, A, 2, B, 1, W, 2, INFO )
         CALL AB_CHKXER( 'AB_ZGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGELS( 'N', 1, 1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZGELS ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGELSS
*
         SRNAMT = 'AB_AB_ZGELSS'
         INFOT = 1
         CALL AB_AB_ZGELSS( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGELSS( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGELSS( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGELSS( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 2, R
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGELSS( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 2, R
     $W,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGELSY
*
         SRNAMT = 'AB_AB_ZGELSY'
         INFOT = 1
         CALL AB_AB_ZGELSY( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGELSY( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGELSY( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $, RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGELSY( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, 10,
     $ RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGELSY( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, 10,
     $ RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZGELSY( 0, 3, 0, A, 1, B, 3, IP, RCOND, IRNK, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSY', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZGELSD
*
         SRNAMT = 'AB_AB_ZGELSD'
         INFOT = 1
         CALL AB_AB_ZGELSD( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $ RW,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGELSD( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $ RW,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGELSD( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $ RW,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGELSD( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 10, 
     $RW,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGELSD( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 10, 
     $RW,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZGELSD( 2, 2, 1, A, 2, B, 2, S, RCOND, IRNK, W, 1, R
     $W, IP,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGELSD', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRLS
*
      END
