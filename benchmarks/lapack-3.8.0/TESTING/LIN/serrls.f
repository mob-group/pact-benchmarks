*> \brief \b AB_SERRLS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRLS( PATH, NUNIT )
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
*> AB_SERRLS tests the error exits for the REAL least squares
*> driver routines (AB_SGELS, AB_AB_SGELSS, AB_AB_SGELSY, AB_AB_SGELSD).
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE AB_SERRLS( PATH, NUNIT )
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
      REAL               A( NMAX, NMAX ), B( NMAX, NMAX ), S( NMAX ),
     $                   W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_SGELS, AB_AB_SGELSD, A
     $B_AB_SGELSS, AB_AB_SGELSY
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
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = 1.0E+0
      A( 1, 2 ) = 2.0E+0
      A( 2, 2 ) = 3.0E+0
      A( 2, 1 ) = 4.0E+0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        Test error exits for the least squares driver routines.
*
*        AB_SGELS
*
         SRNAMT = 'AB_SGELS '
         INFOT = 1
         CALL AB_SGELS( '/', 0, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGELS( 'N', -1, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SGELS( 'N', 0, -1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGELS( 'N', 0, 0, -1, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SGELS( 'N', 2, 0, 0, A, 1, B, 2, W, 2, INFO )
         CALL AB_CHKXER( 'AB_SGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SGELS( 'N', 2, 0, 0, A, 2, B, 1, W, 2, INFO )
         CALL AB_CHKXER( 'AB_SGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SGELS( 'N', 1, 1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGELS ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SGELSS
*
         SRNAMT = 'AB_AB_SGELSS'
         INFOT = 1
         CALL AB_AB_SGELSS( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SGELSS( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SGELSS( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SGELSS( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 2, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SGELSS( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 2, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_SGELSS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SGELSY
*
         SRNAMT = 'AB_AB_SGELSY'
         INFOT = 1
         CALL AB_AB_SGELSY( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SGELSY( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SGELSY( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SGELSY( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, 10,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SGELSY( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, 10,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SGELSY( 2, 2, 1, A, 2, B, 2, IP, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSY', INFOT, NOUT, LERR, OK )
*
*        AB_AB_SGELSD
*
         SRNAMT = 'AB_AB_SGELSD'
         INFOT = 1
         CALL AB_AB_SGELSD( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_SGELSD( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_SGELSD( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_SGELSD( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 10,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_SGELSD( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 10,
     $                IP, INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_SGELSD( 2, 2, 1, A, 2, B, 2, S, RCOND, IRNK, W, 1, I
     $P,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_SGELSD', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRLS
*
      END
