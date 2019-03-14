*> \brief \b AB_DERRLS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRLS( PATH, NUNIT )
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
*> AB_DERRLS tests the error exits for the DOUBLE PRECISION least squares
*> driver routines (AB_DGELS, AB_AB_SGELSS, AB_AB_SGELSY, AB_AB_SGELSD).
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DERRLS( PATH, NUNIT )
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
      DOUBLE PRECISION   A( NMAX, NMAX ), B( NMAX, NMAX ), S( NMAX ),
     $                   W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DGELS, AB_AB_DGELSD, A
     $B_AB_DGELSS, AB_AB_DGELSY
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
      A( 1, 1 ) = 1.0D+0
      A( 1, 2 ) = 2.0D+0
      A( 2, 2 ) = 3.0D+0
      A( 2, 1 ) = 4.0D+0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        Test error exits for the least squares driver routines.
*
*        AB_DGELS
*
         SRNAMT = 'AB_DGELS '
         INFOT = 1
         CALL AB_DGELS( '/', 0, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGELS( 'N', -1, 0, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGELS( 'N', 0, -1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGELS( 'N', 0, 0, -1, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGELS( 'N', 2, 0, 0, A, 1, B, 2, W, 2, INFO )
         CALL AB_CHKXER( 'AB_DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DGELS( 'N', 2, 0, 0, A, 2, B, 1, W, 2, INFO )
         CALL AB_CHKXER( 'AB_DGELS ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DGELS( 'N', 1, 1, 0, A, 1, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGELS ', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGELSS
*
         SRNAMT = 'AB_AB_DGELSS'
         INFOT = 1
         CALL AB_AB_DGELSS( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGELSS( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGELSS( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DGELSS( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 2, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DGELSS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DGELSS( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 2, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_DGELSS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGELSY
*
         SRNAMT = 'AB_AB_DGELSY'
         INFOT = 1
         CALL AB_AB_DGELSY( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGELSY( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGELSY( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, 10
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DGELSY( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, 10,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DGELSY( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, 10,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSY', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DGELSY( 2, 2, 1, A, 2, B, 2, IP, RCOND, IRNK, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSY', INFOT, NOUT, LERR, OK )
*
*        AB_AB_DGELSD
*
         SRNAMT = 'AB_AB_DGELSD'
         INFOT = 1
         CALL AB_AB_DGELSD( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $ IP,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGELSD( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $ IP,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGELSD( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 10,
     $ IP,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DGELSD( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 10, 
     $IP,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DGELSD( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 10, 
     $IP,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DGELSD( 2, 2, 1, A, 2, B, 2, S, RCOND, IRNK, W, 1, I
     $P,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGELSD', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRLS
*
      END
