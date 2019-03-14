*> \brief \b AB_DERRHS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRHS( PATH, NUNIT )
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
*> AB_DERRHS tests the error exits for AB_DGEBAK, AB_SGEBAL, AB_SGEHRD, AB_DORGHR,
*> AB_DORMHR, AB_DHSEQR, AB_SHSEIN, and AB_DTREVC.
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
*> \ingroup double_eig
*
*  =====================================================================
      SUBROUTINE AB_DERRHS( PATH, NUNIT )
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
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 3, LW = ( NMAX+2 )*( NMAX+2 )+NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IHI, ILO, INFO, J, M, NT
*     ..
*     .. Local Arrays ..
      LOGICAL            SEL( NMAX )
      INTEGER            IFAILL( NMAX ), IFAILR( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), C( NMAX, NMAX ), S( NMAX ),
     $                   TAU( NMAX ), VL( NMAX, NMAX ),
     $                   VR( NMAX, NMAX ), W( LW ), WI( NMAX ),
     $                   WR( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_DGEBAK, AB_DGEBAL, AB_DGEHRD, AB_
     $DHSEIN, AB_DHSEQR,
     $                   AB_DORGHR, AB_DORMHR, AB_DTREVC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
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
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
         WI( J ) = DBLE( J )
         SEL( J ) = .TRUE.
   20 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits of the nonsymmetric eigenvalue routines.
*
      IF( AB_AB_LSAMEN( 2, C2, 'HS' ) ) THEN
*
*        AB_DGEBAL
*
         SRNAMT = 'AB_DGEBAL'
         INFOT = 1
         CALL AB_DGEBAL( '/', 0, A, 1, ILO, IHI, S, INFO )
         CALL AB_CHKXER( 'AB_DGEBAL', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEBAL( 'N', -1, A, 1, ILO, IHI, S, INFO )
         CALL AB_CHKXER( 'AB_DGEBAL', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGEBAL( 'N', 2, A, 1, ILO, IHI, S, INFO )
         CALL AB_CHKXER( 'AB_DGEBAL', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_DGEBAK
*
         SRNAMT = 'AB_DGEBAK'
         INFOT = 1
         CALL AB_DGEBAK( '/', 'R', 0, 1, 0, S, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEBAK( 'N', '/', 0, 1, 0, S, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGEBAK( 'N', 'R', -1, 1, 0, S, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGEBAK( 'N', 'R', 0, 0, 0, S, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGEBAK( 'N', 'R', 0, 2, 0, S, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGEBAK( 'N', 'R', 2, 2, 1, S, 0, A, 2, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGEBAK( 'N', 'R', 0, 1, 1, S, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DGEBAK( 'N', 'R', 0, 1, 0, S, -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DGEBAK( 'N', 'R', 2, 1, 2, S, 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEBAK', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_DGEHRD
*
         SRNAMT = 'AB_DGEHRD'
         INFOT = 1
         CALL AB_DGEHRD( -1, 1, 1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEHRD( 0, 0, 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEHRD( 0, 2, 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGEHRD( 1, 1, 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGEHRD( 0, 1, 1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGEHRD( 2, 1, 1, A, 1, TAU, W, 2, INFO )
         CALL AB_CHKXER( 'AB_DGEHRD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DGEHRD( 2, 1, 2, A, 2, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DGEHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_DORGHR
*
         SRNAMT = 'AB_DORGHR'
         INFOT = 1
         CALL AB_DORGHR( -1, 1, 1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGHR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DORGHR( 0, 0, 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGHR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DORGHR( 0, 2, 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGHR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DORGHR( 1, 1, 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGHR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DORGHR( 0, 1, 1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGHR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DORGHR( 2, 1, 1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGHR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DORGHR( 3, 1, 3, A, 3, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_DORGHR', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_DORMHR
*
         SRNAMT = 'AB_DORMHR'
         INFOT = 1
         CALL AB_DORMHR( '/', 'N', 0, 0, 1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DORMHR( 'L', '/', 0, 0, 1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DORMHR( 'L', 'N', -1, 0, 1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DORMHR( 'L', 'N', 0, -1, 1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DORMHR( 'L', 'N', 0, 0, 0, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DORMHR( 'L', 'N', 0, 0, 2, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DORMHR( 'L', 'N', 1, 2, 2, 1, A, 1, TAU, C, 1, W, 2,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DORMHR( 'R', 'N', 2, 1, 2, 1, A, 1, TAU, C, 2, W, 2,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DORMHR( 'L', 'N', 1, 1, 1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DORMHR( 'L', 'N', 0, 1, 1, 1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DORMHR( 'R', 'N', 1, 0, 1, 1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DORMHR( 'L', 'N', 2, 1, 1, 1, A, 1, TAU, C, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DORMHR( 'R', 'N', 1, 2, 1, 1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DORMHR( 'L', 'N', 2, 1, 1, 1, A, 2, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DORMHR( 'L', 'N', 1, 2, 1, 1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DORMHR( 'R', 'N', 2, 1, 1, 1, A, 1, TAU, C, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DORMHR', INFOT, NOUT, LERR, OK )
         NT = NT + 16
*
*        AB_DHSEQR
*
         SRNAMT = 'AB_DHSEQR'
         INFOT = 1
         CALL AB_DHSEQR( '/', 'N', 0, 1, 0, A, 1, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DHSEQR( 'E', '/', 0, 1, 0, A, 1, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DHSEQR( 'E', 'N', -1, 1, 0, A, 1, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DHSEQR( 'E', 'N', 0, 0, 0, A, 1, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DHSEQR( 'E', 'N', 0, 2, 0, A, 1, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DHSEQR( 'E', 'N', 1, 1, 0, A, 1, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DHSEQR( 'E', 'N', 1, 1, 2, A, 1, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DHSEQR( 'E', 'N', 2, 1, 2, A, 1, WR, WI, C, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DHSEQR( 'E', 'V', 2, 1, 2, A, 2, WR, WI, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_DHSEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_DHSEIN
*
         SRNAMT = 'AB_DHSEIN'
         INFOT = 1
         CALL AB_DHSEIN( '/', 'N', 'N', SEL, 0, A, 1, WR, WI, VL, 1, VR,
     $ 1,
     $                0, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DHSEIN( 'R', '/', 'N', SEL, 0, A, 1, WR, WI, VL, 1, VR,
     $ 1,
     $                0, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DHSEIN( 'R', 'N', '/', SEL, 0, A, 1, WR, WI, VL, 1, VR,
     $ 1,
     $                0, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DHSEIN( 'R', 'N', 'N', SEL, -1, A, 1, WR, WI, VL, 1, VR
     $,
     $                1, 0, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DHSEIN( 'R', 'N', 'N', SEL, 2, A, 1, WR, WI, VL, 1, VR,
     $ 2,
     $                4, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DHSEIN( 'L', 'N', 'N', SEL, 2, A, 2, WR, WI, VL, 1, VR,
     $ 1,
     $                4, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DHSEIN( 'R', 'N', 'N', SEL, 2, A, 2, WR, WI, VL, 1, VR,
     $ 1,
     $                4, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_DHSEIN( 'R', 'N', 'N', SEL, 2, A, 2, WR, WI, VL, 1, VR,
     $ 2,
     $                1, M, W, IFAILL, IFAILR, INFO )
         CALL AB_CHKXER( 'AB_DHSEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_DTREVC
*
         SRNAMT = 'AB_DTREVC'
         INFOT = 1
         CALL AB_DTREVC( '/', 'A', SEL, 0, A, 1, VL, 1, VR, 1, 0, M, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTREVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DTREVC( 'L', '/', SEL, 0, A, 1, VL, 1, VR, 1, 0, M, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTREVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DTREVC( 'L', 'A', SEL, -1, A, 1, VL, 1, VR, 1, 0, M, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTREVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DTREVC( 'L', 'A', SEL, 2, A, 1, VL, 2, VR, 1, 4, M, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTREVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DTREVC( 'L', 'A', SEL, 2, A, 2, VL, 1, VR, 1, 4, M, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTREVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DTREVC( 'R', 'A', SEL, 2, A, 2, VL, 1, VR, 1, 4, M, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTREVC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DTREVC( 'L', 'A', SEL, 2, A, 2, VL, 2, VR, 1, 1, M, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_DTREVC', INFOT, NOUT, LERR, OK )
         NT = NT + 7
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH, NT
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits',
     $      ' (', I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of AB_DERRHS
*
      END
