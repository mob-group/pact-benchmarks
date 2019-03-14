*> \brief \b AB_ZERRED
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRED( PATH, NUNIT )
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
*> AB_ZERRED tests the error exits for the eigenvalue driver routines for
*> DOUBLE COMPLEX PRECISION matrices:
*>
*> PATH  driver   description
*> ----  ------   -----------
*> ZEV   AB_ZGEEV    find eigenvalues/eigenvectors for nonsymmetric A
*> ZES   AB_ZGEES    find eigenvalues/Schur form for nonsymmetric A
*> ZVX   AB_AB_ZGEEVX   AB_ZGEEV + balancing and condition estimation
*> ZSX   AB_AB_ZGEESX   AB_ZGEES + balancing and condition estimation
*> ZBD   AB_AB_ZGESVD   compute SVD of an M-by-N matrix A
*>       AB_ZGESDD   compute SVD of an M-by-N matrix A(by divide and
*>                conquer)
*>       AB_ZGEJSV   compute SVD of an M-by-N matrix A where M >= N
*>       AB_AB_AB_ZGESVDX  compute SVD of an M-by-N matrix A(by bisection
*>                and inverse iteration)
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
*> \date June 2016
*
*> \ingroup complex16_eig
*
*  =====================================================================
      SUBROUTINE AB_ZERRED( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
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
      PARAMETER          ( NMAX = 4, LW = 5*NMAX )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IHI, ILO, INFO, J, NS, NT, SDIM
      DOUBLE PRECISION   ABNRM
*     ..
*     .. Local Arrays ..
      LOGICAL            B( NMAX )
      INTEGER            IW( 4*NMAX )
      DOUBLE PRECISION   R1( NMAX ), R2( NMAX ), RW( LW ), S( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), U( NMAX, NMAX ),
     $                   VL( NMAX, NMAX ), VR( NMAX, NMAX ),
     $                   VT( NMAX, NMAX ), W( 10*NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_ZGEES, AB_AB_ZGEESX, AB_ZGEEV, AB
     $_AB_ZGEEVX, AB_AB_ZGESVJ,
     $                   AB_ZGESDD, AB_AB_ZGESVD
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN, AB_ZSLECT
      EXTERNAL           AB_AB_LSAMEN, AB_ZSLECT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN_TRIM
*     ..
*     .. Arrays in Common ..
      LOGICAL            SELVAL( 20 )
      DOUBLE PRECISION   SELWI( 20 ), SELWR( 20 )
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NOUT, SELDIM, SELOPT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
      COMMON             / SSLCT / SELOPT, SELDIM, SELVAL, SELWR, SELWI
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Initialize A
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, NMAX
         A( I, I ) = ONE
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
      IF( AB_AB_LSAMEN( 2, C2, 'EV' ) ) THEN
*
*        Test AB_ZGEEV
*
         SRNAMT = 'AB_ZGEEV '
         INFOT = 1
         CALL AB_ZGEEV( 'X', 'N', 0, A, 1, X, VL, 1, VR, 1, W, 1, RW,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGEEV( 'N', 'X', 0, A, 1, X, VL, 1, VR, 1, W, 1, RW,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGEEV( 'N', 'N', -1, A, 1, X, VL, 1, VR, 1, W, 1, RW,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGEEV( 'N', 'N', 2, A, 1, X, VL, 1, VR, 1, W, 4, RW,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGEEV( 'V', 'N', 2, A, 2, X, VL, 1, VR, 1, W, 4, RW,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGEEV( 'N', 'V', 2, A, 2, X, VL, 1, VR, 1, W, 4, RW,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZGEEV( 'V', 'V', 1, A, 1, X, VL, 1, VR, 1, W, 1, RW,
     $               INFO )
         CALL AB_CHKXER( 'AB_ZGEEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'ES' ) ) THEN
*
*        Test AB_ZGEES
*
         SRNAMT = 'AB_ZGEES '
         INFOT = 1
         CALL AB_ZGEES( 'X', 'N', AB_ZSLECT, 0, A, 1, SDIM, X, VL, 1, W,
     $ 1,
     $               RW, B, INFO )
         CALL AB_CHKXER( 'AB_ZGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGEES( 'N', 'X', AB_ZSLECT, 0, A, 1, SDIM, X, VL, 1, W,
     $ 1,
     $               RW, B, INFO )
         CALL AB_CHKXER( 'AB_ZGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGEES( 'N', 'S', AB_ZSLECT, -1, A, 1, SDIM, X, VL, 1, W
     $, 1,
     $               RW, B, INFO )
         CALL AB_CHKXER( 'AB_ZGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGEES( 'N', 'S', AB_ZSLECT, 2, A, 1, SDIM, X, VL, 1, W,
     $ 4,
     $               RW, B, INFO )
         CALL AB_CHKXER( 'AB_ZGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGEES( 'V', 'S', AB_ZSLECT, 2, A, 2, SDIM, X, VL, 1, W,
     $ 4,
     $               RW, B, INFO )
         CALL AB_CHKXER( 'AB_ZGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZGEES( 'N', 'S', AB_ZSLECT, 1, A, 1, SDIM, X, VL, 1, W,
     $ 1,
     $               RW, B, INFO )
         CALL AB_CHKXER( 'AB_ZGEES ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'VX' ) ) THEN
*
*        Test AB_AB_ZGEEVX
*
         SRNAMT = 'AB_AB_ZGEEVX'
         INFOT = 1
         CALL AB_AB_ZGEEVX( 'X', 'N', 'N', 'N', 0, A, 1, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGEEVX( 'N', 'X', 'N', 'N', 0, A, 1, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGEEVX( 'N', 'N', 'X', 'N', 0, A, 1, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGEEVX( 'N', 'N', 'N', 'X', 0, A, 1, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGEEVX( 'N', 'N', 'N', 'N', -1, A, 1, X, VL, 1, VR, 
     $1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGEEVX( 'N', 'N', 'N', 'N', 2, A, 1, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZGEEVX( 'N', 'V', 'N', 'N', 2, A, 2, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZGEEVX( 'N', 'N', 'V', 'N', 2, A, 2, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 4, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_ZGEEVX( 'N', 'N', 'N', 'N', 1, A, 1, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_ZGEEVX( 'N', 'N', 'V', 'V', 1, A, 1, X, VL, 1, VR, 1
     $, ILO,
     $                IHI, S, ABNRM, R1, R2, W, 2, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SX' ) ) THEN
*
*        Test AB_AB_ZGEESX
*
         SRNAMT = 'AB_AB_ZGEESX'
         INFOT = 1
         CALL AB_AB_ZGEESX( 'X', 'N', AB_ZSLECT, 'N', 0, A, 1, SDIM, X, 
     $VL, 1,
     $                R1( 1 ), R2( 1 ), W, 1, RW, B, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGEESX( 'N', 'X', AB_ZSLECT, 'N', 0, A, 1, SDIM, X, 
     $VL, 1,
     $                R1( 1 ), R2( 1 ), W, 1, RW, B, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGEESX( 'N', 'N', AB_ZSLECT, 'X', 0, A, 1, SDIM, X, 
     $VL, 1,
     $                R1( 1 ), R2( 1 ), W, 1, RW, B, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZGEESX( 'N', 'N', AB_ZSLECT, 'N', -1, A, 1, SDIM, X,
     $ VL, 1,
     $                R1( 1 ), R2( 1 ), W, 1, RW, B, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZGEESX( 'N', 'N', AB_ZSLECT, 'N', 2, A, 1, SDIM, X, 
     $VL, 1,
     $                R1( 1 ), R2( 1 ), W, 4, RW, B, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZGEESX( 'V', 'N', AB_ZSLECT, 'N', 2, A, 2, SDIM, X, 
     $VL, 1,
     $                R1( 1 ), R2( 1 ), W, 4, RW, B, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZGEESX( 'N', 'N', AB_ZSLECT, 'N', 1, A, 1, SDIM, X, 
     $VL, 1,
     $                R1( 1 ), R2( 1 ), W, 1, RW, B, INFO )
         CALL AB_CHKXER( 'AB_AB_ZGEESX', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        Test AB_AB_ZGESVD
*
         SRNAMT = 'AB_AB_ZGESVD'
         INFOT = 1
         CALL AB_AB_ZGESVD( 'X', 'N', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGESVD( 'N', 'X', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZGESVD( 'O', 'O', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZGESVD( 'N', 'N', -1, 0, A, 1, S, U, 1, VT, 1, W, 1,
     $ RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZGESVD( 'N', 'N', 0, -1, A, 1, S, U, 1, VT, 1, W, 1,
     $ RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZGESVD( 'N', 'N', 2, 1, A, 1, S, U, 1, VT, 1, W, 5, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZGESVD( 'A', 'N', 2, 1, A, 2, S, U, 1, VT, 1, W, 5, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZGESVD( 'N', 'A', 1, 2, A, 1, S, U, 1, VT, 1, W, 5, 
     $RW,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZGESVD', INFOT, NOUT, LERR, OK )
         NT = NT + 8
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
*
*        Test AB_ZGESDD
*
         SRNAMT = 'AB_ZGESDD'
         INFOT = 1
         CALL AB_ZGESDD( 'X', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, RW, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGESDD( 'N', -1, 0, A, 1, S, U, 1, VT, 1, W, 1, RW, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGESDD( 'N', 0, -1, A, 1, S, U, 1, VT, 1, W, 1, RW, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGESDD( 'N', 2, 1, A, 1, S, U, 1, VT, 1, W, 5, RW, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGESDD( 'A', 2, 1, A, 2, S, U, 1, VT, 1, W, 5, RW, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGESDD( 'A', 1, 2, A, 1, S, U, 1, VT, 1, W, 5, RW, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZGESDD', INFOT, NOUT, LERR, OK )
         NT = NT - 2
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
*
*        Test AB_ZGEJSV
*
         SRNAMT = 'AB_ZGEJSV'
         INFOT = 1
         CALL AB_ZGEJSV( 'X', 'U', 'V', 'R', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZGEJSV( 'G', 'X', 'V', 'R', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZGEJSV( 'G', 'U', 'X', 'R', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'X', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'R', 'X', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'R', 'N', 'X',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 -1, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 0, -1, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 2, 1, A, 1, S, U, 1, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 2, 2, A, 2, S, U, 1, VT, 2,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_ZGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 2, 2, A, 2, S, U, 2, VT, 1,
     $                 W, 1, RW, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_ZGEJSV', INFOT, NOUT, LERR, OK )
         NT = 11
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
*
*        Test AB_AB_AB_ZGESVDX
*
         SRNAMT = 'AB_AB_AB_ZGESVDX'
         INFOT = 1
         CALL AB_AB_AB_ZGESVDX( 'X', 'N', 'A', 0, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZGESVDX( 'N', 'X', 'A', 0, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_ZGESVDX( 'N', 'N', 'X', 0, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZGESVDX( 'N', 'N', 'A', -1, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_AB_ZGESVDX( 'N', 'N', 'A', 0, -1, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_AB_ZGESVDX( 'N', 'N', 'A', 2, 1, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_ZGESVDX( 'N', 'N', 'V', 2, 1, A, 2, -ONE, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_AB_ZGESVDX( 'N', 'N', 'V', 2, 1, A, 2, ONE, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_ZGESVDX( 'N', 'N', 'I', 2, 2, A, 2, ZERO, ZERO,
     $                 0, 1, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_ZGESVDX( 'V', 'N', 'I', 2, 2, A, 2, ZERO, ZERO,
     $                 1, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_ZGESVDX( 'V', 'N', 'A', 2, 2, A, 2, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_AB_ZGESVDX( 'N', 'V', 'A', 2, 2, A, 2, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, RW, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZGESVDX', INFOT, NOUT, LERR, OK )
         NT = 12
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
      END IF
*
*     Print a summary line.
*
      IF( .NOT.AB_AB_LSAMEN( 2, C2, 'BD' ) ) THEN
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
      END IF
*
 9999 FORMAT( 1X, A, ' passed the tests of the error exits (', I3,
     $      ' tests done)' )
 9998 FORMAT( ' *** ', A, ' failed the tests of the error exits ***' )
      RETURN
*
*     End of AB_ZERRED
*
      END
