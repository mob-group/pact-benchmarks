*> \brief \b AB_DERRED
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRED( PATH, NUNIT )
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
*> AB_DERRED tests the error exits for the eigenvalue driver routines for
*> DOUBLE PRECISION matrices:
*>
*> PATH  driver   description
*> ----  ------   -----------
*> SEV   AB_DGEEV    find eigenvalues/eigenvectors for nonsymmetric A
*> SES   AB_DGEES    find eigenvalues/Schur form for nonsymmetric A
*> SVX   AB_AB_DGEEVX   AB_SGEEV + balancing and condition estimation
*> SSX   AB_AB_DGEESX   AB_SGEES + balancing and condition estimation
*> DBD   AB_AB_DGESVD   compute SVD of an M-by-N matrix A
*>       AB_DGESDD   compute SVD of an M-by-N matrix A (by divide and
*>                conquer)
*>       AB_DGEJSV   compute SVD of an M-by-N matrix A where M >= N
*>       AB_AB_AB_DGESVDX  compute SVD of an M-by-N matrix A(by bisection
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
*> \ingroup double_eig
*
*  =====================================================================
      SUBROUTINE AB_DERRED( PATH, NUNIT )
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
      INTEGER            NMAX
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( NMAX = 4, ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IHI, ILO, INFO, J, NS, NT, SDIM
      DOUBLE PRECISION   ABNRM
*     ..
*     .. Local Arrays ..
      LOGICAL            B( NMAX )
      INTEGER            IW( 2*NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), R1( NMAX ), R2( NMAX ),
     $                   S( NMAX ), U( NMAX, NMAX ), VL( NMAX, NMAX ),
     $                   VR( NMAX, NMAX ), VT( NMAX, NMAX ),
     $                   W( 10*NMAX ), WI( NMAX ), WR( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_DGEES, AB_AB_DGEESX, AB_DGEEV, AB
     $_AB_DGEEVX, AB_DGEJSV,
     $                   AB_DGESDD, AB_AB_DGESVD
*     ..
*     .. External Functions ..
      LOGICAL            AB_DSLECT, AB_AB_LSAMEN
      EXTERNAL           AB_DSLECT, AB_AB_LSAMEN
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
*        Test AB_DGEEV
*
         SRNAMT = 'AB_DGEEV '
         INFOT = 1
         CALL AB_DGEEV( 'X', 'N', 0, A, 1, WR, WI, VL, 1, VR, 1, W, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEEV( 'N', 'X', 0, A, 1, WR, WI, VL, 1, VR, 1, W, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGEEV( 'N', 'N', -1, A, 1, WR, WI, VL, 1, VR, 1, W, 1,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGEEV( 'N', 'N', 2, A, 1, WR, WI, VL, 1, VR, 1, W, 6,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_DGEEV( 'V', 'N', 2, A, 2, WR, WI, VL, 1, VR, 1, W, 8,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DGEEV( 'N', 'V', 2, A, 2, WR, WI, VL, 1, VR, 1, W, 8,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DGEEV( 'V', 'V', 1, A, 1, WR, WI, VL, 1, VR, 1, W, 3,
     $               INFO )
         CALL AB_CHKXER( 'AB_DGEEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'ES' ) ) THEN
*
*        Test AB_DGEES
*
         SRNAMT = 'AB_DGEES '
         INFOT = 1
         CALL AB_DGEES( 'X', 'N', AB_DSLECT, 0, A, 1, SDIM, WR, WI, VL, 
     $1, W,
     $               1, B, INFO )
         CALL AB_CHKXER( 'AB_DGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEES( 'N', 'X', AB_DSLECT, 0, A, 1, SDIM, WR, WI, VL, 
     $1, W,
     $               1, B, INFO )
         CALL AB_CHKXER( 'AB_DGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGEES( 'N', 'S', AB_DSLECT, -1, A, 1, SDIM, WR, WI, VL,
     $ 1, W,
     $               1, B, INFO )
         CALL AB_CHKXER( 'AB_DGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGEES( 'N', 'S', AB_DSLECT, 2, A, 1, SDIM, WR, WI, VL, 
     $1, W,
     $               6, B, INFO )
         CALL AB_CHKXER( 'AB_DGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_DGEES( 'V', 'S', AB_DSLECT, 2, A, 2, SDIM, WR, WI, VL, 
     $1, W,
     $               6, B, INFO )
         CALL AB_CHKXER( 'AB_DGEES ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DGEES( 'N', 'S', AB_DSLECT, 1, A, 1, SDIM, WR, WI, VL, 
     $1, W,
     $               2, B, INFO )
         CALL AB_CHKXER( 'AB_DGEES ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'VX' ) ) THEN
*
*        Test AB_AB_DGEEVX
*
         SRNAMT = 'AB_AB_DGEEVX'
         INFOT = 1
         CALL AB_AB_DGEEVX( 'X', 'N', 'N', 'N', 0, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGEEVX( 'N', 'X', 'N', 'N', 0, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGEEVX( 'N', 'N', 'X', 'N', 0, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGEEVX( 'N', 'N', 'N', 'X', 0, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DGEEVX( 'N', 'N', 'N', 'N', -1, A, 1, WR, WI, VL, 1,
     $ VR,
     $                1, ILO, IHI, S, ABNRM, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DGEEVX( 'N', 'N', 'N', 'N', 2, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DGEEVX( 'N', 'V', 'N', 'N', 2, A, 2, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 6, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_DGEEVX( 'N', 'N', 'V', 'N', 2, A, 2, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 6, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL AB_AB_DGEEVX( 'N', 'N', 'N', 'N', 1, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL AB_AB_DGEEVX( 'N', 'V', 'N', 'N', 1, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 2, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL AB_AB_DGEEVX( 'N', 'N', 'V', 'V', 1, A, 1, WR, WI, VL, 1, 
     $VR, 1,
     $                ILO, IHI, S, ABNRM, R1, R2, W, 3, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SX' ) ) THEN
*
*        Test AB_AB_DGEESX
*
         SRNAMT = 'AB_AB_DGEESX'
         INFOT = 1
         CALL AB_AB_DGEESX( 'X', 'N', AB_DSLECT, 'N', 0, A, 1, SDIM, WR,
     $ WI, VL,
     $                1, R1( 1 ), R2( 1 ), W, 1, IW, 1, B, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGEESX( 'N', 'X', AB_DSLECT, 'N', 0, A, 1, SDIM, WR,
     $ WI, VL,
     $                1, R1( 1 ), R2( 1 ), W, 1, IW, 1, B, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGEESX( 'N', 'N', AB_DSLECT, 'X', 0, A, 1, SDIM, WR,
     $ WI, VL,
     $                1, R1( 1 ), R2( 1 ), W, 1, IW, 1, B, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_DGEESX( 'N', 'N', AB_DSLECT, 'N', -1, A, 1, SDIM, WR
     $, WI, VL,
     $                1, R1( 1 ), R2( 1 ), W, 1, IW, 1, B, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_DGEESX( 'N', 'N', AB_DSLECT, 'N', 2, A, 1, SDIM, WR,
     $ WI, VL,
     $                1, R1( 1 ), R2( 1 ), W, 6, IW, 1, B, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_DGEESX( 'V', 'N', AB_DSLECT, 'N', 2, A, 2, SDIM, WR,
     $ WI, VL,
     $                1, R1( 1 ), R2( 1 ), W, 6, IW, 1, B, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEESX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL AB_AB_DGEESX( 'N', 'N', AB_DSLECT, 'N', 1, A, 1, SDIM, WR,
     $ WI, VL,
     $                1, R1( 1 ), R2( 1 ), W, 2, IW, 1, B, INFO )
         CALL AB_CHKXER( 'AB_AB_DGEESX', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        Test AB_AB_DGESVD
*
         SRNAMT = 'AB_AB_DGESVD'
         INFOT = 1
         CALL AB_AB_DGESVD( 'X', 'N', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGESVD( 'N', 'X', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_DGESVD( 'O', 'O', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_DGESVD( 'N', 'N', -1, 0, A, 1, S, U, 1, VT, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_DGESVD( 'N', 'N', 0, -1, A, 1, S, U, 1, VT, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_DGESVD( 'N', 'N', 2, 1, A, 1, S, U, 1, VT, 1, W, 5, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_DGESVD( 'A', 'N', 2, 1, A, 2, S, U, 1, VT, 1, W, 5, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_DGESVD( 'N', 'A', 1, 2, A, 1, S, U, 1, VT, 1, W, 5, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_DGESVD', INFOT, NOUT, LERR, OK )
         NT = 8
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
*
*        Test AB_DGESDD
*
         SRNAMT = 'AB_DGESDD'
         INFOT = 1
         CALL AB_DGESDD( 'X', 0, 0, A, 1, S, U, 1, VT, 1, W, 1, IW, INFO
     $ )
         CALL AB_CHKXER( 'AB_DGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGESDD( 'N', -1, 0, A, 1, S, U, 1, VT, 1, W, 1, IW, INF
     $O )
         CALL AB_CHKXER( 'AB_DGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGESDD( 'N', 0, -1, A, 1, S, U, 1, VT, 1, W, 1, IW, INF
     $O )
         CALL AB_CHKXER( 'AB_DGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGESDD( 'N', 2, 1, A, 1, S, U, 1, VT, 1, W, 5, IW, INFO
     $ )
         CALL AB_CHKXER( 'AB_DGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DGESDD( 'A', 2, 1, A, 2, S, U, 1, VT, 1, W, 5, IW, INFO
     $ )
         CALL AB_CHKXER( 'AB_DGESDD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DGESDD( 'A', 1, 2, A, 1, S, U, 1, VT, 1, W, 5, IW, INFO
     $ )
         CALL AB_CHKXER( 'AB_DGESDD', INFOT, NOUT, LERR, OK )
         NT = 6
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
*
*        Test AB_DGEJSV
*
         SRNAMT = 'AB_DGEJSV'
         INFOT = 1
         CALL AB_DGEJSV( 'X', 'U', 'V', 'R', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_DGEJSV( 'G', 'X', 'V', 'R', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_DGEJSV( 'G', 'U', 'X', 'R', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_DGEJSV( 'G', 'U', 'V', 'X', 'N', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_DGEJSV( 'G', 'U', 'V', 'R', 'X', 'N',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_DGEJSV( 'G', 'U', 'V', 'R', 'N', 'X',
     $                 0, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_DGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 -1, 0, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_DGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 0, -1, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_DGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 2, 1, A, 1, S, U, 1, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_DGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 2, 2, A, 2, S, U, 1, VT, 2,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_DGEJSV( 'G', 'U', 'V', 'R', 'N', 'N',
     $                 2, 2, A, 2, S, U, 2, VT, 1,
     $                 W, 1, IW, INFO)
         CALL AB_CHKXER( 'AB_DGEJSV', INFOT, NOUT, LERR, OK )
         NT = 11
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT( 1:LEN_TRIM( SRNAMT ) ),
     $           NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
*
*        Test AB_AB_AB_DGESVDX
*
         SRNAMT = 'AB_AB_AB_DGESVDX'
         INFOT = 1
         CALL AB_AB_AB_DGESVDX( 'X', 'N', 'A', 0, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_DGESVDX( 'N', 'X', 'A', 0, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_AB_DGESVDX( 'N', 'N', 'X', 0, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_DGESVDX( 'N', 'N', 'A', -1, 0, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_AB_DGESVDX( 'N', 'N', 'A', 0, -1, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_AB_DGESVDX( 'N', 'N', 'A', 2, 1, A, 1, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_AB_DGESVDX( 'N', 'N', 'V', 2, 1, A, 2, -ONE, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_AB_DGESVDX( 'N', 'N', 'V', 2, 1, A, 2, ONE, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_DGESVDX( 'N', 'N', 'I', 2, 2, A, 2, ZERO, ZERO,
     $                 0, 1, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_DGESVDX( 'V', 'N', 'I', 2, 2, A, 2, ZERO, ZERO,
     $                 1, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_DGESVDX( 'V', 'N', 'A', 2, 2, A, 2, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_AB_DGESVDX( 'N', 'V', 'A', 2, 2, A, 2, ZERO, ZERO,
     $                 0, 0, NS, S, U, 1, VT, 1, W, 1, IW, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_DGESVDX', INFOT, NOUT, LERR, OK )
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
*     End of AB_DERRED
      END
