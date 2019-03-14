*> \brief \b AB_ZERRST
*
*  @precisions fortran z -> c
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRST( PATH, NUNIT )
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
*> AB_ZERRST tests the error exits for AB_ZHETRD, AB_ZUNGTR, AB_CUNMTR, AB_ZHPTRD,
*> AB_ZUNGTR, AB_ZUPMTR, AB_ZSTEQR, AB_CSTEIN, AB_ZPTEQR, AB_ZHBTRD,
*> AB_ZHEEV, AB_AB_CHEEVX, AB_AB_CHEEVD, AB_ZHBEV, AB_AB_CHBEVX, AB_AB_CHBEVD,
*> AB_ZHPEV, AB_AB_CHPEVX, AB_AB_CHPEVD, and AB_ZSTEDC.
*> AB_AB_AB_ZHEEVD_2STAGE, AB_AB_AB_ZHEEVR_2STAGE, AB_AB_AB_ZHEEVX_2STAGE,
*> AB_AB_ZHEEV_2STAGE, AB_AB_ZHBEV_2STAGE, AB_AB_AB_ZHBEVD_2STAGE,
*> AB_AB_AB_ZHBEVX_2STAGE, AB_AB_ZHETRD_2STAGE
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
*> \date June 2017
*
*> \ingroup complex16_eig
*
*  =====================================================================
      SUBROUTINE AB_ZERRST( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.7.1) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2017
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX, LIW, LW
      PARAMETER          ( NMAX = 3, LIW = 12*NMAX, LW = 20*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, M, N, NT
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW )
      DOUBLE PRECISION   D( NMAX ), E( NMAX ), R( LW ), RW( LW ),
     $                   X( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), C( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), TAU( NMAX ), W( LW ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_ZHBEV, AB_AB_ZHBEVD, AB_AB_ZHBEVX
     $, AB_ZHBTRD, AB_ZHEEV,
     $                   AB_AB_ZHEEVD, AB_AB_ZHEEVR, AB_AB_ZHEEVX, AB_ZH
     $ETRD, AB_ZHPEV, AB_AB_ZHPEVD,
     $                   AB_AB_ZHPEVX, AB_ZHPTRD, AB_ZPTEQR, AB_ZSTEDC, 
     $AB_ZSTEIN, AB_ZSTEQR,
     $                   AB_ZUNGTR, AB_ZUNMTR, AB_ZUPGTR, AB_ZUPMTR,
     $                   AB_AB_AB_ZHEEVD_2STAGE, AB_AB_AB_ZHEEVR_2STAGE,
     $ AB_AB_AB_ZHEEVX_2STAGE,
     $                   AB_AB_ZHEEV_2STAGE, AB_AB_ZHBEV_2STAGE, AB_AB_A
     $B_ZHBEVD_2STAGE,
     $                   AB_AB_AB_ZHBEVX_2STAGE, AB_AB_ZHETRD_2STAGE
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
      INTRINSIC          DBLE
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
   20 CONTINUE
      DO 30 J = 1, NMAX
         D( J ) = DBLE( J )
         E( J ) = 0.0D0
         I1( J ) = J
         I2( J ) = J
         TAU( J ) = 1.D0
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
      IF( AB_AB_LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        AB_ZHETRD
*
         SRNAMT = 'AB_ZHETRD'
         INFOT = 1
         CALL AB_ZHETRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHETRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZHETRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_ZHETRD_2STAGE
*
         SRNAMT = 'AB_AB_ZHETRD_2STAGE'
         INFOT = 1
         CALL AB_AB_ZHETRD_2STAGE( '/', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_ZHETRD_2STAGE( 'H', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRD_2STAGE( 'N', '/', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHETRD_2STAGE( 'N', 'U', -1, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHETRD_2STAGE( 'N', 'U', 2, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHETRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZHETRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_AB_ZHETRD_HE2HB
*
         SRNAMT = 'AB_AB_ZHETRD_HE2HB'
         INFOT = 1
         CALL AB_AB_ZHETRD_HE2HB( '/', 0, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHETRD_HE2HB( 'U', -1, 0, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHETRD_HE2HB( 'U', 0, -1, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHETRD_HE2HB( 'U', 2, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHETRD_HE2HB( 'U', 0, 2, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHETRD_HE2HB( 'U', 0, 0, A, 1, C, 1, TAU, W, 0, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_ZHETRD_HB2ST
*
         SRNAMT = 'AB_ZHETRD_HB2ST'
         INFOT = 1
         CALL AB_ZHETRD_HB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRD_HB2ST( 'Y', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRD_HB2ST( 'Y', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHETRD_HB2ST( 'Y', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHETRD_HB2ST( 'Y', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZHETRD_HB2ST( 'Y', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZHETRD_HB2ST( 'Y', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZHETRD_HB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZHETRD_HB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_ZUNGTR
*
         SRNAMT = 'AB_ZUNGTR'
         INFOT = 1
         CALL AB_ZUNGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZUNGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZUNGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZUNGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZUNGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_ZUNMTR
*
         SRNAMT = 'AB_ZUNMTR'
         INFOT = 1
         CALL AB_ZUNMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZUNMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUNMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZUNMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZUNMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZUNMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZUNMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZUNMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZUNMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZUNMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_ZUNMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_ZHPTRD
*
         SRNAMT = 'AB_ZHPTRD'
         INFOT = 1
         CALL AB_ZHPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_ZHPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        AB_ZUPGTR
*
         SRNAMT = 'AB_ZUPGTR'
         INFOT = 1
         CALL AB_ZUPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZUPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZUPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_ZUPMTR
*
         SRNAMT = 'AB_ZUPMTR'
         INFOT = 1
         CALL AB_ZUPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZUPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZUPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZUPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZUPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZUPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZUPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_ZPTEQR
*
         SRNAMT = 'AB_ZPTEQR'
         INFOT = 1
         CALL AB_ZPTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZPTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZPTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_ZSTEIN
*
         SRNAMT = 'AB_ZSTEIN'
         INFOT = 1
         CALL AB_ZSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, RW, IW, I3, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_ZSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_ZSTEQR
*
         SRNAMT = 'AB_ZSTEQR'
         INFOT = 1
         CALL AB_ZSTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZSTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_ZSTEDC
*
         SRNAMT = 'AB_ZSTEDC'
         INFOT = 1
         CALL AB_ZSTEDC( '/', 0, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSTEDC( 'N', -1, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZSTEDC( 'V', 2, D, E, Z, 1, W, 4, RW, 23, IW, 28, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZSTEDC( 'N', 2, D, E, Z, 1, W, 0, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZSTEDC( 'V', 2, D, E, Z, 2, W, 0, RW, 23, IW, 28, INFO 
     $)
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 1, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 1, IW, 28, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 23, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_ZSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 23, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_ZSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_ZHEEVD
*
         SRNAMT = 'AB_AB_ZHEEVD'
         INFOT = 1
         CALL AB_AB_ZHEEVD( '/', 'U', 0, A, 1, X, W, 1, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHEEVD( 'N', '/', 0, A, 1, X, W, 1, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHEEVD( 'N', 'U', -1, A, 1, X, W, 1, RW, 1, IW, 1, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHEEVD( 'N', 'U', 2, A, 1, X, W, 3, RW, 2, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHEEVD( 'N', 'U', 1, A, 1, X, W, 0, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHEEVD( 'N', 'U', 2, A, 2, X, W, 2, RW, 2, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHEEVD( 'V', 'U', 2, A, 2, X, W, 3, RW, 25, IW, 12, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHEEVD( 'N', 'U', 1, A, 1, X, W, 1, RW, 0, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHEEVD( 'N', 'U', 2, A, 2, X, W, 3, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHEEVD( 'V', 'U', 2, A, 2, X, W, 8, RW, 18, IW, 12, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZHEEVD( 'N', 'U', 1, A, 1, X, W, 1, RW, 1, IW, 0, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZHEEVD( 'V', 'U', 2, A, 2, X, W, 8, RW, 25, IW, 11, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_AB_AB_ZHEEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_ZHEEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_ZHEEVD_2STAGE( '/', 'U', 0, A, 1, X, W, 1,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'V', 'U', 0, A, 1, X, W, 1,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', '/', 0, A, 1, X, W, 1,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', 'U', -1, A, 1, X, W, 1,
     $                               RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', 'U', 2, A, 1, X, W, 3,
     $                              RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 0,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 2,
     $                              RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 8
*         CALL AB_AB_AB_ZHEEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 3,
*     $                            RW, 25, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 1,
     $                              RW, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 25,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 10
*         CALL AB_AB_AB_ZHEEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 8,
*     $                            RW, 18, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_ZHEEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 1,
     $                              RW, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
*         CALL AB_AB_AB_ZHEEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 8,
*     $                            RW, 25, IW, 11, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_ZHEEV
*
         SRNAMT = 'AB_ZHEEV '
         INFOT = 1
         CALL AB_ZHEEV( '/', 'U', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHEEV( 'N', '/', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHEEV( 'N', 'U', -1, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZHEEV( 'N', 'U', 2, A, 1, X, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZHEEV( 'N', 'U', 2, A, 2, X, W, 2, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHEEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        AB_AB_ZHEEV_2STAGE
*
         SRNAMT = 'AB_AB_ZHEEV_2STAGE '
         INFOT = 1
         CALL AB_AB_ZHEEV_2STAGE( '/', 'U', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_ZHEEV_2STAGE( 'V', 'U', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHEEV_2STAGE( 'N', '/', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHEEV_2STAGE( 'N', 'U', -1, A, 1, X, W, 1, RW, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHEEV_2STAGE( 'N', 'U', 2, A, 1, X, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHEEV_2STAGE( 'N', 'U', 2, A, 2, X, W, 2, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_ZHEEVX
*
         SRNAMT = 'AB_AB_ZHEEVX'
         INFOT = 1
         CALL AB_AB_ZHEEVX( '/', 'A', 'U', 0, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHEEVX( 'V', '/', 'U', 0, A, 1, 0.0D0, 1.0D0, 1, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHEEVX( 'V', 'A', '/', 0, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_ZHEEVX( 'V', 'A', 'U', -1, A, 1, 0.0D0, 0.0D0, 0, 0,
     $                0.0D0, M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZHEEVX( 'V', 'A', 'U', 2, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHEEVX( 'V', 'V', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHEEVX( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZHEEVX( 'V', 'I', 'U', 2, A, 2, 0.0D0, 0.0D0, 2, 1, 
     $0.0D0,
     $                M, X, Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZHEEVX( 'V', 'A', 'U', 2, A, 2, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 1, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_ZHEEVX( 'V', 'A', 'U', 2, A, 2, 0.0D0, 0.0D0, 0, 0, 
     $0.0D0,
     $                M, X, Z, 2, W, 2, RW, IW, I1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_AB_ZHEEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_ZHEEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_ZHEEVX_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0D0, 1.0D0, 1, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'A', '/', 0, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 2, 1, 0.0D0,
     $                M, X, Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'A', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 0, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 17
         CALL AB_AB_AB_ZHEEVX_2STAGE( 'N', 'A', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 2, W, 0, RW, IW, I1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 11
*
*        AB_AB_ZHEEVR
*
         SRNAMT = 'AB_AB_ZHEEVR'
         N = 1
         INFOT = 1
         CALL AB_AB_ZHEEVR( '/', 'A', 'U', 0, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHEEVR( 'V', '/', 'U', 0, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHEEVR( 'V', 'A', '/', -1, A, 1, 0.0D0, 0.0D0, 1, 1,
     $                0.0D0, M, R, Z, 1, IW, Q, 2*N, RW, 24*N,
     $                IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHEEVR( 'V', 'A', 'U', -1, A, 1, 0.0D0, 0.0D0, 1, 1,
     $                0.0D0, M, R, Z, 1, IW, Q, 2*N, RW, 24*N,
     $                IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZHEEVR( 'V', 'A', 'U', 2, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHEEVR( 'V', 'V', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 0, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 10
*
         CALL AB_AB_ZHEEVR( 'V', 'I', 'U', 2, A, 2, 0.0D0, 0.0D0, 2, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 0, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N-1, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_ZHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N-1, IW( 2*N-1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL AB_AB_ZHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0D0, 0.0D0, 1, 1, 
     $0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW, 10*N-1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHEEVR', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_AB_AB_ZHEEVR_2STAGE
*
         SRNAMT = 'AB_AB_AB_ZHEEVR_2STAGE'
         N = 1
         INFOT = 1
         CALL AB_AB_AB_ZHEEVR_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'A', '/', -1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N,
     $                IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N,
     $                IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 2, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 0, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 18
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N-1, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 20
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, RW, 24*N-1, IW( 2*N-1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 22
         CALL AB_AB_AB_ZHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, RW, 24*N, IW, 10*N-1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 13
*
*        AB_AB_ZHPEVD
*
         SRNAMT = 'AB_AB_ZHPEVD'
         INFOT = 1
         CALL AB_AB_ZHPEVD( '/', 'U', 0, A, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHPEVD( 'N', '/', 0, A, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHPEVD( 'N', 'U', -1, A, X, Z, 1, W, 1, RW, 1, IW, 1
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHPEVD( 'V', 'U', 2, A, X, Z, 1, W, 4, RW, 25, IW, 1
     $2,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 0, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 1, RW, 2, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 2, RW, 25, IW, 1
     $2,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, RW, 0, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 2, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 4, RW, 18, IW, 1
     $2,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, RW, 1, IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 2, RW, 2, IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 4, RW, 25, IW, 2
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_ZHPEV
*
         SRNAMT = 'AB_ZHPEV '
         INFOT = 1
         CALL AB_ZHPEV( '/', 'U', 0, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHPEV( 'N', '/', 0, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHPEV( 'N', 'U', -1, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZHPEV( 'V', 'U', 2, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHPEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_ZHPEVX
*
         SRNAMT = 'AB_AB_ZHPEVX'
         INFOT = 1
         CALL AB_AB_ZHPEVX( '/', 'A', 'U', 0, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHPEVX( 'V', '/', 'U', 0, A, 0.0D0, 1.0D0, 1, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHPEVX( 'V', 'A', '/', 0, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHPEVX( 'V', 'A', 'U', -1, A, 0.0D0, 0.0D0, 0, 0, 0.
     $0D0,
     $                M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHPEVX( 'V', 'V', 'U', 1, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZHPEVX( 'V', 'I', 'U', 1, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHPEVX( 'V', 'I', 'U', 2, A, 0.0D0, 0.0D0, 2, 1, 0.0
     $D0, M,
     $                X, Z, 2, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_ZHPEVX( 'V', 'A', 'U', 2, A, 0.0D0, 0.0D0, 0, 0, 0.0
     $D0, M,
     $                X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHPEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the HB path.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HB' ) ) THEN
*
*        AB_ZHBTRD
*
         SRNAMT = 'AB_ZHBTRD'
         INFOT = 1
         CALL AB_ZHBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZHBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_ZHBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_ZHBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_ZHETRD_HB2ST
*
         SRNAMT = 'AB_ZHETRD_HB2ST'
         INFOT = 1
         CALL AB_ZHETRD_HB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRD_HB2ST( 'N', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHETRD_HB2ST( 'N', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHETRD_HB2ST( 'N', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHETRD_HB2ST( 'N', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZHETRD_HB2ST( 'N', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZHETRD_HB2ST( 'N', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_ZHETRD_HB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_ZHETRD_HB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_ZHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_ZHBEVD
*
         SRNAMT = 'AB_AB_ZHBEVD'
         INFOT = 1
         CALL AB_AB_ZHBEVD( '/', 'U', 0, 0, A, 1, X, Z, 1, W, 1, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHBEVD( 'N', '/', 0, 0, A, 1, X, Z, 1, W, 1, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHBEVD( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, 1, RW, 1,
     $ IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHBEVD( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, 1, RW, 1,
     $ IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZHBEVD( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 2, RW, 2, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 1, W, 8, RW, 25,
     $ IW,
     $                12, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 0, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 1, RW, 2, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 25,
     $ IW,
     $                12, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, RW, 0, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 8, RW, 2, 
     $IW,
     $                12, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, RW, 1, 
     $IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 2, 
     $IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_ZHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 8, RW, 25,
     $ IW,
     $                2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 15
*
*        AB_AB_AB_ZHBEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_ZHBEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_ZHBEVD_2STAGE( '/', 'U', 0, 0, A, 1, X, Z, 1, 
     $                           W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'V', 'U', 0, 0, A, 1, X, Z, 1, 
     $                           W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', '/', 0, 0, A, 1, X, Z, 1,
     $                           W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', -1, 0, A, 1, X, Z, 1,
     $                            W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 0, -1, A, 1, X, Z, 1,
     $                            W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 2, 1, A, 1, X, Z, 1,
     $                           W, 2, RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 0,
     $                         W, 8, RW, 25, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 11
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1,
     $                           W, 0, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 11
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 2,
     $                           W, 1, RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 11
*         CALL AB_AB_AB_ZHBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 2,
*     $                         W, 2, RW, 25, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1,
     $                           W, 1, RW, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 2,
     $                           W, 25, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 13
*         CALL AB_AB_AB_ZHBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 2,
*     $                          W, 25, RW, 2, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1,
     $                           W, 1, RW, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_ZHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 2,
     $                           W, 25, RW, 2, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 15
*         CALL AB_AB_AB_ZHBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 2,
*     $                          W, 25, RW, 25, IW, 2, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_ZHBEV
*
         SRNAMT = 'AB_ZHBEV '
         INFOT = 1
         CALL AB_ZHBEV( '/', 'U', 0, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZHBEV( 'N', '/', 0, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZHBEV( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZHBEV( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZHBEV( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_ZHBEV( 'V', 'U', 2, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_ZHBEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_ZHBEV_2STAGE
*
         SRNAMT = 'AB_AB_ZHBEV_2STAGE '
         INFOT = 1
         CALL AB_AB_ZHBEV_2STAGE( '/', 'U', 0, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_ZHBEV_2STAGE( 'V', 'U', 0, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHBEV_2STAGE( 'N', '/', 0, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHBEV_2STAGE( 'N', 'U', -1, 0, A, 1, X,
     $                         Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZHBEV_2STAGE( 'N', 'U', 0, -1, A, 1, X,
     $                         Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZHBEV_2STAGE( 'N', 'U', 2, 1, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHBEV_2STAGE( 'N', 'U', 2, 0, A, 1, X,
     $                        Z, 0, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHBEV_2STAGE( 'N', 'U', 2, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_AB_ZHBEVX
*
         SRNAMT = 'AB_AB_ZHBEVX'
         INFOT = 1
         CALL AB_AB_ZHBEVX( '/', 'A', 'U', 0, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZHBEVX( 'V', '/', 'U', 0, 0, A, 1, Q, 1, 0.0D0, 1.0D
     $0, 1,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZHBEVX( 'V', 'A', '/', 0, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_ZHBEVX( 'V', 'A', 'U', -1, 0, A, 1, Q, 1, 0.0D0, 0.0
     $D0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZHBEVX( 'V', 'A', 'U', 0, -1, A, 1, Q, 1, 0.0D0, 0.0
     $D0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZHBEVX( 'V', 'A', 'U', 2, 1, A, 1, Q, 2, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 2, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZHBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 2, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_ZHBEVX( 'V', 'V', 'U', 1, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZHBEVX( 'V', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_ZHBEVX( 'V', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0D0, 0.0D
     $0, 1,
     $                2, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_ZHBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 2, 0.0D0, 0.0D
     $0, 0,
     $                0, 0.0D0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_ZHBEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_AB_ZHBEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_ZHBEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_ZHBEVX_2STAGE( '/', 'A', 'U', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         INFOT = 1
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'V', 'A', 'U', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', '/', 'U', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 1.0D0, 1, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'A', '/', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'A', 'U', -1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'A', 'U', 0, -1, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 7
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'A', 'U', 2, 1, A, 1, Q, 2,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 2, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 9
*         CALL AB_AB_AB_ZHBEVX_2STAGE( 'V', 'A', 'U', 2, 0, A, 1, Q, 1,
*     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
*     $                       M, X, Z, 2, W, 0, RW, IW, I3, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'V', 'U', 1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 1, 2, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 18
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'A', 'U', 2, 0, A, 1, Q, 2,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 0, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 20
         CALL AB_AB_AB_ZHBEVX_2STAGE( 'N', 'A', 'U', 2, 0, A, 1, Q, 2,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 12
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
*     End of AB_ZERRST
*
      END
