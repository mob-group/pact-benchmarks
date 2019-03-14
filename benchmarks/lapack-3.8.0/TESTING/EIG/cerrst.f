*> \brief \b AB_CERRST
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_CERRST( PATH, NUNIT )
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
*> AB_CERRST tests the error exits for AB_CHETRD, AB_CUNGTR, AB_CUNMTR, AB_CHPTRD,
*> AB_CUNGTR, AB_CUPMTR, AB_CSTEQR, AB_CSTEIN, AB_CPTEQR, AB_CHBTRD,
*> AB_CHEEV, AB_AB_CHEEVX, AB_AB_CHEEVD, AB_CHBEV, AB_AB_CHBEVX, AB_AB_CHBEVD,
*> AB_CHPEV, AB_AB_CHPEVX, AB_AB_CHPEVD, and AB_CSTEDC.
*> AB_AB_AB_CHEEVD_2STAGE, AB_AB_AB_CHEEVR_2STAGE, AB_AB_AB_CHEEVX_2STAGE,
*> AB_AB_CHEEV_2STAGE, AB_AB_CHBEV_2STAGE, AB_AB_AB_CHBEVD_2STAGE,
*> AB_AB_AB_CHBEVX_2STAGE, AB_AB_CHETRD_2STAGE, AB_AB_CHETRD_HE2HB,
*> AB_CHETRD_HB2ST
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
*> \ingroup complex_eig
*
*  =====================================================================
      SUBROUTINE AB_CERRST( PATH, NUNIT )
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
      REAL               D( NMAX ), E( NMAX ), R( LW ), RW( LW ),
     $                   X( NMAX )
      COMPLEX            A( NMAX, NMAX ), C( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), TAU( NMAX ), W( LW ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHBEV, AB_AB_CHBEVD, AB_AB_CHBEVX, AB_CHBTRD
     $, AB_CHEEV, AB_AB_CHEEVD,
     $                   AB_AB_CHEEVR, AB_AB_CHEEVX, AB_CHETRD, AB_CHKXE
     $R, AB_CHPEV, AB_AB_CHPEVD,
     $                   AB_AB_CHPEVX, AB_CHPTRD, AB_CPTEQR, AB_CSTEDC, 
     $AB_CSTEIN, AB_CSTEQR,
     $                   AB_CUNGTR, AB_CUNMTR, AB_CUPGTR, AB_CUPMTR,
     $                   AB_AB_AB_CHEEVD_2STAGE, AB_AB_AB_CHEEVR_2STAGE,
     $ AB_AB_AB_CHEEVX_2STAGE,
     $                   AB_AB_CHEEV_2STAGE, AB_AB_CHBEV_2STAGE, AB_AB_A
     $B_CHBEVD_2STAGE,
     $                   AB_AB_AB_CHBEVX_2STAGE, AB_AB_CHETRD_2STAGE, AB
     $_AB_CHETRD_HE2HB,
     $                   AB_CHETRD_HB2ST
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
      INTRINSIC          REAL
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
            A( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
   20 CONTINUE
      DO 30 J = 1, NMAX
         D( J ) = REAL( J )
         E( J ) = 0.0
         I1( J ) = J
         I2( J ) = J
         TAU( J ) = 1.
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
      IF( AB_AB_LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        AB_CHETRD
*
         SRNAMT = 'AB_CHETRD'
         INFOT = 1
         CALL AB_CHETRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHETRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CHETRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL AB_CHKXER( 'AB_CHETRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_CHETRD_2STAGE
*
         SRNAMT = 'AB_AB_CHETRD_2STAGE'
         INFOT = 1
         CALL AB_AB_CHETRD_2STAGE( '/', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_CHETRD_2STAGE( 'H', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRD_2STAGE( 'N', '/', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHETRD_2STAGE( 'N', 'U', -1, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHETRD_2STAGE( 'N', 'U', 2, A, 1, D, E, TAU, 
     $                                  C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHETRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CHETRD_2STAGE( 'N', 'U', 0, A, 1, D, E, TAU, 
     $                                  C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_CHETRD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        AB_AB_CHETRD_HE2HB
*
         SRNAMT = 'AB_AB_CHETRD_HE2HB'
         INFOT = 1
         CALL AB_AB_CHETRD_HE2HB( '/', 0, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHETRD_HE2HB( 'U', -1, 0, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_CHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHETRD_HE2HB( 'U', 0, -1, A, 1, C, 1, TAU, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_AB_CHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHETRD_HE2HB( 'U', 2, 0, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHETRD_HE2HB( 'U', 0, 2, A, 1, C, 1, TAU, W, 1, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHETRD_HE2HB( 'U', 0, 0, A, 1, C, 1, TAU, W, 0, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_CHETRD_HE2HB', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_CHETRD_HB2ST
*
         SRNAMT = 'AB_CHETRD_HB2ST'
         INFOT = 1
         CALL AB_CHETRD_HB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRD_HB2ST( 'Y', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRD_HB2ST( 'Y', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHETRD_HB2ST( 'Y', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHETRD_HB2ST( 'Y', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CHETRD_HB2ST( 'Y', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CHETRD_HB2ST( 'Y', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CHETRD_HB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CHETRD_HB2ST( 'Y', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_CUNGTR
*
         SRNAMT = 'AB_CUNGTR'
         INFOT = 1
         CALL AB_CUNGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CUNGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CUNGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CUNGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CUNGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_CUNMTR
*
         SRNAMT = 'AB_CUNMTR'
         INFOT = 1
         CALL AB_CUNMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CUNMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CUNMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CUNMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CUNMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CUNMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CUNMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CUNMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CUNMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CUNMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INF
     $O )
         CALL AB_CHKXER( 'AB_CUNMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_CHPTRD
*
         SRNAMT = 'AB_CHPTRD'
         INFOT = 1
         CALL AB_CHPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_CHPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL AB_CHKXER( 'AB_CHPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        AB_CUPGTR
*
         SRNAMT = 'AB_CUPGTR'
         INFOT = 1
         CALL AB_CUPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CUPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CUPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_CUPMTR
*
         SRNAMT = 'AB_CUPMTR'
         INFOT = 1
         CALL AB_CUPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CUPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CUPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CUPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CUPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CUPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CUPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_CPTEQR
*
         SRNAMT = 'AB_CPTEQR'
         INFOT = 1
         CALL AB_CPTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CPTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CPTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_CSTEIN
*
         SRNAMT = 'AB_CSTEIN'
         INFOT = 1
         CALL AB_CSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO 
     $)
         CALL AB_CHKXER( 'AB_CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, RW, IW, I3, INFO 
     $)
         CALL AB_CHKXER( 'AB_CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_CSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_CSTEQR
*
         SRNAMT = 'AB_CSTEQR'
         INFOT = 1
         CALL AB_CSTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CSTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_CSTEDC
*
         SRNAMT = 'AB_CSTEDC'
         INFOT = 1
         CALL AB_CSTEDC( '/', 0, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CSTEDC( 'N', -1, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CSTEDC( 'V', 2, D, E, Z, 1, W, 4, RW, 23, IW, 28, INFO 
     $)
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CSTEDC( 'N', 2, D, E, Z, 1, W, 0, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CSTEDC( 'V', 2, D, E, Z, 2, W, 0, RW, 23, IW, 28, INFO 
     $)
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 1, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 1, IW, 28, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 23, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_CSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 23, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_CSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_CHEEVD
*
         SRNAMT = 'AB_AB_CHEEVD'
         INFOT = 1
         CALL AB_AB_CHEEVD( '/', 'U', 0, A, 1, X, W, 1, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHEEVD( 'N', '/', 0, A, 1, X, W, 1, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHEEVD( 'N', 'U', -1, A, 1, X, W, 1, RW, 1, IW, 1, I
     $NFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHEEVD( 'N', 'U', 2, A, 1, X, W, 3, RW, 2, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHEEVD( 'N', 'U', 1, A, 1, X, W, 0, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHEEVD( 'N', 'U', 2, A, 2, X, W, 2, RW, 2, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHEEVD( 'V', 'U', 2, A, 2, X, W, 3, RW, 25, IW, 12, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHEEVD( 'N', 'U', 1, A, 1, X, W, 1, RW, 0, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHEEVD( 'N', 'U', 2, A, 2, X, W, 3, RW, 1, IW, 1, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHEEVD( 'V', 'U', 2, A, 2, X, W, 8, RW, 18, IW, 12, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CHEEVD( 'N', 'U', 1, A, 1, X, W, 1, RW, 1, IW, 0, IN
     $FO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CHEEVD( 'V', 'U', 2, A, 2, X, W, 8, RW, 25, IW, 11, 
     $INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_AB_AB_CHEEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_CHEEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_CHEEVD_2STAGE( '/', 'U', 0, A, 1, X, W, 1,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_CHEEVD_2STAGE( 'V', 'U', 0, A, 1, X, W, 1,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', '/', 0, A, 1, X, W, 1,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', 'U', -1, A, 1, X, W, 1,
     $                               RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', 'U', 2, A, 1, X, W, 3,
     $                              RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 0,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 2,
     $                              RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 8
*         CALL AB_AB_AB_CHEEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 3,
*     $                            RW, 25, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 1,
     $                              RW, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', 'U', 2, A, 2, X, W, 25,
     $                              RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 10
*         CALL AB_AB_AB_CHEEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 8,
*     $                            RW, 18, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_AB_CHEEVD_2STAGE( 'N', 'U', 1, A, 1, X, W, 1,
     $                              RW, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
*         CALL AB_AB_AB_CHEEVD_2STAGE( 'V', 'U', 2, A, 2, X, W, 8,
*     $                            RW, 25, IW, 11, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_CHEEVD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_CHEEV
*
         SRNAMT = 'AB_CHEEV '
         INFOT = 1
         CALL AB_CHEEV( '/', 'U', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHEEV( 'N', '/', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHEEV( 'N', 'U', -1, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CHEEV( 'N', 'U', 2, A, 1, X, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_CHEEV( 'N', 'U', 2, A, 2, X, W, 2, RW, INFO )
         CALL AB_CHKXER( 'AB_CHEEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        AB_AB_CHEEV_2STAGE
*
         SRNAMT = 'AB_AB_CHEEV_2STAGE '
         INFOT = 1
         CALL AB_AB_CHEEV_2STAGE( '/', 'U', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_CHEEV_2STAGE( 'V', 'U', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHEEV_2STAGE( 'N', '/', 0, A, 1, X, W, 1, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHEEV_2STAGE( 'N', 'U', -1, A, 1, X, W, 1, RW, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_CHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHEEV_2STAGE( 'N', 'U', 2, A, 1, X, W, 3, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHEEV_2STAGE( 'N', 'U', 2, A, 2, X, W, 2, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_CHEEVX
*
         SRNAMT = 'AB_AB_CHEEVX'
         INFOT = 1
         CALL AB_AB_CHEEVX( '/', 'A', 'U', 0, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHEEVX( 'V', '/', 'U', 0, A, 1, 0.0, 1.0, 1, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHEEVX( 'V', 'A', '/', 0, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_CHEEVX( 'V', 'A', 'U', -1, A, 1, 0.0, 0.0, 0, 0, 0.0
     $, M,
     $                X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CHEEVX( 'V', 'A', 'U', 2, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHEEVX( 'V', 'V', 'U', 1, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHEEVX( 'V', 'I', 'U', 1, A, 1, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_CHEEVX( 'V', 'I', 'U', 2, A, 2, 0.0, 0.0, 2, 1, 0.0,
     $ M, X,
     $                Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CHEEVX( 'V', 'A', 'U', 2, A, 2, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 1, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL AB_AB_CHEEVX( 'V', 'A', 'U', 2, A, 2, 0.0, 0.0, 0, 0, 0.0,
     $ M, X,
     $                Z, 2, W, 2, RW, IW, I1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_AB_AB_CHEEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_CHEEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_CHEEVX_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_CHEEVX_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0D0, 1.0D0, 1, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'A', '/', 0, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 2, 1, 0.0D0,
     $                M, X, Z, 2, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'A', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 0, W, 3, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 17
         CALL AB_AB_AB_CHEEVX_2STAGE( 'N', 'A', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                M, X, Z, 2, W, 0, RW, IW, I1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 11
*
*        AB_AB_CHEEVR
*
         SRNAMT = 'AB_AB_CHEEVR'
         N = 1
         INFOT = 1
         CALL AB_AB_CHEEVR( '/', 'A', 'U', 0, A, 1, 0.0, 0.0, 1, 1, 0.0,
     $ M, R,
     $                Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHEEVR( 'V', '/', 'U', 0, A, 1, 0.0, 0.0, 1, 1, 0.0,
     $ M, R,
     $                Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHEEVR( 'V', 'A', '/', -1, A, 1, 0.0, 0.0, 1, 1, 0.0
     $, M,
     $                R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHEEVR( 'V', 'A', 'U', -1, A, 1, 0.0, 0.0, 1, 1, 0.0
     $, M,
     $                R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CHEEVR( 'V', 'A', 'U', 2, A, 1, 0.0, 0.0, 1, 1, 0.0,
     $ M, R,
     $                Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHEEVR( 'V', 'V', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 0, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 10
*
         CALL AB_AB_CHEEVR( 'V', 'I', 'U', 2, A, 2, 0.0E0, 0.0E0, 2, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 0, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 2*N-1, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL AB_AB_CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N-1, IW( 2*N-1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL AB_AB_CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 
     $0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW, 10*N-1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHEEVR', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        AB_AB_AB_CHEEVR_2STAGE
*
         SRNAMT = 'AB_AB_AB_CHEEVR_2STAGE'
         N = 1
         INFOT = 1
         CALL AB_AB_AB_CHEEVR_2STAGE( '/', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_CHEEVR_2STAGE( 'V', 'A', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', '/', 'U', 0, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'A', '/', -1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N,
     $                IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'A', 'U', -1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N,
     $                IW( 2*N+1 ), 10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'A', 'U', 2, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 8
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'V', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 0, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 10
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'I', 'U', 2, A, 2,
     $                0.0D0, 0.0D0, 2, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 0, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 18
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 2*N-1, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 20
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, RW, 24*N-1, IW( 2*N-1 ),
     $                10*N, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 22
         CALL AB_AB_AB_CHEEVR_2STAGE( 'N', 'I', 'U', 1, A, 1,
     $                0.0D0, 0.0D0, 1, 1, 0.0D0,
     $                M, R, Z, 1, IW, Q, 26*N, RW, 24*N, IW, 10*N-1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHEEVR_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         NT = NT + 13
*
*        AB_AB_CHPEVD
*
         SRNAMT = 'AB_AB_CHPEVD'
         INFOT = 1
         CALL AB_AB_CHPEVD( '/', 'U', 0, A, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHPEVD( 'N', '/', 0, A, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHPEVD( 'N', 'U', -1, A, X, Z, 1, W, 1, RW, 1, IW, 1
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHPEVD( 'V', 'U', 2, A, X, Z, 1, W, 4, RW, 25, IW, 1
     $2,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 0, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 1, RW, 2, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 2, RW, 25, IW, 1
     $2,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, RW, 0, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 2, RW, 1, IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 4, RW, 18, IW, 1
     $2,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, RW, 1, IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 2, RW, 2, IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 4, RW, 25, IW, 2
     $,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_CHPEV
*
         SRNAMT = 'AB_CHPEV '
         INFOT = 1
         CALL AB_CHPEV( '/', 'U', 0, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHPEV( 'N', '/', 0, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHPEV( 'N', 'U', -1, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CHPEV( 'V', 'U', 2, A, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHPEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_AB_CHPEVX
*
         SRNAMT = 'AB_AB_CHPEVX'
         INFOT = 1
         CALL AB_AB_CHPEVX( '/', 'A', 'U', 0, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHPEVX( 'V', '/', 'U', 0, A, 0.0, 1.0, 1, 0, 0.0, M,
     $ X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHPEVX( 'V', 'A', '/', 0, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHPEVX( 'V', 'A', 'U', -1, A, 0.0, 0.0, 0, 0, 0.0, M
     $, X,
     $                Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHPEVX( 'V', 'V', 'U', 1, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_CHPEVX( 'V', 'I', 'U', 1, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHPEVX( 'V', 'I', 'U', 2, A, 0.0, 0.0, 2, 1, 0.0, M,
     $ X, Z,
     $                2, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_AB_CHPEVX( 'V', 'A', 'U', 2, A, 0.0, 0.0, 0, 0, 0.0, M,
     $ X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHPEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the HB path.
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'HB' ) ) THEN
*
*        AB_CHBTRD
*
         SRNAMT = 'AB_CHBTRD'
         INFOT = 1
         CALL AB_CHBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CHBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_CHBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL AB_CHKXER( 'AB_CHBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_CHETRD_HB2ST
*
         SRNAMT = 'AB_CHETRD_HB2ST'
         INFOT = 1
         CALL AB_CHETRD_HB2ST( '/', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRD_HB2ST( 'N', '/', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHETRD_HB2ST( 'N', 'H', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHETRD_HB2ST( 'N', 'N', '/', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHETRD_HB2ST( 'N', 'N', 'U', -1, 0, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_CHETRD_HB2ST( 'N', 'N', 'U', 0, -1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_CHETRD_HB2ST( 'N', 'N', 'U', 0, 1, A, 1, D, E, 
     $                                    C, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_CHETRD_HB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 0, W, 1, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_CHETRD_HB2ST( 'N', 'N', 'U', 0, 0, A, 1, D, E, 
     $                                    C, 1, W, 0, INFO )
         CALL AB_CHKXER( 'AB_CHETRD_HB2ST', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        AB_AB_CHBEVD
*
         SRNAMT = 'AB_AB_CHBEVD'
         INFOT = 1
         CALL AB_AB_CHBEVD( '/', 'U', 0, 0, A, 1, X, Z, 1, W, 1, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHBEVD( 'N', '/', 0, 0, A, 1, X, Z, 1, W, 1, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHBEVD( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, 1, RW, 1,
     $ IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHBEVD( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, 1, RW, 1,
     $ IW,
     $                1, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CHBEVD( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 2, RW, 2, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 1, W, 8, RW, 25,
     $ IW,
     $                12, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 0, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 1, RW, 2, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 25,
     $ IW,
     $                12, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, RW, 0, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 1, 
     $IW, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 8, RW, 2, 
     $IW,
     $                12, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, RW, 1, 
     $IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 2, 
     $IW, 0,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 8, RW, 25,
     $ IW,
     $                2, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 15
*
*        AB_AB_AB_CHBEVD_2STAGE
*
         SRNAMT = 'AB_AB_AB_CHBEVD_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_CHBEVD_2STAGE( '/', 'U', 0, 0, A, 1, X, Z, 1, 
     $                           W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 1
         CALL AB_AB_AB_CHBEVD_2STAGE( 'V', 'U', 0, 0, A, 1, X, Z, 1, 
     $                           W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', '/', 0, 0, A, 1, X, Z, 1,
     $                           W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', -1, 0, A, 1, X, Z, 1,
     $                            W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 4
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 0, -1, A, 1, X, Z, 1,
     $                            W, 1, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 6
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 2, 1, A, 1, X, Z, 1,
     $                           W, 2, RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 9
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 0,
     $                         W, 8, RW, 25, IW, 12, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 11
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1,
     $                           W, 0, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 11
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 2,
     $                           W, 1, RW, 2, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 11
*         CALL AB_AB_AB_CHBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 2,
*     $                         W, 2, RW, 25, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1,
     $                           W, 1, RW, 0, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 2,
     $                           W, 25, RW, 1, IW, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 13
*         CALL AB_AB_AB_CHBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 2,
*     $                          W, 25, RW, 2, IW, 12, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 1, 0, A, 1, X, Z, 1,
     $                           W, 1, RW, 1, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 15
         CALL AB_AB_AB_CHBEVD_2STAGE( 'N', 'U', 2, 1, A, 2, X, Z, 2,
     $                           W, 25, RW, 2, IW, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 15
*         CALL AB_AB_AB_CHBEVD_2STAGE( 'V', 'U', 2, 1, A, 2, X, Z, 2,
*     $                          W, 25, RW, 25, IW, 2, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_CHBEVD_2STAGE', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_CHBEV
*
         SRNAMT = 'AB_CHBEV '
         INFOT = 1
         CALL AB_CHBEV( '/', 'U', 0, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_CHBEV( 'N', '/', 0, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_CHBEV( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_CHBEV( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_CHBEV( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_CHBEV( 'V', 'U', 2, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL AB_CHKXER( 'AB_CHBEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        AB_AB_CHBEV_2STAGE
*
         SRNAMT = 'AB_AB_CHBEV_2STAGE '
         INFOT = 1
         CALL AB_AB_CHBEV_2STAGE( '/', 'U', 0, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 1
         CALL AB_AB_CHBEV_2STAGE( 'V', 'U', 0, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHBEV_2STAGE( 'N', '/', 0, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHBEV_2STAGE( 'N', 'U', -1, 0, A, 1, X,
     $                         Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_CHBEV_2STAGE( 'N', 'U', 0, -1, A, 1, X,
     $                         Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_CHBEV_2STAGE( 'N', 'U', 2, 1, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHBEV_2STAGE( 'N', 'U', 2, 0, A, 1, X,
     $                        Z, 0, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHBEV_2STAGE( 'N', 'U', 2, 0, A, 1, X,
     $                        Z, 1, W, 0, RW, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEV_2STAGE ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_AB_CHBEVX
*
         SRNAMT = 'AB_AB_CHBEVX'
         INFOT = 1
         CALL AB_AB_CHBEVX( '/', 'A', 'U', 0, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_CHBEVX( 'V', '/', 'U', 0, 0, A, 1, Q, 1, 0.0, 1.0, 1
     $, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_CHBEVX( 'V', 'A', '/', 0, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_CHBEVX( 'V', 'A', 'U', -1, 0, A, 1, Q, 1, 0.0, 0.0, 
     $0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_CHBEVX( 'V', 'A', 'U', 0, -1, A, 1, Q, 1, 0.0, 0.0, 
     $0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_CHBEVX( 'V', 'A', 'U', 2, 1, A, 1, Q, 2, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 2, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_CHBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 2, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_CHBEVX( 'V', 'V', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_CHBEVX( 'V', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_AB_CHBEVX( 'V', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 1
     $, 2,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL AB_AB_CHBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 2, 0.0, 0.0, 0
     $, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_CHBEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        AB_AB_AB_CHBEVX_2STAGE
*
         SRNAMT = 'AB_AB_AB_CHBEVX_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_CHBEVX_2STAGE( '/', 'A', 'U', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         INFOT = 1
         CALL AB_AB_AB_CHBEVX_2STAGE( 'V', 'A', 'U', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 2
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', '/', 'U', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 1.0D0, 1, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 3
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'A', '/', 0, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         INFOT = 4
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'A', 'U', -1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 5
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'A', 'U', 0, -1, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 7
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'A', 'U', 2, 1, A, 1, Q, 2,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 2, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
*         INFOT = 9
*         CALL AB_AB_AB_CHBEVX_2STAGE( 'V', 'A', 'U', 2, 0, A, 1, Q, 1,
*     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
*     $                       M, X, Z, 2, W, 0, RW, IW, I3, INFO )
*         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'V', 'U', 1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 12
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 13
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'I', 'U', 1, 0, A, 1, Q, 1,
     $                       0.0D0, 0.0D0, 1, 2, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 18
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'A', 'U', 2, 0, A, 1, Q, 2,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 0, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
     $ )
         INFOT = 20
         CALL AB_AB_AB_CHBEVX_2STAGE( 'N', 'A', 'U', 2, 0, A, 1, Q, 2,
     $                       0.0D0, 0.0D0, 0, 0, 0.0D0,
     $                       M, X, Z, 1, W, 0, RW, IW, I3, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_CHBEVX_2STAGE', INFOT, NOUT, LERR, OK
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
*     End of AB_CERRST
*
      END
