*> \brief \b AB_SERRTR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRTR( PATH, NUNIT )
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
*> AB_SERRTR tests the error exits for the REAL triangular
*> routines.
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
      SUBROUTINE AB_SERRTR( PATH, NUNIT )
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
      INTEGER            INFO
      REAL               RCOND, SCALE
*     ..
*     .. Local Arrays ..
      INTEGER            IW( NMAX )
      REAL               A( NMAX, NMAX ), B( NMAX ), R1( NMAX ),
     $                   R2( NMAX ), W( NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_SLATBS, AB_SLATPS, AB_
     $SLATRS, AB_STBCON,
     $                   AB_STBRFS, AB_STBTRS, AB_STPCON, AB_STPRFS, AB_
     $STPTRI, AB_STPTRS,
     $                   AB_STRCON, AB_STRRFS, AB_STRTI2, AB_STRTRI, AB_
     $STRTRS
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
      A( 1, 1 ) = 1.
      A( 1, 2 ) = 2.
      A( 2, 2 ) = 3.
      A( 2, 1 ) = 4.
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'TR' ) ) THEN
*
*        Test error exits for the general triangular routines.
*
*        AB_STRTRI
*
         SRNAMT = 'AB_STRTRI'
         INFOT = 1
         CALL AB_STRTRI( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STRTRI( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STRTRI( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRI', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STRTRI( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRI', INFOT, NOUT, LERR, OK )
*
*        AB_STRTI2
*
         SRNAMT = 'AB_STRTI2'
         INFOT = 1
         CALL AB_STRTI2( '/', 'N', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STRTI2( 'U', '/', 0, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STRTI2( 'U', 'N', -1, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTI2', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STRTI2( 'U', 'N', 2, A, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTI2', INFOT, NOUT, LERR, OK )
*
*        AB_STRTRS
*
         SRNAMT = 'AB_STRTRS'
         INFOT = 1
         CALL AB_STRTRS( '/', 'N', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STRTRS( 'U', '/', 'N', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STRTRS( 'U', 'N', '/', 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STRTRS( 'U', 'N', 'N', -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STRTRS( 'U', 'N', 'N', 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_STRTRS( 'U', 'N', 'N', 2, 1, A, 1, X, 2, INFO )
         CALL AB_CHKXER( 'AB_STRTRS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_STRTRS( 'U', 'N', 'N', 2, 1, A, 2, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STRTRS', INFOT, NOUT, LERR, OK )
*
*        AB_STRRFS
*
         SRNAMT = 'AB_STRRFS'
         INFOT = 1
         CALL AB_STRRFS( '/', 'N', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STRRFS( 'U', '/', 'N', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STRRFS( 'U', 'N', '/', 0, 0, A, 1, B, 1, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STRRFS( 'U', 'N', 'N', -1, 0, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STRRFS( 'U', 'N', 'N', 0, -1, A, 1, B, 1, X, 1, R1, R2,
     $ W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_STRRFS( 'U', 'N', 'N', 2, 1, A, 1, B, 2, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_STRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 1, X, 2, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_STRRFS( 'U', 'N', 'N', 2, 1, A, 2, B, 2, X, 1, R1, R2, 
     $W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STRRFS', INFOT, NOUT, LERR, OK )
*
*        AB_STRCON
*
         SRNAMT = 'AB_STRCON'
         INFOT = 1
         CALL AB_STRCON( '/', 'U', 'N', 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STRCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STRCON( '1', '/', 'N', 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STRCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STRCON( '1', 'U', '/', 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STRCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STRCON( '1', 'U', 'N', -1, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STRCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_STRCON( '1', 'U', 'N', 2, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STRCON', INFOT, NOUT, LERR, OK )
*
*        AB_SLATRS
*
         SRNAMT = 'AB_SLATRS'
         INFOT = 1
         CALL AB_SLATRS( '/', 'N', 'N', 'N', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_SLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SLATRS( 'U', '/', 'N', 'N', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_SLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SLATRS( 'U', 'N', '/', 'N', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_SLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SLATRS( 'U', 'N', 'N', '/', 0, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_SLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SLATRS( 'U', 'N', 'N', 'N', -1, A, 1, X, SCALE, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_SLATRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SLATRS( 'U', 'N', 'N', 'N', 2, A, 1, X, SCALE, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_SLATRS', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        Test error exits for the packed triangular routines.
*
*        AB_STPTRI
*
         SRNAMT = 'AB_STPTRI'
         INFOT = 1
         CALL AB_STPTRI( '/', 'N', 0, A, INFO )
         CALL AB_CHKXER( 'AB_STPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STPTRI( 'U', '/', 0, A, INFO )
         CALL AB_CHKXER( 'AB_STPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STPTRI( 'U', 'N', -1, A, INFO )
         CALL AB_CHKXER( 'AB_STPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_STPTRS
*
         SRNAMT = 'AB_STPTRS'
         INFOT = 1
         CALL AB_STPTRS( '/', 'N', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STPTRS( 'U', '/', 'N', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STPTRS( 'U', 'N', '/', 0, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STPTRS( 'U', 'N', 'N', -1, 0, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STPTRS( 'U', 'N', 'N', 0, -1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_STPTRS( 'U', 'N', 'N', 2, 1, A, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_STPRFS
*
         SRNAMT = 'AB_STPRFS'
         INFOT = 1
         CALL AB_STPRFS( '/', 'N', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_STPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STPRFS( 'U', '/', 'N', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_STPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STPRFS( 'U', 'N', '/', 0, 0, A, B, 1, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_STPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STPRFS( 'U', 'N', 'N', -1, 0, A, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STPRFS( 'U', 'N', 'N', 0, -1, A, B, 1, X, 1, R1, R2, W,
     $                IW, INFO )
         CALL AB_CHKXER( 'AB_STPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_STPRFS( 'U', 'N', 'N', 2, 1, A, B, 1, X, 2, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_STPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_STPRFS( 'U', 'N', 'N', 2, 1, A, B, 2, X, 1, R1, R2, W, 
     $IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_STPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_STPCON
*
         SRNAMT = 'AB_STPCON'
         INFOT = 1
         CALL AB_STPCON( '/', 'U', 'N', 0, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STPCON( '1', '/', 'N', 0, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STPCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STPCON( '1', 'U', '/', 0, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STPCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STPCON( '1', 'U', 'N', -1, A, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STPCON', INFOT, NOUT, LERR, OK )
*
*        AB_SLATPS
*
         SRNAMT = 'AB_SLATPS'
         INFOT = 1
         CALL AB_SLATPS( '/', 'N', 'N', 'N', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_SLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SLATPS( 'U', '/', 'N', 'N', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_SLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SLATPS( 'U', 'N', '/', 'N', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_SLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SLATPS( 'U', 'N', 'N', '/', 0, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_SLATPS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SLATPS( 'U', 'N', 'N', 'N', -1, A, X, SCALE, W, INFO )
         CALL AB_CHKXER( 'AB_SLATPS', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        Test error exits for the banded triangular routines.
*
*        AB_STBTRS
*
         SRNAMT = 'AB_STBTRS'
         INFOT = 1
         CALL AB_STBTRS( '/', 'N', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STBTRS( 'U', '/', 'N', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STBTRS( 'U', 'N', '/', 0, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STBTRS( 'U', 'N', 'N', -1, 0, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STBTRS( 'U', 'N', 'N', 0, -1, 0, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_STBTRS( 'U', 'N', 'N', 0, 0, -1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_STBTRS( 'U', 'N', 'N', 2, 1, 1, A, 1, X, 2, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_STBTRS( 'U', 'N', 'N', 2, 0, 1, A, 1, X, 1, INFO )
         CALL AB_CHKXER( 'AB_STBTRS', INFOT, NOUT, LERR, OK )
*
*        AB_STBRFS
*
         SRNAMT = 'AB_STBRFS'
         INFOT = 1
         CALL AB_STBRFS( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STBRFS( 'U', '/', 'N', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STBRFS( 'U', 'N', '/', 0, 0, 0, A, 1, B, 1, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STBRFS( 'U', 'N', 'N', -1, 0, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STBRFS( 'U', 'N', 'N', 0, -1, 0, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_STBRFS( 'U', 'N', 'N', 0, 0, -1, A, 1, B, 1, X, 1, R1, 
     $R2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_STBRFS( 'U', 'N', 'N', 2, 1, 1, A, 1, B, 2, X, 2, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_STBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 1, X, 2, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_STBRFS( 'U', 'N', 'N', 2, 1, 1, A, 2, B, 2, X, 1, R1, R
     $2,
     $                W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBRFS', INFOT, NOUT, LERR, OK )
*
*        AB_STBCON
*
         SRNAMT = 'AB_STBCON'
         INFOT = 1
         CALL AB_STBCON( '/', 'U', 'N', 0, 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_STBCON( '1', '/', 'N', 0, 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_STBCON( '1', 'U', '/', 0, 0, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_STBCON( '1', 'U', 'N', -1, 0, A, 1, RCOND, W, IW, INFO 
     $)
         CALL AB_CHKXER( 'AB_STBCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_STBCON( '1', 'U', 'N', 0, -1, A, 1, RCOND, W, IW, INFO 
     $)
         CALL AB_CHKXER( 'AB_STBCON', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_STBCON( '1', 'U', 'N', 2, 1, A, 1, RCOND, W, IW, INFO )
         CALL AB_CHKXER( 'AB_STBCON', INFOT, NOUT, LERR, OK )
*
*        AB_SLATBS
*
         SRNAMT = 'AB_SLATBS'
         INFOT = 1
         CALL AB_SLATBS( '/', 'N', 'N', 'N', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SLATBS( 'U', '/', 'N', 'N', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SLATBS( 'U', 'N', '/', 'N', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SLATBS( 'U', 'N', 'N', '/', 0, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SLATBS( 'U', 'N', 'N', 'N', -1, 0, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SLATBS( 'U', 'N', 'N', 'N', 1, -1, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SLATBS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SLATBS( 'U', 'N', 'N', 'N', 2, 1, A, 1, X, SCALE, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SLATBS', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRTR
*
      END
