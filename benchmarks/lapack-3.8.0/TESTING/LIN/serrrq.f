*> \brief \b AB_SERRRQ
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRRQ( PATH, NUNIT )
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
*> AB_SERRRQ tests the error exits for the REAL routines
*> that use the RQ decomposition of a general matrix.
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
      SUBROUTINE AB_SERRRQ( PATH, NUNIT )
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
      INTEGER            I, INFO, J
*     ..
*     .. Local Arrays ..
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_SGERQ2, AB_AB_SGERQ
     $F, AB_AB_SGERQS, AB_SORGR2,
     $                   AB_SORGRQ, AB_SORMR2, AB_SORMRQ
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
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1. / REAL( I+J )
            AF( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
         B( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
   20 CONTINUE
      OK = .TRUE.
*
*     Error exits for RQ factorization
*
*     AB_AB_SGERQF
*
      SRNAMT = 'AB_AB_SGERQF'
      INFOT = 1
      CALL AB_AB_SGERQF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGERQF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_SGERQF( 2, 1, A, 1, B, W, 2, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_SGERQF( 2, 1, A, 2, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_SGERQ2
*
      SRNAMT = 'AB_AB_SGERQ2'
      INFOT = 1
      CALL AB_AB_SGERQ2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGERQ2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_SGERQ2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQ2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_SGERQS
*
      SRNAMT = 'AB_AB_SGERQS'
      INFOT = 1
      CALL AB_AB_SGERQS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGERQS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_SGERQS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_SGERQS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_SGERQS( 2, 2, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_SGERQS( 2, 2, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_SGERQS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_SGERQS', INFOT, NOUT, LERR, OK )
*
*     AB_SORGRQ
*
      SRNAMT = 'AB_SORGRQ'
      INFOT = 1
      CALL AB_SORGRQ( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORGRQ( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORGRQ( 2, 1, 0, A, 2, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_SORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORGRQ( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORGRQ( 1, 2, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORGRQ( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_SORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_SORGRQ( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORGRQ', INFOT, NOUT, LERR, OK )
*
*     AB_SORGR2
*
      SRNAMT = 'AB_SORGR2'
      INFOT = 1
      CALL AB_SORGR2( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORGR2( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORGR2( 2, 1, 0, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORGR2( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORGR2( 1, 2, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORGR2( 2, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_SORGR2', INFOT, NOUT, LERR, OK )
*
*     AB_SORMRQ
*
      SRNAMT = 'AB_SORMRQ'
      INFOT = 1
      CALL AB_SORMRQ( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORMRQ( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORMRQ( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_SORMRQ( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMRQ( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMRQ( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMRQ( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORMRQ( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORMRQ( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_SORMRQ( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_SORMRQ( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_SORMRQ( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_SORMRQ', INFOT, NOUT, LERR, OK )
*
*     AB_SORMR2
*
      SRNAMT = 'AB_SORMR2'
      INFOT = 1
      CALL AB_SORMR2( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_SORMR2( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_SORMR2( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_SORMR2( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMR2( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMR2( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_SORMR2( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORMR2( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_SORMR2( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_SORMR2( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_SORMR2', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_SERRRQ
*
      END
