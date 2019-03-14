*> \brief \b AB_DERRRQ
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRRQ( PATH, NUNIT )
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
*> AB_DERRRQ tests the error exits for the DOUBLE PRECISION routines
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
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DERRRQ( PATH, NUNIT )
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
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_DGERQ2, AB_AB_DGERQ
     $F, AB_AB_DGERQS, AB_DORGR2,
     $                   AB_DORGRQ, AB_DORMR2, AB_DORMRQ
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
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
            AF( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
         B( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
   20 CONTINUE
      OK = .TRUE.
*
*     Error exits for RQ factorization
*
*     AB_AB_DGERQF
*
      SRNAMT = 'AB_AB_DGERQF'
      INFOT = 1
      CALL AB_AB_DGERQF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGERQF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_DGERQF( 2, 1, A, 1, B, W, 2, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_DGERQF( 2, 1, A, 2, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_DGERQ2
*
      SRNAMT = 'AB_AB_DGERQ2'
      INFOT = 1
      CALL AB_AB_DGERQ2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGERQ2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQ2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_DGERQ2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQ2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_DGERQS
*
      SRNAMT = 'AB_AB_DGERQS'
      INFOT = 1
      CALL AB_AB_DGERQS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGERQS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGERQS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_DGERQS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_DGERQS( 2, 2, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_DGERQS( 2, 2, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_DGERQS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGERQS', INFOT, NOUT, LERR, OK )
*
*     AB_DORGRQ
*
      SRNAMT = 'AB_DORGRQ'
      INFOT = 1
      CALL AB_DORGRQ( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGRQ( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGRQ( 2, 1, 0, A, 2, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_DORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGRQ( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGRQ( 1, 2, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORGRQ( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_DORGRQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DORGRQ( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGRQ', INFOT, NOUT, LERR, OK )
*
*     AB_DORGR2
*
      SRNAMT = 'AB_DORGR2'
      INFOT = 1
      CALL AB_DORGR2( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGR2( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGR2( 2, 1, 0, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGR2( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGR2( 1, 2, 2, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORGR2( 2, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGR2', INFOT, NOUT, LERR, OK )
*
*     AB_DORMRQ
*
      SRNAMT = 'AB_DORMRQ'
      INFOT = 1
      CALL AB_DORMRQ( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORMRQ( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORMRQ( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORMRQ( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMRQ( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMRQ( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMRQ( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMRQ( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMRQ( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORMRQ( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMRQ( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMRQ( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMRQ', INFOT, NOUT, LERR, OK )
*
*     AB_DORMR2
*
      SRNAMT = 'AB_DORMR2'
      INFOT = 1
      CALL AB_DORMR2( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORMR2( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORMR2( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORMR2( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMR2( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMR2( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMR2( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMR2( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMR2( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORMR2( 'L', 'N', 2, 1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORMR2', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRRQ
*
      END
