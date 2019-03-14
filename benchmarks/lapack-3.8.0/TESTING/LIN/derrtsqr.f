*> \brief \b AB_DERRTSQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRTSQR( PATH, NUNIT )
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
*> AB_DERRTSQR tests the error exits for the DOUBLE PRECISION routines
*> that use the TSQR decomposition of a general matrix.
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
      SUBROUTINE AB_DERRTSQR( PATH, NUNIT )
      IMPLICIT NONE
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
      INTEGER            I, INFO, J, NB
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   A( NMAX, NMAX ), T( NMAX, NMAX ), W( NMAX ),
     $                   C( NMAX, NMAX ), TAU(NMAX)
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_DGEQR,
     $                   AB_DGEMQR, AB_DGELQ, AB_DGEMLQ
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
      DO J = 1, NMAX
         DO I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
            C( I, J ) = 1.D0 / DBLE( I+J )
            T( I, J ) = 1.D0 / DBLE( I+J )
         END DO
         W( J ) = 0.D0
      END DO
      OK = .TRUE.
*
*     Error exits for TS factorization
*
*     AB_DGEQR
*
      SRNAMT = 'AB_DGEQR'
      INFOT = 1
      CALL AB_DGEQR( -1, 0, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGEQR( 0, -1, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DGEQR( 1, 1, A, 0, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_DGEQR( 3, 2, A, 3, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGEQR', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DGEQR( 3, 2, A, 3, TAU, 7, W, 0, INFO )
      CALL AB_CHKXER( 'AB_DGEQR', INFOT, NOUT, LERR, OK )
*
*     AB_DGEMQR
*
      TAU(1)=1
      TAU(2)=1
      SRNAMT = 'AB_DGEMQR'
      NB=1
      INFOT = 1
      CALL AB_DGEMQR( '/', 'N', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGEMQR( 'L', '/', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DGEMQR( 'L', 'N', -1, 0, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DGEMQR( 'L', 'N', 0, -1, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DGEMQR( 'L', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DGEMQR( 'R', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DGEMQR( 'L', 'N', 2, 1, 0, A, 0, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_DGEMQR( 'R', 'N', 2, 2, 1, A, 2, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_DGEMQR( 'L', 'N', 2, 2, 1, A, 2, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_DGEMQR( 'L', 'N', 2, 1, 1, A, 2, TAU, 6, C, 0, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
      INFOT = 13
      CALL AB_DGEMQR( 'L', 'N', 2, 2, 1, A, 2, TAU, 6, C, 2, W, 0,INFO)
      CALL AB_CHKXER( 'AB_DGEMQR', INFOT, NOUT, LERR, OK )
*
*     AB_DGELQ
*
      SRNAMT = 'AB_DGELQ'
      INFOT = 1
      CALL AB_DGELQ( -1, 0, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGELQ( 0, -1, A, 1, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DGELQ( 1, 1, A, 0, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 6
      CALL AB_DGELQ( 2, 3, A, 3, TAU, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DGELQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DGELQ( 2, 3, A, 3, TAU, 7, W, 0, INFO )
      CALL AB_CHKXER( 'AB_DGELQ', INFOT, NOUT, LERR, OK )
*
*     AB_DGEMLQ
*
      TAU(1)=1
      TAU(2)=1
      SRNAMT = 'AB_DGEMLQ'
      NB=1
      INFOT = 1
      CALL AB_DGEMLQ( '/', 'N', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DGEMLQ( 'L', '/', 0, 0, 0, A, 1, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DGEMLQ( 'L', 'N', -1, 0, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DGEMLQ( 'L', 'N', 0, -1, 0, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DGEMLQ( 'L', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DGEMLQ( 'R', 'N', 0, 0, -1, A, 1, TAU, 1, C, 1, W,1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DGEMLQ( 'L', 'N', 1, 2, 0, A, 0, TAU, 1, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_DGEMLQ( 'R', 'N', 2, 2, 1, A, 1, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 9
      CALL AB_DGEMLQ( 'L', 'N', 2, 2, 1, A, 1, TAU, 0, C, 1, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 11
      CALL AB_DGEMLQ( 'L', 'N', 1, 2, 1, A, 1, TAU, 6, C, 0, W, 1,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 13
      CALL AB_DGEMLQ( 'L', 'N', 2, 2, 1, A, 2, TAU, 6, C, 2, W, 0,INFO)
      CALL AB_CHKXER( 'AB_DGEMLQ', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRTSQR
*
      END
