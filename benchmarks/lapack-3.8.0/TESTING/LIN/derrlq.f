*> \brief \b AB_DERRLQ
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DERRLQ( PATH, NUNIT )
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
*> AB_DERRLQ tests the error exits for the DOUBLE PRECISION routines
*> that use the LQ decomposition of a general matrix.
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
      SUBROUTINE AB_DERRLQ( PATH, NUNIT )
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
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_AB_DGELQ2, AB_AB_DGELQ
     $F, AB_AB_DGELQS, AB_DORGL2,
     $                   AB_DORGLQ, AB_DORML2, AB_DORMLQ
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
*     Error exits for LQ factorization
*
*     AB_AB_DGELQF
*
      SRNAMT = 'AB_AB_DGELQF'
      INFOT = 1
      CALL AB_AB_DGELQF( -1, 0, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQF', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGELQF( 0, -1, A, 1, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQF', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_DGELQF( 2, 1, A, 1, B, W, 2, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQF', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_AB_DGELQF( 2, 1, A, 2, B, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQF', INFOT, NOUT, LERR, OK )
*
*     AB_AB_DGELQ2
*
      SRNAMT = 'AB_AB_DGELQ2'
      INFOT = 1
      CALL AB_AB_DGELQ2( -1, 0, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQ2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGELQ2( 0, -1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQ2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_AB_DGELQ2( 2, 1, A, 1, B, W, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQ2', INFOT, NOUT, LERR, OK )
*
*     AB_AB_DGELQS
*
      SRNAMT = 'AB_AB_DGELQS'
      INFOT = 1
      CALL AB_AB_DGELQS( -1, 0, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGELQS( 0, -1, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_AB_DGELQS( 2, 1, 0, A, 2, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_AB_DGELQS( 0, 0, -1, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_AB_DGELQS( 2, 2, 0, A, 1, X, B, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_AB_DGELQS( 1, 2, 0, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQS', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_AB_DGELQS( 1, 1, 2, A, 1, X, B, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_AB_DGELQS', INFOT, NOUT, LERR, OK )
*
*     AB_DORGLQ
*
      SRNAMT = 'AB_DORGLQ'
      INFOT = 1
      CALL AB_DORGLQ( -1, 0, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGLQ( 0, -1, 0, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGLQ( 2, 1, 0, A, 2, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_DORGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGLQ( 0, 0, -1, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGLQ( 1, 1, 2, A, 1, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORGLQ( 2, 2, 0, A, 1, X, W, 2, INFO )
      CALL AB_CHKXER( 'AB_DORGLQ', INFOT, NOUT, LERR, OK )
      INFOT = 8
      CALL AB_DORGLQ( 2, 2, 0, A, 2, X, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORGLQ', INFOT, NOUT, LERR, OK )
*
*     AB_DORGL2
*
      SRNAMT = 'AB_DORGL2'
      INFOT = 1
      CALL AB_DORGL2( -1, 0, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGL2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGL2( 0, -1, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGL2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORGL2( 2, 1, 0, A, 2, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGL2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGL2( 0, 0, -1, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGL2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORGL2( 1, 1, 2, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGL2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORGL2( 2, 2, 0, A, 1, X, W, INFO )
      CALL AB_CHKXER( 'AB_DORGL2', INFOT, NOUT, LERR, OK )
*
*     AB_DORMLQ
*
      SRNAMT = 'AB_DORMLQ'
      INFOT = 1
      CALL AB_DORMLQ( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORMLQ( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORMLQ( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORMLQ( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMLQ( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMLQ( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORMLQ( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMLQ( 'L', 'N', 2, 0, 2, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORMLQ( 'R', 'N', 0, 2, 2, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORMLQ( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMLQ( 'L', 'N', 1, 2, 0, A, 1, X, AF, 1, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
      INFOT = 12
      CALL AB_DORMLQ( 'R', 'N', 2, 1, 0, A, 1, X, AF, 2, W, 1, INFO )
      CALL AB_CHKXER( 'AB_DORMLQ', INFOT, NOUT, LERR, OK )
*
*     AB_DORML2
*
      SRNAMT = 'AB_DORML2'
      INFOT = 1
      CALL AB_DORML2( '/', 'N', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 2
      CALL AB_DORML2( 'L', '/', 0, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 3
      CALL AB_DORML2( 'L', 'N', -1, 0, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 4
      CALL AB_DORML2( 'L', 'N', 0, -1, 0, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORML2( 'L', 'N', 0, 0, -1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORML2( 'L', 'N', 0, 1, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 5
      CALL AB_DORML2( 'R', 'N', 1, 0, 1, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORML2( 'L', 'N', 2, 1, 2, A, 1, X, AF, 2, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 7
      CALL AB_DORML2( 'R', 'N', 1, 2, 2, A, 1, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
      INFOT = 10
      CALL AB_DORML2( 'L', 'N', 2, 1, 0, A, 2, X, AF, 1, W, INFO )
      CALL AB_CHKXER( 'AB_DORML2', INFOT, NOUT, LERR, OK )
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_DERRLQ
*
      END
