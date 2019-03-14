*> \brief \b AB_SERRBD
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SERRBD( PATH, NUNIT )
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
*> AB_SERRBD tests the error exits for AB_SGEBD2, AB_SGEBRD, AB_SORGBR, AB_SORMBR,
*> AB_SBDSQR, AB_SBDSDC and AB_SBDSVDX.
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
*> \ingroup single_eig
*
*  =====================================================================
      SUBROUTINE AB_SERRBD( PATH, NUNIT )
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
      PARAMETER          ( NMAX = 4, LW = NMAX )
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, NS, NT
*     ..
*     .. Local Arrays ..
      INTEGER            IQ( NMAX, NMAX ), IW( NMAX )
      REAL               A( NMAX, NMAX ), D( NMAX ), E( NMAX ),
     $                   Q( NMAX, NMAX ), S( NMAX ), TP( NMAX ),
     $                   TQ( NMAX ), U( NMAX, NMAX ),
     $                   V( NMAX, NMAX ), W( LW )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_CHKXER, AB_SBDSDC, AB_SBDSQR, AB_SBDSVDX, AB
     $_SGEBD2,
     $                   AB_SGEBRD, AB_SORGBR, AB_SORMBR
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
            A( I, J ) = 1.D0 / REAL( I+J )
   10    CONTINUE
   20 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits of the SVD routines.
*
      IF( AB_AB_LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        AB_SGEBRD
*
         SRNAMT = 'AB_SGEBRD'
         INFOT = 1
         CALL AB_SGEBRD( -1, 0, A, 1, D, E, TQ, TP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGEBRD( 0, -1, A, 1, D, E, TQ, TP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGEBRD( 2, 1, A, 1, D, E, TQ, TP, W, 2, INFO )
         CALL AB_CHKXER( 'AB_SGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SGEBRD( 2, 1, A, 2, D, E, TQ, TP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SGEBRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        AB_SGEBD2
*
         SRNAMT = 'AB_SGEBD2'
         INFOT = 1
         CALL AB_SGEBD2( -1, 0, A, 1, D, E, TQ, TP, W, INFO )
         CALL AB_CHKXER( 'AB_SGEBD2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SGEBD2( 0, -1, A, 1, D, E, TQ, TP, W, INFO )
         CALL AB_CHKXER( 'AB_SGEBD2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SGEBD2( 2, 1, A, 1, D, E, TQ, TP, W, INFO )
         CALL AB_CHKXER( 'AB_SGEBD2', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        AB_SORGBR
*
         SRNAMT = 'AB_SORGBR'
         INFOT = 1
         CALL AB_SORGBR( '/', 0, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SORGBR( 'Q', -1, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SORGBR( 'Q', 0, -1, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SORGBR( 'Q', 0, 1, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SORGBR( 'Q', 1, 0, 1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SORGBR( 'P', 1, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SORGBR( 'P', 0, 1, 1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SORGBR( 'Q', 0, 0, -1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SORGBR( 'Q', 2, 1, 1, A, 1, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SORGBR( 'Q', 2, 2, 1, A, 2, TQ, W, 1, INFO )
         CALL AB_CHKXER( 'AB_SORGBR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        AB_SORMBR
*
         SRNAMT = 'AB_SORMBR'
         INFOT = 1
         CALL AB_SORMBR( '/', 'L', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SORMBR( 'Q', '/', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SORMBR( 'Q', 'L', '/', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SORMBR( 'Q', 'L', 'T', -1, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SORMBR( 'Q', 'L', 'T', 0, -1, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_SORMBR( 'Q', 'L', 'T', 0, 0, -1, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SORMBR( 'Q', 'L', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SORMBR( 'Q', 'R', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SORMBR( 'P', 'L', 'T', 2, 0, 2, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SORMBR( 'P', 'R', 'T', 0, 2, 2, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_SORMBR( 'Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_SORMBR( 'Q', 'L', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_SORMBR( 'Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL AB_CHKXER( 'AB_SORMBR', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        AB_SBDSQR
*
         SRNAMT = 'AB_SBDSQR'
         INFOT = 1
         CALL AB_SBDSQR( '/', 0, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W, INF
     $O )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SBDSQR( 'U', -1, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SBDSQR( 'U', 0, -1, 0, 0, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SBDSQR( 'U', 0, 0, -1, 0, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_SBDSQR( 'U', 0, 0, 0, -1, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SBDSQR( 'U', 2, 1, 0, 0, D, E, V, 1, U, 1, A, 1, W, INF
     $O )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL AB_SBDSQR( 'U', 0, 0, 2, 0, D, E, V, 1, U, 1, A, 1, W, INF
     $O )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL AB_SBDSQR( 'U', 2, 0, 0, 1, D, E, V, 1, U, 1, A, 1, W, INF
     $O )
         CALL AB_CHKXER( 'AB_SBDSQR', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        AB_SBDSDC
*
         SRNAMT = 'AB_SBDSDC'
         INFOT = 1
         CALL AB_SBDSDC( '/', 'N', 0, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SBDSDC( 'U', '/', 0, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SBDSDC( 'U', 'N', -1, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SBDSDC( 'U', 'I', 2, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SBDSDC( 'U', 'I', 2, D, E, U, 2, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL AB_CHKXER( 'AB_SBDSDC', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        AB_SBDSVDX
*
         SRNAMT = 'AB_SBDSVDX'
         INFOT = 1
         CALL AB_SBDSVDX( 'X', 'N', 'A', 1, D, E, ZERO, ONE, 0, 0,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_SBDSVDX( 'U', 'X', 'A', 1, D, E, ZERO, ONE, 0, 0,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_SBDSVDX( 'U', 'V', 'X', 1, D, E, ZERO, ONE, 0, 0,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_SBDSVDX( 'U', 'V', 'A', -1, D, E, ZERO, ONE, 0, 0,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_SBDSVDX( 'U', 'V', 'V', 2, D, E, -ONE, ZERO, 0, 0,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_SBDSVDX( 'U', 'V', 'V', 2, D, E, ONE, ZERO, 0, 0,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SBDSVDX( 'L', 'V', 'I', 2, D, E, ZERO, ZERO, 0, 2,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_SBDSVDX( 'L', 'V', 'I', 4, D, E, ZERO, ZERO, 5, 2,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SBDSVDX( 'L', 'V', 'I', 4, D, E, ZERO, ZERO, 3, 2,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_SBDSVDX( 'L', 'V', 'I', 4, D, E, ZERO, ZERO, 3, 5,
     $                    NS, S, Q, 1, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_SBDSVDX( 'L', 'V', 'A', 4, D, E, ZERO, ZERO, 0, 0,
     $                    NS, S, Q, 0, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL AB_SBDSVDX( 'L', 'V', 'A', 4, D, E, ZERO, ZERO, 0, 0,
     $                    NS, S, Q, 2, W, IW, INFO)
         CALL AB_CHKXER( 'AB_SBDSVDX', INFOT, NOUT, LERR, OK )
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
*     End of AB_SERRBD
*
      END
