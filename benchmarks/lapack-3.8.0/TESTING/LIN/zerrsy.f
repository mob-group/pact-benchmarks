*> \brief \b AB_ZERRSY
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZERRSY( PATH, NUNIT )
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
*> AB_ZERRSY tests the error exits for the COMPLEX*16 routines
*> for symmetric indefinite matrices.
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
*> \date November 2017
*
*> \ingroup complex16_lin
*
*  =====================================================================
      SUBROUTINE AB_ZERRSY( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
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
      PARAMETER          ( NMAX = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J
      DOUBLE PRECISION   ANRM, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   R( NMAX ), R1( NMAX ), R2( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   E( NMAX ), W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      EXTERNAL           AB_AB_LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAESM, AB_CHKXER, AB_ZSPCON, AB_AB_ZSPRFS, 
     $AB_ZSPTRF, AB_ZSPTRI,
     $                   AB_ZSPTRS, AB_ZSYCON, AB_AB_ZSYCON_3, AB_AB_ZSY
     $CON_ROOK, AB_AB_ZSYRFS,
     $                   AB_ZSYTF2, AB_AB_ZSYTF2_RK, AB_AB_ZSYTF2_ROOK, 
     $AB_ZSYTRF,
     $                   AB_AB_ZSYTRF_RK, AB_AB_ZSYTRF_ROOK, AB_ZSYTRI, 
     $AB_AB_ZSYTRI_3,
     $                   AB_AB_AB_ZSYTRI_3X, AB_AB_ZSYTRI_ROOK, AB_AB_ZS
     $YTRI2, AB_AB_AB_ZSYTRI2X,
     $                   AB_ZSYTRS, AB_AB_ZSYTRS_3, AB_AB_ZSYTRS_ROOK
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
      INTRINSIC          DBLE, DCMPLX
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
            A( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                  -1.D0 / DBLE( I+J ) )
            AF( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                   -1.D0 / DBLE( I+J ) )
   10    CONTINUE
         B( J ) = 0.D0
         E( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      ANRM = 1.0D0
      OK = .TRUE.
*
      IF( AB_AB_LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with patrial
*        (Bunch-Kaufman) diagonal pivoting method.
*
*        AB_ZSYTRF
*
         SRNAMT = 'AB_ZSYTRF'
         INFOT = 1
         CALL AB_ZSYTRF( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSYTRF( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZSYTRF( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZSYTRF( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRF', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZSYTRF( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZSYTF2
*
         SRNAMT = 'AB_ZSYTF2'
         INFOT = 1
         CALL AB_ZSYTF2( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSYTF2( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZSYTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZSYTF2( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_ZSYTF2', INFOT, NOUT, LERR, OK )
*
*        AB_ZSYTRI
*
         SRNAMT = 'AB_ZSYTRI'
         INFOT = 1
         CALL AB_ZSYTRI( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSYTRI( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRI', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZSYTRI( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRI', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTRI2
*
         SRNAMT = 'AB_AB_ZSYTRI2'
         INFOT = 1
         CALL AB_AB_ZSYTRI2( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRI2( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTRI2( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI2', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZSYTRI2X
*
         SRNAMT = 'AB_AB_AB_ZSYTRI2X'
         INFOT = 1
         CALL AB_AB_AB_ZSYTRI2X( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZSYTRI2X( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRI2X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZSYTRI2X( 'U', 2, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRI2X', INFOT, NOUT, LERR, OK )
*
*        AB_ZSYTRS
*
         SRNAMT = 'AB_ZSYTRS'
         INFOT = 1
         CALL AB_ZSYTRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSYTRS( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZSYTRS( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZSYTRS( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_ZSYTRS( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSYTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYRFS
*
         SRNAMT = 'AB_AB_ZSYRFS'
         INFOT = 1
         CALL AB_AB_ZSYRFS( '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYRFS( 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSYRFS( 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $ R2,
     $                W, R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZSYRFS( 'U', 2, 1, A, 1, AF, 2, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZSYRFS( 'U', 2, 1, A, 2, AF, 1, IP, B, 2, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 1, X, 2, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL AB_AB_ZSYRFS( 'U', 2, 1, A, 2, AF, 2, IP, B, 2, X, 1, R1, 
     $R2, W,
     $                R, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZSYCON
*
         SRNAMT = 'AB_ZSYCON'
         INFOT = 1
         CALL AB_ZSYCON( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSYCON( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_ZSYCON( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZSYCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_ZSYCON( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZSYCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SR' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) diagonal pivoting method.
*
*        AB_AB_ZSYTRF_ROOK
*
         SRNAMT = 'AB_AB_ZSYTRF_ROOK'
         INFOT = 1
         CALL AB_AB_ZSYTRF_ROOK( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRF_ROOK( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTRF_ROOK( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZSYTRF_ROOK( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZSYTRF_ROOK( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTF2_ROOK
*
         SRNAMT = 'AB_AB_ZSYTF2_ROOK'
         INFOT = 1
         CALL AB_AB_ZSYTF2_ROOK( '/', 0, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTF2_ROOK( 'U', -1, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTF2_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTF2_ROOK( 'U', 2, A, 1, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTF2_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTRI_ROOK
*
         SRNAMT = 'AB_AB_ZSYTRI_ROOK'
         INFOT = 1
         CALL AB_AB_ZSYTRI_ROOK( '/', 0, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRI_ROOK( 'U', -1, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTRI_ROOK( 'U', 2, A, 1, IP, W, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTRS_ROOK
*
         SRNAMT = 'AB_AB_ZSYTRS_ROOK'
         INFOT = 1
         CALL AB_AB_ZSYTRS_ROOK( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRS_ROOK( 'U', -1, 0, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSYTRS_ROOK( 'U', 0, -1, A, 1, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZSYTRS_ROOK( 'U', 2, 1, A, 1, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYTRS_ROOK( 'U', 2, 1, A, 2, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_ROOK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYCON_ROOK
*
         SRNAMT = 'AB_AB_ZSYCON_ROOK'
         INFOT = 1
         CALL AB_AB_ZSYCON_ROOK( '/', 0, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYCON_ROOK( 'U', -1, A, 1, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYCON_ROOK( 'U', 2, A, 1, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZSYCON_ROOK', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL AB_AB_ZSYCON_ROOK( 'U', 1, A, 1, IP, -ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZSYCON_ROOK', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SK' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with rook
*        (bounded Bunch-Kaufman) pivoting with the new storage
*        format for factors L ( or U) and D.
*
*        L (or U) is stored in A, diagonal of D is stored on the
*        diagonal of A, subdiagonal of D is stored in a separate array E.
*
*        AB_AB_ZSYTRF_RK
*
         SRNAMT = 'AB_AB_ZSYTRF_RK'
         INFOT = 1
         CALL AB_AB_ZSYTRF_RK( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRF_RK( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTRF_RK( 'U', 2, A, 1, E, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYTRF_RK( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_RK', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYTRF_RK( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTF2_RK
*
         SRNAMT = 'AB_AB_ZSYTF2_RK'
         INFOT = 1
         CALL AB_AB_ZSYTF2_RK( '/', 0, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTF2_RK( 'U', -1, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTF2_RK', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTF2_RK( 'U', 2, A, 1, E, IP, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTF2_RK', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTRI_3
*
         SRNAMT = 'AB_AB_ZSYTRI_3'
         INFOT = 1
         CALL AB_AB_ZSYTRI_3( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRI_3( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTRI_3( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYTRI_3( 'U', 0, A, 1, E, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYTRI_3( 'U', 0, A, 1, E, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRI_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_AB_ZSYTRI_3X
*
         SRNAMT = 'AB_AB_AB_ZSYTRI_3X'
         INFOT = 1
         CALL AB_AB_AB_ZSYTRI_3X( '/', 0, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_AB_ZSYTRI_3X( 'U', -1, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRI_3X', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_AB_ZSYTRI_3X( 'U', 2, A, 1, E, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRI_3X', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTRS_3
*
         SRNAMT = 'AB_AB_ZSYTRS_3'
         INFOT = 1
         CALL AB_AB_ZSYTRS_3( '/', 0, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRS_3( 'U', -1, 0, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSYTRS_3( 'U', 0, -1, A, 1, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZSYTRS_3( 'U', 2, 1, A, 1, E, IP, B, 2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_3', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL AB_AB_ZSYTRS_3( 'U', 2, 1, A, 2, E, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_3', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYCON_3
*
         SRNAMT = 'AB_AB_ZSYCON_3'
         INFOT = 1
         CALL AB_AB_ZSYCON_3( '/', 0, A, 1,  E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYCON_3( 'U', -1, A, 1, E, IP, ANRM, RCOND, W, INFO
     $ )
         CALL AB_CHKXER( 'AB_AB_ZSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYCON_3( 'U', 2, A, 1, E, IP, ANRM, RCOND, W, INFO 
     $)
         CALL AB_CHKXER( 'AB_AB_ZSYCON_3', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZSYCON_3( 'U', 1, A, 1, E, IP, -1.0D0, RCOND, W, INF
     $O)
         CALL AB_CHKXER( 'AB_AB_ZSYCON_3', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite packed matrix with patrial
*        (Bunch-Kaufman) pivoting.
*
*        AB_ZSPTRF
*
         SRNAMT = 'AB_ZSPTRF'
         INFOT = 1
         CALL AB_ZSPTRF( '/', 0, A, IP, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSPTRF( 'U', -1, A, IP, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRF', INFOT, NOUT, LERR, OK )
*
*        AB_ZSPTRI
*
         SRNAMT = 'AB_ZSPTRI'
         INFOT = 1
         CALL AB_ZSPTRI( '/', 0, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRI', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSPTRI( 'U', -1, A, IP, W, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRI', INFOT, NOUT, LERR, OK )
*
*        AB_ZSPTRS
*
         SRNAMT = 'AB_ZSPTRS'
         INFOT = 1
         CALL AB_ZSPTRS( '/', 0, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSPTRS( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_ZSPTRS( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_ZSPTRS( 'U', 2, 1, A, IP, B, 1, INFO )
         CALL AB_CHKXER( 'AB_ZSPTRS', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSPRFS
*
         SRNAMT = 'AB_AB_ZSPRFS'
         INFOT = 1
         CALL AB_AB_ZSPRFS( '/', 0, 0, A, AF, IP, B, 1, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSPRFS( 'U', -1, 0, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSPRFS( 'U', 0, -1, A, AF, IP, B, 1, X, 1, R1, R2, W
     $, R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSPRFS( 'U', 2, 1, A, AF, IP, B, 1, X, 2, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPRFS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL AB_AB_ZSPRFS( 'U', 2, 1, A, AF, IP, B, 2, X, 1, R1, R2, W,
     $ R,
     $                INFO )
         CALL AB_CHKXER( 'AB_AB_ZSPRFS', INFOT, NOUT, LERR, OK )
*
*        AB_ZSPCON
*
         SRNAMT = 'AB_ZSPCON'
         INFOT = 1
         CALL AB_ZSPCON( '/', 0, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_ZSPCON( 'U', -1, A, IP, ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZSPCON', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_ZSPCON( 'U', 1, A, IP, -ANRM, RCOND, W, INFO )
         CALL AB_CHKXER( 'AB_ZSPCON', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'SA' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with Aasen's algorithm.
*
*        AB_AB_ZSYTRF_AA
*
         SRNAMT = 'AB_AB_ZSYTRF_AA'
         INFOT = 1
         CALL AB_AB_ZSYTRF_AA( '/', 0, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_AA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRF_AA( 'U', -1, A, 1, IP, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_AA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL AB_AB_ZSYTRF_AA( 'U', 2, A, 1, IP, W, 4, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_AA', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZSYTRF_AA( 'U', 0, A, 1, IP, W, 0, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_AA', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL AB_AB_ZSYTRF_AA( 'U', 0, A, 1, IP, W, -2, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRF_AA', INFOT, NOUT, LERR, OK )
*
*        AB_AB_ZSYTRS_AA
*
         SRNAMT = 'AB_AB_ZSYTRS_AA'
         INFOT = 1
         CALL AB_AB_ZSYTRS_AA( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_AA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL AB_AB_ZSYTRS_AA( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_AA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL AB_AB_ZSYTRS_AA( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_AA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL AB_AB_ZSYTRS_AA( 'U', 2, 1, A, 1, IP, B, 2, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_AA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL AB_AB_ZSYTRS_AA( 'U', 2, 1, A, 2, IP, B, 1, W, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_AA', INFOT, NOUT, LERR, OK )
*
      ELSE IF( AB_AB_LSAMEN( 2, C2, 'S2' ) ) THEN
*
*        Test error exits of the routines that use factorization
*        of a symmetric indefinite matrix with Aasen's algorithm.
*
*        AB_AB_AB_ZSYTRF_AA_2STAGE
*
         SRNAMT = 'AB_AB_AB_ZSYTRF_AA_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_ZSYTRF_AA_2STAGE( '/', 0, A, 1, A, 1, IP, IP, W, 
     $1,
     $                          INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRF_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 2
         CALL AB_AB_AB_ZSYTRF_AA_2STAGE( 'U', -1, A, 1, A, 1, IP, IP, W,
     $ 1,
     $                           INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRF_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 4
         CALL AB_AB_AB_ZSYTRF_AA_2STAGE( 'U', 2, A, 1, A, 2, IP, IP, W, 
     $1,
     $                           INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRF_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 6
         CALL AB_AB_AB_ZSYTRF_AA_2STAGE( 'U', 2, A, 2, A, 1, IP, IP, W, 
     $1,
     $                           INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRF_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 10
         CALL AB_AB_AB_ZSYTRF_AA_2STAGE( 'U', 2, A, 2, A, 8, IP, IP, W, 
     $0,
     $                           INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRF_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
*
*        AB_AB_AB_CHETRS_AA_2STAGE
*
         SRNAMT = 'AB_AB_AB_ZSYTRS_AA_2STAGE'
         INFOT = 1
         CALL AB_AB_AB_ZSYTRS_AA_2STAGE( '/', 0, 0, A, 1, A, 1, IP, IP,
     $                          B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRS_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 2
         CALL AB_AB_AB_ZSYTRS_AA_2STAGE( 'U', -1, 0, A, 1, A, 1, IP, IP,
     $                          B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRS_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 3
         CALL AB_AB_AB_ZSYTRS_AA_2STAGE( 'U', 0, -1, A, 1, A, 1, IP, IP,
     $                          B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRS_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 5
         CALL AB_AB_AB_ZSYTRS_AA_2STAGE( 'U', 2, 1, A, 1, A, 1, IP, IP,
     $                          B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRS_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 7
         CALL AB_AB_AB_ZSYTRS_AA_2STAGE( 'U', 2, 1, A, 2, A, 1, IP, IP,
     $                          B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_AB_ZSYTRS_AA_2STAGE', INFOT, NOUT, LERR,
     $ OK )
         INFOT = 11
         CALL AB_AB_AB_ZSYTRS_AA_2STAGE( 'U', 2, 1, A, 2, A, 8, IP, IP,
     $                          B, 1, INFO )
         CALL AB_CHKXER( 'AB_AB_ZSYTRS_AA_STAGE', INFOT, NOUT, LERR, OK 
     $)
*
      END IF
*
*     Print a summary line.
*
      CALL AB_ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of AB_ZERRSY
*
      END
