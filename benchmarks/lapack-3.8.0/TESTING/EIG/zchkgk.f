*> \brief \b AB_ZCHKGK
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZCHKGK( NIN, NOUT )
*
*       .. Scalar Arguments ..
*       INTEGER            NIN, NOUT
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ZCHKGK tests AB_ZGGBAK, a routine for backward balancing  of
*> a matrix pair (A, B).
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] NIN
*> \verbatim
*>          NIN is INTEGER
*>          The logical unit number for input.  NIN > 0.
*> \endverbatim
*>
*> \param[in] NOUT
*> \verbatim
*>          NOUT is INTEGER
*>          The logical unit number for output.  NOUT > 0.
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
*> \ingroup complex16_eig
*
*  =====================================================================
      SUBROUTINE AB_ZCHKGK( NIN, NOUT )
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            NIN, NOUT
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            LDA, LDB, LDVL, LDVR
      PARAMETER          ( LDA = 50, LDB = 50, LDVL = 50, LDVR = 50 )
      INTEGER            AB_LDE, LDF, LDWORK, LRWORK
      PARAMETER          ( AB_LDE = 50, LDF = 50, LDWORK = 50,
     $                   LRWORK = 6*50 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IHI, ILO, INFO, J, KNT, M, N, NINFO
      DOUBLE PRECISION   ANORM, BNORM, EPS, RMAX, VMAX
      COMPLEX*16         CDUM
*     ..
*     .. Local Arrays ..
      INTEGER            LMAX( 4 )
      DOUBLE PRECISION   LSCALE( LDA ), RSCALE( LDA ), RWORK( LRWORK )
      COMPLEX*16         A( LDA, LDA ), AF( LDA, LDA ), B( LDB, LDB ),
     $                   BF( LDB, LDB ), E( AB_LDE, AB_LDE ), F( LDF, LD
     $F ),
     $                   VL( LDVL, LDVL ), VLF( LDVL, LDVL ),
     $                   VR( LDVR, LDVR ), VRF( LDVR, LDVR ),
     $                   WORK( LDWORK, LDWORK )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DLAMCH, AB_ZLANGE
      EXTERNAL           AB_DLAMCH, AB_ZLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ZGEMM, AB_ZGGBAK, AB_ZGGBAL, AB_ZLACPY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, MAX
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
*     ..
*     .. Executable Statements ..
*
      LMAX( 1 ) = 0
      LMAX( 2 ) = 0
      LMAX( 3 ) = 0
      LMAX( 4 ) = 0
      NINFO = 0
      KNT = 0
      RMAX = ZERO
*
      EPS = AB_DLAMCH( 'Precision' )
*
   10 CONTINUE
      READ( NIN, FMT = * )N, M
      IF( N.EQ.0 )
     $   GO TO 100
*
      DO 20 I = 1, N
         READ( NIN, FMT = * )( A( I, J ), J = 1, N )
   20 CONTINUE
*
      DO 30 I = 1, N
         READ( NIN, FMT = * )( B( I, J ), J = 1, N )
   30 CONTINUE
*
      DO 40 I = 1, N
         READ( NIN, FMT = * )( VL( I, J ), J = 1, M )
   40 CONTINUE
*
      DO 50 I = 1, N
         READ( NIN, FMT = * )( VR( I, J ), J = 1, M )
   50 CONTINUE
*
      KNT = KNT + 1
*
      ANORM = AB_ZLANGE( 'M', N, N, A, LDA, RWORK )
      BNORM = AB_ZLANGE( 'M', N, N, B, LDB, RWORK )
*
      CALL AB_ZLACPY( 'FULL', N, N, A, LDA, AF, LDA )
      CALL AB_ZLACPY( 'FULL', N, N, B, LDB, BF, LDB )
*
      CALL AB_ZGGBAL( 'B', N, A, LDA, B, LDB, ILO, IHI, LSCALE, RSCALE,
     $             RWORK, INFO )
      IF( INFO.NE.0 ) THEN
         NINFO = NINFO + 1
         LMAX( 1 ) = KNT
      END IF
*
      CALL AB_ZLACPY( 'FULL', N, M, VL, LDVL, VLF, LDVL )
      CALL AB_ZLACPY( 'FULL', N, M, VR, LDVR, VRF, LDVR )
*
      CALL AB_ZGGBAK( 'B', 'L', N, ILO, IHI, LSCALE, RSCALE, M, VL, LDVL
     $,
     $             INFO )
      IF( INFO.NE.0 ) THEN
         NINFO = NINFO + 1
         LMAX( 2 ) = KNT
      END IF
*
      CALL AB_ZGGBAK( 'B', 'R', N, ILO, IHI, LSCALE, RSCALE, M, VR, LDVR
     $,
     $             INFO )
      IF( INFO.NE.0 ) THEN
         NINFO = NINFO + 1
         LMAX( 3 ) = KNT
      END IF
*
*     Test of AB_ZGGBAK
*
*     Check tiAB_LDE(VL)'*A*tiAB_LDE(VR) - VL'*tiAB_LDE(A)*VR
*     where tiAB_LDE(A) denotes the transformed matrix.
*
      CALL AB_ZGEMM( 'N', 'N', N, M, N, CONE, AF, LDA, VR, LDVR, CZERO,
     $            WORK, LDWORK )
      CALL AB_ZGEMM( 'C', 'N', M, M, N, CONE, VL, LDVL, WORK, LDWORK,
     $            CZERO, E, AB_LDE )
*
      CALL AB_ZGEMM( 'N', 'N', N, M, N, CONE, A, LDA, VRF, LDVR, CZERO,
     $            WORK, LDWORK )
      CALL AB_ZGEMM( 'C', 'N', M, M, N, CONE, VLF, LDVL, WORK, LDWORK,
     $            CZERO, F, LDF )
*
      VMAX = ZERO
      DO 70 J = 1, M
         DO 60 I = 1, M
            VMAX = MAX( VMAX, CABS1( E( I, J )-F( I, J ) ) )
   60    CONTINUE
   70 CONTINUE
      VMAX = VMAX / ( EPS*MAX( ANORM, BNORM ) )
      IF( VMAX.GT.RMAX ) THEN
         LMAX( 4 ) = KNT
         RMAX = VMAX
      END IF
*
*     Check tiAB_LDE(VL)'*B*tiAB_LDE(VR) - VL'*tiAB_LDE(B)*VR
*
      CALL AB_ZGEMM( 'N', 'N', N, M, N, CONE, BF, LDB, VR, LDVR, CZERO,
     $            WORK, LDWORK )
      CALL AB_ZGEMM( 'C', 'N', M, M, N, CONE, VL, LDVL, WORK, LDWORK,
     $            CZERO, E, AB_LDE )
*
      CALL AB_ZGEMM( 'n', 'n', N, M, N, CONE, B, LDB, VRF, LDVR, CZERO,
     $            WORK, LDWORK )
      CALL AB_ZGEMM( 'C', 'N', M, M, N, CONE, VLF, LDVL, WORK, LDWORK,
     $            CZERO, F, LDF )
*
      VMAX = ZERO
      DO 90 J = 1, M
         DO 80 I = 1, M
            VMAX = MAX( VMAX, CABS1( E( I, J )-F( I, J ) ) )
   80    CONTINUE
   90 CONTINUE
      VMAX = VMAX / ( EPS*MAX( ANORM, BNORM ) )
      IF( VMAX.GT.RMAX ) THEN
         LMAX( 4 ) = KNT
         RMAX = VMAX
      END IF
*
      GO TO 10
*
  100 CONTINUE
*
      WRITE( NOUT, FMT = 9999 )
 9999 FORMAT( 1X, '.. test output of AB_ZGGBAK .. ' )
*
      WRITE( NOUT, FMT = 9998 )RMAX
 9998 FORMAT( ' value of largest test error                  =', D12.3 )
      WRITE( NOUT, FMT = 9997 )LMAX( 1 )
 9997 FORMAT( ' example number where AB_ZGGBAL info is not 0    =', I4 )
      WRITE( NOUT, FMT = 9996 )LMAX( 2 )
 9996 FORMAT( ' example number where AB_ZGGBAK(L) info is not 0 =', I4 )
      WRITE( NOUT, FMT = 9995 )LMAX( 3 )
 9995 FORMAT( ' example number where AB_ZGGBAK(R) info is not 0 =', I4 )
      WRITE( NOUT, FMT = 9994 )LMAX( 4 )
 9994 FORMAT( ' example number having largest error          =', I4 )
      WRITE( NOUT, FMT = 9992 )NINFO
 9992 FORMAT( ' number of examples where info is not 0       =', I4 )
      WRITE( NOUT, FMT = 9991 )KNT
 9991 FORMAT( ' total number of examples tested              =', I4 )
*
      RETURN
*
*     End of AB_ZCHKGK
*
      END
