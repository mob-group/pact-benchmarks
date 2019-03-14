*> \brief \b AB_ZHSEQR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ZHSEQR + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ZHSEQR.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ZHSEQR.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ZHSEQR.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,
*                          WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            IHI, ILO, INFO, LDH, LDZ, LWORK, N
*       CHARACTER          COMPZ, JOB
*       ..
*       .. Array Arguments ..
*       COMPLEX*16         H( LDH, * ), W( * ), WORK( * ), Z( LDZ, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    AB_ZHSEQR computes the eigenvalues of a Hessenberg matrix H
*>    and, optionally, the matrices T and Z from the Schur decomposition
*>    H = Z T Z**H, where T is an upper triangular matrix (the
*>    Schur form), and Z is the unitary matrix of Schur vectors.
*>
*>    Optionally Z may be postmultiplied into an input unitary
*>    matrix Q so that this routine can give the Schur factorization
*>    of a matrix A which has been reduced to the Hessenberg form H
*>    by the unitary matrix Q:  A = Q*H*Q**H = (QZ)*T*(QZ)**H.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] JOB
*> \verbatim
*>          JOB is CHARACTER*1
*>           = 'E':  compute eigenvalues only;
*>           = 'S':  compute eigenvalues and the Schur form T.
*> \endverbatim
*>
*> \param[in] COMPZ
*> \verbatim
*>          COMPZ is CHARACTER*1
*>           = 'N':  no Schur vectors are computed;
*>           = 'I':  Z is initialized to the unit matrix and the matrix Z
*>                   of Schur vectors of H is returned;
*>           = 'V':  Z must contain an unitary matrix Q on entry, and
*>                   the product Q*Z is returned.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>           The order of the matrix H.  N .GE. 0.
*> \endverbatim
*>
*> \param[in] ILO
*> \verbatim
*>          ILO is INTEGER
*> \endverbatim
*>
*> \param[in] IHI
*> \verbatim
*>          IHI is INTEGER
*>
*>           It is assumed that H is already upper triangular in rows
*>           and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
*>           set by a previous call to AB_ZGEBAL, and then passed to AB_ZGEHRD
*>           when the matrix output by AB_ZGEBAL is reduced to Hessenberg
*>           form. Otherwise ILO and IHI should be set to 1 and N
*>           respectively.  If N.GT.0, then 1.LE.ILO.LE.IHI.LE.N.
*>           If N = 0, then ILO = 1 and IHI = 0.
*> \endverbatim
*>
*> \param[in,out] H
*> \verbatim
*>          H is COMPLEX*16 array, dimension (LDH,N)
*>           On entry, the upper Hessenberg matrix H.
*>           On exit, if INFO = 0 and JOB = 'S', H contains the upper
*>           triangular matrix T from the Schur decomposition (the
*>           Schur form). If INFO = 0 and JOB = 'E', the contents of
*>           H are unspecified on exit.  (The output value of H when
*>           INFO.GT.0 is given under the description of INFO below.)
*>
*>           Unlike earlier versions of AB_ZHSEQR, this subroutine may
*>           explicitly H(i,j) = 0 for i.GT.j and j = 1, 2, ... ILO-1
*>           or j = IHI+1, IHI+2, ... N.
*> \endverbatim
*>
*> \param[in] LDH
*> \verbatim
*>          LDH is INTEGER
*>           The leading dimension of the array H. LDH .GE. max(1,N).
*> \endverbatim
*>
*> \param[out] W
*> \verbatim
*>          W is COMPLEX*16 array, dimension (N)
*>           The computed eigenvalues. If JOB = 'S', the eigenvalues are
*>           stored in the same order as on the diagonal of the Schur
*>           form returned in H, with W(i) = H(i,i).
*> \endverbatim
*>
*> \param[in,out] Z
*> \verbatim
*>          Z is COMPLEX*16 array, dimension (LDZ,N)
*>           If COMPZ = 'N', Z is not referenced.
*>           If COMPZ = 'I', on entry Z need not be set and on exit,
*>           if INFO = 0, Z contains the unitary matrix Z of the Schur
*>           vectors of H.  If COMPZ = 'V', on entry Z must contain an
*>           N-by-N matrix Q, which is assumed to be equal to the unit
*>           matrix except for the submatrix Z(ILO:IHI,ILO:IHI). On exit,
*>           if INFO = 0, Z contains Q*Z.
*>           Normally Q is the unitary matrix generated by AB_ZUNGHR
*>           after the call to AB_ZGEHRD which formed the Hessenberg matrix
*>           H. (The output value of Z when INFO.GT.0 is given under
*>           the description of INFO below.)
*> \endverbatim
*>
*> \param[in] LDZ
*> \verbatim
*>          LDZ is INTEGER
*>           The leading dimension of the array Z.  if COMPZ = 'I' or
*>           COMPZ = 'V', then LDZ.GE.MAX(1,N).  Otherwize, LDZ.GE.1.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (LWORK)
*>           On exit, if INFO = 0, WORK(1) returns an estimate of
*>           the optimal value for LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>           The dimension of the array WORK.  LWORK .GE. max(1,N)
*>           is sufficient and delivers very good and sometimes
*>           optimal performance.  However, LWORK as large as 11*N
*>           may be required for optimal performance.  A workspace
*>           query is recommended to determine the optimal workspace
*>           size.
*>
*>           If LWORK = -1, then AB_ZHSEQR does a workspace query.
*>           In this case, AB_ZHSEQR checks the input parameters and
*>           estimates the optimal workspace size for the given
*>           values of N, ILO and IHI.  The estimate is returned
*>           in WORK(1).  No error message related to LWORK is
*>           issued by AB_XERBLA.  Neither H nor Z are accessed.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>             =  0:  successful exit
*>           .LT. 0:  if INFO = -i, the i-th argument had an illegal
*>                    value
*>           .GT. 0:  if INFO = i, AB_ZHSEQR failed to compute all of
*>                the eigenvalues.  Elements 1:ilo-1 and i+1:n of WR
*>                and WI contain those eigenvalues which have been
*>                successfully computed.  (Failures are rare.)
*>
*>                If INFO .GT. 0 and JOB = 'E', then on exit, the
*>                remaining unconverged eigenvalues are the eigen-
*>                values of the upper Hessenberg matrix rows and
*>                columns ILO through INFO of the final, output
*>                value of H.
*>
*>                If INFO .GT. 0 and JOB   = 'S', then on exit
*>
*>           (*)  (initial value of H)*U  = U*(final value of H)
*>
*>                where U is a unitary matrix.  The final
*>                value of  H is upper Hessenberg and triangular in
*>                rows and columns INFO+1 through IHI.
*>
*>                If INFO .GT. 0 and COMPZ = 'V', then on exit
*>
*>                  (final value of Z)  =  (initial value of Z)*U
*>
*>                where U is the unitary matrix in (*) (regard-
*>                less of the value of JOB.)
*>
*>                If INFO .GT. 0 and COMPZ = 'I', then on exit
*>                      (final value of Z)  = U
*>                where U is the unitary matrix in (*) (regard-
*>                less of the value of JOB.)
*>
*>                If INFO .GT. 0 and COMPZ = 'N', then Z is not
*>                accessed.
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
*> \ingroup complex16OTHERcomputational
*
*> \par Contributors:
*  ==================
*>
*>       Karen Braman and Ralph Byers, Department of Mathematics,
*>       University of Kansas, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>             Default values supplied by
*>             AB_ILAENV(ISPEC,'AB_ZHSEQR',JOB(:1)//COMPZ(:1),N,ILO,IHI,LWORK).
*>             It is suggested that these defaults be adjusted in order
*>             to attain best performance in each particular
*>             computational environment.
*>
*>            ISPEC=12: The AB_ZLAHQR vs AB_ZLAQR0 crossover point.
*>                      Default: 75. (Must be at least 11.)
*>
*>            ISPEC=13: Recommended deflation window size.
*>                      This depends on ILO, IHI and NS.  NS is the
*>                      number of simultaneous shifts returned
*>                      by AB_ILAENV(ISPEC=15).  (See ISPEC=15 below.)
*>                      The default for (IHI-ILO+1).LE.500 is NS.
*>                      The default for (IHI-ILO+1).GT.500 is 3*NS/2.
*>
*>            ISPEC=14: Nibble crossover point. (See AB_IPARMQ for
*>                      details.)  Default: 14% of deflation window
*>                      size.
*>
*>            ISPEC=15: Number of simultaneous shifts in a multishift
*>                      QR iteration.
*>
*>                      If IHI-ILO+1 is ...
*>
*>                      greater than      ...but less    ... the
*>                      or equal to ...      than        default is
*>
*>                           1               30          NS =   2(+)
*>                          30               60          NS =   4(+)
*>                          60              150          NS =  10(+)
*>                         150              590          NS =  **
*>                         590             3000          NS =  64
*>                        3000             6000          NS = 128
*>                        6000             infinity      NS = 256
*>
*>                  (+)  By default some or all matrices of this order
*>                       are passed to the implicit double shift routine
*>                       AB_ZLAHQR and this parameter is ignored.  See
*>                       ISPEC=12 above and comments in AB_IPARMQ for
*>                       details.
*>
*>                 (**)  The asterisks (**) indicate an ad-hoc
*>                       function of N increasing from 10 to 64.
*>
*>            ISPEC=16: Select structured matrix multiply.
*>                      If the number of simultaneous shifts (specified
*>                      by ISPEC=15) is less than 14, then the default
*>                      for ISPEC=16 is 0.  Otherwise the default for
*>                      ISPEC=16 is 2.
*> \endverbatim
*
*> \par References:
*  ================
*>
*>       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
*>       Algorithm Part I: Maintaining Well Focused Shifts, and Level 3
*>       Performance, SIAM Journal of Matrix Analysis, volume 23, pages
*>       929--947, 2002.
*> \n
*>       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
*>       Algorithm Part II: Aggressive Early Deflation, SIAM Journal
*>       of Matrix Analysis, volume 23, pages 948--973, 2002.
*
*  =====================================================================
      SUBROUTINE AB_ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            IHI, ILO, INFO, LDH, LDZ, LWORK, N
      CHARACTER          COMPZ, JOB
*     ..
*     .. Array Arguments ..
      COMPLEX*16         H( LDH, * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
*
*     ==== Matrices of order NTINY or smaller must be processed by
*     .    AB_ZLAHQR because of insufficient subdiagonal scratch space.
*     .    (This is a hard limit.) ====
      INTEGER            NTINY
      PARAMETER          ( NTINY = 11 )
*
*     ==== NL allocates some local workspace to help small matrices
*     .    through a rare AB_ZLAHQR failure.  NL .GT. NTINY = 11 is
*     .    required and NL .LE. NMIN = AB_ILAENV(ISPEC=12,...) is recom-
*     .    mended.  (The default value of NMIN is 75.)  Using NL = 49
*     .    allows up to six simultaneous shifts and a 16-by-16
*     .    deflation window.  ====
      INTEGER            NL
      PARAMETER          ( NL = 49 )
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0d0, 0.0d0 ),
     $                   ONE = ( 1.0d0, 0.0d0 ) )
      DOUBLE PRECISION   RZERO
      PARAMETER          ( RZERO = 0.0d0 )
*     ..
*     .. Local Arrays ..
      COMPLEX*16         HL( NL, NL ), WORKL( NL )
*     ..
*     .. Local Scalars ..
      INTEGER            KBOT, NMIN
      LOGICAL            INITZ, LQUERY, WANTT, WANTZ
*     ..
*     .. External Functions ..
      INTEGER            AB_ILAENV
      LOGICAL            AB_LSAME
      EXTERNAL           AB_ILAENV, AB_LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_XERBLA, AB_ZCOPY, AB_ZLACPY, AB_ZLAHQR, AB_Z
     $LAQR0, AB_ZLASET
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     ==== Decode and check the input parameters. ====
*
      WANTT = AB_LSAME( JOB, 'S' )
      INITZ = AB_LSAME( COMPZ, 'I' )
      WANTZ = INITZ .OR. AB_LSAME( COMPZ, 'V' )
      WORK( 1 ) = DCMPLX( DBLE( MAX( 1, N ) ), RZERO )
      LQUERY = LWORK.EQ.-1
*
      INFO = 0
      IF( .NOT.AB_LSAME( JOB, 'E' ) .AND. .NOT.WANTT ) THEN
         INFO = -1
      ELSE IF( .NOT.AB_LSAME( COMPZ, 'N' ) .AND. .NOT.WANTZ ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -5
      ELSE IF( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.MAX( 1, N ) ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
*
      IF( INFO.NE.0 ) THEN
*
*        ==== Quick return in case of invalid argument. ====
*
         CALL AB_XERBLA( 'AB_ZHSEQR', -INFO )
         RETURN
*
      ELSE IF( N.EQ.0 ) THEN
*
*        ==== Quick return in case N = 0; nothing to do. ====
*
         RETURN
*
      ELSE IF( LQUERY ) THEN
*
*        ==== Quick return in case of a workspace query ====
*
         CALL AB_ZLAQR0( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILO, IHI,
     $ Z,
     $                LDZ, WORK, LWORK, INFO )
*        ==== Ensure reported workspace size is backward-compatible with
*        .    previous LAPACK versions. ====
         WORK( 1 ) = DCMPLX( MAX( DBLE( WORK( 1 ) ), DBLE( MAX( 1,
     $               N ) ) ), RZERO )
         RETURN
*
      ELSE
*
*        ==== copy eigenvalues isolated by AB_ZGEBAL ====
*
         IF( ILO.GT.1 )
     $      CALL AB_ZCOPY( ILO-1, H, LDH+1, W, 1 )
         IF( IHI.LT.N )
     $      CALL AB_ZCOPY( N-IHI, H( IHI+1, IHI+1 ), LDH+1, W( IHI+1 ), 
     $1 )
*
*        ==== Initialize Z, if requested ====
*
         IF( INITZ )
     $      CALL AB_ZLASET( 'A', N, N, ZERO, ONE, Z, LDZ )
*
*        ==== Quick return if possible ====
*
         IF( ILO.EQ.IHI ) THEN
            W( ILO ) = H( ILO, ILO )
            RETURN
         END IF
*
*        ==== AB_ZLAHQR/AB_ZLAQR0 crossover point ====
*
         NMIN = AB_ILAENV( 12, 'AB_ZHSEQR', JOB( : 1 ) // COMPZ( : 1 ), 
     $N,
     $          ILO, IHI, LWORK )
         NMIN = MAX( NTINY, NMIN )
*
*        ==== AB_ZLAQR0 for big matrices; AB_ZLAHQR for small ones ====
*
         IF( N.GT.NMIN ) THEN
            CALL AB_ZLAQR0( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILO, I
     $HI,
     $                   Z, LDZ, WORK, LWORK, INFO )
         ELSE
*
*           ==== Small matrix ====
*
            CALL AB_ZLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILO, I
     $HI,
     $                   Z, LDZ, INFO )
*
            IF( INFO.GT.0 ) THEN
*
*              ==== A rare AB_ZLAHQR failure!  AB_ZLAQR0 sometimes succeeds
*              .    when AB_ZLAHQR fails. ====
*
               KBOT = INFO
*
               IF( N.GE.NL ) THEN
*
*                 ==== Larger matrices have enough subdiagonal scratch
*                 .    space to call AB_ZLAQR0 directly. ====
*
                  CALL AB_ZLAQR0( WANTT, WANTZ, N, ILO, KBOT, H, LDH, W,
     $                         ILO, IHI, Z, LDZ, WORK, LWORK, INFO )
*
               ELSE
*
*                 ==== Tiny matrices don't have enough subdiagonal
*                 .    scratch space to benefit from AB_ZLAQR0.  Hence,
*                 .    tiny matrices must be copied into a larger
*                 .    array before calling AB_ZLAQR0. ====
*
                  CALL AB_ZLACPY( 'A', N, N, H, LDH, HL, NL )
                  HL( N+1, N ) = ZERO
                  CALL AB_ZLASET( 'A', NL, NL-N, ZERO, ZERO, HL( 1, N+1 
     $),
     $                         NL )
                  CALL AB_ZLAQR0( WANTT, WANTZ, NL, ILO, KBOT, HL, NL, W
     $,
     $                         ILO, IHI, Z, LDZ, WORKL, NL, INFO )
                  IF( WANTT .OR. INFO.NE.0 )
     $               CALL AB_ZLACPY( 'A', N, N, HL, NL, H, LDH )
               END IF
            END IF
         END IF
*
*        ==== Clear out the trash, if necessary. ====
*
         IF( ( WANTT .OR. INFO.NE.0 ) .AND. N.GT.2 )
     $      CALL AB_ZLASET( 'L', N-2, N-2, ZERO, ZERO, H( 3, 1 ), LDH )
*
*        ==== Ensure reported workspace size is backward-compatible with
*        .    previous LAPACK versions. ====
*
         WORK( 1 ) = DCMPLX( MAX( DBLE( MAX( 1, N ) ),
     $               DBLE( WORK( 1 ) ) ), RZERO )
      END IF
*
*     ==== End of AB_ZHSEQR ====
*
      END
