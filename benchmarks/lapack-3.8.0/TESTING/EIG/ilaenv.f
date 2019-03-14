*> \brief \b AB_ILAENV
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       INTEGER          FUNCTION AB_ILAENV( ISPEC, NAME, OPTS, N1, N2, N3,
*                        N4 )
*
*       .. Scalar Arguments ..
*       CHARACTER*( * )    NAME, OPTS
*       INTEGER            ISPEC, N1, N2, N3, N4
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_ILAENV returns problem-dependent parameters for the local
*> environment.  See ISPEC for a description of the parameters.
*>
*> In this version, the problem-dependent parameters are contained in
*> the integer array IPARMS in the common block CLAENV and the value
*> with index ISPEC is copied to AB_ILAENV.  This version of AB_ILAENV is
*> to be used in conjunction with AB_XLAENV in TESTING and TIMING.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] ISPEC
*> \verbatim
*>          ISPEC is INTEGER
*>          Specifies the parameter to be returned as the value of
*>          AB_ILAENV.
*>          = 1: the optimal blocksize; if this value is 1, an unblocked
*>               algorithm will give the best performance.
*>          = 2: the minimum block size for which the block routine
*>               should be used; if the usable block size is less than
*>               this value, an unblocked routine should be used.
*>          = 3: the crossover point (in a block routine, for N less
*>               than this value, an unblocked routine should be used)
*>          = 4: the number of shifts, used in the nonsymmetric
*>               eigenvalue routines
*>          = 5: the minimum column dimension for blocking to be used;
*>               rectangular blocks must have dimension at least k by m,
*>               where k is given by AB_ILAENV(2,...) and m by AB_ILAENV(5,...)
*>          = 6: the crossover point for the SVD (when reducing an m by n
*>               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*>               this value, a QR factorization is used first to reduce
*>               the matrix to a triangular form.)
*>          = 7: the number of processors
*>          = 8: the crossover point for the multishift QR and QZ methods
*>               for nonsymmetric eigenvalue problems.
*>          = 9: maximum size of the subproblems at the bottom of the
*>               computation tree in the divide-and-conquer algorithm
*>          =10: ieee NaN arithmetic can be trusted not to trap
*>          =11: infinity arithmetic can be trusted not to trap
*>          12 <= ISPEC <= 16:
*>               xHSEQR or one of its subroutines,
*>               see AB_IPARMQ for detailed explanation
*>
*>          Other specifications (up to 100) can be added later.
*> \endverbatim
*>
*> \param[in] NAME
*> \verbatim
*>          NAME is CHARACTER*(*)
*>          The name of the calling subroutine.
*> \endverbatim
*>
*> \param[in] OPTS
*> \verbatim
*>          OPTS is CHARACTER*(*)
*>          The character options to the subroutine NAME, concatenated
*>          into a single character string.  For example, UPLO = 'U',
*>          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*>          be specified as OPTS = 'UTN'.
*> \endverbatim
*>
*> \param[in] N1
*> \verbatim
*>          N1 is INTEGER
*> \endverbatim
*>
*> \param[in] N2
*> \verbatim
*>          N2 is INTEGER
*> \endverbatim
*>
*> \param[in] N3
*> \verbatim
*>          N3 is INTEGER
*> \endverbatim
*>
*> \param[in] N4
*> \verbatim
*>          N4 is INTEGER
*>
*>          Problem dimensions for the subroutine NAME; these may not all
*>          be required.
*> \endverbatim
*>
*> \result AB_ILAENV
*> \verbatim
*>          AB_ILAENV is INTEGER
*>          >= 0: the value of the parameter specified by ISPEC
*>          < 0:  if AB_ILAENV = -k, the k-th argument had an illegal value.
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
*> \ingroup OTHERauxiliary
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The following conventions have been used when calling AB_ILAENV from the
*>  LAPACK routines:
*>  1)  OPTS is a concatenation of all of the character options to
*>      subroutine NAME, in the same order that they appear in the
*>      argument list for NAME, even if they are not used in determining
*>      the value of the parameter specified by ISPEC.
*>  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*>      that they appear in the argument list for NAME.  N1 is used
*>      first, N2 AB_SECOND, and so on, and unused problem dimensions are
*>      passed a value of -1.
*>  3)  The parameter value returned by AB_ILAENV is checked for validity in
*>      the calling subroutine.  For example, AB_ILAENV is used to retrieve
*>      the optimal blocksize for AB_STRTRI as follows:
*>
*>      NB = AB_ILAENV( 1, 'AB_STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*>      IF( NB.LE.1 ) NB = MAX( 1, N )
*> \endverbatim
*>
*  =====================================================================
      INTEGER          FUNCTION AB_ILAENV( ISPEC, NAME, OPTS, N1, N2, N3
     $,
     $                 N4 )
*
*  -- LAPACK test routine (version 3.8.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
*     ..
*
*  =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MIN, REAL
*     ..
*     .. External Functions ..
      INTEGER            AB_IEEECK, IPARAM2STAGE
      EXTERNAL           AB_IEEECK, IPARAM2STAGE
*     ..
*     .. Arrays in Common ..
      INTEGER            IPARMS( 100 )
*     ..
*     .. Common blocks ..
      COMMON             / CLAENV / IPARMS
*     ..
*     .. Save statement ..
      SAVE               / CLAENV /
*     ..
*     .. Executable Statements ..
*
      IF( ISPEC.GE.1 .AND. ISPEC.LE.5 ) THEN
*
*        Return a value from the common block.
*
         AB_ILAENV = IPARMS( ISPEC )
*
      ELSE IF( ISPEC.EQ.6 ) THEN
*
*        Compute SVD crossover point.
*
         AB_ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
*
      ELSE IF( ISPEC.GE.7 .AND. ISPEC.LE.9 ) THEN
*
*        Return a value from the common block.
*
         AB_ILAENV = IPARMS( ISPEC )
*
      ELSE IF( ISPEC.EQ.10 ) THEN
*
*        IEEE NaN arithmetic can be trusted not to trap
*
C        AB_ILAENV = 0
         AB_ILAENV = 1
         IF( AB_ILAENV.EQ.1 ) THEN
            AB_ILAENV = AB_IEEECK( 1, 0.0, 1.0 )
         END IF
*
      ELSE IF( ISPEC.EQ.11 ) THEN
*
*        Infinity arithmetic can be trusted not to trap
*
C        AB_ILAENV = 0
         AB_ILAENV = 1
         IF( AB_ILAENV.EQ.1 ) THEN
            AB_ILAENV = AB_IEEECK( 0, 0.0, 1.0 )
         END IF
*
      ELSE IF(( ISPEC.GE.12 ) .AND. (ISPEC.LE.16)) THEN
*
*     12 <= ISPEC <= 16: xHSEQR or one of its subroutines.
*
         AB_ILAENV = IPARMS( ISPEC )
*         WRITE(*,*) 'ISPEC = ',ISPEC,' AB_ILAENV =',AB_ILAENV
*         AB_ILAENV = AB_IPARMQ( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
*
      ELSE IF(( ISPEC.GE.17 ) .AND. (ISPEC.LE.21)) THEN
*
*     17 <= ISPEC <= 21: 2stage eigenvalues SVD routines. 
*
         IF( ISPEC.EQ.17 ) THEN
             AB_ILAENV = IPARMS( 1 )
         ELSE
             AB_ILAENV = IPARAM2STAGE( ISPEC, NAME, OPTS, N1, N2, N3, N4
     $ ) 
         ENDIF
*
      ELSE
*
*        Invalid value for ISPEC
*
         AB_ILAENV = -1
      END IF
*
      RETURN
*
*     End of AB_ILAENV
*
      END
      INTEGER FUNCTION AB_AB_ILAENV2STAGE( ISPEC, NAME, OPTS, N1, N2,
     $                               N3, N4 )
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
*     ..
*
*  =====================================================================
*
*     .. Local variables ..
      INTEGER            IISPEC
*     .. External Functions ..
      INTEGER            IPARAM2STAGE
      EXTERNAL           IPARAM2STAGE
*     ..
*     .. Arrays in Common ..
      INTEGER            IPARMS( 100 )
*     ..
*     .. Common blocks ..
      COMMON             / CLAENV / IPARMS
*     ..
*     .. Save statement ..
      SAVE               / CLAENV /
*     ..
*     .. Executable Statements ..
*
      IF(( ISPEC.GE.1 ) .AND. (ISPEC.LE.5)) THEN
*
*     1 <= ISPEC <= 5: 2stage eigenvalues SVD routines. 
*
         IF( ISPEC.EQ.1 ) THEN
             AB_AB_ILAENV2STAGE = IPARMS( 1 )
         ELSE
             IISPEC = 16 + ISPEC
             AB_AB_ILAENV2STAGE = IPARAM2STAGE( IISPEC, NAME, OPTS,
     $                                    N1, N2, N3, N4 ) 
         ENDIF
*
      ELSE
*
*        Invalid value for ISPEC
*
         AB_AB_ILAENV2STAGE = -1
      END IF
*
      RETURN
      END
      INTEGER FUNCTION AB_IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK 
     $)
*
      INTEGER            INMIN, INWIN, INIBL, ISHFTS, IACC22
      PARAMETER          ( INMIN = 12, INWIN = 13, INIBL = 14,
     $                   ISHFTS = 15, IACC22 = 16 )
      INTEGER            NMIN, K22MIN, KACMIN, NIBBLE, KNWSWP
      PARAMETER          ( NMIN = 11, K22MIN = 14, KACMIN = 14,
     $                   NIBBLE = 14, KNWSWP = 500 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0 )
*     ..
*     .. Scalar Arguments ..
      INTEGER            IHI, ILO, ISPEC, LWORK, N
      CHARACTER          NAME*( * ), OPTS*( * )
*     ..
*     .. Local Scalars ..
      INTEGER            NH, NS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LOG, MAX, MOD, NINT, REAL
*     ..
*     .. Executable Statements ..
      IF( ( ISPEC.EQ.ISHFTS ) .OR. ( ISPEC.EQ.INWIN ) .OR.
     $    ( ISPEC.EQ.IACC22 ) ) THEN
*
*        ==== Set the number simultaneous shifts ====
*
         NH = IHI - ILO + 1
         NS = 2
         IF( NH.GE.30 )
     $      NS = 4
         IF( NH.GE.60 )
     $      NS = 10
         IF( NH.GE.150 )
     $      NS = MAX( 10, NH / NINT( LOG( REAL( NH ) ) / LOG( TWO ) ) )
         IF( NH.GE.590 )
     $      NS = 64
         IF( NH.GE.3000 )
     $      NS = 128
         IF( NH.GE.6000 )
     $      NS = 256
         NS = MAX( 2, NS-MOD( NS, 2 ) )
      END IF
*
      IF( ISPEC.EQ.INMIN ) THEN
*
*
*        ===== Matrices of order smaller than NMIN get sent
*        .     to LAHQR, the classic double shift algorithm.
*        .     This must be at least 11. ====
*
         AB_IPARMQ = NMIN
*
      ELSE IF( ISPEC.EQ.INIBL ) THEN
*
*        ==== INIBL: skip a multi-shift qr iteration and
*        .    whenever aggressive early deflation finds
*        .    at least (NIBBLE*(window size)/100) deflations. ====
*
         AB_IPARMQ = NIBBLE
*
      ELSE IF( ISPEC.EQ.ISHFTS ) THEN
*
*        ==== NSHFTS: The number of simultaneous shifts =====
*
         AB_IPARMQ = NS
*
      ELSE IF( ISPEC.EQ.INWIN ) THEN
*
*        ==== NW: deflation window size.  ====
*
         IF( NH.LE.KNWSWP ) THEN
            AB_IPARMQ = NS
         ELSE
            AB_IPARMQ = 3*NS / 2
         END IF
*
      ELSE IF( ISPEC.EQ.IACC22 ) THEN
*
*        ==== IACC22: Whether to accumulate reflections
*        .     before updating the far-from-diagonal elements
*        .     and whether to use 2-by-2 block structure while
*        .     doing it.  A small amount of work could be saved
*        .     by making this choice dependent also upon the
*        .     NH=IHI-ILO+1.
*
         AB_IPARMQ = 0
         IF( NS.GE.KACMIN )
     $      AB_IPARMQ = 1
         IF( NS.GE.K22MIN )
     $      AB_IPARMQ = 2
*
      ELSE
*        ===== invalid value of ispec =====
         AB_IPARMQ = -1
*
      END IF
*
*     ==== End of AB_IPARMQ ====
*
      END
