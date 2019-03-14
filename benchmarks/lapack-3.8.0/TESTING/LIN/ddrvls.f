*> \brief \b AB_DDRVLS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_DDRVLS( DOTYPE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
*                          NBVAL, NXVAL, THRESH, TSTERR, A, COPYA, B,
*                          COPYB, C, S, COPYS, NOUT )
*
*       .. Scalar Arguments ..
*       LOGICAL            TSTERR
*       INTEGER            NM, NN, NNB, NNS, NOUT
*       DOUBLE PRECISION   THRESH
*       ..
*       .. Array Arguments ..
*       LOGICAL            DOTYPE( * )
*       INTEGER            MVAL( * ), NBVAL( * ), NSVAL( * ),
*      $                   NVAL( * ), NXVAL( * )
*       DOUBLE PRECISION   A( * ), B( * ), C( * ), COPYA( * ), COPYB( * ),
*      $                   COPYS( * ), S( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DDRVLS tests the least squares driver routines AB_DGELS, AB_DGETSLS, AB_AB_DGELSS, AB_AB_DGELSY,
*> and AB_AB_DGELSD.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] DOTYPE
*> \verbatim
*>          DOTYPE is LOGICAL array, dimension (NTYPES)
*>          The matrix types to be used for testing.  Matrices of type j
*>          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*>          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*>          The matrix of type j is generated as follows:
*>          j=1: A = U*D*V where U and V are random orthogonal matrices
*>               and D has random entries (> 0.1) taken from a uniform
*>               distribution (0,1). A is full rank.
*>          j=2: The same of 1, but A is scaled up.
*>          j=3: The same of 1, but A is scaled down.
*>          j=4: A = U*D*V where U and V are random orthogonal matrices
*>               and D has 3*min(M,N)/4 random entries (> 0.1) taken
*>               from a uniform distribution (0,1) and the remaining
*>               entries set to 0. A is rank-deficient.
*>          j=5: The same of 4, but A is scaled up.
*>          j=6: The same of 5, but A is scaled down.
*> \endverbatim
*>
*> \param[in] NM
*> \verbatim
*>          NM is INTEGER
*>          The number of values of M contained in the vector MVAL.
*> \endverbatim
*>
*> \param[in] MVAL
*> \verbatim
*>          MVAL is INTEGER array, dimension (NM)
*>          The values of the matrix row dimension M.
*> \endverbatim
*>
*> \param[in] NN
*> \verbatim
*>          NN is INTEGER
*>          The number of values of N contained in the vector NVAL.
*> \endverbatim
*>
*> \param[in] NVAL
*> \verbatim
*>          NVAL is INTEGER array, dimension (NN)
*>          The values of the matrix column dimension N.
*> \endverbatim
*>
*> \param[in] NNS
*> \verbatim
*>          NNS is INTEGER
*>          The number of values of NRHS contained in the vector NSVAL.
*> \endverbatim
*>
*> \param[in] NSVAL
*> \verbatim
*>          NSVAL is INTEGER array, dimension (NNS)
*>          The values of the number of right hand sides NRHS.
*> \endverbatim
*>
*> \param[in] NNB
*> \verbatim
*>          NNB is INTEGER
*>          The number of values of NB and NX contained in the
*>          vectors NBVAL and NXVAL.  The blocking parameters are used
*>          in pairs (NB,NX).
*> \endverbatim
*>
*> \param[in] NBVAL
*> \verbatim
*>          NBVAL is INTEGER array, dimension (NNB)
*>          The values of the blocksize NB.
*> \endverbatim
*>
*> \param[in] NXVAL
*> \verbatim
*>          NXVAL is INTEGER array, dimension (NNB)
*>          The values of the crossover point NX.
*> \endverbatim
*>
*> \param[in] THRESH
*> \verbatim
*>          THRESH is DOUBLE PRECISION
*>          The threshold value for the test ratios.  A result is
*>          included in the output file if RESULT >= THRESH.  To have
*>          every test ratio printed, use THRESH = 0.
*> \endverbatim
*>
*> \param[in] TSTERR
*> \verbatim
*>          TSTERR is LOGICAL
*>          Flag that indicates whether error exits are to be tested.
*> \endverbatim
*>
*> \param[out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (MMAX*NMAX)
*>          where MMAX is the maximum value of M in MVAL and NMAX is the
*>          maximum value of N in NVAL.
*> \endverbatim
*>
*> \param[out] COPYA
*> \verbatim
*>          COPYA is DOUBLE PRECISION array, dimension (MMAX*NMAX)
*> \endverbatim
*>
*> \param[out] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension (MMAX*NSMAX)
*>          where MMAX is the maximum value of M in MVAL and NSMAX is the
*>          maximum value of NRHS in NSVAL.
*> \endverbatim
*>
*> \param[out] COPYB
*> \verbatim
*>          COPYB is DOUBLE PRECISION array, dimension (MMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] C
*> \verbatim
*>          C is DOUBLE PRECISION array, dimension (MMAX*NSMAX)
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is DOUBLE PRECISION array, dimension
*>                      (min(MMAX,NMAX))
*> \endverbatim
*>
*> \param[out] COPYS
*> \verbatim
*>          COPYS is DOUBLE PRECISION array, dimension
*>                      (min(MMAX,NMAX))
*> \endverbatim
*>
*> \param[in] NOUT
*> \verbatim
*>          NOUT is INTEGER
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
*> \date June 2017
*
*> \ingroup double_lin
*
*  =====================================================================
      SUBROUTINE AB_DDRVLS( DOTYPE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
     $                   NBVAL, NXVAL, THRESH, TSTERR, A, COPYA, B,
     $                   COPYB, C, S, COPYS, NOUT )
*
*  -- LAPACK test routine (version 3.7.1) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2017
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NM, NN, NNB, NNS, NOUT
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            MVAL( * ), NBVAL( * ), NSVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), COPYA( * ), COPYB( * ),
     $                   COPYS( * ), S( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 16 )
      INTEGER            SMLSIZ
      PARAMETER          ( SMLSIZ = 25 )
      DOUBLE PRECISION   ONE, TWO, ZERO
      PARAMETER          ( ONE = 1.0D0, TWO = 2.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANS
      CHARACTER*3        PATH
      INTEGER            CRANK, I, IM, IMB, IN, INB, INFO, INS, IRANK,
     $                   ISCALE, ITRAN, ITYPE, J, K, LDA, LDB, LDWORK,
     $                   LWLSY, LWORK, M, MNMIN, N, NB, NCOLS, NERRS,
     $                   NFAIL, NRHS, NROWS, NRUN, RANK, MB,
     $                   MMAX, NMAX, NSMAX, LIWORK,
     $                   LWORK_AB_DGELS, LWORK_AB_DGETSLS, LWORK_AB_AB_D
     $GELSS,
     $                   LWORK_AB_AB_DGELSY, LWORK_AB_AB_DGELSD
      DOUBLE PRECISION   EPS, NORMA, NORMB, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 ), IWQ
      DOUBLE PRECISION   RESULT( NTESTS ), WQ
*     ..
*     .. Allocatable Arrays ..
      DOUBLE PRECISION, ALLOCATABLE :: WORK (:)
      INTEGER, ALLOCATABLE :: IWORK (:)
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   AB_DASUM, AB_DLAMCH, AB_DQRT12, AB_DQRT14, AB_D
     $QRT17
      EXTERNAL           AB_DASUM, AB_DLAMCH, AB_DQRT12, AB_DQRT14, AB_D
     $QRT17
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAERH, AB_ALAHD, AB_ALASVM, AB_DAXPY, AB_DE
     $RRLS, AB_DGELS,
     $                   AB_AB_DGELSD, AB_AB_DGELSS, AB_AB_DGELSY, AB_DG
     $EMM, AB_DLACPY,
     $                   AB_DLARNV, AB_AB_DLASRT, AB_DQRT13, AB_DQRT15, 
     $AB_DQRT16, AB_DSCAL,
     $                   AB_XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, LOG, MAX, MIN, SQRT
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, IOUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, IOUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'LS'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
      EPS = AB_DLAMCH( 'Epsilon' )
*
*     Threshold for rank estimation
*
      RCOND = SQRT( EPS ) - ( SQRT( EPS )-EPS ) / 2
*
*     Test the error exits
*
      CALL AB_XLAENV( 2, 2 )
      CALL AB_XLAENV( 9, SMLSIZ )
      IF( TSTERR )
     $   CALL AB_DERRLS( PATH, NOUT )
*
*     Print the AB_HEADER if NM = 0 or NN = 0 and THRESH = 0.
*
      IF( ( NM.EQ.0 .OR. NN.EQ.0 ) .AND. THRESH.EQ.ZERO )
     $   CALL AB_ALAHD( NOUT, PATH )
      INFOT = 0
      CALL AB_XLAENV( 2, 2 )
      CALL AB_XLAENV( 9, SMLSIZ )
*
*     Compute maximal workspace needed for all routines
*
      NMAX = 0
      MMAX = 0
      NSMAX = 0
      DO I = 1, NM
         IF ( MVAL( I ).GT.MMAX ) THEN
            MMAX = MVAL( I )
         END IF
      ENDDO
      DO I = 1, NN
         IF ( NVAL( I ).GT.NMAX ) THEN
            NMAX = NVAL( I )
         END IF
      ENDDO
      DO I = 1, NNS
         IF ( NSVAL( I ).GT.NSMAX ) THEN
            NSMAX = NSVAL( I )
         END IF
      ENDDO
      M = MMAX
      N = NMAX
      NRHS = NSMAX
      MNMIN = MAX( MIN( M, N ), 1 )
*
*     Compute workspace needed for routines
*     AB_DQRT14, AB_DQRT17 (two side cases), AB_DQRT15 and AB_DQRT12
*
      LWORK = MAX( 1, ( M+N )*NRHS,
     $      ( N+NRHS )*( M+2 ), ( M+NRHS )*( N+2 ),
     $      MAX( M+MNMIN, NRHS*MNMIN,2*N+M ),
     $      MAX( M*N+4*MNMIN+MAX(M,N), M*N+2*MNMIN+4*N ) )
      LIWORK = 1
*
*     Iterate through all test cases and compute necessary workspace
*     sizes for ?GELS, ?GETSLS, ?GELSY, ?GELSS and ?GELSD routines.
*
      DO IM = 1, NM
         M = MVAL( IM )
         LDA = MAX( 1, M )
         DO IN = 1, NN
            N = NVAL( IN )
            MNMIN = MAX(MIN( M, N ),1)
            LDB = MAX( 1, M, N )
            DO INS = 1, NNS
               NRHS = NSVAL( INS )
               DO IRANK = 1, 2
                  DO ISCALE = 1, 3
                     ITYPE = ( IRANK-1 )*3 + ISCALE
                     IF( DOTYPE( ITYPE ) ) THEN
                        IF( IRANK.EQ.1 ) THEN
                           DO ITRAN = 1, 2
                              IF( ITRAN.EQ.1 ) THEN
                                 TRANS = 'N'
                              ELSE
                                 TRANS = 'T'
                              END IF
*
*                             Compute workspace needed for AB_DGELS
                              CALL AB_DGELS( TRANS, M, N, NRHS, A, LDA,
     $                                    B, LDB, WQ, -1, INFO )
                              LWORK_AB_DGELS = INT ( WQ )
*                             Compute workspace needed for AB_DGETSLS
                              CALL AB_DGETSLS( TRANS, M, N, NRHS, A, LDA
     $,
     $                                      B, LDB, WQ, -1, INFO )
                              LWORK_AB_DGETSLS = INT( WQ )
                           ENDDO
                        END IF
*                       Compute workspace needed for AB_AB_DGELSY
                        CALL AB_AB_DGELSY( M, N, NRHS, A, LDA, B, LDB, I
     $WQ,
     $                               RCOND, CRANK, WQ, -1, INFO )
                        LWORK_AB_AB_DGELSY = INT( WQ )
*                       Compute workspace needed for AB_AB_DGELSS
                        CALL AB_AB_DGELSS( M, N, NRHS, A, LDA, B, LDB, S
     $,
     $                               RCOND, CRANK, WQ, -1 , INFO )
                        LWORK_AB_AB_DGELSS = INT( WQ )
*                       Compute workspace needed for AB_AB_DGELSD
                        CALL AB_AB_DGELSD( M, N, NRHS, A, LDA, B, LDB, S
     $,
     $                               RCOND, CRANK, WQ, -1, IWQ, INFO )
                        LWORK_AB_AB_DGELSD = INT( WQ )
*                       Compute LIWORK workspace needed for AB_AB_DGELSY and AB_AB_DGELSD
                        LIWORK = MAX( LIWORK, N, IWQ )
*                       Compute LWORK workspace needed for all functions
                        LWORK = MAX( LWORK, LWORK_AB_DGELS, LWORK_AB_DGE
     $TSLS,
     $                               LWORK_AB_AB_DGELSY, LWORK_AB_AB_DGE
     $LSS,
     $                               LWORK_AB_AB_DGELSD )
                     END IF
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO
*
      LWLSY = LWORK
*
      ALLOCATE( WORK( LWORK ) )
      ALLOCATE( IWORK( LIWORK ) )
*
      DO 150 IM = 1, NM
         M = MVAL( IM )
         LDA = MAX( 1, M )
*
         DO 140 IN = 1, NN
            N = NVAL( IN )
            MNMIN = MAX(MIN( M, N ),1)
            LDB = MAX( 1, M, N )
            MB = (MNMIN+1)
*
            DO 130 INS = 1, NNS
               NRHS = NSVAL( INS )
*
               DO 120 IRANK = 1, 2
                  DO 110 ISCALE = 1, 3
                     ITYPE = ( IRANK-1 )*3 + ISCALE
                     IF( .NOT.DOTYPE( ITYPE ) )
     $                  GO TO 110
*
                     IF( IRANK.EQ.1 ) THEN
*
*                       Test AB_DGELS
*
*                       Generate a matrix of scaling type ISCALE
*
                        CALL AB_DQRT13( ISCALE, M, N, COPYA, LDA, NORMA,
     $                               ISEED )
                        DO 40 INB = 1, NNB
                           NB = NBVAL( INB )
                           CALL AB_XLAENV( 1, NB )
                           CALL AB_XLAENV( 3, NXVAL( INB ) )
*
                           DO 30 ITRAN = 1, 2
                              IF( ITRAN.EQ.1 ) THEN
                                 TRANS = 'N'
                                 NROWS = M
                                 NCOLS = N
                              ELSE
                                 TRANS = 'T'
                                 NROWS = N
                                 NCOLS = M
                              END IF
                              LDWORK = MAX( 1, NCOLS )
*
*                             Set up a consistent rhs
*
                              IF( NCOLS.GT.0 ) THEN
                                 CALL AB_DLARNV( 2, ISEED, NCOLS*NRHS,
     $                                        WORK )
                                 CALL AB_DSCAL( NCOLS*NRHS,
     $                                       ONE / DBLE( NCOLS ), WORK,
     $                                       1 )
                              END IF
                              CALL AB_DGEMM( TRANS, 'No transpose', NROW
     $S,
     $                                    NRHS, NCOLS, ONE, COPYA, LDA,
     $                                    WORK, LDWORK, ZERO, B, LDB )
                              CALL AB_DLACPY( 'Full', NROWS, NRHS, B, LD
     $B,
     $                                     COPYB, LDB )
*
*                             Solve LS or overdetermined system
*
                              IF( M.GT.0 .AND. N.GT.0 ) THEN
                                 CALL AB_DLACPY( 'Full', M, N, COPYA, LD
     $A,
     $                                        A, LDA )
                                 CALL AB_DLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, B, LDB )
                              END IF
                              SRNAMT = 'AB_DGELS '
                              CALL AB_DGELS( TRANS, M, N, NRHS, A, LDA, 
     $B,
     $                                    LDB, WORK, LWORK, INFO )
                              IF( INFO.NE.0 )
     $                           CALL AB_ALAERH( PATH, 'AB_DGELS ', INFO
     $, 0,
     $                                        TRANS, M, N, NRHS, -1, NB,
     $                                        ITYPE, NFAIL, NERRS,
     $                                        NOUT )
*
*                             Check correctness of results
*
                              LDWORK = MAX( 1, NROWS )
                              IF( NROWS.GT.0 .AND. NRHS.GT.0 )
     $                           CALL AB_DLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, C, LDB )
                              CALL AB_DQRT16( TRANS, M, N, NRHS, COPYA,
     $                                     LDA, B, LDB, C, LDB, WORK,
     $                                     RESULT( 1 ) )
*
                              IF( ( ITRAN.EQ.1 .AND. M.GE.N ) .OR.
     $                            ( ITRAN.EQ.2 .AND. M.LT.N ) ) THEN
*
*                                Solving LS system
*
                                 RESULT( 2 ) = AB_DQRT17( TRANS, 1, M, N
     $,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         COPYB, LDB, C, WORK,
     $                                         LWORK )
                              ELSE
*
*                                Solving overdetermined system
*
                                 RESULT( 2 ) = AB_DQRT14( TRANS, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         WORK, LWORK )
                              END IF
*
*                             Print information about the tests that
*                             did not pass the threshold.
*
                              DO 20 K = 1, 2
                                 IF( RESULT( K ).GE.THRESH ) THEN
                                    IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                                 CALL AB_ALAHD( NOUT, PATH )
                                    WRITE( NOUT, FMT = 9999 )TRANS, M,
     $                                 N, NRHS, NB, ITYPE, K,
     $                                 RESULT( K )
                                    NFAIL = NFAIL + 1
                                 END IF
   20                         CONTINUE
                              NRUN = NRUN + 2
   30                      CONTINUE
   40                   CONTINUE
*
*
*                       Test AB_DGETSLS
*
*                       Generate a matrix of scaling type ISCALE
*
                        CALL AB_DQRT13( ISCALE, M, N, COPYA, LDA, NORMA,
     $                               ISEED )
                        DO 65 INB = 1, NNB
                           MB = NBVAL( INB )
                           CALL AB_XLAENV( 1, MB )
                             DO 62 IMB = 1, NNB
                              NB = NBVAL( IMB )
                              CALL AB_XLAENV( 2, NB )
*
                           DO 60 ITRAN = 1, 2
                              IF( ITRAN.EQ.1 ) THEN
                                 TRANS = 'N'
                                 NROWS = M
                                 NCOLS = N
                              ELSE
                                 TRANS = 'T'
                                 NROWS = N
                                 NCOLS = M
                              END IF
                              LDWORK = MAX( 1, NCOLS )
*
*                             Set up a consistent rhs
*
                              IF( NCOLS.GT.0 ) THEN
                                 CALL AB_DLARNV( 2, ISEED, NCOLS*NRHS,
     $                                        WORK )
                                 CALL AB_DSCAL( NCOLS*NRHS,
     $                                       ONE / DBLE( NCOLS ), WORK,
     $                                       1 )
                              END IF
                              CALL AB_DGEMM( TRANS, 'No transpose', NROW
     $S,
     $                                    NRHS, NCOLS, ONE, COPYA, LDA,
     $                                    WORK, LDWORK, ZERO, B, LDB )
                              CALL AB_DLACPY( 'Full', NROWS, NRHS, B, LD
     $B,
     $                                     COPYB, LDB )
*
*                             Solve LS or overdetermined system
*
                              IF( M.GT.0 .AND. N.GT.0 ) THEN
                                 CALL AB_DLACPY( 'Full', M, N, COPYA, LD
     $A,
     $                                        A, LDA )
                                 CALL AB_DLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, B, LDB )
                              END IF
                              SRNAMT = 'AB_DGETSLS '
                              CALL AB_DGETSLS( TRANS, M, N, NRHS, A,
     $                                 LDA, B, LDB, WORK, LWORK, INFO )
                              IF( INFO.NE.0 )
     $                           CALL AB_ALAERH( PATH, 'AB_DGETSLS ', IN
     $FO, 0,
     $                                        TRANS, M, N, NRHS, -1, NB,
     $                                        ITYPE, NFAIL, NERRS,
     $                                        NOUT )
*
*                             Check correctness of results
*
                              LDWORK = MAX( 1, NROWS )
                              IF( NROWS.GT.0 .AND. NRHS.GT.0 )
     $                           CALL AB_DLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, C, LDB )
                              CALL AB_DQRT16( TRANS, M, N, NRHS, COPYA,
     $                                     LDA, B, LDB, C, LDB, WORK,
     $                                     RESULT( 15 ) )
*
                              IF( ( ITRAN.EQ.1 .AND. M.GE.N ) .OR.
     $                            ( ITRAN.EQ.2 .AND. M.LT.N ) ) THEN
*
*                                Solving LS system
*
                                 RESULT( 16 ) = AB_DQRT17( TRANS, 1, M, 
     $N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         COPYB, LDB, C, WORK,
     $                                         LWORK )
                              ELSE
*
*                                Solving overdetermined system
*
                                 RESULT( 16 ) = AB_DQRT14( TRANS, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         WORK, LWORK )
                              END IF
*
*                             Print information about the tests that
*                             did not pass the threshold.
*
                              DO 50 K = 15, 16
                                 IF( RESULT( K ).GE.THRESH ) THEN
                                    IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                                 CALL AB_ALAHD( NOUT, PATH )
                                    WRITE( NOUT, FMT = 9997 )TRANS, M,
     $                                 N, NRHS, MB, NB, ITYPE, K,
     $                                 RESULT( K )
                                    NFAIL = NFAIL + 1
                                 END IF
   50                         CONTINUE
                              NRUN = NRUN + 2
   60                      CONTINUE
   62                      CONTINUE
   65                   CONTINUE
                     END IF
*
*                    Generate a matrix of scaling type ISCALE and rank
*                    type IRANK.
*
                     CALL AB_DQRT15( ISCALE, IRANK, M, N, NRHS, COPYA, L
     $DA,
     $                            COPYB, LDB, COPYS, RANK, NORMA, NORMB,
     $                            ISEED, WORK, LWORK )
*
*                    workspace used: MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M)
*
                     LDWORK = MAX( 1, M )
*
*                    Loop for testing different block sizes.
*
                     DO 100 INB = 1, NNB
                        NB = NBVAL( INB )
                        CALL AB_XLAENV( 1, NB )
                        CALL AB_XLAENV( 3, NXVAL( INB ) )
*
*                       Test AB_AB_DGELSY
*
*                       AB_AB_DGELSY:  Compute the minimum-norm solution X
*                       to min( norm( A * X - B ) )
*                       using the rank-revealing orthogonal
*                       factorization.
*
*                       Initialize vector IWORK.
*
                        DO 70 J = 1, N
                           IWORK( J ) = 0
   70                   CONTINUE
*
                        CALL AB_DLACPY( 'Full', M, N, COPYA, LDA, A, LDA
     $ )
                        CALL AB_DLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
*
                        SRNAMT = 'AB_AB_DGELSY'
                        CALL AB_AB_DGELSY( M, N, NRHS, A, LDA, B, LDB, I
     $WORK,
     $                               RCOND, CRANK, WORK, LWLSY, INFO )
                        IF( INFO.NE.0 )
     $                     CALL AB_ALAERH( PATH, 'AB_AB_DGELSY', INFO, 0
     $, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       Test 3:  Compute relative error in svd
*                                workspace: M*N + 4*MIN(M,N) + MAX(M,N)
*
                        RESULT( 3 ) = AB_DQRT12( CRANK, CRANK, A, LDA,
     $                                COPYS, WORK, LWORK )
*
*                       Test 4:  Compute error in solution
*                                workspace:  M*NRHS + M
*
                        CALL AB_DLACPY( 'Full', M, NRHS, COPYB, LDB, WOR
     $K,
     $                               LDWORK )
                        CALL AB_DQRT16( 'No transpose', M, N, NRHS, COPY
     $A,
     $                               LDA, B, LDB, WORK, LDWORK,
     $                               WORK( M*NRHS+1 ), RESULT( 4 ) )
*
*                       Test 5:  Check norm of r'*A
*                                workspace: NRHS*(M+N)
*
                        RESULT( 5 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 5 ) = AB_DQRT17( 'No transpose', 1, M
     $,
     $                                   N, NRHS, COPYA, LDA, B, LDB,
     $                                   COPYB, LDB, C, WORK, LWORK )
*
*                       Test 6:  Check if x is in the rowspace of A
*                                workspace: (M+NRHS)*(N+2)
*
                        RESULT( 6 ) = ZERO
*
                        IF( N.GT.CRANK )
     $                     RESULT( 6 ) = AB_DQRT14( 'No transpose', M, N
     $,
     $                                   NRHS, COPYA, LDA, B, LDB,
     $                                   WORK, LWORK )
*
*                       Test AB_AB_DGELSS
*
*                       AB_AB_DGELSS:  Compute the minimum-norm solution X
*                       to min( norm( A * X - B ) )
*                       using the SVD.
*
                        CALL AB_DLACPY( 'Full', M, N, COPYA, LDA, A, LDA
     $ )
                        CALL AB_DLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
                        SRNAMT = 'AB_AB_DGELSS'
                        CALL AB_AB_DGELSS( M, N, NRHS, A, LDA, B, LDB, S
     $,
     $                               RCOND, CRANK, WORK, LWORK, INFO )
                        IF( INFO.NE.0 )
     $                     CALL AB_ALAERH( PATH, 'AB_AB_DGELSS', INFO, 0
     $, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       workspace used: 3*min(m,n) +
*                                       max(2*min(m,n),nrhs,max(m,n))
*
*                       Test 7:  Compute relative error in svd
*
                        IF( RANK.GT.0 ) THEN
                           CALL AB_DAXPY( MNMIN, -ONE, COPYS, 1, S, 1 )
                           RESULT( 7 ) = AB_DASUM( MNMIN, S, 1 ) /
     $                                   AB_DASUM( MNMIN, COPYS, 1 ) /
     $                                   ( EPS*DBLE( MNMIN ) )
                        ELSE
                           RESULT( 7 ) = ZERO
                        END IF
*
*                       Test 8:  Compute error in solution
*
                        CALL AB_DLACPY( 'Full', M, NRHS, COPYB, LDB, WOR
     $K,
     $                               LDWORK )
                        CALL AB_DQRT16( 'No transpose', M, N, NRHS, COPY
     $A,
     $                               LDA, B, LDB, WORK, LDWORK,
     $                               WORK( M*NRHS+1 ), RESULT( 8 ) )
*
*                       Test 9:  Check norm of r'*A
*
                        RESULT( 9 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 9 ) = AB_DQRT17( 'No transpose', 1, M
     $,
     $                                   N, NRHS, COPYA, LDA, B, LDB,
     $                                   COPYB, LDB, C, WORK, LWORK )
*
*                       Test 10:  Check if x is in the rowspace of A
*
                        RESULT( 10 ) = ZERO
                        IF( N.GT.CRANK )
     $                     RESULT( 10 ) = AB_DQRT14( 'No transpose', M, 
     $N,
     $                                    NRHS, COPYA, LDA, B, LDB,
     $                                    WORK, LWORK )
*
*                       Test AB_AB_DGELSD
*
*                       AB_AB_DGELSD:  Compute the minimum-norm solution X
*                       to min( norm( A * X - B ) ) using a
*                       divide and conquer SVD.
*
*                       Initialize vector IWORK.
*
                        DO 80 J = 1, N
                           IWORK( J ) = 0
   80                   CONTINUE
*
                        CALL AB_DLACPY( 'Full', M, N, COPYA, LDA, A, LDA
     $ )
                        CALL AB_DLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
*
                        SRNAMT = 'AB_AB_DGELSD'
                        CALL AB_AB_DGELSD( M, N, NRHS, A, LDA, B, LDB, S
     $,
     $                               RCOND, CRANK, WORK, LWORK, IWORK,
     $                               INFO )
                        IF( INFO.NE.0 )
     $                     CALL AB_ALAERH( PATH, 'AB_AB_DGELSD', INFO, 0
     $, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       Test 11:  Compute relative error in svd
*
                        IF( RANK.GT.0 ) THEN
                           CALL AB_DAXPY( MNMIN, -ONE, COPYS, 1, S, 1 )
                           RESULT( 11 ) = AB_DASUM( MNMIN, S, 1 ) /
     $                                    AB_DASUM( MNMIN, COPYS, 1 ) /
     $                                    ( EPS*DBLE( MNMIN ) )
                        ELSE
                           RESULT( 11 ) = ZERO
                        END IF
*
*                       Test 12:  Compute error in solution
*
                        CALL AB_DLACPY( 'Full', M, NRHS, COPYB, LDB, WOR
     $K,
     $                               LDWORK )
                        CALL AB_DQRT16( 'No transpose', M, N, NRHS, COPY
     $A,
     $                               LDA, B, LDB, WORK, LDWORK,
     $                               WORK( M*NRHS+1 ), RESULT( 12 ) )
*
*                       Test 13:  Check norm of r'*A
*
                        RESULT( 13 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 13 ) = AB_DQRT17( 'No transpose', 1, 
     $M,
     $                                    N, NRHS, COPYA, LDA, B, LDB,
     $                                    COPYB, LDB, C, WORK, LWORK )
*
*                       Test 14:  Check if x is in the rowspace of A
*
                        RESULT( 14 ) = ZERO
                        IF( N.GT.CRANK )
     $                     RESULT( 14 ) = AB_DQRT14( 'No transpose', M, 
     $N,
     $                                    NRHS, COPYA, LDA, B, LDB,
     $                                    WORK, LWORK )
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 90 K = 3, 14
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL AB_ALAHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9998 )M, N, NRHS, NB,
     $                           ITYPE, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   90                   CONTINUE
                        NRUN = NRUN + 12
*
  100                CONTINUE
  110             CONTINUE
  120          CONTINUE
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
*
*     Print a summary of the results.
*
      CALL AB_ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' TRANS=''', A1, ''', M=', I5, ', N=', I5, ', NRHS=', I4,
     $      ', NB=', I4, ', type', I2, ', test(', I2, ')=', G12.5 )
 9998 FORMAT( ' M=', I5, ', N=', I5, ', NRHS=', I4, ', NB=', I4,
     $      ', type', I2, ', test(', I2, ')=', G12.5 )
 9997 FORMAT( ' TRANS=''', A1,' M=', I5, ', N=', I5, ', NRHS=', I4,
     $      ', MB=', I4,', NB=', I4,', type', I2,
     $      ', test(', I2, ')=', G12.5 )
*
      DEALLOCATE( WORK )
      DEALLOCATE( IWORK )
      RETURN
*
*     End of AB_DDRVLS
*
      END
