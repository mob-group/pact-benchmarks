MODULE COMMONS

      INTEGER NMINA, NMINB, NMIN, NTS, SAVELENGTH, NPFOLD, PFOLDINT, NRWBINS, NUSEPAIRS, NCONNECTPAIRS, &
     &        NATOMS, MAXLENGTH, CONNECTIONS, HORDER, ERROR, MINSEP, NRWREACTANT, &
     &        NATTEMPT, NNEW, NTOTAL, NEXCLUDE, NPERMGROUP, BHSTEPS, NGTSIZE,  &
     &        MAXCONN, KAPPA, ISEED, NTAG, NDIHE, NCPU, GTINT, NCONNMAX, BESTPATHLENGTH, NGLY, &
     &        STARTMINA, STARTMINB, WHICHMIN, SECONDMIN, WHICHTS, MAXATTEMPT, COSTFUNCTIONPOWER, &
     &        NPAIRDONE, NMINDONE, NPAIRFRQ, CHECKSPS, CHECKSPF, &
     &        BISECTSTEPS, BISECTMAXATTEMPTS, NDIAGEIG, QRELONE, QRELTWO, MAXTSATTEMPTS, &
     &        INTCONSEP, INTREPSEP, PAIRDISTMAX, PAIRDIST1, PAIRDIST2, PATOM1, PATOM2, NRANROT, &
     &        NMINADDXYZ, LOCALPERMNEIGH, NRATESCYCLETEMPS, RATESUNIT, CONNMINSTART, DISTANCETO, &
     &        DISTANCETO1, DISTANCETO2, OSTART, OFINISH, PTSTART, PTFINISH, RFMULTIN, RFUNIT, &
     &        RFKMCN, RFKMCSTEPS, NPEQ, MLPIN, MLPSTART, MLPOUT, MLPHIDDEN, NMLP, NOPT, CVSTARTMIN, CVENDMIN, CVINCMIN, &
     &        CONNECTMIN2F, CONNECTMIN2SAVE, NZEROS, MLPNEIGH

      INTEGER, ALLOCATABLE :: LOCATIONA(:), LOCATIONB(:)
      INTEGER, ALLOCATABLE :: NCONN(:)     ! reallocate MAXMIN when used
      INTEGER, ALLOCATABLE :: BESTPATH(:)  
      INTEGER, ALLOCATABLE :: USEPAIRSMIN(:)
      INTEGER, ALLOCATABLE :: CONNECTPAIRSMIN(:,:)
!
! I/O unit numbers
!
      INTEGER, PARAMETER :: UMINDATA=11, UTSDATA=12, UMIN=13, UTS=15

      INTEGER :: MAXMIN=26
      INTEGER :: DMINMAX=10
      INTEGER :: MAXTS=10
      INTEGER :: MAXPAIRS=10
      INTEGER :: MAXDONE=10
      DOUBLE PRECISION, ALLOCATABLE :: FRQS(:)
      DOUBLE PRECISION, POINTER :: MASS(:)
!
! MAXMIN
!
      DOUBLE PRECISION, ALLOCATABLE :: EMIN(:), FVIBMIN(:), PFMIN(:), IXMIN(:),  IYMIN(:), IZMIN(:), &
     &                                 GPFOLD(:), MINDISTMIN(:), MINCURVE(:), MINFRQ2(:)
!
! Changed PAIRDIST to linear scaling rather than quadratic with MAXMIN. DJW 14/4/08
!
      DOUBLE PRECISION, ALLOCATABLE :: PAIRDIST(:,:) ! dimension MAXMIN*PAIRDISTMAX for DIJINITT only runs
      INTEGER, ALLOCATABLE :: PAIRLIST(:,:) ! dimension MAXMIN*PAIRDISTMAX for DIJINITT only runs
      DOUBLE PRECISION, ALLOCATABLE :: ALLPAIRS(:) ! dimension (MAXMIN+2)(MAXMIN-1)/2 for INITIALDISTANCE runs. Scales quadratically!
!
! MAXTS
!
      DOUBLE PRECISION, ALLOCATABLE :: ETS(:), FVIBTS(:), KPLUS(:), KMINUS(:), IXTS(:),  IYTS(:), IZTS(:), NEGEIG(:)
! NATOMS
      DOUBLE PRECISION, ALLOCATABLE :: TAGFAC(:)
! NRWBINS
      DOUBLE PRECISION, ALLOCATABLE :: RWPROB(:)

      DOUBLE PRECISION EDIFFTOL, IDIFFTOL, GEOMDIFFTOL, PFMEAN, TOTALE, TEMPERATURE, PFTOTALA, PFTOTALB, PERTMAX, PERTMIN, &
     &                 PERTVALUE, TAGMASS, PABCONV, REGROUPTHRESH, REGROUPRATETHRESH, CONNECTDIST, &
     &                 ORDERPARAM, BOXLX, BOXLY, BOXLZ, DSCALE, PSCALE, TSTHRESH, MAXBARRIER, MAXDOWNBARRIER, REGROUPPETHRESH, &
     &                 PAIRTHRESH, MAXBREAK, PRODTHRESH, PBTHRESH, OMEGA, EINC, EDELTAMIN, ELOWBAR, RWBINWIDTH, RWEMAX, RWEMIN, &
     &                 GT2RSwitch, GT2Ptol, EUNTRAPTHRESH, PLANCK, REGROUPFREETHRESH, FREETHRESH, EHIGHBAR, BAILDIST, &
     &                 BHACCREJ, BHSTEPSIZE, BHCONV, BHTEMP, BHDISTTHRESH, BHK, BHMAXENERGY, BHSFRAC, &
     &                 BISECTMINDIST, BISECTMAXENERGY, NKMCCYCLES, NGTSWITCH, NTFOLD, TOMEGA, TFOLDTHRESH, DIAGSCALE, &
     &                 CVTMIN, CVTMAX, CVTINC, DOSEMIN, DOSEMAX, DOSEINC, EVCUT, GAMMAFRICTION, &
     &                 INTEPSILON, INTCONSTRAINTDEL, INTCONSTRAINTREP, INTCONSTRAINREPCUT, INTLJDEL, INTLJEPS, NGTCRSWITCH, &
     &                 PFOLDCONV, INTFREEZETOL, LOCALPERMCUT, ORBITTOL, PFORCE, LPDGEOMDIFFTOL, LOCALPERMCUT2,&
     &                 SLEEPTIME1, SLEEPTIME2, RATETARGETAB, RATETARGETBA, RATETARGETFRAC, RFMULTITLOW, RFMULTITINC, TIMESCALE, &
     &                 PFSHIFT, MICROEMIN, MICROEMAX, MICROEINC, MICROT, RFKMCTRATE, RFKMCTINC, RFKMCTSTART, JPARAM, PEQTHRESH, &
     &                 PERTHRESH, SHANNONTMIN, SHANNONTMAX, SHANNONTINC, RATEAB, RATEBA, MINBARRIER, DISBOUND


! AMH
      DOUBLE PRECISION QCONTCUT, RELCOCUT

      DOUBLE PRECISION, PARAMETER :: PI=3.141592654D0
      DOUBLE PRECISION TTSSEARCH, TPFOLD, TTFOLD, TGT, TDIJKSTRA, TCONNECTDIST, TKSHORTESTPATHS ! timers

! MAXMIN
      INTEGER, ALLOCATABLE :: HORDERMIN(:), TOPPOINTER(:), MINGROUP(:), MINCONN(:)
! MAXTS
      INTEGER, ALLOCATABLE :: HORDERTS(:), PLUS(:), MINUS(:), POINTERM(:), POINTERP(:), TSATTEMPT(:)
      INTEGER, ALLOCATABLE :: DMIN1(:), DMIN2(:) ! dimension MINA*MINB for DIJPAIRT runs
! MAXPAIRS
      INTEGER, ALLOCATABLE :: PAIR1(:), PAIR2(:)
! MAXDONE
      INTEGER, ALLOCATABLE :: MINDONE(:)
! NATOMS
      INTEGER, ALLOCATABLE :: NPERMSIZE(:), PERMGROUP(:)
      INTEGER, ALLOCATABLE :: NSETS(:)
      INTEGER, ALLOCATABLE :: SETS(:,:)
      INTEGER, ALLOCATABLE :: TAGNUM(:)
      LOGICAL, ALLOCATABLE, DIMENSION(:) :: FROZEN, INTFROZEN

      INTEGER NFSTART, NFFINISH, NINTS, NCONNMIN, CONNECTMIN1, CONNECTMIN2, NFREEZE, NPATHS, PARALLEL, NRANDOMMETRIC, &
     &        METRICUPAIR, METMATMAX, NPRUNE  

      LOGICAL YESNO, TEST1, TEST2, DEBUG, PRINTT, ADDPT, ADDPT2, ADDPT3, TWOD, BULKT, ANGLEAXIS, TAGT, &
     &        CHARMMT, AMBERT, OPEPT, STARTFROMPATH, RELATIVEET, EXTRACTTSFILET, &
     &        KMCT, UNRST, KMCCOMMITT, REGROUPT, REGROUPRATET, REGROUPPET, NOPOINTS, ADDPATH, NGTT, GTT, GT2T, &
     &        DIJKSTRAT, DIJPAIRT, DIJINITT, EXTRACTMINT, EXTRACTMINFILET, EXTRACTTST, DIJKSTRAWAITT, UNTRAPMETRICT, &
     &        EXPCOSTFUNCTION, COPYOPTIMT, CALCORDERT, CONNECTREGIONT, SHORTCUTT, MERGEDBT, UNTRAPT, AMHT,  AMHALLATOMMINT, &
     &        CHECKCONNECTIONST, AMHALLATOMTST, AMHQT,AMHQENGMINT, AMHQCONTT ,AMHRMSDT, AMHRELQT, AMH_RELCOT, DIAGT, ARNOLDIT, &
     &        GT2Sparse, GT2Switch, GT2AltPbb, GT2Rescale, GT2Normalise, GT2DisconnectSources, BARRIERSORT, &
     &        PERMDIST, PERMISOMER, RIGIDBODY, DIJINITSTARTT, DIJINITCONTT, RETAINSP, REMOVESP, NOFRQS, &
     &        BARRIERSHORT, FREEZE, RATESHORT, DUMMYRUNT, REWEIGHTT, REGROUPFREET, RFMULTIT, REGROUPFREEABT, READMINT, &
     &        DUMPGROUPST, FREEPAIRT, KSHORTESTPATHST, KSHORT_FULL_PRINTT, DIJINITFLYT, BHINTERPT, ICINTERPT, &
     &        DUMMYTST, DOCKT, DSTAGE(6), USEPAIRST, LOWESTFRQT, BISECTT, NGTDISCONNECTALL, ANGLEAXIS2, MULTISITEPYT, TFOLDT, &
     &        SLURMT, INDEXCOSTFUNCTION, CVT, CVMINIMAT, DOST, IMFRQT, CLOSEFILEST, PULLT, FRICTIONT, ATOMMATCHFULL, &
     &        INTCONSTRAINTT, CHECKCONINT, INTLJT, INTERPCOSTFUNCTION, REMOVEUNCONNECTEDT, ATOMMATCHDIST, &
     &        DBPT, DBPTDT, DMBLPYT, EFIELDT, MSSTOCKT, NTIPT, PAHAT, PAPT, PATCHYDT, STOCKAAT, RBAAT, RBSYMT, TRAPT, SILANET, &
     &        OHCELLT, INTFREEZET, LPERMDIST, PBST, RANDOMMETRICT, SSHT, ALLTST, USERPOTT, CHECKMINT, &
     &        CHECKTST, CHECKSPT, FROMLOWESTT, ADDMINXYZT, MACHINE, RATESCYCLET, NOINVERSION, NEWCONNECTIONST, NIMET, NIHEAM7T, &
     &        NIH2LEPST, DISTANCET, RATETARGETT, TARGETHIT, ALLOWABT, MICROTHERMT, RFKMCT, REGROUPKMCT, ONEREGROUPT, PHI4MODT, &
     &        PERSISTT, REGROUPPERSISTT, NOLABELST, SHANNONT, MAKEPAIRS, SKIPPAIRST, PERSISTAPPROXT, ALLCOMPONENTST, &
     &        SHANNONRT, SHANNONZT, CUDAT, MLLJAT3, MLP3T, DIJPRUNET, PRINTSUMMARYT, MKTRAPT, MLPB3T, PRUNECYCLET, PAIRSIGNORET, &
     &        NOTRANSROTT, NOPOINTGROUPT, MACROIONT, CONNECTPAIRST, INITIALDIST, MLPVB3NNT

      LOGICAL, ALLOCATABLE :: SHIFTABLE(:)
      CHARACTER(LEN=80) COORDSLIGANDSTR, COORDSCOMPLEXSTR, COORDSPROTEINSTR
      CHARACTER(LEN=80) EXEC,EXECGMIN
      CHARACTER(LEN=80) PATHNAME, MINNAME, ADDMINXYZNAME, ALLCOMPS
      CHARACTER(LEN=150) COPYFILES
      CHARACTER(LEN=80) USEPAIRSFILE
      CHARACTER(LEN=80) CONNECTPAIRSFILE
      CHARACTER(LEN=80) MAKEPAIRSFILE
      CHARACTER(LEN=2) DIRECTION
      CHARACTER(LEN=5) UNCONNECTEDS
      CHARACTER(LEN=5) ZSYM
      CHARACTER(LEN=2), ALLOCATABLE ::  ZSYMBOL(:)
      CHARACTER(LEN=1) ENSEMBLE

      CHARACTER(LEN=4), ALLOCATABLE :: RESLABEL(:), ATOMLABEL(:)
      INTEGER, ALLOCATABLE :: RESNUMBER(:)
      INTEGER, ALLOCATABLE :: NCONNGROUP(:) ! jmc

!     DC430 >
      INTEGER :: NRBSITES, NTSITES, NRBGROUP, PAHID, PAPID, TIPID
      DOUBLE PRECISION :: PAPALP
      DOUBLE PRECISION, ALLOCATABLE :: RBSITE(:,:), SITEMASS(:)
      DOUBLE PRECISION, ALLOCATABLE :: RBOPS(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: RATESCYCLETEMPS(:)

!
! Constraint potential stuff.
!
      INTEGER, ALLOCATABLE :: CONI(:), CONJ(:), CONION(:), CONJON(:)
      INTEGER, ALLOCATABLE :: REPI(:), REPJ(:)
      DOUBLE PRECISION, ALLOCATABLE :: REPCUT(:),  NREPCUT(:), CONDISTREF(:), CONDISTREFLOCAL(:)
      DOUBLE PRECISION INTCONSTRAINTTOL, REPCON
      INTEGER, ALLOCATABLE :: NREPI(:), NREPJ(:)
      INTEGER :: NNREPULSIVE, NCONSTRAINT, NREPULSIVE, MAXCONUSE
      INTEGER :: NREPMAX=10
      INTEGER :: INTIMAGE=0
      INTEGER :: INTCONMAX=10
      INTEGER :: NTRYING=0
      LOGICAL, ALLOCATABLE :: CONACTIVE(:)
      LOGICAL, ALLOCATABLE :: ATOMACTIVE(:)
!
! SIS epidemiological model stuff
!
      INTEGER :: SMAX, IMAX, POPSS
      DOUBLE PRECISION :: SISMU, SISKAPPA, SISBETA
      LOGICAL :: SIST
!
! Macrocycle stuff
! Add permutitional isomers for cyclic systems (e.g. cyclic peptides)
!
      LOGICAL :: MACROCYCLET = .FALSE.
      INTEGER :: MCYCLEREPEATS=1 !Number of repeat units in macrocycle
      INTEGER :: MCYCLEPERIOD !Length of repeat units in macrocycle (should be NATOMS/MCYCLEPERIOD)


! hk286
      LOGICAL :: GTHOMSONT
      INTEGER :: GTHOMMET
      DOUBLE PRECISION :: GTHOMSONZ

! RIGIDINIT - hk286
      LOGICAL :: RIGIDINIT
      INTEGER :: DEGFREEDOMS

! Get frqs
      LOGICAL :: GETMINFRQST, GETTSFRQST

! MINGAP
      LOGICAL :: MINGAPT, MINGAPRATIOT
      DOUBLE PRECISION :: MINGAPINP

!CONNECTUNCONNECTED
      LOGICAL :: CONNECTLOWESTT, CONNECTETHRESHT, CONNECTDTHRESHT, CONUNCONT
      INTEGER :: REFMIN, NATT

END MODULE COMMONS
