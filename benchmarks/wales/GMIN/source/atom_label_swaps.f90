SUBROUTINE USWAPS(NP, ITER, TIME, BRUN, QDONE, SCREENC)
  !
  USE COMMONS, ONLY : NATOMS, NSPECIES, ATOMLISTS, INVATOMLISTS, &
       MYUNIT, COORDS, NQ, NPAR, RMS, ECONV, PRTFRQ, HIT, &
       LSWAPS_N2, LSWAPS_TEMP, LSWAPS_TFACTOR, LSWAPS_NUP, &
       FIXLABELS, NTYPEA
  !
  IMPLICIT NONE
  !
  ! Parse passed variables
  INTEGER, INTENT(IN) :: NP
  INTEGER, INTENT(INOUT):: ITER,BRUN,QDONE ! for QUENCH
  DOUBLE PRECISION, INTENT(INOUT) :: TIME,SCREENC(3*NATOMS) ! QUENCH
  !
  LOGICAL :: DONE
  INTEGER :: NQTOT, ATOMLISTS_MIN(NSPECIES(0),3,0:NATOMS),&
       INVATOMLISTS_MIN(NATOMS,3),I1,I2,LIST(0:NATOMS),J,K,N,&
       NACC,NREJ,NREJ_STREAK
  DOUBLE PRECISION :: DPRAND, R, POTEL, X0(1:3*NATOMS), E0, &
       XMIN(1:3*NATOMS), EMIN, TEMP
  !
  ! Energy of COORDS from last quench. Common block in QUENCH. 
  COMMON /MYPOT/ POTEL
  ! Total quench count. Commom block in MC.
  COMMON /TOT/ NQTOT
  !
  ATOMLISTS_MIN(:,:,:) = ATOMLISTS(:,:,:)
  INVATOMLISTS_MIN(:,:) = INVATOMLISTS(:,:)
  XMIN(1:3*NATOMS) = COORDS(1:3*NATOMS, NP)
  EMIN = POTEL
  TEMP = LSWAPS_TEMP
  !
  WRITE(MYUNIT,'(A,F20.10,A,F10.8)') &
       'uswaps> Initial E= ', EMIN,' T= ', TEMP
  !
  NACC = 0
  NREJ = 0
  NREJ_STREAK = 0
  N=0
  IF(LSWAPS_N2 /= 0) THEN
     DONE=.FALSE.
  ELSE
     DONE=.TRUE.
  ENDIF
  !
  DO WHILE (.NOT.DONE) !N=1,LSWAPS_N2
     !
     N=N+1
     X0(1:3*NATOMS) = COORDS(1:3*NATOMS,NP)
     E0 = POTEL
     !
     IF(.NOT.FIXLABELS) THEN
        LIST(0:NATOMS) = 0
        DO J=1,NSPECIES(0)
           DO K=1,ATOMLISTS(J,1,0) ! only group 1
              LIST(0) = LIST(0) + 1
              LIST(LIST(0)) = ATOMLISTS(J,1,K) 
           ENDDO
        ENDDO
        I1 = INT(DPRAND()*DBLE(LIST(0))) + 1 ! Forgot +1
        I1 = LIST(I1)
        I2 = INVATOMLISTS(I1,1)
        LIST(:) = 0
        DO J=1,NSPECIES(0)
           IF(J==I2) CYCLE
           DO K=1,ATOMLISTS(J,1,0) ! only group 1
              LIST(0) = LIST(0) + 1
              LIST(LIST(0)) = ATOMLISTS(J,1,K) 
           ENDDO
        ENDDO
        I2 = INT(DPRAND()*DBLE(LIST(0))) + 1 ! Forgot +1
        I2 = LIST(I2)
        CALL SWAP_LABELS(I1,I2,NP)
     ELSE
        I1=INT(DPRAND()*DBLE(NTYPEA)) + 1 
        I2=INT(DPRAND()*DBLE(NATOMS-NTYPEA)) + NTYPEA + 1
        CALL SWAP_COORDS_V2(COORDS(1:3*NATOMS, NP),I1,I2)
        !WRITE(MYUNIT, '(A,I5,I5)') &
        !     'atom_label_swaps> Swapped atoms', I1, I2
     ENDIF
     !
     NQTOT = NQTOT + 1
     NQ(NP) = NQ(NP) + 1
     CALL QUENCH(.FALSE.,NP,ITER,TIME,BRUN,QDONE,SCREENC)
     IF (MOD(N-1,PRTFRQ).EQ.0) THEN
        IF (NPAR.GT.1) THEN
           WRITE(MYUNIT,'(A,I1,A,I10,A,F20.10,A,I5,A,G12.5,A,F11.1)') &
                '[',NP,']Qu ',NQ(NP),' E=',POTEL,' steps=',ITER, &
                ' RMS=',RMS,' t=',TIME
        ELSE
           WRITE(MYUNIT,'(A,I10,A,F20.10,A,I5,A,G12.5,A,F11.1)') &
                'Qu ',NQ(NP),' E=',POTEL,' steps=',ITER, &
                ' RMS=',RMS,' t=',TIME
        ENDIF
     ENDIF
     !
     R=EXP(MIN(2.0D0,(E0-POTEL)/TEMP))
     IF(DPRAND() < R) THEN ! Accept
        NACC = NACC + 1
        NREJ_STREAK=0
        IF(POTEL < EMIN - ECONV) THEN ! Store labels and coordinates
           EMIN=POTEL
           XMIN(1:3*NATOMS)=COORDS(1:3*NATOMS, NP)
           ATOMLISTS_MIN(:,:,:) = ATOMLISTS(:,:,:)
           INVATOMLISTS_MIN(:,:) = INVATOMLISTS(:,:)
           WRITE(MYUNIT,'(A,F20.10,A,I8,A,F10.8)') &
                'uswaps> New EMIN= ',EMIN,' on swap', N,&
                ' at T=', TEMP
           IF(HIT) RETURN
        ENDIF
     ELSE ! Reject. Revert labels and coordinates.
        NREJ = NREJ + 1
        IF(.NOT.FIXLABELS) THEN
           CALL SWAP_LABELS(I1,I2,NP)
        ENDIF
        COORDS(1:3*NATOMS,NP) = X0(1:3*NATOMS)
        POTEL = E0
     ENDIF
     !
     IF(MOD(N,LSWAPS_NUP)==0) THEN
        IF(LSWAPS_TFACTOR <= 1.0D0) THEN 
           ! Simulated annealing: always decrease temperature. 
           TEMP = TEMP*LSWAPS_TFACTOR
        ELSE ! Strive for ACC/REJ ratio of 0.5.
           ! This will have to be recoded for desired ACC/REJ 
           ! ratios other than 0.5!
           IF(NACC < NREJ) THEN
              TEMP = TEMP*LSWAPS_TFACTOR
           ELSEIF(NACC > NREJ) THEN
              TEMP = TEMP/LSWAPS_TFACTOR
           ENDIF
           WRITE(MYUNIT,'(A,F7.5,A,F7.5)') &
                'uswaps> Accepted ratio ', &
                DBLE(NACC)/DBLE(NACC+NREJ),' new T= ', TEMP
           NACC = 0; NREJ = 0
        ENDIF
     ENDIF
     !
     IF((N==LSWAPS_N2).OR.(NREJ_STREAK==-LSWAPS_N2)) THEN
        DONE=.TRUE.
     ENDIF
     !
  ENDDO
  !
  ATOMLISTS(:,:,:) = ATOMLISTS_MIN(:,:,:)
  INVATOMLISTS(:,:) = INVATOMLISTS_MIN(:,:)
  COORDS(1:3*NATOMS, NP) = XMIN(1:3*NATOMS)
  POTEL = EMIN
  !
  WRITE(MYUNIT,'(A,G20.10,A,F8.5)') &
       'uswaps> Final E= ', EMIN,' T= ', TEMP
  !
  RETURN
  !
END SUBROUTINE USWAPS

SUBROUTINE SWAP_COORDS_V2(X,I,J)
  !
  USE COMMONS, ONLY : NATOMS
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: I,J
  DOUBLE PRECISION, INTENT(INOUT) :: X(1:3*NATOMS)
  INTEGER :: K,L,M
  DOUBLE PRECISION :: DUMMY
  !
  K = 3*(I-1)
  L = 3*(J-1)
  DO M = 1,3
     DUMMY = X(K+M)
     X(K+M) = X(L+M)
     X(L+M) = DUMMY
  ENDDO
  !
END SUBROUTINE SWAP_COORDS_V2
