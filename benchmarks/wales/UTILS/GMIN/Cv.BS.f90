!
!  Calculate Cv curve from weights and visit statistics
!  for replicas at different temperatures REPT(J1).
!  Minimise chi^2 statistic to extract best probabilities.
!
PROGRAM CVBS
IMPLICIT NONE
DOUBLE PRECISION TMIN, TMAX, TINT, Z0, Z1, Z2, TEMPERATURE, DUMMY, DELTA, ONEMEXP, PSUM, DP, PSUM2, TANAL, DPLUS, DMINUS
DOUBLE PRECISION COLOURTHRESH, HSHIFT, DUMMY2, DWEIGHT, DMIN
DOUBLE PRECISION, ALLOCATABLE :: ENERGY(:), LNWEIGHT(:), MEANE(:), ZSAVE(:), PERMINP(:), PERMINM(:), EMIN(:), LNWEIGHT2(:)
DOUBLE PRECISION, ALLOCATABLE :: MEANVQI(:), MEANQI(:), ANSHIFT(:), QENERGY(:)
INTEGER, ALLOCATABLE :: CLOSEST(:), QBIN(:), QTOI(:)
INTEGER, ALLOCATABLE :: MINP(:), MINM(:), IBININDEX(:)
INTEGER J1, NTEMP, J2, KAPPA, NBINS, NPLUS, NMINUS, NDUMMY, NMIN, MAXBIN, NQBINS, NCOUNT, NQBINSREAL, ND1
LOGICAL YESNO, DO2DT

OPEN(UNIT=10,FILE='Cv.BS.data',STATUS='OLD')
READ(10,*) TMIN, TMAX, NTEMP, KAPPA, NMIN, NBINS, MAXBIN
CLOSE(10)
ALLOCATE(MEANE(NTEMP),ZSAVE(NTEMP))

PRINT '(3(A,I10))','bins=',NBINS
PRINT '(A,2G15.5,A,I8)','minimum and maximum T for Cv calculation=',TMIN,TMAX,' number of Cv data points=',NTEMP
PRINT '(A,I8)','number of degrees of freedom=',KAPPA

ALLOCATE(ENERGY(NBINS),LNWEIGHT2(NBINS),LNWEIGHT(NBINS),EMIN(NMIN),IBININDEX(MAXBIN))

OPEN(UNIT=10,FILE='min.data',STATUS='OLD')
DO J1=1,NMIN
   READ(10,*) EMIN(J1)
ENDDO
CLOSE(10)

OPEN(UNIT=10,FILE='weights.BS',STATUS='OLD')
DO J1=1,NBINS
   READ(10,*) NDUMMY,DUMMY,ENERGY(J1),DUMMY,LNWEIGHT(J1)
   IBININDEX(NDUMMY)=J1
ENDDO
CLOSE(10)

TINT=(TMAX-TMIN)/(NTEMP-1)
DELTA=ENERGY(2)-ENERGY(1)
!
!  Calculate Z0, Z1 and Z2 over the required T range. Omit factors of (kT/h)^kappa,
!  which cancel.
!
OPEN(UNIT=1,FILE='Cv.out.BS',STATUS='UNKNOWN')
DO J1=1,NTEMP
   Z0=0.0D0
   Z1=0.0D0
   Z2=0.0D0
   TEMPERATURE=TMIN+(J1-1)*TINT
   DO J2=1,NBINS
      DUMMY=EXP(-(ENERGY(J2)-ENERGY(1))/TEMPERATURE+LNWEIGHT(J2))
      Z0=Z0+DUMMY
      Z1=Z1+DUMMY*(ENERGY(J2)-ENERGY(1))
      Z2=Z2+DUMMY*(ENERGY(J2)-ENERGY(1))**2
   ENDDO
   MEANE(J1)=Z1/Z0
   ZSAVE(J1)=Z0
   IF (DELTA/TEMPERATURE.LT.1.0D-7) THEN
      ONEMEXP=-DELTA/TEMPERATURE
   ELSE
      ONEMEXP= 1.0D0-EXP(DELTA/TEMPERATURE)
   ENDIF
   WRITE(1,'(7G20.10)') TEMPERATURE, Z0, Z1, Z2, &
  &                     KAPPA/2.0D0 + &
  &                      1.0D0*(1.0D0 - DELTA**2*EXP(DELTA/TEMPERATURE)/(ONEMEXP**2*TEMPERATURE**2)) &
  &                     - (Z1/(Z0*TEMPERATURE))**2 + Z2/(Z0*TEMPERATURE**2), MEANE(J1), ZSAVE(J1)
ENDDO
CLOSE(1)

OPEN(UNIT=1,FILE='prob.out.BS',STATUS='UNKNOWN')
DO J1=1,NTEMP
   TEMPERATURE=TMIN+(J1-1)*TINT
   PSUM=0.0D0
   PSUM2=0.0D0
   DO J2=1,NBINS
      DUMMY=EXP(-(ENERGY(J2)-ENERGY(1))/TEMPERATURE+LNWEIGHT(J2))
      DP=DUMMY*(ENERGY(J2)-ENERGY(1)-MEANE(J1))/(ZSAVE(J1)*TEMPERATURE**2)
      PSUM=PSUM+DP*(ENERGY(J2)-ENERGY(1)-MEANE(J1))
!     PRINT '(A,2I6,4G20.10)','J1,J2,T,ZSAVE,DUMMY,DP=',J1,J2,TEMPERATURE,ZSAVE(J1),DUMMY,DP
      PSUM2=PSUM2+ZSAVE(J1)*(TEMPERATURE*DP)**2/DUMMY
!     PSUM2=PSUM2+(ZSAVE(J1)/DUMMY)*(TEMPERATURE*DP)**2
   ENDDO
   IF (DELTA/TEMPERATURE.LT.1.0D-7) THEN
      ONEMEXP=-DELTA/TEMPERATURE
   ELSE
      ONEMEXP= 1.0D0-EXP(DELTA/TEMPERATURE)
   ENDIF
   WRITE(1,'(5G20.10)') TEMPERATURE, KAPPA/2.0D0+1.0D0 - DELTA**2*EXP(DELTA/TEMPERATURE)/(ONEMEXP**2*TEMPERATURE**2)+PSUM, &
  &                                  KAPPA/2.0D0+1.0D0 - DELTA**2*EXP(DELTA/TEMPERATURE)/(ONEMEXP**2*TEMPERATURE**2)+PSUM2
ENDDO
CLOSE(1)

INQUIRE(FILE='Tanal.BS',EXIST=YESNO)

IF (YESNO) THEN
   NPLUS=0.0D0
   NMINUS=0.0D0
   DPLUS=0.0D0
   DMINUS=0.0D0
   ALLOCATE(PERMINP(NBINS),PERMINM(NBINS),MINP(NBINS),MINM(NBINS))
   OPEN(UNIT=1,FILE='Tanal.BS')
   READ(1,*) TANAL
   COLOURTHRESH=0.9D0
   READ(1,*,END=666) COLOURTHRESH
666 CONTINUE
   PRINT '(2(A,G20.10))','Analysing heat capacity contributions at T=',TANAL,' colour threshold=',COLOURTHRESH
   CLOSE(1)
   TEMPERATURE=TANAL
   Z0=0.0D0
   Z1=0.0D0
   DO J2=1,NBINS
      DUMMY=EXP(-(ENERGY(J2)-ENERGY(1))/TEMPERATURE+LNWEIGHT(J2))
      Z0=Z0+DUMMY
      Z1=Z1+DUMMY*(ENERGY(J2)-ENERGY(1))
   ENDDO
   MEANE(1)=Z1/Z0
   ZSAVE(1)=Z0
   DO J2=1,NBINS
      DUMMY=EXP(-(ENERGY(J2)-ENERGY(1))/TEMPERATURE+LNWEIGHT(J2))
      DP=DUMMY*(ENERGY(J2)-ENERGY(1)-MEANE(1))/(ZSAVE(1)*TEMPERATURE**2)
      DUMMY=DP*(ENERGY(J2)-ENERGY(1)-MEANE(1))
      IF (DP.LT.0.0D0) THEN
         NMINUS=NMINUS+1
         MINM(NMINUS)=J2
         PERMINM(NMINUS)=DUMMY
         DMINUS=DMINUS+DUMMY
      ELSE
         NPLUS=NPLUS+1
         MINP(NPLUS)=J2
         PERMINP(NPLUS)=DUMMY
         DPLUS=DPLUS+DUMMY
      ENDIF
!     PRINT '(A,I6,3G20.10,2I6)','J2,ENERGY(J2)-ENERGY(1),MEANE,DP,NPLUS,NMINUS=',J2,ENERGY(J2)-ENERGY(1),MEANE(1),DP,NPLUS,NMINUS
   ENDDO
   DO J2=1,NMINUS
      PERMINM(J2)=PERMINM(J2)/DMINUS
   ENDDO
   DO J2=1,NPLUS
      PERMINP(J2)=PERMINP(J2)/DPLUS
   ENDDO
   CALL SORT(NMINUS,NBINS,PERMINM,MINM)
   CALL SORT(NPLUS,NBINS,PERMINP,MINP)
   DUMMY=0.0D0
   PRINT '(A)','i bins with negative dp/dT below threshold:'
   DO J2=1,NMINUS
      DUMMY=DUMMY+PERMINM(J2)
      PRINT '(2I6,4G20.10)',J2,MINM(J2),PERMINM(J2),DUMMY,ENERGY(MINM(J2)),ENERGY(MINM(J2))-ENERGY(1)
      IF (DUMMY.GT.COLOURTHRESH) EXIT
   ENDDO
   DUMMY=0.0D0
   PRINT '(A)','i bins with positive dp/dT:'
   DO J2=1,NPLUS
      DUMMY=DUMMY+PERMINP(J2)
      PRINT '(2I6,4G20.10)',J2,MINP(J2),PERMINP(J2),DUMMY,ENERGY(MINP(J2)),ENERGY(MINP(J2))-ENERGY(1)
      IF (DUMMY.GT.COLOURTHRESH) EXIT
   ENDDO
!
! Harmonic shift to map i bins to q bins. V_i = V_q + kappa*k*T/2
!
   HSHIFT=KAPPA*TANAL/2.0D0
   PRINT '(A,I8,A)','For disconnectionDPS use line TRMIN 2 ',NMIN,' min.minus.hshift.BS min.plus.hshift.BS and '
   PRINT '(A)','CHOOSECOLOURS in the dinfo file'
   OPEN(UNIT=1,FILE='min.minus.hshift.BS',STATUS='UNKNOWN')

   DUMMY=0.0D0
   DO J2=1,NMINUS
      DUMMY=DUMMY+PERMINM(J2)
      DO J1=1,NMIN
         IF (ABS(EMIN(J1)-ENERGY(MINM(J2))+HSHIFT).LT.DELTA/2.0D0) THEN
            WRITE(1,'(I6)') J1
!           WRITE(*,'(A,3I6,3G20.10)') 'J1, J2, MINM(J2), EMIN, ENERGY(MINM(J2))-HSHIFT, abs=',J1, J2, MINM(J2), EMIN(J1), &
! &                                     ENERGY(MINM(J2))-HSHIFT,  ABS(EMIN(J1)-ENERGY(MINM(J2))+HSHIFT)
         ENDIF
      ENDDO
      IF (DUMMY.GT.COLOURTHRESH) EXIT
   ENDDO
   CLOSE(1)
   OPEN(UNIT=1,FILE='min.plus.hshift.BS',STATUS='UNKNOWN')
   DMIN=HUGE(1.0D0)
   DO J1=1,NMIN
   ENDDO

   DUMMY=0.0D0
   DO J2=1,NPLUS
      DUMMY=DUMMY+PERMINP(J2)
      DO J1=1,NMIN
         IF (ABS(EMIN(J1)-ENERGY(MINP(J2))+HSHIFT).LT.DELTA/2.0D0) THEN
            WRITE(1,'(I6)') J1
!           WRITE(*,'(A,3I6,3G20.10)') 'J1, J2, MINP(J2), EMIN, ENERGY(MINP(J2))-HSHIFT, abs=',J1, J2, MINP(J2), EMIN(J1), &
! &                                     ENERGY(MINP(J2))-HSHIFT,  ABS(EMIN(J1)-ENERGY(MINP(J2))+HSHIFT)
         ENDIF
      ENDDO
      IF (DUMMY.GT.COLOURTHRESH) EXIT
   ENDDO
   CLOSE(1)

   INQUIRE(FILE='weights.2D.BS',EXIST=DO2DT)
   IF (DO2DT) THEN
      PRINT '(A)','Reading 2D weights from weights.2D.BS'
      OPEN(UNIT=10,FILE='Cv.BS.data',STATUS='OLD')
!
! This itme, look for NQBINS parameter at the end for allocations
!
      READ(10,*) TMIN, TMAX, NTEMP, KAPPA, NMIN, NBINS, MAXBIN, NQBINS
      CLOSE(10)
      ALLOCATE(MEANVQI(NQBINS),MEANQI(NQBINS),ANSHIFT(NQBINS),CLOSEST(NQBINS),QBIN(NMIN),QENERGY(NQBINS),QTOI(NQBINS))
      MEANVQI(1:NQBINS)=0.0D0
      MEANQI(1:NQBINS)=0.0D0
      OPEN(UNIT=19,FILE='weights.2D.BS',STATUS='OLD')
      NCOUNT=0
      ND1=HUGE(1)
      DO 
         READ(19,*,END=753) J1, J2, DUMMY2, DUMMY, DWEIGHT
         IF (J2.LT.ND1) THEN
            NCOUNT=NCOUNT+1
            QENERGY(NCOUNT)=DUMMY2
         ENDIF
         ND1=J2
         IF (DUMMY-DUMMY2.GT.0.0D0) THEN
            MEANVQI(NCOUNT)=MEANVQI(NCOUNT)+EXP(DWEIGHT-(DUMMY-ENERGY(1))/TEMPERATURE)*(DUMMY-DUMMY2)
            MEANQI(NCOUNT)=MEANQI(NCOUNT)  +EXP(DWEIGHT-(DUMMY-ENERGY(1))/TEMPERATURE)
         ENDIF
      ENDDO
753   CONTINUE

      NQBINSREAL=NCOUNT
      PRINT '(A,I8,A,I8)','Mean thermal PE evaluated for ',NQBINSREAL,' quanch bins, maximum bins was ',NQBINS
      CLOSE(19)
      PRINT '(A)','    q bin,    energy,    weight,    weighted thermal energy,    mean quench pe,       shift,  shift/harmonic'
      DO J1=1,NQBINSREAL
         ANSHIFT(J1)=MEANVQI(J1)/MAX(MEANQI(J1),1.0D-300)
         PRINT '(I8,5G20.10)',J1,QENERGY(J1),MEANQI(J1),ANSHIFT(J1),ANSHIFT(J1)/HSHIFT
      ENDDO
   ENDIF

   IF (DO2DT) THEN
      PRINT '(A,I8,A)','For disconnectionDPS use line TRMIN 2 ',NMIN,' min.minus.anhshift.BS min.plus.anhshift.BS and '
      PRINT '(A)','CHOOSECOLOURS in the dinfo file'
!
! which q bin is this minimum in? 
! which i bin does this q bin correspond to in terms of mean thermal pe?
! is this one of the dominant probability flux i bins?
!
      PRINT '(A)','Assignment of minima to quench bins: minimum, pe, q bin, q energy, diff'
      DO J1=1,NMIN
         DMIN=HUGE(1.0D0)
         DO J2=1,NQBINSREAL
            IF (ABS(EMIN(J1)-QENERGY(J2)).LT.DMIN) THEN
               QBIN(J1)=J2
               DMIN=ABS(EMIN(J1)-QENERGY(J2))
            ENDIF
         ENDDO
         PRINT '(I8,G20.10,I8,2G20.10)',J1,EMIN(J1),QBIN(J1),QENERGY(QBIN(J1)),DMIN
      ENDDO
      PRINT '(A)','Assignment of i bins to q bins for calculated anharmonic thermal pe: q bin, q energy, i bin, i energy, diff'
      DO J2=1,NQBINSREAL
         DMIN=HUGE(1.0D0)
         DO J1=1,NBINS
            IF (ABS(ENERGY(J1)-QENERGY(J2)-ANSHIFT(J2)).LT.DMIN) THEN
               QTOI(J2)=J1
               DMIN=ABS(ENERGY(J1)-QENERGY(J2)-ANSHIFT(J2))
            ENDIF
         ENDDO
         PRINT '(I8,G20.10,I8,2G20.10)',J2,QENERGY(J2),QTOI(J2),ENERGY(QTOI(J2)),DMIN
      ENDDO

      OPEN(UNIT=1,FILE='min.minus.anhshift.BS',STATUS='UNKNOWN')
      DUMMY=0.0D0
      DO J2=1,NMINUS
         DUMMY=DUMMY+PERMINM(J2)
         DO J1=1,NMIN
            IF (QTOI(QBIN(J1)).EQ.MINM(J2)) THEN
               WRITE(1,'(I6)') J1
               WRITE(*,'(A,3I8,3G20.10)') 'J1, J2, MINM(J2), EMIN, diff=', &
  &                                   J1, J2, MINM(J2), EMIN(J1), &
  &                                   ABS(EMIN(J1)-ENERGY(MINM(J2))+ANSHIFT(QBIN(J1)))
            ENDIF
         ENDDO
         IF (DUMMY.GT.COLOURTHRESH) EXIT
      ENDDO
      CLOSE(1)

      OPEN(UNIT=1,FILE='min.plus.anhshift.BS',STATUS='UNKNOWN')
      DUMMY=0.0D0
      DO J2=1,NPLUS
         DUMMY=DUMMY+PERMINP(J2)
         DO J1=1,NMIN
            IF (QTOI(QBIN(J1)).EQ.MINP(J2)) THEN
               WRITE(1,'(I6)') J1
               WRITE(*,'(A,3I6,3G20.10)') 'J1, J2, MINP(J2), EMIN, diff=', &
  &                                   J1, J2, MINP(J2), EMIN(J1), &
  &                                   ABS(EMIN(J1)-ENERGY(MINP(J2))+ANSHIFT(QBIN(J1)))
            ENDIF
         ENDDO
         IF (DUMMY.GT.COLOURTHRESH) EXIT
      ENDDO 
      CLOSE(1)

   ENDIF
ENDIF

END PROGRAM CVBS

SUBROUTINE SORT(N,J3,A,NA)
IMPLICIT NONE
INTEGER J1, L, N, J3, J2, NA(J3), NTEMP
DOUBLE PRECISION TEMP, A(J3)
!
DO 20 J1=1,N-1
   L=J1
   DO 10 J2=J1+1,N
      IF (A(L).LT.A(J2)) L=J2
10 CONTINUE
   TEMP=A(L)
   A(L)=A(J1)
   A(J1)=TEMP
   NTEMP=NA(L)
   NA(L)=NA(J1)
   NA(J1)=NTEMP
20 CONTINUE
RETURN
END SUBROUTINE SORT

