!   OPTIM: A program for optimizing geometries and calculating reaction pathways
!   Copyright (C) 1999-2006 David J. Wales
!   This file is part of OPTIM.
!   OPTIM is free software; you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation; either version 2 of the License, or
!   (at your option) any later version.
!
!   OPTIM is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program; if not, write to the Free Software
!   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!  Energy and gradient for a genrigid setup example
!
!  NATOMS = total number of sites
!  NRIGIDBODY = # rigid bodies
!
SUBROUTINE DJWGR1(NATOMS,X,V,ENERGY,GTEST,SECT)
USE MODHESS
USE GENRIGID
USE KEY, ONLY : NHEXAMERS, CAPSIDRHO, CAPSIDEPS, SIGMAPENT, RADPENT, RADHEX, SIGMAHEX, SIGMAPH
IMPLICIT NONE
LOGICAL GTEST,SECT
INTEGER NATOMS, J1, J2, J3, J4, NPOS1, NPOS2
DOUBLE PRECISION X(3*NATOMS), V(3*NATOMS), ENERGY, DUMMY2, DUMMY3, DUMMY, DIST, SIGMA, XDUMM, RHO, RDIST, RAD, EPSEFF
! DOUBLE PRECISION RADHEX, SIGMAHEX, SIGMAPH
DOUBLE PRECISION FATT, DFATT, FREP, DFREP
DOUBLE PRECISION DDFATT, DDFREP
!
! Derivatives of the pairwise site-site terms in terms of distance
!
FATT(RHO,XDUMM)=-1.0D0 + (1.0D0 - EXP(RHO*(1.0D0 - XDUMM)))**2
DFATT(RHO,XDUMM)=2.0D0*(-EXP(2.0D0*RHO*(1.0D0-XDUMM)) + EXP(RHO*(1.0D0-XDUMM)))*RHO
DDFATT(RHO,XDUMM)=-2.0D0*(-2.0D0*EXP(2.0D0*RHO*(1.0D0-XDUMM)) + EXP(RHO*(1.0D0-XDUMM)))*RHO**2
FREP(SIGMA,XDUMM)=(SIGMA/XDUMM)**12
DFREP(SIGMA,XDUMM)=-12.0D0*(SIGMA/XDUMM)**12/XDUMM
DDFREP(SIGMA,XDUMM)=156.0D0*(SIGMA/XDUMM)**12/XDUMM**2

ENERGY=0.0D0
IF (GTEST) V(1:3*NATOMS)=0.0D0
IF (SECT) HESS(1:3*NATOMS,1:3*NATOMS)=0.0D0

!
! 5 Morse plus two axial site pentamers from
! S.N. Fejer, T. James, J. Hernandez-Rojas and D.J. Wales, Phys. Chem. Chem. Phys., 11, 2098-2104 (2009). 
! Energy Landscapes for Shells Assembled from Pentagonal and Hexagonal Pyramids 
!
! RAD=5.0D0
! RADHEX=RAD*2.0*0.5877852522924731D0  ! 2 * Sin[36] to give the same edge length
! RHO=2.0D0
! SIGMA=(1.0D0+RAD*SQRT((5.0D0+SQRT(5.0D0))/2.0D0))
! SIGMAHEX=(1.0D0+RADHEX*SQRT((5.0D0+SQRT(5.0D0))/2.0D0))
! SIGMAPH=0.5D0*(SIGMA + SIGMAHEX)
! EPSEFF=0.4D0

RHO=CAPSIDRHO
EPSEFF=CAPSIDEPS
SIGMA=SIGMAPENT
RAD=RADPENT

!
! Three different sorts of axial repulsion
!
! pent-pent first
!
DO J1=1,NRIGIDBODY-NHEXAMERS
   DO J2=J1+1,NRIGIDBODY-NHEXAMERS
      NPOS1=RIGIDGROUPS(1,J1)
      NPOS2=RIGIDGROUPS(1,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMA,DIST)  ! axial-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMA,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMA,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMA,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF
      NPOS1=RIGIDGROUPS(1,J1)
      NPOS2=RIGIDGROUPS(2,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMA,DIST)  ! axial-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMA,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMA,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMA,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF

      NPOS1=RIGIDGROUPS(2,J1)
      NPOS2=RIGIDGROUPS(1,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMA,DIST)  ! axial 2-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMA,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMA,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMA,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF
   ENDDO
ENDDO
!
! pent-hex second
!
DO J1=1,NRIGIDBODY-NHEXAMERS
   DO J2=NRIGIDBODY-NHEXAMERS+1,NRIGIDBODY
      NPOS1=RIGIDGROUPS(1,J1)
      NPOS2=RIGIDGROUPS(1,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMAPH,DIST)  ! axial-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMAPH,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMAPH,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMAPH,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF
      NPOS1=RIGIDGROUPS(1,J1)
      NPOS2=RIGIDGROUPS(2,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMAPH,DIST)  ! axial-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMAPH,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMAPH,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMAPH,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF

      NPOS1=RIGIDGROUPS(2,J1)
      NPOS2=RIGIDGROUPS(1,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMAPH,DIST)  ! axial 2-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMAPH,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMAPH,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMAPH,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF
   ENDDO
ENDDO
!
! hex-hex third
!
DO J1=NRIGIDBODY-NHEXAMERS+1,NRIGIDBODY
   DO J2=J1+1,NRIGIDBODY
      NPOS1=RIGIDGROUPS(1,J1)
      NPOS2=RIGIDGROUPS(1,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMAHEX,DIST)  ! axial-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMAHEX,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMAHEX,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMAHEX,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF
      NPOS1=RIGIDGROUPS(1,J1)
      NPOS2=RIGIDGROUPS(2,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMAHEX,DIST)  ! axial-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMAHEX,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMAHEX,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMAHEX,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF

      NPOS1=RIGIDGROUPS(2,J1)
      NPOS2=RIGIDGROUPS(1,J2)
      DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+(X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
      ENERGY=ENERGY+EPSEFF*FREP(SIGMAHEX,DIST)  ! axial 2-axial repulsive term
      IF (GTEST) THEN
         RDIST=1.0D0/DIST
         DUMMY2=EPSEFF*DFREP(SIGMAHEX,DIST)*RDIST
         CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
      ENDIF
      IF (SECT) THEN
         DUMMY2=EPSEFF*DFREP(SIGMAHEX,DIST)
         DUMMY3=EPSEFF*DDFREP(SIGMAHEX,DIST)
         RDIST=1.0D0/DIST
         CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
      ENDIF
   ENDDO
ENDDO
!
! Sum over the attractive sites
!
DO J1=1,NRIGIDBODY
   DO J2=J1+1,NRIGIDBODY
      DO J3=3,NSITEPERBODY(J1)     ! # Morse sites in rb J1 NSITEPERBODY(J1)
         NPOS1=RIGIDGROUPS(J3,J1)    ! where is this site in the list?
         DO J4=3,NSITEPERBODY(J2)  ! # Morse sites in rb J2 NSITEPERBODY(J2)
            NPOS2=RIGIDGROUPS(J4,J2) ! where is this site in the list?
            DIST=SQRT((X(3*(NPOS1-1)+1)-X(3*(NPOS2-1)+1))**2+(X(3*(NPOS1-1)+2)-X(3*(NPOS2-1)+2))**2+ &
  &                   (X(3*(NPOS1-1)+3)-X(3*(NPOS2-1)+3))**2)
            ENERGY=ENERGY+FATT(RHO,DIST) 
            IF (GTEST) THEN
               RDIST=1.0D0/DIST
               DUMMY2=DFATT(RHO,DIST)*RDIST
               CALL DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
            ENDIF
            IF (SECT) THEN
               DUMMY2=DFATT(RHO,DIST)
               DUMMY3=DDFATT(RHO,DIST)
               RDIST=1.0D0/DIST
               CALL DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO

END SUBROUTINE DJWGR1

SUBROUTINE DJWGR1SEC(NATOMS,NPOS1,NPOS2,DUMMY3,DUMMY2,RDIST,X)
USE MODHESS
IMPLICIT NONE
INTEGER NPOS1, NPOS2, NATOMS, J1, J2, J3, J4
DOUBLE PRECISION X(3*NATOMS), DUMMY2, DUMMY3, RDIST, DUMMY4, DUMMY5

DUMMY4=(DUMMY3-DUMMY2*RDIST)*RDIST**2
J3=3*(NPOS1-1)
J4=3*(NPOS2-1)

DO J1=1,3
   DO J2=1,3
      DUMMY5=((X(J3+J1)-X(J4+J1))*(X(J3+J2)-X(J4+J2)))*DUMMY4
      HESS(J3+J1,J3+J2)=HESS(J3+J1,J3+J2)+DUMMY5
      HESS(J4+J1,J4+J2)=HESS(J4+J1,J4+J2)+DUMMY5
      HESS(J3+J1,J4+J2)=HESS(J3+J1,J4+J2)-DUMMY5
      HESS(J4+J1,J3+J2)=HESS(J4+J1,J3+J2)-DUMMY5
   ENDDO
   DUMMY5=DUMMY2*RDIST
   HESS(J3+J1,J3+J1)=HESS(J3+J1,J3+J1)+DUMMY5
   HESS(J4+J1,J4+J1)=HESS(J4+J1,J4+J1)+DUMMY5
   HESS(J3+J1,J4+J1)=HESS(J3+J1,J4+J1)-DUMMY5
   HESS(J4+J1,J3+J1)=HESS(J4+J1,J3+J1)-DUMMY5
ENDDO

END SUBROUTINE DJWGR1SEC

SUBROUTINE DJWGR1GRAD(NATOMS,NPOS1,NPOS2,DUMMY2,RDIST,X,V)
USE MODHESS
IMPLICIT NONE
INTEGER NPOS1, NPOS2, NATOMS, J1, J2, J3, J4
DOUBLE PRECISION X(3*NATOMS), DUMMY2, RDIST, DUMMY5, V(3*NATOMS)

J3=3*(NPOS1-1)
J4=3*(NPOS2-1)

DO J1=1,3
   DUMMY5=DUMMY2*(X(J3+J1)-X(J4+J1))
   V(J3+J1)=V(J3+J1)+DUMMY5
   V(J4+J1)=V(J4+J1)-DUMMY5
ENDDO

END SUBROUTINE DJWGR1GRAD

