!   GMIN: A program for finding global minima
!   Copyright (C) 1999-2006 David J. Wales
!   This file is part of GMIN.
!   Loop structure recoded by J.A. Elliott 2009
!
!   GMIN is free software; you can redistribute it and/or modIFy
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation; either version 2 of the License, or
!   (at your option) any later version.
!
!   GMIN is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program; IF not, write to the Free Software
!   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!=================================================================
! ds656> This module is intended for containing parameters 
!        required by model potentials. Hopefully, this will
!        reduce the clutter in commons.f90.
!=================================================================
!
MODULE POT_PARAMS
  !
  IMPLICIT NONE
  !
  ! --- Multicomponent Lennard-Jones parameters ----------------
  DOUBLE PRECISION, ALLOCATABLE :: MLJ_EPS(:,:), MLJ_SIG(:,:) 
  ! ------------------------------------------------------------
  !
  ! --- Multicomponent Gupta parameters ------------------------
  DOUBLE PRECISION, ALLOCATABLE :: MGUPTA_A(:,:), MGUPTA_XI(:,:), &
       MGUPTA_P(:,:), MGUPTA_Q(:,:), MGUPTA_R0(:,:), &
       MGUPTA_M2Q(:,:), MGUPTA_XI_SQ(:,:), &
       MGUPTA_MP_DIVBY_R0(:,:), MGUPTA_M2Q_DIVBY_R0(:,:)
  ! ------------------------------------------------------------
  !
  ! --- Multicomponent Sutton-Chen parameters ------------------
  INTEGER, ALLOCATABLE :: MSC_N(:,:), MSC_M(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: MSC_EPS(:,:), MSC_A(:,:), &
       MSC_C(:)
  DOUBLE PRECISION, ALLOCATABLE :: CUTA_REP(:,:),CUTB_REP(:,:), &
       CUTC_REP(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: CUTA_ATT(:,:),CUTB_ATT(:,:), &
       CUTC_ATT(:,:)
  ! ------------------------------------------------------------
  !
END MODULE POT_PARAMS
