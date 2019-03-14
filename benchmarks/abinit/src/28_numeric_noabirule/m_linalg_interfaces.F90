!{\src2tex{textfont=tt}}
!!****m* ABINIT/m_linalg_interfaces
!! NAME
!!  m_linalg_interfaces
!!
!! FUNCTION
!!  Interfaces for the BLAS and LAPACK linear algebra routines.
!!
!! COPYRIGHT
!!  Copyright (C) 2011-2018 ABINIT group (Yann Pouillon)
!!  This file is distributed under the terms of the
!!  GNU General Public License, see ~abinit/COPYING
!!  or http://www.gnu.org/copyleft/gpl.txt .
!!
!! WARNING
!!  These routines are used both by real and complex arrays
!!  and are commented (no interface):
!!  - AB_ZTRSM, AB_ZGEMM, AB_ZGEMV, AB_ZHEMM, AB_ZHERk, AB_ZHER, AB_ZGERC
!!  - AB_ZCOPY, AB_ZAXPY, AB_ZDSCAL, AB_ZSCAL, AB_ZDOTC
!!  - AB_ZHPEV, zgsev, AB_ZHEEV, AB_ZGETRF, AB_ZPOTRF, AB_ZHEGV, AB_ZHPEVx
!!  - AB_ZHPGV, AB_ZHEGST
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

module m_linalg_interfaces

 implicit none

 interface
  subroutine AB_CAXPY(n,ca,cx,incx,cy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   complex :: ca
   complex :: cx(*)
   complex :: cy(*)
  end subroutine AB_CAXPY
 end interface

 interface
  subroutine AB_CCOPY(n,cx,incx,cy,incy)
   implicit none
   integer, intent(in) :: incx, incy, n    !vz_i
   complex, intent(in) :: cx(*)    !vz_i
   complex, intent(inout) :: cy(*)    !vz_i
  end subroutine AB_CCOPY
 end interface

 interface
  complex function AB_CDOTC(n,cx,incx,cy,incy)
   implicit none
   integer, intent(in) :: incx, incy, n    !vz_i
   complex, intent(in) :: cx(*), cy(*)    !vz_i
  end function AB_CDOTC
 end interface

 interface
  complex function AB_CDOTU(n,cx,incx,cy,incy)
   implicit none
   integer, intent(in) :: incx, incy, n    !vz_i
   complex, intent(in) :: cx(*), cy(*)    !vz_i
  end function AB_CDOTU
 end interface

 interface
  subroutine AB_CGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: KL
   integer :: KU
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: BETA
   character*1 :: TRANS
   complex :: X( * )
   complex :: Y( * )
  end subroutine AB_CGBMV
 end interface

 interface
  subroutine AB_CGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer,intent(in) :: K,lda,ldb,ldc,m,n    !vz_i
   complex,intent(in) :: A( LDA, * )    !vz_i
   complex,intent(in) :: ALPHA    !vz_i
   complex,intent(in) :: B( LDB, * )    !vz_i
   complex,intent(in) :: BETA    !vz_i
   complex,intent(inout) :: C( LDC, * )    !vz_i
   character*1,intent(in) :: TRANSA    !vz_i
   character*1,intent(in) :: TRANSB    !vz_i
  end subroutine AB_CGEMM
 end interface

 interface
  subroutine AB_CGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer, intent(in) :: INCX, incy, lda, m, n    !vz_i
   complex, intent(in) :: A( LDA, * )    !vz_i
   complex, intent(in) :: ALPHA, beta    !vz_i
   character*1, intent(in) :: TRANS    !vz_i
   complex, intent(in) :: X( * )    !vz_i
   complex, intent(inout) :: Y( * )    !vz_i
  end subroutine AB_CGEMV
 end interface

 interface
  subroutine AB_CGERC ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer, intent(in) :: INCX, incy, lda, m, n    !vz_i
   complex, intent(inout) :: A( LDA, * )    !vz_i
   complex, intent(in) :: ALPHA    !vz_i
   complex, intent(in) :: X( * ), Y( * )    !vz_i
  end subroutine AB_CGERC
 end interface

 interface
  subroutine AB_CGERU ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: X( * )
   complex :: Y( * )
  end subroutine AB_CGERU
 end interface

 interface
  subroutine AB_CHBMV ( UPLO, N, K, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: K
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: BETA
   character*1 :: UPLO
   complex :: X( * )
   complex :: Y( * )
  end subroutine AB_CHBMV
 end interface

 interface
  subroutine AB_CHEMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: B( LDB, * )
   complex :: BETA
   complex :: C( LDC, * )
   character*1 :: SIDE
   character*1 :: UPLO
  end subroutine AB_CHEMM
 end interface

 interface
  subroutine AB_CHEMV ( UPLO, N, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: BETA
   character*1 :: UPLO
   complex :: X( * )
   complex :: Y( * )
  end subroutine AB_CHEMV
 end interface

 interface
  subroutine AB_CHER  ( UPLO, N, ALPHA, X, INCX, A, LDA )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   real :: ALPHA
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CHER
 end interface

 interface
  subroutine AB_CHER2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   character*1 :: UPLO
   complex :: X( * )
   complex :: Y( * )
  end subroutine AB_CHER2
 end interface

 interface
  subroutine AB_CHER2k( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: B( LDB, * )
   real :: BETA
   complex :: C( LDC, * )
   character*1 :: TRANS
   character*1 :: UPLO
  end subroutine AB_CHER2k
 end interface

 interface
  subroutine AB_CHERk ( UPLO, TRANS, N, K, ALPHA, A, LDA,&  
 BETA, C, LDC )
   implicit none
   integer, intent(in) :: K,lda,ldc,n    !vz_i
   complex,intent(in) :: A( LDA, * )    !vz_i
   real,intent(in) :: ALPHA    !vz_i
   real,intent(in) :: BETA    !vz_i
   complex,intent(inout) :: C( LDC, * )    !vz_i
   character*1,intent(in) :: TRANS    !vz_i
   character*1,intent(in) :: UPLO    !vz_i
  end subroutine AB_CHERk
 end interface

 interface
  subroutine AB_CHPMV ( UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   complex :: ALPHA
   complex :: AP( * )
   complex :: BETA
   character*1 :: UPLO
   complex :: X( * )
   complex :: Y( * )
  end subroutine AB_CHPMV
 end interface

 interface
  subroutine AB_CHPR  ( UPLO, N, ALPHA, X, INCX, AP )
   implicit none
   integer :: INCX
   integer :: N
   real :: ALPHA
   complex :: AP( * )
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CHPR
 end interface

 interface
  subroutine AB_CHPR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   complex :: ALPHA
   complex :: AP( * )
   character*1 :: UPLO
   complex :: X( * )
   complex :: Y( * )
  end subroutine AB_CHPR2
 end interface

 interface
  subroutine AB_CROTg(ca,cb,c,s)
   implicit none
   real :: c
   complex :: ca
   complex :: cb
   complex :: s
  end subroutine AB_CROTg
 end interface

 interface
  subroutine  AB_CSCAL(n,ca,cx,incx)
   implicit none
   integer :: incx
   integer :: n
   complex :: ca
   complex :: cx(*)
  end subroutine AB_CSCAL
 end interface

 interface
  subroutine  AB_CSROT (n,cx,incx,cy,incy,c,s)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   real :: c
   real :: s
   complex :: cx(1)
   complex :: cy(1)
  end subroutine AB_CSROT
 end interface

 interface
  subroutine  AB_CSSCAL(n,sa,cx,incx)
   implicit none
   integer :: incx
   integer :: n
   real :: sa
   complex :: cx(*)
  end subroutine AB_CSSCAL
 end interface

 interface
  subroutine  AB_CSWAP (n,cx,incx,cy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   complex :: cx(*)
   complex :: cy(*)
  end subroutine AB_CSWAP
 end interface

 interface
  subroutine AB_CSYMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: B( LDB, * )
   complex :: BETA
   complex :: C( LDC, * )
   character*1 :: SIDE
   character*1 :: UPLO
  end subroutine AB_CSYMM
 end interface

 interface
  subroutine AB_CSYR2k( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: B( LDB, * )
   complex :: BETA
   complex :: C( LDC, * )
   character*1 :: TRANS
   character*1 :: UPLO
  end subroutine AB_CSYR2k
 end interface

 interface
  subroutine AB_CSYRk ( UPLO, TRANS, N, K, ALPHA, A, LDA,&  
 BETA, C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: BETA
   complex :: C( LDC, * )
   character*1 :: TRANS
   character*1 :: UPLO
  end subroutine AB_CSYRk
 end interface

 interface
  subroutine AB_CTBMV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: K
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CTBMV
 end interface

 interface
  subroutine AB_CTBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: K
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CTBSV
 end interface

 interface
  subroutine AB_CTPMV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
   implicit none
   integer :: INCX
   integer :: N
   complex :: AP( * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CTPMV
 end interface

 interface
  subroutine AB_CTPSV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
   implicit none
   integer :: INCX
   integer :: N
   complex :: AP( * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CTPSV
 end interface

 interface
  subroutine AB_CTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,&  
 B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: B( LDB, * )
   character*1 :: DIAG
   character*1 :: SIDE
   character*1 :: TRANSA
   character*1 :: UPLO
  end subroutine AB_CTRMM
 end interface

 interface
  subroutine AB_CTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CTRMV
 end interface

 interface
  subroutine AB_CTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,&  
 B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: B( LDB, * )
   character*1 :: DIAG
   character*1 :: SIDE
   character*1 :: TRANSA
   character*1 :: UPLO
  end subroutine AB_CTRSM
 end interface

 interface
  subroutine AB_CTRSV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex :: X( * )
  end subroutine AB_CTRSV
 end interface

 interface
  double precision function AB_DASUM(n,dx,incx)
   implicit none
   integer :: incx
   integer :: n
   double precision :: dx(*)
  end function AB_DASUM
 end interface

 interface
  subroutine AB_DAXPY(n,da,dx,incx,dy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   double precision :: da
   double precision :: dx(*)
   double precision :: dy(*)
  end subroutine AB_DAXPY
 end interface

 interface
  double precision function AB_DCABS1(z)
   implicit none
   double complex :: z
  end function AB_DCABS1
 end interface

 !interface
 ! subroutine  AB_DCOPY(n,dx,incx,dy,incy)
 !  implicit none
 !  integer :: incx
 !  integer :: incy
 !  integer :: n
 !  double precision :: dx(*)
 !  double precision :: dy(*)
 ! end subroutine AB_DCOPY
 !end interface

 interface
  double precision function AB_DDOT(n,dx,incx,dy,incy)
   implicit none
   integer,intent(in) :: incx
   integer,intent(in) :: incy
   integer,intent(in) :: n
   double precision,intent(in) :: dx(*)
   double precision,intent(in) :: dy(*)
  end function AB_DDOT
 end interface

 interface
  subroutine AB_DGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: KL
   integer :: KU
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: ALPHA
   double precision :: BETA
   character*1 :: TRANS
   double precision :: X( * )
   double precision :: Y( * )
  end subroutine AB_DGBMV
 end interface

 interface
  subroutine AB_DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer,intent(in) :: K,lda,ldb,ldc,m,n    !vz_i
   double precision, intent(in) :: A( LDA, * )    !vz_i
   double precision,intent(in) :: ALPHA    !vz_i
   double precision,intent(in) :: B( LDB, * )    !vz_i
   double precision,intent(in) :: BETA    !vz_i
   double precision,intent(inout) :: C( LDC, * )    !vz_i
   character*1,intent(in) :: TRANSA    !vz_i
   character*1,intent(in) :: TRANSB    !vz_i
  end subroutine AB_DGEMM
 end interface

 interface
  subroutine AB_DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer, intent(in) :: INCX,incy,lda,m,n    !vz_i
   double precision, intent(in) :: A( LDA, * )    !vz_i
   double precision, intent(in) :: ALPHA    !vz_i
   double precision, intent(in) :: BETA    !vz_i
   character*1, intent(in) :: TRANS    !vz_i
   double precision, intent(in) :: X( * )    !vz_i
   double precision, intent(inout) :: Y( * )    !vz_i
  end subroutine AB_DGEMV
 end interface

 interface
  subroutine AB_DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: ALPHA
   double precision :: X( * )
   double precision :: Y( * )
  end subroutine AB_DGER
 end interface

 interface
  double precision function AB_DNRM2 ( N, X, INCX )
   implicit none
   integer, intent(in) :: INCX, n    !vz_i
   double precision,intent(in) :: X( * )    !vz_i
  end function AB_DNRM2
 end interface

 interface
  subroutine  AB_DROT (n,dx,incx,dy,incy,c,s)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   double precision :: c
   double precision :: s
   double precision :: dx(*)
   double precision :: dy(*)
  end subroutine AB_DROT
 end interface

 interface
  subroutine AB_DROTg(da,db,c,s)
   implicit none
   double precision :: c
   double precision :: da
   double precision :: db
   double precision :: s
  end subroutine AB_DROTg
 end interface

 interface
  subroutine AB_DROTm (N,DX,INCX,DY,INCY,DPARAM)
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   double precision :: DPARAM(5)
   double precision :: DX(1)
   double precision :: DY(1)
  end subroutine AB_DROTm
 end interface

 interface
  subroutine AB_DROTmg (DD1,DD2,DX1,DY1,DPARAM)
   implicit none
   double precision :: DD1
   double precision :: DD2
   double precision :: DPARAM(5)
   double precision :: DX1
   double precision :: DY1
  end subroutine AB_DROTmg
 end interface

 interface
  subroutine AB_DSBMV ( UPLO, N, K, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: K
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   double precision :: ALPHA
   double precision :: BETA
   character*1 :: UPLO
   double precision :: X( * )
   double precision :: Y( * )
  end subroutine AB_DSBMV
 end interface

 interface
  subroutine  AB_DSCAL(n,da,dx,incx)
   implicit none
   integer :: incx
   integer :: n
   double precision :: da
   double precision :: dx(*)
  end subroutine AB_DSCAL
 end interface

 interface
  double precision function AB_DSDOT (N, SX, INCX, SY, INCY)
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   real :: SX(*)
   real :: SY(*)
  end function AB_DSDOT
 end interface

 interface
  subroutine  AB_DSWAP (n,dx,incx,dy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   double precision :: dx(*)
   double precision :: dy(*)
  end subroutine AB_DSWAP
 end interface

 interface
  subroutine AB_DSYMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: ALPHA
   double precision :: B( LDB, * )
   double precision :: BETA
   double precision :: C( LDC, * )
   character*1 :: SIDE
   character*1 :: UPLO
  end subroutine AB_DSYMM
 end interface

 interface
  subroutine AB_DSYMV ( UPLO, N, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   double precision :: ALPHA
   double precision :: BETA
   character*1 :: UPLO
   double precision :: X( * )
   double precision :: Y( * )
  end subroutine AB_DSYMV
 end interface

 interface
  subroutine AB_DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,&  
 B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: ALPHA
   double precision :: B( LDB, * )
   character*1 :: DIAG
   character*1 :: SIDE
   character*1 :: TRANSA
   character*1 :: UPLO
  end subroutine AB_DTRMM
 end interface

 interface
  subroutine AB_DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   double precision :: X( * )
  end subroutine AB_DTRMV
 end interface

 interface
  subroutine AB_DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,&  
 B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: ALPHA
   double precision :: B( LDB, * )
   character*1 :: DIAG
   character*1 :: SIDE
   character*1 :: TRANSA
   character*1 :: UPLO
  end subroutine AB_DTRSM
 end interface

 interface
  subroutine AB_DTRSV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   double precision :: X( * )
  end subroutine AB_DTRSV
 end interface

 interface
  double precision function AB_DZASUM(n,zx,incx)
   implicit none
   integer :: incx
   integer :: n
   double complex :: zx(*)
  end function AB_DZASUM
 end interface

 interface
  double precision function AB_DZNRM2( N, X, INCX )
   implicit none
   integer, intent(in) :: INCX, n    !vz_i
   complex*16,intent(in) :: X( * )    !vz_i
  end function AB_DZNRM2
 end interface

 interface
  integer function AB_ICAMAX(n,cx,incx)
   implicit none
   integer :: incx
   integer :: n
   complex :: cx(*)
  end function AB_ICAMAX
 end interface

 interface
  integer function AB_IDAMAX(n,dx,incx)
   implicit none
   integer :: incx
   integer :: n
   double precision :: dx(*)
  end function AB_IDAMAX
 end interface

 interface
  integer function AB_ISAMAX(n,sx,incx)
   implicit none
   integer :: incx
   integer :: n
   real :: sx(*)
  end function AB_ISAMAX
 end interface

 interface
  integer function AB_IZAMAX(n,zx,incx)
   implicit none
   integer :: incx
   integer :: n
   double complex :: zx(*)
  end function AB_IZAMAX
 end interface

 interface
  real function AB_SASUM(n,sx,incx)
   implicit none
   integer :: incx
   integer :: n
   real :: sx(*)
  end function AB_SASUM
 end interface

 interface
  subroutine AB_SAXPY(n,sa,sx,incx,sy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   real :: sa
   real :: sx(*)
   real :: sy(*)
  end subroutine AB_SAXPY
 end interface

 interface
  real function AB_SCASUM(n,cx,incx)
   implicit none
   integer :: incx
   integer :: n
   complex :: cx(*)
  end function AB_SCASUM
 end interface

 interface
  real function AB_SCNRM2( N, X, INCX )
   implicit none
   integer, intent(in) :: INCX, n    !vz_i
   complex, intent(in) :: X( * )    !vz_i
  end function AB_SCNRM2
 end interface

 interface
  subroutine AB_SCOPY(n,sx,incx,sy,incy)
   implicit none
   integer, intent(in) :: incx,incy,n    !vz_i
   real, intent(in) :: sx(*)    !vz_i
   real, intent(inout) :: sy(*)    !vz_i
  end subroutine AB_SCOPY
 end interface

 interface
  real function AB_SDOT(n,sx,incx,sy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   real :: sx(*)
   real :: sy(*)
  end function AB_SDOT
 end interface

 interface
  real function AB_SDSDOT (N, SB, SX, INCX, SY, INCY)
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   real :: SB
   real :: SX(*)
   real :: SY(*)
  end function AB_SDSDOT
 end interface

 interface
  subroutine AB_SGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: KL
   integer :: KU
   integer :: LDA
   integer :: M
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: BETA
   character*1 :: TRANS
   real :: X( * )
   real :: Y( * )
  end subroutine AB_SGBMV
 end interface

 interface
  subroutine AB_SGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer, intent(in) :: K,lda,ldb,ldc,m,n    !vz_i
   real,intent(in) :: A( LDA, * )    !vz_i
   real,intent(in) :: ALPHA    !vz_i
   real,intent(in) :: B( LDB, * )    !vz_i
   real,intent(in) :: BETA    !vz_i
   real,intent(inout) :: C( LDC, * )    !vz_i
   character*1,intent(in) :: TRANSA    !vz_i
   character*1,intent(in) :: TRANSB    !vz_i
  end subroutine AB_SGEMM
 end interface

 interface
  subroutine AB_SGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer,intent(in) :: INCX, incy, lda,m,n    !vz_i
   real,intent(in) :: A( LDA, * )    !vz_i
   real,intent(in) :: ALPHA    !vz_i
   real,intent(in) :: BETA    !vz_i
   character*1,intent(in) :: TRANS    !vz_i
   real,intent(in) :: X( * )    !vz_i
   real,intent(inout) :: Y( * )    !vz_i
  end subroutine AB_SGEMV
 end interface

 interface
  subroutine AB_SGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: M
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: X( * )
   real :: Y( * )
  end subroutine AB_SGER
 end interface

 interface
  real function AB_SNRM2 ( N, X, INCX )
   implicit none
   integer,intent(in) :: INCX,n    !vz_i
   real,intent(in) :: X( * )    !vz_i
  end function AB_SNRM2
 end interface

 interface
  subroutine AB_SROT (n,sx,incx,sy,incy,c,s)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   real :: c
   real :: s
   real :: sx(*)
   real :: sy(*)
  end subroutine AB_SROT
 end interface

 interface
  subroutine AB_SROTg(sa,sb,c,s)
   implicit none
   real :: c
   real :: s
   real :: sa
   real :: sb
  end subroutine AB_SROTg
 end interface

 interface
  subroutine AB_SROTm (N,SX,INCX,SY,INCY,SPARAM)
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   real :: SPARAM(5)
   real :: SX(1)
   real :: SY(1)
  end subroutine AB_SROTm
 end interface

 interface
  subroutine AB_SROTmg (SD1,SD2,SX1,SY1,SPARAM)
   implicit none
   real :: SD1
   real :: SD2
   real :: SPARAM(5)
   real :: SX1
   real :: SY1
  end subroutine AB_SROTmg
 end interface

 interface
  subroutine AB_SSBMV ( UPLO, N, K, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: K
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: BETA
   character*1 :: UPLO
   real :: X( * )
   real :: Y( * )
  end subroutine AB_SSBMV
 end interface

 interface
  subroutine AB_SSCAL(n,sa,sx,incx)
   implicit none
   integer :: incx
   integer :: n
   real :: sa
   real :: sx(*)
  end subroutine AB_SSCAL
 end interface

 interface
  subroutine AB_SSPMV ( UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   real :: ALPHA
   real :: AP( * )
   real :: BETA
   character*1 :: UPLO
   real :: X( * )
   real :: Y( * )
  end subroutine AB_SSPMV
 end interface

 interface
  subroutine AB_SSPR  ( UPLO, N, ALPHA, X, INCX, AP )
   implicit none
   integer :: INCX
   integer :: N
   real :: ALPHA
   real :: AP( * )
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_SSPR
 end interface

 interface
  subroutine AB_SSPR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   real :: ALPHA
   real :: AP( * )
   character*1 :: UPLO
   real :: X( * )
   real :: Y( * )
  end subroutine AB_SSPR2
 end interface

 interface
  subroutine AB_SSWAP (n,sx,incx,sy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   real :: sx(*)
   real :: sy(*)
  end subroutine AB_SSWAP
 end interface

 interface
  subroutine AB_SSYMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: M
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: B( LDB, * )
   real :: BETA
   real :: C( LDC, * )
   character*1 :: SIDE
   character*1 :: UPLO
  end subroutine AB_SSYMM
 end interface

 interface
  subroutine AB_SSYMV ( UPLO, N, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: BETA
   character*1 :: UPLO
   real :: X( * )
   real :: Y( * )
  end subroutine AB_SSYMV
 end interface

 interface
  subroutine AB_SSYR  ( UPLO, N, ALPHA, X, INCX, A, LDA )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_SSYR
 end interface

 interface
  subroutine AB_SSYR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   character*1 :: UPLO
   real :: X( * )
   real :: Y( * )
  end subroutine AB_SSYR2
 end interface

 interface
  subroutine AB_SSYR2k( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: B( LDB, * )
   real :: BETA
   real :: C( LDC, * )
   character*1 :: TRANS
   character*1 :: UPLO
  end subroutine AB_SSYR2k
 end interface

 interface
  subroutine AB_SSYRk ( UPLO, TRANS, N, K, ALPHA, A, LDA,&  
 BETA, C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: BETA
   real :: C( LDC, * )
   character*1 :: TRANS
   character*1 :: UPLO
  end subroutine AB_SSYRk
 end interface

 interface
  subroutine AB_STBMV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: K
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_STBMV
 end interface

 interface
  subroutine AB_STBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: K
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_STBSV
 end interface

 interface
  subroutine AB_STPMV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
   implicit none
   integer :: INCX
   integer :: N
   real :: AP( * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_STPMV
 end interface

 interface
  subroutine AB_STPSV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
   implicit none
   integer :: INCX
   integer :: N
   real :: AP( * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_STPSV
 end interface

 interface
  subroutine AB_STRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,&  
 B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: B( LDB, * )
   character*1 :: DIAG
   character*1 :: SIDE
   character*1 :: TRANSA
   character*1 :: UPLO
  end subroutine AB_STRMM
 end interface

 interface
  subroutine AB_STRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_STRMV
 end interface

 interface
  subroutine AB_STRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,&  
 B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   real :: A( LDA, * )
   real :: ALPHA
   real :: B( LDB, * )
   character*1 :: DIAG
   character*1 :: SIDE
   character*1 :: TRANSA
   character*1 :: UPLO
  end subroutine AB_STRSM
 end interface

 interface
  subroutine AB_STRSV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   real :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   real :: X( * )
  end subroutine AB_STRSV
 end interface

 !interface
 ! subroutine AB_ZAXPY(n,za,zx,incx,zy,incy)
 !  implicit none
 !  integer :: incx
 !  integer :: incy
 !  integer :: n
 !  double complex :: za
 !  double complex :: zx(*)
 !  double complex :: zy(*)
 ! end subroutine AB_ZAXPY
 !end interface

 !interface
 ! subroutine  AB_ZCOPY(n,zx,incx,zy,incy)
 !  implicit none
 !  integer :: incx
 !  integer :: incy
 !  integer :: n
 !  double complex :: zx(*)
 !  double complex :: zy(*)
 ! end subroutine AB_ZCOPY
 !end interface

 !interface
 ! double complex function AB_ZDOTC(n,zx,incx,zy,incy)
 !  implicit none
 !  integer :: incx
 !  integer :: incy
 !  integer :: n
 !  double complex :: zx(*)
 !  double complex :: zy(*)
 ! end function AB_ZDOTC
 !end interface

 interface
  double complex function AB_ZDOTU(n,zx,incx,zy,incy)
   implicit none
   integer, intent(in) :: incx, incy, n    !vz_i
   double complex, intent(in) :: zx(*), zy(*)    !vz_i
  end function AB_ZDOTU
 end interface

 interface
  subroutine AB_ZDROT( N, CX, INCX, CY, INCY, C, S )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   double precision :: C
   complex*16 :: CX( * )
   complex*16 :: CY( * )
   double precision :: S
  end subroutine AB_ZDROT
 end interface

 !interface
 ! subroutine  AB_ZDSCAL(n,da,zx,incx)
 !  implicit none
 !  integer :: incx
 !  integer :: n
 !  double precision :: da
 !  double complex :: zx(*)
 ! end subroutine AB_ZDSCAL
 !end interface

 interface
  subroutine AB_ZGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: KL
   integer :: KU
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: BETA
   character*1 :: TRANS
   complex*16 :: X( * )
   complex*16 :: Y( * )
  end subroutine AB_ZGBMV
 end interface

 !interface
 ! subroutine AB_ZGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC )
 !  implicit none
 !  integer :: K
 !  integer :: LDA
 !  integer :: LDB
 !  integer :: LDC
 !  integer :: M
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: ALPHA
 !  complex*16 :: B( LDB, * )
 !  complex*16 :: BETA
 !  complex*16 :: C( LDC, * )
 !  character*1 :: TRANSA
 !  character*1 :: TRANSB
 ! end subroutine AB_ZGEMM
 !end interface

 !interface
 ! subroutine AB_ZGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY )
 !  implicit none
 !  integer :: INCX
 !  integer :: INCY
 !  integer :: LDA
 !  integer :: M
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: ALPHA
 !  complex*16 :: BETA
 !  character*1 :: TRANS
 !  complex*16 :: X( * )
 !  complex*16 :: Y( * )
 ! end subroutine AB_ZGEMV
 !end interface

 !interface
 ! subroutine AB_ZGERC ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
 !  implicit none
 !  integer :: INCX
 !  integer :: INCY
 !  integer :: LDA
 !  integer :: M
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: ALPHA
 !  complex*16 :: X( * )
 !  complex*16 :: Y( * )
 ! end subroutine AB_ZGERC
 !end interface

 interface
  subroutine AB_ZGERU ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: X( * )
   complex*16 :: Y( * )
  end subroutine AB_ZGERU
 end interface

 interface
  subroutine AB_ZHBMV ( UPLO, N, K, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: K
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: BETA
   character*1 :: UPLO
   complex*16 :: X( * )
   complex*16 :: Y( * )
  end subroutine AB_ZHBMV
 end interface

 !interface
 ! subroutine AB_ZHEMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC )
 !  implicit none
 !  integer :: LDA
 !  integer :: LDB
 !  integer :: LDC
 !  integer :: M
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: ALPHA
 !  complex*16 :: B( LDB, * )
 !  complex*16 :: BETA
 !  complex*16 :: C( LDC, * )
 !  character*1 :: SIDE
 !  character*1 :: UPLO
 ! end subroutine AB_ZHEMM
 !end interface

 interface
  subroutine AB_ZHEMV ( UPLO, N, ALPHA, A, LDA, X, INCX,&  
 BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: BETA
   character*1 :: UPLO
   complex*16 :: X( * )
   complex*16 :: Y( * )
  end subroutine AB_ZHEMV
 end interface

 !interface
 ! subroutine AB_ZHER  ( UPLO, N, ALPHA, X, INCX, A, LDA )
 !  implicit none
 !  integer :: INCX
 !  integer :: LDA
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  double precision :: ALPHA
 !  character*1 :: UPLO
 !  complex*16 :: X( * )
 ! end subroutine AB_ZHER
 !end interface

 interface
  subroutine AB_ZHER2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   character*1 :: UPLO
   complex*16 :: X( * )
   complex*16 :: Y( * )
  end subroutine AB_ZHER2
 end interface

 interface
  subroutine AB_ZHER2k( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB, BETA,&  
 C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: B( LDB, * )
   double precision :: BETA
   complex*16 :: C( LDC, * )
   character :: TRANS
   character :: UPLO
  end subroutine AB_ZHER2k
 end interface

 !interface
 ! subroutine AB_ZHERk( UPLO, TRANS, N, K, ALPHA, A, LDA, BETA, C, LDC )
 !  implicit none
 !  integer :: K
 !  integer :: LDA
 !  integer :: LDC
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  double precision :: ALPHA
 !  double precision :: BETA
 !  complex*16 :: C( LDC, * )
 !  character :: TRANS
 !  character :: UPLO
 ! end subroutine AB_ZHERk
 !end interface

 interface
  subroutine AB_ZHPMV ( UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   complex*16 :: ALPHA
   complex*16 :: AP( * )
   complex*16 :: BETA
   character*1 :: UPLO
   complex*16 :: X( * )
   complex*16 :: Y( * )
  end subroutine AB_ZHPMV
 end interface

 interface
  subroutine AB_ZHPR  ( UPLO, N, ALPHA, X, INCX, AP )
   implicit none
   integer :: INCX
   integer :: N
   double precision :: ALPHA
   complex*16 :: AP( * )
   character*1 :: UPLO
   complex*16 :: X( * )
  end subroutine AB_ZHPR
 end interface

 interface
  subroutine AB_ZHPR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   complex*16 :: ALPHA
   complex*16 :: AP( * )
   character*1 :: UPLO
   complex*16 :: X( * )
   complex*16 :: Y( * )
  end subroutine AB_ZHPR2
 end interface

 interface
  subroutine AB_ZROTg(ca,cb,c,s)
   implicit none
   double precision :: c
   double complex :: ca
   double complex :: cb
   double complex :: s
  end subroutine AB_ZROTg
 end interface

 !interface
 ! subroutine  AB_ZSCAL(n,za,zx,incx)
 !  implicit none
 !  integer :: incx
 !  integer :: n
 !  double complex :: za
 !  double complex :: zx(*)
 ! end subroutine AB_ZSCAL
 !end interface

 interface
  subroutine  AB_ZSWAP (n,zx,incx,zy,incy)
   implicit none
   integer :: incx
   integer :: incy
   integer :: n
   double complex :: zx(*)
   double complex :: zy(*)
  end subroutine AB_ZSWAP
 end interface

 interface
  subroutine AB_ZSYMM ( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: B( LDB, * )
   complex*16 :: BETA
   complex*16 :: C( LDC, * )
   character*1 :: SIDE
   character*1 :: UPLO
  end subroutine AB_ZSYMM
 end interface

 interface
  subroutine AB_ZSYR2k( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB,&  
 BETA, C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: B( LDB, * )
   complex*16 :: BETA
   complex*16 :: C( LDC, * )
   character*1 :: TRANS
   character*1 :: UPLO
  end subroutine AB_ZSYR2k
 end interface

 interface
  subroutine AB_ZSYRk ( UPLO, TRANS, N, K, ALPHA, A, LDA,&  
 BETA, C, LDC )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: BETA
   complex*16 :: C( LDC, * )
   character*1 :: TRANS
   character*1 :: UPLO
  end subroutine AB_ZSYRk
 end interface

 interface
  subroutine AB_ZTBMV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: K
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex*16 :: X( * )
  end subroutine AB_ZTBMV
 end interface

 interface
  subroutine AB_ZTBSV ( UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: K
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex*16 :: X( * )
  end subroutine AB_ZTBSV
 end interface

 interface
  subroutine AB_ZTPMV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
   implicit none
   integer :: INCX
   integer :: N
   complex*16 :: AP( * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex*16 :: X( * )
  end subroutine AB_ZTPMV
 end interface

 interface
  subroutine AB_ZTPSV ( UPLO, TRANS, DIAG, N, AP, X, INCX )
   implicit none
   integer :: INCX
   integer :: N
   complex*16 :: AP( * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex*16 :: X( * )
  end subroutine AB_ZTPSV
 end interface

 interface
  subroutine AB_ZTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,&  
 B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: B( LDB, * )
   character*1 :: DIAG
   character*1 :: SIDE
   character*1 :: TRANSA
   character*1 :: UPLO
  end subroutine AB_ZTRMM
 end interface

 interface
  subroutine AB_ZTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex*16 :: X( * )
  end subroutine AB_ZTRMV
 end interface

 !interface
 ! subroutine AB_ZTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA, B, LDB )
 !  implicit none
 !  integer :: LDA
 !  integer :: LDB
 !  integer :: M
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: ALPHA
 !  complex*16 :: B( LDB, * )
 !  character*1 :: DIAG
 !  character*1 :: SIDE
 !  character*1 :: TRANSA
 !  character*1 :: UPLO
 ! end subroutine AB_ZTRSM
 !end interface

 interface
  subroutine AB_ZTRSV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
   implicit none
   integer :: INCX
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character*1 :: DIAG
   character*1 :: TRANS
   character*1 :: UPLO
   complex*16 :: X( * )
  end subroutine AB_ZTRSV
 end interface

 interface
  subroutine AB_CGETF2( M, N, A, LDA, IPIV, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
  end subroutine AB_CGETF2
 end interface

 interface
  subroutine AB_CGETRF( M, N, A, LDA, IPIV, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
  end subroutine AB_CGETRF
 end interface

 interface
  subroutine AB_CGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: LWORK
   integer :: N
   complex :: A( LDA, * )
   complex :: WORK( * )
  end subroutine AB_CGETRI
 end interface

 interface
  subroutine AB_CHPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, RWORK,&  
 INFO )
   implicit none
   integer :: INFO
   integer :: LDZ
   integer :: N
   complex :: AP( * )
   character :: JOBZ
   real :: RWORK( * )
   character :: UPLO
   real :: W( * )
   complex :: WORK( * )
   complex :: Z( LDZ, * )
  end subroutine AB_CHPEV
 end interface

 interface
  subroutine AB_CHPTRD( UPLO, N, AP, D, E, TAU, INFO )
   implicit none
   integer :: INFO
   integer :: N
   complex :: AP( * )
   real :: D( * )
   real :: E( * )
   complex :: TAU( * )
   character :: UPLO
  end subroutine AB_CHPTRD
 end interface

 interface
  complex function AB_CLADIV( X, Y )
   implicit none
   complex :: X
   complex :: Y
  end function AB_CLADIV
 end interface

 interface
  real function AB_CLANHP( NORM, UPLO, N, AP, WORK )
   implicit none
   integer :: N
   complex :: AP( * )
   character :: NORM
   character :: UPLO
   real :: WORK( * )
  end function AB_CLANHP
 end interface

 interface
  subroutine AB_CLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
   implicit none
   integer :: INCV
   integer :: LDC
   integer :: M
   integer :: N
   complex :: C( LDC, * )
   character :: SIDE
   complex :: TAU
   complex :: V( * )
   complex :: WORK( * )
  end subroutine AB_CLARF
 end interface

 interface
  subroutine AB_CLARFg( N, ALPHA, X, INCX, TAU )
   implicit none
   integer :: INCX
   integer :: N
   complex :: ALPHA
   complex :: TAU
   complex :: X( * )
  end subroutine AB_CLARFg
 end interface

 interface
  subroutine AB_CLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
   implicit none
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   real :: C( * )
   character :: DIRECT
   character :: PIVOT
   real :: S( * )
   character :: SIDE
  end subroutine AB_CLASR
 end interface

 interface
  subroutine AB_CLASSQ( N, X, INCX, SCALE, SUMSQ )
   implicit none
   integer :: INCX
   integer :: N
   real :: SCALE
   real :: SUMSQ
   complex :: X( * )
  end subroutine AB_CLASSQ
 end interface

 interface
  subroutine AB_CLASWP( N, A, LDA, K1, K2, IPIV, INCX )
   implicit none
   integer :: INCX
   integer :: IPIV( * )
   integer :: K1
   integer :: K2
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
  end subroutine AB_CLASWP
 end interface

 interface
  subroutine clazro( M, N, ALPHA, BETA, A, LDA )
   implicit none
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: ALPHA
   complex :: BETA
  end subroutine clazro
 end interface

 interface
  subroutine AB_CSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDZ
   integer :: N
   character :: COMPZ
   real :: D( * )
   real :: E( * )
   real :: WORK( * )
   complex :: Z( LDZ, * )
  end subroutine AB_CSTEQR
 end interface

 interface
  subroutine AB_CTRTRI( UPLO, DIAG, N, A, LDA, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   complex :: A( LDA, * )
   character :: DIAG
   character :: UPLO
  end subroutine AB_CTRTRI
 end interface

 interface
  subroutine AB_CUNG2L( M, N, K, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: TAU( * )
   complex :: WORK( * )
  end subroutine AB_CUNG2L
 end interface

 interface
  subroutine AB_CUNG2R( M, N, K, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: M
   integer :: N
   complex :: A( LDA, * )
   complex :: TAU( * )
   complex :: WORK( * )
  end subroutine AB_CUNG2R
 end interface

 interface
  subroutine AB_CUPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDQ
   integer :: N
   complex :: AP( * )
   complex :: Q( LDQ, * )
   complex :: TAU( * )
   character :: UPLO
   complex :: WORK( * )
  end subroutine AB_CUPGTR
 end interface

 interface
  subroutine AB_DBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,&  
 LDU, C, LDC, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDC
   integer :: LDU
   integer :: LDVT
   integer :: N
   integer :: NCC
   integer :: NCVT
   integer :: NRU
   double precision :: C( LDC, * )
   double precision :: D( * )
   double precision :: E( * )
   double precision :: U( LDU, * )
   character :: UPLO
   double precision :: VT( LDVT, * )
   double precision :: WORK( * )
  end subroutine AB_DBDSQR
 end interface

 interface
  subroutine AB_DGEBD2( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   double precision :: TAUP( * )
   double precision :: TAUQ( * )
   double precision :: WORK( * )
  end subroutine AB_DGEBD2
 end interface

 interface
  subroutine AB_DGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,&  
 INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   double precision :: TAUP( * )
   double precision :: TAUQ( * )
   double precision :: WORK( * )
  end subroutine AB_DGEBRD
 end interface

 interface
  subroutine AB_DGELQ2( M, N, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DGELQ2
 end interface

 interface
  subroutine AB_DGELQf( M, N, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DGELQf
 end interface

 interface
  subroutine AB_DGELSs( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,&  
 WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LDB
   integer :: LWORK
   integer :: M
   integer :: N
   integer :: NRHS
   integer :: RANK
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   double precision :: RCOND
   double precision :: S( * )
   double precision :: WORK( * )
  end subroutine AB_DGELSs
 end interface

 interface
  subroutine AB_DGEQR2( M, N, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DGEQR2
 end interface

 interface
  subroutine AB_DGEQRf( M, N, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DGEQRf
 end interface

 interface
  subroutine AB_DGESVd( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,&  
 WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LDU
   integer :: LDVT
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   character :: JOBU
   character :: JOBVT
   double precision :: S( * )
   double precision :: U( LDU, * )
   double precision :: VT( LDVT, * )
   double precision :: WORK( * )
  end subroutine AB_DGESVd
 end interface

 interface
  subroutine AB_DGETF2( M, N, A, LDA, IPIV, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
  end subroutine AB_DGETF2
 end interface

 interface
  subroutine AB_DGETRF( M, N, A, LDA, IPIV, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
  end subroutine AB_DGETRF
 end interface

 interface
  subroutine AB_DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: LWORK
   integer :: N
   double precision :: A( LDA, * )
   double precision :: WORK( * )
  end subroutine AB_DGETRI
 end interface

 interface
  subroutine AB_DOPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDQ
   integer :: N
   double precision :: AP( * )
   double precision :: Q( LDQ, * )
   double precision :: TAU( * )
   character :: UPLO
   double precision :: WORK( * )
  end subroutine AB_DOPGTR
 end interface

 interface
  subroutine AB_DORG2L( M, N, K, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DORG2L
 end interface

 interface
  subroutine AB_DORG2R( M, N, K, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DORG2R
 end interface

 interface
  subroutine AB_DORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   character :: VECT
   double precision :: WORK( * )
  end subroutine AB_DORGBR
 end interface

 interface
  subroutine AB_DORGL2( M, N, K, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DORGL2
 end interface

 interface
  subroutine AB_DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DORGLQ
 end interface

 interface
  subroutine AB_DORGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DORGQL
 end interface

 interface
  subroutine AB_DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   double precision :: WORK( * )
  end subroutine AB_DORGQR
 end interface

 interface
  subroutine AB_DORGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: N
   double precision :: A( LDA, * )
   double precision :: TAU( * )
   character :: UPLO
   double precision :: WORK( LWORK )
  end subroutine AB_DORGTR
 end interface

 interface
  subroutine AB_DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,&  
 WORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: C( LDC, * )
   character :: SIDE
   double precision :: TAU( * )
   character :: TRANS
   double precision :: WORK( * )
  end subroutine AB_DORM2R
 end interface

 interface
  subroutine AB_DORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,&  
 LDC, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: C( LDC, * )
   character :: SIDE
   double precision :: TAU( * )
   character :: TRANS
   character :: VECT
   double precision :: WORK( * )
  end subroutine AB_DORMBR
 end interface

 interface
  subroutine AB_DORML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,&  
 WORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: C( LDC, * )
   character :: SIDE
   double precision :: TAU( * )
   character :: TRANS
   double precision :: WORK( * )
  end subroutine AB_DORML2
 end interface

 interface
  subroutine AB_DORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,&  
 WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: C( LDC, * )
   character :: SIDE
   double precision :: TAU( * )
   character :: TRANS
   double precision :: WORK( * )
  end subroutine AB_DORMLQ
 end interface

 interface
  subroutine AB_DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,&  
 WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: K
   integer :: LDA
   integer :: LDC
   integer :: LWORK
   integer :: M
   integer :: N
   double precision :: A( LDA, * )
   double precision :: C( LDC, * )
   character :: SIDE
   double precision :: TAU( * )
   character :: TRANS
   double precision :: WORK( * )
  end subroutine AB_DORMQR
 end interface

 interface
  subroutine AB_DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LDB
   integer :: N
   integer :: NRHS
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   character :: UPLO
  end subroutine AB_DPOSV
 end interface

 interface
  subroutine AB_DPOTF2( UPLO, N, A, LDA, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   character :: UPLO
  end subroutine AB_DPOTF2
 end interface

 interface
  subroutine AB_DPOTRF( UPLO, N, A, LDA, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   character :: UPLO
  end subroutine AB_DPOTRF
 end interface

 interface
  subroutine AB_DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LDB
   integer :: N
   integer :: NRHS
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   character :: UPLO
  end subroutine AB_DPOTRS
 end interface

 interface
  subroutine AB_DPPTRF( UPLO, N, AP, INFO )
   implicit none
   integer :: INFO
   integer :: N
   double precision :: AP( * )
   character :: UPLO
  end subroutine AB_DPPTRF
 end interface

 interface
  subroutine AB_DSPGST( ITYPE, UPLO, N, AP, BP, INFO )
   implicit none
   integer :: INFO
   integer :: ITYPE
   integer :: N
   double precision :: AP( * )
   double precision :: BP( * )
   character :: UPLO
  end subroutine AB_DSPGST
 end interface

 interface
  subroutine AB_DSPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,&
 INFO )
   implicit none
   integer :: INFO
   integer :: ITYPE
   integer :: LDZ
   integer :: N
   double precision :: AP( * )
   double precision :: BP( * )
   double precision :: W( * )
   double precision :: WORK( * )
   double precision :: Z( LDZ, * )
   character :: JOBZ
   character :: UPLO
  end subroutine AB_DSPGV
 end interface

 interface
  subroutine AB_DRSCL( N, SA, SX, INCX )
   implicit none
   integer :: INCX
   integer :: N
   double precision :: SA
   double precision :: SX( * )
  end subroutine AB_DRSCL
 end interface

 interface
  subroutine AB_DSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDZ
   integer :: N
   double precision :: AP( * )
   character :: JOBZ
   character :: UPLO
   double precision :: W( * )
   double precision :: WORK( * )
   double precision :: Z( LDZ, * )
  end subroutine AB_DSPEV
 end interface

 interface
  subroutine AB_DSPTRD( UPLO, N, AP, D, E, TAU, INFO )
   implicit none
   integer :: INFO
   integer :: N
   double precision :: AP( * )
   double precision :: D( * )
   double precision :: E( * )
   double precision :: TAU( * )
   character :: UPLO
  end subroutine AB_DSPTRD
 end interface

 interface
  subroutine AB_DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E,&  
 M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,&  
 INFO )
   implicit none
   integer :: IBLOCK( * )
   integer :: IL
   integer :: INFO
   integer :: ISPLIT( * )
   integer :: IU
   integer :: IWORK( * )
   integer :: M
   integer :: N
   integer :: NSPLIT
   double precision :: ABSTOL
   double precision :: D( * )
   double precision :: E( * )
   character :: ORDER
   character :: RANGE
   double precision :: VL
   double precision :: VU
   double precision :: W( * )
   double precision :: WORK( * )
  end subroutine AB_DSTEBZ
 end interface

 interface
  subroutine AB_DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDZ
   integer :: N
   character :: COMPZ
   double precision :: D( * )
   double precision :: E( * )
   double precision :: WORK( * )
   double precision :: Z( LDZ, * )
  end subroutine AB_DSTEQR
 end interface

 interface
  subroutine AB_DSTERF( N, D, E, INFO )
   implicit none
   integer :: INFO
   integer :: N
   double precision :: D( * )
   double precision :: E( * )
  end subroutine AB_DSTERF
 end interface

 interface
  subroutine AB_DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: N
   double precision :: A( LDA, * )
   character :: JOBZ
   character :: UPLO
   double precision :: W( * )
   double precision :: WORK( * )
  end subroutine AB_DSYEV
 end interface

 interface
  subroutine AB_DSYGS2( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
   implicit none
   integer :: INFO
   integer :: ITYPE
   integer :: LDA
   integer :: LDB
   integer :: N
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   character :: UPLO
  end subroutine AB_DSYGS2
 end interface

 interface
  subroutine AB_DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
   implicit none
   integer :: INFO
   integer :: ITYPE
   integer :: LDA
   integer :: LDB
   integer :: N
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   character :: UPLO
  end subroutine AB_DSYGST
 end interface

 interface
  subroutine AB_DSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&  
 LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: ITYPE
   integer :: LDA
   integer :: LDB
   integer :: LWORK
   integer :: N
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   character :: JOBZ
   character :: UPLO
   double precision :: W( * )
   double precision :: WORK( * )
  end subroutine AB_DSYGV
 end interface

 interface
  subroutine AB_DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,&  
 LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: LDB
   integer :: LWORK
   integer :: N
   integer :: NRHS
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   character :: UPLO
   double precision :: WORK( * )
  end subroutine AB_DSYSV
 end interface

 interface
  subroutine AB_DSYTD2( UPLO, N, A, LDA, D, E, TAU, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   double precision :: TAU( * )
   character :: UPLO
  end subroutine AB_DSYTD2
 end interface

 interface
  subroutine AB_DSYTF2( UPLO, N, A, LDA, IPIV, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   character :: UPLO
  end subroutine AB_DSYTF2
 end interface

 interface
  subroutine AB_DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: N
   double precision :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   double precision :: TAU( * )
   character :: UPLO
   double precision :: WORK( * )
  end subroutine AB_DSYTRD
 end interface

 interface
  subroutine AB_DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: LWORK
   integer :: N
   double precision :: A( LDA, * )
   character :: UPLO
   double precision :: WORK( * )
  end subroutine AB_DSYTRF
 end interface

 interface
  subroutine AB_DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: LDB
   integer :: N
   integer :: NRHS
   double precision :: A( LDA, * )
   double precision :: B( LDB, * )
   character :: UPLO
  end subroutine AB_DSYTRS
 end interface

 interface
  subroutine AB_DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   character :: DIAG
   character :: UPLO
  end subroutine AB_DTRTI2
 end interface

 interface
  subroutine AB_DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   double precision :: A( LDA, * )
   character :: DIAG
   character :: UPLO
  end subroutine AB_DTRTRI
 end interface

 interface
  double precision function AB_DZSUM1( N, CX, INCX )
   implicit none
   integer :: INCX
   integer :: N
   complex*16 :: CX( * )
  end function AB_DZSUM1
 end interface

 interface
  integer function AB_IZMAX1( N, CX, INCX )
   implicit none
   integer :: INCX
   integer :: N
   complex*16 :: CX( * )
  end function AB_IZMAX1
 end interface

 interface
  subroutine AB_SSTERF( N, D, E, INFO )
   implicit none
   integer :: INFO
   integer :: N
   real :: D( * )
   real :: E( * )
  end subroutine AB_SSTERF
 end interface

 interface
  subroutine AB_ZBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,&  
 LDU, C, LDC, RWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDC
   integer :: LDU
   integer :: LDVT
   integer :: N
   integer :: NCC
   integer :: NCVT
   integer :: NRU
   complex*16 :: C( LDC, * )
   double precision :: D( * )
   double precision :: E( * )
   double precision :: RWORK( * )
   complex*16 :: U( LDU, * )
   character :: UPLO
   complex*16 :: VT( LDVT, * )
  end subroutine AB_ZBDSQR
 end interface

 interface
  subroutine AB_ZGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,&  
 INFO )
   implicit none
   integer :: IHI
   integer :: ILO
   integer :: INFO
   integer :: LDV
   integer :: M
   integer :: N
   character :: JOB
   double precision :: SCALE( * )
   character :: SIDE
   complex*16 :: V( LDV, * )
  end subroutine AB_ZGEBAK
 end interface

 interface
  subroutine AB_ZGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
   implicit none
   integer :: IHI
   integer :: ILO
   integer :: INFO
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character :: JOB
   double precision :: SCALE( * )
  end subroutine AB_ZGEBAL
 end interface

 interface
  subroutine AB_ZGEBD2( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   complex*16 :: TAUP( * )
   complex*16 :: TAUQ( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGEBD2
 end interface

 interface
  subroutine AB_ZGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,&  
 INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   complex*16 :: TAUP( * )
   complex*16 :: TAUQ( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGEBRD
 end interface

 interface
  subroutine AB_ZGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, W, VS,&  
 LDVS, WORK, LWORK, RWORK, BWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LDVS
   integer :: LWORK
   integer :: N
   integer :: SDIM
   complex*16 :: A( LDA, * )
   logical :: BWORK( * )
   character :: JOBVS
   double precision :: RWORK( * )
   logical :: SELECT
   character :: SORT
   complex*16 :: VS( LDVS, * )
   complex*16 :: W( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGEES
 end interface

 interface
  subroutine AB_ZGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR,&
                    WORK, LWORK, RWORK, INFO )
   implicit none
   character :: JOBVL
   character :: JOBVR
   integer :: INFO 
   integer :: LDA
   integer :: LDVL 
   integer :: LDVR  
   integer :: LWORK
   integer :: N
   double precision :: RWORK( * )
   complex*16 :: A( LDA, * )
   complex*16 :: VL( LDVL, * ) 
   complex*16 :: VR( LDVR, * )
   complex*16 :: W( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGEEV
 end interface

 interface
  subroutine AB_ZGEHD2( N, ILO, IHI, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: IHI
   integer :: ILO
   integer :: INFO
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: TAU( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGEHD2
 end interface

 interface
  subroutine AB_ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: IHI
   integer :: ILO
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: TAU( * )
   complex*16 :: WORK( LWORK )
  end subroutine AB_ZGEHRD
 end interface

 interface
  subroutine AB_ZGELQ2( M, N, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: TAU( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGELQ2
 end interface

 interface
  subroutine AB_ZGELQf( M, N, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: TAU( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGELQf
 end interface

 interface
  subroutine AB_ZGEQR2( M, N, A, LDA, TAU, WORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: TAU( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGEQR2
 end interface

 interface
  subroutine AB_ZGEQRf( M, N, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: TAU( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGEQRf
 end interface

 !interface
 ! subroutine AB_ZGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: IPIV( * )
 !  integer :: LDA
 !  integer :: LDB
 !  integer :: N
 !  integer :: NRHS
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: B( LDB, * )
 ! end subroutine AB_ZGESV
 !end interface

 interface
  subroutine AB_ZGESVd( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,&  
 WORK, LWORK, RWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LDU
   integer :: LDVT
   integer :: LWORK
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   character :: JOBU
   character :: JOBVT
   double precision :: RWORK( * )
   double precision :: S( * )
   complex*16 :: U( LDU, * )
   complex*16 :: VT( LDVT, * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGESVd
 end interface

 interface
  subroutine AB_ZGETF2( M, N, A, LDA, IPIV, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
  end subroutine AB_ZGETF2
 end interface

 interface
  subroutine AB_ZGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
   implicit none
   integer :: INFO 
   integer :: LDA
   integer :: LWORK
   integer :: N
   integer :: IPIV( * )
   complex*16 :: A( LDA, * )
   complex*16 :: WORK( * )
  end subroutine AB_ZGETRI
 end interface

 !interface 
 ! subroutine AB_ZGETRF( M, N, A, LDA, IPIV, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: IPIV( * )
 !  integer :: LDA
 !  integer :: M
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 ! end subroutine AB_ZGETRF
 !end interface

 interface
  subroutine AB_ZGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
   implicit none
   integer :: INFO
   integer :: IPIV( * )
   integer :: LDA
   integer :: LDB
   integer :: N
   integer :: NRHS
   complex*16 :: A( LDA, * )
   complex*16 :: B( LDB, * )
   character :: TRANS
  end subroutine AB_ZGETRS
 end interface

 !interface
 ! subroutine AB_ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: LDA
 !  integer :: LWORK
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  character :: JOBZ
 !  double precision :: RWORK( * )
 !  character :: UPLO
 !  double precision :: W( * )
 !  complex*16 :: WORK( * )
 ! end subroutine AB_ZHEEV
 !end interface

 interface
  subroutine AB_ZHEGS2( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
   implicit none
   integer :: INFO
   integer :: ITYPE
   integer :: LDA
   integer :: LDB
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: B( LDB, * )
   character :: UPLO
  end subroutine AB_ZHEGS2
 end interface

 !interface
 ! subroutine AB_ZHEGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: ITYPE
 !  integer :: LDA
 !  integer :: LDB
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: B( LDB, * )
 !  character :: UPLO
 ! end subroutine AB_ZHEGST
 !end interface

 !interface
 ! subroutine AB_ZHEGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&  
 !  LWORK, RWORK, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: ITYPE
 !  integer :: LDA
 !  integer :: LDB
 !  integer :: LWORK
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  complex*16 :: B( LDB, * )
 !  character :: JOBZ
 !  double precision :: RWORK( * )
 !  character :: UPLO
 !  double precision :: W( * )
 !  complex*16 :: WORK( * )
 ! end subroutine AB_ZHEGV
 !end interface

 interface
  subroutine AB_ZHETD2( UPLO, N, A, LDA, D, E, TAU, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   complex*16 :: TAU( * )
   character :: UPLO
  end subroutine AB_ZHETD2
 end interface

 interface
  subroutine AB_ZHETRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: LWORK
   integer :: N
   complex*16 :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   complex*16 :: TAU( * )
   character :: UPLO
   complex*16 :: WORK( * )
  end subroutine AB_ZHETRD
 end interface

 !interface
 ! subroutine AB_ZHPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, RWORK, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: LDZ
 !  integer :: N
 !  complex*16 :: AP( * )
 !  character :: JOBZ
 !  double precision :: RWORK( * )
 !  character :: UPLO
 !  double precision :: W( * )
 !  complex*16 :: WORK( * )
 !  complex*16 :: Z( LDZ, * )
 ! end subroutine AB_ZHPEV
 !end interface

 !interface
 ! subroutine AB_ZHPEVx( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,&  
 !  ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK,&  
 !  IFAIL, INFO )
 !  implicit none
 !  integer :: IFAIL( * )
 !  integer :: IL
 !  integer :: INFO
 !  integer :: IU
 !  integer :: IWORK( * )
 !  integer :: LDZ
 !  integer :: M
 !  integer :: N
 !  double precision :: ABSTOL
 !  complex*16 :: AP( * )
 !  character :: JOBZ
 !  character :: RANGE
 !  double precision :: RWORK( * )
 !  character :: UPLO
 !  double precision :: VL
 !  double precision :: VU
 !  double precision :: W( * )
 !  complex*16 :: WORK( * )
 !  complex*16 :: Z( LDZ, * )
 ! end subroutine AB_ZHPEVx
 !end interface

 interface
  subroutine AB_ZHPGST( ITYPE, UPLO, N, AP, BP, INFO )
   implicit none
   integer :: INFO
   integer :: ITYPE
   integer :: N
   complex*16 :: AP( * )
   complex*16 :: BP( * )
   character :: UPLO
  end subroutine AB_ZHPGST
 end interface

 !interface
 ! subroutine AB_ZHPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,&  
 !  RWORK, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: ITYPE
 !  integer :: LDZ
 !  integer :: N
 !  complex*16 :: AP( * )
 !  complex*16 :: BP( * )
 !  character :: JOBZ
 !  double precision :: RWORK( * )
 !  character :: UPLO
 !  double precision :: W( * )
 !  complex*16 :: WORK( * )
 !  complex*16 :: Z( LDZ, * )
 ! end subroutine AB_ZHPGV
 !end interface

 interface
  subroutine AB_ZHPTRD( UPLO, N, AP, D, E, TAU, INFO )
   implicit none
   integer :: INFO
   integer :: N
   complex*16 :: AP( * )
   double precision :: D( * )
   double precision :: E( * )
   complex*16 :: TAU( * )
   character :: UPLO
  end subroutine AB_ZHPTRD
 end interface

 interface
  subroutine AB_ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,&  
 WORK, LWORK, INFO )
   implicit none
   integer :: IHI
   integer :: ILO
   integer :: INFO
   integer :: LDH
   integer :: LDZ
   integer :: LWORK
   integer :: N
   character :: COMPZ
   complex*16 :: H( LDH, * )
   character :: JOB
   complex*16 :: W( * )
   complex*16 :: WORK( * )
   complex*16 :: Z( LDZ, * )
  end subroutine AB_ZHSEQR
 end interface

 interface
  subroutine AB_ZLABRD( M, N, NB, A, LDA, D, E, TAUQ, TAUP, X, LDX, Y,&  
 LDY )
   implicit none
   integer :: LDA
   integer :: LDX
   integer :: LDY
   integer :: M
   integer :: N
   integer :: NB
   complex*16 :: A( LDA, * )
   double precision :: D( * )
   double precision :: E( * )
   complex*16 :: TAUP( * )
   complex*16 :: TAUQ( * )
   complex*16 :: X( LDX, * )
   complex*16 :: Y( LDY, * )
  end subroutine AB_ZLABRD
 end interface

 interface
  subroutine AB_ZLACGV( N, X, INCX )
   implicit none
   integer :: INCX
   integer :: N
   complex*16 :: X( * )
  end subroutine AB_ZLACGV
 end interface

 interface
  subroutine AB_ZLACON( N, V, X, EST, KASE )
   implicit none
   integer :: KASE
   integer :: N
   double precision :: EST
   complex*16 :: V( N )
   complex*16 :: X( N )
  end subroutine AB_ZLACON
 end interface

 interface
  subroutine AB_ZLACPY( UPLO, M, N, A, LDA, B, LDB )
   implicit none
   integer :: LDA
   integer :: LDB
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: B( LDB, * )
   character :: UPLO
  end subroutine AB_ZLACPY
 end interface

 interface
  double complex function AB_ZLADIV( X, Y )
   implicit none
   complex*16 :: X
   complex*16 :: Y
  end function AB_ZLADIV
 end interface

 interface
  subroutine AB_ZLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILOZ,&  
 IHIZ, Z, LDZ, INFO )
   implicit none
   integer :: IHI
   integer :: IHIZ
   integer :: ILO
   integer :: ILOZ
   integer :: INFO
   integer :: LDH
   integer :: LDZ
   integer :: N
   complex*16 :: H( LDH, * )
   complex*16 :: W( * )
   logical :: WANTT
   logical :: WANTZ
   complex*16 :: Z( LDZ, * )
  end subroutine AB_ZLAHQR
 end interface

 interface
  subroutine AB_ZLAHRD( N, K, NB, A, LDA, TAU, T, LDT, Y, LDY )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDT
   integer :: LDY
   integer :: N
   integer :: NB
   complex*16 :: A( LDA, * )
   complex*16 :: T( LDT, NB )
   complex*16 :: TAU( NB )
   complex*16 :: Y( LDY, NB )
  end subroutine AB_ZLAHRD
 end interface

 interface
  subroutine AB_ZLAHR2( N, K, NB, A, LDA, TAU, T, LDT, Y, LDY )
   implicit none
   integer :: K
   integer :: LDA
   integer :: LDT
   integer :: LDY
   integer :: N
   integer :: NB
   complex*16 :: A( LDA, * )
   complex*16 :: T( LDT, NB )
   complex*16 :: TAU( NB )
   complex*16 :: Y( LDY, NB )
  end subroutine AB_ZLAHR2
 end interface

 interface
  double precision function AB_ZLANGE( NORM, M, N, A, LDA, WORK )
   implicit none
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   character :: NORM
   double precision :: WORK( * )
  end function AB_ZLANGE
 end interface

 interface
  double precision function AB_ZLANHE( NORM, UPLO, N, A, LDA, WORK )
   implicit none
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character :: NORM
   character :: UPLO
   double precision :: WORK( * )
  end function AB_ZLANHE
 end interface

 interface
  double precision function AB_ZLANHP( NORM, UPLO, N, AP, WORK )
   implicit none
   integer :: N
   complex*16 :: AP( * )
   character :: NORM
   character :: UPLO
   double precision :: WORK( * )
  end function AB_ZLANHP
 end interface

 interface
  double precision function AB_ZLANHS( NORM, N, A, LDA, WORK )
   implicit none
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character :: NORM
   double precision :: WORK( * )
  end function AB_ZLANHS
 end interface

 interface
  subroutine AB_ZLAQR0( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILOZ,&
 IHIZ, Z, LDZ, WORK, LWORK, INFO )
   implicit none
   integer :: IHI
   integer :: IHIZ
   integer :: ILO
   integer :: ILOZ
   integer :: INFO
   integer :: LDH
   integer :: LDZ
   integer :: LWORK
   integer :: N
   
   complex*16 :: H( LDH, * )
   complex*16 :: W( * )
   complex*16 :: WORK( * )
   complex*16 :: Z( LDZ, * )

   logical :: WANTT
   logical :: WANTZ
  end subroutine AB_ZLAQR0
 end interface

 interface
  subroutine AB_ZLAQR1( N, H, LDH, S1, S2, V )
   implicit none
   integer :: LDH
   integer :: N
   complex*16 :: S1 
   complex*16 :: S2
   complex*16 :: H( LDH, * )
   complex*16 :: V( * )
  end subroutine AB_ZLAQR1
 end interface

 interface
  subroutine AB_ZLAQR2( WANTT, WANTZ, N, KTOP, KBOT, NW, H, LDH, ILOZ,&
 IHIZ, Z, LDZ, NS, ND, SH, V, LDV, NH, T, LDT,&
 NV, WV, LDWV, WORK, LWORK )
   implicit none
   integer :: IHIZ, ILOZ, KBOT, KTOP, LDH, LDT
   integer :: LDV, LDWV, LDZ, LWORK, N, ND, NH
   integer :: NS, NV, NW
   logical :: WANTT, WANTZ
   complex*16 :: H( LDH, * ), SH( * ), T( LDT, * )
   complex*16 :: V( LDV, * ),WORK( * ), WV( LDWV, * ), Z( LDZ, * )
  end subroutine AB_ZLAQR2
 end interface

 interface
  subroutine AB_ZLAQR3( WANTT, WANTZ, N, KTOP, KBOT, NW, H, LDH, ILOZ,&
 IHIZ, Z, LDZ, NS, ND, SH, V, LDV, NH, T, LDT,&
 NV, WV, LDWV, WORK, LWORK )
   implicit none
   integer :: IHIZ, ILOZ, KBOT, KTOP, LDH, LDT, LDV, LDWV
   integer :: LDZ, LWORK, N, ND, NH, NS, NV, NW
   logical :: WANTT, WANTZ
   complex*16 :: H( LDH, * ), SH( * ), T( LDT, * )
   complex*16 :: V( LDV, * ),WORK( * ), WV( LDWV, * ), Z( LDZ, * )
  end subroutine AB_ZLAQR3
 end interface

 interface
  subroutine AB_ZLAQR4( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILOZ,&
 IHIZ, Z, LDZ, WORK, LWORK, INFO )
   implicit none
   integer :: IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, LWORK, N
   logical :: WANTT, WANTZ
   complex*16 :: H( LDH, * ), W( * ), WORK( * ), Z( LDZ, * )
  end subroutine AB_ZLAQR4
 end interface

 interface
  subroutine AB_ZLAQR5( WANTT, WANTZ, KACC22, N, KTOP, KBOT, NSHFTS, S,&
 H, LDH, ILOZ, IHIZ, Z, LDZ, V, LDV, U, LDU, NV,&
 WV, LDWV, NH, WH, LDWH )
   implicit none
   integer :: IHIZ, ILOZ, KACC22, KBOT, KTOP, LDH, LDU, LDV
   integer :: LDWH, LDWV, LDZ, N, NH, NSHFTS, NV
   logical :: WANTT, WANTZ
   complex*16 :: H( LDH, * ), S( * ), U( LDU, * ), V( LDV, * )
   complex*16 :: WH( LDWH, * ), WV( LDWV, * ), Z( LDZ, * )
  end subroutine AB_ZLAQR5
 end interface

 interface
  subroutine AB_ZLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
   implicit none
   integer :: INCV
   integer :: LDC
   integer :: M
   integer :: N
   complex*16 :: C( LDC, * )
   character :: SIDE
   complex*16 :: TAU
   complex*16 :: V( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZLARF
 end interface

 interface
  subroutine AB_ZLARFb( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,&  
 T, LDT, C, LDC, WORK, LDWORK )
   implicit none
   integer :: K
   integer :: LDC
   integer :: LDT
   integer :: LDV
   integer :: LDWORK
   integer :: M
   integer :: N
   complex*16 :: C( LDC, * )
   character :: DIRECT
   character :: SIDE
   character :: STOREV
   complex*16 :: T( LDT, * )
   character :: TRANS
   complex*16 :: V( LDV, * )
   complex*16 :: WORK( LDWORK, * )
  end subroutine AB_ZLARFb
 end interface

 interface
  subroutine AB_ZLARFg( N, ALPHA, X, INCX, TAU )
   implicit none
   integer :: INCX
   integer :: N
   complex*16 :: ALPHA
   complex*16 :: TAU
   complex*16 :: X( * )
  end subroutine AB_ZLARFg
 end interface

 interface
  subroutine AB_ZLARFt( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
   implicit none
   integer :: K
   integer :: LDT
   integer :: LDV
   integer :: N
   character :: DIRECT
   character :: STOREV
   complex*16 :: T( LDT, * )
   complex*16 :: TAU( * )
   complex*16 :: V( LDV, * )
  end subroutine AB_ZLARFt
 end interface

 interface
  subroutine AB_ZLARFx( SIDE, M, N, V, TAU, C, LDC, WORK )
   implicit none
   integer :: LDC
   integer :: M
   integer :: N
   complex*16 :: C( LDC, * )
   character :: SIDE
   complex*16 :: TAU
   complex*16 :: V( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZLARFx
 end interface

 interface
  subroutine AB_ZLARTG( F, G, CS, SN, R )
   implicit none
   double precision :: CS
   complex*16 :: F
   complex*16 :: G
   complex*16 :: R
   complex*16 :: SN
  end subroutine AB_ZLARTG
 end interface

 interface
  subroutine AB_ZLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
   implicit none
   integer :: INFO
   integer :: KL
   integer :: KU
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   double precision :: CFROM
   double precision :: CTO
   character :: TYPE
  end subroutine AB_ZLASCL
 end interface

 interface
  subroutine AB_ZLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
   implicit none
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: BETA
   character :: UPLO
  end subroutine AB_ZLASET
 end interface

 interface
  subroutine AB_ZLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
   implicit none
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   double precision :: C( * )
   character :: DIRECT
   character :: PIVOT
   double precision :: S( * )
   character :: SIDE
  end subroutine AB_ZLASR
 end interface

 interface
  subroutine AB_ZLASSQ( N, X, INCX, SCALE, SUMSQ )
   implicit none
   integer :: INCX
   integer :: N
   double precision :: SCALE
   double precision :: SUMSQ
   complex*16 :: X( * )
  end subroutine AB_ZLASSQ
 end interface

 interface
  subroutine AB_ZLASWP( N, A, LDA, K1, K2, IPIV, INCX )
   implicit none
   integer :: INCX
   integer :: IPIV( * )
   integer :: K1
   integer :: K2
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
  end subroutine AB_ZLASWP
 end interface

 interface
  subroutine AB_ZLATRD( UPLO, N, NB, A, LDA, E, TAU, W, LDW )
   implicit none
   integer :: LDA
   integer :: LDW
   integer :: N
   integer :: NB
   complex*16 :: A( LDA, * )
   double precision :: E( * )
   complex*16 :: TAU( * )
   character :: UPLO
   complex*16 :: W( LDW, * )
  end subroutine AB_ZLATRD
 end interface

 interface
  subroutine AB_ZLATRS( UPLO, TRANS, DIAG, NORMIN, N, A, LDA, X, SCALE,&
 CNORM, INFO )
   implicit none
   character :: DIAG, NORMIN, TRANS, UPLO
   integer :: INFO, LDA, N
   double precision :: SCALE      
   double precision :: CNORM( * )
   complex*16 :: A( LDA, * ), X( * )
  end subroutine AB_ZLATRS
 end interface

 interface
  subroutine zlazro( M, N, ALPHA, BETA, A, LDA )
   implicit none
   integer :: LDA
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: ALPHA
   complex*16 :: BETA
  end subroutine zlazro
 end interface

 interface
  subroutine AB_ZPOTF2( UPLO, N, A, LDA, INFO )
   implicit none
   integer :: INFO
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
   character :: UPLO
  end subroutine AB_ZPOTF2
 end interface

 !interface
 ! subroutine AB_ZPOTRF( UPLO, N, A, LDA, INFO )
 !  implicit none
 !  integer :: INFO
 !  integer :: LDA
 !  integer :: N
 !  complex*16 :: A( LDA, * )
 !  character :: UPLO
 ! end subroutine AB_ZPOTRF
 !end interface

 interface
  subroutine AB_ZPPTRF( UPLO, N, AP, INFO )
   implicit none
   integer :: INFO
   integer :: N
   complex*16 :: AP( * )
   character :: UPLO
  end subroutine AB_ZPPTRF
 end interface

 interface
  subroutine AB_ZROT( N, CX, INCX, CY, INCY, C, S )
   implicit none
   integer :: INCX
   integer :: INCY
   integer :: N
   double precision :: C
   complex*16 :: CX( * )
   complex*16 :: CY( * )
   complex*16 :: S
  end subroutine AB_ZROT
 end interface

 interface
  subroutine AB_ZTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,&
                     LDVR, MM, M, WORK, RWORK, INFO )
   implicit none
   character :: HOWMNY, SIDE
   integer :: INFO, LDT, LDVL, LDVR, M, MM, N
   logical :: SELECT( * )
   double precision :: RWORK( * )
   complex*16 :: T( LDT, * ), VL( LDVL, * ), VR( LDVR, * )
   complex*16 :: WORK( * )
  end subroutine AB_ZTREVC
 end interface

 interface
  subroutine AB_ZTREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, INFO )
   implicit none
   integer :: IFST
   integer :: ILST
   integer :: INFO
   integer :: LDQ
   integer :: LDT
   integer :: N
   character :: COMPQ
   complex*16 :: Q( LDQ, * )
   complex*16 :: T( LDT, * )
  end subroutine AB_ZTREXC
 end interface

 interface
  subroutine AB_ZTRSEN( JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, W, M, S,&  
 SEP, WORK, LWORK, INFO )
   implicit none
   integer :: INFO
   integer :: LDQ
   integer :: LDT
   integer :: LWORK
   integer :: M
   integer :: N
   character :: COMPQ
   character :: JOB
   complex*16 :: Q( LDQ, * )
   double precision :: S
   logical :: SELECT( * )
   double precision :: SEP
   complex*16 :: T( LDT, * )
   complex*16 :: W( * )
   complex*16 :: WORK( * )
  end subroutine AB_ZTRSEN
 end interface

 interface
  subroutine AB_ZTRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,&  
 LDC, SCALE, INFO )
   implicit none
   integer :: INFO
   integer :: ISGN
   integer :: LDA
   integer :: LDB
   integer :: LDC
   integer :: M
   integer :: N
   complex*16 :: A( LDA, * )
   complex*16 :: B( LDB, * )
   complex*16 :: C( LDC, * )
   double precision :: SCALE
   character :: TRANA
   character :: TRANB
  end subroutine AB_ZTRSYL
 end interface

 interface
  subroutine AB_ZTRTI2( UPLO, DIAG, N, A, LDA, INFO )
   implicit none
   character :: DIAG
   character :: UPLO
   integer :: INFO 
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
  end subroutine AB_ZTRTI2
 end interface

 interface
  subroutine AB_ZTRTRI( UPLO, DIAG, N, A, LDA, INFO )
   implicit none
   character :: DIAG
   character :: UPLO
   integer :: INFO
   integer :: LDA
   integer :: N
   complex*16 :: A( LDA, * )
  end subroutine AB_ZTRTRI
 end interface

 interface
  subroutine AB_ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
   implicit none
   integer :: INFO, K, LDA, LWORK, M, N
   complex*16 :: A( LDA, * ), TAU( * ), WORK( * )
  end subroutine AB_ZUNGQR
 end interface

end module m_linalg_interfaces
!!***
