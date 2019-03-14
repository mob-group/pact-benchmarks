!{\src2tex{textfont=tt}}
!!****m* ABINIT/m_hide_blas
!! NAME
!!  m_hide_blas
!!
!! FUNCTION
!! This module defines interfaces for overloading BLAS routines.
!! whose goal is twofold. On one hand, using generic interfaces renders
!! the code more readable, especially when the routine can be compiled with
!! different precision type (single-precision or double precision as done for example in the GW code)
!! On the other hand, the generic interfaces defined here introduce a programming
!! layer that can be exploited for interfacing non-standard libraries such as for
!! example CUBLAS routines for GPU computations.
!!
!! COPYRIGHT
!! Copyright (C) 1992-2018 ABINIT group (MG)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~abinit/COPYING
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~abinit/doc/developers/contributors.txt .
!!
!! NOTES
!!
!! The convention about names of interfaced routine is: x<name>,
!! where <name> is usually equal to the name of the standard routine
!! without the first character specifying the type and kind.
!! The full list of names is reported below.
!! BLAS procedures interfaced in this module are marked with an asterisk.
!! A complete list of possible overloaded interfaces is provided as guide for future additions.
!!
!! ================
!! ==== BLAS 1 ====
!! ================
!! FUNCTION AB_IDAMAX AB_ISAMAX AB_ICAMAX AB_IZAMAX  ---> XIAMAX(n,dx,incx)
!! * FUNCTION  AB_SNRM2  AB_DNRM2 AB_SCNRM2 dznmr2  ---> XNRM2(n,x,incx)
!! FUNCTION  AB_SASUM  AB_DASUM AB_SCASUM AB_DZASUM  ---> XASUM(n,x,incx)
!! * FUNCTION               AB_CDOTU  AB_ZDOTU ---> XDOTU(n,x,incx,y,incy)
!! * FUNCTION               AB_CDOTC  AB_ZDOTC ---> XDOTC(n,x,incx,y,incy)
!! FUNCTION  AB_SDOT   AB_DDOT                 ---> XDOT(n,x,incx,y,incy)
!! FUNCTION  AB_SDSDOT AB_SDOT                 ---> XAB_DSDOT(n,x,incx,y,incy)
!! SUBROUTINE AB_SAXPY AB_DAXPY AB_CAXPY  AB_ZAXPY   ---> XAXPY(n,ca,cx,incx,cy,incy)
!! * SUBROUTINE AB_SCOPY AB_DCOPY AB_CCOPY  AB_ZCOPY   ---> XCOPY(n,cx,incx,cy,incy)
!! SUBROUTINE AB_SROTg AB_DROTg AB_CROTg  AB_ZROTg   ---> XROTG(a,b,c,s)
!! SUBROUTINE AB_SROT  AB_DROT  AB_CSROT  AB_ZDROT   ---> XROT(n,x,incx,y,incy,c,s)
!! * SUBROUTINE AB_SSCAL AB_DSCAL AB_CSCAL  AB_ZSCAL
!!                        AB_CSSCAL AB_ZDSCAL  ---> XSCAL(n,a,x,incx)
!! SUBROUTINE AB_SSWAP AB_DSWAP AB_CSWAP  AB_ZSWAP   ---> XSWAP(n,x,incx,y,incy)
!!
!! ================
!! ==== BLAS 2 ====
!! ================
!! SUBROUTINE AB_SGBMV AB_DGBMV AB_CGBMV AB_ZGBMV    ---> XGBMV(trans,m,kl,ku,n,alpha,A,lda,X,incx,beta,Y,incy)
!! * SUBROUTINE AB_SGEMV AB_DGEMV AB_CGEMV AB_ZGEMV    ---> XGEMV(trans,m,n,alpha,A,lda,X,incx,beta,Y,incy)
!! * SUBROUTINE             AB_CGERC AB_ZGERC    ---> XGERC(m,n,alpha,x,incx,y,incy,A,lda)
!! SUBROUTINE             AB_CGERU AB_ZGERU    ---> XGERU(m,n,alpha,x,incx,y,incy,A,lda)
!! SUBROUTINE             AB_CHBMV AB_ZHBMV    ---> XHBMV(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
!! SUBROUTINE             AB_CHEMV AB_ZHEMV    ---> XHEMV(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
!! SUBROUTINE             AB_CHER  AB_ZHER     ---> XHER(uplo,n,alpha,x,incx,A,lda)
!! SUBROUTINE             AB_CHER2 AB_ZHER2    ---> XHER2(uplo,n,alpha,x,incx,y,incy,A,lda)
!! SUBROUTINE             AB_CHPR  AB_ZHPR     ---> XHPR(uplo,n,alpha,x,incx,AP)
!! SUBROUTINE             AB_CHPR2 AB_ZHPR2    ---> XHPR2(uplo,n,alpha,x,incx,y,incy,AP)
!! SUBROUTINE             AB_CHPMV AB_ZHPMV    ---> XHPMV(uplo,n,alpha,AP,X,incx,beta,Y,incy)
!! SUBROUTINE AB_STBMV AB_DTBMV AB_CTBMV AB_ZTBMV    ---> XTBMV(uplo,trans,diag,n,k,A,lda,X,incx)
!! SUBROUTINE AB_STPMV AB_DTPMV AB_CTPMV AB_ZTPMV    ---> XTPMV(uplo,trans,diag,n,AP,X,incx)
!! SUBROUTINE AB_STRMV AB_DTRMV AB_CTRMV AB_ZTRMV    ---> XTRMV(uplo,trans,diag,n,A,lda,X,incx)
!! SUBROUTINE AB_SSYMV AB_DSYMV                ---> XSYMV(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
!! SUBROUTINE AB_SSBMV AB_DSBMV                ---> XSBMV(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
!! SUBROUTINE AB_SSPMV AB_DSPMV                ---> XSPMV(uplo,n,alpha,AP,X,incx,beta,Y,incy)
!! SUBROUTINE AB_STBSV AB_DTBSV AB_CTBSV AB_ZTBSV    ---> XTBSV(uplo,trans,diag,n,k,A,lda,X,incx)
!! SUBROUTINE AB_STPSV AB_DTPSV AB_CTPSV AB_ZTPSV    ---> XTPSV(uplo,trans,diag,n,AP,X,incx)
!! SUBROUTINE AB_STRSV AB_DTRSV AB_CTRSV AB_ZTRSV    ---> XTRSV(uplo,trans,diag,n,A,lda,X,incx)
!! SUBROUTINE  AB_SGER  AB_DGER                ---> XGER(m,n,alpha,x,incx,y,incy,A,lda)
!! SUBROUTINE  AB_SSPR  AB_DSPR                ---> XSPR(uplo,n,alpha,x,incx,AP)
!! SUBROUTINE AB_SSPR2 AB_DSPR2                ---> XSPR2(uplo,n,alpha,x,incx,y,incy,AP)
!! SUBROUTINE  AB_SSYR  AB_DSYR                ---> XSYR(uplo,n,alpha,x,incx,A,lda)
!! SUBROUTINE AB_SSYR2 AB_DSYR2                ---> XSYR2(uplo,n,alpha,x,incx,y,incy,A,lda)
!!
!! ================
!! ==== BLAS 3 ====
!! ================
!! * SUBROUTINE AB_SGEMM AB_DGEMM AB_CGEMM AB_ZGEMM      ---> XGEMM(transA,transB,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
!! SUBROUTINE             AB_CHEMM AB_ZHEMM      ---> XHEMM(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
!! SUBROUTINE            AB_CHER2k AB_ZHER2k     ---> XHER2K(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
!! * SUBROUTINE             AB_CHERk AB_ZHERk      ---> XHERK(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
!! SUBROUTINE AB_SSYMM AB_DSYMM AB_CSYMM AB_ZSYMM      ---> XSYMM(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
!! SUBROUTINE AB_SSYR2k AB_DSYR2k AB_CSYR2k AB_ZSYR2k  ---> XSYR2K(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
!! SUBROUTINE AB_SSYRk AB_DSYRk AB_CSYRk AB_ZSYRk      ---> XSYRK(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
!! SUBROUTINE AB_STRMM AB_DTRMM AB_CTRMM AB_ZTRMM      ---> XTRMM(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
!! SUBROUTINE AB_STRSM AB_DTRSM AB_CTRSM AB_ZTRSM      ---> XTRSM(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
!!-------------------------------------------------------------------------------
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

#include "abi_common.h"

MODULE m_hide_blas

 use defs_basis
 use m_abicore
 use m_errors

 implicit none

 private

!BLAS1
 public :: xnrm2
 public :: xscal
 public :: xdotu
 public :: xdotc
 public :: xcopy

!BLAS2
 public :: xgemv
 public :: xgerc

!BLAS3
 public :: xgemm
 public :: xherk

! Helper functions
 public :: blas_cholesky_ortho     ! Cholesky orthogonalization.

 public :: sqmat_itranspose        ! In-place transposition of a square matrix.
 public :: sqmat_otranspose        ! out-of-place transposition of a square matrix.

 public :: sqmat_iconjgtrans       ! in-place conjugate transpose of a square matrix.
 public :: sqmat_oconjgtrans       ! out-of-place conjugate transpose of a square matrix.

!----------------------------------------------------------------------

interface xnrm2
  !
  function AB_SNRM2 ( n, x, incx )
    use defs_basis
    real(sp) ::  AB_SNRM2
    integer,intent(in) :: incx, n
    real(sp),intent(in) ::  x( * )
  end function AB_SNRM2
  !
  function AB_DNRM2 ( n, x, incx )
    use defs_basis
    real(dp) :: AB_DNRM2
    integer,intent(in) :: incx, n
    real(dp),intent(in) ::  x( * )
  end function AB_DNRM2
  !
  function AB_SCNRM2( n, x, incx )
    use defs_basis
    real(sp) :: AB_SCNRM2
    integer,intent(in) :: incx, n
    complex(spc),intent(in) :: x( * )
  end function AB_SCNRM2
  !
  function AB_DZNRM2( n, x, incx )
    use defs_basis
    real(dp) :: AB_DZNRM2
    integer,intent(in) :: incx, n
    complex(dpc),intent(in) :: x( * )
  end function AB_DZNRM2
  !
end interface xnrm2

!-------------------------------------------------------------------------------

interface xscal
  !
  subroutine AB_SSCAL(n,sa,sx,incx)
    use defs_basis
    implicit none
    integer :: incx
    integer :: n
    real(sp) :: sa
    real(sp) :: sx(*)
  end subroutine AB_SSCAL
  !
  subroutine  AB_DSCAL(n,da,dx,incx)
    use defs_basis
    implicit none
    integer :: incx
    integer :: n
    real(dp):: da
    real(dp):: dx(*)
  end subroutine AB_DSCAL
  !
  subroutine  AB_CSCAL(n,ca,cx,incx)
    use defs_basis
    implicit none
    integer :: incx
    integer :: n
    complex(spc) :: ca
    complex(spc) :: cx(*)
  end subroutine AB_CSCAL
  !
  subroutine  AB_ZSCAL(n,za,zx,incx)
    use defs_basis
    implicit none
    integer :: incx
    integer :: n
    complex(dpc) :: za
    complex(dpc) :: zx(*)
  end subroutine AB_ZSCAL
  !
  subroutine  AB_CSSCAL(n,sa,cx,incx)
    use defs_basis
    implicit none
    integer :: incx
    integer :: n
    real(sp) :: sa
    complex(spc) :: cx(*)
  end subroutine AB_CSSCAL
  !
  subroutine  AB_ZDSCAL(n,da,zx,incx)
    use defs_basis
    implicit none
    integer :: incx
    integer :: n
    real(dp) :: da
    complex(dpc) :: zx(*)
  end subroutine AB_ZDSCAL
  !
end interface xscal

!-------------------------------------------------------------------------------

interface xdotu
  !
#ifdef HAVE_LINALG_ZDOTU_BUG
  module procedure AB_CDOTU
  module procedure AB_ZDOTU
#else
  function AB_CDOTU(n,cx,incx,cy,incy)
    use defs_basis
    complex(spc) :: AB_CDOTU
    complex(spc),intent(in) :: cx(*),cy(*)
    integer,intent(in) :: incx,incy,n
  end function AB_CDOTU
  !
  function AB_ZDOTU(n,zx,incx,zy,incy)
    use defs_basis
    complex(dpc) :: AB_ZDOTU
    complex(dpc),intent(in) :: zx(*),zy(*)
    integer,intent(in) :: incx,incy,n
  end function AB_ZDOTU
#endif
  !
end interface xdotu

!-------------------------------------------------------------------------------


! AB_CDOTC, AB_CDOTU, AB_ZDOTC, and AB_ZDOTU are problematic if Mac OS X's Vec lib is used.
! See http://developer.apple.com/hardwaredrivers/ve/errata.html.
! If needed, we replace them with plain Fortran code.

interface xdotc
  !
#ifdef HAVE_LINALG_ZDOTC_BUG
   module procedure AB_CDOTC
   module procedure AB_ZDOTC
#else
  function AB_CDOTC(n,cx,incx,cy,incy)
    use defs_basis
    complex(spc) :: AB_CDOTC
    complex(spc),intent(in) :: cx(*),cy(*)
    integer,intent(in) :: incx,incy,n
  end function AB_CDOTC
  !
  function AB_ZDOTC(n,zx,incx,zy,incy)
    use defs_basis
    complex(dpc) :: AB_ZDOTC
    complex(dpc),intent(in) :: zx(*),zy(*)
    integer,intent(in) :: incx,incy,n
  end function AB_ZDOTC
#endif
  !
end interface xdotc

!-------------------------------------------------------------------------------

interface xcopy
   !module procedure ABI_xcopy
 !
 subroutine AB_SCOPY(n,sx,incx,sy,incy)
   use defs_basis
   implicit none
   integer,intent(in) :: incx
   integer,intent(in) :: incy
   integer,intent(in) :: n
   real(sp),intent(in) ::  sx(*)
   real(sp),intent(inout) :: sy(*)
 end subroutine AB_SCOPY
 !
 subroutine  AB_DCOPY(n,dx,incx,dy,incy)
   use defs_basis
   implicit none
   integer,intent(in) :: incx
   integer,intent(in) :: incy
   integer,intent(in) :: n
   real(dp),intent(in) :: dx(*)
   real(dp),intent(inout) :: dy(*)
 end subroutine AB_DCOPY
 !
 subroutine  AB_CCOPY(n,cx,incx,cy,incy)
   use defs_basis
   implicit none
   integer,intent(in) :: incx
   integer,intent(in) :: incy
   integer,intent(in) :: n
   complex(spc),intent(in) :: cx(*)
   complex(spc),intent(inout) :: cy(*)
 end subroutine AB_CCOPY
 !
 subroutine  AB_ZCOPY(n,cx,incx,cy,incy)
   use defs_basis
   implicit none
   integer,intent(in) :: incx
   integer,intent(in) :: incy
   integer,intent(in) :: n
   complex(dpc),intent(in) :: cx(*)
   complex(dpc),intent(inout) :: cy(*)
 end subroutine AB_ZCOPY
 !
end interface xcopy

!-------------------------------------------------------------------------------

interface xgemv
  !
  subroutine AB_SGEMV ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )
    use defs_basis
    real(sp),intent(in) :: alpha, beta
    integer,intent(in) :: incx, incy, lda, m, n
    character(len=1),intent(in) :: trans
    real(sp),intent(in) :: a( lda, * ), x( * )
    real(sp),intent(inout) :: y( * )
  end subroutine AB_SGEMV
  !
  subroutine AB_DGEMV ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )
    use defs_basis
    real(dp),intent(in) :: alpha, beta
    integer,intent(in) :: incx, incy, lda, m, n
    character(len=1),intent(in) :: trans
    real(dp),intent(in) :: a( lda, * ), x( * )
    real(dp),intent(inout) :: y( * )
  end subroutine AB_DGEMV
  !
  subroutine AB_CGEMV ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )
    use defs_basis
    complex(spc),intent(in) :: alpha, beta
    integer,intent(in) :: incx, incy, lda, m, n
    character(len=1),intent(in) :: trans
    complex(spc),intent(in) :: a( lda, * ), x( * )
    complex(spc),intent(inout) :: y( * )
  end subroutine AB_CGEMV
  !
  subroutine AB_ZGEMV ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )
    use defs_basis
    complex(dpc),intent(in) :: alpha, beta
    integer,intent(in) :: incx, incy, lda, m, n
    character(len=1),intent(in) :: trans
    complex(dpc),intent(in) :: a( lda, * ), x( * )
    complex(dpc),intent(inout) :: y( * )
  end subroutine AB_ZGEMV
  !
end interface xgemv

!-------------------------------------------------------------------------------

interface xgerc
  !
  subroutine AB_CGERC ( m, n, alpha, x, incx, y, incy, a, lda )
    use defs_basis
    complex(spc),intent(in) :: alpha
    integer,intent(in) :: incx, incy, lda, m, n
    complex(spc),intent(inout) ::  a( lda, * )
    complex(spc),intent(in) :: x( * ), y( * )
  end subroutine AB_CGERC
  !
  subroutine AB_ZGERC ( m, n, alpha, x, incx, y, incy, a, lda )
    use defs_basis
    complex(dpc),intent(in) :: alpha
    integer,intent(in) :: incx, incy, lda, m, n
    complex(dpc),intent(inout) :: a( lda, * )
    complex(dpc),intent(in) :: x( * ), y( * )
  end subroutine AB_ZGERC
  !
end interface xgerc

!-------------------------------------------------------------------------------

interface xgemm
  !
  subroutine AB_SGEMM ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )
    use defs_basis
    character(len=1),intent(in) :: transa, transb
    integer,intent(in) :: m, n, k, lda, ldb, ldc
    real(sp),intent(in) :: alpha, beta
    real(sp),intent(in) :: a( lda, * ), b( ldb, * )
    real(sp),intent(inout) :: c( ldc, * )
  end subroutine AB_SGEMM
  !
  subroutine AB_DGEMM ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )
    use defs_basis
    character(len=1),intent(in) :: transa, transb
    integer,intent(in) :: m, n, k, lda, ldb, ldc
    real(dp),intent(in) :: alpha, beta
    real(dp),intent(in) :: a( lda, * ), b( ldb, * )
    real(dp),intent(inout) :: c( ldc, * )
  end subroutine AB_DGEMM
  !
  subroutine AB_CGEMM ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )
    use defs_basis
    character(len=1),intent(in) :: transa, transb
    integer,intent(in) :: m, n, k, lda, ldb, ldc
    complex(spc),intent(in) :: alpha, beta
    complex(spc),intent(in) :: a( lda, * ), b( ldb, * )
    complex(spc),intent(inout) :: c( ldc, * )
  end subroutine AB_CGEMM
  !
  subroutine AB_ZGEMM ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc )
    use defs_basis
    character(len=1),intent(in) :: transa, transb
    integer,intent(in) :: m, n, k, lda, ldb, ldc
    complex(dpc),intent(in) :: alpha, beta
    complex(dpc),intent(in) :: a( lda, * ), b( ldb, * )
    complex(dpc),intent(inout) :: c( ldc, * )
  end subroutine AB_ZGEMM
  !
end interface xgemm

interface xherk
  !
  subroutine AB_CHERk( uplo, trans, n, k, alpha, a, lda, beta, c, ldc )
    use defs_basis
    character(len=1),intent(in) :: uplo
    character(len=1),intent(in) :: trans
    integer,intent(in) :: n,k,lda,ldc
    real(sp),intent(in) :: alpha
    complex(spc),intent(in) :: a( lda, * )
    real(sp),intent(in) :: beta
    complex(spc),intent(inout) :: c( ldc, * )
  end subroutine AB_CHERk
  !
  subroutine AB_ZHERk( uplo, trans, n, k, alpha, a, lda, beta, c, ldc )
    use defs_basis
    character(len=1),intent(in) :: uplo
    character(len=1),intent(in) :: trans
    integer,intent(in) :: n,k,lda,ldc
    real(dp),intent(in) :: alpha
    complex(dpc),intent(in) :: a( lda, * )
    real(dp),intent(in) :: beta
    complex(dpc),intent(inout) :: c( ldc, * )
  end subroutine AB_ZHERk
  !
end interface xherk

!-------------------------------------------------------------------------------

interface blas_cholesky_ortho
  module procedure blas_cholesky_ortho_spc
  module procedure blas_cholesky_ortho_dpc
end interface blas_cholesky_ortho

interface sqmat_itranspose
  module procedure sqmat_itranspose_sp
  module procedure sqmat_itranspose_dp
  module procedure sqmat_itranspose_spc
  module procedure sqmat_itranspose_dpc
end interface sqmat_itranspose

interface sqmat_otranspose
  module procedure sqmat_otranspose_sp
  module procedure sqmat_otranspose_dp
  module procedure sqmat_otranspose_spc
  module procedure sqmat_otranspose_dpc
end interface sqmat_otranspose

interface sqmat_iconjgtrans
  module procedure sqmat_iconjgtrans_spc
  module procedure sqmat_iconjgtrans_dpc
end interface sqmat_iconjgtrans

interface sqmat_oconjgtrans
  module procedure sqmat_oconjgtrans_spc
  module procedure sqmat_oconjgtrans_dpc
end interface sqmat_oconjgtrans

 real(sp),private,parameter ::  zero_sp = 0._sp
 real(sp),private,parameter ::  one_sp  = 1._sp

 real(dp),private,parameter ::  zero_dp = 0._dp
 real(dp),private,parameter ::  one_dp  = 1._dp

 complex(spc),private,parameter :: czero_spc = (0._sp,0._sp)
 complex(spc),private,parameter :: cone_spc  = (1._sp,0._sp)

 complex(dpc),private,parameter :: czero_dpc = (0._dp,0._dp)
 complex(dpc),private,parameter :: cone_dpc  = (1._dp,0._dp)

CONTAINS  !========================================================================================

! AB_CDOTC, AB_CDOTU, AB_ZDOTC, and AB_ZDOTU are problematic if Mac OS X's Vec lib is used.
! See http://developer.apple.com/hardwaredrivers/ve/errata.html.
! Here we replace them with plain Fortran code.

#ifdef HAVE_LINALG_ZDOTC_BUG
!#warning "Using internal replacement for AB_ZDOTC. External library cannot be used"
#include "replacements/AB_CDOTC.f"
#include "replacements/AB_ZDOTC.f"
#endif

#ifdef HAVE_LINALG_ZDOTU_BUG
!#warning "Using internal replacement for AB_ZDOTU. External library cannot be used"
#include "replacements/AB_CDOTU.f"
#include "replacements/AB_ZDOTU.f"
#endif

!----------------------------------------------------------------------

!!***

!!****f* m_hide_blas/blas_cholesky_ortho_spc
!! NAME
!!  blas_cholesky_ortho_spc
!!
!! FUNCTION
!!  Performs the Cholesky orthonormalization of the vectors stored in iomat.
!!
!! INPUTS
!!  vec_size=Size of each vector.
!!  nvec=Number of vectors in iomat
!!
!! OUTPUT
!!  cf_ovlp=Cholesky factorization of the overlap matrix. ovlp = U^H U with U upper triangle matrix returned in cf_ovlp
!!
!! SIDE EFFECTS
!!  iomat(vec_size,nvec)
!!    input: Input set of vectors.
!!    output: Orthonormalized set.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine blas_cholesky_ortho_spc(vec_size,nvec,iomat,cf_ovlp,use_gemm)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'blas_cholesky_ortho_spc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
 integer,intent(in) :: vec_size,nvec
 logical,optional,intent(in) :: use_gemm
 complex(spc),intent(inout) :: iomat(vec_size,nvec)
 complex(spc),intent(out) :: cf_ovlp(nvec,nvec)

!Local variables ------------------------------
!scalars
 integer :: ierr
 logical :: my_usegemm
 character(len=500) :: msg

! *************************************************************************

 ! 1) Calculate overlap_ij =  <phi_i|phi_j>
 ! TODO: use AB_DSYRk
 my_usegemm = .FALSE.; if (PRESENT(use_gemm)) my_usegemm = use_gemm

 if (my_usegemm) then
   call xgemm("Conjugate","Normal",nvec,nvec,vec_size,cone_spc,iomat,vec_size,iomat,vec_size,czero_spc,cf_ovlp,nvec)
 else
   call xherk("U","C", nvec, vec_size, one_sp, iomat, vec_size, zero_sp, cf_ovlp, nvec)
 end if
 !
 ! 2) Cholesky factorization: ovlp = U^H U with U upper triangle matrix.
 call AB_CPOTRF('U',nvec,cf_ovlp,nvec,ierr)
 if (ierr/=0)  then
   write(msg,'(a,i0)')' AB_ZPOTRF returned info= ',ierr
   MSG_ERROR(msg)
 end if
 !
 ! 3) Solve X U = io_mat. On exit iomat is orthonormalized.
 call AB_CTRSM('Right','Upper','Normal','Normal',vec_size,nvec,cone_spc,cf_ovlp,nvec,iomat,vec_size)

end subroutine blas_cholesky_ortho_spc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/blas_cholesky_ortho_dpc
!! NAME
!!  blas_cholesky_ortho_dpc
!!
!! FUNCTION
!!  Performs the Cholesky orthonormalization of the vectors stored in iomat.
!!
!! INPUTS
!!  vec_size=Size of each vector.
!!  nvec=Number of vectors in iomat
!!
!! OUTPUT
!!  cf_ovlp=Cholesky factorization of the overlap matrix. ovlp = U^H U with U upper triangle matrix returned in cf_ovlp
!!
!! SIDE EFFECTS
!!  iomat(vec_size,nvec)
!!    input: Input set of vectors.
!!    output: Orthonormalized set.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine blas_cholesky_ortho_dpc(vec_size,nvec,iomat,cf_ovlp,use_gemm)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'blas_cholesky_ortho_dpc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
 integer,intent(in) :: vec_size,nvec
 logical,optional,intent(in) :: use_gemm
 complex(dpc),intent(inout) :: iomat(vec_size,nvec)
 complex(dpc),intent(out) :: cf_ovlp(nvec,nvec)

!Local variables ------------------------------
!scalars
 integer :: ierr
 logical :: my_usegemm
 character(len=500) :: msg

! *************************************************************************

 ! 1) Calculate overlap_ij =  <phi_i|phi_j>
 my_usegemm = .FALSE.; if (PRESENT(use_gemm)) my_usegemm = use_gemm

 if (my_usegemm) then
   call xgemm("Conjugate","Normal",nvec,nvec,vec_size,cone_dpc,iomat,vec_size,iomat,vec_size,czero_dpc,cf_ovlp,nvec)
 else
   call xherk("U","C", nvec, vec_size, one_dp, iomat, vec_size, zero_dp, cf_ovlp, nvec)
 end if
 !
 ! 2) Cholesky factorization: ovlp = U^H U with U upper triangle matrix.
 call AB_ZPOTRF('U',nvec,cf_ovlp,nvec,ierr)
 if (ierr/=0)  then
   write(msg,'(a,i0)')' AB_ZPOTRF returned info= ',ierr
   MSG_ERROR(msg)
 end if
 !
 ! 3) Solve X U = io_mat. On exit io_mat is orthonormalized.
 call AB_ZTRSM('Right','Upper','Normal','Normal',vec_size,nvec,cone_dpc,cf_ovlp,nvec,iomat,vec_size)

end subroutine blas_cholesky_ortho_dpc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_itranspose_sp
!! NAME
!!  sqmat_itranspose_sp
!!
!! FUNCTION
!!  Compute alpha * mat^T in place. target: single precision real matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!
!! SIDE EFFECTS
!!   mat(n,n)=in output, it contains alpha * mat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_itranspose_sp(n,mat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_itranspose_sp'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 real(sp),optional,intent(in) :: alpha
!arrays
 real(sp),intent(inout) :: mat(n,n)

! *************************************************************************

#if defined HAVE_LINALG_MKL_IMATCOPY
  if (PRESENT(alpha)) then
    call mkl_simatcopy("Column", "Trans", n, n, alpha, mat, n, n)
  else
    call mkl_simatcopy("Column", "Trans", n, n, one_sp, mat, n, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    mat = alpha * TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    mat = TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_itranspose_sp
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_itranspose_dp
!! NAME
!!  sqmat_itranspose_dp
!!
!! FUNCTION
!!  Compute alpha * mat^T in place. target: double precision real matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!
!! SIDE EFFECTS
!!   mat(n,n)=in output, it contains alpha * mat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_itranspose_dp(n,mat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_itranspose_dp'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 real(dp),optional,intent(in) :: alpha
!arrays
 real(dp),intent(inout) :: mat(n,n)

! *************************************************************************

#if defined HAVE_LINALG_MKL_IMATCOPY
  if (PRESENT(alpha)) then
    call mkl_dimatcopy("Column", "Trans", n, n, alpha, mat, n, n)
  else
    call mkl_dimatcopy("Column", "Trans", n, n, one_dp, mat, n, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    mat = alpha * TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    mat = TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_itranspose_dp
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_itranspose_spc
!! NAME
!!  sqmat_itranspose_spc
!!
!! FUNCTION
!!  Compute alpha * mat^T in place. target: single precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!
!! SIDE EFFECTS
!!   mat(n,n)=in output, it contains alpha * mat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_itranspose_spc(n,mat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_itranspose_spc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(spc),optional,intent(in) :: alpha
!arrays
 complex(spc),intent(inout) :: mat(n,n)

! *************************************************************************

#if defined HAVE_LINALG_MKL_IMATCOPY
  if (PRESENT(alpha)) then
    call mkl_cimatcopy("Column", "Trans", n, n, alpha, mat, n, n)
  else
    call mkl_cimatcopy("Column", "Trans", n, n, cone_spc, mat, n, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    mat = alpha * TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    mat = TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_itranspose_spc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_itranspose_dpc
!! NAME
!!  sqmat_itranspose_dpc
!!
!! FUNCTION
!!  Compute alpha * mat^T in place. target: double precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!
!! SIDE EFFECTS
!!   mat(n,n)=in output, it contains alpha * mat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_itranspose_dpc(n,mat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_itranspose_dpc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(dpc),optional,intent(in) :: alpha
!arrays
 complex(dpc),intent(inout) :: mat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_IMATCOPY
  if (PRESENT(alpha)) then
    call mkl_zimatcopy("Column", "Trans", n, n, alpha, mat, n, n)
  else
    call mkl_zimatcopy("Column", "Trans", n, n, cone_dpc, mat, n, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    mat = alpha * TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    mat = TRANSPOSE(mat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_itranspose_dpc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_otranspose_sp
!! NAME
!!  sqmat_otranspose_sp
!!
!! FUNCTION
!!  Compute alpha * mat^T out-of-place. target: single precision real matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!  imat(n,n)=Input matrix.
!!
!! OUTPUT
!!  omat(n,n)=contains alpha * imat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_otranspose_sp(n,imat,omat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_otranspose_sp'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 real(sp),optional,intent(in) :: alpha
!arrays
 real(sp),intent(in) :: imat(n,n)
 real(sp),intent(out) :: omat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_OMATCOPY
  if (PRESENT(alpha)) then
    call mkl_somatcopy("Column", "Transpose", n, n, alpha, imat, n, omat, n)
  else
    call mkl_somatcopy("Column", "Transpose", n, n, one_sp, imat, n, omat, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    omat = alpha * TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    omat = TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_otranspose_sp
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_otranspose_dp
!! NAME
!!  sqmat_otranspose_dp
!!
!! FUNCTION
!!  Compute alpha * mat^T out-of-place. target: double precision real matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!  imat(n,n)=Input matrix.
!!
!! OUTPUT
!!  omat(n,n)=contains alpha * imat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_otranspose_dp(n,imat,omat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_otranspose_dp'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 real(dp),optional,intent(in) :: alpha
!arrays
 real(dp),intent(in) :: imat(n,n)
 real(dp),intent(out) :: omat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_OMATCOPY
  if (PRESENT(alpha)) then
    call mkl_domatcopy("Column", "Transpose", n, n, alpha, imat, n, omat, n)
  else
    call mkl_domatcopy("Column", "Transpose", n, n, one_dp, imat, n, omat, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    omat = alpha * TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    omat = TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_otranspose_dp
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_otranspose_spc
!! NAME
!!  sqmat_otranspose_spc
!!
!! FUNCTION
!!  Compute alpha * mat^T out-of-place. target: single precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!  imat(n,n)=Input matrix.
!!
!! OUTPUT
!!  omat(n,n)=contains alpha * imat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_otranspose_spc(n,imat,omat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_otranspose_spc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(spc),optional,intent(in) :: alpha
!arrays
 complex(spc),intent(in) :: imat(n,n)
 complex(spc),intent(out) :: omat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_OMATCOPY
  if (PRESENT(alpha)) then
    call mkl_comatcopy("Column", "Transpose", n, n, alpha, imat, n, omat, n)
  else
    call mkl_comatcopy("Column", "Transpose", n, n, cone_spc, imat, n, omat, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    omat = alpha * TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    omat = TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_otranspose_spc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_otranspose_dpc
!! NAME
!!  sqmat_otranspose_dpc
!!
!! FUNCTION
!!  Compute alpha * mat^T out-of-place. target: double precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!  imat(n,n)=Input matrix.
!!
!! OUTPUT
!!  omat(n,n)=contains alpha * imat^T.
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_otranspose_dpc(n,imat,omat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_otranspose_dpc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(dpc),optional,intent(in) :: alpha
!arrays
 complex(dpc),intent(in) :: imat(n,n)
 complex(dpc),intent(out) :: omat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_OMATCOPY
  if (PRESENT(alpha)) then
    call mkl_zomatcopy("Column", "Transpose", n, n, alpha, imat, n, omat, n)
  else
    call mkl_zomatcopy("Column", "Transpose", n, n, cone_dpc, imat, n, omat, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    omat = alpha * TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    omat = TRANSPOSE(imat)
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_otranspose_dpc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_iconjgtrans_spc
!! NAME
!!  sqmat_iconjgtrans_spc
!!
!! FUNCTION
!!  Compute alpha * CONJG(mat^T) in place. target: single precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!
!! SIDE EFFECTS
!!   mat(n,n)=in output, it contains alpha * CONJG(mat^T).
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_iconjgtrans_spc(n,mat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_iconjgtrans_spc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(spc),optional,intent(in) :: alpha
!arrays
 complex(spc),intent(inout) :: mat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_IMATCOPY
  if (PRESENT(alpha)) then
    call mkl_cimatcopy("Column", "C", n, n, alpha, mat, n, n)
  else
    call mkl_cimatcopy("Column", "C", n, n, cone_spc, mat, n, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    mat = alpha * TRANSPOSE(CONJG(mat))
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    mat = TRANSPOSE(CONJG(mat))
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_iconjgtrans_spc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_iconjgtrans_dpc
!! NAME
!!  sqmat_iconjgtrans_dpc
!!
!! FUNCTION
!!  Compute alpha * CONJG(mat^T) in place. target: double precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!
!! SIDE EFFECTS
!!   mat(n,n)=in output, it contains alpha * CONJG(mat^T).
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_iconjgtrans_dpc(n,mat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_iconjgtrans_dpc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(dpc),optional,intent(in) :: alpha
!arrays
 complex(dpc),intent(inout) :: mat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_IMATCOPY
  if (PRESENT(alpha)) then
    call mkl_zimatcopy("Column", "C", n, n, alpha, mat, n, n)
  else
    call mkl_zimatcopy("Column", "C", n, n, cone_dpc, mat, n, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    mat = alpha * TRANSPOSE(CONJG(mat))
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    mat = TRANSPOSE(CONJG(mat))
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_iconjgtrans_dpc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_oconjgtrans_spc
!! NAME
!!  sqmat_oconjgtrans_spc
!!
!! FUNCTION
!!  Compute alpha * CONJG(mat^T) out-of-place. target: single precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!  imat(n,n)=Input matrix.
!!
!! OUTPUT
!!  omat(n,n)=contains alpha * CONJG(imat^T).
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_oconjgtrans_spc(n,imat,omat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_oconjgtrans_spc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(spc),optional,intent(in) :: alpha
!arrays
 complex(spc),intent(in) :: imat(n,n)
 complex(spc),intent(out) :: omat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_OMATCOPY
  if (PRESENT(alpha)) then
    call mkl_comatcopy("Column", "C", n, n, alpha, imat, n, omat, n)
  else
    call mkl_comatcopy("Column", "C", n, n, cone_spc, imat, n, omat, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    omat = alpha * TRANSPOSE(CONJG(imat))
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    omat = TRANSPOSE(CONJG(imat))
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_oconjgtrans_spc
!!***

!----------------------------------------------------------------------

!!****f* m_hide_blas/sqmat_oconjgtrans_dpc
!! NAME
!!  sqmat_oconjgtrans_dpc
!!
!! FUNCTION
!!  Compute alpha * CONJG(mat^T) out-of-place. target: double precision complex matrix.
!!
!! INPUTS
!!  n=size of the matrix
!!  [alpha]=scalar, set to 1.0 if not present
!!  imat(n,n)=Input matrix.
!!
!! OUTPUT
!!  omat(n,n)=contains alpha * CONJG(imat^T).
!!
!! PARENTS
!!
!! CHILDREN
!!      mkl_zomatcopy
!!
!! SOURCE

subroutine sqmat_oconjgtrans_dpc(n,imat,omat,alpha)


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'sqmat_oconjgtrans_dpc'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: n
 complex(dpc),optional,intent(in) :: alpha
!arrays
 complex(dpc),intent(in) :: imat(n,n)
 complex(dpc),intent(out) :: omat(n,n)

! *************************************************************************

#if defined  HAVE_LINALG_MKL_OMATCOPY
  if (PRESENT(alpha)) then
    call mkl_zomatcopy("Column", "C", n, n, alpha, imat, n, omat, n)
  else
    call mkl_zomatcopy("Column", "C", n, n, cone_dpc, imat, n, omat, n)
  end if
#else
  ! Fallback to Fortran.
  if (PRESENT(alpha)) then
!$OMP PARALLEL WORKSHARE
    omat = alpha * TRANSPOSE(CONJG(imat))
!$OMP END PARALLEL WORKSHARE
  else
!$OMP PARALLEL WORKSHARE
    omat = TRANSPOSE(CONJG(imat))
!$OMP END PARALLEL WORKSHARE
  end if
#endif

end subroutine sqmat_oconjgtrans_dpc
!!***

!----------------------------------------------------------------------

END MODULE m_hide_blas
!!***
