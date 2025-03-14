!{\src2tex{textfont=tt}}
!!****f* m_abi_linalg/abi_xpotrf
!! NAME
!!  abi_xpotrf
!!
!! FUNCTION
!!  abi_xpotrf is the generic function for computing the
!!  Cholesky factorization of a real symmetric (or hermitian)
!!    positive definite matrix A.
!!    The factorization has the form
!!      A = U**T * U,  if UPLO = 'U', or
!!      A = L  * L**T,  if UPLO = 'L',
!!    where U is an upper triangular matrix and L is lower triangular.
!!
!! COPYRIGHT
!!  Copyright (C) 2001-2018 ABINIT group (LNguyen,FDahm (CS))
!!  This file is distributed under the terms of the
!!  GNU General Public License, see ~ABINIT/Infos/copyright
!!  or http://www.gnu.org/copyleft/gpl.txt .
!!
!! SOURCE

!!***

!!****f* m_abi_linalg/abi_dpotrf
!! NAME
!! abi_dpotrf
!!
!! FUNCTION
!!
!! INPUTS
!!
!! PARENTS
!!
!! SOURCE

subroutine abi_dpotrf(uplo,n,a,lda,info)

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'abi_dpotrf'
!End of the abilint section

 implicit none
 !Arguments ------------------------------------
 character(len=1), intent(in) :: uplo
 integer, intent(in) :: n,lda
 integer, intent(out) :: info
 real(dp), intent(inout) :: a(*)

! *********************************************************************

#ifdef HAVE_LINALG_PLASMA
 if (XPLASMA_ISON) then
   ! write(std_out,*) "  abi_dpotrf => PLASMA AB_DPOTRF will be called "
   call PLASMA_dpotrf(uplo_plasma(uplo),n,a,lda,info)
   return
 end if
#endif

 call AB_DPOTRF(uplo,n,a,lda,info)

end subroutine abi_dpotrf
!!***

!!****f* m_abi_linalg/abi_zpotrf_2d
!! NAME
!! abi_zpotrf_2d
!!
!! FUNCTION
!!
!! INPUTS
!!
!! PARENTS
!!
!! SOURCE

subroutine abi_zpotrf_2d(uplo,n,a,lda,info)

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'abi_zpotrf_2d'

!End of the abilint section

 implicit none

 !Arguments ------------------------------------
 character(len=1), intent(in) :: uplo
 integer, intent(in) :: lda,n
 integer, intent(out) :: info
 complex(dpc), intent(inout) :: a(lda,*)

! *********************************************************************

 call abi_zpotrf(uplo,n,a(1,1),lda,info)

end subroutine abi_zpotrf_2d
!!***

!!****f* m_abi_linalg/abi_d2AB_ZPOTRF
!! NAME
!! abi_d2AB_ZPOTRF
!!
!! FUNCTION
!!
!! INPUTS
!!
!! PARENTS
!!
!! SOURCE

subroutine abi_d2AB_ZPOTRF(uplo,n,a,lda,info,x_cplx)

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'abi_d2AB_ZPOTRF'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
 character(len=1), intent(in) :: uplo
 integer, intent(in) :: n,lda
 integer, intent(out) :: info
 integer, intent(in), optional :: x_cplx
 real(dp),target, intent(inout) :: a(lda,*)  ! FIXME should be x_cplx * lda

 !Local Variables -----------------------------
 integer  :: cplx_

! *********************************************************************

 cplx_=1 ; if(PRESENT(x_cplx)) cplx_ = x_cplx

#ifdef HAVE_LINALG_PLASMA
 if (XPLASMA_ISON) then
   if(cplx_ == 2) then
      info = PLASMA_zpotrf_c(uplo_plasma(uplo),n,c_loc(a),lda)
   else
      info = PLASMA_dpotrf_c(uplo_plasma(uplo),n,c_loc(a),lda)
   end if
   return
 end if
#endif

 if(cplx_ == 2) then
    call AB_ZPOTRF(uplo,n,a,lda,info)
 else
    call AB_DPOTRF(uplo,n,a,lda,info)
 end if

end subroutine abi_d2AB_ZPOTRF
!!***

!!****f* m_abi_linalg/abi_zpotrf
!! NAME
!! abi_zpotrf
!!
!! FUNCTION
!!
!! INPUTS
!!
!! PARENTS
!!
!! SOURCE

subroutine abi_zpotrf(uplo,n,a,lda,info)

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'abi_zpotrf'
!End of the abilint section

 implicit none
 !Arguments ------------------------------------
 character(len=1), intent(in) :: uplo
 integer, intent(in) :: lda,n
 integer, intent(out) :: info
 complex(dpc), intent(inout) :: a(*)

! *********************************************************************

#ifdef HAVE_LINALG_PLASMA
 if (XPLASMA_ISON) then
   ! write(*,*) "  abi_zpotrf => PLASMA AB_ZPOTRF will be called "
   call PLASMA_zpotrf(uplo_plasma(uplo),n,a,lda,info)
   return
 end if
#endif

 call AB_ZPOTRF(uplo,n,a,lda,info)

end subroutine abi_zpotrf
!!***
