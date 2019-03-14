c     AB_AB_DZNRM2sub.f
c
c     The program is a fortran wrapper for AB_DZNRM2.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_DZNRM2sub(n,x,incx,nrm2)
c
      external AB_DZNRM2
      double precision AB_DZNRM2,nrm2
      integer n,incx
      double complex x(*)
c
      nrm2=AB_DZNRM2(n,x,incx)
      return
      end
