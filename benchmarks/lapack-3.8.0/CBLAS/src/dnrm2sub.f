c     AB_AB_DNRM2sub.f
c
c     The program is a fortran wrapper for AB_DNRM2.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_DNRM2sub(n,x,incx,nrm2)
c
      external AB_DNRM2
      double precision AB_DNRM2,nrm2
      integer n,incx
      double precision x(*)
c
      nrm2=AB_DNRM2(n,x,incx)
      return
      end
