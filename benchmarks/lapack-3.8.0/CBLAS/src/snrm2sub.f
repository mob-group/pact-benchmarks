c     AB_AB_SNRM2sub.f
c
c     The program is a fortran wrapper for AB_SNRM2.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_SNRM2sub(n,x,incx,nrm2)
c
      external AB_SNRM2
      real AB_SNRM2,nrm2
      integer n,incx
      real x(*)
c
      nrm2=AB_SNRM2(n,x,incx)
      return
      end
