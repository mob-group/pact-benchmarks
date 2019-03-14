c     AB_AB_ISAMAXsub.f
c
c     The program is a fortran wrapper for AB_ISAMAX.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_ISAMAXsub(n,x,incx,iamax)
c
      external AB_ISAMAX
      integer  AB_ISAMAX,iamax
      integer n,incx
      real x(*)
c
      iamax=AB_ISAMAX(n,x,incx)
      return
      end
