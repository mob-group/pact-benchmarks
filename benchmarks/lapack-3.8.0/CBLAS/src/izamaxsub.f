c     AB_AB_IZAMAXsub.f
c
c     The program is a fortran wrapper for AB_IZAMAX.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_IZAMAXsub(n,x,incx,iamax)
c
      external AB_IZAMAX
      integer  AB_IZAMAX,iamax
      integer n,incx
      double complex x(*)
c
      iamax=AB_IZAMAX(n,x,incx)
      return
      end
