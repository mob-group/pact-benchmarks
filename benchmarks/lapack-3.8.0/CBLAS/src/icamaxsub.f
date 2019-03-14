c     AB_AB_ICAMAXsub.f
c
c     The program is a fortran wrapper for AB_ICAMAX.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_ICAMAXsub(n,x,incx,iamax)
c
      external AB_ICAMAX
      integer  AB_ICAMAX,iamax
      integer n,incx
      complex x(*)
c
      iamax=AB_ICAMAX(n,x,incx)
      return
      end
