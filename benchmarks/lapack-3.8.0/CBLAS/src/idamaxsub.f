c     AB_AB_ICAMAXsub.f
c
c     The program is a fortran wrapper for AB_IDAMAX.
c     Witten by Keita Teranishi.  2/22/1998
c
      subroutine AB_AB_IDAMAXsub(n,x,incx,iamax)
c
      external AB_IDAMAX
      integer  AB_IDAMAX,iamax
      integer n,incx
      double precision x(*)
c
      iamax=AB_IDAMAX(n,x,incx)
      return
      end
