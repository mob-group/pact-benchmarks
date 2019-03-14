c     AB_DASUMsun.f
c
c     The program is a fortran wrapper for AB_DASUM..
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_DASUMsub(n,x,incx,asum)
c
      external AB_DASUM
      double precision AB_DASUM,asum
      integer n,incx
      double precision x(*)
c
      asum=AB_DASUM(n,x,incx)
      return
      end
