c     AB_AB_SASUMsub.f
c
c     The program is a fortran wrapper for AB_SASUM.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_SASUMsub(n,x,incx,asum)
c
      external AB_SASUM
      real AB_SASUM,asum
      integer n,incx
      real x(*)
c
      asum=AB_SASUM(n,x,incx)
      return
      end
