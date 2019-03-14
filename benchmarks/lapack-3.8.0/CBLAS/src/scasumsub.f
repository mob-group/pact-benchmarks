c     AB_AB_SCASUMsub.f
c
c     The program is a fortran wrapper for AB_SCASUM.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_SCASUMsub(n,x,incx,asum)
c
      external AB_SCASUM
      real AB_SCASUM,asum
      integer n,incx
      complex x(*)
c
      asum=AB_SCASUM(n,x,incx)
      return
      end
