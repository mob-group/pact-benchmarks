c     AB_AB_DZASUMsub.f
c
c     The program is a fortran wrapper for AB_DZASUM.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_DZASUMsub(n,x,incx,asum)
c
      external AB_DZASUM
      double precision AB_DZASUM,asum
      integer n,incx
      double complex x(*)
c
      asum=AB_DZASUM(n,x,incx)
      return
      end
