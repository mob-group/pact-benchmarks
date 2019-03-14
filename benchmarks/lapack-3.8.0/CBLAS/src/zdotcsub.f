c     AB_AB_ZDOTCsub.f
c
c     The program is a fortran wrapper for AB_ZDOTC.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_ZDOTCsub(n,x,incx,y,incy,dotc)
c
      external AB_ZDOTC
      double complex AB_ZDOTC,dotc
      integer n,incx,incy
      double complex x(*),y(*)
c
      dotc=AB_ZDOTC(n,x,incx,y,incy)
      return
      end
