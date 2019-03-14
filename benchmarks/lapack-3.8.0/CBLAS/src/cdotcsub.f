c     AB_AB_CDOTCsub.f
c
c     The program is a fortran wrapper for AB_CDOTC.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_CDOTCsub(n,x,incx,y,incy,dotc)
c
      external AB_CDOTC
      complex AB_CDOTC,dotc
      integer n,incx,incy
      complex x(*),y(*)
c
      dotc=AB_CDOTC(n,x,incx,y,incy)
      return
      end
