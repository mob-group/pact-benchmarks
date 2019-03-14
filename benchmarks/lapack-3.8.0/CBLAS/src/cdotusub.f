c     AB_AB_CDOTUsub.f
c
c     The program is a fortran wrapper for AB_CDOTU.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_CDOTUsub(n,x,incx,y,incy,dotu)
c
      external AB_CDOTU
      complex AB_CDOTU,dotu
      integer n,incx,incy
      complex x(*),y(*)
c
      dotu=AB_CDOTU(n,x,incx,y,incy)
      return
      end
