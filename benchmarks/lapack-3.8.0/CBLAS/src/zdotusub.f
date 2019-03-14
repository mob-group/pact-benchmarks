c     AB_AB_ZDOTUsub.f
c
c     The program is a fortran wrapper for AB_ZDOTU.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_ZDOTUsub(n,x,incx,y,incy,dotu)
c
      external AB_ZDOTU
      double complex AB_ZDOTU,dotu
      integer n,incx,incy
      double complex x(*),y(*)
c
      dotu=AB_ZDOTU(n,x,incx,y,incy)
      return
      end
