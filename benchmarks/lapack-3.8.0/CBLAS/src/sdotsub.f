c     AB_AB_SDOTsub.f
c
c     The program is a fortran wrapper for AB_SDOT.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_SDOTsub(n,x,incx,y,incy,dot)
c
      external AB_SDOT
      real AB_SDOT
      integer n,incx,incy
      real x(*),y(*),dot
c
      dot=AB_SDOT(n,x,incx,y,incy)
      return
      end
