c     AB_AB_DDOTsub.f
c
c     The program is a fortran wrapper for AB_DDOT.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_DDOTsub(n,x,incx,y,incy,dot)
c
      external AB_DDOT
      double precision AB_DDOT
      integer n,incx,incy
      double precision x(*),y(*),dot
c
      dot=AB_DDOT(n,x,incx,y,incy)
      return
      end
