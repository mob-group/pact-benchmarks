c     AB_dAB_AB_SDOTsub.f
c
c     The program is a fortran wrapper for AB_DAB_SDOT.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_dAB_AB_SDOTsub(n,x,incx,y,incy,dot)
c
      external AB_DAB_SDOT
      double precision AB_DAB_SDOT,dot
      integer n,incx,incy
      real x(*),y(*)
c
      dot=AB_DAB_SDOT(n,x,incx,y,incy)
      return
      end
