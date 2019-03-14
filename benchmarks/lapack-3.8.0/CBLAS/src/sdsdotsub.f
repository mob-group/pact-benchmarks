c     sAB_dAB_AB_SDOTsub.f
c
c     The program is a fortran wrapper for sAB_DAB_SDOT.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine sAB_dAB_AB_SDOTsub(n,sb,x,incx,y,incy,dot)
c
      external sAB_DAB_SDOT
      real sb,sAB_DAB_SDOT,dot
      integer n,incx,incy
      real x(*),y(*)
c
      dot=sAB_DAB_SDOT(n,sb,x,incx,y,incy)
      return
      end
