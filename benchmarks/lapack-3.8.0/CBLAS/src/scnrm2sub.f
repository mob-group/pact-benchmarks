c     AB_AB_SCNRM2sub.f
c
c     The program is a fortran wrapper for AB_SCNRM2.
c     Witten by Keita Teranishi.  2/11/1998
c
      subroutine AB_AB_SCNRM2sub(n,x,incx,nrm2)
c
      external AB_SCNRM2
      real AB_SCNRM2,nrm2
      integer n,incx
      complex x(*)
c
      nrm2=AB_SCNRM2(n,x,incx)
      return
      end
