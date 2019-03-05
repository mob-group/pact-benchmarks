      subroutine neb_resize_path(rtdb,bead_list,nbeads1,nbeads2)
      implicit none
      integer rtdb
      character*(*) bead_list
      integer nbeads1
      integer nbeads2

*     **** local variables ****
      logical value
      integer i,geom,geomlen,movecslen,nion
      integer j1,j2,shift
      integer r1(2),r2(2),r3(2),c(2)
      real*8  t,t1,t2,t3
      character*255 geom_name,movecs_name

      integer    MT_DBL
      parameter  (MT_DBL = 1013)

      logical MA_pop_stack
      logical MA_push_get

      external MA_pop_stack
      external MA_push_get

      common /mbc_dbl/  dbl_mb(2)
      double precision  dbl_mb

*     **** external functions ****
      integer     inp_strlen
      character*7 bead_index_name
      external    inp_strlen
      external    bead_index_name

      value = value.and.MA_push_get(mt_dbl,(3*nion*nbeads1),
     >                              'c',c(2),c(1))
      value = value.and.MA_push_get(mt_dbl,(3*nion),'r1',r1(2),r1(1))
      value = value.and.MA_push_get(mt_dbl,(3*nion),'r2',r2(2),r2(1))
      value = value.and.MA_push_get(mt_dbl,(3*nion),'r3',r3(2),r3(1))
      if (.not.value) call errquit('neb_resize_path failed',1,0)

      do i=1,nbeads1
         shift = (i-1)*3*nion
         call coords_get_bead_list(bead_list,i,dbl_mb(c(1)+shift))
      end do
      call reset_bead_list(bead_list)

      do i=1,nbeads2
        t = (i-1)/dble(nbeads2-1)

        j1 = t*(nbeads1-1) + 1
        j2 = j1+1
        t1 = (j1-1)/dble(nbeads1-1)
        t2 = (j2-1)/dble(nbeads1-1)
        t3 = (t-t1)/(t2-t1)

        if (j2.gt.nbeads1) then
           t3 = 0.0d0
           j2=nbeads1
        end if

        shift = (j1-1)*3*nion
        call dcopy(3*nion,dbl_mb(c(1)+shift),1,dbl_mb(r1(1)),1)

        shift = (j2-1)*3*nion
        call dcopy(3*nion,dbl_mb(c(1)+shift),1,dbl_mb(r2(1)),1)

        call dcopy(3*nion,dbl_mb(r1(1)),1,dbl_mb(r3(1)),1)
        call dscal(3*nion,(1.0d0-t3),dbl_mb(r3(1)),1)
        call daxpy(3*nion,t3,dbl_mb(r2(1)),1,dbl_mb(r3(1)),1)
      end do

      value = value.and.MA_pop_stack(r3(2))
      value = value.and.MA_pop_stack(r2(2))
      value = value.and.MA_pop_stack(r1(2))
      value = value.and.MA_pop_stack(c(2))
      if (.not.value) call errquit('neb_new_path failed',3,0)
      return
      end
