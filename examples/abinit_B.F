      subroutine sym2cart(gprimd,nsym,rprimd,symrel,symcart)


      !This section has been created automatically by the script Abilint (TD).
      !Do not modify the following lines by hand.
      !End of the abilint section

      implicit none

      !Arguments -----------------------------------------------
      ! in
      ! out
      !scalars
      integer,intent(in) :: nsym
      !arrays
      integer,intent(in) :: symrel(3,3,nsym)
      real,intent(in) :: gprimd(3,3),rprimd(3,3)
      real,intent(out) :: symcart(3,3,nsym)

      !Local variables-------------------------------
      !scalars
      integer :: isym
      !arrays
      real :: rsym(3,3),rsymcart(3,3),tmp(3,3)

      ! *************************************************************************

      do isym=1,nsym
      rsym(:,:) = dble(symrel(:,:,isym))
      !  write(std_out,*) 'rsym = ',rsym
      call dgemm('N','N',3,3,3,1.0d00,rprimd,3,
     >           rsym,  3,0.0d00,tmp,     3)
      call dgemm('N','N',3,3,3,1.0d00,tmp,   3,
     >           gprimd,3,0.0d00,rsymcart,3)
      !  write(std_out,*) 'rsymcart = ',rsymcart
      symcart(:,:,isym) = rsymcart(:,:)
      ! purify symops in cartesian dp coordinates
      where( abs(symcart(:,:,isym))<0.00000000000001d00)
      symcart(:,:,isym) = 0.0d00
      end where
      end do

      end subroutine sym2cart
