      subroutine strainsym(nsym,rprimd0,rprimd,rprimd_symm,symrel)

      !This section has been created automatically by the script Abilint (TD).
      !Do not modify the following lines by hand.
      !End of the abilint section

      implicit none

      !Arguments ------------------------------------
      !scalars
      integer,intent(in) :: nsym
      !arrays
      integer,intent(in) :: symrel(3,3,nsym)
      real,intent(in) :: rprimd(3,3),rprimd0(3,3)
      real,intent(out) :: rprimd_symm(3,3)

      !Local variables-------------------------------
      !scalars
      integer :: isym
      !arrays
      integer :: symrel_it(3,3)
      real :: rprimd0_inv(3,3),strain(3,3)
      real :: strain_symm(3,3),tmp_mat(3,3)

      !**************************************************************************

      !copy initial rprimd input and construct inverse
      rprimd0_inv = rprimd0
      call matrginv(rprimd0_inv,3,3)

      !define strain as rprimd = strain * rprimd0 (in cartesian frame)
      !so strain = rprimd * rprimd0^{-1}
      !transform to triclinic frame with rprimd0^{-1} * strain * rprimd0
      !giving strain as rprimd0^{-1} * rprimd
      call dgemm('N','N',3,3,3,1.0d00,rprimd0_inv,
     >           3,rprimd,3,0.0d00,strain,3)
      !loop over symmetry elements to obtain symmetrized strain matrix
      strain_symm = 0.0d00
      do isym = 1, nsym

      !  this loop accumulates symrel^{-1}*strain*symrel into strain_symm

      !  mati3inv gives the inverse transpose of symrel
      call mati3inv(symrel(:,:,isym),symrel_it)
      call dgemm('N','N',3,3,3,1.0d00,strain,3,
     >           dble(symrel(:,:,isym)),3,0.0d00,tmp_mat,3)
      call dgemm('T','N',3,3,3,1.0d00,dble(symrel_it),
     >           3,tmp_mat,3,1.0d00,strain_symm,3)

      end do

      !normalize by number of symmetry operations
      strain_symm = strain_symm/dble(nsym)

      !this step is equivalent to r_new = r_old * strain * r_old^{-1} * r_old,
      !that is, convert strain back to cartesian frame and then multipy by r_old,
      !to get the r_new primitive vectors

      call dgemm('N','N',3,3,3,1.0d00,rprimd0,3,
     >           strain_symm,3,0.0d00,rprimd_symm,3)

      end subroutine strainsym
