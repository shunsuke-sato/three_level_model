subroutine calc_dipole(dipole_out)
  use global_variables
  implicit none
  real(8),intent(out) :: dipole_out
  complex(8) :: zDip(3,3)
  complex(8) :: zpsi_t(3)
  integer :: istate

  zDip = 0d0
  zDip(1,2) = zdip12
  zDip(2,1) = conjg(zdip12)

  dipole_out = 0d0
  do istate = 1,nstate
    zpsi_t = matmul(zDip,zpsi(:,istate))
    dipole_out = sum(conjg(zpsi(:,istate))*zpsi_t)
  end do


end subroutine calc_dipole

