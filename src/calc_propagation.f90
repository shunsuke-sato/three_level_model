subroutine calc_propagation
  use global_variables
  implicit none
  integer :: it
  real(8) :: tt


  call dipole_kick
  call calc_dipole(dipole_t(0))

  do it = 0, nt

    call dt_evolve(it)
    call calc_dipole(dipole_t(it+1))

  end do


end subroutine calc_propagation
