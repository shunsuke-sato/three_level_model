subroutine init_laser
  use global_variables
  implicit none
  integer :: it
  real(8) :: tt

  allocate(E_pump(0:nt+1),E_pump_dt2(0:nt+1))
  allocate(dipole_t(0:nt+1))
  dipole_t = 0d0

  do it = 0,nt+1
    tt = dt*it
    E_pump(it) = E0_pump*cos(omega_pump*tt + phi_CEP_pump)

    tt = dt*it + 0.5d0*dt
    E_pump_dt2(it) = E0_pump*cos(omega_pump*tt + phi_CEP_pump)


  end do

end subroutine init_laser
