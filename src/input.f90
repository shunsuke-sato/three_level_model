subroutine input
  use global_variables
  implicit none
  integer :: nt_t

  nstate = 1
  allocate(zpsi(3,nstate))
  zpsi(:,:) = 0d0
  zpsi(1,1) = 1d0

  E0_pump = 1d0
  omega_pump = eps3-eps2
  phi_CEP_pump = 0d0
  kick_probe = 1d-4

  dt = 0.1d0
  Tprop = 100d0
  
  nt_t = aint((2d0*pi/omega_pump)/dt)+1
  dt = (2d0*pi/omega_pump)/nt_t
  nt = aint(Tprop/dt) + 1


  ntime_probe = 0


end subroutine input
