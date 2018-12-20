module global_variables
  implicit none

! mathematical constants
  complex(8),parameter :: zI = (0d0, 1d0)
  real(8),parameter :: pi = 4d0*atan(1d0)

! material parameters
  real(8),parameter :: eps1 = 0d0    ! ground
  real(8),parameter :: eps2 = 1d0    ! bright
  real(8),parameter :: eps3 = 1.1d0  ! dark

  complex(8),parameter :: zdip12 = 1d0
  complex(8),parameter :: zdip13 = 0d0
  complex(8),parameter :: zdip23 = 1d0

! wavefunction
  integer :: nstate
  complex(8),allocatable :: zpsi(:,:)

! lalser
  real(8) :: omega_pump, omega_probe
  real(8) :: E0_pump, E0_probe
  real(8) :: phi_CEP_pump, phi_CEP_probe
  real(8) :: kick_probe
  integer :: ntime_probe
  real(8),allocatable :: E_pump(:),E_pump_dt2(:)
  real(8),allocatable :: dipole_t(:)

! propagation
  real(8) :: Tprop, dt
  integer :: nt


end module global_variables
