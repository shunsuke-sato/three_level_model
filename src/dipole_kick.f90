subroutine dipole_kick
  use global_variables
  implicit none
  complex(8) :: zDip(3,3), kick_mat(3,3)
  complex(8) :: zpsi_t(3)
  integer :: istate
  integer :: iexp
  integer,parameter :: nexp = 16
  complex(8) :: zfact
  
  zDip = 0d0
  zDip(1,2) = zdip12
  zDip(2,1) = conjg(zdip12)

  kick_mat = 0d0
  kick_mat(1,1) = 1d0; kick_mat(2,2) = 1d0; kick_mat(3,3) = 1d0 


  do istate = 1, nstate
    zfact = 1d0
    zpsi_t(:) = zpsi(:,istate)

    do iexp = 1,nexp
      zfact = zfact*(-zI*kick_probe)/iexp
      zpsi_t = matmul(zDip,zpsi_t)
      zpsi(:,istate) = zpsi(:,istate) + zfact*zpsi_t
    end do

  end do
  

end subroutine dipole_kick
