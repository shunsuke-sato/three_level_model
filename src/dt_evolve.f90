subroutine dt_evolve(it)
  use global_variables
  implicit none
  integer,intent(in) :: it
  integer :: istate
  complex(8) :: zpsi_rk(3,1:4),zpsi_t(3)
  complex(8) :: zham(3,3)
  real(8) :: ham0(3,3)


  ham0 = 0d0
  ham0(1,1) = eps1
  ham0(2,2) = eps2
  ham0(3,3) = eps3

  do istate = 1, nstate

! RK1
    zham = ham0
    zham(2,3) = zham(2,3) + zdip23*E_pump(it)
    zham(3,2) = conjg(zham(2,3))
    zpsi_t(:) = zpsi(:,istate)
    zpsi_rk(:,1) = -zI*dt*matmul(zham,zpsi_t(:))

! RK2
    zham = ham0
    zham(2,3) = zham(2,3) + zdip23*E_pump_dt2(it)
    zham(3,2) = conjg(zham(2,3))

    zpsi_t(:) = zpsi(:,istate) + 0.5d0*zpsi_rk(:,1)
    zpsi_rk(:,2) = -zI*dt*matmul(zham,zpsi_t(:))

! RK3
    zpsi_t(:) = zpsi(:,istate) + 0.5d0*zpsi_rk(:,2)
    zpsi_rk(:,3) = -zI*dt*matmul(zham,zpsi_t(:))

! RK4
    zham = ham0
    zham(2,3) = zham(2,3) + zdip23*E_pump(it+1)
    zham(3,2) = conjg(zham(2,3))

    zpsi_t(:) = zpsi(:,istate) + zpsi_rk(:,3)
    zpsi_rk(:,4) = -zI*dt*matmul(zham,zpsi_t(:))

    zpsi(:,istate) = zpsi(:,istate) +1d0/6d0*(&
      zpsi_rk(:,1) + 2d0*zpsi_rk(:,2) + 2d0*zpsi_rk(:,3) + zpsi_rk(:,4))


  end do


end subroutine dt_evolve
