! *****************************************************************************
! ... lorenz.f90
! ... The 1963 Lorenz model.
! ... COSMO project
! ... Quim Ballabrera, March 2017
! *****************************************************************************

module lorenz

use types, only: dp

implicit none

private
public lorenz_sigma,lorenz_beta,lorenz_rho
public lorenz_ini
public lorenz_rhs

real(DP)                         :: lorenz_sigma=10.0_dp
real(DP)                         :: lorenz_beta=8.0_dp/3.0_dp
real(DP)                         :: lorenz_rho=28.0_dp

real(DP), dimension(3) :: lorenz_ini = [1.508870_dp,-1.531271_dp,25.46091_dp]

contains
! ...
! =============================================================================
! ...
subroutine lorenz_rhs (t,x,dxdt)

REAL(dp), intent(in)                  :: t
REAL(dp), DIMENSION(:), intent(in)    :: x
REAL(dp), DIMENSION(:), intent(out)   :: dxdt

dxdt(1) = lorenz_sigma*(x(2)-x(1))
dxdt(2) = x(1)*(lorenz_rho-x(3)) - x(2)
dxdt(3) = x(1)*x(2) - lorenz_beta*x(3)

return
end subroutine lorenz_rhs

end module lorenz
