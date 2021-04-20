module module_rk

use module_types, only: dp

implicit none

contains

! ...
! =====================================================================
! =====================================================================
! ...
subroutine rk5 (n,x,t,h,xn,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a fifth-order Runge-Kutta
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.
! ... Coefficients as in Butcher's Fifth Order
! ... Quim Ballabrera, March 2017.

integer, intent(in)                  :: n
real(dp), intent(in)                 :: t,h
real(dp), dimension(n), intent(in)   :: x
real(dp), dimension(n), intent(out)  :: xn
EXTERNAL                             :: RHS

! ... Local variables
! ...
real(dp), dimension(n)               :: k1,k2,k3,k4,k5,k6

CALL RHS(n,t,x,k1)
k1 = h*k1

CALL RHS(n, t+0.25D0*h, x+0.25D0*k1, k2)
k2 = h*k2

CALL RHS(n, t+0.25D0*h, x+(k1+k2)/8.0D0, k3)
k3 = h*k3

CALL RHS(n, t+0.5D0*h, x-0.5D0*k2+k3, k4)
k4 = h*k4

CALL RHS(n, t+0.75D0*h, x+(3D0*k1+9D0*k4)/16.0D0, k5)
k5 = h*k5

CALL RHS(n, t+h, x+(-3.0D0*k1+2D0*k2+12.0D0*k3-12D0*k4+8D0*k5)/7.0D0, k6)
k6 = h*k6

xn(:) = x(:) + (7.0D0*k1(:) + 32.0D0*k3(:) + 12.0D0*k4(:) + &
                32.0D0*k5(:) + 7.0D0*k6(:))/90.0D0

return
end subroutine rk5
! ...
! ====================================================================

end module module_rk
