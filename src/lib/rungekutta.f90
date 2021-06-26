module module_rk

use module_types, only: dp

implicit none

contains
! ...
! =====================================================================
! =====================================================================
! ...
function rk4(y,x,h,derivs) result(yout)

! Given values for the variables y(1:n) and their derivatives dydx(1:n)
! known at x, use the fourth-order Runge-Kutta method to advance the
! solution over an interval h and return the incremented variable as
! yout(1:n), which neet not be a distinct array from y. The user supplies
! the subroutine derivs(x,y,dydx), which returns derivatives dydx at x.
! Numerical Recipes

real(dp), dimension(:), intent(in)  :: y
real(dp), intent(in)                :: x,h
real(dp), dimension(size(y))        :: yout
external derivs

! ... Local variables
! ...
real(dp) h6,hh,xh
real(dp), dimension(size(y))        :: dydx,dym,dyt,yt

hh = h*0.5d0
h6 = h/6.d0
xh = x + hh

CALL derivs (x,y,dydx)
yt(:) = y(:) + hh*dydx(:)          ! First step

CALL derivs(xh,yt,dyt)             ! Second step
yt(:) = y(:) + hh*dyt(:)

CALL derivs(xh,yt,dym)             ! Third step
yt(:)  = y(:)   + h*dym(:)
dym(:) = dyt(:) + dym(:)

CALL derivs(x+h,yt,dyt)            ! Fourth step

                                   ! Accumulate increments with proper weights
yout(:) = y(:) + h6*(dydx(:)+dyt(:)+2.d0*dym(:))

return
end function rk4
! ...
! =====================================================================
! ...
function rk5 (x,t,h,RHS) result(xn)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a fifth-order Runge-Kutta
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.
! ... Coefficients as in Butcher's Fifth Order
! ... Quim Ballabrera, March 2017.

real(dp), intent(in)                 :: t,h
real(dp), dimension(:), intent(in)   :: x
real(dp), dimension(size(x))         :: xn
external                             :: RHS

! ... Local variables
! ...
real(dp), dimension(size(x))         :: k1,k2,k3,k4,k5,k6

CALL RHS(t,x,k1)
k1 = h*k1

CALL RHS(t+0.25D0*h, x+0.25D0*k1, k2)
k2 = h*k2

CALL RHS(t+0.25D0*h, x+(k1+k2)/8.0D0, k3)
k3 = h*k3

CALL RHS(t+0.5D0*h, x-0.5D0*k2+k3, k4)
k4 = h*k4

CALL RHS(t+0.75D0*h, x+(3D0*k1+9D0*k4)/16.0D0, k5)
k5 = h*k5

CALL RHS(t+h, x+(-3.0D0*k1+2D0*k2+12.0D0*k3-12D0*k4+8D0*k5)/7.0D0, k6)
k6 = h*k6

xn(:) = x(:) + (7.0D0*k1(:) + 32.0D0*k3(:) + 12.0D0*k4(:) + &
                32.0D0*k5(:) + 7.0D0*k6(:))/90.0D0

return
end function rk5
! ...
! ====================================================================

end module module_rk
