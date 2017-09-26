! ****************************************************************************
! ... rk.f90
! ... Runge-Kutta routines
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ****************************************************************************

module runge_kutta

use types, only: dp

implicit none
private
public rk1,rk2,rk3,rk4,rk5

contains
! ...
! =============================================================================
! ...
subroutine rk1 (n,x,t,h,xn,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a first-order Runge-Kutta (Euler) method 
! ... is used to advance the solution over an interval h and return the 
! ... incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.

integer, intent(in)                  :: n
real(dp), intent(in)                 :: t,h
real(dp), dimension(n), intent(in)   :: x
real(dp), dimension(n), intent(out)  :: xn
EXTERNAL                             :: RHS

! ... Local variables
! ...
real(dp), dimension(n)               :: k1

CALL RHS(n,t,x,k1)
k1 = h*k1

xn(:) = x(:) + k1(:)

return
end subroutine rk1
! ...
! =============================================================================
! ...
subroutine rk2 (n,x,t,h,xn,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a second-order Runge-Kutta (midpoint) 
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.

integer, intent(in)                  :: n
real(dp), intent(in)                 :: t,h
real(dp), dimension(n), intent(in)   :: x
real(dp), dimension(n), intent(out)  :: xn
EXTERNAL                             :: RHS

! ... Local variables
! ...
real(dp), dimension(n)               :: k1,k2

CALL RHS(n,t,x,k1)
k1 = h*k1

CALL RHS(n,t+0.5D0*h,x+0.5D0*k1,k2)
k2 = h*k2

xn(:) = x(:) + k2(:)

return
end subroutine rk2
! ...
! =============================================================================
! ...
subroutine rk3 (n,x,t,h,xn,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a second-order Runge-Kutta (midpoint) 
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.

integer, intent(in)                  :: n
real(dp), intent(in)                 :: t,h
real(dp), dimension(n), intent(in)   :: x
real(dp), dimension(n), intent(out)  :: xn
EXTERNAL                             :: RHS

! ... Local variables
! ...
real(dp), dimension(n)               :: k1,k2,k3

CALL RHS(n,t,x,k1)
k1 = h*k1

CALL RHS(n,t+0.5D0*h,x+0.5D0*k1,k2)
k2 = h*k2

CALL RHS(n,t+h,x-k1+2D0*k2,k3)
k3 = h*k3

xn(:) = x(:) + (k1(:) + 4D0*k2(:) + k3(:))/6D0

return
end subroutine rk3
! ...
! =============================================================================
! ...
subroutine rk4 (n,x,t,h,xn,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a fourth-order Runge-Kutta 
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.

integer, intent(in)                  :: n
real(dp), intent(in)                 :: t,h
real(dp), dimension(n), intent(in)   :: x
real(dp), dimension(n), intent(out)  :: xn
EXTERNAL                             :: RHS

! ... Local variables
! ...
real(dp), dimension(n)               :: k1,k2,k3,k4

CALL RHS(n,t,x,k1)
k1 = h*k1

CALL RHS(n,t+0.5D0*h,x+0.5D0*k1,k2)
k2 = h*k2

CALL RHS(n,t+0.5D0*h,x+0.5D0*k2,k3)
k3 = h*k3

CALL RHS(n,t+h,x+k3,k4)
k4 = h*k4

xn(:) = x(:) + (k1(:) + 2D0*k2(:) + 2D0*K3(:) + k4(:))/6D0

return
end subroutine rk4
! ...
! =============================================================================
! ...
subroutine rk5 (n,x,t,h,xn,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a fifth-order Runge-Kutta 
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.
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

CALL RHS(n,t+0.5D0*h,x+0.5D0*k1,k2)
k2 = h*k2

CALL RHS(n,t+0.25D0*h,x+(3D0*k1+k2)/16D0,k3)
k3 = h*k3

CALL RHS(n,t+0.5D0*h,x+0.5D0*k3,k4)
k4 = h*k4

CALL RHS(n,t+0.75D0*h,x+(-3D0*k2+6D0*k3+9D0*k4)/16D0,k5)
k5 = h*k5

CALL RHS(n,t+h,x+(k1+4D0*k2+6D0*k3-12D0*k4+8D0*k5)/7D0,k6)
k6 = h*k6

xn(:) = x(:) + (7D0*k1(:) + 32D0*k3(:) + 12D0*k4(:) + 32D0*k5(:) + 7D0*k6(:))/90D0

return
end subroutine rk5
! ...
! =============================================================================
! ...
end module runge_kutta
