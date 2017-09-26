subroutine RHS(n,t,x,dxdt)

use cosmo, only: dp
use lagrangian, only: nx,ny,dt,urhs,vrhs

implicit none

integer, intent(in)                        :: n
real(dp), intent(in)                       :: t
real(dp), dimension(n), intent(in)         :: x
real(dp), dimension(n), intent(out)        :: dxdt

! ... Local variables
! ... 
integer k

real(dp) hinterpol
external hinterpol

! ... Index of the step. The field is kept constant during
! ... the whole time step.
! ...
k = int(t/dt) + 1

! ... Interpolation at the float location
! ...
!print*, 'rhs', x(1), x(2)
dxdt(1) = hinterpol(urhs(:,:,k),x(1),x(2))
dxdt(2) = hinterpol(vrhs(:,:,k),x(1),x(2))

end subroutine RHS
! ...
! ==========================================================================
! ...
