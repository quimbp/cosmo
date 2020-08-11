! *********************************************************************
! ... rhs.f90
! *********************************************************************

module mrhs

use grids
use mod_floats

type(floater)                           :: FLT
type(cdf_vgrid)                         :: UCDF
type(cdf_vgrid)                         :: VCDF
type(cdf_vgrid)                         :: WCDF
type(cdf_tgrid)                         :: TCDF

! ... Those fields represent the values at any given time
! ... they can be constant or come from a cubic interpolation
! ...
real(dp), dimension(:,:,:,:), pointer   :: urhs,vrhs,wrhs,trhs,srhs,rrhs
real(dp), dimension(:,:,:), pointer     :: hrhs

real(dp)                                :: internal_time
real(dp)                                :: internal_dt

integer                                 :: nx,ny


end module mrhs

! ...
! ==========================================================================
! ...
subroutine RHS(n,t,x,dxdt)

use cosmo
use mrhs
implicit none

integer, intent(in)                        :: n
real(dp), intent(in)                       :: t
real(dp), dimension(n), intent(in)         :: x
real(dp), dimension(n), intent(out)        :: dxdt

! ... Local variables
! ...
integer kk

real(dp) hinterpol
external hinterpol


kk = nint(4.0_dp*(t-internal_time)/internal_dt) + 1

! ... Interpolation at the float location
! ... Hardcoded: first layer !!!!

dxdt(1) = hinterpol(nx,ny,UCDF%xm(:),UCDF%ym(:),urhs(:,:,1,kk),x(1),x(2))
dxdt(2) = hinterpol(nx,ny,VCDF%xm(:),VCDF%ym(:),vrhs(:,:,1,kk),x(1),x(2))

end subroutine RHS

