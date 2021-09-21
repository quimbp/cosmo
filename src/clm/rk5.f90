subroutine spherical_rk5 (n,xo,to,h,xn,uf,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a fifth-order Runge-Kutta
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.
! ... Quim Ballabrera, March 2017.

use module_types, only: dp
use module_model

implicit none

integer, intent(in)                  :: n
real(dp), intent(in)                 :: to,h
real(dp), dimension(n), intent(in)   :: xo
real(dp), dimension(n), intent(out)  :: xn
real(dp), dimension(n), intent(out)  :: uf
EXTERNAL                             :: RHS

! ... Local variables
! ...
real(dp), parameter                  :: RNstdev = sqrt(12.0D0)

real(dp) hh,qh,th
real(dp), dimension(n)               :: u1,u2,u3,u4,u5,u6
real(dp), dimension(n)               :: x1,x2,x3,x4,x5
real(dp), dimension(1)               :: noise1
real(dp), dimension(n)               :: noise2

hh = 0.50_dp*h
qh = 0.25_dp*h
th = 0.75_dp*h

CALL RHS(n,to,xo,u1)

call displacement(n,xo,0.25_dp*u1,h,x1)
CALL RHS(n,to+qh,x1,u2)

call displacement(n,xo,(u1+u2)/8.0_dp,h,x2)
CALL RHS(n,to+qh,x2,u3)

call displacement(n,xo,-0.50_dp*u2+u3,h,x3)
CALL RHS(n,to+hh,x3,u4)

call displacement(n,xo,(3.0_dp*u1+9.0_dp*u4)/16.0_dp,h,x4)
CALL RHS(n,to+th,x4,u5)

call displacement(n,xo,(-3.0_dp*u1+2.0_dp*u2+12.0_dp*u3-12.0_dp*u4+8.0_dp*u5)/7.0_dp,h,x5)
CALL RHS(n,to+h,x5,u6)

uf(:) = (7.0_dp*u1(:) + 32.0_dp*u3(:) + 12.0_dp*u4(:) + 32.0_dp*u5(:) + 7.0_dp*u6(:))/90.0_dp

! ... Diffusion terms
! ...
if (noise_mu.gt.0.0_dp) then
  if (Gaussian_noise) then
    noise1(:) = randn(1)
  else
    call random_number(noise1)
    noise1(:) = (noise1(:)-0.5D0)*RNStdev    ! Uniform noise
  endif
  uf(:) = uf(:) + noise_mu*noise1(1)*uf(:)
endif

if (noise_K0.gt.0.0_dp) then
  if (Gaussian_noise) then
    noise2(:) = randn(n)                                  ! Gaussian noise
  else
    call random_number(noise2)                            ! Uniform noise
    noise2(:) = (noise2(:)-0.5D0)*RNStdev
  endif
  uf(:) = uf(:) + noise_K0*noise2(:) 
endif

! ... Move the particle from xo to xn:
! ...
call displacement(n,xo,uf,h,xn)


return
end subroutine spherical_rk5
! ...
! =======================================================================================
! ...
subroutine displacement(n,xo,u,dt,xn)

use module_types, only: dp
use module_constants, only: Rearth

implicit none

real(dp), parameter        :: Iearth = 1.0D0/Rearth        ! 1/Earth_Radius

integer, intent(in)                  :: n
real(dp), dimension(n), intent(in)   :: xo                 ! Input in radians
real(dp), dimension(n), intent(in)   :: u
real(dp), intent(in)                 :: dt
real(dp), dimension(n), intent(out)  :: xn                 ! Output in radians

real(dp) dx,dy,rxo,ryo,coslat,ryn,rdx

dx  = dt*u(1)
dy  = dt*u(2)

rxo  = xo(1)
ryo  = xo(2)
coslat = cos(ryo)

ryn = asin(sin(ryo+dy*IEarth)*cos(dx*IEarth))
rdx = atan2(sin(dx*Iearth)*coslat,cos(dx*Iearth)-sin(ryo)*sin(ryn))
xn(1)  = (rxo+rdx)
xn(2)  = ryn

end subroutine displacement
