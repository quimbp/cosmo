subroutine spherical_rk5 (n,xo,to,h,xn,RHS)

! ... Given values for the variables x(1:n) and a function, RHS, returning
! ... its derivatives dxdt(1:n), a fifth-order Runge-Kutta
! ... method is used to advance the solution over an interval h and return
! ... the incremented variable as xn(1:n).
! ... The user must supply the subroutine RHS(t,x,dxdt) returning
! ... derivatives dxdt at t.
! ... Quim Ballabrera, March 2017.

use cosmo

implicit none

integer, intent(in)                  :: n
real(dp), intent(in)                 :: to,h
real(dp), dimension(n), intent(in)   :: xo
real(dp), dimension(n), intent(out)  :: xn
EXTERNAL                             :: RHS

! ... Common variables (come from main):
! ...
real(dp) velocity_factor,noise_ampl,noise_frac
common/noise/velocity_factor,noise_ampl,noise_frac

! ... Local variables
! ...
real(dp) hh,hq
real(dp), dimension(n)               :: u1,u2,u3,u4,u5,u6,uf
real(dp), dimension(n)               :: x1,x2,x3,x4,x5
real(dp), dimension(n)               :: Gnoise1
real(dp), dimension(n)               :: Gnoise2

hh = half*h
hq = quarter*h

CALL RHS(n,to,xo,u1)

call displacement(n,xo,half*u1,h,x1)
CALL RHS(n,to+hh,x1,u2)

call displacement(n,xo,(three*u1+u2)/16d0,h,x2)
CALL RHS(n,to+hq,x2,u3)

call displacement(n,xo,half*u3,h,x3)
CALL RHS(n,to+hh,x3,u4)

call displacement(n,xo,(-3D0*u2+6D0*u3+9D0*u4)/16D0,h,x4)
CALL RHS(n,to+0.75D0*h,x4,u5)

call displacement(n,xo,(u1+4D0*u2+6D0*u3-12D0*u4+8D0*u5)/7D0,h,x5)
CALL RHS(n,to+h,x5,u6)

!print*, 'Im rk5, xo: ', xo
!print*, 'Im rk5, x1: ', x1
!print*, 'Im rk5, x2: ', x2
!print*, 'Im rk5, x3: ', x3
!print*, 'Im rk5, x4: ', x4
!print*, 'Im rk5, x5: ', x5
!print*, 'Im rk5, u1: ', u1
!print*, 'Im rk5, u2: ', u2
!print*, 'Im rk5, u3: ', u3
!print*, 'Im rk5, u4: ', u4
!print*, 'Im rk5, u5: ', u5
!print*, 'Im rk5, u6: ', u6

uf(:) = (7D0*u1(:) + 32D0*u3(:) + 12D0*u4(:) + 32D0*u5(:) + 7D0*u6(:))/90D0

! ... Add non-resolved processes:
! ...
if (noise_frac .gt. 1E-10) then
  Gnoise1 = randn(n)
else
  Gnoise1(:) = zero
endif

if (noise_ampl .gt. 1E-10) then
  Gnoise2 = randn(n)
else
  Gnoise2(:) = zero
endif

uf(:) = (velocity_factor + noise_frac*Gnoise1(:))*uf(:) + noise_ampl*Gnoise2(:)


! ... This would be the place to place the random term:

!print*, 'Im rk5, uf: ', uf
!print*, 'Im rk5, h: ', h

!xn(:) = xo(:) + h*uf(:)/Rearth
!print*, 'Im rk5, xn 1: ', xn

call displacement(n,xo,uf,h,xn)

return
end subroutine spherical_rk5
! ...
! =======================================================================================
! ...
subroutine displacement(n,xo,u,dt,xn)

use cosmo

implicit none

real(kind=8), parameter    :: Iearth = 1.0D0/Rearth        ! 1/Earth_Radius

integer, intent(in)                  :: n
real(dp), dimension(n), intent(in)   :: xo                 ! Input in radians
real(dp), dimension(n), intent(in)   :: u
real(dp), intent(in)                 :: dt
real(dp), dimension(n), intent(out)  :: xn                 ! Output in radians

real(dp) dx,dy,rxo,ryo,coslat,ryn,rdx

dx  = dt*u(1)
dy  = dt*u(2)

!rxo  = deg2rad * xo(1)
!ryo  = deg2rad * xo(2)
rxo  = xo(1)
ryo  = xo(2)
coslat = cos(ryo)

ryn = asin(sin(ryo+dy*IEarth)*cos(dx*IEarth))
rdx = atan2(sin(dx*Iearth)*coslat,cos(dx*Iearth)-sin(ryo)*sin(ryn))
!xn(1)  = (rxo+rdx)*rad2deg
!xn(2)  = ryn*rad2deg
xn(1)  = (rxo+rdx)
xn(2)  = ryn

end subroutine displacement
