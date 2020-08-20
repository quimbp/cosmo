! ****************************************************************************
! ... hinterpol.f90
! ... Quim Ballabrera, April 2017
! ... COSMO Lagrangian model
! ... Two-dimensional interpolation of a given field using Akima Splines.
! ... Version 0.1, released October 2017
! ... Version 2.0, released August 2020
! ...              change Akima for bilinear interpolation
! ****************************************************************************

function  hinterpol(nx,ny,xm,ym,f2d,xo,yo) result(fo)

!use cosmo, only: dp, akima
use cosmo

implicit none

integer, intent(in)                                 :: nx,ny
real(dp), dimension(nx), intent(in)                 :: xm             ! Longitudes in radians
real(dp), dimension(ny), intent(in)                 :: ym             ! Latitudes in radians
real(dp), dimension(nx,ny), intent(in)              :: f2d
real(dp), intent(in)                                :: xo,yo
real(dp)                                            :: fo

! ... Local variables
! ...
integer                       :: i,j
!real(dp), dimension(nx)       :: wrk
real(dp)                      :: t,u,f1,f2,f3,f4,rxo,ryo

!do i=1,nx
!  wrk(i) = akima(ym(:),f2d(i,:),yo)
!enddo
!fo = akima(xm(:),wrk(:),xo)

! ... We assume that the longitues and latitudes are expressed in radians
! ...
!rxo = deg2rad*xo
!ryo = deg2rad*yo
rxo = xo
ryo = yo

i = locate(xm,rxo)
j = locate(ym,ryo)

!print*, 'xm: ', xm
!print*, 'rxo: ', rxo
!print*, 'i: ', i
!print*, 'xm(i): ', xm(i)

t   = (rxo-xm(i))/(xm(i+1)-xm(i))
u   = (ryo-ym(j))/(ym(j+1)-ym(j))
f1  = f2d(i,j)
f2  = f2d(i+1,j)
f3  = f2d(i+1,j+1)
f4  = f2d(i,j+1)

fo  = (1-t)*(1-u)*f1 + t*(1-u)*f2 + t*u*f3 + (1-t)*u*f4
!print*, 'In hinterpol: ', f1, f2, f3, f4, ' -> ', fo



end function hinterpol

