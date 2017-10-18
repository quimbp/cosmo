! ****************************************************************************
! ... hinterpol.f90
! ... Quim Ballabrera, April 2017
! ... COSMO Lagrangian model
! ... Two-dimensional interpolation of a given field using Akima Splines.
! ... Version 0.1, released October 2017
! ****************************************************************************

function  hinterpol(nx,ny,xm,ym,f2d,xo,yo) result(fo)

use cosmo, only: dp, akima

implicit none

integer, intent(in)                                 :: nx,ny
real(dp), dimension(nx), intent(in)                 :: xm
real(dp), dimension(ny), intent(in)                 :: ym
real(dp), dimension(nx,ny), intent(in)              :: f2d
real(dp), intent(in)                                :: xo,yo
real(dp)                                            :: fo

! ... Local variables
! ...
integer                       :: i
real(dp), dimension(nx)       :: wrk

do i=1,nx
  wrk(i) = akima(ym(:),f2d(i,:),yo)
enddo
fo = akima(xm(:),wrk(:),xo)

end function hinterpol

