! ***************************************************************************
! ... fillcoast.f90
! ... Quim Ballabrera, August 2017
! ... COSMO Lagrangian model
! ... Sea variables are lineraly interpolated at the very first land point.
! ... Version 0.1, released October 2017
! ***************************************************************************

subroutine fillcoast (nx,ny,land,sea,f)

use cosmo, only: dp

implicit none

integer, intent(in)                        :: nx,ny
logical, dimension(nx,ny), intent(in)      :: land
logical, dimension(nx,ny), intent(in)      :: sea
real(dp), dimension(nx,ny), intent(inout)  :: f

integer i,j
real(dp) xsum,ysum

do j=1,ny
do i=1,nx
  if (land(i,j)) then
    xsum = 0.0d0
    ysum = 0.0d0
    if (i-1.gt.1) then
    if (sea(i-1,j)) then
      xsum = xsum + 2*f(i-1,j) - f(i-2,j)
      ysum = ysum + 1
    endif
    endif
    if (i+1.lt.nx) then
    if (sea(i+1,j)) then
      xsum = xsum + 2*f(i+1,j) - f(i+2,j)
      ysum = ysum + 1
    endif
    endif
    if (j-1.gt.1) then
    if (sea(i,j-1)) then
      xsum = xsum + 2*f(i,j-1) - f(i,j-2)
      ysum = ysum + 1
    endif
    endif
    if (j+1.lt.ny) then
    if (sea(i,j+1)) then
      xsum = xsum + 2*f(i,j+1) - f(i,j+2)
      ysum = ysum + 1
    endif
    endif
    if (ysum.gt.0) then
      f(i,j) = xsum / ysum
    endif
  endif
enddo
enddo


end subroutine fillcoast
