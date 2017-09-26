! *****************************************************************************
! ... grids.f90
! ... Routines to deal with 3D-grids
! ... COSMO project
! ... Quim Ballabrera, September 2017
! *****************************************************************************

module grids

use types, only: sp,dp
use constants, only: zero,half
use utils, only: stop_error

implicit none

type grid
  integer                          :: nx   = 1
  integer                          :: ny   = 1
  integer                          :: nz   = 1
  real(dp)                         :: xmin = zero
  real(dp)                         :: xmax = zero
  real(dp)                         :: ymin = zero
  real(dp)                         :: ymax = zero
  real(dp)                         :: zmin = zero
  real(dp)                         :: zmax = zero
  real(dp), dimension(:), pointer  :: x,y,z
  real(dp), dimension(:), pointer  :: xl,yl,zl
  real(dp), dimension(:), pointer  :: xu,yu,zu
end type grid

contains

subroutine gridAllocate (nx,ny,nz,gridT)

integer, intent(in)             :: nx,ny,nz
type(grid), intent(out)         :: gridT

gridT%nx = nx
gridT%ny = ny
gridT%nz = nz

! ... X dimension:
! ...
allocate(gridT%x(nx))
allocate(gridT%xl(nx))
allocate(gridT%xu(nx))

! ... Y dimension:
! ...
allocate(gridT%y(ny))
allocate(gridT%yl(ny))
allocate(gridT%yu(ny))

! ... Z dimension:
! ...
allocate(gridT%z(nz))
allocate(gridT%zl(nz))
allocate(gridT%zu(nz))

return
end subroutine gridAllocate
! ...
! =====================================================================
! ...
subroutine CgridBounds (gridT,gridU,gridV)

! ... Calculates the bounds of an Arakawa C-grid. The grid index is
! ... supposed as follows
! ...
! ...             v(i,j)
! ...                |
! ...             t(i,j)  -- u(i,j)

type(grid), intent(inout)       :: gridT,gridU,gridV

! ... Local variables:
! ...
integer i,j

if (gridT%nx.ne.gridU%nx) call stop_error(1,'Incompatible C grid')
if (gridT%ny.ne.gridU%ny) call stop_error(1,'Incompatible C grid')
if (gridT%nx.ne.gridV%nx) call stop_error(1,'Incompatible C grid')
if (gridT%ny.ne.gridV%ny) call stop_error(1,'Incompatible C grid')

! ... Grid T bounds:
! ...
gridT%xl(1) = 2*gridT%x(1) - gridU%x(1)
do i=2,gridT%nx
  gridT%xu(i-1) = gridU%x(i-1)
  gridT%xl(i)   = gridU%x(i-1)
enddo
gridT%xu(gridT%nx) = gridU%x(gridU%nx)

gridT%yl(1) = 2*gridT%y(1) - gridV%y(1)
do j=2,gridT%ny
  gridT%yu(j-1) = gridV%y(j-1)
  gridT%yl(j)   = gridV%y(j-1)
enddo
gridT%yu(gridT%ny) = gridV%y(gridV%ny)

! ... Grid U bounds:
! ...
do i=1,gridU%nx-1
  gridU%xl(i)   = gridT%x(i)
  gridU%xu(i)   = gridT%x(i+1)
enddo
gridU%xl(gridU%nx) = gridT%x(gridT%nx)
gridU%xu(gridU%nx) = 2*gridU%x(gridu%nx) - gridT%x(gridT%nx)

gridU%yl(1) = 2*gridU%y(1) - gridV%y(1)
do j=2,gridU%ny
  gridU%yu(j-1) = gridV%y(j-1)
  gridU%yl(j)   = gridV%y(j-1)
enddo
gridU%yu(gridU%ny) = gridV%y(gridV%ny)

! ... Grid V bounds:
! ...
gridV%xl(1) = 2*gridV%x(1) - gridU%x(1)
do i=2,gridV%nx
  gridV%xu(i-1) = gridU%x(i-1)
  gridV%xl(i)   = gridU%x(i-1)
enddo
gridV%xu(gridV%nx) = gridU%x(gridU%nx)

do j=1,gridV%ny-1
  gridV%yl(j)   = gridT%y(j)
  gridV%yu(j)   = gridT%y(j+1)
enddo
gridV%yl(gridU%ny) = gridT%y(gridT%ny)
gridV%yu(gridU%ny) = 2*gridV%y(gridu%ny) - gridT%y(gridT%ny)

return
end subroutine CgridBounds
! ...
! =====================================================================
! ...
subroutine BgridBounds (gridT,gridU,gridV)

! ... Calculates the bounds of an Arakawa C-grid. The grid index is
! ... supposed as follows
! ...
! ...                     -- u(i,j),v(i,j)
! ...                |
! ...             t(i,j)  -- 

type(grid), intent(inout)       :: gridT,gridU,gridV

! ... Local variables:
! ...
integer i,j

if (gridT%nx.ne.gridU%nx) call stop_error(1,'Incompatible B grid')
if (gridT%ny.ne.gridU%ny) call stop_error(1,'Incompatible B grid')
if (gridT%nx.ne.gridV%nx) call stop_error(1,'Incompatible B grid')
if (gridT%ny.ne.gridV%ny) call stop_error(1,'Incompatible B grid')

! ... Grid T bounds:
! ...
gridT%xl(1) = 2*gridT%x(1) - gridU%x(1)
do i=2,gridT%nx
  gridT%xu(i-1) = gridU%x(i-1)
  gridT%xl(i)   = gridU%x(i-1)
enddo
gridT%xu(gridT%nx) = gridU%x(gridU%nx)

gridT%yl(1) = 2*gridT%y(1) - gridV%y(1)
do j=2,gridT%ny
  gridT%yu(j-1) = gridV%y(j-1)
  gridT%yl(j)   = gridV%y(j-1)
enddo
gridT%yu(gridT%ny) = gridV%y(gridV%ny)

! ... Grid U bounds:
! ...
do i=1,gridU%nx-1
  gridU%xl(i)   = gridT%x(i)
  gridU%xu(i)   = gridT%x(i+1)
enddo
gridU%xl(gridU%nx) = gridT%x(gridT%nx)
gridU%xu(gridU%nx) = 2*gridU%x(gridU%nx) - gridT%x(gridT%nx)

do j=1,gridU%ny-1
  gridU%yl(j)   = gridT%y(j)
  gridU%yu(j)   = gridT%y(j+1)
enddo
gridU%yl(gridU%ny) = gridT%y(gridT%ny)
gridU%yu(gridU%ny) = 2*gridU%y(gridu%ny) - gridT%y(gridT%ny)

! ... Grid V bounds:
! ...
do i=1,gridV%nx-1
  gridV%xl(i)   = gridT%x(i)
  gridV%xu(i)   = gridT%x(i+1)
enddo
gridV%xl(gridV%nx) = gridT%x(gridT%nx)
gridV%xu(gridV%nx) = 2*gridV%x(gridV%nx) - gridT%x(gridT%nx)

do j=1,gridV%ny-1
  gridV%yl(j)   = gridT%y(j)
  gridV%yu(j)   = gridT%y(j+1)
enddo
gridV%yl(gridU%ny) = gridT%y(gridT%ny)
gridV%yu(gridU%ny) = 2*gridV%y(gridu%ny) - gridT%y(gridT%ny)

return
end subroutine BgridBounds
! ...
! =====================================================================
! ...
subroutine AxisBounds (x,xl,xu)

! ... Calculates the bounds of an one-dimensional axis.
! ... supposed as follows
! ...
! ...        x(i-1) -- x(i) -- x(i+1)

real(dp), dimension(:), intent(in)        :: x
real(dp), dimension(size(x)), intent(out) :: xl
real(dp), dimension(size(x)), intent(out) :: xu

! ... Local variables:
! ...
integer i,nx

nx = size(x)

xl(1) = half*(x(1)-x(2))
xu(1) = half*(x(1)+x(2))
xl(nx) = half*(x(nx-1)+x(nx))
xu(nx) = half*(3*x(nx)-x(nx-1))

do i=2,nx-1
  xl(i) = half*(x(i-1)+x(i))
  xu(i) = half*(x(i)+x(i+1))
enddo

return
end subroutine AxisBounds
! ...
! =====================================================================
! ...
end module grids
