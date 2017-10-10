! *********************************************************************
! ... streamline.f90
! ... Quim Ballabrera, October 2017
! ... COSMO project
! ... Program that calculates the velocity field from a streamfunction.
! ... The output can be in Arakawa's A or C grids.
! ... The velocity field follows:
! ...      u = - dPsi/dy
! ...      v =   dPsi/dx
! *********************************************************************

use cosmo
use netcdf

implicit none

integer na,nx,ny,i,j,err,fid,idi,idj,idx,idy,idp,idu,idv
real(dp)  dx,dy
real(dp), dimension(:), allocatable             :: x,y
real(dp), dimension(:,:), allocatable           :: psi,u,v
character(len=180)                              :: ofile

logical                                         :: agrid = .false.
logical                                         :: cgrid = .false.
logical                                         :: fdx   = .false.
logical                                         :: fdy   = .false.
logical                                         :: out   = .false.

! ... Default values:
! ...
dx = 0.01
dy = 0.01
ofile = 'streamline.nc'

call lineargs_ini(na)

call argflg('-A',agrid)
call argflg('-C',cgrid)
call argdbl('-dx',fdx,dx)
call argdbl('-dy',fdy,dy)

if (agrid.and.cgrid) call stop_error(1,'Incompatible options -A and -C')
agrid = .not.cgrid

if (agrid) then
  write(*,*) 'Arakawa A grid'
else
  write(*,*) 'Arakawa C grid'
endif

nx = 6.0_dp/dx + 1
ny = 6.0_dp/dy + 1

allocate(x(nx))
allocate(y(ny))
x = arange(-3.0_dp,3.0_dp,nx)
y = arange(-3.0_dp,3.0_dp,ny)

allocate(psi(nx,ny))
allocate(u(nx,ny))
allocate(v(nx,ny))

do j=1,ny
do i=1,nx
  psi(i,j) = psifunc(x(i),y(j))
enddo
enddo


if (agrid) then

  do j=1,ny
  do i=1,nx
    u(i,j) = -half*(psifunc(x(i),y(j)+dy) - psifunc(x(i),y(j)-dy))/dy
    v(i,j) =  half*(psifunc(x(i)+dx,y(j)) - psifunc(x(i)-dx,y(j)))/dx
  enddo
  enddo

  err = nf90_create(ofile,NF90_WRITE,fid)
  err = nf90_def_dim(fid,'x',nx,idi)
  err = nf90_def_dim(fid,'y',ny,idj)
  err = nf90_def_var(fid,'x',NF90_FLOAT,(/idi/),idx)
  err = nf90_def_var(fid,'y',NF90_FLOAT,(/idj/),idy)
  err = nf90_def_var(fid,'psi',NF90_FLOAT,(/idi,idj/),idp)
  err = nf90_def_var(fid,'u',NF90_FLOAT,(/idi,idj/),idu)
  err = nf90_def_var(fid,'v',NF90_FLOAT,(/idi,idj/),idv)
  err = nf90_enddef(fid)

  err = nf90_put_var(fid,idx,x)
  err = nf90_put_var(fid,idy,y)
  err = nf90_put_var(fid,idp,psi)
  err = nf90_put_var(fid,idu,u)
  err = nf90_put_var(fid,idv,v)

  err = nf90_close(fid)

else

endif



call stop_error(0,'Ok')

contains 

  pure function psifunc(x,y) result(psi)

  real(dp), intent(in)     :: x,y
  real(dp)                 :: psi

  psi = half*(x*x+y*y)

  end function psifunc

end 




