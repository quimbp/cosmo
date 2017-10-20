! *********************************************************************
! ... streamline.f90
! ... Quim Ballabrera, October 2017
! ... COSMO project
! ... Program that calculates the velocity field from a streamfunction.
! ... The output can be in Arakawa's A or C grids.
! ... The velocity field follows:
! ...      u = - dPsi/dy
! ...      v =   dPsi/dx
! ... Version 0.1, released October 2017
! *********************************************************************

use cosmo
use netcdf

implicit none

character(len=*), parameter             :: version = 'v0.1'
character(len=*), parameter             :: author  = 'Quim Ballabrera'

! ... I/O flags and options
! ...
logical                                 :: out   = .false.
logical                                 :: agrid = .false.
logical                                 :: cgrid = .false.
logical                                 :: fdx   = .false.
logical                                 :: fdy   = .false.
logical                                 :: hlp   = .false.

! ...
integer na,nx,ny,i,j,err,fid,idi,idj,idx,idy,idp,idu,idv
integer idxp,idyp,idxu,idyu,idxv,idyv
real(dp)  dx,dy
real(dp), dimension(:), allocatable     :: x,y
real(dp), dimension(:), allocatable     :: xu,yu
real(dp), dimension(:), allocatable     :: xv,yv
real(dp), dimension(:,:), allocatable   :: psi,u,v
character(len=180)                      :: ofile

! ... The help
! ...
call help_version(version)
call help_progname('STREAMLINE')
call help_author(author)
call help_summary('Program to calculate the velocity field from an &
 &analytical stream function. The velocity field can be writen in an &
 &Arakawa A or Arakawa C grid. To change the analytic function, the &
 &user needs to modify subroutine UDF before compiling the program.')
call help_option('-out    FILENAME','Output filename','streamline.nc')
call help_option('-A              ','Output in Arakawa A grid','A')
call help_option('-C              ','Output in Arakawa C grid','')
call help_option('-dx             DX','Zonal grid interval','0.01')
call help_option('-dy             DY','Meridional grid interval','0.01')
call help_option ('--options  filename','To read the commandline options &
  &from a file.','')
call help_option ('--help','To show this help','')
call help_example ('> streamline -C -o c_grid.nc')


! ... Default values:
! ...
dx = 0.01
dy = 0.01
ofile = 'streamline.nc'

! ... Lineargs
! ...
call lineargs_ini(na)
if (na.eq.0) call help_write()

call argflg('--h',hlp)
call argflg('-help',hlp)
if (hlp) then
  call help_write()
else
  call header()
endif

call argstr('-o',out,ofile)
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

nx = nint(6.0_dp/dx + 1)
ny = nint(6.0_dp/dy + 1)

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

  allocate (xu(nx))
  allocate (yu(ny))
  allocate (xv(nx))
  allocate (yv(ny))

  xu(:) = x(:) + half*dx
  yu(:) = y(:)
  xv(:) = x(:)
  yv(:) = y(:) + half*dy

  do j=1,ny
  do i=1,nx
    u(i,j) = -half*(psifunc(xu(i),yu(j)+dy) - psifunc(xu(i),yu(j)-dy))/dy
    v(i,j) =  half*(psifunc(xv(i)+dx,yv(j)) - psifunc(xv(i)-dx,yv(j)))/dx
  enddo
  enddo

  err = nf90_create(ofile,NF90_WRITE,fid)
  err = nf90_def_dim(fid,'x',nx,idi)
  err = nf90_def_dim(fid,'y',ny,idj)
  err = nf90_def_var(fid,'x_p',NF90_FLOAT,(/idi/),idxp)
  err = nf90_def_var(fid,'y_p',NF90_FLOAT,(/idj/),idyp)
  err = nf90_def_var(fid,'x_u',NF90_FLOAT,(/idi/),idxu)
  err = nf90_def_var(fid,'y_u',NF90_FLOAT,(/idj/),idyu)
  err = nf90_def_var(fid,'x_v',NF90_FLOAT,(/idi/),idxv)
  err = nf90_def_var(fid,'y_v',NF90_FLOAT,(/idj/),idyv)
  err = nf90_def_var(fid,'psi',NF90_FLOAT,(/idi,idj/),idp)
  err = nf90_def_var(fid,'u',NF90_FLOAT,(/idi,idj/),idu)
  err = nf90_def_var(fid,'v',NF90_FLOAT,(/idi,idj/),idv)
  err = nf90_enddef(fid)

  err = nf90_put_var(fid,idxp,x)
  err = nf90_put_var(fid,idyp,y)
  err = nf90_put_var(fid,idxu,x+half*dx)
  err = nf90_put_var(fid,idyu,y)
  err = nf90_put_var(fid,idxv,x)
  err = nf90_put_var(fid,idyv,y+half*dy)
  err = nf90_put_var(fid,idp,psi)
  err = nf90_put_var(fid,idu,u)
  err = nf90_put_var(fid,idv,v)

  err = nf90_close(fid)

endif

call stop_error(0,'Ok')

contains 

  pure function psifunc(x,y) result(psi)

  real(dp), intent(in)     :: x,y
  real(dp)                 :: psi

  psi = half*(x*x+y*y+x*y)

  end function psifunc

end 




