! ****************************************************************************
! ... interpol3d.f90
! ... Quim Ballabrera, October 2018
! ... COSMO Lagrangian model
! ... Two-dimensional interpolation of a given field.
! ... Version 1.0, released October 2018
! ****************************************************************************

function interpol3d (method,nx,ny,nz,xm,ym,zm,f3d,xo,yo,zo) result(fo)

use cosmo

implicit none

real(dp) interpol2d
external interpol2d

character(len=*), intent(in)                        :: method
integer, intent(in)                                 :: nx,ny,nz
real(dp), dimension(nx), intent(in)                 :: xm
real(dp), dimension(ny), intent(in)                 :: ym
real(dp), dimension(nz), intent(in)                 :: zm
real(dp), dimension(nx,ny,nz), intent(in)           :: f3d
real(dp), intent(in)                                :: xo,yo,zo
real(dp)                                            :: fo

! ... Local variables
! ...
integer                       :: io,jo,ko
real(dp)                      :: f1,f2,z1,z2

if (xo.le.xm(1)) then
  io = 1
else if (xo.ge.xm(nx)) then
  io = nx
else
  io = locate(xm,xo)
endif

if (yo.le.ym(1)) then
  jo = 1
else if (yo.ge.ym(ny)) then
  jo = ny
else
  jo = locate(ym,yo)
endif

if (abs(zo-zm(1)).lt.1D-3) then
 ko = 1
 fo = interpol2d (method,nx,ny,xm,ym,io,jo,f3d(:,:,1),xo,yo)
 return
endif

if (nz.gt.1) then
  ko = locate(zm,zo)
  z1 = zm(ko)
  z2 = zm(ko+1)
  f1 = interpol2d (method,nx,ny,xm,ym,io,jo,f3d(:,:,ko),xo,yo)
  f2 = interpol2d (method,nx,ny,xm,ym,io,jo,f3d(:,:,ko+1),xo,yo)
  fo = f1 + (zo-z1)*(f2-f1)/(z2-z1)
else
  fo = interpol2d (method,nx,ny,xm,ym,io,jo,f3d(:,:,1),xo,yo)
endif

end function interpol3d
! ...
! ========================================================================
! ...
function interpol2d (method,nx,ny,xm,ym,io,jo,f2d,xo,yo) result(fo)

use cosmo

implicit none

character(len=*), intent(in)                        :: method
integer, intent(in)                                 :: nx,ny
real(dp), dimension(nx), intent(in)                 :: xm
real(dp), dimension(ny), intent(in)                 :: ym
integer, intent(in)                                 :: io,jo
real(dp), dimension(nx,ny), intent(in)              :: f2d
real(dp), intent(in)                                :: xo,yo
real(dp)                                            :: fo

! ... Local variables
! ...
integer                       :: i,i1,i2,j1,j2
real(dp), dimension(nx)       :: wrk
real(dp)                      :: x1,x2,y1,y2,f1,f2,f3,f4,t,u


select case (method)

case ('GLOBALSPLINES')
  do i=1,nx
    wrk(i) = akima(ym(:),f2d(i,:),yo)
  enddo
  fo = akima(xm(:),wrk(:),xo)
case ('LOCALSPLINES')
  i1 = max(1,io-10)
  i2 = min(nx,io+10)
  j1 = max(1,jo-10)
  j2 = min(nx,jo+10)
  do i=i1,i2
    wrk(i) = akima(ym(j1:j2),f2d(i,j1:j2),yo)
  enddo
  fo = akima(xm(i1:i2),wrk(i1:i2),xo)


case ('BILINEAL')
  if ((io.eq.nx).and.(jo.eq.ny)) then
    fo = f2d(io,jo)
    return
  else if ((io.eq.1).and.(jo.eq.1)) then
    fo = f2d(io,jo)
    return
  else if (io.eq.nx) then
    x1 = xm(io);   x2 = xm(io)
    y1 = ym(jo);   y2 = ym(jo+1)
    f1 = f2d(io,jo)
    f2 = f2d(io,jo)
    f3 = f2d(io,jo+1)
    f4 = f2d(io,jo+1)
    t  = zero
    u  = (yo-y1)/(y2-y1)
    fo = (one-t)*(one-u)*f1 + t*(one-u)*f2 + t*u*f3 + (one-t)*u*f4
    return
  else if (jo.eq.ny) then
    x1 = xm(io);   x2 = xm(io+1)
    y1 = ym(jo);   y2 = ym(jo)
    f1 = f2d(io,jo)
    f2 = f2d(io+1,jo)
    f3 = f2d(io+1,jo)
    f4 = f2d(io,jo)
    t  = (xo-x1)/(x2-x1)
    u  = zero
    fo = (one-t)*(one-u)*f1 + t*(one-u)*f2 + t*u*f3 + (one-t)*u*f4
  else
    x1 = xm(io);   x2 = xm(io+1)
    y1 = ym(jo);   y2 = ym(jo+1)
    f1 = f2d(io,jo)
    f2 = f2d(io+1,jo)
    f3 = f2d(io+1,jo+1)
    f4 = f2d(io,jo+1)
    t  = (xo-x1)/(x2-x1)
    u  = (yo-y1)/(y2-y1)
    fo = (one-t)*(one-u)*f1 + t*(one-u)*f2 + t*u*f3 + (one-t)*u*f4
  endif

end select

end function interpol2d

