subroutine getmask
! ... Read the first velocity field and extract the mask from it

use lagrangian
use cosmo, only: dp
use netcdf

! ... Local variables
! ...
integer err,i,j,ii,jj
real(dp), dimension(nx,ny)             :: u

allocate (land(nx,ny))
allocate (sea(nx,ny))
allocate (umask(nx,ny))

po(:) = 1
pf(:) = 1
pf(icdf%ppi(uid)) = icdf%nx
pf(icdf%ppj(uid)) = icdf%ny
err = NF90_GET_VAR (icdf%fid,uid,cdf2d,po,pf)

u = cdf2d

land = .false.
if (missingisnan) then
  where(isnan(u))     land = .true.
else
  where(u.eq.missing) land = .true.
endif
sea = .not.land

umask(:,:) = one
where(land) umask = zero

! ... Get iw,ie,js,jn
! ...
allocate (iw(nx,ny))
allocate (ie(nx,ny))
allocate (js(nx,ny))
allocate (jn(nx,ny))

iw = 0
ie = 0
js = 0
jn = 0
do j=1,ny
do i=1,nx
  if (sea(i,j)) then
    do ii=i-1,1,-1
      if (land(ii,j)) exit 
    enddo
    iw(i,j) = ii + 1
    do ii=i+1,nx
      if (land(ii,j)) exit 
    enddo
    ie(i,j) = ii - 1
    do jj=j-1,1,-1
      if (land(i,jj)) exit 
    enddo
    js(i,j) = jj + 1
    do jj=j+1,ny
      if (land(i,jj)) exit 
    enddo
    jn(i,j) = jj - 1
  endif
  !print '(L,4I5)', sea(i,j), iw(i,j), ie(i,j), js(i,j), jn(i,j)
enddo
enddo

end subroutine getmask
