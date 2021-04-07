module module_forcing

use module_grid
use module_status
use module_options

implicit none

type(type_grid)                      :: GOU,GOV              ! Ocean velocity grids
type(type_grid)                      :: GAU,GAV              ! Atmospheric velocity grids
integer                              :: NLAYER
integer, dimension(:), pointer       :: LAYER

contains
! ...
! ====================================================================
! ====================================================================
! ...
subroutine forcing_ini(west,south,east,north,tmin,tmax)

real(dp), intent(out)                :: west,south,east,north
real(dp), intent(out)                :: tmin,tmax

! ... Local variables:
! ...
integer err,k

! ... Zonal ocean current
! ...
if (GOU%open(OcexFname).ne.0) call stop_error(1,'Unable to open OCEAN ZONAL file')
if (GOU%scan(OcexVname).ne.0) call stop_error(1,'Unable to scan OCEAN ZONAL file')
call GOU%show('Zonal ocean current')

! ... Meridional ocean current
! ...
if (GOV%open(OceyFname).ne.0) call stop_error(1,'Unable to open OCEAN MERIDIONAL file')
if (GOV%scan(OceyVname).ne.0) call stop_error(1,'Unable to scan OCEAN MERIDIONAL file')
call GOV%show('Zonal ocean current')

! ... Winds
! ... We know that here, withAtmx and withAtmy are be both true or false.
! ... We check only one of them.
! ...
if (withAtmx) then
  ! ... Zonal wind
  ! ...
  if (GAU%open(AtmxFname).ne.0) call stop_error(1,'Unable to open WIND EASTWARD file')
  if (GAU%scan(AtmxVname,AtmxMaskName,AtmxMaskValue).ne.0) call stop_error(1,'Unable to scan WIND EASTWARD file')
  call GAU%show('Zonal wind')

  ! ... Meridional wind
  ! ...
  if (GAV%open(AtmyFname).ne.0) call stop_error(1,'Unable to open WIND NORTHWARD file')
  if (GAV%scan(AtmyVname,AtmyMaskName,AtmyMaskValue).ne.0) call stop_error(1,'Unable to scan WIND NORTHWARD file')
  call GAV%show('Meridional wind')
endif


! ... System limits in radians:
! ...
if (withAtmx) then
  west  = maxval([GOU%lonmin,GOV%lonmin,GAU%lonmin,GAV%lonmin])
  east  = minval([GOU%lonmax,GOV%lonmax,GAU%lonmax,GAV%lonmax])
  south = maxval([GOU%latmin,GOV%latmin,GAU%latmin,GAV%latmin])
  north = minval([GOU%latmax,GOV%latmax,GAU%latmax,GAV%latmax])
else
  west  = max(GOU%lonmin,GOV%lonmin)
  east  = min(GOU%lonmax,GOV%lonmax)
  south = max(GOU%latmin,GOV%latmin)
  north = min(GOU%latmax,GOV%latmax)
endif

if (CropXmin) west  = max(west,deg2rad*userXmin)
if (CropXmax) east  = min(east,deg2rad*userXmax)
if (CropYmin) south = max(south,deg2rad*userYmin)
if (CropYmax) north = min(north,deg2rad*userYmax)

! ... Crop forcing fields:
! ... Bounding box in radians
! ...
err = GOU%crop(west,south,east,north)
err = GOV%crop(west,south,east,north)
if (withAtmx) then
  err = GAU%crop(west,south,east,north)
  err = GAV%crop(west,south,east,north)
endif

! ... Vertical axis:
! ... We will assume the same vertical structure for horizontal currents
! ...
if (GOU%nz.ne.GOV%nz) call stop_error(1,'Incompatible number of ocean layers')

! ... Re-scale time with reference to the common time:
! ... The zero corresponds to tmin.
! ... Negative values are not a problem
! ...
tmin = UserTini                     ! Julian days
tmax = UserTini + reverse*UserTlen  ! Julian days

GOU%t(:) = nint((GOU%t(:)-tmin)*86400.0_dp)      ! Time in seconds
GOV%t(:) = nint((GOV%t(:)-tmin)*86400.0_dp)      ! Time in seconds
if (withAtmx) then
  GAU%t(:) = nint((GAU%t(:)-tmin)*86400.0_dp)    ! Time in seconds
  GAV%t(:) = nint((GAV%t(:)-tmin)*86400.0_dp)    ! Time in seconds
endif

return
end subroutine forcing_ini
! ...
! ====================================================================
! ...
subroutine forcing_layers(zf,kf)

real(dp), dimension(:), intent(in)         :: zf
integer, dimension(size(zf)), intent(out)  :: kf

! ... Local variables
! ...
logical new
integer Nf,flo,l
integer tmp(GOU%nz)
integer, dimension(:), allocatable  :: ind


Nf = size(zf)

if (GOU%nz.eq.1) then
  NLAYER = 1
  allocate(LAYER(NLAYER))
  LAYER(1) = 1
  kf(:) = 1                   ! All floats will be advected by the same layer

else
  ! ... 
  ! ... loop over released floats
  ! ...
  do flo=1,nf
    if (zf(flo).le.GOU%z(1)) then
      kf(flo) = 1
    else
      kf(flo) = min(locate(GOU%z,zf(flo))+1,GOU%Nz)
    endif
  enddo

  ! ... For each float, we now have the layer that will advect it
  ! ... We get the number of different layers:
  ! ...
  NLAYER = 1
  tmp(1) = kf(1)
  do flo=2,nf
    new = .true.
    do l=1,NLAYER
      if (kf(flo).eq.tmp(l)) new = .false.
    enddo
    if (new) then
      NLAYER = NLAYER + 1
      tmp(NLAYER) = kf(flo)
    endif
  enddo
  allocate(LAYER(NLAYER))
  allocate(ind(NLAYER))
  call indexi(tmp(1:NLAYER),ind)

  do l=1,NLAYER
    LAYER(l) = tmp(ind(l))
  enddo
  deallocate(ind)

  ! ... Finally, get the layers for each float
  ! ...
  do flo=1,nf
    do l=1,NLAYER
      if (kf(flo).eq.LAYER(l)) then
        kf(flo) = l
        cycle
      endif
    enddo
  enddo

endif

GOU%nk = NLAYER
GOV%nk = NLAYER
return

end subroutine forcing_layers
! ...
! ====================================================================
! ...
end module module_forcing
