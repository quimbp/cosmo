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
subroutine forcing_ini(west,south,east,north)

real(dp), intent(out)                :: west,south,east,north

! ... Local variables:
! ...
integer err,k,lu,lv
real(dp) tou,tov,tau,tav,tmp
real(dp) tmin0,tmax0
type(type_date) dmin,dmax

! ... Zonal ocean current
! ...
call assign_names(GOU,GOU_vnames)
if (GOU%open(OcexFname).ne.0) call stop_error(1,'Unable to scan OCEAN ZONAL file')
if (GOU%scan(OcexVname).ne.0) call stop_error(1,'Invalid to find OCEAN ZONAL velocity variable')
call GOU%show('Zonal ocean current')

! ... Meridional ocean current
! ...
call assign_names(GOV,GOV_vnames)
if (GOV%open(OceyFname).ne.0) call stop_error(1,'Unable to scan OCEAN MERIDIONAL file')
if (GOV%scan(OceyVname).ne.0) call stop_error(1,'Unable to find OCEAN MERIDIONAL velocity variable')
call GOV%show('Meridional ocean current')

! ... Winds
! ... We know that here, withAtmx and withAtmy are be both true or false.
! ... We check only one of them.
! ...
if (withAtmx) then
  ! ... Zonal wind
  ! ...
  call assign_names(GAU,GAU_vnames)
  if (GAU%open(AtmxFname).ne.0) call stop_error(1,'Unable to scan WIND EASTWARD file')
  if (GAU%scan(AtmxVname).ne.0) call stop_error(1,'Unable to find WIND EASTWARD velocity variable')
  call GAU%show('Zonal wind')

  ! ... Meridional wind
  ! ...
  call assign_names(GAV,GAV_vnames)
  if (GAV%open(AtmyFname).ne.0) call stop_error(1,'Unable to scan WIND NORTHWARD file')
  if (GAV%scan(AtmyVname).ne.0) call stop_error(1,'Unable to find WIND NORTHWARD velocity variable')
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

return

contains

    subroutine assign_names(GRD,GRD_vnames)
      type(type_grid), intent(inout)          :: GRD
      type(type_vnames), intent(in)           :: GRD_vnames
      if (len_trim(GRD_vnames%xname).gt.0) GRD%xname = trim(GRD_vnames%xname)
      if (len_trim(GRD_vnames%yname).gt.0) GRD%yname = trim(GRD_vnames%yname)
      if (len_trim(GRD_vnames%zname).gt.0) GRD%zname = trim(GRD_vnames%zname)
      if (len_trim(GRD_vnames%tname).gt.0) GRD%tname = trim(GRD_vnames%tname)
    end subroutine assign_names

end subroutine forcing_ini
! ...
! =====================================================================
! ...
subroutine forcing_time(tmin,tmax)

real(dp), intent(out)                :: tmin,tmax

! ... Local variables:
! ...
integer k,lu,lv
real(dp) tou,tov,tau,tav,tmp
real(dp) tmin0,tmax0
type(type_date) dmin,dmax

! ... Time axis: first, check for minimum and maximum common time:
! ...            We use the "unified units" of "seconds since 1970-01-01 00:00:00"
! ...            stored in the "s" arrays
! ...
tou = minval(GOU%s(:))
tov = minval(GOV%s(:))
tmin0 = max(tou,tov)
if (withAtmx) then
  tau = minval(GAU%s(:))
  tav = minval(GAV%s(:))
  tmin0 = maxval([tmin0,tau,tav])
endif
lu = max(locate(GOU%s,tmin0),2)
lv = max(locate(GOV%s,tmin0),2)
tmin0 = max(GOU%s(lu),GOV%s(lv))


tou = maxval(GOU%s(:))
tov = maxval(GOV%s(:))
tmax0 = min(tou,tov)
if (withAtmx) then
  tau = maxval(GAU%s(:))
  tav = maxval(GAV%s(:))
  tmax0 = minval([tmax0,tau,tav])
endif
lu = min(locate(GOU%s,tmax0),GOU%nt-1)
lv = min(locate(GOV%s,tmax0),GOV%nt-1)
tmax0 = min(GOU%s(lu),GOV%s(lv))

if (withTini) then
  userDini = strptime(userSini)
  userDini%calendar = GOU%calendar
  userTini = date2num(userDini,units='seconds since 1970-01-01 00:00:00')
  tmin = userTini
  if (userTini.lt.tmin0) stop 'ERROR: User Initial time too small'
  if (userTini.gt.tmax0) stop 'ERROR: User Initial time too big'
else
  write(*,*) 'WARNING: No initial date proposed by the user'
  if (reverse.eq.1) then
    userTini = tmin0
  else
    userTini = tmax0
  endif
  userDini = num2date(userTini,                                  &
                      units='seconds since 1970-01-01 00:00:00', &
                      calendar=GOU%calendar)
endif

if (withTlen) then
  tmax = tmin + reverse*UserTlen*86400     ! Seconds
else
  tmax = tmin + reverse*7*86400            ! Default 7's day simulation (in Seconds)
endif

! ... Now do not go beyond the maximum allowed simulation
! ...
!print*, 'tmin0, tmax0 : ', tmin0, tmax0
!print*, 'tmin,  tmax  : ', tmin, tmax
!print*, 'reverse      : ', reverse

if (withReverse) then
  if (tmax.lt.tmin0) tmax = tmin0
else
  if (tmax.gt.tmax0) tmax = tmax0
endif

!print*, 'AGAIN !!!!!'
!print*, 'tmin0, tmax0 : ', tmin0, tmax0
!print*, 'tmin,  tmax  : ', tmin, tmax
!print*, 'reverse      : ', reverse

! ... Change origin of time:
! ...
GOU%s(:) = GOU%s(:) - tmin
GOV%s(:) = GOV%s(:) - tmin
if (withAtmx) then
  GAU%s(:) = GAU%s(:) - tmin
  GAV%s(:) = GAV%s(:) - tmin
endif

return
end subroutine forcing_time
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
