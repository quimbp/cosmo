! ***************************************************************************
! ... clm.f90
! ... Quim Ballabrera, August 2017
! ... COSMO Lagrangian model
! ... CLM subroutines:
! ...   clm_ufield_open
! ...   clm_tfield_open
! ...   get_land
! ...   clm_ini
! ...   clm_run
! ...      clm_coeffs
! ...      clm_rhs
! ...      read_fields
! ...      get_data
! ...      i3coeffs
! ...      outside
! ...      beaching
! ...      RHS
! ...      cubic
! ... Version 0.1, released October 2017
! ... Version 0.2, released December 2017
! ...              Increase length for filenames (len=maxlen).
! ***************************************************************************

module clm

use cosmo
use mod_floats
use mod_out

implicit none

! ... Forward/Backward integration
! ... Default: Forward
integer                                  :: lorig          = 1
integer                                  :: time_direction = 1 ! +1, -1
logical                                  :: reverse

! ... Simulation length
! ...
logical                                  :: ftimesim = .false.
logical                                  :: fent     = .false.
logical                                  :: fedt     = .false.
logical                                  :: fidt     = .false.

integer                                  :: record = -1
integer                                  :: external_nsteps
integer                                  :: internal_step
integer                                  :: internal_nsteps
real(dp)                                 :: simulation_length
real(dp)                                 :: external_dt
!real(dp)                                 :: internal_dt
real(dp)                                 :: file_time

! ... Stationnary simulation
! ...
logical                                  :: stationary    = .false.

! ... Degree to meters conversion
! ... Simple esphere
real(dp), parameter                      :: deg2m = deg2rad*Rearth  ! deg -> m
real(dp), parameter                      :: m2deg = one/deg2m       ! m -> deg

! ... Center of the horizontal coordinates:
! ... In degrees
real(dp)                                 :: xcenter
real(dp)                                 :: ycenter
real(dp)                                 :: coslat

! ... Scale factor to pass input time to seconds
! ... Default: 1.0D0
logical                                  :: tscale_flag = .false.
real(dp)                                 :: tscale      = one
character(len=80)                        :: time_units  = ''

! ... Missing value
! ...
logical                                  :: fmv         = .false.
real(dp)                                 :: missing     = nan

! ... Grid
! ...
integer                                  :: nx,ny,nz,nt
real(dp)                                 :: domain_east
real(dp)                                 :: domain_west
real(dp)                                 :: domain_south
real(dp)                                 :: domain_north

! ... Time variables
! ...
logical                                  :: fcal  = .false.
real(dp)                                 :: time_ref
real(dp)                                 :: time_ini
real(dp)                                 :: time_fin
real(dp), dimension(:), pointer          :: ellapsed_time
real(dp), dimension(:), pointer          :: dtime
character(len=80)                        :: calendar = ''
type(date_type)                          :: date_ref
type(date_type)                          :: date_ini
type(date_type)                          :: date_fin

! ... Temporary spaces
! ...
real(dp), dimension(:,:,:), pointer     :: uu,vv,ww,tt,ss,rr

! ... Cubic time interpolation
! ...
integer, dimension(4)                   :: records
real(dp), dimension(:,:,:,:), pointer   :: utab,vtab,wtab,ttab,stab,rtab
real(dp), dimension(:,:,:,:), pointer   :: ucoef,vcoef,wcoef,tcoef,scoef,rcoef

! ... Those fields represent the values at any given time
! ... they can be constant or come from a cubic interpolation
! ...
!real(dp), dimension(:,:,:,:), pointer   :: urhs,vrhs,wrhs,trhs,srhs,rrhs
!real(dp), dimension(:,:,:), pointer     :: hrhs

! ... Working box
! ...
logical                                 :: fso    = .false.  ! Flg south lim
logical                                 :: fno    = .false.  ! Flg north lim
logical                                 :: fea    = .false.  ! Flg west  lim
logical                                 :: fwe    = .false.  ! Flg east  lim
integer                                 :: iwest  = 1
integer                                 :: ieast  = 1
integer                                 :: jsouth = 1
integer                                 :: jnorth = 1
real(dp)                                :: south  = nan
real(dp)                                :: north  = nan
real(dp)                                :: west   = nan
real(dp)                                :: east   = nan

!type field
!  ! This structure contains the necessary information for a
!  ! variable to be read and processed
!  integer                               :: fid               ! ncdf file id
!  integer                               :: id                ! ncdf Var id
!  integer                               :: nx
!  integer                               :: ny
!  integer                               :: nz
!  integer                               :: ndims
!  integer                               :: ppi
!  integer                               :: ppj
!  integer                               :: ppk
!  integer                               :: ppl
!  real(dp)                              :: time
!  logical                               :: missingdefined
!  logical                               :: missingisnan
!  real(dp)                              :: missing_value
!  real(dp)                              :: add_offset
!  real(dp)                              :: scale_factor
!  real(dp), dimension(:,:,:), pointer   :: data
!end type field
!
!! ... Input grids:
!! ...
!type cdf_vgrid
!  logical                               :: defined  = .false.
!  integer                               :: fid      = -1
!  integer                               :: ndims    = -1
!  integer                               :: idx      = -1
!  integer                               :: idy      = -1
!  integer                               :: idz      = -1
!  integer                               :: idt      = -1
!  integer                               :: idvel    = -1
!  integer                               :: nx       =  1
!  integer                               :: ny       =  1
!  integer                               :: nz       =  1
!  integer                               :: nt       =  1
!  character(len=maxlen)                 :: filename = ''
!  character(len=80)                     :: varname  = ''
!  character(len=80)                     :: xname    = ''
!  character(len=80)                     :: yname    = ''
!  character(len=80)                     :: zname    = ''
!  character(len=80)                     :: tname    = ''
!  character(len=80)                     :: calendar = ''
!  integer, dimension(:), pointer        :: po
!  integer, dimension(:), pointer        :: pf
!  real(dp), dimension(:), pointer       :: x
!  real(dp), dimension(:), pointer       :: y
!  real(dp), dimension(:), pointer       :: z
!  real(dp), dimension(:), pointer       :: t
!  real(dp), dimension(:), pointer       :: xm
!  real(dp), dimension(:), pointer       :: ym
!  real(dp)                              :: time_ref
!  type(date_type)                       :: date_ref
!  type(field)                           :: vel
!  logical, dimension(:,:,:), pointer    :: land
!  logical, dimension(:,:,:), pointer    :: sea
!end type cdf_vgrid
!
!type cdf_tgrid
!  logical                               :: defined  = .false.
!  integer                               :: fid      = -1
!  integer                               :: ndims    = -1
!  integer                               :: idx      = -1
!  integer                               :: idy      = -1
!  integer                               :: idz      = -1
!  integer                               :: idt      = -1
!  integer                               :: idtemp   = -1
!  integer                               :: idsalt   = -1
!  integer                               :: iddens   = -1
!  integer                               :: nx       =  1
!  integer                               :: ny       =  1
!  integer                               :: nz       =  1
!  integer                               :: nt       =  1
!  character(len=maxlen)                 :: filename = ''
!  character(len=80)                     :: tempname = ''
!  character(len=80)                     :: saltname = ''
!  character(len=80)                     :: densname = ''
!  character(len=80)                     :: xname    = ''
!  character(len=80)                     :: yname    = ''
!  character(len=80)                     :: zname    = ''
!  character(len=80)                     :: tname    = ''
!  character(len=80)                     :: calendar = ''
!  integer, dimension(:), pointer        :: po
!  integer, dimension(:), pointer        :: pf
!  real(dp), dimension(:), pointer       :: x
!  real(dp), dimension(:), pointer       :: y
!  real(dp), dimension(:), pointer       :: z
!  real(dp), dimension(:), pointer       :: t
!  real(dp), dimension(:), pointer       :: xm
!  real(dp), dimension(:), pointer       :: ym
!  real(dp)                              :: time_ref
!  type(date_type)                       :: date_ref
!  type(field)                           :: temp
!  type(field)                           :: salt
!  type(field)                           :: dens
!  logical, dimension(:,:,:), pointer    :: land
!  logical, dimension(:,:,:), pointer    :: sea
!end type cdf_tgrid

! ==========================================================================
! ==========================================================================

contains

subroutine clm_ufield_open(UCDF)

use grids

type(cdf_vgrid), INTENT(inout)           :: UCDF

! ... Input velocity and temperature grid
! ...
type(gcdf)                               :: icdf
integer                                  :: err,ndims,idv,i

!write(*,*) 'Input file: ', trim(UCDF%filename)
if (len_trim(UCDF%xname).gt.0) icdf%xname = trim(UCDF%xname)
if (len_trim(UCDF%yname).gt.0) icdf%yname = trim(UCDF%yname)
if (len_trim(UCDF%zname).gt.0) icdf%zname = trim(UCDF%zname)
if (len_trim(UCDF%tname).gt.0) icdf%tname = trim(UCDF%tname)
call gcdf_open (UCDF%filename,icdf,err)
UCDF%fid = icdf%fid
UCDF%idx = icdf%idx
UCDF%idy = icdf%idy
UCDF%idz = icdf%idz
UCDF%idt = icdf%idt
UCDF%nx  = icdf%nx
UCDF%ny  = icdf%ny
UCDF%nz  = icdf%nz
UCDF%nt  = icdf%nt

if ((UCDF%idx.eq.-1).or.(UCDF%idy.eq.-1)) then
  call stop_error (1,'ERROR: X and Y grids must be set')
endif

allocate(UCDF%x(UCDF%nx))
allocate(UCDF%y(UCDF%ny))
allocate(UCDF%z(UCDF%nz))
allocate(UCDF%t(UCDF%nt))

allocate(UCDF%xm(UCDF%nx))
allocate(UCDF%ym(UCDF%ny))

ndims = icdf%vndims(UCDF%idx)
if (ndims.eq.1) then
  err = NF90_GET_VAR (UCDF%fid,UCDF%idx,UCDF%x)
  call cdf_error(err,'Error reading X variable')
else 
  call stop_error(1,'Irregular grid not coded yet')
endif

ndims = icdf%vndims(UCDF%idy)
if (ndims.eq.1) then
  UCDF%ny = icdf%dlen(icdf%dimids(1,UCDF%idy))
  err = NF90_GET_VAR (UCDF%fid,UCDF%idy,UCDF%y)
  call cdf_error(err,'Error reading Y variable')
else
  call stop_error(1,'Irregular grid not coded yet')
endif

if (UCDF%idz.gt.0) then
  ndims = icdf%vndims(UCDF%idz)
  if (ndims.eq.1) then
    err = NF90_GET_VAR (UCDF%fid,UCDF%idz,UCDF%z)
    call cdf_error(err,'Error reading Z variable')
  else
    call stop_error(1,'ERROR: Invalid 2D Z grid')
  endif
else
  UCDF%z(:)     = zero
endif

if (UCDF%idt.gt.0) then

  ! ... Calendar
  ! ... By default the calendar field must be empty, if it is not empty, it
  ! ... means that it has been modified by the user.
  ! ...
  if (len_trim(UCDF%calendar).eq.0) then
    err = NF90_GET_ATT(UCDF%fid,UCDF%idt,'calendar',UCDF%calendar)
    if (err.ne.NF90_NOERR) UCDF%calendar = 'gregorian'               ! Default
  endif

  ! ... Reference date:
  ! ...
  err = cdf_timeref(UCDF%fid,UCDF%idt,UCDF%date_ref)
  UCDF%date_ref%calendar = trim(UCDF%calendar)
  if (err.eq.0) then
    UCDF%time_ref = date2jd(UCDF%date_ref)
  else
    UCDF%time_ref = zero
  endif

  ndims = icdf%vndims(UCDF%idt)
  if (ndims.eq.1) then
    err = NF90_GET_VAR (UCDF%fid,UCDF%idt,UCDF%t)
    call cdf_error(err,'Error reading Time variable')
  else
    call stop_error(1,'Error: Time must have one dimension only')
  endif
else
  UCDF%t(:)     = zero
  UCDF%time_ref = zero
  UCDF%date_ref = cal2date(2000,1,1)
endif

write(*,*)
write(*,*) 'Input Netcdf grid'
write(*,*) 'Velocity name         = ', UCDF%varname
write(*,*) 'idx                   = ', UCDF%idx
write(*,*) 'idy                   = ', UCDF%idy
write(*,*) 'idz                   = ', UCDF%idz
write(*,*) 'idt                   = ', UCDF%idt
write(*,*) 'nx                    = ', UCDF%nx 
write(*,*) 'ny                    = ', UCDF%ny 
write(*,*) 'nz                    = ', UCDF%nz  
write(*,*) 'nt                    = ', UCDF%nt 

if (len_trim(UCDF%varname).eq.0) then
  call stop_error(1,'Variable name required')
endif

err = NF90_INQ_VARID (UCDF%fid,UCDF%varname,idv)
call cdf_error(err,'Variable '//trim(UCDF%varname)//' not found')

UCDF%idvel            = idv
UCDF%vel%fid          = UCDF%fid
UCDF%vel%id           = idv
UCDF%vel%nx           = UCDF%nx
UCDF%vel%ny           = UCDF%ny
UCDF%vel%nz           = UCDF%nz
UCDF%vel%ndims        = icdf%vndims(idv)

UCDF%vel%ppi          = -1
UCDF%vel%ppj          = -1
UCDF%vel%ppk          = -1
UCDF%vel%ppl          = -1
do i=1,icdf%vndims(idv)
  if (icdf%dimids(i,idv).eq.icdf%idi) UCDF%vel%ppi = i
  if (icdf%dimids(i,idv).eq.icdf%idj) UCDF%vel%ppj = i
  if (icdf%dimids(i,idv).eq.icdf%idk) UCDF%vel%ppk = i
  if (icdf%dimids(i,idv).eq.icdf%idl) UCDF%vel%ppl = i
enddo


UCDF%vel%add_offset   = icdf%add_offset(idv)
UCDF%vel%scale_factor = icdf%scale_factor(idv)

if (icdf%missing(idv)) then
  UCDF%vel%missingdefined = .true.
  UCDF%vel%missing_value  = icdf%missing_value(idv)
  UCDF%vel%missingisnan   = icdf%missingisnan(idv)
else if (icdf%fill(idv)) then
  UCDF%vel%missingdefined = .true.
  UCDF%vel%missing_value  = icdf%fill_value(idv)
  UCDF%vel%missingisnan   = icdf%missingisnan(idv)
else
  UCDF%vel%missingdefined = .false.
  UCDF%vel%missing_value  = missing
  UCDF%vel%missingisnan   = isnan(missing)
endif

allocate(UCDF%vel%data(UCDF%nx,UCDF%ny,UCDF%nz))

allocate(UCDF%land(UCDF%nx,UCDF%ny,UCDF%nz))
allocate(UCDF%sea(UCDF%nx,UCDF%ny,UCDF%nz))
call get_land(UCDF%vel,UCDF%land)
UCDF%sea = .not.UCDF%land

UCDF%defined = .true.

end subroutine clm_ufield_open
! ...
! ==========================================================================
! ...
subroutine clm_tfield_open(TCDF)

use grids

implicit none

type(cdf_tgrid), INTENT(inout)           :: TCDF


! ... Input velocity and temperature grid
! ...
type(gcdf)                               :: icdf
integer                                  :: err,ndims,idv,i

write(*,*) 'Input file: ', trim(TCDF%filename)
if (len_trim(TCDF%xname).gt.0) icdf%xname = trim(TCDF%xname)
if (len_trim(TCDF%yname).gt.0) icdf%yname = trim(TCDF%yname)
if (len_trim(TCDF%zname).gt.0) icdf%zname = trim(TCDF%zname)
if (len_trim(TCDF%tname).gt.0) icdf%tname = trim(TCDF%tname)
call gcdf_open (TCDF%filename,icdf,err)
TCDF%fid = icdf%fid
TCDF%fid = icdf%fid
TCDF%idx = icdf%idx
TCDF%idy = icdf%idy
TCDF%idz = icdf%idz
TCDF%idt = icdf%idt


!if (trim(TCDF%xname).ne.trim(icdf%xname)) then
!  err = NF90_INQ_VARID (TCDF%fid,TCDF%xname,TCDF%idx)
!  call cdf_error(err,'Variable '//trim(TCDF%xname)//' not found')
!else
!  TCDF%idx = icdf%idx
!endif
!TCDF%idx = icdf%idx

ndims = icdf%vndims(TCDF%idx)
if (ndims.eq.1) then
  TCDF%nx = icdf%dlen(icdf%dimids(1,TCDF%idx))
  allocate(TCDF%x(TCDF%nx))
  allocate(TCDF%xm(TCDF%nx))
  err = NF90_GET_VAR (TCDF%fid,TCDF%idx,TCDF%x)
  call cdf_error(err,'Error reading X variable')
else 
  write(*,*) 'WARNING: X variable has more than one dimension'
  write(*,*) 'WARNING: using first dimension'
  TCDF%nx = icdf%dlen(icdf%dimids(1,TCDF%idx))
  allocate(TCDF%x(TCDF%nx))
  allocate(TCDF%xm(TCDF%nx))
  err = NF90_GET_VAR (TCDF%fid,TCDF%idx,TCDF%x,(/1,1/),(/TCDF%nx,1/))
  call cdf_error(err,'Error reading X variable')
endif
!icdf%idi = icdf%dimids(1,TCDF%idx)

!if (trim(TCDF%yname).ne.trim(icdf%yname)) then
!  err = NF90_INQ_VARID (TCDF%fid,TCDF%yname,TCDF%idy)
!  call cdf_error(err,'Variable '//trim(TCDF%yname)//' not found')
!else
!  TCDF%idy = icdf%idy
!endif

ndims = icdf%vndims(TCDF%idy)
if (ndims.eq.1) then
  TCDF%ny = icdf%dlen(icdf%dimids(1,TCDF%idy))
  allocate(TCDF%y(TCDF%ny))
  allocate(TCDF%ym(TCDF%ny))
  err = NF90_GET_VAR (TCDF%fid,TCDF%idy,TCDF%y)
  call cdf_error(err,'Error reading Y variable')
  !icdf%idj = icdf%dimids(1,TCDF%idy)
else
  write(*,*) 'WARNING: Y variable has more than one dimension'
  write(*,*) 'WARNING: using second dimension'
  TCDF%ny = icdf%dlen(icdf%dimids(2,TCDF%idy))
  allocate(TCDF%y(TCDF%ny))
  allocate(TCDF%ym(TCDF%ny))
  err = NF90_GET_VAR (TCDF%fid,TCDF%idx,TCDF%x,(/1,1/),(/1,TCDF%ny/))
  call cdf_error(err,'Error reading Y variable')
  !icdf%idj = icdf%dimids(2,TCDF%idy)
endif

if (len_trim(TCDF%zname).gt.0) then
  !if (trim(TCDF%zname).ne.trim(icdf%zname)) then
  !  err = NF90_INQ_VARID (TCDF%fid,TCDF%zname,TCDF%idz)
  !  call cdf_error(err,'Variable '//trim(TCDF%zname)//' not found')
  !else
  !  TCDF%idz = icdf%idz
  !endif
  !TCDF%idz = icdf%idz

  ndims = icdf%vndims(TCDF%idz)
  if (ndims.eq.1) then
    TCDF%nz = icdf%dlen(icdf%dimids(1,TCDF%idz))
    allocate(TCDF%z(TCDF%nz))
    err = NF90_GET_VAR (TCDF%fid,TCDF%idz,TCDF%z)
    call cdf_error(err,'Error reading Z variable')
  else
    write(*,*) 'WARNING: Z variable has more than one dimension'
    write(*,*) 'WARNING: using first dimension'
    TCDF%nz = icdf%dlen(icdf%dimids(1,TCDF%idz))
    allocate(TCDF%z(TCDF%nz))
    err = NF90_GET_VAR (TCDF%fid,TCDF%idx,TCDF%x,(/1,1/),(/TCDF%nz,1/))
    call cdf_error(err,'Error reading Z variable')
  endif
  !icdf%idk = icdf%dimids(1,TCDF%idz)
else
  TCDF%nz       = 1
  allocate(TCDF%z(TCDF%nz))
  TCDF%z(:)     = zero
endif

if (len_trim(TCDF%tname).gt.0) then
  !if (trim(TCDF%tname).ne.trim(icdf%tname)) then
  !  err = NF90_INQ_VARID (TCDF%fid,TCDF%tname,TCDF%idt)
  !  call cdf_error(err,'Variable '//trim(TCDF%tname)//' not found')
  !else
  !  TCDF%idt = icdf%idt
  !endif
  !TCDF%idt = icdf%idt

  ! ... Calendar
  ! ... By default the calendar field must be empty, if it is not empty, it
  ! ... means that it has been modified by the user.
  ! ...
  if (len_trim(TCDF%calendar).eq.0) then
    err = NF90_GET_ATT(TCDF%fid,TCDF%idt,'calendar',TCDF%calendar)
    if (err.ne.NF90_NOERR) TCDF%calendar = 'gregorian'               ! Default
  endif

  ! ... Reference date:
  ! ...
  err = cdf_timeref(TCDF%fid,TCDF%idt,TCDF%date_ref)
  TCDF%date_ref%calendar = trim(TCDF%calendar)
  if (err.eq.0) then
    TCDF%time_ref = date2jd(TCDF%date_ref)
  else
    TCDF%time_ref = zero
  endif

  ndims = icdf%vndims(TCDF%idt)
  if (ndims.eq.1) then
    TCDF%nt = icdf%dlen(icdf%dimids(1,TCDF%idt))
    allocate(TCDF%t(TCDF%nt))
    err = NF90_GET_VAR (TCDF%fid,TCDF%idt,TCDF%t)
    call cdf_error(err,'Error reading Time variable')
  else
    call stop_error(1,'Error: Time must have one dimension only')
  endif
  !icdf%idl = icdf%dimids(1,TCDF%idt)
else
  TCDF%nt       = 1
  allocate(TCDF%t(TCDF%nt))
  TCDF%t(:)     = zero
  TCDF%time_ref = zero
  TCDF%date_ref = cal2date(2000,1,1)
endif

if (len_trim(TCDF%tempname).gt.0) then
  err = NF90_INQ_VARID (TCDF%fid,TCDF%tempname,TCDF%idtemp)
  call cdf_error(err,'Variable '//trim(TCDF%tempname)//' not found')
  TCDF%defined = .true.
endif

if (len_trim(TCDF%saltname).gt.0) then
  err = NF90_INQ_VARID (TCDF%fid,TCDF%saltname,TCDF%idsalt)
  call cdf_error(err,'Variable '//trim(TCDF%saltname)//' not found')
  TCDF%defined = .true.
endif

if (len_trim(TCDF%densname).gt.0) then
  err = NF90_INQ_VARID (TCDF%fid,TCDF%densname,TCDF%iddens)
  call cdf_error(err,'Variable '//trim(TCDF%densname)//' not found')
  TCDF%defined = .true.
endif

write(*,*)
write(*,*) 'Input Netcdf grid'
write(*,*) 'idx                   = ', TCDF%idx
write(*,*) 'idy                   = ', TCDF%idy
write(*,*) 'idz                   = ', TCDF%idz
write(*,*) 'idt                   = ', TCDF%idt
write(*,*) 'nx                    = ', TCDF%nx 
write(*,*) 'ny                    = ', TCDF%ny 
write(*,*) 'nz                    = ', TCDF%nz  
write(*,*) 'nt                    = ', TCDF%nt 
write(*,*) 'Temperature           = ', TCDF%tempname
write(*,*) 'Salinity              = ', TCDF%saltname
write(*,*) 'Density               = ', TCDF%densname
write(*,*) 'Temperature id        = ', TCDF%idtemp
write(*,*) 'Salinity id           = ', TCDF%idsalt
write(*,*) 'Density id            = ', TCDF%iddens

if (.not.TCDF%defined) call stop_error (1,'T file has no defined variables')

if (len_trim(TCDF%tempname).gt.0) then
  idv                     = TCDF%idtemp
  TCDF%temp%fid           = TCDF%fid
  TCDF%temp%id            = idv
  TCDF%temp%ndims         = icdf%vndims(idv)
  TCDF%temp%nx            = TCDF%nx
  TCDF%temp%ny            = TCDF%ny
  TCDF%temp%nz            = TCDF%nz
  TCDF%temp%add_offset    = icdf%add_offset(idv)
  TCDF%temp%scale_factor  = icdf%scale_factor(idv)
  if (icdf%missing(idv)) then
    TCDF%temp%missingdefined = .true.
    TCDF%temp%missing_value  = icdf%missing_value(idv)
    TCDF%temp%missingisnan   = icdf%missingisnan(idv)
  else if (icdf%fill(idv)) then
    TCDF%temp%missingdefined = .true.
    TCDF%temp%missing_value  = icdf%fill_value(idv)
    TCDF%temp%missingisnan   = icdf%missingisnan(idv)
  else
    TCDF%temp%missingdefined = .false.
    TCDF%temp%missing_value  = missing
    TCDF%temp%missingisnan   = isnan(missing)
  endif
  allocate(TCDF%temp%data(TCDF%nx,TCDF%ny,TCDF%nz))

  TCDF%temp%ppi          = -1
  TCDF%temp%ppj          = -1
  TCDF%temp%ppk          = -1
  TCDF%temp%ppl          = -1
  do i=1,icdf%vndims(idv)
    if (icdf%dimids(i,idv).eq.icdf%idi) TCDF%temp%ppi = i
    if (icdf%dimids(i,idv).eq.icdf%idj) TCDF%temp%ppj = i
    if (icdf%dimids(i,idv).eq.icdf%idk) TCDF%temp%ppk = i
    if (icdf%dimids(i,idv).eq.icdf%idl) TCDF%temp%ppl = i
  enddo
endif

if (len_trim(TCDF%saltname).gt.0) then
  idv                     = TCDF%idsalt
  TCDF%salt%fid           = TCDF%fid
  TCDF%salt%id            = idv
  TCDF%salt%ndims         = icdf%vndims(idv)
  TCDF%salt%ppi           = icdf%ppi(idv)
  TCDF%salt%ppj           = icdf%ppj(idv)
  TCDF%salt%ppk           = icdf%ppk(idv)
  TCDF%salt%ppl           = icdf%ppl(idv)
  TCDF%salt%nx            = TCDF%nx
  TCDF%salt%ny            = TCDF%ny
  TCDF%salt%nz            = TCDF%nz
  TCDF%salt%add_offset    = icdf%add_offset(idv)
  TCDF%salt%scale_factor = icdf%scale_factor(idv)
  if (icdf%missing(idv)) then
    TCDF%salt%missingdefined = .true.
    TCDF%salt%missing_value  = icdf%missing_value(idv)
    TCDF%salt%missingisnan   = icdf%missingisnan(idv)
  else if (icdf%fill(idv)) then
    TCDF%salt%missingdefined = .true.
    TCDF%salt%missing_value  = icdf%fill_value(idv)
    TCDF%salt%missingisnan   = icdf%missingisnan(idv)
  else
    TCDF%salt%missingdefined = .false.
    TCDF%salt%missing_value  = missing
    TCDF%salt%missingisnan   = isnan(missing)
  endif
  allocate(TCDF%salt%data(TCDF%nx,TCDF%ny,TCDF%nz))

  TCDF%salt%ppi          = -1
  TCDF%salt%ppj          = -1
  TCDF%salt%ppk          = -1
  TCDF%salt%ppl          = -1
  do i=1,icdf%vndims(idv)
    if (icdf%dimids(i,idv).eq.icdf%idi) TCDF%salt%ppi = i
    if (icdf%dimids(i,idv).eq.icdf%idj) TCDF%salt%ppj = i
    if (icdf%dimids(i,idv).eq.icdf%idk) TCDF%salt%ppk = i
    if (icdf%dimids(i,idv).eq.icdf%idl) TCDF%salt%ppl = i
  enddo
endif

if (len_trim(TCDF%densname).gt.0) then
  idv                    = TCDF%iddens
  TCDF%dens%fid          = TCDF%fid
  TCDF%dens%id           = idv
  TCDF%dens%ndims        = icdf%vndims(idv)
  TCDF%dens%ppi          = icdf%ppi(idv)
  TCDF%dens%ppj          = icdf%ppj(idv)
  TCDF%dens%ppk          = icdf%ppk(idv)
  TCDF%dens%ppl          = icdf%ppl(idv)
  TCDF%dens%nx           = TCDF%nx
  TCDF%dens%ny           = TCDF%ny
  TCDF%dens%nz           = TCDF%nz
  TCDF%dens%add_offset   = icdf%add_offset(idv)
  TCDF%dens%scale_factor = icdf%scale_factor(idv)
  if (icdf%missing(idv)) then
    TCDF%dens%missingdefined = .true.
    TCDF%dens%missing_value  = icdf%missing_value(idv)
    TCDF%dens%missingisnan   = icdf%missingisnan(idv)
  else if (icdf%fill(idv)) then
    TCDF%dens%missingdefined = .true.
    TCDF%dens%missing_value  = icdf%fill_value(idv)
    TCDF%dens%missingisnan   = icdf%missingisnan(idv)
  else
    TCDF%dens%missingdefined = .false.
    TCDF%dens%missing_value  = missing
    TCDF%dens%missingisnan   = isnan(missing)
  endif
  allocate(TCDF%dens%data(TCDF%nx,TCDF%ny,TCDF%nz))

  TCDF%dens%ppi          = -1
  TCDF%dens%ppj          = -1
  TCDF%dens%ppk          = -1
  TCDF%dens%ppl          = -1
  do i=1,icdf%vndims(idv)
    if (icdf%dimids(i,idv).eq.icdf%idi) TCDF%dens%ppi = i
    if (icdf%dimids(i,idv).eq.icdf%idj) TCDF%dens%ppj = i
    if (icdf%dimids(i,idv).eq.icdf%idk) TCDF%dens%ppk = i
    if (icdf%dimids(i,idv).eq.icdf%idl) TCDF%dens%ppl = i
  enddo
endif

allocate(TCDF%land(TCDF%nx,TCDF%ny,TCDF%nz))
allocate(TCDF%sea(TCDF%nx,TCDF%ny,TCDF%nz))
call get_land(TCDF%temp,TCDF%land)
TCDF%sea = .not.TCDF%land

end subroutine clm_tfield_open
! ...
! ==========================================================================
! ...
subroutine get_land(u,land)

use grids

type(field), intent(in)                         :: u
logical, dimension(u%nx,u%ny,u%nz), intent(out) :: land

! ... Local variables
! ...
integer err
integer po(u%ndims), pf(u%ndims)

po(:) = 1
pf(:) = 1
pf(u%ppi) = u%nx
pf(u%ppj) = u%ny

err = NF90_GET_VAR (u%fid,u%id,u%data,po,pf)
call cdf_error(err,'Unable to read field for mask calculation')

land = .false.
if (u%missingisnan) then
  where(isnan(u%data))       land = .true.
else
  where(u%data.eq.u%missing_value) land = .true.
endif

end subroutine get_land
! ...
! ==========================================================================
! ...
!subroutine clm_ini(UCDF,VCDF,WCDF,TCDF)
subroutine clm_ini()

use mrhs

!type(cdf_vgrid)                         :: UCDF
!type(cdf_vgrid)                         :: VCDF
!type(cdf_vgrid)                         :: WCDF
!type(cdf_tgrid)                         :: TCDF
!type(floater)                           :: FLT

! ... Local variables:
! ...
integer err,i
character(len=80) units_att

write(*,*)
write(*,*)
write(*,*) '==            CLM initialization             =='
write(*,*) '==============================================='
 

write(*,*)
write(*,*) 'Zonal velocity field       : ', trim(UCDF%varname)
write(*,*) 'Meridional velocity field  : ', trim(VCDF%varname)
if (WCDF%defined)   write(*,*) 'Vertical velocity field    : ', trim(WCDF%varname)
if (TCDF%defined) then
  if (TCDF%idtemp.gt.0) write(*,*) 'Temperature field          : ', trim(TCDF%tempname)
  if (TCDF%idsalt.gt.0) write(*,*) 'Salinity field             : ', trim(TCDF%saltname)
  if (TCDF%iddens.gt.0) write(*,*) 'Density field              : ', trim(TCDF%densname)
endif

nx = UCDF%nx
ny = UCDF%ny
nz = UCDF%nz
nt = UCDF%nt
calendar = trim(UCDF%calendar)

domain_west  = max(UCDF%x(1),VCDF%x(1))
domain_east  = min(UCDF%x(nx),VCDF%x(nx))
domain_south = max(UCDF%y(1),VCDF%y(1))
domain_north = min(UCDF%y(ny),VCDF%y(ny))

xcenter = half*(UCDF%x(nx)+UCDF%x(1))
ycenter = half*(UCDF%y(ny)+UCDF%y(1))
coslat  = cos(deg2rad*ycenter)

write(*,*)
write(*,*) 'Input grid:'
write(*,*)
write(*,*) 'nx, ny, nz           = ', nx, ny, nz
write(*,*)
write(*,*) 'Zonal velocity'
write(*,*) 'xo(west),  xo(east)  = ', UCDF%x(1),UCDF%x(nx)
write(*,*) 'yo(south), yo(north) = ', UCDF%y(1),UCDF%y(ny)
write(*,*) 'Meridional velocity'
write(*,*) 'xo(west),  xo(east)  = ', VCDF%x(1),VCDF%x(nx)
write(*,*) 'yo(south), yo(north) = ', VCDF%y(1),VCDF%y(ny)
if (TCDF%defined) then
  write(*,*) 'Temperature/Salinity/Density'
  write(*,*) 'xo(west),  xo(east)  = ', TCDF%x(1),TCDF%x(nx)
  write(*,*) 'yo(south), yo(north) = ', TCDF%y(1),TCDF%y(ny)
endif
write(*,*)
write(*,*) 'Domain'
write(*,*)
write(*,*) 'West                 = ', domain_west
write(*,*) 'East                 = ', domain_east
write(*,*) 'South                = ', domain_south
write(*,*) 'North                = ', domain_north

! ... Passing from degrees to meters. The reference is the
! ... system center: (xcenter,ycenter):
! ... Version 2.0: Just passing to radians !
! ...
UCDF%xm = deg2rad*UCDF%x
UCDF%ym = deg2rad*UCDF%y
VCDF%xm = deg2rad*VCDF%x
VCDF%ym = deg2rad*VCDF%y
if (WCDF%defined) then
  WCDF%xm = deg2rad*WCDF%x
  WCDF%ym = deg2rad*WCDF%y
endif
if (TCDF%defined) then
  TCDF%xm = deg2rad*TCDF%x
  TCDF%ym = deg2rad*TCDF%y
endif

!UCDF%xm = (UCDF%x-xcenter)*deg2m*coslat
!UCDF%ym = (UCDF%y-ycenter)*deg2m
!VCDF%xm = (VCDF%x-xcenter)*deg2m*coslat
!VCDF%ym = (VCDF%y-ycenter)*deg2m
!if (WCDF%defined) then
!  WCDF%xm = (WCDF%x-xcenter)*deg2m*coslat
!  WCDF%ym = (WCDF%y-ycenter)*deg2m
!endif
!if (TCDF%defined) then
!  TCDF%xm = (TCDF%x-xcenter)*deg2m*coslat
!  TCDF%ym = (TCDF%y-ycenter)*deg2m
!endif

!write(*,*)
!write(*,*) 'Coordinates of the central point'
!write(*,*) 'xcenter, ycenter     = ', xcenter, ycenter
!write(*,*) 'cos(ycenter)         = ', coslat

write(*,*)
write(*,*) 'Time axis:'
write(*,*)
write(*,*) 'nt                   = ', nt
write(*,*) 'Time(1)              = ', UCDF%t(1)
write(*,*) 'Time(nt)             = ', UCDF%t(nt)
write(*,*) 'Time(nt)-Time(1)     = ', UCDF%t(nt) - UCDF%t(1)
write(*,*) 'Calendar             = ', trim(calendar)
write(*,*) 'Reference time       = ', UCDF%time_ref
write(*,*) 'Reference date       = ', date_string(UCDF%date_ref)
write(*,*)

if (.not.tscale_flag) then
  write(*,*)
  write(*,*) 'Trying to read time units attribute'
  err = NF90_GET_ATT(UCDF%fid,UCDF%idt,'units',units_att)
  if (err.ne.0) then
    units_att = ''
    write(*,*) 'WARNING: Attribute units not found. Assuming time in seconds'
    tscale = 1.0_dp
    time_units = 'seconds'
  else
    write(*,*) 'Attribute            = ', TRIM(units_att)
    units_att = uppercase(units_att)
    if (index(units_att,'SECOND').GT.0) THEN
      tscale = 1.0_dp
      time_units = 'seconds'
    else if (index(units_att,'MINUT').GT.0) THEN
      tscale = 60.0_dp
      time_units = 'minutes'
    else if (index(units_att,'HOUR').GT.0) THEN
      tscale = 3600.0_dp
      time_units = 'hours'
    else if (index(units_att,'DAY').GT.0) THEN
      tscale = 86400.0_dp
      time_units = 'days'
    else
      write(*,*)
      write(*,*) 'Time units           = ', TRIM(units_att)
      call stop_error(1,'Unknown units')
    endif
  endif

else
  write(*,*)
  write(*,*) 'Time scale provided by user: ', tscale
  if (tscale.eq.1.0_dp) then
   time_units = 'seconds'
  else if (tscale.eq.60.0_dp) then
   time_units = 'minutes'
  else if (tscale.eq.3600.0_dp) then
   time_units = 'hours'
  else if (tscale.eq.86400.0_dp) then
   time_units = 'days'
  else
   time_units = 'unknown'
  endif
endif

write(*,*) 'Time units           = ', TRIM(time_units)
write(*,*) 'Time scale factor    = ', tscale

date_ini%calendar = trim(calendar)
date_fin%calendar = trim(calendar)

time_ini = UCDF%time_ref + tscale*UCDF%t(1)/86400.0_dp
date_ini = jd2date(time_ini)
time_fin = UCDF%time_ref + tscale*UCDF%t(nt)/86400.0_dp
date_fin = jd2date(time_fin)

!%write(*,*) 'Reference time       = ', date_string(date_ref)
write(*,*) 'Date first record    = ', date_string(date_ini)
write(*,*) 'Date last record     = ', date_string(date_fin)

allocate(ellapsed_time(nt))
allocate(dtime(nt-1))
ellapsed_time = tscale*(UCDF%t(:) - UCDF%t(1))
do i=1,nt-1
  dtime(i) = ellapsed_time(i+1) - ellapsed_time(i)
enddo

if (UCDF%nt.eq.1) stationary = .true.

write(*,*)
write(*,*) '*************************************'
if (stationary) then
  write(*,*) '      Streamline calculation'
else
  write(*,*) '      Trajectory calculation'
endif

if (time_direction.lt.0) then
  write(*,*) '  Reverse trajectory calculation'
  lorig = nt
else
  write(*,*) '  Forward trajectory calculation'
  lorig = 1
endif
write(*,*) '*************************************'
write(*,*)

! ... If the variable record has not been initialized (value -1), 
! ... then take the starting point
! ...
if (record.eq.-1) then
  record = lorig  
else
  lorig  = record
endif

! ... Simulation length and time steps:
! ...
if (stationary) then
  if (ftimesim) then
    simulation_length = 86400._dp * simulation_length ! From day to secs
  else
    simulation_length = 86400._dp * 7                 ! seven days
  endif
  if (.not.fedt) external_dt = 86400._dp
  if (.not.fidt) internal_dt =  3600._dp
else
  if (time_direction.gt.0) then
    simulation_length = ellapsed_time(nt) - ellapsed_time(record)
  else
    simulation_length = ellapsed_time(record) - ellapsed_time(1)
  endif
  external_dt = mean(dtime)
  if (.not.fidt) internal_dt = 3600._dp
endif

external_nsteps = int(simulation_length / external_dt )
internal_nsteps = int(external_dt   / internal_dt )

! ... If necessary, change dt sign
! ...
internal_dt = time_direction * internal_dt

write(*,*) 'Simulation length          = ', simulation_length
write(*,*) 'External time step         = ', external_dt
write(*,*) 'External number time steps = ', external_nsteps
write(*,*) 'Internal time step         = ', internal_dt
write(*,*) 'Internal number time steps = ', internal_nsteps

if (UCDF%vel%missingdefined) then
  missing = UCDF%vel%missing_value
endif

write(*,*) 
write(*,*) 'Missing value              = ', missing

! ... Working box
! ...
!if (fwe) then
!  iwest = locate(UGRID0%xu,west) 
!else
!  iwest = 1
!endif
!if (fea) then
!  ieast = min(UGRID0%nx,locate(UGRID0%xu,east)+1)
!else
!  ieast = UGRID0%nx
!endif
!if (fso) then
!  jsouth = locate(UGRID0%yv,south) 
!else
!  jsouth = 1
!endif
!if (fno) then
!  jnorth = min(UGRID0%ny,locate(UGRID0%yv,north)+1)
!else
!  jnorth = UGRID0%ny
!endif
!
!
!nx = ieast - iwest + 1
!ny = jnorth - jsouth + 1
!if (nx.le.0) call stop_error(1,'Invalid working system coordinates')
!if (ny.le.0) call stop_error(1,'Invalid working system coordinates')
!
!write(*,*)
!write(*,*) 'Working region'
!write(*,*) 'nx, ny = ', nx, ny
!write(*,*) 'WEST   = ', iwest,  UGRID0%xu(iwest)
!write(*,*) 'EAST   = ', ieast,  UGRID0%xu(ieast)
!write(*,*) 'SOUTH  = ', jsouth, UGRID0%yv(jsouth)
!write(*,*) 'NORTH  = ', jnorth, UGRID0%yv(jnorth)
!

return
end subroutine clm_ini
! ...
! ==========================================================================
! ...
!subroutine clm_run (UCDF,VCDF,WCDF,TCDF,FLT)
subroutine clm_run ()

use mrhs

!type(cdf_vgrid)                         :: UCDF
!type(cdf_vgrid)                         :: VCDF
!type(cdf_vgrid)                         :: WCDF
!type(cdf_tgrid)                         :: TCDF
!type(floater)                           :: FLT

! ... Local variables:
! ...
logical init,fractional,sss
integer flo,kout,nfloating,nstranded,noutside
integer                                  :: external_step
real(dp)                                 :: external_time
!real(dp)                                 :: internal_time
real(dp)                                 :: initial_time
real(dp)                                 :: fractional_dt
real(dp), dimension(2)                   :: vp,vn

real(dp)                                 :: system_time
type(date_type)                          :: system_date

real(dp) hinterpol,udf
external hinterpol,udf

! ... Common variables (to communicate with rk5.f90):
! ...
real(dp)                                :: velocity_factor
real(dp)                                :: noise_ampl
real(dp)                                :: noise_frac
common/noise/velocity_factor,noise_ampl,noise_frac

!common/internals/internal_time,internal_dt_common

external RHS


!if (UCDF%vel%missingdefined) then
!  missing = UCDF%vel%missing_value
!endif
system_date%calendar = trim(calendar)
FLT%date%calendar    = trim(calendar)

! ... Allocating temporary memory spaces
! ...
allocate(uu(nx,ny,nz))   ! U tmp array
allocate(vv(nx,ny,nz))   ! V tmp array
allocate(ww(nx,ny,nz))   ! W tmp array
allocate(tt(nx,ny,nz))   ! Temp tmp array
allocate(ss(nx,ny,nz))   ! Salt tmp array
allocate(rr(nx,ny,nz))   ! Dens tmp array

! ... Allocating memory space for cubic time interpolation
! ...
allocate(utab(nx,ny,nz,4))
allocate(vtab(nx,ny,nz,4))
allocate(wtab(nx,ny,nz,4))
allocate(ttab(nx,ny,nz,4))
allocate(stab(nx,ny,nz,4))
allocate(rtab(nx,ny,nz,4))

! ... Memory space for cubic time interpolation coeffs
! ...
allocate(ucoef(nx,ny,nz,4))
allocate(vcoef(nx,ny,nz,4))
allocate(wcoef(nx,ny,nz,4))
allocate(tcoef(nx,ny,nz,4))
allocate(scoef(nx,ny,nz,4))
allocate(rcoef(nx,ny,nz,4))

! ... Allocating space for the RK5 intermediate spaces
! ...
allocate(urhs(nx,ny,nz,5))
allocate(vrhs(nx,ny,nz,5))
allocate(wrhs(nx,ny,nz,5))
allocate(trhs(nx,ny,nz,5))
allocate(srhs(nx,ny,nz,5))
allocate(rrhs(nx,ny,nz,5))

write(*,*)
write(*,*)
write(*,*) '==              CLM simulation               =='
write(*,*) '==============================================='

write(*,*)
write(*,*) '     USED_VELOCITY = (FACTOR + MNA*Normal(0,1))*VELOCITY_ORIG + ANA*Normal(0,1)'
write(*,*)
write(*,*) 'Velocity factor, FACTOR  = ', velocity_factor
write(*,*) 'Additive noise amplitude, ANA = ', noise_ampl
write(*,*) 'Multiplicative noise amplitude, MNA = ', noise_frac
write(*,*)

if (FLT%n.eq.0) call stop_error(1,'No floats defined')

external_time = ellapsed_time(record)
initial_time  = external_time

! ... Initialize floats:
! ... Are they released ?
! ... Are they stranded ?
! ... Are inside the domain ?
! ...
do flo=1,FLT%n
  if (FLT%release_time(flo).le.zero) FLT%released(flo) = .true.
  FLT%stranded(flo) = beaching(FLT%lon(flo),FLT%lat(flo))
  FLT%outside(flo)  = outside(FLT%lon(flo),FLT%lat(flo))
  FLT%floating(flo) = .not.FLT%stranded(flo).and..not.FLT%outside(flo)
  print*, flo, FLT%lon(flo), FLT%lat(flo), FLT%released(flo), FLT%floating(flo)
enddo


! ... Save initial conditions:
! ...
do flo=1,FLT%n
  FLT%time(flo) = zero
  if (FLT%released(flo)) then
    vp(1) = FLT%lon(flo)
    vp(2) = FLT%lat(flo)
    if (TCDF%idtemp.gt.0) then
      FLT%temp(flo) = hinterpol(nx,ny,TCDF%xm(:),TCDF%ym(:), &
                                trhs(:,:,1,1),vp(1),vp(2))
    else
      FLT%temp(flo) = missing
    endif
    if (TCDF%idsalt.gt.0) then
      FLT%salt(flo) = hinterpol(nx,ny,TCDF%xm(:),TCDF%ym(:), &
                                srhs(:,:,1,1),vp(1),vp(2))
    else
      FLT%salt(flo) = missing
    endif
    if (TCDF%iddens.gt.0) then
      FLT%dens(flo) = hinterpol(nx,ny,TCDF%xm(:),TCDF%ym(:), &
                                rrhs(:,:,1,1),vp(1),vp(2))
    else
      if (TCDF%idtemp.gt.0.and.TCDF%idsalt.gt.0) then
        FLT%dens(flo) = sigma0(FLT%temp(flo),FLT%salt(flo))
      else
        FLT%dens(flo) = missing
      endif
    endif
    FLT%UDF(flo)  = udf(2,(/FLT%lon(flo),FLT%lat(flo)/))
  else
    FLT%temp(flo) = missing
    FLT%salt(flo) = missing
    FLT%dens(flo) = missing
    FLT%UDF(flo)  = missing
  endif
  FLT%dist(flo) = zero
enddo

kout = 1
FLT%time(:) = internal_time - initial_time - FLT%release_time(:)
system_time = UCDF%time_ref + (tscale*UCDF%t(1)+internal_time)/86400.0_dp
FLT%date = jd2date(system_time)
call out_save(FLT,kout,file_time,system_time)




do external_step=1,external_nsteps
! ---
! ---
  system_time = UCDF%time_ref + (tscale*UCDF%t(1) + external_time)/86400.0_dp
  file_time = UCDF%t(record+time_direction*(external_step-1))

  write(*,*)
  write(*,*) 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  write(*,*) 'External step = ', external_step
  write(*,*) 'FILE Time     = ', file_time
  write(*,*) 'CLM Time      = ', external_time
  write(*,*) 'SYSTEM Time   = ', system_time
  write(*,*) 'SYSTEM Date   =    ', date_string(jd2date(system_time), &
                                                'iso','extended')
  write(*,*) 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

  ! ... Calculating the cubic time interpolation coefficients
  ! ...
  call clm_coeffs

  internal_time = external_time
  do internal_step=1,internal_nsteps

    call clm_rhs

    ! ... Loop over floaters
    ! ...
    do flo=1,FLT%n

      ! ... First, check the float has been released:
      ! ...
      fractional = .false.
      if (FLT%release_time(flo).le.internal_time-initial_time) then
        FLT%released(flo) = .true.
      else 
        sss = FLT%release_time(flo).lt.(internal_time + internal_dt - initial_time)
        if (sss) then
          write(*,*) 'Floater ', flo, ' released in the middle of a time interval'
          fractional = .true.
          FLT%released(flo) = .true.
          fractional_dt = internal_dt - (FLT%release_time(flo)-internal_time) 
          write(*,*) 'Fractional time interval set to ', fractional_dt
        endif
      endif

      if (FLT%released(flo)) then
        ! ... Now, check if the device is floating
        ! ...
        if (FLT%floating(flo)) then

          ! ... The float position in meters:
          ! ... Reference (xcenter,ycenter)
          ! ...
          vp(1) = FLT%lon(flo)*deg2rad
          vp(2) = FLT%lat(flo)*deg2rad

          ! ... Advance the location of the float from the time t to
          ! ... the time t+dt:
          ! ...
          if (fractional) then
            call spherical_rk5(2,vp,FLT%release_time(flo),fractional_dt,vn,RHS)
          else
            call spherical_rk5(2,vp,internal_time,internal_dt,vn,RHS)
          endif

          ! ... Update travelled distance:
          ! ...
          FLT%dist(flo) = FLT%dist(flo) + haversine_rad(vp(1),vp(2),vn(1),vn(2))

          ! ... Update float position
          ! ...
          vp(:)         = vn(:)
          FLT%lon(flo)  = vp(1)*rad2deg
          FLT%lat(flo)  = vp(2)*rad2deg

          FLT%stranded(flo) = beaching(FLT%lon(flo),FLT%lat(flo))
          FLT%outside(flo)  = outside(FLT%lon(flo),FLT%lat(flo))

          if (FLT%outside(flo)) then
            write(*,*) 'Floater ', flo, ' leaving the region'
            FLT%floating(flo) = .false.
            FLT%temp(flo)     =  missing
            FLT%salt(flo)     =  missing
            FLT%dens(flo)     =  missing
            FLT%UDF(flo)      =  missing
          else 
            if (FLT%stranded(flo)) then
              write(*,*) 'Stranded floater ', flo
              FLT%floating(flo) = .false.
              ! ... It retains its last valid temperature, salinity, etc.
            else
              if (TCDF%idtemp.gt.0) then
                FLT%temp(flo) = hinterpol(nx,ny,TCDF%xm(:),TCDF%ym(:), &
                                          trhs(:,:,1,5),vp(1),vp(2))
              else
                FLT%temp(flo) = missing
              endif
              if (TCDF%idsalt.gt.0) then
                FLT%salt(flo) = hinterpol(nx,ny,TCDF%xm(:),TCDF%ym(:), &
                                          srhs(:,:,1,5),vp(1),vp(2))
              else
                FLT%salt(flo) = missing
              endif
              if (TCDF%iddens.gt.0) then
                FLT%dens(flo) = hinterpol(nx,ny,TCDF%xm(:),TCDF%ym(:), &
                                          rrhs(:,:,1,5),vp(1),vp(2))
              else
                if (TCDF%idtemp.gt.0.and.TCDF%idsalt.gt.0) then
                  FLT%dens(flo) = sigma0(FLT%temp(flo),FLT%salt(flo))
                else
                  FLT%dens(flo) = missing
                endif
              endif
              FLT%UDF(flo)  = udf(2,(/FLT%lon(flo),FLT%lat(flo)/))
            endif
          endif

        endif

      endif

    enddo     ! End loop over floats

!    do flo=1,FLT%n
!      if (FLT%released(flo)) then
!        if (FLT%floating(flo)) then
!          nfloating = nfloating + 1
!        endif
!        if (FLT%stranded(flo)) then
!          nstranded = nstranded + 1
!        endif
!        if (FLT%outside(flo)) then
!          noutside = noutside + 1
!        endif
!      endif
!    enddo

    internal_time = internal_time + internal_dt
    FLT%time(:) = internal_time - initial_time - FLT%release_time(:)

    nfloating = 0
    nstranded = 0
    noutside  = 0
    do flo=1,FLT%n
      ! ... Check if status of the float needs to be changed before writing down
      if (abs(FLT%release_time(flo)-(internal_time-initial_time)).le.0.1) then
        FLT%released(flo) = .true.
      endif
      if (FLT%released(flo)) then
        if (FLT%floating(flo)) then
          nfloating = nfloating + 1
        endif
        if (FLT%stranded(flo)) then
          nstranded = nstranded + 1
        endif
        if (FLT%outside(flo)) then
          noutside = noutside + 1
        endif
      endif
    enddo
    write(*,'("Step: ",I4.4,". Floats released: ",I4.4,", floating: ",I4.4,", stranded: ",I4.4,", outside: ", I4.4)') &
       internal_step, count(FLT%released), nfloating, nstranded, noutside


    system_time = UCDF%time_ref + (tscale*UCDF%t(1)+internal_time)/86400.0_dp
    FLT%date = jd2date(system_time)

    kout = kout + 1
    !call out_save(FLT,kout,system_time)
    call out_save(FLT,kout,file_time+internal_step*internal_dt/external_dt, &
                  system_time)

  enddo       ! End internal loop

  external_time = external_time + time_direction*external_dt

enddo


contains

  subroutine clm_coeffs

  ! ... Local variables
  ! ...
  integer k,kk

  ! -------------------
  if (stationary) then
  ! -------------------

    if (external_step.eq.1) then
      write(*,*) 
      write(*,*) 'Filling RHS arrays for stationary case'
      call read_fields(record)
      do kk=1,5
        urhs(:,:,:,kk) = uu(:,:,:)
        vrhs(:,:,:,kk) = vv(:,:,:)
        wrhs(:,:,:,kk) = ww(:,:,:)
        trhs(:,:,:,kk) = tt(:,:,:)
        srhs(:,:,:,kk) = ss(:,:,:)
        rrhs(:,:,:,kk) = rr(:,:,:)
      enddo
    endif

  ! -------------------
  else
  ! -------------------

    if (external_step.eq.1) then

      records = (/lorig,lorig,lorig+time_direction,lorig+2*time_direction/)
      do kk=2,4
        call read_fields(records(kk))
        utab(:,:,:,kk) = uu(:,:,:)
        vtab(:,:,:,kk) = vv(:,:,:)
        wtab(:,:,:,kk) = ww(:,:,:)
        ttab(:,:,:,kk) = tt(:,:,:)
        stab(:,:,:,kk) = ss(:,:,:)
        rtab(:,:,:,kk) = rr(:,:,:)
      enddo
      print*, 'Linear interpolation'
      utab(:,:,:,1) = two*utab(:,:,:,2) - utab(:,:,:,3)
      vtab(:,:,:,1) = two*vtab(:,:,:,2) - vtab(:,:,:,3)
      wtab(:,:,:,1) = two*wtab(:,:,:,2) - wtab(:,:,:,3)
      ttab(:,:,:,1) = two*ttab(:,:,:,2) - ttab(:,:,:,3)
      stab(:,:,:,1) = two*stab(:,:,:,2) - stab(:,:,:,3)
      rtab(:,:,:,1) = two*rtab(:,:,:,2) - rtab(:,:,:,3)

    else

      do kk=1,3
        records(kk) = records(kk+1)
        utab(:,:,:,kk) = utab(:,:,:,kk+1)
        vtab(:,:,:,kk) = vtab(:,:,:,kk+1)
        wtab(:,:,:,kk) = wtab(:,:,:,kk+1)
        ttab(:,:,:,kk) = ttab(:,:,:,kk+1)
        stab(:,:,:,kk) = stab(:,:,:,kk+1)
        rtab(:,:,:,kk) = rtab(:,:,:,kk+1)
      enddo
      if (external_step.lt.external_nsteps) then
        kk = 4
        records(kk) = records(kk) + time_direction
        call read_fields(records(kk))
        utab(:,:,:,kk) = uu(:,:,:)
        vtab(:,:,:,kk) = vv(:,:,:)
        wtab(:,:,:,kk) = ww(:,:,:)
        ttab(:,:,:,kk) = tt(:,:,:)
        stab(:,:,:,kk) = ss(:,:,:)
        rtab(:,:,:,kk) = rr(:,:,:)
      else
        print*, 'Linear interpolation'
        utab(:,:,:,4) = two*utab(:,:,:,3) - utab(:,:,:,2)
        vtab(:,:,:,4) = two*vtab(:,:,:,3) - vtab(:,:,:,2)
        wtab(:,:,:,4) = two*wtab(:,:,:,3) - wtab(:,:,:,2)
        ttab(:,:,:,4) = two*ttab(:,:,:,3) - ttab(:,:,:,2)
        stab(:,:,:,4) = two*stab(:,:,:,3) - stab(:,:,:,2)
        rtab(:,:,:,4) = two*rtab(:,:,:,3) - rtab(:,:,:,2)
      endif
    endif

    ! ... At this point, we have four records:
    ! ... in utab(:,:,1:4) and vtab(:,:,1:4).
    ! ... Get the coefficients for the cubic interpolation in time
    ! ... The coefficients are valid for the range [0,1]. it tt in [0,1],
    ! ... the physical time in seconds is
    ! ...                                  timetab(2) + tt*(timetab(3)-timetab(2))
    ! ...
    print*, 'records : ', records

    do k=1,nz
      call i3coeffs (UCDF%land(:,:,k),utab(:,:,k,1:4),ucoef(:,:,k,1:4))
      call i3coeffs (VCDF%land(:,:,k),vtab(:,:,k,1:4),vcoef(:,:,k,1:4))
    enddo

    wcoef(:,:,:,:) = zero
    if (WCDF%defined) then
      do k=1,nz
        call i3coeffs (WCDF%land(:,:,k),wtab(:,:,k,1:4),wcoef(:,:,k,1:4))
      enddo
    endif

    tcoef(:,:,:,:) = zero
    scoef(:,:,:,:) = zero
    rcoef(:,:,:,:) = zero
    if (TCDF%defined) then
      do k=1,nz
        if (TCDF%idtemp.gt.0) call i3coeffs (TCDF%land(:,:,k),ttab(:,:,k,1:4),tcoef(:,:,k,1:4))
        if (TCDF%idsalt.gt.0) call i3coeffs (TCDF%land(:,:,k),stab(:,:,k,1:4),scoef(:,:,k,1:4))
        if (TCDF%iddens.gt.0) call i3coeffs (TCDF%land(:,:,k),rtab(:,:,k,1:4),rcoef(:,:,k,1:4))
      enddo
    endif

  ! -------------------
  endif
  ! -------------------

  end subroutine clm_coeffs
  ! ...
  ! ========================================================================
  ! ...
  subroutine clm_rhs()

  integer i,j,k,kk
  real(dp) trk,tf

  ! ... In the case of a stationary simulation, the Xrhs arrays
  ! ... are filled in subroutine clm_coeffs
  ! ...
  if (.not.stationary) then
    do kk=1,5
      trk = internal_time + 0.25_dp*(kk-1)*internal_dt 
      tf = abs((trk-external_time)/external_dt)
      do j=1,ny
      do i=1,nx
        do k=1,nz
          urhs(i,j,k,kk) = cubic(ucoef(i,j,k,:),tf)
          vrhs(i,j,k,kk) = cubic(vcoef(i,j,k,:),tf)
          wrhs(i,j,k,kk) = cubic(wcoef(i,j,k,:),tf)
          trhs(i,j,k,kk) = cubic(tcoef(i,j,k,:),tf)
          srhs(i,j,k,kk) = cubic(scoef(i,j,k,:),tf)
          rrhs(i,j,k,kk) = cubic(rcoef(i,j,k,:),tf)
        enddo
      enddo
      enddo
    enddo
  endif

  end subroutine clm_rhs
  ! ...
  ! ========================================================================
  ! ...
  subroutine read_fields (kstep)

  integer, intent(in)                             :: kstep

  call get_data(UCDF%vel,kstep)
  uu = UCDF%vel%data

  call get_data(VCDF%vel,kstep)
  vv = VCDF%vel%data

  if (WCDF%vel%id.gt.0) then
    call get_data(WCDF%vel,kstep)
    ww = WCDF%vel%data
  else
    ww = zero
  endif

  if (TCDF%temp%id.gt.0) then
    call get_data(TCDF%temp,kstep)
    tt = TCDF%temp%data
    call fillcoast(nx,ny,TCDF%land,TCDF%sea,tt)
  else
    tt = zero
  endif

  if (TCDF%salt%id.gt.0) then
    call get_data(TCDF%salt,kstep)
    ss = TCDF%salt%data
    call fillcoast(nx,ny,TCDF%land,TCDF%sea,ss)
  else
    ss = zero
  endif

  if (TCDF%dens%id.gt.0) then
    call get_data(TCDF%dens,kstep)
    rr = TCDF%dens%data
    call fillcoast(nx,ny,TCDF%land,TCDF%sea,rr)
  else
    rr = zero
  endif

  end subroutine read_fields
  ! ...
  ! ========================================================================
  ! ...
  subroutine get_data (u,l) 

  type(field), intent(inout)                      :: u  
  integer, intent(in)                             :: l  

  integer err
  integer po(u%ndims), pf(u%ndims)

  po(:) = 1
  pf(:) = 1
  if (u%ppl.gt.0) po(u%ppl) = l
  if (u%ppi.gt.0) pf(u%ppi) = u%nx
  if (u%ppj.gt.0) pf(u%ppj) = u%ny
  if (u%ppk.gt.0) pf(u%ppk) = u%nz
  
  err = NF90_GET_VAR(u%fid,u%id,u%data,po,pf)
  call cdf_error(err,'Unable to read field in get_data')

  if (u%missingisnan) then
    where(isnan(u%data))
      u%data = zero
    elsewhere
      u%data = u%add_offset + u%scale_factor*u%data
    endwhere
  else
    where((u%data.eq.u%missing_value))
      u%data = zero
    elsewhere
      u%data = u%add_offset + u%scale_factor*u%data
    endwhere
  endif

  end subroutine get_data
  ! ...
  ! ========================================================================
  ! ...
  subroutine i3coeffs (land,uu,cc)

  logical, dimension(nx,ny), intent(in)      :: land
  real(dp), dimension(nx,ny,4), intent(in)   :: uu
  real(dp), dimension(nx,ny,4), intent(out)  :: cc

  ! ... Local coefficients
  ! ...
  integer i,j

  do j=1,ny
  do i=1,nx
    if (land(i,j)) then
      cc(i,j,:) = zero
    else
      cc(i,j,1) = uu(i,j,2)
      cc(i,j,2) = half*(uu(i,j,3)-uu(i,j,1))
      cc(i,j,3) = uu(i,j,1) - 2.5D0*uu(i,j,2) + two*uu(i,j,3) - half*uu(i,j,4)
      cc(i,j,4) = -half*uu(i,j,1) + 1.5D0*(uu(i,j,2)-uu(i,j,3)) + half*uu(i,j,4)
    endif
  enddo
  enddo

  end subroutine i3coeffs
  ! ...
  ! ==========================================================================
  ! ...
  logical function outside(xo,yo)

  real(dp), intent(in)                    :: xo,yo

  outside = .true.
  if (xo.le.domain_west) return
  if (xo.ge.domain_east) return
  if (yo.le.domain_south) return
  if (yo.ge.domain_north) return

  outside = .false.

  end function outside
  ! ...
  ! ==========================================================================
  ! ...
  logical function beaching(xo,yo)

  real(dp), intent(in)                    :: xo,yo

  ! ... Local variables:
  ! ...
  logical Ubeach,Vbeach
  integer iref,jref
  integer io,jo
  real(dp) d(4)
  integer imin(1)


  ! ... By respect the U grid
  ! ........................................................
  iref = locate(UCDF%x,xo)
  jref = locate(UCDF%y,yo)

  d(1) = (UCDF%x(iref)-xo)**2   + (UCDF%y(jref)-yo)**2
  d(2) = (UCDF%x(iref+1)-xo)**2 + (UCDF%y(jref)-yo)**2
  d(3) = (UCDF%x(iref+1)-xo)**2 + (UCDF%y(jref+1)-yo)**2
  d(4) = (UCDF%x(iref)-xo)**2   + (UCDF%y(jref+1)-yo)**2

  imin = minloc(d)
  select case (imin(1))
  case (1)
    io = iref
    jo = jref
  case (2)
    io = iref + 1
    jo = jref
  case (3)
    io = iref + 1
    jo = jref + 1
  case (4)
    io = iref
    jo = jref + 1
  end select

  Ubeach = UCDF%land(io,jo,1)

  ! ... By respect the V grid
  ! ........................................................
  iref = locate(VCDF%x,xo)
  jref = locate(VCDF%y,yo)

  d(1) = (VCDF%x(iref)-xo)**2   + (VCDF%y(jref)-yo)**2
  d(2) = (VCDF%x(iref+1)-xo)**2 + (VCDF%y(jref)-yo)**2
  d(3) = (VCDF%x(iref+1)-xo)**2 + (VCDF%y(jref+1)-yo)**2
  d(4) = (VCDF%x(iref)-xo)**2   + (VCDF%y(jref+1)-yo)**2

  imin = minloc(d)
  select case (imin(1))
  case (1)
    io = iref
    jo = jref
  case (2)
    io = iref + 1
    jo = jref
  case (3)
    io = iref + 1
    jo = jref + 1
  case (4)
    io = iref
    jo = jref + 1
  end select

  Vbeach = VCDF%land(io,jo,1)

  beaching = Ubeach.and.Vbeach

  end function beaching
  ! ...
  ! ==========================================================================
  ! ...
  pure function cubic(a,t) result(f)

  real(dp), dimension(4), intent(in)   :: a
  real(dp), intent(in)                 :: t
  real(dp)                             :: f

  f = a(1) + t*(a(2)+t*(a(3)+t*a(4)))

  end function cubic
  ! ...
  ! ==========================================================================
  ! ...
end subroutine clm_run

end module clm
