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

logical                                  :: first = True
logical                                  :: check_time = True
logical                                  :: check_equal_time = True

! ... Forward/Backward integration
! ... Default: Forward
integer                                  :: lorig          = 1
integer                                  :: time_direction = 1 ! +1, -1
logical                                  :: reverse
logical                                  :: run_udf        = .false.

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
real(dp)                                 :: time_initial = -1 ! = U%t(1)
real(dp)                                 :: time_final   = -1 ! = U%t(U%nt)
real(dp)                                 :: simulation_length
real(dp)                                 :: external_dt
real(dp)                                 :: internal_dt
real(dp)                                 :: file_time

! ... Stationnary simulation
! ...
logical                                  :: stationary    = .false.

! ... Degree to meters conversion
! ... Simple esphere
real(dp), parameter                      :: deg2m = deg2rad*Rearth  ! deg -> m
real(dp), parameter                      :: m2deg = one/deg2m       ! m -> deg

! ... Lon/Lat or XY grid
! ...
logical                                  :: Fxy      = .false.
logical                                  :: Flonlat  = .true.
real(dp)                                 :: Uscale   = one
real(dp)                                 :: Vscale   = one

! ... Forcing precious and following time steps and time value
! ...
integer                                  :: Ukprev,Uknext
integer                                  :: Vkprev,Vknext
integer                                  :: Wkprev,Wknext
integer                                  :: Tkprev,Tknext
integer                                  :: Skprev,Sknext
integer                                  :: Akprev,Aknext
real(dp)                                 :: Utprev,Utnext
real(dp)                                 :: Vtprev,Vtnext
real(dp)                                 :: Wtprev,Wtnext
real(dp)                                 :: Ttprev,Ttnext
real(dp)                                 :: Stprev,Stnext
real(dp)                                 :: Atprev,Atnext

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
!integer                                  :: nz,nt

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
integer                                  :: Nsteps = 0
real(dp), dimension(:,:,:), pointer      :: uu,vv,ww,tt,ss
real(dp), dimension(:,:,:), pointer      :: aa,bb

! ... Cubic time interpolation
! ... For U,V,W,A
! ...
integer, dimension(4)                    :: records
real(dp), dimension(:,:,:,:), pointer    :: Utab,Vtab,Wtab
real(dp), dimension(:,:,:,:), pointer    :: Atab,Btab
real(dp), dimension(:,:,:,:), pointer    :: Ucoef,Vcoef,Wcoef
!real(dp), dimension(:,:,:,:), pointer    :: tcoef,scoef
real(dp), dimension(:,:,:,:), pointer    :: Acoef,Bcoef

! ... Lineal time interpolation
! ... For T,S
! ...
real(dp), dimension(:,:,:,:), pointer    :: Ttab,Stab

! ... Runge Kutta
! ...
integer                                  :: RKORDER = 5
integer                                  :: RKNSTEPS = 5

! ... Those fields represent the values at any given time
! ... they can be constant or come from a cubic interpolation
! ...
real(dp), dimension(:,:,:,:), pointer   :: Urhs,Vrhs,Wrhs
real(dp), dimension(:,:,:,:), pointer   :: Arhs,Brhs

! ... Working box
! ...
logical                                 :: fso    = .false.  ! Flg south lim
logical                                 :: fno    = .false.  ! Flg north lim
logical                                 :: fea    = .false.  ! Flg west  lim
logical                                 :: fwe    = .false.  ! Flg east  lim
logical                                 :: fkz    = .false.  ! Flg east  lim
integer                                 :: iwest  = 1
integer                                 :: ieast  = 1
integer                                 :: jsouth = 1
integer                                 :: jnorth = 1
real(dp)                                :: selected_south = nan
real(dp)                                :: selected_north = nan
real(dp)                                :: selected_west  = nan
real(dp)                                :: selected_east  = nan
real(dp)                                :: selected_depth = nan

real(dp)                                :: domain_west
real(dp)                                :: domain_east
real(dp)                                :: domain_south
real(dp)                                :: domain_north
real(dp)                                :: domain_top
real(dp)                                :: domain_bottom

! ... Interpolation method
! ...
character(len=16)                       :: METHOD = 'LOCALSPLINES'


type field
  ! This structure contains the necessary information for a
  ! variable to be used
  logical                               :: set            = .False.
  logical                               :: missingdefined = .False.
  logical                               :: missingisnan   = .False.
  integer                               :: idv=-1
  integer                               :: io,if
  integer                               :: jo,jf
  integer                               :: ko,kf
  integer                               :: lo,lf
  integer                               :: nx
  integer                               :: ny
  integer                               :: nz
  integer                               :: nt
  integer                               :: l
  integer                               :: lorig  = 1
  integer                               :: record = -1
  integer, dimension(4)                 :: records
  real(dp)                              :: domain_west
  real(dp)                              :: domain_east
  real(dp)                              :: domain_south
  real(dp)                              :: domain_north
  real(dp)                              :: domain_top
  real(dp)                              :: domain_bottom
  real(dp)                              :: west
  real(dp)                              :: east
  real(dp)                              :: south
  real(dp)                              :: north
  real(dp)                              :: add_offset
  real(dp)                              :: scale_factor
  real(dp)                              :: missing_value
  real(dp)                              :: time_ref
  real(dp)                              :: tscale
  real(dp), dimension(:), pointer       :: x,y,z,t
  real(dp), dimension(:), pointer       :: jd            ! JD
  real(dp), dimension(:), pointer       :: jds           ! JD in secs
  type(date_type), dimension(:), pointer:: dates
  character(len=80)                     :: xname,yname,zname,tname
  character(len=280)                    :: filename = ''
  character(len=80)                     :: varname  = ''
  character(len=80)                     :: calendar = ''
  logical, dimension(:,:,:), pointer    :: land
  logical, dimension(:,:,:), pointer    :: sea
  real(dp), dimension(:,:,:), pointer   :: data         ! Field
  type(gcdf)                            :: icdf
  real(dp)                              :: time
  type(date_type)                       :: date
end type field

! ==========================================================================
! ==========================================================================

contains

subroutine file_open(F,filename,varname)

implicit none

type(field), intent(inout)               :: F
character(len=*), intent(in)             :: filename
character(len=*), intent(in)             :: varname

integer                                  :: err,ndims,idv,i,j
integer                                  :: fid
real(dp)                                 :: time,time_ref
type(date_type)                          :: date_ref
real(dp), dimension(:), allocatable      :: xx
character(len=80)                        :: units_att


! ... Open file
! ...
if (len_trim(F%xname).gt.0) F%icdf%xname = trim(F%xname)
if (len_trim(F%yname).gt.0) F%icdf%yname = trim(F%yname)
if (len_trim(F%zname).gt.0) F%icdf%zname = trim(F%zname)
if (len_trim(F%tname).gt.0) F%icdf%tname = trim(F%tname)

call gcdf_open (filename,F%icdf,err)
if (err.NE.0) call stop_error(1,'Error reading file')

fid        = F%icdf%fid
F%filename = trim(filename)
F%varname  = trim(varname)

! ... Get the ID of the selected varible 
! ...
if (len_trim(varname).eq.0) then
  call stop_error(1,'Variable name required')
endif
err = NF90_INQ_VARID (fid,varname,F%idv)
call cdf_error(err,'Variable '//trim(varname)//' not found')

write(*,*)
write(*,*) 'Input Netcdf grid'
write(*,*) 'Variable name         = ', trim(varname)
write(*,*) 'Variable Id           = ', F%idv
write(*,*) 'idx                   = ', F%icdf%idx
write(*,*) 'idy                   = ', F%icdf%idy
write(*,*) 'idz                   = ', F%icdf%idz
write(*,*) 'idt                   = ', F%icdf%idt
write(*,*) 'nx                    = ', F%icdf%nx 
write(*,*) 'ny                    = ', F%icdf%ny 
write(*,*) 'nz                    = ', F%icdf%nz  
write(*,*) 'nt                    = ', F%icdf%nt 

allocate(xx(F%icdf%nx))
ndims = F%icdf%vndims(F%icdf%idx)
if (ndims.eq.1) then
  err = NF90_GET_VAR (fid,F%icdf%idx,xx)
  call cdf_error(err,'Error reading X variable')
else
  write(*,*) 'WARNING: X variable has more than one dimension'
  write(*,*) 'WARNING: using first dimension'
  err = NF90_GET_VAR (fid,F%icdf%idx,xx,(/1,1/),(/F%icdf%nx,1/))
  call cdf_error(err,'Error reading Y variable')
endif


if (fwe) then
  if (selected_west.lt.xx(1)) then
    write(*,*) 'WARNING: SELECTED_WEST edge outside file domain. IO = 1'
  endif
  F%io = max(locate(xx,selected_west),1)
else
  F%io = 1
endif
if (fea) then
  if (selected_east.gt.xx(F%icdf%nx)) then
    write(*,*) 'WARNING: SELECTED_EAST edge outside file domain. IF = NX'
  endif
  F%if = min(locate(xx,selected_east)+1,F%icdf%nx)
else
  F%if = F%icdf%nx
endif
F%nx = F%if - F%io + 1
allocate(F%x(F%nx))
F%x(:) = xx(F%io:F%if)
F%domain_west = xx(1)
F%domain_east = xx(F%icdf%nx)
F%west = F%x(1)
F%east = F%x(F%nx)
deallocate(xx)


allocate(xx(F%icdf%ny))
ndims = F%icdf%vndims(F%icdf%idy)
if (ndims.eq.1) then
  err = NF90_GET_VAR (fid,F%icdf%idy,xx)
  call cdf_error(err,'Error reading Y variable')
else
  write(*,*) 'WARNING: Y variable has more than one dimension'
  write(*,*) 'WARNING: using second dimension'
  err = NF90_GET_VAR (fid,F%icdf%idy,xx,(/1,1/),(/1,F%icdf%ny/))
  call cdf_error(err,'Error reading Y variable')
endif

if (fso) then
  if (selected_south.lt.xx(1)) then
    write(*,*) 'WARNING: SELECTED_SOUTH edge outside file domain. JO = 1'
  endif
  F%jo = max(locate(xx,selected_south),1)
else
  F%jo = 1
endif
if (fno) then
  if (selected_south.lt.xx(1)) then
    write(*,*) 'WARNING: SELECTED_NORTH edge outside file domain. JF = NY'
  endif
  F%jf = min(locate(xx,selected_north)+1,F%icdf%ny)
else
  F%jf = F%icdf%ny
endif
F%ny = F%jf - F%jo + 1
allocate(F%y(F%ny))
F%y(:) = xx(F%jo:F%jf)
F%domain_south = xx(1)
F%domain_north = xx(F%icdf%ny)
F%south = F%y(1)
F%north = F%y(F%ny)
deallocate(xx)

! ... Vertical axis
! ... Assumed units: meters
! ...
if (F%icdf%idz > -1) then
  allocate(xx(F%icdf%nz))
  err = NF90_GET_VAR (fid,F%icdf%idz,xx)
  call cdf_error(err,'Error reading Z variable')
  if (fkz) then
    if (selected_depth.le.xx(1)) then
      F%nz = 1
    else if (selected_depth.ge.xx(F%icdf%nz)) then
      F%nz = F%icdf%nz
    else 
      F%nz = locate(xx,selected_depth) + 1
    endif
  else
    F%nz = F%icdf%nz
  endif
  F%ko = 1
  F%kf = F%nz
  F%domain_top    = xx(F%ko)
  F%domain_bottom = xx(F%kf)
  allocate(F%z(F%nz))
  F%z(:) = xx(1:F%nz)
  deallocate(xx)
else
  F%ko = 1
  F%kf = 1
  F%nz = 1
  allocate(F%z(1))
  F%z(1) = zero
  F%domain_top    = zero
  F%domain_bottom = zero
endif


! ... Time axis:
! ...
if (F%icdf%idt.gt.-1) then


  if (.not.tscale_flag) then
    write(*,*)
    write(*,*) 'Trying to read time units attribute'
    err = NF90_GET_ATT(fid,F%icdf%idt,'units',units_att)
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

  ! ... Reference date:
  ! ... time_ref: Julian Days offset
  ! ...
  err = cdf_timeref(fid,F%icdf%idt,date_ref)  ! -> date_ref (DATE_TYPE)
  if (err.eq.0) then
    time_ref = date2jd(date_ref)
  else
    time_ref = zero
  endif

  allocate(xx(F%icdf%nt))
  err = NF90_GET_VAR (fid,F%icdf%idt,xx)
  call cdf_error(err,'Error reading Time variable')

  ! ... Select time period according to the options
  ! ... 

  if (first) then
  ! ... First file being read.
  ! ... The user may decide to start in a given record !!!!
  ! ...
    if (.not.reverse) then 
      if (record.lt.0) then
        F%lo = 1
      else
        F%lo = min(record,F%icdf%nt)
      endif
      if (stationary) then
        F%lf = F%lo
      else
        F%lf = F%icdf%nt
      endif
    else
      if (record.lt.0) then
        F%lf = F%icdf%nt
      else
        F%lf = max(record,1)
      endif
      if (stationary) then
        F%lo = F%lf
      else
        F%lo = 1
      endif
    endif

    F%nt = F%lf - F%lo + 1
    allocate(F%t(F%nt))
    allocate(F%dates(F%nt))
    allocate(F%jd(F%nt))
    allocate(F%jds(F%nt))
    F%t(:) = xx(F%lo:F%lf)
    do i=1,F%nt
      time = time_ref + tscale*F%t(i)/86400.0_dp
      F%jd(i)    = time                                   ! -> JD
      F%jds(i)   = time_ref*86400.0_dp + tscale*F%t(i)    ! -> SECONDS
      F%dates(i) = jd2date(time)                          ! -> DATE_TYPE
    enddo

    first = False
    time_initial = F%jd(1)
    time_final   = F%jd(F%nt)

  else
  ! ... We open a second file. If time_initial > 0, then we use
  ! ... this axis to 
  ! ...
    if (check_equal_time) then
    ! ... 
      do i=1,F%icdf%nt
        time = time_ref + tscale*xx(i)/86400.0_dp
        if (abs(time-time_initial).lt.1D-5) exit
      enddo
      if (i.gt.F%icdf%nt) call stop_error(1,'Incompatible time axis')
      F%lo = i
      do i=F%icdf%nt,1,-1
        time = time_ref + tscale*xx(i)/86400.0_dp
        if (abs(time-time_final).lt.1D-5) exit
      enddo
      if (i.lt.1) call stop_error(1,'Incompatible time axis')
      F%lf = i
    else if (check_time) then
      do i=1,F%icdf%nt
        time = time_ref + tscale*xx(i)/86400.0_dp
        if (time.gt.time_initial) exit
      enddo
      if (i.gt.F%icdf%nt) call stop_error(1,'Incompatible time axis')
      F%lo = max(1,i-1)
      do i=F%icdf%nt,1,-1
        time = time_ref + tscale*xx(i)/86400.0_dp
        if (time.lt.time_final) exit
      enddo
      if (i.eq.0) call stop_error(1,'Incompatible time axis')
      F%lf = min(F%icdf%nt,i+1)
    else
      F%lo = 1
      F%lf = F%icdf%nt
    endif
      
    F%nt = F%lf - F%lo + 1
    allocate(F%t(F%nt))
    allocate(F%jd(F%nt))
    allocate(F%jds(F%nt))
    allocate(F%dates(F%nt))
    F%t(:) = xx(F%lo:F%lf)
    do i=1,F%nt
      time = time_ref + tscale*F%t(i)/86400.0_dp
      F%jd(i)    = time
      F%jds(i)   = time_ref*86400.0_dp + tscale*F%t(i)
      F%dates(i) = jd2date(time)
    enddo

  endif
else
  F%lo       = 1
  F%lf       = 1
  F%nt       = 1
  allocate(F%t(F%nt))
  allocate(F%jd(F%nt))
  allocate(F%jds(F%nt))
  allocate(F%dates(F%nt))
  F%t(1)     = zero
  F%jd(1)    = zero
  F%jds(1)   = zero
  F%dates(1) = date_today()
endif

F%time_ref = time_ref
F%tscale   = tscale

write(*,*)
write(*,*) 'Data grid'
write(*,*) 'Domain west, east     = ', F%domain_west,F%domain_east
write(*,*) 'Domain south, north   = ', F%domain_south,F%domain_north
write(*,*) 'Data west, east       = ', F%west,F%east
write(*,*) 'Data south, north     = ', F%south,F%north
write(*,*) 'io, if                = ', F%io,F%if
write(*,*) 'jo, jf                = ', F%jo,F%jf
write(*,*) 'ko, kf                = ', F%ko,F%kf
write(*,*) 'lo, lf                = ', F%lo,F%lf
write(*,*) 'nx                    = ', F%nx
write(*,*) 'ny                    = ', F%ny
write(*,*) 'nz                    = ', F%nz
write(*,*) 'nt                    = ', F%nt
write(*,*) 'Initial date          = ', date_string(F%dates(1))
write(*,*) 'Last date             = ', date_string(F%dates(F%nt))

F%add_offset   = F%icdf%add_offset(F%idv)
F%scale_factor = F%icdf%scale_factor(F%idv)

if (F%icdf%missing(F%idv)) then
  F%missingdefined = .true.
  F%missing_value  = F%icdf%missing_value(F%idv)
  F%missingisnan   = F%icdf%missingisnan(F%idv)
else if (F%icdf%fill(F%idv)) then
  F%missingdefined = .true.
  F%missing_value  = F%icdf%fill_value(F%idv)
  F%missingisnan   = F%icdf%missingisnan(F%idv)
else
  F%missingdefined = fmv
  F%missing_value  = missing
  F%missingisnan   = .false.
endif

write(*,*) 'Variable id           = ', F%idv
write(*,*) 'Value offset          = ', F%add_offset
write(*,*) 'Value scale_factor    = ', F%scale_factor
write(*,*) 'Missing value defined = ', F%missingdefined
write(*,*) 'Missing value         = ', F%missing_value
write(*,*) 'Missing is nan        = ', F%missingisnan

allocate(F%data(F%nx,F%ny,F%nz))
allocate(F%land(F%nx,F%ny,F%nz))
allocate(F%sea(F%nx,F%ny,F%nz))
call get_land(F)
F%sea = .not.F%land
F%set = .True.
!do j=F%ny,1,-1
!  write(*,'(80L)') (F%land(i,j,1),i=1,80)
!enddo

end subroutine file_open
! ...
! =========================================================================
! ...
function read_field(F,k)  result(ff)

type(field), intent(in)                        :: F
integer, intent(in)                            :: k
real(dp), dimension(F%nx,F%ny,F%nz)            :: ff

! ... Local variables
! ...
integer err,ppi,ppj,ppk,ppl
integer po(F%icdf%vndims(F%idv)), pf(F%icdf%vndims(F%idv))

ppi = F%icdf%ppi(F%idv)
ppj = F%icdf%ppj(F%idv)
ppk = F%icdf%ppk(F%idv)
ppl = F%icdf%ppl(F%idv)

po(:) = 1
pf(:) = 1
if (ppi.gt.0) then
  po(ppi) = F%io
  pf(ppi) = F%nx
endif
if (ppj.gt.0) then
  po(ppj) = F%jo
  pf(ppj) = F%ny
endif
if (ppk.gt.0) then
  po(ppk) = F%ko
  pf(ppk) = F%nz
endif
if (ppl.gt.0) then
  po(ppl) = k
  pf(ppl) = 1
endif

err = NF90_GET_VAR (F%icdf%fid,F%idv,ff,po,pf)
call cdf_error(err,'Unable to read field')

if (F%missingisnan) then
  where(isnan(ff))
    ff = zero
  elsewhere
    ff = F%add_offset + F%scale_factor*ff
  endwhere
else
  where((ff.eq.F%missing_value))
    ff = zero
  elsewhere
    ff = F%add_offset + F%scale_factor*ff
  endwhere
endif


end function read_field
! ...
! =========================================================================
! ...
subroutine get_land(u)

type(field), intent(inout)                     :: u

! ... Local variables
! ...
integer err,ppi,ppj,ppk,ppl
integer po(u%icdf%vndims(u%idv)), pf(u%icdf%vndims(u%idv))

ppi = u%icdf%ppi(u%idv)
ppj = u%icdf%ppj(u%idv)
ppk = u%icdf%ppk(u%idv)
ppl = u%icdf%ppl(u%idv)

po(:) = 1
pf(:) = 1
if (ppi.gt.0) then
  po(ppi) = u%io
  pf(ppi) = u%nx
endif
if (ppj.gt.0) then
  po(ppj) = u%jo
  pf(ppj) = u%ny
endif
if (ppk.gt.0) then
  po(ppk) = u%ko
  pf(ppk) = u%nz
endif

! ... Read first register (if time is defined)
! ...
if (ppl.gt.0) then
  po(ppl) = 1
  pf(ppl) = 1
endif

err = NF90_GET_VAR (u%icdf%fid,u%idv,U%data,po,pf)
call cdf_error(err,'Unable to read field for mask calculation')

u%land = .false.
if (u%missingisnan) then
  where(isnan(u%data)) u%land = .true.
else
  where(u%data.eq.u%missing_value) u%land = .true.
endif

end subroutine get_land
! ...
! ==========================================================================
! ...
subroutine clm_ini(U,V,W,T,S,A,B)

type(field)                         :: U
type(field)                         :: V
type(field)                         :: W
type(field)                         :: T
type(field)                         :: S
type(field)                         :: A
type(field)                         :: B

! ... Local variables:
! ...
integer err,i


write(*,*)
write(*,*)
write(*,*) '==            CLM initialization             =='
write(*,*) '==============================================='
 

write(*,*)
write(*,*) 'Zonal velocity field       : ', trim(U%varname)
write(*,*) 'Meridional velocity field  : ', trim(V%varname)
if (W%set) write(*,*) 'Vertical velocity field    : ', trim(W%varname)
if (T%set) write(*,*) 'Temperature field          : ', trim(T%varname)
if (S%set) write(*,*) 'Salinity field             : ', trim(S%varname)

write(*,*)
write(*,*) 'Input grid:'
write(*,*)
write(*,*) 'Zonal velocity'
write(*,*) 'xo(west),  xo(east)  = ', U%x(1),U%x(U%nx)
write(*,*) 'yo(south), yo(north) = ', U%y(1),U%y(U%ny)
write(*,*)
write(*,*) 'Meridional velocity'
write(*,*) 'xo(west),  xo(east)  = ', V%x(1),V%x(V%nx)
write(*,*) 'yo(south), yo(north) = ', V%y(1),V%y(V%ny)
if (W%set) then
  write(*,*)
  write(*,*) 'Vertical velocity'
  write(*,*) 'xo(west),  xo(east)  = ', W%x(1),W%x(W%nx)
  write(*,*) 'yo(south), yo(north) = ', W%y(1),W%y(W%ny)
endif
if (T%set) then
  write(*,*)
  write(*,*) 'Temperature'
  write(*,*) 'xo(west),  xo(east)  = ', T%x(1),T%x(T%nx)
  write(*,*) 'yo(south), yo(north) = ', T%y(1),T%y(T%ny)
endif
if (S%set) then
  write(*,*)
  write(*,*) 'Salinity'
  write(*,*) 'xo(west),  xo(east)  = ', S%x(1),S%x(S%nx)
  write(*,*) 'yo(south), yo(north) = ', S%y(1),S%y(S%ny)
endif
if (A%set) then
  write(*,*)
  write(*,*) 'Eastward Wind'
  write(*,*) 'xa(west),  xa(east)  = ', A%x(1),A%x(A%nx)
  write(*,*) 'ya(south), ya(north) = ', A%y(1),A%y(A%ny)
  write(*,*)
  write(*,*) 'Northward Wind'
  write(*,*) 'xa(west),  xa(east)  = ', B%x(1),B%x(B%nx)
  write(*,*) 'ya(south), ya(north) = ', B%y(1),B%y(B%ny)
endif

write(*,*) 
write(*,*) 'Time axis:'
write(*,*) 
write(*,*) 'Zonal velocity'
write(*,*) 'Initial time           = ', date_string(U%dates(1))

allocate(ellapsed_time(U%nt))
allocate(dtime(U%nt-1))
ellapsed_time = U%tscale*(U%t(:) - U%t(1))
do i=1,U%nt-1
  dtime(i) = ellapsed_time(i+1) - ellapsed_time(i)
enddo

if (U%nt.eq.1) stationary = .true.

write(*,*)
write(*,*) '*************************************'
if (stationary) then
  write(*,*) '      Streamline calculation'
else
  write(*,*) '      Trajectory calculation'
endif

if (time_direction.lt.0) then
  write(*,*) '  Reverse trajectory calculation'
    lorig = U%nt
  U%lorig = U%nt
  V%lorig = V%nt
  W%lorig = W%nt
  T%lorig = T%nt
  S%lorig = S%nt
  A%lorig = A%nt
  B%lorig = B%nt
else
  write(*,*) '  Forward trajectory calculation'
    lorig = 1
  U%lorig = 1
  V%lorig = 1
  W%lorig = 1
  T%lorig = 1
  S%lorig = 1
  A%lorig = 1
  B%lorig = 1
endif
write(*,*) '*************************************'
write(*,*)


! ... If the variable record has not been initialized (value -1), 
! ... then take the starting point
! ...
if (record.eq.-1) then
    record = lorig  
  U%record = U%lorig
  V%record = V%lorig
  W%record = W%lorig
  T%record = T%lorig
  S%record = S%lorig
  A%record = A%lorig
  B%record = B%lorig
else
    lorig  = record
  U%lorig  = U%record
  V%lorig  = V%record
  W%lorig  = W%record
  T%lorig  = T%record
  S%lorig  = S%record
  A%lorig  = A%record
  B%lorig  = B%record
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
  if (ftimesim) then
    simulation_length = min(86400._dp * simulation_length, & ! From day to secs
                            U%jds(U%nt) - U%jds(1))
  else
    simulation_length = U%jds(U%nt) - U%jds(1)
  endif
  external_dt = mean(dtime)
  if (.not.fidt) internal_dt = 3600._dp
endif

external_nsteps = int(simulation_length / external_dt )
internal_nsteps = int(external_dt   / internal_dt )

Nsteps = external_nsteps * internal_nsteps

! ... If necessary, change dt sign
! ...
internal_dt = time_direction * internal_dt

write(*,*) 'Simulation length          = ', simulation_length
write(*,*) 'External time step         = ', external_dt
write(*,*) 'External number time steps = ', external_nsteps
write(*,*) 'Internal time step         = ', internal_dt
write(*,*) 'Internal number time steps = ', internal_nsteps

if (U%missingdefined) then
  missing = U%missing_value
endif

write(*,*) 
write(*,*) 'Missing value              = ', missing

if (RKORDER.lt.1.or.RKORDER.gt.5) RKORDER = 5   ! Default = RK5
select case (RKORDER)
  case (1)
    RKNSTEPS = 1
    ! States required at time=t
  case (2)
    RKNSTEPS = 2
    ! States required at times=t, t+dt/2
  case (3)
    RKNSTEPS = 3
    ! States required at times=t, t+dt/2, t+dt
  case (4)
    RKNSTEPS = 3
    ! States required at times=t, t+dt/2, t+dt
  case (5)
    RKNSTEPS = 5
    ! States required at times=t, t+dt/4, t+dt/2, t+3dt/4, t+dt
end select

write(*,*) 
write(*,*) 'Runge Kutta order          = ', RKORDER
write(*,*) 'Runge Kutta time steps     = ', RKNSTEPS

return
end subroutine clm_ini
! ...
! ==========================================================================
! ...
subroutine clm_run (U,V,W,T,S,A,B,FLT)

type(field)                         :: U
type(field)                         :: V
type(field)                         :: W
type(field)                         :: T
type(field)                         :: S
type(field)                         :: A
type(field)                         :: B
type(floater)                       :: FLT

! ... Local variables:
! ...
logical init
integer flo,kout,nfloating,nstranded,noutside,step,i
integer                                  :: nv,nvars
real(dp)                                 :: external_time
real(dp)                                 :: internal_time
real(dp)                                 :: initial_time
real(dp)                                 :: Tt1,Tt2,Tdt
real(dp)                                 :: St1,St2,Sdt
real(dp), dimension(3)                   :: vp,vn

real(dp)                                 :: system_time
real(dp)                                 :: u_time
type(date_type)                          :: system_date
character(len=80)                        :: varnames

real(dp) udf,interpol3d
external udf,interpol3d

system_date%calendar = trim(calendar)
FLT%date%calendar    = trim(calendar)

! ... Allocating temporary memory spaces
! ...
allocate(uu(U%nx,U%ny,U%nz))   ! U tmp array
allocate(vv(V%nx,V%ny,V%nz))   ! V tmp array
allocate(ww(W%nx,W%ny,W%nz))   ! W tmp array
allocate(tt(T%nx,T%ny,T%nz))   ! Temp array
allocate(ss(S%nx,S%ny,S%nz))   ! Salt array
allocate(aa(A%nx,A%ny,A%nz))   ! A first array
allocate(bb(B%nx,B%ny,B%nz))   ! B first array

! ... Allocating memory space for cubic time interpolation
! ...
allocate(Utab(U%nx,U%ny,U%nz,4))
allocate(Vtab(V%nx,V%ny,V%nz,4))
if (W%set) allocate(Wtab(W%nx,W%ny,W%nz,4))
if (A%set) then
  allocate(Atab(A%nx,A%ny,A%nz,4))
  allocate(Btab(B%nx,B%ny,B%nz,4))
endif
! ... Tracers (:,:,:,2)
if (T%set) allocate(Ttab(T%nx,T%ny,T%nz,2))
if (S%set) allocate(Stab(S%nx,S%ny,S%nz,2))

! ... Memory space for cubic time interpolation coeffs
! ...
allocate(Ucoef(U%nx,U%ny,U%nz,4))
allocate(Vcoef(V%nx,V%ny,V%nz,4))
if (W%set) allocate(Wcoef(W%nx,W%ny,W%nz,4))
if (A%set) then
  allocate(Acoef(A%nx,A%ny,A%nz,4))
  allocate(Bcoef(B%nx,B%ny,B%nz,4))
endif

! ... Allocating space for the RK intermediate spaces
! ... Only for velocity and forcing fields
! ...
allocate(Urhs(U%nx,U%ny,U%nz,RKNSTEPS))
allocate(Vrhs(V%nx,V%ny,V%nz,RKNSTEPS))
if (W%set) allocate(Wrhs(W%nx,W%ny,W%nz,RKNSTEPS))
if (A%set) then
  allocate(Arhs(A%nx,A%ny,A%nz,RKNSTEPS))
  allocate(Brhs(B%nx,B%ny,B%nz,RKNSTEPS))
endif

write(*,*)
write(*,*)
write(*,*) '==              CLM simulation               =='
write(*,*) '==============================================='

!if (FLT%n.eq.0) call stop_error(1,'No floats defined')

external_time = ellapsed_time(1)

if (stationary) then
  initial_time = cal2jd(2000,1,1)*86400_dp
else
  if (reverse) then
    initial_time  = U%jds(U%nt)
  else
    initial_time  = U%jds(1)
  endif
endif


! ... Initialize floats:
! ... Are they released ?
! ... Are they stranded ?
! ... Are inside the domain ?
! ...
do flo=1,FLT%n
  FLT%time(flo)     = zero
  if (FLT%release_time(flo).le.zero) FLT%released(flo) = .true.
  FLT%stranded(flo) = beaching(FLT%lon(flo),FLT%lat(flo))
  FLT%outside(flo)  = outside(FLT%lon(flo),FLT%lat(flo))
  FLT%floating(flo) = .not.FLT%stranded(flo).and..not.FLT%outside(flo)
  print '(I3,3F9.3,F9.0,2X,L,X,L)', flo,    &
               FLT%lon(flo),          &
               FLT%lat(flo),          &
               FLT%depth(flo),        &
               FLT%release_time(flo), &
               FLT%released(flo), FLT%floating(flo)
enddo

! ... Geojson variables to include in the file
! ... Each file has three features:
! ...  Feature 1: Line
! ...  Feature 2: Point Initial
! ...  Feature 3: Point Final
! ...
if (gjson) then
  varnames = '"time", "depth"'
  nvars = 0
  if (T%set) then 
    varnames = trim(varnames)//', "temp"'
    nvars = nvars + 1
  endif
  if (S%set) then 
    varnames = trim(varnames)//', "psal"'
    nvars = nvars + 1
  endif
  do flo=1,FLT%n
    Geofile(flo)%varnames   = '['// trim(varnames) //']'
    Geofeature(flo,1)%type  = 'LineString'
    Geofeature(flo,2)%type  = 'Point'
    Geofeature(flo,3)%type  = 'Point'
   
    allocate(Geofeature(flo,1)%line(Nsteps+1))

    ! ... Filling attributes for the three features (1 line, 2 points)
    do i=1,3

     Geofeature(flo,i)%nvars = nvars
     Geofeature(flo,i)%nprop = 0
     allocate(Geofeature(flo,i)%var_defs(nvars))

     ! ... Time
     Geofeature(flo,i)%time_defs%units      = 'ISO8601'
     Geofeature(flo,i)%time_defs%qc_data    = '[]'
     Geofeature(flo,i)%time_defs%fill_value = '"0000-00-00T00:00:00Z"'
     Geofeature(flo,i)%time_defs%short_name = 'Time'
     Geofeature(flo,i)%time_defs%long_name  = 'Time'

     ! ... Depth
     Geofeature(flo,i)%depth_defs%units      = 'm'
     Geofeature(flo,i)%depth_defs%qc_data    = '[]'
     Geofeature(flo,i)%depth_defs%fill_value = '99999.0'
     Geofeature(flo,i)%depth_defs%short_name = 'Depth'
     Geofeature(flo,i)%depth_defs%long_name  = 'Floater depth'

    enddo

    nv = 0
    ! ... Temperature (optional)
    if (T%set) then 
      nv = nv + 1
      do i=1,3
       Geofeature(flo,i)%var_defs(nv)%name       = 'temp'
       Geofeature(flo,i)%var_defs(nv)%units      = 'degree_Celsius'
       Geofeature(flo,i)%var_defs(nv)%qc_data    = '[]'
       Geofeature(flo,i)%var_defs(nv)%fill_value = '99999.0'
       Geofeature(flo,i)%var_defs(nv)%short_name = 'Temperature'
       Geofeature(flo,i)%var_defs(nv)%long_name  = 'Sea temperature'
      enddo
    endif

    ! ... Salinity (optional)
    if (S%set) then 
      nv = nv + 1
      do i=1,3
       Geofeature(flo,i)%var_defs(nv)%name       = 'psal'
       Geofeature(flo,i)%var_defs(nv)%units      = 'psu'
       Geofeature(flo,i)%var_defs(nv)%qc_data    = '[]'
       Geofeature(flo,i)%var_defs(nv)%fill_value = '99999.0'
       Geofeature(flo,i)%var_defs(nv)%short_name = 'Salinity'
       Geofeature(flo,i)%var_defs(nv)%long_name  = 'Practical Salinity'
      enddo
    endif
  enddo
endif


do step=1,Nsteps

  system_time   = anint(initial_time+(step-1)*internal_dt) ! sec
  internal_time = (step-1)*internal_dt
  if (reverse) then
    u_time      = (U%jds(U%nt)-U%jds(1)) + (step-1)*internal_dt
  else
    u_time      = (step-1)*internal_dt
  endif

  write(*,*)
  write(*,*) 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  write(*,*) 'Step          = ', step  ,'/', Nsteps
  write(*,*) 'SYSTEM Time   = ', system_time
  write(*,*) 'Internal Time = ', internal_time
  write(*,*) 'U axis time   = ', u_time
  write(*,*) 'SYSTEM Date   =    ', date_string(jd2date(system_time/86400._dp), &
                                                'iso','extended')
  write(*,*) 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

  call clm_forcing (system_time)   ! Fill RHS arrays
  call clm_tracers (system_time)   ! Fill Arrays at times t and t+Delta_t

  ! ... Check if floats are going to be released
  ! ...
  do flo=1,FLT%n
    if (reverse) then
      if (FLT%release_time(flo).ge.u_time-0.1) FLT%released(flo) = .true.
    else
      if (FLT%release_time(flo).le.u_time+0.1) FLT%released(flo) = .true.
    endif
  enddo

  if (step.eq.1) then
    kout = 1
    ! ... Save initial positions
    do flo=1,FLT%n
      if (FLT%released(flo)) then
        FLT%dist(flo) = zero
        FLT%lo(flo)   = step
        if (T%set) FLT%temp(flo) = tracer_interpol(T,Ttab,[Tt1,Tt2], &
                                                   FLT%lon(flo),     &
                                                   FLT%lat(flo),     &
                                                   FLT%depth(flo),   &
                                                   system_time)
        if (S%set) FLT%salt(flo) = tracer_interpol(S,Stab,[St1,St2], &
                                                   FLT%lon(flo),     &
                                                   FLT%lat(flo),     &
                                                   FLT%depth(flo),   &
                                                   system_time)
        if (FLT%run_udf) FLT%UDF(flo) = udf(2,(/FLT%lon(flo),FLT%lat(flo)/))
      else
        FLT%dist(flo)            = missing
        if (T%set) FLT%temp(flo) = missing
        if (S%set) FLT%salt(flo) = missing
        if (FLT%run_udf) FLT%UDF(flo) = missing
      endif
    enddo
    FLT%date = jd2date(system_time/86400._dp)
    call out_save(FLT,kout,u_time,system_time/86400_dp)
  endif

  ! ... Integrate the equations
  ! ... Loop over floats
  ! ...
  do flo=1,FLT%n

    if (FLT%released(flo)) then
      if (FLT%floating(flo)) then

        ! ... Buoy Position 
        ! ...
        vp = [ FLT%lon(flo), FLT%lat(flo), FLT%depth(flo) ]

        ! ... Advance the location of the float from the time t to
        ! ... the time t+dt:
        ! ...
        if (RKORDER.eq.1) then
          call rk1(3,vp,internal_time,internal_dt,vn,RHS)
        else if (RKORDER.eq.2) then
          call rk2(3,vp,internal_time,internal_dt,vn,RHS)
        else if (RKORDER.eq.3) then
          call rk3(3,vp,internal_time,internal_dt,vn,RHS)
        else if (RKORDER.eq.4) then
          call rk4(3,vp,internal_time,internal_dt,vn,RHS)
        else
          call rk5(3,vp,internal_time,internal_dt,vn,RHS)
        endif

        ! ... Update time
        ! ...
        FLT%time(flo)  = FLT%time(flo) + internal_dt

        ! ... Update horizontal distance (km):
        ! ...
        ! ... WARNING: ADAPT IN CASE OPTION -XY
        FLT%dist(flo) = FLT%dist(flo) + 0.001_dp*haversine(vp(1),vp(2),vn(1),vn(2))

        ! ... Update float position:
        ! ...
        vp(:)          = vn(:)

        FLT%lon(flo)   = vn(1)
        FLT%lat(flo)   = vn(2)
        FLT%depth(flo) = vn(3)

        FLT%stranded(flo) = beaching(FLT%lon(flo),FLT%lat(flo))
        FLT%outside(flo)  = outside(FLT%lon(flo),FLT%lat(flo))

        if (FLT%outside(flo)) then
          write(*,*) 'Floater ', flo, ' leaving the region'
          FLT%floating(flo) = .false.
          FLT%temp(flo)     =  missing
          FLT%UDF(flo)      =  missing
          FLT%lf(flo)       =  step
        else
          if (FLT%stranded(flo)) then
            write(*,*) 'Stranded floater ', flo
            FLT%floating(flo) = .false.
            FLT%lf(flo)       =  step
            ! ... It retains its last valid temperature, salinity, etc.
          else
            FLT%lf(flo)       =  step    ! keeps updating every time

            if (T%set) then
              FLT%temp(flo) = tracer_interpol(T,Ttab,[Tt1,Tt2], &
                                              FLT%lon(flo),     &
                                              FLT%lat(flo),     &
                                              FLT%depth(flo),   &
                                              system_time)
            endif

            if (S%set) then
              FLT%salt(flo) = tracer_interpol(S,Stab,[St1,St2], &
                                              FLT%lon(flo),     &
                                              FLT%lat(flo),     &
                                              FLT%depth(flo),   &
                                              system_time)
            endif

            if (FLT%run_udf) then
              FLT%UDF(flo)  = udf(2,(/FLT%lon(flo),FLT%lat(flo)/))
              print*, 'UDF: ', FLT%UDF(flo)
            endif

          endif
        endif

        
      endif
    endif
  enddo


  ! ... Advance time for output purposes
  ! ... It will be recalculated at the beginning of the next step
  ! ...
  kout = kout + 1

  system_time = system_time + internal_dt 
  u_time      = u_time + internal_dt
  FLT%date = jd2date(system_time/86400._dp)
  call out_save(FLT,kout,u_time,system_time/86400_dp)

enddo

return


contains
  ! ...
  ! ========================================================================
  ! ========================================================================
  ! ...
  subroutine clm_forcing (time)
  ! ... Routine to fill the RHS arrays.
  ! ... The RHS arrays are defined at the Runge-Kutta time intervals.

  real(dp), intent(in)                     :: time

  integer i,j,k,kk,k0,kprev
  real(dp) dtfract,trk,tf
  real(dp) Utime,Vtime,Wtime,ATime,BTime
  real(dp) Uexternal_dt,Vexternal_dt,Wexternal_dt,Aexternal_dt,Bexternal_dt


  ! -------------------
  if (stationary) then
  ! -------------------
    if (step.eq.1) then
      write(*,*) 
      write(*,*) 'Filling RHS arrays for stationary case'

      uu = read_field(U,1)
      vv = read_field(V,1)
      do kk=1,RKNSTEPS
        Urhs(:,:,:,kk) = uu(:,:,:)
        Vrhs(:,:,:,kk) = vv(:,:,:)
      enddo

      if (W%set) then
        ww = read_field(W,1)
        do kk=1,RKNSTEPS
          Wrhs(:,:,:,kk) = ww(:,:,:)
        enddo
      endif

      if (A%set) then
        aa = read_field(A,1)
        bb = read_field(B,1)
        do kk=1,RKNSTEPS
          Arhs(:,:,:,kk) = aa(:,:,:)
          Brhs(:,:,:,kk) = bb(:,:,:)
        enddo
      endif

    endif

    return
  endif


  ! ... Get the arrays for the CUBIC INTERPOLATION
  ! ...
  if (step.eq.1) then

    ! ... U file:
    ! ...
    if (reverse) then
      k = U%lo + locate(U%jds,time)                ! Netcdf record.
      Utime = U%jds(k-U%lo+1)
      Uexternal_dt = abs(U%jds(k-1-U%lo+1) - Utime)
      if (k.lt.U%icdf%nt) then
        k0 = k + 1
      else
        k0 = k
      endif
    else
      k = U%lo + locate(U%jds,time+0.01D0) - 1
      Utime = U%jds(k-U%lo+1)
      Uexternal_dt = abs(U%jds(k+1-U%lo+1) - Utime)
      if (k.gt.1) then
        k0 = k - 1
      else
        k0 = k
      endif
    endif
    U%records = [k0,k,k+time_direction,k+2*time_direction]
    write(*,*) 'U - Initial records : ', U%records
    do kk=2,4
      Utab(:,:,:,kk) = read_field(U,U%records(kk))
    enddo
    Utab(:,:,:,1) = two*Utab(:,:,:,2) - Utab(:,:,:,3)
    do k=1,U%nz
      call i3coeffs (U%nx,U%ny,U%land(:,:,k),Utab(:,:,k,1:4), &
                     Ucoef(:,:,k,1:4))
    enddo

    ! ... V file:
    ! ...
    if (reverse) then
      k = V%lo + locate(V%jds,time)
      Vtime = V%jds(k-V%lo+1)
      Vexternal_dt = abs(V%jds(k-1-V%lo+1) - Vtime)
      if (k.lt.V%icdf%nt) then
        k0 = k + 1
      else
        k0 = k
      endif
    else
      k = V%lo + locate(V%jds,time+0.01D0) - 1
      Vtime = V%jds(k-V%lo+1)
      Vexternal_dt = abs(V%jds(k+1-V%lo+1) - Vtime)
      if (k.gt.1) then
        k0 = k - 1
      else
        k0 = k
      endif
    endif
    V%records = [k0,k,k+time_direction,k+2*time_direction]
    write(*,*) 'V - Initial records : ', V%records
    do kk=2,4
      Vtab(:,:,:,kk) = read_field(V,V%records(kk))
    enddo
    Vtab(:,:,:,1) = two*Vtab(:,:,:,2) - Vtab(:,:,:,3)
    do k=1,V%nz
      call i3coeffs (V%nx,V%ny,V%land(:,:,k),Vtab(:,:,k,1:4), &
                     Vcoef(:,:,k,1:4))
    enddo

    ! ... W file:
    ! ...
    if (W%set) then
      if (reverse) then
        k = W%lo + locate(W%jds,time)
        Wtime = W%jds(k-W%lo+1)
        Wexternal_dt = abs(W%jds(k-1-W%lo+1) - Wtime)
        if (k.lt.W%icdf%nt) then
          k0 = k + 1
        else
          k0 = k
        endif
      else
        k = W%lo + locate(W%jds,time+0.01D0) - 1
        Wtime = W%jds(k-W%lo+1)
        Wexternal_dt = abs(W%jds(k+1-W%lo+1) - Wtime)
        if (k.gt.1) then
          k0 = k - 1
        else
          k0 = k
        endif
      endif
      W%records = [k0,k,k+time_direction,k+2*time_direction]
      write(*,*) 'W - Initial records : ', W%records
      do kk=2,4
        Wtab(:,:,:,kk) = read_field(W,W%records(kk))
      enddo
      Wtab(:,:,:,1) = two*Wtab(:,:,:,2) - Wtab(:,:,:,3)
      do k=1,W%nz
        call i3coeffs (W%nx,W%ny,W%land(:,:,k),Wtab(:,:,k,1:4), &
                       Wcoef(:,:,k,1:4))
      enddo
    endif

    ! ... A and B file:
    ! ...
    if (A%set) then
      if (reverse) then
        k = A%lo + locate(A%jds,time)
        Atime = A%jds(k-A%lo+1)
        Aexternal_dt = abs(A%jds(k-1-A%lo+1) - Atime)
        if (k.lt.A%icdf%nt) then
          k0 = k + 1
        else
          k0 = k
        endif
      else
        k = A%lo + locate(A%jds,time+0.01D0) - 1
        Atime = A%jds(k-A%lo+1)
        Aexternal_dt = abs(A%jds(k+1-A%lo+1) - Atime)
        if (k.gt.1) then
          k0 = k - 1
        else
          k0 = k
        endif
      endif
      A%records = [k0,k,k+time_direction,k+2*time_direction]
      write(*,*) 'A - Initial records : ', A%records
      do kk=2,4
        Atab(:,:,:,kk) = read_field(A,A%records(kk))
      enddo
      Atab(:,:,:,1) = two*Atab(:,:,:,2) - Atab(:,:,:,3)
      do k=1,A%nz
        call i3coeffs (A%nx,A%ny,A%land(:,:,k),Atab(:,:,k,1:4), &
                       Acoef(:,:,k,1:4))
      enddo

      if (reverse) then
        k = B%lo + locate(B%jds,time)
        Btime = B%jds(k-B%lo+1)
        Bexternal_dt = abs(B%jds(k-1-B%lo+1) - Btime)
        if (k.lt.B%icdf%nt) then
          k0 = k + 1
        else
          k0 = k
        endif
      else
        k = B%lo + locate(B%jds,time+0.01D0) - 1
        Btime = B%jds(k-B%lo+1)
        Bexternal_dt = abs(B%jds(k+1-B%lo+1) - Btime)
        if (k.gt.1) then
          k0 = k - 1
        else
          k0 = k
        endif
      endif
      B%records = [k0,k,k+time_direction,k+2*time_direction]
      write(*,*) 'B - Initial records : ', B%records
      do kk=2,4
        Btab(:,:,:,kk) = read_field(B,B%records(kk))
      enddo
      Btab(:,:,:,1) = two*Btab(:,:,:,2) - Btab(:,:,:,3)
      do k=1,B%nz
        call i3coeffs (B%nx,B%ny,B%land(:,:,k),Btab(:,:,k,1:4), &
                       Bcoef(:,:,k,1:4))
      enddo

    endif

  else
     
    ! ... U file:
    ! ...
    if (reverse) then
      k = U%lo + locate(U%jds,time) 
      Utime = U%jds(k-U%lo+1)
      Uexternal_dt = abs(U%jds(k-1-U%lo+1) - Utime)
    else
      k = U%lo + locate(U%jds,time+0.01D0) - 1
      Utime = U%jds(k-U%lo+1)
      Uexternal_dt = abs(U%jds(k+1-U%lo+1) - Utime)
    endif
    if (k.eq.U%records(2)) then
      ! Nothing to do
    else
      ! Read a new record
      do kk=1,3
        U%records(kk) = U%records(kk+1)
        Utab(:,:,:,kk) = Utab(:,:,:,kk+1)
      enddo
      if (reverse) then
        if (U%records(4).gt.1) then
          U%records(4) = k + 2*time_direction
          Utab(:,:,:,4) = read_field(U,U%records(4))
        else
          Utab(:,:,:,4) = two*Utab(:,:,:,3) - Utab(:,:,:,2)
        endif
      else
        if (U%records(4).lt.U%icdf%nt) then
          U%records(4) = k + 2*time_direction
          Utab(:,:,:,4) = read_field(U,U%records(4))
        else
          Utab(:,:,:,4) = two*Utab(:,:,:,3) - Utab(:,:,:,2)
        endif
      endif
      write(*,*) 'U - Records updated : ', U%records
      do k=1,U%nz
        call i3coeffs (U%nx,U%ny,U%land(:,:,k),Utab(:,:,k,1:4), &
                       Ucoef(:,:,k,1:4))
      enddo
    endif
    !print*, Utab(207,88,1,:)

    ! ... V file:
    ! ...
    if (reverse) then
      k = V%lo + locate(V%jds,time)
      Vtime = V%jds(k-V%lo+1)
      Vexternal_dt = abs(V%jds(k-1-V%lo+1) - Vtime)
    else
      k = V%lo + locate(V%jds,time+0.01D0) - 1
      Vtime = V%jds(k-V%lo+1)
      Vexternal_dt = abs(V%jds(k+1-V%lo+1) - Vtime)
    endif
    if (k.eq.V%records(2)) then
      ! Nothing to do
    else
      ! Read a new record
      do kk=1,3
        V%records(kk) = V%records(kk+1)
        Vtab(:,:,:,kk) = Vtab(:,:,:,kk+1)
      enddo
      if (reverse) then
        if (V%records(4).gt.1) then
          V%records(4) = k + 2*time_direction
          Vtab(:,:,:,4) = read_field(V,V%records(4))
        else
          Vtab(:,:,:,4) = two*Vtab(:,:,:,3) - Vtab(:,:,:,2)
        endif
      else
        if (V%records(4).lt.V%icdf%nt) then
          V%records(4) = k + 2*time_direction
          Vtab(:,:,:,4) = read_field(V,V%records(4))
        else
          Vtab(:,:,:,4) = two*Vtab(:,:,:,3) - Vtab(:,:,:,2)
        endif
      endif
      write(*,*) 'V - Records updated : ', V%records
      do k=1,V%nz
        call i3coeffs (V%nx,V%ny,V%land(:,:,k),Vtab(:,:,k,1:4), &
                       Vcoef(:,:,k,1:4))
      enddo
    endif
    !print*, Vtab(207,88,1,:)

    ! ... W file:
    ! ...
    if (W%set) then
      if (reverse) then
        k = W%lo + locate(W%jds,time)
        Wtime = W%jds(k-W%lo+1)
        Wexternal_dt = abs(W%jds(k-1-W%lo+1) - Wtime)
      else
        k = W%lo + locate(W%jds,time+0.01D0) - 1
        Wtime = W%jds(k-W%lo+1)
        Wexternal_dt = abs(W%jds(k+1-W%lo+1) - Wtime)
      endif
      if (k.eq.W%records(2)) then
        ! Nothing to do
      else
        ! Read a new record
        do kk=1,3
          W%records(kk) = W%records(kk+1)
          Wtab(:,:,:,kk) = Wtab(:,:,:,kk+1)
        enddo
        if (reverse) then
          if (W%records(4).gt.1) then
            W%records(4) = k + 2*time_direction
            Wtab(:,:,:,4) = read_field(W,W%records(4))
          else
            Wtab(:,:,:,4) = two*Wtab(:,:,:,3) - Wtab(:,:,:,2)
          endif
        else
          if (W%records(4).lt.W%icdf%nt) then
            W%records(4) = k + 2*time_direction
            Wtab(:,:,:,4) = read_field(W,W%records(4))
          else
            Wtab(:,:,:,4) = two*Wtab(:,:,:,3) - Wtab(:,:,:,2)
          endif
        endif
        write(*,*) 'W - Records updated : ', W%records
        do k=1,W%nz
          call i3coeffs (W%nx,W%ny,W%land(:,:,k),Wtab(:,:,k,1:4), &
                         Wcoef(:,:,k,1:4))
        enddo
      endif
      !print*, Wtab(207,88,1,:)

    endif

    ! ... A and B file:
    ! ...
    if (A%set) then
      if (reverse) then
        k = A%lo + locate(A%jds,time)
        Atime = A%jds(k-A%lo+1)
        Aexternal_dt = abs(A%jds(k-1-A%lo+1) - Atime)
      else
        k = A%lo + locate(A%jds,time+0.01D0) - 1
        Atime = A%jds(k-A%lo+1)
        Aexternal_dt = abs(A%jds(k+1-A%lo+1) - Atime)
      endif
      if (k.eq.A%records(2)) then
        ! Nothing to do
      else
        ! Read a new record
        do kk=1,3
          A%records(kk) = A%records(kk+1)
          Atab(:,:,:,kk) = Atab(:,:,:,kk+1)
        enddo
        if (reverse) then
          if (A%records(4).gt.1) then
            A%records(4) = k + 2*time_direction
            Atab(:,:,:,4) = read_field(A,A%records(4))
          else
            Atab(:,:,:,4) = two*Atab(:,:,:,3) - Atab(:,:,:,2)
          endif
        else
          if (A%records(4).lt.A%icdf%nt) then
            A%records(4) = k + 2*time_direction
            Atab(:,:,:,4) = read_field(A,A%records(4))
          else
            Atab(:,:,:,4) = two*Atab(:,:,:,3) - Atab(:,:,:,2)
          endif
        endif
        write(*,*) 'A - Records updated : ', A%records
        do k=1,A%nz
          call i3coeffs (A%nx,A%ny,A%land(:,:,k),Atab(:,:,k,1:4), &
                         Acoef(:,:,k,1:4))
        enddo
      endif
    
      if (reverse) then
        k = B%lo + locate(B%jds,time)
        Btime = B%jds(k-B%lo+1)
        Bexternal_dt = abs(B%jds(k-1-B%lo+1) - Btime)
      else
        k = B%lo + locate(B%jds,time+0.01D0) - 1
        Btime = B%jds(k-B%lo+1)
        Bexternal_dt = abs(B%jds(k+1-B%lo+1) - Btime)
      endif
      if (k.eq.B%records(2)) then
        ! Nothing to read
      else
        ! Read a new record
        do kk=1,3
          B%records(kk) = B%records(kk+1)
          Btab(:,:,:,kk) = Btab(:,:,:,kk+1)
        enddo
        if (reverse) then
          if (B%records(4).gt.1) then
            B%records(4) = k + 2*time_direction
            Btab(:,:,:,4) = read_field(B,B%records(4))
          else
            Btab(:,:,:,4) = two*Btab(:,:,:,3) - Btab(:,:,:,2)
          endif
        else
          if (B%records(4).lt.B%icdf%nt) then
            B%records(4) = k + 2*time_direction
            Btab(:,:,:,4) = read_field(B,B%records(4))
          else
            Btab(:,:,:,4) = two*Btab(:,:,:,3) - Btab(:,:,:,2)
          endif
        endif
        write(*,*) 'B - Records updated : ', B%records
        do k=1,B%nz
          call i3coeffs (B%nx,B%ny,B%land(:,:,k),Btab(:,:,k,1:4), &
                         Bcoef(:,:,k,1:4))
        enddo
      endif
      !print*, Btab(207,88,1,:)

    endif

  endif


  ! ... Fill RHS arrays
  ! ...
  if (RKORDER.lt.5) then
    dtfract = half
  else
    dtfract = quarter
  endif

  ! ... U and V
  ! ...
  do kk=1,RKNSTEPS
    trk = time + dtfract*(kk-1)*internal_dt
    tf = abs((trk-Utime)/Uexternal_dt)
    do k=1,U%nz
    do j=1,U%ny
    do i=1,U%nx
      Urhs(i,j,k,kk) = cubic(Ucoef(i,j,k,:),tf)
    enddo
    enddo
    enddo
  enddo

  do kk=1,RKNSTEPS
    trk = time + dtfract*(kk-1)*internal_dt
    tf = abs((trk-Vtime)/Vexternal_dt)
    do k=1,V%nz
    do j=1,V%ny
    do i=1,V%nx
      Vrhs(i,j,k,kk) = cubic(Vcoef(i,j,k,:),tf)
    enddo
    enddo
    enddo
  enddo

  if (W%set) then
    do kk=1,RKNSTEPS
      trk = time + dtfract*(kk-1)*internal_dt
      tf = abs((trk-Wtime)/Wexternal_dt)
      do k=1,W%nz
      do j=1,W%ny
      do i=1,W%nx
        Wrhs(i,j,k,kk) = cubic(Wcoef(i,j,k,:),tf)
      enddo
      enddo
      enddo
    enddo
  endif

  if (A%set) then
    do kk=1,RKNSTEPS
      trk = time + dtfract*(kk-1)*internal_dt
      tf = abs((trk-Atime)/Aexternal_dt)
      do k=1,A%nz
      do j=1,A%ny
      do i=1,A%nx
        Arhs(i,j,k,kk) = cubic(Acoef(i,j,k,:),tf)
      enddo
      enddo
      enddo
    enddo
    do kk=1,RKNSTEPS
      trk = time + dtfract*(kk-1)*internal_dt
      tf = abs((trk-Btime)/Bexternal_dt)
      do k=1,B%nz
      do j=1,B%ny
      do i=1,B%nx
        Brhs(i,j,k,kk) = cubic(Bcoef(i,j,k,:),tf)
      enddo
      enddo
      enddo
    enddo
  endif

  end subroutine clm_forcing
  ! ...
  ! ========================================================================
  ! ...
  subroutine clm_tracers (time)

  implicit none

  real(dp), intent(in)                     :: time

  integer k1,k2
  real(dp) Xt1,Xt2
  logical update

  ! ... The values of 
  ! ...               Tt1, Tt2, Tdt
  ! ...               St1, St2, Sdt
  ! ... are being defined in the routine clm_run, parent of this one.

  if (T%set) then

    if (reverse) then
      k1 = T%lo + locate(T%jds,time)
      k2 = k1 - 1
      Xt1 = T%jds(k1-T%lo+1)
      Xt2 = T%jds(k2-T%lo+1)
    else
      k1 = T%lo + locate(T%jds,time+0.01D0) - 1
      k2 = k1 + 1
      Xt1 = T%jds(k1-T%lo+1)
      Xt2 = T%jds(k2-T%lo+1)
    endif

    if (step.eq.1) then
      update = .true.
    else if (abs(Xt1-Tt1).lt.0.1*abs(Tt1-Tt2)) then
      update = .false.
    else
      update = .true.
    endif

    if (update) then
      write(*,*) 'T - Records updated : ', k1, k2
      Tt1 = Xt1
      Tt2 = Xt2
      Tdt = Tt2 - Tt1
      if (step.eq.1) then
        Ttab(:,:,:,1) = read_field(T,k1)
      else
        Ttab(:,:,:,1) = Ttab(:,:,:,2)
      endif
      Ttab(:,:,:,2) = read_field(T,k2)
    endif
  endif

  if (S%set) then

    if (reverse) then
      k1 = S%lo + locate(S%jds,time)
      k2 = k1 - 1
      Xt1 = S%jds(k1-S%lo+1)
      Xt2 = S%jds(k2-S%lo+1)
    else
      k1 = S%lo + locate(S%jds,time+0.01D0) - 1
      k2 = k1 + 1
      Xt1 = S%jds(k1-S%lo+1)
      Xt2 = S%jds(k2-S%lo+1)
    endif

    if (step.eq.1) then
      update = .true.
    else if (abs(Xt1-St1).lt.0.1*abs(St1-St2)) then
      update = .false.
    else
      update = .true.
    endif

    if (update) then
      write(*,*) 'S - Records updated : ', k1, k2
      St1 = Xt1
      St2 = Xt2
      Sdt = St2 - St1
      if (step.eq.1) then
        Stab(:,:,:,1) = read_field(S,k1)
      else
        Stab(:,:,:,1) = Stab(:,:,:,2) 
      endif
      Stab(:,:,:,2) = read_field(S,k2)
    endif
  endif


  end subroutine clm_tracers
  ! ...
  ! ========================================================================
  ! ...
  function tracer_interpol (TRACER,DAT,TT,px,py,pz,pt) result (ff)

  type(field), intent(in)                              :: TRACER
  real(dp), dimension(TRACER%nx,TRACER%ny,TRACER%nz,2) :: DAT
  real(dp), dimension(2)                               :: TT
  real(dp), intent(in)                                 :: px,py,pz,pt

  real(dp)                                             :: ff

  ! ... Local variables:
  ! ...
  real(dp) t1,t2,f1,f2

  f1 = interpol3d('BILINEAL',TRACER%nx, TRACER%ny, TRACER%nz, &
                             TRACER%x, TRACER%y, TRACER%z,  &
                             DAT(:,:,:,1),px,py,pz)
  f2 = interpol3d('BILINEAL',TRACER%nx, TRACER%ny, TRACER%nz, &
                             TRACER%x, TRACER%y, TRACER%z,  &
                             DAT(:,:,:,2),px,py,pz)
   
  ff = f1 + (f2-f1)*(pt-TT(1))/(TT(2)-TT(1))
             
  !print*, 'Interpolation'
  !print*, pt, (pt-TT(1))/(TT(2)-TT(1)),ff
    

  end function tracer_interpol
  ! ...
  ! ========================================================================
  ! ...
  subroutine read_fields (kstep)

  integer, intent(in)                             :: kstep

  uu = get_data(U,kstep)
  vv = get_data(V,kstep)

  if (W%set) then
    ww = get_data(W,kstep)
  endif

  if (T%set) then
    tt = get_data(T,kstep)
    call fillcoast(T%nx,T%ny,T%land,T%sea,tt)
  endif

  end subroutine read_fields
  ! ...
  ! ========================================================================
  ! ...
  function get_data (u,l)  result(ff)

  type(field), intent(inout)                      :: u  
  integer, intent(in)                             :: l  
  real(dp), dimension(u%nx,u%ny,u%nz)             :: ff

  integer err,ppi,ppj,ppk,ppl
  integer po(u%icdf%vndims(u%idv)), pf(u%icdf%vndims(u%idv))

  ppi = u%icdf%ppi(u%idv)
  ppj = u%icdf%ppj(u%idv)
  ppk = u%icdf%ppk(u%idv)
  ppl = u%icdf%ppl(u%idv)

  po(:) = 1
  pf(:) = 1
  if (ppi.gt.0) then
    po(ppi) = u%io
    pf(ppi) = u%nx
  endif
  if (ppj.gt.0) then
    po(ppj) = u%jo
    pf(ppj) = u%ny
  endif
  if (ppk.gt.0) then
    po(ppk) = u%ko
    pf(ppk) = u%nz
  endif
  if (ppl.gt.0) po(ppl) = l
  
  err = NF90_GET_VAR(u%icdf%fid,u%idv,ff,po,pf)
  call cdf_error(err,'Unable to read field in get_data')

  if (u%missingisnan) then
    where(isnan(ff))
      ff = zero
    elsewhere
      ff = u%add_offset + u%scale_factor*ff
    endwhere
  else
    where((ff.eq.u%missing_value))
      ff = zero
    elsewhere
      ff = u%add_offset + u%scale_factor*ff
    endwhere
  endif

  end function get_data
  ! ...
  ! ========================================================================
  ! ...
  subroutine i3coeffs (nx,ny,land,uu,cc)
  ! ... Cubic interpolation coefficients:
  ! ... u(t) = cc[1] + cc[2]*t + cc[3]*t^2 + cc[4]*t^3
  ! ...
  ! ... The cubic interpolation coefficients, calculated from
  ! ... 
  ! ...      t   |    -1       0       1      2
  ! ...      u   |   uu[1]   uu[2]   uu[3]   uu[4]
  ! ...
  ! ... u(-1) = uu[1]
  ! ... u( 0) = uu[2]
  ! ... u( 1) = uu[3]
  ! ... u( 2) = uu[4]
  ! ...
  ! ... u(0)  = uu[1] = cc[2]
  ! ... u(1)  = uu[2] = cc[1] + cc[2] 
  ! ... u'(0) = cc[2]
  ! ... u'(1) = cc[2] + 2*cc[3] + 3*cc[3]
  ! ...
  ! ... u'(0) = 0.5*(uu[3]-uu[1])
  ! ... u'(1) = 0.5*(uu[4]-uu[2])
 


  integer, intent(in)                        :: nx,ny
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
  if (xo.le.U%domain_west) return
  if (xo.le.V%domain_west) return
  if (xo.ge.U%domain_east) return
  if (xo.ge.V%domain_east) return
  if (yo.le.U%domain_south) return
  if (yo.le.V%domain_south) return
  if (yo.ge.U%domain_north) return
  if (yo.ge.V%domain_north) return

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
  if (xo.le.U%x(1)) then
    beaching = .true.
    return
  else if (xo.ge.U%x(U%nx)) then
    beaching = .true.
    return
  else
    iref = locate(U%x,xo)
  endif

  if (yo.le.U%y(1)) then
    beaching = .true.
    return
  else if (yo.ge.U%y(U%ny)) then
    beaching = .true.
    return
  else
    jref = locate(U%y,yo)
  endif

  d(1) = (U%x(iref)-xo)**2   + (U%y(jref)-yo)**2
  d(2) = (U%x(iref+1)-xo)**2 + (U%y(jref)-yo)**2
  d(3) = (U%x(iref+1)-xo)**2 + (U%y(jref+1)-yo)**2
  d(4) = (U%x(iref)-xo)**2   + (U%y(jref+1)-yo)**2

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

  Ubeach = U%land(io,jo,1)

  ! ... By respect the V grid
  ! ........................................................
  if (xo.le.V%x(1)) then
    beaching = .true.
    return
  else if (xo.ge.V%x(V%nx)) then
    beaching = .true.
    return
  else
    iref = locate(V%x,xo)
  endif

  if (yo.le.V%y(1)) then
    beaching = .true.
    return
  else if (yo.ge.V%y(V%ny)) then
    beaching = .true.
    return
  else
    jref = locate(V%y,yo)
  endif

  d(1) = (V%x(iref)-xo)**2   + (V%y(jref)-yo)**2
  d(2) = (V%x(iref+1)-xo)**2 + (V%y(jref)-yo)**2
  d(3) = (V%x(iref+1)-xo)**2 + (V%y(jref+1)-yo)**2
  d(4) = (V%x(iref)-xo)**2   + (V%y(jref+1)-yo)**2

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

  Vbeach = V%land(io,jo,1)

  beaching = Ubeach.and.Vbeach

  end function beaching
  ! ...
  ! ==========================================================================
  ! ...
  subroutine RHS(n,t,x,dxdt)

  implicit none

  integer, intent(in)                        :: n
  real(dp), intent(in)                       :: t
  real(dp), dimension(n), intent(in)         :: x
  real(dp), dimension(n), intent(out)        :: dxdt

  ! ... Local variables
  ! ... 
  integer kk
  real(dp) rt,rcos

  if (Flonlat) then
    rcos = one/cos(deg2rad*x(2))
    Uscale = m2deg*rcos
    Vscale = m2deg
  endif

  if (RKORDER.eq.1) then
    kk = 1
  else if (RKORDER.eq.2) then
    rt = (t-internal_time)/internal_dt
    if (rt.lt.0.5_dp) then
      kk = 1
    else
      kk = 2
    endif
  else if (RKORDER.eq.3) then
    kk = nint(2.0_dp*(t-internal_time)/internal_dt) + 1
  else if (RKORDER.eq.4) then
    kk = nint(2.0_dp*(t-internal_time)/internal_dt) + 1
  else
    kk = nint(4.0_dp*(t-internal_time)/internal_dt) + 1
  endif

  print*, 'Uscale, Vscale = ', Uscale, Vscale
  print*, 'x = ', x

  dxdt(1) = Uscale*interpol3d(METHOD,U%nx,U%ny,U%nz,  &
                                  U%x,U%y,U%z,   &
                                  urhs(:,:,:,kk),x(1),x(2),x(3))
  dxdt(2) = Vscale*interpol3d(METHOD,V%nx,V%ny,V%nz,  &
                             V%x,V%y,V%z,   &
                             vrhs(:,:,:,kk),x(1),x(2),x(3))

  if (W%set) then
    dxdt(3) = interpol3d(METHOD,W%nx,W%ny,W%nz,  &
                                W%x,W%y,W%z,   &
                                wrhs(:,:,:,kk),x(1),x(2),x(3))
  else
    dxdt(3) = zero
  endif
  print*, 'dxdt : ', dxdt

  end subroutine RHS
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

! ...
! ==========================================================================
! ...
end subroutine clm_run
! ==========================================================================
! ==========================================================================

end module clm
