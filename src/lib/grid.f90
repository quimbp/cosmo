! *********************************************************************
! ... COSMO model
! ... 
! ...
! *********************************************************************

module module_grid

use module_types, only: dp,maxlen
use module_constants
use module_math
use module_utils
use module_datetime
use module_status

use netcdf

implicit none

type type_varatts
  character(len=maxlen)               :: units          = ""
  logical                             :: missing        = .False. ! True if missing_value
  logical                             :: missing_isnan  = .False. ! self-explanatory
  integer                             :: ndims          = -1      ! valid values: 1,2,3,4,...
  logical                             :: xdim           = .False.
  logical                             :: ydim           = .False.
  logical                             :: zdim           = .False.
  logical                             :: tdim           = .False.
  integer, dimension(:), pointer      :: dimids
  real(dp), dimension(:,:,:), pointer :: mask
  real(dp)                            :: add_offset     = 0.0D0
  real(dp)                            :: scale_factor   = 1.0D0
  real(dp)                            :: missing_value  = -999.D0
end type type_varatts

type type_grid
  ! ... Estucture holding the information for reading and interpreting
  ! ... daa from a netcdf file. It includes:
  ! ... Input filename
  ! ... Names of X, Y, Z and T dimensions
  ! ... Names of X, Y, Z and T variables
  ! ... Names of the variable of interest
  ! ... The NetCDF file ID.
  ! ... The IDs of X, Y, Z and T dimensions
  ! ... The IDs of X, Y, Z and T variables
  ! ... The grid size Nx, Ny, Nz, and Nt
  ! ... The 4D-bounding box
  ! ... The file longitudes and latitudes stored in 2D arrays (in rad)
  ! ... The Mercator transformed coordinates (in m)
  ! ... The depth
  ! ... The time read from the file
  ! ... The time in epochs (seconds since 1970-01-01H00:00:00).
  ! ... The indices to be read from the Netcdf (to avoid reading the whole file).
  ! ...                        jo + jl - 1
  ! ...               io                         io + il - 1
  ! ...                            jo
  ! ...
  type(type_varatts)                     :: var
  character(len=maxlen)                  :: filename="",varname=""
  character(len=maxlen)                  :: iname="",jname="",kname="",lname=""
  character(len=maxlen)                  :: xname="",yname="",zname="",tname=""
  logical                                :: grid2d = .False.
  integer                                :: fid=-1,vid=-1
  integer                                :: idi=-1,idj=-1,idk=-1,idl=-1
  integer                                :: idx=-1,idy=-1,idz=-1,idt=-1
  integer                                :: nx=1,ny=1,nz=1,nt=1
  real(dp)                               :: reftime
  real(dp)                               :: lonmin_deg,lonmax_deg,latmin_deg,latmax_deg
  real(dp)                               :: lonmin,lonmax,latmin,latmax
  real(dp)                               :: zmin,zmax,tmin,tmax
  real(dp)                               :: dt                ! in seconds
  real(dp), dimension(:,:), pointer      :: lon,lat
!  real(dp), dimension(:,:), pointer      :: x,y
  real(dp), dimension(:), pointer        :: z
  real(dp), dimension(:), pointer        :: t                 ! time in julian days
  real(dp), dimension(:), pointer        :: s                 ! time in seconds
  type(type_date), dimension(:), pointer :: date
  integer                                :: io,jo,ko,lo 
  integer                                :: ni,nj,nk,nl 

  contains
    procedure                 :: open          => grid_open
    procedure                 :: scan          => grid_scan
    procedure                 :: crop          => grid_crop
!    procedure                 :: lbracket      => grid_lbracket
    procedure                 :: bracket       => grid_bracket
    procedure                 :: read          => grid_read2D
    procedure                 :: locate        => grid_locate2D
    procedure                 :: show          => grid_show
    procedure                 :: hinterpol     => grid_hinterpol
    procedure                 :: point_type    => grid_type

end type type_grid

contains
! ...
! =============================================================================
! =============================================================================
! ...
integer function grid_open(GRD,filename) result(err)
! ...
! ... Function grid_open
! ... Opens a netcdf and get information about the potential names
! ... of geophysical dimensions and variables
! ...
class(type_grid), intent(inout)           :: GRD
character(len=*), intent(in)              :: filename

! ... Local variables
! ... 
integer fid,ndims,nvars,natts,unlimid,ntype,dimids(10)
integer idx,idy,idz,idt,var
character(len=maxlen) word
character(len=1) axis

err = NF90_OPEN(filename,0,fid)
if (err.NE.NF90_NOERR) return

err = NF90_INQUIRE (fid,ndims,nvars,natts,unlimid)
if (err.NE.NF90_NOERR) return

GRD%filename = trim(filename)
GRD%fid = fid


! ... Get information about Geophysical axes
! ...
idx = -1; idy = -1; idz = -1; idt = -1

! ... Check the existence of the axis attribute:
! ...
do var=1,nvars
  axis = ''
  err  = NF90_GET_ATT(fid,var,'axis',axis)
  axis = uppercase(axis)
  if (axis.eq.'X') idx = var
  if (axis.eq.'Y') idy = var
  if (axis.eq.'Z') idz = var
  if (axis.eq.'T') idt = var
enddo

! ... If axis attribute has not been defined...
! ...

if (idx.lt.0) then
  do var=1,nvars
    word = ''
    err  = NF90_INQUIRE_VARIABLE (fid,var,word,ntype,ndims,dimids,natts)
    word = uppercase(word)
    if (idx.LT.0.AND.word(1:1).eq.'X')       idx = var
    if (idx.LT.0.AND.word(1:3).eq.'LON')     idx = var
    if (idx.LT.0.AND.word(1:7).eq.'NAV_LON') idx = var
  enddo
endif
if (idx.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idx,name=GRD%xname)

if (idy.lt.0) then
  do var=1,nvars
    word = ''
    err  = NF90_INQUIRE_VARIABLE (fid,var,word,ntype,ndims,dimids,natts)
    word = uppercase(word)
    if (idy.LT.0.AND.word(1:1).eq.'Y')       idy = var
    if (idy.LT.0.AND.word(1:3).eq.'LAT')     idy = var
    if (idy.LT.0.AND.word(1:7).eq.'NAV_LAT') idy = var
  enddo
endif
if (idy.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idy,name=GRD%yname)

if (idz.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idz,name=GRD%zname)

if (idt.lt.0.and.unlimid.gt.0) idt = unlimid
if (idt.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idt,name=GRD%tname)

GRD%idx = idx; GRD%idy = idy; GRD%idz = idz; GRD%idt = idt

err = 0
return
end function grid_open
! ...
! =====================================================================
! ...
integer function grid_scan(GRD,varname,maskname,maskvalue) result(err)
! ...
! ... Function grid_scan
! ... Gets the Ids of dimensions and updates the IDs of the variables. 
! ... Define grid sizes and fill the grid arrays.
! ...
class(type_grid), intent(inout)            :: GRD
character(len=*), intent(in), optional     :: varname
character(len=*), intent(in), optional     :: maskname
integer, intent(in), optional              :: maskvalue

! ... Local variables
! ...
type(type_date) dateref,dd
logical with_missing,with_fill
integer i,j,mid
integer fid,ndims,nvars,natts,unlimid,ntype,dimids(10),grid_dims
character(len=maxlen) word,time_units,calendar
real(dp) missing_value,fill_value,tscale
real(dp), dimension(:), allocatable :: tmp
real(dp), dimension(:,:,:), allocatable  :: wrk

if (present(varname)) then
  if (len_trim(varname).GT.0) then
    err = NF90_INQ_VARID(GRD%fid,trim(varname),GRD%vid)
    if (err.NE.NF90_NOERR) return
    word = ''
    err  = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%vid,ndims=ndims,dimids=dimids)
    GRD%varname = trim(varname)
    GRD%var%ndims = ndims
    allocate(GRD%var%dimids(ndims))
    GRD%var%dimids(:) = dimids(1:ndims)

    err = NF90_GET_ATT (GRD%fid,GRD%vid,'units',GRD%var%units)
    if (err.ne.NF90_NOERR) GRD%var%units = ''
    err = NF90_GET_ATT (GRD%fid,GRD%vid,'add_offset',GRD%var%add_offset)
    if (err.ne.NF90_NOERR) GRD%var%add_offset = 0.0_dp
    err = NF90_GET_ATT (GRD%fid,GRD%vid,'scale_factor',GRD%var%scale_factor)
    if (err.ne.NF90_NOERR) GRD%var%scale_factor = 1.0_dp


    ! ... Missing value:
    ! ...
    err = NF90_GET_ATT (GRD%fid,GRD%vid,'_FillValue',fill_value)
    if (err.eq.NF90_NOERR) then
      with_fill = .true.
    else
      with_fill = .false.
    endif
    err = NF90_GET_ATT (GRD%fid,GRD%vid,'missing_value',missing_value)
    if (err.eq.NF90_NOERR) then
      with_missing = .true.
    else
      with_missing = .false.
    endif

    if (with_missing) then
      GRD%var%missing = .true.
      GRD%var%missing_value = missing_value
      GRD%var%missing_isnan = isnan(missing_value)
    else if (with_fill) then
      GRD%var%missing = .true.
      GRD%var%missing_value = fill_value
      GRD%var%missing_isnan = isnan(missing_value)
    else
      GRD%var%missing = .false.
      GRD%var%missing_value = -999.0d0
      GRD%var%missing_isnan = .false.
    endif

  else
    err = 1
    STATUS_ERROR = 1
    STATUS_TEXT = "Empty variable name in GRID_SCAN"
    return
  endif
endif

grid_dims = -1

! ... Longitude
! ...
if (len_trim(GRD%xname).GT.0) then
  err = NF90_INQ_VARID(GRD%fid,trim(GRD%xname),GRD%idx)
  if (err.NE.NF90_NOERR) return
  err  = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idx,word,ntype,ndims,dimids,natts)
  if (err.NE.NF90_NOERR) return
  grid_dims = ndims
  GRD%idi = dimids(1)
  err = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idi,name=GRD%iname)
endif

! ... Latitude
! ...
if (len_trim(GRD%yname).GT.0) then
  err = NF90_INQ_VARID(GRD%fid,trim(GRD%yname),GRD%idy)
  if (err.NE.NF90_NOERR) return
  err  = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idy,word,ntype,ndims,dimids,natts)
  if (err.NE.NF90_NOERR) return
  if (GRD%idi.GT.0) then
    if (ndims.NE.grid_dims) then
      err = -900
      return
    endif
    if (ndims.eq.1) then
      GRD%idj = dimids(1)
    else if (ndims.eq.2) then
      GRD%idj = dimids(2)
      GRD%grid2d = .True.
    else
      err = -901
      return
    endif
  else
    GRD%idj = dimids(1)
  endif
  err = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idj,name=GRD%jname)
endif

! ... Depth
! ...
if (len_trim(GRD%zname).GT.0) then
  err = NF90_INQ_VARID(GRD%fid,trim(GRD%zname),GRD%idz)
  if (err.NE.NF90_NOERR) return
  err  = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idz,word,ntype,ndims,dimids,natts)
  if (err.NE.NF90_NOERR) return
  GRD%idk = dimids(1)
  err = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idk,name=GRD%kname)
endif

! ... Time
! ...
if (len_trim(GRD%tname).GT.0) then
  err = NF90_INQ_VARID(GRD%fid,trim(GRD%tname),GRD%idt)
  if (err.NE.NF90_NOERR) return
  err  = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idt,word,ntype,ndims,dimids,natts)
  if (err.NE.NF90_NOERR) return
  GRD%idl = dimids(1)
  err = NF90_INQUIRE_VARIABLE (GRD%fid,GRD%idl,name=GRD%lname)

  ! ... Check for Reference time and calendar
  ! ...
  err = NF90_GET_ATT (GRD%fid,GRD%idt,'calendar',calendar)
  if (err.ne.NF90_NOERR) calendar = 'gregorian'

  err = NF90_GET_ATT (GRD%fid,GRD%idt,'units',time_units)
  if (err.eq.NF90_NOERR) then
    dateref = strpreftime(time_units)
    dateref%calendar = trim(calendar)

    time_units = uppercase(time_units)
    if (index(time_units,'SECOND').GT.0) THEN
      tscale = 1.0_dp
    else if (index(time_units,'MINUT').GT.0) THEN
      tscale = 60.0_dp
    else if (index(time_units,'HOUR').GT.0) THEN
      tscale = 3600.0_dp
    else if (index(time_units,'DAY').GT.0) THEN
      tscale = 86400.0_dp
    else
      write(*,*)
      write(*,*) 'Time units           = ', TRIM(time_units)
      call stop_error(1,'Unknown units')
    endif

  else
    tscale = 1.0_dp 
    GRD%reftime = zero
  endif

endif

if (GRD%idi.GT.0) err = NF90_INQUIRE_DIMENSION(GRD%fid,GRD%idi,len=GRD%nx)
if (GRD%idj.GT.0) err = NF90_INQUIRE_DIMENSION(GRD%fid,GRD%idj,len=GRD%ny)
if (GRD%idk.GT.0) err = NF90_INQUIRE_DIMENSION(GRD%fid,GRD%idk,len=GRD%nz)
if (GRD%idl.GT.0) err = NF90_INQUIRE_DIMENSION(GRD%fid,GRD%idl,len=GRD%nt)

allocate(GRD%lon(GRD%nx,GRD%ny),stat=err); if (err.NE.0) return
allocate(GRD%lat(GRD%nx,GRD%ny),stat=err); if (err.NE.0) return
allocate(GRD%z(GRD%nz),stat=err); if (err.NE.0) return
allocate(GRD%t(GRD%nt),stat=err); if (err.NE.0) return
allocate(GRD%s(GRD%nt),stat=err); if (err.NE.0) return
allocate(GRD%date(GRD%nt),stat=err); if (err.NE.0) return

! ... Read the horizontal grid, the depth and time
! ...
if (GRD%idx.gt.0) then
  if (GRD%grid2d) then
    err = NF90_GET_VAR(GRD%fid,GRD%idx,GRD%lon)
    if (err.ne.NF90_NOERR) return
  else
    allocate(tmp(GRD%nx))
    err = NF90_GET_VAR(GRD%fid,GRD%idx,tmp)
    if (err.ne.NF90_NOERR) return
    do j=1,GRD%ny
      GRD%lon(:,j) = tmp
    enddo
    deallocate(tmp)
  endif
else
  GRD%lon(:,:) = 0.0D0
endif

if (GRD%idy.gt.0) then
  if (GRD%grid2d) then
    err = NF90_GET_VAR(GRD%fid,GRD%idy,GRD%lat)
    if (err.ne.NF90_NOERR) return
  else
    allocate(tmp(GRD%ny))
    err = NF90_GET_VAR(GRD%fid,GRD%idy,tmp)
    if (err.ne.NF90_NOERR) return
    do i=1,GRD%nx
      GRD%lat(i,:) = tmp
    enddo
    deallocate(tmp)
  endif
else
  GRD%lat(:,:) = 0.0D0
endif


if (GRD%idz.gt.0) then
  err = NF90_GET_VAR(GRD%fid,GRD%idz,GRD%z)
  if (err.ne.NF90_NOERR) return
  GRD%zmin = GRD%z(1)
  GRD%zmax = GRD%z(GRD%nz)
endif
  
if (GRD%idt.gt.0) then
  err = NF90_GET_VAR(GRD%fid,GRD%idt,GRD%t)
  if (err.ne.NF90_NOERR) return
  GRD%t(:) = dateref%jd() + tscale*GRD%t(:)/86400.0_dp
  GRD%s(:) = 86400.0D0*GRD%t(:)
  do i=1,GRD%nt
    dd = jd2date(GRD%t(i))
    GRD%date(i) = dd
  enddo
  GRD%tmin = GRD%t(1)
  GRD%tmax = GRD%t(GRD%nt)
  if (GRD%nt.eq.1) then
    GRD%dt = 0.0_dp
  else
    GRD%dt = nint(GRD%s(2)) - nint(GRD%s(1))
  endif
endif

GRD%lonmin_deg = minval(GRD%lon); GRD%lonmax_deg = maxval(GRD%lon)  ! deg
GRD%latmin_deg = minval(GRD%lat); GRD%latmax_deg = maxval(GRD%lat)  ! deg

! ... Longitude and latitudes in Radians:
! ...
GRD%lon(:,:) = deg2rad*GRD%lon(:,:)    ! Longitudes in radians
GRD%lat(:,:) = deg2rad*GRD%lat(:,:)    ! Latitudes in radians

GRD%lonmin = minval(GRD%lon); GRD%lonmax = maxval(GRD%lon)          ! rad
GRD%latmin = minval(GRD%lat); GRD%latmax = maxval(GRD%lat)          ! rad

!allocate(GRD%x(GRD%nx,GRD%ny),stat=err); if (err.NE.0) return
!allocate(GRD%y(GRD%nx,GRD%ny),stat=err); if (err.NE.0) return

! ... Mercator projection:
! ...
!GRD%x(:,:) = Rearth*GRD%lon(:,:)
!GRD%y(:,:) = Rearth*dlog(dtan(0.25D0*pi+0.50D0*GRD%lat(:,:)))

! ... Bounding Box
! ...
!GRD%xmin = minval(GRD%x); GRD%xmax = maxval(GRD%x)
!GRD%ymin = minval(GRD%y); GRD%ymax = maxval(GRD%y)

! ... No initial cropping
! ...
GRD%io = 1; GRD%ni = GRD%nx
GRD%jo = 1; GRD%nj = GRD%ny
GRD%ko = 1; GRD%nk = GRD%nz
GRD%lo = 1; GRD%nl = GRD%nt

! ... Metric terms:
! ...
!allocate(GRD%pm(GRD%nx-1,GRD%ny),stat=err); if (err.NE.0) return
!allocate(GRD%pn(GRD%nx,GRD%ny-1),stat=err); if (err.NE.0) return
!
!do j=1,GRD%ny
!do i=1,GRD%nx-1
!  GRD%pm(i,j) = 1.0_dp/haversine(GRD%lon(i+1,j),GRD%lat(i+1,j), &
!                                 GRD%lon(i,j),GRD%lat(i,j))
!enddo
!enddo
!
!do j=1,GRD%ny-1
!do i=1,GRD%nx
!  GRD%pn(i,j) = 1.0_dp/haversine(GRD%lon(i,j+1),GRD%lat(i,j+1), &
!                                 GRD%lon(i,j),GRD%lat(i,j))
!enddo
!enddo
!

if (present(varname)) then
  GRD%var%xdim = .False.; GRD%var%ydim = .False.
  GRD%var%zdim = .False.; GRD%var%tdim = .False.
  do i=1,GRD%var%ndims 
    if (GRD%var%dimids(i).eq.GRD%idi) GRD%var%xdim = .True.
    if (GRD%var%dimids(i).eq.GRD%idj) GRD%var%ydim = .True.
    if (GRD%var%dimids(i).eq.GRD%idk) GRD%var%zdim = .True.
    if (GRD%var%dimids(i).eq.GRD%idl) GRD%var%tdim = .True.
  enddo

  allocate(GRD%var%mask(GRD%nx,GRD%ny,GRD%nz))
  allocate(wrk(GRD%nx,GRD%ny,GRD%nz))

  if (present(maskname)) then

    if (.not.present(maskvalue)) call stop_error(1,'ERROR in GRID_SCAN: Optional maskname without maskvalue')

    err = NF90_INQ_VARID(GRD%fid,trim(varname),mid)
    err = NF90_GET_VAR(GRD%fid,mid,wrk)
    GRD%var%mask(:,:,:) = 1.0_dp                       ! By default, all valid points
    where(wrk.eq.maskvalue) GRD%var%mask = 0.0_dp

  else
    ! ... Read an initial field:
  ! ...
    if (GRD%var%ndims.eq.2) then
      err = NF90_GET_VAR(GRD%fid,GRD%vid,wrk)
    else if (GRD%var%ndims.eq.3) then
      if (GRD%var%zdim) then
        err = NF90_GET_VAR(GRD%fid,GRD%vid,wrk)
      else
        err = NF90_GET_VAR(GRD%fid,GRD%vid,wrk,[1,1,1],[GRD%nx,GRD%ny,1])
      endif
    else
      err = NF90_GET_VAR(GRD%fid,GRD%vid,wrk,[1,1,1,1],[GRD%nx,GRD%ny,GRD%nz,1])
    endif
    if (err.ne.0) STOP 'wrk'
      
    GRD%var%mask(:,:,:) = 1.0_dp                  ! By default, all valid points
    if (GRD%var%missing) then
      if (GRD%var%missing_isnan) then
        where(isnan(wrk))                   GRD%var%mask = 0.0_dp
      else
        where(wrk.eq.GRD%var%missing_value) GRD%var%mask = 0.0_dp
      endif
    endif

  endif

  deallocate(wrk)
endif

err = 0
return

end function grid_scan
! ...
! =====================================================================
! ...
integer function grid_crop(GRD,x1,y1,x2,y2) result(err)
! ...
! ... Function grid_crop
! ... Given a bounding box, return the indices of the smallest grid
! ... containing the specified region.
! ... Crop grids accordingly.
! ... Returns err =  0  if no error
! ...         err = -1  if bbox and domain do not intersect
! ...         err = -2  invalid bbox coordinates: lower-left to upper-right corner
! ...
class(type_grid), intent(inout)        :: GRD
real(dp), intent(in)                   :: x1,y1,x2,y2   ! Bounding box in radians

! ... Local variables
! ...
integer i,j,io,jo,il,jl,ni,nj
real(dp), dimension(:,:), allocatable  :: tmp
real(dp), dimension(GRD%nx,GRD%ny,GRD%nz) :: imask

! ... Check if system intersects the provided bounding box
! ...
err = -1
if (x1.ge.GRD%lonmax) return
if (x2.le.GRD%lonmin) return
if (y1.ge.GRD%latmax) return
if (y2.le.GRD%latmin) return

! ... Check bounding box goes from SW to NE:
! ...
err = -2
if (x1.ge.x2) return
if (y1.ge.y2) return


io = -1; jo = -1
do j=1,GRD%ny-1
do i=1,GRD%nx-1
  if (GRD%lon(i,j).le.x1.and.GRD%lon(i+1,j).gt.x1.and. &
      GRD%lat(i,j).le.y1.and.GRD%lat(i,j+1).gt.y1) then
        io = i
        jo = j
        goto 1
  endif
enddo
enddo
1 continue
io = max(io-1,1); jo = max(jo-1,1)

il = GRD%nx+1; jl = GRD%ny+1
do j=1,GRD%ny-1
do i=1,GRD%nx-1
  if (GRD%lon(i,j).lt.x2.and.GRD%lon(i+1,j).ge.x2.and. &
      GRD%lat(i,j).lt.y2.and.GRD%lat(i,j+1).ge.y2) then
        il = i + 1
        jl = j + 1
        goto 2
  endif
enddo
enddo
2 continue
il = min(il+1,GRD%nx); jl = min(jl+1,GRD%ny)

! ... Cropping grids
! ...
ni = il - io + 1
nj = jl - jo + 1
allocate(tmp(ni,nj))

GRD%io = io; GRD%ni = ni
GRD%jo = jo; GRD%nj = nj

do j=jo,jo+nj-1
do i=io,io+ni-1
  tmp(i-io+1,j-jo+1) = GRD%lon(i,j)
enddo
enddo
deallocate(GRD%lon) 

allocate(GRD%lon(ni,nj))
do j=1,nj
do i=1,ni
  GRD%lon(i,j) = tmp(i,j)
enddo
enddo


do j=jo,jo+nj-1
do i=io,io+ni-1
  tmp(i-io+1,j-jo+1) = GRD%lat(i,j)
enddo
enddo
deallocate(GRD%lat)

allocate(GRD%lat(ni,nj))
do j=1,nj
do i=1,ni
  GRD%lat(i,j) = tmp(i,j)
enddo
enddo

deallocate(tmp)

if (GRD%vid.gt.0) then
  imask(:,:,:) = GRD%var%mask(:,:,:)
  deallocate (GRD%var%mask)
  allocate(GRD%var%mask(GRD%ni,GRD%nj,GRD%nk))
  do j=jo,jo+nj-1
  do i=io,io+ni-1
    GRD%var%mask(i-io+1,j-jo+1,:) = imask(i,j,:)
  enddo
  enddo
endif


err = 0
return

end function grid_crop
! ...
! =====================================================================
! ...
function grid_read2D(GRD,layer,step) result(F)

class(type_grid), intent(in)           :: GRD
integer, intent(in), optional          :: layer 
integer, intent(in), optional          :: step 
real(dp), dimension(GRD%ni,GRD%nj)     :: F

logical ZZ,TT
integer k,l,err


STATUS_ERROR = 0; STATUS_TEXT  = ""

k = GRD%ko
l = GRD%lo
if (present(layer)) then
  ZZ = .true.
  k  = GRD%ko + layer - 1
else
  ZZ = .False.
endif

if (present(step)) then
  TT = .true.
  l  = GRD%lo + step - 1
else
  TT = .False.
endif

if (k.GT.GRD%nz) then
  STATUS_ERROR = 1
  STATUS_TEXT  = 'in GRID_READ2D layer > Nz'
  return
endif

if (l.GT.GRD%nt) then
  STATUS_ERROR = 1
  STATUS_TEXT  = 'in GRID_READ2D step > Nt'
  return
endif

if (GRD%var%ndims.EQ.1) then
  ! ... Read 1-Dim : ERROR
  ! ...
  STATUS_ERROR = 1
  STATUS_TEXT  = 'in GRID_READ2D asked to read a 1D variable'
  return

else if (GRD%var%ndims.EQ.2) then
  ! ... Read 2-Dim : 
  ! ...
  err = NF90_GET_VAR(GRD%fid,GRD%vid,F,(/GRD%io,GRD%jo/),(/GRD%ni,GRD%nj/))
  if (err.ne.NF90_NOERR) then
    STATUS_ERROR = err
    STATUS_TEXT  = trim(NF90_STRERROR(err))
    return
  endif

else if (GRD%var%ndims.EQ.3) then
  ! ... Read 3-Dim : 
  ! ...
  if (GRD%var%zdim.and.GRD%var%tdim) then
    STATUS_ERROR = 1
    STATUS_TEXT  = 'in GRID_READ2D incompatible Z and T axes in a 3D variable'
    return
  endif
  if (GRD%var%zdim) then
    err = NF90_GET_VAR(GRD%fid,GRD%vid,F,(/GRD%io,GRD%jo,k/),(/GRD%ni,GRD%nj,1/))
    if (err.ne.NF90_NOERR) then
      STATUS_ERROR = err
      STATUS_TEXT  = trim(NF90_STRERROR(err))
      return
    endif
  else if (GRD%var%tdim) then
    err = NF90_GET_VAR(GRD%fid,GRD%vid,F,(/GRD%io,GRD%jo,l/),(/GRD%ni,GRD%nj,1/))
    if (err.ne.NF90_NOERR) then
      STATUS_ERROR = err
      STATUS_TEXT  = trim(NF90_STRERROR(err))
      return
    endif
  else
    STATUS_ERROR = 1
    STATUS_TEXT  = 'in GRID_READ2D undefined Z or T axis in a 3D variable'
    return
  endif

else if (GRD%var%ndims.EQ.4) then
  ! ... Read 4-Dim : 
  ! ...
  err = NF90_GET_VAR(GRD%fid,GRD%vid,F,(/GRD%io,GRD%jo,k,l/),(/GRD%ni,GRD%nj,1,1/))
  if (err.ne.NF90_NOERR) then
    STATUS_ERROR = err
    STATUS_TEXT  = trim(NF90_STRERROR(err))
    return
  endif

else
  STATUS_ERROR = 1
  STATUS_TEXT  = 'in GRID_READ2D asked to read a variable with more than 4 dims'
  return
endif

! ... Scale the field:
! ...
if (GRD%var%missing) then
  if (GRD%var%missing_isnan) then
    where(isnan(F))
      F = zero
    elsewhere
      F = GRD%var%add_offset + GRD%var%scale_factor*F
    endwhere
  else
    where((F.eq.GRD%var%missing_value))
      F = zero
    elsewhere
      F = GRD%var%add_offset + GRD%var%scale_factor*F
    endwhere
  endif
else
  F = GRD%var%add_offset + GRD%var%scale_factor*F
endif

end function grid_read2D
! ...
! =====================================================================
! ...
subroutine grid_locate2D(GRD,xo,yo,il,jl)

class(type_grid), intent(in)           :: GRD
real(dp), intent(in)                   :: xo,yo    ! Radians
integer, intent(out)                   :: il,jl

! ... Local variables
! ...
logical slope1,slope2
integer n1,n2,im,iu,jm,ju

STATUS_ERROR = 0; STATUS_TEXT  = ""

n1 = GRD%ni
n2 = GRD%nj

slope1 = GRD%lon(n1,n2).gt.GRD%lon(1,1)
slope2 = GRD%lat(n1,n2).gt.GRD%lat(1,1)

il = 0; iu = n1 + 1
jl = 0; ju = n2 + 1
do while ((ju-jl.gt.1).or.(iu-il.gt.1))
  im = (iu+il)/2
  jm = (ju+jl)/2
  if (iu-il.gt.1) then
    im = (iu+il)/2
    if (slope1.eqv.(xo.gt.GRD%lon(im,jm))) then
      il = im
    else
      iu = im
    endif
  endif
  if (ju-jl.gt.1) then
    jm = (ju+jl)/2
    if (slope2.eqv.(yo.gt.GRD%lat(im,jm))) then
      jl = jm
    else
      ju = jm
    endif
  endif
enddo

return
end subroutine grid_locate2D
! ...
! =====================================================================
! ...
!integer function grid_lbracket(GRD,time) result(err)
!! ...
!! ... Function grid_lbracket
!! ... Left bracket.
!! ... Given a grid and a give time, this function sets GRD.lo to point the 
!! ... immediately precedent time record. That is
!! ...         GRD%t(GRD.lo) < time <= GRD%t(GRD%lo)
!! ... 
!! ... Returns err =  0  if no error
!! ...         err = -1  if time < GRD%t(1)
!! ...         err = -2  if time > GRD%t(GRD%nt)
!! ...
!
!class(type_grid), intent(inout)        :: GRD
!real(dp), intent(in)                   :: time    ! In JD
!
!STATUS_ERROR = 0; STATUS_TEXT  = ""
!
!if (time.le.GRD%t(1)) then
!  err = -1
!  STATUS_ERROR = -1
!  STATUS_TEXT = "in GRID_LBRACKET. Requested time <= Initial time"
!  return
!endif
!
!if (time.gt.GRD%t(GRD%nt)) then
!  err = -2
!  STATUS_ERROR = -2
!  STATUS_TEXT = "in GRID_LBRACKET. Requested time >  Final time"
!  return
!endif
!
!GRD%lo = locate(GRD%t(:),time)
!GRD%nl = GRD%nt - GRD%lo + 1
!
!end function grid_lbracket
! ...
! =====================================================================
! ...
integer function grid_bracket(GRD,tmin,tmax) result(err)
! ...
! ... Function grid_lbracket
! ... Left bracket.
! ... Given a grid and a give time, this function sets GRD.lo to point the 
! ... immediately precedent time record. That is
! ...         GRD%t(GRD.lo) < time <= GRD%t(GRD%lo)
! ... 
! ... Returns err =  0  if no error
! ...         err = -1  if tmin/tmax < GRD%t(1)
! ...         err = -2  if tmin/tmax > GRD%t(GRD%nt)
! ...         err = -3  if tmin > tmax
! ...

class(type_grid), intent(inout)        :: GRD
real(dp), intent(in)                   :: tmin,tmax    ! In JD

! ... Local variables
! ...
type(type_date) dd
integer l
real(dp), dimension(:), allocatable    :: tmp

STATUS_ERROR = 0; STATUS_TEXT  = ""

if (tmin.gt.tmax) then
  err = -3
  STATUS_ERROR = -3
  STATUS_TEXT  = "in GRID_BRACKET. Invalid time range"
  return
endif

if (tmin.lt.GRD%t(1)) then
  err = -1
  STATUS_ERROR = -1
  STATUS_TEXT = "in GRID_BRACKET. Requested tmin < Initial time"
  return
endif

if (tmin.gt.GRD%t(GRD%nt)) then
  err = -2
  STATUS_ERROR = -2
  STATUS_TEXT = "in GRID_BRACKET. Requested tmin >  Final time"
  return
endif

if (tmax.lt.GRD%t(1)) then
  err = -1
  STATUS_ERROR = -1
  STATUS_TEXT = "in GRID_BRACKET. Requested tmax < Initial time"
  return
endif

if (tmax.gt.GRD%t(GRD%nt)) then
  err = -2
  STATUS_ERROR = -2
  STATUS_TEXT = "in GRID_BRACKET. Requested tmax >  Final time"
  return
endif

GRD%lo = max(locate(GRD%t(:),tmin) - 1, 1)
l      = min(locate(GRD%t(:),tmax) + 1, GRD%nt)
GRD%nl = l - GRD%lo + 1

allocate(tmp(GRD%nl))
do l=GRD%lo,GRD%lo+GRD%nl-1
  tmp(l-GRD%lo+1) = GRD%t(l)
enddo

deallocate(GRD%t)
deallocate(GRD%s)
deallocate(GRD%date)

allocate(GRD%t(GRD%nl))
allocate(GRD%s(GRD%nl))
allocate(GRD%date(GRD%nl))

GRD%t(:) = tmp(:)
GRD%s(:) = 86400.0D0*tmp(:)
do l=1,GRD%nl
  dd = jd2date(GRD%t(l))
  GRD%date(l) = dd
enddo

deallocate(tmp)

end function grid_bracket
! ...
! =====================================================================
! ...
subroutine grid_show(GRD,Label)

class(type_grid), intent(in)               :: GRD
character(len=*), intent(in)               :: Label

! ... Local variables
! ...
type(type_date) di,df
integer dt

di = GRD%date(1)
df = GRD%date(GRD%nt)

write(*,*) 
write(*,*) '-------------------------------'
write(*,*) trim(Label)
write(*,*) 'Filename: ', trim(GRD%filename)
write(*,*) 'Variable: ', trim(GRD%varname)
write(*,*) 'Nx, Ny, Nz, Nt   : ', GRD%Nx, GRD%Ny, GRD%nz, GRD%nt
write(*,*) 'West  (deg, rad) : ', GRD%lonmin_deg,GRD%lonmin
write(*,*) 'East  (deg, rad) : ', GRD%lonmax_deg,GRD%lonmax
write(*,*) 'South (deg, rad) : ', GRD%latmin_deg,GRD%latmin
write(*,*) 'North (deg, rad) : ', GRD%latmax_deg,GRD%latmax
write(*,*) 'Initial date     :    ', trim(di%iso())
write(*,*) 'Final   date     :    ', trim(df%iso())
write(*,*) 'Time interval (s): ', GRD%dt
write(*,*) 

end subroutine grid_show
! ...
! =====================================================================
! ...
real(dp) function grid_hinterpol(GRD,F,xo,yo) result(fo)

class(type_grid), intent(in)                   :: GRD
real(dp), dimension(GRD%ni,GRD%nj), intent(in) :: F
real(dp), intent(in)                           :: xo,yo

! ... Local variables
! ...
integer i,j
real(dp) y1,y2,y3,y4
real(dp) d1,d2,d3,d4
real(dp) w1,w2,w3,w4

call GRD%locate(xo,yo,i,j)

y1 = F(i,j)
y2 = F(i+1,j)
y3 = F(i+1,j+1)
y4 = F(i,j+1)

d1 = haversine(xo,yo,GRD%lon(i,j),GRD%lat(i,j))
d2 = haversine(xo,yo,GRD%lon(i+1,j),GRD%lat(i+1,j))
d3 = haversine(xo,yo,GRD%lon(i+1,j+1),GRD%lat(i+1,j+1))
d4 = haversine(xo,yo,GRD%lon(i,j+1),GRD%lat(i,j+1))

w1 = d2*d3*d4
w2 = d1*d3*d4
w3 = d1*d2*d4
w4 = d1*d2*d3

fo = (w1*y1 + w2*y2 + w3*y3 + w4*y4)/(w1+w2+w3+w4)

return
end function grid_hinterpol
! ...
! =====================================================================
! ...
integer function grid_type(GRD,xo,yo,zo) result(ptype)
! ... Bilinear interpolation of the mask
! ... It returns 1 if point is in water
! ...            0 if point is in land.

class(type_grid), intent(in)                   :: GRD
real(dp), intent(in)                           :: xo,yo
real(dp), optional, intent(in)                 :: zo

! ... Local variables
! ...
integer i,j
real(dp) y1,y2,y3,y4
real(dp) d1,d2,d3,d4
real(dp) w1,w2,w3,w4
real(dp) fo

ptype = -1
if (xo.lt.GRD%lon(1,1)) return
if (xo.gt.GRD%lon(GRD%ni,GRD%nj)) return
if (yo.lt.GRD%lat(1,1)) return
if (yo.gt.GRD%lat(GRD%ni,GRD%nj)) return

call GRD%locate(xo,yo,i,j)

y1 = GRD%var%mask(i,j,1)
y2 = GRD%var%mask(i+1,j,1)
y3 = GRD%var%mask(i+1,j+1,1)
y4 = GRD%var%mask(i,j+1,1)

d1 = haversine(xo,yo,GRD%lon(i,j),GRD%lat(i,j))
d2 = haversine(xo,yo,GRD%lon(i+1,j),GRD%lat(i+1,j))
d3 = haversine(xo,yo,GRD%lon(i+1,j+1),GRD%lat(i+1,j+1))
d4 = haversine(xo,yo,GRD%lon(i,j+1),GRD%lat(i,j+1))

w1 = d2*d3*d4
w2 = d1*d3*d4
w3 = d1*d2*d4
w4 = d1*d2*d3

fo = (w1*y1 + w2*y2 + w3*y3 + w4*y4)/(w1+w2+w3+w4)

if (fo.ge.0.5_dp) then
  ptype = 1                ! Open sea
else
  ptype = 0                ! Land
endif
 
return
end function grid_type
! ...
! =====================================================================
! ...
end module module_grid
