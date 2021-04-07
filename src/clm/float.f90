! ********************************************************************
! ... float.f90
! ... Quim Ballabrera
! ...
! ********************************************************************

module module_float

use module_types, only: dp
use module_constants, only: deg2rad
use module_datetime
use module_options
use module_forcing

implicit none

private read_ascii


type type_float
  ! ... Current float position and deptg at time t (seconds since time_orig)
  ! ...
  integer                                 :: Nfloats
  logical, dimension(:), pointer          :: released 
  logical, dimension(:), pointer          :: outside 
  logical, dimension(:), pointer          :: surface 
  logical, dimension(:), pointer          :: floating
  logical, dimension(:), pointer          :: stranded

  ! ... The floats exit codes are:
  ! ...  -1: Not released
  ! ...   0: Floating
  ! ...   1: Left the system
  ! ...   2: Stranded
  ! ..
  integer, dimension(:), pointer          :: exitcode 

  real(dp), dimension(:), pointer         :: x                ! lon in radians
  real(dp), dimension(:), pointer         :: y                ! lat in radians
  real(dp), dimension(:), pointer         :: z                ! meters 
  real(dp), dimension(:), pointer         :: t                ! seconds
  real(dp), dimension(:), pointer         :: dist             ! travelled distance (m)

  ! ... Each float can be advected by a different layer of the model
  ! ... The layer will depend on the number of layers in the model
  ! ... and also depend on the released depth.
  ! ...
  integer, dimension(:), pointer          :: k                ! Model layer
  ! ...
  ! ... Interpolated velocity field
  ! ...
  real(dp), dimension(:), pointer         :: u                ! m/s
  real(dp), dimension(:), pointer         :: v                ! m/s
  ! ...
  ! ... Release information
  ! ...
  real(dp), dimension(:), pointer         :: release_lon      ! radians
  real(dp), dimension(:), pointer         :: release_lat      ! radians
  real(dp), dimension(:), pointer         :: release_depth    ! meters
  real(dp), dimension(:), pointer         :: release_time     ! seconds
  type(type_date), dimension(:), pointer  :: release_date     ! date            

  ! ... Random:
  ! ...
  logical                                 :: random_floats
  integer                                 :: Nrandom  = 1
  real(dp)                                :: Radius_x = 0.02_dp
  real(dp)                                :: Radius_y = 0.02_dp

  ! ... Miscellania
  ! ...
  real(dp)                                :: missing = -999.0_dp
  real(dp)                                :: model_ref = 0.0_dp  ! Julian Day

  contains
    procedure         :: allocate  => float_allocate

end type type_float

type(type_float)                          :: FLT

contains
! ...
! ====================================================================
! ...
subroutine float_ini(model_jdref)

real(dp), intent(in)                 :: model_jdref

! ... Local variables
! ...
integer n ,i

! ... First of all, tell the float structure the reference time
! ... of the model
! ...
FLT%model_ref = model_jdref

! ... Next, check about the need to generate random release points
! ...
if (withRandom) then 
  FLT%random_floats = .True.
  FLT%Nrandom = userNrandom
  if (withRx) FLT%Radius_x = userRx
  if (withRy) FLT%Radius_y = userRy
endif

write(*,*)
if (withRelease) then
  ! ... Read release information from file
  ! ...
  write(*,*) 'Reading release information from file: ', trim(releaseName)
  n = read_ascii(releaseName)
else
  ! ... Read release information from command line
  ! ...
  write(*,*) 'Release information from command line'
  n = read_commandline()
endif
write(*,*) 'Number floats released:', FLT%Nfloats

if (verb) then
  write(*,*) 'Floats to be released'
  write(*,*) '   lon      lat     depth         date             seconds since ref'
  write(*,*) '===================================================================='
  do i=1,FLT%Nfloats
    write(*,'(F9.3,F9.3,F7.1,3X,A,F9.0)') FLT%release_lon(i), FLT%release_lat(i), &
                                          FLT%release_depth(i), FLT%release_date(i)%iso(), &
                                          FLT%release_time(i)

  enddo
endif


end subroutine float_ini
! ...
! ====================================================================
! ...
integer function read_ascii(filename) result(n)
! ... Routine to read float release positions from an ASCII file.
! ... Two formats are allowed
! ... LON, LAT, DEPTH, SECONDS_SINCE_REFERENCE_TIME  (6.0, 35.4, 0.0, 3600.)
! ... LON, LAT, DEPTH, DATE-AND-TIME OF THE RELEASEA (6.0, 35.4, 0.0, 2018-02-20T00:30:00)
! ...

character(len=*), intent(in)             :: filename

! ... Local variables
! ...
logical withdate,valid
integer iu,i,ii,j,iio,nheader,nlines,nmax
real(dp) x,y,z,t,rnd(4),xmin,ymin,xx,yy,xxr,yyr,zz
character(len=maxlen) line,str

real(dp), dimension(:), allocatable         :: release_lon   
real(dp), dimension(:), allocatable         :: release_lat   
real(dp), dimension(:), allocatable         :: release_depth 
real(dp), dimension(:), allocatable         :: release_time  
type(type_date), dimension(:), allocatable  :: release_date 


iu = unitfree()
open(iu,file=filename,status='old')
nlines = numlines(iu)

! ... Check for header lines
! ...
nheader = 0
do i=1,nlines
  read(iu,'(A)') line
  if (line(1:1).eq.'#') nheader = nheader + 1
enddo
nlines = nlines - nheader
nmax   = nlines * FLT%Nrandom

allocate(release_lon(nmax))
allocate(release_lat(nmax))
allocate(release_depth(nmax))
allocate(release_time(nmax))
allocate(release_date(nmax))

! ... Check with format has been used
! ...
rewind(iu)
do i=1,nheader
  read(iu,*)
enddo

read(iu,'(A)') line
i = index(line,'T')
if (i.gt.0) then
  withdate = .True.
else
  withdate = .False.
endif

rewind(iu)
do i=1,nheader
  read(iu,*)
enddo

 ii = 0
 do i=1,nlines
   if (withdate) then
     read(iu,*) xx, yy, zz, str
   else
     read(iu,*) xx, yy, zz, t
   endif
   xxr = deg2rad*xx; yyr = deg2rad*yy
   valid = .True.
   if (GOU%point_type(xxr,yyr).eq.-1) valid = .False.    ! Out of system
   if (GOV%point_type(xxr,yyr).eq.-1) valid = .False.    ! Out of system
   if (GOU%point_type(xxr,yyr).eq. 0) valid = .False.    ! Land point
   if (GOV%point_type(xxr,yyr).eq. 0) valid = .False.    ! Land point
   if (valid) then
     ii = ii + 1
     release_lon(ii)   = xx
     release_lat(ii)   = yy
     release_depth(ii) = zz
     if (withdate) then
       release_date(ii) = strptime(str)
       release_time(ii) = nint((release_date(ii)%jd()-FLT%model_ref)*86400.0_dp)
     else
       release_date(ii) = jd2date(FLT%model_ref + t/86400.0_dp)
       release_time(ii) = t
     endif

     ! ... Now check for Random points
     ! ...
     iio = ii
     xmin = release_lon(iio) - 0.5_dp*FLT%Radius_x
     ymin = release_lat(iio) - 0.5_dp*FLT%Radius_y
     do j=1,FLT%Nrandom-1
20     call RANDOM_NUMBER(rnd)               ! Uniform distribution
       xx  = xmin + FLT%Radius_x*rnd(1)
       yy  = ymin + FLT%Radius_y*rnd(2)
       xxr = deg2rad*xx; yyr = deg2rad*yy
       if (GOU%point_type(xxr,yyr).eq.-1) goto 20    ! Out of system
       if (GOV%point_type(xxr,yyr).eq.-1) goto 20    ! Out of system 
       if (GOU%point_type(xxr,yyr).eq. 0) goto 20    ! Land
       if (GOV%point_type(xxr,yyr).eq. 0) goto 20    ! Land
       ii = ii + 1
       release_lon(ii)   = xx
       release_lat(ii)   = yy
       release_depth(ii) = release_depth(iio)
       release_date(ii)  = release_date(iio)
       release_time(ii)  = release_time(iio)
     enddo
   else
     write(*,*) 'WARNING: release position not retained ', xx,yy,zz
   endif
 enddo
n = ii

if (n.eq. 0) return

! ... Copy float data into FLT structure:
! ...
call FLT%allocate(n)
do i=1,n
  FLT%release_lon(i)   = release_lon(i)
  FLT%release_lat(i)   = release_lat(i)
  FLT%release_depth(i) = release_depth(i)
  FLT%release_time(i)  = release_time(i)
  FLT%release_date(i)  = release_date(i)
enddo

deallocate(release_lon)
deallocate(release_lat)
deallocate(release_depth)
deallocate(release_time)
deallocate(release_date)

do i=1,n
  if (FLT%release_depth(i).eq.0) then
    FLT%surface(i) = .True.
  else
    FLT%surface(i) = .False.
  endif
enddo

FLT%x(:) = deg2rad*FLT%release_lon(:)
FLT%y(:) = deg2rad*FLT%release_lat(:)
FLT%z(:) = FLT%release_depth(:)
FLT%dist(:) = 0.0_dp

end function read_ascii
! ...
! ====================================================================
! ...
integer function read_commandline() result(n)

! ... Local variables
! ...
logical withdate,valid
integer i,ii,j,iio
real(dp) x,y,z,t,rnd(4),xmin,ymin,xx,yy,xxr,yyr,zz
character(len=maxlen) line,str

real(dp), dimension(:), allocatable         :: release_lon   
real(dp), dimension(:), allocatable         :: release_lat   
real(dp), dimension(:), allocatable         :: release_depth 
real(dp), dimension(:), allocatable         :: release_time  
type(type_date), dimension(:), allocatable  :: release_date 


n = 1 * FLT%Nrandom

xxr = deg2rad*Release_xo; yyr = deg2rad*Release_yo
valid = .True.
if (GOU%point_type(xxr,yyr).eq.-1) valid = .False.    ! Out of system
if (GOV%point_type(xxr,yyr).eq.-1) valid = .False.    ! Out of system
if (GOU%point_type(xxr,yyr).eq. 0) valid = .False.    ! Land point
if (GOV%point_type(xxr,yyr).eq. 0) valid = .False.    ! Land point


if (.Not.valid) then
  write(*,*) 'WARNING: release position not retained ', xx,yy,zz
  n = 0
  return
endif

allocate(release_lon(n))
allocate(release_lat(n))
allocate(release_depth(n))
allocate(release_time(n))
allocate(release_date(n))

! ... Check with format has been used
! ...
if (.not.fto) then
 withdate = .False.
 Release_tstr = "0"
 Release_to = 0.0D0
else
 if (word_type(Release_tstr).eq.3) then
   ! ... The user has specified a date
   ! ...
   withdate = .True.
 else
   ! ... We treat the user input as an integer (seconds since reference date)
   ! ...
   withdate = .False.
 endif
endif

! ... 
xx = Release_xo
yy = Release_yo
zz = Release_zo

if (withdate) then
  str = trim(Release_tstr)
else
  read(Release_tstr,*) t
endif


xxr = deg2rad*xx; yyr = deg2rad*yy
valid = .True.
if (GOU%point_type(xxr,yyr).eq.-1) valid = .False.    ! Out of system
if (GOV%point_type(xxr,yyr).eq.-1) valid = .False.    ! Out of system
if (GOU%point_type(xxr,yyr).eq. 0) valid = .False.    ! Land point
if (GOV%point_type(xxr,yyr).eq. 0) valid = .False.    ! Land point

ii = 1
release_lon(ii)   = xx
release_lat(ii)   = yy
release_depth(ii) = zz
if (withdate) then
  release_date(ii) = strptime(str)
  release_time(ii) = nint((release_date(ii)%jd()-FLT%model_ref)*86400.0_dp)
else
  release_date(ii) = jd2date(FLT%model_ref + t/86400.0_dp)
  release_time(ii) = t
endif

! ... Now check for Random points
! ...
iio = ii
xmin = release_lon(iio) - 0.5_dp*FLT%Radius_x
ymin = release_lat(iio) - 0.5_dp*FLT%Radius_y
do j=1,FLT%Nrandom-1
20   call RANDOM_NUMBER(rnd)               ! Uniform distribution
     xx  = xmin + FLT%Radius_x*rnd(1)
     yy  = ymin + FLT%Radius_y*rnd(2)
     xxr = deg2rad*xx; yyr = deg2rad*yy
     if (GOU%point_type(xxr,yyr).eq.-1) goto 20    ! Out of system
     if (GOV%point_type(xxr,yyr).eq.-1) goto 20    ! Out of system 
     if (GOU%point_type(xxr,yyr).eq. 0) goto 20    ! Land
     if (GOV%point_type(xxr,yyr).eq. 0) goto 20    ! Land
     ii = ii + 1
     release_lon(ii)   = xx
     release_lat(ii)   = yy
     release_depth(ii) = release_depth(iio)
     release_date(ii)  = release_date(iio)
     release_time(ii)  = release_time(iio)
enddo
n = ii

if (n.eq. 0) return

! ... Copy float data into FLT structure:
! ...
call FLT%allocate(n)
do i=1,n
  FLT%release_lon(i)   = release_lon(i)
  FLT%release_lat(i)   = release_lat(i)
  FLT%release_depth(i) = release_depth(i)
  FLT%release_time(i)  = release_time(i)
  FLT%release_date(i)  = release_date(i)
enddo

deallocate(release_lon)
deallocate(release_lat)
deallocate(release_depth)
deallocate(release_time)
deallocate(release_date)

do i=1,n
  if (FLT%release_depth(i).eq.0) then
    FLT%surface(i) = .True.
  else
    FLT%surface(i) = .False.
  endif
enddo

FLT%x(:) = deg2rad*FLT%release_lon(:)
FLT%y(:) = deg2rad*FLT%release_lat(:)
FLT%z(:) = FLT%release_depth(:)
FLT%dist(:) = 0.0_dp


end function read_commandline
! ...
! ====================================================================
! ...
subroutine float_allocate(F,n)

class(type_float)            :: F
integer, intent(in)          :: n

F%Nfloats = n

allocate(F%released(n))
allocate(F%outside(n))
allocate(F%surface(n))
allocate(F%floating(n))
allocate(F%x(n))
allocate(F%y(n))
allocate(F%z(n))
allocate(F%t(n))
allocate(F%dist(n))
allocate(F%u(n))
allocate(F%v(n))
allocate(F%release_lon(n))
allocate(F%release_lat(n))
allocate(F%release_depth(n))
allocate(F%release_time(n))
allocate(F%release_date(n))
allocate(F%exitcode(n))
allocate(F%stranded(n))
allocate(F%k(n))

F%released(:)  = .false.
F%floating(:)  = .false.
F%surface(:)   = .false.
F%outside(:)   = .false.
F%stranded(:)  = .false.
F%exitcode(:)  = -1

end subroutine float_allocate
! ...
! ====================================================================
! ...
end module module_float
