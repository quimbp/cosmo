! ****************************************************************************
! ... floats.f90
! ... Quim Ballabrera, April 2017
! ... COSMO Lagrangian model
! ... Structure of float variables
! ... Subroutines:
! ...   floats_ini
! ...   floats_read
! ...   floats_alloc
! ... The floats exit codes are:
! ...  -1: Not released
! ...   0: Floating
! ...   1: Left the system
! ...   2: Stranded
! ... Version 0.1, released October 2017
! ... Version 0.2, released December 2017
! ...              New float release options from command line
! ...              Allow user to change the name of the initial release file
! ****************************************************************************

module mod_floats

use cosmo
use netcdf

implicit none

! ... Release file
! ...
logical                                  :: fre_in           = .false.
logical                                  :: fre_out          = .false.
logical                                  :: random_floats    = .false.
logical                                  :: fnp              = .false.
logical                                  :: ffx              = .false.
logical                                  :: ffy              = .false.
logical                                  :: ffz              = .false.
logical                                  :: fft              = .false.
logical                                  :: ffd              = .false.
logical                                  :: frx              = .false.
logical                                  :: fry              = .false.
logical                                  :: frt              = .false.
character(len=180)                       :: release_file_in  = 'release.ini'
character(len=180)                       :: release_file_out = 'release.out'
integer                                  :: Nfloats          = 10
real(dp)                                 :: Radius_x         = 0.1D0
real(dp)                                 :: Radius_y         = 0.1D0
real(dp)                                 :: Radius_t         = zero


real(dp)                                 :: fxo
real(dp)                                 :: fyo
real(dp)                                 :: fzo              = zero
real(dp)                                 :: fto              = zero
character(len=20)                        :: fdo              = ''
type(date_type)                          :: fdateo         ! Release date


type floater
  integer                                :: n = 0
  real(dp)                               :: missing        ! missing value
  real(dp), dimension(:), pointer        :: lon            ! lon   degrees
  real(dp), dimension(:), pointer        :: lat            ! lat   degrees
  real(dp), dimension(:), pointer        :: depth          ! depth meters > 0
  real(dp), dimension(:), pointer        :: dist           ! traveled dist (km)
  real(dp), dimension(:), pointer        :: time           ! time position
  real(dp), dimension(:), pointer        :: release_time   ! release time (sec)
  real(dp), dimension(:), pointer        :: u              ! x velocity
  real(dp), dimension(:), pointer        :: v              ! y velocity
  real(dp), dimension(:), pointer        :: w              ! z velocity
  real(dp), dimension(:), pointer        :: temp           ! temperature
  real(dp), dimension(:), pointer        :: salt           ! salinity
  real(dp), dimension(:), pointer        :: dens           ! density
  real(dp), dimension(:), pointer        :: UDF            ! User-defined funct
  type(date_type)                        :: date           ! A given date
  integer, dimension(:), pointer         :: exitcode       ! exit code
  logical, dimension(:), pointer         :: released       ! released flag
  logical, dimension(:), pointer         :: floating       ! status flag
  logical, dimension(:), pointer         :: stranded       ! status flag
  logical, dimension(:), pointer         :: outside        ! status flag
end type floater

contains

subroutine floats_ini(FLT,x,y,land)

type(floater), intent(out)                 :: FLT
real(dp), dimension(:), intent(in)         :: x
real(dp), dimension(:), intent(in)         :: y
logical, dimension(:,:), intent(in)        :: land

logical itsbeach,itsout
integer ii,jj,flo,iu
real(dp) xmin,ymin,tmin,x0,x1,y0,y1,rnd(3)

x0 = minval(x)
x1 = maxval(x)
y0 = minval(y)
y1 = maxval(y)


if (fre_in) then
  call floats_read(release_file_in,FLT)
else 
  if (count((/ffx,ffy/)).eq.1) &
     call stop_error(1,'Invalid use of options -xo and -yo')

  if (ffx.and.ffy) then
    if (random_floats) then
      write(*,*) 
      write(*,*) 'Selecting a random cloud of points'
      write(*,*) 'Center point (x,y) : ', fxo,fyo
      write(*,*) 'Radius_x           : ', Radius_x
      write(*,*) 'Radius_y           : ', Radius_y
      FLT%n = Nfloats
      call floats_alloc (FLT)
      xmin = fxo - half*Radius_x
      ymin = fyo - half*Radius_y
      if (fto.eq.0) then
        tmin = fto 
      else
        tmin = fto - half*Radius_t
      endif
      do flo=1,FLT%n
        20 call RANDOM_NUMBER(rnd)
        FLT%lon(flo) = Radius_x*rnd(1) + xmin
        FLT%lat(flo) = Radius_y*rnd(2) + ymin
        FLT%release_time(flo) = Radius_t*rnd(3) + tmin
        ii = locate(x,FLT%lon(flo))
        jj = locate(y,FLT%lat(flo))
        itsbeach = any((/land(ii,jj),      &
                         land(ii+1,jj),    &
                         land(ii+1,jj+1),  &
                         land(ii,jj+1)/))
        itsout = any((/FLT%lon(flo).le.x0, &
                      FLT%lon(flo).ge.x1,  &
                      FLT%lat(flo).le.y0,  &
                      FLT%lat(flo).ge.y1/))
        if (itsbeach.or.itsout) goto 20
      enddo
      FLT%depth(:)        = fzo
      FLT%stranded(:)     = .false.
      FLT%outside(:)      = .false.
    else
      FLT%n = 1
      call floats_alloc (FLT)
      FLT%lon(1)          = fxo
      FLT%lat(1)          = fyo
      FLT%depth(1)        = fzo
      FLT%release_time(:) = fto
    endif
  else if (random_floats) then
    write(*,*) 
    write(*,*) 'Selecting a random set of points'
    FLT%n = Nfloats
    call floats_alloc (FLT)
    xmin = x0
    ymin = y0
    do flo=1,FLT%n
      30 call RANDOM_NUMBER(rnd)
      FLT%lon(flo) = (x1-x0)*rnd(1) + xmin
      FLT%lat(flo) = (y1-y0)*rnd(2) + ymin
      ii = locate(x,FLT%lon(flo))
      jj = locate(y,FLT%lat(flo))
      itsbeach = any((/land(ii,jj),      &
                       land(ii+1,jj),    &
                       land(ii+1,jj+1),  &
                       land(ii,jj+1)/))
      itsout = any((/FLT%lon(flo).le.x0, &
                    FLT%lon(flo).ge.x1,  &
                    FLT%lat(flo).le.y0,  &
                    FLT%lat(flo).ge.y1/))
      if (itsbeach.or.itsout) goto 30
    enddo
    FLT%depth(:)        = fzo
    FLT%release_time(:) = fto             ! v0.2: Can be user specified
    FLT%stranded(:)     = .false.
    FLT%outside(:)      = .false.
  else
    call stop_error(1,'Incorrect specification floats releasing')
  endif

  write(*,*) 'Saving release information in : ', trim(release_file_in)
  iu = unitfree()
  open(iu,file=release_file_in,status='unknown')

  write(*,*)
  write(*,*) 'Initial position, and release time (secs after initial time) of the floats'
  write(*,*) '    lon,      lat,      depth,  release_time'

  rewind(iu)
  do flo=1,FLT%n
    write(*,'(3F10.4,F11.0)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
                     FLT%release_time(flo)
    write(iu,'(3F10.4,F11.0)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
                     FLT%release_time(flo)
  enddo
  close(iu)

endif

end subroutine floats_ini
! ...
! ============================================================================
! ...
subroutine floats_read(rfile,FLT)

character(len=*), intent(in)            :: rfile
TYPE(floater), intent(out)              :: FLT

! ... Local variables
! ...
integer                                 :: i,err
integer                                 :: ffid
integer                                 :: idp,idl,idx,idy,idz,idt,ide,idr,idd
integer                                 :: idtem,idsal
integer                                 :: Nrec,frec
real(dp), dimension(1)                  :: time

! ... An input float file has been selected
! ... It could be an ASCII file or a Netcdf file
! ...
if (filetype(rfile).eq.'asc') then
  ! ... ASCII input:
  ! ...
  write(*,*)
  write(*,*) 'Opening ASCII file: ', trim(rfile)
  open(10,file=rfile,status='old')
  FLT%n = numlines(10)
  write(*,*) 'Number of entries: ', FLT%n

  ! ... Float tables allocation
  ! ...
  call floats_alloc (FLT)

  FLT%dist(:) = zero
  do i=1,FLT%n
    read(10,*) FLT%lon(i), FLT%lat(i), FLT%depth(i), FLT%release_time(i)
    write(*,'(2F9.3,F12.0)') FLT%lon(i),FLT%lat(i),FLT%release_time(i)
  enddo
  FLT%floating(:) = .false.
  FLT%released(:) = .false.
  close(10)

else if (filetype(rfile).eq.'cdf') then
  ! ... NetCDF input:
  ! ...
  write(*,*)
  write(*,*) 'Opening NetCDF file: ', trim(rfile)
  err = NF90_OPEN(rfile,NF90_NOWRITE,ffid)
  call cdf_error(err,'Unable to open float input file')
  err = NF90_INQ_DIMID(ffid,'particle',idp)
  call cdf_error(err,'Unable to inquire about float dimension')
  err = NF90_INQ_DIMID(ffid,'time',idl)
  call cdf_error(err,'Unable to inquire about time dimension')
  err = NF90_INQUIRE_DIMENSION(ffid,idp,len=FLT%n)
  err = NF90_INQUIRE_DIMENSION(ffid,idl,len=Nrec)
  err = NF90_INQ_VARID(ffid,'lon',idx)
  call cdf_error(err,'Unable to inquire about lon variable')
  err = NF90_INQ_VARID(ffid,'lat',idy)
  call cdf_error(err,'Unable to inquire about lat variable')
  err = NF90_INQ_VARID(ffid,'depth',idz)
  call cdf_error(err,'Unable to inquire about depth variable')
  err = NF90_INQ_VARID(ffid,'time',idt)
  call cdf_error(err,'Unable to inquire about time variable')
  err = NF90_INQ_VARID(ffid,'distance',idd)
  call cdf_error(err,'Unable to inquire about distance variable')

  !if (fte) then
  err = NF90_INQ_VARID(ffid,'temperature',idtem) 
  call cdf_error(err,'Unable to inquire about temperature variable')
  !endif

  !if (fsa) then
  err = NF90_INQ_VARID(ffid,'salinity',idsal)
  call cdf_error(err,'Unable to inquire about salinity variable')
  !endif

  err = NF90_INQ_VARID(ffid,'exitcode',ide)
  call cdf_error(err,'Unable to inquire about exitcode variable')
  err = NF90_INQ_VARID(ffid,'releasedate',idr)
  call cdf_error(err,'Unable to inquire about releasedate variable')
  !if (.not.fir) frec = Nrec

  ! ... Float tables allocation
  ! ...
  call floats_alloc (FLT)

  err = NF90_GET_VAR(ffid,idx,FLT%lon,(/1,frec/),(/FLT%n,1/))
  call cdf_error(err,'Unable to read lon')
  err = NF90_GET_VAR(ffid,idy,FLT%lat,(/1,frec/),(/FLT%n,1/))
  call cdf_error(err,'Unable to read lat')
  err = NF90_GET_VAR(ffid,idz,FLT%depth,(/1,frec/),(/FLT%n,1/))
  call cdf_error(err,'Unable to read depth')
  err = NF90_GET_VAR(ffid,idt,time,(/frec/),(/1/))
  call cdf_error(err,'Unable to read time')
  FLT%time(:) = time(1)
  err = NF90_GET_VAR(ffid,idd,FLT%dist,(/1,frec/),(/FLT%n,1/))
  call cdf_error(err,'Unable to read distance')
  err = NF90_GET_VAR(ffid,idtem,FLT%temp,(/1,frec/),(/FLT%n,1/))
  call cdf_error(err,'Unable to read temperature')
  err = NF90_GET_VAR(ffid,idsal,FLT%salt,(/1,frec/),(/FLT%n,1/))
  call cdf_error(err,'Unable to read temperature')
  err = NF90_GET_VAR(ffid,idr,FLT%release_time)
  call cdf_error(err,'Unable to read releasedate')
  err = NF90_GET_VAR(ffid,ide,FLT%exitcode)
  call cdf_error(err,'Unable to read exitcode')
  err = NF90_CLOSE(ffid)
  call cdf_error(err,'Unable to close file')
  FLT%released(:) = .false.
  FLT%floating(:) = .false.

  do i=1,FLT%n
    write(*,'(I4,3F9.3,F13.0,2F9.3,X,L,L)') i,FLT%lon(i), FLT%lat(i),  &
                                            FLT%depth(i), FLT%time(i), &
                                            FLT%temp(i), FLT%salt(i),  &
                                            FLT%released(i), FLT%floating(i)
  enddo
else
  call stop_error(1,'Unable to identify the format on the float file')
endif

end subroutine floats_read
! ...
! =========================================================================
! ...
subroutine floats_alloc(FLOAT_STRUCTURE)

type(floater), intent(inout)               :: FLOAT_STRUCTURE

allocate(FLOAT_STRUCTURE%lon          (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%lat          (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%depth        (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%time         (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%release_time (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%dist         (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%temp         (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%salt         (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%dens         (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%UDF          (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%released     (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%floating     (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%stranded     (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%outside      (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%exitcode     (FLOAT_STRUCTURE%n) )

FLOAT_STRUCTURE%lon(:)          = zero
FLOAT_STRUCTURE%lat(:)          = zero
FLOAT_STRUCTURE%depth(:)        = zero
FLOAT_STRUCTURE%time(:)         = zero
FLOAT_STRUCTURE%release_time(:) = zero
FLOAT_STRUCTURE%dist(:)         = zero
FLOAT_STRUCTURE%temp(:)         = nan
FLOAT_STRUCTURE%salt(:)         = nan
FLOAT_STRUCTURE%dens(:)         = nan
FLOAT_STRUCTURE%UDF(:)          = nan
FLOAT_STRUCTURE%released(:)     = .false.
FLOAT_STRUCTURE%floating(:)     = .false.
FLOAT_STRUCTURE%stranded(:)     = .false.
FLOAT_STRUCTURE%outside(:)      = .false.
FLOAT_STRUCTURE%exitcode(:)     = -1

end subroutine floats_alloc
! ...
! =========================================================================
! ...
end module mod_floats
