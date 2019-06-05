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
logical                                  :: frz              = .false.
logical                                  :: frt              = .false.
logical                                  :: fri              = .false.

character(len=180)                       :: release_file_in  = 'release.ini'
character(len=180)                       :: release_file_out = 'release.out'
integer                                  :: Nfloats          = 10
real(dp)                                 :: Radius_x         = 0.1D0
real(dp)                                 :: Radius_y         = 0.1D0
real(dp)                                 :: Radius_z         = zero
real(dp)                                 :: Radius_t         = zero
real(dp)                                 :: release_interval = zero


real(dp)                                 :: fxo
real(dp)                                 :: fyo
real(dp)                                 :: fzo              = zero
real(dp)                                 :: fto              = zero
character(len=20)                        :: fdo              = ''
type(date_type)                          :: fdateo         ! Release date

! ... Output float format
! ... Default, Netcdf
logical                                  :: gjson            = .false.
logical                                  :: gj_flag_origin   = .false.
logical                                  :: gj_flag_source   = .false.
logical                                  :: gj_flag_creator  = .false.
logical                                  :: gj_flag_exp      = .false.
character(len=180)                       :: gj_origin        = ''
character(len=180)                       :: gj_source        = 'model'
character(len=180)                       :: gj_creator       = 'blm'
character(len=180)                       :: gj_exp           = ''
character(len=180)                       :: gj_sn            = ''
character(len=180)                       :: gj_name          = ''
character(len=180)                       :: gj_numfloats     = ''


type floater
  integer                                :: n = 0
  logical                                :: run_udf = .false.
  logical                                :: tracer  = .false.
  logical                                :: Tset    = .false.
  logical                                :: Sset    = .false.

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
  real(dp), dimension(:), pointer        :: temp           ! tracer temp
  real(dp), dimension(:), pointer        :: salt           ! tracer salt
  real(dp), dimension(:), pointer        :: rho            ! tracer density
  real(dp), dimension(:), pointer        :: b              ! tracer buoyancy
  real(dp), dimension(:), pointer        :: UDF            ! User-defined funct
  type(date_type)                        :: date           ! A given date
  integer, dimension(:), pointer         :: exitcode       ! exit code
  logical, dimension(:), pointer         :: released       ! released flag
  logical, dimension(:), pointer         :: floating       ! status flag
  logical, dimension(:), pointer         :: stranded       ! status flag
  logical, dimension(:), pointer         :: outside        ! status flag
  integer, dimension(:), pointer         :: lo             ! First floating step
  integer, dimension(:), pointer         :: lf             ! Last floating step
end type floater

contains

subroutine floats_ini(FLT,x,y,z,t,x0,x1,y0,y1,z0,z1,land)

type(floater), intent(out)                 :: FLT
real(dp), intent(in)                       :: x0,x1,y0,y1,z0,z1
real(dp), dimension(:), intent(in)         :: x
real(dp), dimension(:), intent(in)         :: y
real(dp), dimension(:), intent(in)         :: z
real(dp), dimension(:), intent(in)         :: t
logical, dimension(:,:,:), intent(in)      :: land

logical isbeach,isout
integer ii,jj,kk,flo,iu
real(dp) xmin,ymin,tmin,zmin,rnd(4)
!real(dp) x0,x1,y0,y1,z0,z1

if (fre_in) then
  call floats_read(release_file_in,FLT)
else 

  if (fri) then

    if (fxo.le.x0) call stop_error(1,'Floats should be inside the domain')
    if (fxo.ge.x1) call stop_error(1,'Floats should be inside the domain')
    if (fyo.le.y0) call stop_error(1,'Floats should be inside the domain')
    if (fyo.ge.y1) call stop_error(1,'Floats should be inside the domain')

    FLT%n = Nfloats
    call floats_alloc (FLT)

    write(*,*) 
    write(*,*) 'Selecting a random cloud of points'
    write(*,*) 'Number of floats      : ', Nfloats
    write(*,*) 'Release point (x,y)   : ', fxo,fyo
    write(*,*) 'Release depth (z)     : ', fzo
    write(*,*) 'Initial Release time  : ', fto
    write(*,*) 'Release time interval : ', release_interval

    isbeach = False
    isout   = any((/fxo.le.x0,  &
                    fxo.ge.x1,  &
                    fyo.le.y0,  &
                    fyo.ge.y1,  &
                    fzo.lt.z0,  &
                    fzo.gt.z1/))

    if (.not.isout) then
      ii = locate(x,fxo)
      jj = locate(y,fyo)
      kk = max(1,locate(z,fzo))
      isbeach = any((/land(ii,jj,kk),      &
                      land(ii+1,jj,kk),    &
                      land(ii+1,jj+1,kk),  &
                      land(ii,jj+1,kk)/))
    endif

    do flo=1,FLT%n
      FLT%lon(flo)   = fxo
      FLT%lat(flo)   = fyo
      FLT%depth(flo) = fzo
      FLT%release_time(flo) = fto + (flo-1)*release_interval
      FLT%stranded(flo) = isbeach
      FLT%outside(flo)  = isout
    enddo

  else if (random_floats) then

    if (.not.ffx) then
      fxo      = half*(x0+x1)
      fyo      = half*(y0+y1)
      Radius_x = half*(x1-x0)
      Radius_y = half*(y1-y0)
    endif

    if (fxo.le.x0) call stop_error(1,'Floats should be inside the domain')
    if (fxo.ge.x1) call stop_error(1,'Floats should be inside the domain')
    if (fyo.le.y0) call stop_error(1,'Floats should be inside the domain')
    if (fyo.ge.y1) call stop_error(1,'Floats should be inside the domain')

    FLT%n = Nfloats
    call floats_alloc (FLT)

    write(*,*) 
    write(*,*) 'Selecting a random cloud of points'
    write(*,*) 'Number of floats   : ', Nfloats
    write(*,*) 'Center point (x,y) : ', fxo,fyo
    write(*,*) 'Center depth (z)   : ', fzo
    write(*,*) 'Center time        : ', fto
    write(*,*) 'Radius_x           : ', Radius_x
    write(*,*) 'Radius_y           : ', Radius_y
    write(*,*) 'Radius_z           : ', Radius_z
    write(*,*) 'Radius_t           : ', Radius_t

    xmin = max(x0,fxo - Radius_x)
    ymin = max(y0,fyo - Radius_y)
    if (fzo.eq.z0) then
      zmin = z0
    else
      zmin = max(z0,fzo-Radius_z)
    endif
    if (fto.eq.0) then
      tmin = fto 
    else
      tmin = max(zero,fto-Radius_t)
    endif

    do flo=1,FLT%n
      10 call RANDOM_NUMBER(rnd)
      FLT%lon(flo)   = Radius_x*rnd(1) + xmin
      FLT%lat(flo)   = Radius_y*rnd(2) + ymin
      FLT%depth(flo) = Radius_z*rnd(3) + zmin
      FLT%release_time(flo) = aint(Radius_t*rnd(4) + tmin)

      isout = any((/FLT%lon(flo).le.x0,     &
                    FLT%lon(flo).ge.x1,      &
                    FLT%lat(flo).le.y0,      &
                    FLT%lat(flo).ge.y1,      &
                    FLT%depth(flo).lt.z0,    &
                    FLT%depth(flo).gt.z1/))
      if (isout) goto 10

      ii = locate(x,FLT%lon(flo))
      jj = locate(y,FLT%lat(flo))
      kk = max(1,locate(z,FLT%depth(flo)))
      isbeach = any((/land(ii,jj,kk),      &
                      land(ii+1,jj,kk),    &
                      land(ii+1,jj+1,kk),  &
                      land(ii,jj+1,kk)/))
      if (isbeach) goto 10
      FLT%stranded(flo)   = .false.
      FLT%outside(flo)    = .false.
    enddo

  else

    write(*,*) 'Reading floating position from command line ...'
    if (fxo.le.x0) call stop_error(1,'Floats should be inside the domain')
    if (fxo.ge.x1) call stop_error(1,'Floats should be inside the domain')
    if (fyo.le.y0) call stop_error(1,'Floats should be inside the domain')
    if (fyo.ge.y1) call stop_error(1,'Floats should be inside the domain')

    FLT%n = 1
    call floats_alloc (FLT)
    FLT%lon(1)          = fxo
    FLT%lat(1)          = fyo
    FLT%depth(1)        = fzo
    FLT%release_time(1) = fto
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

  if (FLT%tracer) then
    err = NF90_INQ_VARID(ffid,'tracer',idtem) 
    call cdf_error(err,'Unable to inquire about tracer variable')
  endif

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
  call cdf_error(err,'Unable to read tracer')
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
                                            FLT%temp(i), &
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
allocate(FLOAT_STRUCTURE%rho          (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%b            (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%UDF          (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%released     (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%floating     (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%stranded     (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%outside      (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%exitcode     (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%lo           (FLOAT_STRUCTURE%n) )
allocate(FLOAT_STRUCTURE%lf           (FLOAT_STRUCTURE%n) )

FLOAT_STRUCTURE%lon(:)          = zero
FLOAT_STRUCTURE%lat(:)          = zero
FLOAT_STRUCTURE%depth(:)        = zero
FLOAT_STRUCTURE%time(:)         = zero
FLOAT_STRUCTURE%release_time(:) = zero
FLOAT_STRUCTURE%dist(:)         = zero
FLOAT_STRUCTURE%temp(:)         = nan
FLOAT_STRUCTURE%salt(:)         = nan
FLOAT_STRUCTURE%rho(:)          = nan
FLOAT_STRUCTURE%b(:)            = nan
FLOAT_STRUCTURE%UDF(:)          = nan
FLOAT_STRUCTURE%released(:)     = .false.
FLOAT_STRUCTURE%floating(:)     = .false.
FLOAT_STRUCTURE%stranded(:)     = .false.
FLOAT_STRUCTURE%outside(:)      = .false.
FLOAT_STRUCTURE%exitcode(:)     = -1
FLOAT_STRUCTURE%lo(:)           = -1
FLOAT_STRUCTURE%lf(:)           = -1

end subroutine floats_alloc
! ...
! =========================================================================
! ...
end module mod_floats
