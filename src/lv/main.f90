! ****************************************************************************
! ... main.f90
! ... Quim Ballabrera, March 2017
! ... COSMO Lagrangian Model
! ... Model driver
! ... v0.1: Initial version (March 2017)
! ...       Read a netcdf with u and v
! ...       Apply a bicubic interpolation in time
! ...       Apply a Runge Kutta order five to solve equations
! ...       Output as a netcdf file
! ...       Arakawa A,B,C-grid
! ...       Check if particle leaves the area
! ...       Reverse trajectories
! ...       Take into account scale_factor and add_offset
! ...       temp, psal
! ...       User Defined Function (udf)
! ...       Beaching control
! ...       Same spatial interpolation for velocity and tracers
! ...       Fill coastal point for tracers using linear interpolation
! ... v0.2: Update version (December 2017)
! ...       New float initial release options (time (secs) or date).
! ...       Correct the fact that input filename was too short (80).
! ...       Allow user to change the name of the initial release file
! ... v0.3: Change name of default output filename lagrangian-out.nc
! ...       Use updated cosmo library.
! ...
! ...      
! ****************************************************************************

program main

use cosmo
use clm
use mod_floats
use mod_out

implicit none

character(len=*), parameter             :: version = 'v0.2'
character(len=*), parameter             :: author = 'Quim Ballabrera'

type(floater)                           :: FLT
type(field)                             :: U,V,W,T,S,A,B


! ... I/O flags and options
! ...
logical                                 :: fuu = .false.
logical                                 :: fvv = .false.
logical                                 :: fww = .false.
logical                                 :: ftt = .false.
logical                                 :: fss = .false.
logical                                 :: faa = .false.
logical                                 :: fbb = .false.
logical                                 :: frk = .false.
logical                                 :: fme = .false.

logical                                 :: out = .false.  ! Flag output file
logical                                 :: fvc = .false.  ! Flag stationary fld
logical                                 :: hlp = .false.  ! Flag for help
logical                                 :: frs = .false.  ! Flag Rand seed
logical                                 :: fcl = .false.  ! Flag rand cloud rel.

integer                                 :: iseed
integer, dimension(:), allocatable      :: rseed


! ... General variables
! ...
integer                                 :: i
integer                                 :: na
integer                                 :: iu
integer                                 :: flo
integer                                 :: l,lo
real(dp)                                :: system_time
character(len=maxlen)                   :: ofile = 'lagrangian-out.nc' 
character(len=280)                      :: Ufile = ''
character(len=280)                      :: Vfile = ''
character(len=280)                      :: Wfile = ''
character(len=280)                      :: Tfile = ''
character(len=280)                      :: Sfile = ''
character(len=280)                      :: Afile = ''
character(len=280)                      :: Bfile = ''
character(len=80)                       :: Uvar  = ''
character(len=80)                       :: Vvar  = ''
character(len=80)                       :: Wvar  = ''
character(len=80)                       :: Tvar  = ''
character(len=80)                       :: Svar  = ''
character(len=80)                       :: Avar  = ''
character(len=80)                       :: Bvar  = ''
character(len=400)                      :: Ulist = ''
character(len=400)                      :: Vlist = ''
character(len=400)                      :: Wlist = ''
character(len=400)                      :: Tlist = ''
character(len=400)                      :: Slist = ''
character(len=400)                      :: Alist = ''
character(len=400)                      :: Blist = ''


! ... Fill in the help information
! ...
call program_help(version,author)

! ... Lineargs
! ...
call lineargs_ini(na)
if (na.eq.0) call help_write()

call argflg('--h',hlp)
call argflg('-help',hlp)
if (hlp) then
  call help_write()
else
  call header()
endif

call argstr('-out',out,ofile)
call argstr('-traj',out,ofile)
call argstr('-rel',fre_in,release_file_in)
call argstr('-ini',fre_in,release_file_in)
call argstr('-end',fre_out,release_file_out)
call argstr('-fin',fre_out,release_file_out)

call arglst('-U',fuu,Ulist)
call arglst('-V',fvv,Vlist)
call arglst('-W',fww,Wlist)
call arglst('-T',ftt,Tlist)
call arglst('-S',fss,Slist)
call arglst('-AU',faa,Alist)
call arglst('-AV',fbb,Blist)

call argdbl('-miss',fmv,missing)
call argdbl('-time_scal',tscale_flag,tscale)
call argstr('-cal',fcal,calendar)
call argflg('-stat',stationary)
call argint('-rec',fvc,record)
call argdbl('-simulation_ti',ftimesim,simulation_length)
call argdbl('-time_simul',ftimesim,simulation_length)
call argint('-steps',fent,external_nsteps)
call argdbl('-edt',fedt,external_dt)
call argdbl('-external_dt',fedt,external_dt)
call argdbl('-idt',fidt,internal_dt)
call argdbl('-internal_dt',fidt,internal_dt)
call argflg('-rev',reverse)
call argint('-seed',frs,iseed)

! ... Position if xo,yo,zo
call argdbl('-xo',ffx,fxo)
call argdbl('-yo',ffy,fyo)
call argdbl('-zo',ffz,fzo)
call argdbl('-to',fft,fto)

! ... Release time
call argdbl('-time_rel',fft,fto)
call argstr('-do',ffd,fdo)

! ... Cloud computation
call argflg('-cl',fcl)
call argflg('-ran',fcl)

call argdbl('-Rx',frx,Radius_x)
call argdbl('-Ry',fry,Radius_y)
call argdbl('-Rz',frz,Radius_z)
call argdbl('-Rt',frt,Radius_t)
call argdbl('-time_interval',fri,release_interval)
call argint('-nf',fnp,Nfloats)
call argint('-Nf',fnp,Nfloats)
call argint('-floats',fnp,Nfloats)

! ... Lon/Lat or XY grid
! ...
call argflg('-xy',Fxy)

! ... Not yet implemented
! ... These variables affect the segment of the file to be read
call argdbl('-so',fso,selected_south)
call argdbl('-no',fno,selected_north)
call argdbl('-we',fwe,selected_west)
call argdbl('-ea',fea,selected_east)
call argdbl('-dep',fkz,selected_depth)      ! Deepest layer

call argflg('-udf',run_udf)                 ! Run User Defined Function

! ... Runge Kutta:
call argint('-rk',frk,RKORDER)

! ... Output format
! ...
call argflg('-geo',gjson)
call argstr('-source',gj_flag_source,gj_source)
call argstr('-creator',gj_flag_creator,gj_creator)
call argstr('-exp',gj_flag_exp,gj_exp)
call argstr('-origin',gj_flag_origin,gj_origin)

! ... Interpolation method
call argstr('-int',fme,METHOD)
METHOD = uppercase(METHOD)

if (METHOD(1:1).EQ.'B') then
  METHOD = 'BILINEAL'
else if (METHOD(1:2).EQ.'LI') then
  METHOD = 'BILINEAL'
else if (METHOD(1:1).EQ.'G') then
  METHOD = 'GLOBALSPLINES'
else
  METHOD = 'LOCALSPLINES'
endif

Flonlat = .not.Fxy

call argstr('-fin',fre_out,release_file_out)
if (count((/fuu,fvv/)).ne.2) &
   call stop_error(1,'Error. Options -U and -V required')

if (count((/faa,fbb/)).eq.1) &
   call stop_error(1,'Error. Invalid use of atmospheric options.')

if (count((/ffx,ffy/)).eq.1) &
   call stop_error(1,'Error. Use both -xo and -yo options')

if (count((/fft,ffd/)).eq.2) &
   call stop_error(1,'Error. Incompatible options -to and -do options')

if (fcl) random_floats = .true.
if (.not.ffx.and..not.fre_in) then
  if (.not.fri) random_floats = .true.
endif
if (ffx) fre_in = .false.  ! We keep the name it will not be read, but written

if (fri) then
  if (random_floats) &
    call stop_error(1,'Incompatible options -time_interval and -random')
  if (.not.ffx) &
    call stop_error(1,'Option -time_interval requires a release point')
endif

if (ftimesim.and.fent) &
   call stop_error(1,'Incompatible options -time_simulation and -steps')

! ... Random seed
! ...
if (frs) then
  call random_seed(size=i)
  allocate(rseed(i))
  rseed(:) = iseed
  call random_seed(put=rseed)
endif

! ... Get information about grids
! ... Browsing command line arguments
! ...
Ufile   = token_read(Ulist,'file=')
U%xname = token_read(Ulist,'x=')
U%yname = token_read(Ulist,'y=')
U%zname = token_read(Ulist,'z=')
U%tname = token_read(Ulist,'t=')
Uvar    = token_read(Ulist,'u=')
if (len_trim(Uvar).eq.0) Uvar = token_read(Ulist,'var=')
if (len_trim(Uvar).eq.0) Uvar = 'u'

Vfile   = token_read(Vlist,'file=')
V%xname = token_read(Vlist,'x=')
V%yname = token_read(Vlist,'y=')
V%zname = token_read(Vlist,'z=')
V%tname = token_read(Vlist,'t=')
Vvar    = token_read(Vlist,'v=')
if (len_trim(Vfile).eq.0)   Vfile   = trim(Ufile)
if (len_trim(V%xname).eq.0) V%xname = trim(U%xname)
if (len_trim(V%yname).eq.0) V%yname = trim(U%yname)
if (len_trim(V%zname).eq.0) V%zname = trim(U%zname)
if (len_trim(V%tname).eq.0) V%tname = trim(U%tname)
if (len_trim(Vvar).eq.0) Vvar = token_read(Vlist,'var=')
if (len_trim(Vvar).eq.0) Vvar = 'v'

if (fww) then
  Wfile   = token_read(Wlist,'file=')
  W%xname = token_read(Wlist,'x=')
  W%yname = token_read(Wlist,'y=')
  W%zname = token_read(Wlist,'z=')
  W%tname = token_read(Wlist,'t=')
  Wvar    = token_read(Wlist,'w=')
  if (len_trim(Wfile).eq.0)   Wfile   = trim(Ufile)
  if (len_trim(W%xname).eq.0) W%xname = trim(U%xname)
  if (len_trim(W%yname).eq.0) W%yname = trim(U%yname)
  if (len_trim(W%zname).eq.0) W%zname = trim(U%zname)
  if (len_trim(W%tname).eq.0) W%tname = trim(U%tname)
  if (len_trim(Wvar).eq.0) Wvar = token_read(Wlist,'var=')
endif

if (ftt) then
  Tfile   = token_read(Tlist,'file=')
  T%xname = token_read(Tlist,'x=')
  T%yname = token_read(Tlist,'y=')
  T%zname = token_read(Tlist,'z=')
  T%tname = token_read(Tlist,'t=')
  Tvar    = token_read(Tlist,'T=')
  if (len_trim(Tfile).eq.0)   Tfile   = trim(Ufile)
  if (len_trim(T%xname).eq.0) T%xname = trim(U%xname)
  if (len_trim(T%yname).eq.0) T%yname = trim(U%yname)
  if (len_trim(T%zname).eq.0) T%zname = trim(U%zname)
  if (len_trim(T%tname).eq.0) T%tname = trim(U%tname)
  if (len_trim(Tvar).eq.0) Tvar = token_read(Tlist,'var=')
endif

if (fss) then
  Sfile   = token_read(Slist,'file=')
  S%xname = token_read(Slist,'x=')
  S%yname = token_read(Slist,'y=')
  S%zname = token_read(Slist,'z=')
  S%tname = token_read(Slist,'t=')
  Svar    = token_read(Slist,'S=')
  if (len_trim(Sfile).eq.0)   Sfile   = trim(Tfile)
  if (len_trim(S%xname).eq.0) S%xname = trim(T%xname)
  if (len_trim(S%yname).eq.0) S%yname = trim(T%yname)
  if (len_trim(S%zname).eq.0) S%zname = trim(T%zname)
  if (len_trim(S%tname).eq.0) S%tname = trim(T%tname)
  if (len_trim(Svar).eq.0) Svar = token_read(Slist,'var=')
endif

if (faa) then
  Afile   = token_read(Alist,'file=')
  if (len_trim(Afile).eq.0) &
           call stop_error(1,'ERROR: Atmosphere filename required')
  A%xname = token_read(Alist,'x=')
  A%yname = token_read(Alist,'y=')
  A%zname = token_read(Alist,'z=')
  A%tname = token_read(Alist,'t=')
  Avar    = token_read(Alist,'u=')
  if (len_trim(Avar).eq.0) Avar = token_read(Alist,'var=')
  if (len_trim(Avar).eq.0) Avar = 'eastward_wind'

  Bfile   = token_read(Blist,'file=')
  if (len_trim(Bfile).eq.0)   Bfile   = trim(Afile)
  B%xname = token_read(Blist,'x=')
  B%yname = token_read(Blist,'y=')
  B%zname = token_read(Blist,'z=')
  B%tname = token_read(Blist,'t=')
  Bvar    = token_read(Blist,'v=')
  if (len_trim(Bfile).eq.0)   Bfile   = trim(Afile)
  if (len_trim(B%xname).eq.0) B%xname = trim(A%xname)
  if (len_trim(B%yname).eq.0) B%yname = trim(A%yname)
  if (len_trim(B%zname).eq.0) B%zname = trim(A%zname)
  if (len_trim(B%tname).eq.0) B%tname = trim(A%tname)
  if (len_trim(Bvar).eq.0) Bvar = token_read(Blist,'var=')
  if (len_trim(Bvar).eq.0) Bvar = 'northward_wind'
endif

! ... Check if calendar has been specified by user
! ...
if (fcal) then
  write(*,*)
  write(*,*) 'Calendar specified by user: ', trim(calendar)
  U%calendar = trim(calendar)
  V%calendar = trim(calendar)
  W%calendar = trim(calendar)
  T%calendar = trim(calendar)
  S%calendar = trim(calendar)
  A%calendar = trim(calendar)
  B%calendar = trim(calendar)
endif

write(*,*) 
write(*,*) '--- Getting U grid information'
write(*,*) '-------------------------------'
call file_open(U,Ufile,Uvar)

write(*,*) 
write(*,*) '--- Getting V grid information'
write(*,*) '-------------------------------'
check_equal_time = .false.
check_time       = .true.
call file_open(V,Vfile,Vvar)

if (fww) then
  write(*,*) 
  write(*,*) '--- Getting W grid information'
  write(*,*) '-------------------------------'
  check_equal_time = .true.
  check_time       = .true.
  call file_open(W,Wfile,Wvar)
else
  write(*,*) 
  write(*,*) 'WARNING: W grid not provided'
endif

if (ftt) then
  write(*,*) 
  write(*,*) '--- Getting T grid information'
  write(*,*) '-------------------------------'
  check_equal_time = .true.
  check_time       = .true.
  call file_open(T,Tfile,Tvar)
else
  write(*,*) 'WARNING: T grid not provided'
endif

if (fss) then
  write(*,*) 
  write(*,*) '--- Getting S grid information'
  write(*,*) '-------------------------------'
  check_equal_time = .true.
  check_time       = .true.
  call file_open(S,Sfile,Svar)
else
  write(*,*) 'WARNING: S grid not provided'
endif

if (faa) then
  write(*,*) 
  write(*,*) '--- Getting AU grid information'
  write(*,*) '--------------------------------'
  check_equal_time = .false.
  check_time = .true.
  call file_open(A,Afile,Avar)

  write(*,*) 
  write(*,*) '--- Getting AV grid information'
  write(*,*) '--------------------------------'
  check_equal_time = .false.
  check_time = .true.
  call file_open(B,Bfile,Bvar)
else
  write(*,*) 'WARNING: Atmosphere grid not provided'
endif

! ... Now, check if all opts have been used:
! ...
call checkopts()

! ... System domain:
! ...
domain_west   = max(U%domain_west,V%domain_west)
domain_east   = min(U%domain_east,V%domain_east)
domain_south  = max(U%domain_south,V%domain_south)
domain_north  = min(U%domain_north,V%domain_north)
domain_top    = U%domain_top
domain_bottom = U%domain_bottom

! ... Rerverse
! ...
if (reverse) then
  time_direction = -1
else
  time_direction = 1
endif

! ... Get the release time (seconds after initial model time)
! ... from the release date
! ...
if (ffd) then
  fdateo = string2date(fdo) 
  i =  nint((date2jd(fdateo) - date2jd(U%dates(1)))*86400)
  fto = max(0,i)
endif

! ... Default depth and release time of floats:
! ... Surface, released at the initial step
! ...
if (.not.ffz) fzo = domain_top      
if (.not.fft) fto = zero

call floats_ini(FLT,                       &
                U%x,U%y,U%z,U%t,           &
                domain_west,domain_east,   &
                domain_south,domain_north, &
                domain_top,domain_bottom,  &
                U%land)

if (FLT%n.eq.0) call stop_error(1,'No floats defined')

! ... Indicate if UDF function has to be called
! ...
FLT%run_udf = run_udf

if (U%nz.gt.1) then
  do flo=1,FLT%n
    if (FLT%depth(flo).lt.U%z(1)) then
      print*, 'Warning: Changing float depth to uppermost level'
      FLT%depth(flo) = 1.01*U%z(1)
    endif
  enddo
endif

write(*,*)
write(*,*) 'Interpolation method: ', trim(METHOD)

call clm_ini (U,V,W,T,S,A,B)

! ... Open trajectory file:
! ...
FLT%missing = missing
if (T%set) FLT%Tset = .true.
if (S%set) FLT%Sset = .true.

call out_create(ofile,FLT,U%icdf%fid,U%icdf%idt)

if (reverse) then
  if (fto.le.zero) then
    write(*,*)
    write(*,*) 'Reverse mode. Changing deployment time of all floats to zero'
    FLT%release_time(:) = U%jds(U%nt)-U%jds(1)
  endif
endif

! ... Run the trajectory integrator
! ...
call clm_run (U,V,W,T,S,A,B,FLT)

! ... Write exit mode:
! ...
call out_exitmode(FLT)

! ... Close trajectory
! ...
write(*,*) 'Closing trajectory file: ', trim(ofile)
call out_close(FLT)

! ... Save the output positions:
! ...
iu = unitfree()
write(*,*)
write(*,*) 'Saving final floats final position:'
write(*,*) 'Output filename : ', trim(release_file_out)

open(iu,file=release_file_out,status='unknown')
!do flo=1,FLT%n
!  write(*,'(3F10.4,F10.0,4F9.3)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
!                     FLT%time(flo), FLT%temp(flo), FLT%salt(flo), &
!                     FLT%dens(flo), FLT%UDF(flo)
!  write(iu,'(3F10.4,F10.0,4F9.3)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
!                      FLT%time(flo), FLT%temp(flo), FLT%salt(flo), &
!                      FLT%dens(flo), FLT%UDF(flo)
!enddo
do flo=1,FLT%n
  write(*,'(3F10.4,F10.0)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
                     FLT%time(flo)
  write(iu,'(3F10.4,F10.0)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
                      FLT%time(flo)
enddo
close(iu)

call stop_error(0,'Ok')
end program main
