!st ****************************************************************************
! ... main.f90
! ... Quim Ballabrera, April 2017
! ... Lagrangian motion
! ... v0.0: Initial version (March 2017)
! ...       Read a netcdf with u and v
! ...       Apply a bicubic interpolation in time
! ...       Apply a Runge Kutta order five to solve equations
! ...       Output as a netcdf file
! ...       Input must be in days
! ...       Arakawa A-grid
! ... v0.1: Update help options (March 2017)
! ...       Small fixes
! ...       Write commandline in output netcdf
! ...       Check if particle leaves the area
! ... v0.2: Reverse trajectories (April 2017)
! ...       Take into account scale_factor and add_offset
! ...       temp, psal (A-grid)
! ... v1.0: Modular version of the code (April 2017)
! ...       Updated beaching criteria
! ...       Unified spatial interpolation for velocity and tracers
! ...       Fill coastal point for tracers using linear interpolation
! ...
! ...      
! ****************************************************************************


program main

use cosmo
use clm
use mod_floats
use mod_out

implicit none

character(len=*), parameter              :: version = 'v0.0'
character(len=*), parameter              :: author = 'Quim Ballabrera'

type(floater)                           :: FLT
type(cdf_vgrid)                         :: UCDF
type(cdf_vgrid)                         :: VCDF
type(cdf_vgrid)                         :: WCDF
type(cdf_tgrid)                         :: TCDF


! ... I/O flags and options
! ...
logical                                 :: fuu = .false.
logical                                 :: fvv = .false.
logical                                 :: fww = .false.
logical                                 :: ftt = .false.

logical                                 :: inp = .false.  ! Flag input file
logical                                 :: out = .false.  ! Flag output file
logical                                 :: uff = .false.  ! Flag U-field name
logical                                 :: vff = .false.  ! Flag V-field name
logical                                 :: fxu = .false.  ! Flag U-LON name
logical                                 :: fyu = .false.  ! Flag U-LAT name
logical                                 :: fxv = .false.  ! Flag V-LON name
logical                                 :: fyv = .false.  ! Flag V-LAT name
logical                                 :: fxt = .false.  ! Flag T-LON name
logical                                 :: fyt = .false.  ! Flag T-LAT name
logical                                 :: fzt = .false.  ! Flag T-DEPTH name
logical                                 :: fzw = .false.  ! Flag W-DEPTH name
logical                                 :: fnd = .false.  ! Flag simul. time
logical                                 :: fsi = .false.  ! Flag simul. interv.
logical                                 :: fvc = .false.  ! Flag stationary fld
logical                                 :: hlp = .false.  ! Flag for help
logical                                 :: frs = .false.  ! Flag Rand seed

character(len=maxlen)                   :: ufile   = ''   ! Input filename
character(len=maxlen)                   :: vfile   = ''   ! Input filename
character(len=maxlen)                   :: wfile   = ''   ! Input filename
character(len=maxlen)                   :: tfile   = ''   ! Input filename

character(len=maxlen)                   :: ifile   = ''   ! Input filename
character(len=80)                       :: uname   = 'u'  ! Default U-name
character(len=80)                       :: vname   = 'v'  ! Default V-name
character(len=80)                       :: tname   = ''   ! Temperature var
character(len=80)                       :: sname   = ''   ! Salinity var
character(len=80)                       :: rname   = ''   ! Density var
character(len=80)                       :: hname   = ''   ! Sea level var

character(len=80)                       :: uxname  = ''   
character(len=80)                       :: uyname  = ''   
character(len=80)                       :: uzname  = ''   


integer                                 :: Ndays   = 7    ! Number of days
integer                                 :: Nsteps  = 6    ! Time steps per day
integer                                 :: iseed
integer, dimension(:), allocatable      :: rseed


! ... General variables
! ...
logical                                 :: beach
logical                                 :: inside
integer                                 :: i,j,k,ii,jj,kk
integer                                 :: err
integer                                 :: na
integer                                 :: iu
integer                                 :: flo
integer                                 :: cycle
integer                                 :: floater
integer                                 :: step
real(dp)                                :: tf
character(len=maxlen)                   :: ofile = 'out.nc'   ! Output filename
character(len=4000)                     :: Ulist=''
character(len=4000)                     :: Vlist=''
character(len=4000)                     :: Wlist=''
character(len=4000)                     :: Tlist=''

logical beaching
real(dp) hinterpol

external beaching
external hinterpol
external RHS


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

call argstr('-traj',out,ofile)
call argstr('-rel',fre_in,release_file_in)
call argstr('-ini',fre_in,release_file_in)
call argstr('-end',fre_out,release_file_out)
call argstr('-fin',fre_out,release_file_out)

call arglst('-U',fuu,Ulist)
call arglst('-V',fvv,Vlist)
call arglst('-W',fww,Wlist)
call arglst('-T',ftt,Tlist)

call argdbl('-miss',fmv,missing)
call argdbl('-time_scal',tscale_flag,tscale)
call argstr('-cal',fcal,calendar)
call argflg('-stat',stationary)
call argint('-rec',fvc,record)
call argdbl('-time_sim',ftimesim,simulation_length)
call argint('-steps',fent,external_nsteps)
call argdbl('-edt',fedt,external_dt)
call argdbl('-idt',fidt,internal_dt)
call argflg('-rev',reverse)
call argint('-seed',frs,iseed)

call argdbl('-Rx',frx,Radius_x)
call argdbl('-Ry',fry,Radius_y)
call argdbl('-xo',ffx,fxo)
call argdbl('-yo',ffy,fyo)
call argint('-nf',fnp,Nfloats)

! ... Not yet implemented
!call argdbl('-so',fso,south)
!call argdbl('-no',fno,north)
!call argdbl('-we',fwe,west)
!call argdbl('-ea',fea,east)

if (count((/fuu,fvv/)).ne.2) &
   call stop_error(1,'Error. Options -U and -V required')

if (count((/ffx,ffy/)).eq.1) &
   call stop_error(1,'Error. Use both -xo and -yo options')

if (frx.or.fry) random_floats = .true.
if (.not.ffx.and..not.fre_in) random_floats = .true.

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
UCDF%filename = token_read(Ulist,'file=')
UCDF%varname  = token_read(Ulist,'vel=')
UCDF%xname    = token_read(Ulist,'x=')
UCDF%yname    = token_read(Ulist,'y=')
UCDF%zname    = token_read(Ulist,'z=')
UCDF%tname    = token_read(Ulist,'t=')
if (len_trim(UCDF%varname).eq.0) UCDF%varname = 'u'

VCDF%filename = token_read(Vlist,'file=')
VCDF%varname  = token_read(Vlist,'vel=')
VCDF%xname    = token_read(Vlist,'x=')
VCDF%yname    = token_read(Vlist,'y=')
VCDF%zname    = token_read(Vlist,'z=')
VCDF%tname    = token_read(Vlist,'t=')
if (len_trim(VCDF%varname).eq.0) VCDF%varname = 'v'
if (len_trim(VCDF%filename).eq.0) VCDF%filename = trim(UCDF%filename)
if (len_trim(VCDF%xname).eq.0)    VCDF%xname = trim(UCDF%xname)
if (len_trim(VCDF%yname).eq.0)    VCDF%yname = trim(UCDF%yname)
if (len_trim(VCDF%zname).eq.0)    VCDF%zname = trim(UCDF%zname)
if (len_trim(VCDF%tname).eq.0)    VCDF%tname = trim(UCDF%tname)

if (fvv) then
  WCDF%filename = token_read(Wlist,'file=')
  WCDF%varname  = token_read(Wlist,'w=')
  WCDF%xname    = token_read(Wlist,'x=')
  WCDF%yname    = token_read(Wlist,'y=')
  WCDF%zname    = token_read(Wlist,'z=')
  WCDF%tname    = token_read(Wlist,'t=')
else
  WCDF%defined  = .false.
endif

if (ftt) then
  TCDF%filename = token_read(Tlist,'file=')
  TCDF%tempname = token_read(Tlist,'tem=')
  TCDF%saltname = token_read(Tlist,'sal=')
  TCDF%densname = token_read(Tlist,'den=')
  TCDF%xname    = token_read(Tlist,'x=')
  TCDF%yname    = token_read(Tlist,'y=')
  TCDF%zname    = token_read(Tlist,'z=')
  TCDF%tname    = token_read(Tlist,'t=')
  if (len_trim(TCDF%tempname).eq.0) TCDF%tempname = token_read(Tlist,'temp=')
  if (len_trim(TCDF%saltname).eq.0) TCDF%saltname = token_read(Tlist,'salt=')
  if (len_trim(TCDF%densname).eq.0) TCDF%densname = token_read(Tlist,'dens=')
  if (len_trim(TCDF%filename).eq.0) TCDF%filename = trim(UCDF%filename)
  if (len_trim(TCDF%xname).eq.0)    TCDF%xname = trim(UCDF%xname)
  if (len_trim(TCDF%yname).eq.0)    TCDF%yname = trim(UCDF%yname)
  if (len_trim(TCDF%zname).eq.0)    TCDF%zname = trim(UCDF%zname)
  if (len_trim(TCDF%tname).eq.0)    TCDF%tname = trim(UCDF%tname)
else
  WCDF%defined  = .false.
endif

! ... Check if calendar has been specified by user
! ...
if (fcal) then
  write(*,*)
  write(*,*) 'Calendar specified by user: ', trim(calendar)
  UCDF%calendar = trim(calendar)
  VCDF%calendar = trim(calendar)
  WCDF%calendar = trim(calendar)
  TCDF%calendar = trim(calendar)
endif

if (fcal) then
endif

if (len_trim(UCDF%filename).gt.0) then
  write(*,*) 
  write(*,*) '--- Getting U grid information'
  write(*,*) '-------------------------------'
  call clm_ufield_open (UCDF)
else
  call stop_error(1,'U grid information is necessary')
endif

if (len_trim(VCDF%filename).gt.0) then
  write(*,*) 
  write(*,*) '--- Getting V grid information'
  write(*,*) '-------------------------------'
  call clm_ufield_open (VCDF)
else
  call stop_error(1,'V grid information is necessary')
endif

if (len_trim(WCDF%filename).gt.0) then
  write(*,*) 
  write(*,*) '--- Getting W grid information'
  write(*,*) '-------------------------------'
  call clm_ufield_open (WCDF)
else
  write(*,*) 
  write(*,*) 'WARNING: W grid not provided'
endif

if (len_trim(TCDF%filename).gt.0) then
  write(*,*) 
  write(*,*) '--- Getting T grid information'
  write(*,*) '-------------------------------'
  call clm_tfield_open (TCDF)
else
  write(*,*) 'WARNING: T grid not provided'
endif


! ... Now, check if all opts have been used:
! ...
call checkopts()

! ... Check grid sanity:
! ...
if (UCDF%nx.ne.VCDF%nx) call stop_error(1,'Incompatible U and V grids: X')
if (UCDF%ny.ne.VCDF%ny) call stop_error(1,'Incompatible U and V grids: Y')
if (UCDF%nz.ne.VCDF%nz) call stop_error(1,'Incompatible U and V grids: Z')
if (UCDF%nt.ne.VCDF%nt) call stop_error(1,'Incompatible U and V grids: T')

if (TCDF%defined) then 
  if (UCDF%nx.ne.TCDF%nx) call stop_error(1,'Incompatible U and V grids: X')
  if (UCDF%ny.ne.TCDF%ny) call stop_error(1,'Incompatible U and V grids: Y')
  if (UCDF%nz.ne.TCDF%nz) call stop_error(1,'Incompatible U and V grids: Z')
  if (UCDF%nt.ne.TCDF%nt) call stop_error(1,'Incompatible U and V grids: T')
endif


! ... Rerverse
! ...
if (reverse) then
  time_direction = -1
else
  time_direction = 1
endif


call floats_ini(FLT,UCDF%x,UCDF%y,UCDF%land(:,:,1))

call clm_ini (UCDF,VCDF,WCDF,TCDF,FLT)

! ... Open trajectory file:
! ...
FLT%missing = missing
call out_create(ofile,FLT)

if (reverse) then
  write(*,*)
  write(*,*) 'Reverse mode. Changing deployment time of all floats to zero'
  FLT%release_time(:) = zero
endif

! ... Run the trajectory integrator
! ...
call clm_run (UCDF,VCDF,WCDF,TCDF,FLT)

! ... Write exit mode:
! ...
call out_exitmode(FLT)

! ... Close trajectory
! ...
call out_close(ofile)


! ... Save the output positions:
! ...
iu = unitfree()
write(*,*)
write(*,*) 'Saving final floats final position:'
write(*,*) 'Output filename : ', trim(release_file_out)

open(iu,file=release_file_out,status='unknown')
do flo=1,FLT%n
  write(*,'(3F10.4,F10.0,4F9.3)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
                     FLT%time(flo), FLT%temp(flo), FLT%salt(flo), &
                     FLT%dens(flo), FLT%UDF(flo)
  write(iu,'(3F10.4,F10.0,4F9.3)') FLT%lon(flo), FLT%lat(flo), FLT%depth(flo), &
                      FLT%time(flo), FLT%temp(flo), FLT%salt(flo), &
                      FLT%dens(flo), FLT%UDF(flo)
enddo
close(iu)

call stop_error(0,'Ok')
end program main
