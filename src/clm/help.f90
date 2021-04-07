! ****************************************************************************
! ... help.f90
! ... Quim Ballabrera, April 2017
! ... COSMO Lagrangian model
! ... Help information for program lagrangian
! ... V3.0: Initial version (April 2021)
! ****************************************************************************

subroutine program_help(HLP,version,author)

use module_help

implicit none

type(type_help), intent(inout)                         :: HLP
character(len=*), intent(in)                           :: version
character(len=*), intent(in)                           :: author

! ... The help
! ...
HLP%version = version
HLP%progname = 'CLM'
HLP%author = author
call HLP%set_summary('Reads the zonal and meridional velocity components &
  &from a NetCDF file and calculates trajectories from the time-evolving &
  &velocity field or streamlines for stationary cases. If a list of &
  &floats is not provided, NFLOATS positions will be randomly generated. The &
  &number and position of the floats may be read from an ASCII file &
  &(LON, LAT, DEPTH, RELEASE_TIME [,...]) or passed though command line &
  &using options -xo and -yo (and optionally -to or -do). In the later case, &
  &a random cloud of NFLOATS may also be generated using the option -rand. &
  &The number of internal time steps can be modified using the &
  &option -idt, that specifies the time step of the internal loop. &
  &The program writes a trajectory file (NetCDF) and the final position of &
  &the floats (ASCII). The names can be specified using options -trajectory &
  &and -end, respctively.')
call HLP%add_option ('-OU token=value [token=value...]','Input ocean U field (required)','')
call HLP%add_option ('-OV token=value [token=value...]','Input ocean V field (optional)','')
call HLP%add_option ('-AU token=value [token=value...]','Input atmosphere U field (optional)','')
call HLP%add_option ('-AV token=value [token=value...]','Input atmosphere V field (optional)','')
call HLP%add_option ('-A11        value','Component a11 of the atmosphere response matrix','0.0')
call HLP%add_option ('-A12        value','Component a12 of the atmosphere response matrix','0.0')
call HLP%add_option ('-A21        value','Component a21 of the atmosphere response matrix','0.0')
call HLP%add_option ('-A22        value','Component a22 of the atmosphere response matrix','0.0')
call HLP%add_option ('-release    filename ','Initial position release file name. &
 &It must exist if no initial coordinates are specified (options -xo and -yo)','')
call HLP%add_option ('-from       INITIAL_DATE','Date at which the Lagrangian simulation will start','')
call HLP%add_option ('-for        TIME_PERIOD','Length of the Lagrangian simulation','')
call HLP%add_option ('-dt         DT (in seconds)','Runge-Kutta time step value','600')
call HLP%add_option ('-reverse           ','Perform backward integration','')
call HLP%add_option ('-trajectory filename ','Output trajectory file','out.nc')
call HLP%add_option ('-end        filename ','Output final position file','release.out')
call HLP%add_option ('-xmin       MIN_LONGITUDE','Option to crop the computational domain','')
call HLP%add_option ('-xmax       MAX_LONGITUDE','Option to crop the computational domain','')
call HLP%add_option ('-ymin       MIN_LATITUDE','Option to crop the computational domain','')
call HLP%add_option ('-ymax       MAX_LATITUDE','Option to crop the computational domain','')
call HLP%add_option ('-xo         XO','Optional value of the float initial &
 &position','')
call HLP%add_option ('-yo         YO','Optional value of the float initial &
 &position','')
call HLP%add_option ('-mu         value','Non-dimensioanl amplitude of the gaussian multiplicative noise','0')
call HLP%add_option ('-va         value','Amplitude of the gaussian istropic velocity fluctuation','0')
call HLP%add_option ('-alpha      value','Non-dimensional ocean velocity multiplicator','1.0')
call HLP%add_option ('-rand       NFLOATS','Option to request a simulation with NFLOATS randomly &
  &generated floats per each release position.','1')
call HLP%add_option ('-Rx         RADIUS_X','Longitude radius for releasing random floats &
 &around each release location','0.02')
call HLP%add_option ('-Ry         RADIUS_Y','Latitude radius for releasing random floats &
 &around each release location','0.02')
call HLP%add_option ('-verbose','To increase output verbosity','')
call HLP%add_option ('--options   filename','To read the commandline options from a file','')
call HLP%add_option ('--help','To show this help','')

call HLP%add_option ('-do         RELEASE_DATE','Optional value of the float &
 &initial release time (Date in iso8601 format)','')

call HLP%add_option ('-to         TO','Optional value of the float initial &
 &release time (seconds after initial simulation time)','0')
call HLP%set_example('clm -OU file=roms.nc u=u x=lon y=lat t=time &
 &-V file=roms.nc v=v x=lon y=lat t=time -release release.inp &
 &-trajectory float.nc -end release.out')

end subroutine program_help
