! ****************************************************************************
! ... help.f90
! ... Quim Ballabrera, April 2017
! ... COSMO Lagrangian model
! ... Help information for program lagrangian
! ... Version 0.1, released October 2017
! ****************************************************************************

subroutine program_help(version,author)

use cosmo

implicit none

character(len=*), intent(in)                           :: version
character(len=*), intent(in)                           :: author

! ... The help
! ...
call help_version(version)
call help_progname('LAGRANGIAN')
call help_author(author)
call help_summary('Reads the zonal and meridional velocity components &
  &from an NetCDF file and calculates trajectories from the time-evolving &
  &velocity field or streamlines for stationary cases. If a list of &
  &floats is not provided, NFLOATS positions will be randomly generated. The &
  &number and position of the floats may be read from an ASCII file &
  &(LON, LAT, DEPTH, RELEASE_TIME [,...]) or passed though command line &
  &using options -xo and -yo. In the later case, a cloud of &
  &NFLOATS may also be generated using the option -cloud. &
  &The number of internal time steps can be modified using the &
  &option -idt, that specifies the time step of the internal loop. &
  &The program writes a trajectory file (NetCDF) and the final position of &
  &the floats (ASCII). The names can be specified using options -trajectory &
  &and -end, respctively.')
call help_option ('-U token=value [token=value...]','Input U field (required)','')
call help_option ('-V token=value [token=value...]','Input U field (required)','')
call help_option ('-W token=value [token=value...]','Input W field (optional and not yet implemented)','')
call help_option ('-T token=value [token=value...]','Input T field (optional)','')
call help_option ('-cal        calendar','Optional input time calendar','gregorian')
call help_option ('-edt        DT (in seconds)','Option to select the &
 &time step of the external loop (if input velocity field has only one time &
 &step)','86400')
call help_option ('-end        filename ','Output final position file name', &
        'release.out')
call help_option ('-idt        DT (in seconds)','Option to select the &
 &time step of the internal loop','3600')
call help_option ('-nfloats    NFLOATS','Number of floats to be randomly &
 &generated if the positions file is not provided.','10')
call help_option ('-record     RECORD','Option to start the trajectories &
 &from the specified record. In case of streamlines, use the selected record &
 &in the file','1')
call help_option ('-release    filename ','Input position release file name','')
call help_option ('-reverse           ','Option for backward integration','')
call help_option ('-Rx      RADIUS_X','If options -xo and -yo are used, the &
 &-Rx option allows setting a cloud of NFLOATS floats randomly distributed &
 &in a RADIUS_X x RADIUS_Y region centered at the (XO,YO) position.','0.1')
call help_option ('-Ry      RADIUS_Y','If options -xo and -yo are used, the &
 &-Rx option allows setting a cloud of NFLOATS floats randomly distributed &
 &in a RADIUS_X x RADIUS_Y region centered at the (XO,YO) position.','0.1')
call help_option ('-seed       INTEGER','Option to modify the random number generator','')
call help_option ('-stationary','Option to calculate streamlines from a given &
 &record (By the default the first one)','')
call help_option ('-time_scale SCALE_FACTOR','Optional factor to transform the &
 &time values of velocity field to seconds.','1.')
call help_option ('-time_sim  Time_length (in days)','Option to select the &
 &length of the simulation (if input velocity field has only one time &
 &step)','7')
call help_option ('-trajectory filename ','Output trajectory file name','out.nc')
call help_option ('-xo         XO','Optional value of the float initial &
 &position','')
call help_option ('-yo         YO','Optional value of the float initial &
 &position','')
call help_option ('--options   filename','To read the commandline options from a file.','')
call help_option ('--help','To show this help','')
call help_example('lagrangian -U file=roms.nc u=u x=lon y=lat t=time &
 &-V file=roms.nc v=v x=lon y=lat t=time -release release.inp &
 &-trajectory float.nc -end release.out')

end subroutine program_help
