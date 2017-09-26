! ****************************************************************************
! ... help.f90
! ... Quim Ballabrera, April 2017
! ... Help information for program lagrangian
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
  &velocity field or the streamlines for the stationary case. If a list of &
  &floats is not provided, NFLOATS positions will be randomly generated. The &
  &position of the NFLOATS floats may be read from an ASCII file (compatible &
  &with the Connectivity Modeling System format), Netcdf file or &
  &a position passed though command line. In the later case, a cloud of &
  &NFLOATS may also be generated using the option -cloud. &
  &The number of time steps can be modified using the &
  &option -steps. The trajectory is written in a NetCDF file.')
call help_option ('-inp       filename','Input Netcdf file','')
call help_option ('-cal       calendar','Optional input time calendar','gregorian')
call help_option ('-reverse           ','Option for backward integration','')
call help_option ('-u         varname','Name of variable containing U','u')
call help_option ('-v         varname','Name of variable containing V','v')
call help_option ('-temp      varname','Must be specified to read temperature','')
call help_option ('-sal       varname','Must be specified to read salinity','')
call help_option ('-out       filename','Output netcdf file','out.nc')
call help_option ('-release   filename','Optional ASCII or NetCDF file with & 
 &the initial float positions. If ASCII, tt must have the CMS format. &
 &If NetCDF, it must have the same structure as the output trajectory file.','')
call help_option ('-frec      step','Optional value of the record of the input &
 &NetCDF floating positions with the initial location of the float. If negative, &
 &the last record in the file is used as the initial location.','-1')
call help_option ('-nfloats   NFLOATS','Number of floats to be randomly &
 &generated if the positions file is not provided.','10')
call help_option ('-xo        XO','Optional value of the float initial &
 &position','')
call help_option ('-yo        YO','Optional value of the float initial &
 &position','')
call help_option ('-cloud     RADIUS','If options -xo and -yo are used, the &
 &-cloud option allows setting a cloud of NFLOATS floats randomly distributed &
 &in a RADIUS x RADIUS region centered at the (XO,YO) position.','')
call help_option ('-days      DAYS','Length of the simulation in days. In case &
 &of calculating trajectories from a multiple time-step file, the default &
 &length of the simulation is the number of records in the file.','7')
call help_option ('-scale     SCALE_FACTOR','Optional factor to transform the &
 &time values fromhe input NetCDF file to seconds.','')
call help_option ('-record    RECORD','Option to select a given &
 &time-record as a stationnary field.','')
call help_option ('-steps     STEPS','Option to select the number &
 &of steps per day to solve the differential equations.','6')
call help_option ('-iname     X DIM NAME','Option to specify the name of &
 &the X dimension.','[Automatically derived]')
call help_option ('-jname     Y DIM NAME','Option to specify the name of &
 &the Y dimension.','[Automatically derived]')
call help_option ('-kname     Z DIM NAME','Option to specify the name of &
 &the Z dimension.','[Automatically derived]')
call help_option ('-lname     T DIM NAME','Option to specify the name of &
 &the T dimension.','[Automatically derived]')
call help_option ('-xname     X VAR NAME','Option to specify the name of &
 &the X variable.','[Automatically derived]')
call help_option ('-yname     Y VAR NAME','Option to specify the name of &
 &the Y variable.','[Automatically derived]')
call help_option ('-zname     Z VAR NAME','Option to specify the name of &
 &the Z variable.','[Automatically derived]')
call help_option ('-tname     T VAR NAME','Option to specify the name of &
 &the T variable.','[Automatically derived]')
call help_option ('--options  filename','To read the commandline options from a file.','')
call help_option ('--help','To show this help','')
call help_example('lagrangian -inp http://thredds.socib.es/thredds/dodsC/operational_models&
 &/oceanographical/hydrodynamics/wmop/2017/03/roms_wmop_20170327.nc &
 &-xname lon_uv -yname lat_uv -days 10 -steps 24 -record 1 -out &
 & socib.nc')

end subroutine program_help

