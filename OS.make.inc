# COSMO PROJECT
# Quim Ballabrera, September 2017
#
# Compiler options and libraries
# The code has been created and tested using gfortran.
# The NF90_LIB and NF90_INC variables had been fed from the
# bash environment.

# Modify according operative system and library paths:
#
# COSMO = the project root folder
# FC = Fortran95 compiler
# FFLAGS = Compiler options
# INSTALL = Program used to copy files setting theis mode bits, etc.
# AR = Library archive manager 
# NF90_INC = Path to the  NetCDF Fortran 90 modules
# NF90_LIB = Path to the  NetCDF Fortran 90 libraries
# 
FC = gfortran
FFLAGS = -O4 -fno-range-check -finit-real=nan -finit-integer=0 -finit-character=0 -finit-logical=false  -fno-automatic 
INSTALL = install
AR = ar cq
#
# Include the path to the netcdf fortran modules and libraries
#
NF90_INC = /usr/local/Cellar/netcdf/4.8.0_1/include
NF90_LIB = /usr/local/Cellar/netcdf/4.8.0_1/lib
#
# Folders where files are going to be installed
#
CDFLIB = -L$(NF90_LIB) -lnetcdf -lnetcdff
CDFINC = -I$(NF90_INC)
