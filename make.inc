# COSMO PROJECT
# Quim Ballabrera, September 2017
#
# Compiler options and libraries
# The code has been created and tested using gfortran.
# The NF90_LIB and NF90_INC variables had been fed from the
# bash environment.

# Modify according operative system and library paths:
#
# PYTHON   = Python2/3 interpreter
# FC       = Fortran95 compiler
# FFLAGS   = Compiler options
# INSTALL  = Program used to copy files setting theis mode bits, etc.
# AR       = Library archive manager 
# NF90_INC = Path to the  NetCDF Fortran 90 modules
# NF90_LIB = Path to the  NetCDF Fortran 90 libraries
# 

PYTHON = /bin/python3

FC = gfortran
FFLAGS = -O3 -fno-range-check -pedantic -finit-real=nan -finit-integer=0 -finit-character=0 -finit-logical=false 
INSTALL = install
AR = ar cq

# CENTOS
#NF90_INC = /usr/include/
#NF90_LIB = /usr/lib/i386-linux-gnu/
NF90_INC = /usr/include
NF90_LIB = /usr/lib

# Folders where files are going to be installed
#
BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

CDFLIB = -L$(NF90_LIB) -lnetcdff
CDFINC = -I$(NF90_INC)
