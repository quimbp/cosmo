# COSMO PROJECT
# Compiler options and libraries
# The code has been created and tested using gfortran.
# The NF90_LIB and NF90_INC variables had been fed from the
# bash environment.

INSTALL = install
FC = gfortran
FFLAGS = -O4 -fno-range-check -finit-real=nan -finit-integer=0 -finit-character=0 -finit-logical=false 

AR = ar cq

# Folders where files are going to be installed
#
COSMO = $(HOME)/cosmo
BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

# Netcdf for fortran90 location of libraries and modules:
#

NF90_INC = /usr/lib64/gfortran/modules/
NF90_LIB = /usr/lib64/

CDFLIB = -L$(NF90_LIB) -lnetcdff
CDFINC = -I$(NF90_INC)
