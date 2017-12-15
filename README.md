# COSMO

Fortran libraries and tools used in the COSMO project
(CTM2016-79474-R, MINECO/FEDER, UE)

The code has been developped under Centos, using the GNU Fortran (GCC)
4.8.5 compiler and the Fortran 90 netcdf library version 4.3.3.1 and python version 3.6.3

Purpose: Build a utility library and a Lagrangian trajectory integrator.
Requirements: **Fortran compiler** and the **NetCDF F90 library** version 4.3.X.X

# Installing

To install the code, use:

$ git clone https://github.com/quimbp/cosmo.git

# Compilling

Edit the file **make.inc** and modify the paths of the installation softer
(Default: $(HOME)/cosmo), the fortran compiler (Default: gfortran), and
the path to the NetCDF Fortran 90 modules and libraries (NF90_INC and
NF90_LIB).

Then type,

$ make

