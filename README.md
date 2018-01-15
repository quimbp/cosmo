# COSMO

Fortran libraries and tools used in the COSMO project
(CTM2016-79474-R, MINECO/FEDER, UE)

The code has been developped under Centos, using the GNU Fortran (GCC)
4.8.5 compiler and the Fortran 90 netcdf library version 4.3.3.1 and python version 3.6.3

Purpose: Build a utility library and a Lagrangian trajectory integrator.
Requirements: **Fortran compiler** and the **NetCDF F90 library** version 4.3.X.X

## Installing

To install the code, use:

$ git clone https://github.com/quimbp/cosmo.git

## Compilling Fortran code

Edit the file **make.inc** and modify the paths of the installation softer
(Default: $(HOME)/cosmo), the fortran compiler (Default: gfortran), and
the path to the NetCDF Fortran 90 modules and libraries (NF90_INC and
NF90_LIB).

Then type,

$ make

## Python utilities

Three utilities are currently available:

### cosmo-json.py 

A visualization tool to visualize JSON files with lagrangian trajectories generated during the COSMO project. This tool allows simple selection of initial and final points and to remove undesired locations.

``` 
$ python3 cosmo-jason.py
```

### release-point.py
A tool returning the initial position and the release date of a JSON lagrangian trajectory. 

```
$ python3 release-point.py ../data/exp001.json
Opening file ../data/exp0001.json
1.4167 41.0002 2017-11-22T10:35:13
```

Using the option '-label' the corresponding option names required by the COSMO Lagrangian Model are included.

```
$ python3 release-point.py -label ../data/exp001.json
Opening file ../data/exp0001.json
-xo  1.4167 -yo  41.0002  -do  2017-11-22T10:35:13
```


### cosmo-view.py

A visualization tool to visualize ocean currents. It can load fields directly from operational providers or from Netcdf files. It can superpose satellite SST data received by the HPRT station located in the Institut de Ciencies del Mar (ICM/CSIC) and lagrangian trajectories created by the COSMO Lagrangian Model or JSON files from field experiments.

```
$ python3 cosmo-view.py
```
