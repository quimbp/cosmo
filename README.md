# COSMO

Fortran libraries and tools used in the COSMO project
(CTM2016-79474-R, MINECO/FEDER, UE)

The code has been developed under Centos, using the GNU Fortran (GCC)
4.8.5 compiler and the Fortran 90 netcdf library version 4.3.3.1 and python version 3.6.3. It has been also tested in Ubuntu 16.04.3 LTS (xenial).

Purpose: Build a fortran utilities library and a Lagrangian trajectory integrator (COSMO Lagrangian Model).
Requirements: **Fortran compiler** and the **NetCDF F90 library** version 4.3.X.X

## Dependencies

To have access to all the capabilities of the codes, both a fortran compiler and a Python 3 interpreter are necessary. In addition, the following libraries, utilities and python modules are required:

### Utilities

* FFmpeg: a collection of libraries and programs for handling video and multimedia (Required by cosmo-view to create animations).

### Libraries

* NetCDF-Fortran library for running the Lagrangian model (written in Fortran).
* GEOS
* PROJ
* Shapely

### Python3 modules

* tkinter
* PIL
* numpy
* scipy
* matplotlib
* netcdf4
* Beautiful Soup
* wget
* psycopg2
* cartopy
* future
* requests
* tkcolorpicker
* shapely


Some of these packages may already be installed in your operational system. Other might need to be installed. Be aware that the actual names of these packages and the commands required for their installation will vary from one system to another. **Windows 10** users are strongly encouraged to install a _Windows Subsystem for Linux_ (WSL) that will allow installing a **Ubuntu 20.04 LTS** environment running directly under windows.


## Installing

Once all dependencies have been installed, the cosmo code can be retrieved through **git**:

```
$ git clone https://github.com/quimbp/cosmo.git
```
## Downloading isobath data

Auxiliary isobath contours, useful for cosmo-view can be downloaded from here:

https://nuvol.cmima.csic.es/owncloud/s/x8fGx5cEyHo7KMt/download

Download this file and save in the cosmo root folder (the folder where the file make.inc is located). Then, install it as
```
$ tar -xvzf isobaths.tar.gz
```
This command should extract the isobaths and place them in the folder **data/isobaths**

## Compilling Fortran code

Edit (if necessary) the file **make.inc** and modify the fortran compiler, the fortran compiler options or the paths to the NetCDF modules and libraries. Be aware that **make.inc** uses the utility **nc-config** to obtain the location of the NetCDF files. If using the **gfortran** compiler and the standard version of the NetCDF libraries, no modifications of **make.inc** should be necessary.

Then type,

```
$ make
```

If compilation succeeds, the COSMO Lagrangian Model will be located in **$COSMO/bin/clm**

Type
```
$ $COSMO/bin/clm --help
```
to obtain the list of options for running the model. A detailed User Documentation will be uploaded shortly.

## Python utilities

Three utilities are currently available:

### cosmo-json.py 

A visualization tool to visualize JSON files with lagrangian trajectories generated during the COSMO project. This tool allows simple selection of initial and final points and to remove undesired locations.

``` 
$ python3 cosmo-jason.py
```

### release_point.py
A tool returning the initial position and the release date of a JSON lagrangian trajectory. 

```
$ python3 release_point.py ../data/exp001.json
Opening file ../data/exp0001.json
1.4167 41.0002 2017-11-22T10:35:13
```

Using the option '-label' the corresponding option names required by the COSMO Lagrangian Model are included.

```
$ python3 release_point.py -label ../data/exp001.json
Opening file ../data/exp0001.json
-xo  1.4167 -yo  41.0002  -do  2017-11-22T10:35:13
```


### cosmo-view.py

A visualization tool to visualize ocean currents. It can load fields directly from operational providers or from Netcdf files. It can superimpose satellite SST data received by the HPRT station located in the Institut de Ciencies del Mar (ICM/CSIC) and lagrangian trajectories created by the COSMO Lagrangian Model or JSON files from field experiments.

```
$ $COSMO/bin/cosmo-view
```

A detailed User Documentation will be uploaded shortly.

## Authors

* **Joaquim Ballabrera** (*ICM/CSIC*) E-mail: joaquim@icm.csic.es
* **Emilio García** (*ICM/CSIC*) E-mail: emilio@icm.csic.es

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgements

The COSMO project is funded by the Spanish Government Minstry of Economy and Competitivity (CTM2016-79474-R, MINECO/FEDER, UE). This work is the result of the collaboration of ICM-CSIC Research Team: Dr. Emilio García, Dr. Jordi Isern, Dr. Jordi Solé and Dr. José Antonio Jiménez. 
