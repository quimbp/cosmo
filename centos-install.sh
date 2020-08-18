ca://t /etc/centos-release

sudo yum update -y
sudo yum install xorg-x11-apps -y

export DISPLAY=127.0.0.1:0.0
xeyes 

sudo yum install epel-release -y
sudo yum install gcc-gfortran -y
sudo yum install netcdf-fortran-devel -y
sudo yum install git -y

# Note that in Centos, the fortran netcdf libraries
# -I/usr/lib64/gfortran/modules
# -L/usr/lib64


# In Centos 7, there is an incompatibility between the 
# latest available version of the PROJ library and the
# version required for Cartopy [In Centos 8 is already fixed].
# Therefore, the simplest method in Centos 7 is to use
# conda. To avoid downloading the whole Anaconda cllection,
# we will use the Miniconda distribution that comes with only
# conda and python3
#
# https://docs.conda.io/en/latest/miniconda.html

sudo yum install wget
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
exit

# After logging in ...

conda install -c anaconda pillow
conda install -c anaconda python-dateutil
conda install -c anaconda future
conda install -c anaconda psycopg2
conda install -c anaconda requests
conda install -c anaconda numpy
conda install -c anaconda netcdf4
conda install -c anaconda scipy
conda install -c anaconda beautifulsoup4
conda install -c menpo ffmpeg

# The following command will download:
# shapely, geos-3.8.0, proj-6.2.1, pyshp-2.10, matplotlib-3.3.1, cartopy-0.18.0, ...
conda install -c conda-forge cartopy

pip install tkcolorpicker
pip install wget

# get the cosmo:
git clone https://github.com/quimbp/cosmo.git
cd cosmo
cp centos_make.inc make.inc
make

# After compilation,
cd bin
./cosmo-view



