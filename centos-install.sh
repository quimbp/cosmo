cat /etc/centos-release

sudo yum update -y
sudo yum install git -y
sudo yum install wget -y
sudo yum install xorg-x11-apps -y
sudo yum install epel-release -y
sudo yum install gcc-gfortran -y
sudo yum install postgresql-libs -y
sudo yum install postgresql-devel -y
sudo yum install netcdf-fortran-devel -y

# Note that in Centos, the fortran netcdf libraries are located at
# -I/usr/lib64/gfortran/modules
# -L/usr/lib64

# In Centos 7 and 8, it is better to install Cartoy
# through the Anaconda environment. To install Anaconda,
# visit 
#         https://www.anaconda.com/products/individual
#
# After download,
# sudo sh Anaconda3-2021.05-Linux-x86_64.sh
#
# After installing Anaconda3:

conda update conda
conda update --all

conda install -c anaconda pillow
conda install -c anaconda future
conda install -c anaconda psycopg2
conda install -c anaconda requests
conda install -c anaconda netcdf4
conda install -c anaconda beautifulsoup4
conda install -c menpo ffmpeg

# The following command will download:
# shapely, geos-3.8.0, proj-6.2.1, pyshp-2.10, matplotlib-3.3.1, cartopy-0.18.0, ...
conda install -c conda-forge cartopy

python3 -m pip install wget
python3 -m pip install owslib
python3 -m pip install motuclient
python3 -m pip install tkcalendar

# get the cosmo:
git clone https://github.com/quimbp/cosmo.git
cd cosmo
cp centos_make.inc make.inc
make

# After compilation,
cd bin
./cosmo-view



