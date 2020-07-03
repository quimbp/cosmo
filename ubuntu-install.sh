sudo apt-get update
sudo apt-get install x11-apps

# gfortran and libnetcdff
# -----------------------
sudo apt-get install gfortran
sudo apt-get install netcdf-bin
sudo apt-get install libnetcdff-dev
dpkg -L libnetcdff-dev

# Python3 tkinter and utilities
# -----------------------------
sudo apt-get install python3-tk
sudo apt-get install python3-pil
sudo apt-get install python3-pil.imagetk
sudo apt-get install python3-setuptools
sudo apt-get install python3-numpy
sudo apt-get install python3-cftime
sudo apt-get install python3-netcdf4
sudo apt-get install python3-matplotlib
sudo apt-get install python3-bs4
sudo apt-get install python3-scipy
sudo apt-get install python3-pip

sudo python3 -m pip install tkcolorpicker
sudo python3 -m pip install wget
sudo python3 -m pip install py-postgresql
sudo python3 -m pip install psycopg2-binary
sudo python3 -m pip install owslib
sudo python3 -m pip install motuclient

# Cartopy
# -------
sudo apt-get install cython3
sudo apt-get install libgeos-3.8.0
sudo apt-get install libgeos-dev
sudo apt-get install shapelib
sudo apt-get install python3-shapely
sudo apt-get install python3-pyshp
sudo apt-get install python3-six
sudo apt-get install proj-bin
sudo apt-get install libproj15
sudo apt-get install libproj-dev
sudo apt-get install python3-cartopy

# Virtual environment
# -------------------
sudo apt-get install python3-virtualenv
sudo apt-get install python3-venv
python3 -m venv --system-site-packages venv
source venv/bin/activate

# Other utilities
# ---------------
sudo apt-get install synaptic
sudo apt-get install nco
