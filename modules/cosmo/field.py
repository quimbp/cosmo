import tkinter as tk
import numpy as np
from netCDF4 import Dataset,num2date

from cosmo import tools

# =====================
class fld_parameters():
# =====================
  ''' Class for FLD fields.

  class CONTOUR
    |  |
    |  class FLD
    |    |
    |    class CDF (FILENAME,NC,ICDF)
    |
    class PARAMS

  class VELOCITY
    | | |
    | | class U FLD
    | |   |
    | |   class CDF (FILENAME,NC,ICDF)
    | |
    | class V FLD
    |   |
    |   class CDF (FILENAME,NC,ICDF)
    |
    class PARAMS
    '''

  def __init__(self,filename=None,varname=None):
  # ============================================
    ''' Initialization of the 2D FLD class'''

    self.MESSAGE    = '\nFLD_PARA:\n'
    #self.FILENAME   = tk.StringVar()
    self.varname    = None
    self.georef     = tk.BooleanVar()
    self.nc         = None
    self.icdf       = None
    self.varid      = None
    self.ndims      = None
    self.K          = tk.IntVar()
    self.L          = tk.IntVar()
    self.with_axes  = None              
    self.x          = None              # input netcdf x
    self.y          = None              # input netcdf y
    self.xx         = None              # meshed x
    self.yy         = None              # meshed y
    self.xmin       = None
    self.xmax       = None
    self.ymin       = None
    self.ymax       = None
    self.data       = None              # 2D values
    self.minval     = None
    self.maxval     = None
    self.units      = None
    self.missing    = None


  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes '''

    conf = {}
    #conf['FILENAME'] = self.FILENAME.get()
    conf['VARNAME']  = self.varname
    conf['K']        = self.K.get()
    conf['L']        = self.L.get()
    conf['varid']    = self.varid
    conf['ndims']    = self.ndims
    conf['georef']   = self.georef.get()
    if self.icdf is None:
      conf['ICDF']   = None
    else:
      conf['ICDF']   = self.icdf.conf_get()

  def conf_set(self):
  # =================
    ''' Set class attributes from class dictionary '''

    #self.FILENAME.set(conf['FILENAME'])
    self.varname = conf['VARNAME']
    self.K.set(conf['K'])
    self.L.set(conf['L'])
    self.varid = conf['varid']
    self.ndims = conf['ndims']
    self.georef.set(conf['georef'])
    if conf['ICDF'] is None:
      pass
    else:
      self.icdf.set(conf['ICDF'])

  def open(self,filename,**args):
  # =============================
    ''' Open netcdf file and initialize data '''

    try:
      wid = args["wid"]
    except:
      wid = None

    #self.FILENAME.set(filename)
    self.nc   = Dataset(filename)
    self.icdf = tools.geocdf(filename, wid=wid)


  def read(self,K=None,L=None,**args):
  # ====================
    ''' Read 2D field from netcdf file '''

    try:
      wid = args["wid"]
    except:
      wid = None

    if self.varid is None:
      self.data = None
      tools.toconsola('Undefined variable,',wid=wid)
      return

    if self.ndims is None:
      self.data = None
      tools.toconsola('Undefined number of variable dimensions,',wid=wid)
      return

    if self.ndims == 2:
      data = self.nc.variables[self.varname][:,:]
    elif self.ndims == 3:
      if self.icdf.ppl[self.varid] > -1:
        data = self.nc.variables[self.varname][L,:,:].squeeze()
      elif CDF.icdf.ppk[self.varid]  > -1:
        data = self.nc.variables[self.varname][K,:,:].squeeze()
      else:
        messagebox.showinfo(message='Invalid variable dimensions')
        data = None
        return
    elif self.ndims == 4:
      data = self.nc.variables[self.varname][L,K,:,:].squeeze()


    # Check if the returned arrays is Masked Array:
    if isinstance(data,np.ma.MaskedArray):
      self.data = data.copy()
    else:
      if self.missing is None:
        self.data = np.ma.masked_array(data)
      else:
        self.data = np.ma.masked_equal(data,self.missing)

    self.minval = float(self.data.min())
    self.maxval = float(self.data.max())

    tools.toconsola('Min val = '+str(self.minval),wid=wid)
    tools.toconsola('Max val = '+str(self.maxval),wid=wid)

    #print(type(self.data))
    #print(self.data.fill_value)
    #print(self.data.mask)

  def get_info(self,**args):
  # ========================
    ''' Retrieve information about the selected variable '''

    try:
      wid = args["wid"]
    except:
      wid = None

    # --- Units
    try:
      self.units = self.nc.variables[self.varid].getncattr('units')
    except:
      self.units = ''


    # --- Fill or missing value
    try:
      self.missing = self.nc.variables[self.varid].getncattr('_FillValue')
    except:
      try:
        self.missing = self.nc.variables[self.varid].getncattr('missing_value')
      except:
        self.missing = None

 
    tools.toconsola('Missing value: '+ '%s'%self.missing+ '\n',wid=wid)



  def get_grid(self,**args):
  # ========================
    ''' Get the grid of the axes from the netcdf file '''

    try:
      wid = args["wid"]
    except:
      wid = None

    if self.varid is None:
      tools.toconsola('Undefined variable,',wid=wid)
      return

    if self.icdf.withX and self.icdf.withY:
      self.with_axes = True
    else:
      self.with_axes = False

    if self.with_axes:

      self.x = self.nc.variables[self.icdf.xname][:]
      self.y = self.nc.variables[self.icdf.yname][:]

      print(self.icdf.grid2d)
      if self.icdf.grid2d:
        self.xx = self.x
        self.yy = self.y
      else:
        self.xx,self.yy = np.meshgrid(self.x,self.y)

    else:
      
      self.x = np.arange(self.icdf.nx)
      self.y = np.arange(self.icdf.ny)
      self.xx,self.yy = np.meshgrid(self.x,self.y)

    self.xmin = float(np.min(self.x))
    self.xmax = float(np.max(self.x))
    self.ymin = float(np.min(self.y))
    self.ymax = float(np.max(self.y))

  def get_zlist(self,**args):
  #============================
    ''' Set the Z_LIST depth selector values '''

    try:
      wid = args["wid"]
    except:
      wid = None

    if self.icdf.idk > -1:
      if self.icdf.withZ:
        wrk = self.nc.variables[self.icdf.zname][:]
        Z_LIST = list(wrk)
      else:
        Z_LIST = np.arange(self.icdf.nz)
      tools.toconsola(str(Z_LIST),wid=wid)
    else:
      Z_LIST = []

    return Z_LIST


  def get_tlist(self,**args):
  #============================
    ''' Sets the T_LIST, DATE and TIME selector values '''

    try:
      wid = args["wid"]
    except:
      wid = None

    if self.icdf.idl > -1:

      # T_LIST
      if self.icdf.withT:
        wrk = self.nc.variables[self.icdf.tname][:]
        T_LIST = list(wrk)
      else:
        T_LIST = np.arange(self.icdf.nt)
      tools.toconsola(str(T_LIST),wid=wid)

      # DATE
      print(T_LIST)
      print(self.icdf.time_units)
      print(self.icdf.time_calendar)

      try:
        DATE = []
        for i in range(self.icdf.nt):
          DATE.append(num2date(T_LIST[i],       \
                      units=self.icdf.time_units,    \
                      calendar=self.icdf.time_calendar))
      except:
        DATE = np.arange(self.icdf.nt)

      # TIME
      try:
        TIME = np.array([(DATE[i]-DATE[0]).total_seconds() \
                           for i in range(self.icdf.nt)])
      except:
        TIME = np.array([(DATE[i]-DATE[0]) \
                           for i in range(self.icdf.nt)])

    else:
      T_LIST = []
      DATE = [' ']
      TIME = np.array([0])

    print('T_LIST: ', T_LIST)
    print('DATE: ', DATE)
    print('TIME: ', TIME)
    return T_LIST, DATE, TIME

