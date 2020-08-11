import tkinter as tk
from tkinter import ttk
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
    conf['VARNAME']  = self.varname
    conf['varid']    = self.varid
    conf['ndims']    = self.ndims
    conf['with_axes']= self.with_axes
    conf['georef']   = self.georef.get()
    if self.icdf is None:
      conf['ICDF']   = None
    else:
      conf['ICDF']   = self.icdf.conf_get()

    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from class dictionary '''

    #self.FILENAME.set(conf['FILENAME'])
    self.varname = conf['VARNAME']
    self.varid = conf['varid']
    self.ndims = conf['ndims']
    self.with_axes = conf['with_axes']
    self.georef.set(conf['georef'])
    if conf['ICDF'] is None:
      pass
    else:
      self.icdf.conf_set(conf['ICDF'])


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


  def read(self,K=0,L=0,**args):
  # ====================
    ''' Read 2D field from netcdf file and returns the data array'''

    try:
      wid = args["wid"]
    except:
      wid = None

    if self.varid is None:
      data = None
      tools.toconsola('Undefined variable,',wid=wid)
      return

    if self.ndims is None:
      data = None
      tools.toconsola('Undefined number of variable dimensions,',wid=wid)
      return


    tools.toconsola('Reading, K, L = '+str(K)+', '+str(L),wid=wid)

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


    # Eliminate NaN values in field:
    fill_value = data.fill_value
    a = data.filled()
    a[np.isnan(a)] = fill_value
    data = np.ma.masked_equal(a,fill_value)

    # Check if the returned arrays is Masked Array:
    #if isinstance(data,np.ma.MaskedArray):
    #  pass
    #else:
    #  if self.missing is None:
    #    data = np.ma.masked_array(data)
    #  else:
    #    data = np.ma.masked_equal(data,self.missing)

    #self.minval = float(self.data.min())
    #self.maxval = float(self.data.max())
    #tools.toconsola('Min val = '+str(self.minval),wid=wid)
    #tools.toconsola('Max val = '+str(self.maxval),wid=wid)

    return data

  def mean(self,nt,K=0,**args):
  # ============================
    ''' Computes the time mean of 2D field from netcdf file and returns the data array'''

    #print('In FIELD MEAN ....', K)
    #print('varname: ', self.varname)
    #print('varid: ', self.varid)
    try:
      wid = args["wid"]
    except:
      wid = None

    for L in range(0,nt):
      print('L = ', L)
      data = self.read(K=K,L=L,wid=wid)
      print(type(data))
      if L==0:
        num = data.copy()
      else:
        num = num + data

    self.data = num / nt
    self.minval = float(self.data.min())
    self.maxval = float(self.data.max())


  def variance(self,nt,K=0,**args):
  # ===============================
    ''' Computes the time variance of 2D field from netcdf file and returns the data array'''

    print('In FIELD VARIANCE ....', K)
    print('varname: ', self.varname)
    print('varid: ', self.varid)
    try:
      wid = args["wid"]
    except:
      wid = None

    print('nt = ', nt)

    for L in range(0,nt):
      print('L = ', L)
      data = self.read(K=K,L=L,wid=wid)
      print(type(data))
      if L==0:
        num1 = data.copy()
        num2 = np.square(data)
      else:
        num1 += data
        num2 += np.square(data)

    data = num2/nt - np.square(num1/nt)

    self.data = data.copy()
    self.minval = float(self.data.min())
    self.maxval = float(self.data.max())



  def get_info(self,**args):
  # ========================
    ''' Retrieve information about the selected variable '''

    try:
      wid = args["wid"]
    except:
      wid = None

    # --- Units
    try:
      self.units = self.nc.variables[self.varname].getncattr('units')
    except:
      self.units = ''


    # --- Fill or missing value
    try:
      self.missing = self.nc.variables[self.varname].getncattr('_FillValue')
    except:
      try:
        self.missing = self.nc.variables[self.varname].getncattr('missing_value')
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
    else:
      Z_LIST = []

    return Z_LIST


  def get_tlist(self,**args):
  #============================
    ''' Sets the T_LIST, DATE and TIME selector values '''

    answer = tk.StringVar()

    def _yes():
    # ---------
      global ww
      ww.destroy()
      ww = None
      answer.set('Y')

    def _no():
    # ---------
      global ww
      ww.destroy()
      ww = None
      answer.set('N')

    try:
      wid = args["wid"]
    except:
      wid = None

    if self.icdf.idl > -1:

      # T_LIST
      if self.icdf.withT:

        # ... Read the time axis
        # ...
        wrk = self.nc.variables[self.icdf.tname][:]

        # ... Verify that all values are valid,
        # ... If not, raise an alarm !
        # ...
        if np.ma.is_masked(wrk):          # Not good

          wrk2 = wrk.copy()
          wrk2.mask = False

          global ww
          ww = tk.Toplevel()
          ww.title('Time values')
          ww.resizable(width=False,height=False)
          ww.protocol('WM_DELETE_WINDOW',_no)
          ttk.Label(ww,text='Error').grid(row=0,column=0,sticky='w')
          ttk.Label(ww,text='Time axis has masked values').grid(row=1,column=0,sticky='w')
          ttk.Label(ww,text='List of values in NetCDF file:').grid(row=2,column=0,sticky='ew')
          log = tk.Text(ww,height=5)
          log.grid(row=3,column=0,columnspan=3,padx=10,pady=10,sticky='nsew')

          # Scrollbar
          scrollb = tk.Scrollbar(ww,command=log.yview)
          scrollb.grid(row=3,column=3,sticky='nsew',padx=2,pady=2)
          log['yscrollcommand'] = scrollb.set

          for i in range(len(wrk2)):
            string = '\t {};\n'.format(wrk2[i])
            log.insert('end',string)
          log.configure(state='disabled')
          ttk.Label(ww,text='Accept values?').grid(row=8,column=0)
          ttk.Button(ww,text='Yes',command=_yes).grid(row=8,column=1)
          ttk.Button(ww,text='No',command=_no).grid(row=8,column=2)
          ww.wait_window()

          if answer.get() == 'Y':
            T_LIST = list(wrk2)
          else:
            T_LIST = np.arange(self.icdf.nt)
        else:
          T_LIST = list(wrk)
      else:
        T_LIST = np.arange(self.icdf.nt)

      # DATE
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

    return T_LIST, DATE, TIME

