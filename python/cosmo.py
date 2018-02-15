# Module for the applications built for the COSMO project 
# Quim Ballabrera, May 2017

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.image as image
from matplotlib.figure import Figure
import matplotlib.cm as cm
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg

import numpy as np
import os

COSMOCPATH = os.path.expanduser('~') + os.sep + '.cosmo-view'

# ===========
class geocdf:
# ===========
  ''' Class whose attributes will contain the information about dimensions
      and variables of a Netcdf file, making a guessing of the 
      variables representing the X, Y, Z, and T axis.'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__(self,filename):
    from netCDF4 import Dataset
    import math

    print('Opening file ',filename)
    try:
      ncid = Dataset(filename)
    except:
      print('Unable to open file')
      return None

    # Save the name of the input file:
    self.filename = filename

    # Names of the dimensions and variables:
    self.DIM_LIST = [dim for dim in ncid.dimensions] # List of nc dimensions
    self.VAR_LIST = [var for var in ncid.variables] # List of nc dimensions

    # The following variables can be used to create tkinter menus 
    self.DIM_MENU = self.DIM_LIST[:]
    self.DIM_MENU.append(' ')
    self.VAR_MENU = self.VAR_LIST[:]
    self.VAR_MENU.append(' ')

    # Number of dimensions and variables:
    self.fndims = len(self.DIM_LIST)
    self.fnvars = len(self.VAR_LIST)

    self.nx  =  1; self.ny  =  1; self.nz  =  1; self.nt  =  1
    self.idi = -1; self.idj = -1; self.idk = -1; self.idl = -1
    self.idx = -1; self.idy = -1; self.idz = -1; self.idt = -1
    self.iname = ''; self.jname = ''; self.kname = ''; self.lname = ''
    self.xname = ''; self.yname = ''; self.zname = ''; self.tname = ''
    self.time_units = ''
    self.time_calendar = ''
    self.regular = None

    # We will try to identify the axes in the file:
    for name,variable in ncid.variables.items():
      try:
        axis = getattr(variable,'axis')
        if axis == 'X':
          self.xname = name
          self.idx = self.VAR_LIST.index(name)
          if len(variable.dimensions) == 1:
            self.regular = True
            self.iname = variable.dimensions[0]
            self.idi = self.DIM_LIST.index(self.iname)
            self.nx = ncid.dimensions[self.iname].size
          else:
            self.regular = False
            for dim,dname in enumerate(self.DIM_LIST):
              word = dname.upper()
              if word[0:1] == 'X' or 'LON' in word:
                self.iname = dname
                self.idi   = dim
                self.nx    = ncid.dimensions[dname].size
        elif axis == 'Y':
          self.yname = name
          self.idy = self.VAR_LIST.index(name)
          if len(variable.dimensions) == 1:
            self.regular = True
            self.jname = variable.dimensions[0]
            self.idj = self.DIM_LIST.index(self.jname)
            self.ny = ncid.dimensions[self.jname].size
          else:
            self.regular = False
            for dim,dname in enumerate(self.DIM_LIST):
              word = dname.upper()
              if word[0:1] == 'X' or 'LON' in word:
                self.iname = dname
                self.idi   = dim
                self.nx    = ncid.dimensions[dname].size
        elif axis == 'Z':
          self.zname = name
          self.idz = self.VAR_LIST.index(name)
          self.kname = variable.dimensions[0]
          self.idk = self.DIM_LIST.index(self.kname)
          self.nz = ncid.dimensions[self.kname].size
        elif axis == 'T':
          self.tname = name
          self.idt = self.VAR_LIST.index(name)
          self.lname = variable.dimensions[0]
          self.idl = self.DIM_LIST.index(self.lname)
          self.nt = ncid.dimensions[self.lname].size
      except:
        pass

    if (self.idi,self.idj,self.idk,self.idl).count(-1) == 4:
      print('GEOCDF > No axis attributes used')
      self.geocdf_varnames(ncid)

    # Unlimitted dimension:
    self.unlimited_name = ''; self.unlimited_id = -1
    for name,dimension in ncid.dimensions.items():
      if dimension.isunlimited():
        self.unlimited_name = name
        self.unlimited_id = self.DIM_LIST.index(name)

    # Time units and calendar:
    try:
      self.time_units = ncid.variables[self.tname].units # Get time units
    except AttributeError:                        # Attribute does not exist
      self.time_units = ''

    try:
      self.time_calendar = ncid.variables[self.tname].calendar
    except AttributeError:                        # Attribute does not exist
      self.time_calendar = u"gregorian"           # Standard calendar

    # Information about dimensions:
    self.dlen          = []
    for dim,dname in enumerate(self.DIM_LIST):
      self.dlen.append(ncid.dimensions[dname].size)

    # Information about variables:
    self.vname         = []
    self.ndims         = []
    self.scale_factor  = []
    self.add_offset    = []
    self.missing       = []
    self.missing_value = []
    self.missingisnan  = []
    self.ppi           = [ -1 for i in range(self.fnvars)]
    self.ppj           = [ -1 for i in range(self.fnvars)]
    self.ppk           = [ -1 for i in range(self.fnvars)]
    self.ppl           = [ -1 for i in range(self.fnvars)]
    self.dimids        = [[-1 for i in range(self.fndims)] \
                          for j in range(self.fnvars)]
    vv = 0
    for name,variable in ncid.variables.items():
      self.vname.append(name)
      self.ndims.append(len(variable.dimensions))

      try:
        xx = getattr(variable,'scale_factor')
      except:
        xx = 1.0
      self.scale_factor.append(xx)

      try:
        xx = getattr(variable,'add_offset')
      except:
        xx = 0.0
      self.add_offset.append(xx)
 
      try:
        xx = getattr(variable,'_FillValue')
        missing = True
        isnan = math.isnan(xx)
      except:
        try:
          xx = getattr(variable,'missing_value')
          missing = True
          isnan = math.isnan(xx)
        except:
          missing = False
          xx      = None
          isnan   = None
      self.missing.append(missing)
      self.missing_value.append(xx)
      self.missingisnan.append(isnan)

      for dim,dname in enumerate(variable.dimensions):
        self.dimids[vv][dim] = self.DIM_LIST.index(dname)
        if self.dimids[vv][dim] == self.idi:
          self.ppi[vv] = dim
        if self.dimids[vv][dim] == self.idj:
          self.ppj[vv] = dim
        if self.dimids[vv][dim] == self.idk:
          self.ppk[vv] = dim
        if self.dimids[vv][dim] == self.idl:
          self.ppl[vv] = dim
     
      vv = vv + 1

  def geocdf_varnames(self,ncid):

    for dim,dname in enumerate(self.DIM_LIST):
      word = dname.upper()
      if word[0:1] == 'X' or 'LON' in word:
        self.idi   = dim
        self.iname = dname
        self.nx    = ncid.dimensions[dname].size

      if word[0:1] == 'Y' or 'LAT' in word:
        self.idj   = dim
        self.jname = dname
        self.ny    = ncid.dimensions[dname].size

      if word[0:1] == 'Z' or 'DEP' in word:
        self.idk   = dim
        self.kname = dname
        self.nz    = ncid.dimensions[dname].size

      if word[0:1] == 'T' or 'TIME' in word:
        self.idl   = dim
        self.lname = dname
        self.nt    = ncid.dimensions[dname].size

    if self.idi > -1:
      for var,vname in enumerate(self.VAR_LIST):
        word = vname.upper()
        if word[0:1] == 'X' or 'LON' in word:
          self.idx   = var
          self.xname = vname
          if len(ncid.variables[vname].dimensions) == 1:
            regular = True
          else:
            regular = False

    if self.idj > -1:
      for var,vname in enumerate(self.VAR_LIST):
        word = vname.upper()
        if word[0:1] == 'Y' or 'LAT' in word:
          self.idy   = var
          self.yname = vname
          if len(ncid.variables[vname].dimensions) == 1:
            regular = True
          else:
            regular = False

    if self.idk > -1:
      for var,vname in enumerate(self.VAR_LIST):
        word = vname.upper()
        if word[0:1] == 'Z' or 'DEP' in word:
          self.idz   = var
          self.zname = vname

    if self.idl > -1:
      for name,variable in ncid.variables.items():
        if len(variable.dimensions) == 1 and \
             variable.dimensions[0] == self.lname:
            self.idt = self.VAR_LIST.index(name)
            self.tname = name


# ==================
class plot_params():
# ==================


  def __init__ (self):
    self.MAP_PROJECTION    = tk.StringVar()
    self.MAP_PROJECTION.set('cyl')

    self.MAP_RESOLUTION    = tk.StringVar()
    self.MAP_RESOLUTION.set('l')

    self.DATA_SOUTH        = None
    self.DATA_NORTH        = None
    self.DATA_WEST         = None
    self.DATA_EAST         = None
    self.SOUTH             = tk.DoubleVar()
    self.NORTH             = tk.DoubleVar()
    self.WEST              = tk.DoubleVar()
    self.EAST              = tk.DoubleVar()

    self.COASTLINE         = tk.IntVar()
    self.COASTLINE.set(1)
    self.COASTLINE_WIDTH   = tk.DoubleVar()
    self.COASTLINE_WIDTH.set(1)
    self.COASTLINE_COLOR   = tk.StringVar()
    self.COASTLINE_COLOR.set('black')
    self.LAND_COLOR        = tk.StringVar()
    self.WATER_COLOR       = tk.StringVar()
    self.LAND_COLOR.set('coral')
    self.LAND_COLOR.set('None')
    self.WATER_COLOR.set('aqua')
    self.WATER_COLOR.set('None')

    self.TITLE             = tk.StringVar()
    self.TITLE.set('')
    self.TITLE_SIZE        = tk.IntVar()
    self.TITLE_SIZE.set(22)
    self.TITLE_BOLD        = tk.IntVar()
    self.TITLE_BOLD.set(0)

    self.XLABEL            = tk.StringVar()
    self.YLABEL            = tk.StringVar()
    self.LABEL_SIZE        = tk.IntVar()
    self.LABEL_SIZE.set(16)
    self.LABEL_PAD         = tk.IntVar()
    self.LABEL_PAD.set(24)

    self.ZLABEL            = tk.StringVar()
    self.TLABEL            = tk.StringVar()
    self.ZLABEL.set('--')
    self.TLABEL.set('--')

    self.DPI               = tk.IntVar()
    self.DPI.set(72)
    self.OUT_FILENAME      = None
    self.OUT_DPI           = tk.IntVar()
    self.OUT_DPI.set(300)

    self.SHOW_GRID         = tk.IntVar()
    self.SHOW_GRID.set(1)

    self.MERIDIAN_INI      = tk.DoubleVar()
    self.MERIDIAN_FIN      = tk.DoubleVar()
    self.MERIDIAN_INT      = tk.DoubleVar()
    self.PARALLEL_INI      = tk.DoubleVar()
    self.PARALLEL_FIN      = tk.DoubleVar()
    self.PARALLEL_INT      = tk.DoubleVar()

    self.LONLAT_COLOR      = tk.StringVar()
    self.LONLAT_SIZE       = tk.IntVar()
    self.LONLAT_COLOR.set('black')
    self.LONLAT_SIZE.set(12)
    self.X                 = None
    self.Y                 = None

    self.BLUEMARBLE        = tk.IntVar()
    self.ETOPO             = tk.IntVar()
    self.RIVERS            = tk.IntVar()
    self.BLUEMARBLE.set(0)
    self.ETOPO.set(0)
    self.RIVERS.set(0)
    self.RIVERS_WIDTH      = tk.IntVar()
    self.RIVERS_WIDTH.set(1)
    self.RIVERS_COLOR      = tk.StringVar()
    self.RIVERS_COLOR.set('black')

    self.ARCGISIMAGE       = tk.IntVar()
    self.ARCGISIMAGE.set(0)
    self.ARCGISSERVICE     = tk.StringVar()
    self.ARCGISSERVICE.set('ESRI_Imagery_world_2D')
    self.ARCGISSERVICE_LIST  = ['ESRI_Imagery_World_2D', \
                                'ESRI_StreetMap_World_2D', \
                                'NatGEo_World_Map',  \
                                'Ocean_Basemap',  \
                                'World_Imagery',  \
                                'World_Physical_Map',  \
                                'World_Shaded_Relief',  \
                                'World_Street_Map',  \
                                'World_Terrain_Base',  \
                                'World_Topo_Map']
    self.ARCGISPIXELS      = tk.IntVar()
    self.ARCGISPIXELS.set(1000)
    self.ARCGISDPI       = tk.IntVar()
    self.ARCGISDPI.set(96)
    self.ARCGISVERBOSE     = tk.BooleanVar()
    self.ARCGISVERBOSE.set(True)

    self.LOGO_FILE         = tk.StringVar()
    self.LOGO_FILE.set('cosmo-logo.png')
    self.LOGO_IMAGE        = image.imread(self.LOGO_FILE.get())
    self.LOGO_ZOOM         = tk.DoubleVar()
    self.LOGO_ZOOM.set(0.40)
    self.LOGO_LOCATION     = tk.StringVar()
    self.LOGO_LOCATION.set('SW')
    self.LOGO_X            = tk.DoubleVar()
    self.LOGO_Y            = tk.DoubleVar()
    self.LOGO_DISPLAY      = tk.BooleanVar()
    self.LOGO_DISPLAY.set(False)

    self.ISOBAT_PATH       =  tk.StringVar()
    self.ISOBATHS          =  [   0,  50, 100, 200, 400, 600, 800, \
                               1000,1200,1400,1600,1800,2000,2500, \
                               3000,3500,4000,4500,5000,5500,6000]
    self.nisobat = len(self.ISOBATHS)
    self.ISOBAT_WIDTH      = tk.DoubleVar()
    self.ISOBAT_COLOR      = tk.StringVar()
    self.ISOBAT_PATH.set('../data/isobaths/')
    self.ISOBAT_COLOR.set('black')
    self.ISOBAT_WIDTH.set(1)
    self.ISOBAT_indx       = []
    self.ISOBAT_selected   = []
    self.ISOBAT_Z          = []
    self.wvar              = []
    for i in range(self.nisobat):
      self.wvar.append(tk.IntVar())
    self.ISOBAT_LABEL      = [      '',  '50 m', '100 m', '200 m', '400 m', \
                               '600 m', '800 m','1000 m','1200 m','1400 m', \
                              '1600 m','1800 m','2000 m','2500 m','3000 m', \
                              '3500 m','4000 m','4500 m','5000 m','5500 m', \
                              '6000 m'   ]
    self.ISOBAT_LABEL_SIZE = tk.IntVar()
    self.ISOBAT_LABEL_SIZE.set(12)

    self.TIMESTAMP_SHOW    = tk.BooleanVar()
    self.TIMESTAMP_BOLD    = tk.BooleanVar()
    self.TIMESTAMP_X       = tk.DoubleVar()
    self.TIMESTAMP_Y       = tk.DoubleVar()
    self.TIMESTAMP_SIZE    = tk.IntVar()
    self.TIMESTAMP_COLOR   = tk.StringVar()
    self.TIMESTAMP_SHOW.set(True)
    self.TIMESTAMP_BOLD.set(True)
    self.TIMESTAMP_X.set(0.07)
    self.TIMESTAMP_Y.set(0.03)
    self.TIMESTAMP_COLOR.set('black')
    self.TIMESTAMP_SIZE.set(15)


# =================
class WinGeoaxes():
# =================
  '''Class to interactively select/modify the names of the
     axis variables of a netcdf file'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__ (self,icdf,parent=None):

    self.parent = parent

    #self.FILENAME = tk.StringVar()
    #self.FILENAME.set(icdf.filename)

    self.Ibox     = -1
    self.Jbox     = -1
    self.Kbox     = -1
    self.Lbox     = -1
    self.Iname    = tk.StringVar()
    self.Jname    = tk.StringVar()
    self.Kname    = tk.StringVar()
    self.Lname    = tk.StringVar()
    self.Iname.set(icdf.iname)
    self.Jname.set(icdf.jname)
    self.Kname.set(icdf.kname)
    self.Lname.set(icdf.lname)

    self.Xbox     = -1
    self.Ybox     = -1
    self.Zbox     = -1
    self.Tbox     = -1
    self.Xname    = tk.StringVar()
    self.Yname    = tk.StringVar()
    self.Zname    = tk.StringVar()
    self.Tname    = tk.StringVar()
    self.Xname.set(icdf.xname)
    self.Yname.set(icdf.yname)
    self.Zname.set(icdf.zname)
    self.Tname.set(icdf.tname)

    self.strnx = tk.StringVar()
    self.strny = tk.StringVar()
    self.strnz = tk.StringVar()
    self.strnt = tk.StringVar()
    self.Uname = tk.StringVar()
    self.Vname = tk.StringVar()
    self.strnx.set(str(icdf.nx))
    self.strny.set(str(icdf.ny))
    self.strnz.set(str(icdf.nz))
    self.strnt.set(str(icdf.nt))

  # --------------------------------------------- Frame for X, Y, Z, T

    Fr0 = ttk.Frame(self.parent,padding=5,width=700)
    ttk.Label(Fr0,text='File = '+icdf.filename,padding=5,font='italic').grid(row=0,column=0)
    #ttk.Label(Fr0,text='Filename',padding=5,font='Helvetica 12 bold').grid(row=0,column=0)
    #Wfilename = ttk.Entry(Fr0,textvariable=self.FILENAME,justify='left',width=89)
    #Wfilename.grid(row=0,column=1,sticky='ew')
    Fr0.grid(row=0,column=0,sticky='E'+'W'+'N'+'S')


    Fr1 = ttk.Frame(self.parent,padding=5,width=700,borderwidth=5,relief='sunken')
    ttk.Label(Fr1,text='Axis',width=12,font='Helvetica 12 bold').grid(row=0,column=0)
    ttk.Label(Fr1,text='X',width=17,justify='center',font='Helvetica 12 bold').grid(row=0,column=1)
    ttk.Label(Fr1,text='Y',width=17,justify='center',font='Helvetica 12 bold').grid(row=0,column=2)
    ttk.Label(Fr1,text='Z',width=17,justify='center',font='Helvetica 12 bold').grid(row=0,column=3)
    ttk.Label(Fr1,text='T',width=17,justify='center',font='Helvetica 12 bold').grid(row=0,column=4)

    ttk.Label(Fr1,text='Dimension',width=10,font='Helvetica 12 bold').grid(row=1,column=0,stick='w')
    self.Ibox = ttk.Combobox(Fr1,textvariable=self.Iname,width=12)
    self.Jbox = ttk.Combobox(Fr1,textvariable=self.Jname,width=12)
    self.Kbox = ttk.Combobox(Fr1,textvariable=self.Kname,width=12)
    self.Lbox = ttk.Combobox(Fr1,textvariable=self.Lname,width=12)
    self.Ibox['values'] = icdf.DIM_MENU
    self.Jbox['values'] = icdf.DIM_MENU
    self.Kbox['values'] = icdf.DIM_MENU
    self.Lbox['values'] = icdf.DIM_MENU

    self.Ibox.grid(row=1,column=1,sticky='W')
    self.Jbox.grid(row=1,column=2,sticky='W')
    self.Kbox.grid(row=1,column=3,sticky='W')
    self.Lbox.grid(row=1,column=4,sticky='W')
    self.Ibox.bind('<<ComboboxSelected>>',lambda e : self.iselection(icdf))
    self.Jbox.bind('<<ComboboxSelected>>',lambda e : self.jselection(icdf))
    self.Kbox.bind('<<ComboboxSelected>>',lambda e : self.kselection(icdf))
    self.Lbox.bind('<<ComboboxSelected>>',lambda e : self.lselection(icdf))

    ttk.Label(Fr1,text='Size',width=12, \
              font='Helvetica 12 bold').grid(row=2,column=0)
    wnx = ttk.Entry(Fr1,textvariable=self.strnx, \
              state='readonly',width=8).grid(row=2,column=1,sticky='W')
    wny = ttk.Entry(Fr1,textvariable=self.strny, \
              state='readonly',width=8).grid(row=2,column=2,sticky='W')
    wnz = ttk.Entry(Fr1,textvariable=self.strnz, \
              state='readonly',width=8).grid(row=2,column=3,sticky='W')
    wnt = ttk.Entry(Fr1,textvariable=self.strnt, \
              state='readonly',width=8).grid(row=2,column=4,sticky='W')

    ttk.Label(Fr1,text='Variable',width=12, \
              borderwidth=3,font='Helvetica 12 bold').grid(row=3,column=0)
    self.Xbox = ttk.Combobox(Fr1,textvariable=self.Xname,width=12)
    self.Ybox = ttk.Combobox(Fr1,textvariable=self.Yname,width=12)
    self.Zbox = ttk.Combobox(Fr1,textvariable=self.Zname,width=12)
    self.Tbox = ttk.Combobox(Fr1,textvariable=self.Tname,width=12)
    self.Xbox['values'] = icdf.VAR_MENU
    self.Ybox['values'] = icdf.VAR_MENU
    self.Zbox['values'] = icdf.VAR_MENU
    self.Tbox['values'] = icdf.VAR_MENU
    self.Xbox.grid(row=3,column=1,sticky='W')
    self.Ybox.grid(row=3,column=2,sticky='W')
    self.Zbox.grid(row=3,column=3,sticky='W')
    self.Tbox.grid(row=3,column=4,sticky='W')
    self.Xbox.bind('<<ComboboxSelected>>',lambda e: self.xselection(icdf))
    self.Ybox.bind('<<ComboboxSelected>>',lambda e: self.yselection(icdf))
    self.Zbox.bind('<<ComboboxSelected>>',lambda e: self.zselection(icdf))
    self.Tbox.bind('<<ComboboxSelected>>',lambda e: self.tselection(icdf))
    Fr1.grid(row=1,column=0,rowspan=4,sticky='ewns')


  def iselection(self,icdf):
    value_selected = self.Ibox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idi   = ind
      icdf.nx    = icdf.dlen[ind]
      icdf.iname = value_selected
    else:
      icdf.idi = -1
      icdf.nx = 1
    self.Ibox.selection_clear()
    self.strnx.set(str(icdf.nx))


  def jselection(self,icdf):
    value_selected = self.Jbox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idj   = ind
      icdf.ny    = icdf.dlen[ind]
      icdf.jname = value_selected
    else:
      icdf.idj = -1
      icdf.ny = 1
    self.Jbox.selection_clear()
    self.strny.set(str(icdf.ny))


  def kselection(self,icdf):
    value_selected = self.Kbox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idk   = ind
      icdf.nz    = icdf.dlen[ind]
      icdf.kname = value_selected
    else:
      icdf.idk = -1
      icdf.nz = 1
    self.Kbox.selection_clear()
    self.strnz.set(str(icdf.nz))


  def xselection(self,icdf):
    value_selected = self.Xbox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      if icdf.ndims[ind] == 1:
        icdf.idx   = ind
        icdf.xname = value_selected

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idi   = kk
        icdf.nx    = icdf.dlen[kk]
        icdf.iname = icdf.DIM_LIST[kk]
        self.Iname.set(icdf.iname)
        self.strnx.set(str(icdf.nx))
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Xname.set(icdf.VAR_MENU[icdf.idx])
    else:
      icdf.idx = -1
      icdf.idi = -1
      icdf.nx  =  1
      self.Xname.set(icdf.VAR_MENU[icdf.idx])
      self.Iname.set(icdf.DIM_MENU[icdf.idi])
      self.strnx.set(str(icdf.nx))
    self.Xbox.selection_clear()


  def yselection(self,icdf):
    value_selected = self.Ybox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      if icdf.ndims[ind] == 1:
        icdf.idy   = ind
        icdf.yname = value_selected

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idj   = kk
        icdf.ny    = icdf.dlen[kk]
        icdf.jname = icdf.DIM_LIST[kk]
        self.Jname.set(icdf.jname)
        self.strny.set(str(icdf.ny))
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Yname.set(icdf.VAR_MENU[icdf.idy])
    else:
      icdf.idy = -1
      icdf.idj = -1
      icdf.ny  =  1
      self.Yname.set(icdf.VAR_MENU[icdf.idy])
      self.Jname.set(icdf.DIM_MENU[icdf.idj])
      self.strny.set(str(icdf.ny))
    self.Ybox.selection_clear()


  def zselection(self,icdf):
    value_selected = self.Zbox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      if icdf.ndims[ind] == 1:
        icdf.idz   = ind
        icdf.zname = value_selected

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idk   = kk
        icdf.nz    = icdf.dlen[kk]
        icdf.kname = icdf.DIM_LIST[kk]
        self.Kname.set(icdf.kname)
        self.strnz.set(str(icdf.nz))
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Zname.set(icdf.VAR_MENU[icdf.idz])
    else:
      icdf.idz = -1
      icdf.idk = -1
      icdf.nz  =  1
      self.Zname.set(icdf.VAR_MENU[icdf.idz])
      self.Kname.set(icdf.DIM_MENU[icdf.idk])
      self.strnz.set(str(icdf.nz))
    self.Zbox.selection_clear()


  def tselection(self,icdf):
    value_selected = self.Tbox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      if icdf.ndims[ind] == 1:
        icdf.idt = ind
        icdf.tname = value_selected

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idl   = kk
        icdf.nt    = icdf.dlen[kk]
        icdf.lname = icdf.DIM_LIST[kk]
        self.Lname.set(icdf.lname)
        self.strnt.set(str(icdf.nt))
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Tname.set(icdf.VAR_MENU[icdf.idt])
    else:
      icdf.idt = -1
      icdf.idl = -1
      icdf.nt  =  1
      self.Tname.set(icdf.VAR_MENU[icdf.idt])
      self.Lname.set(icdf.DIM_MENU[icdf.idl])
      self.strnt.set(str(icdf.nt))
      icdf.time_units = ''
      icdf.time_calendar = ''
    self.Tbox.selection_clear()


# ================================
class ColormapPicker(tk.Toplevel):
# ================================
  ''' Opens a widget to select a color map'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__ (self,parent=None,cmap='jet'):

    tk.Toplevel.__init__(self,parent)
    self.title('Colormap Selector')
    self.resizable(False,False)
    self.lift()

    self.CM_NAME = tk.StringVar()
    self.CM_NAME.set(cmap)

    self.CM_LIST = [m for m in cm.datad if not m.endswith("_r")]
    self.CM_LIST.sort()
    self.a=np.outer(np.ones(1),np.arange(0,1,0.02))   # pseudo image data

    frame = tk.Frame(self)
    ttk.Label(frame,text='Select colormap', \
              font='Helvetica 12 bold').grid(row=0,column=0,padx=5)
    self.cms = ttk.Combobox(frame,textvariable=self.CM_NAME,width=14, \
                            font='Helvetica 12 bold')
    self.cms.grid(row=0,column=1,padx=5)
    self.cms.bind('<<ComboboxSelected>>',lambda e: self.cm_selection())
    self.cms['values'] = self.CM_LIST

    self.fig = Figure(figsize=(5,0.4))
    self.ax1 = self.fig.add_subplot(111)
    self.ax1.axis("off")
    self.canvas = FigureCanvasTkAgg(self.fig,master=frame)
    self.canvas.show()
    self.canvas.get_tk_widget().grid(row=0,column=2,rowspan=1,columnspan=5)
    self.canvas._tkcanvas.grid()
    self.ax1.imshow(self.a,aspect='auto',cmap=cm.get_cmap(self.CM_NAME.get()))
    self.canvas.draw()

    ttk.Button(frame,text='Done',command=self.done, \
               padding=3).grid(row=0,column=7,padx=5)

    frame.grid()


  def cm_selection(self):
    self.ax1.imshow(self.a,aspect='auto',cmap=cm.get_cmap(self.CM_NAME.get()))
    self.canvas.draw()
    self.cms.selection_clear()


  def get_colormap(self):
    return self.CM_NAME.get()


  def done(self):
    self.destroy()


# ============================================
def colormap_selector(cmap='jet',parent=None):
# ============================================
  '''Function to get the name of a colormap'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  col = ColormapPicker(parent,cmap)
  col.wait_window(col)
  res = col.get_colormap()
  return res


# =======================
class cosmo_view_field():
# =======================


  def __init__(self,FILENAME,ncid,icdf,uid,vid):

    from netCDF4 import Dataset,num2date

    self.FILENAME = FILENAME
    self.ncid     = ncid
    self.icdf     = icdf
    self.uid      = uid
    self.vid      = vid
    self.sid      = -1        # ID superposed field
    self.K        = tk.IntVar()
    self.L        = tk.IntVar()
    self.K.set(0)             # Default layer
    self.L.set(0)             # Default time step

    self.K_LIST   = list(np.arange(0,icdf.nz))
    self.L_LIST   = list(np.arange(0,icdf.nt))

   # Time and Depth selectors
    if icdf.idk > -1:
      wrk = ncid.variables[icdf.zname][:]
      self.Z_LIST = list(wrk)
    else:
      self.Z_LIST = []

    if icdf.idl > -1:
      wrk = ncid.variables[icdf.tname][:]
      self.T_LIST = list(wrk)
    else:
      self.T_LIST = []


    self.DATE = []
    for i in range(icdf.nt):
      self.DATE.append(num2date(self.T_LIST[i],            \
                       units=icdf.time_units, \
                       calendar=icdf.time_calendar))

    self.TIME = np.array([(self.DATE[i]-self.DATE[0]).total_seconds() \
                           for i in range(icdf.nt)])

# ================
def truncation(x):
# ================
  '''Function to truncate a real number to a limited set of 
     digits. For example truncation(0.23456) = 0.2 '''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  factor = 10**np.round(np.log(np.absolute(x))/np.log(10))
  if factor > np.absolute(x):
    factor = 0.1*factor

  return np.round(x/factor)*factor


# ================
def empty(string):
# ================
  '''Logical function that checks if a string is empty:
     empty('     ') = True'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  if bool(string.strip()):
    return False
  else:
    return True

