''' COSMO project python module.
    It contains the functions required for the utilitites created
    for the COSMO project to work
	EGL, 06/2020: Changes:
		No more support to python 2.X 
		Added: toconsola(), colsel(), map_proj()
		Modified: scale_bar()
'''

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import font as tkfont

#import matplotlib
#matplotlib.use('TkAgg')
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.cm as cm
from matplotlib.figure import Figure
from matplotlib.font_manager import FontProperties

import numpy as np
import os
import io
import json
from os import listdir
from os.path import isfile, join
import datetime
import math
import requests
from bs4 import BeautifulSoup

from cosmo import ncdump

try:
  to_unicode = unicode
except:
  to_unicode = str

__version__ = '1.0'
__author__  = 'Quim Ballabrera'
__date__    = 'February 2018'

# ================
def empty(string):
# ================
  ''' Logical function to check if a string is empty'''
  __version__ = "1.0"
  __author__  = 'Quim Ballabrera'
  __date__    = 'June 2017'

  if bool(string.strip()):
    return False
  else:
    return True


# ===================
def exists(filename):
# ===================
  ''' Logical function to check if a file exists'''
  __version__ = "1.0"
  __author__  = 'Quim Ballabrera'
  __date__    = 'April 2018'

  try:
    os.stat(filename)
    exist = True
  except:
    exist = False
  return exist


# ====================
def myround(x,prec=1):
# ====================
  '''Function to truncate a real number to a limited set of 
     digits. For example truncation(0.23456) = 0.2 '''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  from math import floor,log10,fabs,pow

  r = floor(log10(fabs(x)))
  factor = pow(10,r)

  return float(round(x/factor,prec)*factor)

# ================================
class Win_permission(tk.Toplevel):
# ================================
  ''' Opens a widget asking for permission'''

  def isyes(self):
  # ==============
    self.destroy()
    self.value = True
    return 

  def isno(self):
  # =============
    self.destroy()
    self.value = False
    return

  def get_value(self):
  # ==================
    return self.value

  def __init__(self,parent,question,nowarning):

    BGC = 'orange red'
    BTC = 'red'
    tk.Toplevel.__init__(self,parent)
    self.configure(bg=BGC)
    self.title('Warning')
    self.lift()
      
    self.value = False
    f0 = tk.Frame(self,bg=BGC)
    tk.Label(f0,text=question,bg=BGC).grid(row=0,column=0,padx=3)
    tk.Button(f0,text='Yes',command=self.isyes,bg=BTC).grid(row=0,column=1,padx=3)
    tk.Button(f0,text='Cancel',command=self.isno,bg=BTC).grid(row=0,column=2,padx=3)
    #ttk.Checkbutton(f0,text='Do not ask again', \
    tk.Checkbutton(f0,text='', \
                   bg=BGC, \
                   bd=0,   \
                   fg=BGC, \
                   variable=nowarning).grid(row=1, \
                                             column=0, \
                                             sticky='e', \
                                             padx=3)
    tk.Label(f0,text='Do not ask again',bg=BGC).grid(row=1,column=1,columnspan=2,sticky='w')
    f0.grid()


# ============================
class get_remote(tk.Toplevel):
# ============================
  ''' Opens a widget to select a remote file'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__ (self,parent=None):
  # ==============================
    self.url = tk.StringVar()

    tk.Toplevel.__init__(self,parent)
    self.title('Open Remote')
    self.protocol('WM_DELETE_WINDOW',self.cancel)
    self.lift()

    F0 = ttk.Frame(self,borderwidth=5,padding=5)
    tk.Label(F0,text='Enter the URL of a remote dataset to open:').grid(row=0,column=0,sticky='w',padx=3,columnspan=8)
    Window_entry = ttk.Entry(F0,textvariable=self.url,width=80)
    Window_entry.grid(row=1,column=0,columnspan=12,sticky='w')
    Bcancel = ttk.Button(F0,text='Cancel',command=self.cancel)
    Bcancel.grid(row=2,column=5,padx=3)
    Bdone = ttk.Button(F0,text='Done',command=self.done)
    Bdone.grid(row=2,column=6,padx=3)
    F0.grid()

    self.wait_window()


  def cancel(self):
  # ================
    self.url.set('')
    self.destroy()

  def done(self):
  # ================
    self.destroy()

  def filename(self):
  # =================
    return self.url.get()

# ===================================================
def askforpermission(parent,question,nowarning):
# ===================================================

  ask = Win_permission(parent,question,nowarning)
  ask.wait_window(ask)
  return ask.get_value()

# ====================
def placeontop(a,ind):
# ====================
  '''Move the cell a[ind] to the top a[0]'''

  aa = a.copy()
  a[0] = aa[ind]
  ii = -1
  for i in range(len(a)-1):
    if i != ind:
      ii = ii + 1
      a[1+i] = aa[ii]
  return a

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
    self.canvas.draw()
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

# =============
class geocdf():
# =============
  ''' Class whose attributes will contain the information about dimensions
      and variables of a Netcdf file, making a guessing of the 
      variables representing the X, Y, Z, and T axis. 
      version 1.0 - Initial version, June 2017.
      version 1.1 - Corrected undesired behavior, July 2020'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2020"

  def __init__(self,filename=None,**args):
  # ================================
    from netCDF4 import Dataset
    import math

    try:
      wid = args["wid"]
    except:
      wid = None

    if filename is None:
      toconsola('Empty Geocdf values ',wid=wid)
      self.filename = ''
      self.varname  = ''
      self.withX  = False
      self.withY  = False
      self.withZ  = False
      self.withT  = False
      self.grid2d = False
      self.georef = False
      self.nx     =  1
      self.ny     =  1
      self.nz     =  1
      self.nt     =  1
      self.idi    = -1
      self.idj    = -1
      self.idk    = -1
      self.idl    = -1
      self.idx    = -1
      self.idy    = -1
      self.idz    = -1
      self.idt    = -1
      self.xname  = ''
      self.yname  = ''
      self.zname  = ''
      self.tname  = ''
      self.time_units  = ''
      self.time_calendar  = ''
      self.ndims  = ''
      self.ppi    = []
      self.ppj    = []
      self.ppk    = []
      self.ppl    = []
      self.DIM_LIST = []
      self.VAR_LIST = []
      self.VAR_MENU = []
      return



    toconsola('Opening file '+filename,wid=wid)
    try:
      ncid = Dataset(filename)
    except:
      toconsola('Unable to open file '+filename,wid=wid)
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

    # To account for the role as axis of each dimension and variable:
    # Values: None, 'X', 'Y', 'Z', 'T'
    self.DIM_AXIS = [None for dim in self.DIM_MENU]
    self.VAR_AXIS = [None for dim in self.VAR_MENU]

    # Number of dimensions and variables:
    self.fndims = len(self.DIM_LIST)
    self.fnvars = len(self.VAR_LIST)

    self.withX = False; self.withY = False; 
    self.withZ = False; self.withT = False
    self.nx  =  1; self.ny  =  1; self.nz  =  1; self.nt  =  1
    self.idi = -1; self.idj = -1; self.idk = -1; self.idl = -1
    self.idx = -1; self.idy = -1; self.idz = -1; self.idt = -1
    self.iname = ''; self.jname = ''; self.kname = ''; self.lname = ''
    self.xname = ''; self.yname = ''; self.zname = ''; self.tname = ''
    self.time_units = ''
    self.time_calendar = ''
    self.grid2d = False
    self.georef  = True

    # Trying to get the GEOCDF axes:
    # Strategy: 
    # A) Through attribute axis
    # B) Through attribute long_name
    # C) Through attribute standard name
    # D) Through variable name
    # E) The first variable
    
    self.withX = False
    for name,variable in reversed(list(ncid.variables.items())):
      try:
        axis = getattr(variable,'axis').lower()
        with_axis = True
      except:
        with_axis = False
      try:
        long_name = getattr(variable,'long_name').lower()
        with_longname = True
      except:
        with_longname = False
      try:
        standard_name = getattr(variable,'standard_name').lower()
        with_stdname = True
      except:
        with_stdname = False

      if name[0:3].lower() == 'lon':
        this_one = True
      else:
        this_one = False

      if with_axis and axis == 'x': 
        this_one = True
      elif with_longname and 'longitude' in long_name:
        this_one = True
      elif with_stdname and 'longitude' in standard_name:
        this_one = True

      if this_one:
        ndms = len(variable.dimensions)
        if ndms == 1 or ndms == 2:
          self.xname = name
          self.idx = self.VAR_LIST.index(name)
          self.VAR_AXIS[self.idx] = 'X'
          self.withX = True
          ndms = len(variable.dimensions)
          if ndms == 1:
            self.iname = variable.dimensions[0]
            self.idi = self.DIM_LIST.index(self.iname)
            self.DIM_AXIS[self.idi] = 'X'
            self.nx = ncid.dimensions[self.iname].size
          else:
            self.jname = variable.dimensions[0]
            self.idj = self.DIM_LIST.index(self.jname)
            self.DIM_AXIS[self.idj] = 'Y'
            self.ny = ncid.dimensions[self.jname].size
            self.iname = variable.dimensions[1]
            self.idi = self.DIM_LIST.index(self.iname)
            self.DIM_AXIS[self.idi] = 'X'
            self.nx = ncid.dimensions[self.iname].size
            self.grid2d = True

    withY = False
    for name,variable in reversed(list(ncid.variables.items())):
      try:
        axis = getattr(variable,'axis').lower()
        with_axis = True
      except:
        with_axis = False
      try:
        long_name = getattr(variable,'long_name').lower()
        with_longname = True
      except:
        with_longname = False
      try:
        standard_name = getattr(variable,'standard_name').lower()
        with_stdname = True
      except:
        with_stdname = False

      if name[0:3].lower() == 'lat':
        this_one = True
      else:
        this_one = False

      if with_axis and axis == 'y': 
        this_one = True
      elif with_longname and 'latitude' in long_name:
        this_one = True
      elif with_stdname and 'latitude' in standard_name:
        this_one = True

      if this_one:
        ndms = len(variable.dimensions)
        if ndms == 1 or ndms == 2:
          self.yname = name
          self.idy = self.VAR_LIST.index(name)
          self.VAR_AXIS[self.idy] = 'Y'
          self.withY = True
          if ndms == 1:
            self.jname = variable.dimensions[0]
            self.idj = self.DIM_LIST.index(self.jname)
            self.DIM_AXIS[self.idj] = 'Y'
            self.ny = ncid.dimensions[self.jname].size
          else:
            self.jname = variable.dimensions[0]
            self.idj = self.DIM_LIST.index(self.jname)
            self.DIM_AXIS[self.idj] = 'Y'
            self.ny = ncid.dimensions[self.jname].size
            self.iname = variable.dimensions[1]
            self.idi = self.DIM_LIST.index(self.iname)
            self.DIM_AXIS[self.idi] = 'X'
            self.nx = ncid.dimensions[self.iname].size
            self.grid2d = True

    withZ = False
    for name,variable in reversed(list(ncid.variables.items())):
      try:
        axis = getattr(variable,'axis').lower()
        with_axis = True
      except:
        with_axis = False
      try:
        long_name = getattr(variable,'long_name').lower()
        with_longname = True
      except:
        with_longname = False
      try:
        standard_name = getattr(variable,'standard_name').lower()
        with_stdname = True
      except:
        with_stdname = False

      if 'depth' in name.lower():
        this_one = True
      else:
        this_one = False

      if name.lower() == 's_rho':
        this_one = True
      if name.lower() == 's_w':
        this_one = True

      if with_axis and axis == 'z': 
        this_one = True
      elif with_longname and 'depth' in long_name:
        this_one = True
      elif with_stdname and 'depth' in standard_name:
        this_one = True

      if this_one:
        ndms = len(variable.dimensions)
        if ndms == 1:
          self.zname = name
          self.idz = self.VAR_LIST.index(name)
          self.VAR_AXIS[self.idz] = 'Z'
          self.withZ = True
          
          self.kname = variable.dimensions[0]
          self.idk = self.DIM_LIST.index(self.kname)
          self.DIM_AXIS[self.idk] = 'Z'
          self.nz = ncid.dimensions[self.kname].size

    withT = False
    for name,variable in reversed(list(ncid.variables.items())):
      try:
        axis = getattr(variable,'axis').lower()
        with_axis = True
      except:
        with_axis = False
      try:
        long_name = getattr(variable,'long_name').lower()
        with_longname = True
      except:
        with_longname = False
      try:
        standard_name = getattr(variable,'standard_name').lower()
        with_stdname = True
      except:
        with_stdname = False
      try:
        units = getattr(variable,'units').lower()
        with_units = True
      except:
        with_units = False



      if 'time' in name.lower():
        this_one = True
      else:
        this_one = False


      if with_axis and axis == 't': 
        this_one = True
      elif with_longname and 'time' in long_name:
        this_one = True
        if 'scale' in long_name.lower():      # Check if variable is a time scale !
          this_one = False
      elif with_stdname and 'time' in standard_name:
        this_one = True
        if 'scale' in long_name.lower():      # Check if variable is a time scale !
          this_one = False
      elif with_units and 'since' in units:
        this_one = True

      if this_one:
        ndms = len(variable.dimensions)
        if ndms == 1:
          self.tname = name
          self.idt = self.VAR_LIST.index(name)
          self.VAR_AXIS[self.idt] = 'T'
          self.withT = True
          
          self.lname = variable.dimensions[0]
          self.idl = self.DIM_LIST.index(self.lname)
          self.DIM_AXIS[self.idl] = 'T'
          self.nt = ncid.dimensions[self.lname].size


    if self.withX and self.withY:
      self.georef = True
    else:
      self.georef = False


    # Unlimitted dimension:
    # By default we set it to time dimension
    self.unlimited_name = ''; self.unlimited_id = -1
    for name,dimension in ncid.dimensions.items():
      if dimension.isunlimited():
        toconsola('UNLIMITED dimension: '+name,wid=wid)
        self.unlimited_name = name
        self.unlimited_id = self.DIM_LIST.index(name)
        self.lname = name
        self.idl = self.unlimited_id
        self.DIM_AXIS[self.idl] = 'T'
        self.nt = ncid.dimensions[name].size

    # Time units and calendar:
    if self.idt > -1:
      try:
        self.time_units = ncid.variables[self.tname].units # Get time units
      except AttributeError:                        # Attribute does not exist
        self.time_units = ''
      try:
        self.time_calendar = ncid.variables[self.tname].calendar
      except AttributeError:                        # Attribute does not exist
        self.time_calendar = u"gregorian"           # Standard calendar
    else:
      print('GEOCDF > No time axis')
      self.time_units = ''
      self.time_calendar = u"gregorian"             # Standard calendar

    # Information about dimensions:
    self.dlen          = []
    for dim,dname in enumerate(self.DIM_LIST):
      self.dlen.append(ncid.dimensions[dname].size)

    if self.idx == -1 | self.idy == -1:
      self.georef = False

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

  def conf_get(self):
  # =================

    conf = {}
    conf['withX'] = self.withX
    conf['withY'] = self.withY
    conf['withZ'] = self.withZ
    conf['withT'] = self.withT
    conf['grid2d'] = self.grid2d
    conf['georef'] = self.georef
    conf['nx'] = self.nx
    conf['ny'] = self.ny
    conf['nz'] = self.nz
    conf['nt'] = self.nt
    conf['idi'] = self.idi
    conf['idj'] = self.idj
    conf['idk'] = self.idk
    conf['idl'] = self.idl
    conf['idx'] = self.idx
    conf['idy'] = self.idy
    conf['idz'] = self.idz
    conf['idt'] = self.idt
    conf['xname'] = self.xname
    conf['yname'] = self.yname
    conf['zname'] = self.zname
    conf['tname'] = self.tname
    conf['time_units'] = self.time_units
    conf['time_calendar'] = self.time_calendar
    conf['ndims'] = self.ndims
    conf['ppi'] = self.ppi
    conf['ppj'] = self.ppj
    conf['ppk'] = self.ppk
    conf['ppl'] = self.ppl
    conf['vname'] = self.vname
    conf['VAR_MENU'] = self.VAR_MENU
    return conf

  def conf_set(self,conf):
  # ======================

    self.withX   = conf['withX']
    self.withY   = conf['withY']
    self.withZ   = conf['withZ']
    self.withT   = conf['withT']
    self.grid2d  = conf['grid2d']
    self.georef  = conf['georef']
    self.nx  = conf['nx']
    self.ny  = conf['ny']
    self.nz  = conf['nz']
    self.nt  = conf['nt']
    self.idi = conf['idi']
    self.idj = conf['idj']
    self.idk = conf['idk']
    self.idl = conf['idl']
    self.idx = conf['idx']
    self.idy = conf['idy']
    self.idz = conf['idz']
    self.idt = conf['idt']
    self.xname = conf['xname']
    self.yname = conf['yname']
    self.zname = conf['zname']
    self.tname = conf['tname']
    self.time_units = conf['time_units']
    self.time_calendar = conf['time_calendar']
    self.ndims = conf['ndims']
    self.ppi = conf['ppi']
    self.ppj = conf['ppj']
    self.ppk = conf['ppk']
    self.ppl = conf['ppl']
    self.vname = conf['vname']
    self.VAR_MENU = conf['VAR_MENU']

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
            self.grid2d = False
          else:
            self.grid2d = True

    if self.idj > -1:
      for var,vname in enumerate(self.VAR_LIST):
        word = vname.upper()
        if word[0:1] == 'Y' or 'LAT' in word:
          self.idy   = var
          self.yname = vname
          if len(ncid.variables[vname].dimensions) == 1:
            self.grid2d = False
          else:
            self.grid2d = True

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

# =================
class WinGeoaxes():
# =================
  '''Class to interactively select/modify the names of the
     axis variables of a netcdf file.
     Version 1.0, June 2017 original version
     Version 1.1, July 2020 move the ncdump from the menu to the 
                  widget body as a button.'''


  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2020"

  def __init__ (self,icdf,ncid,parent=None):

    self.parent = parent
    self.icdf   = icdf
    self.ncid   = ncid

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

    self.grid2d = tk.BooleanVar()
    self.georef = tk.BooleanVar()
    self.grid2d.set(icdf.grid2d)
    self.georef.set(icdf.georef)

    self.FILENAME = tk.StringVar()
    self.FILENAME.set(icdf.filename)

  # --------------------------------------------- Frame for X, Y, Z, T

    #menubar = tk.Menu(self.parent)
    #menu = tk.Menu(menubar,tearoff=0)
    #menubar.add_cascade(label='File',menu=menu)
    #menu.add_command(label='ncdump',command=self.ncdump)
    #try:
    #  self.parent.config(menu=menubar)
    #except AttributeError:
    #  # parent is a toplevel window (Python 2.4/Tkinter 1.63)
    #  self.parent.tk.call(self.parent, "config", "-menu", menubar)

    font_italic = tkfont.Font(font='TkDefaultFont').copy()
    font_italic['slant'] = tkfont.ITALIC
    
    Fr0 = ttk.Frame(self.parent,padding=5,width=700)
    #ttk.Label(Fr0,text='Filename = '+icdf.filename,padding=5,font=font_italic)  \
    #   .grid(row=0,column=0,columnspan=7)
    ttk.Label(Fr0,text='Filename = ',padding=5,font=font_italic)  \
       .grid(row=0,column=0,columnspan=1)
    self.fname = ttk.Entry(Fr0,textvariable=self.FILENAME,justify='left',width=50,state='readonly')
    self.fname.grid(row=0,column=1,columnspan=6)

    self.Bncdump = ttk.Button(Fr0,text='ncdump')
    self.Bncdump.grid(row=0,column=8,padx=2)
    self.Bncdump.bind('<Button-1>',lambda e: self.ncdump(ncid))

    # AAA
    Fr0.grid(row=0,column=0,sticky='E'+'W'+'N'+'S')

    font_bold = tkfont.Font(font='TkDefaultFont').copy()
    font_bold['weight']='bold'

    Fr1 = ttk.Frame(self.parent,padding=5,width=700,borderwidth=5,relief='sunken')
    ttk.Label(Fr1,text='Axis',width=12,font=font_bold).grid(row=0,column=0)
    ttk.Label(Fr1,text='X',width=17,anchor='center',font=font_bold) \
       .grid(row=0,column=1)
    ttk.Label(Fr1,text='Y',width=17,anchor='center',font=font_bold) \
       .grid(row=0,column=2)
    ttk.Label(Fr1,text='Z',width=17,anchor='center',font=font_bold) \
       .grid(row=0,column=3)
    ttk.Label(Fr1,text='T',width=17,anchor='center',font=font_bold) \
       .grid(row=0,column=4)

    ttk.Label(Fr1,text='Dimension',width=10,font=font_bold) \
       .grid(row=1,column=0,stick='w')
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
    self.Ibox.bind('<<ComboboxSelected>>',lambda e : self.iselection(icdf,ncid))
    self.Jbox.bind('<<ComboboxSelected>>',lambda e : self.jselection(icdf,ncid))
    self.Kbox.bind('<<ComboboxSelected>>',lambda e : self.kselection(icdf,ncid))
    self.Lbox.bind('<<ComboboxSelected>>',lambda e : self.lselection(icdf,ncid))

    ttk.Label(Fr1,text='Size',width=12, \
              font=font_bold).grid(row=2,column=0)
    wnx = ttk.Entry(Fr1,textvariable=self.strnx, \
              state='readonly',width=8).grid(row=2,column=1,sticky='W')
    wny = ttk.Entry(Fr1,textvariable=self.strny, \
              state='readonly',width=8).grid(row=2,column=2,sticky='W')
    wnz = ttk.Entry(Fr1,textvariable=self.strnz, \
              state='readonly',width=8).grid(row=2,column=3,sticky='W')
    wnt = ttk.Entry(Fr1,textvariable=self.strnt, \
              state='readonly',width=8).grid(row=2,column=4,sticky='W')

    ttk.Label(Fr1,text='Variable',width=12, \
              borderwidth=3,font=font_bold).grid(row=3,column=0)
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
    self.Xbox.bind('<<ComboboxSelected>>',lambda e: self.xselection(icdf,ncid))
    self.Ybox.bind('<<ComboboxSelected>>',lambda e: self.yselection(icdf,ncid))
    self.Zbox.bind('<<ComboboxSelected>>',lambda e: self.zselection(icdf,ncid))
    self.Tbox.bind('<<ComboboxSelected>>',lambda e: self.tselection(icdf,ncid))

    self.wgr = tk.Checkbutton(Fr1,text='Georeferenced',variable=self.georef, \
          font=font_bold,onvalue=True,offvalue=False)
    self.wgr.grid(row=4,column=1)
    self.wgr.bind('<Button-1>',lambda e: self._georef(icdf))

    #self.wrg = tk.Checkbutton(Fr1,text='Regular grid',variable=self.grid2d,font=font_bold)
    #self.wrg.grid(row=4,column=3)

    Fr1.grid(row=1,column=0,rowspan=4,sticky='ewns')

    self.Window_ncdump = None


  def ncdump(self,NC):
  # ========================
    ''' Show the contents of the file'''

    # -----------
    def _close():
    # -----------
      self.Window_ncdump.destroy()


    if self.ncid is None:
      messagebox.showinfo(message='Load a file fisrt')
      return

    if self.Window_ncdump is None:
      self.Window_ncdump = tk.Toplevel(self.parent)
      self.Window_ncdump.title('ncdump')
      self.Window_ncdump.resizable(width=True,height=True)
      self.Window_ncdump.protocol('WM_DELETE_WINDOW',_close)
      ncdump.WinNcdump(self.Window_ncdump,NC)
    else:
      self.Window_ncdump.lift()

  def iselection(self,icdf,ncid):
  # =============================
    value_selected = self.Ibox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idi   = ind
      icdf.nx    = icdf.dlen[ind]
      icdf.iname = value_selected
      icdf.DIM_AXIS[icdf.idi] = 'X'
      icdf.withX = True
      #Update ppi
      vv = 0
      for name,variable in ncid.variables.items():
        for dim,dname in enumerate(variable.dimensions):
          if icdf.dimids[vv][dim] == icdf.idi:
            icdf.ppi[vv] = dim
        vv = vv + 1
      #Update ppi
    else:
      icdf.idi = -1
      icdf.nx = 1
      icdf.withX = False
    self.Ibox.selection_clear()
    self.strnx.set(str(icdf.nx))

  def jselection(self,icdf,ncid):
  # =============================
    value_selected = self.Jbox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idj   = ind
      icdf.ny    = icdf.dlen[ind]
      icdf.jname = value_selected
      icdf.DIM_AXIS[icdf.idj] = 'Y'
      icdf.withY = True
      #Update ppj
      vv = 0
      for name,variable in ncid.variables.items():
        for dim,dname in enumerate(variable.dimensions):
          if icdf.dimids[vv][dim] == icdf.idj:
            icdf.ppj[vv] = dim
        vv = vv + 1
      #Update ppj
    else:
      icdf.idj = -1
      icdf.ny = 1
      icdf.withY = False
    self.Jbox.selection_clear()
    self.strny.set(str(icdf.ny))

  def kselection(self,icdf,ncid):
  # =============================
    value_selected = self.Kbox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idk   = ind
      icdf.nz    = icdf.dlen[ind]
      icdf.kname = value_selected
      icdf.DIM_AXIS[icdf.idk] = 'Z'
      icdf.withZ = True
      #Update ppk
      vv = 0
      for name,variable in ncid.variables.items():
        for dim,dname in enumerate(variable.dimensions):
          if icdf.dimids[vv][dim] == icdf.idk:
            icdf.ppk[vv] = dim
        vv = vv + 1
      #Update ppl
    else:
      icdf.idk = -1
      icdf.nz = 1
      icdf.withZ = False
    self.Kbox.selection_clear()
    self.strnz.set(str(icdf.nz))

  def lselection(self,icdf,ncid):
  # =============================
    value_selected = self.Lbox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idl   = ind
      icdf.nt    = icdf.dlen[ind]
      icdf.lname = value_selected
      icdf.DIM_AXIS[icdf.idl] = 'T'
      icdf.withT = True

      #Update ppl
      vv = 0
      for name,variable in ncid.variables.items():
        for dim,dname in enumerate(variable.dimensions):
          if icdf.dimids[vv][dim] == icdf.idl:
            icdf.ppl[vv] = dim
        vv = vv + 1
      #Update ppl
    else:
      icdf.idl = -1
      icdf.nt = 1
      icdf.withT = False
    self.Lbox.selection_clear()
    self.strnt.set(str(icdf.nt))

  def xselection(self,icdf,ncid,value_selected=None):
  # ============================================
    if value_selected is None:
      value_selected = self.Xbox.get()

    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      icdf.idx   = ind
      icdf.xname = value_selected
      icdf.withX = True

      if icdf.ndims[ind] == 1:

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idi   = kk
        icdf.nx    = icdf.dlen[kk]
        icdf.iname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.grid2d = False
        self.Iname.set(icdf.iname)
        self.strnx.set(str(icdf.nx))
        self.georef.set(icdf.georef)
        self.grid2d.set(icdf.grid2d)
        #Update ppi
        vv = 0
        for name,variable in ncid.variables.items():
          for dim,dname in enumerate(variable.dimensions):
            if icdf.dimids[vv][dim] == icdf.idi:
              icdf.ppi[vv] = dim
          vv = vv + 1
        #Update ppi
      elif icdf.ndims[ind] == 2:
        #messagebox.showinfo(message='Two-dimensional grid')
        kk = icdf.dimids[ind][1]
        icdf.idi   = kk
        icdf.nx    = icdf.dlen[kk]
        icdf.iname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.grid2d = True
        self.Iname.set(icdf.iname)
        self.strnx.set(str(icdf.nx))
        self.georef.set(icdf.georef)
        self.grid2d.set(icdf.grid2d)
        kk = icdf.dimids[ind][0]
        icdf.idj   = kk
        icdf.ny    = icdf.dlen[kk]
        icdf.jname = icdf.DIM_LIST[kk]
        self.Jname.set(icdf.jname)
        self.strny.set(str(icdf.ny))
        #Update ppi and ppj
        vv = 0
        for name,variable in ncid.variables.items():
          for dim,dname in enumerate(variable.dimensions):
            if icdf.dimids[vv][dim] == icdf.idi:
              icdf.ppi[vv] = dim
            if icdf.dimids[vv][dim] == icdf.idj:
              icdf.ppj[vv] = dim
          vv = vv + 1
        #Update ppi and ppj
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Xname.set(icdf.VAR_MENU[icdf.idx])
    else:
      icdf.withX = False
      icdf.idx = -1
      #icdf.idi = -1
      #icdf.nx  =  1
      self.Xname.set(icdf.VAR_MENU[icdf.idx])
      #self.Iname.set(icdf.DIM_MENU[icdf.idi])
      #self.strnx.set(str(icdf.nx))
    self.Xbox.selection_clear()

    if icdf.withX and icdf.withY:
      icdf.georef = True
    else:
      icdf.georef = False
    self.georef.set(icdf.georef)

  def yselection(self,icdf,ncid):
  # ==============================
    value_selected = self.Ybox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      icdf.idy   = ind
      icdf.yname = value_selected
      icdf.withY = True
      if icdf.ndims[ind] == 1:

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idj   = kk
        icdf.ny    = icdf.dlen[kk]
        icdf.jname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.grid2d = False
        self.Jname.set(icdf.jname)
        self.strny.set(str(icdf.ny))
        #Update ppj
        vv = 0
        for name,variable in ncid.variables.items():
          for dim,dname in enumerate(variable.dimensions):
            if icdf.dimids[vv][dim] == icdf.idj:
              icdf.ppj[vv] = dim
          vv = vv + 1
        #Update ppj
      elif icdf.ndims[ind] == 2:
        #messagebox.showinfo(message='Two-dimensional grid')
        kk = icdf.dimids[ind][1]
        icdf.idi   = kk
        icdf.nx    = icdf.dlen[kk]
        icdf.iname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.grid2d = True
        self.Iname.set(icdf.iname)
        self.strnx.set(str(icdf.nx))
        kk = icdf.dimids[ind][0]
        icdf.idj   = kk
        icdf.ny    = icdf.dlen[kk]
        icdf.jname = icdf.DIM_LIST[kk]
        self.Jname.set(icdf.jname)
        self.strny.set(str(icdf.ny))
        #Update ppi and ppj
        vv = 0
        for name,variable in ncid.variables.items():
          for dim,dname in enumerate(variable.dimensions):
            if icdf.dimids[vv][dim] == icdf.idi:
              icdf.ppi[vv] = dim
            if icdf.dimids[vv][dim] == icdf.idj:
              icdf.ppj[vv] = dim
          vv = vv + 1
        #Update ppi and ppj
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Yname.set(icdf.VAR_MENU[icdf.idy])
    else:
      icdf.withY = False
      icdf.idy = -1
      #icdf.idj = -1
      #icdf.ny  =  1
      self.Yname.set(icdf.VAR_MENU[icdf.idy])
      #self.Jname.set(icdf.DIM_MENU[icdf.idj])
      #self.strny.set(str(icdf.ny))
    self.Ybox.selection_clear()

    if icdf.withX and icdf.withY:
      icdf.georef = True
    else:
      icdf.georef = False
    self.georef.set(icdf.georef)


  def _georef(self,icdf):
  # ========================
    if self.georef.get() == False:
      #   If self.geooref.get() == False
      #   we pass from false to true
      icdf.georef = True
    else:
      #   If self.geooref.get() == True
      #   we pass from true to false
      icdf.georef = False
      icdf.idx    = -1
      icdf.idy    = -1
      icdf.xname = ''
      icdf.yname = ''
      self.Xname.set(icdf.VAR_MENU[icdf.idx])
      self.Yname.set(icdf.VAR_MENU[icdf.idy])

  def zselection(self,icdf,ncid):
  # =============================
    value_selected = self.Zbox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      icdf.withZ = True
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
        #Update ppk
        vv = 0
        for name,variable in ncid.variables.items():
          for dim,dname in enumerate(variable.dimensions):
            if icdf.dimids[vv][dim] == icdf.idk:
              icdf.ppk[vv] = dim
          vv = vv + 1
        #Update ppk
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Zname.set(icdf.VAR_MENU[icdf.idz])
    else:
      icdf.withZ = False
      icdf.idz = -1
      #icdf.idk = -1
      #icdf.nz  =  1
      self.Zname.set(icdf.VAR_MENU[icdf.idz])
      #self.Kname.set(icdf.DIM_MENU[icdf.idk])
      #self.strnz.set(str(icdf.nz))
    self.Zbox.selection_clear()

  def tselection(self,icdf,ncid):
  # =============================
    value_selected = self.Tbox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      icdf.withT = True
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

        #Update ppl
        vv = 0
        for name,variable in ncid.variables.items():
          for dim,dname in enumerate(variable.dimensions):
            if icdf.dimids[vv][dim] == icdf.idl:
              icdf.ppl[vv] = dim
          vv = vv + 1
        #Update ppl

      elif icdf.ndims[ind] == 0:
        print('No associated dimension')
        icdf.idt = ind
        icdf.tname = value_selected
        # No dimension associated:
        icdf.idl = -1
        icdf.nt  = 1
        icdf.lname = ''
        self.Lname.set(icdf.lname)
        self.strnt.set(str(icdf.nt))
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Tname.set(icdf.VAR_MENU[icdf.idt])
    else:
      icdf.withT = False
      icdf.idt = -1
      #icdf.idl = -1
      #icdf.nt  =  1
      self.Tname.set(icdf.VAR_MENU[icdf.idt])
      #self.Lname.set(icdf.DIM_MENU[icdf.idl])
      #self.strnt.set(str(icdf.nt))
      icdf.time_units = ''
      icdf.time_calendar = ''
    self.Tbox.selection_clear()

  def selected_var(self,icdf,ncid,Vbox):
  # ====================================
    value_selected = Vbox.get()
    # Save currently selected variables and axes
    nx  = icdf.nx;  ny  = icdf.ny;   nz = icdf.nz;   nt = icdf.nt
    idi = icdf.idi; idj = icdf.idj; idk = icdf.idk; idl = icdf.idl
    idx = icdf.idx; idy = icdf.idy; idz = icdf.idz; idt = icdf.idt
    doX = False;    doY = False;    doZ = False;    doT = False

    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      dlist = icdf.dimids[ind]
      nd = len(dlist) - dlist.count(-1)
      dlist[nd:] = []
      idim = 0
      icdf.ny = 1
      icdf.nz = 1
      icdf.nt = 1
      self.strny.set(str(icdf.ny))
      self.strnz.set(str(icdf.nz))
      self.strnt.set(str(icdf.nt))
      try:
        axes_list = ncid.variables[value_selected].getncattr('coordinates').split(' ')
        for aname in axes_list:
          kk = icdf.VAR_LIST.index(aname)
          if icdf.VAR_AXIS[kk] == 'X':
            icdf.idx   = kk
            icdf.withX = True
            icdf.xname = aname
            self.Xname.set(aname)
            avar = ncid.variables[aname]
            ndms = len(avar.dimensions)
            if ndms == 1:
              doX = True
              icdf.iname = avar.dimensions[0]
              icdf.idi = icdf.DIM_LIST.index(cdf.iname)
              icdf.DIM_AXIS[icdf.idi] = 'X'
              icdf.nx = ncid.dimensions[cdf.iname].size
              self.Iname.set(icdf.iname)
              self.strnx.set(str(icdf.nx))
              icdf.grid2d = False
            else:
              doY = True
              doX = True
              icdf.jname = avar.dimensions[0]
              icdf.idj = icdf.DIM_LIST.index(icdf.jname)
              icdf.DIM_AXIS[icdf.idj] = 'Y'
              icdf.ny = ncid.dimensions[icdf.jname].size
              icdf.iname = avar.dimensions[1]
              icdf.idi = icdf.DIM_LIST.index(icdf.iname)
              icdf.DIM_AXIS[icdf.idi] = 'X'
              icdf.nx = ncid.dimensions[icdf.iname].size
              self.Jname.set(icdf.jname)
              self.strny.set(str(icdf.ny))
              self.Iname.set(icdf.iname)
              self.strnx.set(str(icdf.nx))
              icdf.grid2d = True
          if icdf.VAR_AXIS[kk] == 'Y':
            icdf.idy   = kk
            icdf.withY = True
            icdf.yname = aname
            self.Yname.set(aname)
            avar = ncid.variables[aname]
            ndms = len(avar.dimensions)
            if ndms == 1:
              doY = True
              icdf.jname = avar.dimensions[0]
              icdf.idj = icdf.DIM_LIST.index(icdf.jname)
              icdf.DIM_AXIS[icdf.idj] = 'Y'
              icdf.ny = ncid.dimensions[icdf.jname].size
              self.Jname.set(icdf.jname)
              self.strny.set(str(icdf.ny))
              icdf.grid2d = False
            else:
              doY = True
              doX = True
              icdf.jname = avar.dimensions[0]
              icdf.idj = icdf.DIM_LIST.index(icdf.jname)
              icdf.DIM_AXIS[icdf.idj] = 'Y'
              icdf.ny = ncid.dimensions[icdf.jname].size
              icdf.iname = avar.dimensions[1]
              icdf.idi = icdf.DIM_LIST.index(icdf.iname)
              icdf.DIM_AXIS[icdf.idi] = 'X'
              icdf.nx = ncid.dimensions[icdf.iname].size
              self.Jname.set(icdf.jname)
              self.strny.set(str(icdf.ny))
              self.Iname.set(icdf.iname)
              self.strnx.set(str(icdf.nx))
              icdf.grid2d = True
          if icdf.VAR_AXIS[kk] == 'Z':
            icdf.idz   = kk
            icdf.withZ = True
            icdf.zname = aname
            self.Zname.set(aname)
            avar = ncid.variables[aname]
            doZ = True
            icdf.kname = avar.dimensions[0]
            icdf.idk = icdf.DIM_LIST.index(icdf.kname)
            icdf.DIM_AXIS[icdf.idk] = 'Z'
            icdf.nz = ncid.dimensions[icdf.kname].size
            self.Kname.set(icdf.kname)
            self.strnz.set(str(icdf.nz))
          if icdf.VAR_AXIS[kk] == 'T':
            icdf.idt   = kk
            icdf.withT = True
            icdf.tname = aname
            self.Tname.set(aname)
            avar = ncid.variables[aname]
            doT = True
            icdf.lname = avar.dimensions[0]
            icdf.idl = icdf.DIM_LIST.index(icdf.lname)
            icdf.DIM_AXIS[icdf.idl] = 'T'
            icdf.nt = ncid.dimensions[icdf.lname].size
            self.Lname.set(icdf.lname)
            self.strnt.set(str(icdf.nt))
              
      except:
        # No axis information is specified in the netcdf file
        # The onyl information we have are the dimensions
        # 
        #print('retrieving information of dimensions. Axe variables must be set by hand ...')
        for kk in dlist:
          if icdf.DIM_AXIS[kk] == 'X':
            doX        = True
            icdf.idi   = kk
            icdf.nx    = icdf.dlen[kk]
            icdf.iname = icdf.DIM_LIST[kk]
            self.Iname.set(icdf.iname)
            self.strnx.set(str(icdf.nx))
          if icdf.DIM_AXIS[kk] == 'Y':
            doY        = True
            icdf.idj   = kk
            icdf.ny    = icdf.dlen[kk]
            icdf.jname = icdf.DIM_LIST[kk]
            self.Jname.set(icdf.jname)
            self.strny.set(str(icdf.ny))
          if icdf.DIM_AXIS[kk] == 'Z':
            doZ        = True
            icdf.idk   = kk
            icdf.nz    = icdf.dlen[kk]
            icdf.kname = icdf.DIM_LIST[kk]
            self.Kname.set(icdf.kname)
            self.strnz.set(str(icdf.nz))
          if icdf.DIM_AXIS[kk] == 'T':
            doT        = True
            icdf.idl   = kk
            icdf.nt    = icdf.dlen[kk]
            icdf.lname = icdf.DIM_LIST[kk]
            self.Lname.set(icdf.lname)
            self.strnt.set(str(icdf.nt))

      if not doX:
        icdf.nx = 1
        self.strnx.set(str(icdf.nx))
        icdf.idi = -1
        icdf.idx = -1
        self.Xname.set(icdf.VAR_MENU[icdf.idx])
        self.Iname.set(icdf.DIM_MENU[icdf.idi])
        icdf.withX = False

      if not doY:
        icdf.ny = 1
        self.strny.set(str(icdf.ny))
        icdf.idj = -1
        icdf.idy = -1
        self.Yname.set(icdf.VAR_MENU[icdf.idy])
        self.Jname.set(icdf.DIM_MENU[icdf.idj])
        icdf.withY = False
      if not doZ:
        icdf.nz = 1
        self.strnz.set(str(icdf.nz))
        icdf.idk = -1
        icdf.idz = -1
        self.Zname.set(icdf.VAR_MENU[icdf.idz])
        self.Kname.set(icdf.DIM_MENU[icdf.idk])
        icdf.withZ = False
      if not doT:
        icdf.nt = 1
        self.strnt.set(str(icdf.nt))
        icdf.idl = -1
        icdf.idt = -1
        self.Tname.set(icdf.VAR_MENU[icdf.idt])
        self.Lname.set(icdf.DIM_MENU[icdf.idl])
        icdf.withT = False

      if icdf.withX and icdf.withY:
        icdf.georef = True
      else:
        icdf.georef = False
      self.georef.set(icdf.georef)


def marker_string(s):
# ===================
  if len(s) > 1:
    return r'$'+s+'$'

  if ord(s) > 255:
    return u'$'+s+'$'
    #return u''+s+''
  else:
    return s

def caldat(nn):
# =============

    from math import floor
    from math import fmod

    if np.isnan(nn):
      return (np.nan,np.nan,np.nan,np.nan,np.nan,np.nan)

    n = floor(nn)
    isecs = round((nn - n)*86400,0)

    a = n + 32044
    b = (4*a + 3)//146097
    c = a - (146097*b)//4
    d = (4*c + 3)//1461
    e = c - (1461*d)//4
    m = (5*e + 2)//153

    #ret = Date()
    day = e + 1 - (153*m + 2)//5
    month = m + 3 - 12*(m//10)
    year = 100*b + d - 4800 + m/10

    second = fmod(isecs,60)
    isecs = (isecs-second)/60
    minute = fmod(isecs,60)
    hour   = (isecs-minute)/60

    return (int(year),int(month),int(day),int(hour),int(minute),int(second))

def julday(date):
# ===============
  """
  1. Get current values for year, month, and day
  2. Same for time and make it a day fraction
  3. Calculate the julian day number according to numerical recipes
  4. Add the day fraction to the julian day number
  """

  IGREG = 15+31*(10+12*1582)

  year = date.year
  month = date.month
  day = date.day

  #fraction = (date.hour*3600 + date.minute*60 + date.second)/86400

  jy = year
  if month > 2:
    jm = month + 1
  else:
    jy = jy - 1
    jm = month + 13

  jd = math.floor(365.25*jy) + math.floor(30.6001*jm) + day + 1720995
  if (day+31*(month+12*year) > IGREG):
    ja = math.floor(0.01*jy)
    jd = jd + 2 - ja + math.floor(0.25*ja)

  return jd

# =====================
class Select_Columns():
# =====================
  '''Widget to select the column content of colum table'''

  __version__ = "0.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "February 2018"

  def __init__ (self,master,line,sep):

    master.protocol('WM_DELETE_WINDOW',self.cancel)
    self.master = master
    self.LINE = tk.StringVar()
    self.SEPARATOR = tk.StringVar()

    line = line.strip()
    self.LINE.set(line)
    self.SEPARATOR.set(sep)
    self.columns = line.split(self.SEPARATOR.get())

    self.ncol = len(self.columns)
    self.clist = list(range(self.ncol))

    # Add an exra option to signal that the variable is not present
    self.columns.append(' ')
    self.clist.append(-1)

    self.LON    = tk.IntVar()
    self.LAT    = tk.IntVar()
    self.DEPTH  = tk.IntVar()
    self.YEAR   = tk.IntVar()
    self.MONTH  = tk.IntVar()
    self.DAY    = tk.IntVar()
    self.HOUR   = tk.IntVar()
    self.MINUTE = tk.IntVar()
    self.SECOND = tk.IntVar()
    self.DATE   = tk.IntVar()
    self.TIME   = tk.IntVar()
    self.FMT    = tk.StringVar()
    self.DFMT   = tk.StringVar()
    self.TFMT   = tk.StringVar()

    self.LON.set(0)
    self.LAT.set(1)
    self.DEPTH.set(-1)
    self.YEAR.set(-1)
    self.MONTH.set(-1)
    self.DAY.set(-1)
    self.HOUR.set(-1)
    self.MINUTE.set(-1)
    self.SECOND.set(-1)
    self.DATE.set(-1)
    self.TIME.set(-1)
    self.FMT.set('%Y-%m-%dT%H:%M:%S')
    self.DFMT.set('%Y-%m-%d')
    self.TFMT.set('%H:%M:%S')

    FM = ttk.Frame(self.master)

    ttk.Label(FM,text='First line').grid(row=0,column=0,padx=3,pady=5)
    ttk.Entry(FM,textvariable=self.LINE,width=50,state='readonly') \
       .grid(row=0,column=1,columnspan=8,padx=3,pady=5,sticky='w')
    ttk.Label(FM,text='Separator').grid(row=1,column=0,padx=3,pady=5)
    self.sep = ttk.Entry(FM,textvariable=self.SEPARATOR,width=5)
    self.sep.grid(row=1,column=1,padx=3,pady=5,sticky='w')
    self.sep.bind('<Return>',lambda e: self.recount())
    tk.Label(FM,text='Num col :').grid(row=1,column=2,padx=3,pady=5)
    self.wcol = ttk.Label(FM,text='%s' % self.ncol)
    self.wcol.grid(row=1,column=3,padx=3,pady=5,sticky='w')

    ttk.Label(FM,text='Select the float column information') \
       .grid(row=2,column=0,columnspan=3)

    #self.nb = ttk.Notebook(self.master)
    self.nb = ttk.Notebook(FM)
    self.page1 = ttk.Frame(self.nb)
    self.page2 = ttk.Frame(self.nb)
    self.page3 = ttk.Frame(self.nb)
    self.nb.add(self.page1,text='Date in different columns')
    self.nb.add(self.page2,text='Date in ISO 8601 format')
    self.nb.add(self.page3,text='Date and time')
    self.nb.grid(row=2,column=0,columnspan=5)


    #F0 = ttk.Frame(self.master,padding=5)
    F0 = ttk.Frame(self.page1,padding=5)
    ttk.Label(F0,text='Variable').grid(row=1,column=0,padx=3)
    ttk.Label(F0,text='Longitude').grid(row=1,column=1,padx=3)
    ttk.Label(F0,text='Latitude').grid(row=1,column=2,padx=3)
    ttk.Label(F0,text='Depth').grid(row=1,column=3,padx=3)
    ttk.Label(F0,text='Year').grid(row=1,column=4,padx=3)
    ttk.Label(F0,text='Month').grid(row=1,column=5,padx=3)
    ttk.Label(F0,text='Day').grid(row=1,column=6,padx=3)
    ttk.Label(F0,text='Hour').grid(row=1,column=7,padx=3)
    ttk.Label(F0,text='Minute').grid(row=1,column=8,padx=3)
    ttk.Label(F0,text='Second').grid(row=1,column=9,padx=3)

    self.clon = ttk.Combobox(F0,textvariable=self.LON,values=self.clist,width=10) 
    self.clon.grid(row=2,column=1,padx=3,pady=5)
    self.clon.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.clat = ttk.Combobox(F0,textvariable=self.LAT,values=self.clist,width=10) 
    self.clat.grid(row=2,column=2,padx=3,pady=5)
    self.clat.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.cdep = ttk.Combobox(F0,textvariable=self.DEPTH,values=self.clist,width=10)
    self.cdep.grid(row=2,column=3,padx=3,pady=5)
    self.cdep.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.cyea = ttk.Combobox(F0,textvariable=self.YEAR,values=self.clist,width=10)
    self.cyea.grid(row=2,column=4,padx=3,pady=5)
    self.cyea.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.cmon = ttk.Combobox(F0,textvariable=self.MONTH,values=self.clist,width=10)
    self.cmon.grid(row=2,column=5,padx=3,pady=5)
    self.cmon.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.cday = ttk.Combobox(F0,textvariable=self.DAY,values=self.clist,width=10)
    self.cday.grid(row=2,column=6,padx=3,pady=5)
    self.cday.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.chou = ttk.Combobox(F0,textvariable=self.HOUR,values=self.clist,width=10)
    self.chou.grid(row=2,column=7,padx=3,pady=5)
    self.chou.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.cmin = ttk.Combobox(F0,textvariable=self.MINUTE,values=self.clist,width=10)
    self.cmin.grid(row=2,column=8,padx=3,pady=5)
    self.cmin.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    self.csec = ttk.Combobox(F0,textvariable=self.SECOND,values=self.clist,width=10)
    self.csec.grid(row=2,column=9,padx=3,pady=5)
    self.csec.bind('<<ComboboxSelected>>',lambda e: self.relabel())

    ttk.Label(F0,text='Value').grid(row=3,column=0,padx=3)
    self.wlon = ttk.Label(F0,text=self.columns[self.LON.get()],width=10)
    self.wlon.grid(row=3,column=1,padx=3,pady=5)
    self.wlat = ttk.Label(F0,text=self.columns[self.LAT.get()],width=10)
    self.wlat.grid(row=3,column=2,padx=3,pady=5)
    self.wdep = ttk.Label(F0,text=self.columns[self.DEPTH.get()],width=10)
    self.wdep.grid(row=3,column=3,padx=3,pady=5)
    self.wyea = ttk.Label(F0,text=self.columns[self.YEAR.get()],width=10)
    self.wyea.grid(row=3,column=4,padx=3,pady=5)
    self.wmon = ttk.Label(F0,text=self.columns[self.MONTH.get()],width=10)
    self.wmon.grid(row=3,column=5,padx=3,pady=5)
    self.wday = ttk.Label(F0,text=self.columns[self.DAY.get()],width=10)
    self.wday.grid(row=3,column=6,padx=3,pady=5)
    self.whou = ttk.Label(F0,text=self.columns[self.HOUR.get()],width=10)
    self.whou.grid(row=3,column=7,padx=3,pady=5)
    self.wmin = ttk.Label(F0,text=self.columns[self.MINUTE.get()],width=10)
    self.wmin.grid(row=3,column=8,padx=3,pady=5)
    self.wsec = ttk.Label(F0,text=self.columns[self.SECOND.get()],width=10)
    self.wsec.grid(row=3,column=9,padx=3,pady=5)

    ttk.Button(F0,text='Cancel',command=self.cancel).grid(row=5,column=7,padx=3,pady=5)
    ttk.Button(F0,text='Done',command=self.done0).grid(row=5,column=8,padx=3,pady=5)

    F0.grid()

    F1 = ttk.Frame(self.page2,padding=5)
    ttk.Label(F1,text='Variable').grid(row=1,column=0,padx=3)
    ttk.Label(F1,text='Longitude').grid(row=1,column=1,padx=3)
    ttk.Label(F1,text='Latitude').grid(row=1,column=2,padx=3)
    ttk.Label(F1,text='Depth').grid(row=1,column=3,padx=3)
    ttk.Label(F1,text='Date').grid(row=1,column=4,padx=3,sticky='w')

    self.clon1 = ttk.Combobox(F1,textvariable=self.LON,values=self.clist,width=10) 
    self.clon1.grid(row=2,column=1,padx=3,pady=5)
    self.clon1.bind('<<ComboboxSelected>>',lambda e: self.relabel1())

    self.clat1 = ttk.Combobox(F1,textvariable=self.LAT,values=self.clist,width=10) 
    self.clat1.grid(row=2,column=2,padx=3,pady=5)
    self.clat1.bind('<<ComboboxSelected>>',lambda e: self.relabel1())

    self.cdep1 = ttk.Combobox(F1,textvariable=self.DEPTH,values=self.clist,width=10)
    self.cdep1.grid(row=2,column=3,padx=3,pady=5)
    self.cdep1.bind('<<ComboboxSelected>>',lambda e: self.relabel1())

    self.cdat1 = ttk.Combobox(F1,textvariable=self.DATE,values=self.clist,width=10)
    self.cdat1.grid(row=2,column=4,padx=3,pady=5,sticky='w')
    self.cdat1.bind('<<ComboboxSelected>>',lambda e: self.relabel1())

    ttk.Label(F1,text='Value').grid(row=3,column=0,padx=3)
    self.wlon1 = ttk.Label(F1,text=self.columns[self.LON.get()],width=10)
    self.wlon1.grid(row=3,column=1,padx=3,pady=5)
    self.wlat1 = ttk.Label(F1,text=self.columns[self.LAT.get()],width=10)
    self.wlat1.grid(row=3,column=2,padx=3,pady=5)
    self.wdep1 = ttk.Label(F1,text=self.columns[self.DEPTH.get()],width=10)
    self.wdep1.grid(row=3,column=3,padx=3,pady=5)
    self.wdat1 = ttk.Label(F1,text=self.columns[self.DATE.get()],width=20)
    self.wdat1.grid(row=3,column=4,columnspan=2,padx=3,pady=5,sticky='w')

    ttk.Label(F1,text='Format').grid(row=4,column=0,padx=3,pady=5)
    self.wfmt1 = ttk.Entry(F1,text=self.FMT,width=30)
    self.wfmt1.grid(row=4,column=4,columnspan=3,padx=3,pady=5,sticky='w')

    ttk.Button(F1,text='Cancel',command=self.cancel).grid(row=5,column=5,padx=3,pady=5)
    ttk.Button(F1,text='Done',command=self.done1).grid(row=5,column=6,padx=3,pady=5)
    F1.grid()

    F2 = ttk.Frame(self.page3,padding=5)
    ttk.Label(F2,text='Variable').grid(row=1,column=0,padx=3)
    ttk.Label(F2,text='Longitude').grid(row=1,column=1,padx=3)
    ttk.Label(F2,text='Latitude').grid(row=1,column=2,padx=3)
    ttk.Label(F2,text='Depth').grid(row=1,column=3,padx=3)
    ttk.Label(F2,text='Date').grid(row=1,column=4,padx=3)
    ttk.Label(F2,text='Time').grid(row=1,column=5,padx=3)

    self.clon2 = ttk.Combobox(F2,textvariable=self.LON,values=self.clist,width=10) 
    self.clon2.grid(row=2,column=1,padx=3,pady=5)
    self.clon2.bind('<<ComboboxSelected>>',lambda e: self.relabel2())

    self.clat2 = ttk.Combobox(F2,textvariable=self.LAT,values=self.clist,width=10) 
    self.clat2.grid(row=2,column=2,padx=3,pady=5)
    self.clat2.bind('<<ComboboxSelected>>',lambda e: self.relabel2())

    self.cdep2 = ttk.Combobox(F2,textvariable=self.DEPTH,values=self.clist,width=10)
    self.cdep2.grid(row=2,column=3,padx=3,pady=5)
    self.cdep2.bind('<<ComboboxSelected>>',lambda e: self.relabel2())

    self.cdat2 = ttk.Combobox(F2,textvariable=self.DATE,values=self.clist,width=10)
    self.cdat2.grid(row=2,column=4,padx=3,pady=5,sticky='w')
    self.cdat2.bind('<<ComboboxSelected>>',lambda e: self.relabel2())

    self.ctim2 = ttk.Combobox(F2,textvariable=self.TIME,values=self.clist,width=10)
    self.ctim2.grid(row=2,column=5,padx=3,pady=5,sticky='w')
    self.ctim2.bind('<<ComboboxSelected>>',lambda e: self.relabel2())

    ttk.Label(F2,text='Value').grid(row=3,column=0,padx=3)
    self.wlon2 = ttk.Label(F2,text=self.columns[self.LON.get()],width=10)
    self.wlon2.grid(row=3,column=1,padx=3,pady=5)
    self.wlat2 = ttk.Label(F2,text=self.columns[self.LAT.get()],width=10)
    self.wlat2.grid(row=3,column=2,padx=3,pady=5)
    self.wdep2 = ttk.Label(F2,text=self.columns[self.DEPTH.get()],width=10)
    self.wdep2.grid(row=3,column=3,padx=3,pady=5)
    self.wdat2 = ttk.Label(F2,text=self.columns[self.DATE.get()],width=10)
    self.wdat2.grid(row=3,column=4,padx=3,pady=5)
    self.wtim2 = ttk.Label(F2,text=self.columns[self.TIME.get()],width=10)
    self.wtim2.grid(row=3,column=5,padx=3,pady=5)

    ttk.Label(F2,text='Format').grid(row=4,column=0,padx=3,pady=5)
    self.wdfmt = ttk.Entry(F2,text=self.DFMT,width=11)
    self.wdfmt.grid(row=4,column=4,padx=3,pady=5,sticky='w')
    self.wtfmt = ttk.Entry(F2,text=self.TFMT,width=11)
    self.wtfmt.grid(row=4,column=5,padx=3,pady=5,sticky='w')

    ttk.Button(F2,text='Cancel',command=self.cancel).grid(row=5,column=5,padx=3,pady=5)
    ttk.Button(F2,text='Done',command=self.done2).grid(row=5,column=6,padx=3,pady=5)
    F2.grid()


    FM.grid()


    self.lon = 1

  def recount(self):
  # ================
    line = self.LINE.get()
    line = line.strip()
    self.columns = line.split(self.SEPARATOR.get())

    self.ncol = len(self.columns)
    self.clist = list(range(self.ncol))
    self.columns.append(' ')
    self.clist.append(-1)

    self.wcol['text']   = '%s' % self.ncol
    self.clon['values'] = self.clist
    self.clat['values'] = self.clist
    self.cdep['values'] = self.clist
    self.cyea['values'] = self.clist
    self.cmon['values'] = self.clist
    self.cday['values'] = self.clist
    self.chou['values'] = self.clist
    self.cmin['values'] = self.clist
    self.csec['values'] = self.clist

    self.clon1['values'] = self.clist
    self.clat1['values'] = self.clist
    self.cdep1['values'] = self.clist
    self.cdat1['values'] = self.clist

    self.clon2['values'] = self.clist
    self.clat2['values'] = self.clist
    self.cdep2['values'] = self.clist
    self.ctim2['values'] = self.clist

  def cancel(self):
  # ================
    self.lon    = None
    self.lat    = None
    self.depth  = None
    self.year   = None
    self.month  = None
    self.day    = None
    self.hour   = None
    self.minute = None
    self.second = None
    self.date   = None
    self.time   = None
    self.fmt    = None

    self.master.destroy()
    return

  def done0(self):
  # ================
    self.type   = 0
    self.lon    = None
    self.lat    = None
    self.depth  = None
    self.year   = None
    self.month  = None
    self.day    = None
    self.hour   = None
    self.minute = None
    self.second = None
    self.date   = None
    self.time   = None
    self.fmt    = None

    if self.LON.get() > -1:
      self.lon = self.LON.get()
    if self.LAT.get() > -1:
      self.lat = self.LAT.get()
    if self.DEPTH.get() > -1:
      self.depth = self.DEPTH.get()
    if self.YEAR.get() > -1:
      self.year = self.YEAR.get()
    if self.MONTH.get() > -1:
      self.month = self.MONTH.get()
    if self.DAY.get() > -1:
      self.day = self.DAY.get()
    if self.HOUR.get() > -1:
      self.hour = self.HOUR.get()
    if self.MINUTE.get() > -1:
      self.minute = self.MINUTE.get()
    if self.SECOND.get() > -1:
      self.second = self.SECOND.get()

    self.master.destroy()
    return

  def done1(self):
  # ================
    self.type   = 1
    self.lon    = None
    self.lat    = None
    self.depth  = None
    self.year   = None
    self.month  = None
    self.day    = None
    self.hour   = None
    self.minute = None
    self.second = None
    self.date   = None
    self.time   = None
    self.fmt    = None

    if self.LON.get() > -1:
      self.lon = self.LON.get()
    if self.LAT.get() > -1:
      self.lat = self.LAT.get()
    if self.DEPTH.get() > -1:
      self.depth = self.DEPTH.get()
    if self.DATE.get() > -1:
      self.date = self.DATE.get()
    self.fmt = self.FMT.get()

    self.master.destroy()
    return

  def done2(self):
  # ================
    self.type   = 2
    self.lon    = None
    self.lat    = None
    self.depth  = None
    self.year   = None
    self.month  = None
    self.day    = None
    self.hour   = None
    self.minute = None
    self.second = None
    self.date   = None
    self.time   = None
    self.fmt    = None

    if self.LON.get() > -1:
      self.lon = self.LON.get()
    if self.LAT.get() > -1:
      self.lat = self.LAT.get()
    if self.DEPTH.get() > -1:
      self.depth = self.DEPTH.get()
    if self.DATE.get() > -1:
      self.date = self.DATE.get()
    if self.TIME.get() > -1:
      self.time = self.TIME.get()
    self.fmt = self.DFMT.get()+'T'+self.TFMT.get()

    self.master.destroy()
    return

  def relabel(self):
  # ================
    self.wlon['text'] = self.columns[self.LON.get()]
    self.wlat['text'] = self.columns[self.LAT.get()]
    self.wdep['text'] = self.columns[self.DEPTH.get()]
    self.wyea['text'] = self.columns[self.YEAR.get()]
    self.wmon['text'] = self.columns[self.MONTH.get()]
    self.wday['text'] = self.columns[self.DAY.get()]
    self.whou['text'] = self.columns[self.HOUR.get()]
    self.wmin['text'] = self.columns[self.MINUTE.get()]
    self.wsec['text'] = self.columns[self.SECOND.get()]

  def relabel1(self):
  # ================
    self.wlon1['text'] = self.columns[self.LON.get()]
    self.wlat1['text'] = self.columns[self.LAT.get()]
    self.wdep1['text'] = self.columns[self.DEPTH.get()]
    self.wdat1['text'] = self.columns[self.DATE.get()]

  def relabel2(self):
  # ================
    self.wlon2['text'] = self.columns[self.LON.get()]
    self.wlat2['text'] = self.columns[self.LAT.get()]
    self.wdep2['text'] = self.columns[self.DEPTH.get()]
    self.wdat2['text'] = self.columns[self.DATE.get()]
    self.wtim2['text'] = self.columns[self.TIME.get()]

# ================================
#class Win_permission(tk.Toplevel):
# ================================
#  def __init__(self,parent,question,nowarning):
#    BGC = 'orange red'
#    BTC = 'red'
#    tk.Toplevel.__init__(self,parent)
#    self.configure(bg=BGC)
#    self.title('Warning')
#    self.lift()

# ==========================
class get_Date():
# ==========================
  '''Widget to select the Date and Time'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2018"

  def __init__ (self):
  # =========================

    self.master = tk.Toplevel(None)

    #tk.Toplevel.__init__(self)
    self.master.protocol('WM_DELETE_WINDOW',self.cancel)
    self.master.title('Select Date and Time')
    self.master.lift()

    self.YEAR   = tk.IntVar()
    self.MONTH  = tk.IntVar()
    self.DAY    = tk.IntVar()
    self.DAYY   = tk.IntVar()
    self.JDAY   = tk.IntVar()
    self.HOUR   = tk.IntVar()
    self.MINUTE = tk.IntVar()
    self.SECOND = tk.IntVar()
    self.date   = None

    TODAY = datetime.datetime.now().date()
    self.YEAR.set(TODAY.year)
    self.MONTH.set(TODAY.month)
    self.DAY.set(TODAY.day)
    self.DAYY.set(int(TODAY.strftime('%j')))
    self.JDAY.set(julday(TODAY))
    self.HOUR.set(0)
    self.MINUTE.set(0)
    self.SECOND.set(0)

    font_bold = tkfont.Font(font='TkDefaultFont').copy()
    font_bold['weight']='bold'

    FD = ttk.Frame(self.master,relief='ridge',borderwidth=3)
    ttk.Label(FD,text='Date',font=font_bold).grid(row=0,column=0,padx=1,pady=5)
    ttk.Label(FD,text='Year',width=6).grid(row=1,column=0,sticky='we')
    ttk.Label(FD,text='Month',width=6).grid(row=1,column=1,sticky='we')
    ttk.Label(FD,text='Day',width=6).grid(row=1,column=2,sticky='we')
    ttk.Entry(FD,text=self.YEAR,width=6).grid(row=2,column=0,sticky='we')
    ttk.Entry(FD,text=self.MONTH,width=6).grid(row=2,column=1,sticky='we')
    ttk.Entry(FD,text=self.DAY,width=6).grid(row=2,column=2,sticky='we')
    FD.grid(row=0,column=0,columnspan=3,padx=[5,1],pady=5,sticky='w')

    FT = ttk.Frame(self.master,relief='ridge',borderwidth=3)
    ttk.Label(FT,text='Time',font=font_bold).grid(row=0,column=0,padx=1,pady=5)
    ttk.Label(FT,text='Hour',width=6).grid(row=1,column=0,sticky='we')
    ttk.Label(FT,text='Minute',width=6).grid(row=1,column=1,sticky='we')
    ttk.Label(FT,text='Second',width=6).grid(row=1,column=2,sticky='we')
    ttk.Entry(FT,text=self.HOUR,width=6).grid(row=2,column=0,sticky='we')
    ttk.Entry(FT,text=self.MINUTE,width=6).grid(row=2,column=1,sticky='we')
    ttk.Entry(FT,text=self.SECOND,width=6).grid(row=2,column=2,sticky='we')
    FT.grid(row=0,column=3,columnspan=3,padx=[1,5],pady=5,sticky='w')

    ttk.Button(self.master,text='Done',command=self.done0).grid(row=0,column=6,padx=3,pady=5,sticky='w')

    FD1 = ttk.Frame(self.master,relief='ridge',borderwidth=3)
    ttk.Label(FD1,text='Date',font=font_bold).grid(row=0,column=0,padx=1,pady=5)
    ttk.Label(FD1,text='Year',
              width=6).grid(row=1,column=0,sticky='we')
    ttk.Label(FD1,text='Day of the year',
              width=12).grid(row=1,column=1,columnspan=2,sticky='we')
    ttk.Entry(FD1,text=self.YEAR,
              width=6).grid(row=2,column=0,sticky='we')
    ttk.Entry(FD1,text=self.DAYY,
              width=12).grid(row=2,column=1,columnspan=2,sticky='we')
    FD1.grid(row=1,column=0,columnspan=3,padx=[5,1],pady=5,sticky='w')

    ttk.Button(self.master,text='Done',command=self.done1).grid(row=1,column=3,padx=3,pady=5,sticky='w')

    FD2 = ttk.Frame(self.master,relief='ridge',borderwidth=3)
    ttk.Label(FD2,text='Date',font=font_bold).grid(row=0,column=0,padx=1,pady=5)
    ttk.Label(FD2,text='Astronomical Julian Day',
              width=20).grid(row=1,column=0,columnspan=3,sticky='we')
    ttk.Entry(FD2,text=self.JDAY,
              width=12).grid(row=2,column=0,sticky='we')
    FD2.grid(row=2,column=0,columnspan=3,padx=[5,1],pady=5,sticky='w')

    ttk.Button(self.master,text='Done',command=self.done2).grid(row=2,column=3,padx=3,pady=5,sticky='w')




    #ttk.Button(self.master,text='Cancel',command=self.cancel).grid(row=1,column=4,padx=3,pady=5)
    #ttk.Button(self.master,text='Done',command=self.done0).grid(row=1,column=5,padx=3,pady=5)

    self.master.wait_window()

  def cancel(self):
  # ================
    self.master.destroy()
    self.year = None
    self.month = None
    self.day = None
    self.hour = None
    self.minute = None
    self.second = None
    self.date = None
    return 

  def done0(self):
  # ================

    self.master.destroy()
    self.year = self.YEAR.get()
    self.month = self.MONTH.get()
    self.day = self.DAY.get()
    self.hour = self.HOUR.get()
    self.minute = self.MINUTE.get()
    self.second = self.SECOND.get()
    self.date = datetime.datetime(self.YEAR.get(),
                                  self.MONTH.get(),
                                  self.DAY.get(),
                                  self.HOUR.get(),
                                  self.MINUTE.get(),
                                  self.SECOND.get())
    return

  def done1(self):
  # ================

    self.master.destroy()
    self.date = datetime.datetime(self.YEAR.get(),1,1,
                                  self.HOUR.get(),
                                  self.MINUTE.get(),
                                  self.SECOND.get()) +  \
                datetime.timedelta(self.DAYY.get()-1)

    self.year = self.date.year
    self.month = self.date.month
    self.day = self.date.day
    self.hour = self.date.hour
    self.minute = self.date.minute
    self.second = self.date.second
    return

  def done2(self):
  # ================

    self.master.destroy()
    fraction = (self.HOUR.get()*3600 + \
                self.MINUTE.get()*60 + \
                self.SECOND.get())/86400
    self.year,self.month,self.day,self.hour,self.minute,self.second = \
                caldat(self.JDAY.get() + fraction)
    self.date = datetime.datetime(self.year,
                                  self.month,
                                  self.day,
                                  self.hour,
                                  self.minute,
                                  self.second)
    return

def folderList(path,ext=''):
# ==========================
  ''' Gets a list with names of files in a local folder'''

  a = listdir(path)
  onlyfiles = [f for f in a if isfile(join(path,f))]

  if empty(ext):
    return onlyfiles

  filelist = [f for f in onlyfiles if f.lower().endswith(ext)]
  return filelist

def urlList(url,ext='',params={}):
# ================================
  ''' Gets a list with names of files in a remote folder'''

  response = requests.get(url, params=params)
  if response.ok:
    response_text = response.text
  else:
    return response.raise_for_status()
  soup = BeautifulSoup(response_text, 'html.parser')
  parent = [url + node.get('href') for node in soup.find_all('a') if node.get('href').endswith(ext)]
  return parent

  a = listdir(path)
  onlyfiles = [f for f in a if isfile(join(path,f))]

  if empty(ext):
    return onlyfiles

  filelist = [f for f in onlyfiles if f.lower().endswith(ext)]
  return filelist

def haversine(point1,point2):
# ===========================
  ''' Calculate the great circle distance between two points
       on the earth given as couples of decimal degrees (LON, LAT)'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "December 2017"
  from math import radians, cos, sin, asin, sqrt

  AVG_EARTH_RADIUS = 6371  # in km

  # unpack latitude/longitude
  lng1, lat1 = point1
  lng2, lat2 = point2

  # convert all latitudes/longitudes from decimal degrees to radians
  lat1, lng1, lat2, lng2 = map(radians, (lat1, lng1, lat2, lng2))

  # calculate haversine
  lat = lat2 - lat1
  lng = lng2 - lng1
  d = sin(lat * 0.5) ** 2 + cos(lat1) * cos(lat2) * sin(lng * 0.5) ** 2
  h = 2 * AVG_EARTH_RADIUS * asin(sqrt(d))
  return h * 1000  # in meters

def initial_bearing(point1,point2):
# ===================================
  ''' Calculate the initial bearing between two points
       on the earth given as couples of decimal degrees (LON, LAT)

    The formulae used is the following:
        θ = atan2(sin(Δlong).cos(lat2),
                  cos(lat1).sin(lat2) − sin(lat1).cos(lat2).cos(Δlong))
    :Parameters:
      - point1: The tuple representing the (LON,LAT) for the first point.
      - point2: The tuple representing the (LON,LAT) for the second point.
                Longitudes and latitudes must be in decimal degrees.
    :Returns:
      The bearing in degrees
    :Returns Type:
      float
    '''
  if (type(point1) != tuple) or (type(point2) != tuple):
      raise TypeError("Only tuples are supported as arguments")

  lat1 = math.radians(point1[1])
  lat2 = math.radians(point2[1])

  diffLong = math.radians(point2[0] - point1[0])

  x = math.sin(diffLong) * math.cos(lat2)
  y = math.cos(lat1) * math.sin(lat2) - (math.sin(lat1)
            * math.cos(lat2) * math.cos(diffLong))

  initial_bearing = math.atan2(x, y)

  # Now we have the initial bearing but math.atan2 return values
  # from -180° to + 180° which is not what we want for a compass bearing
  # The solution is to normalize the initial bearing as shown below
  initial_bearing = math.degrees(initial_bearing)
  compass_bearing = (initial_bearing + 360) % 360

  return compass_bearing

def angle_diff (a,b):
# ===================
  ''' Calculates the smallest difference between angles a and b'''

  dif = a - b
  dif = (dif + 180) % 360 - 180
  return dif


def simple_form(Title,Label,value=''):
# =======================================

  ENTRY = tk.StringVar()
  ENTRY.set(value)

  def _close():
  # ===========
    ENTRY.set('')
    win.destroy()

  def _done():
  # ===========
    win.destroy()

  win = tk.Toplevel()
  win.title(Title)
  win.resizable(False,False)
  win.protocol('WM_DELETE_WINDOW',_close)
  F0 = ttk.Frame(win,padding=5)
  ttk.Label(F0,text=Label,padding=5).grid(row=0,column=0,sticky='w')
  ttk.Entry(F0,textvariable=ENTRY,justify='left',width=80). \
      grid(row=0,column=1,columnspan=8,sticky='ew')
  ttk.Button(F0,text='Cancel',command=_close).grid(row=1,column=7)
  ttk.Button(F0,text='Done',command=_done).grid(row=1,column=8)
  F0.grid()
  win.wait_window(win)
  win.destroy()
  return ENTRY.get()

def json_save(conf,FILENAME):
# ============================
  ''' Save a dictionary on a JSON file'''

  with io.open(FILENAME,'w',encoding='utf8') as outfile:
    _str = json.dumps(conf,ensure_ascii=False,
                           sort_keys=False,
                           indent=2,
                           separators=(',',': '))
    outfile.write(to_unicode(_str))
    outfile.close()

def fontconfig(font=None,sample=None):
# ====================================
  '''Widget dialogue to config a Matplotlib FontPorperty object'''

  global myfont
  global canvas
  global ax

  if font is None:
    myfont = FontProperties().copy()
  else:
    myfont = font.copy()
  backup = myfont.copy()

  if sample is None:
    sample = 'Matplotlib Font sample'

  def _cancel():
  # ==========
    global myfont
    myfont = backup.copy()
    window.destroy()

  def _done():
  # ==========
    global myfont
    myfont.set_family(family.get())
    myfont.set_style(style.get())
    myfont.set_variant(variant.get())
    try:
      myfont.set_stretch(int(stretch.get()))
    except:
      myfont.set_stretch(stretch.get())
    try:
      myfont.set_weight(int(weight.get()))
    except:
      myfont.set_weight(weight.get())

    values=['xx-small',
            'x-small',
            'small',
            'medium',
            'large',
            'x-large',
            'xx-large']
    try:
      ii = values.index(size.get())
      ischar = True
    except:
      ischar = False

    if ischar:
      myfont.set_size(size.get())
    else:
      myfont.set_size(int(size.get()))

    window.destroy()

  def _update():
  # ==========
    global myfont
    global canvas
    global ax
    myfont.set_family(family.get())
    myfont.set_style(style.get())
    myfont.set_variant(variant.get())
    try:
      myfont.set_stretch(int(stretch.get()))
    except:
      myfont.set_stretch(stretch.get())
    try:
      myfont.set_weight(int(weight.get()))
    except:
      myfont.set_weight(weight.get())
    try:
      myfont.set_size(int(size.get()))
    except:
      myfont.set_size(size.get())

    values=['xx-small',
            'x-small',
            'small',
            'medium',
            'large',
            'x-large',
            'xx-large']
    try:
      ii = values.index(size.get())
      ischar = True
    except:
      ischar = False

    if ischar:
      myfont.set_size(size.get())
    else:
      myfont.set_size(int(size.get()))

    ax.clear()
    align = {'horizontalalignment': 'center', 'verticalalignment': 'baseline'}
    ax.text(1,1,sample,fontproperties=myfont,**align)
    ax.set_xlim(0,2)
    ax.set_ylim(0,2)
    ax.get_xaxis().set_visible(False)
    ax.get_yaxis().set_visible(False)
    canvas.draw()

  family = tk.StringVar() 
  style = tk.StringVar() 
  variant = tk.StringVar() 
  stretch = tk.StringVar() 
  weight = tk.StringVar() 
  size = tk.StringVar() 
 
  family.set(myfont.get_family()[0]) 
  style.set(myfont.get_style()) 
  variant.set(myfont.get_variant()) 
  stretch.set(myfont.get_stretch()) 
  weight.set(myfont.get_weight()) 
  try:
    size.set(int(myfont.get_size()))
  except:
    size.set(myfont.get_size())

  window = tk.Toplevel()
  window.title('Matplotlib Font Manager Font Properties')
  frame1 = ttk.Frame(window,padding=15)

  ttk.Label(frame1,text='Family').grid(row=0,column=0,sticky='w',padx=3)
  wfam = ttk.Combobox(frame1,
                      textvariable=family,
                      values=['serif',
                              'sans-serif',
                              'fantasy',
                              'monospace'],
                      width=10)
  wfam.grid(row=0,column=1,sticky='w',padx=3)
  wfam.bind('<<ComboboxSelected>>',lambda e: _update())

  ttk.Label(frame1,text='Style').grid(row=1,column=0,sticky='w',padx=3)
  wsty = ttk.Combobox(frame1,
                      textvariable=style,
                      values=['normal','italic','oblique'],
                      width=10)
  wsty.grid(row=1,column=1,sticky='w',padx=3)
  wsty.bind('<<ComboboxSelected>>',lambda e: _update())

  ttk.Label(frame1,text='Variant').grid(row=2,column=0,sticky='w',padx=3)
  wvar = ttk.Combobox(frame1,
                      textvariable=variant,
                      values=['normal','small-caps'],
                      width=10)
  wvar.grid(row=2,column=1,sticky='w',padx=3)
  wvar.bind('<<ComboboxSelected>>',lambda e: _update())

  ttk.Label(frame1,text='Stretch').grid(row=3,column=0,sticky='w',padx=3)
  wstr = ttk.Combobox(frame1,
                      textvariable=stretch,
                      values=['ultra-condensed',
                              'extra-condensed',
                              'condensed',
                              'semi-condensed',
                              'normal',
                              'semi-expanded',
                              'expanded',
                              'extra-expanded',
                              'ultra-expanded'],
                      width=10)
  wstr.grid(row=3,column=1,sticky='w',padx=3)
  wstr.bind('<<ComboboxSelected>>',lambda e: _update())

  ttk.Label(frame1,text='Weight').grid(row=4,column=0,sticky='w',padx=3)
  wwei = ttk.Combobox(frame1,
                      textvariable=weight,
                      values=['ultralight',
                              'light',
                              'normal',
                              'regular',
                              'book',
                              'medium',
                              'roman',
                              'semibold',
                              'demibold',
                              'demi',
                              'bold',
                              'extra',
                              'extra bold',
                              'black'],
                      width=10)
  wwei.grid(row=4,column=1,sticky='w',padx=3)
  wwei.bind('<<ComboboxSelected>>',lambda e: _update())

  ttk.Label(frame1,text='Size').grid(row=5,column=0,sticky='w',padx=3)
  wsiz = ttk.Combobox(frame1,
                      textvariable=size,
                      values=['xx-small',
                              'x-small',
                              'small',
                              'medium',
                              'large',
                              'x-large',
                              'xx-large'],
                      width=10)
  wsiz.grid(row=5,column=1,sticky='w',padx=3)
  wsiz.bind('<Return>',lambda e: _update())
  wsiz.bind('<<ComboboxSelected>>',lambda e: _update())

  frame1.grid()

  frame2 = ttk.Frame(window,padding=15)
  fig = Figure(figsize=(8,1))
  ax = fig.add_subplot(111,frameon=False)
  align = {'horizontalalignment': 'center', 'verticalalignment': 'baseline'}
  ax.text(1,1,sample,fontproperties=myfont,**align)
  ax.set_xlim(0,2)
  ax.set_ylim(0,2)
  ax.get_xaxis().set_visible(False)
  ax.get_yaxis().set_visible(False)
  canvas = FigureCanvasTkAgg(fig, master=frame2)
  canvas.get_tk_widget().pack()
  canvas.draw()
  frame2.grid()

  frame3 = ttk.Frame(window,padding=15)
  ttk.Button(frame3,
             text='Cancel',
             command=_cancel).grid(row=0,column=1,padx=3)
  ttk.Button(frame3,
             text='Done',
             command=_done).grid(row=0,column=2,padx=3)
  frame3.grid()

  window.wait_window(window)

  return myfont

def setfont(fontdict):
# ====================
  '''Returns a Matplotlib font object from dictionary'''

  family = fontdict['_family'][0]
  slant = fontdict['_slant']
  variant = fontdict['_variant']
  stretch = fontdict['_stretch']
  weight = fontdict['_weight']
  size = fontdict['_size']
  myfont = FontProperties(family=family,
                          style=slant,
                          variant=variant,
                          stretch=stretch,
                          weight=weight,
                          size=size).copy()
  return myfont

def read_lines(ifile):
# ====================
  print('Loading file ' + ifile)
  d = {}
  xo = []
  yo=[]
  with open(ifile) as infile:
    for line in infile.readlines():
      line = line.strip()
      columns = line.split(' ')
      x4 = float(columns[0])
      y4 = float(columns[1])
      notnan = x4 < 0 or x4 > 0
      if notnan:
        xo.append(x4)
        yo.append(y4)
      else:
        xo.append(np.nan)
        yo.append(np.nan)
  d['lon']=xo
  d['lat']=yo
  return d

# ==============================
def map_proj(name, params=None):
# ==============================
  '''
    Function: map_proj()
    Purpose: Implement a color selector by calling 	askcolor().
    Input:
       name: string. In the basic usage, name correspond to the Cartopy
             projections. Other values are:
        name = "lista": returns a list of all projections names
        name = "defs" provides the definition of each projection
        name = "state" provides the state vector of parameters
       params: structure with all the possible options for the Cartopy
               projections.
    EGL, 06/2020
  '''
  import cartopy.crs as ccrs
  
  params_def = {"central_longitude":0.0, "central_latitude":0.0, 
        "min_latitude":-80.0, "max_latitude":84.0,
        "false_easting":0.0, "false_northing":0.0,
        "latitude_true_scale":0.0,
        "true_scale_latitude":None, "scale_factor":None,
        "satellite_height":35785831, "sweep_axis":'y'}    
  if params is None:
    params = params_def
        
  options = {'PlateCarree':{'text':'Equidistant cylindrical',
              'proj':ccrs.PlateCarree(central_longitude=params['central_longitude']),
              'state':[1,0,0,0,0,0,0,0,0,0,0]}, 
             'LambertCylindrical':{'text':'LambertCylindrical',
              'proj':ccrs.LambertCylindrical(central_longitude=params['central_longitude']),
              'state':[1,0,0,0,0,0,0,0,0,0,0]},
             'Mercator':{'text':'Mercator',
              'proj':ccrs.Mercator(central_longitude=params['central_longitude'], 
                 min_latitude=params['min_latitude'], max_latitude=params['max_latitude'], 
                 latitude_true_scale=params['latitude_true_scale'], 
                 false_easting=params['false_easting'], false_northing=params['false_northing']),
              'state':[1,0,1,1,1,1,1,0,0,0,0]},
             'Miller':{'text':'Miller',
              'proj':ccrs.Miller(central_longitude=params['central_longitude']),
              'state':[1,0,0,0,0,0,0,0,0,0,0]},
             'Mollweide':{'text':'Pseudocylindrical and equal area',
              'proj':ccrs.Mollweide(central_longitude=params['central_longitude'], 
                    false_easting=params['false_easting'], 
                    false_northing=params['false_northing']),
              'state':[1,0,0,0,1,1,0,0,0,0,0]},
             'Orthographic':{'text':'Orthographic',
              'proj':ccrs.Orthographic(central_longitude=params['central_longitude'],
                                       central_latitude=params['central_latitude']),
              'state':[1,1,0,0,0,0,0,0,0,0,0]},
             'Robinson':{'text':'Robinson pseudocylindrical',
             'proj':ccrs.Robinson(central_longitude=params['central_longitude'],
                    false_easting=params['false_easting'],false_northing=params['false_northing']),
              'state':[1,0,0,0,1,1,0,0,0,0,0]},
               #'UTM':{'text':'UTM','proj':ccrs.UTM()},
             'EuroPP':{'text':'UTM Zone 32 projection for EuroPP domain','proj':ccrs.EuroPP(),
              'state':[0,0,0,0,0,0,0,0,0,0,0]},
             'Geostationary':{'text':'Appropriate view for satellites in Geostationary Earth orbit',
              'proj':ccrs.Geostationary(central_longitude=params['central_longitude'], 
                false_easting=params['false_easting'],false_northing=params['false_northing'],
                satellite_height=params['satellite_height'],sweep_axis=params['sweep_axis']),
              'state':[1,0,0,0,1,1,0,0,0,1,1]},
             'EqualEarth':{'text':'Pseudocylindrical and equal area DOI: 10.1080/13658816.2018.1504949',
              'proj':ccrs.EqualEarth(central_longitude=params['central_longitude'], 
                    false_easting=params['false_easting'], 
                    false_northing=params['false_northing']),
              'state':[1,0,0,0,1,1,0,0,0,0,0]},
             'LambertAzimuthalEqualArea':{'text':'A Lambert Azimuthal Equal-Area projection',
              'proj':ccrs.LambertAzimuthalEqualArea(
                     central_longitude=params['central_longitude'], 
                     central_latitude=params['central_latitude'],
                     false_easting=params['false_easting'], 
                     false_northing=params['false_northing']),
              'state':[1,1,0,0,1,1,0,0,0,0,0]},
             'NorthPolarStereo':{'text':'NorthPolarStereo',
              'proj':ccrs.NorthPolarStereo(central_longitude=params['central_longitude'],
              true_scale_latitude=params['true_scale_latitude']),
              'state':[1,0,0,0,0,0,0,1,0,0,0]},
             'SouthPolarStereo':{'text':'SouthPolarStereo',
              'proj':ccrs.SouthPolarStereo(central_longitude=params['central_longitude'],
              true_scale_latitude=params['true_scale_latitude']),
              'state':[1,0,0,0,0,0,0,1,0,0,0]}
            }     
  ids = options.keys()
  if name == 'lista': 
   return list(ids)
  elif name  == 'all':
   return options
  elif name  == 'defs':
   key = list(ids)
   value = [options[i]['text'] for i in key]
   text = {key: value for (key, value) in zip(key,value)}
   return text
  elif name in ids:
    return options[name]
  else:
   return False

# ===============================================================
def scale_bar(ax, length=None, location=(0.5, 0.05), linewidth=3):
# ================================================================
    """
    ax is the axes to draw the scalebar on.
    length is the length of the scalebar in km.
    location is center of the scalebar in axis coordinates.
    (ie. 0.5 is the middle of the plot)
    linewidth is the thickness of the scalebar.
    EGL, 06/2020, Adapted to cartopy.
    """
    import cartopy.crs as ccrs
    import numpy as np
    
    #Get the limits of the axis in lat long
    llx0, llx1, lly0, lly1 = ax.get_extent(ccrs.PlateCarree())
    #Make tmc horizontally centred on the middle of the map,
    #vertically at scale bar location
    sbllx = (llx1 + llx0) / 2
    sblly = lly0 + (lly1 - lly0) * location[1]
    tmc = ccrs.TransverseMercator(sbllx, sblly)
    #Get the extent of the plotted area in coordinates in metres
    x0, x1, y0, y1 = ax.get_extent(tmc)
    #Turn the specified scalebar location into coordinates in metres
    sbx = x0 + (x1 - x0) * location[0]
    sby = y0 + (y1 - y0) * location[1]

    #Calculate a scale bar length if none has been given
    #(Theres probably a more pythonic way of rounding the number but this works)
    if not length: 
        length = (x1 - x0) / 5000 #in km
        ndim = int(np.floor(np.log10(length))) #number of digits in number
        length = round(length, -ndim) #round to 1sf
        #Returns numbers starting with the list
        def scale_number(x):
            if str(x)[0] in ['1', '2', '5']: return int(x)        
            else: return scale_number(x - 10 ** ndim)
        length = scale_number(length) 

    #Generate the x coordinate for the ends of the scalebar
    bar_xs = [sbx - length * 500, sbx + length * 500]
    #Plot the scalebar
    ax.plot(bar_xs, [sby, sby], transform=tmc, color='k', linewidth=linewidth)
    #Plot the scalebar label
    ax.text(sbx, sby, str(length) + ' km', transform=tmc,
            horizontalalignment='center', verticalalignment='bottom')

# =================================================
def colsel(tkvar,tkstyle,widget,style,master=None):
# =================================================
  ''' Function: colsel()
  	Purpose: Implement a color selector by calling 	askcolor().
  	Input: 
		tkvar: String tkinter var with the color name
		tkstyle: a ttk.Style() that updates when changing tkvar
		style: string denoting the style name
		widget: a reference to the id widget (tk:Label)
		master: Optionally the widget id of the parent widget.

	Example of its use: tipically you need two Labels widgets and a 
	button and a ttk.Styke().
	
	bline = ttk.Style()
    bline.configure("bline.TLabel",background=PLOT.FIGURE_COLOR.get())
	ttk.Label(wid,text='Line color').grid()
    OB = ttk.Label(wid,textvariable=PLOT.FIGURE_COLOR,style="obgrid.TLabel",width=8)
    OB.grid()
    ttk.Button(wid,text='Select',\
       command=lambda:colsel(PLOT.FIGURE_COLOR, bline, OB,"bline.TLabel",master=parent)).grid()
	EGL, 06/2020
'''
  #from tkcolorpicker import askcolor
  from tkinter.colorchooser import askcolor
  if master is not None:
    backup = tkvar.get()
    if tkvar.get() == 'None':
      rgb, hx = askcolor(parent=master)
    else:
      rgb, hx = askcolor(color=tkvar.get(),parent=master)
    if hx is None:
      tkvar.set(backup)
    else:
      tkvar.set(hx)
      tkstyle.configure(style,background=tkvar.get())
      widget.configure(style=style)
  else:
    print("Error: master is not set")
    return False

# =============================================
def toconsola(message, tag="", wid=None):
# =============================================
  ''' Function: toconsola()
    Purpose: Send message to a consola
    Input: 
       wid: widget id of the consola.
       message: string to be send
       tag: string not implemented allows to pass tags to format the output
       concerning the font, the color, etc...
       As an example three tags has been introduced:
         "y" ("yellow" and "bold")
         "o" ("orange" and "bold")
         "r" ("red" and "bold")
    EGL, 06/2020
  '''
  if wid is not None:
    wid.insert("end", message + "\n", tag)
    wid.see(tk.END)
  else:
    print(message)

# =============================================
def initial_position(VEC,FLOAT,**args):
# =============================================
  ''' Calculate the initial position of the float position
      intersecting the model domain and time period. 
      If there is no common location, None values are 
      returned '''

  # VEC class:
  # --------------
  # VEC.DATE
  # VEC.TIME
  # VEC.Z_LIST
  # VEC.K
  #
  # FLOAT class:
  # --------------
  # FLOAT.nfloats
  # FLOAT.nrecords
  # FLOAT.lon
  # FLOAT.lat
  # FLOAT.DATE
  # FLOAT.TIME

  # Identify the first buoy record within the model simulation period:
  #
  vko = []
  vxo = []
  vyo = []
  vzo = []
  vdo = []
  vto = []
  vdt = []
  for flo in range(FLOAT.nfloats):

    if FLOAT.nfloats == 1:
      buoy_lon = FLOAT.lon[:]
      buoy_lat = FLOAT.lat[:]
    else:
      buoy_lon = FLOAT.lon[:][flo]
      buoy_lat = FLOAT.lat[:][flo]

    if len(FLOAT.DATE.shape) == 1:
      buoy_date = FLOAT.DATE[:].copy()
      buoy_time = FLOAT.TIME[:]
    else:
      buoy_date = FLOAT.DATE[:][flo].copy()
      buoy_time = FLOAT.TIME[:][flo]


    do = None
    for dd in reversed(buoy_date):
      if dd >= VEC.DATE[0] and dd < VEC.DATE[-1]:
        do = dd

    if do is None:
      print('ERROR: No match between model and buoy')
      return None
  
    #ko = buoy_date.index(do)
    ko = np.where(buoy_date == do)
    ko = ko[0]

  # Time difference between the first record and the initial model time:


    vko.append(ko)
    vxo.append(buoy_lon[ko])
    vyo.append(buoy_lat[ko])
    try:
      vzo.append(VEC.Z_LIST[VEC.K.get()])
    except:
      vzo.append([0])

    vdo.append(do)
    vto.append(buoy_time[ko])
    vdt.append(buoy_time[ko] - VEC.TIME[0])

    #print("%9.3f, %9.3f, %9.3f, %9.0f" % (xo[-1], yo[-1], zo[-1], to[-1]))

  return vko, vxo, vyo, vzo, vdo, vto, vdt

# =============================================
def dhist(UU,VV,**args):
# =============================================
  ''' Print the double histogram of U and V '''

  import matplotlib.pyplot as plt

  U = UU
  V = VV
  for i in reversed(range(len(V))):
    if V[i] < 1e-4:
      V = np.delete(V,i)
      U = np.delete(U,i)


  print('V = ',V)

  fig = plt.figure(3)
  ax  = plt.axes([0.15,0.10,0.80,0.66])

  ax.set_xlabel('Velocity')
  ax.set_ylabel('Count')
  ax.set_xlabel('Buoy')
  ax.set_ylabel('Model')
  ax.grid(True)

  Umax = np.max(U)

  # IQR:
  Q1 = np.percentile(U, 25, interpolation = 'midpoint')
  Q3 = np.percentile(U, 75, interpolation = 'midpoint')
  IQR = Q3 - Q1
  dU = 2*IQR/np.cbrt(len(U))

  n = np.ceil(Umax/dU)
  bins = np.linspace(0,Umax,n)

  ax.plot(U,V,'o')
  #ax.hist([U,V],bins,label=['Buoy','Model'])
  ax.legend()

  plt.show()

