''' COSMO project python module.
    It contains the functions required for the utilitites created
    for the COSMO project to work'''

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkinter import font as tkfont
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  import tkFont as tkfont

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

# =============
class geocdf():
# =============
  ''' Class whose attributes will contain the information about dimensions
      and variables of a Netcdf file, making a guessing of the 
      variables representing the X, Y, Z, and T axis.'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__(self,filename):
  # ===========================
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
    self.regular = True
    self.georef  = True

    # We will try to identify the axes in the file:
    for name,variable in ncid.variables.items():
      try:
        axis = getattr(variable,'axis')
        if axis == 'X':
          self.georef  = True
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
          self.georef  = True
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
    conf['regular'] = self.regular
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

    self.regular  = conf['regular']
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



# =================
class WinGeoaxes():
# =================
  '''Class to interactively select/modify the names of the
     axis variables of a netcdf file'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

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

    self.regular = tk.BooleanVar()
    self.georef  = tk.BooleanVar()
    self.regular.set(icdf.regular)
    self.georef.set(icdf.georef)


  # --------------------------------------------- Frame for X, Y, Z, T

    menubar = tk.Menu(self.parent)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='File',menu=menu)
    menu.add_command(label='ncdump',command=self.ncdump)
    try:
      self.parent.config(menu=menubar)
    except AttributeError:
      # parent is a toplevel window (Python 2.4/Tkinter 1.63)
      self.parent.tk.call(self.parent, "config", "-menu", menubar)


    Fr0 = ttk.Frame(self.parent,padding=5,width=700)
    ttk.Label(Fr0,text='Filename = '+icdf.filename,padding=5,font='italic')  \
       .grid(row=0,column=0)
    Fr0.grid(row=0,column=0,sticky='E'+'W'+'N'+'S')

    font_bold = tkfont.Font(font='TkDefaultFont').copy()
    font_bold['weight']='bold'

    Fr1 = ttk.Frame(self.parent,padding=5,width=700,borderwidth=5,relief='sunken')
    ttk.Label(Fr1,text='Axis',width=12,font=font_bold).grid(row=0,column=0)
    ttk.Label(Fr1,text='X',width=17,justify='center',font=font_bold) \
       .grid(row=0,column=1)
    ttk.Label(Fr1,text='Y',width=17,justify='center',font=font_bold) \
       .grid(row=0,column=2)
    ttk.Label(Fr1,text='Z',width=17,justify='center',font=font_bold) \
       .grid(row=0,column=3)
    ttk.Label(Fr1,text='T',width=17,justify='center',font=font_bold) \
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
    self.Ibox.bind('<<ComboboxSelected>>',lambda e : self.iselection(icdf))
    self.Jbox.bind('<<ComboboxSelected>>',lambda e : self.jselection(icdf))
    self.Kbox.bind('<<ComboboxSelected>>',lambda e : self.kselection(icdf))
    self.Lbox.bind('<<ComboboxSelected>>',lambda e : self.lselection(icdf))

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
    self.Xbox.bind('<<ComboboxSelected>>',lambda e: self.xselection(icdf))
    self.Ybox.bind('<<ComboboxSelected>>',lambda e: self.yselection(icdf))
    self.Zbox.bind('<<ComboboxSelected>>',lambda e: self.zselection(icdf))
    self.Tbox.bind('<<ComboboxSelected>>',lambda e: self.tselection(icdf))

    self.wgr = tk.Checkbutton(Fr1,text='Georeferenced',variable=self.georef, \
          font=font_bold,onvalue=True,offvalue=False)
    self.wgr.grid(row=4,column=1)
    self.wgr.bind('<Button-1>',lambda e: self._georef(icdf))

    self.wrg = tk.Checkbutton(Fr1,text='Regular grid',variable=self.regular,font=font_bold)
    self.wrg.grid(row=4,column=3)

    Fr1.grid(row=1,column=0,rowspan=4,sticky='ewns')

    self.Window_ncdump = None


  def ncdump(self):
  # ===============
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
      ncdump.WinNcdump(self.Window_ncdump,self.ncid)
    else:
      self.Window_ncdump.lift()


  def iselection(self,icdf):
  # ========================
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
  # ========================
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
  # ========================
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

  def lselection(self,icdf):
  # ========================
    value_selected = self.Lbox.get()
    if not empty(value_selected):
      ind = icdf.DIM_LIST.index(value_selected)
      icdf.idl   = ind
      icdf.nt    = icdf.dlen[ind]
      icdf.lname = value_selected
    else:
      icdf.idl = -1
      icdf.nt = 1
    self.Lbox.selection_clear()
    self.strnt.set(str(icdf.nt))


  def xselection(self,icdf):
    value_selected = self.Xbox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      icdf.idx   = ind
      icdf.xname = value_selected

      print(icdf.ndims[ind])
      print(icdf.dimids[ind])

      if icdf.ndims[ind] == 1:
        print('xselection 1D')

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idi   = kk
        icdf.nx    = icdf.dlen[kk]
        icdf.iname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.regular = True
        self.Iname.set(icdf.iname)
        self.strnx.set(str(icdf.nx))
        self.georef.set(icdf.georef)
        self.regular.set(icdf.regular)
      elif icdf.ndims[ind] == 2:
        #messagebox.showinfo(message='Two-dimensional grid')
        print('xselection 2D grid')
        kk = icdf.dimids[ind][1]
        icdf.idi   = kk
        icdf.nx    = icdf.dlen[kk]
        icdf.iname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.regular = False
        self.Iname.set(icdf.iname)
        self.strnx.set(str(icdf.nx))
        self.georef.set(icdf.georef)
        self.regular.set(icdf.regular)
        kk = icdf.dimids[ind][0]
        icdf.idj   = kk
        icdf.ny    = icdf.dlen[kk]
        icdf.jname = icdf.DIM_LIST[kk]
        self.Jname.set(icdf.jname)
        self.strny.set(str(icdf.ny))
      else:
        messagebox.showinfo(message='Invalid variable. \
                                     It must have a single dimension')
        self.Xname.set(icdf.VAR_MENU[icdf.idx])
    else:
      icdf.idx = -1
      icdf.idi = -1
      icdf.nx  =  1
      icdf.georef =  False
      self.Xname.set(icdf.VAR_MENU[icdf.idx])
      self.Iname.set(icdf.DIM_MENU[icdf.idi])
      self.strnx.set(str(icdf.nx))
      self.georef.set(icdf.georef)
    self.Xbox.selection_clear()


  def yselection(self,icdf):
    value_selected = self.Ybox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      icdf.idy   = ind
      icdf.yname = value_selected
      if icdf.ndims[ind] == 1:

        # Update dimension to match selected variable
        kk = icdf.dimids[ind][0]
        icdf.idj   = kk
        icdf.ny    = icdf.dlen[kk]
        icdf.jname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.regular = True
        self.Jname.set(icdf.jname)
        self.strny.set(str(icdf.ny))
      elif icdf.ndims[ind] == 2:
        #messagebox.showinfo(message='Two-dimensional grid')
        print('2D grid')
        kk = icdf.dimids[ind][1]
        icdf.idi   = kk
        icdf.nx    = icdf.dlen[kk]
        icdf.iname = icdf.DIM_LIST[kk]
        icdf.georef = True
        icdf.regular = False
        self.Iname.set(icdf.iname)
        self.strnx.set(str(icdf.nx))
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
      icdf.georef =  False
      self.Yname.set(icdf.VAR_MENU[icdf.idy])
      self.Jname.set(icdf.DIM_MENU[icdf.idj])
      self.strny.set(str(icdf.ny))
    self.Ybox.selection_clear()

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


  def zselection(self,icdf):
  # ========================
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
  # ========================
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
      icdf.idt = -1
      icdf.idl = -1
      icdf.nt  =  1
      self.Tname.set(icdf.VAR_MENU[icdf.idt])
      self.Lname.set(icdf.DIM_MENU[icdf.idl])
      self.strnt.set(str(icdf.nt))
      icdf.time_units = ''
      icdf.time_calendar = ''
    self.Tbox.selection_clear()

  def selected_var(self,icdf,Vbox):
  # ===============================
    value_selected = Vbox.get()
    if not empty(value_selected):
      ind = icdf.VAR_LIST.index(value_selected)
      dlist = icdf.dimids[ind]
      nd = len(dlist) - dlist.count(-1)
      dlist[nd:] = []
      idim = 0
      for kk in reversed(dlist):
        idim += 1
        if idim == 1:
          icdf.idi   = kk
          icdf.nx    = icdf.dlen[kk]
          icdf.iname = icdf.DIM_LIST[kk]
          self.Iname.set(icdf.iname)
          self.strnx.set(str(icdf.nx))
        elif idim == 2:
          icdf.idj   = kk
          icdf.ny    = icdf.dlen[kk]
          icdf.jname = icdf.DIM_LIST[kk]
          self.Jname.set(icdf.jname)
          self.strny.set(str(icdf.ny))
        elif idim == 3:
          icdf.idk   = kk
          icdf.nz    = icdf.dlen[kk]
          icdf.kname = icdf.DIM_LIST[kk]
          self.Kname.set(icdf.kname)
          self.strnz.set(str(icdf.nz))
        elif idim == 4:
          icdf.idl   = kk
          icdf.nt    = icdf.dlen[kk]
          icdf.lname = icdf.DIM_LIST[kk]
          self.Lname.set(icdf.lname)
          self.strnt.set(str(icdf.nt))

        



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
                      values=['normal',
                              'italic',
                              'oblique'],
                      width=10)
  wsty.grid(row=1,column=1,sticky='w',padx=3)
  wsty.bind('<<ComboboxSelected>>',lambda e: _update())

  ttk.Label(frame1,text='Variant').grid(row=2,column=0,sticky='w',padx=3)
  wvar = ttk.Combobox(frame1,
                      textvariable=variant,
                      values=['normal',
                              'small-caps'],
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



