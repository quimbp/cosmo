''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems
    EGL, 06/2020: Changes:
		No more support to python 2.7 
		Support to Basemap deprecated and updated to Cartopy system
		Limited support to geographical projections. Everything is 
		plotted in PlateCarree and data are supposed to be provided
		in geodetic (lon,lat) values.
		A heap variable MESSAGE has been introduce to store "print" messages
 '''
import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

import dateutil.parser as dparser
import numpy as np
import datetime
import json
import os
import io
import urllib.request
try:
  to_unicode = unicode
except:
  to_unicode = str

#import matplotlib
#matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure

import cosmo.lineplot as lineplot
from cosmo.tools import toconsola
from cosmo.tools import exists
from cosmo.tools import caldat
from cosmo.tools import Select_Columns
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA

#EG To manage cartopy projections
#EG from cosmo.tools import map_proj

__version__ = "1.0"
__author__  = "Quim Ballabrera"
__date__    = "July 2018"

# =================
class parameters():
# =================
  ''' Class for Lagrangian floats'''
  
  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "July 2018"

  def __init__ (self, wid=False):
  # ==================
    ''' Define ans initialize class attrobutes'''

    self.MESSAGE = ""

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF        = COSMO_CONF + 'trajectory.conf'

    self.FILENAME        = tk.StringVar()
    self.SOURCE          = 'FILE'
    self.ALIAS           = tk.StringVar()
    self.show            = tk.BooleanVar()
    self.I               = tk.IntVar()
    self.L               = tk.IntVar()
    self.L1              = tk.IntVar()
    self.L2              = tk.IntVar()
    self.SEPARATED_COLOR = tk.BooleanVar()
    self.FLOAT_COLOR     = []
    self.FLOAT_SHOW      = []
    self.FLOAT_ZORDER    = []
    self.CROP            = tk.BooleanVar()

    self.PLOT            = lineplot.parameters()
    
    self.MESSAGE +=  self.PLOT.MESSAGE
    
    self.nfloats         = 0
    self.nrecords        = 0
    self.lon             = []
    self.lat             = []
    self.DATE            = []
    self.TIME            = []
    self.speed           = []
    self.SOURCE          = 'FILE'
    self.I.set(0)
    self.L.set(0)
    self.PLOT.LINE_COLOR.set('blue')
    self.SEPARATED_COLOR.set(False)
    self.show.set(True)
    self.ALIAS.set('')
    self.CROP.set(False)
    self.Fx              = None
    self.Fy              = None

    self.LINK            = tk.BooleanVar()
    self.LINK.set(False)
    
    self.cons = wid

    #if exists(self.FILECONF):
    #  print('Reading Lagrangian configuration file '+self.FILECONF)
    #  try:
    #    conf = self.conf_load(self.FILECONF)
    #    self.conf_set(conf)
    #  except:
    #    print('Error reading, using default parameters')
    #    conf = self.conf_get()
    #    self.conf_save(conf,self.FILECONF)
    #else:
    #  print('Saving Lagrangian configuration file ',self.FILECONF)
    #  conf = self.conf_get()
    #  self.conf_save(conf,self.FILECONF)

  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes '''

    conf = {}
    conf['NFLOATS'] = self.nfloats
    conf['SOURCE']  = self.SOURCE
    conf['ALIAS']  = self.ALIAS.get()
    conf['SHOW']  = self.show.get()
    conf['I']  = self.I.get()
    conf['L']  = self.L.get()
    conf['L1'] = self.L1.get()
    conf['L2'] = self.L2.get()
    conf['SEPARATED_COLOR']  = self.SEPARATED_COLOR.get()
    FLOAT_COLOR  = []
    FLOAT_SHOW   = []
    FLOAT_ZORDER = []
    try:
      for i in range(self.nfloats):
        FLOAT_COLOR.append(self.FLOAT_COLOR[i].get())
        FLOAT_SHOW.append(self.FLOAT_SHOW[i].get())
        FLOAT_ZORDER.append(self.FLOAT_ZORDER[i].get())
    except:
      for i in range(self.nfloats):
        FLOAT_COLOR.append(self.PLOT.LINE_COLOR.get())
        FLOAT_SHOW.append(self.show.get())
        FLOAT_ZORDER.append(self.PLOT.ZORDER.get())
    conf['FLOAT_COLOR'] = FLOAT_COLOR.copy()
    conf['FLOAT_SHOW']  = FLOAT_SHOW.copy()
    conf['FLOAT_ZORDER']  = FLOAT_ZORDER.copy()
    conf['CROP'] = self.CROP.get()
    conf['LINK']  = self.LINK.get()
    conf['PLOT'] = self.PLOT.conf_get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class dictionary from class attributes '''

    self.nfloats = conf['NFLOATS']
    self.SOURCE = conf['SOURCE']
    self.ALIAS.set(conf['ALIAS'])
    self.show.set(conf['SHOW'])
    self.I.set(conf['I'])
    self.L.set(conf['L'])
    self.L1.set(conf['L1'])
    self.L2.set(conf['L2'])
    self.SEPARATED_COLOR.set(conf['SEPARATED_COLOR'])
    self.FLOAT_COLOR = []
    self.FLOAT_SHOW = []
    self.FLOAT_ZORDER = []
    try:
      for i in range(self.nfloats):
        self.FLOAT_COLOR.append(tk.StringVar(value=conf['FLOAT_COLOR'][i]))
        self.FLOAT_SHOW.append(tk.BooleanVar(value=conf['FLOAT_SHOW'][i]))
        self.FLOAT_ZORDER.append(tk.IntVar(value=conf['FLOAT_ZORDER'][i]))
    except:
      for i in range(self.nfloats):
        self.FLOAT_COLOR.append(tk.StringVar(value=self.PLOT.LINE_COLOR.get()))
        self.FLOAT_SHOW.append(tk.BooleanVar(value=self.show.get()))
        self.FLOAT_ZORDER.append(tk.IntVar(value=self.PLOT.ZORDER.get()))
    self.CROP.set(conf['CROP'])
    self.LINK.set(conf['LINK'])
    self.PLOT.conf_set(conf['PLOT'])

  def conf_load(self,filename):
  # ===========================
    '''Open an read the configuration file'''

    # Read configuration
    with open(filename) as infile:
      conf = json.load(infile)
    return conf

  def conf_save(self,conf,filename):
  # ===============================
    '''Save the configuration file'''

    with io.open(filename,'w',encoding='utf8') as outfile:
      str_ = json.dumps(conf,ensure_ascii=False, \
                             sort_keys=True,     \
                             indent=2,           \
                             separators=(',',': '))
      outfile.write(to_unicode(str_))
      outfile.close()

  def Read(self,filename):
  # ======================
    '''Opens and reads a trajectory file'''

    __version__ = "0.3"
    __author__  = "Quim Ballabrera"
    __date__    = "February 2018"

    self.FILENAME.set(filename)

    # --------------------------------------
    def read_trajectory_ncdf(filename):
    # --------------------------------------
      '''Read a set of trajectories from a netcdf file'''
      from netCDF4 import Dataset
	  
	  #EG
      self.MESSAGE +='\n Reading netcdf file - '+filename
      if self.cons: toconsola(self.MESSAGE, wid=self.cons)
      else:  print(self.MESSAGE)
      
      ncid = Dataset(filename)
      self.nfloats  = ncid.dimensions['floats'].size
      self.nrecords = ncid.dimensions['time'].size

      # Check time dimensions:
      # In BLM files, the time variables has a unique dimension.
      # In MLM files, the time variables has two dimensions.
      #
      if len(ncid.variables['time'].shape) == 1:
        self.SOURCE = 'blm'
      elif len(ncid.variables['time'].shape) == 2:
        self.SOURCE = 'mlm'
      else:
        #EG
        self.MESSAGE +='\nError: Unknown Trajectory source'
        if self.cons: toconsola(self.MESSAGE, wid=self.cons)
        else:  print(self.MESSAGE)
        return

      try:
        self.lon  = ncid.variables['lon'][:,:].squeeze()    
      except:
        self.lon  = ncid.variables['longitude'][:,:]

      try:
        self.lat  = ncid.variables['lat'][:,:].squeeze()   
      except:
        self.lat  = ncid.variables['latitude'][:,:]

      # If some longitudes have missing values, we set them
      # equal to nan:
      #
      try:
        self.lon = self.lon.filled(fill_value=np.nan)
      except:
        pass
      try:
        self.lat = self.lat.filled(fill_value=np.nan)
      except:
        pass

      # Get the date of the floats:
      #
      if self.SOURCE == 'blm':
      # ------------------------
        Time_jd = ncid.variables['time'][:]
        self.DATE = []
        for i in range(self.nrecords):
          a = caldat(Time_jd[i])
          self.DATE.append(datetime.datetime(a[0],a[1],a[2],a[3],a[4],a[5]))
      elif self.SOURCE == 'mlm':
      # ------------------------
        Time_jd = ncid.variables['time'][:,:]
        Time_jd[Time_jd>1e10] = np.nan

        self.DATE = []
        for j in range(ncid.variables['time'].shape[0]):
          tmpdate = []
          for i in range(self.nfloats):
            a = caldat(Time_jd[j,i])
            if np.isnan(a[0]):
              tmpdate.append(datetime.datetime(6050,1,1,12,0,0))
            else:
              tmpdate.append(datetime.datetime(a[0],a[1],a[2],a[3],a[4],a[5]))
          self.DATE.append(tmpdate)

    # --------------------------------------
    def read_trajectory_json(filename):
    # --------------------------------------
      '''Read a trajectory from a json file'''
      import json

      if filename[0:5].lower() == 'http:':
        self.MESSAGE +='\nReading remote json file.. '+filename.split("/")[-1]
        self.MESSAGE +='\nPath: '+'/'.join(filename.split("/")[:-1])+"/n"
        if self.cons: toconsola(self.MESSAGE, wid=self.cons)
                
        response = urllib.request.urlopen(filename)
        data = response.read()
        text = data.decode('utf-8')
        DATA = json.loads(text)
      else:
        self.MESSAGE +='\nReading local json file.. '+filename.split("/")[-1]
        self.MESSAGE +='\nPath: '+'/'.join(filename.split("/")[:-1])+"/n"
        if self.cons: toconsola(self.MESSAGE, wid=self.cons)
        else:  print(self.MESSAGE)
        
        with open(filename) as datafile:
          DATA = json.load(datafile)

      nfeatures = len(DATA["features"])
      self.MESSAGE +="\nNumber of features: "+str(nfeatures)
      if self.cons: toconsola(self.MESSAGE, wid=self.cons)
      else:  print(self.MESSAGE)
      # Detect the GEOJSON MODE
      # In the "Dated LineString", the date is stored in the property "time"
      # of the trajectory
      # In the "Undated LineString", the date is stored in the points
      #

      pline = [DATA["features"][i]["geometry"]["type"] \
                           for i in range(nfeatures)].index("LineString")

      try:
        nl = DATA["features"][pline]["properties"]["time"]["data"]
        fileFormat = "Dated LineString"
      except:
        fileFormat = "Undated LineString"

      self.lon = []
      self.lat = []
      self.DATE = []

      if fileFormat == "Undated LineString":
        for i in range(nfeatures):
          if DATA["features"][i]["geometry"]["type"] == "Point":
            a = DATA["features"][i]["geometry"]["coordinates"]
            b = DATA["features"][i]["properties"]["time"]
            self.lon.append(a[0])
            self.lat.append(a[1])
            self.DATE.append(datetime.datetime.strptime(b, \
                             '%Y-%m-%dT%H:%M:%SZ'))

      elif fileFormat == "Dated LineString":
        POINTS = DATA["features"][pline]["geometry"]["coordinates"]
        DATES = DATA["features"][pline]["properties"]["time"]["data"]
        for i in range(len(DATES)):
          self.lon.append(POINTS[i][0])
          self.lat.append(POINTS[i][1])
          self.DATE.append(datetime.datetime.strptime(DATES[i], \
                           '%Y-%m-%dT%H:%M:%SZ'))

      else:
        self.MESSAGE +='Unknown GEOJSON file format'
        if self.cons: toconsola(self.MESSAGE, wid=self.cons)
        else:  print(self.MESSAGE)
        
      self.nfloats  = 1
      self.nrecords = len(self.lon)
      self.lon = np.array(self.lon)
      self.lat = np.array(self.lat)

    # --------------------------------------
    def read_trajectory_txt(filename):
    # --------------------------------------
      '''Read a trajectory from a txt file'''

      with open(filename,'r') as f:
        first_line = f.readline()

      self.MESSAGE +='\nReading txt file '+filename
      if self.cons: toconsola(self.MESSAGE, wid=self.cons)
      else:  print(self.MESSAGE)
      
      win = tk.Toplevel()
      win.title('Float column')
      Axes = Select_Columns(win,first_line,' ')
      win.wait_window()

      self.nfloats  = None
      self.nrecords = None
      self.lon  = []
      self.lat  = []
      self.DATE = []
      if Axes.lon is None:
        return
      if Axes.lat is None:
        return

      with open(filename) as datafile:
        for line in datafile.readlines():
          line = line.strip()
          columns = line.split(Axes.SEPARATOR.get())

          self.lon.append(float(columns[Axes.lon]))
          self.lat.append(float(columns[Axes.lat]))
          if Axes.type == 0:
            year   = int(columns[Axes.year])
            month  = int(columns[Axes.month])
            day    = int(columns[Axes.day])
            if Axes.hour is None:
              hour = 0
            else:
              hour   = int(columns[Axes.hour])
            if Axes.minute is None:
              minute = 0
            else:
              minute = int(columns[Axes.minute])
            if Axes.second is None:
              second = 0
            else:
              second = int(columns[Axes.second])
            self.DATE.append(datetime.datetime(year,month,day, \
                                               hour,minute,second))
          elif Axes.type == 1:
            self.DATE.append(datetime.datetime.strptime(columns[Axes.date],Axes.fmt))

          elif Axes.type == 2:
            self.DATE.append(datetime.datetime.strptime(columns[Axes.date]+'T'+columns[Axes.time],Axes.fmt))

          else:
            self.MESSAGE +='unknown ASCII file format'
            if self.cons: toconsola(self.MESSAGE, wid=self.cons)
            else:  print(self.MESSAGE)
            return
        
      self.nfloats  = 1
      self.nrecords = len(self.lon)
      self.lon = np.array(self.lon)
      self.lat = np.array(self.lat)

    filename = self.FILENAME.get()
    if filename.lower().endswith(('.nc','.cdf','.ncdf')):
      read_trajectory_ncdf(filename)

    elif filename.lower().endswith(('.geojson','.json')):
      read_trajectory_json(filename)

    else:
      read_trajectory_txt(filename)

#    elif filename.lower().endswith(('.txt')):
#      read_trajectory_txt(filename)
#
#    elif filename.lower().endswith(('.csv')):
#      print('csv: not yet coded')
#      self.nfloats  = None
#      self.nrecords = None
#
#    elif filename.lower().endswith(('.dat','.data')):
#      print('ascii data: not yet coded')
#      self.nfloats  = None
#      self.nrecords = None


    # Cheack that something has been read:
    if self.nfloats is None:
      self.MESSAGE +='\nReading txt file.. '+filename.split("/")[-1]
      self.MESSAGE +='\nPath: '+'/'.join(filename.split("/")[:-1])
      if self.cons: toconsola(self.MESSAGE, wid=self.cons)
      else:  print(self.MESSAGE)
      self = None
      return

    self.TIME = []
    for i in range(self.nrecords):
      self.TIME.append(self.DATE[i].timestamp())
    self.TIME = np.array(self.TIME)

    self.DATE = np.array(self.DATE)

    # If we have data, we fill some fields to their default value.
    self.I.set(0)
    self.L.set(0)
    self.L1.set(0)
    self.L2.set(self.nrecords-1)
    self.FLOAT_COLOR  = []
    self.FLOAT_SHOW   = []
    self.FLOAT_ZORDER = []
    for i in range(self.nfloats):
      self.FLOAT_COLOR.append(tk.StringVar(value=self.PLOT.LINE_COLOR.get()))
      self.FLOAT_SHOW.append(tk.BooleanVar(value=True))
      self.FLOAT_ZORDER.append(tk.IntVar(value=self.PLOT.ZORDER.get()))

# =======================
def drawing(ax,proj,FLT):
# ======================================
  ''' Draw a 2D vector plot. Option of vectors or stream function'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  if FLT.nfloats == 0:
    return

  if not FLT.show.get():
    return

  #EG recover the cartopy projection
  #EG projection = map_proj(proj)

  r1 = FLT.L1.get()
  r2 = FLT.L2.get() + 1

  for i in range(FLT.nfloats):       # Loop over buoys

    if FLT.SEPARATED_COLOR.get():
      color = FLT.FLOAT_COLOR[i].get()
      visible = FLT.FLOAT_SHOW[i].get()
      zorder  = FLT.FLOAT_ZORDER[i].get()
    else:
      color = FLT.PLOT.LINE_COLOR.get()
      visible = FLT.show.get()
      zorder  = FLT.PLOT.ZORDER.get()

    if FLT.nfloats == 1:
      xx, yy =  FLT.lon[r1:r2],FLT.lat[r1:r2]
    else:
      xx, yy =  FLT.lon[r1:r2,i],FLT.lat[r1:r2,i]

    if FLT.PLOT.LINE_SHOW.get():
      ax.plot(xx,yy,FLT.PLOT.LINE_STYLE.get(),      \
             linewidth=FLT.PLOT.LINE_WIDTH.get(),   \
             transform=proj,                        \
             alpha=FLT.PLOT.ALPHA.get(),            \
             zorder=zorder,                         \
             visible=visible,                       \
             color=color)
             
    if FLT.PLOT.MARKER_SHOW.get():
      ax.plot(xx,yy,FLT.PLOT.MARKER_STYLE.get(),   \
             ms=FLT.PLOT.MARKER_SIZE.get(),       \
             transform=proj,                       \
             alpha=FLT.PLOT.ALPHA.get(),           \
             zorder=zorder,                        \
             visible=visible,                      \
             color=FLT.PLOT.MARKER_COLOR.get())
             
    if FLT.PLOT.INITIAL_SHOW.get():

      # Check that we get the first valid position:
      #
      vr1 = r1
      for i in range(r1,r2):
        if np.isnan(xx[i]) or np.isnan(yy[i]):
          pass
        else:
          vr1 = i
          break
      ax.plot(xx[vr1],yy[vr1],                \
             FLT.PLOT.INITIAL_STYLE.get(),    \
             ms=FLT.PLOT.INITIAL_SIZE.get(),  \
             transform=proj,                  \
             alpha=FLT.PLOT.ALPHA.get(),      \
             zorder=zorder,                   \
             visible=visible,                 \
             color=FLT.PLOT.INITIAL_COLOR.get())
             
    if FLT.PLOT.ONMAP_SHOW.get():
      L = FLT.L.get()

      if FLT.nfloats == 1:
        xx,yy = FLT.MAPX[L], FLT.MAPY[L]
      else:
        xx,yy = FLT.MAPX[L][i], FLT.MAPY[L][i]

      ax.plot(xx,yy,                          \
             FLT.PLOT.ONMAP_STYLE.get(),      \
             ms=FLT.PLOT.ONMAP_SIZE.get(),    \
             transform=proj,                  \
             alpha=FLT.PLOT.ALPHA.get(),      \
             zorder=zorder,                   \
             visible=visible,                 \
             color=FLT.PLOT.ONMAP_COLOR.get())


# =======================
def ShowData(master,LL):
# ======================================
  ''' Shows data from a Lagrangian Trajectory'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  def iselection(LL):
  # =================

    log = tk.Text(master)
    log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')
    log.grid_columnconfigure(0,weight=1)
    log.grid_rowconfigure(0,weight=1)
    # Scrollbar
    scrollb = tk.Scrollbar(master,command=log.yview)
    scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
    log['yscrollcommand'] = scrollb.set

    if LL.SOURCE == 'blm':
      if LL.nfloats == 1:
        for l in range(LL.nrecords):
          string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {} \n'.format(l, \
                                                     LL.lon[l],     \
                                                     LL.lat[l],     \
                                                     LL.DATE[l])
          log.insert('end',string)
      else:
        i = int(LL.I.get())
        for l in range(LL.nrecords):
          string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {} \n'.format(l, \
                                                   LL.lon[l][i],  \
                                                   LL.lat[l][i],  \
                                                   LL.DATE[l])
          log.insert('end',string)
    elif LL.SOURCE == 'mlm':
        i = int(LL.I.get())
        for l in range(LL.nrecords):
          string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {} \n'.format(l, \
                                                   LL.lon[l][i],  \
                                                   LL.lat[l][i],  \
                                                   LL.DATE[l][i])
          log.insert('end',string)


  iselection(LL)

  F0 = ttk.Frame(master)
  ttk.Label(F0,text='Floater:').grid(row=0,column=0,padx=3)
  ibox = ttk.Combobox(F0,textvariable=LL.I,width=5)
  ibox.grid(row=0,column=1)
  ibox.bind('<<ComboboxSelected>>',lambda e: iselection(LL))
  ibox['values'] = list(range(LL.nfloats))
  if LL.nfloats == 1:
    ibox.configure(state='disabled')
  else:
    ibox.configure(state='!disabled')
  F0.grid()

# =============
def editor(LL):
# =============

  global Deploy_date
  global Recover_date
  global REJECT
  global Station_pointer
  global NRECORDS

  BACKUP_lon = LL.lon.copy()
  BACKUP_lat = LL.lat.copy()
  BACKUP_date = LL.DATE.copy()

  REJECT = []
  for i in range(LL.nrecords):
    REJECT.append(tk.BooleanVar(value=False))
 
  Deploy_date = tk.StringVar() 
  Recover_date = tk.StringVar() 
  Station_pointer = tk.IntVar() 
  NRECORDS = tk.IntVar() 

  Deploy_date.set(LL.DATE[0].__str__())
  Recover_date.set(LL.DATE[LL.nrecords-1].__str__())
  Station_pointer.set(0)
  NRECORDS.set(LL.nrecords)

  def _close():
  # ===========
    win.destroy()

  def _cancel():
  # ===========
    LL.lon = BACKUP_lon.copy()
    LL.lat = BACKUP_lat.copy()
    LL.DATE = BACKUP_date.copy()
    win.destroy()

  def _deploytime():
  # ================
    ''' Reject positions before the specified date'''
    print('In deploy date: ', Deploy_date.get())
    t0 = dparser.parse(Deploy_date.get())
    for i in range(LL.nrecords):
      if LL.DATE[i] < t0:
        REJECT[i].set(True)

  def _recovertime():
  # =================
    ''' Reject positions after the specified date'''
    print('In recover date: ', Recover_date.get())
    t0 = dparser.parse(Recover_date.get())
    for i in range(LL.nrecords):
      if LL.DATE[i] > t0:
        REJECT[i].set(True)

  def _purge():
  # =================
    ''' Purge rejected positions'''

  def _station_up():
  # =================
    ''' Purge rejected positions'''

  def _station_down():
  # =================
    ''' Purge rejected positions'''

  def _reject_before():
  # =================
    ''' Purge previous positions'''

  def _reject_after():
  # =================
    ''' Purge following positions'''

  def _reject_this():
  # =================
    ''' Reject a position'''

  def _save():
  # =================
    ''' Save valid positions'''

  win = tk.Toplevel()
  win.title('Trajectory Editor')
  win.resizable(False,False)
  win.protocol('WM_DELETE_WINDOW',_cancel)

  #global Deploy_date
  #global Recover_date

  # Define tabs:
  nb = ttk.Notebook(win)
  page1 = ttk.Frame(nb)
  page2 = ttk.Frame(nb)
  nb.add(page1,text='Canvas')
  nb.add(page2,text='Attributes')

  F0 = ttk.Frame(page1,padding=5)
  ttk.Label(F0,text='Filename',padding=5).grid(row=0,column=0,sticky='w')
  ttk.Entry(F0,textvariable=LL.FILENAME,justify='left',width=80).\
      grid(row=0,column=1,columnspan=8,sticky='ew')
  F0.grid()

  F1 = ttk.Frame(page1,padding=5)
  ttk.Label(F1,text='Deployment date = ').grid(row=0,column=0,
                                          columnspan=2,
                                          padx=3,sticky='w')

  wdeploy = ttk.Entry(F1,textvariable=Deploy_date,
                         justify='left',width=18)
  wdeploy.grid(row=0,column=2,columnspan=2,sticky='w')
  wdeploy.bind("<Return>", lambda f: _deploytime())

  ttk.Label(F1,text='Recovery date = ').grid(row=0,column=4,
                                             columnspan=2,
                                             padx=3,sticky='w')
  wrecover = ttk.Entry(F1,textvariable=Recover_date,
                          justify='left',width=18)
  wrecover.grid(row=0,column=6,columnspan=2,sticky='w')
  wrecover.bind('<Return>', lambda f: _recovertime())
  F1.grid(sticky='w')

  fig = Figure(dpi=150)
  ax1 = fig.add_subplot(111)
  canvas = FigureCanvasTkAgg(fig,master=page1)
  canvas.show()
  canvas.get_tk_widget().grid(sticky='nsew')
  canvas._tkcanvas.grid()
  ax1.get_xaxis().set_visible(False)
  ax1.get_yaxis().set_visible(False)

  # Bottom menu
  F2 = ttk.Frame(page1,padding=5)
  ttk.Button(F2,text='+',command=_station_up,padding=3).  \
                       grid(row=0,column=0)
  ttk.Label(F2,text='station = ',padding=3).grid(row=0,column=1)
  wstat = ttk.Entry(F2,textvariable=Station_pointer)
  wstat.grid(row=0,column=2,columnspan=2)
  wstat.bind('<Return>', lambda f: station_manual())
  ttk.Label(F2,text='/ ',padding=3).grid(row=0,column=4)
  ttk.Entry(F2,textvariable=NRECORDS,
            state='readonly').grid(row=0,column=5,columnspan=2)
  ttk.Checkbutton(F2,text='Reject',command=_reject_this, \
                 variable=REJECT[Station_pointer.get()]).grid(row=0,column=7)
  ttk.Button(F2,text='Reject stations before this', \
               command=_reject_before).grid(row=0,column=8,columnspan=2)
  ttk.Button(F2,text='Reject stations after this', \
               command=_reject_after).grid(row=0,column=10,columnspan=2)


  ttk.Button(F2,text='-',command=_station_down,padding=3). \
                      grid(row=1,column=0)
  ttk.Label(F2,text='Date = ',padding=3).grid(row=1,column=1)
  ttk.Entry(F2,value='Katachin').grid(row=1,column=2,columnspan=2)
  #ttk.Entry(F2,textvariable=Station_date).grid(row=1,column=2,columnspan=2)
  #ttk.Label(F2,text='Longitude = ',padding=3).grid(row=1,column=4)
  #ttk.Entry(F2,textvariable=Station_lon).grid(row=1,column=5,columnspan=2)
  #ttk.Label(F2,text='Latitude = ',padding=3).grid(row=1,column=7)
  #ttk.Entry(F2,textvariable=Station_lat).grid(row=1,column=8,columnspan=2)
  #ttk.Label(F2,text='Speed = ',padding=3).grid(row=1,column=10)
  #ttk.Entry(F2,textvariable=Station_speed).grid(row=1,column=11,columnspan=2)

  ttk.Button(F2,text='Purge',command=_purge).grid(row=2,column=10)
  ttk.Button(F2,text='Save as',command=_save).grid(row=2,column=11)
  ttk.Button(F2,text='Close',command=_close).grid(row=2,column=12)
  F2.grid(sticky='ew')

  nb.grid()
  #win.wait_window(win)

def main():
# =========

  root = tk.Tk()
  nn = filedialog.askopenfile()
  if nn is None:
    quit()
 
  filename = '%s' % nn.name
  #filename = 'trajectory_20171122.nc'
  #filename = 'histo-300234060640350.txt'
  root.title(filename)
  root.resizable(width=False,height=False)
  root.rowconfigure(0,weight=1)
  tr = parameters()
  tr.Read(filename)
  ShowData(root,tr)
  root.mainloop()

if __name__ == '__main__':
  main()


 
