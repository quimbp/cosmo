''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems'''

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkinter import filedialog
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  import tkFileDialog as filedialog

import dateutil.parser as dparser
import numpy as np
import datetime

from cosmo import *
import lineplot

__version__ = "1.1"
__author__  = "Quim Ballabrera"
__date__    = "July 2017"

class FLOAT_CLASS():
# ===================
  ''' Parameters for CODAR stations'''
  
  __version__ = "1.2"
  __author__  = "Quim Ballabrera"
  __date__    = "July 2017"

  # Version 1.0 (June 2017) : Initial version
  # Version 1.1 (July 2017) : Allows multiple time steps in CODAR file
  # Version 1.2 (December 2017) : Update path names

  def __init__ (self):
    self.FILENAME      = tk.StringVar()
    self.I             = tk.IntVar()
    self.L             = tk.IntVar()

    self.PLOT          = lineplot.parameters()
    self.nfloats       = None
    self.nrecords      = None
    self.lon           = None
    self.lat           = None
    self.date          = None
    self.speed         = None
    self.I.set(0)
    self.L.set(0)
    self.PLOT.LINE_COLOR.set('red')


# =========
class Read:
# =========
  '''Opens and reads a trajectory file'''

  __version__ = "0.1"
  __author__  = "Quim Ballabrera"
  __date__    = "December 2017"

  def __init__ (self,filename):

    if filename.lower().endswith(('.nc','.cdf','.ncdf')):
      self = self.read_trajectory_ncdf(filename)

    elif filename.lower().endswith(('.geojson','.json')):
      self = self.read_trajectory_json(filename)

    elif filename.lower().endswith(('.txt')):
      self = self.read_trajectory_txt(filename)

    elif filename.lower().endswith(('.csv')):
      print('csv: not yet coded')
      self.nfloats  = None
      self.nrecords = None
      return

    elif filename.lower().endswith(('.dat','.data')):
      print('ascii data: not yet coded')
      self.nfloats  = None
      self.nrecords = None
      return

  # --------------------------------------
  def read_trajectory_ncdf(self,filename):
  # --------------------------------------
    '''Read a set of trajectories from a netcdf file'''
    from netCDF4 import Dataset

    print('Reading netcdf file ',filename)
    ncid = Dataset(filename)
    self.nfloats  = ncid.dimensions['floats'].size
    self.nrecords = ncid.dimensions['time'].size
    self.lon  = ncid.variables['lon'][:,:]           # Buoys, Time
    self.lat  = ncid.variables['lat'][:,:]           # Buoys, Time
    TT        = ncid.variables['system_date']        #        Time

    # In python 3.x, the read-in characters are bytes.
    # Use the decode function to decode the input, a byte at a time.
    #
    self.date = []
    for i in range(self.nrecords):
        a = TT[i,:]
        b = ''.join(a[e].decode('utf-8') for e in range(14))
        c = str("%s-%s-%s:%s:%s" % (b[0:4],b[4:6],b[6:11],b[11:13],b[13:15]))
        d = dparser.parse(c)
        self.date.append(d)

    self.FILENAME      = tk.StringVar()
    self.I             = tk.IntVar()
    self.L             = tk.IntVar()
    self.PLOT          = lineplot.parameters()
    self.FILENAME.set(filename)
    self.I.set(0)
    self.L.set(0)

  # --------------------------------------
  def read_trajectory_json(self,filename):
  # --------------------------------------
    '''Read a trajectory from a json file'''
    import json

    print('Reading json file ',filename)
    with open(filename) as datafile:
      DATA = json.load(datafile)

    nf = len(DATA["features"])
    for i in range(nf):
      if DATA["features"][i]["geometry"]["type"] == "LineString":
        POINTS = DATA["features"][i]["geometry"]["coordinates"]
        DATES  = DATA["features"][i]["properties"]["time"]["data"]

    self.nfloats  = 1
    self.nrecords = len(DATES)
    self.lon = [POINTS[j][0] for j in range(self.nrecords)]
    self.lat = [POINTS[j][1] for j in range(self.nrecords)]
    self.date = []
    for i in range(self.nrecords):
      d = dparser.parse(DATES[i])
      self.date.append(d)

    self.FILENAME      = tk.StringVar()
    self.I             = tk.IntVar()
    self.L             = tk.IntVar()
    self.PLOT          = lineplot.parameters()
    self.FILENAME.set(filename)
    self.I.set(0)
    self.L.set(0)

  # --------------------------------------
  def read_trajectory_txt(self,filename):
  # --------------------------------------
    '''Read a trajectory from a txt file'''

    print('Reading txt file ',filename)

    self.lon  = []
    self.lat  = []
    self.date = []
    with open(filename) as datafile:
      for line in datafile.readlines():
        line = line.strip()
        columns = line.split(',')
        self.lon.append(float(columns[2]))
        self.lat.append(float(columns[1]))
        year   = int(columns[3])
        month  = int(columns[4])
        day    = int(columns[5])
        hour   = int(columns[6])
        minute = int(columns[7])
        second = int(columns[8])
        self.date.append(datetime.datetime(year,month,day, \
                                           hour,minute,second))
    self.nfloats  = 1
    self.nrecords = len(self.lon)
    self.FILENAME      = tk.StringVar()
    self.I             = tk.IntVar()
    self.L             = tk.IntVar()
    self.PLOT          = lineplot.parameters()
    self.FILENAME.set(filename)
    self.I.set(0)
    self.L.set(0)



# ======================================
def drawing(fig,ax,m,FLT):
# ======================================
  ''' Draw a 2D vector plot. Option of vectors or stream function'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  if FLT.nfloats > 1:
    for i in range(FLT.nfloats):       # Loop over buoys
      xx,yy = m(FLT.lon[:,i],FLT.lat[:,i])
      if FLT.PLOT.LINE_SHOW.get():
        m.plot(xx,yy,FLT.PLOT.LINE_STYLE.get(),     \
               linewidth=FLT.PLOT.LINE_WIDTH.get(), \
               color=FLT.PLOT.LINE_COLOR.get())
      if FLT.PLOT.MARKER_SHOW.get():
        m.plot(xx,yy,FLT.PLOT.MARKER_STYLE.get(),   \
               ms=FLT.PLOT.INITIAL_SIZE.get(), \
               color=FLT.PLOT.LINE_COLOR.get())
      if FLT.PLOT.INITIAL_SHOW.get():
        m.plot(xx[0],yy[0],                    \
               FLT.PLOT.INITIAL_STYLE.get(),  \
               ms=FLT.PLOT.INITIAL_SIZE.get(), \
               color=FLT.PLOT.INITIAL_COLOR.get())
      if FLT.PLOT.ONMAP.get():
        L = FLT.L.get()
        xx,yy = m(FLT.MAPX[i][L],FLT.MAPY[i][L])
        m.plot(xx,yy,  \
               FLT.PLOT.ONMAP_STYLE.get(),     \
               ms=FLT.PLOT.ONMAP_SIZE.get(),    \
               color=FLT.PLOT.ONMAP_COLOR.get())
  else:
    xx,yy = m(FLT.lon,FLT.lat)
    if FLT.PLOT.LINE_SHOW.get():
      m.plot(xx,yy,FLT.PLOT.LINE_STYLE.get(),     \
             linewidth=FLT.PLOT.LINE_WIDTH.get(), \
             color=FLT.PLOT.LINE_COLOR.get())
    if FLT.PLOT.MARKER_SHOW.get():
      m.plot(xx,yy,FLT.PLOT.MARKER_STYLE.get(),   \
             ms=FLT.PLOT.INITIAL_SIZE.get(), \
             color=FLT.PLOT.LINE_COLOR.get())
    if FLT.PLOT.INITIAL_SHOW.get():
      m.plot(xx[0],yy[0],                    \
             FLT.PLOT.INITIAL_STYLE.get(),  \
             ms=FLT.PLOT.INITIAL_SIZE.get(), \
             color=FLT.PLOT.INITIAL_COLOR.get())
    if FLT.PLOT.ONMAP.get():
      L = FLT.L.get()
      xx,yy = m(FLT.MAPX[L],FLT.MAPY[L])
      m.plot(xx,yy,  \
             FLT.PLOT.ONMAP_STYLE.get(),     \
             ms=FLT.PLOT.ONMAP_SIZE.get(),    \
             color=FLT.PLOT.ONMAP_COLOR.get())



# ======================================
def ShowData(master,LL):
# ======================================
  ''' Shows data from a LAgrangian Trajectory'''

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

    if LL.nfloats == 1:
      for l in range(LL.nrecords):
        string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {} \n'.format(l, \
                                                   LL.lon[l],     \
                                                   LL.lat[l],     \
                                                   LL.date[l])
        log.insert('end',string)
    else:
      i = int(LL.I.get())
      for l in range(LL.nrecords):
        string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {} \n'.format(l, \
                                                   LL.lon[l][i],  \
                                                   LL.lat[l][i],  \
                                                   LL.date[l])
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


def main():

  root = tk.Tk()
  nn = filedialog.askopenfile()
  if nn is None:
    quit()
 
  filename = '%s' % nn.name
  #filename = 'trajectory_20171122.nc'
  #filename = 'histo-300234060640350.txt'
  root.title(filename)
  root.resizable(width=False,height=True)
  root.rowconfigure(0,weight=1)
  tr = Read(filename)
  ShowData(root,tr)
  root.mainloop()

if __name__ == '__main__':
  main()


 
