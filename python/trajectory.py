# trajectory.py
# Visual analysis of Lagrangian trajectories 
# Input: geojson file
# Output (optional): A json file with the same structure as the input file
#
import tkinter as tk
from tkinter import ttk
from tkinter import filedialog
from tkinter import messagebox
import dateutil.parser as dparser
import json

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.figure import Figure
from mpl_toolkits.basemap import Basemap


PROGNAME = 'TRAJECTORY'
VERSION = '0.2 (December 2017)'
AUTHOR = 'Quim Ballabrera (ICM/CSIC)'


import datetime
import sys
from math import radians, cos, sin, asin, sqrt

def truncation(x):
# ================
  '''Function to truncate a real number to a limited set of 
     digits. For example truncation(0.23456) = 0.2 '''

  __version__ = "1.1"
  __author__  = "Quim Ballabrera"
  __date__    = "November 2017"

  import numpy as np

  x1 = int(x)
  x2 = x - x1
  factor = 10**np.round(np.log(np.absolute(x2))/np.log(10))
  if factor > np.absolute(x2):
    factor = 0.1*factor

  return x1 + np.round(x2/factor)*factor


def empty(string):
# ================
  '''Logical function that checks if a string is empty:
     empty('     ') = True'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "June 2017"

  if bool(string.strip()):
    return False
  else:
    return True


def haversine(point1,point2):
# ===========================
  ''' Calculate the great circle distance between two points
       on the earth given as couples of decimal degrees (LON, LAT)'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "December 2017"

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
   



class GUI:
  ''' Launches the main canvas, widgets and dialogues'''

  def __init__(self,master):

    self.master = master
    self.master.protocol('WM_DELETE_WINDOW',self.close)


    # GEOJSON structure
    self.DATA = None
    self.Nfeatures = 0

    # Trajectory info
    self.Trajectory_name   = tk.StringVar()
    self.Trajectory_length = tk.IntVar()
    self.Trajectory_name.set('')
    self.Trajectory_length.set(0)
    self.Trajectory_read     = False
    self.Trajectory_POINTS   = None
    self.Trajectory_date     = None
    self.Trajectory_lon      = None
    self.Trajectory_lat      = None
    self.Trajectory_time     = None
    self.Trajectory_distance = None
    self.Trajectory_speed    = None
    self.Trajectory_reject   = None

    # Map info
    self.Mapxmin           = tk.DoubleVar()
    self.Mapxmax           = tk.DoubleVar()
    self.Mapymin           = tk.DoubleVar()
    self.Mapymax           = tk.DoubleVar()
    self.aspectratio       = False

    self.Deploy_date       = tk.StringVar()
    self.Recover_date       = tk.StringVar()
    self.Deploy_date.set('')
    self.Recover_date.set('')

    # Info about the selected stations
    self.Station_date      = tk.StringVar()
    self.Station_lon       = tk.DoubleVar()
    self.Station_lat       = tk.DoubleVar()
    self.Station_speed     = tk.DoubleVar()
    self.Station_pointer   = tk.IntVar()
    self.Station_reject    = tk.IntVar()
    self.Station_date.set('')
    self.Station_lon.set(None)
    self.Station_lat.set(None)
    self.Station_lon.set(None)
    self.Station_speed.set(None)
    self.Station_pointer.set(None)
    self.Station_reject.set(False)
    self.map_size = 1.0

    # Info about events 0 and 1
    self.serial_number = tk.StringVar()
    self.buoy_name = tk.StringVar()
    self.source = tk.StringVar()
    self.owner = tk.StringVar()
    self.contact = tk.StringVar()
    self.time_units = tk.StringVar()
    self.event0_lon = tk.DoubleVar()
    self.event0_lat = tk.DoubleVar()
    self.event0_date = tk.StringVar()
    self.event0_qc = tk.IntVar()
    self.event1_lon = tk.DoubleVar()
    self.event1_lat = tk.DoubleVar()
    self.event1_date = tk.StringVar()
    self.event1_qc = tk.IntVar()

    self.serial_number.set(None)
    self.buoy_name.set(None)
    self.source.set(None)
    self.owner.set(None)
    self.contact.set(None)
    self.time_units.set(None)

    self.event0_lon.set(None)
    self.event0_lat.set(None)
    self.event0_date.set(None)
    self.event0_qc.set(None)
    self.event1_lon.set(None)
    self.event1_lat.set(None)
    self.event1_date.set(None)
    self.event1_qc.set(None)

    self.event0_lon_orig = None
    self.event0_lat_orig = None
    self.event0_date_orig = None
    self.event0_qc_orig = None
    self.event1_lon_orig = None
    self.event1_lat_orig = None
    self.event1_date_orig = None
    self.event1_qc_orig = None
    self.buoy_name_orig = None
    self.source_orig = None
    self.owner_orig = None
    self.contact_orig = None
    self.time_units_orig = None


    # -------------------------
    # Design of the main window
    # -------------------------

    # Filename frame (top of the window)

    F0 = ttk.Frame(self.master,padding=5)
    ttk.Label(F0,text='Filename',padding=5).grid(row=0,column=0,sticky='w')
    ttk.Entry(F0,textvariable=self.Trajectory_name,justify='left',width=100). \
        grid(row=0,column=1,sticky='ew')
    ttk.Button(F0,text='Load',state='enabled',command=self.open_geojson,padding=3). \
        grid(row=0,column=14,padx=5,sticky='e')
    F0.grid(row=0,column=0,columnspan=14,sticky='ew')
    F0.grid_columnconfigure(0,weight=0)    # Allows horizontal stretching
    F0.grid_rowconfigure(0,weight=0)

    # Define tabs:
    self.nb = ttk.Notebook(self.master)
    self.page1 = ttk.Frame(self.nb)
    self.page2 = ttk.Frame(self.nb)

    self.nb.add(self.page1,text='Canvas')
    self.nb.add(self.page2,text='Attributes')

    # PAGE 1: CANVAS
    F1 = ttk.Frame(self.page1)

    ttk.Label(F1,text='Deployment date = ',padding=3).grid(row=0,column=0,columnspan=2)
    self.wdeploy = ttk.Entry(F1,textvariable=self.Deploy_date,width=10)
    self.wdeploy.grid(row=0,column=2,columnspan=2,sticky='ew')
    self.wdeploy.bind("<Return>", lambda f: self.deploytime())
    ttk.Label(F1,text='Recovery date = ').grid(row=1,column=0,columnspan=2)
    self.wrecover = ttk.Entry(F1,textvariable=self.Recover_date,width=10)
    self.wrecover.grid(row=1,column=2,columnspan=2,sticky='ew')
    self.wrecover.bind('<Return>', lambda f: self.recovertime())

    ttk.Label(F1,text='West = ',padding=3).grid(row=0,column=4)
    ttk.Entry(F1,textvariable=self.Mapxmax,width=10).grid(row=0,column=5,columnspan=2,sticky='ew')
    ttk.Label(F1,text='East = ',padding=3).grid(row=1,column=4)
    ttk.Entry(F1,textvariable=self.Mapxmin,width=10).grid(row=1,column=5,columnspan=2,sticky='ew')

    ttk.Label(F1,text='South = ',padding=3).grid(row=0,column=7)
    ttk.Entry(F1,textvariable=self.Mapymin,width=10).grid(row=0,column=8,columnspan=2,sticky='ew')
    ttk.Label(F1,text='North = ',padding=3).grid(row=1,column=7)
    ttk.Entry(F1,textvariable=self.Mapymax,width=10).grid(row=1,column=8,columnspan=2,sticky='ew')

    ttk.Button(F1,text='Redraw',command=self.redraw,padding=5).grid(row=0,column=10,rowspan=2,padx=3)
    ttk.Button(F1,text='Zoom in',command=self.zoom_in,padding=5).grid(row=0,column=11,rowspan=2,padx=3)
    ttk.Button(F1,text='Zoom out',command=self.zoom_out,padding=5).grid(row=0,column=12,rowspan=2,padx=3)
    ttk.Button(F1,text='Aspect Ratio',command=self.ratio,padding=5).grid(row=0,column=13,rowspan=2,padx=3)
    ttk.Button(F1,text='Reset',command=self.reset,padding=5).grid(row=0,column=14,rowspan=2,padx=3)

    F1.grid(sticky='nswe')
    F1.grid_rowconfigure(0,weight=0)
    F1.grid_columnconfigure(0,weight=0)

    FC = ttk.Frame(self.page1)
    self.fig = Figure(dpi=100)
    self.ax1 = self.fig.add_subplot(111)
    #self.canvas = FigureCanvasTkAgg(self.fig,master=self.page1)
    self.canvas = FigureCanvasTkAgg(self.fig,master=FC)
    self.canvas.show()
    self.canvas._tkcanvas.grid(sticky='nsew')
    self.canvas._tkcanvas.grid_rowconfigure(0,weight=1)
    self.canvas._tkcanvas.grid_columnconfigure(0,weight=1)
    self.ax1.get_xaxis().set_visible(False)
    self.ax1.get_yaxis().set_visible(False)
    FC.grid(sticky='nswe')
    FC.grid_rowconfigure(0,weight=1)
    FC.grid_columnconfigure(0,weight=1)


    # Bottom menu
    F2 = ttk.Frame(self.page1,padding=5)
    ttk.Button(F2,text='+',command=self.station_up,padding=3).  \
                       grid(row=0,column=0)
    ttk.Label(F2,text='station = ',padding=3).grid(row=0,column=1)
    self.wstat = ttk.Entry(F2,textvariable=self.Station_pointer)
    self.wstat.grid(row=0,column=2,columnspan=2)
    self.wstat.bind('<Return>', lambda f: self.station_manual())
    ttk.Label(F2,text='/ ',padding=3).grid(row=0,column=4)
    ttk.Entry(F2,textvariable=self.Trajectory_length).grid(row=0,column=5,columnspan=2)
    ttk.Checkbutton(F2,text='Reject',command=self.reject_this,variable=self.Station_reject). \
                       grid(row=0,column=7)
    ttk.Button(F2,text='Reject stations before this',command=self.reject_before). \
                       grid(row=0,column=8,columnspan=2)
    ttk.Button(F2,text='Reject stations after this',command=self.reject_after). \
                       grid(row=0,column=11,columnspan=2)


    ttk.Button(F2,text='-',command=self.station_down,padding=3). \
                      grid(row=1,column=0)
    ttk.Label(F2,text='Date = ',padding=3).grid(row=1,column=1)
    ttk.Entry(F2,textvariable=self.Station_date).grid(row=1,column=2,columnspan=2)

    ttk.Label(F2,text='Longitude = ',padding=3).grid(row=1,column=4)
    ttk.Entry(F2,textvariable=self.Station_lon).grid(row=1,column=5,columnspan=2)
    ttk.Label(F2,text='Latitude = ',padding=3).grid(row=1,column=7)
    ttk.Entry(F2,textvariable=self.Station_lat).grid(row=1,column=8,columnspan=2)
    ttk.Label(F2,text='Speed = ',padding=3).grid(row=1,column=10)
    ttk.Entry(F2,textvariable=self.Station_speed).grid(row=1,column=11,columnspan=2)

    ttk.Button(F2,text='Purge',command=self.purge).grid(row=2,column=10)
    ttk.Button(F2,text='Save as',command=self.save).grid(row=2,column=11)
    ttk.Button(F2,text='Close',command=self.close).grid(row=2,column=12)
    F2.grid(sticky='ew')
    F2.grid_rowconfigure(0,weight=0)
    F2.grid_columnconfigure(0,weight=0)

    # PAGE 2: ATTRIBUTES
    ttk.Label(self.page2,text='Properties',padding=3,font='Helvetical 12 bold').grid(row=0,column=0)
    ttk.Label(self.page2,text='Serial number',padding=3).grid(row=1,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.serial_number,state='disabled').grid(row=1,column=2,columnspan=3)
    ttk.Button(self.page2,text='Restore',command=self.restore,padding=3).grid(row=1,column=6,padx=5)
    ttk.Label(self.page2,text='Name',padding=3).grid(row=2,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.buoy_name).grid(row=2,column=2,columnspan=3)
    ttk.Label(self.page2,text='Source',padding=3).grid(row=3,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.source).grid(row=3,column=2,columnspan=3)
    ttk.Label(self.page2,text='Owner',padding=3).grid(row=4,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.owner).grid(row=4,column=2,columnspan=3)
    ttk.Label(self.page2,text='Contact',padding=3).grid(row=5,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.contact).grid(row=5,column=2,columnspan=3)

    ttk.Label(self.page2,text="Initial point ('event':0)",padding=3,font='Helvetical 12 bold').grid(row=6,column=0)
    ttk.Label(self.page2,text='Longitude',padding=3).grid(row=7,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event0_lon).grid(row=7,column=2,columnspan=3)
    ttk.Button(self.page2,text='Update',command=self.update_event0,padding=3).grid(row=7,column=5,padx=5)
    ttk.Label(self.page2,text='Latitude',padding=3).grid(row=8,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event0_lat).grid(row=8,column=2,columnspan=3)
    ttk.Label(self.page2,text='Date',padding=3).grid(row=9,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event0_date).grid(row=9,column=2,columnspan=3)
    ttk.Label(self.page2,text='Date units',padding=3).grid(row=10,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.time_units).grid(row=10,column=2,columnspan=3)
    ttk.Label(self.page2,text='Date QC',padding=3).grid(row=11,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event0_qc).grid(row=11,column=2,columnspan=3)

    ttk.Label(self.page2,text="Final point ('event':1)",padding=3,font='Helvetical 12 bold').grid(row=12,column=0)
    ttk.Label(self.page2,text='Longitude',padding=3).grid(row=13,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event1_lon).grid(row=13,column=2,columnspan=3)
    ttk.Button(self.page2,text='Update',command=self.update_event1,padding=3).grid(row=13,column=5,padx=5)
    ttk.Label(self.page2,text='Latitude',padding=3).grid(row=14,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event1_lat).grid(row=14,column=2,columnspan=3)
    ttk.Label(self.page2,text='Date',padding=3).grid(row=15,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event1_date).grid(row=15,column=2,columnspan=3)
    ttk.Label(self.page2,text='Date units',padding=3).grid(row=16,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.time_units).grid(row=16,column=2,columnspan=3)
    ttk.Label(self.page2,text='Date QC',padding=3).grid(row=17,column=1,padx=5)
    ttk.Entry(self.page2,textvariable=self.event1_qc).grid(row=17,column=2,columnspan=3)


    # PACK THE WHOLE THING

    self.nb.grid(sticky='nswe')
    self.nb.grid_columnconfigure(0,weight=1)
    self.nb.grid_rowconfigure(0,weight=1)

  # ---------------------
  def close(self):
  # ---------------------
    ''' Closing the main widget '''
    messagebox.askquestion('Close','Are you sure?',icon='warning')
    if 'yes':
      quit()


  # ---------------------
  def open_geojson(self):
  # ---------------------
    ''' Opening geojson file '''
    backup = self.Trajectory_name.get()
    try:
      nn = tk.filedialog.askopenfile(filetypes=[('GEOJSON','*.geojson'),('JSON','*.json'),('ALL','*.*')])
      self.Trajectory_name.set(nn.name)
    except:
      self.Trajectory_name.set(backup)

    if not empty(self.Trajectory_name.get()):
      # Read the GEOJSON file using json.load
      # :::::::::::::::::::::::::::::::::::::
      with open(self.Trajectory_name.get()) as data_file:
        self.DATA = json.load(data_file)

      # Split the information into LON, LAT, SST, etc
      # :::::::::::::::::::::::::::::::::::::::::::::
      self.Trajectory_read = True
      self.Nfeatures = len(self.DATA["features"])
      for i in range(self.Nfeatures):
        if self.DATA["features"][i]["geometry"]["type"] == "LineString":
          self.Trajectory_POINTS = self.DATA["features"][i]["geometry"]["coordinates"]
          DATES = self.DATA["features"][i]["properties"]["time"]["data"]
          self.Trajectory_temp = self.DATA["features"][i]["properties"]["sst"]["data"]

      self.Trajectory_length.set(len(DATES))

      self.Trajectory_lon = [self.Trajectory_POINTS[j][0] for j in range(self.Trajectory_length.get())]
      self.Trajectory_lat = [self.Trajectory_POINTS[j][1] for j in range(self.Trajectory_length.get())]

      self.Trajectory_date = []
      self.Trajectory_seconds = []
      for i in range(self.Trajectory_length.get()):
        d = dparser.parse(DATES[i])
        self.Trajectory_date.append(d)
        self.Trajectory_seconds.append(int(d.strftime('%s')))


      # Get travelled distance (in meters)
      # ::::::::::::::::::::::::::::::::::
      self.Trajectory_reject = []
      self.Trajectory_distance = []
      for i in range(self.Trajectory_length.get()):
        self.Trajectory_reject.append(tk.IntVar())
        self.Trajectory_reject[i].set(0)
        if i == 0:
          self.Trajectory_distance.append(float(0))
          dist = 0
        else:
          dr = haversine(self.Trajectory_POINTS[i],self.Trajectory_POINTS[i-1])
          dist += dr
          self.Trajectory_distance.append(dist)

      # Get time between stations
      self.Trajectory_time = []
      for i in range(self.Trajectory_length.get()):
        if i == 0:
          self.Trajectory_time.append(float(0))
        else:
          secs = (self.Trajectory_date[i]-self.Trajectory_date[i-1]).seconds
          self.Trajectory_time.append(secs)

      # Speed required to move between stations
      self.Trajectory_speed = []
      self.Trajectory_speed.append(0)
      for i in range(1,self.Trajectory_length.get()):
        u = (self.Trajectory_distance[i]-self.Trajectory_distance[i-1])/self.Trajectory_time[i]
        self.Trajectory_speed.append((self.Trajectory_distance[i]-self.Trajectory_distance[i-1])/
                                self.Trajectory_time[i])

      self.map_size = 1.0
      self.data_xmin = min(self.Trajectory_lon)
      self.data_xmax = max(self.Trajectory_lon)
      self.data_ymin = min(self.Trajectory_lat)
      self.data_ymax = max(self.Trajectory_lat)
      self.data_xsize = self.data_xmax - self.data_xmin
      self.data_ysize = self.data_ymax - self.data_ymin
      self.Mapxmin.set(truncation(self.data_xmin - self.map_size*self.data_xsize))
      self.Mapxmax.set(truncation(self.data_xmax + self.map_size*self.data_xsize))
      self.Mapymin.set(truncation(self.data_ymin - self.map_size*self.data_ysize))
      self.Mapymax.set(truncation(self.data_ymax + self.map_size*self.data_ysize))
     
      self.Station_pointer.set(0)
      self.Station_date.set(self.Trajectory_date[0])
      self.Station_lon.set(self.Trajectory_lon[0])
      self.Station_lat.set(self.Trajectory_lat[0])
      self.Station_speed.set(self.Trajectory_speed[0])
      self.Station_reject.set(self.Trajectory_reject[0].get())

      self.Deploy_date.set(self.Trajectory_date[0])
      self.Recover_date.set(self.Trajectory_date[self.Trajectory_length.get()-1])
 
      self.ax1.clear()
      self.draw_map()
      self.make_plot()

      # Scan for info about points:
      for i in range(self.Nfeatures):
        if self.DATA["features"][i]["geometry"]["type"] == "Point":
          #print(self.DATA["features"][i]["properties"])
          if self.DATA["features"][i]["properties"]["event"] == 0:
            self.event0_lon.set(self.DATA["features"][i]["geometry"]["coordinates"][0])
            self.event0_lat.set(self.DATA["features"][i]["geometry"]["coordinates"][1])
            self.serial_number.set(self.DATA["features"][i]["properties"]["code_sn"])
            self.buoy_name.set(self.DATA["features"][i]["properties"]["name"])
            self.contact.set(self.DATA["features"][i]["properties"]["contact"])
            self.source.set(self.DATA["features"][i]["properties"]["source"])
            self.owner.set(self.DATA["features"][i]["properties"]["owner"])
            self.event0_date.set(self.DATA["features"][i]["properties"]["time"]["data"][0])
            self.event0_qc.set(self.DATA["features"][i]["properties"]["time"]["qc_data"])
            self.time_units.set(self.DATA["features"][i]["properties"]["time"]["units"])

          if self.DATA["features"][i]["properties"]["event"] == 1:
            self.event1_lon.set(self.DATA["features"][i]["geometry"]["coordinates"][0])
            self.event1_lat.set(self.DATA["features"][i]["geometry"]["coordinates"][1])
            self.event1_date.set(self.DATA["features"][i]["properties"]["time"]["data"][0])
            self.event1_qc.set(self.DATA["features"][i]["properties"]["time"]["qc_data"])
            
      self.buoy_name_orig = self.buoy_name.get()
      self.source_orig = self.source.get()
      self.owner_orig = self.owner.get()
      self.contact_orig = self.contact.get()
      self.time_units_orig = self.time_units.get()

      self.event0_lon_orig = self.event0_lon.get()
      self.event0_lat_orig = self.event0_lat.get()
      self.event0_date_orig = self.event0_date.get()
      self.event0_qc_orig = self.event0_qc.get()

      self.event1_lon_orig = self.event1_lon.get()
      self.event1_lat_orig = self.event1_lat.get()
      self.event1_date_orig = self.event0_date.get()
      self.event1_qc_orig = self.event0_qc.get()


    else:
      self.DATA = None
      self.Trajectory = None
      self.Trajectory_read = False
      self.Nfeatures = 0


  # --------------
  def restore(self):
  # --------------
    '''Restore GEOJSON properties'''
    self.buoy_name.set(self.buoy_name_orig)
    self.source.set(self.source_orig)
    self.owner.set(self.owner_orig)
    self.contact.set(self.contact_orig)
    self.time_units.set(self.time_units_orig)

    self.event0_lon.set(self.event0_lon_orig)
    self.event0_lat.set(self.event0_lat_orig)
    self.event0_date.set(self.event0_date_orig)
    self.event0_qc.set(self.event0_qc_orig)

    self.event1_lon.set(self.event1_lon_orig)
    self.event1_lat.set(self.event1_lat_orig)
    self.event1_date.set(self.event1_date_orig)
    self.event1_qc.set(self.event1_qc_orig)


  # --------------
  def update_event0(self):
  # --------------
    '''Update coordinates and date of the first point'''
    self.event0_lon.set(self.Trajectory_lon[0])
    self.event0_lat.set(self.Trajectory_lat[0])
    self.event0_date.set(self.Trajectory_date[0])
    self.event0_qc.set(1)


  # --------------
  def update_event1(self):
  # --------------
    '''Update coordinates and date of the first point'''
    i = self.Trajectory_length.get()-1
    self.event1_lon.set(self.Trajectory_lon[i])
    self.event1_lat.set(self.Trajectory_lat[i])
    self.event1_date.set(self.Trajectory_date[i])
    self.event1_qc.set(1)


  # --------------
  def purge(self):
  # --------------
    '''Purge rejected stations '''
    for i in reversed(range(self.Trajectory_length.get())):
      if self.Trajectory_reject[i].get() == 1:
        print('Removing station = ',i)
        del self.Trajectory_POINTS[i]
        del self.Trajectory_lon[i]
        del self.Trajectory_lat[i]
        del self.Trajectory_date[i]
        del self.Trajectory_temp[i]

    self.Trajectory_length.set(len(self.Trajectory_lon))
    self.Station_pointer.set(0)

    # Get travelled distance (in meters)
    # ::::::::::::::::::::::::::::::::::
    self.Trajectory_reject = []
    self.Trajectory_distance = []
    for i in range(self.Trajectory_length.get()):
      self.Trajectory_reject.append(tk.IntVar())
      self.Trajectory_reject[i].set(0)
      if i == 0:
        self.Trajectory_distance.append(float(0))
        dist = 0
      else:
        dr = haversine(self.Trajectory_POINTS[i],self.Trajectory_POINTS[i-1])
        dist += dr
        self.Trajectory_distance.append(dist)

    # Get time between stations
    self.Trajectory_time = []
    for i in range(self.Trajectory_length.get()):
      if i == 0:
        self.Trajectory_time.append(float(0))
      else:
        secs = (self.Trajectory_date[i]-self.Trajectory_date[i-1]).seconds
        self.Trajectory_time.append(secs)

    # Speed required to move between stations
    self.Trajectory_speed = []
    self.Trajectory_speed.append(0)
    for i in range(1,self.Trajectory_length.get()):
      u = (self.Trajectory_distance[i]-self.Trajectory_distance[i-1])/self.Trajectory_time[i]
      self.Trajectory_speed.append((self.Trajectory_distance[i]-self.Trajectory_distance[i-1])/
                              self.Trajectory_time[i])

    self.Station_date.set(self.Trajectory_date[self.Station_pointer.get()])
    self.Station_lon.set(self.Trajectory_lon[self.Station_pointer.get()])
    self.Station_lat.set(self.Trajectory_lat[self.Station_pointer.get()])
    self.Station_speed.set(self.Trajectory_speed[self.Station_pointer.get()])
    self.Station_reject.set(self.Trajectory_reject[self.Station_pointer.get()].get())

    self.draw_map()
    self.make_plot()


  # -------------
  def save(self):
  # -------------
    '''Save new file'''

    nn = tk.filedialog.asksaveasfilename(title='Save',confirmoverwrite=True)
    if nn is not None:
      outfile = '%s' % nn

      # The following values may have changed: POINTS, DATE, TEMP
      for i in range(self.Nfeatures):
        if self.DATA["features"][i]["geometry"]["type"] == "LineString":
          self.DATA["features"][i]["geometry"]["coordinates"] = self.Trajectory_POINTS
          self.DATA["features"][i]["properties"]["time"]["data"] = self.Trajectory_date
          self.DATA["features"][i]["properties"]["sst"]["data"] = self.Trajectory_temp

      # The following values may have changed: COORDINATES, PROPERTIES
      for i in range(self.Nfeatures):
        if self.DATA["features"][i]["geometry"]["type"] == "Point":
          if self.DATA["features"][i]["properties"]["event"] == 0:
            self.DATA["features"][i]["geometry"]["coordinates"][0] = self.event0_lon.get()
            self.DATA["features"][i]["geometry"]["coordinates"][1] = self.event0_lat.get()
            self.DATA["features"][i]["properties"]["name"] = self.buoy_name.get()
            self.DATA["features"][i]["properties"]["source"] = self.source.get()
            self.DATA["features"][i]["properties"]["owner"] = self.owner.get()
            self.DATA["features"][i]["properties"]["contact"] = self.contact.get()
            self.DATA["features"][i]["properties"]["time"]["data"][0] = self.event0_date.get()
            self.DATA["features"][i]["properties"]["time"]["units"] = self.time_units.get()
            self.DATA["features"][i]["properties"]["time"]["qc_data"] = self.event0_qc.get()

          if self.DATA["features"][i]["properties"]["event"] == 1:
            self.DATA["features"][i]["geometry"]["coordinates"][0] = self.event1_lon.get()
            self.DATA["features"][i]["geometry"]["coordinates"][1] = self.event1_lat.get()
            self.DATA["features"][i]["properties"]["name"] = self.buoy_name.get()
            self.DATA["features"][i]["properties"]["source"] = self.source.get()
            self.DATA["features"][i]["properties"]["owner"] = self.owner.get()
            self.DATA["features"][i]["properties"]["contact"] = self.contact.get()
            self.DATA["features"][i]["properties"]["time"]["data"][0] = self.event1_date.get()
            self.DATA["features"][i]["properties"]["time"]["units"] = self.time_units.get()
            self.DATA["features"][i]["properties"]["time"]["qc_data"] = self.event1_qc.get()

      with open(outfile,'w') as fp:
        json.dump(self.DATA,fp)
      print('File %s written',outfile)


  # --------------------
  def reject_this(self):
  # --------------------
    '''Rejects a station'''
    if self.Trajectory_read:
      self.Trajectory_reject[self.Station_pointer.get()].set(self.Station_reject.get())


  # ----------------------
  def reject_before(self):
  # ----------------------
    '''Rejects a station'''
    if self.Trajectory_read:
      for i in range(self.Station_pointer.get()):
        self.Trajectory_reject[i].set(1)
      self.make_plot()


  # ----------------------
  def reject_after(self):
  # ----------------------
    '''Rejects a station'''
    if self.Trajectory_read:
      for i in range(self.Station_pointer.get()+1,self.Trajectory_length.get()):
        self.Trajectory_reject[i].set(1)
      self.make_plot()


  # -----------------------
  def recovertime(self):
  # -----------------------
    '''Manual selection of the final period'''
    if self.Trajectory_read:
      dd = self.wrecover.get()
      tt = dparser.parse(dd)
      t0 = int(tt.strftime('%s'))
      for i in range(self.Trajectory_length.get()):
        if self.Trajectory_seconds[i] > t0:
          self.Trajectory_reject[i].set(1)
      self.Station_reject.set(self.Trajectory_reject[self.Station_pointer.get()].get())  
      self.make_plot()


  # -----------------------
  def deploytime(self):
  # -----------------------
    '''Manual selection of the initial period'''
    if self.Trajectory_read:
      dd = self.wdeploy.get()
      tt = dparser.parse(dd)
      t0 = int(tt.strftime('%s'))
      for i in range(self.Trajectory_length.get()):
        if self.Trajectory_seconds[i] < t0:
          self.Trajectory_reject[i].set(1)
      self.Station_reject.set(self.Trajectory_reject[self.Station_pointer.get()].get())  
      self.make_plot()

  # -----------------------
  def station_manual(self):
  # -----------------------
    '''Manual selection of an station'''
    if self.Trajectory_read:
      num = self.wstat.get()
      self.Station_pointer.set(num)
      self.Station_date.set(self.Trajectory_date[self.Station_pointer.get()])
      self.Station_lon.set(self.Trajectory_lon[self.Station_pointer.get()])
      self.Station_lat.set(self.Trajectory_lat[self.Station_pointer.get()])
      self.Station_speed.set(self.Trajectory_speed[self.Station_pointer.get()])
      self.Station_reject.set(self.Trajectory_reject[self.Station_pointer.get()].get())
      self.make_plot()

  # -------------------
  def station_up(self):
  # -------------------
    '''Moves pointer up one'''
    if self.Station_pointer.get() < self.Trajectory_length.get()-1:
      i = self.Station_pointer.get()
      if self.Trajectory_reject[i].get() == 1:
        self.m.plot(self.xx[i], \
                    self.yy[i],'o',ms=4,color='grey')
      else:
        self.m.plot(self.xx[i], \
                    self.yy[i],'o',ms=4,color='white')

      self.Station_pointer.set(self.Station_pointer.get()+1)
      self.m.plot(self.xx[self.Station_pointer.get()], \
                  self.yy[self.Station_pointer.get()],'o',ms=4,color='red')
      self.Station_date.set(self.Trajectory_date[self.Station_pointer.get()])
      self.Station_lon.set(self.Trajectory_lon[self.Station_pointer.get()])
      self.Station_lat.set(self.Trajectory_lat[self.Station_pointer.get()])
      self.Station_speed.set(self.Trajectory_speed[self.Station_pointer.get()])
      self.Station_reject.set(self.Trajectory_reject[self.Station_pointer.get()].get())
      self.canvas.draw()


  # --------------------
  def station_down(self):
  # --------------------
    '''Moves pointer down one'''
    if self.Station_pointer.get() > 0:
      i = self.Station_pointer.get()
      if self.Trajectory_reject[i].get() == 1:
        self.m.plot(self.xx[i], \
                    self.yy[i],'o',ms=4,color='grey')
      else:
        self.m.plot(self.xx[i], \
                    self.yy[i],'o',ms=4,color='white')
      #self.m.plot(self.xx[self.Station_pointer.get()], \
      #            self.yy[self.Station_pointer.get()],'o',ms=4,color='white')
      self.Station_pointer.set(self.Station_pointer.get()-1)
      self.m.plot(self.xx[self.Station_pointer.get()], \
                  self.yy[self.Station_pointer.get()],'o',ms=4,color='red')
      self.Station_date.set(self.Trajectory_date[self.Station_pointer.get()])
      self.Station_lon.set(self.Trajectory_lon[self.Station_pointer.get()])
      self.Station_lat.set(self.Trajectory_lat[self.Station_pointer.get()])
      self.Station_speed.set(self.Trajectory_speed[self.Station_pointer.get()])
      self.Station_reject.set(self.Trajectory_reject[self.Station_pointer.get()].get())
      self.canvas.draw()


  # ---------------
  def reset(self):
  # ---------------
    '''Resets the map limits'''
    self.map_size = 1.0
    self.data_xmin = min(self.Trajectory_lon)
    self.data_xmax = max(self.Trajectory_lon)
    self.data_ymin = min(self.Trajectory_lat)
    self.data_ymax = max(self.Trajectory_lat)
    self.data_xsize = self.data_xmax - self.data_xmin
    self.data_ysize = self.data_ymax - self.data_ymin
    self.Mapxmin.set(truncation(self.data_xmin - self.map_size*self.data_xsize))
    self.Mapxmax.set(truncation(self.data_xmax + self.map_size*self.data_xsize))
    self.Mapymin.set(truncation(self.data_ymin - self.map_size*self.data_ysize))
    self.Mapymax.set(truncation(self.data_ymax + self.map_size*self.data_ysize))

    self.Deploy_date.set(self.Trajectory_date[0])
    self.Recover_date.set(self.Trajectory_date[self.Trajectory_length.get()-1])
    for i in range(self.Trajectory_length.get()):
      self.Trajectory_reject[i].set(0)
    
    self.ax1.clear()
    self.draw_map()
    self.make_plot()


  # ---------------
  def zoom_in(self):
  # ---------------
    '''Zooms in the map'''
    self.map_size = 0.50*self.map_size
    self.Mapxmin.set(self.data_xmin - self.map_size*self.data_xsize)
    self.Mapxmax.set(self.data_xmax + self.map_size*self.data_xsize)
    self.Mapymin.set(self.data_ymin - self.map_size*self.data_ysize)
    self.Mapymax.set(self.data_ymax + self.map_size*self.data_ysize)
    self.draw_map()
    self.make_plot()


  # ---------------
  def zoom_out(self):
  # ---------------
    '''Zooms out the map'''
    self.map_size = self.map_size/0.50
    self.Mapxmin.set(self.data_xmin - self.map_size*self.data_xsize)
    self.Mapxmax.set(self.data_xmax + self.map_size*self.data_xsize)
    self.Mapymin.set(self.data_ymin - self.map_size*self.data_ysize)
    self.Mapymax.set(self.data_ymax + self.map_size*self.data_ysize)
    self.draw_map()
    self.make_plot()

  def ratio(self):
    '''Redraws the map'''
    self.aspectratio = not self.aspectratio
    self.draw_map()
    self.make_plot()

  def redraw(self):
    '''Redraws the map'''
    self.draw_map()
    self.make_plot()

  def draw_map(self):
    '''Draws the map'''
    self.ax1.clear()
    projection = 'cyl'
    projection = 'merc'
    self.m = Basemap(                                 \
                #projection=projection,               \
                #resolution='i',                      \
                llcrnrlat=self.Mapymin.get(),         \
                urcrnrlat=self.Mapymax.get(),         \
                llcrnrlon=self.Mapxmin.get(),         \
                urcrnrlon=self.Mapxmax.get(),         \
                fix_aspect=self.aspectratio,          \
                ax=self.ax1)
    self.m.arcgisimage(service='ESRI_Imagery_World_2D', xpixels = 1000, verbose= True)

    self.xx,self.yy = self.m(self.Trajectory_lon,self.Trajectory_lat)
    self.m.plot(self.xx[self.Station_pointer.get()], \
                self.yy[self.Station_pointer.get()],'o',ms=4,color='red')


  def make_plot(self):
    '''Draws the trajectory over the map'''

    self.m.plot(self.xx,self.yy,'--',linewidth=0.8,color='grey')
    #self.m.plot(self.xx,self.yy,'o',ms=4,linewidth=1,color='white')

    for i in range(self.Trajectory_length.get()):
      if self.Trajectory_reject[i].get() == 1:
        self.m.plot(self.xx[i],self.yy[i],'o',ms=4,linewidth=1,color='grey')
      else:
        self.m.plot(self.xx[i],self.yy[i],'o',ms=4,linewidth=1,color='white')

    self.m.plot(self.xx[self.Station_pointer.get()], \
                self.yy[self.Station_pointer.get()],'o',ms=4,color='red')

    self.canvas.draw()


def main():
  root = tk.Tk()
  root.title(PROGNAME)
  #root.resizable(width=True,height=True)
  root.grid_columnconfigure(0,weight=1)
  root.grid_rowconfigure(0,weight=1)
  app = GUI(root)
  root.mainloop()

if __name__ == '__main__':
  main()
