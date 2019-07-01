
__version__ = "1.0"
__author__  = "Quim Ballabrera"
__date__    = "December 2017"

import sys
import os
from os.path import isfile, join
import numpy as np
import numpy.ma as ma
from scipy import interpolate

import json
import io
#import matplotlib
#matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.image as image
import matplotlib.font_manager
import ast
import math
from matplotlib.font_manager import FontProperties
from matplotlib.figure import Figure
from matplotlib.offsetbox import TextArea, OffsetImage, AnnotationBbox
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib import cm as CM
from matplotlib import colors
from mpl_toolkits.basemap import Basemap
from netCDF4 import Dataset,num2date
from itertools import chain

from PIL import Image, ImageTk
import matplotlib.animation as manimation

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkinter import filedialog as filedialog
  from tkcolorpicker import askcolor
  from tkinter import font as tkfont
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  import tkFileDialog as filedialog
  from tkColorChooser import askcolor
  import tkFont as tkfont

try:
  to_unicode = unicode
except:
  to_unicode = str

import cosmo.tools as tools
import cosmo.contourplot as contourplot
import cosmo.vectorplot as vectorplot
import cosmo.providers as providers
import cosmo.codar as codar
import cosmo.copernicus as copernicus
import cosmo.saidin as saidin
import cosmo.lagrangian as lagrangian
import cosmo.lineplot as lineplot
import cosmo.blm as blm
import cosmo.mlm as mlm
import cosmo.db as db
import cosmo.geomarker as geomarker
import cosmo.dotplot as dotplot
import cosmo.json_editor as jeditor
import cosmo.legend as legend

from cosmo.tools import empty
from cosmo.tools import myround
from cosmo.tools import exists
from cosmo.tools import askforpermission
from cosmo.tools import placeontop
from cosmo.tools import get_remote
from cosmo.tools import get_Date
from cosmo.tools import folderList
from cosmo.tools import urlList
from cosmo.tools import simple_form
from cosmo.tools import haversine
from cosmo.tools import fontconfig
from cosmo.tools import setfont
from cosmo.tools import read_lines
from cosmo import COSMO_CONF_NAME
from cosmo import COSMO_CONF
from cosmo import COSMO_ROOT
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA
from cosmo import VERSION

global COSMO_CONF,COSMO_CONF_PATH,COSMO_CONF_NAME,COSMO_CONF_DATA

print('COSMO_CONF_DATA = ', COSMO_CONF_DATA)
print('COSMO_CONF = ', COSMO_CONF)

BGC  = 'pale green'    # Background color
BWC  = 'lime green'    # Buttons (PREV and NEXT) color
EBC  = 'forest green'  # Exit Buttons color
FONT = 'Helvetica 14'  # Default font

# =====================
class fld_parameters():
# =====================
  ''' Class for 2D data fields'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "December 2017"

  def __init__ (self):
  # ==================
    ''' Define and initialize the class attributes '''

    self.PLOT          = contourplot.parameters()
    self.missing       = tk.DoubleVar()
    self.masked        = tk.BooleanVar()
    self.show          = tk.BooleanVar()
    self.masked.set(True)
    self.show.set(True)
    self.F             = None
    self.minval        = None
    self.maxval        = None
    self.mask          = None
    self.data          = None
    self.varname       = None
    self.units         = None
    self.missing_value = None
    self.cbar          = None

  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes '''

    conf = {}
    conf['MISSING'] = self.missing.get()
    conf['MASKED'] = self.masked.get()
    conf['SHOW'] = self.show.get()
    conf['PLOT'] = self.PLOT.conf_get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class dictionary from class attributes '''

    self.missing.set(conf['MISSING'])
    self.masked.set(conf['MASKED'])
    self.show.set(conf['SHOW'])
    self.PLOT.conf_set(conf['PLOT'])


# =====================
class vel_parameters():
# =====================
  ''' Class for 2D velocity fields'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "December 2017"

  def __init__ (self):
  # ==================
    ''' Define and initialize the class attributes '''

    self.PLOT          = vectorplot.parameters()
    self.u             = None
    self.v             = None
    self.speed         = None
    self.F             = None
    self.cbar          = None
    self.show          = tk.BooleanVar()
    self.show.set(True)

  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes '''

    conf = {}
    conf['SHOW'] = self.show.get()
    conf['PLOT'] = self.PLOT.conf_get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class dictionary from class attributes '''

    self.show.set(conf['SHOW'])
    self.PLOT.conf_set(conf['PLOT'])



# =====================
class cdf_parameters():
# =====================
  ''' Class for NetCDF files'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "December 2017"

  def __init__ (self):
  # ==================
    ''' Define and initialize the class attributes '''

    self.FILENAME      = tk.StringVar()
    self.varname       = tk.StringVar()
    self.uname         = tk.StringVar()
    self.vname         = tk.StringVar()
    self.K             = tk.IntVar()
    self.L             = tk.IntVar()
    self.ncid          = None
    self.icdf          = None
    self.varid         = None
    self.uid           = None
    self.vid           = None
    self.K_LIST        = []
    self.L_LIST        = []
    self.Z_LIST        = []
    self.T_LIST        = []
    self.DATE          = []
    self.TIME          = []
    self.K.set(0)
    self.L.set(0)
    self.FIELD         = None
    self.VEL           = None
    self.lon           = None
    self.lat           = None
    self.xx            = None
    self.yy            = None
    self.varname.set('')
    self.uname.set('')
    self.vname.set('')

  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes'''

    conf = {}
    #conf['FILENAME'] = self.FILENAME.get()
    conf['varname'] = self.varname.get()
    conf['uname'] = self.uname.get()
    conf['vname'] = self.vname.get()
    conf['K'] = self.K.get()
    conf['L'] = self.L.get()
    conf['varid'] = self.varid
    conf['uid'] = self.uid
    conf['vid'] = self.vid

    if self.icdf is None:
      conf['ICDF'] = None
    else:
      conf['ICDF'] = self.icdf.conf_get()

    if self.FIELD is None:
      conf['FIELD'] = None
    else:
      conf['FIELD'] = self.FIELD.conf_get()

    if self.VEL is None:
      conf['VEL'] = None
    else:
      conf['VEL'] = self.VEL.conf_get()

    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from dictionary '''

    #self.FILENAME.set(conf['FILENAME'])
    self.varname.set(conf['varname'])
    self.uname.set(conf['uname'])
    self.vname.set(conf['vname'])
    self.K.set(conf['K'])
    self.L.set(conf['L'])
    self.varid = conf['varid']
    self.uid = conf['uid']
    self.vid = conf['vid']
    if conf['ICDF'] == "None":
      pass
    else:
      self.icdf.conf_set(conf['ICDF'])

    if self.FIELD is None:
      pass
    else:
      self.FIELD.conf_set(conf['FIELD'])

    if self.VEL is None:
      pass
    else:
      self.VEL.conf_set(conf['VEL'])





# ====================
class DrawingConfig():
# ====================

  def __init__(self):
  # =================

    #fileconf = '%s' % COSMO_CONF + 'configure.conf'
    #self.CONFLIST = []
    #if exists(fileconf):
    #  f = open(fileconf,'r')
    #  for line in f:
    #    self.CONFLIST.append('%s' % line[0:-1])
    #  f.close()
    #  self.FILECONF = '%s' % self.CONFLIST[0]
    #else:
    #  self.FILECONF           = '%s' % COSMO_CONF + 'drawing.conf'
    #  f = open(fileconf,'w')
    #  f.write(self.FILECONF)
    #  f.close()
    #  self.CONFLIST.append([self.FILECONF])

    self.FILECONF           = '%s' % COSMO_CONF + 'drawing.conf'
    self.VERSION            = __version__
    self.OUTPUT_FIGURE      = tk.BooleanVar()
    self.OUTPUT_LEAFLET     = tk.BooleanVar()
    self.GEOMAP             = tk.BooleanVar()
    self.MAP_PROJECTION     = tk.StringVar()
    self.MAP_RESOLUTION     = tk.StringVar()
    self.EPSG               = tk.IntVar()
    self.SOUTH              = tk.DoubleVar()
    self.NORTH              = tk.DoubleVar()
    self.WEST               = tk.DoubleVar()
    self.EAST               = tk.DoubleVar()
    self.WIDTH              = tk.DoubleVar()
    self.HEIGHT             = tk.DoubleVar()
    self.LAT_0              = tk.DoubleVar()
    self.LON_0              = tk.DoubleVar()
    self.SATELLITE_HEIGHT   = tk.DoubleVar()

    self.COASTLINE_SHOW     = tk.BooleanVar()
    self.COASTLINE_WIDTH    = tk.DoubleVar()
    self.COASTLINE_COLOR    = tk.StringVar()
    self.COUNTRYLINE_SHOW   = tk.BooleanVar()
    self.COUNTRYLINE_WIDTH  = tk.DoubleVar()
    self.COUNTRYLINE_COLOR  = tk.StringVar()
    self.LAND_COLOR         = tk.StringVar()
    self.WATER_COLOR        = tk.StringVar()

    self.TITLE              = tk.StringVar()
    self.TITLEFONT          = FontProperties().copy()
    self.TITLE_PAD          = tk.DoubleVar()

    self.XLABEL             = tk.StringVar()
    self.YLABEL             = tk.StringVar()
    self.LABEL_SIZE         = tk.IntVar()
    self.LABEL_PAD          = tk.IntVar()
    self.ZLABEL             = tk.StringVar()
    self.TLABEL             = tk.StringVar()

    self.DPI                = tk.IntVar()
    self.OUT_FILENAME       = None
    self.FIGURE_COLOR       = tk.StringVar()
    self.TEXT_COLOR         = tk.StringVar()

    self.GRID_SHOW          = tk.BooleanVar()
    self.GRID_LINEWIDTH     = tk.DoubleVar()
    self.MERIDIAN_INI       = tk.DoubleVar()
    self.MERIDIAN_FIN       = tk.DoubleVar()
    self.MERIDIAN_INT       = tk.DoubleVar()
    self.PARALLEL_INI       = tk.DoubleVar()
    self.PARALLEL_FIN       = tk.DoubleVar()
    self.PARALLEL_INT       = tk.DoubleVar()
    self.GRID_COLOR         = tk.StringVar()
    self.GRID_FONTCOLOR     = tk.StringVar()
    self.GRID_SIZE          = tk.IntVar()
    self.GRID_NORTH         = tk.BooleanVar()
    self.GRID_SOUTH         = tk.BooleanVar()
    self.GRID_WEST          = tk.BooleanVar()
    self.GRID_EAST          = tk.BooleanVar()
    self.GRID_LINESTYLE     = tk.StringVar()
    self.GRID_ALPHA         = tk.DoubleVar()

    self.SCALE_SHOW         = tk.BooleanVar()
    self.SCALE_X            = tk.DoubleVar()
    self.SCALE_Y            = tk.DoubleVar()
    self.SCALE_XO           = tk.DoubleVar()
    self.SCALE_YO           = tk.DoubleVar()
    self.SCALE_LENGTH       = tk.DoubleVar()
    self.SCALE_UNITS        = tk.StringVar()
    self.SCALE_STYLE        = tk.StringVar()
    self.SCALE_FONTSIZE     = tk.IntVar()
    self.SCALE_FONTCOLOR    = tk.StringVar()
    self.SCALE_LABELSTYLE   = tk.StringVar()
    self.SCALE_FORMAT       = tk.StringVar()
    self.SCALE_YOFFSET      = tk.DoubleVar()
    self.SCALE_FILLCOLOR1   = tk.StringVar()
    self.SCALE_FILLCOLOR2   = tk.StringVar()
    self.SCALE_LINECOLOR    = tk.StringVar()
    self.SCALE_LINEWIDTH    = tk.IntVar()

    self.X                  = None
    self.Y                  = None
    self.RELIEF             = tk.BooleanVar()
    self.BLUEMARBLE         = tk.BooleanVar()
    self.ETOPO              = tk.BooleanVar()
    self.BACKGROUND_SCALE   = tk.DoubleVar()
    self.RIVERS_SHOW        = tk.BooleanVar()
    self.RIVERS_WIDTH       = tk.DoubleVar()
    self.RIVERS_COLOR       = tk.StringVar()
    self.ARCGISIMAGE        = tk.IntVar()
    self.ARCGISSERVICE      = tk.StringVar()
    self.ARCGISSERVICE_LIST = ['ESRI_Imagery_World_2D', \
                                'ESRI_StreetMap_World_2D', \
                                'NatGEo_World_Map',  \
                                'Ocean_Basemap',  \
                                'World_Imagery',  \
                                'World_Physical_Map',  \
                                'World_Shaded_Relief',  \
                                'World_Street_Map',  \
                                'World_Terrain_Base',  \
                                'World_Topo_Map']
    self.ARCGISPIXELS       = tk.IntVar()
    self.ARCGISDPI          = tk.IntVar()
    self.ARCGISVERBOSE      = tk.BooleanVar()
    self.LOGO_FILE          = tk.StringVar()
    self.LOGO_ZOOM          = tk.DoubleVar()
    self.LOGO_LOCATION      = tk.StringVar()
    self.LOGO_X             = tk.DoubleVar()
    self.LOGO_Y             = tk.DoubleVar()
    self.LOGO_DISPLAY       = tk.BooleanVar()
    self.TIMESTAMP_SHOW     = tk.BooleanVar()
    self.TIMESTAMP_BOLD     = tk.BooleanVar()
    self.TIMESTAMP_X        = tk.DoubleVar()
    self.TIMESTAMP_Y        = tk.DoubleVar()
    self.TIMESTAMP_SIZE     = tk.IntVar()
    self.TIMESTAMP_COLOR    = tk.StringVar()
    self.VIDEO_NAME         = tk.StringVar()
    self.VIDEO_TITLE        = tk.StringVar()
    self.VIDEO_AUTHOR       = tk.StringVar()
    self.VIDEO_COMMENT      = tk.StringVar()
    self.VIDEO_FPS          = tk.IntVar()
    self.VIDEO_DPI          = tk.IntVar()
    self.VIDEO_L1           = tk.IntVar()
    self.VIDEO_L2           = tk.IntVar()
    self.WINDOW_FONT_TYPE  = tk.StringVar()
    self.WINDOW_FONT_SIZE  = tk.IntVar()
    self.MAP_FONT_TYPE     = tk.StringVar()

    self.LEGEND            = legend.LegendConfig()
    self.LEGEND.SHOW.set(False)

    self.SIZE = [9,6]
    self.OUTPUT_FIGURE.set(True)
    self.OUTPUT_LEAFLET.set(False)
    self.GEOMAP.set(True)
    self.MAP_PROJECTION.set('cyl')
    self.MAP_RESOLUTION.set('l')
    self.EPSG.set(4326)
    self.SOUTH.set(-90)
    self.NORTH.set(90)
    self.WEST.set(-180)
    self.EAST.set(180)
    self.WIDTH.set(0)
    self.HEIGHT.set(0)
    self.LAT_0.set(0)
    self.LON_0.set(0)
    self.SATELLITE_HEIGHT.set(35786000)
    self.COASTLINE_SHOW.set(True)
    self.COASTLINE_WIDTH.set(1)
    self.COASTLINE_COLOR.set('black')
    self.COUNTRYLINE_SHOW.set(False)
    self.COUNTRYLINE_WIDTH.set(2)
    self.COUNTRYLINE_COLOR.set('grey')
    self.LAND_COLOR.set('coral')
    self.WATER_COLOR.set('aqua')
    self.TITLE.set('')
    self.TITLEFONT.set_size(22)
    self.TITLEFONT.set_weight('bold')
    self.TITLE_PAD.set(0)
    self.XLABEL.set('Longitude')
    self.YLABEL.set('Latitude')
    self.LABEL_SIZE.set(16)
    self.LABEL_PAD.set(24)
    self.ZLABEL.set('')
    self.TLABEL.set('')
    self.DPI.set(72)
    self.FIGURE_COLOR.set('white')
    self.TEXT_COLOR.set('black')
    self.GRID_SHOW.set(False)
    self.GRID_LINEWIDTH.set(1)
    self.MERIDIAN_INI.set(-180)
    self.MERIDIAN_FIN.set(180)
    self.MERIDIAN_INT.set(60)
    self.PARALLEL_INI.set(-90)
    self.PARALLEL_FIN.set(90)
    self.PARALLEL_INT.set(30)
    self.GRID_COLOR.set('black')
    self.GRID_FONTCOLOR.set('black')
    self.GRID_SIZE.set(12)
    self.GRID_NORTH.set(False)
    self.GRID_SOUTH.set(True)
    self.GRID_WEST.set(False)
    self.GRID_EAST.set(True)
    self.GRID_LINESTYLE.set(':')
    self.GRID_ALPHA.set(1.0)
    self.SCALE_SHOW.set(False)
    self.SCALE_X.set(0)
    self.SCALE_Y.set(0)
    self.SCALE_XO.set(0)
    self.SCALE_YO.set(0)
    self.SCALE_LENGTH.set(100)
    self.SCALE_UNITS.set('km')
    self.SCALE_STYLE.set('fancy')
    self.SCALE_FONTSIZE.set(9)
    self.SCALE_FONTCOLOR.set('k')
    self.SCALE_LABELSTYLE.set('simple')
    self.SCALE_FORMAT.set('%d')
    self.SCALE_YOFFSET.set(None)
    self.SCALE_FILLCOLOR1.set('w')
    self.SCALE_FILLCOLOR2.set('k')
    self.SCALE_LINECOLOR.set('k')
    self.SCALE_LINEWIDTH.set(None)
    self.RELIEF.set(False)
    self.BLUEMARBLE.set(False)
    self.ETOPO.set(False)
    self.BACKGROUND_SCALE.set(1.0)
    self.RIVERS_SHOW.set(False)
    self.RIVERS_WIDTH.set(0.2)
    self.RIVERS_COLOR.set('blue')
    self.ARCGISIMAGE.set(0)
    self.ARCGISSERVICE.set('ESRI_Imagery_world_2D')
    self.ARCGISPIXELS.set(400)
    self.ARCGISDPI.set(96)
    self.ARCGISVERBOSE.set(True)
    self.LOGO_FILE.set(COSMO_CONF_PATH+'cosmo-logo.png')
    self.LOGO_IMAGE = image.imread(self.LOGO_FILE.get())
    self.LOGO_ZOOM.set(0.40)
    self.LOGO_LOCATION.set('SW')
    self.LOGO_DISPLAY.set(False)

    self.ISOBAT_PATH =  tk.StringVar()
    self.ISOBAT_PATH.set(COSMO_ROOT+'data/isobaths/')

#    self.ISOBAT_Z     = [   0,  100,  200,  400, 
#                          600,  800, 1000, 1200, 1400,
#                         1600, 1800, 2000, 2500, 3000,
#                        ]
#
#    self.ISOBAT_LABEL = ['coastline', '100 m', '200 m', '400 m',
#                             '600 m', '800 m','1000 m','1200 m','1400 m',
#                            '1600 m','1800 m','2000 m','2500 m','3000 m',
#                        ]
#
    self.ISOBAT_Z     = [   0,  50, 100,  200, 250, 400, 500,
                          600,  750, 800, 1000, 1250, 1500, 1750,
                         2000, 2500, 3000, 3500, 4000, 4500, 5000]
                        

    self.ISOBAT_LABEL = ['coastline',  '50 m', '100 m', '200 m', 
                             '250 m', '400 m', '500 m', '600 m', '750 m',
                             '800 m','1000 m','1250 m','1500 m','1750 m',
                            '2000 m','2500 m','3000 m','5500 m','4000 m',
                            '4500 m','5000 m' ]

    self.nisobat = len(self.ISOBAT_Z)

    self.ISOBAT_SELEC = []
    self.ISOBAT_COLOR = []
    self.ISOBAT_STYLE = []
    self.ISOBAT_WIDTH = []
    self.ISOBAT_SHOW  = []
    self.ISOBAT_DATA = []
    for i in range(self.nisobat):
      self.ISOBAT_SELEC.append(tk.BooleanVar(value=False))
      self.ISOBAT_COLOR.append(tk.StringVar(value='black'))
      self.ISOBAT_STYLE.append(tk.StringVar(value='-'))
      self.ISOBAT_WIDTH.append(tk.DoubleVar(value=1))
      self.ISOBAT_SHOW.append(False)
      self.ISOBAT_DATA.append(None)

    self.ISOBAT_LABEL_SHOW = tk.BooleanVar()
    self.ISOBAT_LABEL_SHOW.set(False)

    self.ISOBAT_NPLOT   = sum(self.ISOBAT_SHOW)
    self.ISOBAT_ZPOINTER = tk.StringVar()
    self.ISOBAT_ZPOINTER.set(self.ISOBAT_LABEL[0])
    self.ISOBAT_selected = False
    self.ISOBAT_loaded   = False
    self.ISOBAT_cropped  = False
    self.ISOBAT_LEGEND   = legend.LegendConfig()
    self.ISOBAT_LEGEND.TITLE.set('Isobaths')
    self.ISOBAT_LEGEND.LOC.set(2)

    self.TIMESTAMP_SHOW.set(False)
    self.TIMESTAMP_BOLD.set(False)
    self.TIMESTAMP_X.set(0.12)
    self.TIMESTAMP_Y.set(0.12)
    self.TIMESTAMP_COLOR.set('black')
    self.TIMESTAMP_SIZE.set(15)

    self.VIDEO_NAME.set('movie.mp4')
    self.VIDEO_TITLE.set('COSMO-VIEW Movie')
    self.VIDEO_AUTHOR.set('Matplotlib')
    self.VIDEO_COMMENT.set('Ocean currents movie')
    self.VIDEO_FPS.set(2)
    self.VIDEO_DPI.set(100)
    self.VIDEO_L1.set(0)

    self.WINDOW_FONT_TYPE.set('Helvetica')
    self.WINDOW_FONT_SIZE.set(14)
    font_type = matplotlib.rcParams['font.family'][0]
    self.MAP_FONT_TYPE.set(font_type)


    if exists(self.FILECONF):
      print('Reading configuration file '+self.FILECONF)
      try:
        conf = self.conf_load(self.FILECONF)
        self.conf_set(conf)
      except:
        print('Error reading, using default parameters')
        conf = self.conf_get()
        self.conf_save(conf,self.FILECONF)
    else:
      print('Saving configuration file ...')
      conf = self.conf_get()
      self.conf_save(conf,self.FILECONF)


  def conf_get(self):
  # ===========================
    '''Get the conf dictionnary from program variables'''

    conf = {}
    conf['_VERSION_'] = self.VERSION
    conf['OUTPUT_FIGURE'] = self.OUTPUT_FIGURE.get()
    conf['OUTPUT_LEAFLET'] = self.OUTPUT_LEAFLET.get()
    conf['SIZE'] = [self.SIZE[0],self.SIZE[1]]
    conf['DPI'] = self.DPI.get()
    conf['FIGURE_COLOR'] = self.FIGURE_COLOR.get()
    conf['TEXT_COLOR'] = self.TEXT_COLOR.get()
    conf['GEOMAP'] = self.GEOMAP.get()
    conf['MAP_PROJECTION'] = self.MAP_PROJECTION.get()
    conf['MAP_RESOLUTION'] = self.MAP_RESOLUTION.get()
    conf['EPSG'] = self.EPSG.get()
    conf['SOUTH'] = self.SOUTH.get()
    conf['NORTH'] = self.NORTH.get()
    conf['WEST'] = self.WEST.get()
    conf['EAST'] = self.EAST.get()
    conf['WIDTH'] = self.WIDTH.get()
    conf['HEIGHT'] = self.HEIGHT.get()
    conf['LAT_0'] = self.LAT_0.get()
    conf['LON_0'] = self.LON_0.get()
    conf['SATELLITE_HEIGHT'] = self.SATELLITE_HEIGHT.get()
    conf['COASTLINE_SHOW'] = self.COASTLINE_SHOW.get()
    conf['COASTLINE_WIDTH'] = self.COASTLINE_WIDTH.get()
    conf['COASTLINE_COLOR'] = self.COASTLINE_COLOR.get()
    conf['COUNTRYLINE_SHOW'] = self.COUNTRYLINE_SHOW.get()
    conf['COUNTRYLINE_WIDTH'] = self.COUNTRYLINE_WIDTH.get()
    conf['COUNTRYLINE_COLOR'] = self.COUNTRYLINE_COLOR.get()
    conf['LAND_COLOR'] = self.LAND_COLOR.get()
    conf['WATER_COLOR'] = self.WATER_COLOR.get()
    conf['TITLE'] = self.TITLE.get()
    conf['TITLEFONT'] = self.TITLEFONT.__dict__
    conf['TITLE_PAD'] = self.TITLE_PAD.get()
    conf['LABEL_SIZE'] = self.LABEL_SIZE.get()
    conf['LABEL_PAD'] = self.LABEL_PAD.get()
    conf['GRID_SHOW'] = self.GRID_SHOW.get()
    conf['GRID_LINEWIDTH'] = self.GRID_LINEWIDTH.get()
    conf['MERIDIAN_INI'] = self.MERIDIAN_INI.get()
    conf['MERIDIAN_FIN'] = self.MERIDIAN_FIN.get()
    conf['MERIDIAN_INT'] = self.MERIDIAN_INT.get()
    conf['PARALLEL_INI'] = self.PARALLEL_INI.get()
    conf['PARALLEL_FIN'] = self.PARALLEL_FIN.get()
    conf['PARALLEL_INT'] = self.PARALLEL_INT.get()
    conf['GRID_COLOR'] = self.GRID_COLOR.get()
    conf['GRID_FONTCOLOR'] = self.GRID_FONTCOLOR.get()
    conf['GRID_SIZE'] = self.GRID_SIZE.get()
    conf['GRID_NORTH'] = self.GRID_NORTH.get()
    conf['GRID_SOUTH'] = self.GRID_SOUTH.get()
    conf['GRID_WEST'] = self.GRID_WEST.get()
    conf['GRID_EAST'] = self.GRID_EAST.get()
    conf['GRID_LINESTYLE'] = self.GRID_LINESTYLE.get()
    conf['GRID_ALPHA'] = self.GRID_ALPHA.get()

    conf['SCALE_SHOW'] = self.SCALE_SHOW.get()
    conf['SCALE_X'] = self.SCALE_X.get()
    conf['SCALE_Y'] = self.SCALE_Y.get()
    conf['SCALE_XO'] = self.SCALE_XO.get()
    conf['SCALE_YO'] = self.SCALE_YO.get()
    conf['SCALE_LENGTH'] = self.SCALE_LENGTH.get()
    conf['SCALE_UNITS'] = self.SCALE_UNITS.get()
    conf['SCALE_STYLE'] = self.SCALE_STYLE.get()
    conf['SCALE_FONTSIZE'] = self.SCALE_FONTSIZE.get()
    conf['SCALE_FONTCOLOR'] = self.SCALE_FONTCOLOR.get()
    conf['SCALE_LABELSTYLE'] = self.SCALE_LABELSTYLE.get()
    conf['SCALE_FORMAT'] = self.SCALE_FORMAT.get()
    try:
      conf['SCALE_YOFFSET'] = self.SCALE_YOFFSET.get()
    except:
      conf['SCALE_YOFFSET'] = None
    conf['SCALE_FILLCOLOR1'] = self.SCALE_FILLCOLOR1.get()
    conf['SCALE_FILLCOLOR2'] = self.SCALE_FILLCOLOR2.get()
    conf['SCALE_LINECOLOR'] = self.SCALE_LINECOLOR.get()
    try:
      conf['SCALE_LINEWIDTH'] = self.SCALE_LINEWIDTH.get()
    except:
      conf['SCALE_LINEWIDTH'] = None

    conf['RELIEF'] = self.RELIEF.get()
    conf['BLUEMARBLE'] = self.BLUEMARBLE.get()
    conf['ETOPO'] = self.ETOPO.get()
    conf['BACKGROUND_SCALE'] = self.BACKGROUND_SCALE.get()
    conf['RIVERS_SHOW'] = self.RIVERS_SHOW.get()
    conf['RIVERS_WIDTH'] = self.RIVERS_WIDTH.get()
    conf['RIVERS_COLOR'] = self.RIVERS_COLOR.get()
    conf['ARCGISIMAGE'] = self.ARCGISIMAGE.get()
    conf['ARCGISSERVICE'] = self.ARCGISSERVICE.get()
    conf['ARCGISPIXELS'] = self.ARCGISPIXELS.get()
    conf['ARCGISDPI'] = self.ARCGISDPI.get()
    conf['ARCGISVERBOSE'] = self.ARCGISVERBOSE.get()
    conf['LOGO_FILE'] = self.LOGO_FILE.get()
    conf['LOGO_ZOOM'] = self.LOGO_ZOOM.get()
    conf['LOGO_LOCATION'] = self.LOGO_LOCATION.get()
    conf['LOGO_X'] = self.LOGO_X.get()
    conf['LOGO_Y'] = self.LOGO_Y.get()
    conf['LOGO_DISPLAY'] = self.LOGO_DISPLAY.get()

    conf['ISOBAT_PATH'] = self.ISOBAT_PATH.get()
    conf['ISOBAT_Z'] = self.ISOBAT_Z
    conf['ISOBAT_LABEL'] = self.ISOBAT_LABEL
    WIDTH = []
    COLOR = []
    STYLE = []
    SELEC = []
    for i in range(self.nisobat):
      WIDTH.append(self.ISOBAT_WIDTH[i].get())
      COLOR.append(self.ISOBAT_COLOR[i].get())
      STYLE.append(self.ISOBAT_STYLE[i].get())
      SELEC.append(self.ISOBAT_SELEC[i].get())
    conf['ISOBAT_WIDTH'] = WIDTH
    conf['ISOBAT_COLOR'] = COLOR
    conf['ISOBAT_STYLE'] = STYLE
    conf['ISOBAT_SELEC'] = SELEC
    conf['ISOBAT_LABEL_SHOW'] = self.ISOBAT_LABEL_SHOW.get()
    conf['ISOBAT_cropped'] = self.ISOBAT_cropped
    conf['ISOBAT_LEGEND'] = self.ISOBAT_LEGEND.conf_get()
    conf['LEGEND'] = self.LEGEND.conf_get()

    conf['TIMESTAMP_SHOW'] = self.TIMESTAMP_SHOW.get()
    conf['TIMESTAMP_BOLD'] = self.TIMESTAMP_BOLD.get()
    conf['TIMESTAMP_X'] = self.TIMESTAMP_X.get()
    conf['TIMESTAMP_Y'] = self.TIMESTAMP_Y.get()
    conf['TIMESTAMP_SIZE'] = self.TIMESTAMP_SIZE.get()
    conf['TIMESTAMP_COLOR'] = self.TIMESTAMP_COLOR.get()
    conf['VIDEO_NAME'] = self.VIDEO_NAME.get()
    conf['VIDEO_TITLE'] = self.VIDEO_TITLE.get()
    conf['VIDEO_AUTHOR'] = self.VIDEO_AUTHOR.get()
    conf['VIDEO_COMMENT'] = self.VIDEO_COMMENT.get()
    conf['VIDEO_FPS'] = self.VIDEO_FPS.get()
    conf['VIDEO_DPI'] = self.VIDEO_DPI.get()


    conf['WINDOW_FONT_TYPE'] = self.WINDOW_FONT_TYPE.get()
    conf['WINDOW_FONT_SIZE'] = self.WINDOW_FONT_SIZE.get()
    conf['MAP_FONT_TYPE'] = self.MAP_FONT_TYPE.get()
    return conf


  def conf_set(self,conf):
  # =======================
    '''Set program variables from the conf dictionnary'''

    self.VERSION = conf['_VERSION_']
    self.OUTPUT_FIGURE.set(conf['OUTPUT_FIGURE'])
    self.OUTPUT_LEAFLET.set(conf['OUTPUT_LEAFLET'])
    self.SIZE = conf['SIZE']
    self.DPI.set(conf['DPI'])
    self.FIGURE_COLOR.set(conf['FIGURE_COLOR'])
    self.TEXT_COLOR.set(conf['TEXT_COLOR'])
    self.GEOMAP.set(conf['GEOMAP'])
    self.MAP_PROJECTION.set(conf['MAP_PROJECTION'])
    self.MAP_RESOLUTION.set(conf['MAP_RESOLUTION'])
    self.EPSG.set(conf['EPSG'])
    self.SOUTH.set(conf['SOUTH'])
    self.NORTH.set(conf['NORTH'])
    self.WEST.set(conf['WEST'])
    self.EAST.set(conf['EAST'])
    self.WIDTH.set(conf['WIDTH'])
    self.HEIGHT.set(conf['HEIGHT'])
    self.LAT_0.set(conf['LAT_0'])
    self.LON_0.set(conf['LON_0'])
    self.SATELLITE_HEIGHT.set(conf['SATELLITE_HEIGHT'])
    self.MERIDIAN_INI.set(conf['MERIDIAN_INI'])
    self.MERIDIAN_FIN.set(conf['MERIDIAN_FIN'])
    self.MERIDIAN_INT.set(conf['MERIDIAN_INT'])
    self.PARALLEL_INI.set(conf['PARALLEL_INI'])
    self.PARALLEL_FIN.set(conf['PARALLEL_FIN'])
    self.PARALLEL_INT.set(conf['PARALLEL_INT'])
    self.COASTLINE_SHOW.set(conf['COASTLINE_SHOW'])
    self.COASTLINE_WIDTH.set(conf['COASTLINE_WIDTH'])
    self.COASTLINE_COLOR.set(conf['COASTLINE_COLOR'])
    self.COUNTRYLINE_SHOW.set(conf['COUNTRYLINE_SHOW'])
    self.COUNTRYLINE_WIDTH.set(conf['COUNTRYLINE_WIDTH'])
    self.COUNTRYLINE_COLOR.set(conf['COUNTRYLINE_COLOR'])
    self.LAND_COLOR.set(conf['LAND_COLOR'])
    self.WATER_COLOR.set(conf['WATER_COLOR'])
    self.TITLE.set(conf['TITLE'])
    self.TITLEFONT = setfont(conf['TITLEFONT'])
    self.TITLE_PAD.set(conf['TITLE_PAD'])
    self.LABEL_SIZE.set(conf['LABEL_SIZE'])
    self.LABEL_PAD.set(conf['LABEL_PAD'])
    self.GRID_SHOW.set(conf['GRID_SHOW'])
    self.GRID_LINEWIDTH.set(conf['GRID_LINEWIDTH'])
    self.GRID_COLOR.set(conf['GRID_COLOR'])
    self.GRID_FONTCOLOR.set(conf['GRID_FONTCOLOR'])
    self.GRID_SIZE.set(conf['GRID_SIZE'])
    self.GRID_NORTH.set(conf['GRID_NORTH'])
    self.GRID_SOUTH.set(conf['GRID_SOUTH'])
    self.GRID_WEST.set(conf['GRID_WEST'])
    self.GRID_EAST.set(conf['GRID_EAST'])
    self.GRID_LINESTYLE.set(conf['GRID_LINESTYLE'])
    self.GRID_ALPHA.set(conf['GRID_ALPHA'])

    self.SCALE_SHOW.set(conf['SCALE_SHOW'])
    self.SCALE_X.set(conf['SCALE_X'])
    self.SCALE_Y.set(conf['SCALE_Y'])
    self.SCALE_XO.set(conf['SCALE_XO'])
    self.SCALE_YO.set(conf['SCALE_YO'])
    self.SCALE_LENGTH.set(conf['SCALE_LENGTH'])
    self.SCALE_UNITS.set(conf['SCALE_UNITS'])
    self.SCALE_STYLE.set(conf['SCALE_STYLE'])
    self.SCALE_FONTSIZE.set(conf['SCALE_FONTSIZE'])
    self.SCALE_FONTCOLOR.set(conf['SCALE_FONTCOLOR'])
    self.SCALE_LABELSTYLE.set(conf['SCALE_LABELSTYLE'])
    self.SCALE_FORMAT.set(conf['SCALE_FORMAT'])
    self.SCALE_YOFFSET.set(conf['SCALE_YOFFSET'])
    self.SCALE_FILLCOLOR1.set(conf['SCALE_FILLCOLOR1'])
    self.SCALE_FILLCOLOR2.set(conf['SCALE_FILLCOLOR2'])
    self.SCALE_LINECOLOR.set(conf['SCALE_LINECOLOR'])
    self.SCALE_LINEWIDTH.set(conf['SCALE_LINEWIDTH'])

    self.RELIEF.set(conf['RELIEF'])
    self.BLUEMARBLE.set(conf['BLUEMARBLE'])
    self.ETOPO.set(conf['ETOPO'])
    self.BACKGROUND_SCALE.set(conf['BACKGROUND_SCALE'])
    self.RIVERS_SHOW.set(conf['RIVERS_SHOW'])
    self.RIVERS_WIDTH.set(conf['RIVERS_WIDTH'])
    self.RIVERS_COLOR.set(conf['RIVERS_COLOR'])
    self.ARCGISIMAGE.set(conf['ARCGISIMAGE'])
    self.ARCGISSERVICE.set(conf['ARCGISSERVICE'])
    self.ARCGISPIXELS.set(conf['ARCGISPIXELS'])
    self.ARCGISDPI.set(conf['ARCGISDPI'])
    self.ARCGISVERBOSE.set(conf['ARCGISVERBOSE'])
    self.LOGO_FILE.set(conf['LOGO_FILE'])
    self.LOGO_ZOOM.set(conf['LOGO_ZOOM'])
    self.LOGO_LOCATION.set(conf['LOGO_LOCATION'])
    self.LOGO_X.set(conf['LOGO_X'])
    self.LOGO_Y.set(conf['LOGO_Y'])
    self.LOGO_DISPLAY.set(conf['LOGO_DISPLAY'])

    self.ISOBAT_PATH.set(conf['ISOBAT_PATH'])
    self.ISOBAT_Z = conf['ISOBAT_Z']
    self.ISOBAT_LABEL = conf['ISOBAT_LABEL']
    self.ISOBAT_LABEL_SHOW.set(conf['ISOBAT_LABEL_SHOW'])
    self.ISOBAT_cropped = conf['ISOBAT_cropped']
    self.ISOBAT_LEGEND.conf_set(conf['ISOBAT_LEGEND'])
    self.nisobat = len(self.ISOBAT_Z)
    WIDTH = conf['ISOBAT_WIDTH']
    COLOR = conf['ISOBAT_COLOR']
    STYLE = conf['ISOBAT_STYLE']
    SELEC = conf['ISOBAT_SELEC']
    self.ISOBAT_WIDTH = []
    self.ISOBAT_COLOR = []
    self.ISOBAT_STYLE = []
    self.ISOBAT_SELEC = []
    self.ISOBAT_SHOW = []
    self.ISOBAT_DATA = []
    for i in range(self.nisobat):
      self.ISOBAT_SELEC.append(tk.BooleanVar(value=SELEC[i]))
      self.ISOBAT_COLOR.append(tk.StringVar(value=COLOR[i]))
      self.ISOBAT_STYLE.append(tk.StringVar(value=STYLE[i]))
      self.ISOBAT_WIDTH.append(tk.DoubleVar(value=WIDTH[i]))
      self.ISOBAT_SHOW.append(False)
      self.ISOBAT_DATA.append(None)

    if sum(SELEC) == 0:
      self.ISOBAT_selected = False
    else:
      self.ISOBAT_selected = True

    self.ISOBAT_loaded = False
    for i in range(self.nisobat):
      if self.ISOBAT_SELEC[i].get():
        filename = self.ISOBAT_PATH.get() + \
                     '/%04d' % self.ISOBAT_Z[i] + '.dat'
        self.ISOBAT_SHOW[i] = True
        self.ISOBAT_loaded  = True
        try:
          self.ISOBAT_DATA[i] = read_lines(filename)
        except:
          messagebox.showinfo(message='Error: unable to read '+filemane)
          self.ISOBAT_DATA[i] = None
          self.ISOBAT_SHOW[i] = False
          self.ISOBAT_loaded  = False

      self.ISOBAT_NPLOT = sum(self.ISOBAT_SHOW)

    if self.ISOBAT_cropped:
      self.isobath_crop()

    self.TIMESTAMP_SHOW.set(conf['TIMESTAMP_SHOW'])
    self.TIMESTAMP_BOLD.set(conf['TIMESTAMP_BOLD'])
    self.TIMESTAMP_X.set(conf['TIMESTAMP_X'])
    self.TIMESTAMP_Y.set(conf['TIMESTAMP_Y'])
    self.TIMESTAMP_SIZE.set(conf['TIMESTAMP_SIZE'])
    self.TIMESTAMP_COLOR.set(conf['TIMESTAMP_COLOR'])
    self.VIDEO_NAME.set(conf['VIDEO_NAME'])
    self.VIDEO_TITLE.set(conf['VIDEO_TITLE'])
    self.VIDEO_AUTHOR.set(conf['VIDEO_AUTHOR'])
    self.VIDEO_COMMENT.set(conf['VIDEO_COMMENT'])
    self.VIDEO_FPS.set(conf['VIDEO_FPS'])
    self.VIDEO_DPI.set(conf['VIDEO_DPI'])
    self.LEGEND.conf_set(conf['LEGEND'])
    self.WINDOW_FONT_TYPE.set(conf['WINDOW_FONT_TYPE'])
    self.WINDOW_FONT_SIZE.set(conf['WINDOW_FONT_SIZE'])
    self.MAP_FONT_TYPE.set(conf['MAP_FONT_TYPE'])

    # Derived variables:
    self.LOGO_IMAGE = image.imread(self.LOGO_FILE.get())

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


# ===================
class CosmoDrawing():
# ===================

  # ===============
  def close(self):
  # ===============
    if self.nfiles + self.nfeatures == 0:
      quit()

    aa = messagebox.askquestion('Close','Are you sure?',icon='warning')
    if aa == 'yes':
      self.master.destroy()
      self.master = None
      quit()

  # ===========================
  def __init__ (self,master):
  # ===========================

    # Initialization

    master.protocol('WM_DELETE_WINDOW',self.close)

    self.master = master
    self.master.configure(bg=BGC)

    self.PLOT   = DrawingConfig()
    self.first  = True
    self.ftime  = True

    try:
      font_name = self.PLOT.WINDOW_FONT_TYPE.get().split()[0]
      font = '%s %d' % (font_name, self.PLOT.WINDOW_FONT_SIZE.get())
      self.master.option_add('*Font',font)
    except:
      self.master.option_add('*Font',FONT)

    self.default_font = tkfont.nametofont('TkDefaultFont')
    self.default_font.configure(family=self.PLOT.WINDOW_FONT_TYPE.get().
                                                              split()[0])
    self.default_font.configure(size=self.PLOT.WINDOW_FONT_SIZE.get())

    self.L_LIST = []
    self.T_LIST = []
    self.DATE   = []
    #self.TFILE  = ''
    self.L      = tk.IntVar()
    self.K      = tk.IntVar()
    self.NL     = 0
    self.NZ     = 0

    self.L.set(0)
    self.K.set(0)

    self.nfiles        = 0
    self.FILENAMES     = []
    self.FILETYPES     = []
    self.FILEORDER     = []
    self.SEQUENCES     = []

    self.nvec          = 0
    self.VEC           = []
    self.VEC_LIST      = [None]
    self.VEC_INDX      = tk.IntVar()
    self.VEC_INDX.set(0)
    self.CURRENT_OPTIONS = ['Operational',     \
                            'HF Radar',        \
                            'COPERNICUS',      \
                            'Local Dataset',   \
                            'Remote Dataset',  \
                            'Selected FIELD']

    self.ncdf          = 0
    self.CDF           = []
    self.CDF_LIST      = [None]
    self.CDF_INDX      = tk.IntVar()
    self.CDF_INDX.set(0)
    self.cdfbar        = []
    self.Mcdfbar       = []
    self.CONTOUR_OPTIONS = ['Operational',       \
                            'COPERNICUS',        \
                            'Local Dataset',     \
                            'Remote Dataset',    \
                            'Selected CURRENT']

    self.nfloat        = 0
    self.FLOAT         = []
    self.FLOAT_LIST    = ['0']
    self.FLOAT_INDX    = tk.IntVar()
    self.FLOAT_INDX.set(0)
    self.FLOAT_OPTIONS = ['Local Dataset', \
                          'Local Folder', \
                          'Remote Folder', \
                          'Trajectories Database']

    self.SAIDIN        = cdf_parameters()
    self.SAIDIN.FIELD  = fld_parameters()
    self.sbar          = []
    self.Msbar         = []

    # Features: Markers or shapefiles
    # Stationary information. Types: MARKER,SHAPE

    self.nfeatures = 0
    self.FEATNAMES     = []
    self.FEATTYPES     = []
    self.FEATORDER     = []

    self.nmarker       = 0
    self.MARKER        = []
    self.MARKER_LIST   = ['0']
    self.MARKER_INDX   = tk.IntVar()
    self.MARKER_INDX.set(0)

    self.nshape        = 0
    self.SHAPE        = []
    self.SHAPE_LIST   = ['0']
    self.SHAPE_INDX   = tk.IntVar()
    self.SHAPE_INDX.set(0)

    # Initialize BLM command:
    self.BLM = blm.parameters()

    # Initialize MLM command:
    self.MLM = mlm.parameters()

    tmp = matplotlib.font_manager.get_fontconfig_fonts()
    if type(tmp) is dict:
      flist = list(tmp)
    else:
      flist = tmp.copy()
      del tmp

    # Get rid of fonts not well defined:
    nf = len(flist)
    for i in range(nf-1,-1,-1):
      fname = flist[i]
      try:
        ftype = matplotlib.font_manager.FontProperties(fname=fname).get_name()
      except:
        del flist[i]

    FONT_TYPES = [matplotlib.font_manager.FontProperties(fname=fname).get_family() for fname in flist]
    try:
      self.FONT_TYPES = list(set(FONT_TYPES))
    except:
      self.FONT_TYPES = FONT_TYPES.copy()

    self.FONT_TYPES.sort()
    self.FONT_SIZES = list(range(1,25))

    self.GET_TIMESTAMP_LOCATION = False

    # Initialize matplotlib
    self.fig = None
    #print('main ', self.PLOT.SIZE)
    #self.fig = plt.figure('COSMO-VIEW canvas',    \
    #                         figsize=self.PLOT.SIZE, \
    #                         dpi=self.PLOT.DPI.get())
    #
    #self.fig.canvas.mpl_connect('close_event',self.on_closing_figure)
    #self.fig.canvas.callbacks.connect('button_press_event',self.on_click)
    #self.ax = self.fig.add_subplot(111)
    self.drawmap = None

    # Window design
    self.CreateMenu()

    gui_style = ttk.Style()
    gui_style.configure('My.TFrame',background="green")
    gui_style.configure('My.TLabel',background="green")

    #F0 = ttk.Frame(self.master,style='My.TFrame')
    F0 = tk.Frame(self.master,bg=BGC)

    tk.Label(F0,text='Time',bg=BGC).grid(row=0,column=0,padx=3)
    self.lbox = ttk.Combobox(F0,textvariable=self.L,width=5)
    self.lbox.grid(row=0,column=1,sticky='ew')
    self.lbox.configure(state='disabled')
    self.lbox.bind('<<ComboboxSelected>>',lambda e: self.lselection())
    self.lbox.bind('<Return>',lambda e: self.lselection())

    self.bprev = tk.Button(F0,text='PREV',command=self.tprev,bg=BWC)
    self.bprev.grid(row=0,column=2,padx=3,sticky='e')

    tk.Entry(F0,textvariable=self.PLOT.TLABEL, \
              state='readonly',width=20,bg='white').grid(row=0,column=3,\
              columnspan=3,sticky='w',padx=3)

    self.bnext = tk.Button(F0,text='NEXT',command=self.tnext,bg=BWC)
    self.bnext.grid(row=0,column=6,padx=3,stick='w')

    if len(self.DATE) <= 0:
      self.bprev.configure(state='disabled')
      self.lbox.configure(state='disabled')
      self.bnext.configure(state='disabled')
    else:
      self.lbox['values'] = list(range(len(self.L_LIST)))


    tk.Button(F0,text='Draw',command=self.make_plot,bg=EBC)  \
       .grid(row=1,column=5,padx=3,pady=3,sticky='e')
    tk.Button(F0,text='Quit',command=self.close,bg=EBC)      \
       .grid(row=1,column=6,padx=3,pady=3)
    tk.Label(F0,text='COSMO project, July 2018',bg=BGC)  \
       .grid(row=2,column=5,columnspan=2,padx=3)
    F0.grid()
    
    # Initialize window widget IDs:
    self.Window_cfile         = None
    self.Window_legendconfig  = None
    self.Window_mapconfig     = None
    self.Window_vectorconfig  = None
    self.Window_contourconfig = None
    self.Window_lineconfig    = None
    self.Window_other         = None
    self.Window_saidin        = None
    self.Window_currents      = None
    self.Window_opendap       = None
    self.Window_copernicus    = None
    self.Window_codar         = None
    self.Window_isobat        = None
    self.Window_float         = None
    self.Window_saidinconfig  = None
    self.Window_floatconfig   = None
    self.Window_blm           = None
    self.Window_mlm           = None
    self.Window_dpi           = None
    self.Window_anim          = None
    self.Window_ncdf          = None
    self.Window_vec           = None
    self.Window_logo          = None
    self.Window_files         = None
    self.Window_about         = None
    self.Window_widgetconfig  = None
    self.Window_marker        = None
    self.Window_markerconfig  = None
    self.Window_dotconfig     = None
    self.Window_editor        = None


  # ================================
  def on_closing_figure(self,event):
  # ================================
    self.PLOT.SIZE = list(self.fig.get_size_inches())
    self.fig = None


  # =======================
  def on_click(self,event):
  # =======================

    if self.GET_TIMESTAMP_LOCATION:
      try:
        self.time_stamp.remove()
      except:
        pass
      self.GET_TIMESTAMP_LOCATION = False
      xx = event.x/self.PLOT.DPI.get()/self.PLOT.SIZE[0]
      yy = event.y/self.PLOT.DPI.get()/self.PLOT.SIZE[1]
      self.PLOT.TIMESTAMP_X.set(np.round(xx,3))
      self.PLOT.TIMESTAMP_Y.set(np.round(yy,3))

      if self.PLOT.TIMESTAMP_SHOW.get():
        font_family = self.PLOT.MAP_FONT_TYPE.get()
        if self.PLOT.TIMESTAMP_BOLD.get():
          font_weight = 'bold'
        else:
          font_weight = 'normal'
        font = {'family' : font_family,
                'weight' : font_weight,
                'color'  : self.PLOT.TIMESTAMP_COLOR.get(),
                'size'   : self.PLOT.TIMESTAMP_SIZE.get()}

        self.time_stamp = self.fig.text(self.PLOT.TIMESTAMP_X.get(),
                                      self.PLOT.TIMESTAMP_Y.get(),
                                      self.DATE[self.L.get()],
                                      fontdict=font)
        self.fig.show()

      return
     
    if self.nvec > 0:
      ii = self.VEC_INDX.get()
      if self.VEC[ii].VEL.PLOT.KEY_GETXY:
        self.VEC[ii].VEL.PLOT.KEY_GETXY = False
        xx = event.x/self.PLOT.DPI.get()/self.PLOT.SIZE[0]
        yy = event.y/self.PLOT.DPI.get()/self.PLOT.SIZE[1]
        self.VEC[ii].VEL.PLOT.KEY_X.set(np.round(xx,3))
        self.VEC[ii].VEL.PLOT.KEY_Y.set(np.round(yy,3))
        self.VEC[ii].VEL.PLOT.KEY_OBJ.X = xx
        self.VEC[ii].VEL.PLOT.KEY_OBJ.Y = yy
        self.fig.show()
        return

    if event.inaxes is not None:
      print(event.xdata, event.ydata)
      xo,yo = self.m(event.xdata,event.ydata,inverse=True)
      print(xo,yo)
      #print('Current speed = ', self.CURRENTS.F(event.xdata,event.ydata))
      #if not empty(self.SAIDIN.FILENAME.get()):
      #  print('SAIDIN SST = ', self.SAIDIN.FIELD.F(xo,yo))
      self.BLM.xo.set(xo)
      self.BLM.yo.set(yo)
      self.MLM.xo.set(xo)
      self.MLM.yo.set(yo)


  # ===========================
  def on_xlims_change(self,ax):
  # ===========================
    lims = self.ax.get_xlim()
    self.PLOT.WEST.set(lims[0])
    self.PLOT.EAST.set(lims[1])
    self.drawmap = True


  # ===========================
  def on_ylims_change(self,ax):
  # ===========================
    lims = self.ax.get_ylim()
    self.PLOT.SOUTH.set(lims[0])
    self.PLOT.NORTH.set(lims[1])
    self.drawmap = True


  # ====================
  def CreateMenu (self):
  # ====================
    ''' Create options menu'''

    menubar = tk.Menu(self.master)

    plotmenu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='File',menu=plotmenu)
    plotmenu.add_command(label='Save figure',
                         command=self.figure_save)
    plotmenu.add_command(label='Read figure',
                         command=self.figure_read)
    plotmenu.add_separator()
    plotmenu.add_command(label='Plot layers',
                         command=self.layers)
    plotmenu.add_separator()
    plotmenu.add_command(label='Save plot',
                         command=self.save)
    plotmenu.add_command(label='Save plot as',
                         command=self.saveas)
    plotmenu.add_separator()
    plotmenu.add_command(label='Quit',
                         command=self.close)

    insmenu  = tk.Menu(menubar, tearoff=0)
    menubar.add_cascade(label='Import/Select',menu=insmenu)
    insmenu.add_command(label='Vector field',command=self.get_vector)
    insmenu.add_command(label='Satellite SST',command=self.get_saidin)
    insmenu.add_command(label='Contour field',command=self.get_contour)
    insmenu.add_command(label='Trajectory',command=self.get_lagrangian)
    insmenu.add_command(label='Marker',command=self.get_marker)

    confmenu = tk.Menu(menubar, tearoff=0)
    menubar.add_cascade(label='Configure',menu=confmenu)
    confmenu.add_command(label='Widgets',command=self.widget_config)
    confmenu.add_command(label='Map',command=self.map_config)
    confmenu.add_command(label='Legends',command=self.legend_config)
    confmenu.add_command(label='Logo',command=self.logo_config)
    confmenu.add_separator()
    confmenu.add_command(label='Vector field',
                         command=self.currents_config)
    confmenu.add_command(label='Satellite SST', \
                         command=self.saidin_config)
    confmenu.add_command(label='Contour field',
                         command=self.contour_config)
    confmenu.add_command(label='Trajectory',
                         command=self.lagrangian_config)
    confmenu.add_command(label='Marker',
                         command=self.marker_config)
    confmenu.add_separator()
    confmenu.add_command(label='Select configuration',
                         command=self.configuration_file)

    toolmenu = tk.Menu(menubar, tearoff=0)
    menubar.add_cascade(label='Tools',menu=toolmenu)
    toolmenu.add_command(label='Trajectory editor',
                         command=self.trajectory_editor)
    toolmenu.add_command(label='Make animation',
                         command=self.make_anim)
    toolmenu.add_command(label='COSMO B Lagrangian Model (BLM)',
                         command=self.blm)
    toolmenu.add_command(label='COSMO M Lagrangian Model (MLM)',
                         command=self.mlm)

    helpmenu = tk.Menu(menubar, tearoff=0)
    menubar.add_cascade(label='Help',menu=helpmenu)
    helpmenu.add_command(label='About',command=self.about)

    try:
      self.master.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.master.tk.call(master, "config", "-menu", menubar)


  # ============================
  def about(self):
  # ============================
    '''Widget to print some help '''
    def _close():
      self.Window_about.destroy()
      self.Window_about = None

    if self.Window_about is None:
      self.Window_about = tk.Toplevel(self.master)
      self.Window_about.title('About')
      self.Window_about.resizable(width=False,height=False)
      self.Window_about.protocol('WM_DELETE_WINDOW',_close)

      photoimage = ImageTk.PhotoImage(Image.open(self.PLOT.LOGO_FILE.get()))

      panel1 = tk.Label(self.Window_about,image=photoimage)
      panel1.grid(row=0,column=0,sticky='we')

      # save the panel's image from 'garbage collection'
      panel1.image = photoimage

      _author = 'Author: Quim Ballabrera (COSMO Project)'
      _description = ' Ocean visualization tool for the COSMO project'
      tk.Label(self.Window_about,text='COSMO-VIEW'). \
              grid(row=1,column=0,sticky='ew')
      tk.Label(self.Window_about,text='Version '+VERSION). \
              grid(row=2,column=0,sticky='ew')
      tk.Label(self.Window_about,text=_author) \
              .grid(row=3,column=0,sticky='ew')
      tk.Label(self.Window_about,text=_description). \
              grid(row=4,column=0,sticky='ew')
      tk.Button(self.Window_about,text='Close',command=_close). \
              grid(row=5,column=0,sticky='ew')
    else:
      self.Window_about.lift()


  # ============================
  def get_vector(self):
  # ============================
    '''Widget to read files with currents (U,V) '''

    self.VSOURCE = tk.StringVar()
    self.VSOURCE.set('Operational')

    def _close():
    # =========== 
      self.Window_currents.destroy()
      self.Window_currents = None

    def _done():
    # ==========
      ii = self.VEC_INDX.get()
      self.read_UV(self.VEC[ii])
      _close()
      self.make_plot()
      if self.Window_vectorconfig is not None:
        self.Window_vectorconfig.destroy()
        self.Window_vectorconfig = None
        self.currents_config()

    def _clear():
    # ===========
      if self.nvec == 0:
        return

      ii = self.VEC_INDX.get()

      for i in range(self.nfiles):
        if self.FILETYPES[i] == 'VEC' and self.FILEORDER[i] == ii:
          del self.FILENAMES[i]
          del self.FILETYPES[i]
          del self.FILEORDER[i]
          del self.SEQUENCES[i]
      self.nfiles -= 1

      print('Erasing record ', ii)
      del self.VEC[ii]
      self.nvec -= 1
      ii = self.nvec-1 if ii>= self.nvec else ii
      self.VEC_INDX.set(ii)
      _refill(ii)
      self.make_plot()

    def _reget():
    # ===========
      self.VEC_INDX.set(_wsel.get())
      ii = self.VEC_INDX.get()
      _refill(ii)

    def _refill(ii):
    # ==============
      if ii >= 0:
        self.VEC_LIST = list(range(self.nvec))
        _wsel.configure(state='!disabled')
        _wsel['values'] = self.VEC_LIST
        _went['textvariable'] = self.VEC[ii].FILENAME
        _uvar.configure(state='!disabled')
        _uvar['textvariable'] = self.VEC[ii].uname
        _uvar['values'] = self.VEC[ii].icdf.VAR_MENU
        _vvar.configure(state='!disabled')
        _vvar['textvariable'] = self.VEC[ii].vname
        _vvar['values'] = self.VEC[ii].icdf.VAR_MENU
        _kbox.configure(state='!disabled')
        _kbox['textvariable'] = self.VEC[ii].K
        _kbox['values'] = self.VEC[ii].K_LIST
        _lbox.configure(state='!disabled')
        _lbox['textvariable'] = self.VEC[ii].L
        _lbox['values'] = self.VEC[ii].L_LIST
        if self.VEC[ii].icdf.idk < 0:
          _kbox.configure(state='disabled')
          _zbox['text']='--'
        else:
          _zbox['text']=self.VEC[ii].Z_LIST[self.VEC[ii].K.get()]
        if self.VEC[ii].icdf.idl < 0:
          _lbox.configure(state='disabled')
          _dbox['text']='--'
        else:
          _lbox['textvariable'] = self.VEC[ii].L
          _lbox['values'] = self.VEC[ii].L_LIST
          _dbox['text'] = self.VEC[ii].DATE[self.VEC[ii].L.get()]
        _show['variable'] = self.VEC[ii].VEL.show

      else:
        self.VEC         = []
        self.VEC_LIST    = [None]
        self.VEC_INDX    = tk.IntVar()
        self.VEC_INDX.set(0)

        _wsel.configure(state='disabled')
        _uvar.configure(state='disabled')
        _vvar.configure(state='disabled')
        _kbox.configure(state='disabled')
        _lbox.configure(state='disabled')
        _wsel['values'] = self.VEC_LIST
        _went['textvariable'] = ''
        _uvar['textvariable'] = ''
        _uvar['values'] = ['']
        _uvar.configure(state='disabled')
        _vvar['textvariable'] = ''
        _vvar['values'] = ['']
        _vvar.configure(state='disabled')
        _kbox['textvariable'] = ''
        _kbox['values'] = ['']
        _zbox['text'] = '--'
        _lbox['text'] = ''
        _lbox['values'] = ['']
        _lbox['textvariable'] = ''
        _lbox['values'] = ['']
        _dbox['text'] = ['--']

    def _add(SOURCE):
    # ===============

      global Window_select
      global VEC


      def _cancel():
      # ============
        global Window_select
        Window_select.destroy()
        Window_select = None

      def _done():
      # ==========
        global Window_select
        global _uvar,_vvar
        global VEC

        if empty(VEC.uname.get()):
          VEC.uid = None
        else:
          VEC.uid = VEC.icdf.vname.index(VEC.uname.get())
        if empty(VEC.vname.get()):
          VEC.vid = None
        else:
          VEC.vid = VEC.icdf.vname.index(VEC.vname.get())

        if VEC.uid is None or VEC.vid is None:
          messagebox.showinfo(parent=Window_select,message='Select velocity components')
          return

        # Seems a suitable location for those statements:
        #
        self.read_lonlat(VEC,VEC.icdf.xname,VEC.icdf.yname)
        self.DepthandDate(VEC)
        VEC.VEL.show.set(True)

        self.nvec += 1
        self.VEC.append(VEC)
        self.VEC_INDX.set(self.nvec-1)
        self.VEC_LIST = list(range(self.nvec))

        self.nfiles += 1
        self.FILENAMES.append(VEC.FILENAME.get())
        self.FILETYPES.append('VEC')
        self.FILEORDER.append(self.nvec-1)
        self.SEQUENCES.append(tk.BooleanVar(value=False))

        ii = self.VEC_INDX.get()

        if self.first:
          if self.drawmap is None:
            self.PLOT.WEST.set(np.min(self.VEC[ii].lon))
            self.PLOT.EAST.set(np.max(self.VEC[ii].lon))
            self.PLOT.SOUTH.set(np.min(self.VEC[ii].lat))
            self.PLOT.NORTH.set(np.max(self.VEC[ii].lat))
            self.plot_initialize()
          self.L.set(self.VEC[ii].L.get())
          self.L_LIST = list(range(self.VEC[ii].icdf.nt))
          self.NL = len(self.L_LIST)
          self.lbox.configure(state='!disabled')
          self.lbox['values'] = self.L_LIST
          self.DATE = self.VEC[ii].DATE.copy()
          self.TIME = self.VEC[ii].TIME.copy()
          #self.TFILE = '%d' % self.nfiles
          self.PLOT.TLABEL.set(self.VEC[ii].DATE[self.L.get()])
          if len(self.DATE) > 1:
            self.bnext.configure(state='normal')
          try:
            self.PLOT.XLABEL.set(self.VEC[ii].ncid.variables[self.VEC[ii].icdf.xname].getncattr('long_name'))
          except:
            self.PLOT.XLABEL.set(self.VEC[ii].icdf.xname)
          try:
            self.PLOT.YLABEL.set(self.VEC[ii].ncid.variables[self.VEC[ii].icdf.yname].getncattr('long_name'))
          except:
            self.PLOT.YLABEL.set(self.VEC[ii].icdf.yname)
          self.SEQUENCES[-1].set(True)
          self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
          self.first = False
        else:
          ntime = len(self.DATE)
          same  = True
          if len(self.VEC[ii].DATE) == ntime:
            print('Same number of time records')
            for i in range(ntime):
              if self.DATE[i] != self.VEC[ii].DATE[i]:
                same = False
            self.SEQUENCES[-1].set(same)

        _refill(ii)
        Window_select.destroy()
        Window_select = None


      #names = ['Operational','CODAR','COPERNICUS','Local']
      ISOURCE = self.CURRENT_OPTIONS.index(SOURCE)
      if ISOURCE == 0:
        filename = self.get_opendap_filename()
      elif ISOURCE == 1:
        filename = self.get_codar_filename()
      elif ISOURCE == 2:
        filename = self.get_copernicus_filename()
      elif ISOURCE == 3:
        nn = filedialog.askopenfilename(parent=self.Window_currents, \
                                        filetypes=[('Netcdf','*.nc'),  \
                                                   ('CDF','*.cdf'),  \
                                                   ('ALL','*')])
        if len(nn) == 0:
          return
        else:
          filename = '%s' % nn
      elif ISOURCE == 4:
        aa = get_remote()
        filename = aa.filename()
      else:
        ii = self.CDF_INDX.get()
        filename = self.CDF[ii].FILENAME.get()
    
      if empty(filename):
        return

      # Not empty filename:
      VEC = cdf_parameters()
      VEC.FILENAME.set(filename)
      VEC.VEL = vel_parameters()
      VEC.ncid = Dataset(filename)
      VEC.icdf = tools.geocdf(filename)


#     self.read_lonlat(VEC,VEC.icdf.xname,VEC.icdf.yname)
#     self.DepthandDate(VEC)
#     VEC.VEL.show.set(True)

      if Window_select is None:
        Window_select = tk.Toplevel(self.master)
        Window_select.title('SELECT VARIABLES')
        Window_select.protocol('WM_DELETE_WINDOW',Window_select.destroy)
      else:
        Window_select.lift()
        return

      axesid = tools.WinGeoaxes(VEC.icdf,VEC.ncid,Window_select)

      font_bold = tkfont.Font(font='TkDefaultFont').copy()
      font_bold['weight']='bold'

      F0 = ttk.Frame(Window_select,padding=5,borderwidth=5)
      ttk.Label(F0,text='Select U', \
                   borderwidth=3,   \
                   font=font_bold).grid(row=0,column=0)
      ttk.Combobox(F0,textvariable=VEC.uname,   \
                      values=VEC.icdf.VAR_MENU, \
                      width=20).grid(row=0,column=1,columnspan=2)
      ttk.Label(F0,text='Select V', \
                   borderwidth=3,   \
                   font=font_bold).grid(row=0,column=4)
      ttk.Combobox(F0,textvariable=VEC.vname,   \
                      values=VEC.icdf.VAR_MENU, \
                      width=20).grid(row=0,column=5,columnspan=2)
      F0.grid()

      F1 = ttk.Frame(Window_select,padding=5)
      cancel = ttk.Button(F1,text='Cancel',command=_cancel)
      cancel.grid(row=0,column=3,sticky='e',padx=10)
      cancel.bind("<Return>",lambda e:_cancel())
      done = ttk.Button(F1,text='Done',command=_done)
      done.grid(row=0,column=4,sticky='e',padx=10)
      done.bind("<Return>",lambda e:_done())
      F1.grid(sticky='we')
      Window_select.wait_window(Window_select)

    def _lselection():
    # ================
      _dbox['text'] = self.VEC[ii].DATE[self.VEC[ii].L.get()]

    def _kselection():
    # ================
      _zbox['text'] = self.VEC[ii].Z_LIST[self.VEC[ii].L.get()]

    def _uselection():
    # ================
      ii = self.VEC_INDX.get()
      try:
        self.VEC[ii].uid = self.VEC[ii].icdf.vname.index( \
                                             self.VEC[ii].uname.get())
      except:
        self.VEC[ii].uid = -1

    def _vselection():
    # ================
      ii = self.VEC_INDX.get()
      try:
        self.VEC[ii].vid = self.VEC[ii].icdf.vname.index( \
                                             self.VEC[ii].vname.get())
      except:
        self.VEC[ii].vid = -1



    # Main Window
    # ============
    if self.Window_currents is None:
      self.Window_currents = tk.Toplevel(self.master)
      self.Window_currents.title("Currents selector")
      self.Window_currents.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_currents.lift()

    if self.nvec > 0:
      ii = self.VEC_INDX.get()
    else:
      ii = -1

    global Window_select
    global _uvar,_vvar

    Window_select = None
    F0 = ttk.Frame(self.Window_currents,padding=5)

    # Add
    ttk.Button(F0,text='Import',command=lambda:_add(self.VSOURCE.get())).grid(row=1,column=0,padx=3)
    _source = ttk.Combobox(F0,textvariable=self.VSOURCE, \
                     values=self.CURRENT_OPTIONS)
    _source.grid(row=0,column=0,padx=3)
    #_source.bind('<<ComboboxSelected>>', \
    #              lambda e: _add(self.VSOURCE.get()))

    # Filename:
    ttk.Label(F0,text='Netcdf file').grid(row=0,column=1,padx=3)
    _wsel = ttk.Combobox(F0,textvariable=self.VEC_INDX, \
                                  values=self.VEC_LIST,width=5)
    _wsel.grid(row=0,column=2)
    _wsel.bind('<<ComboboxSelected>>',lambda e: _reget())
    _went = ttk.Entry(F0,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=3,columnspan=5,padx=3,sticky='w')

    # Velocity components:
    ttk.Label(F0,text='Zonal').grid(row=1,column=1,padx=3,pady=3)
    _uvar = ttk.Combobox(F0,width=15)
    _uvar.grid(row=1,column=2,columnspan=2,sticky='w')
    _uvar.bind('<<ComboboxSelected>>',lambda e: _uselection())

    ttk.Label(F0,text='Meridional').grid(row=1,column=4,padx=3,pady=3)
    _vvar = ttk.Combobox(F0,width=15)
    _vvar.grid(row=1,column=5,columnspan=2,sticky='w')
    _vvar.bind('<<ComboboxSelected>>',lambda e: _vselection())

    # Depth:
    ttk.Label(F0,text='Depth').grid(row=2,column=1,padx=3,pady=3)
    _kbox = ttk.Combobox(F0,values=['0'],width=5)
    _kbox.grid(row=2,column=2)
    _zbox = ttk.Label(F0,width=20)
    _zbox.grid(row=2,column=3,columnspan=2,sticky='w')

    # Time:
    ttk.Label(F0,text='Time').grid(row=3,column=1,padx=3,pady=3)
    _lbox = ttk.Combobox(F0,width=5)
    _lbox.grid(row=3,column=2)
    _lbox.bind('<<ComboboxSelected>>',lambda e: _lselection())
    _dbox = ttk.Label(F0,width=20)
    _dbox.grid(row=3,column=3,columnspan=2,sticky='w')

    if ii == -1:
      _wsel.configure(state='disabled')
      _uvar.configure(state='disabled')
      _vvar.configure(state='disabled')
      _kbox.configure(state='disabled')
      _lbox.configure(state='disabled')
    else:
      _went['textvariable'] = self.VEC[ii].FILENAME
      _uvar['textvariable'] = self.VEC[ii].uname
      _vvar['textvariable'] = self.VEC[ii].vname
      _uvar['values'] = self.VEC[ii].icdf.VAR_MENU
      _vvar['values'] = self.VEC[ii].icdf.VAR_MENU
      _kbox['textvariable'] = self.VEC[ii].K
      _kbox['values'] = self.VEC[ii].K_LIST
      if self.VEC[ii].icdf.idk < 0:
        _kbox.configure(state='disabled')
        _zbox['text']='--'
      else:
        _zbox['text']=self.VEC[ii].Z_LIST[self.VEC[ii].K.get()]
      if self.VEC[ii].icdf.idl < 0:
        _lbox.configure(state='disabled')
        _dbox['text']='--'
      else:
        _lbox['textvariable'] = self.VEC[ii].L
        _lbox['values'] = self.VEC[ii].L_LIST
        _dbox['text'] = self.VEC[ii].DATE[self.VEC[ii].L.get()]

    F0.grid(row=0,column=0)

    F1 = ttk.Frame(self.Window_currents,padding=5)
    if ii == -1:
      _show = ttk.Checkbutton(F1,text='Show')
    else:
      _show = ttk.Checkbutton(F1,text='Show')
      _show['variable']=self.VEC[ii].VEL.show
      _show.configure(command=self.make_plot)
    _show.grid(row=1,column=5)
    ttk.Button(F1,text='Cancel',command=_close).grid(row=1,column=6,padx=3)
    ttk.Button(F1,text='Clear',command=_clear).grid(row=1,column=7,padx=3)
    ttk.Button(F1,text='Plot',command=_done).grid(row=1,column=8,padx=3)
    F1.grid(row=1,column=0)



  # =============================
  def get_opendap_filename(self):
  # =============================

    def _close():
      self.Window_opendap.destroy()
      self.Window_opendap = None

    if self.Window_opendap is None:
      self.Window_opendap = tk.Toplevel(self.master)
      self.Window_opendap.title('Load Operational service Opendap file')
      self.Window_opendap.protocol('WM_DELETE_WINDOW',_close)
      a = providers.WinOpendap(self.Window_opendap)
      self.Window_opendap.wait_window()
      self.Window_opendap = None
      filename = a.get_filename()
      return filename
    else:
      self.Window_opendap.lift()


  # =============================
  def get_codar_filename(self):
  # =============================

    def _close():
      self.Window_codar.destroy()
      self.Window_codar = None

    if self.Window_codar is None:
      self.Window_codar = tk.Toplevel(self.master)
      self.Window_codar.title('HF Radar station selector')
      self.Window_codar.protocol('WM_DELETE_WINDOW',_close)
      a = codar.WinCodar(self.Window_codar)
      self.Window_codar.wait_window()
      self.Window_codar = None
      filename = a.get_filename()
      return filename
    else:
      self.Window_codar.lift()


  # ================================
  def get_copernicus_filename(self):
  # ================================

    def _close():
      self.Window_copernicus.destroy()
      self.Window_copernicus = None

    if self.Window_copernicus is None:
      self.Window_copernicus = tk.Toplevel(self.master)
      self.Window_copernicus.title('COPERNICUS file selector')
      self.Window_copernicus.configure(background='#87CEEB')

      self.Window_copernicus.protocol('WM_DELETE_WINDOW',_close)
      a = copernicus.WinTracking(self.Window_copernicus)
      self.Window_copernicus.wait_window()
      self.Window_copernicus = None
      filename = a.out()
      return filename
    else:
      self.Window_copernicus.lift()


  # ==================
  def layers(self):
  # ==================
    '''Display the files being opened'''

    def _close():
    # ===========
      self.Window_files.destroy()
      self.Window_files = None
   
    #Main Window
    # =========
    if self.nfiles == 0:
      print('No files opened yet')
      return

    if self.Window_files is None:
      self.Window_files = tk.Toplevel(self.master)
      self.Window_files.title('Plot layers')
      self.Window_files.resizable(width=True,height=True)
      self.Window_files.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_files.lift()
      return

    F0 = ttk.Frame(self.Window_files,borderwidth=5,padding=5)
    ttk.Label(F0,text='SHOW').grid(row=0,column=0)
    ttk.Label(F0,text='TYPE').grid(row=0,column=1)
    ttk.Label(F0,text='NAME').grid(row=0,column=2,sticky='we')

    nvec = -1
    nfld = -1
    nflo = -1
    for i in range(self.nfiles):
      #ttk.Label(F0,text='%s'%(i), \
      #             justify='right').grid(row=i+1,column=0,padx=5)
      if self.FILETYPES[i] == 'VEC':
        nvec += 1
        ttk.Checkbutton(F0,variable=self.VEC[nvec].VEL.show,\
                 command=self.make_plot). \
                 grid(row=i+1,column=0,padx=3)
        
      if self.FILETYPES[i] == 'FLD':
        nfld += 1
        ttk.Checkbutton(F0,variable=self.CDF[nfld].FIELD.show,\
                 command=self.make_plot). \
                 grid(row=i+1,column=0,padx=3)
        
      if self.FILETYPES[i] == 'FLOAT':
        nflo += 1
        ttk.Checkbutton(F0,variable=self.FLOAT[nflo].SHOW,\
                 command=self.make_plot). \
                 grid(row=i+1,column=0,padx=3)
        
      if self.FILETYPES[i] == 'SAIDIN':
        ttk.Checkbutton(F0,variable=self.SAIDIN.FIELD.show,\
                 command=self.make_plot). \
                 grid(row=i+1,column=0,padx=3)
        
      ttk.Label(F0,text=self.FILETYPES[i], \
                   width=7,justify='left').grid(row=i+1, \
                                                 column=1, \
                                                 columnspan=1,padx=3)
      ttk.Label(F0,text=self.FILENAMES[i], \
                   width=80,justify='left').grid(row=i+1, \
                                                 column=2, \
                                                 columnspan=8,padx=3)
    F0.grid()
    for i in range(self.nfiles):
      print('%s as %s' % (self.FILENAMES[i],self.FILETYPES[i]))


  # ===========================
  def configuration_file(self):
  # ===========================
    ''' Launch the Configuration file script '''

#COSMO_CONF_PATH = COSMO_ROOT + 'conf' + os.sep
#COSMO_CONF_NAME = 'default'
#COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    # -----------
    def _done():
    # -----------
      '''Close the widget'''
      if exists(self.PLOT.FILECONF):
        print('Reading configuration file '+self.PLOT.FILECONF)
        try:
          conf = self.PLOT.conf_load(self.PLOT.FILECONF)
          self.PLOT.conf_set(conf)
        except:
          print('Error reading, using default parameters')
          conf = self.PLOT.conf_get()
          self.PLOT.conf_save(conf,self.PLOT.FILECONF)
      else:
        print('Saving configuration file ...')
        conf = self.PLOT.conf_get()
        self.PLOT.conf_save(conf,self.PLOT.FILECONF)

      conf = {}
      conf['COSMO_CONF_PATH']=COSMO_CONF_PATH
      conf['COSMO_CONF_NAME']=COSMO_CONF_NAME
      with io.open(COSMO_CONF_DATA,'w',encoding='utf8') as outfile:
        _str = json.dumps(conf,ensure_ascii=False,
                           sort_keys=False,
                           indent=2,
                           separators=(',',': '))
        outfile.write(to_unicode(_str))
        outfile.close()

      self.Window_cfile.destroy()
      self.Window_cfile = None

    # -----------
    def _cancel():
    # -----------
      '''Recover backup value and close the widget'''

      with open(COSMO_CONF_DATA) as infile:
        conf = json.load(infile)
      #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
      COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
      COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

      self.Window_cfile.destroy()
      self.Window_cfile = None


    def _select():
    # ============
      global COSMO_CONF,COSMO_CONF_PATH,COSMO_CONF_NAME
      nn = tk.filedialog.askdirectory(parent=self.Window_cfile,
                                      initialdir=COSMO_CONF_PATH)
      if len(nn) == 0:
        return

      if os.path.isdir(nn):
        print('Configuration folder exists')
        COSMO_CONF_NAME = '%s' % os.path.basename(os.path.normpath(nn))
        COSMO_CONF = nn + os.sep
      else:
        print('New Configuration folder')
        os.makedirs(nn)
        COSMO_CONF_NAME = '%s' % os.path.basename(os.path.normpath(nn))
        COSMO_CONF = nn + os.sep
      print('COSMO_CONF_PATH = ',COSMO_CONF_PATH)
      print('COSMO_CONF_NAME = ',COSMO_CONF_NAME)
      print('COSMO_CONF = ',COSMO_CONF)
      self.PLOT.FILECONF = COSMO_CONF + 'drawing.conf'
      print('self.PLOT.FILECONF = ',self.PLOT.FILECONF)

    # Main window
    # -----------
    if self.Window_cfile is not None:
      self.Window_cfile.lift()
      return

    self.Window_cfile = tk.Toplevel(self.master)
    self.Window_cfile.title('Configuration file')
    self.Window_cfile.resizable(width=False,height=False)
    self.Window_cfile.protocol('WM_DELETE_WINDOW',_cancel)

    font_bold = tkfont.Font(font='TkDefaultFont').copy()
    font_bold['weight']='bold'

    F0 = ttk.Frame(self.Window_cfile,borderwidth=5,padding=5)
    ttk.Label(F0,text='Configuration PATH: ',
                 font=font_bold).grid(row=0,column=0)
    ttk.Label(F0,text=COSMO_CONF_PATH,width=40,
                 justify='left').grid(row=0,column=1,columnspan=4)
    ttk.Label(F0,text='Configuration name: ',
                 font=font_bold).grid(row=1,column=0)
    ttk.Label(F0,text=COSMO_CONF_NAME,width=40,
                 justify='left').grid(row=1,column=1,columnspan=4)
    ttk.Label(F0,text='New configuration name',
                 font=font_bold).grid(row=2,column=0)
    ttk.Button(F0,text='Select',command=_select).grid(row=2,column=1,padx=3)

    cancel = ttk.Button(F0,text='Cancel',command=_cancel)
    cancel.grid(row=3,column=0,padx=3)
    cancel.bind("<Return>",lambda e:_cancel())
    done = ttk.Button(F0,text='Done',command=_done)
    done.grid(row=3,column=1,padx=3)
    done.bind("<Return>",lambda e:_done())
    F0.grid()


  # ==================================
  def figure_save(self):
  # ==================================
    ''' Saving Drawing configuration using json'''

    print('Saving figure ...')

    CONF = []

    # Add the main PLOT class:
    #
    conf = self.PLOT.conf_get()
    CONF.append(conf)
 

    # Add the FILES (SAIDIN; CONTOURS, VECTORS, TRAJECTORIES):
    # Types: VEC, FLD, SAIDIN, FLOAT
    # ZZZ
    for i in range(self.nfiles):

      ii = self.FILEORDER[i]

      conf = {}
      conf['FILENAME'] = self.FILENAMES[i]
      conf['TYPE']     = self.FILETYPES[i]
      conf['SEQUENCE'] = self.SEQUENCES[i].get()
      if self.FILETYPES[i] == 'FLD':
        conf['CDF'] = self.CDF[ii].conf_get()
      elif self.FILETYPES[i] == 'VEC':
        conf['VEC'] = self.VEC[ii].conf_get()
      elif self.FILETYPES[i] == 'SAIDIN':
        conf['SAIDIN'] = self.SAIDIN.conf_get()
      elif self.FILETYPES[i] == 'FLOAT':
        conf['FLOAT'] = self.FLOAT[ii].conf_get()
      else:
        print('Unknown file type')
        return

      CONF.append(conf)

    for i in range(self.nfeatures):

      ii = self.FEATORDER[i]

      conf = {}
      conf['FILENAME'] = self.FEATNAMES[i]
      conf['TYPE']     = self.FEATTYPES[i]
      if self.FEATTYPES[i] == 'MARKER':
        conf['MARKER'] = self.MARKER[ii].conf_get()

      CONF.append(conf)

    # Request output configuration filename:
    #
    filetypes = [('COSMO-VIEW','.cvw')]
    nn = filedialog.asksaveasfilename(title='Save plot configuration',
                             initialdir='./',
                             filetypes=filetypes,
                             confirmoverwrite=True)
    if nn is None or len(nn) == 0:
      return

    # Write JSON file:
    #
    self.save_conf(CONF,nn)


  # ==================================
  def figure_read(self,filename=None):
  # ==================================
    ''' Load Figure configuration from json'''

    self.first = True

    if filename is None:
      nn = filedialog.askopenfilename(title='Load plot configuration',
                                      initialdir='./')
      if len(nn) == 0:
        return
      filename = '%s' % nn

    print('Restoring figure configuration from ',filename)

    CONF = json.load(open(filename))

    # The PLOT:
    #
    self.PLOT.conf_set(CONF[0])

    # Initialize matplotlib
    #
    self.fig = None
    self.ax  = None
    self.drawmap = True
    #try:
    #  self.fig = plt.figure('COSMO-VIEW canvas',    \
    #                      figsize=self.PLOT.SIZE, \
    #                      dpi=self.PLOT.DPI.get())
    #except:
    #  print('Failure')
#
#    self.fig.canvas.mpl_connect('close_event',self.on_closing_figure)
#    self.fig.canvas.callbacks.connect('button_press_event',self.on_click)
#    self.ax = self.fig.add_subplot(111)
#    self.drawmap = True

    for ii in range(1,len(CONF)):

      filename = CONF[ii]['FILENAME']

      if CONF[ii]['TYPE'] == 'FLD':

        # Initialize classes:
        #
        CDF = cdf_parameters()
        CDF.FIELD = fld_parameters()
        CDF.FILENAME.set(filename)
        CDF.ncid = Dataset(filename)
        CDF.icdf = tools.geocdf(filename)

        # Update from CONF attributes:
        #
        CDF.conf_set(CONF[ii]['CDF'])

        if self.first:
          self.K.set(CDF.K.get())
          self.L.set(CDF.L.get())
          self.L_LIST = list(range(CDF.icdf.nt))
          self.NL = len(self.L_LIST)

        # Read data:
        #
        self.read_lonlat(CDF,CDF.icdf.xname,CDF.icdf.yname)
        self.DepthandDate(CDF)
        self.read_CDF(CDF,update_lims=False)

        self.ncdf += 1
        self.CDF.append(CDF)
        self.CDF_INDX.set(self.ncdf-1)
        self.CDF_LIST = list(range(self.ncdf))

        self.nfiles += 1
        self.FILENAMES.append(filename)
        self.FILETYPES.append('FLD')
        self.FILEORDER.append(self.ncdf-1)
        self.SEQUENCES.append(tk.BooleanVar(value=CONF[ii]['SEQUENCE']))

        if self.first:
          #self.TFILE = '%d' % self.nfiles
          self.PLOT.TLABEL.set(CDF.DATE[self.L.get()])

          self.lbox.configure(state='!disabled')
          self.lbox['values'] = self.L_LIST

          self.DATE = CDF.DATE.copy()
          self.TIME = CDF.TIME.copy()
          if self.L.get() == 0:
            self.bprev.configure(state='disabled')
          else:
            self.bprev.configure(state='normal')
          if self.L.get() == self.NL - 1:
            self.bnext.configure(state='disabled')
          else:
            self.bnext.configure(state='normal')

          #self.TFILE = '%d' % self.nfiles
          self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
          self.first = False


      if CONF[ii]['TYPE'] == 'VEC':

        # Initialize classes:
        #
        VEC = cdf_parameters()
        VEC.VEL = vel_parameters()
        VEC.FILENAME.set(filename)
        VEC.ncid = Dataset(filename)
        VEC.icdf = tools.geocdf(filename)
       
        # Update from CONF attributes:
        #
        VEC.conf_set(CONF[ii]['VEC'])

        if self.first:
          self.K.set(VEC.K.get())
          self.L.set(VEC.L.get())
          self.L_LIST = list(range(VEC.icdf.nt))
          self.NL = len(self.L_LIST)

        # Read data:
        #
        self.read_lonlat(VEC,VEC.icdf.xname,VEC.icdf.yname)
        self.DepthandDate(VEC)
        self.read_UV(VEC)

        self.nvec += 1
        self.VEC.append(VEC)
        self.VEC_INDX.set(self.nvec-1)
        self.VEC_LIST = list(range(self.nvec))

        self.nfiles += 1
        self.FILENAMES.append(filename)
        self.FILETYPES.append('VEC')
        self.FILEORDER.append(self.nvec-1)
        self.SEQUENCES.append(tk.BooleanVar(value=CONF[ii]['SEQUENCE']))

        if self.first:
          #self.TFILE = '%d' % self.nfiles
          self.PLOT.TLABEL.set(VEC.DATE[self.L.get()])

          self.lbox.configure(state='!disabled')
          self.lbox['values'] = self.L_LIST

          self.DATE = VEC.DATE.copy()
          self.TIME = VEC.TIME.copy()
          if self.L.get() == 0:
            self.bprev.configure(state='disabled')
          else:
            self.bprev.configure(state='normal')
          if self.L.get() == self.NL - 1:
            self.bnext.configure(state='disabled')
          else:
            self.bnext.configure(state='normal')

          #self.TFILE = '%d' % self.nfiles
          self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
          self.first = False

      if CONF[ii]['TYPE'] == 'FLOAT':

        # Initialize classes:
        #
        FLT = lagrangian.parameters()
        FLT.Read(filename)

        # Update from CONF attributes:
        FLT.conf_set(CONF[ii]['FLOAT'])

        if self.first:
          # Set Figure DATA and TIME reference:
          if FLT.SOURCE == 'blm':
            self.DATE = [FLT.date[i].replace(tzinfo=None) for i in
                                                  range(FLT.nrecords)]
            self.TIME = np.array([(FLT.date[i].replace(tzinfo=None)-
                                   self.DATE[0]).total_seconds() 
                                       for i in range(FLT.nrecords)])
          elif FLT.SOURCE == 'mlm':
            self.DATE = [FLT.date[i][0].replace(tzinfo=None)
                              for i in range(FLT.nrecords)]
            self.TIME = np.array([(FLT.date[i][0].replace(tzinfo=None)-
                                   self.DATE[0]).total_seconds()
                                       for i in range(FLT.nrecords)])

        # Set each FLOAT time values and interpolated positions.

        if FLT.SOURCE == 'blm':
          FLT.TIME = np.array([(FLT.date[i].replace(tzinfo=None)-
                                self.DATE[0]).total_seconds() 
                                    for i in range(FLT.nrecords)])
          FLT.MAPX = []
          FLT.MAPY = []
          if FLT.nfloats > 1:
            for i in range(FLT.nfloats):
              f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon[:,i]),
                                       bounds_error=False, fill_value=np.NaN)
              FLT.MAPX.append(list(f(self.TIME)))
              f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat[:,i]),
                                       bounds_error=False, fill_value=np.NaN)
              FLT.MAPY.append(list(f(self.TIME)))
            # Transpose FLT.MAPX and FLT.MAPY:
            FLT.MAPX = np.array(FLT.MAPX).T.tolist()
            FLT.MAPY = np.array(FLT.MAPY).T.tolist()
          else:
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon),
                                     bounds_error=False, fill_value=np.NaN)
            FLT.MAPX = list(f(self.TIME))
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat),
                                     bounds_error=False, fill_value=np.NaN)
            FLT.MAPY = list(f(self.TIME))
          error = 0

        elif FLT.SOURCE == 'mlm':
          FLT.TIME = []
          FLT.MAPX = []
          FLT.MAPY = []
          for j in range(FLT.nfloats):
            FTIME = np.array([(FLT.date[i][j].replace(tzinfo=None)-
                               self.DATE[0]).total_seconds()
                                   for i in range(FLT.nrecords)])
            f = interpolate.interp1d(FTIME,np.array(FLT.lon[:,j]),
                                     bounds_error=False,fill_value=np.NaN)
            FLT.MAPX.append(list(f(self.TIME)))
            f = interpolate.interp1d(FTIME,np.array(FLT.lat[:,j]),
                                     bounds_error=False,fill_value=np.NaN)
            FLT.MAPY.append(list(f(self.TIME)))
            FLT.TIME.append(FTIME)

          # Transpose FLT.MAPX and FLT.MAPY:
          FLT.MAPX = np.array(FLT.MAPX).T.tolist()
          FLT.MAPY = np.array(FLT.MAPY).T.tolist()
          FLT.TIME = np.array(FLT.TIME).T.tolist()
          error = 0
        else:
          messagebox.showinfo(message='FLOAT source not found')
          error = 1
          
        if error == 0:

          if self.first:
            self.PLOT.TLABEL.set(self.DATE[self.L.get()])
            self.L_LIST = list(range(len(FLT.date)))
            self.NL = len(self.L_LIST)
            self.lbox.configure(state='!disabled')
            self.lbox['values'] = self.L_LIST
            if self.L.get() == 0:
              self.bprev.configure(state='disabled')
            else:
              self.bprev.configure(state='normal')
            if self.L.get() == self.NL - 1:
              self.bnext.configure(state='disabled')
            else:
              self.bnext.configure(state='normal')
            self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
            self.first = False

          self.nfloat += 1
          self.FLOAT.append(FLT)
          self.FLOAT_INDX.set(self.nfloat-1)
          self.FLOAT_LIST = list(range(self.nfloat))

          self.nfiles += 1
          self.FILENAMES.append(filename)
          self.FILETYPES.append('FLOAT')
          self.FILEORDER.append(self.nfloat-1)
          self.SEQUENCES.append(tk.BooleanVar(value=CONF[ii]['SEQUENCE']))

      if CONF[ii]['TYPE'] == 'SAIDIN':

        # Initialize classes:
        #
        self.SAIDIN.FILENAME.set(filename)
        self.SAIDIN.ncid = Dataset(filename)
        self.SAIDIN.icdf = tools.geocdf(filename)

        # Update from CONF attributes:
        #
        self.SAIDIN.conf_set(CONF[ii]['SAIDIN'])


        # Read the data:
        self.SAIDIN.lon = self.SAIDIN.ncid.variables['lon'][:]
        self.SAIDIN.lat = self.SAIDIN.ncid.variables['lat'][:]
        self.SAIDIN.FIELD.varname = 'mcsst'
        self.SAIDIN.FIELD.data = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname][0,:,:].squeeze()
        self.SAIDIN.xx,self.SAIDIN.yy = np.meshgrid(self.SAIDIN.lon,self.SAIDIN.lat)
        self.DepthandDate(self.SAIDIN)

        self.nfiles += 1
        self.FILENAMES.append(filename)
        self.FILETYPES.append('SAIDIN')
        self.FILEORDER.append(0)
        self.SEQUENCES.append(tk.BooleanVar(value=CONF[ii]['SEQUENCE']))

        if self.first:
          self.DATE = self.SAIDIN.DATE.copy()
          self.TIME = self.SAIDIN.TIME.copy()
          self.first = False


      if CONF[ii]['TYPE'] == 'MARKER':

        # Initialize classes:
        #
        MARKER = geomarker.parameters()
        MARKER.Read(filename)

        # Update from CONF attributes:
        #
        MARKER.conf_set(CONF[ii]['MARKER'])

        self.nmarker += 1
        self.MARKER.append(MARKER)
        self.MARKER_INDX.set(self.nmarker-1)
        self.MARKER_LIST = list(range(self.nmarker))

        self.nfeatures += 1
        self.FEATNAMES.append(filename)
        self.FEATTYPES.append('MARKER')
        self.FEATORDER.append(self.nfeatures-1)


    self.make_plot()


  # ===========================
  def save_conf(self,conf,filename):
  # ===========================

    # Write JSON file:
    with io.open(filename,'w',encoding='utf8') as outfile:
      str_ = json.dumps(conf,ensure_ascii=False, \
                             sort_keys=True,     \
                             indent=2,           \
                             separators=(',',': '))
      outfile.write(to_unicode(str_))



  # =============
  def save(self):
  # =============
    '''If name has been already set, save the plot'''
    if self.PLOT.OUT_FILENAME is None:
      self.saveas()

    # If output filename exist, we save:
    if self.PLOT.OUT_FILENAME is not None:
      print('Saving in ',self.PLOT.OUT_FILENAME)
      self.fig.savefig(self.PLOT.OUT_FILENAME,
                       dpi=self.PLOT.DPI.get(),
                       bbox_inches='tight')


  # ===============
  def saveas(self):
  # ===============
      '''Get the output filename and the save the plot'''
      filetypes = [('PNG file','.png'),('JPG file','.jpg'),('PDF file','.pdf')]
      nn = tk.filedialog.asksaveasfilename(title='Save',
                                           initialdir='./',
                                           filetypes=filetypes,
                                           confirmoverwrite=True)
      if len(nn) == 0:
        self.PLOT.OUT_FILENAME = None
      else:
        self.PLOT.OUT_FILENAME = '%s' % nn
        print('Saving in ',self.PLOT.OUT_FILENAME)
        self.fig.savefig(self.PLOT.OUT_FILENAME,
                         dpi=self.PLOT.DPI.get(),
                         bbox_inches='tight')


  # ======================
  def widget_config(self):
  # ======================
    '''Options for the widget font type and size'''

    #global WINDOW_FONT_TYPE_BACKUP
    #global WINDOW_FONT_SIZE_BACKUP

    def _cancel():
    # ===========

      self.PLOT.WINDOW_FONT_TYPE.set(WINDOW_FONT_TYPE_BACKUP)
      self.PLOT.WINDOW_FONT_SIZE.set(WINDOW_FONT_SIZE_BACKUP)
      self.Window_widgetconfig.destroy()
      self.Window_widgetconfig = None

    def _close():
    # ===========
      self.Window_widgetconfig.destroy()
      self.Window_widgetconfig = None

    def _apply():
    # ===========
      font_name = self.PLOT.WINDOW_FONT_TYPE.get().split()[0]
      font = '%s %d' % (font_name, self.PLOT.WINDOW_FONT_SIZE.get())
      self.master.option_add('*Font',font)

      self.default_font.configure(family=self.PLOT.WINDOW_FONT_TYPE.get().
                                                              split()[0])
      self.default_font.configure(size=self.PLOT.WINDOW_FONT_SIZE.get())

      if self.Window_mapconfig is not None:
        self.Window_mapconfig.destroy()
        self.Window_mapconfig = None
        self.map_config()
        

    def _loadconf():
    # =============
      '''Load the Widget config parameters'''
      conf = self.PLOT.conf_load(self.PLOT.FILECONF)
      self.PLOT.WINDOW_FONT_TYPE.set(conf['WINDOW_FONT_TYPE'])
      self.PLOT.WINDOW_FONT_SIZE.set(conf['WINDOW_FONT_SIZE'])


    def _saveconf():
    # =============
      '''Save the Widget config parameters'''
      if self.widget_nowarning.get() == False:
        ans = askforpermission(self.Window_widgetconfig, \
                               'Are you sure ?',       \
                               self.widget_nowarning)
        if ans == False:
          return

      print('Updating widget font default values')
      conf = self.PLOT.conf_load(self.PLOT.FILECONF)
      conf['WINDOW_FONT_TYPE'] = self.PLOT.WINDOW_FONT_TYPE.get()
      conf['WINDOW_FONT_SIZE'] = self.PLOT.WINDOW_FONT_SIZE.get()
      self.save_conf(conf,self.PLOT.FILECONF)



    if self.Window_widgetconfig is not None:
      self.Window_widgetconfig.lift()
      return

    WINDOW_FONT_TYPE_BACKUP = self.PLOT.WINDOW_FONT_TYPE.get()
    WINDOW_FONT_SIZE_BACKUP = self.PLOT.WINDOW_FONT_SIZE.get()

    self.Window_widgetconfig = tk.Toplevel(self.master)
    self.Window_widgetconfig.title('Widget options')
    self.Window_widgetconfig.resizable(width=True,height=True)
    self.Window_widgetconfig.protocol('WM_DELETE_WINDOW',_close)

    self.widget_nowarning = tk.BooleanVar()
    self.widget_nowarning.set(False)

    menubar = tk.Menu(self.Window_widgetconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Save',command=_saveconf)
    try:
      self.Window_widgetconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.Window_widgetconfig.tk.call(self.Window_widgetconfig, "config", "-menu", menubar)


    F0 = ttk.Frame(self.Window_widgetconfig,borderwidth=5,padding=5)
    ttk.Label(F0,text='Font style').grid(row=0,column=0,padx=3,sticky='w')
    mp = ttk.Combobox(F0,textvariable=self.PLOT.WINDOW_FONT_TYPE,values=self.FONT_TYPES,width=35)
    mp.grid(row=0,column=1,columnspan=7,padx=3,sticky='w')
    ttk.Label(F0,text='Font size').grid(row=1,column=0,padx=3,sticky='w')
    mp = ttk.Combobox(F0,textvariable=self.PLOT.WINDOW_FONT_SIZE,values=self.FONT_SIZES,width=5)
    mp.grid(row=1,column=1,columnspan=1,padx=3,sticky='w')
    ttk.Button(F0,text='Cancel',command=_cancel).grid(row=2,column=5,padx=3)
    ttk.Button(F0,text='Apply',command=_apply).grid(row=2,column=6,padx=3)
    ttk.Button(F0,text='Close',command=_close).grid(row=2,column=7,padx=3)
    F0.grid()

  # =====================
  def isobath_crop(self):
  # =====================
    '''Crop isobaths from domain'''
    west = self.PLOT.WEST.get() - 5
    east = self.PLOT.EAST.get() + 5
    south = self.PLOT.SOUTH.get() - 5
    north = self.PLOT.NORTH.get() + 5

    print('Cropping isobaths')
    for i in range(self.PLOT.nisobat):
      if self.PLOT.ISOBAT_SHOW[i]:
        xo = self.PLOT.ISOBAT_DATA[i]['lon']
        yo = self.PLOT.ISOBAT_DATA[i]['lat']
        for ii in range(len(xo)-1,-1,-1):
          if xo[ii] < west:
            del xo[ii]
            del yo[ii]
          elif xo[ii] > east:
            del xo[ii]
            del yo[ii]
          elif yo[ii] < south:
            del xo[ii]
            del yo[ii]
          elif yo[ii] > north:
            del xo[ii]
            del yo[ii]
          else:
            pass
        self.PLOT.ISOBAT_DATA[i]['lon'] = xo
        self.PLOT.ISOBAT_DATA[i]['lat'] = yo
    print('done')
    self.PLOT.ISOBATH_crop = True


  # ======================
  def legend_config(self):
  # ======================
    '''Options for Map limits and colors'''

    def _apply():
    # ===========
      self.make_plot()

    def _close():
    # ==========
      self.make_plot()
      self.Window_legendconfig.destroy()
      self.Window_legendconfig = None

    def _loadconf():
    # =============
      '''Load map configuration'''
      conf = self.PLOT.conf_load(self.PLOT.FILECONF)
      self.PLOT.conf_set(conf)

    def _saveconf():
    # =============
      '''Save current map configuration as default'''

      print('Updating map default values')
      conf = self.PLOT.conf_get()
      self.save_conf(conf,self.PLOT.FILECONF)


    self.Window_legendconfig = tk.Toplevel(self.master)
    self.Window_legendconfig.title('Legend options')
    self.Window_legendconfig.resizable(width=True,height=True)
    self.Window_legendconfig.protocol('WM_DELETE_WINDOW',_close)
    self.map_nowarning = tk.BooleanVar()
    self.map_nowarning.set(False)
 
    menubar = tk.Menu(self.Window_legendconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Save',command=_saveconf)
    try:
      self.Window_legendconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.Window_legendconfig.tk.call(self.Window_legendconfig, "config", "-menu", menubar)
 

    # Define tabs
    maptabs = ttk.Notebook(self.Window_legendconfig)
    page1 = ttk.Frame(maptabs)
    page2 = ttk.Frame(maptabs)

    maptabs.add(page1,text='Isobaths')
    maptabs.add(page2,text='Markers')

    self.PLOT.ISOBAT_LEGEND.Winconfig(page1)
    self.PLOT.LEGEND.Winconfig(page2)

    maptabs.grid()

    frame5 = ttk.Frame(self.Window_legendconfig,borderwidth=5,padding=5)
    ttk.Button(frame5,text='Apply',command=_apply).grid(row=0,column=5,padx=3)
    ttk.Button(frame5,text='Close',command=_close).grid(row=0,column=6,padx=3)
    frame5.grid(row=24,column=0,columnspan=5)

 

  # ===================
  def map_config(self):
  # ===================
    '''Options for Map limits and colors'''

    pdict = {'aeqd' : 'Azimuthal Equidistant Projection',
             'cass' :'Cassini',
             'cyl'  :'Cylindrical Equidistant',
             'geos' :'Geostationary Projection',
             'gnom' :'Gnomonic Projection',
             'merc' :'Mercator',
             'moll' :'Mollweide',
             'cea'  :'Cylindrical Equal Area',
             'gall' :'Gall Stereographic Cylindrical', 
             'ortho':'Orthographic Projection',
             'tmerc':'Transverse Mercator',
             'epsg' :'European Petroleum Survey Group',
            }

    rdict = {'c':'Crude',             \
             'l':'Low',               \
             'i':'Intermediate',      \
             'h':'High',              \
             'f':'Full',              \
             }

    LEGEND_LOCATION_LIST = ['best','upper right','upper left','lower left', \
                     'lower right', 'right', 'center left', 'center right', \
                     'lower center', 'upper center', 'center']
    LEGEND_MODE_LIST = ['None','expand']

    PSIZE = tk.StringVar()
    PSIZE.set(str(self.PLOT.SIZE))

    BACKUP = self.PLOT.conf_get()

    font_norm = tkfont.Font(font='TkDefaultFont').copy()
    font_bold = tkfont.Font(font='TkDefaultFont').copy()
    font_bold['weight']='bold'

    def _cancel():
      self.PLOT.conf_set(BACKUP)
      self.Window_mapconfig.destroy()
      self.Window_mapconfig = None
      if self.fig is not None:
        self.make_plot()

    def _close():
      self.Window_mapconfig.destroy()
      self.Window_mapconfig = None

    def pselection():
      mpl.config(text=pdict[self.PLOT.MAP_PROJECTION.get()],width=25)
      self.drawmap = True

    def rselection():
      mrl.config(text=rdict[self.PLOT.MAP_RESOLUTION.get()],width=10)
      self.drawmap = True

    def ccselection():
      backup = self.PLOT.COASTLINE_COLOR.get()
      rgb, hx = askcolor(color=self.PLOT.COASTLINE_COLOR.get(),
                         parent=self.master)
      if hx is None:
        self.PLOT.COASTLINE_COLOR.set(backup)
      else:
        self.PLOT.COASTLINE_COLOR.set(hx)

    def ycselection():
      backup = self.PLOT.COUNTRYLINE_COLOR.get()
      rgb, hx = askcolor(color=self.PLOT.COUNTRYLINE_COLOR.get(),
                         parent=self.master)
      if hx is None:
        self.PLOT.COUNTRYLINE_COLOR.set(backup)
      else:
        self.PLOT.COUNTRYLINE_COLOR.set(hx)

    def icselection():
      ii = self.PLOT.ISOBAT_LABEL.index(self.PLOT.ISOBAT_ZPOINTER.get())
      backup = self.PLOT.ISOBAT_COLOR[ii].get()
      rgb, hx = askcolor(color=self.PLOT.ISOBAT_COLOR[ii].get(),
                         parent=self.master)
      if hx is None:
        self.PLOT.ISOBAT_COLOR[ii].set(backup)
      else:
        self.PLOT.ISOBAT_COLOR[ii].set(hx)

    def lcselection():
      backup = self.PLOT.LAND_COLOR.get()
      if self.PLOT.LAND_COLOR.get() == 'None':
        rgb, hx = askcolor(parent=self.master)
      else:
        rgb, hx = askcolor(color=self.PLOT.LAND_COLOR.get(),
                         parent=self.master)
      if hx is None:
        self.PLOT.LAND_COLOR.set(backup)
      else:
        self.PLOT.LAND_COLOR.set(hx)

    def wcselection():
      backup = self.PLOT.WATER_COLOR.get()
      if self.PLOT.WATER_COLOR.get() == 'None':
        rgb, hx = askcolor(parent=self.master)
      else:
        rgb, hx = askcolor(color=self.PLOT.WATER_COLOR.get(),
                         parent=self.master)
      if hx is None:
        self.PLOT.WATER_COLOR.set(backup)
      else:
        self.PLOT.WATER_COLOR.set(hx)

    def rcselection():
      backup = self.PLOT.RIVERS_COLOR.get()
      if self.PLOT.RIVERS_COLOR.get() == 'None':
        rgb, hx = askcolor(parent=self.master)
      else:
        rgb, hx = askcolor(color=self.PLOT.RIVERS_COLOR.get(),
                         parent=self.master)
      if hx is None:
        self.PLOT.RIVERS_COLOR.set(backup)
      else:
        self.PLOT.RIVERS_COLOR.set(hx)


    def lims_reset():
    # ================
      ''' Resets the domain and grid to the default values'''

      conf = self.PLOT.conf_load(self.PLOT.FILECONF)

      self.PLOT.WEST.set(conf['WEST'])
      self.PLOT.EAST.set(conf['EAST'])
      self.PLOT.SOUTH.set(conf['SOUTH'])
      self.PLOT.NORTH.set(conf['NORTH'])

      self.PLOT.MERIDIAN_INI.set(conf['MERIDIAN_INI'])
      self.PLOT.MERIDIAN_FIN.set(conf['MERIDIAN_FIN'])
      self.PLOT.MERIDIAN_INT.set(conf['MERIDIAN_INT'])
      self.PLOT.PARALLEL_INI.set(conf['PARALLEL_INI'])
      self.PLOT.PARALLEL_FIN.set(conf['PARALLEL_FIN'])
      self.PLOT.PARALLEL_INT.set(conf['PARALLEL_INT'])

      self.drawmap = True
      self.make_plot()

    def iload():
    # =================
      '''Load from external file the selected isobaths'''

      for i in range(self.PLOT.nisobat):
        if self.PLOT.ISOBAT_SELEC[i].get():
          filename = self.PLOT.ISOBAT_PATH.get() + \
                     '/%04d' % self.PLOT.ISOBAT_Z[i] + '.dat'
          try:
            self.PLOT.ISOBAT_DATA[i] = read_lines(filename)
            self.PLOT.ISOBAT_SHOW[i] = True
            wwr.configure(font=font_norm)
            wwr.configure(foreground='#125704')
            wwr['text'] = 'Isobaths have been loaded'
            self.PLOT.ISOBAT_loaded = True
          except:
            messagebox.showinfo(message='Error: unable to read '+filename)
            self.PLOT.ISOBAT_DATA[i] = None
            self.PLOT.ISOBAT_SHOW[i] = False

      self.PLOT.ISOBAT_NPLOT = sum(self.PLOT.ISOBAT_SHOW)
      if self.PLOT.ISOBAT_loaded:
        wlr.configure(state='enabled')
      else:
        wlr.configure(state='disabled')
      self.PLOT.ISOBATH_crop = False
     
    def _pselect():
    # =================
      nn = tk.filedialog.askdirectory(parent=self.Window_mapconfig)
      if not empty(nn):
        self.PLOT.ISOBAT_PATH.set(nn)

    def select_isobaths():
    # =================
      some_selected = False
      for i in range(self.PLOT.nisobat):
        if self.PLOT.ISOBAT_SELEC[i].get():
          some_selected = True

      if some_selected:
        wwr['text'] = 'Isobaths need to be loaded'
        wwr.configure(font=font_bold)
        wwr.configure(foreground='red')
        self.PLOT.ISOBAT_selected = True
        self.PLOT.ISOBAT_loaded = False
      else:
        wwr['text'] = 'No isobaths have been selected'
        wwr.configure(font=font_norm)
        wwr.configure(foreground='black')
        self.PLOT.ISOBAT_selected = False
      if self.PLOT.ISOBAT_selected:
        wli.configure(state='enabled')
      else:
        wli.configure(state='disabled')

      for i in range(self.PLOT.nisobat):
        self.PLOT.ISOBAT_DATA[i] = None
        self.PLOT.ISOBAT_SHOW[i] = False
        self.PLOT.ISOBAT_NPLOT = 0



    def _updated():
    # =============
      self.drawmap = True
      self.make_plot()

    def _apply():
    # ===========
      self.make_plot()

    def _done():
    # ==========
      self.make_plot()
      self.Window_mapconfig.destroy()
      self.Window_mapconfig = None

    def _loadconf():
    # =============
      '''Load map configuration'''
      conf = self.PLOT.conf_load(self.PLOT.FILECONF)
      self.PLOT.conf_set(conf)
 
    def _saveconf():
    # =============
      '''Save current map configuration as default'''

      print('Updating map default values')
      conf = self.PLOT.conf_get()
      self.save_conf(conf,self.PLOT.FILECONF)

    def legend_location():
    # =============
      ''' Process the location combobox'''
      location_name = loc.get()
      self.PLOT.LEGEND.LOC.set(LEGEND_LOCATION_LIST.index(location_name))
      
    def legend_mode():
    # =============
      ''' Process the location combobox'''
      mode_name = mod.get()
      self.PLOT.LEGEND.MODE.set(LEGEND_MODE_LIST.index(mode_name))
      
    def sizeupdate():
    # ===============
      self.PLOT.SIZE = ast.literal_eval(PSIZE.get())
      plt.close(self.fig)
      self.fig = None
      self.make_plot()
       
    def _calculator():
    # ================
      SOUTH = float(self.PLOT.SOUTH.get())
      NORTH = float(self.PLOT.NORTH.get())
      WEST  = float(self.PLOT.WEST.get())
      EAST  = float(self.PLOT.EAST.get())
      LON_0  = 0.5*(WEST+EAST)
      LAT_0  = 0.5*(SOUTH+NORTH)
      width = haversine((WEST,LAT_0),(EAST,LAT_0))
      height = haversine((LON_0,SOUTH),(LON_0,NORTH))
      self.PLOT.LON_0.set(LON_0)
      self.PLOT.LAT_0.set(LAT_0)
      self.PLOT.WIDTH.set(width)
      self.PLOT.HEIGHT.set(height)

    if self.Window_mapconfig is not None:
      self.Window_mapconfig.lift()
      return

    self.Window_mapconfig = tk.Toplevel(self.master)
    self.Window_mapconfig.title('Map options')
    self.Window_mapconfig.resizable(width=True,height=True)
    self.Window_mapconfig.protocol('WM_DELETE_WINDOW',_close)
    self.map_nowarning = tk.BooleanVar()
    self.map_nowarning.set(False)
 
    menubar = tk.Menu(self.Window_mapconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Save',command=_saveconf)
    try:
      self.Window_mapconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.Window_mapconfig.tk.call(self.Window_mapconfig, "config", "-menu", menubar)
 

    # Define tabs
    maptabs = ttk.Notebook(self.Window_mapconfig)
    page1 = ttk.Frame(maptabs)
    page2 = ttk.Frame(maptabs)
    page3 = ttk.Frame(maptabs)
    page4 = ttk.Frame(maptabs)
    page5 = ttk.Frame(maptabs)
    page7 = ttk.Frame(maptabs)
    page8 = ttk.Frame(maptabs)

    maptabs.add(page1,text='Domain')
    maptabs.add(page2,text='Background')
    maptabs.add(page3,text='Isobaths')
    maptabs.add(page4,text='Grid')
    maptabs.add(page5,text='Labels')
    maptabs.add(page7,text='Scale')
    maptabs.add(page8,text='Other')

    PROJECTION_LIST = ['aeqd',
                       'cass',
                       'cea',
                       'cyl',
                       'gall',
                       'geos',
                       'gnom',
                       'merc',
                       'moll',
                       'ortho',
                       'tmerc',
                       'epsg']

    f1 = ttk.Frame(page1,borderwidth=5,padding=5)
    ttk.Label(f1,
              text='Map Projection').grid(row=0,
                                          column=0,
                                          padx=3,
                                          sticky='w')
    mp = ttk.Combobox(f1,
                      textvariable=self.PLOT.MAP_PROJECTION,
                      values=PROJECTION_LIST,width=7)
    mp.grid(row=0,column=1,padx=3)
    mp.bind('<<ComboboxSelected>>',lambda e: pselection())
    mpl = ttk.Label(f1,
                    text=pdict[self.PLOT.MAP_PROJECTION.get()],
                    width=40)
    mpl.grid(row=0,column=2,columnspan=3,padx=3)

    ttk.Label(f1,text='Map Resolution').grid(row=1,
                                             column=0,
                                             padx=3,
                                             sticky='w')
    mr = ttk.Combobox(f1,
                      textvariable=self.PLOT.MAP_RESOLUTION,
                      values=('c','l','i','h','f'),width=7)
    mr.grid(row=1,column=1,padx=3)
    mr.bind('<<ComboboxSelected>>',lambda e: rselection())
    mrl = ttk.Label(f1,
                    text=rdict[self.PLOT.MAP_RESOLUTION.get()],
                    width=10)
    mrl.grid(row=1,column=2,columnspan=2,padx=3,sticky='w')

    ttk.Label(f1,text='EPSG').grid(row=2,column=0,padx=3,sticky='w')
    ttk.Entry(f1,textvariable=self.PLOT.EPSG,
                 width=7).grid(row=2,column=1,padx=3,sticky='w')
    f1.grid(row=0,column=0)

    f2 = ttk.Frame(page1,borderwidth=5,padding=5,relief='sunken')
    ttk.Label(f2,
              text='Plot limits',
              font='Helvetica 12 bold').grid(row=0,
                                             column=0,
                                             padx=3,
                                             sticky='w')
    ttk.Label(f2,text='North').grid(row=1,column=3,pady=5,padx=3)
    eno = ttk.Entry(f2,textvariable=self.PLOT.NORTH,width=10)
    eno.grid(row=2,column=3,pady=5,padx=3)
    eno.bind('<Return>',lambda e:_updated())

    ttk.Label(f2,text='West').grid(row=3,column=1,pady=5,padx=3)
    ewe = ttk.Entry(f2,textvariable=self.PLOT.WEST,width=10)
    ewe.grid(row=3,column=2,pady=5,padx=3)
    ewe.bind('<Return>',lambda e:_updated())

    eea = ttk.Entry(f2,textvariable=self.PLOT.EAST,width=10)
    eea.grid(row=3,column=4,pady=5,padx=3)
    eea.bind('<Return>',lambda e:_updated())
    ttk.Label(f2,text='East').grid(row=3,column=5,pady=5,padx=3)
    eso = ttk.Entry(f2,textvariable=self.PLOT.SOUTH,width=10)
    eso.grid(row=4,column=3,pady=5,padx=3)
    eso.bind('<Return>',lambda e:_updated())
    ttk.Label(f2,text='South').grid(row=5,column=3,pady=5,padx=3)
    ttk.Button(f2,text='Reset',command=lims_reset).grid(row=6,column=5)
    f2.grid()

    fh = ttk.Frame(page1,borderwidth=5,padding=5)
    ttk.Label(fh,text='Width').grid(row=0,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.WIDTH,
                 width=10).grid(row=0,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Optional map keywords').grid(row=0,column=2,
                                                   padx=3,sticky='e')
    ttk.Button(fh,text='Estimate',
                  command=_calculator).grid(row=0,
                  column=3,padx=3,sticky='ew')
    ttk.Label(fh,text='Height').grid(row=1,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.HEIGHT,
                 width=10).grid(row=1,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Lon_0').grid(row=2,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.LON_0,
                 width=10).grid(row=2,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Lat_0').grid(row=3,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.LAT_0,
                 width=10).grid(row=3,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Satellite height').grid(row=4,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.SATELLITE_HEIGHT,
                 width=10).grid(row=4,column=1,padx=3,sticky='w')
    ttk.Button(fh,text='Update projection',
                  command=_updated).grid(row=4,
                  column=3,padx=3,sticky='ew')
    fh.grid()

    f3 = ttk.Frame(page2,borderwidth=5,padding=5)

    ttk.Label(f3,text='Continent color').grid(row=0,column=0,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.LAND_COLOR,justify='left',width=7). \
              grid(row=0,column=1,padx=3,sticky='we')
    ttk.Button(f3,text='Select',command=lcselection).grid(row=0,column=2,padx=3,sticky='ew')

    ttk.Label(f3,text='Sea color').grid(row=1,column=0,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.WATER_COLOR,justify='left',width=7). \
             grid(row=1,column=1,padx=3,sticky='we')
    mwp = ttk.Button(f3,text='Select',command=wcselection).grid(row=1,column=2,padx=3,sticky='ew')

    ttk.Label(f3,text='Bluemarble').grid(row=2,column=0,padx=3,pady=[15,1],
                                         sticky='w')
    ttk.Checkbutton(f3,text='Show',
                       variable=self.PLOT.BLUEMARBLE,
                       command=self.make_plot).grid(row=2,
                                                    column=1,
                                                    pady=[15,1],
                                                    padx=3)
    ttk.Label(f3,
              text='Bluemarble background',
              width=25).grid(row=2,
                             column=2,
                             columnspan=3,
                             pady=[15,1],
                             padx=3)
    ttk.Label(f3,text='Etopo').grid(row=3,column=0,padx=3,sticky='w')
    ttk.Checkbutton(f3,text='Show',
                       variable=self.PLOT.ETOPO,
                       command=self.make_plot).grid(row=3,column=1,padx=3)
    ttk.Label(f3,text='Etopo background',width=25).grid(row=3,column=2,columnspan=3,padx=3)

    ttk.Label(f3,text='Shaded Relief').grid(row=4,column=0,padx=3,sticky='w')
    ttk.Checkbutton(f3,text='Show',
                       variable=self.PLOT.RELIEF,
                       command=self.make_plot).grid(row=4,column=1,padx=3)
    ttk.Label(f3,text='Land relief background',width=25).grid(row=4,column=2,columnspan=3,padx=3)
    ttk.Label(f3,text='Scale factor').grid(row=5,column=6,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.BACKGROUND_SCALE,width=7). \
             grid(row=5,column=7,padx=3,sticky='we')

    ttk.Label(f3,text='Arcgisimage').grid(row=6,column=0,padx=3,pady=[15,1],
                                         sticky='w')
    ttk.Checkbutton(f3,text='Show',
                       variable=self.PLOT.ARCGISIMAGE,
                       command=self.make_plot).grid(row=6,
                                                    column=1,
                                                    pady=[15,1],
                                                    padx=3)
    ttk.Label(f3,
              text='ArcGis Image background',
              width=25).grid(row=6,
                             column=2,
                             columnspan=3,
                             pady=[15,1],
                             padx=3)
    ttk.Label(f3,text='Service').grid(row=7,column=1,padx=3,sticky='w')
    ttk.Combobox(f3,textvariable=self.PLOT.ARCGISSERVICE,width=30, \
                 values=self.PLOT.ARCGISSERVICE_LIST).grid(row=7,column=2,columnspan=3)
    ttk.Label(f3,text='DPI').grid(row=8,column=1,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.ARCGISDPI,justify='left',width=7). \
              grid(row=8,column=2,padx=3,sticky='we')
    ttk.Label(f3,text='Pixels').grid(row=9,column=1,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.ARCGISPIXELS,justify='left',width=7). \
              grid(row=9,column=2,padx=3,sticky='we')
    ttk.Checkbutton(f3,text='Verbose',variable=self.PLOT.ARCGISVERBOSE).grid(row=10,column=1,padx=3)
    f3.grid()

    fc = ttk.Frame(page3,borderwidth=5,padding=5)
    ttk.Label(fc,text='Coastline').grid(row=0,column=0,padx=3,sticky='w')
    ttk.Checkbutton(fc,text='Show',
                    variable=self.PLOT.COASTLINE_SHOW).grid(row=0,column=1,padx=3)
    
    ttk.Label(fc,text='Coastline width').grid(row=1,column=0,padx=3,sticky='w')
    ttk.Entry(fc,textvariable=self.PLOT.COASTLINE_WIDTH,
          justify='left',width=7).grid(row=1,column=1,padx=3,sticky='we')

    ttk.Label(fc,text='Coastline color').grid(row=2,column=0,padx=3,sticky='w')
    mcc = ttk.Entry(fc,textvariable=self.PLOT.COASTLINE_COLOR,justify='left',width=7). \
                   grid(row=2,column=1,padx=3,sticky='we')
    ttk.Button(fc,text='Select',command=ccselection).grid(row=2,column=2,padx=3,sticky='ew')


    ttk.Label(fc,text='Countryline').grid(row=3,column=0,padx=3,pady=[9,1],
                                          sticky='w')
    ttk.Checkbutton(fc,text='Show',
                    variable=self.PLOT.COUNTRYLINE_SHOW).grid(row=3,column=1,padx=3)
    
    ttk.Label(fc,text='Countryline width').grid(row=4,column=0,padx=3,sticky='w')
    ttk.Entry(fc,textvariable=self.PLOT.COUNTRYLINE_WIDTH,
                 justify='left',width=7).grid(row=4,column=1,
                 padx=3,sticky='we')

    ttk.Label(fc,text='Countryline color').grid(row=5,column=0,padx=3,sticky='w')
    ttk.Entry(fc,textvariable=self.PLOT.COUNTRYLINE_COLOR,
                 justify='left',width=7).grid(row=5,column=1,padx=3,sticky='we')
    ttk.Button(fc,text='Select',command=ycselection).grid(row=5,column=2,padx=3,sticky='ew')


    ttk.Label(fc,text='Rivers').grid(row=6,column=0,padx=3,pady=[9,1],
                                          sticky='w')
    ttk.Checkbutton(fc,text='Show',
                    variable=self.PLOT.RIVERS_SHOW).grid(row=6,column=1,padx=3)
    ttk.Label(fc,text='Rivers width').grid(row=7,column=0,padx=3,sticky='w')
    ttk.Entry(fc,textvariable=self.PLOT.RIVERS_WIDTH,justify='left',width=7). \
              grid(row=7,column=1,padx=3,sticky='we')
    ttk.Label(fc,text='Rivers color').grid(row=8,column=0,padx=3,sticky='w')
    ttk.Entry(fc,textvariable=self.PLOT.RIVERS_COLOR,justify='left',width=7). \
              grid(row=8,column=1,padx=3,sticky='we')
    ttk.Button(fc,text='Select',command=rcselection).grid(row=8,column=2,padx=3,sticky='ew')
    fc.grid()



    f4 = ttk.Frame(page3,borderwidth=5,padding=5,relief='sunken')
    ttk.Label(f4,text='Isobaths (meters)', \
              font='Helvetica 12 bold').grid(row=0,column=0,\
                                             columnspan=7,padx=3,sticky='we')
    self.w = []
    for i in range(self.PLOT.nisobat):
      self.w.append(tk.Checkbutton(f4,text=str(self.PLOT.ISOBAT_Z[i]), \
                    variable=self.PLOT.ISOBAT_SELEC[i], \
                    command=select_isobaths,justify='right'))
    ii = 0
    jj = 1
    for i in range(self.PLOT.nisobat):
      self.w[i].grid(row=jj,column=ii,sticky='w')
      ii += 1
      if ii > 6:
        ii = 0
        jj += 1

    #ttk.Label(f4,text='Width',justify='right').grid(row=4,column=0,padx=3,sticky='e')
    #ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_WIDTH, \
    #                justify='left',width=7)  
    #ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_WIDTH, \
    #          justify='left',width=7).grid(row=4,column=1,\
    #          padx=3,sticky='we')
    #ttk.Label(f4,text='Color').grid(row=4,column=2,padx=3,sticky='e')
    #ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_COLOR,justify='left', \
    #          width=7).grid(row=4,column=3,padx=3,sticky='we')
    #ttk.Button(f4,text='Select',command=icselection). \
    #    grid(row=4,column=4,padx=3,sticky='ew')
    ttk.Label(f4,text='Path',justify='right').grid(row=5,column=0)
    ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_PATH, \
            justify='left',width=50).grid(row=5,column=1,columnspan=5,padx=3,pady=5)
    ttk.Button(f4,text='Select',command=_pselect).grid(row=5,column=6)

    wwr = ttk.Label(f4,width=26,justify='left')
    wwr.grid(row=6,column=0,columnspan=3,sticky='w',padx=5)
    if self.PLOT.ISOBAT_selected:
      if self.PLOT.ISOBAT_loaded:
        wwr.configure(font=font_norm)
        wwr.configure(foreground='#125704')
        wwr['text'] = 'Isobaths have been loaded'
      else:
        wwr.configure(font=font_bold)
        wwr.configure(foreground='red')
        wwr['text'] = 'Isobaths need to be loaded'
    else:
      wwr['text'] = 'No isobaths have been selected'
      wwr.configure(font=font_norm)
      wwr.configure(foreground='black')

    wli = ttk.Button(f4,text='Load isobaths',command=iload)
    wli.grid(row=6,column=3,columnspan=2,padx=3,sticky='ew')
    wlr = ttk.Button(f4,text='Crop isobaths',command=self.isobath_crop)
    wlr.grid(row=6,column=5,columnspan=2,padx=3,sticky='ew')
    if self.PLOT.ISOBAT_selected:
      wli.configure(state='enabled')
    else:
      wli.configure(state='disabled')
    if self.PLOT.ISOBAT_loaded:
      wlr.configure(state='enabled')
    else:
      wlr.configure(state='disabled')
    f4.grid()

    # ....................
    def update_name():
      ii = self.PLOT.ISOBAT_LABEL.index(self.PLOT.ISOBAT_ZPOINTER.get())
      wly['textvariable'] = self.PLOT.ISOBAT_STYLE[ii]
      wlw['textvariable'] = self.PLOT.ISOBAT_WIDTH[ii]
      wlc['textvariable'] = self.PLOT.ISOBAT_COLOR[ii]
    # ....................

    # Select the style, width and color of isobaths
    f4b = ttk.Frame(page3,borderwidth=5,padding=5)
    ii = self.PLOT.ISOBAT_LABEL.index(self.PLOT.ISOBAT_ZPOINTER.get())
    wln = ttk.Combobox(f4b,
                       width=10,
                       textvariable=self.PLOT.ISOBAT_ZPOINTER,
                       values=self.PLOT.ISOBAT_LABEL)
    wln.grid(row=1,column=0)
    wln.bind('<<ComboboxSelected>>',lambda e: update_name())

    ttk.Label(f4b,text='Line style').grid(row=1,column=1,padx=3)
    wly = ttk.Combobox(f4b,
                       textvariable=self.PLOT.ISOBAT_STYLE[ii],
                       width=4,
                       values=['-',':','--','-.',' '])
    wly.grid(row=1,column=2)

    ttk.Label(f4b,text='Line width').grid(row=1,column=3,padx=3)
    wlw = ttk.Entry(f4b,
                    textvariable=self.PLOT.ISOBAT_WIDTH[ii],
                    width=4)
    wlw.grid(row=1,column=4)

    ttk.Label(f4b,text='Line color').grid(row=1,column=5,padx=3)
    wlc = ttk.Entry(f4b,
                    textvariable=self.PLOT.ISOBAT_COLOR[ii],
                    width=10)
    wlc.grid(row=1,column=6)

    ttk.Button(f4b,text='Select',command=icselection).grid(row=1,column=7)

    wls = ttk.Checkbutton(f4b,
                          variable=self.PLOT.ISOBAT_LABEL_SHOW)
    wls.grid(row=2,
             column=6,
             sticky='e')
    ttk.Label(f4b,text='Label isobaths').grid(row=2,
                                              column=7,
                                              sticky='w')
    # ....................
    def cgrad():
      R0 = CM.Blues(80)
      R1 = CM.Blues(255)
      N = self.PLOT.nisobat
      Ra = [(R1[0]-R0[0])/(N-1),(R1[1]-R0[1])/(N-1),(R1[2]-R0[2])/(N-1),1]
      for i in range(N):
        self.PLOT.ISOBAT_COLOR[i].set([R0[0]+Ra[0]*i,
                                     R0[1]+Ra[1]*i,
                                     R0[2]+Ra[2]*i,
                                     1])
    # ....................
    ttk.Button(f4b,
               text='Color grad',command=cgrad).grid(row=2,column=5,padx=3)
    f4b.grid()

    f5 = ttk.Frame(page4,padding=5)
    ttk.Label(f5,text='Show grid').grid(row=0,column=1,padx=3,sticky='e')
    ttk.Checkbutton(f5,variable=self.PLOT.GRID_SHOW,command=self.make_plot) \
       .grid(row=0,column=2,padx=3,sticky='w')
    ttk.Label(f5,text='Meridians',font='Helvetica 12 bold').grid(row=1,column=0,sticky='w')
    ttk.Label(f5,text='Initial').grid(row=2,column=1,sticky='w')
    wxo = ttk.Entry(f5,textvariable=self.PLOT.MERIDIAN_INI,justify='left',width=8)
    wxo.grid(row=2,column=2)
    ttk.Label(f5,text='Final').grid(row=3,column=1,sticky='w')
    wdx = ttk.Entry(f5,textvariable=self.PLOT.MERIDIAN_FIN,justify='left',width=8)
    wdx.grid(row=3,column=2)
    ttk.Label(f5,text='Interval').grid(row=4,column=1,sticky='w')
    wdx = ttk.Entry(f5,textvariable=self.PLOT.MERIDIAN_INT,justify='left',width=8)
    wdx.grid(row=4,column=2)
    ttk.Checkbutton(f5,text='North',
                       variable=self.PLOT.GRID_NORTH).grid(row=2,
                                                           column=3,padx=6)
    ttk.Checkbutton(f5,text='South',
                       variable=self.PLOT.GRID_SOUTH).grid(row=3,
                                                           column=3,padx=6)

    ttk.Label(f5,text='Parallels',font='Helvetica 12 bold').grid(row=5,column=0,sticky='w')
    ttk.Label(f5,text='Initial').grid(row=6,column=1,sticky='w')
    wxo = ttk.Entry(f5,textvariable=self.PLOT.PARALLEL_INI,justify='left',width=8)
    wxo.grid(row=6,column=2)
    ttk.Label(f5,text='Final').grid(row=7,column=1,sticky='w')
    wdx = ttk.Entry(f5,textvariable=self.PLOT.PARALLEL_FIN,justify='left',width=8)
    wdx.grid(row=7,column=2)
    ttk.Label(f5,text='Interval').grid(row=8,column=1,sticky='w')
    wdx = ttk.Entry(f5,textvariable=self.PLOT.PARALLEL_INT,justify='left',width=8)
    wdx.grid(row=8,column=2)
    ttk.Checkbutton(f5,text='West',
                       variable=self.PLOT.GRID_WEST).grid(row=6,
                                                           column=3,padx=6)
    ttk.Checkbutton(f5,text='East',
                       variable=self.PLOT.GRID_EAST).grid(row=7,
                                                           column=3,padx=6)
    ttk.Label(f5,text='Configuration',font='Helvetica 12 bold') \
       .grid(row=10,column=0,sticky='w')
    ttk.Label(f5,text='Character Size').grid(row=11,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_SIZE,justify='left',width=8) \
       .grid(row=11,column=2)
    ttk.Label(f5,text='Line Color').grid(row=12,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_COLOR,justify='left',width=8) \
       .grid(row=12,column=2)
    ttk.Label(f5,text='Line Width').grid(row=13,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_LINEWIDTH,justify='left',width=8) \
       .grid(row=13,column=2)
    ttk.Label(f5,text='Line Style').grid(row=14,column=1,sticky='w')
    ttk.Combobox(f5,textvariable=self.PLOT.GRID_LINESTYLE,
                    justify='left',
                    values=['',' ','None','--','-.','-',':'],width=8) \
       .grid(row=14,column=2)
    ttk.Label(f5,text='Line alpha').grid(row=15,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_ALPHA,justify='left',width=8) \
       .grid(row=15,column=2)
    ttk.Label(f5,text='Font color').grid(row=16,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_FONTCOLOR,justify='left',width=8) \
       .grid(row=16,column=2)
    f5.grid()


    f6 = ttk.Frame(page5,borderwidth=5,padding=5)
    ttk.Label(f6,text='Title').grid(row=1,column=0,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TITLE,width=40). \
        grid(row=1,column=1,columnspan=4,sticky='w')
    def titleprop0():
      self.PLOT.TITLEFONT = fontconfig(font=self.PLOT.TITLEFONT,
                                  sample=self.PLOT.TITLE.get())
    #ttk.Label(f6,text='Title font').grid(row=2,
    #                                     column=0,
    #                                     columnspan=1,
    #                                     sticky='w')
    ttk.Button(f6,text='Set font',command=titleprop0).grid(row=1,
                                                       column=5,
                                                       padx=5,
                                                       sticky='ew')
    #ttk.Checkbutton(f6,text='Bold',variable=self.PLOT.TITLE_BOLD). \
    #    grid(row=1,column=5)
    #ttk.Label(f6,text='Size').grid(row=2,column=0,columnspan=1,sticky='w')
    #ttk.Entry(f6,textvariable=self.PLOT.TITLE_SIZE,width=7). \
    #    grid(row=2,column=1,sticky='w')
    ttk.Label(f6,text='Title Pad').grid(row=2,column=0,columnspan=1,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TITLE_PAD,width=7). \
        grid(row=2,column=1,sticky='w')

    ttk.Label(f6,text='X label').grid(row=4,column=0,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.XLABEL,width=40). \
        grid(row=4,column=1,columnspan=4,sticky='w')
    ttk.Label(f6,text='Y label').grid(row=5,column=0,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.YLABEL,width=40). \
        grid(row=5,column=1,columnspan=4,sticky='w')
    ttk.Label(f6,text='Size').grid(row=6,column=0,columnspan=1,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.LABEL_SIZE,width=5). \
        grid(row=6,column=1,columnspan=1,sticky='w')
    ttk.Label(f6,text='Label Pad'). \
        grid(row=7,column=0,columnspan=1,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.LABEL_PAD,width=5). \
        grid(row=7,column=1,columnspan=1,sticky='w')
    #ttk.Label(f6,text='Plot logo'). \
    #    grid(row=8,column=0,sticky='w')
    #ttk.Checkbutton(f6,variable=self.PLOT.LOGO_DISPLAY). \
    #    grid(row=8,column=1,sticky='w',padx=3)
    ttk.Label(f6,text='Timestamp'). \
        grid(row=9,column=0,sticky='w',pady=[15,1])

    ttk.Checkbutton(f6,text='Show',variable=self.PLOT.TIMESTAMP_SHOW). \
        grid(row=10,column=1,sticky='w')
    ttk.Checkbutton(f6,text='Bold',variable=self.PLOT.TIMESTAMP_BOLD). \
        grid(row=11,column=1,sticky='w')

    def getlabelpos():
    # ================
      self.GET_TIMESTAMP_LOCATION = True
      
    ttk.Label(f6,text='X pos'). \
        grid(row=12,column=0,columnspan=1,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TIMESTAMP_X,width=12). \
        grid(row=12,column=1,columnspan=1,sticky='w')
    ttk.Button(f6,text='Select',command=getlabelpos).grid(row=12,column=2)

    ttk.Label(f6,text='Y pos'). \
        grid(row=13,column=0,columnspan=1,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TIMESTAMP_Y,width=12). \
        grid(row=13,column=1,columnspan=1,sticky='w')
    ttk.Label(f6,text='Size'). \
        grid(row=14,column=0,columnspan=1,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TIMESTAMP_SIZE,width=5). \
        grid(row=14,column=1,columnspan=1,sticky='w')
    ttk.Label(f6,text='Color'). \
        grid(row=15,column=0,columnspan=1,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TIMESTAMP_COLOR,width=20). \
        grid(row=15,column=1,columnspan=2,sticky='w')

    f6.grid()


    # ---------------------------------------------
    def center():
      SOUTH = float(self.PLOT.SOUTH.get())
      NORTH = float(self.PLOT.NORTH.get())
      WEST  = float(self.PLOT.WEST.get())
      EAST  = float(self.PLOT.EAST.get())
      self.PLOT.SCALE_XO.set(0.5*(WEST+EAST))
      self.PLOT.SCALE_YO.set(0.5*(SOUTH+NORTH))
    # ---------------------------------------------

    fs = ttk.Frame(page7,borderwidth=5,padding=5)
    ttk.Label(fs,text='Show').grid(row=0,column=0,padx=3)
    ttk.Checkbutton(fs,variable=self.PLOT.SCALE_SHOW).grid(row=0,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='LON = ').grid(row=1,column=0,padx=3,sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_X,
                 width=10).grid(row=1,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='LAT = ').grid(row=2,column=0,padx=3,sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_Y,
                 width=10).grid(row=2,column=1,padx=3,sticky='w')
    ttk.Label(fs,
              text='Map position where Scale will be drawn').grid(row=1,
              column=2,rowspan=2,columnspan=2,padx=3,pady=5)

    ttk.Label(fs,text='LON0 = ').grid(row=3,column=0,padx=3,sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_XO,
                 width=10).grid(row=3,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='LAT0 = ').grid(row=4,column=0,padx=3,sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_YO,
                 width=10).grid(row=4,column=1,padx=3,sticky='w')
    ttk.Label(fs,
              text='Coordinate to which the Scale distance apply').grid(row=3,
              column=2,rowspan=2,columnspan=2,padx=3,pady=5)
    ttk.Button(fs,text='Map center',command=center).grid(row=3,column=4,
                                                         rowspan=2,padx=3)
    ttk.Label(fs,text='Length = ').grid(row=5,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_LENGTH,
                 width=10).grid(row=5,column=1,padx=3,sticky='w')
    
    ttk.Label(fs,text='Units = ').grid(row=6,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Combobox(fs,textvariable=self.PLOT.SCALE_UNITS,
                 values=['km','mi','nmi','ft','m'],
                 width=10).grid(row=6,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Bar style = ').grid(row=7,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Combobox(fs,textvariable=self.PLOT.SCALE_STYLE,
                 values=['simple','fancy'],
                 width=10).grid(row=7,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Yoffset = ').grid(row=8,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_YOFFSET,
                 width=10).grid(row=8,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Default: 0.02*(MAXLAT-MINLAT)').grid(row=8,
                                 column=2,columnspan=2,padx=3,sticky='w')
    ttk.Label(fs,text='Label style = ').grid(row=9,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Combobox(fs,textvariable=self.PLOT.SCALE_LABELSTYLE,
                 values=['simple','fancy'],
                 width=10).grid(row=9,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Font size = ').grid(row=10,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_FONTSIZE,
                 width=10).grid(row=10,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Font color = ').grid(row=11,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_FONTCOLOR,
                 width=10).grid(row=11,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Format = ').grid(row=12,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_FORMAT,
                 width=10).grid(row=12,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Line width = ').grid(row=13,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_LINEWIDTH,
                 width=10).grid(row=13,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Line color = ').grid(row=14,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_LINECOLOR,
                 width=10).grid(row=14,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Fill color 1 = ').grid(row=15,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_FILLCOLOR1,
                 width=10).grid(row=15,column=1,padx=3,sticky='w')
    ttk.Label(fs,text='Fill color 2 = ').grid(row=16,column=0,padx=3,
                                        pady=[5,1],sticky='e')
    ttk.Entry(fs,textvariable=self.PLOT.SCALE_FILLCOLOR2,
                 width=10).grid(row=16,column=1,padx=3,sticky='w')
    
    fs.grid()

    f8 = ttk.Frame(page8,borderwidth=5,padding=5)
    ttk.Label(f8,text='Map dots per inch (DPI): ').grid(row=0,column=0,sticky='w')
    ttk.Entry(f8,textvariable=self.PLOT.DPI,width=10).grid(row=0,column=1,sticky='w')
    ttk.Label(f8,text='Map window size: ').grid(row=1,column=0,sticky='w')
    size = ttk.Entry(f8,textvariable=PSIZE,width=30)
    size.grid(row=1,column=1,columnspan=3,sticky='w')
    size.bind("<Return>",lambda f: sizeupdate())
    ttk.Label(f8,text='(It will close current map) ').grid(row=1,column=4,sticky='w')
    ttk.Label(f8,text='Font style').grid(row=2,column=0,sticky='w')
    ttk.Combobox(f8,textvariable=self.PLOT.MAP_FONT_TYPE, \
                 values=self.FONT_TYPES,width=30).grid(row=2,column=1, \
                                                       columnspan=3,   \
                                                       padx=3,sticky='w')
    ttk.Label(f8,text='Background color').grid(row=3,column=0,sticky='w')
    ttk.Entry(f8,textvariable=self.PLOT.FIGURE_COLOR,
             width=30).grid(row=3,column=1,columnspan=3,sticky='w')
    ttk.Label(f8,text='Text color').grid(row=4,column=0,sticky='w')
    ttk.Entry(f8,textvariable=self.PLOT.TEXT_COLOR,
             width=30).grid(row=4,column=1,columnspan=3,sticky='w')
    f8.grid()

    maptabs.grid()

    frame5 = ttk.Frame(self.Window_mapconfig,borderwidth=5,padding=5)
    ttk.Button(frame5,text='Cancel',command=_cancel).grid(row=0,column=4,padx=3)
    ttk.Button(frame5,text='Apply',command=_apply).grid(row=0,column=5,padx=3)
    ttk.Button(frame5,text='Close',command=_done).grid(row=0,column=6,padx=3)
    frame5.grid(row=24,column=0,columnspan=5)


  # ====================
  def logo_config(self):
  # ====================

    def _close():
    # ===========
      self.Window_logo.destroy()
      self.Window_logo = None
      self.make_plot()
   
    def new_logo():
    # =============
      nn = tk.filedialog.askopenfile()
      if not empty(nn.name):
        self.PLOT.LOGO_FILE.set(nn.name)
        self.PLOT.LOGO_IMAGE = image.imread(self.PLOT.LOGO_FILE.get())
      self.make_plot()

    def _loadconf():
    # =============
      '''Load map configuration'''
      cfilename = COSMO_CONF + 'drawing.conf'
      try:
        # Read configuration
        with open(cfilename) as infile:
          conf = json.load(infile)
        self.PLOT.LOGO_FILE.set(conf['LOGO_FILE'])
        self.PLOT.LOGO_ZOOM.set(conf['LOGO_ZOOM'])
        self.PLOT.LOGO_LOCATION.set(conf['LOGO_LOCATION'])
        self.PLOT.LOGO_X.set(conf['LOGO_X'])
        self.PLOT.LOGO_Y.set(conf['LOGO_Y'])
        self.PLOT.LOGO_IMAGE = image.imread(self.PLOT.LOGO_FILE.get())
      except:
        print('Cannot read default configuration file ',cfilename)
      self.make_plot()

    def _saveconf():
    # =============
      '''Save map configuration'''
      cfilename = COSMO_CONF + 'drawing.conf'
      try:
        # Read configuration
        with open(cfilename) as infile:
          conf = json.load(infile)
        conf['LOGO_FILE'] = self.PLOT.LOGO_FILE.get()
        conf['LOGO_ZOOM'] = self.PLOT.LOGO_ZOOM.get()
        conf['LOGO_LOCATION'] = self.PLOT.LOGO_LOCATION.get()
        conf['LOGO_X'] = self.PLOT.LOGO_X.get()
        conf['LOGO_Y'] = self.PLOT.LOGO_Y.get()
        # Write JSON file:
        with io.open(cfilename,'w',encoding='utf8') as outfile:
          str_ = json.dumps(conf,ensure_ascii=False, \
                                   sort_keys=True,     \
                                   indent=2,           \
                                   separators=(',',': '))
          outfile.write(to_unicode(str_))
        print('New default values saved in file ',cfilename)
      except:
        print('Cannot open default configuration file ',cfilename)

    # Main Window
    # ============
    if self.Window_logo is None:
      self.Window_logo = tk.Toplevel(self.master)
      self.Window_logo.title("Logo configuration")
      self.Window_logo.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_logo.lift()
      return

    menubar = tk.Menu(self.Window_logo)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Default configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Save',command=_saveconf)
    try:
      self.Window_logo.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.Window_logo.tk.call(master, "config", "-menu", menubar)


    F0 = ttk.Frame(self.Window_logo,borderwidth=5,padding=5)

    ttk.Label(F0,text='Plot logo'). \
        grid(row=0,column=1,sticky='w')
    ttk.Checkbutton(F0,variable=self.PLOT.LOGO_DISPLAY). \
        grid(row=0,column=2,sticky='w',padx=3)

    ttk.Label(F0,text='File', \
              font='Helvetica 12 bold').grid(row=1,column=0,sticky='w')
    le = ttk.Entry(F0,textvariable=self.PLOT.LOGO_FILE, \
                   justify='left',width=30)
    le.grid(row=1,column=1,columnspan=5,sticky='w')
    le.bind('<<ComboboxSelected>>',lambda e: new_logo())
    ttk.Button(F0,text='Open', \
               command=new_logo).grid(row=1,column=6,sticky='w')
    ttk.Label(F0,text='Zoom', \
               font='Helvetica 12 bold').grid(row=2,column=0,sticky='w')
    ttk.Entry(F0,textvariable=self.PLOT.LOGO_ZOOM, \
               justify='left',width=8).grid(row=2,column=1,sticky='w')
    ttk.Label(F0,text='Location', \
               font='Helvetica 12 bold').grid(row=3,column=0,sticky='w')
    ttk.Radiobutton(F0,text='SW',variable=self.PLOT.LOGO_LOCATION,\
                    value='SW').grid(row=4,column=1,sticky='w')
    ttk.Radiobutton(F0,text='NW',variable=self.PLOT.LOGO_LOCATION,\
                    value='NW').grid(row=5,column=1,sticky='w')
    ttk.Radiobutton(F0,text='NE',variable=self.PLOT.LOGO_LOCATION,\
                    value='NE').grid(row=6,column=1,sticky='w')
    ttk.Radiobutton(F0,text='SE',variable=self.PLOT.LOGO_LOCATION,\
                    value='SE').grid(row=7,column=1,sticky='w')
    ttk.Radiobutton(F0,text='Other',variable=self.PLOT.LOGO_LOCATION,\
                    value='OTHER').grid(row=8,column=1,sticky='w')
    lx = ttk.Entry(F0,textvariable=self.PLOT.LOGO_X,\
                   justify='left',width=7)
    lx.grid(row=8,column=2,sticky='w')
    ly = ttk.Entry(F0,textvariable=self.PLOT.LOGO_Y,\
                   justify='left',width=7)
    ly.grid(row=8,column=3,sticky='w')

    ttk.Button(F0,text='Apply',command=_close,padding=5).grid(row=9,column=6)
    F0.grid()


  # ==================
  def plot_logo(self):
  # ==================
    '''Add a logo in the plot'''

    im = OffsetImage(self.PLOT.LOGO_IMAGE,zoom=self.PLOT.LOGO_ZOOM.get())

    if self.PLOT.LOGO_LOCATION.get() == 'SW':
      xx = self.PLOT.WEST.get()
      yy = self.PLOT.SOUTH.get()
      ba = (0,0)
    elif self.PLOT.LOGO_LOCATION.get() == 'NW':
      xx = self.PLOT.WEST.get()
      yy = self.PLOT.NORTH.get()
      ba = (0,1)
    elif self.PLOT.LOGO_LOCATION.get() == 'NE':
      xx = self.PLOT.EAST.get()
      yy = self.PLOT.NORTH.get()
      ba = (1,1)
    elif self.PLOT.LOGO_LOCATION.get() == 'SE':
      xx = self.PLOT.EAST.get()
      yy = self.PLOT.SOUTH.get()
      ba = (1,0)
    else:
      xx = self.PLOT.LOGO_X.get()
      yy = self.PLOT.LOGO_Y.get()
      ba = (0,0)

    self.ab = AnnotationBbox(im,[xx,yy], xycoords='data', \
                             box_alignment=ba,pad=0.0,frameon=True)
    if self.PLOT.GEOMAP.get():
      self.with_logo = self.m._check_ax().add_artist(self.ab)
    else:
      self.with_logo = self.ax.add_artist(self.ab)


  # =====================
  def mlm(self):
  # =====================
    '''Options to launch the COSMO M - Lagrangian Model'''

    def _close():
    # ===========
      self.Window_mlm.destroy()
      self.Window_mlm = None

    def _run(options):
    # ================

      command = self.MLM.PATH.get() + \
                self.MLM.BIN.get()

      command += options
      print(command)
      os.system(command)

      if os.path.isfile(self.MLM.TRAJECTORY.get()):
        FLT = lagrangian.parameters()
        FLT.Read(self.MLM.TRAJECTORY.get())
        if FLT is None:
          return

        FLT.TIME = []
        FLT.MAPX = []
        FLT.MAPY = []
        for j in range(FLT.nfloats):
          FTIME = np.array([(FLT.date[i][j].replace(tzinfo=None)-self.DATE[0]).total_seconds() for i in range(FLT.nrecords)])
          f = interpolate.interp1d(FTIME,np.array(FLT.lon[:,j]), \
                                   bounds_error=False,  \
                                   fill_value=np.NaN)
          FLT.MAPX.append(list(f(self.TIME)))
          f = interpolate.interp1d(FTIME,np.array(FLT.lat[:,j]), \
                                   bounds_error=False,  \
                                   fill_value=np.NaN)
          FLT.MAPY.append(list(f(self.TIME)))

          FLT.TIME.append(FTIME)
        FLT.MAPX = np.array(FLT.MAPX).T.tolist() 
        FLT.MAPY = np.array(FLT.MAPY).T.tolist() 
        FLT.TIME = np.array(FLT.TIME).T.tolist() 

        self.nfloat += 1
        self.FLOAT.append(FLT)
        self.FLOAT_INDX.set(self.nfloat-1)
        self.FLOAT_LIST = list(range(self.nfloat))

        self.nfiles += 1
        self.FILENAMES.append(FLT.FILENAME.get())
        self.FILETYPES.append('FLOAT')
        self.SEQUENCES.append(tk.BooleanVar(value=False))
        self.FILEORDER.append(self.nfloat-1)

        self.make_plot()
      else:
        messagebox.showinfo(message='COSMO M Lagrangian Model failed')


    def _help():
    # ==========
      options = '--help'
      _run(options)

    def _run_single():
    # ================

      options = mlm.Basic_options(self.MLM)
      if empty(options):
        return

      if self.MLM.INI_USE.get():
        pass
      else:
        try:
          aa = ' -t0 %s' % self.MLM.to.get()
          options += aa
        except:
          messagebox.showinfo(message='Invalid release time')
          return

      if self.MLM.reverse.get():
        try:
          aa = ' -time_sim %s' % -np.abs(self.MLM.time_sim.get())
          options += aa
        except:
          aa = ' -time_sim %s' % -self.MLM.to.get()/86400.0
          options += aa
      else:
        try:
          aa = ' -time_sim %s' % self.MLM.time_sim.get()
          options += aa
        except:
          pass

      try:
        aa = ' -idt %s' % self.MLM.idt.get()
        options += aa
      except:
        pass


      _run(options)

    def _run_ensemble():
    # ==================

      options = mlm.Basic_options(self.MLM)

      if self.MLM.reverse.get():
        try:
          aa = ' -time_sim %s' % -math.abs(self.MLM.time_sim.get())
          options += aa
        except:
          aa = ' -time_sim %s' % -math.abs(self.MLM.to.get())
          options += aa
      else:
        try:
          aa = ' -time_sim %s' % self.MLM.time_sim.get()
          options += aa
        except:
          pass

      if self.MLM.INI_USE.get():
        pass
      else:
        try:
          aa = ' -to %s' % self.MLM.to.get()
          options += aa
        except:
          messagebox.showinfo(message='Invalid release time')
          return

      try:
        aa = ' -seed %s' % self.MLM.seed.get()
        options += aa
      except:
        pass

      try:
        aa = ' -nfloats %s' % self.MLM.nfloats.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Rx %s' % self.MLM.Rx.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Ry %s' % self.MLM.Ry.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Rt %s' % self.MLM.Rt.get()
        options += aa
      except:
        pass

      _run(options)

    # Main MLM Window
    if self.nvec == 0:
      messagebox.showinfo(message='No file with ocean currents has ben opened yet')
      return

    if self.Window_mlm is not None:
      self.Window_mlm.lift()
      return

    # Copy the VEC information to the MLM class
    #
    self.MLM.VEC = self.VEC

    string = self.DATE[self.L.get()]
    try:
      self.MLM.do.set(string.replace(' ','T'))
    except:
      pass
    self.MLM.to.set(self.TIME[self.L.get()])
    self.MLM.record.set(self.L.get()+1)

    self.Window_mlm = tk.Toplevel(self.master)
    self.Window_mlm.title('COSMO M Lagrangian Model options')
    self.Window_mlm.protocol('WM_DELETE_WINDOW',_close)

    mlm.WinConfig(self.Window_mlm,self.MLM)

    F0 = ttk.Frame(self.Window_mlm,padding=5)
    ttk.Checkbutton(F0,text='Reverse Run',variable=self.MLM.reverse). \
        grid(row=0,column=1,padx=5)
    ttk.Button(F0,text='Run Single',command=_run_single).grid(row=0,column=2,padx=5)
    ttk.Button(F0,text='Run Ensemble',command=_run_ensemble).grid(row=0,column=3,padx=5)
    ttk.Button(F0,text='Run Help',command=_help).grid(row=0,column=4,padx=5)
    F0.grid()








  # =====================
  def blm(self):
  # =====================
    '''Options to launch the COSMO B - Lagrangian Model'''

    def _close():
    # ===========
      self.Window_blm.destroy()
      self.Window_blm = None

    def _run(options):
    # ================

      command = self.BLM.PATH.get() + \
                self.BLM.BIN.get()

      command += options
      print(command)
      os.system(command)

      if os.path.isfile(self.BLM.TRAJECTORY.get()):
        FLT = lagrangian.parameters()
        FLT.Read(self.BLM.TRAJECTORY.get())
        if FLT is None:
          return

        FLT.TIME = np.array([(FLT.date[i].replace(tzinfo=None)-\
                              self.DATE[0]).total_seconds() \
                              for i in range(FLT.nrecords)])
        FLT.MAPX = []
        FLT.MAPY = []
        if FLT.nfloats > 1:
          for i in range(FLT.nfloats):
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon[:,i]), bounds_error=False, fill_value=np.NaN)
            FLT.MAPX.append(list(f(self.TIME)))
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat[:,i]), bounds_error=False, fill_value=np.NaN)
            FLT.MAPY.append(list(f(self.TIME)))
          FLT.MAPX = np.array(FLT.MAPX).T.tolist() 
          FLT.MAPY = np.array(FLT.MAPY).T.tolist() 
        else:
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon), bounds_error=False, fill_value=np.NaN)
          FLT.MAPX = list(f(self.TIME))
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat), bounds_error=False, fill_value=np.NaN)
          FLT.MAPY = list(f(self.TIME))

        self.nfloat += 1
        self.FLOAT.append(FLT)
        self.FLOAT_INDX.set(self.nfloat-1)
        self.FLOAT_LIST = list(range(self.nfloat))

        self.nfiles += 1
        self.FILENAMES.append(FLT.FILENAME.get())
        self.FILETYPES.append('FLOAT')
        self.SEQUENCES.append(tk.BooleanVar(value=False))
        self.FILEORDER.append(self.nfloat-1)

        self.make_plot()
      else:
        messagebox.showinfo(message='COSMO B Lagrangian Model failed')


    def _help():
    # ==========
      options = '--help'
      _run(options)
  
    def _run_single():
    # ================

      options = blm.Basic_options(self.BLM)
      if empty(options):
        return

      if self.BLM.INI_USE.get():
        pass
      else:
        if self.BLM.reverse.get():
          try:
            aa = ' -to %s' % 0.0
            options += aa
          except:
            pass
        else:
          if self.BLM.to_use.get():
            try:
              aa = ' -to %s' % self.BLM.to.get()
              options += aa
            except:
              messagebox.showinfo(message='Invalid release time')
              return
          else:
            if empty(self.BLM.do.get()):
              messagebox.showinfo(message='Invalid release date')
              return
            else:
              aa = ' -do %s' % self.BLM.do.get()
              options += aa

      if self.BLM.record_use.get():
        try:
          aa = ' -record %s' % self.BLM.record.get()
          options += aa
        except:
          messagebox.showinfo(message='Invalid simulation starting record')
          return
      
      if self.BLM.reverse.get():
        options += ' -reverse'

      _run(options)

    def _run_ensemble():
    # ==================

      options = blm.Basic_options(self.BLM)

      if self.BLM.reverse.get():

        options += ' -reverse'

        if self.BLM.INI_USE.get():
          pass
        else:
          try:
            aa = ' -to %s' % 0.0
            options += aa
          except:
            pass

      else:

        if self.BLM.INI_USE.get():
          pass
        else:
          if self.BLM.to_use.get():
            try:
              aa = ' -to %s' % self.BLM.to.get()
              options += aa
            except:
              messagebox.showinfo('Invalid release time')
              return
          else:
             if empty(self.BLM.do.get()):
               messagebox.showinfo('Invalid release date')
               return
             else:
               aa = ' -do %s' % self.BLM.do.get()
               options += aa


      if self.BLM.record_use.get():
        try:
          aa = ' -record %s' % self.BLM.record.get()
          options += aa
        except:
          messagebox.showinfo('Invalid simulation starting record')
          return
      
      try:
        aa = ' -seed %s' % self.BLM.seed.get()
        options += aa
      except:
        pass

      try:
        aa = ' -nfloats %s' % self.BLM.nfloats.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Rx %s' % self.BLM.Rx.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Ry %s' % self.BLM.Ry.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Rt %s' % self.BLM.Rt.get()
        options += aa
      except:
        pass


      _run(options)


  
    # Main BLM Window
    if self.nvec == 0:
      messagebox.showinfo(message='No file with ocean currents has ben opened yet')
      return

    if self.Window_blm is not None:
      self.Window_blm.lift()
      return

    # Copy the VEC information to the BLM class
    #
    self.BLM.VEC = self.VEC

    string = self.DATE[self.L.get()]
    try:
      self.BLM.do.set(string.replace(' ','T'))
    except:
      pass
    self.BLM.to.set(self.TIME[self.L.get()])
    self.BLM.record.set(self.L.get()+1)

    self.Window_blm = tk.Toplevel(self.master)
    self.Window_blm.title('COSMO B Lagrangian Model options')
    self.Window_blm.resizable(width=True,height=True)
    self.Window_blm.protocol('WM_DELETE_WINDOW',_close)

    blm.WinConfig(self.Window_blm,self.BLM)

    F0 = ttk.Frame(self.Window_blm,padding=5)
    ttk.Checkbutton(F0,text='Reverse Run',variable=self.BLM.reverse). \
        grid(row=0,column=1,padx=5)
    ttk.Button(F0,text='Run Single',command=_run_single).grid(row=0,column=2,padx=5)
    ttk.Button(F0,text='Run Ensemble',command=_run_ensemble).grid(row=0,column=3,padx=5)
    ttk.Button(F0,text='Run Help',command=_help).grid(row=0,column=4,padx=5)
    F0.grid()


  # ==================
  def make_anim(self):
  # ==================
    ''' Launch the matplotlib animation'''

    # -----------
    def _close():
    # -----------
      self.Window_anim.destroy()
      self.Window_anim = None

    def _done():
    # ----------

      L_Backup = self.L.get()
      FFMpegWriter = manimation.writers['ffmpeg']
      metadata = dict(title=self.PLOT.VIDEO_TITLE.get(), 
                      artist=self.PLOT.VIDEO_AUTHOR.get(),
                      comment=self.PLOT.VIDEO_COMMENT.get())
      writer = FFMpegWriter(fps=self.PLOT.VIDEO_FPS.get(),metadata=metadata)

      with writer.saving(self.Mfig,self.PLOT.VIDEO_NAME.get(),self.PLOT.VIDEO_DPI.get()):
        for L in range(self.PLOT.VIDEO_L1.get(),self.PLOT.VIDEO_L2.get()+1):
          self.L.set(L)
          self.PLOT.TLABEL.set(self.DATE[L])
          for i in range(self.nfiles):
            if self.SEQUENCES[i].get():
              if self.FILETYPES[i] == 'VEC':
                self.VEC[self.FILEORDER[i]].L.set(L)
                self.read_UV(self.VEC[self.FILEORDER[i]])
              elif self.FILETYPES[i] == 'FLD':
                self.CDF[self.FILEORDER[i]].L.set(L)
                self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
              else:
                print('Somethin wrong')
                quit()
          self.make_Mplot()
          writer.grab_frame()
      messagebox.showinfo(parent=self.Window_anim,message='Movie has been saved')
      self.L.set(L_Backup)

    def _loadconf():
    # -------------
      '''Load ANIM configuration'''
      print('Retrieving VIDEO defaults.')
      with open(self.PLOT.FILECONF) as infile:
        conf = json.load(infile)
      self.PLOT.VIDEO_NAME.set(conf['VIDEO_NAME'])
      self.PLOT.VIDEO_TITLE.set(conf['VIDEO_TITLE'])
      self.PLOT.VIDEO_AUTHOR.set(conf['VIDEO_AUTHOR'])
      self.PLOT.VIDEO_COMMENT.set(conf['VIDEO_COMMENT'])
      self.PLOT.VIDEO_FPS.set(conf['VIDEO_FPS'])
      self.PLOT.VIDEO_DPI.set(conf['VIDEO_DPI'])

    def _saveconf():
    # -------------
      '''Save ANIM configuration'''
      with open(self.PLOT.FILECONF) as infile:
        conf = json.load(infile)
      print('Updating VIDEO defaults.')
      conf['VIDEO_NAME'] = self.PLOT.VIDEO_NAME.get()
      conf['VIDEO_TITLE'] = self.PLOT.VIDEO_TITLE.get()
      conf['VIDEO_AUTHOR'] = self.PLOT.VIDEO_AUTHOR.get()
      conf['VIDEO_COMMENT'] = self.PLOT.VIDEO_COMMENT.get()
      conf['VIDEO_FPS'] = self.PLOT.VIDEO_FPS.get()
      conf['VIDEO_DPI'] = self.PLOT.VIDEO_DPI.get()
      with io.open(self.PLOT.FILECONF,'w',encoding='utf8') as outfile:
        str_ = json.dumps(conf,ensure_ascii=False,    \
                               sort_keys=True,        \
                               indent=2,              \
                               separators=(',',': '))
        outfile.write(to_unicode(str_))


    # Main
    # ----
    if self.nfiles == 0:
      messagebox.showinfo(message='No data files have been uploaded')
      return

    if self.Window_anim is not None:
      self.Window_anim.lift()
      return

   
    self.Window_anim = tk.Toplevel(self.master)
    self.Window_anim.title('Animation creation')
    self.Window_anim.resizable(width=True,height=True)
    self.Window_anim.protocol('WM_DELETE_WINDOW',_close)


    # Menu:
    # AAA
    menubar = tk.Menu(self.Window_anim)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Save',command=_saveconf)
    try:
      self.Window_anim.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.Window_anim.tk.call(self.Window_anim, "config", "-menu", menubar)

    # Widgets
    #
    F0 = ttk.Frame(self.Window_anim,borderwidth=5,padding=5)
    ttk.Label(F0,text='Output filename : ').grid(row=0,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_NAME,width=40).grid(row=0,column=1,columnspan=4,sticky='w')
    ttk.Label(F0,text='Video title : ').grid(row=1,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_TITLE,width=40).grid(row=1,column=1,columnspan=4,sticky='w')
    ttk.Label(F0,text='Author : ').grid(row=2,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_AUTHOR,width=40).grid(row=2,column=1,columnspan=4,sticky='w')
    ttk.Label(F0,text='Comment : ').grid(row=3,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_COMMENT,width=40).grid(row=3,column=1,columnspan=4,sticky='w')
    ttk.Label(F0,text='Initial frame : ').grid(row=4,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_L1,width=7).grid(row=4,column=1,sticky='w')
    ttk.Label(F0,text='Final frame : ').grid(row=5,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_L2,width=7).grid(row=5,column=1,sticky='w')
    ttk.Label(F0,text='FPS : ').grid(row=6,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_FPS,width=7).grid(row=6,column=1,sticky='w')
    ttk.Label(F0,text='DPI : ').grid(row=7,column=0)
    ttk.Entry(F0,textvariable=self.PLOT.VIDEO_DPI,width=7).grid(row=7,column=1,sticky='w')
    done = ttk.Button(F0,text='Do it',command=_done)
    done.grid(row=8,column=3,padx=3)
    done.bind("<Return>",lambda e:_done())
    close = ttk.Button(F0,text='Close',command=_close)
    close.grid(row=8,column=4,padx=3)
    close.bind("<Return>",lambda e:_close())
    F0.grid()
    F1 = ttk.Frame(self.Window_anim,borderwidth=5,padding=5)
    self.Mfig = Figure(figsize=self.PLOT.SIZE,dpi=self.PLOT.DPI.get())
    self.Max = self.Mfig.add_subplot(111)
    self.Mcanvas = FigureCanvasTkAgg(self.Mfig,master=F1)
    self.Mcanvas.show()
    self.Mcanvas.get_tk_widget().grid(row=0,column=0,columnspan=11,sticky='wn')
    self.Mdrawmap = True
    F1.grid()
    self.make_Mplot()








  # ===========================================
  def DepthandDate(self,CDF):
  # ===========================================
    '''Fill the lists: K_LIST, L_LIST, Z_LIST, T_LIST and DATE'''

    CDF.K.set(0)             # Default layer
    CDF.L.set(0)             # Default time step

    CDF.K_LIST   = list(range(CDF.icdf.nz))
    CDF.L_LIST   = list(range(CDF.icdf.nt))

   # Depth selector
    if CDF.icdf.idk > -1:
      if self.PLOT.GEOMAP.get():
        wrk = CDF.ncid.variables[CDF.icdf.zname][:]
        CDF.Z_LIST = list(wrk)
        print(CDF.Z_LIST)
      else:
        CDF.Z_LIST = np.arange(CDF.icdf.nz)
    else:
      CDF.Z_LIST = []

   # Time selector and TIME and DATE values
    CDF.DATE = []
    if CDF.icdf.idl > -1:
      wrk = CDF.ncid.variables[CDF.icdf.tname][:]
      CDF.T_LIST = list(wrk)
      try:
        for i in range(CDF.icdf.nt):
          CDF.DATE.append(num2date(CDF.T_LIST[i],       \
                          units=CDF.icdf.time_units,    \
                          calendar=CDF.icdf.time_calendar))
      except:
        for i in range(CDF.icdf.nt):
          CDF.DATE.append(i)

      try:
        CDF.TIME = np.array([(CDF.DATE[i]-CDF.DATE[0]).total_seconds() \
                           for i in range(CDF.icdf.nt)])
      except:
        CDF.TIME = np.array([(CDF.DATE[i]-CDF.DATE[0]) \
                           for i in range(CDF.icdf.nt)])

    else:
      CDF.T_LIST = []
      CDF.DATE = [' ']
      CDF.TIME = np.array([0])



  
  # ===========================================
  def read_lonlat(self,CDF,xname,yname):
  # ===========================================
    '''Read 1D/2D lon lat grid '''

    if CDF.icdf.georef:
      vlon = CDF.ncid.variables[xname]
      vlat = CDF.ncid.variables[yname]
      print(vlon)
      print(vlat)
    else:
      print('Georef is False')
      self.PLOT.GEOMAP.set(False)
      vlon = np.arange(CDF.icdf.nx)
      vlat = np.arange(CDF.icdf.ny)

    CDF.lon = vlon[:].copy()
    CDF.lat = vlat[:].copy()
    if len(vlon.shape) == 1:
      CDF.xx,CDF.yy = np.meshgrid(CDF.lon,CDF.lat)
    else:
      CDF.xx = vlon[:].copy()
      CDF.yy = vlat[:].copy()

  # ===========================================
  def read_UV(self,VEC):
  # ===========================================
    '''Read 2D velocity data according to user selections'''
    #K     = self.K.get()
    #L     = self.L.get()
    K     = VEC.K.get()
    L     = VEC.L.get()
    uname = '%s' % VEC.uname.get()
    vname = '%s' % VEC.vname.get()
    ndim  = VEC.icdf.ndims[VEC.uid]

    #VEC.K.set(K)
    #VEC.L.set(L)

    if ndim == 2:
      VEC.VEL.u = VEC.ncid.variables[uname][:,:]
      VEC.VEL.v = VEC.ncid.variables[vname][:,:]
    elif ndim == 3:
      if VEC.icdf.ppl[VEC.uid] > -1:
        VEC.VEL.u = VEC.ncid.variables[uname][L,:,:].squeeze()
        VEC.VEL.v = VEC.ncid.variables[vname][L,:,:].squeeze()
      elif VEC.icdf.ppk[VEC.uid] > -1:
        VEC.VEL.u = VEC.ncid.variables[uname][K,:,:].squeeze()
        VEC.VEL.v = VEC.ncid.variables[vname][K,:,:].squeeze()
      else:
        print('Invalid file!')
        return
    elif ndim == 4:
      VEC.VEL.u = VEC.ncid.variables[uname][L,K,:,:].squeeze()
      VEC.VEL.v = VEC.ncid.variables[vname][L,K,:,:].squeeze()
    else:
      print('Invalid number of dimensions, ',ndim)

    _u   = VEC.VEL.u.copy()
    _v   = VEC.VEL.v.copy()
    msku = ma.getmask(VEC.VEL.u)
    mskv = ma.getmask(VEC.VEL.v)
    msk  = ma.mask_or(msku,mskv)
    VEC.VEL.u = ma.array(_u,mask=msk).copy()
    VEC.VEL.v = ma.array(_v,mask=msk).copy()
    #VEC.VEL.speed = np.sqrt(VEC.VEL.u**2+VEC.VEL.v**2)
    #VEC.VEL.F = interpolate.interp2d(VEC.lon, \
    #                                 VEC.lat, \
    #                                 VEC.VEL.speed)



  # ===========================================
  #def read_Field(self,FIELD,ncid,icdf,sid,K,L):
  # ===========================================
  # ===========================================
  def read_CDF(self,CDF,update_lims=True):
  # ===========================================
    '''Read 2D data according to user selections'''

    #  self.read_Field(self.CDF[ii].FIELD,   \
    #                  self.CDF[ii].ncid,    \
    #                  self.CDF[ii].icdf,    \
    #                  self.CDF[ii].varid,   \
    #                  self.CDF[ii].K.get(), \
    #                  self.CDF[ii].L.get())
    if CDF.varid < 0:
      CDF.FIELD.data = None
      return

    K = CDF.K.get()
    L = CDF.L.get()

    vname = '%s' % CDF.varname.get()
    print('READ_FIELD Reading Var, Level and Time:', CDF.varid,   \
                                                     CDF.K.get(), \
                                                     CDF.L.get())

    ndim = CDF.icdf.ndims[CDF.varid]
    if ndim == 2:
      CDF.FIELD.data = CDF.ncid.variables[vname][:,:]
    elif ndim == 3:
      if CDF.icdf.ppl[CDF.varid] > -1:
        CDF.FIELD.data = CDF.ncid.variables[vname][L,:,:].squeeze()
      elif CDF.icdf.ppk[CDF.varid]  > -1:
        CDF.FIELD.data = CDF.ncid.variables[vname][K,:,:].squeeze()
      else:
        messagebox.showinfo(message='Invalid variable dimensions')
        CDF.FIELD.data = None
    elif ndim == 4:
      CDF.FIELD.data = CDF.ncid.variables[vname][L,K,:,:].squeeze()

    CDF.FIELD.missing_value = None

    if CDF.FIELD.data is not None:
      CDF.FIELD.varname = vname
      try:
        CDF.FIELD.units = CDF.ncid.variables[vname].getncattr('units')
      except:
        CDF.FIELD.units = ''

      try:
        CDF.FIELD.missing_value = CDF.ncid.variables[vname].getncattr('_FillValue')
      except:
        try:
          CDF.FIELD.missing_value = CDF.ncid.variables[vname].getncattr('missing_value')
        except:
          CDF.FIELD.missing_value = None

      if CDF.FIELD.missing_value is not None:
        CDF.FIELD.mask = ma.getmask(CDF.FIELD.data)
        CDF.FIELD.data[CDF.FIELD.data==CDF.FIELD.missing_value] = np.nan

      # Contour intervals
      CDF.FIELD.minval = float(CDF.FIELD.data.min())
      CDF.FIELD.maxval = float(CDF.FIELD.data.max())
      print('Min val = ',CDF.FIELD.minval)
      print('Max val = ',CDF.FIELD.maxval)

      if update_lims:
        try:
          CDF.FIELD.PLOT.CONTOUR_MIN.set(myround(CDF.FIELD.minval))
        except:
          CDF.FIELD.PLOT.CONTOUR_MIN.set(CDF.FIELD.minval)
        try:
          CDF.FIELD.PLOT.CONTOUR_MAX.set(myround(CDF.FIELD.maxval))
        except:
          CDF.FIELD.PLOT.CONTOUR_MAX.set(CDF.FIELD.maxval)

        dd =   CDF.FIELD.PLOT.CONTOUR_MAX.get() \
             - CDF.FIELD.PLOT.CONTOUR_MIN.get()
        try:
          CDF.FIELD.PLOT.CONTOUR_INTERVAL.set(myround(0.1*dd,0))
        except:
          CDF.FIELD.PLOT.CONTOUR_INTERVAL.set(0.1*dd)


  # ==================
  def get_contour(self):
  # ==================
    '''Widget to read Netcdf files'''

    self.CSOURCE = tk.StringVar()
    self.CSOURCE.set('Local Dataset')
    self.DATETIME = ''

    def _close():
    # ===========
      self.Window_ncdf.destroy()
      self.Window_ncdf = None
      return

    def _done():
    # ===========
      ii = self.CDF_INDX.get()
      self.read_CDF(self.CDF[ii])
      #self.read_Field(self.CDF[ii].FIELD,   \
      #                self.CDF[ii].ncid,    \
      #                self.CDF[ii].icdf,    \
      #                self.CDF[ii].varid,   \
      #                self.CDF[ii].K.get(), \
      #                self.CDF[ii].L.get())

      try:
        nodate = empty(self.DATE[0])
      except:
        nodate = False
      try:
        nodatetime = empty(self.DATETIME)
      except:
        nodatetime = False

      if not nodatetime:
        if nodate:
          self.DATE[0] = self.DATETIME
        else:
          if len(self.DATE[0]) == 1:
            a = self.DATE[0].__str__()
            b = self.CDF[ii].DATE[0].__str__()
            if a == b:
              self.DATE[0] = self.DATETIME
        self.CDF[ii].DATE[0] = self.DATETIME

      #self.CDF[ii].FIELD.F = interpolate.interp2d(self.CDF[ii].lon, \
      #                                           self.CDF[ii].lat, \
      #                                           self.CDF[ii].FIELD.data)

      _close()
      self.make_plot()
      if self.Window_contourconfig is not None:
        self.Window_contourconfig.destroy()
        self.Window_contourconfig = None
        self.contour_config()


    def _clear():
    # ===========
      if self.ncdf == 0:
        return

      ii = self.CDF_INDX.get()
      print('Erasing record ', ii)

      for i in range(self.nfiles):
        if self.FILETYPES[i] == 'FLD' and self.FILEORDER[i] == ii:
          del self.FILENAMES[i]
          del self.FILETYPES[i]
          del self.FILEORDER[i]
          del self.SEQUENCES[i]
      self.nfiles -= 1

      del self.CDF[ii]
      self.ncdf -= 1
      ii = self.ncdf-1 if ii >= self.ncdf else ii
      self.CDF_INDX.set(ii)
      _refill(ii)
      self.make_plot()

    def _reget():
    # ===========
      self.CDF_INDX.set(_wsel.get())
      ii = self.CDF_INDX.get()
      _refill(ii)

    def _refill(ii):
    # ==============
      if ii >= 0:
        self.CDF_LIST = list(range(self.ncdf))
        _wsel.configure(state='!disabled')
        _wsel['values'] = self.CDF_LIST
        _went['textvariable'] = self.CDF[ii].FILENAME
        _wvar.configure(state='!disabled')
        _wvar['textvariable'] = self.CDF[ii].varname
        _wvar['values'] = self.CDF[ii].icdf.VAR_MENU
        _kbox.configure(state='!disabled')
        _kbox['textvariable'] = self.CDF[ii].K
        _kbox['values'] = self.CDF[ii].K_LIST
        _lbox.configure(state='!disabled')
        _lbox['textvariable'] = self.CDF[ii].L
        _lbox['values'] = self.CDF[ii].L_LIST
        if self.CDF[ii].icdf.idk < 0:
          _kbox.configure(state='disabled')
          _zbox['text']='--'
        else:
          _zbox['text']=self.CDF[ii].Z_LIST[self.CDF[ii].K.get()]
        if self.CDF[ii].icdf.idl < 0:
          _lbox.configure(state='disabled')
          _dbox['text']='--'
        else:
          _lbox['textvariable'] = self.CDF[ii].L
          _lbox['values'] = self.CDF[ii].L_LIST
          _dbox['text'] = self.CDF[ii].DATE[self.CDF[ii].L.get()]
        _show['variable'] = self.CDF[ii].FIELD.show

      else:
        self.CDF         = []
        self.CDF_LIST    = [None]
        self.CDF_INDX    = tk.IntVar()
        self.CDF_INDX.set(0)

        _wsel.configure(state='disabled')
        _wvar.configure(state='disabled')
        _kbox.configure(state='disabled')
        _lbox.configure(state='disabled')
        _wsel['values'] = self.CDF_LIST
        _went['textvariable'] = ''
        _wvar['textvariable'] = ''
        _wvar['values'] = ['']
        _wvar.configure(state='disabled')
        _kbox['textvariable'] = ''
        _kbox['values'] = ['']
        _zbox['text'] = '--'
        _lbox['text'] = ''
        _lbox['values'] = ['']
        _lbox['textvariable'] = ''
        _lbox['values'] = ['']
        _dbox['text'] = ['--']

    def _add(SOURCE):
    # ===============

      global Window_select

      def _cancel():
      # ============
        global Window_select
        Window_select.destroy()
        Window_select = None

      def _done():
      # ==========
        global Window_select
        global _wvar

        if empty(CDF.varname.get()):
          messagebox.showinfo(parent=Window_select,message='Select variable')
          return
        else:
          CDF.varid = CDF.icdf.vname.index(CDF.varname.get())

        # Seems the good place where to put this:
        self.read_lonlat(CDF,CDF.icdf.xname,CDF.icdf.yname)
        self.DepthandDate(CDF)
        CDF.FIELD.show.set(True)
        if empty(CDF.DATE[0].__str__()):
          _dsel.configure(state='enabled')

        self.ncdf += 1
        self.CDF.append(CDF)
        self.CDF_INDX.set(self.ncdf-1)
        self.CDF_LIST = list(range(self.ncdf))

        self.nfiles += 1
        self.FILENAMES.append(CDF.FILENAME.get())
        self.FILETYPES.append('FLD')
        self.FILEORDER.append(self.ncdf-1)
        self.SEQUENCES.append(tk.BooleanVar(value=False))

        ii = self.CDF_INDX.get()

        if not empty(self.DATETIME):
          self.CDF[ii].DATE.append(self.DATETIME)

        if self.first:
          if self.drawmap is None:
            self.PLOT.WEST.set(np.min(self.CDF[ii].lon))
            self.PLOT.EAST.set(np.max(self.CDF[ii].lon))
            self.PLOT.SOUTH.set(np.min(self.CDF[ii].lat))
            self.PLOT.NORTH.set(np.max(self.CDF[ii].lat))
            self.plot_initialize()
          self.L.set(self.CDF[ii].L.get())
          self.L_LIST = list(range(self.CDF[ii].icdf.nt))
          self.NL = len(self.L_LIST)
          self.lbox.configure(state='!disabled')
          self.lbox['values'] = self.L_LIST
          self.DATE = self.CDF[ii].DATE.copy()
          self.TIME = self.CDF[ii].TIME.copy()
          #self.TFILE = '%d' % self.nfiles
          self.PLOT.TLABEL.set(self.CDF[ii].DATE[self.CDF[ii].L.get()])
          if len(self.DATE) > 1:
            self.bnext.configure(state='normal')
          try:
            self.PLOT.XLABEL.set(self.CDF[ii].ncid.variables[self.CDF[ii].icdf.xname].getncattr('long_name'))
          except:
            self.PLOT.XLABEL.set(self.CDF[ii].icdf.xname)
          try:
            self.PLOT.YLABEL.set(self.CDF[ii].ncid.variables[self.CDF[ii].icdf.yname].getncattr('long_name'))
          except:
            self.PLOT.YLABEL.set(self.CDF[ii].icdf.yname)
          self.SEQUENCES[-1].set(True)
          self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
          self.first = False
        else:
          ntime = len(self.DATE)
          same  = True
          if len(self.CDF[ii].DATE) == ntime:
            print('Same number of time records')
            for i in range(ntime):
              if self.DATE[i] != self.CDF[ii].DATE[i]:
                same = False
            self.SEQUENCES[-1].set(same)
                       
        _refill(ii)
        Window_select.destroy()
        Window_select = None
        self.DATETIME = ''



      ISOURCE = self.CONTOUR_OPTIONS.index(SOURCE)
      if ISOURCE == 0:
        filename = self.get_opendap_filename()
      elif ISOURCE == 1:
        filename = self.get_copernicus_filename()
      elif ISOURCE == 2:
        nn = filedialog.askopenfilename(parent=self.Window_ncdf, \
                                        filetypes=[('Netcdf','*.nc'),  \
                                                   ('CDF','*.cdf'),  \
                                                   ('ALL','*')])
        if len(nn) == 0:
          return
        else:
          filename = '%s' % nn
      elif ISOURCE == 3:
        aa = get_remote()
        filename = aa.filename()
      else:
        if self.nvec <= 0:
          messagebox.showinfo(message='No Trajectory file opened yet')
          return
        else:
          jj = self.VEC_INDX.get()
          filename = self.VEC[ii].FILENAME.get()

      if empty(filename):
        return

      # Not empty filename:
      CDF = cdf_parameters()
      CDF.FIELD = fld_parameters()
      CDF.FILENAME.set(filename)
      CDF.ncid = Dataset(filename)
      CDF.icdf = tools.geocdf(filename)

      #self.read_lonlat(CDF,CDF.icdf.xname,CDF.icdf.yname)
      #self.DepthandDate(CDF)
      #CDF.FIELD.show.set(True)

      #if empty(CDF.DATE[0].__str__()):
      #  _dsel.configure(state='enabled')


      if Window_select is None:
        Window_select = tk.Toplevel(self.master)
        Window_select.title('SELECT VARIABLES')
        Window_select.protocol('WM_DELETE_WINDOW',Window_select.destroy)
      else:
        Window_select.lift()
        return

      axesid = tools.WinGeoaxes(CDF.icdf,CDF.ncid,Window_select)

      F0 = ttk.Frame(Window_select,padding=5,borderwidth=5)
      ttk.Label(F0,text='Select variable',borderwidth=3,font='bold') \
         .grid(row=0,column=0)
      dvar = ttk.Combobox(F0,textvariable=CDF.varname, \
                   values=CDF.icdf.VAR_MENU,    \
                   width=20)
      dvar.grid(row=0,column=1,columnspan=2)
      dvar.bind('<<ComboboxSelected>>',lambda e: axesid.selected_var(CDF.icdf,dvar))

      F0.grid()

      CDF.icdf.nx = -9999
      F1 = ttk.Frame(Window_select,padding=5)
      cancel = ttk.Button(F1,text='Cancel',command=_cancel)
      cancel.grid(row=0,column=3,sticky='e',padx=10)
      cancel.bind("<Return>",lambda e:_cancel())
      done = ttk.Button(F1,text='Done',command=_done)
      done.grid(row=0,column=4,sticky='e',padx=10)
      done.bind("<Return>",lambda e:_done())
      F1.grid(sticky='we')
      Window_select.wait_window(Window_select)

    def _lselection():
    # ================
      _dbox['text'] = self.CDF[ii].DATE[self.CDF[ii].L.get()]

    def _kselection():
    # ================
      _zbox['text'] = self.CDF[ii].Z_LIST[self.CDF[ii].L.get()]

    def _vselection():
    # ================
      try:
        self.CDF[ii].varid = self.CDF[ii].icdf.vname.index( \
                                             self.CDF[ii].varname.get())
      except:
        self.CDF[ii].varid = -1

    def _date():
    # ==========
      ''' Manually select a date'''
      aa = get_Date()
      self.DATETIME = aa.date
      _dbox['text'] = self.DATETIME

    # Main window:
    # ============
    if self.Window_ncdf is None:
      self.Window_ncdf = tk.Toplevel(self.master)
      self.Window_ncdf.title("Contour files")
      self.Window_ncdf.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_ncdf.lift()

    if self.ncdf > 0:
      ii = self.CDF_INDX.get()
    else:
      ii = -1

    global Window_select
    global _wvar

    Window_select = None
    F0 = ttk.Frame(self.Window_ncdf,padding=5)

    # Add
    ttk.Button(F0,text='Import', \
               command=lambda:_add(self.CSOURCE.get())).grid(row=1, \
                                                            column=0,padx=3)
    _source = ttk.Combobox(F0,textvariable=self.CSOURCE, \
                           values=self.CONTOUR_OPTIONS)
    _source.grid(row=0,column=0,padx=3)

    # Filename:
    ttk.Label(F0,text='Netcdf file').grid(row=0,column=1,padx=3)
    _wsel = ttk.Combobox(F0,textvariable=self.CDF_INDX, \
                                  values=self.CDF_LIST,width=5)
    _wsel.grid(row=0,column=2)
    _wsel.bind('<<ComboboxSelected>>',lambda e: _reget())
    _went = ttk.Entry(F0,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=3,columnspan=5,padx=3,sticky='w')

    # Variable:
    ttk.Label(F0,text='Variable').grid(row=1,column=1,padx=3,pady=3)
    _wvar = ttk.Combobox(F0,width=15)
    _wvar.grid(row=1,column=2,columnspan=2,sticky='w')
    _wvar.bind('<<ComboboxSelected>>',lambda e: _vselection())

    # Depth:
    ttk.Label(F0,text='Depth').grid(row=2,column=1,padx=3,pady=3)
    _kbox = ttk.Combobox(F0,values=['0'],width=5)
    _kbox.grid(row=2,column=2)
    _kbox.bind('<<ComboboxSelected>>',lambda e: _kselection())
    _zbox = ttk.Label(F0,width=20)
    _zbox.grid(row=2,column=3,columnspan=2,sticky='w')

    # Time:
    ttk.Label(F0,text='Time').grid(row=3,column=1,padx=3,pady=3)
    _lbox = ttk.Combobox(F0,width=5)
    _lbox.grid(row=3,column=2)
    _lbox.bind('<<ComboboxSelected>>',lambda e: _lselection())
    _dbox = ttk.Label(F0,width=20)
    _dbox.grid(row=3,column=3,columnspan=2,sticky='w')

    _dsel = ttk.Button(F0,text='Select date',command=_date)
    _dsel.grid(row=3,column=5,sticky='w')

    if ii == -1:
      _wsel.configure(state='disabled')
      _wvar.configure(state='disabled')
      _kbox.configure(state='disabled')
      _lbox.configure(state='disabled')
      _dsel.configure(state='disabled')
    else:
      _went['textvariable'] = self.CDF[ii].FILENAME
      _wvar['textvariable'] = self.CDF[ii].varname
      _wvar['values'] = self.CDF[ii].icdf.VAR_MENU
      _kbox['textvariable'] = self.CDF[ii].K
      _kbox['values'] = self.CDF[ii].K_LIST
      if self.CDF[ii].icdf.idk < 0:
        _kbox.configure(state='disabled')
        _zbox['text']='--'
      else:
        _zbox['text']=self.CDF[ii].Z_LIST[self.CDF[ii].K.get()]
      if self.CDF[ii].icdf.idl < 0:
        _lbox.configure(state='disabled')
        _dsel.configure(state='enabled')
        try:
          nodate = empty(sefl.CDF[ii].DATE[0])
        except:
          nodate = False
        if nodate:
          _dbox['text']='--'
        else:
          _dbox['text']=self.CDF[ii].DATE[0]

      else:
        _lbox['textvariable'] = self.CDF[ii].L
        _lbox['values'] = self.CDF[ii].L_LIST
        _dbox['text'] = self.CDF[ii].DATE[self.CDF[ii].L.get()]
        _dsel.configure(state='disabled')

    F0.grid(row=0,column=0)

    F1 = ttk.Frame(self.Window_ncdf,padding=5)
    if ii == -1:
      _show = ttk.Checkbutton(F1,text='Show')
      _show.configure(state='disabled')
    else:
      _show = ttk.Checkbutton(F1,text='Show',command=self.make_plot)
      _show['variable']=self.CDF[ii].FIELD.show
      _show.configure(command=self.make_plot)
    _show.grid(row=1,column=5)
    ttk.Button(F1,text='Cancel',command=_close).grid(row=1,column=6,padx=3)
    ttk.Button(F1,text='Clear',command=_clear).grid(row=1,column=7,padx=3)
    ttk.Button(F1,text='Plot',command=_done).grid(row=1,column=8,padx=3)
    F1.grid(row=1,column=0)


  #====================
  def get_saidin(self):
  #====================
    '''Function to retrieve the SAIDIN data'''

    def _close():
      self.Window_saidin.destroy()
      self.Window_saidin = None

    def _selector():
      name = saidin.saidin_selector(parent=self.master)
      if not empty(name):
        self.SAIDIN.FILENAME.set(name)

    def _done():
      if (empty(self.SAIDIN.FILENAME.get())):
        messagebox.showinfo(message='No image selected')
        return

      self.SAIDIN.ncid = Dataset(self.SAIDIN.FILENAME.get(),'r')
      self.SAIDIN.icdf = tools.geocdf(self.SAIDIN.FILENAME.get())
      self.SAIDIN.FIELD.varname = 'mcsst'
      self.SAIDIN.lon = self.SAIDIN.ncid.variables['lon'][:]
      self.SAIDIN.lat = self.SAIDIN.ncid.variables['lat'][:]
      self.SAIDIN.FIELD.data = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname][0,:,:].squeeze()
      self.SAIDIN.xx,self.SAIDIN.yy = np.meshgrid(self.SAIDIN.lon,self.SAIDIN.lat)
      self.DepthandDate(self.SAIDIN)

      self.nfiles += 1
      self.FILENAMES.append(self.SAIDIN.FILENAME.get())
      self.FILETYPES.append('SAIDIN')
      self.FILEORDER.append(0)
      self.SEQUENCES.append(tk.BooleanVar())

      if self.first:
        if self.drawmap is None:
          self.PLOT.WEST.set(np.min(self.SAIDIN.lon))
          self.PLOT.EAST.set(np.max(self.SAIDIN.lon))
          self.PLOT.SOUTH.set(np.min(self.SAIDIN.lat))
          self.PLOT.NORTH.set(np.max(self.SAIDIN.lat))
          self.plot_initialize()
        self.L.set(self.SAIDIN.L.get())
        self.DATE = self.SAIDIN.DATE.copy()
        self.TIME = self.SAIDIN.TIME.copy()
        try:
          self.PLOT.XLABEL.set(self.SAIDIN.ncid.variables[self.SAIDIN.icdf.xname]. \
                                                                          getncattr('long_name'))
        except:
          self.PLOT.XLABEL.set(self.SAIDIN.icdf.xname)
        try:
          self.PLOT.YLABEL.set(self.SAIDIN.ncid.variables[self.SAIDIN.icdf.yname] \
                                                                          .getncattr('long_name'))
        except:
          self.PLOT.YLABEL.set(self.SAIDIN.icdf.yname)
        self.first = False

      try:
        self.SAIDIN.FIELD.units = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname] \
                                                                          .getncattr('units')
      except:
        self.SAIDIN.FIELD.units = ''

      try:
        self.SAIDIN.FIELD.missing_value = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname] \
                                                                          .getncattr('_FillValue')
      except:
        try:
          self.SAIDIN.FIELD.missing_value = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname] \
                                                                          .getncattr('missing_value')
        except:
          self.SAIDIN.FIELD.missing_value = None

      print(self.SAIDIN.FIELD.data.min())
      print(self.SAIDIN.FIELD.data.max())
      if self.SAIDIN.FIELD.masked.get():
        print('Applying land/sea mask ...')
        _a  = self.SAIDIN.FIELD.data.copy()
        tmp  = self.SAIDIN.ncid.variables['lsmask'][0,:,:].squeeze()
        msk = ma.masked_where(tmp==1,tmp)
        self.SAIDIN.FIELD.data = ma.array(_a,mask=msk).copy()

      self.SAIDIN.FIELD.mask = ma.getmask(self.SAIDIN.FIELD.data)

      # Contour intervals
      self.SAIDIN.FIELD.minval = self.SAIDIN.FIELD.data.min()
      self.SAIDIN.FIELD.maxval = self.SAIDIN.FIELD.data.max()
      try:
        self.SAIDIN.FIELD.PLOT.CONTOUR_MIN.set(myround(self.SAIDIN.FIELD.minval))
      except:
        self.SAIDIN.FIELD.PLOT.CONTOUR_MIN.set(self.SAIDIN.FIELD.minval)
      try:
        self.SAIDIN.FIELD.PLOT.CONTOUR_MAX.set(myround(self.SAIDIN.FIELD.maxval))
      except:
        self.SAIDIN.FIELD.PLOT.CONTOUR_MAX.set(self.SAIDIN.FIELD.maxval)

      dd =   self.SAIDIN.FIELD.PLOT.CONTOUR_MAX.get() \
           - self.SAIDIN.FIELD.PLOT.CONTOUR_MIN.get()
      try:
        self.SAIDIN.FIELD.PLOT.CONTOUR_INTERVAL.set(myround(0.1*dd))
      except:
        self.SAIDIN.FIELD.PLOT.CONTOUR_INTERVAL.set(0.1*dd)

      #self.SAIDIN.FIELD.F = interpolate.interp2d(self.SAIDIN.lon, \
      #                                     self.SAIDIN.lat, \
      #                                     self.SAIDIN.FIELD.data)
      _close()
      self.make_plot()

    def _clear():
      self.SAIDIN.FILENAME.set('')
      self.SAIDIN.lon = None
      self.SAIDIN.lat = None
      self.SAIDIN.xx  = None
      self.SAIDIN.yy  = None
      self.SAIDIN.FIELD.data = None
      _close()

    if self.Window_saidin is None:
      self.Window_saidin = tk.Toplevel(self.master)
      self.Window_saidin.title("Satellite Sea surface temperature (SAIDIN)")
      self.Window_saidin.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_saidin.lift()

    F0 = ttk.Frame(self.Window_saidin,padding=5)
    ttk.Entry(F0,textvariable=self.SAIDIN.FILENAME,justify='left', \
              width=80).grid(row=0,column=0,columnspan=8,padx=3)
    ttk.Button(F0,text='Select',command=_selector).grid(row=0,column=8,padx=3)
    ttk.Checkbutton(F0,text='Mask land data',variable=self.SAIDIN.FIELD.masked).grid(row=1,column=5,padx=3)
    ttk.Button(F0,text='Cancel',command=_clear).grid(row=1,column=6,padx=3)
    ttk.Button(F0,text='Clear',command=_clear).grid(row=1,column=7,padx=3)
    ttk.Button(F0,text='Plot',command=_done).grid(row=1,column=8,padx=3)
    F0.grid()


  # =======================
  def get_marker(self):
  # =======================
    '''Widget to read Markers'''

    def _close():
    # ===========
      self.Window_marker.destroy()
      self.Window_marker = None

    def _done():
    # ===========
      ii = self.MARKER_INDX.get()
      if ii >= 0:
        self.MARKER[ii].LABEL.set(_wlab.get())
        self.make_plot()

      self.Window_marker.destroy()
      self.Window_marker = None
      if self.Window_dotconfig is not None:
        self.Window_dotconfig.destroy()
        self.Window_dotconfig = None
        self.marker_config()

    def _clear():
    # ===========
      '''Note that markers have no time axis'''
      if self.nmarker == 0:
        return

      ii = self.MARKER_INDX.get()

      for i in range(self.nfeatures):
        if self.FEATTYPES[i] == 'MARKER' and self.FEATORDER[i] == ii:
          del self.FEATNAMES[i]
          del self.FEATTYPES[i]
          del self.FEATORDER[i]
      self.neatures -= 1

      print('Erasing marker ',ii)
      del self.MARKER[ii]
      self.nmarker -= 1

      ii = self.nmarker-1 if ii >= self.nmarker else ii
      print('New marker = ',self.nmarker)
      self.MARKER_INDX.set(ii)
      _refill(ii)
      self.make_plot()
      _close()


    def _reget():
    # ===========
      self.MARKER_INDX.set(_wsel.get())
      ii = self.MARKER_INDX.get()
      _refill(ii)


    def _refill(ii):
    # ==============
      if ii >= 0:
        self.MARKER_LIST = list(range(self.nmarker))
        _wsel['values'] = self.MARKER_LIST
        _went['textvariable'] = self.MARKER[ii].FILENAME
        _wstat['text'] = ' N = '+str(self.MARKER[ii].n)
        _wsel.configure(state='!disabled')
        _wlab['state'] = '!disabled'
        _wlab['textvariable'] = self.MARKER[ii].LABEL
      else:
        self.MARKER         = []
        self.MARKER_LIST    = ['0']
        self.MARKER_INDX    = tk.IntVar()
        self.MARKER_INDX.set(0)
        _wsel['values'] = self.MARKER_LIST
        _went['textvariable'] = ''
        _wstat['text'] = ''
        _wsel.configure(state='disabled')
        _wlab['textvariable'] = ''
        _wlab.configure(state='disabled')

    def _add():
    # ========
      nn = filedialog.askopenfilename(filetypes=[('CSV','*.csv'),
                                                 ('TXT','*.txt'),
                                                 ('ALL','*')],
                                       initialdir='./',
                                       parent=self.Window_marker)
      if len(nn) == 0:
        return
      else:
        filename = '%s' % nn

      # Not empty filename:
      MARKER = geomarker.parameters()
      MARKER.Read(filename)
      if MARKER.n == 0:
        return

      self.nmarker += 1
      self.MARKER.append(MARKER)
      self.MARKER_INDX.set(self.nmarker-1)
      self.MARKER_LIST = list(range(self.nmarker))

      self.nfeatures += 1
      self.FEATNAMES.append(MARKER.FILENAME.get())
      self.FEATTYPES.append('MARKER')
      self.FEATORDER.append(self.nfeatures-1)

      ii = self.MARKER_INDX.get()
      _refill(ii)

      #self.make_plot()

    # Main window:
    # ============

    if self.Window_marker is not None:
      self.Window_marker.lift()
      return

    self.Window_marker = tk.Toplevel(self.master)
    self.Window_marker.title('Geomarkers')
    self.Window_marker.protocol('WM_DELETE_WINDOW',_close)

    if self.nmarker > 0:
      ii = self.MARKER_INDX.get()
    else:
      ii = -1

    F0 = ttk.Frame(self.Window_marker,padding=5)

    # Add
    ttk.Button(F0,text='Add',command=_add).grid(row=0,column=0,padx=3)

    # Filename:
    ttk.Label(F0,text='Marker file').grid(row=0,column=1,padx=3)

    _wsel = ttk.Combobox(F0,textvariable=self.MARKER_INDX, \
                                  values=self.MARKER_LIST,width=5)
    _wsel.grid(row=0,column=2)
    _wsel.bind('<<ComboboxSelected>>',lambda e: _reget())
    _went = ttk.Entry(F0,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=3,columnspan=5,padx=3,sticky='w')

    # AAA
    if ii == -1:
      _wstat = ttk.Label(F0,text='',width=50,justify='left')
      _wsel.configure(state='disabled')
    else:
      _wstat = ttk.Label(F0,text=' N = '+str(self.MARKER[ii].n),width=50,justify='left')
      _went['textvariable'] = self.MARKER[ii].FILENAME

    _wstat.grid(row=1,column=3,columnspan=5,padx=3,sticky='w')

    ttk.Label(F0,text='Marker Label').grid(row=2,column=1,padx=3)
    _wlab = ttk.Entry(F0,justify='left',width=18)
    _wlab.grid(row=2,column=2,columnspan=2,padx=3,sticky='w')
    if ii == -1:
      _wlab['state'] = 'disabled'
    else:
      _wlab['textvariable'] = self.MARKER[ii].LABEL

    F0.grid(row=0,column=0)

    F1 = ttk.Frame(self.Window_marker,padding=5)
    ttk.Button(F1,text='Clear',command=_clear).grid(row=1,column=6,padx=3)
    ttk.Button(F1,text='Done',command=_done).grid(row=1,column=7,padx=3)
    F1.grid(row=1,column=0)


  # =======================
  def marker_config(self):
  # =======================
    '''Widget to configure Markers'''
    #self.dot_config(self.MARKER[self.MARKER_INDX.get()])

    global ishow

    if self.nmarker == 0:
      messagebox.showinfo(message='No Marker file opened yet')
      return

    def _cancel():
    # ============
      self.Window_dotconfig.destroy()
      self.Window_dotconfig = None

    def _apply():
    # ============
      self.make_plot()

    def _done():
    # ============
      self.make_plot()
      self.Window_dotconfig.destroy()
      self.Window_dotconfig = None

    def _selected():
    # ==============
      global ishow
      itab = self.Mnb.index('current')
      ishow.destroy()
      # The usual configuration:
      ii = self.MARKER_INDX.get()
      _went['textvariable'] = self.MARKER[ii].FILENAME

      ishow = ttk.Frame(self.Window_dotconfig,padding=10)

      # Define tabs:
      self.Mnb = ttk.Notebook(ishow)
      page0 = ttk.Frame(self.Mnb)
      page1 = ttk.Frame(self.Mnb)
      page2 = ttk.Frame(self.Mnb)
      page3 = ttk.Frame(self.Mnb)
      self.Mnb.add(page0,text='Label Aspect')
      self.Mnb.add(page1,text='Marker Aspect')
      self.Mnb.add(page2,text='Label Text')
      self.Mnb.add(page3,text='Marker coordinates')
      self.Mnb.grid()
      self.Mnb.select(itab)

      # Page0
      ttk.Label(page0,
              text='Show as text',
              padding=3).grid(row=0,column=0,padx=3,sticky='e')
      ttk.Checkbutton(page0,
                    variable=self.MARKER[ii].textmode).grid(row=0,
                                                            column=1,
                                                            padx=3,
                                                            sticky='w')
      ttk.Label(page0,
              text='Generic label',
              padding=3).grid(row=1,column=0,padx=3,sticky='e')
      ttk.Entry(page0,
              textvariable=self.MARKER[ii].LABEL).grid(row=1,
                                                       column=1,
                                                       padx=3,
                                                       sticky='w')

      
      # Page 1
      dotplot.Configuration(page1,self.MARKER[ii].PLOT)
  
      # Page 2
      geomarker.TextConfigure(page2,self.MARKER[ii].PLOT)

      # Page 3
      geomarker.ShowData(page3,self.MARKER[ii])

      f0 = ttk.Frame(ishow,padding=5)
      ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
          grid(row=0,column=0,padx=3)
      ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
          grid(row=0,column=1,padx=3)
      ttk.Button(f0,text='Done',command=_done,padding=5).     \
          grid(row=0,column=2,padx=3)
      f0.grid(sticky='ew',columnspan=3)
      ishow.grid()

    def _loadconf():
    # =============
      '''Load dot configuration'''
      ii = self.MARKER_INDX.get()
      print('Restoring dot configuration')
      try:
        self.MARKER[ii].PLOT.load(self.MARKER[ii].PLOT.FILECONF)
      except:
        print('Error: Unable to load file ',self.MARKER[ii].PLOT.FILECONF)
      self.make_plot()

    def _saveconf():
    # =============
      '''Load dot configuration'''
      ii = self.MARKER_INDX.get()
      print('Saving dot configuration')
      try:
        self.MARKER[ii].PLOT.save(self.MARKER[ii].PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',self.MARKER[ii].PLOT.FILECONF)

    def _loadfromconf():
    # ==================
      '''Load dot configuration from a file'''
      ii = self.MARKER_INDX.get()
      nn = filedialog.askopenfilename(title='Load dot configuration',
                                      parent=self.Window_dotconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.MARKER[ii].PLOT.FILECONF = '%s' % nn
      print('Restoring dot configuration from ',
            self.MARKER[ii].PLOT.FILECONF)
      try:
        self.MARKER[ii].PLOT.load(self.MARKER[ii].PLOT.FILECONF)
      except:
        print('Error: Unable to load file ',self.MARKER[ii].PLOT.FILECONF)
      self.make_plot()

    def _saveasconf():
    # ================
      '''Save dot configuration to a file'''
      ii = self.MARKER_INDX.get()
      nn = filedialog.asksaveasfilename(title='Save dot configuration',
                                        parent=self.Window_dotconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      self.MARKER[ii].PLOT.FILECONF = '%s' % nn
      print('Saving dot configuration to ',self.MARKER[ii].PLOT.FILECONF)
      try:
        self.MARKER[ii].PLOT.save(self.MARKER[ii].PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',self.MARKER[ii].PLOT.FILECONF)





    if self.Window_dotconfig is not None:
      self.Window_dotconfig.lift()
      return

    self.Window_dotconfig = tk.Toplevel(self.master)
    self.Window_dotconfig.title('Marker plot configuration')
    self.Window_dotconfig.resizable(width=True,height=True)
    self.Window_dotconfig.protocol('WM_DELETE_WINDOW',_cancel)

    menubar = tk.Menu(self.Window_dotconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Restore from',command=_loadfromconf)
    menu.add_command(label='Save',command=_saveconf)
    menu.add_command(label='Save as',command=_saveasconf)
    try:
      self.Window_dotconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      master.tk.call(Window_dotconfig, "config", "-menu", menubar)


    fsel = ttk.Frame(self.Window_dotconfig,padding=10)
    ttk.Label(fsel,text="File: ").grid(row=0,column=0,sticky='e',padx=3)
    _wsel = ttk.Combobox(fsel,textvariable=self.MARKER_INDX,
                              values=self.MARKER_LIST,width=5)
    _wsel.grid(row=0,column=1,sticky='w',padx=3)
    _wsel.bind('<<ComboboxSelected>>',lambda e:_selected())
    _went = ttk.Entry(fsel,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=2,columnspan=5,padx=3,sticky='w')
    _went = ttk.Entry(fsel,justify='left',width=80,state='readonly')
    _went.grid(row=0,column=2,columnspan=8,padx=3,sticky='w')
    fsel.grid()

    # The usual configuration:
    ii = self.MARKER_INDX.get()
    _went['textvariable'] = self.MARKER[ii].FILENAME

    ishow = ttk.Frame(self.Window_dotconfig,padding=10)
    # Define tabs:
    self.Mnb = ttk.Notebook(ishow)
    page0 = ttk.Frame(self.Mnb)
    page1 = ttk.Frame(self.Mnb)
    page2 = ttk.Frame(self.Mnb)
    page3 = ttk.Frame(self.Mnb)
    self.Mnb.add(page0,text='Label Aspect')
    self.Mnb.add(page1,text='Marker Aspect')
    self.Mnb.add(page2,text='Label Text')
    self.Mnb.add(page3,text='Marker coordinates')
    self.Mnb.grid()

    # Page0
    ttk.Label(page0,
              text='Show as text',
              padding=3).grid(row=0,column=0,padx=3,sticky='e')
    ttk.Checkbutton(page0,
                    variable=self.MARKER[ii].textmode).grid(row=0,
                                                            column=1,
                                                            padx=3,
                                                            sticky='w')
    ttk.Label(page0,
              text='Generic label',
              padding=3).grid(row=1,column=0,padx=3,sticky='e')
    ttk.Entry(page0,
              textvariable=self.MARKER[ii].LABEL).grid(row=1,
                                                       column=1,
                                                       padx=3,
                                                       sticky='w')
    # Page 1
    dotplot.Configuration(page1,self.MARKER[ii].PLOT)

    # Page 2
    geomarker.TextConfigure(page2,self.MARKER[ii].PLOT)

    # Page 3
    geomarker.ShowData(page3,self.MARKER[ii])

    f0 = ttk.Frame(ishow,padding=5)
    ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
        grid(row=0,column=0,padx=3)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Done',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew')
    ishow.grid()




  # =======================
  def get_lagrangian(self):
  # =======================
    '''Widget to retrieve Lagrangian trajectory data'''

    self.LSOURCE = tk.StringVar()
    self.LSOURCE.set(self.FLOAT_OPTIONS[0])

    def _cancel():
    # ===========
      self.Window_float.destroy()
      self.Window_float = None

    def _close():
    # ===========
      self.Window_float.destroy()
      self.Window_float = None
      self.make_plot()
      if self.Window_lineconfig is not None:
        self.Window_lineconfig.destroy()
        self.Window_lineconfig = None
        self.lagrangian_config()

    def _clear():
    # ===========
      if self.nfloat == 0:
        return

      ii = self.FLOAT_INDX.get()

      for i in range(self.nfiles):
        if self.FILETYPES[i] == 'FLOAT' and self.FILEORDER[i] == ii:
          del self.FILENAMES[i]
          del self.FILETYPES[i]
          del self.FILEORDER[i]
          del self.SEQUENCES[i]
      self.nfiles -= 1

      if self.nfiles == 0:
        self.TIME = []
        self.DATE = []
        self.L.set(0)
        self.L_LIST = []
        self.NL = 0
        self.bnext.configure(state='disabled')
        self.bprev.configure(state='disabled')
        self.PLOT.TLABEL.set('')
        self.lbox['values'] = self.L_LIST
        self.lbox.configure(state='disabled')
        self.first = True

      print('Erasing record ', ii)
      del self.FLOAT[ii]
      self.nfloat -= 1

      ii = self.nfloat-1 if ii >= self.nfloat else ii
      print('new nfloat = ',self.nfloat)
      self.FLOAT_INDX.set(ii)
      _refill(ii)
      #_close()

    def _reget():
    # ===========
      self.FLOAT_INDX.set(_wsel.get())
      ii = self.FLOAT_INDX.get()
      _refill(ii)

    def _refill(ii):
    # ==============
      if ii >= 0:
        self.FLOAT_LIST = list(range(self.nfloat))
        _wsel['values'] = self.FLOAT_LIST
        _went['textvariable'] = self.FLOAT[ii].FILENAME
        _wstat['text'] = ' Nfloats = '+str(self.FLOAT[ii].nfloats)
        _wsel.configure(state='!disabled')
        _show['variable']=self.FLOAT[ii].SHOW
      else:
        self.FLOAT         = []
        self.FLOAT_LIST    = ['0']
        self.FLOAT_INDX    = tk.IntVar()
        self.FLOAT_INDX.set(0)
        _wsel['values'] = self.FLOAT_LIST
        _went['textvariable'] = ''
        _wstat['text'] = ''
        _wsel.configure(state='disabled')

    def _add():
    # ========
      ISOURCE = self.FLOAT_OPTIONS.index(self.LSOURCE.get())

      if ISOURCE == 0:

        types=[('Netcdf','*.nc'),('JSON','*.json'),       \
               ('GEOJSON','*.geojson'),('ALL','*')]
        nn = filedialog.askopenfile(parent=self.Window_float, \
                                    filetypes=types)
        try:
          if empty(nn.name):
            return
        except:
          return
        _load_trajectory(nn.name)

      elif ISOURCE == 1:
        path = '%s' % filedialog.askdirectory(parent=self.Window_float, \
                                   title='Select local trajectory folder')
        if empty(path):
          return

        filelist = folderList(path,'geojson')
        if len(filelist) > 0:
          for f in filelist:
            filename = join(path,f)
            print('Loading file: ',filename)
            _load_trajectory(filename)

      elif ISOURCE == 2:
        url = simple_form('Select remote trajectory folder','url')
        if empty(url):
          return

        filelist = urlList(url,'geojson')
        if len(filelist) > 0:
          for filename in filelist:
            print('Loading file: ',filename)
            _load_trajectory(filename)

      elif ISOURCE == 3:
        filelist = db.select_exp()
        if len(filelist) > 0:
          for filename in filelist:
            _load_trajectory(filename)


    def _load_trajectory(filename):
    # ==================================

      FLT = lagrangian.parameters()
      FLT.Read(filename)

      if FLT.nfloats is None or FLT.nfloats==0 or FLT.nrecords==0:
        return

      if self.first:
        # Set figure DATE and TIME references
        if FLT.SOURCE == 'blm':
          self.DATE = [FLT.date[i].replace(tzinfo=None) 
                                     for i in range(FLT.nrecords)]
          self.TIME = np.array([(FLT.date[i].replace(tzinfo=None)-  \
                                self.DATE[0]).total_seconds()       \
                                     for i in range(FLT.nrecords)])

        elif FLT.SOURCE == 'mlm':
          #messagebox.showinfo(message='No field has been opened yet. '  + \
          #'The TIME and DATE vectors will be the ones in this float file.' + \
          #     'The FIRST of the floats is taken a reference')
          self.DATE = [FLT.date[i][0].replace(tzinfo=None) 
                                     for i in range(FLT.nrecords)]
          self.TIME = np.array([(FLT.date[i][0].replace(tzinfo=None)-  \
                               self.DATE[0]).total_seconds()           \
                                     for i in range(FLT.nrecords)])

      if FLT.SOURCE == 'blm':
        # Set the values of TIME for the float:
        FLT.TIME = np.array([(FLT.date[i].replace(tzinfo=None)-  \
                             self.DATE[0]).total_seconds()       \
                                           for i in range(FLT.nrecords)])
        FLT.MAPX = []
        FLT.MAPY = []
        if FLT.nfloats > 1:
          for i in range(FLT.nfloats):
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon[:,i]),
                                     bounds_error=False, fill_value=np.NaN)
            FLT.MAPX.append(list(f(self.TIME)))
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat[:,i]),
                                     bounds_error=False, fill_value=np.NaN)
            FLT.MAPY.append(list(f(self.TIME)))
          # Transpose FLT.MAPX and FLT.MAPY:
          FLT.MAPX = np.array(FLT.MAPX).T.tolist() 
          FLT.MAPY = np.array(FLT.MAPY).T.tolist() 
        else:
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon),
                                   bounds_error=False, fill_value=np.NaN)
          FLT.MAPX = list(f(self.TIME))
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat),
                                   bounds_error=False, fill_value=np.NaN)
          FLT.MAPY = list(f(self.TIME))

      elif FLT.SOURCE == 'mlm':

        FLT.TIME =  []
        FLT.MAPX =  []
        FLT.MAPY =  []
        for j in range(FLT.nfloats):

          FTIME = np.array([(FLT.date[i][j].replace(tzinfo=None)-self.DATE[0]).total_seconds() for i in range(FLT.nrecords)])
          f = interpolate.interp1d(FTIME,np.array(FLT.lon[:,j]), bounds_error=False, fill_value=np.NaN)
          FLT.MAPX.append(list(f(self.TIME)))
          f = interpolate.interp1d(FTIME,np.array(FLT.lat[:,j]), bounds_error=False, fill_value=np.NaN)
          FLT.MAPY.append(list(f(self.TIME)))
          FLT.TIME.append(FTIME)

        # Transpose FLT.MAPX and FLT.MAPY:
        FLT.MAPX = np.array(FLT.MAPX).T.tolist() 
        FLT.MAPY = np.array(FLT.MAPY).T.tolist() 
        FLT.TIME = np.array(FLT.TIME).T.tolist() 
      
      self.nfloat += 1
      self.FLOAT.append(FLT)
      self.FLOAT_INDX.set(self.nfloat-1)
      self.FLOAT_LIST = list(range(self.nfloat))

      self.nfiles += 1
      self.FILENAMES.append(FLT.FILENAME.get())
      self.FILETYPES.append('FLOAT')
      self.SEQUENCES.append(tk.BooleanVar(value=False))
      self.FILEORDER.append(self.nfloat-1)

      if self.first:
        # Set the plot limits according to the sata'''
        if self.drawmap is None:
          self.PLOT.WEST.set(np.nanmin(FLT.lon)-1)
          self.PLOT.EAST.set(np.nanmax(FLT.lon)+1)
          self.PLOT.SOUTH.set(np.nanmin(FLT.lat)-1)
          self.PLOT.NORTH.set(np.nanmax(FLT.lat)+1)
          self.plot_initialize()

        self.L.set(0)
        self.L_LIST = list(range(len(FLT.date)))
        self.NL = len(self.L_LIST)
        self.lbox.configure(state='!disabled')
        self.lbox['values'] = self.L_LIST
        if len(self.DATE) > 0:
          self.bnext.configure(state='normal')
        self.PLOT.TLABEL.set(self.DATE[self.L.get()])
        if len(self.DATE) > 1:
          self.bnext.configure(state='normal')

        self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
        self.first = False
        
      ii = self.FLOAT_INDX.get()
      _refill(ii)


    # Main window:
    # ============
    if self.Window_float is None:
      self.Window_float = tk.Toplevel(self.master)
      self.Window_float.title("Lagrangian Trajectories")
      self.Window_float.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_float.lift()

    if self.nfloat > 0:
      ii = self.FLOAT_INDX.get()
    else:
      ii = -1

    F0 = ttk.Frame(self.Window_float,padding=5)

    # Add
    ttk.Combobox(F0,textvariable=self.LSOURCE, \
                 values=self.FLOAT_OPTIONS).grid(row=0,column=0,padx=3) 
    ttk.Button(F0,text='Import',command=_add).grid(row=1,column=0,padx=3)

    # Filename:
    ttk.Label(F0,text='Float file').grid(row=0,column=1,padx=3)

    _wsel = ttk.Combobox(F0,textvariable=self.FLOAT_INDX, \
                                  values=self.FLOAT_LIST,width=5)
    _wsel.grid(row=0,column=2)
    _wsel.bind('<<ComboboxSelected>>',lambda e: _reget())
    _went = ttk.Entry(F0,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=3,columnspan=5,padx=3,sticky='w')

    # AAA
    if ii == -1:
      _wstat = ttk.Label(F0,text='',width=50,justify='left')
      _wsel.configure(state='disabled')
    else:
      _wstat = ttk.Label(F0,text=' Floats in the file= '+str(self.FLOAT[ii].nfloats),width=50,justify='left')
      _went['textvariable'] = self.FLOAT[ii].FILENAME

    _wstat.grid(row=1,column=3,columnspan=5,padx=3,sticky='w')

    F0.grid(row=0,column=0)

    F1 = ttk.Frame(self.Window_float,padding=5)
    if ii == -1:
      _show = ttk.Checkbutton(F1,text='Show')
    else:
      _show = ttk.Checkbutton(F1,text='Show',command=self.make_plot)
      _show['variable']=self.FLOAT[ii].SHOW
    _show.grid(row=1,column=5,padx=3)
    ttk.Button(F1,text='Cancel',command=_cancel).grid(row=1,column=6,padx=3)
    ttk.Button(F1,text='Clear',command=_clear).grid(row=1,column=7,padx=3)
    ttk.Button(F1,text='Plot',command=_close).grid(row=1,column=8,padx=3)
    F1.grid(row=1,column=0)


  # ========================
  def currents_config(self):
  # ========================

    global fshow

    if self.nvec == 0:
      messagebox.showinfo(message='No currents file opened yet')
      return
    #else:
    #  self.vector_config(self.VEC[self.VEC_INDX.get()].VEL)

    def _cancel():
    # ============
      self.Window_vectorconfig.destroy()
      self.Window_vectorconfig = None

    def _apply():
    # ============
      self.make_plot()

    def _done():
    # ============
      self.make_plot()
      self.Window_vectorconfig.destroy()
      self.Window_vectorconfig = None

    def _selected():
    # ==============
      global fshow
      fshow.destroy()
      # The usual configuration:
      ii = self.VEC_INDX.get()
      _went['textvariable'] = self.VEC[ii].FILENAME
  
      fshow = ttk.Frame(self.Window_vectorconfig,padding=10)
      vectorplot.Configuration(parent=fshow,
                               PLOT=self.VEC[ii].VEL.PLOT)

      f0 = ttk.Frame(fshow,padding=5)
      ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
          grid(row=0,column=0,padx=3)
      ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
          grid(row=0,column=1,padx=3)
      ttk.Button(f0,text='Done',command=_done,padding=5).     \
          grid(row=0,column=2,padx=3)
      f0.grid(sticky='ew',columnspan=3)
      fshow.grid()

    def _loadconf():
    # =============
      '''Load vector configuration'''
      print('Restoring vector configuration from ',
            self.VEC[ii].VEL.PLOT.FILECONF)
      try:
        self.VEC[ii].VEL.PLOT.load(self.VEC[ii].VEL.PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',
            self.VEC[ii].VEL.PLOT.FILECONF)

    def _saveconf():
    # =============
      '''Load vector configuration'''
      print('Saving vector configuration to ',
            self.VEC[ii].VEL.PLOT.FILECONF)
      try:
        self.VEC[ii].VEL.PLOT.save(self.VEC[ii].VEL.PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to write file ',
            self.VEC[ii].VEL.PLOT.FILECONF)

    def _loadfromconf():
    # ==================
      '''Load vector configuration from a file'''
      nn = filedialog.askopenfilename(title='Load vector configuration',
                                      parent=self.Window_vectorconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.VEC[ii].VEL.PLOT.FILECONF = '%s' % nn
      print('Restoring vector configuration from ',
            self.VEC[ii].VEL.PLOT.FILECONF)
      try:
        self.VEC[ii].VEL.PLOT.load(self.VEC[ii].VEL.PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',
             self.VEC[ii].VEL.PLOT.FILECONF)

    def _saveasconf():
    # ================
      '''Load vector configuration'''
      nn = filedialog.asksaveasfilename(title='Save vector configuration', 
                                        parent=self.Window_vectorconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      self.VEC[ii].VEL.PLOT.FILECONF = '%s' % nn
      print('Saving vector configuration to ',self.VEC[ii].VEL.PLOT.FILECONF)
      try:
        self.VEC[ii].VEL.PLOT.save(self.VEC[ii].VEL.PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',
             self.VEC[ii].VEL.PLOT.FILECONF)

    if self.Window_vectorconfig is not None:
      self.Window_vectorconfig.lift()
      return

    self.Window_vectorconfig = tk.Toplevel(self.master)
    self.Window_vectorconfig.title('Vector plot configuration')
    self.Window_vectorconfig.resizable(width=True,height=True)
    self.Window_vectorconfig.protocol('WM_DELETE_WINDOW',_cancel)

    menubar = tk.Menu(self.Window_vectorconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Restore from',command=_loadfromconf)
    menu.add_command(label='Save',command=_saveconf)
    menu.add_command(label='Save as',command=_saveasconf)
    try:
      self.Window_vectorconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      master.tk.call(self.Window_vectorconfig, "config", "-menu", menubar)

    fsel = ttk.Frame(self.Window_vectorconfig,padding=10)
    ttk.Label(fsel,text="File: ").grid(row=0,column=0,sticky='e',padx=3)
    _wsel = ttk.Combobox(fsel,textvariable=self.VEC_INDX,
                              values=self.VEC_LIST,width=5)
    _wsel.grid(row=0,column=1,sticky='w',padx=3)
    _wsel.bind('<<ComboboxSelected>>',lambda e:_selected())
    _went = ttk.Entry(fsel,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=2,columnspan=5,padx=3,sticky='w')
    _went = ttk.Entry(fsel,justify='left',width=80,state='readonly')
    _went.grid(row=0,column=2,columnspan=8,padx=3,sticky='w')
    fsel.grid()

    # The usual configuration:
    ii = self.VEC_INDX.get()
    _went['textvariable'] = self.VEC[ii].FILENAME

    fshow = ttk.Frame(self.Window_vectorconfig,padding=10)
    vectorplot.Configuration(parent=fshow,
                             PLOT=self.VEC[ii].VEL.PLOT)

    f0 = ttk.Frame(fshow,padding=5)
    ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
        grid(row=0,column=0,padx=3)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Done',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)
    fshow.grid()


  # ========================
  def saidin_config(self):
  # ========================

    if empty(self.SAIDIN.FILENAME.get()):
      messagebox.showinfo(message='No SST image opened yet')
      return

    def _apply():
    # ===========
      self.make_plot()

    def _done():
    # ==========
      self.make_plot()
      self.Window_saidinconfig.destroy()
      self.Window_saidinconfig = None

    def _loadconf():
    # =============
      '''Load contour configuration'''
      print('Restoring contour configuration from ',
            self.SAIDIN.FIELD.PLOT.FILECONF)
      try:
        self.SAIDIN.FIELD.PLOT.load(self.SAIDIN.FIELD.PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',
              self.SAIDIN.FIELD.PLOT.FILECONF)

    def _saveconf():
    # =============
      '''Load contour configuration'''
      print('Saving contour configuration to ',
            self.SAIDIN.FIELD.PLOT.FILECONF)
      try:
        self.SAIDIN.FIELD.PLOT.save(FF.PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',
            self.SAIDIN.FIELD.PLOT.FILECONF)

    def _loadfromconf():
    # ==================
      '''Load contour configuration from a file'''
      nn = filedialog.askopenfilename(title='Load contour configuration',
                                      parent=self.Window_saidinconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.SAIDIN.FIELD.PLOT.FILECONF = '%s' % nn
      print('Restoring contour configuration from ',
            self.SAIDIN.FIELD.PLOT.FILECONF)
      try:
        self.SAIDIN.FIELD.PLOT.load(self.SAIDIN.FIELD.PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',
              self.SAIDIN.FIELD.PLOT.FILECONF)

    def _saveasconf():
    # ================
      '''Load contour configuration'''
      nn = filedialog.asksaveasfilename(title='Save contour configuration',
                                        parent=self.Window_saidinconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      self.SAIDIN.FIELD.PLOT.FILECONF = '%s' % nn
      print('Saving contour configuration to ',
            self.SAIDIN.FIELD.PLOT.FILECONF)
      try:
        self.SAIDIN.FIELD.PLOT.save(self.SAIDIN.FIELD.PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',
            self.SAIDIN.FIELD.PLOT.FILECONF)

    if self.Window_saidinconfig is not None:
      self.Window_saidinconfig.lift()
      return

    self.Window_saidinconfig = tk.Toplevel(self.master)
    self.Window_saidinconfig.title('SST image configuration')
    self.Window_saidinconfig.resizable(width=True,height=True)
    self.Window_saidinconfig.protocol('WM_DELETE_WINDOW',_done)

    menubar = tk.Menu(self.Window_saidinconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Restore from',command=_loadfromconf)
    menu.add_command(label='Save',command=_saveconf)
    menu.add_command(label='Save as',command=_saveasconf)
    try:
      self.Window_saidinconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      master.tk.call(Window_saidinconfig, "config", "-menu", menubar)


    gshow = ttk.Frame(self.Window_saidinconfig,padding=10)

    contourplot.Configuration(parent=gshow,
                              varname=self.SAIDIN.FIELD.varname,
                              units=self.SAIDIN.FIELD.units,
                              missing=self.SAIDIN.FIELD.missing_value,
                              minval=self.SAIDIN.FIELD.minval,
                              maxval=self.SAIDIN.FIELD.maxval,
                              PLOT=self.SAIDIN.FIELD.PLOT)

    f0 = ttk.Frame(gshow,padding=5)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Close',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)
    gshow.grid()





  # ========================
  def contour_config(self):
  # ========================

    global gshow

    if self.ncdf == 0:
      messagebox.showinfo(message='No Netcdf file opened yet')
      return

    def _cancel():
    # ============
      self.Window_contourconfig.destroy()
      self.Window_contourconfig = None

    def _apply():
    # ===========
      self.make_plot()

    def _done():
    # ==========
      self.Window_contourconfig.destroy()
      self.Window_contourconfig = None
      self.make_plot()

    def _selected():
    # ==============
      global gshow
      gshow.destroy()
      # The usual configuration
      ii = self.CDF_INDX.get()
      _went['textvariable'] = self.CDF[ii].FILENAME

      gshow = ttk.Frame(self.Window_contourconfig,padding=10)
      contourplot.Configuration(parent=gshow,
                              varname=self.CDF[ii].FIELD.varname,
                              units=self.CDF[ii].FIELD.units,
                              missing=self.CDF[ii].FIELD.missing_value,
                              minval=self.CDF[ii].FIELD.minval,
                              maxval=self.CDF[ii].FIELD.maxval,
                              PLOT=self.CDF[ii].FIELD.PLOT)

      f0 = ttk.Frame(gshow,padding=5)
      ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
          grid(row=0,column=0,padx=3)
      ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
          grid(row=0,column=1,padx=3)
      ttk.Button(f0,text='Done',command=_done,padding=5).     \
            grid(row=0,column=2,padx=3)
      f0.grid(sticky='ew',columnspan=3)
      gshow.grid()


    def _loadconf():
    # =============
      '''Load contour configuration'''
      print('Restoring contour configuration from ',
            self.CDF[ii].FIELD.PLOT.FILECONF)
      try:
        self.CDF[ii].FIELD.PLOT.load(self.CDF[ii].FIELD.PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',
              self.CDF[ii].FIELD.PLOT.FILECONF)

    def _saveconf():
    # =============
      '''Load contour configuration'''
      print('Saving contour configuration to ',
            self.CDF[ii].FIELD.PLOT.FILECONF)
      try:
        self.CDF[ii].FIELD.PLOT.save(FF.PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',
            self.CDF[ii].FIELD.PLOT.FILECONF)

    def _loadfromconf():
    # ==================
      '''Load contour configuration from a file'''
      nn = filedialog.askopenfilename(title='Load contour configuration',
                                      parent=self.Window_contourconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.CDF[ii].FIELD.PLOT.FILECONF = '%s' % nn
      print('Restoring contour configuration from ',
            self.CDF[ii].FIELD.PLOT.FILECONF)
      try:
        self.CDF[ii].FIELD.PLOT.load(self.CDF[ii].FIELD.PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',
              self.CDF[ii].FIELD.PLOT.FILECONF)

    def _saveasconf():
    # ================
      '''Load contour configuration'''
      nn = filedialog.asksaveasfilename(title='Save contour configuration',
                                        parent=self.Window_contourconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      
      self.CDF[ii].FIELD.PLOT.FILECONF = '%s' % nn
      print('Saving contour configuration to ',
            self.CDF[ii].FIELD.PLOT.FILECONF)
      try:
        self.CDF[ii].FIELD.PLOT.save(self.CDF[ii].FIELD.PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',
            self.CDF[ii].FIELD.PLOT.FILECONF)

    if self.Window_contourconfig is not None:
      self.Window_contourconfig.lift()
      return

    self.Window_contourconfig = tk.Toplevel(self.master)
    self.Window_contourconfig.title('Contour plot configuration')
    self.Window_contourconfig.resizable(width=True,height=True)
    self.Window_contourconfig.protocol('WM_DELETE_WINDOW',_cancel)

    menubar = tk.Menu(self.Window_contourconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Restore from',command=_loadfromconf)
    menu.add_command(label='Save',command=_saveconf)
    menu.add_command(label='Save as',command=_saveasconf)
    try:
      self.Window_contourconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      master.tk.call(Window_contourconfig, "config", "-menu", menubar)


    fsel = ttk.Frame(self.Window_contourconfig,padding=10)
    ttk.Label(fsel,text="File: ").grid(row=0,column=0,sticky='e',padx=3)
    _wsel = ttk.Combobox(fsel,textvariable=self.CDF_INDX,
                              values=self.CDF_LIST,width=5)
    _wsel.grid(row=0,column=1,sticky='w',padx=3)
    _wsel.bind('<<ComboboxSelected>>',lambda e:_selected())
    _went = ttk.Entry(fsel,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=2,columnspan=5,padx=3,sticky='w')
    _went = ttk.Entry(fsel,justify='left',width=80,state='readonly')
    _went.grid(row=0,column=2,columnspan=8,padx=3,sticky='w')
    fsel.grid()

    # The usual configuration:
    ii = self.CDF_INDX.get()
    _went ['textvariable'] = self.CDF[ii].FILENAME

    gshow = ttk.Frame(self.Window_contourconfig,padding=10)

    contourplot.Configuration(parent=gshow,
                              varname=self.CDF[ii].FIELD.varname,
                              units=self.CDF[ii].FIELD.units,
                              missing=self.CDF[ii].FIELD.missing_value,
                              minval=self.CDF[ii].FIELD.minval,
                              maxval=self.CDF[ii].FIELD.maxval,
                              PLOT=self.CDF[ii].FIELD.PLOT)

    f0 = ttk.Frame(gshow,padding=5)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Close',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)
    gshow.grid()

  # ==========================
  def lagrangian_config(self):
  # ==========================

    global hshow

    if self.nfloat == 0:
      messagebox.showinfo(message='No Trajectory file opened yet')
      return

    def _cancel():
    # ============
      self.Window_lineconfig.destroy()
      self.Window_lineconfig = None

    def _apply():
    # ============
      self.make_plot()

    def _done():
    # ============
      self.make_plot()
      self.Window_lineconfig.destroy()
      self.Window_lineconfig = None

    def _selected():
    # ==============
      global hshow
      hshow.destroy()
      # The usual configuration:
      ii = self.FLOAT_INDX.get()
      _went['textvariable'] = self.FLOAT[ii].FILENAME

      hshow = ttk.Frame(self.Window_lineconfig,padding=10)

      # Define tabs:
      nb = ttk.Notebook(hshow)
      page1 = ttk.Frame(nb)
      page2 = ttk.Frame(nb)
      page3 = ttk.Frame(nb)
      nb.add(page1,text='Line Configuration')
      nb.add(page2,text='Trajectory options')
      nb.add(page3,text='Trajectory data')
      nb.grid()

      # Page 1
      #lineplot.WinConfig(self.Window_lineconfig,LL)
      lineplot.Configuration(page1,self.FLOAT[ii].PLOT)

      # Page 2
      lineplot.Configuration_OnMap(page2,self.FLOAT[ii].PLOT,self.FLOAT[ii])

      # Page 3
      lagrangian.ShowData(page3,self.FLOAT[ii])

      f0 = ttk.Frame(hshow,padding=5)
      ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
          grid(row=0,column=0,padx=3)
      ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
          grid(row=0,column=1,padx=3)
      ttk.Button(f0,text='Done',command=_done,padding=5).     \
          grid(row=0,column=2,padx=3)
      f0.grid(sticky='ew',columnspan=3)
      hshow.grid()


    def _loadconf():
    # =============
      '''Load line configuration'''
      print('Restoring line configuration from ',
            self.FLOAT[ii].PLOT.FILECONF)
      try:
        self.FLOAT[ii].PLOT.load(self.FLOAT[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',self.FLOAT[ii].PLOT.FILECONF)

    def _saveconf():
    # =============
      '''Load line configuration'''
      print('Saving line configuration to ',
            self.FLOAT[ii].PLOT.FILECONF)
      try:
        self.FLOAT[ii].PLOT.save(self.FLOAT[ii].PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',self.FLOAT[ii].PLOT.FILECONF)

    def _loadfromconf():
    # ==================
      '''Load line configuration from a file'''
      nn = filedialog.askopenfilename(title='Load line configuration',
                                      parent=self.Window_lineconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.FLOAT[ii].PLOT.FILECONF = '%s' % nn
      print('Restoring line configuration from ',
            self.FLOAT[ii].PLOT.FILECONF)
      try:
        self.FLOAT[ii].PLOT.load(self.FLOAT[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        print('Error: Unable to load file ',
              self.FLOAT[ii].PLOT.FILECONF)

    def _saveasconf():
    # ================
      '''Load line configuration'''
      nn = filedialog.asksaveasfilename(title='Save line configuration', 
                                        parent=self.Window_lineconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      self.FLOAT[ii].PLOT.FILECONF = '%s' % nn
      print('Saving line configuration to ',self.FLOAT[ii].PLOT.FILECONF)
      try:
        self.FLOAT[ii].PLOT.save(self.FLOAT[ii].PLOT.FILECONF)
      except:
        print('Error: Unable to write file ',self.FLOAT[ii].PLOT.FILECONF)


    if self.Window_lineconfig is not None:
      self.Window_lineconfig.lift()
      return

    self.Window_lineconfig = tk.Toplevel(self.master)
    self.Window_lineconfig.title('Trajectory plot configuration')
    self.Window_lineconfig.resizable(width=False,height=False)
    self.Window_lineconfig.protocol('WM_DELETE_WINDOW',_cancel)

    menubar = tk.Menu(self.Window_lineconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Restore from',command=_loadfromconf)
    menu.add_command(label='Save',command=_saveconf)
    menu.add_command(label='Save as',command=_saveasconf)
    try:
      self.Window_lineconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      master.tk.call(Window_lineconfig, "config", "-menu", menubar)

    fsel = ttk.Frame(self.Window_lineconfig,padding=10)
    ttk.Label(fsel,text="File: ").grid(row=0,column=0,sticky='e',padx=3)
    _wsel = ttk.Combobox(fsel,textvariable=self.FLOAT_INDX,
                              values=self.FLOAT_LIST,width=5)
    _wsel.grid(row=0,column=1,sticky='w',padx=3)
    _wsel.bind('<<ComboboxSelected>>',lambda e:_selected())
    _went = ttk.Entry(fsel,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=2,columnspan=5,padx=3,sticky='w')
    _went = ttk.Entry(fsel,justify='left',width=80,state='readonly')
    _went.grid(row=0,column=2,columnspan=8,padx=3,sticky='w')
    fsel.grid()

    # The usual configuration
    ii = self.FLOAT_INDX.get()
    _went['textvariable'] = self.FLOAT[ii].FILENAME

    hshow = ttk.Frame(self.Window_lineconfig,padding=10)

    # Define tabs:
    nb = ttk.Notebook(hshow)
    page1 = ttk.Frame(nb)
    page2 = ttk.Frame(nb)
    page3 = ttk.Frame(nb)
    nb.add(page1,text='Line Configuration')
    nb.add(page2,text='Trajectory options')
    nb.add(page3,text='Trajectory data')
    nb.grid()

    # Page 1
    #lineplot.WinConfig(self.Window_lineconfig,LL)
    lineplot.Configuration(page1,self.FLOAT[ii].PLOT)

    # Page 2
    lineplot.Configuration_OnMap(page2,self.FLOAT[ii].PLOT,self.FLOAT[ii])

    # Page 3
    lagrangian.ShowData(page3,self.FLOAT[ii])

    f0 = ttk.Frame(hshow,padding=5)
    ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
        grid(row=0,column=0,padx=3)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Done',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)
    hshow.grid()



  # ===================
  def lselection(self):
  # ===================
    '''Sets all files in the SEQUENCE list to the same time step'''

    self.L.set(int(self.lbox.get()))
    self.PLOT.TLABEL.set(self.DATE[self.L.get()])
    L = self.L.get()
    if L == 0:
      self.bprev.configure(state='disabled')
    else:
      self.bprev.configure(state='normal')
    if L == self.NL - 1:
      self.bnext.configure(state='disabled')
    else:
      self.bnext.configure(state='normal')

    for i in range(self.nfiles):
      if self.SEQUENCES[i].get():
        if self.FILETYPES[i] == 'VEC':
          self.VEC[self.FILEORDER[i]].L.set(L)
          self.read_UV(self.VEC[self.FILEORDER[i]])
        elif self.FILETYPES[i] == 'FLD':
          self.CDF[self.FILEORDER[i]].L.set(L)
          self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
    self.make_plot()


  # ===================================
  def tprev(self):
  # ===================================
    '''Points to the previous time step'''
    if self.L.get() > 0:
      self.L.set(self.L.get() - 1)
      self.PLOT.TLABEL.set(self.DATE[self.L.get()])
      if self.L.get() == 0:
        self.bprev.configure(state='disabled')
      if self.L.get() < self.NL - 1:
        self.bnext.configure(state='normal')
      for i in range(self.nfiles):
        if self.SEQUENCES[i].get():
          if self.FILETYPES[i] == 'VEC':
            L  = self.VEC[self.FILEORDER[i]].L.get()
            Lm = self.VEC[self.FILEORDER[i]].L.get() - 1
            self.VEC[self.FILEORDER[i]].L.set(Lm)
            self.read_UV(self.VEC[self.FILEORDER[i]])
          elif self.FILETYPES[i] == 'FLD':
            L  = self.CDF[self.FILEORDER[i]].L.get()
            Lm = self.CDF[self.FILEORDER[i]].L.get() - 1
            self.CDF[self.FILEORDER[i]].L.set(Lm)
            self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
      self.make_plot()
    else:
      return


  # ================================
  def tnext(self):
  # ================================
    '''Points to the next time step'''
    
    if self.L.get() < self.NL - 1:
      self.L.set(self.L.get() + 1)

      self.PLOT.TLABEL.set(self.DATE[self.L.get()])
      if self.L.get() == self.NL - 1:
        self.bnext.configure(state='disabled')
      if self.L.get() > 0:
        self.bprev.configure(state='normal')

      for i in range(self.nfiles):
        if self.SEQUENCES[i].get():
          if self.FILETYPES[i] == 'VEC':
            L  = self.VEC[self.FILEORDER[i]].L.get()
            Lp = self.VEC[self.FILEORDER[i]].L.get() + 1
            self.VEC[self.FILEORDER[i]].L.set(Lp)
            self.read_UV(self.VEC[self.FILEORDER[i]])
          elif self.FILETYPES[i] == 'FLD':
            L  = self.CDF[self.FILEORDER[i]].L.get()
            Lp = self.CDF[self.FILEORDER[i]].L.get() + 1
            self.CDF[self.FILEORDER[i]].L.set(Lp)
            self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
      self.make_plot()
    else:
      return


  # ======================================
  def data_update(self):
  # ======================================
    '''Makes the new plot according to the user selections. It call self.read to get the new data'''
    self.read_UV(self.FLD.ncid,self.FLD.icdf,self.FLD.uid,self.FLD.vid)
    self.read_S(self.FLD.ncid,self.FLD.icdf,self.FLD.sid)
    self.make_plot()


  # ============================
  def get_date(self,ncid,icdf):
  # ============================
 
    self.T_LIST = []
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


  # ==================================
  def plot_initialize(self):
  # ==================================

    # Meridian and parallel range and intervalls:
    tmp1 = np.trunc(100*(self.PLOT.EAST.get()-self.PLOT.WEST.get())/4)/100
    if tmp1 > 1:
      tmp1 = np.rint(tmp1)
    self.PLOT.MERIDIAN_INT.set(tmp1)

    self.PLOT.MERIDIAN_INI.set(np.trunc(self.PLOT.WEST.get()/tmp1 - 1)*tmp1)
    self.PLOT.MERIDIAN_FIN.set(np.trunc(self.PLOT.EAST.get()/tmp1 + 1)*tmp1)
    tmp1 = None

    tmp2 = np.trunc(100*(self.PLOT.NORTH.get() - self.PLOT.SOUTH.get())/4)/100
    if tmp2 > 1:
      tmp2 = np.rint(tmp2)
    self.PLOT.PARALLEL_INT.set(tmp2)
    self.PLOT.PARALLEL_INI.set(np.trunc(self.PLOT.SOUTH.get()/tmp2 -
1)*tmp2)
    self.PLOT.PARALLEL_FIN.set(np.trunc(self.PLOT.NORTH.get()/tmp2 + 1)*tmp2)
    tmp2 = None


  def on_resizing_event(self,canvas):
    ''' Update PLOT.SIZE variable according to the window size'''
    self.PLOT.SIZE = list(self.fig.get_size_inches())

  # ==================
  def make_plot(self):
  # ==================

    if self.PLOT.OUTPUT_FIGURE.get():
      if self.fig is None:
        self.fig = plt.figure('COSMO-VIEW canvas',    
                              figsize=self.PLOT.SIZE, 
                              facecolor=self.PLOT.FIGURE_COLOR.get(),
                              dpi=self.PLOT.DPI.get())
        self.fig.canvas.mpl_connect('close_event',self.on_closing_figure)
        self.fig.canvas.mpl_connect('resize_event',self.on_resizing_event)
        self.fig.canvas.callbacks.connect('button_press_event',self.on_click)
        self.ax = self.fig.add_subplot(111)
        #self.ax.set_Frame_on(False)
        self.ax.axis('off')
        self.drawmap = True
      self.draw_figure()

  # ====================
  def setmap(self,ax,target=0):
  # ====================
    '''Routine focused to set the Basemap projection'''

    def cyl():
      if target == 0:
        self.m = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)
      else:
        self.Mm = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)

    def epsg():
      if target == 0:
        self.m = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       epsg=self.PLOT.EPSG.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)
      else:
        self.Mm = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       epsg=self.PLOT.EPSG.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)

    def geos():
      if target == 0:
        self.m = Basemap(llcrnrlat=SOUTH,
                       urcrnrlat=NORTH,
                       llcrnrlon=WEST,
                       urcrnrlon=EAST,
                       lon_0=self.PLOT.LON_0.get(),
                       satellite_height=self.PLOT.SATELLITE_HEIGHT.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(), \
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)
      else:
        self.Mm = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       satellite_height=self.PLOT.SATELLITE_HEIGHT.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(), \
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)

    def gnom():
      if target == 0:
        self.m = Basemap(width=self.PLOT.WIDTH.get(),
                       height=self.PLOT.HEIGHT.get(),
                       lat_0=self.PLOT.LAT_0.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(), \
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)
      else:
        self.Mm = Basemap(width=self.PLOT.WIDTH.get(),
                       height=self.PLOT.HEIGHT.get(),
                       lat_0=self.PLOT.LAT_0.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(), \
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       ax=ax)

    def merc():
      lat_ts = 0.5*(self.PLOT.SOUTH.get()+self.PLOT.NORTH.get())
      if target == 0:
        self.m = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lat_ts=lat_ts,
                       ax=ax)
      else:
        self.Mm = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lat_ts=lat_ts,
                       ax=ax)

    def moll():
      if target == 0:
        self.m = Basemap(projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       ax=ax)
      else:
        self.Mm = Basemap(projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       ax=ax)

    def ortho():
      if target == 0:
        self.m = Basemap(projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lat_0=self.PLOT.LAT_0.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       ax=ax)
      else:
        self.Mm = Basemap(projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lat_0=self.PLOT.LAT_0.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       ax=ax)

    def tmerc():
      if target == 0:
        self.m = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lat_0=self.PLOT.LAT_0.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       ax=ax)
      else:
        self.Mm = Basemap(llcrnrlat=self.PLOT.SOUTH.get(),
                       urcrnrlat=self.PLOT.NORTH.get(),
                       llcrnrlon=self.PLOT.WEST.get(),
                       urcrnrlon=self.PLOT.EAST.get(),
                       projection=self.PLOT.MAP_PROJECTION.get(),
                       resolution=self.PLOT.MAP_RESOLUTION.get(),
                       lat_0=self.PLOT.LAT_0.get(),
                       lon_0=self.PLOT.LON_0.get(),
                       ax=ax)

    try:
      SOUTH = float(self.PLOT.SOUTH.get())
    except:
      SOUTH = None

    try:
      NORTH = float(self.PLOT.NORTH.get())
    except:
      NORTH = None

    try:
      WEST  = float(self.PLOT.WEST.get())
    except:
      WEST = None

    try:
      EAST  = float(self.PLOT.EAST.get())
    except:
      EAST  = None

    options = {'aeqd' : gnom,
               'cass' : tmerc,
               'cea'  : cyl,
               'cyl'  : cyl,
               'gall' : cyl,
               'epsg' : epsg,
               'geos' : geos,
               'gnom' : gnom,
               'merc' : merc,
               'moll' : moll,
               'ortho': ortho,
               'tmerc': tmerc,
               }

    projection = self.PLOT.MAP_PROJECTION.get()
    options[projection]()



  # ====================
  def draw_figure(self):
  # ====================


    try:
      self.scbar.remove()
    except:
      pass

    for bar in self.cdfbar:
      try:
         bar.remove()
      except:
        pass
    self.cdfbar = []

    self.ax.clear()



    # EPSG
    epsg = int(self.PLOT.EPSG.get())

    try:
      SOUTH = float(self.PLOT.SOUTH.get())
    except:
      SOUTH = None

    try:
      NORTH = float(self.PLOT.NORTH.get())
    except:
      NORTH = None

    try:
      WEST  = float(self.PLOT.WEST.get())
    except:
      WEST = None

    try:
      EAST  = float(self.PLOT.EAST.get())
    except:
      EAST  = None

    if self.PLOT.GEOMAP.get() == False:
      '''Plotting maps without MATPLOTLIB'''

      self.m = None
    else:
      '''Plotting the maps using MATPLOTLIB utilities'''

      if self.drawmap:
        self.setmap(self.ax,0)
        self.drawmap = False

      if self.PLOT.RELIEF.get():
        self.m.shadedrelief(scale=self.PLOT.BACKGROUND_SCALE.get())

      if self.PLOT.BLUEMARBLE.get():
        self.m.bluemarble(scale=self.PLOT.BACKGROUND_SCALE.get())

      if self.PLOT.ETOPO.get():
        self.m.etopo(scale=self.PLOT.BACKGROUND_SCALE.get())

      if self.PLOT.ARCGISIMAGE.get() == 1:
        self.m.arcgisimage(service=self.PLOT.ARCGISSERVICE.get(), \
                         xpixels=self.PLOT.ARCGISPIXELS.get(),  \
                         dpi=self.PLOT.ARCGISDPI.get(),         \
                         epsg=epsg,                             \
                         verbose=self.PLOT.ARCGISVERBOSE.get())

 
    # Draw SAIDIN:
    # 
    if not empty(self.SAIDIN.FILENAME.get()):
      if self.SAIDIN.FIELD.show.get():
        self.scbar = contourplot.drawing(self.fig,self.ax,self.m,
                       self.SAIDIN.xx,self.SAIDIN.yy,self.SAIDIN.FIELD.data,
                       self.SAIDIN.FIELD.mask,
                       self.SAIDIN.FIELD.PLOT)

    # Draw fields:
    # 
    if self.ncdf > 0:
      for ii in range(self.ncdf):
        if self.CDF[ii].FIELD.show.get():
          self.cdfbar.append (contourplot.drawing(self.fig,self.ax,self.m,
                                  self.CDF[ii].xx,
                                  self.CDF[ii].yy,
                                  self.CDF[ii].FIELD.data,
                                  self.CDF[ii].FIELD.mask,
                                  self.CDF[ii].FIELD.PLOT))

    # Draw currents:
    #
    if self.nvec > 0:
      for ii in range(self.nvec):
        if self.VEC[ii].VEL.show.get():
          vectorplot.drawing (self.fig,self.ax,self.m,self.VEC[ii])

    # Draw floats:
    #
    if self.nfloat > 0:
      for ii in range(self.nfloat):
        self.FLOAT[ii].L.set(self.L.get())
        lagrangian.drawing(self.fig,self.ax,self.m,self.FLOAT[ii])

    # Draw markers:
    #
    if self.nmarker > 0:
      for ii in range(self.nmarker):
        geomarker.drawing(self.fig,self.ax,self.m,self.MARKER[ii])


    if self.PLOT.GEOMAP.get():
      '''Plotting maps without MATPLOTLIB'''
      if self.PLOT.COASTLINE_SHOW.get():
        self.m.drawcoastlines(linewidth=self.PLOT.COASTLINE_WIDTH.get(), \
                             color=self.PLOT.COASTLINE_COLOR.get())
 

      if self.PLOT.COUNTRYLINE_SHOW.get():
        self.m.drawcountries(linewidth=self.PLOT.COUNTRYLINE_WIDTH.get(), \
                            color=self.PLOT.COUNTRYLINE_COLOR.get())

      if self.PLOT.ISOBAT_NPLOT > 0:
        # Plot isobaths and its legend:
        lines = []
        labels = []
        for ii in range(self.PLOT.nisobat):
          if self.PLOT.ISOBAT_LABEL_SHOW.get():
            label = self.PLOT.ISOBAT_LABEL[ii]
          else:
            label = None
          try:
            color = eval(self.PLOT.ISOBAT_COLOR[ii].get())
          except:
            color = self.PLOT.ISOBAT_COLOR[ii].get()
          if self.PLOT.ISOBAT_SHOW[ii]:
            z = self.PLOT.ISOBAT_DATA[ii]
            ilon = z['lon']
            ilat = z['lat']
            isox,isoy = self.m(ilon,ilat)
            for i in range(len(isox)):
              if isox[i] > 1e29:
                isox[i] = np.nan
                isoy[i] = np.nan
            isbt, = self.m.plot(isox,isoy,
                                marker=None, 
                                linestyle=self.PLOT.ISOBAT_STYLE[ii].get(),
                                linewidth=self.PLOT.ISOBAT_WIDTH[ii].get(),
                                color=color)
            lines.append(isbt)
            labels.append(label)

            if self.PLOT.ISOBAT_LEGEND.SHOW.get():
              if self.PLOT.ISOBAT_LEGEND.FONTSIZE.get() < 1:
                fontsize = None
              else:
                fontsize = self.PLOT.ISOBAT_LEGEND.FONTSIZE.get()
              if self.PLOT.ISOBAT_LEGEND.MODE.get() == 1:
                mode = 'expand'
              else:
                mode = None
  
              self.Ilegend = plt.legend([lines[i] for i in range(len(lines))],
                          [labels[i] for i in range(len(lines))],
                          loc=self.PLOT.ISOBAT_LEGEND.LOC.get(), 
                          ncol=self.PLOT.ISOBAT_LEGEND.NCOL.get(),
                          fontsize=fontsize,
                          frameon=self.PLOT.ISOBAT_LEGEND.FRAMEON.get(),
                          fancybox=self.PLOT.ISOBAT_LEGEND.FANCYBOX.get(),
                          shadow=self.PLOT.ISOBAT_LEGEND.SHADOW.get(),
                          framealpha=self.PLOT.ISOBAT_LEGEND.ALPHA.get(),
                          mode=mode,
                          facecolor=self.PLOT.ISOBAT_LEGEND.COLOR.get(),
                          edgecolor=self.PLOT.ISOBAT_LEGEND.EDGECOLOR.get(),
                          markerscale=self.PLOT.ISOBAT_LEGEND.MARKERSCALE.get(),
                          borderpad=self.PLOT.ISOBAT_LEGEND.BORDERPAD.get(),
                          handletextpad=self.PLOT.ISOBAT_LEGEND.HANDLETEXTPAD.get(),
                          borderaxespad=self.PLOT.ISOBAT_LEGEND.BORDERAXESPAD.get(),
                          labelspacing=self.PLOT.ISOBAT_LEGEND.LABELSPACING.get(),
                               )

              if not empty(self.PLOT.ISOBAT_LEGEND.TITLE.get()):
                try:
                  self.Ilegend.set_title(self.PLOT.ISOBAT_LEGEND.TITLE.get(),
                                   prop=self.PLOT.ISOBAT_LEGEND.TITLEFONT)
                except:
                  pass

            #self.ax.add_artist(self.Ilegend)



      if self.PLOT.WATER_COLOR.get() is not 'None':
        self.m.drawmapboundary(fill_color=self.PLOT.WATER_COLOR.get())

      if self.PLOT.LAND_COLOR.get() is not 'None':
        self.m.fillcontinents(color=self.PLOT.LAND_COLOR.get())

      if self.PLOT.RIVERS_SHOW.get():
        self.m.drawrivers(linewidth=self.PLOT.RIVERS_WIDTH.get(), \
                          color=self.PLOT.RIVERS_COLOR.get())



    if self.PLOT.GRID_SHOW.get():

      def setcolor(x,color):
        for m in x:
          for t in x[m][1]:
            t.set_color(color)

      vmeridians = np.arange(self.PLOT.MERIDIAN_INI.get(), \
                             self.PLOT.MERIDIAN_FIN.get(), \
                             self.PLOT.MERIDIAN_INT.get())
      vparallels = np.arange(self.PLOT.PARALLEL_INI.get(), \
                             self.PLOT.PARALLEL_FIN.get(), \
                             self.PLOT.PARALLEL_INT.get())

      if self.PLOT.GEOMAP.get():
        par = self.m.drawmeridians(vmeridians,                            \
                        labels=[self.PLOT.GRID_WEST.get(),        \
                                self.PLOT.GRID_EAST.get(),        \
                                self.PLOT.GRID_NORTH.get(),       \
                                self.PLOT.GRID_SOUTH.get()],      \
                        fontsize=self.PLOT.GRID_SIZE.get(),       \
                        linewidth=self.PLOT.GRID_LINEWIDTH.get(), \
                        color=self.PLOT.GRID_COLOR.get())
        setcolor(par,self.PLOT.GRID_FONTCOLOR.get())

        mer = self.m.drawparallels(vparallels,                            \
                        labels=[self.PLOT.GRID_WEST.get(),        \
                                self.PLOT.GRID_EAST.get(),        \
                                self.PLOT.GRID_NORTH.get(),       \
                                self.PLOT.GRID_SOUTH.get()],      \
                        fontsize=self.PLOT.GRID_SIZE.get(),       \
                        linewidth=self.PLOT.GRID_LINEWIDTH.get(), \
                        color=self.PLOT.GRID_COLOR.get())
        setcolor(mer,self.PLOT.GRID_FONTCOLOR.get())

        # Modify the line characteristics:
        lat_lines = chain(*(tup[1][0] for tup in mer.items()))
        lon_lines = chain(*(tup[1][0] for tup in par.items()))
        all_lines = chain(lat_lines,lon_lines)

        # cylce through these lines and set the desired style
        for line in all_lines:
          line.set(linestyle=self.PLOT.GRID_LINESTYLE.get(),
                   alpha=self.PLOT.GRID_ALPHA.get())

      else:

        self.ax.grid(True)

        self.ax.set_xticks(vmeridians)
        self.ax.set_yticks(vparallels)
        plt.rc('grid',alpha=self.PLOT.GRID_ALPHA.get())
        plt.rc('grid',color=colors.to_rgba(self.PLOT.GRID_COLOR.get()))
        plt.rc('grid',linewidth= self.PLOT.GRID_LINEWIDTH.get())
        plt.rc('grid',linestyle=self.PLOT.GRID_LINESTYLE.get())
        plt.rc('xtick',color=self.PLOT.GRID_FONTCOLOR.get())
        plt.rc('xtick',labelsize=self.PLOT.GRID_SIZE.get())
        plt.rc('ytick',color=self.PLOT.GRID_FONTCOLOR.get())
        plt.rc('ytick',labelsize=self.PLOT.GRID_SIZE.get())




    if self.PLOT.GEOMAP.get():
      if self.PLOT.SCALE_SHOW.get():
          try:
            YOFFSET = float(self.PLOT.SCALE_YOFFSET.get())
          except:
            YOFFSET = None
              
          try:
            LINEWIDTH = float(self.PLOT.SCALE_LINEWIDTH.get())
          except:
            LINEWIDTH = None

          self.m.drawmapscale(self.PLOT.SCALE_X.get(),
                              self.PLOT.SCALE_Y.get(),
                              self.PLOT.SCALE_XO.get(),
                              self.PLOT.SCALE_YO.get(),
                              length=self.PLOT.SCALE_LENGTH.get(),
                              units=self.PLOT.SCALE_UNITS.get(),
                              barstyle=self.PLOT.SCALE_STYLE.get(),
                              fontsize=self.PLOT.SCALE_FONTSIZE.get(),
                              yoffset=YOFFSET, 
                              labelstyle=self.PLOT.SCALE_LABELSTYLE.get(),
                              fontcolor=self.PLOT.SCALE_FONTCOLOR.get(),
                              fillcolor1=self.PLOT.SCALE_FILLCOLOR1.get(),
                              fillcolor2=self.PLOT.SCALE_FILLCOLOR2.get(),
                              format=self.PLOT.SCALE_FORMAT.get(),
                              linecolor=self.PLOT.SCALE_LINECOLOR.get(),
                              linewidth=LINEWIDTH)


    # Lables and titles:
    font_family = self.PLOT.MAP_FONT_TYPE.get()

    font_size   = self.PLOT.LABEL_SIZE.get()
    font_weight = 'normal'

    font = {'family' : font_family,
            'weight' : font_weight,
            'color'  : self.PLOT.TEXT_COLOR.get(),
            'size'   : font_size}

    self.ax.xaxis.labelpad = self.PLOT.LABEL_PAD.get()
    self.ax.yaxis.labelpad = self.PLOT.LABEL_PAD.get()
    self.ax.set_xlabel(self.PLOT.XLABEL.get(),fontdict=font)
    self.ax.set_ylabel(self.PLOT.YLABEL.get(),fontdict=font)

    self.ax.set_title(self.PLOT.TITLE.get(),
                      fontproperties=self.PLOT.TITLEFONT)
    px,py = self.ax.title.get_position()
    dy = self.PLOT.TITLE_PAD.get()/self.fig.get_dpi()
    self.ax.title.set_position((px,py+dy))

    # Time stamp
    try:
      self.time_stamp.remove()
    except:
      pass

    if len(self.DATE) > 0:
      if self.PLOT.TIMESTAMP_SHOW.get():
        if self.PLOT.TIMESTAMP_BOLD.get():
          font_weight = 'bold'
        else:
          font_weight = 'normal'
        font = {'family' : font_family,
                'weight' : font_weight,
                'color'  : self.PLOT.TIMESTAMP_COLOR.get(),
                'size'   : self.PLOT.TIMESTAMP_SIZE.get()}

        self.time_stamp = self.fig.text(self.PLOT.TIMESTAMP_X.get(),     \
                                      self.PLOT.TIMESTAMP_Y.get(),     \
                                      self.DATE[self.L.get()], \
                                      fontdict=font)

    if self.PLOT.LOGO_DISPLAY.get() == 1:
      self.plot_logo()

    self.ax.callbacks.connect('xlim_changed', self.on_xlims_change)
    self.ax.callbacks.connect('ylim_changed', self.on_ylims_change)

    if self.nmarker > 0 and self.PLOT.LEGEND.SHOW.get():
      if self.PLOT.LEGEND.FONTSIZE.get() < 1:
        fontsize = None
      else:
        fontsize = self.PLOT.LEGEND.FONTSIZE.get()
      if self.PLOT.LEGEND.MODE.get() == 1:
        mode = 'expand'
      else:
        mode = None

      try:
        legend = self.ax.legend(loc=self.PLOT.LEGEND.LOC.get(), 
                        ncol=self.PLOT.LEGEND.NCOL.get(),
                        fontsize=fontsize,
                        frameon=self.PLOT.LEGEND.FRAMEON.get(),
                        fancybox=self.PLOT.LEGEND.FANCYBOX.get(),
                        shadow=self.PLOT.LEGEND.SHADOW.get(),
                        framealpha=self.PLOT.LEGEND.ALPHA.get(),
                        mode=mode,
                        facecolor=self.PLOT.LEGEND.COLOR.get(),
                        edgecolor=self.PLOT.LEGEND.EDGECOLOR.get(),
                        markerscale=self.PLOT.LEGEND.MARKERSCALE.get(),
                        borderpad=self.PLOT.LEGEND.BORDERPAD.get(),
                        handletextpad=self.PLOT.LEGEND.HANDLETEXTPAD.get(),
                        borderaxespad=self.PLOT.LEGEND.BORDERAXESPAD.get(),
                        labelspacing=self.PLOT.LEGEND.LABELSPACING.get(),
                               )

      except:
        pass

      if not empty(self.PLOT.LEGEND.TITLE.get()):
        try:
          legend.set_title(self.PLOT.LEGEND.TITLE.get(),
                           prop=self.PLOT.LEGEND.TITLEFONT)
        except:
          pass


    self.fig.show()


  # ====================
  def make_Mplot(self):
  # ====================
    '''Plotting the maps using MATPLOTLIB utilities, 
       output directed to Movie window'''

    try:
      self.SAIDIN.Mcbar.remove()
    except:
      pass

    try:
      self.Mscbar.remove()
    except:
      pass

    for bar in self.Mcdfbar:
      try:
         bar.remove()
      except:
        pass
    self.Mcdfbar = []

    self.Max.clear()

    # EPSG
    epsg = int(self.PLOT.EPSG.get())

    SOUTH = float(self.PLOT.SOUTH.get())
    NORTH = float(self.PLOT.NORTH.get())
    WEST  = float(self.PLOT.WEST.get())
    EAST  = float(self.PLOT.EAST.get())

    if self.Mdrawmap:
      self.setmap(self.Max,1)
      self.Mdrawmap = False

    if self.PLOT.RELIEF.get():
      self.Mm.shadedrelief(scale=self.PLOT.BACKGROUND_SCALE.get())

    if self.PLOT.BLUEMARBLE.get():
      self.Mm.bluemarble(scale=self.PLOT.BACKGROUND_SCALE.get())

    if self.PLOT.ETOPO.get():
      self.Mm.etopo(scale=self.PLOT.BACKGROUND_SCALE.get())

    if self.PLOT.ARCGISIMAGE.get() == 1:
      self.Mm.arcgisimage(service=self.PLOT.ARCGISSERVICE.get(), \
                         xpixels=self.PLOT.ARCGISPIXELS.get(),   \
                         dpi=self.PLOT.ARCGISDPI.get(),          \
                         epsg=epsg,                              \
                         verbose=self.PLOT.ARCGISVERBOSE.get())


    # Draw SAIDIN:
    #
    if not empty(self.SAIDIN.FILENAME.get()):
      self.Mscbar = contourplot.drawing(self.Mfig,self.Max,self.Mm, \
                          self.SAIDIN.xx,self.SAIDIN.yy, \
                          self.SAIDIN.FIELD.data, \
                          self.SAIDIN.FIELD.mask, \
                          self.SAIDIN.FIELD.PLOT)

    # Draw fields:
    #
    if self.ncdf > 0:
      for ii in range(self.ncdf):
        if self.CDF[ii].FIELD.show.get():
          self.Mcdfbar.append (contourplot.drawing(self.Mfig,self.Max, \
                                    self.Mm, \
                                    self.CDF[ii].xx,   \
                                    self.CDF[ii].yy,   \
                                    self.CDF[ii].FIELD.data, \
                                    self.CDF[ii].FIELD.mask, \
                                    self.CDF[ii].FIELD.PLOT))

    # Draw currents:
    #
    if self.nvec > 0:
      for ii in range(self.nvec):
        if self.VEC[ii].VEL.show.get():
          vectorplot.drawing (self.Mfig,self.Max,self.Mm,self.VEC[ii])

    # Draw floats:
    #
    if self.nfloat > 0:
      for ii in range(self.nfloat):
        self.FLOAT[ii].L.set(self.L.get())
        lagrangian.drawing(self.Mfig,self.Max,self.Mm,self.FLOAT[ii])


    # Draw markers:
    #
    if self.nmarker > 0:
      for ii in range(self.nmarker):
        geomarker.drawing(self.Mfig,self.Max,self.Mm,self.MARKER[ii])


    if self.PLOT.COASTLINE_SHOW.get():
      self.Mm.drawcoastlines(linewidth=self.PLOT.COASTLINE_WIDTH.get(), \
                       color=self.PLOT.COASTLINE_COLOR.get())

    if self.PLOT.COUNTRYLINE_SHOW.get():
      self.Mm.drawcountries(linewidth=self.PLOT.COUNTRYLINE_WIDTH.get(), \
                        color=self.PLOT.COUNTRYLINE_COLOR.get())


    if self.PLOT.ISOBAT_NPLOT > 0:
      # Plot isobaths and its legend:
      lines = []
      labels = []
      for ii in range(self.PLOT.nisobat):
        if self.PLOT.ISOBAT_LABEL_SHOW.get():
          label = self.PLOT.ISOBAT_LABEL[ii]
        else:
          label = None
        try:
          color = eval(self.PLOT.ISOBAT_COLOR[ii].get())
        except:
          color = self.PLOT.ISOBAT_COLOR[ii].get()
        if self.PLOT.ISOBAT_SHOW[ii]:
          z = self.PLOT.ISOBAT_DATA[ii]
          ilon = z['lon']
          ilat = z['lat']
          isox,isoy = self.Mm(ilon,ilat)
          isbt, = self.Mm.plot(isox,isoy,
                              marker=None,
                              linestyle=self.PLOT.ISOBAT_STYLE[ii].get(),
                              linewidth=self.PLOT.ISOBAT_WIDTH[ii].get(),
                              color=color)
          lines.append(isbt)
          labels.append(label)

          if self.PLOT.ISOBAT_LEGEND.SHOW.get():
            if self.PLOT.ISOBAT_LEGEND.FONTSIZE.get() < 1:
              fontsize = None
            else:
              fontsize = self.PLOT.ISOBAT_LEGEND.FONTSIZE.get()
            if self.PLOT.ISOBAT_LEGEND.MODE.get() == 1:
              mode = 'expand'
            else:
              mode = None

            self.MIlegend = plt.legend([lines[i] for i in range(len(lines))],
                        [labels[i] for i in range(len(lines))],
                        loc=self.PLOT.ISOBAT_LEGEND.LOC.get(),
                        ncol=self.PLOT.ISOBAT_LEGEND.NCOL.get(),
                        fontsize=fontsize,
                        frameon=self.PLOT.ISOBAT_LEGEND.FRAMEON.get(),
                        fancybox=self.PLOT.ISOBAT_LEGEND.FANCYBOX.get(),
                        shadow=self.PLOT.ISOBAT_LEGEND.SHADOW.get(),
                        framealpha=self.PLOT.ISOBAT_LEGEND.ALPHA.get(),
                        mode=mode,
                        facecolor=self.PLOT.ISOBAT_LEGEND.COLOR.get(),
                        edgecolor=self.PLOT.ISOBAT_LEGEND.EDGECOLOR.get(),
                        markerscale=self.PLOT.ISOBAT_LEGEND.MARKERSCALE.get(),
                        borderpad=self.PLOT.ISOBAT_LEGEND.BORDERPAD.get(),
                        handletextpad=self.PLOT.ISOBAT_LEGEND.HANDLETEXTPAD.get(),
                        borderaxespad=self.PLOT.ISOBAT_LEGEND.BORDERAXESPAD.get(),
                        labelspacing=self.PLOT.ISOBAT_LEGEND.LABELSPACING.get(),
                               )

            if not empty(self.PLOT.ISOBAT_LEGEND.TITLE.get()):
              try:
                self.MIlegend.set_title(self.PLOT.ISOBAT_LEGEND.TITLE.get(),
                                 prop=self.PLOT.ISOBAT_LEGEND.TITLEFONT)
              except:
                pass




    if self.PLOT.WATER_COLOR.get() is not 'None':
      self.Mm.drawmapboundary(fill_color=self.PLOT.WATER_COLOR.get())

    if self.PLOT.LAND_COLOR.get() is not 'None':
      self.Mm.fillcontinents(color=self.PLOT.LAND_COLOR.get())

    if self.PLOT.RIVERS_SHOW.get():
      self.Mm.drawrivers(linewidth=self.PLOT.RIVERS_WIDTH.get(), \
                        color=self.PLOT.RIVERS_COLOR.get())


    if self.PLOT.GRID_SHOW.get():

      def setcolor(x,color):
        for m in x:
          for t in x[m][1]:
            t.set_color(color)

      vmeridians = np.arange(self.PLOT.MERIDIAN_INI.get(), \
                             self.PLOT.MERIDIAN_FIN.get(), \
                             self.PLOT.MERIDIAN_INT.get())
      par = self.Mm.drawmeridians(vmeridians,                     \
                        labels=[self.PLOT.GRID_WEST.get(),        \
                                self.PLOT.GRID_EAST.get(),        \
                                self.PLOT.GRID_NORTH.get(),       \
                                self.PLOT.GRID_SOUTH.get()],      \
                        fontsize=self.PLOT.GRID_SIZE.get(),       \
                        linewidth=self.PLOT.GRID_LINEWIDTH.get(), \
                        color=self.PLOT.GRID_COLOR.get())
      setcolor(par,self.PLOT.GRID_FONTCOLOR.get())


      vparallels = np.arange(self.PLOT.PARALLEL_INI.get(), \
                             self.PLOT.PARALLEL_FIN.get(), \
                             self.PLOT.PARALLEL_INT.get())
      mer = self.Mm.drawparallels(vparallels,
                        labels=[self.PLOT.GRID_WEST.get(),        \
                                self.PLOT.GRID_EAST.get(),        \
                                self.PLOT.GRID_NORTH.get(),       \
                                self.PLOT.GRID_SOUTH.get()],      \
                        fontsize=self.PLOT.GRID_SIZE.get(),       \
                        linewidth=self.PLOT.GRID_LINEWIDTH.get(), \
                        color=self.PLOT.GRID_COLOR.get())
      setcolor(mer,self.PLOT.GRID_FONTCOLOR.get())

      # Modify the line characteristics:
      lat_lines = chain(*(tup[1][0] for tup in mer.items()))
      lon_lines = chain(*(tup[1][0] for tup in par.items()))
      all_lines = chain(lat_lines,lon_lines)

      # cylce through these lines and set the desired style
      for line in all_lines:
        line.set(linestyle=self.PLOT.GRID_LINESTYLE.get(),
                 alpha=self.PLOT.GRID_ALPHA.get())

    if self.PLOT.SCALE_SHOW.get():
        try:
          YOFFSET = float(self.PLOT.SCALE_YOFFSET.get())
        except:
          YOFFSET = None

        try:
          LINEWIDTH = float(self.PLOT.SCALE_LINEWIDTH.get())
        except:
          LINEWIDTH = None

        self.Mm.drawmapscale(self.PLOT.SCALE_X.get(),
                             self.PLOT.SCALE_Y.get(),
                             self.PLOT.SCALE_XO.get(),
                             self.PLOT.SCALE_YO.get(),
                             length=self.PLOT.SCALE_LENGTH.get(),
                             units=self.PLOT.SCALE_UNITS.get(),
                             barstyle=self.PLOT.SCALE_STYLE.get(),
                             fontsize=self.PLOT.SCALE_FONTSIZE.get(),
                             yoffset=YOFFSET,
                             labelstyle=self.PLOT.SCALE_LABELSTYLE.get(),
                             fontcolor=self.PLOT.SCALE_FONTCOLOR.get(),
                             fillcolor1=self.PLOT.SCALE_FILLCOLOR1.get(),
                             fillcolor2=self.PLOT.SCALE_FILLCOLOR2.get(),
                             format=self.PLOT.SCALE_FORMAT.get(),
                             linecolor=self.PLOT.SCALE_LINECOLOR.get(),
                             linewidth=LINEWIDTH)



    # Lables and titles:
    font_family = self.PLOT.MAP_FONT_TYPE.get()
    font_size   = self.PLOT.LABEL_SIZE.get()
    font_weight = 'normal'

    font = {'family' : font_family,
            'weight' : font_weight,
            'size'   : font_size}

    self.Max.xaxis.labelpad = self.PLOT.LABEL_PAD.get()
    self.Max.yaxis.labelpad = self.PLOT.LABEL_PAD.get()
    self.Max.set_xlabel(self.PLOT.XLABEL.get(),fontdict=font)
    self.Max.set_ylabel(self.PLOT.YLABEL.get(),fontdict=font)

    self.Max.set_title(self.PLOT.TITLE.get(), \
                        fontproperties=self.PLOT.TITLEFONT)
    px,py = self.Max.title.get_position()
    dy = self.PLOT.TITLE_PAD.get()/self.Mfig.get_dpi()
    self.Max.title.set_position((px,py+dy))


    # Time stamp
    try:
      self.Mtime_stamp.remove()
    except:
      pass

    if len(self.DATE) > 0:
      if self.PLOT.TIMESTAMP_SHOW.get():
        if self.PLOT.TIMESTAMP_BOLD.get():
          font_weight = 'bold'
        else:
          font_weight = 'normal'

        font = {'family' : font_family,
                'weight' : font_weight,
                'color'  : self.PLOT.TIMESTAMP_COLOR.get(),
                'size'   : self.PLOT.TIMESTAMP_SIZE.get()}

        self.Mtime_stamp = self.Mfig.text(self.PLOT.TIMESTAMP_X.get(),     \
                                      self.PLOT.TIMESTAMP_Y.get(),     \
                                      self.DATE[self.L.get()], \
                                      fontdict=font)


    if self.nmarker > 0 and self.PLOT.LEGEND.SHOW.get():
      if self.PLOT.LEGEND.FONTSIZE.get() < 1:
        fontsize = None
      else:
        fontsize = self.PLOT.LEGEND.FONTSIZE.get()
      if self.PLOT.LEGEND.MODE.get() == 1:
        mode = 'expand'
      else:
        mode = None

      try:
        legend = self.Max.legend(loc=self.PLOT.LEGEND.LOC.get(),
                        ncol=self.PLOT.LEGEND.NCOL.get(),
                        fontsize=fontsize,
                        frameon=self.PLOT.LEGEND.FRAMEON.get(),
                        fancybox=self.PLOT.LEGEND.FANCYBOX.get(),
                        shadow=self.PLOT.LEGEND.SHADOW.get(),
                        framealpha=self.PLOT.LEGEND.ALPHA.get(),
                        mode=mode,
                        facecolor=self.PLOT.LEGEND.COLOR.get(),
                        edgecolor=self.PLOT.LEGEND.EDGECOLOR.get(),
                        markerscale=self.PLOT.LEGEND.MARKERSCALE.get(),
                        borderpad=self.PLOT.LEGEND.BORDERPAD.get(),
                        handletextpad=self.PLOT.LEGEND.HANDLETEXTPAD.get(),
                        borderaxespad=self.PLOT.LEGEND.BORDERAXESPAD.get(),
                        labelspacing=self.PLOT.LEGEND.LABELSPACING.get(),
                               )

      except:
        pass

      if not empty(self.PLOT.LEGEND.TITLE.get()):
        try:
          legend.set_title(self.PLOT.LEGEND.TITLE.get(),
                           prop=self.PLOT.LEGEND.TITLEFONT)
        except:
          pass


    if self.PLOT.LOGO_DISPLAY.get() == 1:
      if self.PLOT.LOGO_LOCATION.get() == 'SW':
        xx = self.PLOT.WEST.get()
        yy = self.PLOT.SOUTH.get()
        ba = (0,0)
      elif self.PLOT.LOGO_LOCATION.get() == 'NW':
        xx = self.PLOT.WEST.get()
        yy = self.PLOT.NORTH.get()
        ba = (0,1)
      elif self.PLOT.LOGO_LOCATION.get() == 'NE':
        xx = self.PLOT.EAST.get()
        yy = self.PLOT.NORTH.get()
        ba = (1,1)
      elif self.PLOT.LOGO_LOCATION.get() == 'SE':
        xx = self.PLOT.EAST.get()
        yy = self.PLOT.SOUTH.get()
        ba = (1,0)
      else:
        xx = self.PLOT.LOGO_X.get()
        yy = self.PLOT.LOGO_Y.get()
        ba = (0,0)

      im = OffsetImage(self.PLOT.LOGO_IMAGE,zoom=self.PLOT.LOGO_ZOOM.get())
      self.ab = AnnotationBbox(im,(xx,yy), xycoords='data', \
                               box_alignment=ba,pad=0.0,frameon=True)
      self.with_logo = self.Mm._check_ax().add_artist(self.ab)


    self.Mcanvas.draw()

  def trajectory_editor(self):
  # ==========================
    ''' Launch the editor of a trajectory '''
    
    def _close():
    # ===========
      self.Window_editor.destroy()
      self.Window_editor = None

    # Check if the window was closed by EDITOR !!
    if self.Window_editor is None:
      pass
    else:
      try:
        self.Window_editor.lift()
      except: 
        self.Window_editor = None

   
    if self.Window_editor is None:
      self.Window_editor = tk.Toplevel(self.master)
      self.Window_editor.title('GEOJSON EDITOR')
      self.Window_editor.resizable(width=False,height=False)
      self.Window_editor.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_editor.lift()
      return


    if self.nfloat == 0: 
      jeditor.EDITOR(self.Window_editor)
    else:
      jeditor.EDITOR(self.Window_editor, \
                     self.FLOAT[self.FLOAT_INDX.get()].FILENAME.get())
