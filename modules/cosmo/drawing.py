''' COSMO-VIEW
    Script: drawing.py
    Changes:
		J. Ballabrera, December 2017
		EGL, 06/2020:
			No more support to python 2.7 
			Support to Basemap deprecated and updated to cartopy
			A consola can be added to the main window (it requires 
			to change all prints and use instead the tools.toconsola() 
			function. In some cases the wid of the consola is passed to
			others constructors or a heap variable MESSAGE has been 
			introduced to collect "print" messages.			
			Added limited support to loading and drawing shapefiles
			Base layers of topography and relief substituted by GEBCO and
			EMODNET tile services (requieres internet connection)
			Limited support to geographical projections. Everything is 
			plotted in PlateCarree
			setmap() is now deprecated
			Corrected some text font managements
			All color selections are now managed through tools.colsel() function
			Cartopy projection can be accessed through tools.map_proj()
			
'''
__version__ = "2.0"
__author__  = "Quim Ballabrera and Emilio García"
__date__    = "July 2020"

import sys
import os
from os.path import isfile, join
import numpy as np
import numpy.ma as ma
from scipy import interpolate

import json
import io
import ast
import math

import matplotlib.pyplot as plt
import matplotlib.image as image
import matplotlib.font_manager
import matplotlib.ticker as mticker
from matplotlib.font_manager import FontProperties
from matplotlib.figure import Figure
from matplotlib.offsetbox import TextArea, OffsetImage, AnnotationBbox, AnchoredText
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk
from matplotlib import cm as CM
from matplotlib import colors

#EG Cartopy
import cartopy
import cartopy.crs as ccrs
import cartopy.feature as cfeat
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from cartopy.io.shapereader import Reader
from cartopy.feature import ShapelyFeature

#EG from mpl_toolkits.basemap import Basemap

from netCDF4 import Dataset,num2date
from itertools import chain

from PIL import Image, ImageTk
import matplotlib.animation as manimation

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog as filedialog
from tkcolorpicker import askcolor
from tkinter import font as tkfont

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
#EG
import cosmo.shape as shape
import cosmo.geoplot as geoplot
import cosmo.field as field

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
from cosmo.tools import colsel

#EG
from cosmo.tools import map_proj
from cosmo.tools import scale_bar

#EG consola
from cosmo.tools import toconsola

from cosmo import COSMO_CONF_NAME
from cosmo import COSMO_CONF
from cosmo import COSMO_ROOT
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA
from cosmo import VERSION

global COSMO_CONF,COSMO_CONF_PATH,COSMO_CONF_NAME,COSMO_CONF_DATA

BGC  = 'pale green'    # Background color
BWC  = 'lime green'    # Buttons (PREV and NEXT) color
EBC  = 'forest green'  # Exit Buttons color
FONT = 'Helvetica 14'  # Default font


# =====================
class CONTOUR():
# =====================
  ''' Class for 2D data contours'''

  __version__ = "2.0" 
  __author__ = "Quim Ballabrera" 
  __date__ = "July 2020"

  def __init__(self,filename=None):
  # ===============================
    ''' Define and initialize the class attributes '''

    self.MESSAGE = "\nCONTOUR class:\n"

    self.FILENAME = tk.StringVar()

    if filename is None:
      pass
    else:
      self.FILENAME.set(filename)


    self.FLD     = field.fld_parameters()
    self.PLOT    = contourplot.parameters()
    self.MESSAGE += self.PLOT.MESSAGE

    self.show    = tk.BooleanVar()
    self.varname = tk.StringVar()
    self.minval  = tk.DoubleVar()
    self.maxval  = tk.DoubleVar()

    self.K       = tk.IntVar()
    self.L       = tk.IntVar()

    self.K_LIST  = []
    self.L_LIST  = []
    self.Z_LIST  = []
    self.T_LIST  = []
    self.DATE    = []
    self.TIME    = []

    self.cbar   = None
    self.show.set(True)

  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes '''

    conf = {}
    conf['MINVAL'] = self.minval.get()
    conf['MAXVAL'] = self.maxval.get()
    conf['SHOW']   = self.show.get()
    conf['PLOT']   = self.PLOT.conf_get()
    conf['FLD']    = self.FLD.conf_get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class dictionary from class attributes '''

    self.minval.set(conf['MINVAL'])
    self.maxval.set(conf['MAXVAL'])
    self.show.set(conf['SHOW'])
    self.PLOT.conf_set(conf['PLOT'])
    self.FLD.conf_set(conf['FLD'])

  def read(self,**args):
  # ====================

    try:
      wid = args["wid"]
    except:
      wid = None

    try:
      update_lims = args["update_lims"]
    except:
      update_lims = True

    K = self.K.get()
    L = self.L.get()

    toconsola("Reading contour, K, L = "+str(K)+", "+str(L),wid=wid)

    if self.FLD.ndims == 2:
      u = self.FLD.nc.variables[self.FLD.varname][:,:]
    elif self.FLD.ndims == 3:
      if self.FLD.icdf.ppl[self.FLD.varid] > -1:
        u = self.FLD.nc.variables[self.FLD.varname][L,:,:].squeeze()
      elif self.FLD.icdf.ppk[self.FLD.varid] > -1:
        u = self.FLD.nc.variables[self.FLD.varname][K,:,:].squeeze()
      else:
        toconsola('Invalid file!',wid=wid)
        return
    elif self.FLD.ndims == 4:
      u = self.FLD.nc.variables[self.FLD.varname][L,K,:,:].squeeze()
    else:
      toconsola("Invalid number of dimensions, "+str(ndim),wid=wid)

    # Min and max values
    self.FLD.minval = float(u.min())
    self.FLD.maxval = float(u.max())
    toconsola('Min val = '+str(self.FLD.minval),wid=wid)
    toconsola('Max val = '+str(self.FLD.maxval),wid=wid)

    # Make sure that the missing value is NaN:
    _u = u.filled(fill_value=np.nan)
    self.FLD.data = np.ma.masked_equal(_u,np.nan); del _u

    if update_lims:
      toconsola('Setting contour intervals ...',wid=wid)
      try:
        self.PLOT.CONTOUR_MIN.set(myround(self.FLD.minval))
      except:
        self.PLOT.CONTOUR_MIN.set(self.FLD.minval)
      try:
        self.PLOT.CONTOUR_MAX.set(myround(self.FLD.maxval))
      except:
        self.PLOT.CONTOUR_MAX.set(self.FLD.maxval)

      dd = self.PLOT.CONTOUR_MAX.get() - self.PLOT.CONTOUR_MIN.get()
      try:
        self.PLOT.CONTOUR_INTERVAL.set(myround(0.1*dd,0))
      except:
        self.PLOT.CONTOUR_INTERVAL.set(0.1*dd)
    else:
      toconsola('Preserving contour intervals.',wid=wid)


# =====================
class vector():
# =====================
  ''' Class for 2D data (x,y) vectors'''

  __version__ = "2.0" 
  __author__ = "Quim Ballabrera" 
  __date__ = "July 2020"

  def __init__(self,ufile=None,vfile=None):
  # =======================================
    ''' Define and initialize the class attributes '''

    self.MESSAGE = "\nVECTOR class:\n"

    self.UFILENAME = tk.StringVar()
    self.VFILENAME = tk.StringVar()

    if ufile is None:
      pass
    else:
      self.UFILENAME.set(ufile)
      if vfile is None:
        self.VFILENAME.set(ufile)
      else:
        self.VFILENAME.set(vfile)


    self.U         = field.fld_parameters()
    self.V         = field.fld_parameters()

    self.uname     = tk.StringVar()
    self.vname     = tk.StringVar()

    self.PLOT      = vectorplot.parameters()
    self.MESSAGE  += self.PLOT.MESSAGE
    
    self.K         = tk.IntVar()
    self.L         = tk.IntVar()

    self.K_LIST    = []
    self.L_LIST    = []
    self.Z_LIST    = []
    self.T_LIST    = []
    self.DATE      = []
    self.TIME      = []

    self.show      = tk.BooleanVar()
    self.show.set(True)

    # Select grid type:
    self.grid_type = tk.StringVar()
    self.grid_type_list = ['A','B','C','D']
    self.grid_type.set('A')


  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes '''

    conf = {}
    conf['SHOW'] = self.show.get()
    conf['PLOT'] = self.PLOT.conf_get()
    conf['GRID'] = self.grid_type.get()
    conf['U']    = self.U.conf_get()
    conf['V']    = self.V.conf_get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class dictionary from class attributes '''

    self.show.set(conf['SHOW'])
    self.grid_type.set(conf['GRID_TYPE'])
    self.PLOT.conf_set(conf['PLOT'])
    self.U.conf_set(conf['U'])
    self.V.conf_set(conf['V'])


  def read(self,**args):
  # ====================

    try:
      wid = args["wid"]
    except:
      wid = None

    K = self.K.get()
    L = self.L.get()

    toconsola("Reading vector, K, L = "+str(K)+", "+str(L),wid=wid)

    if self.U.ndims == 2:
      u = self.U.nc.variables[uname][:,:]
      v = self.V.nc.variables[vname][:,:]
    elif self.U.ndims == 3:
      if self.U.icdf.ppl[self.U.varid] > -1:
        u = self.U.nc.variables[self.U.varname][L,:,:].squeeze()
        v = self.V.nc.variables[self.V.varname][L,:,:].squeeze()
      elif self.icdf.ppk[self.U.varid] > -1:
        u = self.U.nc.variables[self.U.varname][K,:,:].squeeze()
        v = self.V.nc.variables[self.V.varname][K,:,:].squeeze()
      else:
        toconsola('Invalid file!',wid=wid)
        return
    elif self.U.ndims == 4:
      u = self.U.nc.variables[self.U.varname][L,K,:,:].squeeze()
      v = self.V.nc.variables[self.V.varname][L,K,:,:].squeeze()
    else:
      toconsola("Invalid number of dimensions, "+str(ndim),wid=wid)

    # Make sure that the missing value is NaN:
    _u = u.filled(fill_value=np.nan)
    _v = v.filled(fill_value=np.nan)
    u = np.ma.masked_equal(_u,np.nan); del _u
    v = np.ma.masked_equal(_v,np.nan); del _v

    if self.grid_type.get() == 'A' or self.grid_type.get() == 'B':
      toconsola("Velocities in a A-grid",wid=wid)
      self.U.data = u.copy()
      self.V.data = v.copy()
      return

    if self.grid_type.get() == 'C':
      toconsola("Regrid C-grid velocities",wid=wid)
      self.U.data = 0.5*(u[1:-1,:-1]+u[1:-1,1:])
      self.V.data = 0.5*(v[:-1,1:-1]+v[1:,1:-1])
      return


# =====================
class fld_parameters():
# =====================
  ''' Class for 2D data fields'''

  __version__ = "1.0" 
  __author__ = "Quim Ballabrera" 
  __date__ = "December 2017"

  def __init__ (self):
  # ==================
    ''' Define and initialize the class attributes '''

    self.MESSAGE = "\nFLD_PARA:\n"
    
    self.PLOT          = contourplot.parameters()
    
    self.MESSAGE += self.PLOT.MESSAGE
    
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

    self.MESSAGE = "VEL_PARA:\n"

    self.PLOT         = vectorplot.parameters()
    
    self.MESSAGE += self.PLOT.MESSAGE
    
    self.u             = None
    self.v             = None
    self.xu            = None    # To allow grid-types
    self.yu            = None    # To allow grid-types
    self.xv            = None    # To allow grid-types
    self.yv            = None    # To allow grid-types
    self.xt            = None    # To allow grid-types
    self.yt            = None    # To allow grid-types
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

    # Add mutiple grid types:
    self.grid_type     = tk.StringVar()
    self.grid_type_list = ['A','B','C','D']
    self.grid_type.set('A')

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
    conf['grid_type'] = self.grid_type.get()

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
    self.grid_type.set(conf['grid_type'])
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
  # ========================

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
    self.WITH_AXIS          = tk.BooleanVar()
    #EG Cartopy projection and parameters
    self.MAP_PROJECTION     = tk.StringVar()
    self.MAP_PROJ_LAT_0		= tk.DoubleVar()
    self.MAP_PROJ_LON_0		= tk.DoubleVar()
    self.MAP_PROJ_MIN_LAT	= tk.DoubleVar()
    self.MAP_PROJ_MAX_LAT	= tk.DoubleVar()
    self.MAP_PROJ_F_NORTH	= tk.DoubleVar()
    self.MAP_PROJ_F_EAST	= tk.DoubleVar()
    self.MAP_PROJ_LAT_T_SCA	= tk.DoubleVar()
    self.MAP_PROJ_T_SCA_LAT	= tk.DoubleVar()
    self.MAP_PROJ_SCA_FAC	= tk.DoubleVar()
    self.MAP_PROJ_SATELLITE_HEIGHT	= tk.DoubleVar()
    self.MAP_PROJ_SWEEP_AXIS = tk.StringVar()
    
    self.MAP_RESOLUTION     = tk.StringVar()
    self.EPSG               = tk.IntVar()
    self.SOUTH              = tk.DoubleVar()
    self.NORTH              = tk.DoubleVar()
    self.WEST               = tk.DoubleVar()
    self.EAST               = tk.DoubleVar()
    self.WIDTH              = tk.DoubleVar()
    self.HEIGHT             = tk.DoubleVar()
    self.LAT_0              = tk.DoubleVar() #
    self.LON_0              = tk.DoubleVar()
    self.SATELLITE_HEIGHT   = tk.DoubleVar()

    self.COASTLINE_SHOW     = tk.BooleanVar()
    # EG 1:Natural-Earth 2: EMODNET
    self.COASTLINE_SOURCE   = tk.IntVar()
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
    #EG RELIEF=1 GEBCO, RELIEF=2 EMODNET
    self.RELIEF_SHOW        = tk.BooleanVar()
    self.RELIEF             = tk.IntVar()
    #EG self.BLUEMARBLE         = tk.BooleanVar()
    #EG self.ETOPO              = tk.BooleanVar()
    self.BACKGROUND_SCALE   = tk.DoubleVar()
    self.RIVERS_SHOW        = tk.BooleanVar()
    self.RIVERS_WIDTH       = tk.DoubleVar()
    self.RIVERS_COLOR       = tk.StringVar()
    #EG ARCGIS changed by EMODNET
    self.EMODNET_ISO        = tk.BooleanVar()
    #EG self.ARCGISIMAGE        = tk.IntVar()
    #EG self.ARCGISSERVICE      = tk.StringVar()
    #EG self.ARCGISSERVICE_LIST = ['ESRI_Imagery_World_2D', \
    #EG                                'ESRI_StreetMap_World_2D', \
    #EG                                'NatGEo_World_Map',  \
    #EG                                'Ocean_Basemap',  \
    #EG                                'World_Imagery',  \
    #EG                                'World_Physical_Map',  \
    #EG                                'World_Shaded_Relief',  \
    #EG                                'World_Street_Map',  \
    #EG                                'World_Terrain_Base',  \
    #EG                               'World_Topo_Map']
    #EG self.ARCGISPIXELS       = tk.IntVar()
    #EG self.ARCGISDPI          = tk.IntVar()
    #EG self.ARCGISVERBOSE      = tk.BooleanVar()
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
    self.WITH_AXIS.set(False)
    #EG Default Cartopy PlateCarree and parameters
    self.MAP_PROJECTION.set('PlateCarree')
    self.MAP_PROJ_LAT_0.set(0.0)
    self.MAP_PROJ_LON_0.set(0.0)
    self.MAP_PROJ_MIN_LAT.set(-80.0)
    self.MAP_PROJ_MAX_LAT.set(84.0)
    self.MAP_PROJ_F_NORTH.set(0.0)
    self.MAP_PROJ_F_EAST.set(0.0)
    self.MAP_PROJ_LAT_T_SCA.set(0.0)
    self.MAP_PROJ_T_SCA_LAT.set(-1)
    self.MAP_PROJ_SCA_FAC.set(-1)
    self.MAP_PROJ_SATELLITE_HEIGHT.set(35785831)
    self.MAP_PROJ_SWEEP_AXIS.set('y')
    
    self.MAP_RESOLUTION.set('50m')
    self.EPSG.set(4326)
    
    #EG self.MAP_PROJECTION.set('cyl')
    #EG self.MAP_RESOLUTION.set('l')
    
    self.SOUTH.set(-90)
    self.NORTH.set(90)
    self.WEST.set(-180)
    self.EAST.set(180)
    self.WIDTH.set(0)
    self.HEIGHT.set(0)
    self.LAT_0.set(0)
    self.LON_0.set(0)
    self.SATELLITE_HEIGHT.set(35786000)
    self.COASTLINE_SHOW.set(False)
    self.COASTLINE_SOURCE.set(1)
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
    self.GRID_WEST.set(True)
    self.GRID_EAST.set(False)
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
    #EG RELIEF refers to GEBCO tile vms
    self.RELIEF_SHOW.set(False)
    self.RELIEF.set(1)
    self.BACKGROUND_SCALE.set(1.0)
    self.RIVERS_SHOW.set(False)
    self.RIVERS_WIDTH.set(0.2)
    self.RIVERS_COLOR.set('blue')
    #EG EMODNET
    #self.EMODNET_COAST.set(False)
    self.EMODNET_ISO.set(False)
    #EG self.ARCGISIMAGE.set(0)
    #EG self.ARCGISSERVICE.set('ESRI_Imagery_world_2D')
    #EG self.ARCGISPIXELS.set(400)
    #EG self.ARCGISDPI.set(96)
    #EG self.ARCGISVERBOSE.set(True)
    self.LOGO_FILE.set(COSMO_CONF_PATH+'cosmo-logo.png')
    self.LOGO_IMAGE = image.imread(self.LOGO_FILE.get())
    self.LOGO_ZOOM.set(0.40)
    self.LOGO_LOCATION.set('SW')
    self.LOGO_DISPLAY.set(False)

    self.ISOBAT_PATH =  tk.StringVar()
    self.ISOBAT_PATH.set(COSMO_ROOT+'/data/isobaths/')

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

    self.MESSAGE = "\n"+self.LEGEND.MESSAGE+"\n"+self.ISOBAT_LEGEND.MESSAGE

    if exists(self.FILECONF):
      self.MESSAGE += "\nReading conf. file: "+self.FILECONF
      try:
        conf = self.conf_load(self.FILECONF)
        self.conf_set(conf)
      except:
        self.MESSAGE += '\n\tError reading, using default parameters'
        conf = self.conf_get()
        self.conf_save(conf,self.FILECONF)
    else:
      self.MESSAGE += '\n\tSaving configuration file ...'
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
    conf['WITH_AXIS'] = self.WITH_AXIS.get()
    
    #EG Default Cartopy PlateCarree and parameters
    conf['MAP_PROJECTION'] = self.MAP_PROJECTION.get()
    conf['MAP_PROJ_LAT_0'] = self.MAP_PROJ_LAT_0.get()
    conf['MAP_PROJ_LON_0'] = self.MAP_PROJ_LON_0.get()
    conf['MAP_PROJ_MIN_LAT'] = self.MAP_PROJ_MIN_LAT.get()
    conf['MAP_PROJ_MAX_LAT'] = self.MAP_PROJ_MAX_LAT.get()
    conf['MAP_PROJ_F_NORTH'] = self.MAP_PROJ_F_NORTH.get()
    conf['MAP_PROJ_F_EAST'] = self.MAP_PROJ_F_EAST.get()
    conf['MAP_PROJ_LAT_T_SCA'] = self.MAP_PROJ_LAT_T_SCA.get()
    conf['MAP_PROJ_T_SCA_LAT'] = self.MAP_PROJ_T_SCA_LAT.get()
    conf['MAP_PROJ_SCA_FAC'] = self.MAP_PROJ_SCA_FAC.get()
    conf['MAP_PROJ_SATELLITE_HEIGHT'] = self.MAP_PROJ_SATELLITE_HEIGHT.get()
    conf['MAP_PROJ_SWEEP_AXIS'] = self.MAP_PROJ_SWEEP_AXIS.get()
    
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
    conf['COASTLINE_SOURCE'] = self.COASTLINE_SHOW.get()
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
    #EG RELIEF refers to GEBCO
    conf['RELIEF_SHOW'] = self.RELIEF_SHOW.get()
    conf['RELIEF'] = self.RELIEF.get()
    #EGconf['BLUEMARBLE'] = self.BLUEMARBLE.get()
    #EGconf['ETOPO'] = self.ETOPO.get()
    conf['BACKGROUND_SCALE'] = self.BACKGROUND_SCALE.get()
    conf['RIVERS_SHOW'] = self.RIVERS_SHOW.get()
    conf['RIVERS_WIDTH'] = self.RIVERS_WIDTH.get()
    conf['RIVERS_COLOR'] = self.RIVERS_COLOR.get()
    #EG EMODNET
    #conf['EMODNET_COAST'] = self.EMODNET_COAST.get()
    conf['EMODNET_ISO'] = self.EMODNET_ISO.get()
    #EG conf['ARCGISIMAGE'] = self.ARCGISIMAGE.get()
    #EG conf['ARCGISSERVICE'] = self.ARCGISSERVICE.get()
    #EG conf['ARCGISPIXELS'] = self.ARCGISPIXELS.get()
    #EG conf['ARCGISDPI'] = self.ARCGISDPI.get()
    #EG conf['ARCGISVERBOSE'] = self.ARCGISVERBOSE.get()
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
    self.WITH_AXIS.set(conf['WITH_AXIS'])
    
    #EG Default Cartopy PlateCarree and parameters
    self.MAP_PROJECTION.set(conf['MAP_PROJECTION'])
    self.MAP_PROJ_LAT_0.set(conf['MAP_PROJ_LAT_0'])
    self.MAP_PROJ_LON_0.set(conf['MAP_PROJ_LON_0'])
    self.MAP_PROJ_MIN_LAT.set(conf['MAP_PROJ_MIN_LAT'])
    self.MAP_PROJ_MAX_LAT.set(conf['MAP_PROJ_MAX_LAT'])
    self.MAP_PROJ_F_NORTH.set(conf['MAP_PROJ_F_NORTH'])
    self.MAP_PROJ_F_EAST.set(conf['MAP_PROJ_F_EAST'])
    self.MAP_PROJ_LAT_T_SCA.set(conf['MAP_PROJ_LAT_T_SCA'])
    self.MAP_PROJ_T_SCA_LAT.set(conf['MAP_PROJ_T_SCA_LAT'])
    self.MAP_PROJ_SCA_FAC.set(conf['MAP_PROJ_SCA_FAC'])
    self.MAP_PROJ_SATELLITE_HEIGHT.set(conf['MAP_PROJ_SATELLITE_HEIGHT'])
    self.MAP_PROJ_SWEEP_AXIS.set(conf['MAP_PROJ_SWEEP_AXIS'])
    
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
    self.COASTLINE_SOURCE.set(conf['COASTLINE_SOURCE'])
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

    #EG Refers to GEBCO tile vms
    self.RELIEF_SHOW.set(conf['RELIEF_SHOW'])
    self.RELIEF.set(conf['RELIEF'])
    #EGself.BLUEMARBLE.set(conf['BLUEMARBLE'])
    #EGself.ETOPO.set(conf['ETOPO'])
    self.BACKGROUND_SCALE.set(conf['BACKGROUND_SCALE'])
    self.RIVERS_SHOW.set(conf['RIVERS_SHOW'])
    self.RIVERS_WIDTH.set(conf['RIVERS_WIDTH'])
    self.RIVERS_COLOR.set(conf['RIVERS_COLOR'])
    #EG EMODNET
    #self.EMODNET_COAST.set(conf['EMODNET_COAST'])
    self.EMODNET_ISO.set(conf['EMODNET_ISO'])
    #EG self.ARCGISIMAGE.set(conf['ARCGISIMAGE'])
    #EG self.ARCGISSERVICE.set(conf['ARCGISSERVICE'])
    #EG self.ARCGISPIXELS.set(conf['ARCGISPIXELS'])
    #EG self.ARCGISDPI.set(conf['ARCGISDPI'])
    #EG self.ARCGISVERBOSE.set(conf['ARCGISVERBOSE'])
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

  # =======================================
  def __init__ (self,master,tconsola=None):
  # =======================================
    # Initialization

    global COSMO_CONF,COSMO_CONF_DATA

    versions  = 'Built with:\n'
    versions += 'Tkinter '+ str(tk.TkVersion) + '\n'
    versions += 'Matplotlib '+ str(sys.modules[plt.__package__].__version__) + '\n'
    versions += 'Cartopy '+ str(cartopy.__version__) + '\n'

    mess  = "CONF_PATH: "+COSMO_CONF_PATH
    mess += '\nCONF_DATA: '+COSMO_CONF_DATA
    mess += '\nCONF: '+ COSMO_CONF

    master.protocol('WM_DELETE_WINDOW',self.close)

    self.master = master
    self.master.configure(bg=BGC)
    # EG we pass the console id
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

    self.SAIDIN        = CONTOUR()
    #self.SAIDIN        = cdf_parameters()
    #self.SAIDIN.FIELD  = fld_parameters()
    # Add an aditional logical flag: landmask (True if land must be masked)
    self.SAIDIN.landmask = tk.BooleanVar()
    self.SAIDIN.landmask.set(False)
    self.sbar          = []
    self.Msbar         = []

    # Features: Markers or shapefiles
    # Stationary information. Types: MARKER,SHAPE

    self.nfeatures = 0
    self.FEATNAMES     = []
    self.FEATTYPES     = []
    self.FEATORDER     = []
	#EG 
    self.nmarker       = 0 			# Numero de fixters de marcadors
    self.MARKER        = []        # llista de estructures de marcadors dim(self.nmarker)
    self.MARKER_LIST   = ['0']      # Llista marker files en configuracion 
    self.MARKER_INDX   = tk.IntVar() # contador de files
    self.MARKER_INDX.set(0)         
	#EG Shape files
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

    # Initialize matplotlib and Cartopy
    self.params = None
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
    #F0 = tk.Frame(self.master,bg="yellow")
    #F0 = tk.Frame(self.master,bg="yellow")
    #EG F0 dropped to facilitate the consola design
    tk.Label(self.master,text='Time',bg=BGC).grid(row=0,column=0,padx=3)
    self.lbox = ttk.Combobox(self.master,textvariable=self.L,width=5)
    self.lbox.grid(row=0,column=1)
    self.lbox.configure(state='disabled')
    self.lbox.bind('<<ComboboxSelected>>',lambda e: self.lselection())
    self.lbox.bind('<Return>',lambda e: self.lselection())

    self.bprev = tk.Button(self.master,text='PREV',command=self.tprev,bg=BWC)
    self.bprev.grid(row=0,column=2,padx=3,sticky='e')

    tk.Entry(self.master,textvariable=self.PLOT.TLABEL, \
              state='readonly',width=20,bg='white').grid(row=0,column=3, padx=3)

    self.bnext = tk.Button(self.master,text='NEXT',command=self.tnext,bg=BWC)
    self.bnext.grid(row=0,column=4,padx=3,stick='w')
    tk.Label(self.master,bg=BGC).grid(row=0,column=5)

    if len(self.DATE) <= 0:
      self.bprev.configure(state='disabled')
      self.lbox.configure(state='disabled')
      self.bnext.configure(state='disabled')
    else:
      self.lbox['values'] = list(range(len(self.L_LIST)))

    tk.Button(self.master,text='Draw',command=self.make_plot,bg=EBC)  \
       .grid(row=1,column=4,padx=3,pady=3,sticky='e')
    tk.Button(self.master,text='Quit',command=self.close,bg=EBC)      \
       .grid(row=1,column=5,padx=3,pady=3,sticky='w')
  
    tk.Label(self.master,text='COSMO project, July 2018',bg=BGC)  \
       .grid(row=2,column=4,columnspan=6,sticky='e')
    
    #F0.grid(row=0, column=0,sticky='ew')
       
    #EG Afegim una Consola
    #EG is the widget referencing the toconsola()
    if tconsola is not None:
        if len(tconsola) > 0:
         wiconsola = tk.Frame(self.master)  # Expandimos la Consola
         wiconsola.grid_rowconfigure(0, weight=1)
         cscrollb = tk.Scrollbar(wiconsola)
         cscrollb.grid(row=0,column=1,sticky='nswe')
         self.cons = tk.Text(wiconsola, bg="black", fg="white", \
         							 yscrollcommand=cscrollb.set)
         # tags to highligth different cathegories of messages by formating the the text
         self.cons.tag_config("y", foreground="yellow", font="-weight bold")
         self.cons.tag_config("o", foreground="orange", font="-weight bold")
         self.cons.tag_config("r", foreground="red", font="-weight bold")
         self.cons.grid(row=0,column=0,sticky='we')
         cscrollb.config(command=self.cons.yview)
         line = tconsola + '\n'+ versions + "\n"+ mess + self.PLOT.MESSAGE+ \
                 self.SAIDIN.FLD.MESSAGE+self.BLM.MESSAGE+self.MLM.MESSAGE
         self.cons.insert("end", line + "\n")
         self.cons.see(tk.END)
         wiconsola.grid(row=3, column=0, columnspan=6, pady=5, sticky='nsew')  
    #EG

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
    self.Window_currents_sel  = None
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
    #EG SHAPE files
    self.Window_shapefile     = None
    self.Window_shapeconfig   = None
    self.Window_geoconfig     = None
    
    self.Window_mapa     = None

  ## CosmoDrawing EVENTS Handlers ##############

  # =============================
  def canvas_closing(self,event):
  # =============================
    ''' Update PLOT.SIZE variable according to the window size'''
    self.PLOT.SIZE = list(self.fig.get_size_inches())
    # Destruir lo que queda en memoria
    self.fig = None
    
  # =============================
  def canvas_resizing(self,event):
  # =============================
    ''' Update PLOT.SIZE variable according to the window size'''
    self.PLOT.SIZE = list(self.fig.get_size_inches())

  # ===========================
  def canvas_click(self,event):
  # ===========================
    if self.GET_TIMESTAMP_LOCATION:
      toconsola("EG Click_event: self.GET_TIMESTAMP_LOCATION",wid=self.cons)
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
        font_weight = 'normal'
        if self.PLOT.TIMESTAMP_BOLD.get(): font_weight = 'bold'

        self.ax.annotate(self.DATE[self.L.get()],xy=(self.PLOT.TIMESTAMP_X.get(), \
				self.PLOT.TIMESTAMP_Y.get()), \
				xycoords='figure fraction', color=self.PLOT.TIMESTAMP_COLOR.get(), \
				fontsize=self.PLOT.TIMESTAMP_SIZE.get(),fontfamily=font_family, \
				fontweight=font_weight,annotation_clip=False)
        self.canvas.draw()
      return
     
    if self.nvec > 0:
      ii = self.VEC_INDX.get()
      if self.VEC[ii].PLOT.KEY_GETXY:
        self.VEC[ii].PLOT.KEY_GETXY = False
        xx = event.x/self.PLOT.DPI.get()/self.PLOT.SIZE[0]
        yy = event.y/self.PLOT.DPI.get()/self.PLOT.SIZE[1]
        self.VEC[ii].PLOT.KEY_X.set(np.round(xx,3))
        self.VEC[ii].PLOT.KEY_Y.set(np.round(yy,3))
        self.VEC[ii].PLOT.KEY_OBJ.X = xx
        self.VEC[ii].PLOT.KEY_OBJ.Y = yy
        self.canvas.draw()
        return

    if event.inaxes is not None:
      #EG xo,yo = self.m(event.xdata,event.ydata,inverse=True)
      p_ref = map_proj('PlateCarree')
      p_local = map_proj(self.PLOT.MAP_PROJECTION.get())
      latlon = p_ref['proj'].transform_point(event.xdata, event.ydata, \
                 p_local['proj'])

      toconsola("Selected Point : "+str(latlon[0])+" - "+str(latlon[1]),wid=self.cons)
      #print('Current speed = ', self.CURRENTS.F(event.xdata,event.ydata))
      #if not empty(self.SAIDIN.FILENAME.get()):
      #  print('SAIDIN SST = ', self.SAIDIN.FIELD.F(xo,yo))
      
      self.BLM.xo.set(latlon[0])
      self.BLM.yo.set(latlon[1])
      self.MLM.xo.set(latlon[0])
      self.MLM.yo.set(latlon[1])

  # ===========================
  def on_xlims_change(self,event):
  # ===========================
    lims = self.ax.get_xlim()
    self.PLOT.WEST.set(lims[0])
    self.PLOT.EAST.set(lims[1])
    self.drawmap = True

  # ===========================
  def on_ylims_change(self,event):
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
    #EG Shapefile and WMS server
    insmenu.add_command(label='Shapefile',command=self.get_shapefile)
    insmenu.add_command(label='WMS Service',state="disable",command=self.get_wms)

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
    confmenu.add_command(label='Shape geometry',
                         command=self.shape_config)
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

      _author = 'Author: Quim Ballabrera (COSMO Project) \n Emilio Garcia (COSMO Project)'
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

      if self.nvec == 0:
        messagebox.showinfo(message='No currents file opened yet')
        return

      ii = self.VEC_INDX.get()
      #EG Corrected exception when the user tries to plot before 
      #EG importing product
      #try:
      #  self.read_UV(self.VEC[ii])
      #except:
      #  toconsola("Press Import to select a product",tag="o", wid=self.cons)
      #  return

      self.VEC[ii].read(wid=self.cons)

      #self.read_UV(self.VEC[ii])
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
      toconsola('Erasing record '+str(ii),wid=self.cons)
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
        _went['textvariable'] = self.VEC[ii].UFILENAME
        _went2['textvariable'] = self.VEC[ii].VFILENAME
        _uvar.configure(state='!disabled')
        _uvar['textvariable'] = self.VEC[ii].uname
        _uvar['values'] = self.VEC[ii].U.icdf.VAR_MENU
        _vvar.configure(state='!disabled')
        _vvar['textvariable'] = self.VEC[ii].vname
        _vvar['values'] = self.VEC[ii].V.icdf.VAR_MENU
        _kbox.configure(state='!disabled')
        _kbox['textvariable'] = self.VEC[ii].K
        _kbox['values'] = self.VEC[ii].K_LIST
        _lbox.configure(state='!disabled')
        _lbox['textvariable'] = self.VEC[ii].L
        _lbox['values'] = self.VEC[ii].L_LIST
        if self.VEC[ii].U.icdf.idk < 0:
          _kbox.configure(state='disabled')
          _zbox['text']='--'
        else:
          _zbox['text']=self.VEC[ii].Z_LIST[self.VEC[ii].K.get()]
        if self.VEC[ii].U.icdf.idl < 0:
          _lbox.configure(state='disabled')
          _dbox['text']='--'
        else:
          _lbox['textvariable'] = self.VEC[ii].L
          _lbox['values'] = self.VEC[ii].L_LIST
          _dbox['text'] = self.VEC[ii].DATE[self.VEC[ii].L.get()]
        _show['variable'] = self.VEC[ii].show

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

      global VEC

      def _cancel():
      # ============
        self.Window_currents_sel.destroy()
        self.Window_currents_sel = None

      def _done():
      # ==========
        global _uvar,_vvar
        global VEC

        if empty(VEC.uname.get()):
          VEC.U.varid = None
        else:
          VEC.U.varid = VEC.U.icdf.vname.index(VEC.uname.get())
        if empty(VEC.vname.get()):
          VEC.V.varid = None
        else:
          VEC.V.varid = VEC.V.icdf.vname.index(VEC.vname.get())

        if VEC.U.varid is None or VEC.V.varid is None:
          messagebox.showinfo(parent=self.Window_currents_sel,message='Select velocity components')
          return

        # Seems a suitable location for those statements:
        VEC.U.varname = VEC.uname.get()
        #VEC.U.varid   = VEC.U.icdf.vname.index(VEC.U.varname)
        VEC.U.ndims   = VEC.U.icdf.ndims[VEC.U.varid]
        VEC.U.get_info(wid=self.cons)
        VEC.U.get_grid()

        VEC.V.varname = VEC.vname.get()
        #VEC.V.varid   = VEC.V.icdf.vname.index(VEC.V.varname)
        VEC.V.ndims   = VEC.V.icdf.ndims[VEC.V.varid]
        VEC.V.get_info(wid=self.cons)

        if VEC.grid_type.get() == 'A' or VEC.grid_type.get() == 'B':
          VEC.V.icdf.VAR_MENU = VEC.U.icdf.VAR_MENU[:]
        else:
          VEC.V.get_grid()

          if VEC.grid_type.get() == 'C':

            # X-center
            xmu0 = 0.5*(VEC.U.xx[:,:-1]+VEC.U.xx[:,1:])
            xmv0 = VEC.V.xx[:,1:-1]
            ymu0 = 0.5*(VEC.U.yy[:,:-1]+VEC.U.yy[:,1:])
            ymv0 = VEC.V.yy[:,1:-1]
            # Y-center
            VEC.V.xx = 0.5*(xmv0[:-1,:]+xmv0[1:,:])
            VEC.U.xx = xmu0[1:-1,:]
            VEC.V.yy = 0.5*(ymv0[:-1,:]+ymv0[1:,:])
            VEC.U.yy = ymu0[1:-1,:]


        #self.read_lonlat(VEC,VEC.icdf.xname,VEC.icdf.yname)
        VEC.K_LIST   = list(range(VEC.U.icdf.nz))
        VEC.L_LIST   = list(range(VEC.U.icdf.nt))

        VEC.K.set(0)
        VEC.Z_LIST = VEC.U.get_zlist()

        VEC.L.set(0)
        VEC.T_LIST, VEC.DATE, VEC.TIME = VEC.U.get_tlist()


        #self.DepthandDate(VEC)
        VEC.show.set(True)

        self.nvec += 1
        self.VEC.append(VEC)
        self.VEC_INDX.set(self.nvec-1)
        self.VEC_LIST = list(range(self.nvec))

        self.nfiles += 1
        self.FILENAMES.append(VEC.UFILENAME.get())
        self.FILETYPES.append('VEC')
        self.FILEORDER.append(self.nvec-1)
        self.SEQUENCES.append(tk.BooleanVar(value=False))

        ii = self.VEC_INDX.get()

        if self.first:
          if self.drawmap is None:
            if VEC.grid_type.get() == 'A' or VEC.grid_type.get() == 'B':
              self.PLOT.WEST.set(self.VEC[ii].U.xmin)
              self.PLOT.EAST.set(self.VEC[ii].U.xmax)
              self.PLOT.SOUTH.set(self.VEC[ii].U.ymin)
              self.PLOT.NORTH.set(self.VEC[ii].U.ymax)
            else:
              self.PLOT.WEST.set(max(self.VEC[ii].U.xmin,self.VEC[ii].V.xmin))
              self.PLOT.EAST.set(min(self.VEC[ii].U.xmax,self.VEC[ii].V.xmax))
              self.PLOT.SOUTH.set(max(self.VEC[ii].U.ymin,self.VEC[ii].V.ymin))
              self.PLOT.NORTH.set(min(self.VEC[ii].U.ymax,self.VEC[ii].V.ymax))
            self.plot_initialize()
          self.L.set(self.VEC[ii].L.get())
          self.L_LIST = list(range(self.VEC[ii].U.icdf.nt))
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
            self.PLOT.XLABEL.set(self.VEC[ii].U.nc.variables[self.VEC[ii].U.icdf.xname].getncattr('long_name'))
          except:
            self.PLOT.XLABEL.set(self.VEC[ii].U.icdf.xname)
          try:
            self.PLOT.YLABEL.set(self.VEC[ii].U.nc.variables[self.VEC[ii].U.icdf.yname].getncattr('long_name'))
          except:
            self.PLOT.YLABEL.set(self.VEC[ii].U.icdf.yname)
          self.SEQUENCES[-1].set(True)
          self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
          self.first = False
        else:
          ntime = len(self.DATE)
          same  = True
          if len(self.VEC[ii].DATE) == ntime:
            toconsola('Same number of time records',wid=self.cons)
            for i in range(ntime):
              if self.DATE[i] != self.VEC[ii].DATE[i]:
                same = False
            self.SEQUENCES[-1].set(same)

        _refill(ii)
        self.Window_currents_sel.destroy()
        self.Window_currents_sel = None

      def _arakawa():
      # =============
        toconsola('Selected Arakawa '+VEC.grid_type.get()+' grid ',wid=self.cons)
        if VEC.grid_type.get() == 'A' or VEC.grid_type.get() == 'B':
          vselect['state'] = 'disabled'
          vaxesid.Ibox['state'] = 'disabled'
          vaxesid.Jbox['state'] = 'disabled'
          vaxesid.Kbox['state'] = 'disabled'
          vaxesid.Lbox['state'] = 'disabled'
          vaxesid.Xbox['state'] = 'disabled'
          vaxesid.Ybox['state'] = 'disabled'
          vaxesid.Zbox['state'] = 'disabled'
          vaxesid.Tbox['state'] = 'disabled'
          vaxesid.wgr['state']  = 'disabled'
        else:
          vselect['state'] = 'normal'
          vaxesid.Ibox['state'] = 'normal'
          vaxesid.Jbox['state'] = 'normal'
          vaxesid.Kbox['state'] = 'normal'
          vaxesid.Lbox['state'] = 'normal'
          vaxesid.Xbox['state'] = 'normal'
          vaxesid.Ybox['state'] = 'normal'
          vaxesid.Zbox['state'] = 'normal'
          vaxesid.Tbox['state'] = 'normal'
          vaxesid.wgr['state']  = 'normal'


      def _vselect():
      # =============
        print('Select V file ...')
        print('Need to be coded !')


      # Main part of the function ...
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
      '''Update to account for multiple Arakawa grids. We begin by duplicatinge
      the velocity object and use the first one for the U information
      and the second one for the V information. Once the grid information
      has been filled, we merge the V-information of the second object
      into the first one '''
      VEC = vector()

      #VEC = cdf_parameters()
      VEC.UFILENAME.set(filename)
      VEC.VFILENAME.set(filename)
      #VEC.VEL = vel_parameters()

      #toconsola(VEC.VEL.MESSAGE,wid=self.cons)
      
      VEC.U.nc = Dataset(filename)
      VEC.U.icdf = tools.geocdf(filename, wid=self.cons)

      VEC.V.nc = Dataset(filename)
      VEC.V.icdf = tools.geocdf(filename, wid=self.cons)

      # Object to capture the information about the V-field
      #VEC2 = cdf_parameters()
      #VEC2.FILENAME.set(filename)
      #VEC2.VEL = vel_parameters()

      toconsola(VEC.MESSAGE,wid=self.cons)
      
      #VEC2.ncid = Dataset(filename)
      #VEC2.icdf = tools.geocdf(filename, wid=self.cons)


      # self.read_lonlat(VEC,VEC.icdf.xname,VEC.icdf.yname)
      # self.DepthandDate(VEC)
      # VEC.VEL.show.set(True)

      if self.Window_currents_sel is None:
        self.Window_currents_sel = tk.Toplevel(self.master)
        self.Window_currents_sel.title('SELECT VARIABLES')
        self.Window_currents_sel.protocol('WM_DELETE_WINDOW',self.Window_currents_sel.destroy)
      #else:
      #  self.Window_currents_sel.lift()
      #  return

      font_bold = tkfont.Font(font='TkDefaultFont').copy()
      font_bold['weight']='bold'

      '''Now, we launch two WinGeoaxes widgets to capturate the two
      components of the field'''

      FAgrid = ttk.Frame(self.Window_currents_sel,padding=5,borderwidth=5)
      ttk.Label(FAgrid,text='Grid type', \
                   font=font_bold).grid(row=0,column=0,sticky='w')
      gtype = ttk.Combobox(FAgrid,textvariable=VEC.grid_type,   \
                      values=VEC.grid_type_list, \
                      width=5)
      gtype.grid(row=0,column=1,columnspan=1,sticky='w')
      gtype.bind('<<ComboboxSelected>>',lambda e: _arakawa())

      FAgrid.grid(row=0,column=0,columnspan=5)


      # -------------------------------------------------------
      FUmain = ttk.Frame(self.Window_currents_sel,padding=5,borderwidth=5)

      FU = ttk.Frame(FUmain,padding=5,borderwidth=5)
      uaxesid = tools.WinGeoaxes(VEC.U.icdf,VEC.U.nc,FU)
      FU.grid(row=0,column=0,columnspan=5)

      ttk.Label(FUmain,text='Select U', \
                   borderwidth=3,   \
                   font=font_bold).grid(row=1,column=2)
      Usel = ttk.Combobox(FUmain,textvariable=VEC.uname,   \
                      values=VEC.U.icdf.VAR_MENU, \
                      width=20)
      Usel.bind('<<ComboboxSelected>>',lambda e: uaxesid.selected_var(VEC.U.icdf,Usel))
      Usel.grid(row=1,column=3,columnspan=2)

      FUmain.grid()


      # -------------------------------------------------------
      FVmain = ttk.Frame(self.Window_currents_sel,padding=5,borderwidth=5)

      vselect = ttk.Button(FVmain,text='Open meridional velocity file',command=_vselect)
      vselect.grid(row=0,column=0,columnspan=2)

      FV = ttk.Frame(FVmain,padding=5,borderwidth=5)
      vaxesid = tools.WinGeoaxes(VEC.V.icdf,VEC.V.nc,FV)
      FV.grid(row=1,column=0,columnspan=5)

      ttk.Label(FVmain,text='Select V', \
                   borderwidth=3,   \
                   font=font_bold).grid(row=2,column=2)
      Vsel = ttk.Combobox(FVmain,textvariable=VEC.vname,   \
                      values=VEC.V.icdf.VAR_MENU, \
                      width=20)
      Vsel.bind('<<ComboboxSelected>>',lambda e: vaxesid.selected_var(VEC.V.icdf,Vsel))
      Vsel.grid(row=2,column=3,columnspan=2)

      FVmain.grid()

      if VEC.grid_type.get() == 'A' or VEC.grid_type.get() == 'B':
        vselect['state'] = 'disabled'
        vaxesid.Ibox['state'] = 'disabled'
        vaxesid.Jbox['state'] = 'disabled'
        vaxesid.Kbox['state'] = 'disabled'
        vaxesid.Lbox['state'] = 'disabled'
        vaxesid.Xbox['state'] = 'disabled'
        vaxesid.Ybox['state'] = 'disabled'
        vaxesid.Zbox['state'] = 'disabled'
        vaxesid.Tbox['state'] = 'disabled'
        vaxesid.wgr['state']  = 'disabled'

      F1 = ttk.Frame(self.Window_currents_sel,padding=5)
      cancel = ttk.Button(F1,text='Cancel',command=_cancel)
      cancel.grid(row=0,column=3,sticky='e',padx=10)
      cancel.bind("<Return>",lambda e:_cancel())
      done = ttk.Button(F1,text='Done',command=_done)
      done.grid(row=0,column=4,sticky='e',padx=10)
      done.bind("<Return>",lambda e:_done())
      F1.grid(sticky='we')
      self.Window_currents_sel.wait_window(self.Window_currents_sel)

    def _lselection():
    # ================
      _dbox['text'] = self.VEC[ii].DATE[self.VEC[ii].L.get()]

    def _kselection():
    # ================
      _zbox['text'] = self.VEC[ii].Z_LIST[self.VEC[ii].K.get()]

    def _uselection():
    # ================
      ii = self.VEC_INDX.get()
      try:
        self.VEC[ii].U.varname = self.VEC[ii].uname.get()
        self.VEC[ii].U.varid = self.VEC[ii].icdf.vname.index( \
                                             self.VEC[ii].uname.get())
      except:
        self.VEC[ii].U.varname = None
        self.VEC[ii].U.varid = -1

    def _vselection():
    # ================
      ii = self.VEC_INDX.get()
      try:
        self.VEC[ii].V.varname = self.VEC[ii].vname.get()
        self.VEC[ii].V.varid = self.VEC[ii].icdf.vname.index( \
                                             self.VEC[ii].vname.get())
      except:
        self.VEC[ii].V.varname = None
        self.VEC[ii].V.varid = -1

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

    global _uvar,_vvar

    self.Window_currents_sel = None
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
    _went2 = ttk.Entry(F0,justify='left',width=50,state='readonly')
    _went2.grid(row=1,column=3,columnspan=5,padx=3,sticky='w')

    # Velocity components:
    ttk.Label(F0,text='Zonal').grid(row=2,column=1,padx=3,pady=3)
    _uvar = ttk.Combobox(F0,width=15)
    _uvar.grid(row=2,column=2,columnspan=2,sticky='w')
    _uvar.bind('<<ComboboxSelected>>',lambda e: _uselection())

    ttk.Label(F0,text='Meridional').grid(row=2,column=4,padx=3,pady=3)
    _vvar = ttk.Combobox(F0,width=15)
    _vvar.grid(row=2,column=5,columnspan=2,sticky='w')
    _vvar.bind('<<ComboboxSelected>>',lambda e: _vselection())

    # Depth:
    ttk.Label(F0,text='Depth').grid(row=3,column=1,padx=3,pady=3)
    _kbox = ttk.Combobox(F0,values=['0'],width=5)
    _kbox.grid(row=3,column=2)
    _kbox.bind('<<ComboboxSelected>>',lambda e: _kselection())
    _zbox = ttk.Label(F0,width=20)
    _zbox.grid(row=3,column=3,columnspan=2,sticky='w')

    # Time:
    ttk.Label(F0,text='Time').grid(row=4,column=1,padx=3,pady=3)
    _lbox = ttk.Combobox(F0,width=5)
    _lbox.grid(row=4,column=2)
    _lbox.bind('<<ComboboxSelected>>',lambda e: _lselection())
    _dbox = ttk.Label(F0,width=20)
    _dbox.grid(row=4,column=3,columnspan=2,sticky='w')

    if ii == -1:
      _wsel.configure(state='disabled')
      _uvar.configure(state='disabled')
      _vvar.configure(state='disabled')
      _kbox.configure(state='disabled')
      _lbox.configure(state='disabled')
    else:
      _went['textvariable'] = self.VEC[ii].UFILENAME
      _went2['textvariable'] = self.VEC[ii].VFILENAME
      _uvar['textvariable'] = self.VEC[ii].uname
      _vvar['textvariable'] = self.VEC[ii].vname
      _uvar['values'] = self.VEC[ii].U.icdf.VAR_MENU
      _vvar['values'] = self.VEC[ii].V.icdf.VAR_MENU
      _kbox['textvariable'] = self.VEC[ii].K
      _kbox['values'] = self.VEC[ii].K_LIST
      if self.VEC[ii].U.icdf.idk < 0:
        _kbox.configure(state='disabled')
        _zbox['text']='--'
      else:
        _zbox['text']=self.VEC[ii].Z_LIST[self.VEC[ii].K.get()]
      if self.VEC[ii].U.icdf.idl < 0:
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
      _show['variable']=self.VEC[ii].show
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
      toconsola(a.MESSAGE,wid=self.cons)
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
      toconsola(a.MESSAGE,wid=self.cons)
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
      toconsola(a.MESSAGE,wid=self.cons)
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
      toconsola('No files opened yet',wid=self.cons)
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
        ttk.Checkbutton(F0,variable=self.VEC[nvec].show,\
                 command=self.make_plot). \
                 grid(row=i+1,column=0,padx=3)
        
      if self.FILETYPES[i] == 'FLD':
        nfld += 1
        ttk.Checkbutton(F0,variable=self.CDF[nfld].show,\
                 command=self.make_plot). \
                 grid(row=i+1,column=0,padx=3)
        
      if self.FILETYPES[i] == 'FLOAT':
        nflo += 1
        ttk.Checkbutton(F0,variable=self.FLOAT[nflo].SHOW,\
                 command=self.make_plot). \
                 grid(row=i+1,column=0,padx=3)
        
      if self.FILETYPES[i] == 'SAIDIN':
        ttk.Checkbutton(F0,variable=self.SAIDIN.show,\
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
      toconsola('%s as %s' % (self.FILENAMES[i],self.FILETYPES[i]),wid=self.cons)

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
        toconsola('Reading configuration file '+self.PLOT.FILECONF,wid=self.cons)
        try:
          conf = self.PLOT.conf_load(self.PLOT.FILECONF)
          self.PLOT.conf_set(conf)
        except:
          toconsola('Error reading, using default parameters',wid=self.cons)
          conf = self.PLOT.conf_get()
          self.PLOT.conf_save(conf,self.PLOT.FILECONF)
      else:
        toconsola('Saving configuration file ...',wid=self.cons)
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
        toconsola('Configuration folder exists',wid=self.cons)
        COSMO_CONF_NAME = '%s' % os.path.basename(os.path.normpath(nn))
        COSMO_CONF = nn + os.sep
      else:
        toconsola('New Configuration folder',wid=self.cons)
        os.makedirs(nn)
        COSMO_CONF_NAME = '%s' % os.path.basename(os.path.normpath(nn))
        COSMO_CONF = nn + os.sep
      
      message ='COSMO_CONF_PATH = '+COSMO_CONF_PATH+"\n"+ \
               'COSMO_CONF_NAME = '+COSMO_CONF_NAME+"\n"+ \
               'COSMO_CONF = '+COSMO_CONF
      toconsola(message,wid=self.cons)
      
      self.PLOT.FILECONF = COSMO_CONF + 'drawing.conf'
      toconsola('self.PLOT.FILECONF = '+self.PLOT.FILECONF,wid=self.cons)

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

    toconsola('Saving figure ...',wid=self.cons)

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
        toconsola('Unknown file type',wid=self.cons)
        return

      CONF.append(conf)

    for i in range(self.nfeatures):

      ii = self.FEATORDER[i]

      conf = {}
      conf['FILENAME'] = self.FEATNAMES[i]
      conf['TYPE']     = self.FEATTYPES[i]
      if self.FEATTYPES[i] == 'MARKER':
        conf['MARKER'] = self.MARKER[ii].conf_get()
      elif self.FEATTYPES[i] == 'SHAPE':
        conf['SHAPE'] = self.SHAPE[ii].conf_get()
        
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

    toconsola('Restoring figure configuration from '+filename,wid=self.cons)

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
  # self.fig.canvas.mpl_connect('close_event',self.on_closing_figure)
  # self.fig.canvas.callbacks.connect('button_press_event',self.on_click)
  # self.ax = self.fig.add_subplot(111)
  # self.drawmap = True

    for ii in range(1,len(CONF)):

      filename = CONF[ii]['FILENAME']

      if CONF[ii]['TYPE'] == 'FLD':

        # Initialize contour class:
        CDF = CONTOUR(filename)
        CDF.FLD.open(filename,wid=self.cons)

        # Initialize classes:
        #
        #CDF = cdf_parameters()
        #CDF.FIELD = fld_parameters()
        #CDF.FILENAME.set(filename)
        #CDF.ncid = Dataset(filename)
        #CDF.icdf = tools.geocdf(filename, wid=self.cons)

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
        CDF.read(update_lims=False,wid=self.cons)
        #self.read_CDF(CDF,update_lims=False)

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
        VEC.icdf = tools.geocdf(filename, wid=self.cons)
       
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
        self.SAIDIN.FLD.nc   = Dataset(filename)
        self.SAIDIN.FLD.icdf = tools.geocdf(filename, wid=self.cons)

        # Update from CONF attributes:
        #
        self.SAIDIN.conf_set(CONF[ii]['SAIDIN'])

        # Read the data:
        self.SAIDIN.FLD.x = self.SAIDIN.FLD.nc.variables['lon'][:]
        self.SAIDIN.FLD.y = self.SAIDIN.FLD.nc.variables['lat'][:]
        self.SAIDIN.varname.set('mcsst')
        self.SAIDIN.FLD.varname = 'mcsst'
        self.SAIDIN.FLD.data = self.SAIDIN.FLD.nc.variables[self.SAIDIN.FLD.varname][0,:,:].squeeze()
        self.SAIDIN.FLD.xx,self.SAIDIN.FLD.yy = np.meshgrid(self.SAIDIN.FLD.x,self.SAIDIN.FLD.y)
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

      if CONF[ii]['TYPE'] == 'SHAPE':

        # Initialize classes:
        #
        SHAPE = shape.parameters()
        SHAPE.Read(filename)

        # Update from CONF attributes:
        #
        SHAPE.conf_set(CONF[ii]['SHAPE'])

        self.nshape += 1
        self.SHAPE.append(SHAPE)
        self.SHAPE.set(self.nshape-1)
        self.SHAPE_LIST = list(range(self.nshape))

        self.nfeatures += 1
        self.FEATNAMES.append(filename)
        self.FEATTYPES.append('SHAPE')
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
      toconsola('Saving in '+self.PLOT.OUT_FILENAME,wid=self.cons)
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
        toconsola('Saving in '+self.PLOT.OUT_FILENAME,wid=self.cons)
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

      toconsola('Updating widget font default values',wid=self.cons)
      
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

    toconsola('Cropping isobaths',wid=self.cons)
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
    toconsola('done',wid=self.cons)
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
      toconsola('Updating map default values',wid=self.cons)
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
	# Options for Map limits and colors
	# EG Now the list of projections is recovered from map_proj in tools
	# By default 50m (50 miles) is the default
	# EG pdict = {} substituted by map_proj

    pdict = map_proj('defs')
    
    rdict = {'110m':'Crude','50m':'Intermediate','10m':'High'}

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
        
    #EG Projection selection
    def pselection():
        ''' We set the appropriate enable/disable fields for each projection
        selection. We recover all the widgets of type Entry and recover
        the state array of the selected projection. Finally the state
        of the widgets are updated.'''
        entries_id = []
        wids = self.fh.winfo_children()
        #entries_id = map(lambda x: x if isinstance(x,tk.ttk.Entry), wids)
        #print(self.fh.winfo_children())
        for wid in wids:
          if isinstance(wid,tk.ttk.Entry):
            entries_id.append(wid)
        
        new_proj = self.PLOT.MAP_PROJECTION.get()
        mpl.config(text=new_proj,width=25)
        proj_state = map_proj(new_proj)
        var_state =  list(map(lambda x: "normal" if x==1 else "disabled", proj_state["state"]))
        toconsola("New PROJECTION selected: "+self.PLOT.MAP_PROJECTION.get(),wid=self.cons)
        for i in range(len(entries_id)): entries_id[i]["state"]=var_state[i]
        self.drawmap = True

    def rselection():
        mrl.config(text=rdict[self.PLOT.MAP_RESOLUTION.get()],width=10)
        self.drawmap = True
# EG deprecated ?
    def icselection():
        ii = self.PLOT.ISOBAT_LABEL.index(self.PLOT.ISOBAT_ZPOINTER.get())
        backup = self.PLOT.ISOBAT_COLOR[ii].get()
        rgb, hx = askcolor(color=self.PLOT.ISOBAT_COLOR[ii].get(),
                          parent=self.master)
        if hx is None:
          self.PLOT.ISOBAT_COLOR[ii].set(backup)
        else:
          self.PLOT.ISOBAT_COLOR[ii].set(hx)

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
          toconsola("New PROJECTION selected: "+self.PLOT.MAP_PROJECTION.get(),wid=self.cons)          
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
    # =============
      nn = tk.filedialog.askdirectory(parent=self.Window_mapconfig)
      if not empty(nn):
        self.PLOT.ISOBAT_PATH.set(nn)

    def select_isobaths():
    # ====================
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
    
    #EG We need to set a new projection object
    def _updated():
    # =============
      self.drawmap = True
      self.make_plot()

    def _apply():
    # ===========
      toconsola("(Apply) Drawing...wait",wid=self.cons)
      self.make_plot()
      toconsola("Done !",wid=self.cons)
      
    def _done():
    # ==========
      toconsola("(Done) Drawing...wait",wid=self.cons)
      self.make_plot()
      toconsola("Done !",wid=self.cons)
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
      toconsola("Saving map default values",wid=self.cons)
      conf = self.PLOT.conf_get()
      self.save_conf(conf,self.PLOT.FILECONF)

    def legend_location():
    # ====================
      ''' Process the location combobox'''
      location_name = loc.get()
      self.PLOT.LEGEND.LOC.set(LEGEND_LOCATION_LIST.index(location_name))
      
    def legend_mode():
    # ================
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

    maptabs.add(page1,text=' Domain ')
    maptabs.add(page2,text=' Background ')
    maptabs.add(page3,text=' Isobaths ')
    maptabs.add(page4,text=' Grid ')
    maptabs.add(page5,text=' Labels ')
    maptabs.add(page7,text=' Scale ')
    maptabs.add(page8,text=' Other ')
    
    #EG We get the projection from tools.map_proj instead of a list
    PROJECTION_LIST = map_proj('lista')

    #EG PAGE 1
    f1 = ttk.Frame(page1,borderwidth=5,padding=5)
    ttk.Label(f1,text='Map Projection').grid(row=0,column=0,padx=3,sticky='w')
    mp = ttk.Combobox(f1,
          textvariable=self.PLOT.MAP_PROJECTION,
          values=PROJECTION_LIST,width=14)
    mp.grid(row=0,column=1,padx=3)
    mp.bind('<<ComboboxSelected>>',lambda e: pselection())
    mpl = ttk.Label(f1,
          text=pdict[self.PLOT.MAP_PROJECTION.get()],width=40)
    mpl.grid(row=0,column=2,columnspan=3,padx=3)

    ttk.Label(f1,text='Map Resolution').grid(row=1,column=0,padx=3,sticky='w')
    #EG values=('c','l','i','h','f') changed by ('110m','50m','10m')
    mr = ttk.Combobox(f1,
          textvariable=self.PLOT.MAP_RESOLUTION,
          values=('110m','50m','10m'),width=7,justify="center")
    mr.grid(row=1,column=1,padx=3,sticky='w')
    mr.bind('<<ComboboxSelected>>',lambda e: rselection())

    mrl = ttk.Label(f1,text=rdict[self.PLOT.MAP_RESOLUTION.get()],width=10)
    mrl.grid(row=1,column=2,columnspan=2,padx=3,sticky='w')
      
    ttk.Label(f1,text='EPSG').grid(row=2,column=0,padx=3,sticky='w')
    ttk.Entry(f1,textvariable=self.PLOT.EPSG,width=7,justify="center").grid(row=2,column=1,padx=3,sticky='w')
    f1.grid(row=0,column=0)

    f2 = ttk.Frame(page1,borderwidth=5,padding=5,relief='sunken')
    ttk.Label(f2,text='Plot limits',font=font_bold).grid(row=0,column=0,padx=3,sticky='w')
    
    ttk.Label(f2,text='North').grid(row=1,column=3,pady=5,padx=3)
    eno = ttk.Entry(f2,textvariable=self.PLOT.NORTH,width=10,justify="center")
    eno.grid(row=2,column=3,pady=5,padx=3)
    eno.bind('<Return>',lambda e:_updated())

    ttk.Label(f2,text='West').grid(row=3,column=1,pady=5,padx=3)
    ewe = ttk.Entry(f2,textvariable=self.PLOT.WEST,width=10,justify="center")
    ewe.grid(row=3,column=2,pady=5,padx=3)
    ewe.bind('<Return>',lambda e:_updated())

    eea = ttk.Entry(f2,textvariable=self.PLOT.EAST,width=10,justify="center")
    eea.grid(row=3,column=4,pady=5,padx=3,sticky='w')
    eea.bind('<Return>',lambda e:_updated())
    ttk.Label(f2,text='East').grid(row=3,column=5,pady=5,padx=3)
    eso = ttk.Entry(f2,textvariable=self.PLOT.SOUTH,width=10,justify="center")
    eso.grid(row=4,column=3,pady=5,padx=3)
    eso.bind('<Return>',lambda e:_updated())
    ttk.Label(f2,text='South').grid(row=5,column=3,pady=5,padx=3)
    ttk.Button(f2,text='Reset',command=lims_reset).grid(row=6,column=5)
    f2.grid(row=1,column=0,padx=30,sticky='w')
    
    #EG We recover the full properties of each projection
    proj_state = map_proj(self.PLOT.MAP_PROJECTION.get())
    var_state =  list(map(lambda x: "normal" if x==1 else "disabled", proj_state["state"]))

    self.params = {"central_longitude":self.PLOT.MAP_PROJ_LAT_0.get(), 
              "central_latitude":self.PLOT.MAP_PROJ_LON_0.get(), 
              "min_latitude":self.PLOT.MAP_PROJ_MIN_LAT.get(), 
              "max_latitude":self.PLOT.MAP_PROJ_MAX_LAT.get(),
              "false_easting":self.PLOT.MAP_PROJ_F_EAST.get(), 
              "false_northing":self.PLOT.MAP_PROJ_F_NORTH.get(),
              "latitude_true_scale":self.PLOT.MAP_PROJ_LAT_T_SCA.get(),
              "true_scale_latitude":self.PLOT.MAP_PROJ_T_SCA_LAT.get(),
              "scale_factor":self.PLOT.MAP_PROJ_SCA_FAC.get(),
              "satellite_height":self.PLOT.MAP_PROJ_SATELLITE_HEIGHT.get(),
              "sweep_axis":self.PLOT.MAP_PROJ_SWEEP_AXIS.get()}
    
    var_proj = [self.PLOT.MAP_PROJ_LAT_0, self.PLOT.MAP_PROJ_LON_0, 
              self.PLOT.MAP_PROJ_MIN_LAT, self.PLOT.MAP_PROJ_MAX_LAT,
              self.PLOT.MAP_PROJ_F_EAST, self.PLOT.MAP_PROJ_F_NORTH,
              self.PLOT.MAP_PROJ_LAT_T_SCA, self.PLOT.MAP_PROJ_T_SCA_LAT,  
              self.PLOT.MAP_PROJ_SCA_FAC, self.PLOT.MAP_PROJ_SWEEP_AXIS,
              self.PLOT.MAP_PROJ_SATELLITE_HEIGHT]
                
    var_txt = ['Central Longitude','Central Latitude',
               'False Easting','False Northing',
               'Min. Latitude','Max. Latitude',
               'Latitude true scale','True scale Latitude',
               'Scale Factor','Sweep Axis',
               'Satellite Height']
 
    self.fh = ttk.Frame(page1,borderwidth=5,padding=5)
    ivar = 0
    for i in range(5):
      ivar = 2*i
      ttk.Label(self.fh,text=var_txt[ivar]).grid(row=i,column=0,padx=3,sticky='e')
      ttk.Entry(self.fh,textvariable=var_proj[ivar],state=var_state[ivar], width=10). \
         grid(row=i,column=1,padx=3,sticky='w')
      ttk.Label(self.fh,text=var_txt[ivar+1]).grid(row=i,column=2,padx=3,sticky='e')
      ttk.Entry(self.fh,textvariable=var_proj[ivar+1],state=var_state[ivar+1], width=10). \
         grid(row=i,column=3,padx=3,sticky='w')
    
    ttk.Label(self.fh,text=var_txt[10]).grid(row=5,column=0,padx=3,sticky='e')
    ttk.Entry(self.fh,textvariable=var_proj[10], state=var_state[10], width=10). \
         grid(row=5,column=1,padx=3,sticky='w')
         
    ttk.Button(self.fh,text='Update projection',command=_updated). \
               grid(row=6,column=0,pady=10,columnspan=4, sticky='ew')
    '''
    ttk.Label(fh,text='Width').grid(row=0,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.WIDTH, width=10).grid(row=0,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Optional map keywords').grid(row=0,column=2,padx=3,sticky='e')
    ttk.Button(fh,text='Estimate', command=_calculator).grid(row=0,column=3,padx=3,sticky='ew')
    ttk.Label(fh,text='Height').grid(row=1,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.HEIGHT, width=10).grid(row=1,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Lon_0').grid(row=2,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.LON_0, width=10).grid(row=2,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Lat_0').grid(row=3,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.LAT_0, width=10).grid(row=3,column=1,padx=3,sticky='w')
    ttk.Label(fh,text='Satellite height').grid(row=4,column=0,padx=3,sticky='e')
    ttk.Entry(fh,textvariable=self.PLOT.SATELLITE_HEIGHT, width=10).grid(row=4,column=1,padx=3,sticky='w')
    '''
    self.fh.grid(row=2,column=0,padx=15,sticky='ew')
    
    #EG PAGE 2, Background and Features
    f3 = ttk.Frame(page2,borderwidth=5,padding=5)
    # Styles
    self.sland, self.swater = ttk.Style(), ttk.Style()
    self.scoast, self.scount = ttk.Style(), ttk.Style()
    self.sriv, scenter = ttk.Style(), ttk.Style()
    self.sland.configure("sland.TLabel",background=self.PLOT.LAND_COLOR.get(),anchor="center")
    self.swater.configure("swater.TLabel",background=self.PLOT.WATER_COLOR.get(),anchor="center")
    self.scoast.configure("scoast.TLabel",background=self.PLOT.COASTLINE_COLOR.get(),anchor="center")
    self.scount.configure("scount.TLabel",background=self.PLOT.COUNTRYLINE_COLOR.get(),anchor="center")
    self.sriv.configure("sriv.TLabel",background=self.PLOT.RIVERS_COLOR.get(),anchor="center")
    scenter.configure("scenter.TEntry",anchor="center")
    tpad = ttk.Style()
    tpad.configure("tpad.TLabelframe",padding=[20,5,5,10])
    
    #Land & Sea
    f3_b=ttk.LabelFrame(f3,text='Basic',borderwidth=5,style='tpad.TLabelframe')
    ttk.Label(f3_b,text='Continents').grid(row=0,column=0,padx=5)
    self.LLabel = ttk.Label(f3_b,textvariable=self.PLOT.LAND_COLOR,width=7,style="sland.TLabel")
    self.LLabel.grid(row=0,column=1,padx=5)
    ttk.Button(f3_b,text='Select',command=lambda:colsel(self.PLOT.LAND_COLOR, \
				self.sland,self.LLabel,"sland.TLabel",master=self.Window_mapconfig)). \
				grid(row=0,column=2,padx=5,sticky='w')
    ttk.Label(f3_b,text='Sea').grid(row=1,column=0,padx=5)
    self.WLabel = ttk.Label(f3_b,textvariable=self.PLOT.WATER_COLOR,width=7,style="swater.TLabel")
    self.WLabel.grid(row=1,column=1,padx=5)
    ttk.Button(f3_b,text='Select',command=lambda:colsel(self.PLOT.WATER_COLOR, \
				self.swater,self.WLabel,"swater.TLabel",master=self.Window_mapconfig)). \
				grid(row=1,column=2,padx=5,sticky='w')
    f3_b.grid(row=0,column=0,padx=5,pady=10,sticky='ewsn')
    
    # Features: Coastlines
    f3_c=ttk.LabelFrame(f3,text='Coastlines',borderwidth=5,style='tpad.TLabelframe')
    ttk.Label(f3_c,text='').grid(row=0,column=0)
    ttk.Checkbutton(f3_c,text=' Show',variable=self.PLOT.COASTLINE_SHOW). \
            grid(row=0,column=1,columnspan=2,padx=3,sticky='w')
    ttk.Label(f3_c,text='Width').grid(row=0,column=3)
    ttk.Label(f3_c,text='Color').grid(row=0,column=4,columnspan=2)
    ttk.Label(f3_c,text='').grid(row=1,column=0)
    ttk.Label(f3_c,text='Natural-Earth').grid(row=1,column=1,padx=3,sticky='w')
    ttk.Radiobutton(f3_c,text=' Show',variable=self.PLOT.COASTLINE_SOURCE,value=1).\
            grid(row=1,column=2,padx=7)
    ttk.Entry(f3_c,textvariable=self.PLOT.COASTLINE_WIDTH,width=7,justify="center"). \
            grid(row=1,column=3,padx=3,sticky='we')
    self.CoLabel = ttk.Label(f3_c,textvariable=self.PLOT.COASTLINE_COLOR,width=7,style="scoast.TLabel")
    self.CoLabel.grid(row=1,column=4,padx=3)
    ttk.Button(f3_c,text='Select',command=lambda:colsel(self.PLOT.COASTLINE_COLOR, \
            self.scoast,self.CoLabel,"scoast.TLabel",master=self.Window_mapconfig)). 	\
            grid(row=1,column=5,padx=3,sticky='ew')
    ttk.Label(f3_c,text='EMODNET').grid(row=2,column=1,padx=5,sticky='w')    
    ttk.Radiobutton(f3_c,text=' Show',variable=self.PLOT.COASTLINE_SOURCE,value=2). \
                grid(row=2,column=2,padx=5)
    f3_c.grid(row=1,column=0,padx=5,pady=10,sticky='ewsn')
    
    # Miscelanea
    f3_m=ttk.LabelFrame(f3,text='Miscelanea',borderwidth=5,style='tpad.TLabelframe')
    ttk.Label(f3_m,text='Countryline').grid(row=0,column=0,sticky='w')
    ttk.Checkbutton(f3_m,text=' Show',variable=self.PLOT.COUNTRYLINE_SHOW). \
            grid(row=0,column=1,padx=3)
    ttk.Entry(f3_m,textvariable=self.PLOT.COUNTRYLINE_WIDTH,width=7,justify="center"). \
            grid(row=0,column=2,padx=3,sticky='we')
    self.CouLabel = ttk.Label(f3_m,textvariable=self.PLOT.COUNTRYLINE_COLOR,width=7,style="scount.TLabel") 
    self.CouLabel.grid(row=0,column=3,padx=3)
    ttk.Button(f3_m,text='Select',command=lambda:colsel(self.PLOT.COUNTRYLINE_COLOR, \
            self.scount,self.CouLabel,"scount.TLabel",master=self.Window_mapconfig)).\
            grid(row=0,column=4,padx=3,sticky='ew')
    ttk.Label(f3_m,text='Rivers').grid(row=1,column=0,padx=3,sticky='w')
    ttk.Checkbutton(f3_m,text=' Show',variable=self.PLOT.RIVERS_SHOW). \
            grid(row=1,column=1,padx=3)
    ttk.Entry(f3_m,textvariable=self.PLOT.RIVERS_WIDTH,width=7,justify="center"). \
            grid(row=1,column=2,padx=3,sticky='we')
    self.RLabel = ttk.Label(f3_m,textvariable=self.PLOT.RIVERS_COLOR,width=7,style="sriv.TLabel")
    self.RLabel.grid(row=1,column=3,padx=3)         
    ttk.Button(f3_m,text='Select',command=lambda:colsel(self.PLOT.RIVERS_COLOR, \
            self.sriv,self.RLabel,"sriv.TLabel",master=self.Window_mapconfig)). \
            grid(row=1,column=4,padx=3,sticky='ew')
    f3_m.grid(row=2,column=0,padx=5,pady=10,sticky='ewsn')

    #EG RELIEF AND ISOBATHS
    f3_r = ttk.LabelFrame(f3,text='Earth Relief (WMS Tiles)',borderwidth=5,style='tpad.TLabelframe')
    ttk.Checkbutton(f3_r,text='  Show',variable=self.PLOT.RELIEF_SHOW). \
                grid(row=0,column=0,columnspan=3,padx=3,sticky='w')
    ttk.Label(f3_r,text='GEBCO service').grid(row=1,column=0,padx=5,sticky='w')
    ttk.Radiobutton(f3_r,text=' Show', variable=self.PLOT.RELIEF, value=1).\
                grid(row=1,column=1,padx=3)
    ttk.Label(f3_r,text='Land & Ocean relief',width=25). \
                grid(row=1,column=2,padx=3)
    ttk.Label(f3_r,text='EMODNET service').grid(row=2,column=0,padx=5,pady=10,sticky='w')
    ttk.Radiobutton(f3_r ,text=' Show',variable=self.PLOT.RELIEF, value=2). \
                grid(row=2,column=1,padx=5,pady=10)
    ttk.Label(f3_r,text='Land & Ocean relief',width=25). \
                grid(row=2,column=2,padx=3,pady=10)  
    f3_r.grid(row=3,column=0,padx=5,pady=10,sticky='ewsn')
 
    f3.grid()
    
    #EG PAGE 3, ISOBATHS
    f4a=ttk.Frame(page3,borderwidth=5,padding=5)
    ttk.Label(f4a,text='EMODNET Depth contours').grid(row=0,column=0,padx=5,pady=10,sticky='w')
    ttk.Checkbutton(f4a,text=' Show',variable=self.PLOT.EMODNET_ISO). \
        grid(row=0,column=1,padx=5,pady=10,columnspan=2)
    f4a.grid(row=0,column=0,pady=10,padx=5,ipadx=5,sticky='w')
    
    f4aa=ttk.LabelFrame(page3,text='Custom Isobaths (meters)',borderwidth=5,padding=5)
    ttk.Label(f4aa,text='Path:',justify='right').grid(row=0,column=0)
    ttk.Entry(f4aa,textvariable=self.PLOT.ISOBAT_PATH, \
              justify='left',width=50).grid(row=0,column=1,padx=3,pady=10)
    ttk.Button(f4aa,text='Select',command=_pselect).grid(row=0,column=2)
    
    f4b = tk.LabelFrame(f4aa,text='Isobaths (meters)',borderwidth=5,relief='sunken')
    self.w = []
    for i in range(self.PLOT.nisobat):
        self.w.append(tk.Checkbutton(f4b,text=str(self.PLOT.ISOBAT_Z[i]), \
                      variable=self.PLOT.ISOBAT_SELEC[i], \
                      command=select_isobaths,justify='right'))
    ii, jj = 0, 1
    for i in range(self.PLOT.nisobat):
        self.w[i].grid(row=jj,column=ii,sticky='w')
        ii += 1
        if ii > 7:
          ii = 0
          jj += 1

    wwr = ttk.Label(f4b,width=26,justify='left')
    wwr.grid(row=4,column=0,columnspan=3,sticky='w',padx=5)
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

    wli = ttk.Button(f4b,text='Load isobaths',command=iload)
    wli.grid(row=4,column=3,columnspan=2,padx=3,sticky='ew')
    wlr = ttk.Button(f4b,text='Crop isobaths',command=self.isobath_crop)
    wlr.grid(row=4,column=5,columnspan=2,padx=3,sticky='ew')
    if self.PLOT.ISOBAT_selected:
        wli.configure(state='enabled')
    else:
        wli.configure(state='disabled')
    if self.PLOT.ISOBAT_loaded:
        wlr.configure(state='enabled')
    else:
        wlr.configure(state='disabled')
    f4b.grid(row=1,column=0,columnspan=3,sticky='we',padx=10)

    # ....................
    def update_name():
        ii = self.PLOT.ISOBAT_LABEL.index(self.PLOT.ISOBAT_ZPOINTER.get())
        wly['textvariable'] = self.PLOT.ISOBAT_STYLE[ii]
        wlw['textvariable'] = self.PLOT.ISOBAT_WIDTH[ii]
        wlc['textvariable'] = self.PLOT.ISOBAT_COLOR[ii]
    # ....................

    # Select the style, width and color of isobaths
    f4c = tk.Frame(f4aa,borderwidth=5)
    ii = self.PLOT.ISOBAT_LABEL.index(self.PLOT.ISOBAT_ZPOINTER.get())
    wln = ttk.Combobox(f4c,width=10,justify="center",
                        textvariable=self.PLOT.ISOBAT_ZPOINTER,
                        values=self.PLOT.ISOBAT_LABEL)
    wln.grid(row=0,column=0, padx=10)
    wln.bind('<<ComboboxSelected>>',lambda e: update_name())

    ttk.Label(f4c,text='Line style').grid(row=0,column=1,padx=5)
    wly = ttk.Combobox(f4c,textvariable=self.PLOT.ISOBAT_STYLE[ii],
                        width=4,justify="center",
                        values=['-',':','--','-.',' '])
    wly.grid(row=0,column=2,padx=5)

    ttk.Label(f4c,text='Line width').grid(row=0,column=3,padx=5)
    wlw = ttk.Entry(f4c,textvariable=self.PLOT.ISOBAT_WIDTH[ii],
                      width=4)
    wlw.grid(row=0,column=4)

    ttk.Label(f4c,text='Line color').grid(row=0,column=5,padx=3)
    wlc = ttk.Entry(f4c, textvariable=self.PLOT.ISOBAT_COLOR[ii],width=10)
    wlc.grid(row=0,column=6)

    ttk.Button(f4c,text='Select',command=icselection).grid(row=0,column=7)

    wls = ttk.Checkbutton(f4c,variable=self.PLOT.ISOBAT_LABEL_SHOW)
    wls.grid(row=1, column=5, sticky='e')
    
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
    ttk.Button(f4c,text='Color grad',command=cgrad).grid(row=1,column=5,padx=3)
    ttk.Label(f4c,text='Label isobaths').grid(row=1,column=7,sticky='w')
    
    f4c.grid(row=2,column=0,columnspan=3,padx=10)
    f4aa.grid(row=1,column=0,pady=10,padx=5,ipadx=5)

    #EG PAGE 4
    font_bold = tkfont.Font(font='TkDefaultFont').copy()
    font_bold['weight']='bold'
    self.sgrid, self.sfgrid = ttk.Style(), ttk.Style()
    self.sgrid.configure("sgrid.TLabel",background=self.PLOT.GRID_COLOR.get(),anchor="center")
    self.sfgrid.configure("sfgrid.TLabel",background=self.PLOT.GRID_FONTCOLOR.get(),anchor="center")
    f5 = ttk.Frame(page4,padding=5)
    ttk.Label(f5,text='Show grid').grid(row=0,column=1,padx=3,sticky='e')
    ttk.Checkbutton(f5,variable=self.PLOT.GRID_SHOW,command=self.make_plot) \
        .grid(row=0,column=2,padx=3,sticky='w')
    ttk.Label(f5,text='Meridians',font=font_bold).grid(row=1,column=0,sticky='w')
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

    ttk.Label(f5,text='Parallels',font=font_bold).grid(row=5,column=0,sticky='w')
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
    ttk.Label(f5,text='Configuration',font=font_bold) \
        .grid(row=10,column=0,sticky='w')
    ttk.Label(f5,text='Character Size').grid(row=11,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_SIZE,justify='left',width=8) \
        .grid(row=11,column=2)
    ttk.Label(f5,text='Line Color').grid(row=12,column=1,sticky='w')
    self.Glabel = ttk.Label(f5,textvariable=self.PLOT.GRID_COLOR,style="sgrid.TLabel",width=8)
    self.Glabel.grid(row=12,column=2,padx=3)
    ttk.Button(f5,text='Select',command=lambda:colsel(self.PLOT.GRID_COLOR, \
            self.sgrid,self.Glabel,"sgrid.TLabel",master=self.Window_mapconfig)). \
            grid(row=12,column=3,padx=3)    
    ttk.Label(f5,text='Line Width').grid(row=13,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_LINEWIDTH,justify='left',width=8) \
        .grid(row=13,column=2)
    ttk.Label(f5,text='Line Style').grid(row=14,column=1,sticky='w')
    ttk.Combobox(f5,textvariable=self.PLOT.GRID_LINESTYLE,
                      justify='left',
                      #EG values=['',' ','None','--','-.','-',':'],width=8) \
                      values=['None','--','-.','-',':'],width=8) \
        .grid(row=14,column=2)
    ttk.Label(f5,text='Line alpha').grid(row=15,column=1,sticky='w')
    ttk.Entry(f5,textvariable=self.PLOT.GRID_ALPHA,justify='left',width=8) \
        .grid(row=15,column=2)
    ttk.Label(f5,text='Font color').grid(row=16,column=1,sticky='w')
    self.GFlabel = ttk.Label(f5,textvariable=self.PLOT.GRID_FONTCOLOR,style="sfgrid.TLabel",width=8)
    self.GFlabel.grid(row=16,column=2,padx=3)
    ttk.Button(f5,text='Select',command=lambda:colsel(self.PLOT.GRID_COLOR, \
            self.sgrid,self.GFlabel,"sfgrid.TLabel",master=self.Window_mapconfig)). \
            grid(row=16,column=3,padx=3)   
    f5.grid()

    f6 = ttk.Frame(page5,borderwidth=5,padding=5)
    self.stsgrid= ttk.Style()
    self.stsgrid.configure("stsgrid.TLabel",background=self.PLOT.TIMESTAMP_COLOR.get(),anchor="center")
    
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
    ttk.Button(f6,text='Set font',command=titleprop0).grid(row=1,column=5,padx=5,sticky='ew')
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
          grid(row=12,column=0,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TIMESTAMP_X,width=12). \
          grid(row=12,column=1,sticky='w')
    ttk.Button(f6,text='Select',command=getlabelpos).grid(row=12,column=2)

    ttk.Label(f6,text='Y pos'). \
          grid(row=13,column=0,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TIMESTAMP_Y,width=12). \
          grid(row=13,column=1,columnspan=1,sticky='w')
    ttk.Label(f6,text='Size'). \
          grid(row=14,column=0,sticky='w')
    ttk.Entry(f6,textvariable=self.PLOT.TIMESTAMP_SIZE,width=5). \
          grid(row=14,column=1,sticky='w')
    ttk.Label(f6,text='Color').grid(row=15,column=0,sticky='w') 
    self.GTSlabel = ttk.Label(f6,textvariable=self.PLOT.TIMESTAMP_COLOR,style="stsgrid.TLabel",width=8)
    self.GTSlabel.grid(row=15,column=1,sticky='w')
    ttk.Button(f6,text='Select',command=lambda:colsel(self.PLOT.TIMESTAMP_COLOR, \
            self.stsgrid,self.GTSlabel,"stsgrid.TLabel",master=self.Window_mapconfig)). \
            grid(row=15,column=2,sticky='w')

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
    self.obgrid, self.otgrid = ttk.Style(),ttk.Style()
    self.obgrid.configure("obgrid.TLabel",background=self.PLOT.FIGURE_COLOR.get(),anchor="center")
    self.otgrid.configure("otgrid.TLabel",background=self.PLOT.TEXT_COLOR.get(),anchor="center")
    
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
    self.OBlabel = ttk.Label(f8,textvariable=self.PLOT.FIGURE_COLOR,style="obgrid.TLabel",width=8)
    self.OBlabel.grid(row=3,column=1,padx=3)
    ttk.Button(f8,text='Select',command=lambda:colsel(self.PLOT.FIGURE_COLOR, \
            self.obgrid,self.OBlabel,"obgrid.TLabel",master=self.Window_mapconfig)). \
            grid(row=3,column=2,padx=3)            
    ttk.Label(f8,text='Text color').grid(row=4,column=0,sticky='w')
    self.OTlabel = ttk.Label(f8,textvariable=self.PLOT.TEXT_COLOR,style="otgrid.TLabel",width=8)
    self.OTlabel.grid(row=4,column=1,padx=3)
    ttk.Button(f8,text='Select',command=lambda:colsel(self.PLOT.TEXT_COLOR, \
            self.otgrid,self.OTlabel,"otgrid.TLabel",master=self.Window_mapconfig)). \
            grid(row=4,column=2,padx=3)
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
        toconsola('Cannot read default configuration file '+cfilename,wid=self.cons)
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
        toconsola("New default values saved in file "+cfilename,wid=self.cons)
      except:
        toconsola('Cannot open default configuration file '+cfilename,wid=self.cons)

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
                             box_alignment=ba,pad=0.0,frameon=True,zorder=100)
    self.with_logo = self.ax.add_artist(self.ab)

  # ============
  def mlm(self):
  # ============
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
      toconsola(command,wid=self.cons)
      #print(command)
      os.system(command)

      if os.path.isfile(self.MLM.TRAJECTORY.get()):
        FLT = lagrangian.parameters()
        toconsola(FLT.MESSAGE,wid=self.cons)
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
      toconsola(command,wid=self.cons)
      #print(command)
      os.system(command)

      if os.path.isfile(self.BLM.TRAJECTORY.get()):
        FLT = lagrangian.parameters()
        toconsola(FLT.MESSAGE,wid=self.cons)
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
                #self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
                self.CDF[self.FILEORDER[i]].read(update_lims=False,wid=self.cons)
              else:
                toconsola("Something wrong",wid=self.cons)
                #print('Something wrong')
                quit()
          self.make_Mplot()
          writer.grab_frame()
      messagebox.showinfo(parent=self.Window_anim,message='Movie has been saved')
      self.L.set(L_Backup)

    def _loadconf():
    # -------------
      '''Load ANIM configuration'''
      toconsola('Retrieving VIDEO defaults.',wid=self.cons)
      #print('Retrieving VIDEO defaults.')
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
      toconsola('Updating VIDEO defaults.',wid=self.cons)
      #print('Updating VIDEO defaults.')
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
    #EG Projection
    projection = self.PLOT.MAP_PROJECTION.get()
    proj = map_proj(projection)
    self.Max = self.Mfig.add_subplot(111, projection=proj['proj'])
    self.Mcanvas = FigureCanvasTkAgg(self.Mfig, master=F1)
    self.Mcanvas.draw()
    self.Mcanvas.get_tk_widget().grid(row=0,column=0,columnspan=11,sticky='wn')
    self.Mdrawmap = True
    F1.grid()
    self.make_Mplot(proj=proj['proj'])

  # =========================
  def DepthandDate(self,CDF):
  # =========================
    '''Fill the lists: K_LIST, L_LIST, Z_LIST, T_LIST and DATE'''

    CDF.K.set(0)             # Default layer
    CDF.L.set(0)             # Default time step

    CDF.K_LIST   = list(range(CDF.FLD.icdf.nz))
    CDF.L_LIST   = list(range(CDF.FLD.icdf.nt))

   # Depth selector
    if CDF.FLD.icdf.idk > -1:
      if self.PLOT.GEOMAP.get():
        wrk = CDF.FLD.nc.variables[CDF.FLD.icdf.zname][:]
        CDF.Z_LIST = list(wrk)
        toconsola(str(CDF.Z_LIST),wid=self.cons)
        #print(CDF.Z_LIST)
      else:
        CDF.Z_LIST = np.arange(CDF.FLD.icdf.nz)
    else:
      CDF.Z_LIST = []

   # Time selector and TIME and DATE values
    CDF.DATE = []
    if CDF.FLD.icdf.idl > -1:
      wrk = CDF.FLD.nc.variables[CDF.FLD.icdf.tname][:]
      CDF.T_LIST = list(wrk)
      try:
        for i in range(CDF.icdf.nt):
          CDF.DATE.append(num2date(CDF.T_LIST[i],       \
                          units=CDF.FLD.icdf.time_units,    \
                          calendar=CDF.FLD.icdf.time_calendar))
      except:
        for i in range(CDF.FLD.icdf.nt):
          CDF.DATE.append(i)

      try:
        CDF.TIME = np.array([(CDF.DATE[i]-CDF.DATE[0]).total_seconds() \
                           for i in range(CDF.FLD.icdf.nt)])
      except:
        CDF.TIME = np.array([(CDF.DATE[i]-CDF.DATE[0]) \
                           for i in range(CDF.FLD.icdf.nt)])

    else:
      CDF.T_LIST = []
      CDF.DATE = [' ']
      CDF.TIME = np.array([0])
  
  # ====================================
  def read_lonlat(self,CDF,xname,yname):
  # ====================================
    '''Read 1D/2D lon lat grid '''

    if CDF.icdf.georef:
      vlon = CDF.ncid.variables[xname]
      vlat = CDF.ncid.variables[yname]
      toconsola(str(vlon),wid=self.cons)
      toconsola(str(vlat),wid=self.cons)
    else:
      toconsola('Georef is False',wid=self.cons)
      #print('Georef is False')
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

#  # ====================
#  def read_UV(self,VEC):
#  # ====================
#    '''Read 2D velocity data according to user selections'''
#    #K     = self.K.get()
#    #L     = self.L.get()
#    K     = VEC.K.get()
#    L     = VEC.L.get()
#    uname = '%s' % VEC.uname.get()
#    vname = '%s' % VEC.vname.get()
#    ndim  = VEC.icdf.ndims[VEC.uid]
#
#    #VEC.K.set(K)
#    #VEC.L.set(L)
#
#    if ndim == 2:
#      VEC.VEL.u = VEC.ncid.variables[uname][:,:]
#      VEC.VEL.v = VEC.ncid.variables[vname][:,:]
#    elif ndim == 3:
#      if VEC.icdf.ppl[VEC.uid] > -1:
#        VEC.VEL.u = VEC.ncid.variables[uname][L,:,:].squeeze()
#        VEC.VEL.v = VEC.ncid.variables[vname][L,:,:].squeeze()
#      elif VEC.icdf.ppk[VEC.uid] > -1:
#        VEC.VEL.u = VEC.ncid.variables[uname][K,:,:].squeeze()
#        VEC.VEL.v = VEC.ncid.variables[vname][K,:,:].squeeze()
#      else:
#        toconsola('Invalid file!',wid=self.cons)
#        print('Invalid file!')
#        return
#    elif ndim == 4:
#      VEC.VEL.u = VEC.ncid.variables[uname][L,K,:,:].squeeze()
#      VEC.VEL.v = VEC.ncid.variables[vname][L,K,:,:].squeeze()
#    else:
#      toconsola("Invalid number of dimensions, "+str(ndim),wid=self.cons)
#      #print('Invalid number of dimensions, '+str(ndim))
#
#    _u   = VEC.VEL.u.copy()
#    _v   = VEC.VEL.v.copy()
#    msku = ma.getmask(VEC.VEL.u)
#    mskv = ma.getmask(VEC.VEL.v)
#    msk  = ma.mask_or(msku,mskv)
#    VEC.VEL.u = ma.array(_u,mask=msk).copy()
#    VEC.VEL.v = ma.array(_v,mask=msk).copy()
#    #VEC.VEL.speed = np.sqrt(VEC.VEL.u**2+VEC.VEL.v**2)
#    #VEC.VEL.F = interpolate.interp2d(VEC.lon, \
#    #                                 VEC.lat, \
#    #                                 VEC.VEL.speed)
#
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
      CDF.FLD.data = None
      return

    K = CDF.K.get()
    L = CDF.L.get()

    vname = '%s' % CDF.varname.get()
    toconsola('READ_FIELD Reading Var, Level and Time:'+str(CDF.varid)+
                                                     ", "+str(CDF.K.get())+
                                                     ", "+str(CDF.L.get()),wid=self.cons)
    #print('READ_FIELD Reading Var, Level and Time:'+str(CDF.varid)+
     #                                                ", "+str(CDF.K.get())+
     #                                                ", "+str(CDF.L.get()))

    ndim = CDF.icdf.ndims[CDF.varid]
    if ndim == 2:
      CDF.FLD.data = CDF.ncid.variables[vname][:,:]
    elif ndim == 3:
      if CDF.icdf.ppl[CDF.varid] > -1:
        CDF.FLD.data = CDF.ncid.variables[vname][L,:,:].squeeze()
      elif CDF.icdf.ppk[CDF.varid]  > -1:
        CDF.FLD.data = CDF.ncid.variables[vname][K,:,:].squeeze()
      else:
        messagebox.showinfo(message='Invalid variable dimensions')
        CDF.FLD.data = None
    elif ndim == 4:
      CDF.FLD.data = CDF.ncid.variables[vname][L,K,:,:].squeeze()

    CDF.FLD.missing_value = None

    if CDF.FLD.data is not None:
      CDF.FLD.varname = vname
      try:
        CDF.FLD.units = CDF.ncid.variables[vname].getncattr('units')
      except:
        CDF.FLD.units = ''

      try:
        CDF.FLD.missing_value = CDF.ncid.variables[vname].getncattr('_FillValue')
      except:
        try:
          CDF.FLD.missing_value = CDF.ncid.variables[vname].getncattr('missing_value')
        except:
          CDF.FLD.missing_value = None

      if CDF.FLD.missing_value is not None:
        CDF.FLD.mask = ma.getmask(CDF.FLD.data)
        CDF.FLD.data[CDF.FLD.data==CDF.FLD.missing_value] = np.nan

      # Contour intervals
      CDF.FLD.minval = float(CDF.FLD.data.min())
      CDF.FLD.maxval = float(CDF.FLD.data.max())
      toconsola('Min val = '+str(CDF.FLD.minval),wid=self.cons)
      toconsola('Max val = '+str(CDF.FLD.maxval),wid=self.cons)
      #print('Min val = '+str(CDF.FIELD.minval))
      #print('Max val = '+str(CDF.FIELD.maxval))

      print('Here: ', update_lims)
      print(CDF.FLD.minval)
      print(CDF.FLD.maxval)

      if update_lims:
        try:
          CDF.PLOT.CONTOUR_MIN.set(myround(CDF.FLD.minval))
        except:
          CDF.PLOT.CONTOUR_MIN.set(CDF.FLD.minval)
        try:
          CDF.PLOT.CONTOUR_MAX.set(myround(CDF.FLD.maxval))
        except:
          CDF.PLOT.CONTOUR_MAX.set(CDF.FLD.maxval)

        dd =   CDF.PLOT.CONTOUR_MAX.get() \
             - CDF.PLOT.CONTOUR_MIN.get()
        try:
          CDF.PLOT.CONTOUR_INTERVAL.set(myround(0.1*dd,0))
        except:
          CDF.PLOT.CONTOUR_INTERVAL.set(0.1*dd)

  # ===================
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
      #self.CDF[ii].FLD.read(self.CDF[ii].K.get(),self.CDF[ii].L.get(),wid=self.cons)
      self.CDF[ii].read(wid=self.cons)
      #self.read_CDF(self.CDF[ii])
      #self.read_Field(self.CDF[ii].FIELD,   \
      #                self.CDF[ii].ncid,    \
      #                self.CDF[ii].icdf,    \
      #                self.CDF[ii].varid,   \
      #                self.CDF[ii].K.get(), \
      #                self.CDF[ii].L.get())


      # Set the contour levels
      #try:
      #  self.CDF[ii].PLOT.CONTOUR_MIN.set(myround(self.CDF[ii].FLD.minval))
      #except:
      #  self.CDF[ii].PLOT.CONTOUR_MIN.set(self.CDF[ii].FLD.minval)
      #try:
      #  self.CDF[ii].PLOT.CONTOUR_MAX.set(myround(self.CDF[ii].FLD.maxval))
      #except:
      #  self.CDF[ii].PLOT.CONTOUR_MAX.set(self.CDF[ii].FLD.maxval)
#
#      dd = self.CDF[ii].PLOT.CONTOUR_MAX.get() - self.CDF[ii].PLOT.CONTOUR_MIN.get()
#      try:
#        self.CDF[ii].PLOT.CONTOUR_INTERVAL.set(myround(0.1*dd,0))
#      except:
#        self.CDF[ii].PLOT.CONTOUR_INTERVAL.set(0.1*dd)

      # The date of the data
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
      toconsola('Erasing record '+str(ii),wid=self.cons)
      #print('Erasing record '+str(ii))

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
        _wvar['values'] = self.CDF[ii].FLD.icdf.VAR_MENU
        _kbox.configure(state='!disabled')
        _kbox['textvariable'] = self.CDF[ii].K
        _kbox['values'] = self.CDF[ii].K_LIST
        _lbox.configure(state='!disabled')
        _lbox['textvariable'] = self.CDF[ii].L
        _lbox['values'] = self.CDF[ii].L_LIST
        if self.CDF[ii].FLD.icdf.idk < 0:
          _kbox.configure(state='disabled')
          _zbox['text']='--'
        else:
          _zbox['text']=self.CDF[ii].Z_LIST[self.CDF[ii].K.get()]
        if self.CDF[ii].FLD.icdf.idl < 0:
          _lbox.configure(state='disabled')
          _dbox['text']='--'
        else:
          _lbox['textvariable'] = self.CDF[ii].L
          _lbox['values'] = self.CDF[ii].L_LIST
          _dbox['text'] = self.CDF[ii].DATE[self.CDF[ii].L.get()]
        _show['variable'] = self.CDF[ii].show

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
          CDF.FLD.varname = CDF.varname.get()
          CDF.FLD.varid = CDF.FLD.icdf.vname.index(CDF.FLD.varname)
          CDF.FLD.ndims = CDF.FLD.icdf.ndims[CDF.FLD.varid]
          CDF.FLD.get_info(wid=self.cons)
          

        # Seems the good place where to put this:
        CDF.FLD.get_grid()
        #self.read_lonlat(CDF,CDF.FLD.icdf.xname,CDF.FLD.icdf.yname)
        self.DepthandDate(CDF)
        CDF.show.set(True)
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
            self.PLOT.WEST.set(self.CDF[ii].FLD.xmin)
            self.PLOT.EAST.set(self.CDF[ii].FLD.xmax)
            self.PLOT.SOUTH.set(self.CDF[ii].FLD.ymin)
            self.PLOT.NORTH.set(self.CDF[ii].FLD.ymax)
            self.plot_initialize()
          self.L.set(self.CDF[ii].L.get())
          self.L_LIST = list(range(self.CDF[ii].FLD.icdf.nt))
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
            self.PLOT.XLABEL.set(self.CDF[ii].FLD.nc.variables[self.CDF[ii].FLD.icdf.xname].getncattr('long_name'))
          except:
            self.PLOT.XLABEL.set(self.CDF[ii].FLD.icdf.xname)
          try:
            self.PLOT.YLABEL.set(self.CDF[ii].FLD.nc.variables[self.CDF[ii].FLD.icdf.yname].getncattr('long_name'))
          except:
            self.PLOT.YLABEL.set(self.CDF[ii].FLD.icdf.yname)
          self.SEQUENCES[-1].set(True)
          self.PLOT.VIDEO_L2.set(len(self.DATE)-1)
          self.first = False
        else:
          ntime = len(self.DATE)
          same  = True
          if len(self.CDF[ii].DATE) == ntime:
            toconsola('Same number of time records',wid=self.cons)
            #print('Same number of time records')
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

      # Initialize contour class:
      CDF = CONTOUR(filename)
      CDF.FLD.open(filename,wid=self.cons)


      # Not empty filename:
      #CDF = cdf_parameters()
      #CDF.FIELD = fld_parameters()
      #CDF.FILENAME.set(filename)
      #CDF.ncid = Dataset(filename)
      #CDF.icdf = tools.geocdf(filename, wid=self.cons)

      ##self.read_lonlat(CDF,CDF.icdf.xname,CDF.icdf.yname)
      ##self.DepthandDate(CDF)
      ##CDF.FIELD.show.set(True)

      ##if empty(CDF.DATE[0].__str__()):
      ##  _dsel.configure(state='enabled')

      if Window_select is None:
        Window_select = tk.Toplevel(self.master)
        Window_select.title('SELECT VARIABLE')
        Window_select.protocol('WM_DELETE_WINDOW',Window_select.destroy)
      else:
        Window_select.lift()
        return

      #axesid = tools.WinGeoaxes(CDF.icdf,CDF.ncid,Window_select)
      axesid = tools.WinGeoaxes(CDF.FLD.icdf,CDF.FLD.nc,Window_select)
      
      font_bold = tkfont.Font(font='TkDefaultFont').copy()
      font_bold['weight']='bold'

      F0 = ttk.Frame(Window_select,padding=5,borderwidth=5)
      ttk.Label(F0,text='Select variable',borderwidth=3,font=font_bold) \
         .grid(row=0,column=0)
      dvar = ttk.Combobox(F0,textvariable=CDF.varname, \
                   values=CDF.FLD.icdf.VAR_MENU,    \
                   width=20)
      dvar.grid(row=0,column=1,columnspan=2)
      dvar.bind('<<ComboboxSelected>>',lambda e: axesid.selected_var(CDF.FLD.icdf,dvar))

      F0.grid()

      #CDF.icdf.nx = -9999
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
      _zbox['text'] = self.CDF[ii].Z_LIST[self.CDF[ii].K.get()]

    def _vselection():
    # ================
      try:
        self.CDF[ii].FLD.varid = self.CDF[ii].FLD.icdf.vname.index( \
                                             self.CDF[ii].varname.get())
      except:
        self.CDF[ii].FLD.varid = -1

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
      _wvar['values'] = self.CDF[ii].FLD.icdf.VAR_MENU
      _kbox['textvariable'] = self.CDF[ii].K
      _kbox['values'] = self.CDF[ii].K_LIST
      if self.CDF[ii].FLD.icdf.idk < 0:
        _kbox.configure(state='disabled')
        _zbox['text']='--'
      else:
        _zbox['text']=self.CDF[ii].Z_LIST[self.CDF[ii].K.get()]
      if self.CDF[ii].FLD.icdf.idl < 0:
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
      _show['variable']=self.CDF[ii].show
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
      name = saidin.saidin_selector(parent=self.master, wid=self.cons)
      if not empty(name):
        self.SAIDIN.FILENAME.set(name)

    def _done():
      if (empty(self.SAIDIN.FILENAME.get())):
        messagebox.showinfo(message='No image selected')
        return

      self.SAIDIN.FLD.nc = Dataset('[FillMismatch]'+self.SAIDIN.FILENAME.get(),'r')
      self.SAIDIN.FLD.icdf = tools.geocdf(self.SAIDIN.FILENAME.get(), wid=self.cons)
      self.SAIDIN.varname.set('mcsst')
      self.SAIDIN.FLD.varname = 'mcsst'
      self.SAIDIN.FLD.x = self.SAIDIN.FLD.nc.variables['lon'][:]
      self.SAIDIN.FLD.y = self.SAIDIN.FLD.nc.variables['lat'][:]
      self.SAIDIN.FLD.data = self.SAIDIN.FLD.nc.variables[self.SAIDIN.FLD.varname][0,:,:].squeeze()
      self.SAIDIN.FLD.xx,self.SAIDIN.FLD.yy = np.meshgrid(self.SAIDIN.FLD.x,self.SAIDIN.FLD.y)
      self.DepthandDate(self.SAIDIN)

      self.nfiles += 1
      self.FILENAMES.append(self.SAIDIN.FILENAME.get())
      self.FILETYPES.append('SAIDIN')
      self.FILEORDER.append(0)
      self.SEQUENCES.append(tk.BooleanVar())

      if self.first:
        if self.drawmap is None:
          self.PLOT.WEST.set(np.min(self.SAIDIN.FLD.x))
          self.PLOT.EAST.set(np.max(self.SAIDIN.FLD.x))
          self.PLOT.SOUTH.set(np.min(self.SAIDIN.FLD.y))
          self.PLOT.NORTH.set(np.max(self.SAIDIN.FLD.y))
          self.plot_initialize()
        self.L.set(self.SAIDIN.L.get())
        self.DATE = self.SAIDIN.DATE.copy()
        self.TIME = self.SAIDIN.TIME.copy()
        try:
          self.PLOT.XLABEL.set(self.SAIDIN.FLD.nc.variables[self.SAIDIN.icdf.xname]. \
                                                                          getncattr('long_name'))
        except:
          self.PLOT.XLABEL.set(self.SAIDIN.FLD.icdf.xname)
        try:
          self.PLOT.YLABEL.set(self.SAIDIN.FLD.nc.variables[self.SAIDIN.icdf.yname] \
                                                                          .getncattr('long_name'))
        except:
          self.PLOT.YLABEL.set(self.SAIDIN.FLD.icdf.yname)
        self.first = False

      self.SAIDIN.FLD.get_info(wid=self.cons)
      #try:
      #  self.SAIDIN.FLD.units = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname] \
      #                                                                    .getncattr('units')
      #except:
      #  self.SAIDIN.FLD.units = ''
#
#      try:
#        self.SAIDIN.FLD.missing_value = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname] \
#                                                                          .getncattr('_FillValue')
#      except:
#        try:
#          self.SAIDIN.FLD.missing_value = self.SAIDIN.ncid.variables[self.SAIDIN.FIELD.varname] \
#                                                                          .getncattr('missing_value')
#        except:
#          self.SAIDIN.FIELD.missing_value = None

      toconsola(str(self.SAIDIN.FLD.minval),wid=self.cons)
      toconsola(str(self.SAIDIN.FLD.maxval),wid=self.cons)
      if self.SAIDIN.landmask.get():
        toconsola('Applying land/sea mask ...',wid=self.cons)
        _a  = self.SAIDIN.FLD.data.copy()
        tmp  = self.SAIDIN.FLD.nc.variables['lsmask'][0,:,:].squeeze()
        msk = ma.masked_where(tmp==1,tmp)
        self.SAIDIN.FLD.data = ma.array(_a,mask=msk).copy()

      self.SAIDIN.FLD.mask = ma.getmask(self.SAIDIN.FLD.data)

      # Contour intervals
      self.SAIDIN.FLD.minval = self.SAIDIN.FLD.data.min()
      self.SAIDIN.FLD.maxval = self.SAIDIN.FLD.data.max()
      try:
        self.SAIDIN.PLOT.CONTOUR_MIN.set(myround(self.SAIDIN.FLD.minval))
      except:
        self.SAIDIN.PLOT.CONTOUR_MIN.set(self.SAIDIN.FLD.minval)
      try:
        self.SAIDIN.PLOT.CONTOUR_MAX.set(myround(self.SAIDIN.FLD.maxval))
      except:
        self.SAIDIN.PLOT.CONTOUR_MAX.set(self.SAIDIN.FLD.maxval)

      dd =   self.SAIDIN.PLOT.CONTOUR_MAX.get() - self.SAIDIN.PLOT.CONTOUR_MIN.get()
      try:
        self.SAIDIN.PLOT.CONTOUR_INTERVAL.set(myround(0.1*dd))
      except:
        self.SAIDIN.PLOT.CONTOUR_INTERVAL.set(0.1*dd)

      #self.SAIDIN.FIELD.F = interpolate.interp2d(self.SAIDIN.lon, \
      #                                     self.SAIDIN.lat, \
      #                                     self.SAIDIN.FIELD.data)
      _close()
      self.make_plot()

    def _clear():
      self.SAIDIN.FILENAME.set('')
      self.SAIDIN.FLD.x    = None
      self.SAIDIN.FLD.y    = None
      self.SAIDIN.FLD.xx   = None
      self.SAIDIN.FLD.yy   = None
      self.SAIDIN.FLD.data = None
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
    ttk.Checkbutton(F0,text='Mask land data',variable=self.SAIDIN.landmask).grid(row=1,column=5,padx=3)
    ttk.Button(F0,text='Cancel',command=_clear).grid(row=1,column=6,padx=3)
    ttk.Button(F0,text='Clear',command=_clear).grid(row=1,column=7,padx=3)
    ttk.Button(F0,text='Plot',command=_done).grid(row=1,column=8,padx=3)
    F0.grid()

  # ===================
  def get_marker(self):
  # ===================
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
      self.nfeatures -= 1

      toconsola('Erasing marker '+str(ii),wid=self.cons)
      del self.MARKER[ii]
      self.nmarker -= 1

      ii = self.nmarker-1 if ii >= self.nmarker else ii
      toconsola('New marker = '+str(self.nmarker),wid=self.cons)
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

      toconsola(MARKER.MESSAGE,wid=self.cons)      

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

  # ======================
  def get_shapefile(self):
  # ==========================
    def _close():
      self.Window_shapefile.destroy()
      self.Window_shapefile = None

    def _done():
      ii = self.SHAPE_INDX.get()
      if ii >= 0:
        self.SHAPE[ii].LABEL.set(_wlab.get())
        self.make_plot()

      self.Window_shapefile.destroy()
      self.Window_shapefile = None
      if self.Window_geoconfig is not None:
        self.Window_geoconfig.destroy()
        self.Window_geoconfig = None
        self.Window_geoconfig()

    def _clear():
    # ===========
      '''Note that shape geometries have no time axis in principle'''
      if self.nshape == 0:
        return

      ii = self.SHAPE_INDX.get()

      for i in range(self.nfeatures):
        if self.FEATTYPES[i] == 'MARKER' and self.FEATORDER[i] == ii:
          del self.FEATNAMES[i]
          del self.FEATTYPES[i]
          del self.FEATORDER[i]
      self.nfeatures -= 1

      toconsola('Erasing marker '+str(ii),wid=self.cons)
      del self.SHAPE[ii]
      self.nmarker -= 1

      ii = self.nmarker-1 if ii >= self.nmarker else ii
      toconsola('New marker = '+str(self.nmarker),wid=self.cons)
      self.SHAPE.set(ii)
      _refill(ii)
      self.make_plot()
      _close()

    def _reget():
    # ===========
      self.SHAPE_INDX.set(_wsel.get())
      ii = self.SHAPE_INDX.get()
      _refill(ii)
    
    def _refill(ii):
    # ==============
      if ii >= 0:
        self.SHAPE_LIST = list(range(self.nshape))
        _wsel['values'] = self.SHAPE_LIST
        _went['textvariable'] = self.SHAPE[ii].FILENAME
        _wstat['text'] = ' N = '+str(self.SHAPE[ii].n)+' geometries'
        _wsel.configure(state='!disabled')
        _wlab['state'] = '!disabled'
        _wlab['textvariable'] = self.SHAPE[ii].LABEL
      else:
        self.SHAPE         = []
        self.SHAPE_LIST    = ['0']
        self.SHAPE_INDX    = tk.IntVar()
        self.SHAPE_INDX.set(0)
        _wsel['values'] = self.SHAPE_LIST
        _went['textvariable'] = ''
        _wstat['text'] = ''
        _wsel.configure(state='disabled')
        _wlab['textvariable'] = ''
        _wlab.configure(state='disabled')

    def _add():
    # ========
      nn = filedialog.askopenfilename(filetypes=[('shp','*.shp')],
                                       initialdir='./',
                                       parent=self.Window_shapefile)
      if len(nn) == 0:
        return
      else:
        filename = '%s' % nn

      # Not empty filename:
      SHAPE = shape.parameters()
      toconsola(SHAPE.MESSAGE,wid=self.cons)      
      SHAPE.Read(filename)
      
      if SHAPE.n == 0:
        return
      
      self.nshape += 1
      self.SHAPE.append(SHAPE)
      self.SHAPE_INDX.set(self.nshape-1)
      self.SHAPE_LIST = list(range(self.nshape))

      self.nfeatures += 1
      self.FEATNAMES.append(SHAPE.FILENAME.get())
      self.FEATTYPES.append('SHAPE')
      self.FEATORDER.append(self.nfeatures-1)

      ii = self.SHAPE_INDX.get()
      
      _refill(ii)

    # Main window:
    # ============

    if self.Window_shapefile is not None:
      self.Window_shapefile.lift()
      return

    self.Window_shapefile = tk.Toplevel(self.master)
    self.Window_shapefile.title('Shape file')
    self.Window_shapefile.protocol('WM_DELETE_WINDOW',_close)

    if self.nshape > 0:
      ii = self.SHAPE_INDX.get()
    else:
      ii = -1

    F0 = ttk.Frame(self.Window_shapefile,padding=5)

    # Add
    ttk.Button(F0,text='Add',command=_add).grid(row=0,column=0,padx=3)

    # Filename:
    ttk.Label(F0,text='Shape file').grid(row=0,column=1,padx=3)

    _wsel = ttk.Combobox(F0,textvariable=self.SHAPE_INDX, \
                                  values=self.SHAPE_LIST,width=5)
    _wsel.grid(row=0,column=2)
    _wsel.bind('<<ComboboxSelected>>',lambda e: _reget())
    _went = ttk.Entry(F0,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=3,columnspan=5,padx=3,sticky='w')

    # AAA
    if ii == -1:
      _wstat = ttk.Label(F0,text='',width=50,justify='left')
      _wsel.configure(state='disabled')
    else:
      _wstat = ttk.Label(F0,text=' N = '+str(self.SHAPE[ii].n),width=50,justify='left')
      _went['textvariable'] = self.SHAPE[ii].FILENAME

    _wstat.grid(row=1,column=3,columnspan=5,padx=3,sticky='w')

    ttk.Label(F0,text='Shape Label').grid(row=2,column=1,padx=3)
    _wlab = ttk.Entry(F0,justify='left',width=18)
    _wlab.grid(row=2,column=2,columnspan=2,padx=3,sticky='w')
    if ii == -1:
      _wlab['state'] = 'disabled'
    else:
      _wlab['textvariable'] = self.SHAPE[ii].LABEL

    F0.grid(row=0,column=0)

    F1 = ttk.Frame(self.Window_shapefile,padding=5)
    ttk.Button(F1,text='Clear',command=_clear).grid(row=1,column=6,padx=3)
    ttk.Button(F1,text='Done',command=_done).grid(row=1,column=7,padx=3)
    F1.grid(row=1,column=0)

  # ================
  def get_wms(self):
  # ==========================
    pass

  # ======================
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
      toconsola('Restoring dot configuration',wid=self.cons)
      try:
        self.MARKER[ii].PLOT.load(self.MARKER[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to load file '+self.MARKER[ii].PLOT.FILECONF,wid=self.cons)
      self.make_plot()

    def _saveconf():
    # =============
      '''Load dot configuration'''
      ii = self.MARKER_INDX.get()
      toconsola('Saving dot configuration',wid=self.cons)
      try:
        self.MARKER[ii].PLOT.save(self.MARKER[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+self.MARKER[ii].PLOT.FILECONF,wid=self.cons)

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
      toconsola('Restoring dot configuration from '+
            self.MARKER[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.MARKER[ii].PLOT.load(self.MARKER[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to load file '+self.MARKER[ii].PLOT.FILECONF,wid=self.cons)
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
      toconsola('Saving dot configuration to '+self.MARKER[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.MARKER[ii].PLOT.save(self.MARKER[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+self.MARKER[ii].PLOT.FILECONF,wid=self.cons)

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

  # ======================
  def shape_config(self):
  # =======================
    '''Widget to configure Markers'''
    #self.dot_config(self.MARKER[self.MARKER_INDX.get()])

    global ishow

    if self.nshape == 0:
      messagebox.showinfo(message='No Shape file opened yet')
      return

    def _cancel():
    # ============
      self.Window_geoconfig.destroy()
      self.Window_geoconfig = None

    def _apply():
    # ============
      self.make_plot()

    def _done():
    # ============
      self.make_plot()
      self.Window_geoconfig.destroy()
      self.Window_geoconfig = None

    def _selected():
    # ==============
      global ishow
      # ?????
      itab = self.Mnb.index('current')
      ishow.destroy()
      # The usual configuration:
      ii = self.SHAPE_INDX.get()
      _went['textvariable'] = self.SHAPE[ii].FILENAME

      ishow = ttk.Frame(self.Window_geoconfig,padding=10)

      # Define tabs:
      self.Mnb = ttk.Notebook(ishow)
      page0 = ttk.Frame(self.Mnb)
      page1 = ttk.Frame(self.Mnb)
      page2 = ttk.Frame(self.Mnb)
      page3 = ttk.Frame(self.Mnb)
      self.Mnb.add(page0,text='Label Aspect')
      self.Mnb.add(page1,text='Geometry Aspect')
      self.Mnb.add(page2,text='Geometry Text',state="disabled")
      self.Mnb.add(page3,text='Geometry coordinates',state="disabled")
      self.Mnb.grid()
      self.Mnb.select(itab)

      # Page0
      ttk.Label(page0, text='Show as text',padding=3). \
              grid(row=0,column=0,padx=3,sticky='e')
      ttk.Checkbutton(page0,variable=self.SHAPE[ii].textmode). \
              grid(row=0,column=1,padx=3,sticky='w')
      ttk.Label(page0, text='Generic label',padding=3). \
              grid(row=1,column=0,padx=3,sticky='e')
      ttk.Entry(page0,textvariable=self.SHAPE[ii].LABEL). \
              grid(row=1,column=1,padx=3,sticky='w')
               
      # Page 1
      geoplot.Configuration(page1,self.SHAPE[ii].PLOT)
  
      # Page 2
      shape.TextConfigure(page2,self.SHAPE[ii].PLOT)

      # Page 3
      #shape.ShowData(page3,self.SHAPE[ii])

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
      ii = self.SHAPE_INDX.get()
      toconsola('Restoring dot configuration',wid=self.cons)
      try:
        self.SHAPE[ii].PLOT.load(self.SHAPE[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to load file '+self.SHAPE[ii].PLOT.FILECONF,wid=self.cons)
      self.make_plot()

    def _saveconf():
    # =============
      '''Load dot configuration'''
      ii = self.SHAPE_INDX.get()
      toconsola('Saving dot configuration',wid=self.cons)
      try:
        self.SHAPE[ii].PLOT.save(self.SHAPE[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+self.SHAPE[ii].PLOT.FILECONF,wid=self.cons)

    def _loadfromconf():
    # ==================
      '''Load dot configuration from a file'''
      ii = self.SHAPE_INDX.get()
      nn = filedialog.askopenfilename(title='Load geometry configuration',
                                      parent=self.Window_dotconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.SHAPE[ii].PLOT.FILECONF = '%s' % nn
      toconsola('Restoring dot configuration from '+
            self.SHAPE[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.SHAPE[ii].PLOT.load(self.SHAPE[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to load file '+self.SHAPE[ii].PLOT.FILECONF,wid=self.cons)
      self.make_plot()

    def _saveasconf():
    # ================
      '''Save dot configuration to a file'''
      ii = self.SHAPE_INDX.get()
      nn = filedialog.asksaveasfilename(title='Save geometry configuration',
                                        parent=self.Window_dotconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      self.SHAPE[ii].PLOT.FILECONF = '%s' % nn
      toconsola('Saving dot configuration to '+self.SHAPE[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.SHAPE[ii].PLOT.save(self.SHAPE[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+self.SHAPE[ii].PLOT.FILECONF,wid=self.cons)

    if self.Window_geoconfig is not None:
      self.Window_geoconfig.lift()
      return

    self.Window_geoconfig = tk.Toplevel(self.master)
    self.Window_geoconfig.title('Shape geometry plot configuration')
    self.Window_geoconfig.resizable(width=True,height=True)
    self.Window_geoconfig.protocol('WM_DELETE_WINDOW',_cancel)

    menubar = tk.Menu(self.Window_geoconfig)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='Restore',command=_loadconf)
    menu.add_command(label='Restore from',command=_loadfromconf)
    menu.add_command(label='Save',command=_saveconf)
    menu.add_command(label='Save as',command=_saveasconf)
    try:
      self.Window_geoconfig.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      master.tk.call(Window_geoconfig, "config", "-menu", menubar)

    fsel = ttk.Frame(self.Window_geoconfig,padding=10)
    ttk.Label(fsel,text="File: ").grid(row=0,column=0,sticky='e',padx=3)
    _wsel = ttk.Combobox(fsel,textvariable=self.SHAPE_INDX,
                              values=self.SHAPE_LIST,width=5)
    _wsel.grid(row=0,column=1,sticky='w',padx=3)
    _wsel.bind('<<ComboboxSelected>>',lambda e:_selected())
    _went = ttk.Entry(fsel,justify='left',width=50,state='readonly')
    _went.grid(row=0,column=2,columnspan=5,padx=3,sticky='w')
    _went = ttk.Entry(fsel,justify='left',width=80,state='readonly')
    _went.grid(row=0,column=2,columnspan=8,padx=3,sticky='w')
    fsel.grid()

    # The usual configuration:
    ii = self.SHAPE_INDX.get()
    _went['textvariable'] = self.SHAPE[ii].FILENAME

    ishow = ttk.Frame(self.Window_geoconfig,padding=10)
    # Define tabs:
    self.Mnb = ttk.Notebook(ishow)
    page0 = ttk.Frame(self.Mnb)
    page1 = ttk.Frame(self.Mnb)
    page2 = ttk.Frame(self.Mnb)
    page3 = ttk.Frame(self.Mnb)
    self.Mnb.add(page0,text='Label Aspect')
    self.Mnb.add(page1,text='Geometry Aspect')
    self.Mnb.add(page2,text='Label Text',state="disabled")
    self.Mnb.add(page3,text='Geometry coordinates',state="disabled")
    self.Mnb.grid()

    # Page0
    ttk.Label(page0,text='Show as text',padding=3). \
              grid(row=0,column=0,padx=3,sticky='e')
    ttk.Checkbutton(page0,variable=self.SHAPE[ii].textmode). \
              grid(row=0, column=1,padx=3, sticky='w')
    ttk.Label(page0,text='Generic label',padding=3). \
              grid(row=1,column=0,padx=3,sticky='e')
    ttk.Entry(page0,textvariable=self.SHAPE[ii].LABEL).\
              grid(row=1, column=1,padx=3, sticky='w')
              
    # Page 1
    geoplot.Configuration(page1,self.SHAPE[ii].PLOT)

    # Page 2
    shape.TextConfigure(page2,self.SHAPE[ii].PLOT)

    # Page 3 Si hay Muchas geometrias inmanejable
    #shape.ShowData(page3,self.SHAPE[ii])

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

      toconsola('Erasing record '+str(ii),wid=self.cons)
      del self.FLOAT[ii]
      self.nfloat -= 1

      ii = self.nfloat-1 if ii >= self.nfloat else ii
      toconsola('new nfloat = '+str(self.nfloat),wid=self.cons)
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
            toconsola('Loading file: '+filename,wid=self.cons)
            _load_trajectory(filename)

      elif ISOURCE == 2:
        url = simple_form('Select remote trajectory folder','url')
        if empty(url):
          return

        filelist = urlList(url,'geojson')
        if len(filelist) > 0:
          for filename in filelist:
            toconsola('Loading file: '+filename,wid=self.cons)
            _load_trajectory(filename)

      elif ISOURCE == 3:
        filelist = db.select_exp()
        if len(filelist) > 0:
          for filename in filelist:
            _load_trajectory(filename)

    def _load_trajectory(filename):
    # ==================================

      FLT = lagrangian.parameters()
      toconsola(FLT.MESSAGE, wid=self.cons)     
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
      _went['textvariable'] = self.VEC[ii].UFILENAME
  
      fshow = ttk.Frame(self.Window_vectorconfig,padding=10)
      vectorplot.Configuration(parent=fshow,
                               PLOT=self.VEC[ii].PLOT)

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
      toconsola('Restoring vector configuration from '+
            self.VEC[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.VEC[ii].PLOT.load(self.VEC[ii].PLOT.UFILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file '+
            self.VEC[ii].PLOT.FILECONF,wid=self.cons)

    def _saveconf():
    # =============
      '''Load vector configuration'''
      toconsola('Saving vector configuration to '+
            self.VEC[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.VEC[ii].PLOT.save(self.VEC[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to write file '+
            self.VEC[ii].PLOT.FILECONF,wid=self.cons)

    def _loadfromconf():
    # ==================
      '''Load vector configuration from a file'''
      nn = filedialog.askopenfilename(title='Load vector configuration',
                                      parent=self.Window_vectorconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.VEC[ii].PLOT.FILECONF = '%s' % nn
      toconsola('Restoring vector configuration from '+
            self.VEC[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.VEC[ii].PLOT.load(self.VEC[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file '+
             self.VEC[ii].PLOT.FILECONF,wid=self.cons)

    def _saveasconf():
    # ================
      '''Load vector configuration'''
      nn = filedialog.asksaveasfilename(title='Save vector configuration', 
                                        parent=self.Window_vectorconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      self.VEC[ii].PLOT.FILECONF = '%s' % nn
      toconsola('Saving vector configuration to '+self.VEC[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.VEC[ii].PLOT.save(self.VEC[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+
             self.VEC[ii].PLOT.FILECONF,wid=self.cons)

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
    self.Window_vectorconfig.config(menu=menubar)
    #try:
    #  self.Window_vectorconfig.config(menu=menubar)
    #except AttributeError:
    #  # master is a toplevel window (Python 2.4/Tkinter 1.63)
    #  master.tk.call(self.Window_vectorconfig, "config", "-menu", menubar)

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
    _went['textvariable'] = self.VEC[ii].UFILENAME

    fshow = ttk.Frame(self.Window_vectorconfig,padding=10)
    vectorplot.Configuration(parent=fshow,
                             PLOT=self.VEC[ii].PLOT)

    f0 = ttk.Frame(fshow,padding=5)
    ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
        grid(row=0,column=0,padx=3)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Done',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)
    fshow.grid()

  # ======================
  def saidin_config(self):
  # ======================

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
      toconsola('Restoring contour configuration from '+
            self.SAIDIN.PLOT.FILECONF,wid=self.cons)
      try:
        self.SAIDIN.PLOT.load(self.SAIDIN.PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file '+
              self.SAIDIN.PLOT.FILECONF,wid=self.cons)

    def _saveconf():
    # =============
      '''Load contour configuration'''
      toconsola('Saving contour configuration to '+
            self.SAIDIN.PLOT.FILECONF,wid=self.cons)
      try:
        self.SAIDIN.PLOT.save(FF.PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+
            self.SAIDIN.PLOT.FILECONF,wid=self.cons)

    def _loadfromconf():
    # ==================
      '''Load contour configuration from a file'''
      nn = filedialog.askopenfilename(title='Load contour configuration',
                                      parent=self.Window_saidinconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.SAIDIN.PLOT.FILECONF = '%s' % nn
      toconsola('Restoring contour configuration from '+
            self.SAIDIN.PLOT.FILECONF,wid=self.cons)
      try:
        self.SAIDIN.PLOT.load(self.SAIDIN.PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file '+
              self.SAIDIN.PLOT.FILECONF,wid=self.cons)

    def _saveasconf():
    # ================
      '''Load contour configuration'''
      nn = filedialog.asksaveasfilename(title='Save contour configuration',
                                        parent=self.Window_saidinconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      self.SAIDIN.PLOT.FILECONF = '%s' % nn
      toconsola('Saving contour configuration to '+
            self.SAIDIN.PLOT.FILECONF,wid=self.cons)
      try:
        self.SAIDIN.PLOT.save(self.SAIDIN.PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+
            self.SAIDIN.PLOT.FILECONF,wid=self.cons)

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
                              varname=self.SAIDIN.FLD.varname,
                              units=self.SAIDIN.FLD.units,
                              missing=self.SAIDIN.FLD.missing,
                              minval=self.SAIDIN.FLD.minval,
                              maxval=self.SAIDIN.FLD.maxval,
                              PLOT=self.SAIDIN.PLOT)

    f0 = ttk.Frame(gshow,padding=5)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Close',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)
    gshow.grid()

  # =======================
  def contour_config(self):
  # =======================

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
                              varname=self.CDF[ii].FLD.varname,
                              units=self.CDF[ii].FLD.units,
                              missing=self.CDF[ii].FLD.missing_value,
                              minval=self.CDF[ii].FLD.minval,
                              maxval=self.CDF[ii].FLD.maxval,
                              PLOT=self.CDF[ii].FLD.PLOT)

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
      toconsola('Restoring contour configuration from '+
            self.CDF[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.CDF[ii].PLOT.load(self.CDF[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file '+
              self.CDF[ii].PLOT.FILECONF,wid=self.cons)

    def _saveconf():
    # =============
      '''Load contour configuration'''
      toconsola('Saving contour configuration to '+
            self.CDF[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.CDF[ii].PLOT.save(FF.PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+
            self.CDF[ii].PLOT.FILECONF,wid=self.cons)

    def _loadfromconf():
    # ==================
      '''Load contour configuration from a file'''
      nn = filedialog.askopenfilename(title='Load contour configuration',
                                      parent=self.Window_contourconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.CDF[ii].PLOT.FILECONF = '%s' % nn
      toconsola('Restoring contour configuration from '+
            self.CDF[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.CDF[ii].PLOT.load(self.CDF[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file '+
              self.CDF[ii].PLOT.FILECONF,wid=self.cons)

    def _saveasconf():
    # ================
      '''Load contour configuration'''
      nn = filedialog.asksaveasfilename(title='Save contour configuration',
                                        parent=self.Window_contourconfig,
                                        initialdir=COSMO_CONF,
                                        confirmoverwrite=True)
      if nn is None or len(nn) == 0:
        return

      
      self.CDF[ii].PLOT.FILECONF = '%s' % nn
      toconsola('Saving contour configuration to '+
            self.CDF[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.CDF[ii].PLOT.save(self.CDF[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+
            self.CDF[ii].PLOT.FILECONF,wid=self.cons)

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
                              varname=self.CDF[ii].varname,
                              units=self.CDF[ii].FLD.units,
                              missing=self.CDF[ii].FLD.missing,
                              minval=self.CDF[ii].FLD.minval,
                              maxval=self.CDF[ii].FLD.maxval,
                              PLOT=self.CDF[ii].PLOT)

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
      toconsola('Restoring line configuration from '+
            self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.FLOAT[ii].PLOT.load(self.FLOAT[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file ',self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)

    def _saveconf():
    # =============
      '''Load line configuration'''
      toconsola('Saving line configuration to '+
            self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.FLOAT[ii].PLOT.save(self.FLOAT[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)

    def _loadfromconf():
    # ==================
      '''Load line configuration from a file'''
      nn = filedialog.askopenfilename(title='Load line configuration',
                                      parent=self.Window_lineconfig,
                                      initialdir=COSMO_CONF)
      if len(nn) == 0:
        return

      self.FLOAT[ii].PLOT.FILECONF = '%s' % nn
      toconsola('Restoring line configuration from '+
            self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.FLOAT[ii].PLOT.load(self.FLOAT[ii].PLOT.FILECONF)
        self.make_plot()
      except:
        toconsola('Error: Unable to load file '+
              self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)

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
      toconsola('Saving line configuration to '+self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)
      try:
        self.FLOAT[ii].PLOT.save(self.FLOAT[ii].PLOT.FILECONF)
      except:
        toconsola('Error: Unable to write file '+self.FLOAT[ii].PLOT.FILECONF,wid=self.cons)

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
          self.VEC[self.FILEORDER[i]].read(wid=self.cons)
          #self.read_UV(self.VEC[self.FILEORDER[i]])
        elif self.FILETYPES[i] == 'FLD':
          self.CDF[self.FILEORDER[i]].L.set(L)
          self.CDF[self.FILEORDER[i]].read(update_lims=False,wid=self.cons)
          #self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
    self.make_plot()

  # ==============
  def tprev(self):
  # ==============
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
            self.VEC[self.FILEORDER[i]].read(wid=self.cons)
            #self.read_UV(self.VEC[self.FILEORDER[i]])
          elif self.FILETYPES[i] == 'FLD':
            L  = self.CDF[self.FILEORDER[i]].L.get()
            Lm = self.CDF[self.FILEORDER[i]].L.get() - 1
            self.CDF[self.FILEORDER[i]].L.set(Lm)
            self.CDF[self.FILEORDER[i]].read(update_lims=False,wid=self.cons)
            #self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
      self.make_plot()
    else:
      return

  # ==============
  def tnext(self):
  # ==============
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
            self.VEC[self.FILEORDER[i]].read(wid=self.cons)
            #self.read_UV(self.VEC[self.FILEORDER[i]])
          elif self.FILETYPES[i] == 'FLD':
            L  = self.CDF[self.FILEORDER[i]].L.get()
            Lp = self.CDF[self.FILEORDER[i]].L.get() + 1
            self.CDF[self.FILEORDER[i]].L.set(Lp)
            self.CDF[self.FILEORDER[i]].read(update_lims=False,wid=self.cons)
            #self.read_CDF(self.CDF[self.FILEORDER[i]],update_lims=False)
      toconsola("EG Drawing next.................",wid=self.cons)
      #print("EG Drawing next.................")
      self.make_plot()
      toconsola("EG next DOne",wid=self.cons)
      #print("EG next DOne")
    else:
      return

  # ====================
  def data_update(self):
  # ====================
    '''Makes the new plot according to the user selections. It call self.read to get the new data'''
    self.read_UV(self.FLD.ncid,self.FLD.icdf,self.FLD.uid,self.FLD.vid)
    self.read_S(self.FLD.ncid,self.FLD.icdf,self.FLD.sid)
    self.make_plot()

  # ===========================
  def get_date(self,ncid,icdf):
  # ===========================
 
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

  # ========================
  def plot_initialize(self):
  # ========================

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
    self.PLOT.PARALLEL_INI.set(np.trunc(self.PLOT.SOUTH.get()/tmp2 - 1)*tmp2)
    self.PLOT.PARALLEL_FIN.set(np.trunc(self.PLOT.NORTH.get()/tmp2 + 1)*tmp2)
    tmp2 = None

  # ==================
  def make_plot(self):
  # ==================
    toconsola("EG make_plot:\n    PLOT.OUTPUT_FIGURE: "+str(self.PLOT.OUTPUT_FIGURE.get()),
              wid=self.cons)
      
    if self.PLOT.OUTPUT_FIGURE.get():
      if self.fig is None:
        toconsola("\n    EGL creation", wid=self.cons)
        self.Window_mapa = tk.Toplevel(self.master)
        self.Window_mapa.title("COSMO-VIEW plotting tool")
        self.Window_mapa.resizable(width=True,height=True)
        self.Window_mapa.grid_columnconfigure(0, weight=1)
        self.Window_mapa.grid_rowconfigure(0, weight=1)
        #self.Window_mapa.wm_geometry("1900x1200")
        
        #self.canvas = None # canvas
        # Frame container
        topframe = tk.Frame(self.Window_mapa)
        topframe.grid_rowconfigure(0, weight=1)
        topframe.grid(sticky='swen')
        topframe.grid_columnconfigure(0, weight=1)	
        # Two panels Utilizamos pack en canvas y grid en consola
        # Afegim el canvas
        top_panel = tk.Frame(topframe, pady = 20)
        # Initialize figure,canvas an Plot panel
        #self.ax=None
        
        self.fig = Figure(figsize=self.PLOT.SIZE, \
           facecolor=self.PLOT.FIGURE_COLOR.get(),dpi=self.PLOT.DPI.get())
        toconsola("    MAP_PLOT: Set projection parameters",wid=self.cons)
        proj = map_proj(self.PLOT.MAP_PROJECTION.get(), params=self.params)
        self.ax = self.fig.add_subplot(111, projection=proj['proj'])
        self.canvas = FigureCanvasTkAgg(self.fig, master=top_panel)
        #EG Dibujamos con self.draw_figure
        #EG self.canvas.draw()
        self.canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)
        toolbar = NavigationToolbar2Tk(self.canvas, top_panel)
        toolbar.update()
        self.canvas.get_tk_widget().pack(side=tk.BOTTOM, fill=tk.BOTH, expand=1)
        
        #EG event controllers
        self.canvas.mpl_connect('button_press_event',self.canvas_click)
        self.canvas.mpl_connect('close_event',self.canvas_closing)
        self.canvas.mpl_connect('resize_event',self.canvas_resizing)
        top_panel.grid(row=0, column=0, sticky='swen')
        
        self.drawmap = True
      else: toconsola("    EG ojo fig existe",wid=self.cons)
      self.draw_figure()

  # ========================
  def setmap(self,target=0):
  # ========================
    #'''EG OJOJ new setmap Routine focused to set the projection
    # We implement a function to manage projection with Cartopy 
    # map_proj(name,list). See tools module
    #'''
    #projection = self.PLOT.MAP_PROJECTION.get()

    #EG self.toconsola("EG Set map Projection")
    #proj = map_proj(projection)
    #self.ax = self.fig.add_subplot(111, projection=proj['proj'])
    #self.ax.set_extent([ float(self.PLOT.WEST.get()), \
    #float(self.PLOT.EAST.get()), float(self.PLOT.SOUTH.get()),  \
    #float(self.PLOT.NORTH.get())],proj['proj'])
    #EG self.ax.coastlines()
    #EG Projection
    '''
    if proj is None:
		proj = map_proj(self.PLOT.MAP_PROJECTION.get())
		
    self.ax = self.fig.add_subplot(111, projection=proj['proj'])
    '''
    return

  # ====================
  def draw_figure(self):
  # ====================
    global CONSOLA
    
    toconsola("EG draw_figure:",wid=self.cons)
    toconsola(("    EG Configuration:\n"+ \
			"\t Projection: "+str(self.PLOT.MAP_PROJECTION.get())+ \
			"\n\t Domain:\t \t West - East: "+str(float(self.PLOT.WEST.get()))+  \
			" - "+str(float(self.PLOT.EAST.get()))+  \
			"\n\t \t South - North: "+str(float(self.PLOT.SOUTH.get()))+  \
			" - "+str(float(self.PLOT.NORTH.get()))),wid=self.cons) 
    try:
      self.scbar.remove()
    except:  pass

    for bar in self.cdfbar:
      try:
         bar.remove()
      except: pass
      
    self.cdfbar = []
      
    proj = map_proj(self.PLOT.MAP_PROJECTION.get())
    self.ax.clear()

    # EPSG
    # EG Not necessary
    # epsg = int(self.PLOT.EPSG.get())
	
    # Este bloque podría rehacerse ahora
    # Temporally deprecated self.PLOT.GEOMAP.get()
    
    self.ax.set_extent([float(self.PLOT.WEST.get()) ,float(self.PLOT.EAST.get()),\
         float(self.PLOT.SOUTH.get()),float(self.PLOT.NORTH.get())],\
         crs=proj['proj'])
    #Eg pruebas con projeccions self.ax.coastlines()
    
    toconsola("    EG self.PLOT.GEOMAP: "+str(self.PLOT.GEOMAP.get()),wid=self.cons)
    if self.drawmap:
      toconsola("    EG draw_figure: call setmap no more needed !",wid=self.cons)
      self.drawmap = False

    #EG We implement GEBCO+EMODNET Tiles services
    toconsola("   EG: RELIEF tiles"+str(self.PLOT.RELIEF_SHOW.get()),wid=self.cons)
    if self.PLOT.RELIEF_SHOW.get():
      if self.PLOT.RELIEF.get() == 1:
        gebco ="GEBCO_2019_Grid"
        try:
          toconsola("\t EG: GEBCO tiles",wid=self.cons)
          self.ax.add_wms(wms='https://www.gebco.net/data_and_products/gebco_web_services/2019/mapserv?request=getmap&service=wms&BBOX=-90,-180,90,360&crs=EPSG:4326&format=image/jpeg&layers=gebco_2019_grid&width=1200&height=600&version=1.3.0',layers=gebco,zorder=0)
        except:
          toconsola("\t WARNING: GEBCO server failed !, it is disabled......",wid=self.cons)  
      elif self.PLOT.RELIEF.get() == 2: 
        emod_land="emodnet:mean_atlas_land"
        toconsola("\t EG: EMODNET tiles",wid=self.cons)  
        try:
          self.ax.add_wms(wms='http://ows.emodnet-bathymetry.eu/wms',layers=emod_land,zorder=0)
        except:
          toconsola("\t WARNING: EMODNET server failed !, it is disabled......",wid=self.cons)
      else:
        #EG Sometimes this situation is possible (i.e. manual edition of conf files)
        self.PLOT.RELIEF_SHOW.set(False)

    if self.PLOT.EMODNET_ISO.get():
      emodnet="emodnet:contours"
      toconsola("\t EG: EMODNET contours",wid=self.cons)
      try:
        self.ax.add_wms(wms='http://ows.emodnet-bathymetry.eu/wms',layers=emodnet,zorder=0)
      except:
        toconsola("\t WARNING: EMODNET contours failed !, it is disabled......",wid=self.cons)
        
    # Draw SAIDIN:
    # 
    if not empty(self.SAIDIN.FILENAME.get()):
      if self.SAIDIN.show.get():
        toconsola("EG plot SAIDIN",wid=self.cons)
        #EG Added projection argument, map reference dropped
        self.scbar = contourplot.drawing(self.fig,self.ax,proj['proj'],
                       self.SAIDIN.FLD.xx, self.SAIDIN.FLD.yy,
                       self.SAIDIN.FLD.data,
                       self.SAIDIN.FLD.data.mask,
                       self.SAIDIN.PLOT)
    # Draw fields:
    # 
    if self.ncdf > 0:
      #EG Added projection argument, map reference dropped	
      toconsola("EG: plot netcdf",wid=self.cons)
      for ii in range(self.ncdf):
        if self.CDF[ii].show.get():
          self.cdfbar.append(contourplot.drawing(self.fig, 
							self.ax, proj['proj'],
							self.CDF[ii].FLD.xx, self.CDF[ii].FLD.yy,
							self.CDF[ii].FLD.data,
							self.CDF[ii].FLD.data.mask,
							self.CDF[ii].PLOT))
    # Draw currents:
    #
    if self.nvec > 0:
      toconsola("EG plot currents",wid=self.cons)
      for ii in range(self.nvec):
        if self.VEC[ii].show.get():
          vectorplot.drawing(self.ax, proj['proj'], self.VEC[ii])
          
    # Draw floats:
    #
    if self.nfloat > 0:
      toconsola("EG plot floats",wid=self.cons)
      for ii in range(self.nfloat):
        self.FLOAT[ii].L.set(self.L.get())
        lagrangian.drawing(self.ax, proj['proj'], self.FLOAT[ii])

    # Draw markers:
    #
    if self.nmarker > 0:
      toconsola("EG plot markers",wid=self.cons)
      for ii in range(self.nmarker): 
        #EG Added projection argument, reference map and fig dropped
        geomarker.drawing(self.ax, proj['proj'], self.MARKER[ii])
        
    # Draw SHAPES:
    #
    if self.nshape > 0:
      toconsola("EG plot shapes",wid=self.cons)
      for ii in range(self.nshape):	
        toconsola("\tSHAPE"+str(ii),wid=self.cons)
        #EG Added projection argument, reference map and fig 
        shape.drawing(self.ax, proj['proj'], self.SHAPE[ii])

    #EG Coastlines
    toconsola("EG: COASTLINES"+str(self.PLOT.COASTLINE_SHOW.get()),wid=self.cons)
    if self.PLOT.COASTLINE_SHOW.get():
      if self.PLOT.COASTLINE_SOURCE.get() == 1:
        emodnet="coastlines"
        try:
          self.ax.add_wms(wms='http://ows.emodnet-bathymetry.eu/wms',layers=emodnet,zorder=0)
        except:
          toconsola("\t WARNING: EMODNET coastlines !, it is disabled......",wid=self.cons)
      else:
        toconsola("\t EG COASTLINE: Natura_Earth (50m by default) or EMODNET wms",wid=self.cons)
        self.ax.coastlines(self.PLOT.MAP_RESOLUTION.get(),color=self.PLOT.COASTLINE_COLOR.get(),
							linewidth=self.PLOT.COASTLINE_WIDTH.get(),zorder=0)

    if self.PLOT.ISOBAT_NPLOT > 0:
      toconsola("EG plot Custom ISOBATHS",wid=self.cons)
      # Plot isobaths and its legend:
      lines, labels = [], []
      toconsola("\t lABEL_SHOW"+str(self.PLOT.ISOBAT_LABEL_SHOW.get()),wid=self.cons)
      for ii in range(self.PLOT.nisobat):
        label = None
        if self.PLOT.ISOBAT_LABEL_SHOW.get():
          label = self.PLOT.ISOBAT_LABEL[ii]
        try:
          color = eval(self.PLOT.ISOBAT_COLOR[ii].get())
        except:
          color = self.PLOT.ISOBAT_COLOR[ii].get()
          
        if self.PLOT.ISOBAT_SHOW[ii]:
          toconsola("\t EG ISOBATA:"+str(self.PLOT.ISOBAT_LABEL[ii]),wid=self.cons)
          z = self.PLOT.ISOBAT_DATA[ii]
          isox,isoy = z['lon'],z['lat']
          for i in range(len(isox)):
            if isox[i] > 1e29:
              isox[i], isoy[i] = np.nan, np.nan
            
          isbt, = self.ax.plot(isox,isoy,marker=None, 
                                linestyle=self.PLOT.ISOBAT_STYLE[ii].get(),
                                linewidth=self.PLOT.ISOBAT_WIDTH[ii].get(),
                                color=color)
          lines.append(isbt)
          labels.append(label)
            
          if self.PLOT.ISOBAT_LEGEND.SHOW.get():
            toconsola("\t self.PLOT.ISOBAT_LEGEND.SHOW"+str(self.PLOT.ISOBAT_LEGEND.SHOW.get()),wid=self.cons)
            fontsize = self.PLOT.ISOBAT_LEGEND.FONTSIZE.get()
            mode = None
            if self.PLOT.ISOBAT_LEGEND.FONTSIZE.get() < 1:
                fontsize = None
            if self.PLOT.ISOBAT_LEGEND.MODE.get() == 1:
                mode = 'expand'
            if not empty(self.PLOT.ISOBAT_LEGEND.TITLE.get()):
              try: pass
              except: pass
      self.ax.legend(lines,labels, \
                     title=self.PLOT.ISOBAT_LEGEND.TITLE.get(),
                     title_fontsize=24,
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
                     labelspacing=self.PLOT.ISOBAT_LEGEND.LABELSPACING.get())

    if self.PLOT.WATER_COLOR.get() != 'None':
      toconsola("PLOT.WATER_COLOR por defecto 50m",wid=self.cons)
      self.ax.add_feature(cfeat.NaturalEarthFeature('physical', 'ocean', \
					self.PLOT.MAP_RESOLUTION.get(), \
					facecolor=self.PLOT.WATER_COLOR.get()),zorder=0)
    if self.PLOT.LAND_COLOR.get() != 'None': 
      toconsola("PLOT.LAND_COLOR por defecto 50m",wid=self.cons)
      self.ax.add_feature(cfeat.NaturalEarthFeature('physical', 'land', \
					self.PLOT.MAP_RESOLUTION.get(), \
					facecolor=self.PLOT.LAND_COLOR.get()),zorder=0)
    if self.PLOT.COUNTRYLINE_SHOW.get():
      toconsola("PLOT.COUNTRYLINE",wid=self.cons)
      self.ax.add_feature(cfeat.BORDERS,edgecolor=self.PLOT.COASTLINE_COLOR.get(),
							linewidth=self.PLOT.COASTLINE_WIDTH.get(),zorder=1)
    if self.PLOT.RIVERS_SHOW.get(): 
      toconsola("PLOT.RIVERS",wid=self.cons)
      self.ax.add_feature(cfeat.NaturalEarthFeature('physical','rivers_and_lakes_centerlines', \
			self.PLOT.MAP_RESOLUTION.get(), \
			linewidth=self.PLOT.RIVERS_WIDTH.get(),
			edgecolor=self.PLOT.RIVERS_COLOR.get(),zorder=0))

    if self.PLOT.GRID_SHOW.get():
      toconsola("EG PLOT.GRID"+self.PLOT.GRID_LINESTYLE.get(),wid=self.cons)
      #EG adaptar falat comprobar
      #def setcolor(x,color):
      #  for m in x:
      #    for t in x[m][1]:
      #      t.set_color(color)
      vmeridians = np.arange(self.PLOT.MERIDIAN_INI.get(), \
                             self.PLOT.MERIDIAN_FIN.get(), \
                             self.PLOT.MERIDIAN_INT.get())
      vparallels = np.arange(self.PLOT.PARALLEL_INI.get(), \
                             self.PLOT.PARALLEL_FIN.get(), \
                             self.PLOT.PARALLEL_INT.get())
      lstyle = {'size':self.PLOT.GRID_SIZE.get(),'color':self.PLOT.GRID_COLOR.get()}
      lstyle = {'size':self.PLOT.GRID_SIZE.get(),'color':self.PLOT.GRID_COLOR.get()}
      gl = self.ax.gridlines(crs=proj['proj'],draw_labels=True,
						linewidth=self.PLOT.GRID_LINEWIDTH.get(),
						color=self.PLOT.GRID_FONTCOLOR.get(),
						alpha=self.PLOT.GRID_ALPHA.get(),
						linestyle=self.PLOT.GRID_LINESTYLE.get())
      # Lines visibility
      gl.xlines, gl.ylines = True, True
      if self.PLOT.GRID_LINESTYLE.get() == "None":
        gl.xlines, gl.ylines = False, False
        
      # xy labels visibility
      gl.xlabels_top = self.PLOT.GRID_NORTH.get()
      gl.xlabels_bottom = self.PLOT.GRID_SOUTH.get()
      gl.ylabels_left = self.PLOT.GRID_WEST.get()
      gl.ylabels_right = self.PLOT.GRID_EAST.get()
      
      gl.xlocator = mticker.FixedLocator(vmeridians)
      gl.ylocator = mticker.FixedLocator(vparallels)
      gl.xlabel_style, gl.ylabel_style = lstyle, lstyle
      gl.xformatter = LONGITUDE_FORMATTER
      gl.xformatter = LONGITUDE_FORMATTER
      gl.xlabel_style, gl.ylabel_style = lstyle, lstyle
      #gl.xpadding , gl.ypadding = self.PLOT.LABEL_PAD.get(), self.PLOT.LABEL_PAD.get()
    else:
      # Default: no labels, no grid just Latitude and Longitude
      toconsola("EG XYLabels ..\n\t"+self.PLOT.XLABEL.get()+" - "+self.PLOT.YLABEL.get(),wid=self.cons)
      font_family = self.PLOT.MAP_FONT_TYPE.get()
      font_size   = self.PLOT.LABEL_SIZE.get()
      font_weight = 'normal'
      font = {'family' : font_family, 'weight' : font_weight,
              'color'  : self.PLOT.TEXT_COLOR.get(),
              'size'   : font_size}
      
      self.ax.text(-0.07, 0.55, self.PLOT.YLABEL.get(), va="bottom", \
					ha="center", rotation="vertical", rotation_mode="anchor",
					transform=self.ax.transAxes,fontdict=font)
      self.ax.text(0.5, -0.2, self.PLOT.XLABEL.get(), va="bottom", \
					ha="center", rotation="horizontal", rotation_mode="anchor",
					transform=self.ax.transAxes,fontdict=font)
    # Title
    toconsola("Plot Title: "+self.PLOT.TITLE.get(),wid=self.cons)
    self.ax.set_title(self.PLOT.TITLE.get(),fontproperties=self.PLOT.TITLEFONT)                  
    px,py = self.ax.title.get_position()
    dy = self.PLOT.TITLE_PAD.get()/self.fig.get_dpi()
    self.ax.title.set_position((px,py+dy))

    if self.PLOT.GEOMAP.get():	
      toconsola("EG PLOT.GEOMAP 2 scale: Not yet implemented",wid=self.cons)
      if self.PLOT.SCALE_SHOW.get():
          try:
            YOFFSET = float(self.PLOT.SCALE_YOFFSET.get())
          except: YOFFSET = None
          try:
            LINEWIDTH = float(self.PLOT.SCALE_LINEWIDTH.get())
          except:  LINEWIDTH = None
          #EG no parecefuncionarojo scale_bar from tools
          toconsola("EG bar scale", wid=self.cons)
          scale_bar(self.ax, 1)
          '''EG To be implemented with Cartopy
          print("EG PLOT.GEOMAP 2 drawmapscale")
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
          ''' 
    # Time stamp
    try:
      self.time_stamp.remove()
    except: pass
    
    if len(self.DATE) > 0:
      toconsola("EG Time stamp: len(self.DATE) > 0", wid=self.cons)
      if self.PLOT.TIMESTAMP_SHOW.get():
        toconsola("EG Time stamp: "+self.DATE[self.L.get()], wid=self.cons)
        font_weight = 'normal'
        if self.PLOT.TIMESTAMP_BOLD.get(): font_weight = 'bold'
   
        self.ax.annotate(self.DATE[self.L.get()], \
					xy=(self.PLOT.TIMESTAMP_X.get(), \
					   self.PLOT.TIMESTAMP_Y.get()), \
					xycoords='figure fraction', \
					color=self.PLOT.TIMESTAMP_COLOR.get(), \
					fontsize=self.PLOT.TIMESTAMP_SIZE.get(), \
					fontfamily=font_family, fontweight=font_weight, \
					annotation_clip=False)
    
    if self.PLOT.LOGO_DISPLAY.get() == 1: self.plot_logo()
     
    self.ax.callbacks.connect('xlim_changed', self.on_xlims_change)
    self.ax.callbacks.connect('ylim_changed', self.on_ylims_change)

    if self.nmarker > 0 and self.PLOT.LEGEND.SHOW.get():
      toconsola("EG self.nmarker ?",wid=self.cons)
      fontsize = self.PLOT.LEGEND.FONTSIZE.get()
      mode = None
      
      if self.PLOT.LEGEND.FONTSIZE.get() < 1: fontsize = None
      if self.PLOT.LEGEND.MODE.get() == 1: mode = 'expand'

      try:
        toconsola("EG ax.legend",wid=self.cons)
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
                        labelspacing=self.PLOT.LEGEND.LABELSPACING.get())
      except: pass

      if not empty(self.PLOT.LEGEND.TITLE.get()):
        try:
          legend.set_title(self.PLOT.LEGEND.TITLE.get(),
                           prop=self.PLOT.LEGEND.TITLEFONT)
        except: pass
    
    self.canvas.draw()
    toconsola("End draw_figure:",wid=self.cons)
    return

  # ============================
  def make_Mplot(self,proj=None):
  # ============================
    '''Plotting the maps using CARTOPY, 
       output directed to Movie window'''
    try:
      self.SAIDIN.Mcbar.remove()
    except:  pass

    try:
      self.Mscbar.remove()
    except:  pass

    for bar in self.Mcdfbar:
      try:
         bar.remove()
      except:  pass
      
    #EG recover the cartopy projection
    if proj is None:
      rproj = map_proj(self.PLOT.MAP_PROJECTION.get())
      proj = rproj['proj']
      
    self.Mcdfbar = []
    self.Max.clear()

    # Deshabilitado temporalmente EPSG
    # epsg = int(self.PLOT.EPSG.get())

    SOUTH = float(self.PLOT.SOUTH.get())
    NORTH = float(self.PLOT.NORTH.get())
    WEST  = float(self.PLOT.WEST.get())
    EAST  = float(self.PLOT.EAST.get())

    if self.Mdrawmap:
      #EG no se necesita mas self.setmap(self.Max,1)
      self.Mdrawmap = False

    toconsola("EG: RELIEF tiles",wid=self.cons)
    #print("EG: RELIEF tiles")
    if self.PLOT.RELIEF_SHOW.get():
      if self.PLOT.RELIEF.get() == 1:
        gebco ="GEBCO_2019_Grid"
        try:
          toconsola("\tEG: GEBCO tiles",wid=self.cons)
          #print("\tEG: GEBCO tiles")
          self.Max.add_wms(wms='https://www.gebco.net/data_and_products/gebco_web_services/2019/mapserv?request=getmap&service=wms&BBOX=-90,-180,90,360&crs=EPSG:4326&format=image/jpeg&layers=gebco_2019_grid&width=1200&height=600&version=1.3.0',layers=gebco,zorder=0)
        except:
          toconsola("\tWARNING: GEBCO server failed !, it is disabled......",wid=self.cons)
          #print("\tWARNING: GEBCO server failed !, it is disabled......")      
      elif self.PLOT.RELIEF.get() == 2: 
        emod_land="emodnet:mean_atlas_land"
        toconsola("\tEG: EMODNET tiles",wid=self.cons)
        #print("\tEG: EMODNET tiles")
        try:
          self.Max.add_wms(wms='http://ows.emodnet-bathymetry.eu/wms',layers=emod_land,zorder=0)
        except:
          toconsola("\tWARNING: EMODNET server failed !, it is disabled......",wid=self.cons)
          #print("\tWARNING: EMODNET server failed !, it is disabled......")
      else:
        #EG Sometimes this situation is possible (i.e. manual edition of conf files)
        self.PLOT.RELIEF_SHOW.set(False)

    if self.PLOT.EMODNET_ISO.get():
      emodnet="emodnet:contours"
      toconsola("EG: EMODNET contours",wid=self.cons)
      #print("EG: EMODNET contours")
      try:
        self.Max.add_wms(wms='http://ows.emodnet-bathymetry.eu/wms',layers=emodnet,zorder=0)
      except:
        toconsola("\t WARNING: EMODNET contours failed !, it is disabled......",wid=self.cons)
        #print("\t WARNING: EMODNET contours failed !, it is disabled......") 
    
    # Draw SAIDIN:
    if not empty(self.SAIDIN.FILENAME.get()):
      self.Mscbar = contourplot.drawing(self.Mfig,self.Max, proj,\
                          self.SAIDIN.FLD.xx,self.SAIDIN.FLD.yy, \
                          self.SAIDIN.FLD.data, \
                          self.SAIDIN.FLD.data.mask, \
                          self.SAIDIN.PLOT)
    # Draw fields:
    if self.ncdf > 0:
      for ii in range(self.ncdf):
        if self.CDF[ii].show.get():
          self.Mcdfbar.append(contourplot.drawing(self.Mfig,self.Max, proj,\
                                    self.CDF[ii].FLD.xx,   \
                                    self.CDF[ii].FLD.yy,   \
                                    self.CDF[ii].FLD.data, \
                                    self.CDF[ii].FLD.data.mask, \
                                    self.CDF[ii].PLOT))

    # Draw currents:
    if self.nvec > 0:
      for ii in range(self.nvec):
        if self.VEC[ii].show.get():
          vectorplot.drawing(self.Max,proj,self.VEC[ii])
    # Draw floats:
    if self.nfloat > 0:
      for ii in range(self.nfloat):
        self.FLOAT[ii].L.set(self.L.get())
        lagrangian.drawing(self.Max,proj,self.FLOAT[ii])
    # Draw markers:
    if self.nmarker > 0:
      for ii in range(self.nmarker):
        geomarker.drawing(self.Max,proj,self.MARKER[ii])

    #EG Coastlines
    toconsola("EG: COASTLINES",wid=self.cons)
    #print("EG: COASTLINES")
    if self.PLOT.COASTLINE_SHOW.get():
      if self.PLOT.COASTLINE_SOURCE.get() == 1:
        emodnet="coastlines"
        try:
          self.Max.add_wms(wms='http://ows.emodnet-bathymetry.eu/wms',layers=emodnet,zorder=0)
        except:
          toconsola("WARNING: EMODNET coastlines !, it is disabled......",wid=self.cons)
          #print("WARNING: EMODNET coastlines !, it is disabled......")
      else:
        toconsola("EG COASTLINE: Natura_Earth (50m by default) or EMODNET wms",wid=self.cons)
        #print("EG COASTLINE: Natura_Earth (50m by default) or EMODNET wms")
        self.Max.coastlines(self.PLOT.MAP_RESOLUTION.get(),color=self.PLOT.COASTLINE_COLOR.get(),
							linewidth=self.PLOT.COASTLINE_WIDTH.get(),zorder=0)

    if self.PLOT.ISOBAT_NPLOT > 0:
      toconsola("EG Custom ISOBATHS",wid=self.cons)
      #print("EG Custom ISOBATHS")
      # Plot isobaths and its legend:
      lines, labels = [], []
      toconsola("\t lABEL_SHOW",self.PLOT.ISOBAT_LABEL_SHOW.get(),wid=self.cons)
      #print("\t lABEL_SHOW",self.PLOT.ISOBAT_LABEL_SHOW.get())
      for ii in range(self.PLOT.nisobat):
        label = None
        if self.PLOT.ISOBAT_LABEL_SHOW.get():
          label = self.PLOT.ISOBAT_LABEL[ii]
        try:
          color = eval(self.PLOT.ISOBAT_COLOR[ii].get())
        except:
          color = self.PLOT.ISOBAT_COLOR[ii].get()
          
        if self.PLOT.ISOBAT_SHOW[ii]:
          toconsola("\t EG ISOBATA:",self.PLOT.ISOBAT_LABEL[ii],wid=self.cons)			
          #print("\t EG ISOBATA:",self.PLOT.ISOBAT_LABEL[ii])
          z = self.PLOT.ISOBAT_DATA[ii]
          isox,isoy = z['lon'],z['lat']
          for i in range(len(isox)):
            if isox[i] > 1e29:
              isox[i], isoy[i] = np.nan, np.nan
            
          isbt, = self.Max.plot(isox,isoy,marker=None, 
                                linestyle=self.PLOT.ISOBAT_STYLE[ii].get(),
                                linewidth=self.PLOT.ISOBAT_WIDTH[ii].get(),
                                color=color)
          lines.append(isbt)
          labels.append(label)
            
          if self.PLOT.ISOBAT_LEGEND.SHOW.get():
            toconsola("\t EG self.PLOT.ISOBAT_LEGEND.SHOW",wid=self.cons)
            #print("\t EG self.PLOT.ISOBAT_LEGEND.SHOW")
            fontsize = self.PLOT.ISOBAT_LEGEND.FONTSIZE.get()
            mode = None
            if self.PLOT.ISOBAT_LEGEND.FONTSIZE.get() < 1:
                fontsize = None
            if self.PLOT.ISOBAT_LEGEND.MODE.get() == 1:
                mode = 'expand'
            if not empty(self.PLOT.ISOBAT_LEGEND.TITLE.get()):
              try: pass
              except: pass
      self.Max.legend(lines,labels, \
                     title=self.PLOT.ISOBAT_LEGEND.TITLE.get(),
                     title_fontsize=24,
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
                     labelspacing=self.PLOT.ISOBAT_LEGEND.LABELSPACING.get())

    if self.PLOT.WATER_COLOR.get() != 'None':
      toconsola("EG PLOT.WATER_COLOR por defecto 50m",wid=self.cons)
      #print("EG PLOT.WATER_COLOR por defecto 50m")
      self.Max.add_feature(cfeat.NaturalEarthFeature('physical', 'ocean', \
					self.PLOT.MAP_RESOLUTION.get(), \
					facecolor=self.PLOT.WATER_COLOR.get()),zorder=0)
    if self.PLOT.LAND_COLOR.get() != 'None': 
      toconsola("EG PLOT.LAND_COLOR por defecto 50m",wid=self.cons)
      #print("EG PLOT.LAND_COLOR por defecto 50m")
      self.Max.add_feature(cfeat.NaturalEarthFeature('physical', 'land', \
					self.PLOT.MAP_RESOLUTION.get(), \
					facecolor=self.PLOT.LAND_COLOR.get()),zorder=0)
    if self.PLOT.COUNTRYLINE_SHOW.get():
      toconsola("EG PLOT.COUNTRYLINE",wid=self.cons)
      #print("EG PLOT.COUNTRYLINE")
      self.Max.add_feature(cfeat.BORDERS,edgecolor=self.PLOT.COASTLINE_COLOR.get(),
							linewidth=self.PLOT.COASTLINE_WIDTH.get(),zorder=1)
    if self.PLOT.RIVERS_SHOW.get(): 
      toconsola("EG PLOT.RIVERS",wid=self.cons)
      #print("EG PLOT.RIVERS")
      self.Max.add_feature(cfeat.NaturalEarthFeature('physical','rivers_and_lakes_centerlines', \
			self.PLOT.MAP_RESOLUTION.get(), \
			linewidth=self.PLOT.RIVERS_WIDTH.get(),
			edgecolor=self.PLOT.RIVERS_COLOR.get(),zorder=0))

    if self.PLOT.GRID_SHOW.get():
      toconsola("EG PLOT.GRID"+str(self.PLOT.GRID_LINESTYLE.get()),wid=self.cons)
      #print("EG PLOT.GRID",self.PLOT.GRID_LINESTYLE.get())
      #EG adaptar falat comprobar
      #def setcolor(x,color):
      #  for m in x:
      #    for t in x[m][1]:
      #      t.set_color(color)
      vmeridians = np.arange(self.PLOT.MERIDIAN_INI.get(), \
                             self.PLOT.MERIDIAN_FIN.get(), \
                             self.PLOT.MERIDIAN_INT.get())
      vparallels = np.arange(self.PLOT.PARALLEL_INI.get(), \
                             self.PLOT.PARALLEL_FIN.get(), \
                             self.PLOT.PARALLEL_INT.get())
      lstyle = {'size':self.PLOT.GRID_SIZE.get(),'color':self.PLOT.GRID_COLOR.get()}
      lstyle = {'size':self.PLOT.GRID_SIZE.get(),'color':self.PLOT.GRID_COLOR.get()}
      gl = self.Max.gridlines(crs=proj,draw_labels=True,
						linewidth=self.PLOT.GRID_LINEWIDTH.get(),
						color=self.PLOT.GRID_FONTCOLOR.get(),
						alpha=self.PLOT.GRID_ALPHA.get(),
						linestyle=self.PLOT.GRID_LINESTYLE.get())
      # Lines visibility
      gl.xlines, gl.ylines = True, True
      if self.PLOT.GRID_LINESTYLE.get() == "None":
        gl.xlines, gl.ylines = False, False
        
      # xy labels visibility
      gl.xlabels_top = self.PLOT.GRID_NORTH.get()
      gl.xlabels_bottom = self.PLOT.GRID_SOUTH.get()
      gl.ylabels_left = self.PLOT.GRID_WEST.get()
      gl.ylabels_right = self.PLOT.GRID_EAST.get()
      
      gl.xlocator = mticker.FixedLocator(vmeridians)
      gl.ylocator = mticker.FixedLocator(vparallels)
      gl.xlabel_style, gl.ylabel_style = lstyle, lstyle
      gl.xformatter = LONGITUDE_FORMATTER
      gl.xformatter = LONGITUDE_FORMATTER
      gl.xlabel_style, gl.ylabel_style = lstyle, lstyle
      #gl.xpadding , gl.ypadding = self.PLOT.LABEL_PAD.get(), self.PLOT.LABEL_PAD.get()
    else:
      # Default: no labels, no grid just Latitude and Longitude
      toconsola("EG XYLabels ..\n\t"+self.PLOT.XLABEL.get()+self.PLOT.YLABEL.get(),wid=self.cons)
      #print("EG XYLabels ..\n\t",self.PLOT.XLABEL.get(),self.PLOT.YLABEL.get())
      font_family = self.PLOT.MAP_FONT_TYPE.get()
      font_size   = self.PLOT.LABEL_SIZE.get()
      font_weight = 'normal'

      font = {'family' : font_family, 'weight' : font_weight,
              'color'  : self.PLOT.TEXT_COLOR.get(),
              'size'   : font_size}
      
      self.Max.text(-0.07, 0.55, self.PLOT.YLABEL.get(), va="bottom", \
					ha="center", rotation="vertical", rotation_mode="anchor",
					transform=self.ax.transAxes,fontdict=font)
      self.Max.text(0.5, -0.2, self.PLOT.XLABEL.get(), va="bottom", \
					ha="center", rotation="horizontal", rotation_mode="anchor",
					transform=self.ax.transAxes,fontdict=font)
    # Title
    toconsola("Title:\n"+self.PLOT.TITLE.get(),wid=self.cons)
    #print("Title:\n",self.PLOT.TITLE.get(),self.PLOT.TITLE_PAD.get())
    self.Max.set_title(self.PLOT.TITLE.get(),fontproperties=self.PLOT.TITLEFONT)                  
    px,py = self.Max.title.get_position()
    dy = self.PLOT.TITLE_PAD.get()/self.fig.get_dpi()
    self.Max.title.set_position((px,py+dy))

    if self.PLOT.GEOMAP.get():	
      toconsola("EG PLOT.GEOMAP 2 scale: Not yet implemented",wid=self.cons)
      #print("EG PLOT.GEOMAP 2 scale: Not yet implemented")
      if self.PLOT.SCALE_SHOW.get():
          try:
            YOFFSET = float(self.PLOT.SCALE_YOFFSET.get())
          except: YOFFSET = None
          try:
            LINEWIDTH = float(self.PLOT.SCALE_LINEWIDTH.get())
          except:  LINEWIDTH = None
          #EG no parecefuncionarojo scale_bar from tools
          toconsola("EG bar scale",wid=self.cons)
          #print("EG bar scale")
          scale_bar(self.Max, 1)

    # Time stamp
    try:
      self.Mtime_stamp.remove()
    except: pass
    
    if len(self.DATE) > 0:
      print("EG Time stamp: len(self.DATE) > 0")
      if self.PLOT.TIMESTAMP_SHOW.get():
        toconsola("EG Time stamp: "+str(self.DATE[self.L.get()]),wid=self.cons)
        #print("EG Time stamp: ", self.DATE[self.L.get()])
        font_weight = 'normal'
        if self.PLOT.TIMESTAMP_BOLD.get(): font_weight = 'bold'
   
        self.Max.annotate(self.DATE[self.L.get()], \
					xy=(self.PLOT.TIMESTAMP_X.get(), \
					   self.PLOT.TIMESTAMP_Y.get()), \
					xycoords='figure fraction', \
					color=self.PLOT.TIMESTAMP_COLOR.get(), \
					fontsize=self.PLOT.TIMESTAMP_SIZE.get(), \
					fontfamily=font_family, fontweight=font_weight, \
					annotation_clip=False)
    
    if self.nmarker > 0 and self.PLOT.LEGEND.SHOW.get():
      fontsize = self.PLOT.LEGEND.FONTSIZE.get()
      mode = None
      if self.PLOT.LEGEND.FONTSIZE.get() < 1: fontsize = None
      if self.PLOT.LEGEND.MODE.get() == 1: mode = 'expand'
      # HASTA AQUI
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
                        labelspacing=self.PLOT.LEGEND.LABELSPACING.get())
      except:  pass

      if not empty(self.PLOT.LEGEND.TITLE.get()):
        try:
          legend.set_title(self.PLOT.LEGEND.TITLE.get(),
                           prop=self.PLOT.LEGEND.TITLEFONT)
        except:
          pass

    if self.PLOT.LOGO_DISPLAY.get() == 1:
      self.plot_logo()  

    self.Max.set_extent([float(self.PLOT.WEST.get()) ,float(self.PLOT.EAST.get()),\
						float(self.PLOT.SOUTH.get()),float(self.PLOT.NORTH.get())],\
						crs=proj)

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
      jeditor.EDITOR(self.Window_editor,wid=self.cons)
    else:
      jeditor.EDITOR(self.Window_editor, \
                     self.FLOAT[self.FLOAT_INDX.get()].FILENAME.get(),\
                     wid=self.cons)

