'''
 Module for plotting contours, built for the COSMO project 
 Quim Ballabrera, May 2017
 Changes: 
  - 06/2020: EGL:
		No more support to python 2.7 
		Support to Basemap deprecated and updated to cartopy
		Limited support to geographical projections. Everything is 
		plotted in PlateCarree. Cartopy projection can be accessed 
		through tools.map_proj()
		A heap variable MESSAGE has been introduce to store "print" messages
'''

import tkinter as tk
from tkinter import ttk

try:
  from tkcolorpicker import askcolor
except:
  from tkColorChooser import askcolor

import datetime
import sys
#import matplotlib
#matplotlib.use('TkAgg')
import matplotlib.cm as cm
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
import matplotlib.pyplot as plt
from matplotlib import rcParams
from mpl_toolkits.axes_grid1 import AxesGrid
from mpl_toolkits.axes_grid1 import make_axes_locatable

import numpy as np
import numpy.ma as ma
import json
import io
import os
from cosmo.tools import colormap_selector 
from cosmo.tools import exists
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA
from cosmo.tools import colsel

#EG To manage cartopy projections
#from cosmo.tools import map_proj

try:
  to_unicode = unicode
except:
  to_unicode = str

# =================
class parameters():
# =============================
  '''Class for contour plots: 
     Valid for either line contours or shaded contours'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__ (self):
  # ==================
    '''Define and initialize the class attributes'''

    self.MESSAGE = ''

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF          = COSMO_CONF + 'contour.conf'

    self.DATA_MIN          = tk.DoubleVar()
    self.DATA_MAX          = tk.DoubleVar()
    self.CONTOUR_MODE      = tk.IntVar()       # 0=Contour, 1=Shades
    self.CONTOUR_LOG       = tk.BooleanVar()
    self.CONTOUR_MIN       = tk.DoubleVar()
    self.CONTOUR_MAX       = tk.DoubleVar()
    self.CONTOUR_INTERVAL  = tk.DoubleVar()
    self.CONTOUR_WIDTH     = tk.DoubleVar()
    self.CONTOUR_DASHEDNEG = tk.BooleanVar()
    self.CONTOUR_LINEMODE  = tk.StringVar()    # 'C'=One color,'M'=colormap
    self.CONTOUR_COLOR     = tk.StringVar()
    self.CONTOUR_COLORMAP  = tk.StringVar()
    self.CONTOUR_REVERSE   = tk.BooleanVar()
    self.ALPHA             = tk.DoubleVar()    # 0=Transparent, 1=Opaque
    self.COLORBAR_SHOW     = tk.BooleanVar()
    self.COLORBAR_LOCATION = tk.StringVar()
    self.COLORBAR_PAD      = tk.DoubleVar()
    self.COLORBAR_LABEL    = tk.StringVar()
    self.COLORBAR_LABELPAD = tk.IntVar()
    self.COLORBAR_SIZE     = tk.StringVar()
    self.COLORBAR_LABELSIZE= tk.IntVar()
    self.COLORBAR_TICKSIZE = tk.IntVar()
    self.COLORBAR_SHRINK   = tk.DoubleVar()
    self.LABEL_SHOW        = tk.BooleanVar()
    self.LABEL_SET         = tk.StringVar()    # 'A'=All, 'V'=Selected Values
    self.LABEL_SIZE        = tk.IntVar()
    self.LABEL_FORMAT      = tk.StringVar()
    self.LABEL_VALUES      = tk.StringVar()

    self.ZORDER            = tk.IntVar()

    # Default attribute values'''
    #
    self.CONTOUR_MODE.set(2)             # Default: Line contours
    self.CONTOUR_LOG.set(False)
    self.CONTOUR_WIDTH.set(1)            # Default: Line width = 1
    self.CONTOUR_DASHEDNEG.set(True)     # Default: Dashed negative values
    self.CONTOUR_LINEMODE.set('M')       # Default: Map color contours
    self.CONTOUR_COLOR.set('black')      # Default: Black contour lines
    self.CONTOUR_COLORMAP.set('jet')     # Default: Jet colormap
    self.CONTOUR_REVERSE.set(False)      # Default: Standard colormap
    self.ALPHA.set(1)                    # Default: Transparency = Opaque
    self.COLORBAR_SHOW.set(True)         # Default: Show colorbar on the right
    self.COLORBAR_LOCATION.set('right')  # Default: Show colorbar on the right
    self.COLORBAR_PAD.set(8)             # Default: Show colorbar on the right
    self.COLORBAR_LABEL.set('')          # Default: Show no colorbar title
    self.COLORBAR_LABELPAD.set(5)        # Default: Show no colorbar title
    self.COLORBAR_SIZE.set('5%')
    self.COLORBAR_LABELSIZE.set(14)
    self.COLORBAR_TICKSIZE.set(12)
    self.LABEL_SHOW.set(True)            # Default: Show contour labels
    self.LABEL_SET.set('A')              # Default: Show All contour labels
    self.LABEL_SIZE.set(12)              # Default: Label character size
    self.LABEL_FORMAT.set(' {:.1f} ')    # Default: One decimal with space
    self.LABEL_VALUES.set('')            # Default: Label character size
    self.ZORDER.set(1)                   # Default: Zorder


    # If configuration file exists, it is read and 
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.

    if exists(self.FILECONF):
      self.MESSAGE += 'Reading CONTOUR configuration from '+self.FILECONF
      try:
        self.load(self.FILECONF)
      except:
        self.MESSAGE += '\nError: Saving default CONTOUR configuration'
        self.save(self.FILECONF)
    else:
      self.MESSAGE += '\nSaving CONTOUR configuration'
      self.save(self.FILECONF)

  def conf_get(self):
  # =================
    ''' Set class dictionary from class attributes'''

    conf = {}
    #conf['FILECONF'] = self.FILECONF
    conf['CONTOUR_MODE'] = self.CONTOUR_MODE.get()
    conf['CONTOUR_LOG'] = self.CONTOUR_LOG.get()
    conf['CONTOUR_MIN'] = self.CONTOUR_MIN.get()
    conf['CONTOUR_MAX'] = self.CONTOUR_MAX.get()
    conf['CONTOUR_INTERVAL'] = self.CONTOUR_INTERVAL.get()
    conf['CONTOUR_WIDTH'] = self.CONTOUR_WIDTH.get()
    conf['CONTOUR_DASHEDNEG'] = self.CONTOUR_DASHEDNEG.get()
    conf['CONTOUR_LINEMODE'] = self.CONTOUR_LINEMODE.get()
    conf['CONTOUR_COLOR'] = self.CONTOUR_COLOR.get()
    conf['CONTOUR_COLORMAP'] = self.CONTOUR_COLORMAP.get()
    conf['CONTOUR_REVERSE'] = self.CONTOUR_REVERSE.get()
    conf['ALPHA'] = self.ALPHA.get()
    conf['COLORBAR_SHOW'] = self.COLORBAR_SHOW.get()
    conf['COLORBAR_LOCATION'] = self.COLORBAR_LOCATION.get()
    conf['COLORBAR_PAD'] = self.COLORBAR_PAD.get()
    conf['COLORBAR_LABEL'] = self.COLORBAR_LABEL.get()
    conf['COLORBAR_LABELPAD'] = self.COLORBAR_LABELPAD.get()
    conf['COLORBAR_SIZE'] = self.COLORBAR_SIZE.get()
    conf['COLORBAR_LABELSIZE'] = self.COLORBAR_LABELSIZE.get()
    conf['COLORBAR_TICKSIZE'] = self.COLORBAR_TICKSIZE.get()
    conf['LABEL_SHOW'] = self.LABEL_SHOW.get()
    conf['LABEL_SET'] = self.LABEL_SET.get()
    conf['LABEL_SIZE'] = self.LABEL_SIZE.get()
    conf['LABEL_FORMAT'] = self.LABEL_FORMAT.get()
    conf['LABEL_VALUES'] = self.LABEL_VALUES.get()
    conf['ZORDER'] = self.ZORDER.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    #self.FILECONF = conf['FILECONF']
    self.CONTOUR_MODE.set(conf['CONTOUR_MODE'])   
    self.CONTOUR_LOG.set(conf['CONTOUR_LOG'])   
    self.CONTOUR_MIN.set(conf['CONTOUR_MIN'])   
    self.CONTOUR_MAX.set(conf['CONTOUR_MAX'])   
    self.CONTOUR_INTERVAL.set(conf['CONTOUR_INTERVAL'])   
    self.CONTOUR_WIDTH.set(conf['CONTOUR_WIDTH'])   
    self.CONTOUR_DASHEDNEG.set(conf['CONTOUR_DASHEDNEG'])   
    self.CONTOUR_LINEMODE.set(conf['CONTOUR_LINEMODE'])   
    self.CONTOUR_COLOR.set(conf['CONTOUR_COLOR'])   
    self.CONTOUR_COLORMAP.set(conf['CONTOUR_COLORMAP'])   
    self.CONTOUR_REVERSE.set(conf['CONTOUR_REVERSE'])   
    self.ALPHA.set(conf['ALPHA'])   
    self.COLORBAR_SHOW.set(conf['COLORBAR_SHOW'])   
    self.COLORBAR_LOCATION.set(conf['COLORBAR_LOCATION'])   
    self.COLORBAR_PAD.set(conf['COLORBAR_PAD'])   
    self.COLORBAR_LABEL.set(conf['COLORBAR_LABEL'])   
    self.COLORBAR_LABELPAD.set(conf['COLORBAR_LABELPAD'])   
    self.COLORBAR_SIZE.set(conf['COLORBAR_SIZE'])   
    self.COLORBAR_LABELSIZE.set(conf['COLORBAR_LABELSIZE'])   
    self.COLORBAR_TICKSIZE.set(conf['COLORBAR_TICKSIZE'])   
    self.LABEL_SHOW.set(conf['LABEL_SHOW'])   
    self.LABEL_SET.set(conf['LABEL_SET'])   
    self.LABEL_SIZE.set(conf['LABEL_SIZE'])   
    self.LABEL_FORMAT.set(conf['LABEL_FORMAT'])   
    self.LABEL_VALUES.set(conf['LABEL_VALUES'])   
    self.ZORDER.set(conf['ZORDER'])   

  def conf_load(self,filename):
  # ============================
    ''' Get class dictionnary from file'''

    conf = json.load(open(filename))
    return conf

  def conf_save(self,conf,filename):
  # ================================
    ''' Save class dictionnary to a file'''

    with io.open(filename,'w',encoding='utf8') as outfile:
      str_ = json.dumps(conf,ensure_ascii=False,    \
                             sort_keys=True,        \
                             indent=2,              \
                             separators=(',',': '))
      outfile.write(to_unicode(str_))

  def load(self,filename):
  # ======================
    ''' Set class attributes from configuration file'''

    conf = json.load(open(filename))
    self.conf_set(conf)

  def save(self,filename):
  # ======================
    ''' Save class attributes to a configuration file'''

    conf = self.conf_get()
    self.conf_save(conf,filename)

# =================================================================
def Configuration(parent,varname,units,missing,minval,maxval,PLOT):
# ======================================================================
  ''' Interactive widget to modify the options of 2D contour plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def mselection():
  # ===============
    ''' Color picker with backup'''
    if PLOT.CONTOUR_COLORMAP.get() == 'None':
      name = colormap_selector(parent=parent)
    else:
      name = colormap_selector(cmap=PLOT.CONTOUR_COLORMAP.get(),parent=parent)
    PLOT.CONTOUR_COLORMAP.set(name)

  frame1 = ttk.Frame(parent,borderwidth=5,padding=5,relief='sunken')
  ttk.Label(frame1,text='Contour field',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  ttk.Label(frame1,text='Variable Name:').grid(row=1,column=0,sticky='w')
  ttk.Label(frame1,text=varname,width=30).grid(row=1,column=1,sticky='w')
  ttk.Label(frame1,text='Units:').grid(row=2,column=0,sticky='w')
  ttk.Label(frame1,text=units,width=30).grid(row=2,column=1,sticky='w')
  ttk.Label(frame1,text='Missing value:').grid(row=3,column=0,sticky='w')
  ttk.Label(frame1,text=missing,width=30).grid(row=3,column=1,sticky='w')
  ttk.Label(frame1,text='Minimum value:').grid(row=4,column=0,sticky='w')
  ttk.Label(frame1,text=minval,width=30).grid(row=4,column=1,sticky='w')
  ttk.Label(frame1,text='Maximum value:').grid(row=5,column=0,sticky='w')
  ttk.Label(frame1,text=maxval,width=30).grid(row=5,column=1,sticky='w')
  frame1.grid()

  frame0 = ttk.Frame(parent,borderwidth=10,padding=2,relief='raised')
  ttk.Radiobutton(frame0,text='Line contour',variable=PLOT.CONTOUR_MODE,value=0).grid(row=0,column=0,columnspan=2)
  ttk.Radiobutton(frame0,text='Filled contour',variable=PLOT.CONTOUR_MODE,value=1).grid(row=0,column=2,columnspan=2)
  ttk.Radiobutton(frame0,text='Pseudo-color',variable=PLOT.CONTOUR_MODE,value=2).grid(row=0,column=4,columnspan=2)
  frame0.grid()

  frame2 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame2,text='Contour minimum:').grid(row=0,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_MIN,width=20).grid(row=0,column=1)
  tk.Checkbutton(frame2,text='Plot Log10',variable=PLOT.CONTOUR_LOG).grid(row=0,column=2,sticky='w')
  ttk.Label(frame2,text='Contour maximum:').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_MAX,width=20).grid(row=1,column=1)
  ttk.Label(frame2,text='Contour interval:').grid(row=2,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_INTERVAL,width=20).grid(row=2,column=1)
  ttk.Label(frame2,text='Line width:').grid(row=3,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_WIDTH,width=7).grid(row=3,column=1,sticky='w')
  tk.Checkbutton(frame2,text='Dashed Negative',variable=PLOT.CONTOUR_DASHEDNEG).grid(row=3,column=2,sticky='w')

  ccol = ttk.Radiobutton(frame2,text='Color',variable=PLOT.CONTOUR_LINEMODE,value='C')
  cmap = ttk.Radiobutton(frame2,text='Colormap',variable=PLOT.CONTOUR_LINEMODE,value='M')
  ccol.grid(row=4,column=0,sticky='w')
  cmap.grid(row=5,column=0,sticky='w')
  
  sclabel = ttk.Style()
  sclabel.configure("sclabel.TLabel",background=PLOT.CONTOUR_COLOR.get(),anchor="center")
  ecol = ttk.Label(frame2,textvariable=PLOT.CONTOUR_COLOR,width=8,style="sclabel.TLabel") # Map Contour Color
  ecol.grid(row=4,column=1,padx=3,sticky='we')
  ttk.Button(frame2,text='Select',command=lambda:colsel(PLOT.CONTOUR_COLOR, \
            sclabel,ecol,"sclabel.TLabel",master=parent)).\
            grid(row=4,column=2,padx=3,sticky='ew')    
  
  emap = ttk.Entry(frame2,textvariable=PLOT.CONTOUR_COLORMAP,justify='left',width=7)  # Map Contour Color
  emap.grid(row=5,column=1,padx=3,sticky='we')
  ttk.Button(frame2,text='Select',command=mselection).grid(row=5,column=2,padx=3,sticky='ew')
  tk.Checkbutton(frame2,text='Reverse',variable=PLOT.CONTOUR_REVERSE).grid(row=5,column=3,sticky='w')
  ttk.Label(frame2,text='Alpha:').grid(row=6,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.ALPHA,width=7).grid(row=6,column=1,sticky='w')
  ttk.Label(frame2,text='Zorder:').grid(row=7,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.ZORDER,width=7).grid(row=7,column=1,sticky='w')
  #ttk.Label(frame2,text='0: transparent; 1: opaque').grid(row=6,column=2,sticky='w')
  frame2.grid()

  frame3 = ttk.Frame(parent,borderwidth=5,padding=5)
  tk.Checkbutton(frame3,text='Show labels',variable=PLOT.LABEL_SHOW).grid(row=0,column=0,sticky='w')
  lall = ttk.Radiobutton(frame3,text='All',variable=PLOT.LABEL_SET,value='A')
  lset = ttk.Radiobutton(frame3,text='Values',variable=PLOT.LABEL_SET,value='V')
  lall.grid(row=1,column=1,sticky='w')
  lset.grid(row=2,column=1,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.LABEL_VALUES,width=30).grid(row=2,column=2,sticky='w')
  ttk.Label(frame3,text='e.g.: 20,21,...',width=15).grid(row=2,column=3,sticky='w')
  ttk.Label(frame3,text='Label size:').grid(row=3,column=1,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.LABEL_SIZE,width=7).grid(row=3,column=2,sticky='w')
  ttk.Label(frame3,text='Label format:').grid(row=4,column=1,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.LABEL_FORMAT,width=7).grid(row=4,column=2,sticky='w')
  ttk.Label(frame3,text='e.g.: {:.0f} ',width=15).grid(row=4,column=3,sticky='w')

  tk.Checkbutton(frame3,text='Show colorbar',variable=PLOT.COLORBAR_SHOW).grid(row=6,column=0,sticky='w')
  ttk.Label(frame3,text='Label').grid(row=7,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_LABEL,width=30).grid(row=7,column=2,sticky='w')
  ttk.Label(frame3,text='Labelpad').grid(row=8,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_LABELPAD,width=7).grid(row=8,column=2,sticky='w')
  ttk.Label(frame3,text='Location').grid(row=9,column=1,sticky='w')
  cb = ttk.Combobox(frame3,textvariable=PLOT.COLORBAR_LOCATION,width=12)
  cb.grid(row=9,column=2,sticky='w')
  cb['values']=('left','right','bottom','top')
  ttk.Label(frame3,text='Colorbar Padding').grid(row=10,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_PAD,width=7).grid(row=10,column=2,sticky='w')
  ttk.Label(frame3,text='Colorbar Size').grid(row=11,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_SIZE,width=7).grid(row=11,column=2,sticky='w')
  ttk.Label(frame3,text='Label Size').grid(row=12,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_LABELSIZE,width=7).grid(row=12,column=2,sticky='w')
  ttk.Label(frame3,text='Tick Size').grid(row=13,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_TICKSIZE,width=7).grid(row=13,column=2,sticky='w')
  frame3.grid()

# ============================================
def drawing(fig,ax,proj,X,Y,IFIELD,MASK,PLOT):
# =================================================
  ''' Draw a 2D contour plot. Option of lines or filled contours'''

  __version__ = "2.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2019"

  _levels = np.arange(PLOT.CONTOUR_MIN.get(),                            \
                      PLOT.CONTOUR_MAX.get()+PLOT.CONTOUR_INTERVAL.get(), \
                      PLOT.CONTOUR_INTERVAL.get())

  FIELD = IFIELD.copy()

  if PLOT.DATA_MIN.get() < PLOT.CONTOUR_MIN.get() :
    FIELD[FIELD<PLOT.CONTOUR_MIN.get()] = PLOT.CONTOUR_MIN.get()
  if PLOT.DATA_MAX.get() > PLOT.CONTOUR_MAX.get() :
    FIELD[FIELD>PLOT.CONTOUR_MAX.get()] = PLOT.CONTOUR_MAX.get()

  if PLOT.CONTOUR_LOG.get():
    FIELD = np.log10(FIELD)
  
  if MASK is not None:
    FIELD = ma.array(FIELD,mask=MASK)  

  # Colormap: Direct or Reversed
  if PLOT.CONTOUR_REVERSE.get():
    cmap=cm.get_cmap(PLOT.CONTOUR_COLORMAP.get()+'_r')
  else:
    cmap=cm.get_cmap(PLOT.CONTOUR_COLORMAP.get())


  if PLOT.CONTOUR_MODE.get() == 0:
    # ----------------------------- CONTOURS
    if PLOT.CONTOUR_LINEMODE.get() == 'M':    # colormapped contours
      PLOT.MESSAGE += "EG Contouring CONTOURS M"
      #print("EG Contouring CONTOURS M")
      _cl = ax.contour(X,Y,FIELD, \
                         linewidths=PLOT.CONTOUR_WIDTH.get(), \
                         levels=_levels, \
                         cmap=cmap, \
                         zorder=PLOT.ZORDER.get(), \
                         transform=proj, \
                         alpha=PLOT.ALPHA.get())
    else:
      if PLOT.CONTOUR_DASHEDNEG.get():
        rcParams['contour.negative_linestyle'] = 'dashed'
      else:
        rcParams['contour.negative_linestyle'] = 'solid'

      _cl = ax.contour(X,Y,FIELD,                                   \
                       linewidths=PLOT.CONTOUR_WIDTH.get(),         \
                       levels=_levels,                              \
                       zorder=PLOT.ZORDER.get(), \
                       transform=proj, \
                       colors=PLOT.CONTOUR_COLOR.get(),             \
                       alpha=PLOT.ALPHA.get())

    # About contour labels
    if PLOT.LABEL_SHOW.get():
      if PLOT.LABEL_SET.get() == 'A':   # Label all contours
        ax.clabel(_cl,_cl.levels,inline=1, \
                  fontsize=PLOT.LABEL_SIZE.get(), \
                  fmt=PLOT.LABEL_FORMAT.get().format)
      else:
        l1 = str(PLOT.LABEL_VALUES.get())
        label_list = [float(ll) for ll in l1.split(',')]
        ax.clabel(_cl,label_list,inline=1, \
                  fontsize=PLOT.LABEL_SIZE.get(), \
                  fmt=PLOT.LABEL_FORMAT.get().format)

  elif PLOT.CONTOUR_MODE.get() == 1:
    # ----------------------------- SHADED
    PLOT.MESSAGE += "EG Contouring CONTOUR_DASHEDNE G"
    #print("EG Contouring CONTOUR_DASHEDNE G")
    #EG divider give errors, divider is for colorbar
    divider = make_axes_locatable(ax)
    _cf = ax.contourf(X,Y,FIELD,
        levels=_levels,
        cmap=cmap,
        zorder=PLOT.ZORDER.get(), 
        transform=proj, 
        alpha=PLOT.ALPHA.get())
      
    cbar = None
    if PLOT.COLORBAR_SHOW.get():
      padding = PLOT.COLORBAR_PAD.get()/100
      if PLOT.COLORBAR_LOCATION.get() == 'right':
            location = 'right'
            orientation = 'vertical'
      elif PLOT.COLORBAR_LOCATION.get() == 'left':
            location = 'left'
            orientation = 'vertical'
      elif PLOT.COLORBAR_LOCATION.get() == 'bottom':
            location = 'bottom'
            orientation = 'horizontal'
      else:
            location = 'top'
            orientation = 'horizontal'
            
      # EG divider gives cartopy errors
      cax = divider.append_axes(location,size="5%",pad=0.1,axes_class=plt.Axes)
      fig.add_axes(cax)
      
      cbar = fig.colorbar(_cf, cax=cax, orientation=orientation)
      cbar.ax.tick_params(labelsize=PLOT.COLORBAR_TICKSIZE.get())
      cbar.set_label(PLOT.COLORBAR_LABEL.get(),
			labelpad=PLOT.COLORBAR_LABELPAD.get(),
			size=PLOT.COLORBAR_LABELSIZE.get())
    return cbar

  else:
    # ----------------------------- COLORMESH
    PLOT.MESSAGE += "EG Contouring COLORMESH"
    #print("EG Contouring COLORMESH")
	# EG divider gives cartopy errors
    divider = make_axes_locatable(ax)
    _cp = ax.pcolormesh(X,Y,FIELD,                        \
                         cmap=cmap,                         \
                         zorder=PLOT.ZORDER.get(), \
                         transform=proj, \
                         vmin=PLOT.CONTOUR_MIN.get(),       \
                         vmax=PLOT.CONTOUR_MAX.get(),       \
                         alpha=PLOT.ALPHA.get())
    cbar = None
    if PLOT.COLORBAR_SHOW.get():
      padding = PLOT.COLORBAR_PAD.get()/100
      if PLOT.COLORBAR_LOCATION.get() == 'right':
          location = 'right'
          orientation = 'vertical'
      elif PLOT.COLORBAR_LOCATION.get() == 'left':
          location = 'left'
          orientation = 'vertical'
      elif PLOT.COLORBAR_LOCATION.get() == 'bottom':
          location = 'bottom'
          orientation = 'horizontal'
      else:
          location = 'top'
          orientation = 'horizontal'
      # EG 
      cax = divider.append_axes(location,size=PLOT.COLORBAR_SIZE.get(),pad=padding,axes_class=plt.Axes)
      fig.add_axes(cax)   

      cbar = fig.colorbar(_cp, cax=cax, orientation=orientation)
      cbar.ax.tick_params(labelsize=PLOT.COLORBAR_TICKSIZE.get())
      cbar.set_label(PLOT.COLORBAR_LABEL.get(),
                     labelpad=PLOT.COLORBAR_LABELPAD.get(),
                     size=PLOT.COLORBAR_LABELSIZE.get())      
    return cbar
