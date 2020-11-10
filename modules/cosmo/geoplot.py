''' Module for setting plotting characteristics on shape files, built 
for the COSMO project. 
#   Emili Garcia, June 2020: Version entirely based on dotplot() class
	EGL, 06/2020: Changes:
		No support to python 2.X 
		Limited support to loading and drawing shapefiles. Shapefiles
		may contain a large number of complex geometries, so support
		for individual gemetries is not implemented. 
		All color selections are managed through tools.colsel() function
		Cartopy projection can be accessed through tools.map_proj()
		A heap variable MESSAGE has been introduce to store "print" messages
	EGL, 07/2020: Changes:
		Corrected minor bug with face and edge color selection
'''
import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
#from tkcolorpicker import askcolor
from tkinter.colorchooser import askcolor
from tkinter import font as tkfont

import json
import os
import io
try:
  to_unicode = unicode
except:
  to_unicode = str

from cosmo.tools import empty
from cosmo.tools import exists
from cosmo.tools import colsel

from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA

# ============================
class parameters():
# ============================
  '''Class for plotting geometries'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2018"

  def __init__ (self):
  # ==================
    '''Define and initialize class attributes'''

    self.MESSAGE = ''
    
    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF          = COSMO_CONF + 'geo.conf'

    self.SHOW              = tk.BooleanVar()
    self.SIZE              = tk.DoubleVar()
    self.SYMBOL            = tk.StringVar()
    self.COLOR             = tk.StringVar()
    self.EDGECOLOR         = tk.StringVar()
    self.FACECOLOR         = tk.StringVar()
    self.LINEWIDTH         = tk.DoubleVar()
    self.ALPHA             = tk.DoubleVar()

    self.HA                = tk.StringVar()
    self.VA                = tk.StringVar()
    self.WRAP              = tk.BooleanVar()
    self.TCOLOR            = tk.StringVar()
    self.STYLE             = tk.StringVar()
    self.WEIGHT            = tk.StringVar()
    self.ZORDER            = tk.IntVar()
    self.TSIZE             = tk.DoubleVar()
    self.ANGLE             = tk.DoubleVar()
    self.XPAD              = tk.DoubleVar()
    self.YPAD              = tk.DoubleVar()
    self.BBOX              = tk.BooleanVar()
    self.BBOX_FACECOLOR    = tk.StringVar()
    self.BBOX_ALPHA        = tk.DoubleVar()

    # Default attribute values:
    #
    self.SHOW.set(True)
    self.SIZE.set(1.5)
    self.SYMBOL.set('o')
    self.COLOR.set('black')
    self.ALPHA.set(1.0)
    self.EDGECOLOR.set('blue')
    self.FACECOLOR.set('aqua')
    self.LINEWIDTH.set(1.0)
    self.HA.set('left')
    self.VA.set('baseline')
    self.WRAP.set(True)
    self.TCOLOR.set('black')
    self.STYLE.set('normal')
    self.WEIGHT.set('bold')
    self.ZORDER.set(1)
    self.TSIZE.set(10)
    self.ANGLE.set(0)
    self.XPAD.set(0.1)
    self.YPAD.set(0)
    self.BBOX.set(False)
    self.BBOX_FACECOLOR.set('white')
    self.BBOX_ALPHA.set(0.5)

    # If configuration file exists, it is read and
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.
    #
    if exists(self.FILECONF):
      try:
        self.MESSAGE += 'Loading GEO configuration'
        #print('Loading GEO configuration')
        self.load(self.FILECONF)
      except:
        self.MESSAGE += 'Error: Saving default GEO configuration'
        #print('Error: Saving default GEO configuration')
        self.save(self.FILECONF)
    else:
      self.MESSAGE += 'Saving default GEO configuration'
      #print('Saving default GEO configuration')
      self.save(self.FILECONF)

  def conf_get(self):
  # =================
    ''' Set class dictionnary from class attributes'''

    conf = {}
    conf['SHOW'] = self.SHOW.get()
    conf['SIZE'] = self.SIZE.get()
    conf['SYMBOL'] = self.SYMBOL.get()
    conf['COLOR'] = self.COLOR.get()
    conf['ALPHA'] = self.ALPHA.get()
    conf['EDGECOLOR'] = self.EDGECOLOR.get()
    conf['FACECOLOR'] = self.FACECOLOR.get()
    conf['LINEWIDTH'] = self.LINEWIDTH.get()
    conf['HA'] = self.HA.get()
    conf['VA'] = self.VA.get()
    conf['WRAP'] = self.WRAP.get()
    conf['TCOLOR'] = self.TCOLOR.get()
    conf['STYLE'] = self.STYLE.get()
    conf['WEIGHT'] = self.WEIGHT.get()
    conf['ZORDER'] = self.ZORDER.get()
    conf['TSIZE'] = self.TSIZE.get()
    conf['ANGLE'] = self.ANGLE.get()
    conf['XPAD'] = self.XPAD.get()
    conf['YPAD'] = self.YPAD.get()
    conf['BBOX'] = self.BBOX.get()
    conf['BBOX_FACECOLOR'] = self.BBOX_FACECOLOR.get()
    conf['BBOX_ALPHA'] = self.BBOX_ALPHA.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    self.SHOW.set(conf['SHOW'])
    self.SIZE.set(conf['SIZE'])
    self.SYMBOL.set(conf['SYMBOL'])
    self.COLOR.set(conf['COLOR'])
    self.ALPHA.set(conf['ALPHA'])
    self.EDGECOLOR.set(conf['EDGECOLOR'])
    self.FACECOLOR.set(conf['FACECOLOR'])
    self.LINEWIDTH.set(conf['LINEWIDTH'])
    self.HA.set(conf['HA'])
    self.VA.set(conf['VA'])
    self.WRAP.set(conf['WRAP'])
    self.TCOLOR.set(conf['TCOLOR'])
    self.STYLE.set(conf['STYLE'])
    self.WEIGHT.set(conf['WEIGHT'])
    self.ZORDER.set(conf['ZORDER'])
    self.TSIZE.set(conf['TSIZE'])
    self.ANGLE.set(conf['ANGLE'])
    self.XPAD.set(conf['XPAD'])
    self.YPAD.set(conf['YPAD'])
    self.BBOX.set(conf['BBOX'])
    self.BBOX_FACECOLOR.set(conf['BBOX_FACECOLOR'])
    self.BBOX_ALPHA.set(conf['BBOX_ALPHA'])


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

# ========================================
def Configuration(parent,PLOT):
# ========================================
  ''' Interactive widget to modify the options of Dot plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "May 2018"

  # Main widget
  # ===========

  # Symbol list using standard and utf technical symbols
  #
  SYMBOL_LIST = ['.',',','o','v','^','<','>', \
                '1','2','3','4','s','p','*',  \
                'h','H','+','x','D','d','|','_']
  for n in range(ord('\u2600'),ord('\u26B8')+1):
    SYMBOL_LIST.append(chr(n))

  font = tkfont.Font(font='TkDefaultFont').copy()
  font_bold = tkfont.Font(font='TkDefaultFont').copy()
  font_bold['weight']='bold'
  sdlabel = ttk.Style()
  sdlabel.configure("sdlabel.TLabel",background=PLOT.COLOR.get(),anchor="center")
  selabel = ttk.Style()
  selabel.configure("selabel.TLabel",background=PLOT.EDGECOLOR.get(),anchor="center")
  sflabel = ttk.Style()
  sflabel.configure("sflabel.TLabel",background=PLOT.FACECOLOR.get(),anchor="center")
  
  F0 = ttk.Frame(parent,borderwidth=5,padding=5)
  tk.Checkbutton(F0,text='Show geometries',font=font, variable=PLOT.SHOW). \
            grid(row=0,column=0,columnspan=6,sticky='ew')
  
  ttk.Label(F0,text='Polygons',font=font).grid(row=1,column=0,sticky='w')
  ttk.Label(F0,text='Edge Color').grid(row=2,padx=3,column=0,sticky='w')
  ELabel = ttk.Label(F0,textvariable=PLOT.EDGECOLOR,width=8,style="selabel.TLabel")
  ELabel.grid(row=2,padx=3,column=1)
  ttk.Button(F0,text='Select',command=lambda:colsel(PLOT.EDGECOLOR, \
            selabel,ELabel,"selabel.TLabel",master=parent)).\
            grid(row=2,column=2,padx=3,sticky='ew')
            
  ttk.Label(F0,text='Face Color').grid(row=3,padx=3,column=0,sticky='w')
  FLabel = ttk.Label(F0,textvariable=PLOT.FACECOLOR,width=8,style="sflabel.TLabel")
  FLabel.grid(row=3,column=1)
  ttk.Button(F0,text='Select',command=lambda:colsel(PLOT.FACECOLOR, \
            sflabel,FLabel,"sflabel.TLabel",master=parent)).\
            grid(row=3,column=2,padx=3,sticky='ew')
  
  ttk.Label(F0,text='Points',font=font).grid(row=1,column=3,sticky='w')
  ttk.Label(F0,text='Symbol').grid(row=2,column=3,padx=3,columnspan=1,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.SYMBOL, \
               values=SYMBOL_LIST,width=8).grid(row=2,column=4,padx=3,sticky='w')
  ttk.Label(F0,text='Size').grid(row=3,column=3,padx=3,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.SIZE,width=8).grid(row=3,column=4,sticky='w')
  ttk.Label(F0,text='Color').grid(row=4,column=3,padx=3,sticky='w')
  DLabel = ttk.Label(F0,textvariable=PLOT.COLOR,width=8,style="sdlabel.TLabel")
  DLabel.grid(row=4,column=4,padx=3)
  ttk.Button(F0,text='Select',command=lambda:colsel(PLOT.COLOR, \
            sdlabel,DLabel,"sdlabel.TLabel",master=parent)).\
            grid(row=4,column=5,sticky='ew')
            
  fs0 = ttk.Frame(F0)
  ttk.Label(fs0,text='Alpha').grid(row=0,column=1,padx=3)
  ttk.Entry(fs0,textvariable=PLOT.ALPHA,width=8).grid(row=0,column=2,padx=3)
  
  ttk.Label(fs0,text='Zorder').grid(row=0,column=3,padx=3)
  ttk.Entry(fs0,textvariable=PLOT.ZORDER,width=8).grid(row=0,column=4,padx=3)
  fs0.grid(row=5,column=0,columnspan=6,sticky='we',pady=10)         
  F0.grid()


