# Module for configuring legends for the COSMO project 
# Quim Ballabrera, September 2018

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkcolorpicker import askcolor
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  from tkColorChooser import askcolor

import json
import os
import io

import matplotlib.pyplot as plt
import matplotlib.image as image
import matplotlib.font_manager
import ast
import math
from matplotlib.font_manager import FontProperties

try:
  to_unicode = unicode
except:
  to_unicode = str

from cosmo.tools import empty
from cosmo.tools import exists
from cosmo.tools import fontconfig
from cosmo.tools import setfont
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA


# ====================
class LegendConfig():
# ====================
  '''Class for legend configuration'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "September 2018"

  def __init__(self):
  # =================
    '''Define and initialize class attributes'''

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF    = COSMO_CONF + 'legend.conf'
 
    self.SHOW        = tk.BooleanVar()
    self.LOC         = tk.IntVar()
    self.MODE        = tk.IntVar()
    self.NCOL        = tk.IntVar()
    self.FONTSIZE    = tk.IntVar()
    self.FANCYBOX    = tk.BooleanVar()
    self.FRAMEON     = tk.BooleanVar()
    self.SHADOW      = tk.BooleanVar()
    self.TITLE       = tk.StringVar()
    self.ALPHA       = tk.DoubleVar()
    self.COLOR       = tk.StringVar()
    self.EDGECOLOR   = tk.StringVar()
    self.MARKERSCALE = tk.DoubleVar()
    self.LABELSPACING = tk.DoubleVar()
    self.BORDERPAD = tk.DoubleVar()
    self.HANDLETEXTPAD = tk.DoubleVar()
    self.BORDERAXESPAD = tk.DoubleVar()
    self.TITLEFONT   = FontProperties().copy()

    # Default values:
    self.SHOW.set(True)
    self.LOC.set(0)
    self.MODE.set(0)
    self.NCOL.set(1)
    self.FONTSIZE.set(12)
    self.FANCYBOX.set(True)
    self.FRAMEON.set(True)
    self.SHADOW.set(True)
    self.TITLE.set('')
    self.ALPHA.set(1)
    self.COLOR.set('white')
    self.EDGECOLOR.set(None)

    self.MARKERSCALE.set(matplotlib.rcParams['legend.markerscale'])
    self.LABELSPACING.set(matplotlib.rcParams['legend.labelspacing'])
    self.BORDERPAD.set(matplotlib.rcParams['legend.borderpad'])
    self.HANDLETEXTPAD.set(matplotlib.rcParams['legend.handletextpad'])
    self.BORDERAXESPAD.set(matplotlib.rcParams['legend.borderaxespad'])
    self.TITLEFONT.set_weight('bold')

    # If configuration file exists, it is read and
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.
    #
    if exists(self.FILECONF):
      try:
        print('Loading LEGEND configuration')
        self.load(self.FILECONF)
      except:
        print('Error: Saving default LEGEND configuration')
        self.save(self.FILECONF)
    else:
      print('Saving default LEGEND configuration')
      self.save(self.FILECONF)


  def conf_get(self):
  # ======================
    '''Get the conf dictionary from program variables'''
    conf = {}
    conf['ALPHA'] = self.ALPHA.get()
    conf['COLOR'] = self.COLOR.get()
    conf['FANCYBOX'] = self.FANCYBOX.get()
    conf['FONTSIZE'] = self.FONTSIZE.get()
    conf['FRAMEON'] = self.FRAMEON.get()
    conf['LOC'] = self.LOC.get()
    conf['MODE'] = self.MODE.get()
    conf['NCOL'] = self.NCOL.get()
    conf['SHADOW'] = self.SHADOW.get()
    conf['SHOW'] = self.SHOW.get()
    conf['TITLE'] = self.TITLE.get()
    try:
      conf['EDGECOLOR'] = self.EDGECOLOR.get()
    except:
      conf['EDGECOLOR'] = None
    conf['LABELSPACING'] = self.LABELSPACING.get()
    conf['BORDERPAD'] = self.BORDERPAD.get()
    conf['HANDLETEXTPAD'] = self.HANDLETEXTPAD.get()
    conf['BORDERAXESPAD'] = self.BORDERAXESPAD.get()
    conf['MARKERSCALE'] = self.MARKERSCALE.get()
    conf['TITLEFONT'] = self.TITLEFONT.__dict__
    return conf

  def conf_set(self,conf):
  # =======================
    '''Set program variables from the conf dictionnary'''
    self.ALPHA.set(conf['ALPHA'])
    self.COLOR.set(conf['COLOR'])
    self.FANCYBOX.set(conf['FANCYBOX'])
    self.FONTSIZE.set(conf['FONTSIZE'])
    self.FRAMEON.set(conf['FRAMEON'])
    self.LOC.set(conf['LOC'])
    self.MODE.set(conf['MODE'])
    self.NCOL.set(conf['NCOL'])
    self.SHADOW.set(conf['SHADOW'])
    self.SHOW.set(conf['SHOW'])
    self.TITLE.set(conf['TITLE'])
    if conf['EDGECOLOR'].upper() == 'NONE':
      self.EDGECOLOR.set(None)
    else:
      self.EDGECOLOR.set(conf['EDGECOLOR'])
    self.LABELSPACING.set(conf['LABELSPACING'])
    self.BORDERPAD.set(conf['BORDERPAD'])
    self.HANDLETEXTPAD.set(conf['HANDLETEXTPAD'])
    self.BORDERAXESPAD.set(conf['BORDERAXESPAD'])
    self.MARKERSCALE.set(conf['MARKERSCALE'])
    self.TITLEFONT = setfont(conf['TITLEFONT'])

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

  def Winconfig(self,parent):
  # =========================

    LEGEND_LOCATION_LIST = ['best','upper right','upper left','lower left', \
                     'lower right', 'right', 'center left', 'center right', \
                     'lower center', 'upper center', 'center']
    LEGEND_MODE_LIST = ['None','expand']

    def legend_location():
    # =============
      ''' Process the location combobox'''
      location_name = loc.get()
      self.LOC.set(LEGEND_LOCATION_LIST.index(location_name))

    def legend_mode():
    # =============
      ''' Process the location combobox'''
      mode_name = mod.get()
      self.MODE.set(LEGEND_MODE_LIST.index(mode_name))



    f5 = ttk.Frame(parent,borderwidth=5,padding=5)
    ttk.Label(f5,text='Show').grid(row=0,column=0,padx=3)
    ttk.Checkbutton(f5,
                    variable=self.SHOW).grid(row=0,
                                             column=1,
                                             padx=3,
                                             sticky='w')
    f5.grid()

    f6 = ttk.Frame(parent,borderwidth=5,padding=5)
    ttk.Label(f6,
              text='Legend title').grid(row=10,column=0,padx=3)
    ttk.Entry(f6,
              textvariable=self.TITLE,
              width=30).grid(row=10,column=1,columnspan=2,padx=3,sticky='w')

    def titleprop():
      self.TITLEFONT = fontconfig(font=self.TITLEFONT,
                                          sample=self.TITLE.get())

    ttk.Label(f6,
              text='Title font').grid(row=11,column=0,padx=3)
    ttk.Button(f6,
               text='View',
               command=titleprop).grid(row=11,column=1,padx=3)
    f6.grid()

    f7 = ttk.Frame(parent,borderwidth=5,padding=5)
    ttk.Label(f7,text='Location').grid(row=1,column=0,padx=3)
    loc = ttk.Combobox(f7,values=LEGEND_LOCATION_LIST)
    loc.grid(row=1,column=1,padx=3,sticky='w')
    loc.set(LEGEND_LOCATION_LIST[self.LOC.get()])
    loc.bind('<<ComboboxSelected>>',lambda e: legend_location())

    ttk.Label(f7,text='Num columns').grid(row=2,column=0,padx=3)
    ttk.Entry(f7,textvariable=self.NCOL,width=10).grid(row=2,column=1,padx=3,sticky='w')

    ttk.Label(f7,text='MODE').grid(row=3,column=0,padx=3)
    mod = ttk.Combobox(f7,values=LEGEND_MODE_LIST)
    mod.grid(row=3,column=1,padx=3,sticky='w')
    mod.set(LEGEND_MODE_LIST[self.MODE.get()])
    mod.bind('<<ComboboxSelected>>',lambda e: legend_mode())
    f7.grid()


    f8 = ttk.Frame(parent,borderwidth=5,padding=5)
    ttk.Label(f8,
              text='Font size').grid(row=0,
                                     column=0,
                                     padx=3)
    ttk.Entry(f8,
              textvariable=self.FONTSIZE,
              width=10).grid(row=0,
                             column=1,
                             padx=3,
                             sticky='w')
    ttk.Label(f8,
              text='Default: < 1').grid(row=0,
                                        column=2,
                                        padx=3,
                                        sticky='w')


    ttk.Label(f8,
              text='Marker scale').grid(row=1,
                                     column=0,
                                     padx=3)
    ttk.Entry(f8,
              textvariable=self.MARKERSCALE,
              width=10).grid(row=1,
                             column=1,
                             padx=3,
                             sticky='w')

    ttk.Label(f8,
              text='Label spacing').grid(row=2,
                                     column=0,
                                     padx=3)
    ttk.Entry(f8,
              textvariable=self.LABELSPACING,
              width=10).grid(row=2,
                             column=1,
                             padx=3,
                             sticky='w')
    ttk.Label(f8,
              text='Handle text pad').grid(row=3,
                                     column=0,
                                     padx=3)
    ttk.Entry(f8,
              textvariable=self.HANDLETEXTPAD,
              width=10).grid(row=3,
                             column=1,
                             padx=3,
                             sticky='w')
    ttk.Label(f8,
              text='Border pad spacing').grid(row=4,
                                     column=0,
                                     padx=3)
    ttk.Entry(f8,
              textvariable=self.BORDERPAD,
              width=10).grid(row=4,
                             column=1,
                             padx=3,
                             sticky='w')
    ttk.Label(f8,
              text='Border axes pad').grid(row=5,
                                     column=0,
                                     padx=3)
    ttk.Entry(f8,
              textvariable=self.BORDERAXESPAD,
              width=10).grid(row=5,
                             column=1,
                             padx=3,
                             sticky='w')
    f8.grid()

    

    f9 = ttk.Frame(parent,borderwidth=5,padding=5)
    ttk.Label(f9,text='Show frame').grid(row=5,column=0,padx=3)
    ttk.Checkbutton(f9,variable=self.FRAMEON).grid(row=5,column=1,padx=3,sticky='w')
    ttk.Label(f9,text='Fancy box').grid(row=6,column=0,padx=3)
    ttk.Checkbutton(f9,variable=self.FANCYBOX).grid(row=6,column=1,padx=3,sticky='w')
    ttk.Label(f9,text='Frame shadow').grid(row=7,column=0,padx=3)
    ttk.Checkbutton(f9,variable=self.SHADOW).grid(row=7,column=1,padx=3,sticky='w')
    ttk.Label(f9,text='Frame color').grid(row=8,column=0,padx=3)
    ttk.Entry(f9,textvariable=self.COLOR,width=10).grid(row=8,column=1,padx=3,sticky='w')
    ttk.Label(f9,
              text='Edge color').grid(row=9,
                                     column=0,
                                     padx=3)
    ttk.Entry(f9,
              textvariable=self.EDGECOLOR,
              width=10).grid(row=9,
                             column=1,
                             padx=3,
                             sticky='w')
    ttk.Label(f9,text='Frame alpha').grid(row=10,column=0,padx=3)
    ttk.Entry(f9,textvariable=self.ALPHA,width=10).grid(row=10,column=1,padx=3,sticky='w')
    f9.grid()



