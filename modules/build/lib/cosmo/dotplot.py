# Module for plotting dots, built for the COSMO project 
# Quim Ballabrera, May 2018

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
try:
  to_unicode = unicode
except:
  to_unicode = str

from cosmo.tools import empty
from cosmo.tools import exists
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA



# ============================
class parameters():
# ============================
  '''Class for plotting dots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2018"

  def __init__ (self):
  # ==================
    '''Define and initialize class attributes'''

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF          = COSMO_CONF + 'dot.conf'

    self.SHOW              = tk.BooleanVar()
    self.SIZE              = tk.DoubleVar()
    self.SYMBOL            = tk.StringVar()
    self.COLOR             = tk.StringVar()
    self.ALPHA             = tk.DoubleVar()

    self.HA                = tk.StringVar()
    self.VA                = tk.StringVar()
    self.WRAP              = tk.BooleanVar()
    self.TCOLOR            = tk.StringVar()
    self.STYLE             = tk.StringVar()
    self.WEIGHT            = tk.StringVar()
    self.ZORDER            = tk.IntVar()
    self.TSIZE             = tk.DoubleVar()

    # Default attribute values:
    #
    self.SHOW.set(True)
    self.SIZE.set(1.5)
    self.SYMBOL.set('o')
    self.COLOR.set('black')
    self.ALPHA.set(1.0)
    self.HA.set('left')
    self.VA.set('baseline')
    self.WRAP.set(True)
    self.TCOLOR.set('black')
    self.STYLE.set('normal')
    self.WEIGHT.set('bold')
    self.ZORDER.set(1)
    self.TSIZE.set(10)

    # If configuration file exists, it is read and
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.
    #
    if exists(self.FILECONF):
      try:
        print('Loading DOT configuration')
        self.load(self.FILECONF)
      except:
        print('Error: Saving default DOT configuration')
        self.save(self.FILECONF)
    else:
      print('Saving default DOT configuration')
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
    conf['HA'] = self.HA.get()
    conf['VA'] = self.VA.get()
    conf['WRAP'] = self.WRAP.get()
    conf['TCOLOR'] = self.TCOLOR.get()
    conf['STYLE'] = self.STYLE.get()
    conf['WEIGHT'] = self.WEIGHT.get()
    conf['ZORDER'] = self.ZORDER.get()
    conf['TSIZE'] = self.TSIZE.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    self.SHOW.set(conf['SHOW'])
    self.SIZE.set(conf['SIZE'])
    self.SYMBOL.set(conf['SYMBOL'])
    self.COLOR.set(conf['COLOR'])
    self.ALPHA.set(conf['ALPHA'])
    self.HA.set(conf['HA'])
    self.VA.set(conf['VA'])
    self.WRAP.set(conf['WRAP'])
    self.TCOLOR.set(conf['TCOLOR'])
    self.STYLE.set(conf['STYLE'])
    self.WEIGHT.set(conf['WEIGHT'])
    self.ZORDER.set(conf['ZORDER'])
    self.TSIZE.set(conf['TSIZE'])

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

  def color():
  # ==========
    ''' Color picker with backup'''

    backup = PLOT.COLOR.get()
    if PLOT.COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.COLOR.get(),parent=parent)

    if hx is None:
      PLOT.COLOR.set(backup)
    else:
      PLOT.COLOR.set(hx)

  # Main widget
  # ===========

  # Symbol list using standard and utf technical symbols
  #
  SYMBOL_LIST = ['.',',','o','v','^','<','>', \
                '1','2','3','4','s','p','*',  \
                'h','H','+','x','D','d','|','_']
  for n in range(ord('\u2600'),ord('\u26B8')+1):
    SYMBOL_LIST.append(chr(n))


  F0 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F0,text='Dot options', \
            font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  tk.Checkbutton(F0,text='Show',font='Helvetica 12 bold', \
                 variable=PLOT.SHOW).grid(row=1,column=1)
  ttk.Label(F0,text='Symbol').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.SYMBOL, \
               values=SYMBOL_LIST,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(F0,text='Size').grid(row=3,column=0,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.SIZE, \
            width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(F0,text='Color').grid(row=4,column=0, \
            columnspan=1,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.COLOR, \
            justify='left',width=7).grid(row=4,column=1,sticky='w')
  ttk.Button(F0,text='Select', \
             command=color).grid(row=4,column=2,padx=3,sticky='ew')
  ttk.Label(F0,text='Alpha').grid(row=5,column=0, \
                                  columnspan=1,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.ALPHA, \
            justify='left',width=7).grid(row=5,column=1,sticky='w')
  #ttk.Label(F0,text='Label').grid(row=6,column=0,columnspan=1,sticky='w')
  #ttk.Entry(F0,textvariable=PLOT.LABEL, \
  #          justify='left',width=20).grid(row=6,column=1, \
  #                                        columnspan=2,sticky='w')
  F0.grid()
