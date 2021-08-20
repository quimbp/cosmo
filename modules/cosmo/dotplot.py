''' Module for plotting dots, built for the COSMO project 
 Quim Ballabrera, May 2018
	EGL, 06/2020: Changes:
	No more support to python 2.7 
	All color selection functions are now managed through tools.colsel() function
	As an example: 
	i) we define an style (sdlabel)
	  sdlabel = ttk.Style()
      sdlabel.configure("sdlabel.TLabel",background=PLOT.COLOR.get(),anchor="center")
    ii) we build Label widget (here DLabel) instead of Entry widget  
      ttk.Label(F0,text='Color').grid(row=4,column=0,columnspan=1,sticky='w')
      DLabel = ttk.Label(F0,textvariable=PLOT.COLOR,width=8,style="sdlabel.TLabel")
      DLabel.grid(row=4,column=1)
    iii) we call colsel() function passing the variable, the style, the Label, 
    the attached style and the parent widget
	  ttk.Button(F0,text='Select',command=lambda:colsel(PLOT.COLOR, \
            sdlabel,DLabel,"sdlabel.TLabel",master=parent)).\
            grid(row=4,column=2,padx=3,sticky='ew')
    A heap variable MESSAGE has been introduce to store "print" messages
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
#EG
from cosmo.tools import colsel

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
    
    #EG To collect message
    self.MESSAGE =''

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
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
    self.ANGLE             = tk.DoubleVar()
    self.XPAD              = tk.DoubleVar()
    self.YPAD              = tk.DoubleVar()

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
    self.ANGLE.set(0)
    self.XPAD.set(0.1)
    self.YPAD.set(0)

    # If configuration file exists, it is read and
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.
    #
    if exists(self.FILECONF):
      try:
        self.MESSAGE += 'Loading DOT configuration'
        #print('Loading DOT configuration')
        self.load(self.FILECONF)
      except:
        self.MESSAGE += 'Error: Saving default DOT configuration'
        #print('Error: Saving default DOT configuration')
        self.save(self.FILECONF)
    else:
      self.MESSAGE += 'Saving default DOT configuration'
      #print('Saving default DOT configuration')
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
    conf['ANGLE'] = self.ANGLE.get()
    conf['XPAD'] = self.XPAD.get()
    conf['YPAD'] = self.YPAD.get()
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
    self.ANGLE.set(conf['ANGLE'])
    self.XPAD.set(conf['XPAD'])
    self.YPAD.set(conf['YPAD'])

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
      outfile.write(to_unicode(str_)+'\n')

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
  F0 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F0,text='Dot options',font=font).grid(row=0,column=0,sticky='w')
  tk.Checkbutton(F0,text='Show',font=font, \
                 variable=PLOT.SHOW).grid(row=1,column=1)
  ttk.Label(F0,text='Symbol').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.SYMBOL, \
               values=SYMBOL_LIST,width=8).grid(row=2,column=1,sticky='w')
  ttk.Label(F0,text='Size').grid(row=3,column=0,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.SIZE,width=8).grid(row=3,column=1,sticky='w')
            
  sdlabel = ttk.Style()
  sdlabel.configure("sdlabel.TLabel",background=PLOT.COLOR.get(),anchor="center")
  ttk.Label(F0,text='Color').grid(row=4,column=0,columnspan=1,sticky='w')
  DLabel = ttk.Label(F0,textvariable=PLOT.COLOR,width=8,style="sdlabel.TLabel")
  DLabel.grid(row=4,column=1)
  ttk.Button(F0,text='Select',command=lambda:colsel(PLOT.COLOR, \
            sdlabel,DLabel,"sdlabel.TLabel",master=parent)).\
            grid(row=4,column=2,padx=3,sticky='ew')
   
  ttk.Label(F0,text='Alpha').grid(row=5,column=0, \
                                  columnspan=1,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.ALPHA, \
            justify='left',width=8).grid(row=5,column=1,sticky='w')
  #ttk.Label(F0,text='Label').grid(row=6,column=0,columnspan=1,sticky='w')
  #ttk.Entry(F0,textvariable=PLOT.LABEL, \
  #          justify='left',width=20).grid(row=6,column=1, \
  #                                        columnspan=2,sticky='w')
  F0.grid()

