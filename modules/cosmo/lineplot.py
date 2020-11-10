'''Module for plotting lines built for the COSMO project 
# Quim Ballabrera, January 2018
	EGL, 06/2020: Changes:
		No more support to python 2.7 
		Small adjustments of fonts and widget style properties
		All color selections are now managed through tools.colsel() function
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
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA

#EG
from cosmo.tools import colsel

from math import atan2,degrees
import numpy as np

# =================
class parameters():
# =================
  '''Class for plotting lines'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  def __init__ (self):
  # ==================
    '''Define and initialize class attributes'''

    self.MESSAGE = ""

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF          = COSMO_CONF + 'line.conf'

    self.LINE_SHOW         = tk.BooleanVar()
    self.LINE_STYLE        = tk.StringVar()
    self.LINE_WIDTH        = tk.DoubleVar()
    self.LINE_COLOR        = tk.StringVar()
    self.MARKER_SHOW       = tk.BooleanVar()
    self.MARKER_STYLE      = tk.StringVar()
    self.MARKER_SIZE       = tk.DoubleVar()
    self.MARKER_COLOR      = tk.StringVar()
    self.INITIAL_SHOW      = tk.BooleanVar()
    self.INITIAL_STYLE     = tk.StringVar()
    self.INITIAL_SIZE      = tk.IntVar()
    self.INITIAL_COLOR     = tk.StringVar()
    self.ONMAP_SHOW        = tk.BooleanVar()
    self.ONMAP_STYLE       = tk.StringVar()
    self.ONMAP_SIZE        = tk.IntVar()
    self.ONMAP_COLOR       = tk.StringVar()
    self.ZORDER            = tk.IntVar()
    self.ALPHA             = tk.DoubleVar()

    # Default attribute values
    #
    self.LINE_SHOW.set(True)
    self.LINE_WIDTH.set(1.5)
    self.LINE_COLOR.set('blue')
    self.LINE_STYLE.set('-')

    self.MARKER_SHOW.set(False)
    self.MARKER_STYLE.set('o')
    self.MARKER_COLOR.set('blue')
    self.MARKER_SIZE.set(3)

    self.INITIAL_SHOW.set(True)
    self.INITIAL_STYLE.set('o')
    self.INITIAL_COLOR.set('red')
    self.INITIAL_SIZE.set(4)

    self.ONMAP_SHOW.set(False)
    self.ONMAP_STYLE.set('o')
    self.ONMAP_COLOR.set('red')
    self.ONMAP_SIZE.set(4)

    self.ZORDER.set(4)
    self.ALPHA.set(1.0)

    # If configuration file exists, it is read and
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.
    #
    if exists(self.FILECONF):
      try:
        self.MESSAGE += 'Loading LINE configuration'
        #print('Loading LINE configuration')
        self.load(self.FILECONF)
      except:
        self.MESSAGE += 'Error: Saving default LINE configuration'
        #print('Error: Saving default LINE configuration')
        self.save(self.FILECONF)
    else:
      self.MESSAGE += 'Saving default LINE configuration'
      #print('Saving default LINE configuration')
      self.save(self.FILECONF)

  def conf_get(self):
  # =================
    ''' Set class dictionnary from class attributes'''

    conf = {}
    conf['LINE_SHOW'] = self.LINE_SHOW.get()
    conf['LINE_WIDTH'] = self.LINE_WIDTH.get()
    conf['LINE_COLOR'] = self.LINE_COLOR.get()
    conf['LINE_STYLE'] = self.LINE_STYLE.get()
    conf['MARKER_SHOW'] = self.MARKER_SHOW.get()
    conf['MARKER_STYLE'] = self.MARKER_STYLE.get()
    conf['MARKER_COLOR'] = self.MARKER_COLOR.get()
    conf['MARKER_SIZE'] = self.MARKER_SIZE.get()
    conf['INITIAL_SHOW'] = self.INITIAL_SHOW.get()
    conf['INITIAL_STYLE'] = self.INITIAL_STYLE.get()
    conf['INITIAL_COLOR'] = self.INITIAL_COLOR.get()
    conf['INITIAL_SIZE'] = self.INITIAL_SIZE.get()
    conf['ONMAP_SHOW'] = self.ONMAP_SHOW.get()
    conf['ONMAP_STYLE'] = self.ONMAP_STYLE.get()
    conf['ONMAP_COLOR'] = self.ONMAP_COLOR.get()
    conf['ONMAP_SIZE'] = self.ONMAP_SIZE.get()
    conf['ZORDER'] = self.ZORDER.get()
    conf['ALPHA'] = self.ALPHA.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    self.LINE_SHOW.set(conf['LINE_SHOW'])
    self.LINE_WIDTH.set(conf['LINE_WIDTH'])
    self.LINE_COLOR.set(conf['LINE_COLOR'])
    self.LINE_STYLE.set(conf['LINE_STYLE'])
    self.MARKER_SHOW.set(conf['MARKER_SHOW'])
    self.MARKER_STYLE.set(conf['MARKER_STYLE'])
    self.MARKER_COLOR.set(conf['MARKER_COLOR'])
    self.MARKER_SIZE.set(conf['MARKER_SIZE'])
    self.INITIAL_SHOW.set(conf['INITIAL_SHOW'])
    self.INITIAL_STYLE.set(conf['INITIAL_STYLE'])
    self.INITIAL_COLOR.set(conf['INITIAL_COLOR'])
    self.INITIAL_SIZE.set(conf['INITIAL_SIZE'])
    self.ONMAP_SHOW.set(conf['ONMAP_SHOW'])
    self.ONMAP_STYLE.set(conf['ONMAP_STYLE'])
    self.ONMAP_COLOR.set(conf['ONMAP_COLOR'])
    self.ONMAP_SIZE.set(conf['ONMAP_SIZE'])
    self.ZORDER.set(conf['ZORDER'])
    self.ALPHA.set(conf['ALPHA'])

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
  ''' Interactive widget to modify the options of 2D line plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "December 2017"

  # Main widget
  # ===========
  LINE_STYLE_LIST = ['-','--','-.',':']
  MARKER_STYLE_LIST = ['.',',','o','v','^','<','>','1','2','3','4','s','p','*','h','H','+','x','D','d','|','_']

  # Styles
  font_bold = tkfont.Font(font='TkDefaultFont').copy()
  font_bold['weight']='bold'
  sline, smarker, sinicia = ttk.Style(), ttk.Style(), ttk.Style()
  sline.configure("sline.TLabel",background=PLOT.LINE_COLOR.get(),anchor="center")
  smarker.configure("smarker.TLabel",background=PLOT.MARKER_COLOR.get(),anchor="center")
  sinicia.configure("sinicia.TLabel",background=PLOT.INITIAL_COLOR.get(),anchor="center")
  fcbold=ttk.Style()
  fcbold.configure("fcbold.TCheckbutton",font=font_bold)
  
  F0 = tk.LabelFrame(parent,text='Line options',borderwidth=5,font=font_bold)
  ttk.Checkbutton(F0,text='Line show',variable=PLOT.LINE_SHOW, \
            style="fcbold.TCheckbutton").grid(row=0,column=0,columnspan=2,pady=10,sticky='e')
  ttk.Label(F0,text='Line Style').grid(row=1,column=0,padx=10,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.LINE_STYLE,values=LINE_STYLE_LIST,\
            width=8,justify="center").grid(row=1,column=1,sticky='w')
  ttk.Label(F0,text='Line Width').grid(row=2,column=0,padx=10,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.LINE_WIDTH,width=8,justify="center").grid(row=2,column=1,sticky='w')
  ttk.Label(F0,text='Line Color').grid(row=3,column=0,padx=10,sticky='w') 
  LLabel = ttk.Label(F0,textvariable=PLOT.LINE_COLOR,width=8,style="sline.TLabel")
  LLabel.grid(row=3,column=1,sticky='w')
  ttk.Button(F0,text='Select',command=lambda:colsel(PLOT.LINE_COLOR, \
            sline,LLabel,"sline.TLabel",master=parent)). \
            grid(row=3,column=2,sticky='w')
  F0.grid(row=0,column=0,padx=20,pady=10,ipadx=20,ipady=10)

  F3 = tk.LabelFrame(parent,text='Other options',borderwidth=5,font=font_bold)
  ttk.Label(F3,text='Alpha').grid(row=0,column=0,padx=10,sticky='w')
  ttk.Entry(F3,textvariable=PLOT.ALPHA,width=8,justify="center").grid(row=0,column=1,sticky='w')
  ttk.Label(F3,text='Zorder').grid(row=1,column=0,padx=10,sticky='w')
  ttk.Entry(F3,textvariable=PLOT.ZORDER,width=8,justify="center").grid(row=1,column=1,sticky='w')
  F3.grid(row=0,column=1,padx=20,pady=10,ipadx=20,ipady=10)


  F1 = tk.LabelFrame(parent,text='Marker options',borderwidth=5,font=font_bold)
  ttk.Checkbutton(F1,text='Marker show',variable=PLOT.MARKER_SHOW, \
            style="fcbold.TCheckbutton").grid(row=0,column=0,columnspan=2,pady=10,sticky='e')
  ttk.Label(F1,text='Marker Style').grid(row=1,column=0,padx=10,sticky='w')
  ttk.Combobox(F1,textvariable=PLOT.MARKER_STYLE,values=MARKER_STYLE_LIST,width=8, \
            justify="center").grid(row=1,column=1,sticky='w')
  ttk.Label(F1,text='Marker Size').grid(row=2,column=0,padx=10,sticky='w')
  ttk.Entry(F1,textvariable=PLOT.MARKER_SIZE,width=8,justify="center").grid(row=2,column=1,sticky='w')
  ttk.Label(F1,text='Marker Color').grid(row=3,column=0,padx=10,sticky='w')
  MLabel = ttk.Label(F1,textvariable=PLOT.MARKER_COLOR,width=8,style="smarker.TLabel")
  MLabel.grid(row=3,column=1,sticky='w')
  ttk.Button(F1,text='Select',command=lambda:colsel(PLOT.MARKER_COLOR, \
            smarker,MLabel,"smarker.TLabel",master=parent)). \
            grid(row=3,column=2,sticky='w')
  F1.grid(row=1,column=0,padx=20,pady=10,ipadx=20,ipady=10)

  F2 = tk.LabelFrame(parent,text='Initial Marker options',borderwidth=5,font=font_bold)
  ttk.Checkbutton(F2,text='Initial show',variable=PLOT.INITIAL_SHOW, \
            style="fcbold.TCheckbutton").grid(row=0,column=0,columnspan=2,pady=10,sticky='e')
  ttk.Label(F2,text='Initial Style').grid(row=1,column=0,padx=10,sticky='w')
  ttk.Combobox(F2,textvariable=PLOT.INITIAL_STYLE,values=MARKER_STYLE_LIST,\
            width=8,justify="center").grid(row=1,column=1,sticky='w')
  ttk.Label(F2,text='Initial Size').grid(row=2,column=0,padx=10,sticky='w')
  ttk.Entry(F2,textvariable=PLOT.INITIAL_SIZE,width=8,justify="center").grid(row=2,column=1,sticky='w')
  ttk.Label(F2,text='Initial Color').grid(row=3,column=0,padx=10,sticky='w')
  ILabel = ttk.Label(F2,textvariable=PLOT.INITIAL_COLOR,width=8,style="sinicia.TLabel")
  ILabel.grid(row=3,column=1,sticky='w')
  ttk.Button(F2,text='Select',command=lambda:colsel(PLOT.INITIAL_COLOR, \
            sinicia,ILabel,"sinicia.TLabel",master=parent)). \
            grid(row=3,column=2,sticky='w')
  F2.grid(row=2,column=0,padx=20,pady=10,ipadx=20,ipady=10)

# ========================================
def Configuration_OnMap(parent,PLOT,LL):
# ========================================
  ''' Interactive widget to modify the options of 2D line plots'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  sfline, ssmarker = ttk.Style(), ttk.Style()
  global LLabel

  def iselection():
    global Llabel
    '''Select floater'''
    #cbox['text'] = LL.FLOAT_COLOR[LL.I.get()]
    LLabel['textvariable'] = LL.FLOAT_COLOR[LL.I.get()]
    _ifshow['variable'] = LL.FLOAT_SHOW[LL.I.get()]
    _ifzord['textvariable'] = LL.FLOAT_ZORDER[LL.I.get()]
    sfline.configure("sfline.TLabel",background=LL.FLOAT_COLOR[LL.I.get()].get(),anchor="center")
    #LLabel.configure(style="sfline.Tlabel")


  def cselection():
    '''Select floater'''
    #print(cbox.get())
    LL.FLOAT_COLOR[LL.I.get()].set(cbox.get())
    LLabel['textvariable'] = LL.FLOAT_COLOR[LL.I.get()]

  def default_color():
    for i in range(LL.nfloats):
      LL.FLOAT_COLOR[i].set(LL.PLOT.LINE_COLOR.get())

  '''def line_color():
    if LL.FLOAT_COLOR[LL.I.get()].get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      backup = LL.FLOAT_COLOR[LL.I.get()].get()
      rgb, hx = askcolor(color=LL.FLOAT_COLOR[LL.I.get()].get(),parent=parent)
      if hx is None:
        LL.FLOAT_COLOR[LL.I.get()].set(backup)
      else:
        LL.FLOAT_COLOR[LL.I.get()].set(hx)'''

  '''def marker_color():
    if PLOT.ONMAP_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.ONMAP_COLOR.get(),parent=parent)
    PLOT.ONMAP_COLOR.set(hx)'''

  # Main widget
  # ===========
  MARKER_STYLE_LIST = ['.',',','o','v','^','<','>','1','2','3','4','s','p','*','h','H','+','x','D','d','|','_']

  # Styles
  font_bold = tkfont.Font(font='TkDefaultFont').copy()
  font_bold['weight']='bold'

  ssmarker.configure("ssmarker.TLabel",background=PLOT.ONMAP_COLOR.get(),anchor="center")
  sfline.configure("sfline.TLabel",background=LL.FLOAT_COLOR[LL.I.get()].get(),anchor="center")

  fcbold=ttk.Style()
  fcbold.configure("fcbold.TCheckbutton",font=font_bold)
 
  F0 = tk.LabelFrame(parent,text='Trajectory interpolation',borderwidth=5,font=font_bold)
  ttk.Checkbutton(F0,text='Show interpolated',variable=PLOT.ONMAP_SHOW, \
            style="fcbold.TCheckbutton").grid(row=0,column=0,columnspan=2,pady=10,sticky='e')
  ttk.Label(F0,text='Marker Style').grid(row=1,column=0,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.ONMAP_STYLE,values=MARKER_STYLE_LIST,\
            width=8,justify="center").grid(row=1,column=1,sticky='w')
  ttk.Label(F0,text='Marker Size').grid(row=2,column=0,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.ONMAP_SIZE,width=8,justify="center").grid(row=2,column=1,sticky='w')
  ttk.Label(F0,text='Marker Color').grid(row=3,column=0,sticky='w')
  MMLabel = ttk.Label(F0,textvariable=PLOT.ONMAP_COLOR,width=8,style="ssmarker.TLabel")
  MMLabel.grid(row=3,column=1,sticky='w')
  ttk.Button(F0,text='Select',command=lambda:colsel(PLOT.ONMAP_COLOR, \
            ssmarker,MMLabel,"ssmarker.TLabel",master=parent)). \
            grid(row=3,column=2,sticky='w')
  F0.grid(row=0,column=0,padx=20,pady=10,ipadx=20,ipady=10,sticky='w')

  F1 = ttk.Frame(parent,borderwidth=5)
  ttk.Label(F1,text='Trajectory limits',font=font_bold).\
            grid(row=0,column=0,columnspan=2,sticky='ew')
  ttk.Label(F1,text='Initial record').grid(row=1,column=0,sticky='w')
  ttk.Entry(F1,textvariable=LL.L1,width=8).grid(row=1,column=1,sticky='w')
  ttk.Label(F1,text='Last record').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Entry(F1,textvariable=LL.L2,justify='left',width=8).grid(row=2,column=1,sticky='w')
  F1.grid(row=1,column=0,padx=20,pady=10,ipadx=20,ipady=10,sticky='ew')

  F2 = tk.LabelFrame(parent,text='Individual Float options',borderwidth=5,font=font_bold)
  ttk.Checkbutton(F2,text='Use individual options',variable=LL.SEPARATED_COLOR, \
            style="fcbold.TCheckbutton").grid(row=0,column=1,columnspan=2)

  ttk.Label(F2,text='Floater:').grid(row=1,column=0,padx=3)
  ibox = ttk.Combobox(F2,textvariable=LL.I,width=5)
  ibox.grid(row=1,column=1,sticky='w')
  ibox.bind('<<ComboboxSelected>>',lambda e: iselection())
  ibox['values'] = list(range(LL.nfloats))

  _ifshow = ttk.Checkbutton(F2,text='Show',variable=LL.FLOAT_SHOW[LL.I.get()])
  _ifshow.grid(row=1,column=2,padx=3,sticky='we')

  ttk.Label(F2,text='Zorder').grid(row=1,column=3,padx=10,sticky='w')
  _ifzord = ttk.Entry(F2,textvariable=LL.FLOAT_ZORDER[LL.I.get()],justify='left',width=8)
  _ifzord.grid(row=1,column=4,sticky='w')

  ttk.Label(F2,text='Color').grid(row=1,column=5,padx=10,sticky='w')
  LLabel = ttk.Label(F2,textvariable=LL.FLOAT_COLOR[LL.I.get()],width=8,style="sfline.TLabel")
  LLabel.grid(row=1,column=6,sticky='w')
  LLabel.bind('<Return>',lambda e: cselection())
  ttk.Button(F2,text='Select',command=lambda:colsel(LL.FLOAT_COLOR[LL.I.get()], \
            sfline,LLabel,"sfline.TLabel",master=parent)). \
            grid(row=1,column=7,sticky='w')
            
  ''' OJO NO SE SI ESCORRECTO
  cbox = ttk.Entry(F2,textvariable=LL.FLOAT_COLOR[LL.I.get()],justify='left',width=10)
  cbox.grid(row=1,column=3,sticky='w')
  cbox.bind('<Return>',lambda e: cselection())
  '''
  # AAA
  #ttk.Button(F2,text='Select',command=line_color).grid(row=1,column=4,padx=3,sticky='ew')
  ttk.Button(F2,text='Default',command=default_color).grid(row=1,column=8,padx=3,sticky='w')
  F2.grid(row=2,column=0,padx=20,pady=10,ipadx=20,ipady=10,sticky='w')

# ======================================================
def LabelLine(ax,line,x,label=None,align=True,**kwargs):
# ======================================================
  ''' Label line with line2D label data
      Update from the script writen by NauticalMile
      https://stackoverflow.com/questions/16992038/inline-labels-in-matplotlib
  '''

  xdata = line.get_xdata()
  ydata = line.get_ydata()

  if (x < xdata[0]) or (x > xdata[-1]):
    print('x label location is outside data range!')
    return

  #Find corresponding y co-ordinate and angle of the line
  ip = 1
  for i in range(len(xdata)):
    if x < xdata[i]:
      ip = i
      break

  y = ydata[ip-1] + \
      (ydata[ip]-ydata[ip-1])*(x-xdata[ip-1])/(xdata[ip]-xdata[ip-1])

  if not label:
    label = line.get_label()

  if align:
    #Compute the slope
    dx = xdata[ip] - xdata[ip-1]
    dy = ydata[ip] - ydata[ip-1]
    ang = degrees(atan2(dy,dx))

    #Transform to screen co-ordinates
    pt = np.array([x,y]).reshape((1,2))
    trans_angle = ax.transData.transform_angles(np.array((ang,)),pt)[0]

  else:
    trans_angle = 0

  #Set a bunch of keyword arguments
  if 'color' not in kwargs:
    kwargs['color'] = line.get_color()

  if ('horizontalalignment' not in kwargs) and ('ha' not in kwargs):
    kwargs['ha'] = 'center'

  if ('verticalalignment' not in kwargs) and ('va' not in kwargs):
    kwargs['va'] = 'center'

  if 'backgroundcolor' not in kwargs:
    kwargs['backgroundcolor'] = ax.get_facecolor()

  if 'clip_on' not in kwargs:
    kwargs['clip_on'] = True

  if 'zorder' not in kwargs:
    kwargs['zorder'] = 2.5

  t = ax.text(x,y,label,rotation=trans_angle,**kwargs)
  t.set_bbox(dict(alpha=0.1))


