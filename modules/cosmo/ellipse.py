'''
 Module for plotting ellipses, built for the COSMO project 
 Quim Ballabrera, July 2020
 Changes: 
  - 07/2020: QB:
                Initial version
'''

import tkinter as tk
from tkinter import ttk

#try:
#  from tkcolorpicker import askcolor
#except:
#  from tkColorChooser import askcolor
from tkinter.colorchooser import askcolor


from tkinter import font as tkfont
import matplotlib.pyplot as plt
from matplotlib import rcParams

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
from cosmo import ellipse

#EG To manage cartopy projections
#from cosmo.tools import map_proj

try:
  to_unicode = unicode
except:
  to_unicode = str

# =================
class parameters():
# =================
  '''Class for ellipse plots: '''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2020"

  def __init__ (self):
  # ==================
    '''Define and initialize the class attributes'''

    self.MESSAGE = "\nELLIPSE class:\n"


    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF          = COSMO_CONF + 'ellipse.conf'

    self.scale             = tk.DoubleVar()
    self.LINE_COLOR        = tk.StringVar()
    self.LINE_STYLE        = tk.StringVar()
    self.LINE_WIDTH        = tk.DoubleVar()
    self.ADD_ROTATION      = tk.BooleanVar()
    self.SHOW_MAJOR        = tk.BooleanVar()
    self.SHOW_CENTER       = tk.BooleanVar()
    self.ALPHA             = tk.DoubleVar()    # 0=Transparent, 1=Opaque

    self.ZORDER            = tk.IntVar()       

    # Default attribute values'''
    #
    self.scale.set(1.0)
    self.LINE_COLOR.set('black')
    self.LINE_STYLE.set('-')
    self.LINE_WIDTH.set(1.0)
    self.ADD_ROTATION.set(True)
    self.SHOW_MAJOR.set(True)
    self.SHOW_CENTER.set(True)
    self.ALPHA.set(1.0)
    self.ZORDER.set(2)

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
    conf['SCALE'] = self.scale.get()
    conf['LINE_COLOR'] = self.LINE_COLOR.get()
    conf['LINE_STYLE'] = self.LINE_STYLE.get()
    conf['LINE_WIDTH'] = self.LINE_WIDTH.get()
    conf['ADD_ROTATION'] = self.ADD_ROTATION.get()
    conf['SHOW_MAJOR'] = self.SHOW_MAJOR.get()
    conf['SHOW_CENTER'] = self.SHOW_CENTER.get()
    conf['ALPHA'] = self.ALPHA.get()
    conf['ZORDER'] = self.ZORDER.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    #self.FILECONF = conf['FILECONF']
    self.scale.set(conf['SCALE'])   
    self.LINE_COLOR.set(conf['LINE_COLOR'])   
    self.LINE_STYLE.set(conf['LINE_STYLE'])   
    self.LINE_WIDTH.set(conf['LINE_WIDTH'])   
    self.ADD_ROTATION.set(conf['ADD_ROTATION'])   
    self.SHOW_MAJOR.set(conf['SHOW_MAJOR'])   
    self.SHOW_CENTER.set(conf['SHOW_CENTER'])   
    self.ALPHA.set(conf['ALPHA'])   
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

# =================================================================
class ELLIPSE():
# =================================================================
  ''' Class for 2D ellipses '''

  __version__ = "2.3"
  __author__  = "Joaquim Ballabrera"
  __date__ = "August 2020"

  def __init__(self,filename=None):
  # ===============================
    ''' DEfine and initialize the class attributes '''

    self.MESSAGE  = "\n ELLIPSE class:\n"
    self.ALIAS    = tk.StringVar()
    self.FILENAME = tk.StringVar()
    self.show     = tk.BooleanVar()
    self.SOURCE   = "FILE"
    self.PARENT   = None

    if filename is None:
      pass
    else:
      self.FILENAME.set(filename)

    self.n        = 0
    self.xo       = []
    self.yo       = []
    self.zo       = []
    self.phim     = []      # Is read in degs
    self.phia     = []      # Is read in degs
    self.a        = []
    self.b        = []

    self.PLOT = ellipse.parameters()
    self.MESSAGE += self.PLOT.MESSAGE

    self.ALIAS.set('')
    self.show.set(True)

  def default(self):
  # ================

    self.n        = 0
    self.xo       = []
    self.yo       = []
    self.zo       = []
    self.phim     = []
    self.phia     = []
    self.a        = []
    self.b        = []

  def conf_get(self):
  # =================

    conf = {}
    conf['N'] = self.n
    conf['FILENAME'] = self.FILENAME.get()
    conf['SOURCE']   = self.SOURCE
    conf['SHOW']     = self.show.get()
    conf['ALIAS']    = self.ALIAS.get()
    conf['PARENT']   = self.PARENT
    conf['XO']       = self.xo
    conf['YO']       = self.yo
    conf['ZO']       = self.zo
    conf['PHIM']     = self.phim
    conf['PHIA']     = self.phia
    conf['A']        = self.a
    conf['B']        = self.b
    conf['PLOT']     = self.PLOT.conf_get()
    return conf

  def conf_set(self,conf):
  # =======================

    self.n = conf['N'] = self.n
    self.FILENAME.set(conf['FILENAME'])
    self.SOURCE = conf['SOURCE']
    self.show.set(conf['SHOW'])
    self.ALIAS.set(conf['ALIAS'])
    self.PARENT = conf['PARENT']
    self.xo = conf['XO']
    self.yo = conf['YO']
    self.zo = conf['ZO']
    self.phim = conf['PHIM']
    self.phia = conf['PHIA']
    self.a = conf['A']
    self.b = conf['B']
    self.PLOT.conf_set(conf['PLOT'])

  def Read(self,filename=None):
  # ===========================

    if filename is None:
      filename = self.FILENAME.get()
    else:
      self.FILENAME.set(filename)

    with open(filename) as datafile:
      for line in datafile.readlines():
        line = line.strip()
        columns = line.split(',')

        self.xo.append(float(columns[0]))
        self.yo.append(float(columns[1]))
        self.zo.append(float(columns[2]))
        self.phim.append(float(columns[3]))
        self.phia.append(float(columns[4]))
        self.a.append(float(columns[5]))
        self.b.append(float(columns[6]))

    self.n = len(self.xo)

    if self.n == 0:
      self.default()



# =================================================================
def Configuration(parent,E):
# =================================================================
  ''' Interactive widget to modify the options of 2D contour plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2020"

  frame2 = ttk.Frame(parent,borderwidth=5,padding=5)

  ttk.Label(frame2,text='Ellipse position, xo:').grid(row=0,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.xo,width=10).grid(row=0,column=1,sticky='we')
  ttk.Label(frame2,text='yo:').grid(row=0,column=2,sticky='e')
  ttk.Entry(frame2,textvariable=E.yo,width=10).grid(row=0,column=3,sticky='we')

  ttk.Label(frame2,text='Semi-axis, major, a:').grid(row=1,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.a,width=10).grid(row=1,column=1,sticky='we')
  ttk.Label(frame2,text='minor, b:').grid(row=1,column=2,sticky='e')
  ttk.Label(frame2,text='Orientation, phi:').grid(row=2,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.phi,width=10).grid(row=2,column=1,sticky='we')
  ttk.Label(frame2,text='Scale factor:').grid(row=3,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.scale,width=10).grid(row=3,column=1,sticky='we')


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
  ttk.Entry(frame2,textvariable=E.ALPHA,width=7).grid(row=6,column=1,sticky='w')
  ttk.Label(frame2,text='Zorder:').grid(row=7,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=E.ZORDER,width=7).grid(row=7,column=1,sticky='w')
  #ttk.Label(frame2,text='0: transparent; 1: opaque').grid(row=6,column=2,sticky='w')
  frame2.grid()

# =================================================================
def Configuration2(parent,E):
# =================================================================
  ''' Interactive widget to modify the options of 2D contour plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "August 2020"

  font_bold = tkfont.Font(font='TkDefaultFont').copy()
  font_bold['weight']='bold'

  frame1 = ttk.Frame(parent,borderwidth=5,padding=5,relief='sunken')
  ttk.Label(frame1,text='Ellipse of variance',font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame1,text='ALIAS:').grid(row=1,column=0,sticky='w')
  ttk.Label(frame1,text=E.ALIAS.get(),width=30).grid(row=1,column=1,sticky='w')
  ttk.Label(frame1,text='SOURCE:').grid(row=2,column=0,sticky='w')
  ttk.Label(frame1,text=E.SOURCE,width=30).grid(row=2,column=1,sticky='w')
  ttk.Label(frame1,text='PARENT:').grid(row=3,column=0,sticky='w')
  ttk.Label(frame1,text=E.PARENT,width=30).grid(row=3,column=1,sticky='w')
  frame1.grid()

  frame2 = ttk.Frame(parent,borderwidth=5,padding=5)

  ttk.Label(frame2,text='Add mean and anomaly directions:').grid(row=0,column=0,columnspan=2,sticky='we')
  tk.Checkbutton(frame2,variable=E.PLOT.ADD_ROTATION).grid(row=0,column=2,sticky='w')

  ttk.Label(frame2,text='Size scale:').grid(row=1,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.scale,width=10).grid(row=1,column=1,sticky='we')
  ttk.Label(frame2,text='Line style:').grid(row=2,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.LINE_STYLE,width=10).grid(row=2,column=1,sticky='we')
  ttk.Label(frame2,text='Line width:').grid(row=3,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.LINE_WIDTH,width=10).grid(row=3,column=1,sticky='we')
  
  ttk.Label(frame2,text='Line color:').grid(row=4,column=0,sticky='e')
  sclabel = ttk.Style()
  sclabel.configure("sclabel.TLabel",background=E.PLOT.LINE_COLOR.get(),anchor="center")
  ecol = ttk.Label(frame2,textvariable=E.PLOT.LINE_COLOR,width=8,style="sclabel.TLabel") # Map Contour Color
  ecol.grid(row=4,column=1,padx=3,sticky='we')
  ttk.Button(frame2,text='Select',command=lambda:colsel(E.PLOT.LINE_COLOR, \
            sclabel,ecol,"sclabel.TLabel",master=parent)).\
            grid(row=4,column=2,padx=3,sticky='ew')

  ttk.Label(frame2,text='Show center point:').grid(row=5,column=0,columnspan=2,sticky='we')
  tk.Checkbutton(frame2,variable=E.PLOT.SHOW_CENTER).grid(row=5,column=2,sticky='w')
  ttk.Label(frame2,text='Show mean direction:').grid(row=6,column=0,columnspan=2,sticky='we')
  tk.Checkbutton(frame2,variable=E.PLOT.SHOW_MAJOR).grid(row=6,column=2,sticky='w')
  ttk.Label(frame2,text='Alpha:').grid(row=7,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.ALPHA,width=10).grid(row=7,column=1,sticky='we')
  ttk.Label(frame2,text='Zorder:').grid(row=8,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.ZORDER,width=10).grid(row=8,column=1,sticky='we')
  frame2.grid()



# ============================================
def drawing(ax,proj,E):
# ============================================
  ''' Draw a 2D ellipse '''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2020"

  if not E.show.get():
    return

  for ii in range(len(E.xo)):

    if E.PLOT.ADD_ROTATION.get():
      phi = E.phim[ii] + E.phia[ii]
    else:
      phi = E.phia[ii]

    phi = np.pi * phi / 180   # From Deg to rads

    xo  = E.xo[ii]
    yo  = E.yo[ii]

    a   = E.PLOT.scale.get()*E.a[ii]
    b   = E.PLOT.scale.get()*E.b[ii]

    # Rotation matrix
    R = np.array([[np.cos(phi), -np.sin(phi)],[np.sin(phi), np.cos(phi)]])

    # Parametric centered ellipse
    n = 100
    t = np.linspace(0,2*np.pi,n)
    Ell = np.array([a*np.cos(t), b*np.sin(t)])

    Er = np.zeros((2,n))
    for i in range(n):
      Er[:,i] = np.dot(R,Ell[:,i])

    if E.PLOT.SHOW_CENTER.get():
      ax.plot(xo,yo,marker='o', \
              color=E.PLOT.LINE_COLOR.get(),        \
              ms=2.0*E.PLOT.LINE_WIDTH.get(),    \
              fillstyle='full',                     \
              alpha=E.PLOT.ALPHA.get(),             \
              zorder=E.PLOT.ZORDER.get(),           \
              transform=proj)

    ax.plot(xo+Er[0,:], yo+Er[1,:],               \
            linestyle=E.PLOT.LINE_STYLE.get(),    \
            color=E.PLOT.LINE_COLOR.get(),        \
            linewidth=E.PLOT.LINE_WIDTH.get(),    \
            alpha=E.PLOT.ALPHA.get(),             \
            zorder=E.PLOT.ZORDER.get(),           \
            transform=proj)

    # Direction mean flow
    if E.PLOT.SHOW_MAJOR.get():
      phim = E.phim[ii]*np.pi/180   #From Deg to Rads
      #tc = np.arctan2(np.tan(phim)*a,b)
      #xmax = np.abs(a*np.cos(tc))
      #xmin = -np.abs(a*np.cos(tc))
      m = np.tan(phim)
      xmax = 1/np.sqrt(1/(a*a) + m*m/(b*b))
      xmin = -xmax

      x = np.linspace(xmin,xmax,4)
      y = m*x
  
      ax.plot(xo+x, yo+y,                       \
              linestyle='--',                   \
              color=E.PLOT.LINE_COLOR.get(),         \
              linewidth=0.75*E.PLOT.LINE_WIDTH.get(), \
              alpha=E.PLOT.ALPHA.get(),              \
              zorder=E.PLOT.ZORDER.get(),            \
              transform=proj)

