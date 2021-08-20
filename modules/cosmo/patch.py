'''
 Module for plotting patches, built for the COSMO project 
 Quim Ballabrera, August 2020
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
from cosmo import patch
import matplotlib.patches as mpatches
import cartopy.crs as ccrs



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

    self.MESSAGE = "\nPATCH PLOT class:\n"


    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF          = COSMO_CONF + 'patch.conf'

    self.LINE_COLOR        = tk.StringVar()
    self.LINE_STYLE        = tk.StringVar()
    self.LINE_WIDTH        = tk.DoubleVar()
    self.FILL              = tk.BooleanVar()
    self.FACE_COLOR        = tk.StringVar()
    self.ANGLE             = tk.DoubleVar()    # 0=Transparent, 1=Opaque
    self.ALPHA             = tk.DoubleVar()    # 0=Transparent, 1=Opaque
    self.ZORDER            = tk.IntVar()       

    # Default attribute values'''
    #
    self.LINE_COLOR.set('black')
    self.LINE_STYLE.set('-')
    self.LINE_WIDTH.set(1.0)
    self.FILL.set(False)
    self.FACE_COLOR.set('Blue')
    self.ANGLE.set(0)
    self.ALPHA.set(0.5)
    self.ZORDER.set(3)

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
    conf['FILECONF'] = self.FILECONF
    conf['LINE_COLOR'] = self.LINE_COLOR.get()
    conf['LINE_STYLE'] = self.LINE_STYLE.get()
    conf['LINE_WIDTH'] = self.LINE_WIDTH.get()
    conf['FILL'] = self.FILL.get()
    conf['FACE_COLOR'] = self.FACE_COLOR.get()
    conf['ANGLE'] = self.ANGLE.get()
    conf['ALPHA'] = self.ALPHA.get()
    conf['ZORDER'] = self.ZORDER.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    self.FILECONF = conf['FILECONF']
    self.LINE_COLOR.set(conf['LINE_COLOR'])   
    self.LINE_STYLE.set(conf['LINE_STYLE'])   
    self.LINE_WIDTH.set(conf['LINE_WIDTH'])   
    self.FILL.set(conf['FILL'])   
    self.FACE_COLOR.set(conf['FACE_COLOR'])   
    self.ANGLE.set(conf['ANGLE'])   
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
class PATCH():
# =================================================================
  ''' Class for 2D patches '''

  __version__ = "2.3"
  __author__  = "Joaquim Ballabrera"
  __date__ = "August 2020"

  def __init__(self,filename=None):
  # ===============================
    ''' DEfine and initialize the class attributes '''

    self.MESSAGE  = "\n PATCH class:\n"
    self.ALIAS    = tk.StringVar()
    self.show     = tk.BooleanVar()
    self.TYPE     = tk.StringVar()
    self.SOURCE   = "FILE"

    self.xo       = tk.DoubleVar()
    self.yo       = tk.DoubleVar()
    self.dx       = tk.DoubleVar()
    self.dy       = tk.DoubleVar()

    self.PLOT = patch.parameters()
    self.MESSAGE += self.PLOT.MESSAGE

    self.ALIAS.set('')
    self.show.set(True)

  def conf_get(self):
  # =================

    conf = {}
    conf['ALIAS']    = self.ALIAS.get()
    conf['SOURCE']   = self.SOURCE
    conf['SHOW']     = self.show.get()
    conf['TYPE']     = self.TYPE.get()
    conf['XO']       = self.xo.get()
    conf['YO']       = self.yo.get()
    conf['DX']       = self.dx.get()
    conf['DY']       = self.dy.get()
    conf['PLOT']     = self.PLOT.conf_get()
    return conf

  def conf_set(self,conf):
  # ======================

    self.show.set(conf['SHOW'])
    self.ALIAS.set(conf['ALIAS'])
    self.SOURCE = conf['SOURCE']
    self.TYPE.set(conf['TYPE'])
    self.xo.set(conf['XO'])
    self.yo.set(conf['YO'])
    self.dx.set(conf['DX'])
    self.dy.set(conf['DY'])
    self.PLOT.conf_set(conf['PLOT'])

# =================================================================
def Configuration(parent,E):
# =================================================================
  ''' Interactive widget to modify the options of 2D contour plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "August 2020"

  font_bold = tkfont.Font(font='TkDefaultFont').copy()
  font_bold['weight']='bold'

  frame1 = ttk.Frame(parent,borderwidth=5,padding=5,relief='sunken')
  ttk.Label(frame1,text='Patch',font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame1,text='ALIAS:').grid(row=1,column=0,sticky='w')
  ttk.Label(frame1,text=E.ALIAS.get(),width=30).grid(row=1,column=1,sticky='w')
  frame1.grid()

  frame2 = ttk.Frame(parent,borderwidth=5,padding=5)

  ttk.Label(frame2,text='Line style:').grid(row=2,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.LINE_STYLE,width=10).grid(row=2,column=1,sticky='we')
  ttk.Label(frame2,text='Line width:').grid(row=3,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.LINE_WIDTH,width=10).grid(row=3,column=1,sticky='we')

  ttk.Label(frame2,text='Edge color:').grid(row=4,column=0,sticky='e')
  sclabel = ttk.Style()
  sclabel.configure("sclabel.TLabel",background=E.PLOT.LINE_COLOR.get(),anchor="center")
  ecol = ttk.Label(frame2,textvariable=E.PLOT.LINE_COLOR,width=8,style="sclabel.TLabel") # Map Contour Color
  ecol.grid(row=4,column=1,padx=3,sticky='we')
  ttk.Button(frame2,text='Select',command=lambda:colsel(E.PLOT.LINE_COLOR, \
            sclabel,ecol,"sclabel.TLabel",master=parent)).\
            grid(row=4,column=2,padx=3,sticky='ew')

  ttk.Label(frame2,text='Fill:').grid(row=5,column=0,sticky='e')
  tk.Checkbutton(frame2,variable=E.PLOT.FILL).grid(row=5,column=1,sticky='w')

  ttk.Label(frame2,text='Face color:').grid(row=6,column=0,sticky='e')
  scfacec = ttk.Style()
  scfacec.configure("scfacec.TLabel",background=E.PLOT.FACE_COLOR.get(),anchor="center")
  fcol = ttk.Label(frame2,textvariable=E.PLOT.FACE_COLOR,width=8,style="scfacec.TLabel") # Map Contour Color
  fcol.grid(row=6,column=1,padx=3,sticky='we')
  ttk.Button(frame2,text='Select',command=lambda:colsel(E.PLOT.FACE_COLOR, \
            scfacec,fcol,"scfacec.TLabel",master=parent)).\
            grid(row=6,column=2,padx=3,sticky='ew')

  #ttk.Label(frame2,text='Face color:').grid(row=6,column=0,columnspan=2,sticky='we')
  #tk.Checkbutton(frame2,variable=E.PLOT.FACE_COLOR).grid(row=6,column=2,sticky='w')
  ttk.Label(frame2,text='Angle:').grid(row=7,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.ANGLE,width=10).grid(row=7,column=1,sticky='we')
  ttk.Label(frame2,text='Alpha:').grid(row=8,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.ALPHA,width=10).grid(row=8,column=1,sticky='we')
  ttk.Label(frame2,text='Zorder:').grid(row=9,column=0,sticky='e')
  ttk.Entry(frame2,textvariable=E.PLOT.ZORDER,width=10).grid(row=9,column=1,sticky='we')
  frame2.grid()



# ============================================
def drawing(ax,proj,P):
# ============================================
  ''' Draw a 2D ellipse '''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "July 2020"

  if not P.show.get():
    return

  if P.TYPE.get() == 'Rectangle':
    ax.add_patch(mpatches.Rectangle(xy=[P.xo.get(),P.yo.get()], 
                                         width=P.dx.get(),
                                         height=P.dy.get(),
                                         angle=P.PLOT.ANGLE.get(),
                                         fill=P.PLOT.FILL.get(),
                                         edgecolor=P.PLOT.LINE_COLOR.get(),
                                         facecolor=P.PLOT.FACE_COLOR.get(),
                                         linestyle=P.PLOT.LINE_STYLE.get(),
                                         linewidth=P.PLOT.LINE_WIDTH.get(),
                                         alpha=P.PLOT.ALPHA.get(),
                                         zorder=P.PLOT.ZORDER.get(),
                                         transform=ccrs.PlateCarree()))

  elif P.TYPE.get() == 'Circle':
    ax.add_patch(mpatches.Circle(xy=[P.xo.get(),P.yo.get()], 
                                         radius=P.dx.get(),
                                         fill=P.PLOT.FILL.get(),
                                         edgecolor=P.PLOT.LINE_COLOR.get(),
                                         facecolor=P.PLOT.FACE_COLOR.get(),
                                         linestyle=P.PLOT.LINE_STYLE.get(),
                                         linewidth=P.PLOT.LINE_WIDTH.get(),
                                         alpha=P.PLOT.ALPHA.get(),
                                         zorder=P.PLOT.ZORDER.get(),
                                         transform=ccrs.PlateCarree()))



