''' Module: shape.py
    Purpose: Provides classes and functions to give limited support to 
    load and draw geometries from shape files.
    COSMO-View project python module. 
    EGL, June 2020:
		Entirely based on geomarker.py (from Cosmo project) and requires
		Cartopy, geoplot module (from Cosmo project) and colse() function 
		from tools modules (from Cosmo project).
		A heap variable MESSAGE has been introduce to store "print" messages
'''

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog

import matplotlib.pyplot as plt
import numpy as np
import json
import os
import io
import csv
try:
  to_unicode = unicode
except:
  to_unicode = str

import cosmo.geoplot as geoplot
from cosmo.tools import marker_string
from cosmo.tools import empty
from cosmo.tools import colsel

#EG 
from cartopy.io.shapereader import Reader
from cartopy.feature import ShapelyFeature
import cartopy.crs as ccrs

__version__ = "1.0"
__author__  = "Quim Ballabrera"
__date__    = "July 2018"

class parameters():
# ===================
  ''' Parameters for Geomarker variables'''
  
  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "May 2018"

  # Version 1.0 (May 2018)      : Initial version

  def __init__ (self):
  # ==================
    #EG objeto shape
    
    self.MESSAGE = ''
    
    self.shp   = None
    self.geom  = None
    self.type  = None
    self.FILENAME        = tk.StringVar()

    self.PLOT            = geoplot.parameters()
    self.LABEL           = tk.StringVar()
    self.n               = 0
    self.textmode        = tk.BooleanVar()
    self.lon             = []
    self.lat             = []
    self.label           = []
    
    #EG We collect message ftom dotplot.parameters
    self.MESSAGE += self.PLOT.MESSAGE

  def get_type(self,code):
  # =================
    '''Returns the type of geometry in shape files'''
    # Only these types are implemented in cartopy 0.17
    defined_types={"1":"POINT","3":"POLYLINE","5":"POLYGON",
     "11":"POINTZ","13":"POLYLINEZ","15":"POLYGONZ"}
    return(defined_types[str(code)])

  def conf_get(self):
  # =================
    ''' Set class dictionnary from class attributes'''
    conf = {}
    conf['PLOT'] = self.PLOT.conf_get()
    conf['TEXTMODE'] = self.textmode.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    self.PLOT.conf_set(conf['PLOT'])
    self.textmode.set(conf['TEXTMODE'])

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

  def Read(self,filename):
  # ======================
    '''Opens and reads a set of points'''

    __version__ = "1.0"
    __author__  = "Quim Ballabrera"
    __date__    = "May 2018"

    # --------------------------------------
    def read(filename):
    # --------------------------------------
      '''Read a shp file'''
      self.shp = Reader(filename)
      self.lon = list(self.shp.geometries())
              
    self.FILENAME.set(filename)

    if filename.lower().endswith(('.shp')):
      read(filename)
      
    self.n = len(self.lon)
    self.geom = self.shp
    #self.type = self.shp.shapeType
                          
    # Cheack that something has been read:
    if self.n is 0:
      self = None
      return

  def Draw(self,fig=None,ax=None,m=None):
  # ================================

    if fig==None:
      fig = plt.figure()

    if ax==None:
      ax = fig.add_subplot(111)
      ax.set_xlabel('Longitude')
      ax.set_ylabel('Latitude')

    if m==None:
      for i in range(self.n):
        plot.plot(self.lon[i],self.lat[i],                     \
                linestyle='',                                  \
                marker=marker_string(self.PLOT.SYMBOL.get()),  \
                ms=self.PLOT.SIZE.get(),                       \
                visible=self.PLOT.SHOW.get(),                  \
                label=self.PLOT.LABEL.get(),                   \
                alpha=self.PLOT.ALPHA.get(),                   \
                color=self.PLOT.COLOR.get())
    else:
      for i in range(self.n):
        m.plot(self.lon[i],self.lat[i],                       \
                linestyle='',                                  \
                marker=marker_string(self.PLOT.SYMBOL.get()),  \
                ms=self.PLOT.SIZE.get(),                       \
                visible=self.PLOT.SHOW.get(),                  \
                label=self.PLOT.LABEL.get(),                   \
                alpha=self.PLOT.ALPHA.get(),                   \
                color=self.PLOT.COLOR.get())

    ax.legend()
    plt.show()

# ======================================
def drawing(ax,proj,SHAPE):
# ======================================
  '''Draw geometries from shapes files'''

  __version__ = "1.0"
  __author__  = "Emili Garcia, based on Joaquin Ballabrera geomarker.py"
  __date__    = "May 2018"

  feature = ShapelyFeature(SHAPE.geom.geometries(), proj)
  ax.add_feature(feature,
             visible=SHAPE.PLOT.SHOW.get(),
             linewidth=SHAPE.PLOT.LINEWIDTH.get(),
             #label=SHAPE.LABEL.get(),
             alpha=SHAPE.PLOT.ALPHA.get(),
             edgecolor=SHAPE.PLOT.EDGECOLOR.get(),
             facecolor=SHAPE.PLOT.FACECOLOR.get(),
             zorder=SHAPE.PLOT.ZORDER.get())
                
# ======================================
def TextConfigure(parent,MPLOT):
# ======================================
  '''Dialogs to get marker text properties'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "May 2018"

  VARIANT_LIST = ['normal',
                  'small-caps']
  STYLE_LIST   = ['normal',
                  'italic',
                  'oblique']
  WEIGHT_LIST = ['ultralight',
                 'light',
                 'normal',
                 'regular',
                 'book',
                 'medium',
                 'roman',
                 'semibold',
                 'demibold',
                 'demi',
                 'bold',
                 'heavy',
                 'extra bold',
                 'black']
  HA_LIST     = ['center',
                 'right',
                 'left']
  VA_LIST     = ['center',
                 'top',
                 'bottom',
                 'baseline']
  
  sfclabel = ttk.Style()
  sfclabel.configure("sfclabel.TLabel",background=MPLOT.TCOLOR.get(),anchor="center")

  f0 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(f0,
            text='Font size').grid(row=0,
                                   column=0,
                                   padx=3)
  ttk.Entry(f0,
            textvariable=MPLOT.TSIZE,
            width=10).grid(row=0,
                           column=1,
                           padx=3,
                           sticky='w')
  ttk.Label(f0,
            text='Font style').grid(row=1,
                                   column=0,
                                   padx=3)
  ttk.Combobox(f0,
            textvariable=MPLOT.STYLE,
            values=STYLE_LIST,
            width=10).grid(row=1,
                           column=1,
                           padx=3,
                           sticky='w')
                           
  ttk.Label(f0,text='Font color').grid(row=2,column=0,padx=3)
  FCLabel = ttk.Label(f0,textvariable=MPLOT.TCOLOR,width=10,style="sfclabel.TLabel")
  FCLabel.grid(row=2,column=1)
  ttk.Button(f0,text='Select',command=lambda:colsel(MPLOT.TCOLOR, \
            sfclabel,FCLabel,"sfclabel.TLabel",master=parent)).grid(row=2,column=2)
                                  
  ttk.Label(f0,
            text='Font weight').grid(row=3,
                                   column=0,
                                   padx=3)
  ttk.Combobox(f0,
            textvariable=MPLOT.WEIGHT,
            values=WEIGHT_LIST,
            width=10).grid(row=3,
                           column=1,
                           padx=3,
                           sticky='w')
  ttk.Label(f0,
            text='Horizontal alignment').grid(row=4,
                                   column=0,
                                   padx=3)
  ttk.Combobox(f0,
            textvariable=MPLOT.HA,
            values=HA_LIST,
            width=10).grid(row=4,
                           column=1,
                           padx=3,
                           sticky='w')
  ttk.Label(f0,
            text='Vertical alignment').grid(row=5,
                                   column=0,
                                   padx=3)
  ttk.Combobox(f0,
            textvariable=MPLOT.VA,
            values=VA_LIST,
            width=10).grid(row=5,
                           column=1,
                           padx=3,
                           sticky='w')
  ttk.Label(f0,
            text='Text wrap').grid(row=6,
                                   column=0,
                                   padx=3)
  ttk.Combobox(f0,
            textvariable=MPLOT.WRAP,
            values=[True,False],
            width=10).grid(row=6,
                           column=1,
                           padx=3,
                           sticky='w')
  ttk.Label(f0,
            text='Zorder').grid(row=7,
                                   column=0,
                                   padx=3)
  ttk.Entry(f0,
            textvariable=MPLOT.ZORDER,
            width=10).grid(row=7,
                           column=1,
                           padx=3,
                           sticky='w')
  f0.grid()

# ======================================
def ShowData(master,LL):
# ======================================
  ''' Shows data '''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  log = tk.Text(master)
  log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')
  log.grid_columnconfigure(0,weight=0)
  log.grid_rowconfigure(0,weight=1)
  # Scrollbar
  scrollb = tk.Scrollbar(master,command=log.yview)
  scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
  log['yscrollcommand'] = scrollb.set

  '''for l in range(LL.n):
    string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {}\n'.format(l, \
                                                 LL.lon[l],     \
                                                 LL.lat[l],
                                                 LL.label[l])
    log.insert('end',string)'''

def main():

  root = tk.Tk()
  nn = filedialog.askopenfile()
  if nn is None:
    quit()
 
  filename = '%s' % nn.name
  #filename = 'trajectory_20171122.nc'
  #filename = 'histo-300234060640350.txt'
  root.title(filename)
  root.resizable(width=False,height=True)
  root.rowconfigure(0,weight=1)
  tr = Float(filename)
  ShowData(root,tr)
  root.mainloop()

if __name__ == '__main__':
  main()


 
