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
    self.ALIAS           = tk.StringVar()
    self.SOURCE          = ''
    self.show            = tk.BooleanVar()

    self.PLOT            = geoplot.parameters()
    self.LABEL           = tk.StringVar()
    self.n               = 0
    self.textmode        = tk.BooleanVar()
    self.lon             = []
    self.lat             = []
    self.name            = []
    self.KEY_LIST        = []
    self.LABEL_KEY       = tk.StringVar() 

    self.LABEL_KEY.set('')
    self.SOURCE = 'FILE'
    self.show.set(True)
    
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
    conf['FILENAME'] = self.FILENAME.get()
    conf['ALIAS'] = self.ALIAS.get()
    conf['SOURCE'] = self.SOURCE
    conf['SHOW'] = self.show.get()
    conf['TEXTMODE'] = self.textmode.get()
    conf['LABEL_KEY'] = self.LABEL_KEY.get()
    conf['PLOT'] = self.PLOT.conf_get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    self.FILENAME.set(conf['FILENAME'])
    self.ALIAS.set(conf['ALIAS'])
    self.SOURCE = conf['SOURCE']
    self.show.set(conf['SHOW'])
    self.textmode.set(conf['TEXTMODE'])
    self.LABEL_KEY.set(conf['LABEL_KEY'])
    self.PLOT.conf_set(conf['PLOT'])

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
    self.SOURCE = 'FILE'

    if filename.lower().endswith(('.shp')):
      read(filename)
      
    self.n = len(self.lon)
    self.geom = self.shp
    #self.type = self.shp.shapeType

    # Label Key:
    records = list(self.shp.records())
    self.KEY_LIST = list(records[0].attributes.keys())
    self.KEY_LIST.insert(0,' ')
    del records

                          
    # Cheack that something has been read:
    if self.n == 0:
      self = None
      return

  # --------------------------------------
  def get_name(self):
  # --------------------------------------

    if empty(self.LABEL_KEY.get()):
      print('No name selected')

    records = list(self.shp.records())
    self.name = []
    for i in range(len(records)):
      self.name.append(records[i].attributes[self.LABEL_KEY.get()])


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

  def keys(self):
  # =============
    records = list(reader.records())

    self.KEY_LIST = list(records[0].attributes.keys())
    self.KEY_LIST.insert(0,' ')


# ======================================
def drawing(ax,proj,SHAPE):
# ======================================
  '''Draw geometries from shapes files'''

  __version__ = "1.0"
  __author__  = "Emili Garcia, based on Joaquin Ballabrera geomarker.py"
  __date__    = "May 2018"

  if not SHAPE.show.get():
    return

  # Axis lims
  xlim = ax.get_xlim()
  ylim = ax.get_ylim()

  records = list(SHAPE.geom.records())
  geoms   = list(SHAPE.geom.geometries())
  GTYPE   = type(geoms[0]).__name__.upper()

  xpad = SHAPE.PLOT.XPAD.get()
  ypad = SHAPE.PLOT.YPAD.get()

  # BBOX:
  if SHAPE.PLOT.BBOX.get():
    bbox = dict(facecolor=SHAPE.PLOT.BBOX_FACECOLOR.get(),
                alpha=SHAPE.PLOT.BBOX_ALPHA.get())
  else:
    bbox = None

  lshp = None
  if GTYPE[0:5] == 'POINT':
    for i in range(len(records)):
      poly = geoms[i]
      name = '%s' % records[i].attributes['name']
      if poly.x > xlim[0] and poly.x < xlim[1]:
        if poly.y > ylim[0] and poly.y < ylim[1]:
          lshp, = ax.plot(poly.x,poly.y,                                  \
                  linestyle='',                                   \
                  marker=marker_string(SHAPE.PLOT.SYMBOL.get()),  \
                  ms=SHAPE.PLOT.SIZE.get(),                       \
                  label=SHAPE.LABEL.get(),                        \
                  color=SHAPE.PLOT.COLOR.get(),                   \
                  alpha=SHAPE.PLOT.ALPHA.get(),                   \
                  zorder=SHAPE.PLOT.ZORDER.get(),
                  transform=ccrs.PlateCarree())
                  #transform=proj)

          if SHAPE.textmode.get():
            # Here, every marker is identified by its label
            ax.text(xpad+poly.x,ypad+poly.y,SHAPE.name[i],
                 ha=SHAPE.PLOT.HA.get(),
                 va=SHAPE.PLOT.VA.get(),
                 wrap=SHAPE.PLOT.WRAP.get(),
                 style=SHAPE.PLOT.STYLE.get(),
                 weight=SHAPE.PLOT.WEIGHT.get(),
                 color=SHAPE.PLOT.TCOLOR.get(),
                 size=SHAPE.PLOT.TSIZE.get(),
                 zorder=SHAPE.PLOT.ZORDER.get()+1,
                 rotation=SHAPE.PLOT.ANGLE.get(),
                 bbox=bbox,
                 transform=ccrs.PlateCarree())
                 #transform=proj)


  else:
    for i in range(len(geoms)):
      feature = ShapelyFeature(geoms[i], proj)
      ax.add_feature(feature,
               visible=SHAPE.PLOT.SHOW.get(),
               linewidth=SHAPE.PLOT.LINEWIDTH.get(),
               alpha=SHAPE.PLOT.ALPHA.get(),
               edgecolor=SHAPE.PLOT.EDGECOLOR.get(),
               facecolor=SHAPE.PLOT.FACECOLOR.get(),
               zorder=SHAPE.PLOT.ZORDER.get())
      if SHAPE.textmode.get():
        # Here, every marker is identified by its label
        xx = geoms[i].centroid.x
        yy = geoms[i].centroid.y
        ss = ax.text(xpad+xx,ypad+yy,SHAPE.name[i],
                ha=SHAPE.PLOT.HA.get(),
                va=SHAPE.PLOT.VA.get(),
                wrap=SHAPE.PLOT.WRAP.get(),
                style=SHAPE.PLOT.STYLE.get(),
                weight=SHAPE.PLOT.WEIGHT.get(),
                color=SHAPE.PLOT.TCOLOR.get(),
                size=SHAPE.PLOT.TSIZE.get(),
                zorder=SHAPE.PLOT.ZORDER.get()+1,
                rotation=SHAPE.PLOT.ANGLE.get(),
                #bbox=dict(facecolor='white',alpha=0.5),
                #bbox=None,
                bbox=bbox,
                transform=ccrs.PlateCarree())
                #transform=proj)

  return lshp

#
#    feature = ShapelyFeature(SHAPE.geom.geometries(), proj)
#    ax.add_feature(feature,
#             visible=SHAPE.PLOT.SHOW.get(),
#             linewidth=SHAPE.PLOT.LINEWIDTH.get(),
#             #label=SHAPE.LABEL.get(),
#             label='nomed',
#             alpha=SHAPE.PLOT.ALPHA.get(),
#             edgecolor=SHAPE.PLOT.EDGECOLOR.get(),
#             facecolor=SHAPE.PLOT.FACECOLOR.get(),
#             zorder=SHAPE.PLOT.ZORDER.get())
                
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

  bbxlabel = ttk.Style()
  bbxlabel.configure("bbxlabel.TLabel",background=MPLOT.BBOX_FACECOLOR.get(),anchor="center")
  
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
            text='Text angle').grid(row=7,
                                   column=0,
                                   padx=3)
  ttk.Entry(f0,
            textvariable=MPLOT.ANGLE,
            width=10).grid(row=7,
                           column=1,
                           padx=3,
                           sticky='w')
  ttk.Label(f0,
            text='x pad').grid(row=8,
                                   column=0,
                                   padx=3)
  ttk.Entry(f0,
            textvariable=MPLOT.XPAD,
            width=10).grid(row=8,
                           column=1,
                           padx=3,
                           sticky='w')
  ttk.Label(f0,
            text='y pad').grid(row=9,
                                   column=0,
                                   padx=3)
  ttk.Entry(f0,
            textvariable=MPLOT.YPAD,
            width=10).grid(row=9,
                           column=1,
                           padx=3,
                           sticky='w')

  ttk.Label(f0,
            text='Show text BBox').grid(row=10,
                                   column=0,
                                   padx=3)
  ttk.Checkbutton(f0,variable=MPLOT.BBOX).grid(row=10,column=1,sticky='w')

  ttk.Label(f0,
            text='BBox Facecolor').grid(row=11,
                                   column=0,
                                   padx=3)
  BCLabel = ttk.Label(f0,textvariable=MPLOT.BBOX_FACECOLOR,width=10,style="bbxlabel.TLabel")
  BCLabel.grid(row=11,column=1)
  ttk.Button(f0,text='Select',command=lambda:colsel(MPLOT.BBOX_FACECOLOR, \
            bbxlabel,BCLabel,"bbxlabel.TLabel",master=parent)).grid(row=11,column=2)

  ttk.Label(f0,
            text='BBOX Alpha').grid(row=12,
                                   column=0,
                                   padx=3)
  ttk.Entry(f0,
            textvariable=MPLOT.BBOX_ALPHA,
            width=10).grid(row=12,
                           column=1,
                           padx=3,
                           sticky='w')
  #ttk.Label(f0,
  #          text='Zorder').grid(row=10,
  #                                 column=0,
  #                                 padx=3)
  #ttk.Entry(f0,
  #          textvariable=MPLOT.ZORDER,
  #          width=10).grid(row=10,
  #                         column=1,
  #                         padx=3,
  #                         sticky='w')
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


 
