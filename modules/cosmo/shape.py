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
#from cartopy.io.shapereader import Reader
from cartopy.feature import ShapelyFeature
import cartopy.crs as ccrs

#QB new shapefile version
import cartopy
from cartopy.io import shapereader
from shapely import geometry


__version__ = "2.0"
__author__  = "Quim Ballabrera"
__date__    = "December 2020"

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
    
    self.shp             = None
#    self.geom            = None
    self.type            = None
    self.FILENAME        = tk.StringVar()
    self.ALIAS           = tk.StringVar()
    self.SOURCE          = ''
    self.show            = tk.BooleanVar()

    self.PLOT            = geoplot.parameters()
    self.LABEL           = tk.StringVar()
    self.textmode        = tk.BooleanVar()
    self.n               = 0
    self.record          = []
    self.KEY_LIST        = []
    self.LABEL_KEY       = tk.StringVar() 
    self.CROP            = tk.BooleanVar()
    self.namestohide     = ''


    self.LABEL_KEY.set('')
    self.SOURCE = 'FILE'
    self.CROP.set(False)
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
    conf['LABEL'] = self.LABEL.get()
    conf['LABEL_KEY'] = self.LABEL_KEY.get()
    conf['CROP'] = self.CROP.get()
    conf['NAMESTOHIDE'] = self.namestohide
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
    self.LABEL.set(conf['LABEL'])
    self.LABEL_KEY.set(conf['LABEL_KEY'])
    self.CROP.set(conf['CROP'])
    self.namestohide = conf['NAMESTOHIDE']
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

    __version__ = "2.0"
    __author__  = "Quim Ballabrera"
    __date__    = "December 2018"


    '''Read a shp file'''
    try:
      self.shp = shapereader.Reader(filename)
    except:
      print('ERROR: shapereader.Reader cannot open this file')
      self = None
      return

    self.FILENAME.set(filename)
    self.SOURCE = 'FILE'

    self.n   = len(self.shp)
    self.record = list(self.shp.records())
    self.type     = self.record[0].geometry.geom_type.upper()


    # Label Key:
    self.KEY_LIST = list(self.record[0].attributes.keys())
    self.KEY_LIST.insert(0,' ')

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

  def Crop(self,bbox):
  # ==================
    xmin = bbox[0]
    xmax = bbox[1]
    ymin = bbox[2]
    ymax = bbox[3]
    pgon = geometry.Polygon(((xmin, ymin),  \
                             (xmin, ymax),  \
                             (xmax, ymax),  \
                             (xmax, ymin),  \
                             (xmin, ymin)))

    if self.type == 'POINT':
      # Shapes that are points 
      #
      for i in range(self.n-1,-1,-1):
        point = self.record[i]
        x = point.geometry.x
        y = point.geometry.y
        remove = False
        if x <= xmin or x>= xmax:
          remove = True
        if y <= ymin or y>= ymax:
          remove = True
        if remove:
          del self.record[i]

    else:
      # Shapes other than points 
      #
      for i in range(self.n-1,-1,-1):
        feature = self.record[i]

        # Check for intersection between element's bbox (egon) and map (pgon).
        #
        areas = []
        for element in feature.geometry:    # Allows for multipolygons
          exmin = element.bounds[0]
          exmax = element.bounds[2]
          eymin = element.bounds[1]
          eymax = element.bounds[3]
          egon = geometry.Polygon(((exmin, eymin),  \
                                   (exmin, eymax),  \
                                   (exmax, eymax),  \
                                   (exmax, eymin),  \
                                   (exmin, eymin)))
          areas.append(egon.intersection(pgon).area)
        # If not significant, remove the shape:
        if sum(areas) <= 1E-5:
          del self.record[i]

    self.n = len(self.record)      # Set the new number of records


# ======================================
def drawing(ax,proj,SHAPE):
# ======================================
  '''Draw geometries from shapes files'''

  __version__ = "2.0"
  __author__  = "Emili Garcia, based on Joaquin Ballabrera geomarker.py"
  __date__    = "December 2020"

  if not SHAPE.show.get():
    return

  # Axis lims
  xmin, xmax = ax.get_xlim()
  ymin, ymax = ax.get_ylim()
  # Transform to lon,lat:
  xmin, ymin = ccrs.PlateCarree().transform_point(xmin,ymin,ax.projection)
  xmax, ymax = ccrs.PlateCarree().transform_point(xmax,ymax,ax.projection)

  xpad = SHAPE.PLOT.XPAD.get()
  ypad = SHAPE.PLOT.YPAD.get()

  # BBOX:
  if SHAPE.PLOT.BBOX.get():
    bbox = dict(facecolor=SHAPE.PLOT.BBOX_FACECOLOR.get(),
                alpha=SHAPE.PLOT.BBOX_ALPHA.get())
  else:
    bbox = None

  lshp = None

  # - - - - - - - - - - - - -
  if 'POINT' in SHAPE.type:
  # - - - - - - - - - - - - -

    for i in range(SHAPE.n):
      point = SHAPE.record[i]
      x = point.geometry.x
      y = point.geometry.y

      try:
        label = point.attributes[SHAPE.LABEL_KEY.get()]
        if label.upper() in SHAPE.namestohide:
          plotit = False     # Label in names to hide list
        else:
          plotit = True      # Label not in names to hide list
      except:
        label  = None
        plotit = True


      if x <= xmin or x>= xmax:
        plotit = False
      if y <= ymin or y>= ymax:
        plotit = False

      if plotit:
       
        lshp, = ax.plot(x,y,             \
                  linestyle='',                                   \
                  marker=marker_string(SHAPE.PLOT.SYMBOL.get()),  \
                  ms=SHAPE.PLOT.SIZE.get(),                       \
                  label=SHAPE.LABEL.get(),                        \
                  color=SHAPE.PLOT.COLOR.get(),                   \
                  alpha=SHAPE.PLOT.ALPHA.get(),                   \
                  zorder=SHAPE.PLOT.ZORDER.get(),
                  transform=ccrs.PlateCarree())

        if SHAPE.textmode.get():
          # Here, every marker is identified by its label
          ax.text(xpad+x,ypad+y,label,
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


  # - - - - - - - - - - - - -
  elif 'LINE' in SHAPE.type:
  # - - - - - - - - - - - - -

    for record in SHAPE.record:

      # Get label and check if has been asked not to be shown
      #
      try:
        label = record.attributes[SHAPE.LABEL_KEY.get()]
        if label.upper() in SHAPE.namestohide:
          plotit = False     # Label in names to hide list
        else:
          plotit = True      # Label not in names to hide list
      except:
        label  = None
        plotit = True


      if plotit:
        for line in record.geometry:
          x,y = line.xy
          lshp, = ax.plot(x,y,
                          linewidth=SHAPE.PLOT.LINEWIDTH.get(),
                          linestyle=SHAPE.PLOT.LINESTYLE.get(),
                          alpha=SHAPE.PLOT.ALPHA.get(),
                          color=SHAPE.PLOT.EDGECOLOR.get(),
                          zorder=SHAPE.PLOT.ZORDER.get(),
                          transform=ccrs.PlateCarree())

        if SHAPE.textmode.get():
          # Here, every marker is identified by its label

          # Get the center of the longest segment
          #
          lmax = 0
          for line in record.geometry:
            x,y = line.xy
            if len(x) > lmax:
              lmax = len(x)
              xmean = np.mean(x)
              ymean = np.mean(y)

          # Place the text
          #
          ax.text(xpad+xmean,ypad+ymean,label,
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


  # - - - - - - - - - - - - -
  else:
  # - - - - - - - - - - - - -
    # Geometries other than POINT or LINESTRINGS:
    #
    for record in SHAPE.record:

      # Get label and check if has been asked not to be shown
      #
      try:
        label = record.attributes[SHAPE.LABEL_KEY.get()]
        if label.upper() in SHAPE.namestohide:
          plotit = False     # Label in names to hide list
        else:
          plotit = True      # Label not in names to hide list
      except:
        label  = None
        plotit = True

      if plotit:
        if SHAPE.PLOT.FILLED.get():
          shape_feature = ShapelyFeature(record.geometry,ccrs.PlateCarree())
          ax.add_feature(shape_feature, 
                         linewidth=SHAPE.PLOT.LINEWIDTH.get(),
                         linestyle=SHAPE.PLOT.LINESTYLE.get(),
                         alpha=SHAPE.PLOT.ALPHA.get(),
                         edgecolor=SHAPE.PLOT.EDGECOLOR.get(),
                         facecolor=SHAPE.PLOT.FACECOLOR.get(),
                         zorder=SHAPE.PLOT.ZORDER.get())
        else:
          for element in record.geometry:
            x,y = element.exterior.coords.xy
            lshp, = ax.plot(x,y,
                            linewidth=SHAPE.PLOT.LINEWIDTH.get(),
                            linestyle=SHAPE.PLOT.LINESTYLE.get(),
                            alpha=SHAPE.PLOT.ALPHA.get(),
                            color=SHAPE.PLOT.EDGECOLOR.get(),
                            zorder=SHAPE.PLOT.ZORDER.get(),
                            transform=ccrs.PlateCarree())

        if SHAPE.textmode.get():
          # label variable holds the name of the feature
          # We will label the largest feature
          amax = 0
          imax = -1
          for i  in range(len(record.geometry)):
            element = record.geometry[i]
            area = element.area
            if area > amax:
              amax = area
              imax = i

          # Now label at the center of the larger area:
          #
          geom = record.geometry[imax]
          xx = geom.centroid.x
          yy = geom.centroid.y
          ss = ax.text(xpad+xx,ypad+yy,label,
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

  return lshp

                
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

# ======================================
def HideData(master,LL):
# ======================================
  ''' Hide data '''

  __version__ = "2.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "December 2020"

  def _clear(board):
    board.delete('1.0',tk.END)
    LL.namestohide = ''

  def _hide(board):
    LL.namestohide = board.get('1.0',tk.END).upper()

  F0 = tk.Frame(master)
  tk.Label(F0,text='Enter the names of the Features to hide:').grid(row=0,column=0)
  tk.Label(F0,text='[Label Key (Label Aspect TAB) must be selected]').grid(row=1,column=0)
  board = tk.Text(F0,height=6)
  board.grid(row=2,column=0,padx=10,pady=10,sticky='nsew')
  board.grid_columnconfigure(0,weight=0)
  board.grid_rowconfigure(0,weight=1)
  # Scrollbar
  scrollb = tk.Scrollbar(F0,command=board.yview)
  scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
  board['yscrollcommand'] = scrollb.set

  F0.grid()
  F1 = tk.Frame(master)
  ttk.Button(F1,text='Clear',command=lambda:_clear(board)).grid(row=0,column=0)
  ttk.Button(F1,text='Hide',command=lambda:_hide(board)).grid(row=0,column=1)
  F1.grid()



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


 
