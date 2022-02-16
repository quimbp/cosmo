''' Module: feature.py
    Purpose: Provides classes and functions to give limited support to
    load and draw features from json files.
    COSMO-View project python module.
    QBP, February 2022:
    Entirely based on geomarker.py (from Cosmo project) and requires
    Cartopy, geoplot module (from Cosmo project) and colse() function
    from tools modules (from Cosmo project).
    A heap variable MESSAGE has been introduce to store "print" messages
'''

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog
from tkinter import font as tkfont

import matplotlib.pyplot as plt
from shapely.geometry.polygon import Polygon
import numpy as np
import json

import cosmo.geoplot as geoplot
from cosmo.tools import marker_string
from cosmo.tools import colsel
from cosmo.tools import toconsola

from cosmo import COSMO_CONF_PATH

try:
  to_unicode = unicode
except:
  to_unicode = str

import cartopy.crs as ccrs

__version__ = "1.0"    # Initial version
__author__  = "Quim Ballabrera"
__date__    = "February 2022"

class parameters():
# =================
  ''' Parameters for feature variables'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrera"
  __date__    = "February 2022"

  def __init__ (self):
  # ==================
    '''Feature object '''

    self.MESSAGE = ''

    self.json            = None
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

    self.crs_name        = None
    self.crs_code        = None
    self.features        = None

    self.LABEL_KEY.set('')
    self.SOURCE = 'FILE'
    self.CROP.set(False)
    self.show.set(True)

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


  def Read(self,filename):
  # ======================
    '''Opens and reads the feature from a JSON file'''

    __version__ = "1.0"
    __author__  = "Quim Ballabrera"
    __date__    = "February 2022"

    '''Read JSON file'''
    try:
      with open(filename,'r') as J:
        self.data = json.loads(J.read())
    except:
      print('ERROR: feature. Cannot read input file')
      self.data = None
      return

    self.FILENAME.set(filename)
    self.SOURCE = 'FILE'

    try:
      tmp = self.data['crs']['properties']['name']
      tmp = tmp.split('::')
      if len(tmp) == 2:
        a = tmp[0].split(':')
        self.crs_name = a[-1]
        self.crs_code = int(tmp[1])
      else:
        self.crs_name = None
        self.code = None
    except:
      self.crs_name = None
      self.code = None

   
    try:
      self.features = self.data['features']
      self.n = len(self.features)
    except:
      print('ERROR: feature. Cannot read features')
      self.features = None
      self.n = 0

  # ==================================
  def drawing(self,ax=None,proj=None):
  # ==================================

    if self.data is None:
      return

    if not self.show.get():
      return

    if ax is None:
      print('Defining map axis ...')
      fig = plt.figure()
      ax = fig.add_subplot(1,1,1,projection=ccrs.Mercator())
      ax.coastlines(resolution='10m',alpha=0.5)
      show_here = True
    else:
      show_here = False

    if self.crs_name.lower() == 'epsg':
      TRANSFORM=ccrs.epsg(self.crs_code)
    else:
      TRANSFORM=ccrs.PlateCarree()

    # Loop over features
    #
    for FF in self.features:

      if FF['geometry']['type'].lower() == 'point':
        coords = FF['geometry']['coordinates']
        ax.plot(coords[0],coords[1],
                linestyle='',                                  
                marker=marker_string(self.PLOT.SYMBOL.get()),
                ms=self.PLOT.SIZE.get(),                    
#               label=self.LABEL.get(),                    
                color=self.PLOT.COLOR.get(),              
                alpha=self.PLOT.ALPHA.get(),             
                zorder=self.PLOT.ZORDER.get(),          
                transform=TRANSFORM)


      if FF['geometry']['type'].lower() == 'linestring':
        coords = FF['geometry']['coordinates']
        xx = []
        yy = []
        for cc in coords:
          xx.append(cc[0])
          yy.append(cc[1])
        ax.plot(xx,yy,
                linewidth=self.PLOT.LINEWIDTH.get(),
                linestyle=self.PLOT.LINESTYLE.get(),
                alpha=self.PLOT.ALPHA.get(),
                color=self.PLOT.LINECOLOR.get(),
                zorder=self.PLOT.ZORDER.get(),
                transform=TRANSFORM)


      if FF['geometry']['type'].lower() == 'polygon':
        coords = FF['geometry']['coordinates']
        for ll in coords:
          pp = Polygon(ll)

          if self.PLOT.FILLED.get():
            ax.add_geometries([pp],
                         linewidth=self.PLOT.EDGEWIDTH.get(),
                         linestyle=self.PLOT.EDGESTYLE.get(),
                         alpha=self.PLOT.ALPHA.get(),
                         edgecolor=self.PLOT.EDGECOLOR.get(),
                         facecolor=self.PLOT.FACECOLOR.get(),
                         zorder=self.PLOT.ZORDER.get(),
                         crs=TRANSFORM)
          else:
            xx = []
            yy = []
            for cc in ll:
              xx.append(cc[0])
              yy.append(cc[1])
            ax.plot(xx,yy,
                    linewidth=self.PLOT.EDGEWIDTH.get(),
                    linestyle=self.PLOT.EDGESTYLE.get(),
                    alpha=self.PLOT.ALPHA.get(),
                    color=self.PLOT.EDGECOLOR.get(),
                    zorder=self.PLOT.ZORDER.get(),
                    transform=TRANSFORM)

    if show_here:
      plt.show()


def Configuration(parent,E):
# =================================================================
  ''' Interactive widget to modify the options of 2D contour plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "February 2022"

  # Symbol list using standard and utf technical symbols
  #
  SYMBOL_LIST = ['.',',','o','v','^','<','>', \
                '1','2','3','4','s','p','*',  \
                'h','H','+','x','D','d','|','_']
  STYLE_LIST =  ['-','--',':','-.','']
  for n in range(ord('\u2600'),ord('\u26B8')+1):
    SYMBOL_LIST.append(chr(n))

  # Bold font
  #
  font_bold = tkfont.Font(font='TkDefaultFont').copy()
  font_bold['weight']='bold'

  # Tkinter styles
  #
  tpad = ttk.Style()
  tpad.configure("tpad.TLabelframe",padding=[20,5,5,10])


  global sdlabel
  global sllabel
  global selabel
  global sflabel

  # Point
  #
  sdlabel = ttk.Style()
  sdlabel.configure("sdlabel.TLabel",background=E.PLOT.COLOR.get(),anchor="center")

  frame1=ttk.LabelFrame(parent,text='Point',borderwidth=5,style='tpad.TLabelframe')

  ttk.Label(frame1,text='Point Symbol').grid(row=0,column=3,padx=3,sticky='w')
  ttk.Combobox(frame1,textvariable=E.PLOT.SYMBOL, \
               values=SYMBOL_LIST,width=8).grid(row=0,column=4,padx=3,sticky='w')
  ttk.Label(frame1,text='Point Size').grid(row=1,column=3,padx=3,sticky='w')
  ttk.Entry(frame1,textvariable=E.PLOT.SIZE,width=8).grid(row=1,column=4,sticky='w')
  ttk.Label(frame1,text='Point Color').grid(row=2,column=3,padx=3,sticky='w')
  DLabel = ttk.Label(frame1,textvariable=E.PLOT.COLOR,width=8,style="sdlabel.TLabel")
  DLabel.grid(row=2,column=4,padx=3)
  ttk.Button(frame1,text='Select',command=lambda:colsel(E.PLOT.COLOR, \
            sdlabel,DLabel,"sdlabel.TLabel",master=parent)).\
            grid(row=2,column=5,sticky='ew')
  frame1.grid()

  # Lines
  #
  sllabel = ttk.Style()
  sllabel.configure("sllabel.TLabel",background=E.PLOT.LINECOLOR.get(),anchor="center")
  frame2=ttk.LabelFrame(parent,text='LineString',borderwidth=5,style='tpad.TLabelframe')

  ttk.Label(frame2,text='Line Style').grid(row=0,column=3,padx=3,sticky='w')
  ttk.Combobox(frame2,textvariable=E.PLOT.LINESTYLE, \
               values=STYLE_LIST,width=8).grid(row=0,column=4,padx=3,sticky='w')
  ttk.Label(frame2,text='Line Width').grid(row=1,column=3,padx=3,sticky='w')
  ttk.Entry(frame2,textvariable=E.PLOT.LINEWIDTH,width=8).grid(row=1,column=4,sticky='w')
  ttk.Label(frame2,text='Line Color').grid(row=2,column=3,padx=3,sticky='w')
  LLabel = ttk.Label(frame2,textvariable=E.PLOT.LINECOLOR,width=8,style="sllabel.TLabel")
  LLabel.grid(row=2,column=4,padx=3)
  ttk.Button(frame2,text='Select',command=lambda:colsel(E.PLOT.LINECOLOR, \
            sllabel,LLabel,"sllabel.TLabel",master=parent)).\
            grid(row=2,column=5,sticky='ew')
  frame2.grid()

  # Polygones
  #
  selabel = ttk.Style()
  sflabel = ttk.Style()
  selabel.configure("selabel.TLabel",background=E.PLOT.EDGECOLOR.get(),anchor="center")
  sflabel.configure("sflabel.TLabel",background=E.PLOT.FACECOLOR.get(),anchor="center")

  frame3=ttk.LabelFrame(parent,text='Polygon',borderwidth=5,style='tpad.TLabelframe')

  ttk.Label(frame3,text='Line Style').grid(row=0,column=3,padx=3,sticky='w')
  ttk.Combobox(frame3,textvariable=E.PLOT.EDGESTYLE, \
               values=STYLE_LIST,width=8).grid(row=0,column=4,padx=3,sticky='w')
  ttk.Label(frame3,text='Line Width').grid(row=1,column=3,padx=3,sticky='w')
  ttk.Entry(frame3,textvariable=E.PLOT.EDGEWIDTH,width=8).grid(row=1,column=4,sticky='w')
  ttk.Label(frame3,text='Line Color').grid(row=2,column=3,padx=3,sticky='w')
  LEabel = ttk.Label(frame3,textvariable=E.PLOT.EDGECOLOR,width=8,style="selabel.TLabel")
  LEabel.grid(row=2,column=4,padx=3)
  ttk.Button(frame3,text='Select',command=lambda:colsel(E.PLOT.EDGECOLOR, \
            selabel,LEabel,"selabel.TLabel",master=parent)).\
            grid(row=2,column=5,sticky='ew')
  tk.Checkbutton(frame3,variable=E.PLOT.FILLED,text='Filled').grid(row=3,column=3,sticky='w')
  ttk.Label(frame3,text='Face Color').grid(row=4,column=3,padx=3,sticky='w')
  LFabel = ttk.Label(frame3,textvariable=E.PLOT.FACECOLOR,width=8,style="sflabel.TLabel")
  LFabel.grid(row=4,column=4,padx=3)
  ttk.Button(frame3,text='Select',command=lambda:colsel(E.PLOT.FACECOLOR, \
            sflabel,LFabel,"sflabel.TLabel",master=parent)).\
            grid(row=4,column=5,sticky='ew')
  frame3.grid()

  frame4=ttk.Frame(parent)
  ttk.Label(frame4,text='Show:').grid(row=0,column=0,sticky='we')
  tk.Checkbutton(frame4,variable=E.show).grid(row=0,column=1,sticky='w')
  ttk.Label(frame4,text='Alpha:').grid(row=0,column=2,sticky='e')
  ttk.Entry(frame4,textvariable=E.PLOT.ALPHA,width=10).grid(row=0,column=3,sticky='we')
  ttk.Label(frame4,text='Zorder:').grid(row=0,column=4,sticky='e')
  ttk.Entry(frame4,textvariable=E.PLOT.ZORDER,width=10).grid(row=0,column=5,sticky='we')
  frame4.grid()



def Configuration_Menu(DRAWING,OBJECT):
# =====================================
  ''' Sets the menu for configuring the Feature appearance''' 

  global eshow
  global Window
 
  ii = OBJECT.INDX.get()

  def cancel():
  # ================
    Window.destroy()
    DRAWING.Window_featureconfig = None

  def _apply():
  # ===========
    DRAWING.make_plot()

  def _done():
  # ==========
    DRAWING.make_plot()
    cancel()


  def loadconf():
  # =============
    '''Load feature configuration'''

    global sdlabel
    global sllabel
    global selabel
    global sflabel
    filename = OBJECT.DATA[ii].PLOT.FILECONF

    toconsola('Restoring feature configuration from '+
          filename,wid=DRAWING.cons)
    try:
      OBJECT.DATA[ii].PLOT.load(filename)
      sdlabel.configure("sdlabel.TLabel",background=OBJECT.DATA[ii].PLOT.COLOR.get(),anchor="center")
      sllabel.configure("sllabel.TLabel",background=OBJECT.DATA[ii].PLOT.LINECOLOR.get(),anchor="center")
      selabel.configure("selabel.TLabel",background=OBJECT.DATA[ii].PLOT.EDGECOLOR.get(),anchor="center")
      sflabel.configure("sflabel.TLabel",background=OBJECT.DATA[ii].PLOT.FACECOLOR.get(),anchor="center")
      DRAWING.make_plot()
    except:
      toconsola('Error: Unable to load file '+
            filename,wid=DRAWING.cons)

  def saveconf():
  # =============
    '''Load feature configuration'''

    filename = OBJECT.DATA[ii].PLOT.FILECONF

    toconsola('Saving feature configuration to '+
          filename,wid=DRAWING.cons)
    try:
      OBJECT.DATA[ii].PLOT.save(filename)
    except:
      toconsola('Error: Unable to write file '+
          filename,wid=DRAWING.cons)

  def loadfromconf():
  # ==================
    '''Load feature configuration from a file'''

    global sdlabel
    global sllabel
    global selabel
    global sflabel

    nn = filedialog.askopenfilename(title='Load feature configuration',
                                    parent=Window,
                                    initialdir=COSMO_CONF_PATH)
    if len(nn) == 0:
      return

    OBJECT.DATA[ii].PLOT.FILECONF = '%s' % nn
    toconsola('Restoring feature configuration from '+
          OBJECT.DATA[ii].PLOT.FILECONF,wid=DRAWING.cons)
    try:
      OBJECT.DATA[ii].PLOT.load(OBJECT.DATA[ii].PLOT.FILECONF)
      sdlabel.configure("sdlabel.TLabel",background=OBJECT.DATA[ii].PLOT.COLOR.get(),anchor="center")
      sllabel.configure("sllabel.TLabel",background=OBJECT.DATA[ii].PLOT.LINECOLOR.get(),anchor="center")
      selabel.configure("selabel.TLabel",background=OBJECT.DATA[ii].PLOT.EDGECOLOR.get(),anchor="center")
      sflabel.configure("sflabel.TLabel",background=OBJECT.DATA[ii].PLOT.FACECOLOR.get(),anchor="center")
      DRAWING.make_plot()
    except:
      toconsola('Error: Unable to load file '+
            OBJECT.DATA[ii].PLOT.FILECONF,wid=DRAWING.cons)


  def saveasconf():
  # ================
    '''Load feature configuration'''
    nn = filedialog.asksaveasfilename(title='Save feature configuration',
                                      parent=Window,
                                      initialdir=COSMO_CONF_PATH,
                                      confirmoverwrite=True)
    if nn is None or len(nn) == 0:
      return


    OBJECT.DATA[ii].PLOT.FILECONF = '%s' % nn
    toconsola('Saving feature configuration to '+
          OBJECT.DATA[ii].PLOT.FILECONF,wid=DRAWING.cons)
    try:
      OBJECT.DATA[ii].PLOT.save(OBJECT.DATA[ii].PLOT.FILECONF)
    except:
      toconsola('Error: Unable to write file '+
          OBJECT.DATA[ii].PLOT.FILECONF,wid=DRAWING.cons)

    if Window is not None:
      Window.lift()
      return

  def selected():
  # ===============

    global eshow
    eshow.destroy()

    ii = OBJECT.INDX.get()
    eshow = ttk.Frame(Window,padding=10)

    Configuration(eshow,OBJECT.DATA[ii])

    f0 = ttk.Frame(eshow,padding=5)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Close',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)
    eshow.grid()

  # Main window
  # ============

  Window = tk.Toplevel(DRAWING.master)
  Window.title('Feature plot configuration')
  Window.resizable(width=False,height=False)
  Window.protocol('WM_DELETE_WINDOW',cancel)

  # ---
  DRAWING.Window_featureconfig = Window
  # ---

  menubar = tk.Menu(Window)
  menu = tk.Menu(menubar,tearoff=0)
  menubar.add_cascade(label='Configuration',menu=menu)
  menu.add_command(label='Restore',command=loadconf)
  menu.add_command(label='Restore from',command=loadfromconf)
  menu.add_command(label='Save',command=saveconf)
  menu.add_command(label='Save as',command=saveasconf)
  try:
    Window.config(menu=menubar)
  except AttributeError:
    # master is a toplevel window (Python 2.4/Tkinter 1.63)
    DRAWING.master.tk.call(Window, "config", "-menu", menubar)


  fsel = ttk.Frame(Window,padding=10)
  ttk.Label(fsel,text="File: ").grid(row=0,column=0,sticky='e',padx=3)
  _wsel = ttk.Combobox(fsel,textvariable=OBJECT.INDX,
                            values=OBJECT.LIST,width=5)
  _wsel.grid(row=0,column=1,sticky='w',padx=3)
  _wsel.bind('<<ComboboxSelected>>',lambda e:_selected())
  _went = ttk.Entry(fsel,justify='left',width=50,state='readonly')
  _went.grid(row=0,column=2,columnspan=5,padx=3,sticky='w')
  _went = ttk.Entry(fsel,justify='left',width=80,state='readonly')
  _went.grid(row=0,column=2,columnspan=8,padx=3,sticky='w')
  fsel.grid()

  _went ['textvariable'] = OBJECT.DATA[ii].FILENAME

  eshow = ttk.Frame(Window,padding=10)

  Configuration(eshow,OBJECT.DATA[ii])

  f0 = ttk.Frame(eshow,padding=5)
  ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
      grid(row=0,column=1,padx=3)
  ttk.Button(f0,text='Close',command=_done,padding=5).     \
      grid(row=0,column=2,padx=3)
  f0.grid(sticky='ew',columnspan=3)
  eshow.grid()


def main():

  root = tk.Tk()
  nn = filedialog.askopenfile()
  if nn is None:
    quit()

  filename = '%s' % nn.name
  root.title(filename)
  root.resizable(width=False,height=True)
  root.rowconfigure(0,weight=1)
  f = parameters()
  f.Read(filename)
  f.PLOT.FACECOLOR.set('Yellow')
  f.PLOT.FILLED.set(True)
  f.PLOT.ALPHA.set(0.3)
  f.drawing()

  root.mainloop()

if __name__ == '__main__':
  main()


