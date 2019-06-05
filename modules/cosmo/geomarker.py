''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Geomarker class and functions'''

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkinter import filedialog
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  import tkFileDialog as filedialog

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

import cosmo.dotplot as dotplot
from cosmo.tools import marker_string
from cosmo.tools import empty

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
    self.FILENAME        = tk.StringVar()

    self.PLOT            = dotplot.parameters()
    self.LABEL           = tk.StringVar()
    self.n               = 0
    self.textmode        = tk.BooleanVar()
    self.lon             = []
    self.lat             = []
    self.label           = []

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
    def read_csv(filename):
    # --------------------------------------
      '''Read a trajectory from a txt file'''

      with open(filename,'r') as datafile:
        reader = csv.reader(datafile)
        for row in reader:
          self.lon.append(float(row[0]))
          self.lat.append(float(row[1]))
          if len(row) == 3:
            self.label.append(row[2])
          else:
            self.label.append(None)

    # --------------------------------------
    def read_txt(filename):
    # --------------------------------------
      '''Read a trajectory from a txt file'''

      with open(filename) as datafile:
        for line in datafile.readlines():
          line = line.strip()
          columns = line.split(',')

          self.lon.append(float(columns[0]))
          self.lat.append(float(columns[1]))
          if len(columns) == 3:
            self.label.append(columns[2])
          else:
            self.label.append(None)

    self.FILENAME.set(filename)

    if filename.lower().endswith(('.txt')):
      read_txt(filename)

    elif filename.lower().endswith(('.csv')):
      read_csv(filename)

    elif filename.lower().endswith(('.dat','.data')):
      read_txt(filename)

    self.n = len(self.lon)

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
        plot.plot(self.lon[i],self.lat[i],                       \
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
def drawing(fig,ax,m,MARKER):
# ======================================
  ''' Draw a 2D vector plot. Option of vectors or stream function'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "May 2018"

  west  = m.__dict__['xmin']
  east  = m.__dict__['xmax']
  south = m.__dict__['ymin']
  north = m.__dict__['ymax']
  #west,east = ax.get_xlim()
  #south,north = ax.get_ylim()
  xv = []
  yv = []
  lv = []
  for i in range(MARKER.n):
    xx,yy = m(MARKER.lon[i],MARKER.lat[i])
    if xx > west and xx < east and \
       yy > south and yy < north:
      xv.append(xx)
      yv.append(yy)
      lv.append(MARKER.label[i])
  nn = len(xv)
  
  if nn > 0:
    if MARKER.textmode.get():
      # Here, every marker is identified by its label
      for i in range(nn):
        ax.text(xv[i],yv[i],lv[i],
                 ha=MARKER.PLOT.HA.get(),
                 va=MARKER.PLOT.VA.get(),
                 wrap=MARKER.PLOT.WRAP.get(),
                 style=MARKER.PLOT.STYLE.get(),
                 weight=MARKER.PLOT.WEIGHT.get(),
                 color=MARKER.PLOT.TCOLOR.get(),
                 size=MARKER.PLOT.TSIZE.get(),
                 zorder=MARKER.PLOT.ZORDER.get(),
                )
        m.plot(xv[i],yv[i],linestyle='',
               marker=marker_string(MARKER.PLOT.SYMBOL.get()),
               ms=MARKER.PLOT.SIZE.get(),
               alpha=MARKER.PLOT.ALPHA.get(),
               color=MARKER.PLOT.COLOR.get(),
               zorder=MARKER.PLOT.ZORDER.get(),
              )
                
    else:
      m.plot(xv,yv,linestyle='',
             marker=marker_string(MARKER.PLOT.SYMBOL.get()),
             ms=MARKER.PLOT.SIZE.get(),
             visible=MARKER.PLOT.SHOW.get(),
             label=MARKER.LABEL.get(),
             alpha=MARKER.PLOT.ALPHA.get(),
             color=MARKER.PLOT.COLOR.get(),
             zorder=MARKER.PLOT.ZORDER.get(),
            )
                

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
  ttk.Label(f0,
            text='Font color').grid(row=2,
                                   column=0,
                                   padx=3)
  ttk.Entry(f0,
            textvariable=MPLOT.TCOLOR,
            width=10).grid(row=2,
                           column=1,
                           padx=3,
                           sticky='w')
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
  ''' Shows data from a LAgrangian Trajectory'''

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

  for l in range(LL.n):
    string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {}\n'.format(l, \
                                                 LL.lon[l],     \
                                                 LL.lat[l],
                                                 LL.label[l])
    log.insert('end',string)

  #F0 = ttk.Frame(master)
  #ttk.Label(F0,text='Floater:').grid(row=0,column=0,padx=3)
  #ibox = ttk.Combobox(F0,textvariable=LL.I,width=5)
  #ibox.grid(row=0,column=1)
  #ibox.bind('<<ComboboxSelected>>',lambda e: iselection(LL))
  #ibox['values'] = list(range(LL.nfloats))
  #if LL.nfloats == 1:
  #  ibox.configure(state='disabled')
  #else:
  #  ibox.configure(state='!disabled')
  #F0.grid()


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


 
