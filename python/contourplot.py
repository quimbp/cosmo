# Module for the applications built for the COSMO project 
# Quim Ballabrera, May 2017

try:
  import tkinter as tk
  from tkinter import ttk
  from tkcolorpicker import askcolor
except:
  import Tkinter as tk
  import ttk
  from tkColorChooser import askcolor

import datetime
import matplotlib, sys
matplotlib.use('TkAgg')
import matplotlib.cm as cm
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure

from cosmo import colormap_selector 


# =============================
class parameters():
# =============================
  '''Options for 2D plots: Valid for either line contours or shaded contours'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__ (self):
    '''Define and initialize the 2D plot options'''
    self.DATA_MIN          = tk.DoubleVar()
    self.DATA_MAX          = tk.DoubleVar()
    self.CONTOUR_MODE      = tk.IntVar()       # 0=Contour, 1=Shades
    self.CONTOUR_MIN       = tk.DoubleVar()
    self.CONTOUR_MAX       = tk.DoubleVar()
    self.CONTOUR_INTERVAL  = tk.DoubleVar()
    self.CONTOUR_WIDTH     = tk.DoubleVar()
    self.CONTOUR_DASHEDNEG = tk.BooleanVar()
    self.CONTOUR_LINEMODE  = tk.StringVar()    # 'C'=One color,'M'=colormap
    self.CONTOUR_COLOR     = tk.StringVar()
    self.CONTOUR_COLORMAP  = tk.StringVar()
    self.ALPHA             = tk.DoubleVar()    # 0=Transparent, 1=Opaque
    self.COLORBAR_SHOW     = tk.BooleanVar()
    self.COLORBAR_LOCATION = tk.StringVar()
    self.COLORBAR_PAD      = tk.DoubleVar()
    self.COLORBAR_LABEL    = tk.StringVar()
    self.COLORBAR_LABELPAD = tk.IntVar()
    self.LABEL_SHOW        = tk.BooleanVar()
    self.LABEL_SET         = tk.StringVar()    # 'A'=All, 'V'=Selected Values
    self.LABEL_SIZE        = tk.IntVar()
    self.LABEL_VALUES      = tk.StringVar()

    self.CONTOUR_MODE.set(0)             # Default: Line contours
    self.CONTOUR_WIDTH.set(1)            # Default: Line width = 1
    self.CONTOUR_DASHEDNEG.set(True)     # Default: Dashed negative values
    self.CONTOUR_LINEMODE.set('M')       # Default: Map color contours
    self.CONTOUR_COLOR.set('black')      # Default: Black contour lines
    self.CONTOUR_COLORMAP.set('jet')     # Default: Jet colormap
    self.ALPHA.set(1)                    # Default: Transparency = Opaque
    self.COLORBAR_SHOW.set(True)         # Default: Show colorbar on the right
    self.COLORBAR_LOCATION.set('right')  # Default: Show colorbar on the right
    self.COLORBAR_PAD.set(8)             # Default: Show colorbar on the right
    self.COLORBAR_LABEL.set('')          # Default: Show no colorbar title
    self.COLORBAR_LABELPAD.set(5)        # Default: Show no colorbar title
    self.LABEL_SHOW.set(True)            # Default: Show contour labels
    self.LABEL_SET.set('A')              # Default: Show All contour labels
    self.LABEL_SIZE.set(12)              # Default: Label character size
    self.LABEL_VALUES.set('')            # Default: Label character size


# ======================================================================
def config(parent,varname,units,missing,minval,maxval,PLOT):
# ======================================================================
  ''' Interactive widget to modify the options of 2D contour plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"


  def cselection():
    if PLOT.CONTOUR_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.CONTOUR_COLOR.get(),parent=parent)
    PLOT.CONTOUR_COLOR.set(hx)


  def mselection():
    if PLOT.CONTOUR_COLORMAP.get() == 'None':
      name = colormap_selector(parent=parent)
    else:
      name = colormap_selector(cmap=PLOT.CONTOUR_COLORMAP.get(),parent=parent)
    PLOT.CONTOUR_COLORMAP.set(name)


  frame1 = ttk.Frame(parent,borderwidth=5,padding=5,relief='sunken')
  ttk.Label(frame1,text='Contour field',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  ttk.Label(frame1,text='Variable Name:').grid(row=1,column=0,sticky='w')
  ttk.Label(frame1,text=varname,width=30).grid(row=1,column=1,sticky='w')
  ttk.Label(frame1,text='Units:').grid(row=2,column=0,sticky='w')
  ttk.Label(frame1,text=units,width=30).grid(row=2,column=1,sticky='w')
  ttk.Label(frame1,text='Missing value:').grid(row=3,column=0,sticky='w')
  ttk.Label(frame1,text=missing,width=30).grid(row=3,column=1,sticky='w')
  ttk.Label(frame1,text='Minimum value:').grid(row=4,column=0,sticky='w')
  ttk.Label(frame1,text=minval,width=30).grid(row=4,column=1,sticky='w')
  ttk.Label(frame1,text='Maximum value:').grid(row=5,column=0,sticky='w')
  ttk.Label(frame1,text=maxval,width=30).grid(row=5,column=1,sticky='w')
  frame1.grid()

  frame0 = ttk.Frame(parent,borderwidth=10,padding=2,relief='raised')
  ttk.Radiobutton(frame0,text='Line contour',variable=PLOT.CONTOUR_MODE,value=0).grid(row=0,column=0,columnspan=2)
  ttk.Radiobutton(frame0,text='Filled contour',variable=PLOT.CONTOUR_MODE,value=1).grid(row=0,column=2,columnspan=2)
  ttk.Radiobutton(frame0,text='Pseudo-color',variable=PLOT.CONTOUR_MODE,value=2).grid(row=0,column=4,columnspan=2)
  frame0.grid()

  frame2 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame2,text='Contour minimum:').grid(row=0,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_MIN,width=20).grid(row=0,column=1)
  ttk.Label(frame2,text='Contour maximum:').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_MAX,width=20).grid(row=1,column=1)
  ttk.Label(frame2,text='Contour interval:').grid(row=2,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_INTERVAL,width=20).grid(row=2,column=1)
  ttk.Label(frame2,text='Line width:').grid(row=3,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.CONTOUR_WIDTH,width=7).grid(row=3,column=1,sticky='w')
  ccol = ttk.Radiobutton(frame2,text='Color',variable=PLOT.CONTOUR_LINEMODE,value='C')
  cmap = ttk.Radiobutton(frame2,text='Colormap',variable=PLOT.CONTOUR_LINEMODE,value='M')
  ccol.grid(row=4,column=0,sticky='w')
  cmap.grid(row=6,column=0,sticky='w')
  ecol = ttk.Entry(frame2,textvariable=PLOT.CONTOUR_COLOR,justify='left',width=7)  # Map Contour Color
  ecol.grid(row=4,column=1,padx=3,sticky='we')
  ttk.Button(frame2,text='Select',command=cselection).grid(row=4,column=2,padx=3,sticky='ew')
  emap = ttk.Entry(frame2,textvariable=PLOT.CONTOUR_COLORMAP,justify='left',width=7)  # Map Contour Color
  emap.grid(row=6,column=1,padx=3,sticky='we')
  ttk.Button(frame2,text='Select',command=mselection).grid(row=6,column=2,padx=3,sticky='ew')
  tk.Checkbutton(frame2,text='Dashed Negative',variable=PLOT.CONTOUR_DASHEDNEG).grid(row=5,column=1,sticky='w')
  ttk.Label(frame2,text='Alpha:').grid(row=7,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.ALPHA,width=7).grid(row=7,column=1,sticky='w')
  ttk.Label(frame2,text='0: transparent; 1: opaque').grid(row=7,column=2,sticky='w')
  frame2.grid(row=7,column=0)

  frame3 = ttk.Frame(parent,borderwidth=5,padding=5)
  tk.Checkbutton(frame3,text='Show labels',variable=PLOT.LABEL_SHOW).grid(row=0,column=0,sticky='w')
  lall = ttk.Radiobutton(frame3,text='All',variable=PLOT.LABEL_SET,value='A')
  lset = ttk.Radiobutton(frame3,text='Values',variable=PLOT.LABEL_SET,value='V')
  lall.grid(row=1,column=1,sticky='w')
  lset.grid(row=2,column=1,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.LABEL_VALUES,width=30).grid(row=2,column=2,sticky='w')
  ttk.Label(frame3,text='Label size:').grid(row=3,column=1,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.LABEL_SIZE,width=7).grid(row=3,column=2,sticky='w')

  tk.Checkbutton(frame3,text='Show colorbar',variable=PLOT.COLORBAR_SHOW).grid(row=5,column=0,sticky='w')
  ttk.Label(frame3,text='Label').grid(row=6,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_LABEL,width=30).grid(row=6,column=2,sticky='w')
  ttk.Label(frame3,text='Labelpad').grid(row=7,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_LABELPAD,width=7).grid(row=7,column=2,sticky='w')
  ttk.Label(frame3,text='Location').grid(row=8,column=1,sticky='w')
  cb = ttk.Combobox(frame3,textvariable=PLOT.COLORBAR_LOCATION,width=12)
  cb.grid(row=8,column=2,sticky='w')
  cb['values']=('left','right','bottom','top')
  ttk.Label(frame3,text='Colorbar Padding').grid(row=9,column=1,sticky='w')
  tk.Entry(frame3,textvariable=PLOT.COLORBAR_PAD,width=7).grid(row=9,column=2,sticky='w')
  frame3.grid(row=14,column=0)


# =================================================
def drawing(fig,ax,map,X,Y,FIELD,PLOT):
# =================================================
  ''' Draw a 2D contour plot. Option of lines or filled contours'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  import numpy as np

  _levels = np.arange(PLOT.CONTOUR_MIN.get(),                            \
                      PLOT.CONTOUR_MAX.get()+PLOT.CONTOUR_INTERVAL.get(), \
                      PLOT.CONTOUR_INTERVAL.get())

  if PLOT.CONTOUR_MODE.get() == 0:
    # ----------------------------- CONTOURS
    if PLOT.CONTOUR_LINEMODE.get() == 'M':    # colormapped contours

      _cl = map.contour(X,Y,FIELD,                                      \
                        linewidths=PLOT.CONTOUR_WIDTH.get(),            \
                        levels=_levels,                                 \
                        cmap=cm.get_cmap(PLOT.CONTOUR_COLORMAP.get()),  \
                        latlon = True,                                  \
                        alpha=PLOT.ALPHA.get())

    else:
      if PLOT.CONTOUR_DASHEDNEG.get():
        matplotlib.rcParams['contour.negative_linestyle'] = 'dashed'
      else:
        matplotlib.rcParams['contour.negative_linestyle'] = 'solid'

      _cl = map.contour(X,Y,FIELD,                                      \
                        linewidths=PLOT.CONTOUR_WIDTH.get(),            \
                        levels=_levels,                                 \
                        colors=PLOT.CONTOUR_COLOR.get(),                \
                        latlon = True,                                  \
                        alpha=PLOT.ALPHA.get())


    # About contour labels
    if PLOT.LABEL_SHOW.get():
      if PLOT.LABEL_SET.get() == 'A':   # Label all contours
        ax.clabel(_cl,_cl.levels,inline=1,fontsize=PLOT.LABEL_SIZE.get())
      else:
        l1 = str(PLOT.LABEL_VALUES.get())
        label_list = [float(ll) for ll in l1.split(',')]
        ax.clabel(_cl,label_list,inline=1,fontsize=PLOT.LABEL_SIZE.get())

  elif PLOT.CONTOUR_MODE.get() == 1:
    # ----------------------------- SHADED
    _cf = map.contourf(X,Y,FIELD,                                      \
                     levels=_levels,                                   \
                     cmap=cm.get_cmap(PLOT.CONTOUR_COLORMAP.get()),    \
                     latlon = True,                                    \
                     alpha=PLOT.ALPHA.get())

    cbar = None
    if PLOT.COLORBAR_SHOW.get():
      padding = '%4.1f' % PLOT.COLORBAR_PAD.get() + '%'
      cbar = map.colorbar(_cf,location=PLOT.COLORBAR_LOCATION.get(), \
                          pad=padding,fig=fig)
      cbar.set_label(PLOT.COLORBAR_LABEL.get(),labelpad=PLOT.COLORBAR_LABELPAD.get())
    return cbar

  else:
    _cp = map.pcolormesh(X,Y,FIELD,                                        \
                     cmap=cm.get_cmap(PLOT.CONTOUR_COLORMAP.get()),    \
                     vmin=PLOT.CONTOUR_MIN.get(),                      \
                     vmax=PLOT.CONTOUR_MAX.get(),                      \
                     latlon = True,                                    \
                     alpha=PLOT.ALPHA.get())

    cbar = None
    if PLOT.COLORBAR_SHOW.get():
      padding = '%4.1f' % PLOT.COLORBAR_PAD.get() + '%'
      cbar = map.colorbar(_cp,location=PLOT.COLORBAR_LOCATION.get(), \
                          pad=padding,fig=fig)
      cbar.set_label(PLOT.COLORBAR_LABEL.get(),labelpad=PLOT.COLORBAR_LABELPAD.get())
    return cbar


