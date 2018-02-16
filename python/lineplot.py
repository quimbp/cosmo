# Module for the applications built for the COSMO project 
# Quim Ballabrera, January 2018

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkcolorpicker import askcolor
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  from tkColorChooser import askcolor

from math import atan2,degrees
import numpy as np



# ============================
class parameters():
# ============================
  '''Options for 2D line plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  def __init__ (self):

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

    self.ONMAP     = tk.BooleanVar()
    self.ONMAP.set(False)
    self.ONMAP_STYLE     = tk.StringVar()
    self.ONMAP_SIZE      = tk.IntVar()
    self.ONMAP_COLOR     = tk.StringVar()
    self.ONMAP_STYLE.set('o')
    self.ONMAP_COLOR.set('red')
    self.ONMAP_SIZE.set(4)

    self.KEY_SHOW  = tk.BooleanVar()
    self.KEY_LABEL = tk.StringVar()
    self.KEY_X     = tk.DoubleVar()
    self.KEY_Y     = tk.DoubleVar()
    self.KEY_VALUE = tk.DoubleVar()
    self.KEY_POS   = tk.StringVar()
    self.KEY_COLOR = tk.StringVar()
    self.KEY_SHOW.set(True)
    self.KEY_VALUE.set(1)
    self.KEY_LABEL.set('m/s')
    self.KEY_X.set(0.75)
    self.KEY_Y.set(0.05)
    self.KEY_POS.set('E')
    self.KEY_COLOR.set('black')


# ========================================
def WinConfig(parent,PLOT):
# ========================================
  ''' Interactive widget to modify the options of 2D line plots'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "December 2017"

  def line_color():
    if PLOT.LINE_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.LINE_COLOR.get(),parent=parent)
    PLOT.LINE_COLOR.set(hx)

  def marker_color():
    if PLOT.MARKER_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.MARKER_COLOR.get(),parent=parent)
    PLOT.MARKER_COLOR.set(hx)

  def initial_color():
    if PLOT.MARKER_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.MARKER_COLOR.get(),parent=parent)
    PLOT.MARKER_COLOR.set(hx)

  # Main widget
  # ===========
  LINE_STYLE_LIST = ['-','--','-.',':']
  MARKER_STYLE_LIST = ['.',',','o','v','^','<','>','1','2','3','4','s','p','*','h','H','+','x','D','d','|','_']

  F0 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F0,text='Line options',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  tk.Checkbutton(F0,text='Line show',font='Helvetica 12 bold',variable=PLOT.LINE_SHOW).grid(row=1,column=1)
  ttk.Label(F0,text='Line Style').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.LINE_STYLE,values=LINE_STYLE_LIST,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(F0,text='Line Width').grid(row=3,column=0,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.LINE_WIDTH,width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(F0,text='Line Color').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.LINE_COLOR,justify='left',width=7).grid(row=7,column=1,sticky='w')
  ttk.Button(F0,text='Select',command=line_color).grid(row=7,column=2,padx=3,sticky='ew')
  F0.grid()

  F1 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F1,text='Marker options',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  tk.Checkbutton(F1,text='Marker show',font='Helvetica 12 bold',variable=PLOT.MARKER_SHOW).grid(row=1,column=1)
  ttk.Label(F1,text='Marker Style').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Combobox(F1,textvariable=PLOT.MARKER_STYLE,values=MARKER_STYLE_LIST,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(F1,text='Marker Size').grid(row=3,column=0,sticky='w')
  ttk.Entry(F1,textvariable=PLOT.MARKER_SIZE,width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(F1,text='Marker Color').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(F1,textvariable=PLOT.MARKER_COLOR,justify='left',width=7).grid(row=7,column=1,sticky='w')
  ttk.Button(F1,text='Select',command=marker_color).grid(row=7,column=2,padx=3,sticky='ew')
  F1.grid()

  F2 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F2,text='Initial Marker options',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  tk.Checkbutton(F2,text='Initial show',font='Helvetica 12 bold',variable=PLOT.INITIAL_SHOW).grid(row=1,column=1)
  ttk.Label(F2,text='Initial Style').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Combobox(F2,textvariable=PLOT.INITIAL_STYLE,values=MARKER_STYLE_LIST,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(F2,text='Initial Size').grid(row=3,column=0,sticky='w')
  ttk.Entry(F2,textvariable=PLOT.INITIAL_SIZE,width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(F2,text='Initial Color').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(F2,textvariable=PLOT.INITIAL_COLOR,justify='left',width=7).grid(row=7,column=1,sticky='w')
  ttk.Button(F2,text='Select',command=initial_color).grid(row=7,column=2,padx=3,sticky='ew')
  F2.grid()

# ========================================
def WinOnMapConfig(parent,PLOT,LL):
# ========================================
  ''' Interactive widget to modify the options of 2D line plots'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  def iselection():
    '''Select floater'''
    cbox['text'] = LL.FLOAT_COLOR[LL.I.get()]

  def cselection():
    '''Select floater'''
    print(cbox.get())
    LL.FLOAT_COLOR[LL.I.get()].set(cbox.get())

  def default_color():
    for i in range(LL.nfloats):
      LL.FLOAT_COLOR[i].set(LL.PLOT.LINE_COLOR.get())

  def line_color():
    if LL.FLOAT_COLOR[LL.I.get()].get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      backup = LL.FLOAT_COLOR[LL.I.get()].get()
      rgb, hx = askcolor(color=LL.FLOAT_COLOR[LL.I.get()].get(),parent=parent)
      if hx is None:
        LL.FLOAT_COLOR[LL.I.get()].set(backup)
      else:
        LL.FLOAT_COLOR[LL.I.get()].set(hx)

  def marker_color():
    if PLOT.ONMAP_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.ONMAP_COLOR.get(),parent=parent)
    PLOT.ONMAP_COLOR.set(hx)

  # Main widget
  # ===========
  MARKER_STYLE_LIST = ['.',',','o','v','^','<','>','1','2','3','4','s','p','*','h','H','+','x','D','d','|','_']

  F0 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F0,text='Trajectory interpolation',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  tk.Checkbutton(F0,text='Show interpolated',font='Helvetica 12 bold',variable=PLOT.ONMAP).grid(row=1,column=1)
  ttk.Label(F0,text='Marker Style').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.ONMAP_STYLE,values=MARKER_STYLE_LIST,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(F0,text='Marker Size').grid(row=3,column=0,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.ONMAP_SIZE,width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(F0,text='Marker Color').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.ONMAP_COLOR,justify='left',width=7).grid(row=7,column=1,sticky='w')
  ttk.Button(F0,text='Select',command=marker_color).grid(row=7,column=2,padx=3,sticky='ew')
  F0.grid()

  F1 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F1,text='Trajectory limits',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  ttk.Label(F1,text='Initial record').grid(row=1,column=0,sticky='w')
  ttk.Entry(F1,textvariable=LL.L1,width=7).grid(row=1,column=1,sticky='w')
  ttk.Label(F1,text='Last record').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Entry(F1,textvariable=LL.L2,justify='left',width=7).grid(row=2,column=1,sticky='w')
  F1.grid()

  F2 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(F2,text='Individual Float Color',font='Helvetica 12 bold').grid(row=0,column=0,columnspan=2,sticky='w')
  tk.Checkbutton(F2,text='Individual colors',font='Helvetica 12 bold',variable=LL.SEPARATED_COLOR).grid(row=1,column=1,columnspan=2)
  ttk.Label(F2,text='Floater:').grid(row=2,column=0,padx=3)
  ibox = ttk.Combobox(F2,textvariable=LL.I,width=5)
  ibox.grid(row=2,column=1)
  ibox.bind('<<ComboboxSelected>>',lambda e: iselection())
  ibox['values'] = list(range(LL.nfloats))
  ttk.Label(F2,text='Line Color').grid(row=2,column=2,columnspan=1,sticky='w')
  cbox = ttk.Entry(F2,textvariable=LL.FLOAT_COLOR[LL.I.get()],justify='left',width=10)
  cbox.grid(row=2,column=3,sticky='w')
  cbox.bind('<Return>',lambda e: cselection())
  # AAA
  ttk.Button(F2,text='Select',command=line_color).grid(row=2,column=4,padx=3,sticky='ew')
  ttk.Button(F2,text='Default',command=default_color).grid(row=2,column=5,padx=3,sticky='ew')
  F2.grid()

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


