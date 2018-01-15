# Module for the applications built for the COSMO project 
# Quim Ballabrera, January 2018

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox


import datetime
import matplotlib, sys
matplotlib.use('TkAgg')
import matplotlib.cm as cm
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
from tkcolorpicker import askcolor


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
def WinOnMapConfig(parent,PLOT):
# ========================================
  ''' Interactive widget to modify the options of 2D line plots'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

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
  ttk.Label(F0,text='Map synchronize',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  tk.Checkbutton(F0,text='On Map show',font='Helvetica 12 bold',variable=PLOT.ONMAP).grid(row=1,column=1)
  ttk.Label(F0,text='Marker Style').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Combobox(F0,textvariable=PLOT.ONMAP_STYLE,values=MARKER_STYLE_LIST,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(F0,text='Marker Size').grid(row=3,column=0,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.ONMAP_SIZE,width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(F0,text='Marker Color').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(F0,textvariable=PLOT.ONMAP_COLOR,justify='left',width=7).grid(row=7,column=1,sticky='w')
  ttk.Button(F0,text='Select',command=marker_color).grid(row=7,column=2,padx=3,sticky='ew')
  F0.grid()

