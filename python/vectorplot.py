# Module for the applications built for the COSMO project 
# Quim Ballabrera, May 2017

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


import datetime
import matplotlib, sys
matplotlib.use('TkAgg')
import matplotlib.cm as cm
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure


# ============================
class parameters():
# ============================
  '''Options for 2D current plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__ (self):

    self.CURRENT_NX        = tk.IntVar()
    self.CURRENT_NY        = tk.IntVar()
    self.CURRENT_SCALE     = tk.DoubleVar()
    self.CURRENT_WIDTH     = tk.DoubleVar()
    self.CURRENT_HEADLENGTH= tk.IntVar()
    self.CURRENT_HEADWIDTH = tk.IntVar()
    self.CURRENT_COLOR     = tk.StringVar()
    self.CURRENT_NX.set(41)
    self.CURRENT_NY.set(41)
    self.CURRENT_SCALE.set(20)
    self.CURRENT_WIDTH.set(0.002)
    self.CURRENT_HEADLENGTH.set(5)
    self.CURRENT_HEADWIDTH.set(3)
    self.CURRENT_COLOR.set('black')

    self.STREAM_PLOT       = tk.BooleanVar()
    self.STREAM_DENSITY    = tk.DoubleVar()
    self.STREAM_WIDTH      = tk.DoubleVar()
    self.STREAM_COLOR      = tk.StringVar()
    self.STREAM_PLOT.set(False)
    self.STREAM_DENSITY.set(1)
    self.STREAM_WIDTH.set(2)
    self.STREAM_COLOR.set('black')

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
def config(parent,PLOT,BACKUP):
# ========================================
  ''' Interactive widget to modify the options of 2D vector plots'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "December 2017"

  def cselection():
    if PLOT.CURRENT_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.CURRENT_COLOR.get(),parent=parent)
    PLOT.CURRENT_COLOR.set(hx)


  def tselection():
    if PLOT.STREAM_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.STREAM_COLOR.get(),parent=parent)
    PLOT.STREAM_COLOR.set(hx)


  def creset():
    ''' Reset to original values'''
    PLOT.CURRENT_NX.set(BACKUP.CURRENT_NX.get())
    PLOT.CURRENT_NY.set(BACKUP.CURRENT_NY.get())
    PLOT.CURRENT_SCALE.set(BACKUP.CURRENT_SCALE.get())
    PLOT.CURRENT_WIDTH.set(BACKUP.CURRENT_WIDTH.get())
    PLOT.CURRENT_HEADLENGTH.set(BACKUP.CURRENT_HEADLENGTH.get())
    PLOT.CURRENT_HEADWIDTH.set(BACKUP.CURRENT_HEADWIDTH.get())
    PLOT.CURRENT_COLOR.set(BACKUP.CURRENT_COLOR.get())


  def sreset():
    ''' Reset to original values'''
    PLOT.STREAM_DENSITY.set(BACKUP.STREAM_DENSITY.get())
    PLOT.STREAM_WIDTH.set(BACKUP.STREAM_WIDTH.get())
    PLOT.STREAM_COLOR.set(BACKUP.STREAM_COLOR.get())


  def kreset():
    ''' Reset to original values'''
    PLOT.KEY_SHOW.set(BACKUP.KEY_SHOW.get())
    PLOT.KEY_LABEL.set(BACKUP.KEY_LABEL.get())
    PLOT.KEY_VALUE.set(BACKUP.KEY_VALUE.get())
    PLOT.KEY_X.set(BACKUP.KEY_X.get())
    PLOT.KEY_Y.set(BACKUP.KEY_Y.get())
    PLOT.KEY_POS.set(BACKUP.KEY_POS.get())
    PLOT.KEY_COLOR.set(BACKUP.KEY_COLOR.get())


  frame = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame,text='Current vector options',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  ttk.Label(frame,text='Number X points').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame,textvariable=PLOT.CURRENT_NX,width=7).grid(row=1,column=1,sticky='w')
  ttk.Label(frame,text='Number Y points').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame,textvariable=PLOT.CURRENT_NY,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(frame,text='Scale').grid(row=3,column=0,sticky='w')
  ttk.Entry(frame,textvariable=PLOT.CURRENT_SCALE,width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(frame,text='Width').grid(row=4,column=0,sticky='w')
  ttk.Entry(frame,textvariable=PLOT.CURRENT_WIDTH,width=7).grid(row=4,column=1,sticky='w')
  ttk.Label(frame,text='Head length').grid(row=5,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame,textvariable=PLOT.CURRENT_HEADLENGTH,width=7).grid(row=5,column=1,sticky='w')
  ttk.Label(frame,text='Head width').grid(row=6,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame,textvariable=PLOT.CURRENT_HEADWIDTH,width=7).grid(row=6,column=1,sticky='w')
  ttk.Label(frame,text='Color').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame,textvariable=PLOT.CURRENT_COLOR,justify='left',width=7).grid(row=7,column=1,sticky='w')
  ttk.Button(frame,text='Select',command=cselection).grid(row=7,column=2,padx=3,sticky='ew')
  ttk.Button(frame,text='Reset',command=creset).grid(row=8,column=2,padx=3,pady=6,sticky='ew')
  frame.grid()

  frame2 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame2,text='Current Key options',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  tk.Checkbutton(frame2,text='Show',variable=PLOT.KEY_SHOW).grid(row=0,column=2)
  ttk.Label(frame2,text='Label').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_LABEL,width=20).grid(row=1,column=1,columnspan=2,sticky='w')
  ttk.Label(frame2,text='Value').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_VALUE,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(frame2,text='X').grid(row=3,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_X,width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(frame2,text='Y').grid(row=4,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_Y,width=7).grid(row=4,column=1,sticky='w')
  ttk.Label(frame2,text='Label side').grid(row=5,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_POS,width=7).grid(row=5,column=1,sticky='w')
  ttk.Label(frame2,text='Label color').grid(row=6,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_COLOR,width=7).grid(row=6,column=1,sticky='w')
  ttk.Button(frame2,text='Reset',command=creset).grid(row=7,column=2,padx=3,pady=6,sticky='ew')
  frame2.grid(row=9,column=0)

  frame4 = ttk.Frame(parent,borderwidth=5,padding=5)
  tk.Checkbutton(frame4,text='Plot Streamlines',font='Helvetica 12 bold',variable=PLOT.STREAM_PLOT).grid(row=0,column=2)
  frame4.grid()

  frame3 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame3,text='Stream line options',font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
  ttk.Label(frame3,text='Density').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_DENSITY,width=7).grid(row=1,column=1,sticky='w')
  ttk.Label(frame3,text='Width').grid(row=2,column=0,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_WIDTH,width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(frame3,text='Color').grid(row=3,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_COLOR,justify='left',width=7).grid(row=3,column=1,sticky='w')
  ttk.Button(frame3,text='Select',command=tselection).grid(row=3,column=2,padx=3,sticky='ew')
  ttk.Button(frame3,text='Reset',command=sreset).grid(row=4,column=2,padx=3,pady=6,sticky='ew')
  #frame3.grid(row=17,column=0)
  frame3.grid()


# ======================================
def drawing(fig,ax,m,CFIELD):
# ======================================
  ''' Draw a 2D vector plot. Option of vectors or stream function'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  import numpy as np


  if CFIELD.PLOT.STREAM_PLOT.get():
    # Streamfunction plot
    # -------------------
    ax.streamplot(CFIELD.xx, CFIELD.yy, CFIELD.u, CFIELD.v,      \
                 color=CFIELD.PLOT.STREAM_COLOR.get(),           \
                 linewidth=CFIELD.PLOT.STREAM_WIDTH.get(),       \
                 density=CFIELD.PLOT.STREAM_DENSITY.get())

  else:
    # Vector plot
    # -------------------
    uplt,vplt,xplt,yplt = m.transform_vector(CFIELD.u,CFIELD.v,            \
                                             CFIELD.lon,CFIELD.lat,        \
                                             CFIELD.PLOT.CURRENT_NX.get(), \
                                             CFIELD.PLOT.CURRENT_NY.get(), \
                                             returnxy=True,                \
                                             masked=True)

    quiver = m.quiver(xplt,yplt,uplt,vplt,                             \
                      color=CFIELD.PLOT.CURRENT_COLOR.get(),           \
                      width=CFIELD.PLOT.CURRENT_WIDTH.get(),           \
                      headwidth=CFIELD.PLOT.CURRENT_HEADWIDTH.get(),   \
                      headlength=CFIELD.PLOT.CURRENT_HEADLENGTH.get(), \
                      scale=CFIELD.PLOT.CURRENT_SCALE.get())

    if CFIELD.PLOT.KEY_SHOW.get():
      ax.quiverkey(quiver, CFIELD.PLOT.KEY_X.get(), \
                           CFIELD.PLOT.KEY_Y.get(), \
                           CFIELD.PLOT.KEY_VALUE.get(), \
                           CFIELD.PLOT.KEY_LABEL.get(), \
                           labelpos=CFIELD.PLOT.KEY_POS.get(), \
                           coordinates='figure', \
                           color=CFIELD.PLOT.KEY_COLOR.get())


