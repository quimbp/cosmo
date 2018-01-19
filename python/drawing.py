''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems'''

import sys
import numpy as np
import numpy.ma as ma
from scipy import interpolate

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkinter import filedialog as filedialog
  from tkcolorpicker import askcolor
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  import tkFileDialog as filedialog
  from tkColorChooser import askcolor

from netCDF4 import Dataset,num2date
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.image as image
from mpl_toolkits.basemap import Basemap
import matplotlib.font_manager as font_manager
#from matplotlib.backend_bases import key_press_handler
from matplotlib.figure import Figure
from matplotlib.offsetbox import TextArea, OffsetImage, AnnotationBbox
import matplotlib.cm as cm
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg

import json
from subprocess import call
import os

from cosmo import *
import vectorplot
import contourplot
import lineplot
import codar
import saidin
import lagrangian
import clm

class command_parameters():
  def __init__ (self):
    self.PATH          = tk.StringVar()
    self.BIN           = tk.StringVar()
    self.UNAME         = tk.StringVar()
    self.VNAME         = tk.StringVar()
    self.Ulon          = tk.StringVar()
    self.Ulat          = tk.StringVar()
    self.Udepth        = tk.StringVar()
    self.Utime         = tk.StringVar()
    self.Vlon          = tk.StringVar()
    self.Vlat          = tk.StringVar()
    self.Vdepth        = tk.StringVar()
    self.Vtime         = tk.StringVar()

    self.PATH.set('/home/joaquim/cosmo/bin/')
    self.BIN.set('lagrangian')

class current_parameters():
  def __init__ (self):
    self.FILENAME      = tk.StringVar()
    self.PLOT          = vectorplot.parameters()
    self.BACKUP        = vectorplot.parameters()
    self.lon           = None
    self.lat           = None
    self.xx            = None
    self.yy            = None
    self.u             = None
    self.v             = None
    self.speed         = None
    self.F             = None

class field_parameters():
  def __init__ (self):
    self.FILENAME      = tk.StringVar()
    self.PLOT          = contourplot.parameters()
    self.BACKUP        = contourplot.parameters()

    self.missing       = tk.DoubleVar()
    self.mask          = tk.BooleanVar()
    self.mask.set(True)

    self.lon           = None
    self.lat           = None
    self.xx            = None
    self.yy            = None
    self.F             = None
    #self.sst           = None
    self.minval        = None
    self.maxval        = None
    self.varname       = None
    self.units         = None
    self.missing_value = None
    self.cbar          = None
    self.FILENAME.set('')
    self.PLOT.CONTOUR_MODE.set(0)

# ==================
class WinDrawPlot():
# ==================

  def _close(self):
  # ================
    aa = messagebox.askquestion('Close','Are you sure?',icon='warning')
    if aa == 'yes':
      sys.exit(0)


  def __init__ (self,master,FLD,DPI):

    self.master = master
    self.FLD    = FLD
    self.frame  = tk.Frame(self.master)
    self.frame.grid()
    self.frame.grid_rowconfigure(0,weight=1)
    self.frame.grid_columnconfigure(1,weight=1)


    self.CURRENTS      = current_parameters()
    self.FIELD         = field_parameters()

    self.init_current  = True
    self.init_field    = True

    self.PLOT          = plot_params()
    self.BACKUP        = plot_params()    # Default initial parameters
    self.PLOT.DPI.set(DPI)

    self.superpose     = False
    self.SVARNAME      = tk.StringVar()
    self.SVARNAME.set('')

    self.ncodar        = 0
    self.CODAR         = []
    self.CODAR_LIST    = ['0']
    self.CODAR_INDX    = tk.IntVar()
    self.CODAR_INDX.set(0)

    self.nfloat        = 0
    self.FLOAT         = []
    self.FLOAT_LIST    = ['0']
    self.FLOAT_INDX    = tk.IntVar()
    self.FLOAT_INDX.set(0)

    self.CODAR_BACKUP  = codar.CODAR_CLASS() 

    self.VIDEO_NAME    = tk.StringVar()
    self.VIDEO_TITLE   = tk.StringVar()
    self.VIDEO_AUTHOR  = tk.StringVar()
    self.VIDEO_COMMENT = tk.StringVar()
    self.VIDEO_FPS     = tk.IntVar()
    self.VIDEO_DPI     = tk.IntVar()
    self.VIDEO_L1      = tk.IntVar()
    self.VIDEO_L2      = tk.IntVar()
    self.VIDEO_NAME.set('movie.mp4')
    self.VIDEO_TITLE.set('COSMO-VIEW Movie')
    self.VIDEO_AUTHOR.set('Matplotlib')
    self.VIDEO_COMMENT.set('Ocean currents movie')
    self.VIDEO_FPS.set(2)
    self.VIDEO_DPI.set(100)

    # Input velocity units
    try:
      theunits = FLD.ncid.variables[FLD.icdf.vname[FLD.uid]].getncattr('units')
    except:
      theunits = 'm/s'
    self.CURRENTS.PLOT.KEY_LABEL.set(theunits)
    self.CURRENTS.BACKUP.KEY_LABEL.set(theunits)
   

    self.SAIDIN = field_parameters() 
    self.SAIDIN.FILENAME.set('')
    self.SAIDIN.PLOT.CONTOUR_MODE.set(1)


    # Define Menu:
    self.CreateMenu()

    # Initialize CLM command:
    self.CLM = clm.parameters()
    self.CLM.UFILE.set(FLD.FILENAME)
    self.CLM.Ux.set(FLD.icdf.xname)
    self.CLM.Uy.set(FLD.icdf.yname)
    self.CLM.Uz.set(FLD.icdf.zname)
    self.CLM.Ut.set(FLD.icdf.tname)
    self.CLM.Uu.set(FLD.icdf.vname[FLD.uid])
    self.CLM.Vv.set(FLD.icdf.vname[FLD.vid])

    # Initialize app
    self.plot_initialize(FLD.ncid,FLD.icdf)

    ttk.Label(self.frame,text='Input file'). \
        grid(row=0,column=0,padx=5,sticky='w')
    wfile=ttk.Entry(self.frame,width=30)
    wfile.grid(row=0,column=1,columnspan=9,sticky='ew')
    wfile.grid_rowconfigure(0,weight=0)
    wfile.grid_columnconfigure(0,weight=1)
    wfile.insert(0,FLD.icdf.filename)
    wfile.configure(state='disabled')

    # Read the data 
    self.read_UV(FLD.ncid,FLD.icdf,FLD.uid,FLD.vid)

    self.with_logo        = None
    self.VIDEO_L1.set(0)
    self.VIDEO_L2.set(self.FLD.icdf.nt-1)

    # Define main app window:

    F0 = ttk.Frame(self.frame,padding=5)

    FD = ttk.Frame(F0)
    ttk.Label(FD,text='Depth',width=5).grid(row=0,column=0,sticky='w',padx=[1,3])
    self.kbox = ttk.Combobox(FD,textvariable=self.FLD.K,width=5)
    self.kbox.grid(row=0,column=1,sticky='ew',padx=3)
    self.kbox.bind('<<ComboboxSelected>>',lambda e: self.kselection(FLD.icdf))
    if FLD.icdf.idz < 0:
      self.kbox.configure(state='disabled')
    else:
      self.kbox['values'] = list(range(len(self.FLD.K_LIST)))
      self.PLOT.ZLABEL.set(str(self.FLD.Z_LIST[self.FLD.K.get()]))

    ttk.Entry(FD,textvariable=self.PLOT.ZLABEL, \
              state='readonly',width=10).grid(row=0,column=2,sticky='ew')
    FD.grid(row=0,column=0,columnspan=3,sticky='w')

    FT = ttk.Frame(F0)
    ttk.Label(FT,text='Time').grid(row=0,column=0,padx=3)
    self.lbox = ttk.Combobox(FT,textvariable=self.FLD.L,width=5)
    self.lbox.grid(row=0,column=1,sticky='ew')
    self.lbox.bind('<<ComboboxSelected>>',lambda e: self.lselection(FLD.icdf))

    ttk.Button(FT,text='PREV',command=self.tprev,width=5).grid(row=0,column=2,sticky='e')
    if FLD.icdf.idt < 0:
      self.lbox.configure(state='disabled')
    else:
      self.lbox['values'] = list(range(len(self.FLD.L_LIST)))
      string = '{}'.format(num2date(self.FLD.T_LIST[self.FLD.L.get()], \
                                                  units=FLD.icdf.time_units, \
                                                  calendar=FLD.icdf.time_calendar))
      self.PLOT.TLABEL.set(string)

    ttk.Entry(FT,textvariable=self.PLOT.TLABEL, \
              state='readonly',width=20).grid(row=0,column=3,\
              columnspan=3,sticky='w',padx=3)
    ttk.Button(FT,text='NEXT',command=self.tnext,width=5).grid(row=0,column=6,padx=[5,1],stick='w')
    FT.grid(row=0,column=3,columnspan=7,sticky='w',padx=10)

    ttk.Button(F0,text='Redraw',command=self.data_update).grid(row=0,column=10,stick='e')

    F0.grid(row=1,column=0,columnspan=11,sticky='ew')
    F0.grid_rowconfigure(0,weight=0)
    for i in range(11):
      F0.grid_columnconfigure(i,weight=1)
 
    ttk.Label(self.frame,text='Superpose').grid(row=2,column=0,padx=5)
    self.sbox = ttk.Combobox(self.frame,textvariable=self.SVARNAME,width=12)
    self.sbox.grid(row=2,column=1,columnspan=2,sticky='w')
    self.sbox['values'] = FLD.icdf.VAR_MENU
    self.sbox.bind('<<ComboboxSelected>>',lambda e: self.sselection(FLD.ncid,FLD.icdf))

    #  The CANVAS:
   
    self.fig = Figure(figsize=(9,6),dpi=self.PLOT.DPI.get())
    #self.fig = Figure(dpi=self.PLOT.DPI.get())
    self.ax1 = self.fig.add_subplot(111)
    self.canvas = FigureCanvasTkAgg(self.fig,master=self.frame)
    self.canvas.show()
    self.canvas.get_tk_widget().grid(row=3,column=0,columnspan=11,sticky='wn')

    self.canvas._tkcanvas.grid()
    self.canvas.callbacks.connect('button_press_event',self.on_click)
    ttk.Label(self.frame,text='COSMO project, December 2017').grid(column=9,padx=3,sticky='e')

    self.make_plot()

    self.Window_mapconfig     = None
    self.Window_gridconfig    = None
    self.Window_labelconfig   = None
    self.Window_vectorconfig  = None
    self.Window_contourconfig = None
    self.Window_lineconfig    = None
    self.Window_other         = None
    self.Window_saidin        = None
    self.Window_codar         = None
    self.Window_isobat        = None
    self.Window_float         = None
    self.Window_saidin_config = None
    self.Window_codar_config  = None
    self.Window_float_config  = None
    self.Window_clm           = None
    self.Window_dpi           = None
    self.Window_anim          = None


  def saveconf(self):
    print('Saving configuration ...')


  # ==================================
  def on_click(self,event):
  # ==================================

    if event.inaxes is not None:
      print(event.xdata, event.ydata)
      print('Current speed = ', self.CURRENTS.F(event.xdata,event.ydata))
      if not empty(self.SAIDIN.FILENAME.get()):
        print('SAIDIN SST = ', self.SAIDIN.F(event.xdata,event.ydata))
      self.CLM.xo.set(event.xdata)
      self.CLM.yo.set(event.ydata)
    

  # ==================================
  def plot_initialize(self,ncid,icdf):
  # ==================================

    self.CURRENTS.lon = ncid.variables[icdf.xname][:]
    self.CURRENTS.lat = ncid.variables[icdf.yname][:]
    self.CURRENTS.xx,self.CURRENTS.yy =  \
                    np.meshgrid(self.CURRENTS.lon,self.CURRENTS.lat)

    self.FIELD.lon = ncid.variables[icdf.xname][:]
    self.FIELD.lat = ncid.variables[icdf.yname][:]
    self.FIELD.xx,self.FIELD.yy =  \
                    np.meshgrid(self.FIELD.lon,self.FIELD.lat)

    self.PLOT.X           = self.CURRENTS.lon[:]
    self.PLOT.Y           = self.CURRENTS.lat[:]
    self.PLOT.DATA_WEST   = np.min(self.PLOT.X)
    self.PLOT.DATA_EAST   = np.max(self.PLOT.X)
    self.PLOT.DATA_SOUTH  = np.min(self.PLOT.Y)
    self.PLOT.DATA_NORTH  = np.max(self.PLOT.Y)

    self.PLOT.WEST.set(self.PLOT.DATA_WEST)
    self.PLOT.EAST.set(self.PLOT.DATA_EAST)
    self.PLOT.SOUTH.set(self.PLOT.DATA_SOUTH)
    self.PLOT.NORTH.set(self.PLOT.DATA_NORTH)

    # Meridian and parallel range and intervalls:
    tmp1 = np.trunc(100*(self.PLOT.DATA_EAST-self.PLOT.DATA_WEST)/4)/100
    if tmp1 > 1:
      tmp1 = np.rint(tmp1)
    self.PLOT.MERIDIAN_INT.set(tmp1)

    self.PLOT.MERIDIAN_INI.set(np.trunc(np.min(self.PLOT.X)/tmp1 - 1)*tmp1)
    self.PLOT.MERIDIAN_FIN.set(np.trunc(np.max(self.PLOT.X)/tmp1 + 1)*tmp1)
    tmp1 = None

    tmp2 = np.trunc(100*(self.PLOT.DATA_NORTH - self.PLOT.DATA_SOUTH)/4)/100
    if tmp2 > 1:
      tmp2 = np.rint(tmp2)
    self.PLOT.PARALLEL_INT.set(tmp2)
    self.PLOT.PARALLEL_INI.set(np.trunc(self.PLOT.DATA_SOUTH/tmp2 - 1)*tmp2)
    self.PLOT.PARALLEL_FIN.set(np.trunc(self.PLOT.DATA_NORTH/tmp2 + 1)*tmp2)
    tmp2 = None

    try:
      self.PLOT.XLABEL.set(ncid.variables[icdf.xname].getncattr('long_name'))
    except:
      self.PLOT.XLABEL.set(icdf.xname)

    try:
      self.PLOT.YLABEL.set(ncid.variables[icdf.yname].getncattr('long_name'))
    except:
      self.PLOT.YLABEL.set(icdf.yname)
    

  # ====================
  def CreateMenu (self):
  # ====================
    ''' Create options menu'''

    self.menubar = tk.Menu(self.master)
    menu = tk.Menu(self.menubar,tearoff=0)
    self.menubar.add_cascade(label='File',menu=menu)
    menu.add_command(label='Save',command=self.save)
    menu.add_command(label='Save as',command=self.saveas)
    menu.add_separator()
    menu.add_command(label='Quit',command=self._close)

    menu = tk.Menu(self.menubar, tearoff=0)
    self.menubar.add_cascade(label='Import/Select',menu=menu)
    menu.add_command(label='SAIDIN',command=self.saidin)
    menu.add_command(label='CODAR',command=self.codar)
    #menu.add_command(label='Field',command=self.logo_config)
    #menu.add_command(label='Vector',command=self.logo_config)
    menu.add_command(label='Lagrangian',command=self.floats)

    menu = tk.Menu(self.menubar, tearoff=0)
    self.menubar.add_cascade(label='Configure',menu=menu)
    menu.add_command(label='Map',command=self.map_config)
    menu.add_command(label='Grid',command=self.grid_config)
    menu.add_command(label='Labels',command=self.label_config)
    menu.add_command(label='Vectors', \
                     command=lambda:self.vector_config(self.CURRENTS.PLOT,self.CURRENTS.BACKUP))
    menu.add_command(label='Contours', \
                     command=lambda:self.contour_config(self.FIELD))
    menu.add_separator()
    menu.add_command(label='SAIDIN',command=lambda:self.contour_config(self.SAIDIN))
    menu.add_command(label='CODAR', \
             command=lambda:self.vector_config(self.CODAR[self.CODAR_INDX.get()].PLOT,self.CODAR_BACKUP.PLOT))
    #menu.add_command(label='Field',command=self.logo_config)
    #menu.add_command(label='Vector',command=self.logo_config)
    menu.add_command(label='Trajectories', \
             command=self.lagrangian_config)

    menu = tk.Menu(self.menubar, tearoff=0)
    self.menubar.add_cascade(label='Tools',menu=menu)
    menu.add_command(label='Logo',command=self.logo_config)
    menu.add_command(label='Output file DPI',command=self.wdpi)
    menu.add_command(label='Make animation',command=self.make_anim)
    menu.add_command(label='COSMO Lagrangian Model',command=self.clm)

    try:
      self.master.config(menu=self.menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.master.tk.call(master, "config", "-menu", self.menubar)


  # =============
  def save(self):
  # =============
     # First check that the variable has been filled, if not open dialog box:
     if self.PLOT.OUT_FILENAME is None:
       self.saveas()

     # If output filename exist, we save:
     if self.PLOT.OUT_FILENAME is not None:
        self.fig.savefig(self.PLOT.OUT_FILENAME,dpi=self.PLOT.OUT_DPI.get(),bbox_inches='tight')
        print('Saving in ',self.PLOT.OUT_FILENAME)


  # ===============
  def saveas(self):
  # ===============
      filetypes = [('PNG file','.png'),('JPG file','.jpg'),('PDF file','.pdf')]
      nn = tk.filedialog.asksaveasfilename(title='Save',filetypes=filetypes,confirmoverwrite=True)
      if nn is not None:
        self.PLOT.OUT_FILENAME = '%s' % nn
        self.fig.savefig(self.PLOT.OUT_FILENAME,dpi=self.PLOT.OUT_DPI.get(),bbox_inches='tight')
        print('Saving in ',self.PLOT.OUT_FILENAME)
      else:
        self.PLOT.OUT_FILENAME = None


  # ======================
  def isobat_config(self):
  # ======================
    '''Open an entry window to modify the Isobath's path'''

    def _close():
      self.Window_isobat.destroy()
      self.Window_isobat = None
    
    if self.Window_isobat is None:
      self.Window_isobat = tk.Toplevel(self.master)
      self.Window_isobat.title("Isobath's path")
      self.Window_isobat.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_isobat.lift()

    ttk.Entry(self.Window_isobat,textvariable=self.PLOT.ISOBAT_PATH, \
              justify='left',width=70).grid(row=0,column=0,columnspan=3,padx=3,pady=5)
    ttk.Button(self.Window_isobat,text='Done',command=_close).grid(row=1,column=2,sticky='e', \
                                                              padx=3,pady=5)
     

  # ====================
  def logo_config(self):
  # ====================

    def new_logo():
    # =============
      nn = tk.filedialog.askopenfile()
      if not empty(nn.name):
        self.PLOT.LOGO_FILE.set(nn.name)
        self.PLOT.LOGO_IMAGE = image.imread(self.PLOT.LOGO_FILE.get())


    self.toplevel = tk.Toplevel(self.master)
    self.toplevel.title('Logo configuration')
    ttk.Label(self.toplevel,text='File', \
              font='Helvetica 12 bold').grid(row=0,column=0,sticky='w')
    le = ttk.Entry(self.toplevel,textvariable=self.PLOT.LOGO_FILE, \
                   justify='left',width=30)
    le.grid(row=0,column=1,columnspan=5,sticky='w')
    le.bind('<<ComboboxSelected>>',lambda e: new_logo())
    ttk.Button(self.toplevel,text='Open', \
               command=new_logo).grid(row=0,column=6,sticky='w')
    ttk.Label(self.toplevel,text='Zoom', \
               font='Helvetica 12 bold').grid(row=1,column=0,sticky='w')
    ttk.Entry(self.toplevel,textvariable=self.PLOT.LOGO_ZOOM, \
               justify='left',width=8).grid(row=1,column=1,sticky='w')
    ttk.Label(self.toplevel,text='Location', \
               font='Helvetica 12 bold').grid(row=2,column=0,sticky='w')
    ttk.Radiobutton(self.toplevel,text='SW',variable=self.PLOT.LOGO_LOCATION,\
                    value='SW').grid(row=3,column=1,sticky='w')
    ttk.Radiobutton(self.toplevel,text='NW',variable=self.PLOT.LOGO_LOCATION,\
                    value='NW').grid(row=4,column=1,sticky='w')
    ttk.Radiobutton(self.toplevel,text='NE',variable=self.PLOT.LOGO_LOCATION,\
                    value='NE').grid(row=5,column=1,sticky='w')
    ttk.Radiobutton(self.toplevel,text='SE',variable=self.PLOT.LOGO_LOCATION,\
                    value='SE').grid(row=6,column=1,sticky='w')
    ttk.Radiobutton(self.toplevel,text='Other',variable=self.PLOT.LOGO_LOCATION,\
                    value='OTHER').grid(row=7,column=1,sticky='w')
    lx = ttk.Entry(self.toplevel,textvariable=self.PLOT.LOGO_X,\
                   justify='left',width=7)
    lx.grid(row=7,column=2,sticky='w')
    ly = ttk.Entry(self.toplevel,textvariable=self.PLOT.LOGO_Y,\
                   justify='left',width=7)
    ly.grid(row=7,column=3,sticky='w')

    ttk.Button(self.toplevel,text='Done',command=self.toplevel.destroy,\
               padding=5).grid(row=8,column=6)


  # ====================
  def grid_config(self):
  # ====================

    def _close():
    # ===========
      self.Window_gridconfig.destroy()
      self.Window_gridconfig = None

    def _apply():
    # ===========
      self.make_plot()

    def _done():
    # ===========
      self.make_plot()
      self.Window_gridconfig.destroy()
      self.Window_gridconfig = None


    if self.Window_gridconfig is not None:
      self.Window_gridconfig.lift()
      return
    else:
      self.Window_gridconfig = tk.Toplevel(self.master)
      self.Window_gridconfig.title('Grid configuration')
      self.Window_gridconfig.resizable(width=True,height=True)
      self.Window_gridconfig.protocol('WM_DELETE_WINDOW',_close)

    f0 = ttk.Frame(self.Window_gridconfig,padding=5)
    chkgrid = ttk.Checkbutton(f0,text='Show grid',\
                              variable=self.PLOT.SHOW_GRID)
    chkgrid.grid(row=0,column=0)

    ttk.Label(f0,text='Meridians',font='Helvetica 12 bold').grid(row=1,column=0,sticky='w')
    ttk.Label(f0,text='Initial').grid(row=2,column=1,sticky='w')
    wxo = ttk.Entry(f0,textvariable=self.PLOT.MERIDIAN_INI,justify='left',width=8)
    wxo.grid(row=2,column=2)
    ttk.Label(f0,text='Iterval').grid(row=3,column=1,sticky='w')
    wdx = ttk.Entry(f0,textvariable=self.PLOT.MERIDIAN_INT,justify='left',width=8)
    wdx.grid(row=3,column=2)
    ttk.Label(f0,text='Final').grid(row=4,column=1,sticky='w')
    wdx = ttk.Entry(f0,textvariable=self.PLOT.MERIDIAN_FIN,justify='left',width=8)
    wdx.grid(row=4,column=2)

    ttk.Label(f0,text='Parallels',font='Helvetica 12 bold').grid(row=5,column=0,sticky='w')
    ttk.Label(f0,text='Initial').grid(row=6,column=1,sticky='w')
    wxo = ttk.Entry(f0,textvariable=self.PLOT.PARALLEL_INI,justify='left',width=8)
    wxo.grid(row=6,column=2)
    ttk.Label(f0,text='Iterval').grid(row=7,column=1,sticky='w')
    wdx = ttk.Entry(f0,textvariable=self.PLOT.PARALLEL_INT,justify='left',width=8)
    wdx.grid(row=7,column=2)
    ttk.Label(f0,text='Final').grid(row=8,column=1,sticky='w')
    wdx = ttk.Entry(f0,textvariable=self.PLOT.PARALLEL_FIN,justify='left',width=8)
    wdx.grid(row=8,column=2)
    ttk.Label(f0,text='Configuration',font='Helvetica 12 bold') \
       .grid(row=10,column=0,sticky='w')
    ttk.Label(f0,text='Character Size').grid(row=11,column=1,sticky='w')
    ttk.Entry(f0,textvariable=self.PLOT.LONLAT_SIZE,justify='left',width=8) \
       .grid(row=11,column=2)
    ttk.Label(f0,text='Character Color').grid(row=12,column=1,sticky='w')
    ttk.Entry(f0,textvariable=self.PLOT.LONLAT_COLOR,justify='left',width=8) \
       .grid(row=12,column=2)

    ttk.Button(f0,text='Apply',command=_apply,padding=5).grid(row=13,column=0,pady=[5,3])
    ttk.Button(f0,text='Cancel',command=_close,padding=5).grid(row=13,column=1,pady=3)
    done = ttk.Button(f0,text='Done',command=_done,padding=5)
    done.grid(row=13,column=2,pady=3)
    done.bind("<Return>",lambda e:_done())
    f0.grid()


  # ===============================
  def vector_config(self,CUR,BACK):
  # ===============================

    def _cancel():
    # ============
      CUR.CURRENT_NX.set(BACK.CURRENT_NX.get())
      CUR.CURRENT_NY.set(BACK.CURRENT_NY.get())
      CUR.CURRENT_SCALE.set(BACK.CURRENT_SCALE.get())
      CUR.CURRENT_WIDTH.set(BACK.CURRENT_WIDTH.get())
      CUR.CURRENT_HEADLENGTH.set(BACK.CURRENT_HEADLENGTH.get())
      CUR.CURRENT_HEADWIDTH.set(BACK.CURRENT_HEADWIDTH.get())
      CUR.CURRENT_COLOR.set(BACK.CURRENT_COLOR.get())
      CUR.KEY_SHOW.set(BACK.KEY_SHOW.get())
      CUR.KEY_LABEL.set(BACK.KEY_LABEL.get())
      CUR.KEY_VALUE.set(BACK.KEY_VALUE.get())
      CUR.KEY_X.set(BACK.KEY_X.get())
      CUR.KEY_Y.set(BACK.KEY_Y.get())
      CUR.KEY_POS.set(BACK.KEY_POS.get())
      CUR.KEY_COLOR.set(BACK.KEY_COLOR.get())
      CUR.STREAM_DENSITY.set(BACK.STREAM_DENSITY.get())
      CUR.STREAM_WIDTH.set(BACK.STREAM_WIDTH.get())
      CUR.STREAM_COLOR.set(BACK.STREAM_COLOR.get())
      self.Window_vectorconfig.destroy()
      self.Window_vectorconfig = None

    def _apply():
    # ============
      self.make_plot()

    def _done():
    # ============
      self.make_plot()
      self.Window_vectorconfig.destroy()
      self.Window_vectorconfig = None

    if self.Window_vectorconfig is not None:
      self.Window_vectorconfig.lift()
      return
    else:
      self.Window_vectorconfig = tk.Toplevel(self.master)
      self.Window_vectorconfig.title('Vector plot configuration')
      self.Window_vectorconfig.resizable(width=True,height=True)
      self.Window_vectorconfig.protocol('WM_DELETE_WINDOW',_cancel)
    
    vectorplot.config(parent=self.Window_vectorconfig,PLOT=CUR,BACKUP=BACK)

    f0 = ttk.Frame(self.Window_vectorconfig,padding=5)
    ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
        grid(row=0,column=0,padx=3)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Done',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)


  # ==========================
  def contour_config(self,FF):
  # ==========================
    def _cancel():
      self.Window_contourconfig.destroy()
      self.Window_contourconfig = None

    def _apply():
      self.make_plot()

    def _done():
      self.make_plot()
      self.Window_contourconfig.destroy()
      self.Window_contourconfig = None

    if self.Window_contourconfig is not None:
      self.Window_contourconfig.lift()
      return
    else:
      self.Window_contourconfig = tk.Toplevel(self.master)
      self.Window_contourconfig.title('Contour plot configuration')
      self.Window_contourconfig.resizable(width=True,height=True)
      self.Window_contourconfig.protocol('WM_DELETE_WINDOW',_cancel)
    
    contourplot.config(parent=self.Window_contourconfig,  \
                       varname=FF.varname,                \
                       units=FF.units,                    \
                       missing=FF.missing_value,          \
                       minval=FF.minval,                  \
                       maxval=FF.maxval,                  \
                       PLOT=FF.PLOT)

    f0 = ttk.Frame(self.Window_contourconfig,padding=5)
    ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
        grid(row=0,column=0,padx=3)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Done',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)


  # ==========================
  def lagrangian_config(self):
  # ==========================

    if self.nfloat == 0:
      messagebox.showinfo(message='No Lagrangian file opened yet')
      return

    self.line_config(self.FLOAT[self.FLOAT_INDX.get()])

  # =======================
  def line_config(self,LL):
  # =======================

    def _cancel():
    # ============
      self.Window_lineconfig.destroy()
      self.Window_lineconfig = None

    def _apply():
    # ============
      self.make_plot()

    def _done():
    # ============
      self.make_plot()
      self.Window_lineconfig.destroy()
      self.Window_lineconfig = None


    if self.Window_lineconfig is not None:
      self.Window_lineconfig.lift()
      return
    else:
      self.Window_lineconfig = tk.Toplevel(self.master)
      self.Window_lineconfig.title('Contour plot configuration')
      self.Window_lineconfig.resizable(width=True,height=True)
      self.Window_lineconfig.protocol('WM_DELETE_WINDOW',_cancel)
    
    # Define tabs:
    nb = ttk.Notebook(self.Window_lineconfig)
    page1 = ttk.Frame(nb)
    page2 = ttk.Frame(nb)
    page3 = ttk.Frame(nb)
    nb.add(page1,text='Line Configuration')
    nb.add(page2,text='Trajectory options')
    nb.add(page3,text='Trajectory data')
    nb.grid()

    # Page 1
    #lineplot.WinConfig(self.Window_lineconfig,LL)
    lineplot.WinConfig(page1,LL.PLOT)

    # Page 2
    lineplot.WinOnMapConfig(page2,LL.PLOT,LL)

    # Page 3
    lagrangian.ShowData(page3,LL)

    #log = tk.Text(page2)
    #log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')
    ## Scrollbar
    #scrollb = tk.Scrollbar(page2,command=log.yview)
    #scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
    #log['yscrollcommand'] = scrollb.set
#
#    for i in range(LL.nrecords):
#      string = '\t {} \t {: 7.3f} \t {: 7.3f} \t {} \n'.format(i,LL.lon[i], \
#                                                     LL.lat[i], \
#                                                     LL.date[i])
#      log.insert('end',string)


    f0 = ttk.Frame(self.Window_lineconfig,padding=5)
    ttk.Button(f0,text='Cancel',command=_cancel,padding=5). \
        grid(row=0,column=0,padx=3)
    ttk.Button(f0,text='Apply',command=_apply,padding=5).   \
        grid(row=0,column=1,padx=3)
    ttk.Button(f0,text='Done',command=_done,padding=5).     \
        grid(row=0,column=2,padx=3)
    f0.grid(sticky='ew',columnspan=3)


  # ==============
  def codar(self):
  # ==============
    '''Widget to retrieve CODAR data'''

    def _close():
    # ===========
      self.Window_codar.destroy()
      self.Window_codar = None

    def _clear():
    # ===========
      print('original ncodar = ',self.ncodar)
      if self.ncodar == 0:
        _close()
        return
      ii = self.CODAR_INDX.get()
      print('Erasing record ', ii)
      del self.CODAR[ii]
      self.ncodar -= 1
      ii = self.ncodar-1 if ii >= self.ncodar else ii
      print('new ncodar = ',self.ncodar)
      _refill(ii)
    
    def _add():
    # =========
      CDR = codar.CODAR_CLASS()
      if CDR.Window_id is None:
        CDR.Window_id = tk.Toplevel(self.Window_codar)
        codar.WinCodar(CDR.Window_id,CDR)
        CDR.Window_id.wait_window()
        if empty(CDR.FILENAME.get()):
          return
        self.ncodar += 1
        self.CODAR.append(CDR)
        self.CODAR_INDX.set(self.ncodar-1)
        self.CODAR_LIST = list(range(self.ncodar))
        _wsel['values'] = self.CODAR_LIST
        ii = self.CODAR_INDX.get()
        _process()
      else:
        CDR.Window_id.lift()

    def _process():
    # =============
      ii = self.CODAR_INDX.get()

      print('reading ', self.CODAR[ii].FILENAME.get())

      try:
        self.CODAR[ii].ncid = Dataset(self.CODAR[ii].FILENAME.get(),'r')
      except:
        messagebox.showinfo(message='Unable to open file')
        del self.CODAR[ii]
        self.ncodar -= 1
        ii = self.ncodar-1 if ii >= self.ncodar else ii
        return
   
      _wsel['state'] = '!disabled'
      #_bsel['state'] = '!disabled'

      self.CODAR[ii].icdf = geocdf(self.CODAR[ii].FILENAME.get())
      self.CODAR[ii].uid  = self.CODAR[ii].icdf.vname.index(self.CODAR[ii].uname)
      self.CODAR[ii].vid  = self.CODAR[ii].icdf.vname.index(self.CODAR[ii].vname)
      self.CODAR[ii].lon  = self.CODAR[ii].ncid.variables[self.CODAR[ii].xname][:]
      self.CODAR[ii].lat  = self.CODAR[ii].ncid.variables[self.CODAR[ii].yname][:]
      self.CODAR[ii].time = self.CODAR[ii].ncid.variables[self.CODAR[ii].tname][:]
      self.CODAR[ii].xx,self.CODAR[ii].yy = np.meshgrid(self.CODAR[ii].lon, \
                                                        self.CODAR[ii].lat)
      try:
        self.CODAR[ii].time_units = \
                       self.CODAR[ii].ncid.variables[self.CODAR[ii].tname].units
      except:
        self.CODAR[ii].time_units = ''
      try:
        self.CODAR[ii].time_calendar = \
                       self.CODAR[ii].ncid.variables[self.CODAR[ii].tname].calendar
      except:
        self.CODAR[ii].time_calendar = 'gregorian'

      _wstat.config(text=self.CODAR[ii].STATION.get())
      self.CODAR[ii].L.set(0)
      self.CODAR[ii].nt = len(self.CODAR[ii].time)
      string = '{}'.format(num2date( \
                           self.CODAR[ii].time[self.CODAR[ii].L.get()], \
                           units=self.CODAR[ii].time_units, \
                           calendar=self.CODAR[ii].time_calendar))
      self.CODAR[ii].tlabel.set(string)
      _refill(ii)

    def _refill(ii):
    # ==============
      print('_refill ii = ',ii)
      if ii >= 0:
        self.CODAR_LIST = list(range(self.ncodar))
        _wsel['values'] = self.CODAR_LIST
        _went['textvariable'] = self.CODAR[ii].FILENAME
        _lbox['textvariable'] = self.CODAR[ii].tlabel
        _wstat['text'] = self.CODAR[ii].STATION.get()
      else:
        self.ncodar        = 0
        self.CODAR         = []
        self.CODAR_LIST    = ['0']
        self.CODAR_INDX    = tk.IntVar()
        self.CODAR_INDX.set(0)
        _wsel['values'] = self.CODAR_LIST
        _went['textvariable'] = ''
        _lbox['textvariable'] = ''
        _wstat['text'] = ''

      if self.CODAR[ii].nt > 1:
        _prev['state'] = '!disabled'
        _next['state'] = '!disabled'
      else:
        _prev['state'] = 'disabled'
        _next['state'] = 'disabled'

    def _tprev():
    # ===========
      '''Get the previous time step'''
      if self.CODAR[ii].L.get() > 0:
        self.CODAR[ii].L.set(self.CODAR[ii].L.get() - 1)
        string = '{}'.format(num2date(self.CODAR[ii].time[self.CODAR[ii].L.get()], \
                                            units=self.CODAR[ii].time_units, \
                                            calendar=self.CODAR[ii].time_calendar))
        self.CODAR[ii].tlabel.set(string)
        _read()
        self.make_plot()


    def _tnext():
    # ===========
      '''Get the next time step'''
      ii = self.CODAR_INDX.get()
      if self.CODAR[ii].L.get() < self.CODAR[ii].nt - 1:
        self.CODAR[ii].L.set(self.CODAR[ii].L.get() + 1)
        string = '{}'.format(num2date(self.CODAR[ii].time[self.CODAR[ii].L.get()], \
                                            units=self.CODAR[ii].time_units, \
                                            calendar=self.CODAR[ii].time_calendar))
        self.CODAR[ii].tlabel.set(string)
        _read()
        self.make_plot()

    def _read():
    # ===========
      ii    = self.CODAR_INDX.get()
      uname = self.CODAR[ii].uname
      vname = self.CODAR[ii].vname
      L     = self.CODAR[ii].L.get()
      self.CODAR[ii].u = self.CODAR[ii].ncid.variables[uname][L,:,:].squeeze()
      self.CODAR[ii].v = self.CODAR[ii].ncid.variables[vname][L,:,:].squeeze()
      
      _u   = self.CODAR[ii].u.copy()
      _v   = self.CODAR[ii].v.copy()
      msku = ma.getmask(self.CODAR[ii].u)
      mskv = ma.getmask(self.CODAR[ii].v)
      msk  = ma.mask_or(msku,mskv)
      self.CODAR[ii].u = ma.array(_u,mask=msk).copy()
      self.CODAR[ii].v = ma.array(_v,mask=msk).copy()

    def _done():
    # ===========
      ii = self.CODAR_INDX.get()
      if empty(self.CODAR[ii].FILENAME.get()):
        messagebox.showinfo(message='No station selected')
      else:
        _read()
        self.make_plot()
        _close()

    def _reget():
    # ===========
      self.CODAR_INDX.set(_wsel.get())
      ii = self.CODAR_INDX.get()
      _refill(ii)


    if self.Window_codar is None:
      self.Window_codar = tk.Toplevel(self.master)
      self.Window_codar.title("CODAR Stations")
      self.Window_codar.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_codar.lift()

    if self.ncodar > 0:
      ii = self.CODAR_INDX.get()
    else:
      ii = -1

    print('main codar window, ii = ',ii)

    _frame0 = ttk.Frame(self.Window_codar,padding=5)
    ttk.Label(_frame0,text='CODAR').grid(row=0,column=0,padx=3)
    
    _wsel = ttk.Combobox(_frame0,textvariable=self.CODAR_INDX, \
                                 values=self.CODAR_LIST)
    _wsel.grid(row=0,column=1)
    _wsel.bind('<<ComboboxSelected>>',lambda e: _reget())
      
    if ii == -1:
      _wstat = ttk.Label(_frame0,text='',width=50,justify='left')
    else:
      _wstat = ttk.Label(_frame0,text=self.CODAR[ii].STATION.get(),width=50,justify='left')
    _wstat.grid(row=0,column=2,padx=3)

    ttk.Button(_frame0,text='Add',command=_add).grid(row=0,column=4)
    _frame0.grid(row=0,column=0)
 
    _frame = ttk.Frame(self.Window_codar,padding=5)

    if ii == -1:
      _went = ttk.Entry(_frame,textvariable='', \
                               justify='left',width=80)
    else:
      _went = ttk.Entry(_frame,textvariable=self.CODAR[ii].FILENAME, \
                               justify='left',width=80)
    _went.grid(row=0,column=0,columnspan=8,padx=3)

    #_bsel = ttk.Button(_frame,text='Select',command=_selector)
    #_bsel.grid(row=0,column=8,padx=3)
    ttk.Label(_frame,text='Time').grid(row=1,column=0,padx=3)
    _prev = ttk.Button(_frame,text='PREV',command=_tprev)
    _prev.grid(row=1,column=1,padx=3)

    if ii == -1:
      _lbox = ttk.Entry(_frame,textvariable='', \
                               justify='left',width=36,state='readonly')
    else:
      _lbox = ttk.Entry(_frame,textvariable=self.CODAR[ii].tlabel, \
                               justify='left',width=36,state='readonly')
    _lbox.grid(row=1,column=2,columnspan=3,padx=3,sticky='w')
    _next = ttk.Button(_frame,text='NEXT',command=_tnext)
    _next.grid(row=1,column=5,padx=3)
    ttk.Button(_frame,text='Clear',command=_clear).grid(row=1,column=7,padx=3)
    ttk.Button(_frame,text='Done',command=_done).grid(row=1,column=8,padx=3)
    _frame.grid(row=1,column=0)

    if self.ncodar == 0:
      _wsel['state'] = 'disabled'
      #_bsel['state'] = 'disabled'
      _prev['state'] = 'disabled'
      _next['state'] = 'disabled'


  # ==============
  def floats(self):
  # ==============
    '''Widget to retrieve Lagrangian trajectory data'''

    def _close():
    # ===========
      self.Window_float.destroy()
      self.Window_float = None

    def _clear():
    # ===========
      if self.nfloat == 0:
        _close()
        return
      ii = self.FLOAT_INDX.get()
      print('Erasing record ', ii)
      del self.FLOAT[ii]
      self.nfloat -= 1
      ii = self.nfloat-1 if ii >= self.nfloat else ii
      print('new nfloat = ',self.nfloat)
      self.FLOAT_INDX.set(ii)
      _refill(ii)
      self.make_plot()
      _close()
    
    def _reget():
    # ===========
      self.FLOAT_INDX.set(_wsel.get())
      ii = self.FLOAT_INDX.get()
      _refill(ii)

    def _refill(ii):
    # ==============
      if ii >= 0:
        self.FLOAT_LIST = list(range(self.nfloat))
        _wsel['values'] = self.FLOAT_LIST
        _went['textvariable'] = self.FLOAT[ii].FILENAME
        _wstat['text'] = ' Nfloats = '+str(self.FLOAT[ii].nfloats)
      else:
        self.FLOAT         = []
        self.FLOAT_LIST    = ['0']
        self.FLOAT_INDX    = tk.IntVar()
        self.FLOAT_INDX.set(0)
        _wsel['values'] = self.FLOAT_LIST
        _went['textvariable'] = ''
        _wstat['text'] = ''


    def _add():
    # ========
      nn = filedialog.askopenfile(filetypes=[('JSON','*.json'),        \
                                             ('GEOJSON','*.geojson'),  \
                                             ('Netcdf','*.nc'),  \
                                             ('CDF','*.cdf'),  \
                                             ('ALL','*')])
      try:
        if empty(nn.name):
          return
      except:
        return

      # Not empty filename:
      FLT = lagrangian.Read(nn.name)
      FLT.TIME = np.array([(FLT.date[i].replace(tzinfo=None)-self.FLD.DATE[0]).total_seconds() for i in range(FLT.nrecords)])
      FLT.MAPX = []
      FLT.MAPY = []
      if FLT.nfloats > 1:
        for i in range(FLT.nfloats):
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon[:,i]), bounds_error=False, fill_value=np.NaN)
          FLT.MAPX.append(list(f(self.FLD.TIME)))
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat[:,i]), bounds_error=False, fill_value=np.NaN)
          FLT.MAPY.append(list(f(self.FLD.TIME)))
      else:
        f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon), bounds_error=False, fill_value=np.NaN)
        FLT.MAPX = list(f(self.FLD.TIME))
        f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat), bounds_error=False, fill_value=np.NaN)
        FLT.MAPY = list(f(self.FLD.TIME))

      self.nfloat += 1
      self.FLOAT.append(FLT)
      self.FLOAT_INDX.set(self.nfloat-1)
      self.FLOAT_LIST = list(range(self.nfloat))
      ii = self.FLOAT_INDX.get()
      _refill(ii)

      #_wsel['values'] = self.FLOAT_LIST
      #_went['textvariable'] = self.FLOAT[ii].FILENAME
      self.make_plot()

    # Main window:
    # ============
    if self.Window_float is None:
      self.Window_float = tk.Toplevel(self.master)
      self.Window_float.title("Lagrangian Trajectories")
      self.Window_float.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_float.lift()

    if self.nfloat > 0:
      ii = self.FLOAT_INDX.get()
    else:
      ii = -1

    F0 = ttk.Frame(self.Window_float,padding=5)
    ttk.Label(F0,text='Trajectory').grid(row=0,column=0,padx=3)
    
    _wsel = ttk.Combobox(F0,textvariable=self.FLOAT_INDX, \
                                  values=self.FLOAT_LIST)
    _wsel.grid(row=0,column=1)
    _wsel.bind('<<ComboboxSelected>>',lambda e: _reget())

    if ii == -1:
      _wstat = ttk.Label(F0,text='',width=50,justify='left')
    else:
      _wstat = ttk.Label(F0,text=' Nfloats = '+str(self.FLOAT[ii].nfloats),width=50,justify='left')
    _wstat.grid(row=0,column=2,columnspan=5,padx=3)

    ttk.Button(F0,text='Add',command=_add).grid(row=0,column=7,padx=3)
    F0.grid(row=0,column=0)
 
    F1 = ttk.Frame(self.Window_float,padding=5)

    ttk.Label(F1,text='Filename').grid(row=0,column=0,padx=3)
    if ii == -1:
      _went = ttk.Entry(F1,textvariable='', \
                               justify='left',width=80)
    else:
      _went = ttk.Entry(F1,textvariable=self.FLOAT[ii].FILENAME, \
                               justify='left',width=80)
    _went.grid(row=0,column=1,columnspan=7,padx=3)

    ttk.Button(F1,text='Clear',command=_clear).grid(row=1,column=6,padx=3)
    ttk.Button(F1,text='Done',command=_close).grid(row=1,column=7,padx=3)
    F1.grid(row=1,column=0)
      


  #================
  def saidin(self):
  #================
    '''Function to retrieve the SAIDIN data'''

    def _close():
      self.Window_saidin.destroy()
      self.Window_saidin = None
    
    def _selector():
      name = saidin.saidin_selector(parent=self.master)
      if not empty(name):
        self.SAIDIN.FILENAME.set(name)

    def _done():
      if (empty(self.SAIDIN.FILENAME.get())):
        messagebox.showinfo(message='No image selected')
        return

      scdf = Dataset(self.SAIDIN.FILENAME.get(),'r')
      self.SAIDIN.varname = 'mcsst'
      self.SAIDIN.lon = scdf.variables['lon'][:]
      self.SAIDIN.lat = scdf.variables['lat'][:]
      self.SAIDIN.sst = scdf.variables[self.SAIDIN.varname][0,:,:].squeeze()
      self.SAIDIN.xx,self.SAIDIN.yy = np.meshgrid(self.SAIDIN.lon,self.SAIDIN.lat)

      # Check if data has to be masked:
      if self.SAIDIN.mask.get():
        print('Applyng land/sea mask ...')
        _a   = self.SAIDIN.sst.copy()
        msk1 = ma.getmask(self.SAIDIN.sst)
        tmp  = scdf.variables['lsmask'][0,:,:].squeeze()
        msk2 = ma.make_mask(ma.masked_values(tmp,1),dtype=bool)
        msk  = ma.mask_or(msk1,msk2)
        self.SAIDIN.sst = ma.array(_a,mask=msk).copy()

      try:
        self.SAIDIN.units = scdf.variables[self.SAIDIN.varname].getncattr('units')
      except:
        self.SAIDIN.units = ''

      try:
        self.SAIDIN.missing_value = scdf.variables[self.SAIDIN.varname].getncattr('_FillValue')
      except:
        try:
          self.SAIDIN.missing_value = scdf.variables[self.SAIDIN.varname].getncattr('missing_value')
        except:
          self.SAIDIN.missing_value = None
 
      self.SAIDIN.minval = self.SAIDIN.sst.min()
      self.SAIDIN.maxval = self.SAIDIN.sst.max()
      self.SAIDIN.PLOT.CONTOUR_MIN.set(truncation(self.SAIDIN.minval))
      self.SAIDIN.PLOT.CONTOUR_MAX.set(truncation(self.SAIDIN.maxval))
      self.SAIDIN.PLOT.CONTOUR_INTERVAL.set(truncation(0.1*(self.SAIDIN.PLOT.CONTOUR_MAX.get() \
                                                           -self.SAIDIN.PLOT.CONTOUR_MIN.get())))
      self.SAIDIN.PLOT.CONTOUR_MODE.set(1)
      self.SAIDIN.F = interpolate.interp2d(self.SAIDIN.lon, \
                                           self.SAIDIN.lat, \
                                           self.SAIDIN.sst)
      self.make_plot()
      _close()

    def _clear():
      self.SAIDIN.FILENAME.set('')
      self.SAIDIN.lon = None
      self.SAIDIN.lat = None
      self.SAIDIN.xx  = None
      self.SAIDIN.yy  = None
      self.SAIDIN.sst = None
      _close()

    if self.Window_saidin is None:
      self.Window_saidin = tk.Toplevel(self.master)
      self.Window_saidin.title("SAIDIN temperature images")
      self.Window_saidin.protocol('WM_DELETE_WINDOW',_close)
    else:
      self.Window_saidin.lift()

    _frame = ttk.Frame(self.Window_saidin,padding=5)
    ttk.Entry(_frame,textvariable=self.SAIDIN.FILENAME,justify='left', \
              width=80).grid(row=0,column=0,columnspan=8,padx=3)
    ttk.Button(_frame,text='Select',command=_selector).grid(row=0,column=8,padx=3)
    ttk.Checkbutton(_frame,text='Mask data',variable=self.SAIDIN.mask).grid(row=1,column=6,padx=3)
    ttk.Button(_frame,text='Clear',command=_clear).grid(row=1,column=7,padx=3)
    ttk.Button(_frame,text='Done',command=_done).grid(row=1,column=8,padx=3)


    _frame.grid(row=0,column=0)


  # =============================
  def sselection(self,ncid,icdf):
  # =============================
    '''Read 2D field and superposes it to the velocity plot'''

    value_selected = self.SVARNAME.get()
    if bool(value_selected.strip()):
       self.FIELD.varname = self.SVARNAME.get()
       index = icdf.vname.index(value_selected)
       if index != self.FLD.sid:
         self.init_field = True
       if icdf.ndims[index] == 1:
         messagebox.showinfo(message='Invalid variable. \
                    It must have at least two dimensions')
         self.superpose = False
         self.SVARNAME.set('')
         self.FLD.sid = -1
         self.int_field     = True
       else:
         self.FLD.sid = index
         self.read_S (self.FLD.ncid,self.FLD.icdf,self.FLD.sid)
    else:
      self.superpose = False
      self.SVARNAME.set('')
      self.FLD.sid       = -1
      self.FIELD.varname = None
      self.int_field     = True
    self.sbox.selection_clear()

    self.make_plot()

  # =============================
  def read_S(self,ncid,icdf,sid):
  # =============================
    '''Read 2D data according to user selections'''

    if sid < 0:
      self.FIELD.data = None
      self.init_field = True
      return

    K     = self.FLD.K.get()
    L     = self.FLD.L.get()
    sname = self.SVARNAME.get()

    print('READ_S Reading Level and Time:', sname, K, L)

    ndim = icdf.ndims[sid]
    if ndim == 2:
      self.FIELD.data = ncid.variables[sname][:,:]
    elif ndim == 3:
      if icdf.ppl[sid] > -1:
        self.FIELD.data = ncid.variables[sname][L,:,:].squeeze()
      elif icdf.ppk[sid]  > -1:
        self.FIELD.data = ncid.variables[sname][K,:,:].squeeze()
      else:
        messagebox.showinfo(message='Invalid variable dimensions')
        self.FIELD.data = None
    elif ndim == 4:
      self.FIELD.data = ncid.variables[sname][L,K,:,:].squeeze()


    if self.FIELD.data is not None:
      try:
        self.FIELD.units = ncid.variables[sname].getncattr('units')
      except:
        self.FIELD.units = ''

      try:
        self.FIELD.missing_value = ncid.variables[sname].getncattr('_FillValue')
      except:
        try:
          self.FIELD.missing_value = ncid.variables[sname].getncattr('missing_value')
        except:
          self.FIELD.missing_value = None
 
      self.superpose = True
      if self.init_field:
        self.FIELD.minval = self.FIELD.data.min()
        self.FIELD.maxval = self.FIELD.data.max()
        self.FIELD.PLOT.CONTOUR_MIN.set(truncation(self.FIELD.minval))
        self.FIELD.PLOT.CONTOUR_MAX.set(truncation(self.FIELD.maxval))
        ds = truncation(0.10*(self.FIELD.PLOT.CONTOUR_MAX.get()-
                              self.FIELD.PLOT.CONTOUR_MIN.get()))
        self.FIELD.PLOT.CONTOUR_INTERVAL.set(ds)
        self.init_field = False
    else:
      self.superpose = False


  # ========================
  def kselection(self,icdf):
  # ========================
    self.FLD.K.set(int(self.kbox.get()))
    self.PLOT.ZLABEL.set(str(self.FLD.Z_LIST[self.FLD.K.get()]))


  # ========================
  def lselection(self,icdf):
  # ========================
    self.FLD.L.set(int(self.lbox.get()))
    string = '{}'.format(num2date(self.FLD.T_LIST[self.FLD.L.get()], \
                                                  units=icdf.time_units, \
                                                  calendar=icdf.time_calendar))
    self.PLOT.TLABEL.set(string)


  # ===================================
  def tprev(self):
  # ===================================
    '''Points to the previous time step'''
    if self.FLD.L.get() > 0:
      self.FLD.L.set(self.FLD.L.get() - 1)
      string = '{}'.format(num2date(self.FLD.T_LIST[self.FLD.L.get()], \
                                       units=self.FLD.icdf.time_units, \
                                       calendar=self.FLD.icdf.time_calendar))
      self.PLOT.TLABEL.set(string)
      self.read_UV(self.FLD.ncid,self.FLD.icdf,self.FLD.uid,self.FLD.vid)
      self.read_S(self.FLD.ncid,self.FLD.icdf,self.FLD.sid)
      self.make_plot()

  # ================================
  def tnext(self):
  # ================================
    '''Points to the next time step'''
    if self.FLD.L.get() < self.FLD.icdf.nt - 1:
      self.FLD.L.set(self.FLD.L.get() + 1)
      string = '{}'.format(num2date(self.FLD.T_LIST[self.FLD.L.get()], \
                                       units=self.FLD.icdf.time_units, \
                                       calendar=self.FLD.icdf.time_calendar))
      self.PLOT.TLABEL.set(string)
      self.read_UV(self.FLD.ncid,self.FLD.icdf,self.FLD.uid,self.FLD.vid)
      self.read_S(self.FLD.ncid,self.FLD.icdf,self.FLD.sid)
      self.make_plot()


  # ======================================
  def data_update(self):
  # ======================================
    '''Makes the new plot according to the user selections. It call self.read to get the new data'''
    self.read_UV(self.FLD.ncid,self.FLD.icdf,self.FLD.uid,self.FLD.vid)
    self.read_S(self.FLD.ncid,self.FLD.icdf,self.FLD.sid)
    self.make_plot()


  # ====================================
  def read_UV(self,ncid,icdf,uid,vid):
  # ====================================
    '''Read u and v data according to user selections'''

    K    = self.FLD.K.get()
    L    = self.FLD.L.get()
    uname = icdf.vname[uid]
    vname = icdf.vname[vid]

    print('READ_UV Reading Level and Time:', K, L)

    ndim = icdf.ndims[uid]
    if ndim == 2:
      self.CURRENTS.u = ncid.variables[uname][:,:]
      self.CURRENTS.v = ncid.variables[vname][:,:]
    elif ndim == 3:
      if icdf.ppl[uid] > -1:
        self.CURRENTS.u = ncid.variables[uname][L,:,:].squeeze()
        self.CURRENTS.v = ncid.variables[vname][L,:,:].squeeze()
      elif icdf.ppk[uid] > -1:
        self.CURRENTS.u = ncid.variables[uname][K,:,:].squeeze()
        self.CURRENTS.v = ncid.variables[vname][K,:,:].squeeze()
      else:
        print('Invalid file!')
        quit()
    elif ndim == 4:
      self.CURRENTS.u = ncid.variables[uname][L,K,:,:].squeeze()
      self.CURRENTS.v = ncid.variables[vname][L,K,:,:].squeeze()
    else:
      print('Invalid number of dimensions, ',ndim)

    _u   = self.CURRENTS.u.copy()
    _v   = self.CURRENTS.v.copy()
    msku = ma.getmask(self.CURRENTS.u)
    mskv = ma.getmask(self.CURRENTS.v)
    msk  = ma.mask_or(msku,mskv)
    self.CURRENTS.u = ma.array(_u,mask=msk).copy()
    self.CURRENTS.v = ma.array(_v,mask=msk).copy()
    self.CURRENTS.speed = np.sqrt(self.CURRENTS.u**2+self.CURRENTS.v**2)
    self.CURRENTS.F = interpolate.interp2d(self.CURRENTS.lon, \
                                           self.CURRENTS.lat, \
                                           self.CURRENTS.speed)

  # ---------------------------------------------------------------
  # --------------------------------------------------------------- MAKE_PLOT

  # ==================
  def make_plot(self):
  # ==================
    '''Draws the map'''

    try:
      self.FIELD.cbar.remove()
    except:
      pass
 
    try:
      self.SAIDIN.cbar.remove()
    except:
      pass
 
    self.ax1.clear()

    global m
    SOUTH = float(self.PLOT.SOUTH.get())
    NORTH = float(self.PLOT.NORTH.get())
    WEST  = float(self.PLOT.WEST.get())
    EAST  = float(self.PLOT.EAST.get())
    m = Basemap(projection=self.PLOT.MAP_PROJECTION.get(),            \
                           resolution=self.PLOT.MAP_RESOLUTION.get(), \
                           llcrnrlat=SOUTH,                           \
                           urcrnrlat=NORTH,                           \
                           llcrnrlon=WEST,                            \
                           urcrnrlon=EAST,                            \
                           ax=self.ax1)

    if not empty(self.SAIDIN.FILENAME.get()):
      self.SAIDIN.cbar = contourplot.drawing(self.fig,self.ax1,m, \
                          self.SAIDIN.xx,self.SAIDIN.yy,self.SAIDIN.sst, \
                          self.SAIDIN.PLOT)

    # Superpose scalar plot from the input file ...
    if self.superpose:
      self.FIELD.cbar = contourplot.drawing(self.fig,self.ax1,m, \
                          self.FIELD.xx,self.FIELD.yy,self.FIELD.data, \
                          self.FIELD.PLOT)

    if self.PLOT.COASTLINE.get() == 1:
      m.drawcoastlines(linewidth=self.PLOT.COASTLINE_WIDTH.get(), \
                       color=self.PLOT.COASTLINE_COLOR.get())

    if len(self.PLOT.ISOBAT_selected) > 0:
      for ii in range(len(self.PLOT.ISOBAT_Z)):
        z = self.PLOT.ISOBAT_Z[ii]
        for segment in range(z['tramos']):
          ilon = z['lon'][z['tini'][segment]:z['tfin'][segment]]
          ilat = z['lat'][z['tini'][segment]:z['tfin'][segment]]
          isox,isoy = m(ilon,ilat)
          isbt, = m.plot(isox,isoy,marker=None, \
                    linewidth=self.PLOT.ISOBAT_WIDTH.get(), \
                    color=self.PLOT.ISOBAT_COLOR.get(),     \
                    label=self.PLOT.ISOBAT_LABEL[self.PLOT.ISOBAT_indx[ii]])

          #lineplot.LabelLine(self.ax1,isbt,-4,isbt.get_label(),align=True)

    if self.PLOT.WATER_COLOR.get() is not 'None':
      m.drawmapboundary(fill_color=self.PLOT.WATER_COLOR.get())

    if self.PLOT.LAND_COLOR.get() is not 'None':
      m.fillcontinents(color=self.PLOT.LAND_COLOR.get())

    if self.PLOT.RIVERS.get() == 1:
      m.drawrivers(linewidth=self.PLOT.RIVERS_WIDTH.get(), \
                   color=self.PLOT.RIVERS_COLOR.get())

    if self.PLOT.BLUEMARBLE.get() == 1:
      m.bluemarble()

    if self.PLOT.ETOPO.get() == 1:
      m.etopo()

    if self.PLOT.ARCGISIMAGE.get() == 1:
      m.arcgisimage(service=self.PLOT.ARCGISSERVICE.get(), \
                    xpixels=self.PLOT.ARCGISPIXELS.get(),  \
                    dpi=self.PLOT.ARCGISDPI.get(),  \
                    #epsg= self.PLOT.ARCGISEPSG.get(),  \
                    verbose=self.PLOT.ARCGISVERBOSE.get())

    if self.PLOT.SHOW_GRID.get() == 1:
      vmeridians = np.arange(self.PLOT.MERIDIAN_INI.get(), \
                             self.PLOT.MERIDIAN_FIN.get(), \
                             self.PLOT.MERIDIAN_INT.get())
      m.drawmeridians(vmeridians,labels=[1,0,0,1], \
                        fontsize=self.PLOT.LONLAT_SIZE.get(), \
                        color=self.PLOT.LONLAT_COLOR.get())

      vparallels = np.arange(self.PLOT.PARALLEL_INI.get(), \
                             self.PLOT.PARALLEL_FIN.get(), \
                             self.PLOT.PARALLEL_INT.get())
      m.drawparallels(vparallels,labels=[0,1,0,1], \
                        fontsize=self.PLOT.LONLAT_SIZE.get(), \
                        color=self.PLOT.LONLAT_COLOR.get())


    # Plot currents
    vectorplot.drawing(self.fig,self.ax1,m,self.CURRENTS)


    if self.ncodar > 0:
      for ii in range(self.ncodar):
        vectorplot.drawing(self.fig,self.ax1,m,self.CODAR[ii])

    if self.nfloat > 0:
      for ii in range(self.nfloat):
        self.FLOAT[ii].L.set(self.FLD.L.get())
        lagrangian.drawing(self.fig,self.ax1,m,self.FLOAT[ii])

    # Lables and titles:
    self.ax1.xaxis.labelpad = self.PLOT.LABEL_PAD.get()
    self.ax1.yaxis.labelpad = self.PLOT.LABEL_PAD.get()
    self.ax1.set_xlabel(self.PLOT.XLABEL.get(), \
                        fontsize=self.PLOT.LABEL_SIZE.get())
    self.ax1.set_ylabel(self.PLOT.YLABEL.get(), \
                        fontsize=self.PLOT.LABEL_SIZE.get())

    if int(self.PLOT.TITLE_BOLD.get()) == 1:
      weight = 'bold'
    else:
      weight = 'normal'
    self.ax1.set_title(self.PLOT.TITLE.get(), \
                        fontsize=self.PLOT.TITLE_SIZE.get(),fontweight=weight)

    # Time stamp
    try:
      self.time_stamp.remove()
    except:
      pass
    if self.PLOT.TIMESTAMP_SHOW.get():
      if self.PLOT.TIMESTAMP_BOLD.get():
        weight = 'bold'
      else:
        weight = 'normal'
      self.time_stamp = self.fig.text(self.PLOT.TIMESTAMP_X.get(),     \
                                      self.PLOT.TIMESTAMP_Y.get(),     \
                                      self.FLD.DATE[self.FLD.L.get()], \
                                      fontsize=self.PLOT.TIMESTAMP_SIZE.get(), \
                                      color=self.PLOT.TIMESTAMP_COLOR.get(), \
                                      fontweight=weight)

    if self.PLOT.LOGO_DISPLAY.get() == 1:
      self.plot_logo()

    self.canvas.draw()

  
  # ==================
  def plot_logo(self):
  # ==================
    '''Add a logo in the plot'''

    global m
    im = OffsetImage(self.PLOT.LOGO_IMAGE,zoom=self.PLOT.LOGO_ZOOM.get())

    if self.PLOT.LOGO_LOCATION.get() == 'SW':
      xx = self.PLOT.WEST.get()
      yy = self.PLOT.SOUTH.get()
      ba = (0,0)
    elif self.PLOT.LOGO_LOCATION.get() == 'NW':
      xx = self.PLOT.WEST.get()
      yy = self.PLOT.NORTH.get()
      ba = (0,1)
    elif self.PLOT.LOGO_LOCATION.get() == 'NE':
      xx = self.PLOT.EAST.get()
      yy = self.PLOT.NORTH.get()
      ba = (1,1)
    elif self.PLOT.LOGO_LOCATION.get() == 'SE':
      xx = self.PLOT.EAST.get()
      yy = self.PLOT.SOUTH.get()
      ba = (1,0)
    else:
      xx = self.PLOT.LOGO_X.get()
      yy = self.PLOT.LOGO_Y.get()
      ba = (0,0)

    self.ab = AnnotationBbox(im,(xx,yy), xycoords='data', \
                             box_alignment=ba,pad=0.0,frameon=True)
    self.with_logo = m._check_ax().add_artist(self.ab)
    #self.canvas.draw()


  # =====================
  def label_config(self):
  # =====================
    '''Options for Map titles and labels'''

    def _close():
      self.Window_labelconfig.destroy()
      self.Window_labelconfig = None

    def _done():
      self.make_plot()
      self.Window_labelconfig.destroy()
      self.Window_labelconfig = None

    if self.Window_labelconfig is not None:
      self.Window_labelconfig.lift()
      return
    else:
      self.Window_labelconfig = tk.Toplevel(self.master)
      self.Window_labelconfig.title('Title and Label options')
      self.Window_labelconfig.resizable(width=True,height=True)
      self.Window_labelconfig.protocol('WM_DELETE_WINDOW',_close)
    
    # Main
    # ----
    frame = ttk.Frame(self.Window_labelconfig,borderwidth=5,padding=5)
    ttk.Label(frame,text='Title').grid(row=0,column=0,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.TITLE,width=40). \
        grid(row=0,column=1,columnspan=3)
    ttk.Checkbutton(frame,text='Bold',variable=self.PLOT.TITLE_BOLD). \
        grid(row=0,column=5)
    ttk.Label(frame,text='Size').grid(row=1,column=0,columnspan=1,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.TITLE_SIZE,width=7). \
        grid(row=1,column=1,sticky='w')
    ttk.Label(frame,text='X label').grid(row=2,column=0,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.XLABEL,width=40). \
        grid(row=2,column=1,columnspan=3,sticky='w')
    ttk.Label(frame,text='Y label').grid(row=3,column=0,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.YLABEL,width=40). \
        grid(row=3,column=1,columnspan=3,sticky='w')
    ttk.Label(frame,text='Size').grid(row=4,column=0,columnspan=1,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.LABEL_SIZE,width=5). \
        grid(row=4,column=1,columnspan=1,sticky='w')
    ttk.Label(frame,text='Label Pad'). \
        grid(row=5,column=0,columnspan=1,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.LABEL_PAD,width=5). \
        grid(row=5,column=1,columnspan=1,sticky='w')
    ttk.Checkbutton(frame,text='Plot logo',variable=self.PLOT.LOGO_DISPLAY). \
        grid(row=6,column=1,sticky='w')
    ttk.Label(frame,text='Timestamp'). \
        grid(row=7,column=0,pady=4,sticky='w')

    ttk.Checkbutton(frame,text='Show',variable=self.PLOT.TIMESTAMP_SHOW). \
        grid(row=8,column=1,sticky='w')
    ttk.Checkbutton(frame,text='Bold',variable=self.PLOT.TIMESTAMP_BOLD). \
        grid(row=9,column=1,sticky='w')
    ttk.Label(frame,text='X pos'). \
        grid(row=10,column=0,columnspan=1,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.TIMESTAMP_X,width=5). \
        grid(row=10,column=1,columnspan=1,sticky='w')
    ttk.Label(frame,text='Y pos'). \
        grid(row=11,column=0,columnspan=1,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.TIMESTAMP_Y,width=5). \
        grid(row=11,column=1,columnspan=1,sticky='w')
    ttk.Label(frame,text='Size'). \
        grid(row=12,column=0,columnspan=1,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.TIMESTAMP_SIZE,width=5). \
        grid(row=12,column=1,columnspan=1,sticky='w')
    ttk.Label(frame,text='Color'). \
        grid(row=13,column=0,columnspan=1,sticky='w')
    ttk.Entry(frame,textvariable=self.PLOT.TIMESTAMP_COLOR,width=10). \
        grid(row=13,column=1,columnspan=2,sticky='w')



    ttk.Button(frame,text='Done',command=_done).grid(row=14,column=5,pady=4)
    frame.grid()


  # ===================
  def map_config(self):
  # ===================
    '''Options for Map limits and colors'''

    pdict = {'cyl' :'Cylindrical Equidistant', \
             'merc':'Mercator',               \
             'cea' :'Cylindrical Equal Area',  \
             'gall':'Gall Stereographic Cylindrical'}

    rdict = {'c':'Crude',             \
             'l':'Low',               \
             'i':'Intermediate',      \
             'h':'High',              \
             'f':'Full'}

    def _close():
      self.Window_mapconfig.destroy()
      self.Window_mapconfig = None

    def pselection():
      mpl.config(text=pdict[self.PLOT.MAP_PROJECTION.get()],width=25)

    def rselection():
      mrl.config(text=rdict[self.PLOT.MAP_RESOLUTION.get()],width=10)

    def ccselection():
      rgb, hx = askcolor(color=self.PLOT.COASTLINE_COLOR.get(),parent=self.master)
      self.PLOT.COASTLINE_COLOR.set(hx)

    def icselection():
      rgb, hx = askcolor(color=self.PLOT.ISOBAT_COLOR.get(),parent=self.master)
      self.PLOT.ISOBAT_COLOR.set(hx)

    def lcselection():
      if self.PLOT.LAND_COLOR.get() == 'None':
        rgb, hx = askcolor(parent=self.master)
      else:
        rgb, hx = askcolor(color=self.PLOT.LAND_COLOR.get(),parent=self.master)
      self.PLOT.LAND_COLOR.set(hx)

    def wcselection():
      if self.PLOT.WATER_COLOR.get() == 'None':
        rgb, hx = askcolor(parent=self.master)
      else:
        rgb, hx = askcolor(color=self.PLOT.WATER_COLOR.get(),parent=self.master)
      self.PLOT.WATER_COLOR.set(hx)

    def rcselection():
      if self.PLOT.RIVERS_COLOR.get() == 'None':
        rgb, hx = askcolor(parent=self.master)
      else:
        rgb, hx = askcolor(color=self.PLOT.RIVERS_COLOR.get(),parent=self.master)
      self.PLOT.RIVERS_COLOR.set(hx)

    def lims_apply():
      global m
      self.ax1.set_xlim(self.PLOT.WEST.get(),self.PLOT.EAST.get())
      self.ax1.set_ylim(self.PLOT.SOUTH.get(),self.PLOT.NORTH.get())
      self.canvas.draw()

    def add_isobath(): 
      self.PLOT.selec = []
      for i in range(self.PLOT.nisobat):
        if self.PLOT.wvar[i].get() == 1:
          self.PLOT.selec.append(self.PLOT.ISOBATHS[i])

    def lims_reset():
      self.PLOT.WEST.set(self.PLOT.DATA_WEST)
      self.PLOT.EAST.set(self.PLOT.DATA_EAST)
      self.PLOT.SOUTH.set(self.PLOT.DATA_SOUTH)
      self.PLOT.NORTH.set(self.PLOT.DATA_NORTH)
      self.make_plot()

    def iload():
      '''Load from external file the selected isobaths'''

      self.PLOT.ISOBAT_Z = []
      for i in range(len(self.PLOT.ISOBAT_selected)):
        filejson = self.PLOT.ISOBAT_PATH.get() + \
                   '/%04d' % self.PLOT.ISOBAT_selected[i] + '.json'
        print('Loading file ' + filejson)
        try:
          with open(filejson) as infile:
            d = json.load(infile)
            self.PLOT.ISOBAT_Z.append(d) 
        except:
          messagebox.showinfo(message='Error file '+filejson+' not found')
     
    def _pselect():
      nn = tk.filedialog.askdirectory(parent=self.Window_mapconfig)
      if not empty(nn):
        self.PLOT.ISOBAT_PATH.set(nn)

    def get_isobaths():
      self.PLOT.ISOBAT_selected = []
      self.PLOT.ISOBAT_indx = []
      for i in range(self.PLOT.nisobat):
        if self.PLOT.wvar[i].get() == 1:
          self.PLOT.ISOBAT_selected.append(self.PLOT.ISOBATHS[i])
          self.PLOT.ISOBAT_indx.append(i)
          #print(str(self.PLOT.ISOBATHS[i]) + ' added')

    def _cancel():
      self.Window_mapconfig.destroy()
      self.Window_mapconfig = None

    def _apply():
      self.make_plot()

    def _done():
      self.make_plot()
      self.Window_mapconfig.destroy()
      self.Window_mapconfig = None


    if self.Window_mapconfig is not None:
      self.Window_mapconfig.lift()
      return

    self.Window_mapconfig = tk.Toplevel(self.master)
    self.Window_mapconfig.title('Map options')
    self.Window_mapconfig.resizable(width=True,height=True)
    self.Window_mapconfig.protocol('WM_DELETE_WINDOW',_close)
  

    f1 = ttk.Frame(self.Window_mapconfig,borderwidth=5,padding=5)
    ttk.Label(f1,text='Map Projection').grid(row=0,column=0,padx=3,sticky='w')
    mp = ttk.Combobox(f1,textvariable=self.PLOT.MAP_PROJECTION,values=('cyl','merc','cea','gall'),width=7)
    mp.grid(row=0,column=1,padx=3)
    mp.bind('<<ComboboxSelected>>',lambda e: pselection())
    mpl = ttk.Label(f1,text=pdict[self.PLOT.MAP_PROJECTION.get()],width=25)
    mpl.grid(row=0,column=2,columnspan=3,padx=3)

    ttk.Label(f1,text='Map Resolution').grid(row=1,column=0,padx=3,sticky='w')
    mr = ttk.Combobox(f1,textvariable=self.PLOT.MAP_RESOLUTION,values=('c','l','i','h','f'),width=7)
    mr.grid(row=1,column=1,padx=3)
    mr.bind('<<ComboboxSelected>>',lambda e: rselection())
    mrl = ttk.Label(f1,text=rdict[self.PLOT.MAP_RESOLUTION.get()],width=10)
    mrl.grid(row=1,column=2,columnspan=2,padx=3,sticky='w')

    ttk.Label(f1,text='Coastline').grid(row=2,column=0,padx=3,sticky='w')
    mcf = ttk.Checkbutton(f1,text='Show',variable=self.PLOT.COASTLINE)   # Map Coastline Flag
    mcf.grid(row=2,column=1,padx=3)
    
    ttk.Label(f1,text='Coastline width').grid(row=3,column=0,padx=3,sticky='w')
    mcw = ttk.Entry(f1,textvariable=self.PLOT.COASTLINE_WIDTH,justify='left',width=7)  # Map Coastline Width
    mcw.grid(row=3,column=1,padx=3,sticky='we')

    ttk.Label(f1,text='Coastline color').grid(row=4,column=0,padx=3,sticky='w')
    mcc = ttk.Entry(f1,textvariable=self.PLOT.COASTLINE_COLOR,justify='left',width=7). \
                   grid(row=4,column=1,padx=3,sticky='we')
    ttk.Button(f1,text='Select',command=ccselection).grid(row=4,column=2,padx=3,sticky='ew')

    ttk.Label(f1,text='Continent color').grid(row=5,column=0,padx=3,sticky='w')
    ttk.Entry(f1,textvariable=self.PLOT.LAND_COLOR,justify='left',width=7). \
              grid(row=5,column=1,padx=3,sticky='we')
    ttk.Button(f1,text='Select',command=lcselection).grid(row=5,column=2,padx=3,sticky='ew')

    ttk.Label(f1,text='Sea color').grid(row=6,column=0,padx=3,sticky='w')
    ttk.Entry(f1,textvariable=self.PLOT.WATER_COLOR,justify='left',width=7). \
             grid(row=6,column=1,padx=3,sticky='we')
    mwp = ttk.Button(f1,text='Select',command=wcselection).grid(row=6,column=2,padx=3,sticky='ew')

    f1.grid(row=0,column=0)

    f2 = ttk.Frame(self.Window_mapconfig,borderwidth=5,padding=5,relief='sunken')
    ttk.Label(f2,text='Plot limits',font='Helvetica 12 bold').grid(row=0,column=0,padx=3,sticky='w')
    ttk.Label(f2,text='North').grid(row=1,column=3,pady=5,padx=3)
    ttk.Entry(f2,textvariable=self.PLOT.NORTH,width=10).grid(row=2,column=3,pady=5,padx=3)
    ttk.Label(f2,text='West').grid(row=3,column=1,pady=5,padx=3)
    ttk.Entry(f2,textvariable=self.PLOT.WEST,width=10).grid(row=3,column=2,pady=5,padx=3)
    ttk.Entry(f2,textvariable=self.PLOT.EAST,width=10).grid(row=3,column=4,pady=5,padx=3)
    ttk.Label(f2,text='East').grid(row=3,column=5,pady=5,padx=3)
    ttk.Entry(f2,textvariable=self.PLOT.SOUTH,width=10).grid(row=4,column=3,pady=5,padx=3)
    ttk.Label(f2,text='South').grid(row=5,column=3,pady=5,padx=3)
    ttk.Button(f2,text='Reset',command=lims_reset).grid(row=6,column=5)
    f2.grid()

    f3 = ttk.Frame(self.Window_mapconfig,borderwidth=5,padding=5)
    ttk.Label(f3,text='Rivers').grid(row=0,column=0,padx=3,sticky='w')
    ttk.Checkbutton(f3,text='Show',variable=self.PLOT.RIVERS).grid(row=0,column=1,padx=3)
    ttk.Label(f3,text='Rivers width').grid(row=1,column=0,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.RIVERS_WIDTH,justify='left',width=7). \
              grid(row=1,column=1,padx=3,sticky='we')
    ttk.Label(f3,text='Rivers color').grid(row=2,column=0,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.RIVERS_COLOR,justify='left',width=7). \
              grid(row=2,column=1,padx=3,sticky='we')
    ttk.Button(f3,text='Select',command=rcselection).grid(row=2,column=2,padx=3,sticky='ew')
    ttk.Label(f3,text='Bluemarble').grid(row=3,column=0,padx=3,sticky='w')
    ttk.Checkbutton(f3,text='Show',variable=self.PLOT.BLUEMARBLE).grid(row=3,column=1,padx=3)
    ttk.Label(f3,text='Bluemarble background',width=25).grid(row=3,column=2,columnspan=3,padx=3)
    ttk.Label(f3,text='Etopo').grid(row=4,column=0,padx=3,sticky='w')
    ttk.Checkbutton(f3,text='Show',variable=self.PLOT.ETOPO).grid(row=4,column=1,padx=3)
    ttk.Label(f3,text='Etopo background',width=25).grid(row=4,column=2,columnspan=3,padx=3)
    ttk.Label(f3,text='Arcgisimage').grid(row=5,column=0,padx=3,sticky='w')
    ttk.Checkbutton(f3,text='Show',variable=self.PLOT.ARCGISIMAGE).grid(row=5,column=1,padx=3)
    ttk.Label(f3,text='ArcGis Image background',width=25).grid(row=5,column=2,columnspan=3,padx=3)
    ttk.Label(f3,text='Service').grid(row=6,column=1,padx=3,sticky='w')
    ttk.Combobox(f3,textvariable=self.PLOT.ARCGISSERVICE,width=20, \
                 values=self.PLOT.ARCGISSERVICE_LIST).grid(row=6,column=2,columnspan=2)
    ttk.Label(f3,text='DPI').grid(row=7,column=1,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.ARCGISDPI,justify='left',width=7). \
              grid(row=7,column=2,padx=3,sticky='we')
    ttk.Label(f3,text='Pixels').grid(row=8,column=1,padx=3,sticky='w')
    ttk.Entry(f3,textvariable=self.PLOT.ARCGISPIXELS,justify='left',width=7). \
              grid(row=8,column=2,padx=3,sticky='we')
    ttk.Checkbutton(f3,text='Verbose',variable=self.PLOT.ARCGISVERBOSE).grid(row=9,column=1,padx=3)
    f3.grid(row=13,column=0,columnspan=5)

    f4 = ttk.Frame(self.Window_mapconfig,borderwidth=5,padding=5,relief='sunken')
    ttk.Label(f4,text='Isobaths (meters)', \
              font='Helvetica 12 bold').grid(row=0,column=0,\
                                             columnspan=7,padx=3,sticky='we')
    self.w = []
    for i in range(self.PLOT.nisobat):
      self.w.append(tk.Checkbutton(f4,text=str(self.PLOT.ISOBATHS[i]), \
                    variable=self.PLOT.wvar[i], \
                    command=get_isobaths,justify='right'))
    ii = 0
    jj = 1
    for i in range(self.PLOT.nisobat):
      self.w[i].grid(row=jj,column=ii,sticky='w')
      ii += 1
      if ii > 6:
        ii = 0
        jj += 1

    ttk.Label(f4,text='Width',justify='right').grid(row=4,column=0,padx=3,sticky='e')
    ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_WIDTH, \
                    justify='left',width=7)  
    ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_WIDTH, \
              justify='left',width=7).grid(row=4,column=1,\
              padx=3,sticky='we')
    ttk.Label(f4,text='Color').grid(row=4,column=2,padx=3,sticky='e')
    ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_COLOR,justify='left', \
              width=7).grid(row=4,column=3,padx=3,sticky='we')
    ttk.Button(f4,text='Select',command=icselection). \
        grid(row=4,column=4,padx=3,sticky='ew')
    ttk.Button(f4,text='Load isobaths',command=iload). \
        grid(row=4,column=5,columnspan=2,padx=3,sticky='ew')
    ttk.Label(f4,text='Path',justify='right').grid(row=5,column=0)
    ttk.Entry(f4,textvariable=self.PLOT.ISOBAT_PATH, \
            justify='left',width=50).grid(row=5,column=1,columnspan=5,padx=3,pady=5)
    ttk.Button(f4,text='Select',command=_pselect).grid(row=5,column=6)
    f4.grid(row=18,column=0,columnspan=7)

    frame5 = ttk.Frame(self.Window_mapconfig,borderwidth=5,padding=5)
    ttk.Button(frame5,text='Cancel',command=_cancel).grid(row=0,column=4,padx=3)
    ttk.Button(frame5,text='Apply',command=_apply).grid(row=0,column=5,padx=3)
    ttk.Button(frame5,text='Done',command=_done).grid(row=0,column=6,padx=3)
    frame5.grid(row=24,column=0,columnspan=5)


  # =====================
  def clm(self):
  # =====================
    '''Options to launch the COSMO Lagrangian Model'''

    def _close():
    # ===========
      self.Window_clm.destroy()
      self.Window_clm = None

    def _run(options):
    # ================

      command = self.CLM.PATH.get() + \
                self.CLM.BIN.get()

      command += options
      print(command)
      os.system(command)

      if os.path.isfile(self.CLM.TRAJECTORY.get()):
        FLT = lagrangian.Read(self.CLM.TRAJECTORY.get())
        FLT.TIME = np.array([(FLT.date[i].replace(tzinfo=None)-\
                              self.FLD.DATE[0]).total_seconds() \
                              for i in range(FLT.nrecords)])
        FLT.MAPX = []
        FLT.MAPY = []
        if FLT.nfloats > 1:
          for i in range(FLT.nfloats):
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon[:,i]), bounds_error=False, fill_value=np.NaN)
            FLT.MAPX.append(list(f(self.FLD.TIME)))
            f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat[:,i]), bounds_error=False, fill_value=np.NaN)
            FLT.MAPY.append(list(f(self.FLD.TIME)))
        else:
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lon), bounds_error=False, fill_value=np.NaN)
          FLT.MAPX = list(f(self.FLD.TIME))
          f = interpolate.interp1d(FLT.TIME,np.array(FLT.lat), bounds_error=False, fill_value=np.NaN)
          FLT.MAPY = list(f(self.FLD.TIME))

        self.nfloat += 1
        self.FLOAT.append(FLT)
        self.FLOAT_INDX.set(self.nfloat-1)
        self.FLOAT_LIST = list(range(self.nfloat))
        self.make_plot()
      else:
        messagebox.showinfo(message='COSMO Lagrangian Model failed')


    def _help():
    # ==========
      options = '--help'
      _run(options)
  
    def _run_single():
    # ================

      options = clm.Basic_options(self.CLM)
      if empty(options):
        return

      if self.CLM.INI_USE.get():
        pass
      else:
        if self.CLM.reverse.get():
          try:
            aa = ' -to %s' % 0.0
            options += aa
          except:
            pass
        else:
          if self.CLM.to_use.get():
            try:
              aa = ' -to %s' % self.CLM.to.get()
              options += aa
            except:
              messagebox.showinfo('Invalid release time')
              return
          else:
            if empty(self.CLM.do.get()):
              messagebox.showinfo('Invalid release date')
              return
            else:
              aa = ' -do %s' % self.CLM.do.get()
              options += aa

      if self.CLM.record_use.get():
        try:
          aa = ' -record %s' % self.CLM.record.get()
          options += aa
        except:
          messagebox.showinfo('Invalid simulation starting record')
          return
      
      if self.CLM.reverse.get():
        options += ' -reverse'

      _run(options)

    def _run_ensemble():
    # ==================

      options = clm.Basic_options(self.CLM)

      if self.CLM.reverse.get():

        options += ' -reverse'

        if self.CLM.INI_USE.get():
          pass
        else:
          try:
            aa = ' -to %s' % 0.0
            options += aa
          except:
            pass

      else:

        if self.CLM.INI_USE.get():
          pass
        else:
          if self.CLM.to_use.get():
            try:
              aa = ' -to %s' % self.CLM.to.get()
              options += aa
            except:
              messagebox.showinfo('Invalid release time')
              return
          else:
             if empty(self.CLM.do.get()):
               messagebox.showinfo('Invalid release date')
               return
             else:
               aa = ' -do %s' % self.CLM.do.get()
               options += aa


      if self.CLM.record_use.get():
        try:
          aa = ' -record %s' % self.CLM.record.get()
          options += aa
        except:
          messagebox.showinfo('Invalid simulation starting record')
          return
      
      try:
        aa = ' -seed %s' % self.CLM.seed.get()
        options += aa
      except:
        pass

      try:
        aa = ' -nfloats %s' % self.CLM.nfloats.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Rx %s' % self.CLM.Rx.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Ry %s' % self.CLM.Ry.get()
        options += aa
      except:
        pass

      try:
        aa = ' -Rt %s' % self.CLM.Rt.get()
        options += aa
      except:
        pass


      _run(options)


  
    # Main CLM Window
    if self.Window_clm is not None:
      self.Window_clm.lift()
      return

    string = '{}'.format(num2date(self.FLD.T_LIST[self.FLD.L.get()], \
                                 units=self.FLD.icdf.time_units, \
                                 calendar=self.FLD.icdf.time_calendar))
    self.CLM.do.set(string.replace(' ','T'))
    self.CLM.to.set(self.FLD.TIME[self.FLD.L.get()])
    self.CLM.record.set(self.FLD.L.get()+1)

    self.Window_clm = tk.Toplevel(self.master)
    self.Window_clm.title('COSMO Lagrangian Model options')
    self.Window_clm.resizable(width=True,height=True)
    self.Window_clm.protocol('WM_DELETE_WINDOW',_close)

    clm.WinConfig(self.Window_clm,self.CLM)

    F0 = ttk.Frame(self.Window_clm,padding=5)
    ttk.Checkbutton(F0,text='Reverse Run',variable=self.CLM.reverse). \
        grid(row=0,column=1,padx=5)
    ttk.Button(F0,text='Run Single',command=_run_single).grid(row=0,column=2,padx=5)
    ttk.Button(F0,text='Run Ensemble',command=_run_ensemble).grid(row=0,column=3,padx=5)
    ttk.Button(F0,text='Run Help',command=_help).grid(row=0,column=4,padx=5)
    F0.grid()


  # ==================
  def wdpi(self):
  # ==================
    ''' Launch the DPI script '''

    # -----------
    def _close():
    # -----------
      global DPI
      DPI = self.PLOT.OUT_DPI.get()
      self.Window_dpi.destroy()
      self.Window_dpi = None

    # -----------
    def _cancel():
    # -----------
      global DPI
      self.PLOT.OUT_DPI.set(DPI)
      self.Window_dpi.destroy()
      self.Window_dpi = None

    if self.Window_dpi is None:
      self.Window_dpi = tk.Toplevel(self.master)
      self.Window_dpi.title('Output DPI')
      self.Window_dpi.resizable(width=True,height=True)
      self.Window_dpi.protocol('WM_DELETE_WINDOW',_close)
      F0 = ttk.Frame(self.Window_dpi,borderwidth=5,padding=5)
      ttk.Label(F0,text='Dots per inch : ').grid(row=0,column=0)
      ttk.Entry(F0,textvariable=self.PLOT.OUT_DPI,width=10).grid(row=0,column=1)
      cancel = ttk.Button(F0,text='Cancel',command=_cancel)
      cancel.grid(row=0,column=2,padx=3)
      cancel.bind("<Return>",lambda e:_cancel())
      done = ttk.Button(F0,text='Done',command=_close)
      done.grid(row=0,column=3,padx=3)
      done.bind("<Return>",lambda e:_close())
      F0.grid()
    else:
      self.Window_dpi.lift()
 

  # ==================
  def make_anim(self):
  # ==================
    ''' Launch the matplotlib animation'''

    # -----------
    def _close():
    # -----------
      self.Window_anim.destroy()
      self.Window_anim = None

    def _done():
    # ----------

      import matplotlib.animation as manimation
      FFMpegWriter = manimation.writers['ffmpeg']
      metadata = dict(title=self.VIDEO_TITLE.get(), artist=self.VIDEO_AUTHOR.get(), \
                      comment=self.VIDEO_COMMENT.get())
      writer = FFMpegWriter(fps=self.VIDEO_FPS.get(),metadata=metadata)

      with writer.saving(self.fig,self.VIDEO_NAME.get(),self.VIDEO_DPI.get()):
        for L in range(self.VIDEO_L1.get(),self.VIDEO_L2.get()+1):
          self.FLD.L.set(L)
          string = '{}'.format(num2date(self.FLD.T_LIST[L], \
                                        units=self.FLD.icdf.time_units, \
                                        calendar=self.FLD.icdf.time_calendar))
          self.PLOT.TLABEL.set(string)
          self.read_UV(self.FLD.ncid,self.FLD.icdf,self.FLD.uid,self.FLD.vid)
          self.read_S(self.FLD.ncid,self.FLD.icdf,self.FLD.sid)
          self.make_plot()
          writer.grab_frame()
      messagebox.showinfo(self.Window_anim,message='Movie has been saved')

    # Main
    # ----
    if self.Window_anim is None:
      self.Window_anim = tk.Toplevel(self.master)
      self.Window_anim.title('Animation creation')
      self.Window_anim.resizable(width=True,height=True)
      self.Window_anim.protocol('WM_DELETE_WINDOW',_close)
      F0 = ttk.Frame(self.Window_anim,borderwidth=5,padding=5)
      ttk.Label(F0,text='Output filename : ').grid(row=0,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_NAME,width=40).grid(row=0,column=1,columnspan=4,sticky='w')
      ttk.Label(F0,text='Video title : ').grid(row=1,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_TITLE,width=40).grid(row=1,column=1,columnspan=4,sticky='w')
      ttk.Label(F0,text='Author : ').grid(row=2,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_AUTHOR,width=40).grid(row=2,column=1,columnspan=4,sticky='w')
      ttk.Label(F0,text='Comment : ').grid(row=3,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_COMMENT,width=40).grid(row=3,column=1,columnspan=4,sticky='w')
      ttk.Label(F0,text='Initial frame : ').grid(row=4,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_L1,width=7).grid(row=4,column=1,sticky='w')
      ttk.Label(F0,text='Final frame : ').grid(row=5,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_L2,width=7).grid(row=5,column=1,sticky='w')
      ttk.Label(F0,text='FPS : ').grid(row=6,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_FPS,width=7).grid(row=6,column=1,sticky='w')
      ttk.Label(F0,text='DPI : ').grid(row=7,column=0)
      ttk.Entry(F0,textvariable=self.VIDEO_DPI,width=7).grid(row=7,column=1,sticky='w')
      cancel = ttk.Button(F0,text='Cancel',command=_close)
      cancel.grid(row=8,column=2,padx=3)
      cancel.bind("<Return>",lambda e:_close())
      done = ttk.Button(F0,text='Done',command=_done)
      done.grid(row=8,column=3,padx=3)
      done.bind("<Return>",lambda e:_done())
      close = ttk.Button(F0,text='Close',command=_close)
      close.grid(row=8,column=4,padx=3)
      close.bind("<Return>",lambda e:_close())
      F0.grid()
    else:
      self.Window_anim.lift()

# =========
def main():
# =========

  root = tk.Tk()
  root.title('DRAW CURRENTS')
  root.grid_rowconfigure(0,weight=1)
  root.grid_columnconfigure(0,weight=1)
  root.protocol('WM_DELETE_WINDOW',quit)


  PLOT = plot_params()
  PLOT.SOUTH = 38
  PLOT.NORTH = 44
  PLOT.WEST  = -1
  PLOT.EAST  = 9

  ifile = 'SAMGIB-PdE-dm-2017122600-2017122823-B2017122600-FC.nc'
  ifile = 'roms_wmop_20171121.nc'
  ifile = 'roms_wmop_20171122.nc'
  ifile = 'SAMGIB-PdE-hm-2018011800-2018012023-B2018011800-FC.nc'
  ncid  = Dataset(ifile,'r')
  icdf  = geocdf(ifile)
  uid = icdf.vname.index('u')
  vid = icdf.vname.index('v')
  FLD = cosmo_view_field(ifile,ncid,icdf,uid,vid)
  WinDrawPlot(root,FLD,100)
  root.mainloop()

if __name__ == '__main__':
  main()
