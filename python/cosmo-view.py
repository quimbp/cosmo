''' COSMO-VIEW
    Quim Ballabrera, May 2017 
    Script for selecting and visualizing model outputs provided
    by various operational systems'''

import numpy as np
import sys
import datetime
from netCDF4 import Dataset
import wget

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkinter import filedialog as filedialog
  from PIL import Image, ImageTk
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  import tkFileDialog as filedialog
  from PIL import Image, ImageTk


import cosmo
import copernicus
from providers import *
from ncdump import *
from showgrid import *
from codar import *
from drawing import *

PROGNAME   = 'COSMO-VIEW'
VERSION    = '0.3 (December 2017)'
AUTHOR     = 'Quim Ballabrera (ICM/CSIC)'

TODAY = datetime.datetime.now().date()

global PROVIDER

# =========
class GUI:
# =========
  '''Main GUI '''

  def __init__(self,root):
    self.master=root
    self.master.protocol('WM_DELETE_WINDOW',self.close)

    self.FILENAME = tk.StringVar()
    self.FILENAME.set('')

    self.dpi = tk.IntVar()
    self.dpi.set(DPI)

    self.ncid            = None
    self.icdf            = None
    self.Window_ncdump   = None
    self.Window_about    = None
    self.Window_codar    = None
    self.Window_showgrid = None
    self.Window_dpi      = None

    menubar = tk.Menu(self.master)
    menu = tk.Menu(menubar,tearoff=0)
    menubar.add_cascade(label='File',menu=menu)
    menu.add_command(label='Operational filename', \
                     command=self.get_opendap_filename)
    menu.add_command(label='CODAR filename', \
                     command=self.get_codar_filename)
    menu.add_command(label='Download Copernicus', \
                     command=self.copernicus)
    menu.add_command(label='Local filename', \
                     command=self.get_local_filename)
    menu.add_separator()
    menu.add_command(label='Quit',command=self.close)

    menu = tk.Menu(menubar, tearoff=0)
    menubar.add_cascade(label='Tools',menu=menu)
    menu.add_command(label='ncdump',command=self.ncdump)
    menu.add_command(label='Show grid',command=self.showgrid)
    menu.add_command(label='Drawing DPI',command=self.wdpi)
#
    menu = tk.Menu(menubar, tearoff=0)
    menubar.add_cascade(label='Help',menu=menu)
    menu.add_command(label='About',command=self.about)

    try:
      self.master.config(menu=menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.master.tk.call(self.master, "config", "-menu", menubar)

    Fr0 = ttk.Frame(self.master,padding=5,width=700)
    ttk.Label(Fr0,text='Filename',padding=5,font='Helvetica 12 bold').grid(row=0,column=0)
    ttk.Entry(Fr0,textvariable=self.FILENAME,justify='left',width=89).grid(row=0,column=1,sticky='ew')
    ttk.Button(Fr0,text='Load',command=self.openfile).grid(row=0,column=5,padx=5,sticky='W'+'E')
    Fr0.grid(row=0,column=0,sticky='ewns')


  # =========================
  def close(self):
  # =========================
    ''' Closing the main widget '''

    if (empty(self.FILENAME.get())):
      sys.exit(0)
    else:
      aa = messagebox.askquestion('Close','Are you sure?',icon='warning')
      if aa == 'yes':
        sys.exit(0)


  # =========================
  def about(self):
  # =========================
    ''' About information '''

    def _close():
      self.Window_about.destroy()
      self.Window_about = None

    if self.Window_about is None:
      self.Window_about = tk.Toplevel(self.master)
      self.Window_about.title('About')
      self.Window_about.resizable(width=True,height=True)
      self.Window_about.protocol('WM_DELETE_WINDOW',_close)

      photoimage = ImageTk.PhotoImage(Image.open('cosmo-logo.png'))

      panel1 = tk.Label(self.Window_about,image=photoimage)
      panel1.grid(row=0,column=0,sticky='we')

      # save the panel's image from 'garbage collection'
      panel1.image = photoimage

      _author = 'Author: Quim Ballabrera (COSMO Project)'
      _description = ' Ocean visualization tool for the COSMO project'
      tk.Label(self.Window_about,text=PROGNAME). \
              grid(row=1,column=0,sticky='ew')
      tk.Label(self.Window_about,text='Version '+VERSION). \
              grid(row=2,column=0,sticky='ew')
      tk.Label(self.Window_about,text=_author) \
              .grid(row=3,column=0,sticky='ew')
      tk.Label(self.Window_about,text=_description). \
              grid(row=4,column=0,sticky='ew')
      tk.Button(self.Window_about,text='Close',command=_close). \
              grid(row=5,column=0,sticky='ew')
    else:
      self.Window_about.lift()


  # =============================
  def get_opendap_filename(self):
  # =============================

    global PROVIDER

    def _close():
      global PROVIDER
      PROVIDER.Window_id.destroy()
      PROVIDER.Window_id = None


    if PROVIDER.Window_id is None:
      PROVIDER.Window_id = tk.Toplevel(self.master)
      PROVIDER.Window_id.title('Load Operational service Opendap file')
      PROVIDER.Window_id.protocol('WM_DELETE_WINDOW',_close)
      WinOpendap(PROVIDER.Window_id,PROVIDER)
      PROVIDER.Window_id.wait_window()
      PROVIDER.Window_id = None
      self.FILENAME.set(PROVIDER.FILENAME.get())
    else:
      PROVIDER.Window_id.lift()


  # =============================
  def copernicus(self):
  # =============================
    ''' Download a Copernicus file'''
    print('copernicus')

    Window_drawing = tk.Toplevel(self.master)
    Window_drawing.title('COPERNICUS')
    Window_drawing.protocol('WM_DELETE_WINDOW',Window_drawing.destroy)
    #Window_drawing.resizable(width=False, height=False)

    image = Image.open('cosmo-logo.png')
    photo = ImageTk.PhotoImage(image)
    Window_drawing.tk.call('wm','iconphoto',Window_drawing._w,photo)
    copernicus.WinTracking(Window_drawing)
    Window_drawing.wait_window()


  # =============================
  def get_codar_filename(self):
  # =============================
    ''' Obtain a HFR CODAR file name'''

    global CODAR_STATION

    def _close():
      global CODAR_STATION
      CODAR_STATION.Window_id.destroy()
      CODAR_STATION.Window_id = None

    if CODAR_STATION.Window_id is None:
      CODAR_STATION.Window_id = tk.Toplevel(self.master)
      CODAR_STATION.Window_id.title("CODAR Station selector")
      CODAR_STATION.Window_id.protocol('WM_DELETE_WINDOW',_close)
      WinCodar(CODAR_STATION.Window_id,CODAR_STATION)
      CODAR_STATION.Window_id.wait_window()
      CODAR_STATION.Window_id = None
      self.FILENAME.set(CODAR_STATION.FILENAME.get())
    else:
      CODAR_STATION.Window_id.lift()


  # ===========================
  def get_local_filename(self):
  # ===========================
    ''' Obtain the name of a local file'''
    try:
      nn = filedialog.askopenfile(filetypes=[('Netcdf','*.nc')])
      self.FILENAME.set(nn.name)
      print(self.FILENAME.get())
    except:
      pass


  # ==================
  def openfile(self):
  # ==================
    ''' Open the input file. Select the axes and variables and draw '''

    # -----------
    def _close():
    # -----------
      Window_axes.destroy()
     

    # -----------
    def _draw():
    # -----------
      if not empty(self.Uname.get()) and not empty(self.Vname.get()):
        uid = self.icdf.vname.index(self.Uname.get())
        vid = self.icdf.vname.index(self.Vname.get())
        print('uid = ',uid)
        print('vid = ',vid)

        FLD = cosmo_view_field(self.FILENAME.get(),self.ncid, \
                               self.icdf,uid,vid)
        Window_drawing = tk.Toplevel(self.master)
        Window_drawing.title('DRAW CURRENTS')
        Window_drawing.protocol('WM_DELETE_WINDOW',Window_drawing.destroy)
        Window_drawing.resizable(width=True, height=True)
        Window_drawing.grid_rowconfigure(0,weight=1)
        Window_drawing.grid_columnconfigure(0,weight=1)

        image = Image.open('cosmo-logo.png')
        photo = ImageTk.PhotoImage(image)
        Window_drawing.tk.call('wm','iconphoto',Window_drawing._w,photo)
        
        WinDrawPlot(Window_drawing,FLD,self.dpi.get())

    try:
      self.ncid = Dataset(self.FILENAME.get())
    except:
      messagebox.showinfo(message='Unable to open file')
      self.ncid = None
      return

    self.icdf = geocdf(self.FILENAME.get())

    Window_axes = tk.Toplevel(self.master)
    Window_axes.title('Axes selection')
    Window_axes.protocol('WM_DELETE_WINDOW',_close)
    axesid = WinGeoaxes(self.icdf,Window_axes)

    self.Uname = tk.StringVar()
    self.Vname = tk.StringVar()

    # --
    Fr2 = ttk.Frame(Window_axes,padding=5,width=700, \
                    borderwidth=5,relief='sunken')

    ttk.Label(Fr2,text='Velocity field: ',width=15, \
            font='bold').grid(row=0,column=0)
    ttk.Label(Fr2,text='Zonal (U)',borderwidth=3,   \
            font='bold').grid(row=0,column=1)
    Ubox = ttk.Combobox(Fr2,textvariable=self.Uname,width=12)
    Ubox.grid(row=0,column=2,sticky='W')
    Ubox['values'] = self.icdf.VAR_MENU

    ttk.Label(Fr2,text='Meridional (V)', \
            font='bold').grid(row=0,column=3,padx=(20,10))
    Vbox = ttk.Combobox(Fr2,textvariable=self.Vname,width=12)
    Vbox.grid(row=0,column=4,sticky='e')
    Vbox['values'] = self.icdf.VAR_MENU

    Fr2.grid(sticky='we')
    # --

    Fr3 = ttk.Frame(Window_axes,padding=5,width=700,borderwidth=5)
    ttk.Button(Fr3,text='Cancel',command=_close).grid(row=0,column=3,sticky='e',padx=10)
    done = ttk.Button(Fr3,text='Done',command=_draw)
    done.grid(row=0,column=4,sticky='e',padx=10)
    done.bind("<Return>",lambda e:_draw())

    Fr3.grid(sticky='we')
   
  # ==================
  def ncdump(self):
  # ==================
    ''' Launch the Ncdump script '''

    # -----------
    def _close():
    # -----------
      self.Window_ncdump.destroy()


    if self.ncid is None:
      messagebox.showinfo(message='Load a file fisrt')
      return

    if self.Window_ncdump is None:
      self.Window_ncdump = tk.Toplevel(self.master)
      self.Window_ncdump.title('ncdump')
      self.Window_ncdump.resizable(width=True,height=True)
      self.Window_ncdump.protocol('WM_DELETE_WINDOW',_close)
      WinNcdump(self.Window_ncdump,self.ncid)
    else:
      self.Window_ncdump.lift()
  

     

  # ==================
  def showgrid(self):
  # ==================
    ''' Launch the Showgrid script '''

    # -----------
    def _close():
    # -----------
      self.Window_showgrid.destroy()


    if self.ncid is None:
      messagebox.showinfo(message='Load a file fisrt')
      return

    if self.Window_showgrid is None:
      self.Window_showgrid = tk.Toplevel(self.master)
      self.Window_showgrid.title('showgrid')
      self.Window_showgrid.resizable(width=True,height=True)
      self.Window_showgrid.protocol('WM_DELETE_WINDOW',_close)
      WinShowgrid(self.Window_showgrid,self.ncid,self.icdf)
    else:
      self.Window_showgrid.lift()

     
  # ==================
  def wdpi(self):
  # ==================
    ''' Launch the DPI script '''

    # -----------
    def _close():
    # -----------
      global DPI
      DPI = self.dpi.get()
      self.Window_dpi.destroy()
      self.Window_dpi = None

    # -----------
    def _cancel():
    # -----------
      global DPI
      self.dpi.set(DPI)
      self.Window_dpi.destroy()
      self.Window_dpi = None

    if self.Window_dpi is None:
      self.Window_dpi = tk.Toplevel(self.master)
      self.Window_dpi.title('Plot DPI')
      self.Window_dpi.resizable(width=True,height=True)
      self.Window_dpi.protocol('WM_DELETE_WINDOW',_close)
      F0 = ttk.Frame(self.Window_dpi,borderwidth=5,padding=5)
      ttk.Label(F0,text='Dots per inch : ').grid(row=0,column=0)
      ttk.Entry(F0,textvariable=self.dpi,width=10).grid(row=0,column=1)
      cancel = ttk.Button(F0,text='Cancel',command=_cancel)
      cancel.grid(row=0,column=2,padx=3)
      cancel.bind("<Return>",lambda e:_cancel())
      done = ttk.Button(F0,text='Done',command=_close)
      done.grid(row=0,column=3,padx=3)
      done.bind("<Return>",lambda e:_close())
      F0.grid()
    else:
      self.Window_dpi.lift()
  

# =========
def main():
# =========
  '''Launch the main GUI'''
  global PROVIDER
  global CODAR_STATION

  print(PROGNAME)
  print(VERSION)
  print(TODAY)

  root = tk.Tk()
  root.title(PROGNAME)
  root.resizable(width=True,height=True)

  image = Image.open('cosmo-logo.png')
  photo = ImageTk.PhotoImage(image)
  root.tk.call('wm','iconphoto',root._w,photo)

  PROVIDER = PROVIDER_CLASS()
  CODAR_STATION = CODAR_CLASS()
  GUI(root)
  root.mainloop()


# =========
def usage():
# =========
  '''Help information'''
  print('Script cosmo-view.py')
  print('Usage: ')
  print('python cosmo-view.py [--help] [DPI]')
  print('Option DPI adapts the size of the GUI. Default (100)')


if __name__ == '__main__':

  global DPI
  DPI = 100
  leave = False

  try:
    option = str(sys.argv[1])
    if option == str('--help'):
      leave = True
      usage()
    else:
      try:
        DPI = int(sys.argv[1])
      except:
        usage()
        leave = True
  except:
    pass

  if leave:
    sys.exit(1)
  else:
    main()
