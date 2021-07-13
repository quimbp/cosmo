''' Module for the applications built for the COSMO project 
    Quim Ballabrera, May 2017
    EGL, 06/2020:
     small adjustments of text fonts
     A heap variable MESSAGE has been introduce to store "print" messages
'''

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog
from tkinter import font as tkfont

import json
import os
import io
import datetime

try:
  to_unicode = unicode
except:
  to_unicode = str

from cosmo.tools import empty
from cosmo.tools import exists
from cosmo import COSMO_ROOT
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA

# ===================
class parameters:
# ===================
  ''' Class whose attributes will contain the options for running
      the COSMO Lagrangian Model'''

  __version__ = "3.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "April 2021"

  def __init__(self):

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF   = os.path.join(COSMO_CONF,'clm.conf')

    self.PATH       = tk.StringVar()
    self.BIN        = tk.StringVar()

    self.VEC        = []
    self.UINDEX     = 0
    self.VINDEX     = 0
    self.UFILE      = tk.StringVar()
    self.Ux         = tk.StringVar()
    self.Uy         = tk.StringVar()
    self.Uz         = tk.StringVar()
    self.Ut         = tk.StringVar()
    self.Uu         = tk.StringVar()

    self.VFILE      = tk.StringVar()
    self.Vx         = tk.StringVar()
    self.Vy         = tk.StringVar()
    self.Vz         = tk.StringVar()
    self.Vt         = tk.StringVar()
    self.Vv         = tk.StringVar()

    self.INI_USE    = tk.BooleanVar()
    self.INI        = tk.StringVar()
    self.FIN        = tk.StringVar()
    self.TRAJECTORY = tk.StringVar()
    self.SFILE      = tk.StringVar()
    self.INI_USE.set(False)

    self.xo         = tk.DoubleVar()
    self.yo         = tk.DoubleVar()
    self.zo         = tk.DoubleVar()
    self.to         = tk.DoubleVar()
    self.do         = tk.StringVar()
    self.to_use     = tk.BooleanVar()

    self.reverse    = tk.BooleanVar()
    self.crop       = tk.BooleanVar()
    self.west       = tk.DoubleVar()
    self.east       = tk.DoubleVar()
    self.south      = tk.DoubleVar()
    self.north      = tk.DoubleVar()
    self.Lini       = tk.IntVar()
    self.Tini       = tk.DoubleVar()
    self.Dini       = tk.StringVar()
    self.Duration   = tk.StringVar()
    self.dt         = tk.IntVar()
    self.script     = tk.BooleanVar()

    self.alpha      = tk.DoubleVar()
    self.mu         = tk.DoubleVar()
    self.va         = tk.DoubleVar()
    self.nfloats    = tk.IntVar()
    self.Rx         = tk.DoubleVar()
    self.Ry         = tk.DoubleVar()


    # Default "Manufacturer" parameters:
    #
    self.PATH.set(os.path.join(COSMO_ROOT,'bin/'))
    self.BIN.set('clm')
    self.TRAJECTORY.set('clm.nc')
    self.INI.set('')
    self.FIN.set('final.dat')
    self.SFILE.set('simulation.clm')

    self.xo.set(None)
    self.yo.set(None)
    self.zo.set(0.0)
    self.to.set(0.0)
    self.do.set('')
    self.crop.set(False)
    self.Lini.set(2)
    self.Duration.set(7)
    self.dt.set(600)
    self.to_use.set(True)
    self.script.set(False)

    self.alpha.set(1.0)
    self.mu.set(0.0)
    self.va.set(0.0)
    self.nfloats.set(10)
    self.Rx.set(0.02)
    self.Ry.set(0.02)

    self.MESSAGE = "\n"
   
    if exists(self.FILECONF):
      self.MESSAGE += 'Reading CLM configuration'
      self.load(self.FILECONF)
    else:
      self.MESSAGE += 'Saving CLM configuration'
      self.save(self.FILECONF)

  def load(self,filename):
  # ======================
    '''Read configuration values from file'''

    with open(filename) as infile:
      conf = json.load(infile)

    self.PATH.set(conf['PATH'])
    self.BIN.set(conf['BIN'])
    self.TRAJECTORY.set(conf['TRAJECTORY'])
    self.INI_USE.set(conf['INI_USE'])
    self.INI.set(conf['INI'])
    self.FIN.set(conf['FIN'])
    self.SFILE.set(conf['SFILE'])
    self.alpha.set(conf['ALPHA'])
    self.mu.set(conf['MU'])
    self.va.set(conf['VA'])
    self.nfloats.set(conf['NFLOATS'])
    self.Rx.set(conf['RX'])
    self.Ry.set(conf['RY'])
    self.dt.set(conf['DT'])

  def save(self,filename):
  # ======================
    conf = {}
    conf['PATH'] = self.PATH.get()
    conf['BIN'] = self.BIN.get()
    conf['TRAJECTORY'] = self.TRAJECTORY.get()
    conf['INI_USE'] = self.INI_USE.get()
    conf['INI'] = self.INI.get()
    conf['FIN'] = self.FIN.get()
    conf['SFILE'] = self.SFILE.get()
    conf['ALPHA'] = self.alpha.get()
    conf['MU'] = self.mu.get()
    conf['VA'] = self.va.get()
    conf['NFLOATS'] = self.nfloats.get()
    conf['RX'] = self.Rx.get()
    conf['RY'] = self.Ry.get()
    conf['DT'] = self.dt.get()
    with io.open(filename,'w',encoding='utf8') as outfile:
      str_ = json.dumps(conf,ensure_ascii=False,    \
                             sort_keys=True,        \
                             indent=2,              \
                             separators=(',',': '))
      outfile.write(to_unicode(str_))

# =============
def inout(CLM):
# =============
  options  = ''
  return options

# =====================
def Basic_options(CLM):
# =====================
  options  = ' -OU' + ' file=%s' % CLM.UFILE.get() \
                   + ' x=%s' % CLM.Ux.get()       \
                   + ' y=%s' % CLM.Uy.get()
  if empty(CLM.Uz.get()):
    pass
  else:
    options += ' z=%s' % CLM.Uz.get()

  if empty(CLM.Ut.get()):
    pass
  else:
    options += ' t=%s' % CLM.Ut.get()

  options += ' var=%s' % CLM.Uu.get()

  options += ' -OV'
  if empty(CLM.VFILE.get()):
    pass
  else:
    options += ' file=%s' % CLM.VFILE.get()

  if empty(CLM.Vx.get()):
    pass
  else:
    options += ' x=%s' % CLM.Vx.get()

  if empty(CLM.Vy.get()):
    pass
  else:
    options += ' y=%s' % CLM.Vy.get()

  if empty(CLM.Vz.get()):
    pass
  else:
    options += ' z=%s' % CLM.Vz.get()

  if empty(CLM.Vt.get()):
    pass
  else:
    options += ' t=%s' % CLM.Vt.get()

  options += ' var=%s' % CLM.Vv.get()

  if empty(CLM.TRAJECTORY.get()):
    pass
  else:
    options += ' -trajectory %s' % CLM.TRAJECTORY.get()

  if empty(CLM.FIN.get()):
    pass
  else:
    options += ' -final %s' % CLM.FIN.get()
                  
  if CLM.crop.get():
    try:
      aa = ' -xmin %s' % CLM.west.get()
      options += aa
    except:
      pass
    try:
      aa = ' -xmax %s' % CLM.east.get()
      options += aa
    except:
      pass
    try:
      aa = ' -ymin %s' % CLM.south.get()
      options += aa
    except:
      pass
    try:
      aa = ' -ymax %s' % CLM.north.get()
      options += aa
    except:
      pass

  if empty(CLM.Dini.get()):
    pass
  else:
    d = '%s' % CLM.Dini.get()
    d = d.replace(' ','T')
    aa = ' -from %s' % d
    options += aa

  try:
    aa = ' -for %s' % CLM.Duration.get()
    options += aa
  except:
    pass
    
  if CLM.INI_USE.get():
    if empty(CLM.INI.get()):
      messagebox.showinfo(message='No release file has been selected')
      return ''
    else:
      aa = ' -release %s' % CLM.INI.get()
      options += aa
  else:
    try: 
      aa = ' -xo %s' % CLM.xo.get()
      options += aa
    except:
      messagebox.showinfo(message='Invalid release longitude')
      return ''
    try: 
      aa = ' -yo %s' % CLM.yo.get()
      options += aa
    except:
      messagebox.showinfo(message='Invalid release latitude')
      return ''
    try: 
      aa = ' -zo %s' % CLM.zo.get()
      options += aa
    except:
      messagebox.showinfo(message='Invalid release depth')
      return ''
    try: 
      aa = ' -to %s' % CLM.to.get()
      options += aa
    except:
      messagebox.showinfo(message='Invalid release time')
      return ''

  try: 
    aa = ' -dt %s' % CLM.dt.get()
    options += aa
  except:
    pass

  if CLM.reverse.get():
    options += ' -reverse'

  try: 
    aa = ' -alpha %s' % CLM.alpha.get()
    options += aa
  except:
    pass

  try: 
    aa = ' -mu %s' % CLM.mu.get()
    options += aa
  except:
    pass

  try: 
    aa = ' -va %s' % CLM.va.get()
    options += aa
  except:
    pass


  return options

# ==============
class WinConfig:
# ==============

   def __init__(self,master,CLM,TIME,DATE):

     self.ULIST = [CLM.VEC[i].UFILENAME.get() for i in range(len(CLM.VEC))]
     self.ULIST.append('')
     self.VLIST = [CLM.VEC[i].VFILENAME.get() for i in range(len(CLM.VEC))]
     self.VLIST.append('')

     DI = datetime.datetime.strptime(CLM.Dini.get(),'%Y-%m-%d %H:%M:%S')
     CLM.do.set(DI+datetime.timedelta(seconds=CLM.to.get()))

     def switch_mode():
       if CLM.INI_USE.get():
         self.wxo.configure(state='disabled')
         self.wyo.configure(state='disabled')
         self.wzo.configure(state='disabled')
         self.wto.configure(state='disabled')
         self.wdo.configure(state='disabled')
       else:
         self.wxo.configure(state='!disabled')
         self.wyo.configure(state='!disabled')
         self.wzo.configure(state='!disabled')
         if CLM.to_use.get():
           self.wto.configure(state='!disabled')
           self.wdo.configure(state='disabled')
         else:
           self.wto.configure(state='disabled')
           self.wdo.configure(state='!disabled')

     def open_traj():
       nn = tk.filedialog.asksaveasfilename(title='Save', \
                                           filetypes=[('Netcdf','*.nc')], \
                                           confirmoverwrite=True)
       if len(nn) == 0:
         pass
       else:
         CLM.TRAJECTORY.set(nn)

     def open_init():
       nn = filedialog.askopenfile()
       if nn is None:
         pass
       else:
         CLM.INI.set(nn.name)

     def select_to():
       DI = datetime.datetime.strptime(CLM.Dini.get(),'%Y-%m-%d %H:%M:%S')
       CLM.do.set(DI+datetime.timedelta(seconds=CLM.to.get()))

     def select_do():
       DI = datetime.datetime.strptime(CLM.Dini.get(),'%Y-%m-%d %H:%M:%S')
       DO = datetime.datetime.strptime(CLM.do.get(),'%Y-%m-%d %H:%M:%S')
       dD = DO - DI
       to = dD.total_seconds()
       if to < 0:
         to = 0
       CLM.to.set(to)

     def crop():
       if CLM.crop.get():
         self.wcw.configure(state='!disabled')
         self.wce.configure(state='!disabled')
         self.wcs.configure(state='!disabled')
         self.wcn.configure(state='!disabled')
       else:
         self.wcw.configure(state='disabled')
         self.wce.configure(state='disabled')
         self.wcs.configure(state='disabled')
         self.wcn.configure(state='disabled')

     def select_ufile():
       CLM.UINDEX = self.ULIST.index(CLM.UFILE.get())
       uid = CLM.VEC[CLM.UINDEX].U.varid
       CLM.Ux.set(CLM.VEC[CLM.UINDEX].U.icdf.xname)
       CLM.Uy.set(CLM.VEC[CLM.UINDEX].U.icdf.yname)
       CLM.Uz.set(CLM.VEC[CLM.UINDEX].U.icdf.zname)
       CLM.Ut.set(CLM.VEC[CLM.UINDEX].U.icdf.tname)
       CLM.Uu.set(CLM.VEC[CLM.UINDEX].U.icdf.vname[uid])
       if CLM.VINDEX is None:
         vid = CLM.VEC[CLM.UINDEX].V.varid
         CLM.Vv.set(CLM.VEC[CLM.UINDEX].V.icdf.vname[vid])
       else:
         vid = CLM.VEC[CLM.VINDEX].V.varid
         CLM.Vv.set(CLM.VEC[CLM.VINDEX].V.icdf.vname[vid])

     def select_vfile():
       if empty(CLM.VFILE.get()):
         CLM.VINDEX = None
         CLM.Vx.set('')
         CLM.Vy.set('')
         CLM.Vz.set('')
         CLM.Vt.set('')
         vid = CLM.VEC[CLM.UINDEX].V.varid
         CLM.Vv.set(CLM.VEC[CLM.UINDEX].V.icdf.vname[vid])
       else:
         CLM.VINDEX = self.VLIST.index(CLM.VFILE.get())
         vid = CLM.VEC[CLM.VINDEX].V.varid
         CLM.Vx.set(CLM.VEC[CLM.VINDEX].V.icdf.xname)
         CLM.Vy.set(CLM.VEC[CLM.VINDEX].V.icdf.yname)
         CLM.Vz.set(CLM.VEC[CLM.VINDEX].V.icdf.zname)
         CLM.Vt.set(CLM.VEC[CLM.VINDEX].V.icdf.tname)
         CLM.Vv.set(CLM.VEC[CLM.VINDEX].V.icdf.vname[vid])

     def loadconf():
       print('Loading default configuration')
       CLM.load(CLM.FILECONF)


     def saveconf():
       print('Saving default configuration')
       CLM.save(CLM.FILECONF)

     def record_set():
       L = CLM.Lini.get()
       CLM.Tini.set(TIME[L])
       CLM.Dini.set(DATE[L])
       _wle.configure(foreground='black')
       CLM.do.set(DATE[L]+datetime.timedelta(seconds=CLM.to.get()))

     def date_set():
       D = CLM.Dini.get()
       DD = datetime.datetime.strptime(CLM.Dini.get(),'%Y-%m-%d %H:%M:%S')
       i = 0
       while DATE[i] < DD:
         i = i + 1

       if DATE[i] > DD:
         i = i - 1
         _wle.configure(foreground='grey')
       else:
         _wle.configure(foreground='black')

       CLM.Lini.set(i)
       CLM.do.set(DD+datetime.timedelta(seconds=CLM.to.get()))


     menubar = tk.Menu(master)
     menu = tk.Menu(menubar,tearoff=0)
     menubar.add_cascade(label='Configuration',menu=menu)
     menu.add_command(label='Restore',command=loadconf)
     menu.add_command(label='Save',command=saveconf)
     try:
       master.config(menu=menubar)
     except AttributeError:
       # master is a toplevel window (Python 2.4/Tkinter 1.63)
       master.tk.call(master, "config", "-menu", menubar)

     font_bold = tkfont.Font(font='TkDefaultFont').copy()
     font_bold['weight']='bold'

     F0 = ttk.Frame(master,padding=5)
     ttk.Label(F0,text='PATH',font=font_bold).grid(row=0,column=0,padx=3,pady=3)
     ttk.Entry(F0,textvariable=CLM.PATH,width=80).grid(row=0,column=1,columnspan=8)
     ttk.Label(F0,text='BIN',font=font_bold).grid(row=1,column=0,padx=3,pady=3)
     ttk.Entry(F0,textvariable=CLM.BIN,width=80).grid(row=1,column=1,columnspan=8)
     F0.grid()

     # Define tabs:
     self.nb = ttk.Notebook(master)
     self.page1 = ttk.Frame(self.nb)
     self.page2 = ttk.Frame(self.nb)
     self.page3 = ttk.Frame(self.nb)
     self.page4 = ttk.Frame(self.nb)
     self.page5 = ttk.Frame(self.nb)
     self.page6 = ttk.Frame(self.nb)

     self.nb.add(self.page1,text='Zonal Velocity')
     self.nb.add(self.page2,text='Meridional Velocity')
     self.nb.add(self.page3,text='Domain simulation')
     self.nb.add(self.page4,text='Input/Output files')
     self.nb.add(self.page5,text='Simulation period')
     self.nb.add(self.page6,text='Stochastic model')

     self.nb.grid()

     # Initialize the CLM files and variables:
     # AAA

     CLM.UFILE.set(self.ULIST[CLM.UINDEX])
     uid = CLM.VEC[CLM.UINDEX].U.varid

     CLM.VFILE.set(self.VLIST[CLM.VINDEX])
     vid = CLM.VEC[CLM.VINDEX].V.varid

     CLM.Ux.set(CLM.VEC[CLM.UINDEX].U.icdf.xname)
     CLM.Uy.set(CLM.VEC[CLM.UINDEX].U.icdf.yname)
     CLM.Uz.set(CLM.VEC[CLM.UINDEX].U.icdf.zname)
     CLM.Ut.set(CLM.VEC[CLM.UINDEX].U.icdf.tname)
     CLM.Uu.set(CLM.VEC[CLM.UINDEX].U.icdf.vname[uid])

     CLM.Vx.set(CLM.VEC[CLM.VINDEX].V.icdf.xname)
     CLM.Vy.set(CLM.VEC[CLM.VINDEX].V.icdf.yname)
     CLM.Vz.set(CLM.VEC[CLM.VINDEX].V.icdf.zname)
     CLM.Vt.set(CLM.VEC[CLM.VINDEX].V.icdf.tname)
     CLM.Vv.set(CLM.VEC[CLM.VINDEX].V.icdf.vname[vid])

#     if CLM.VINDEX is None:
#       vid = CLM.VEC[CLM.UINDEX].V.varid
#       CLM.Vv.set(CLM.VEC[CLM.UINDEX].V.icdf.vname[vid])
#     else:
#       vid = CLM.VEC[CLM.VINDEX].V.varid
#       CLM.Vv.set(CLM.VEC[CLM.VINDEX].V.icdf.vname[vid])

     # The main window
     #
     F1 = ttk.Frame(self.page1,padding=5)
     ttk.Label(F1,text='Ocean Zonal velocity file -OU',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Label(F1,text='file =')    \
        .grid(row=1,column=0,padx=3,sticky='e')
     ubox = ttk.Combobox(F1,textvariable=CLM.UFILE,width=80, \
                            values=self.ULIST)
     ubox.grid(row=1,column=1,columnspan=8)
     ubox.bind('<<ComboboxSelected>>',lambda e: select_ufile())

     ttk.Label(F1,text='x =')    \
        .grid(row=2,column=0,padx=3,sticky='e')
     ttk.Entry(F1,textvariable=CLM.Ux,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F1,text='y =')    \
        .grid(row=3,column=0,padx=3,sticky='e')
     ttk.Entry(F1,textvariable=CLM.Uy,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F1,text='z =')    \
        .grid(row=4,column=0,padx=3,sticky='e')
     ttk.Entry(F1,textvariable=CLM.Uz,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F1,text='t =')    \
        .grid(row=5,column=0,padx=3,sticky='e')
     ttk.Entry(F1,textvariable=CLM.Ut,width=50).grid(row=5,column=1,columnspan=5)
     ttk.Label(F1,text='variable =',width=8,justify='right')    \
        .grid(row=6,column=0,padx=3,sticky='e')
     ttk.Entry(F1,textvariable=CLM.Uu,width=50).grid(row=6,column=1,columnspan=5)
     F1.grid(pady=5)


     F2 = ttk.Frame(self.page2,padding=5)
     ttk.Label(F2,text='Ocean Meridional velocity file -OV',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Label(F2,text='file =') \
        .grid(row=1,column=0,padx=3,sticky='e')
     vbox = ttk.Combobox(F2,textvariable=CLM.VFILE,width=80, \
                                values=self.VLIST)
     vbox.grid(row=1,column=1,columnspan=8)
     vbox.bind('<<ComboboxSelected>>',lambda e: select_vfile())
     ttk.Label(F2,text='x =')    \
        .grid(row=2,column=0,padx=3,sticky='e')
     ttk.Entry(F2,textvariable=CLM.Vx,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F2,text='y =')    \
        .grid(row=3,column=0,padx=3,sticky='e')
     ttk.Entry(F2,textvariable=CLM.Vy,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F2,text='z =')    \
        .grid(row=4,column=0,padx=3,sticky='e')
     ttk.Entry(F2,textvariable=CLM.Vz,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F2,text='t =')    \
        .grid(row=5,column=0,padx=3,sticky='e')
     ttk.Entry(F2,textvariable=CLM.Vt,width=50).grid(row=5,column=1,columnspan=5)
     ttk.Label(F2,text='variable =',width=8,justify='right') \
        .grid(row=6,column=0,padx=3,sticky='e')
     ttk.Entry(F2,textvariable=CLM.Vv,width=50).grid(row=6,column=1,columnspan=5)
     F2.grid(pady=5)

     F4 = ttk.Frame(master,padding=5)
     ttk.Label(F4,text='Initial float position',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Checkbutton(F4,text='Use INIT file (See Input/Output files tab)', \
                     variable=CLM.INI_USE,command=switch_mode).grid(row=1,column=1,padx=3)
     ttk.Label(F4,text='xo =').grid(row=2,column=0,padx=3)
     self.wxo = ttk.Entry(F4,textvariable=CLM.xo,width=30)
     self.wxo.grid(row=2,column=1,columnspan=3)
     ttk.Label(F4,text='yo =').grid(row=3,column=0,padx=3)
     self.wyo = ttk.Entry(F4,textvariable=CLM.yo,width=30)
     self.wyo.grid(row=3,column=1,columnspan=3)
     ttk.Label(F4,text='zo =').grid(row=4,column=0,padx=3)
     self.wzo = ttk.Entry(F4,textvariable=CLM.zo,width=30)
     self.wzo.grid(row=4,column=1,columnspan=3)
     ttk.Label(F4,text='to =').grid(row=5,column=0,padx=3)
     self.wto = ttk.Entry(F4,textvariable=CLM.to,width=30)
     self.wto.grid(row=5,column=1,columnspan=3)
     self.wto.bind("<Return>", lambda f: select_to())
     ttk.Label(F4,text='Seconds since the beginning').grid(row=5,column=4,padx=3,stick='e')
     ttk.Label(F4,text='of the simulation.').grid(row=6,column=4,padx=3,stick='e')
#     ttk.Checkbutton(F4,text='Use initial time',variable=CLM.to_use,command=select_to) \
#        .grid(row=5,column=6,padx=[5,1])
     ttk.Label(F4,text='do =').grid(row=6,column=0,padx=3)
     self.wdo = ttk.Entry(F4,textvariable=CLM.do,width=30)
     self.wdo.grid(row=6,column=1,columnspan=3)
     self.wdo.bind("<Return>", lambda f: select_do())
     F4.grid(pady=5)
#     if CLM.to_use.get():
#       self.wto.configure(state='!disabled')
#       self.wdo.configure(state='disabled')
#     else:
#       self.wto.configure(state='disabled')
#       self.wdo.configure(state='!disabled')

     F3 = ttk.Frame(self.page3,padding=5)
     ttk.Label(F3,text='Domain simulation',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Checkbutton(F3,text='Crop domain simulation',variable=CLM.crop,command=crop) \
        .grid(row=1,column=1,padx=[5,1])
     ttk.Label(F3,text='West =').grid(row=2,column=0,padx=3)
     self.wcw = ttk.Entry(F3,textvariable=CLM.west,width=20)
     self.wcw.grid(row=2,column=1,columnspan=2)
     ttk.Label(F3,text='East =').grid(row=3,column=0,padx=3)
     self.wce = ttk.Entry(F3,textvariable=CLM.east,width=20)
     self.wce.grid(row=3,column=1,columnspan=2)
     ttk.Label(F3,text='South =').grid(row=4,column=0,padx=3)
     self.wcs = ttk.Entry(F3,textvariable=CLM.south,width=20)
     self.wcs.grid(row=4,column=1,columnspan=2)
     ttk.Label(F3,text='North =').grid(row=5,column=0,padx=3)
     self.wcn = ttk.Entry(F3,textvariable=CLM.north,width=20)
     self.wcn.grid(row=5,column=1,columnspan=2)
     F3.grid(pady=5)


     F5 = ttk.Frame(self.page4,padding=5)
     ttk.Label(F5,text='Input/Output files',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Label(F5,text='Trajectory =').grid(row=1,column=0,padx=3)
     ttk.Entry(F5,textvariable=CLM.TRAJECTORY,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Button(F5,text='Select',padding=3,command=open_traj).grid(row=1,column=6,padx=[5,1])
     ttk.Label(F5,text='Initial positions =').grid(row=2,column=0,padx=3)
     ttk.Entry(F5,textvariable=CLM.INI,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Button(F5,text='Select',padding=3,command=open_init).grid(row=2,column=6,padx=[5,1])
     ttk.Label(F5,text='Final positions =').grid(row=3,column=0,padx=3)
     ttk.Entry(F5,textvariable=CLM.FIN,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F5,text='Model options =').grid(row=4,column=0,padx=3)
     ttk.Entry(F5,textvariable=CLM.SFILE,width=50).grid(row=4,column=1,columnspan=5)
     F5.grid(pady=5)

     F6 = ttk.Frame(self.page5,padding=5)
     ttk.Label(F6,text='Simulation period',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Label(F6,text='Record').grid(row=1,column=1,padx=3)
     #ttk.Label(F6,text='Time (julian day)').grid(row=1,column=2,columnspan=2,padx=3)
     ttk.Label(F6,text='Date').grid(row=1,column=2,columnspan=2,padx=3)
     ttk.Label(F6,text='From =').grid(row=2,column=0,padx=3,sticky='e')
     _wle = ttk.Entry(F6,textvariable=CLM.Lini,width=10)
     _wle.grid(row=2,column=1,columnspan=1,padx=3)

     _wle.bind("<Return>", lambda f: record_set())

     #ttk.Entry(F6,textvariable=CLM.Tini,width=20,state='disabled').grid(row=2,column=2,columnspan=2,padx=3)
     _wde = ttk.Entry(F6,textvariable=CLM.Dini,width=20)
     _wde.grid(row=2,column=2,columnspan=2,padx=3)
     _wde.bind("<Return>", lambda f: date_set())

     ttk.Label(F6,text='Duration =').grid(row=3,column=0,padx=3,sticky='e')
     self.wrn = ttk.Entry(F6,textvariable=CLM.Duration,width=10)
     self.wrn.grid(row=3,column=1,columnspan=1)
     ttk.Label(F6,text='days').grid(row=3,column=2,padx=3,sticky='w')
     ttk.Label(F6,text='Time step =').grid(row=4,column=0,padx=3,sticky='e')
     ttk.Entry(F6,textvariable=CLM.dt,width=10).grid(row=4,column=1,columnspan=1,padx=3)
     ttk.Label(F6,text='seconds').grid(row=4,column=2,padx=3,sticky='w')
     ttk.Checkbutton(F6,text='Reverse Run',variable=CLM.reverse). \
         grid(row=5,column=1,padx=10)

     F6.grid(pady=5)

     F7 = ttk.Frame(self.page6,padding=5)
     ttk.Label(F7,text='Stochastic model',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Label(F7,text='Alpha =').grid(row=1,column=0,padx=3,sticky='e')
     ttk.Entry(F7,textvariable=CLM.alpha,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Label(F7,text='Multiplicative noise factor =').grid(row=2,column=0,padx=3,sticky='e')
     ttk.Entry(F7,textvariable=CLM.mu,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F7,text='Additive velocity fluctuation =').grid(row=3,column=0,padx=3,sticky='e')
     ttk.Entry(F7,textvariable=CLM.va,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F7,text='Number of floats =').grid(row=4,column=0,padx=3,sticky='e')
     ttk.Entry(F7,textvariable=CLM.nfloats,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F7,text='X Radius =').grid(row=5,column=0,padx=3,sticky='e')
     ttk.Entry(F7,textvariable=CLM.Rx,width=50).grid(row=5,column=1,columnspan=5)
     ttk.Label(F7,text='Y Radius =').grid(row=6,column=0,padx=3,sticky='e')
     ttk.Entry(F7,textvariable=CLM.Ry,width=50).grid(row=6,column=1,columnspan=5)
     F7.grid(pady=5)

