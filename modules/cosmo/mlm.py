''' Module for the applications built for the COSMO project 
  Quim Ballabrera, May 2017
  EGL, 06/2020 
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
try:
  to_unicode = unicode
except:
  to_unicode = str

from cosmo.tools import empty
from cosmo.tools import exists
from cosmo import COSMO_ROOT
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA

# ===============
class parameters:
# ===================
  ''' Class whose attributes will contain the options for running
      the COSMO M Lagrangian Model'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2018"

  def __init__(self):

    self.MESSAGE = "\n"

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF   = os.path.join(COSMO_CONF,'mlm.conf')

    self.PATH       = tk.StringVar()
    self.BIN        = tk.StringVar()

    self.VEC        = []
    self.UINDEX     = 0
    self.VINDEX     = None
    self.TINDEX     = None
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

    self.TFILE      = tk.StringVar()
    self.Tx         = tk.StringVar()
    self.Ty         = tk.StringVar()
    self.Tz         = tk.StringVar()
    self.Tt         = tk.StringVar()
    self.Tvname     = tk.StringVar()

    self.INI_USE    = tk.BooleanVar()
    self.INI        = tk.StringVar()
    self.FIN        = tk.StringVar()
    self.TRAJECTORY = tk.StringVar()
    self.INI_USE.set(False)

    self.xo         = tk.DoubleVar()
    self.yo         = tk.DoubleVar()
    self.to         = tk.DoubleVar()
    self.time_sim   = tk.DoubleVar()
    #self.do         = tk.StringVar()
    #self.to_use     = tk.BooleanVar()
    #self.to_use.set(False)

    self.reverse    = tk.BooleanVar()
    self.stationary = tk.BooleanVar()
    self.record_use = tk.BooleanVar()
    self.record     = tk.IntVar()
    self.idt        = tk.IntVar()
    #self.edt        = tk.IntVar()
    #self.record_use.set(False)

    self.seed       = tk.IntVar()
    self.nfloats    = tk.IntVar()
    self.Rx         = tk.DoubleVar()
    self.Ry         = tk.DoubleVar()
    self.Rt         = tk.DoubleVar()


    # Default "Manufacturer" parameters:
    #
    self.PATH.set(os.path.join(COSMO_ROOT,'bin/'))
    self.BIN.set('mlm')
    self.TRAJECTORY.set('mlm.nc')
    self.INI.set('')
    self.FIN.set('mlm.end')

    self.xo.set(None)
    self.yo.set(None)
    self.to.set(None)
    #self.do.set('')
    #self.record.set(None)
    self.stationary.set(False)
    #self.edt.set(None)
    self.idt.set(1200)

    self.seed.set(None)
    self.nfloats.set(10)
    self.Rx.set(0.10)
    self.Ry.set(0.10)
    self.Rt.set(0)
    
    if exists(self.FILECONF):
      self.MESSAGE += 'Reading MLM configuration'
      self.load(self.FILECONF)
    else:
      self.MESSAGE += 'Saving MLM configuration'
      self.save(self.FILECONF)
  
  def load(self,filename):
  # ======================
    '''Read configuration values from file'''

    with open(filename) as infile:
      conf = json.load(infile)

    self.PATH.set(conf['PATH'])
    self.BIN.set(conf['BIN'])
    self.TRAJECTORY.set(conf['TRAJECTORY'])
    self.INI.set(conf['INI'])
    self.FIN.set(conf['FIN'])
    self.INI_USE.set(conf['INI_USE'])
    #self.to_use.set(conf['TO_USE'])
    self.seed.set(conf['SEED'])
    self.nfloats.set(conf['NFLOATS'])
    self.Rx.set(conf['RX'])
    self.Ry.set(conf['RY'])
    self.Rt.set(conf['RT'])
    #self.edt.set(conf['EDT'])
    self.idt.set(conf['IDT'])

  def save(self,filename):
  # ======================
    conf = {}
    conf['PATH'] = self.PATH.get()
    conf['BIN'] = self.BIN.get()
    conf['TRAJECTORY'] = self.TRAJECTORY.get()
    conf['INI'] = self.INI.get()
    conf['FIN'] = self.FIN.get()
    conf['INI_USE'] = self.INI_USE.get()
    #conf['TO_USE'] = self.to_use.get()
    try:
      conf['SEED'] = self.seed.get()
    except:
      conf['SEED'] = None
    conf['NFLOATS'] = self.nfloats.get()
    conf['RX'] = self.Rx.get()
    conf['RY'] = self.Ry.get()
    conf['RT'] = self.Rt.get()
    try:
      conf['EDT'] = self.edt.get()
    except:
      conf['EDT'] = None
    conf['IDT'] = self.idt.get()
    with io.open(filename,'w',encoding='utf8') as outfile:
      str_ = json.dumps(conf,ensure_ascii=False,    \
                             sort_keys=True,        \
                             indent=2,              \
                             separators=(',',': '))
      outfile.write(to_unicode(str_)+'\n')

# =============
def inout(CLM):
# =============
  options  = ''
  return options

# =====================
def Basic_options(CLM):
# =====================
  options  = ' -U' + ' file=%s' % CLM.UFILE.get() \
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

  options += ' u=%s' % CLM.Uu.get()

  options += ' -V'
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

  options += ' v=%s' % CLM.Vv.get()

  if empty(CLM.TRAJECTORY.get()):
    pass
  else:
    options += ' -out %s' % CLM.TRAJECTORY.get()

  if empty(CLM.FIN.get()):
    pass
  else:
    options += ' -end  %s' % CLM.FIN.get()
                  
  if CLM.INI_USE.get():
    if empty(CLM.INI.get()):
      messagebox.showinfo(message='No release file has been selected')
      return ''
    else:
      aa = ' -release %s' % CLM.INI.get()
      options += aa
  else:
    try: 
      aa = ' -x0 %s' % CLM.xo.get()
      options += aa
    except:
      messagebox.showinfo(message='Invalid release longitude')
      return ''
    try: 
      aa = ' -y0 %s' % CLM.yo.get()
      options += aa
    except:
      messagebox.showinfo(message='Invalid release latitude')
      return ''

  #try: 
  #  aa = ' -idt %s' % CLM.idt.get()
  #  options += aa
  #except:
  #  pass

  try: 
    aa = ' -edt %s' % CLM.edt.get()
    options += aa
  except:
    pass

  return options

# ==============
class WinConfig:
# ==============

   def __init__(self,master,CLM):

     self.ULIST = [CLM.VEC[i].UFILENAME.get() for i in range(len(CLM.VEC))]
     self.ULIST.append('')

     self.VLIST = [CLM.VEC[i].VFILENAME.get() for i in range(len(CLM.VEC))]
     self.VLIST.append('')

     def switch_mode():
       if CLM.INI_USE.get():
         self.wxo.configure(state='disabled')
         self.wyo.configure(state='disabled')
         self.wto.configure(state='disabled')
       else:
         self.wxo.configure(state='!disabled')
         self.wyo.configure(state='!disabled')
         self.wto.configure(state='!disabled')

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
       if CLM.to_use.get():
         self.wto.configure(state='!disabled')
         self.wdo.configure(state='disabled')
       else:
         self.wto.configure(state='disabled')
         self.wdo.configure(state='!disabled')

     #def select_record():
     #  if CLM.record_use.get():
     #    self.wrn.configure(state='!disabled')
     #  else:
     #    self.wrn.configure(state='disabled')

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
         vid = CLM.VEC[CLM.VINDEX].V.varid
         CLM.Vv.set(CLM.VEC[CLM.VINDEX].V.icdf.vname[vid])
       else:
         CLM.VINDEX = self.VLIST.index(CLM.VFILE.get())
         vid = CLM.VEC[CLM.VINDEX].V.varid
         CLM.Vx.set(CLM.VEC[CLM.VINDEX].V.icdf.xname)
         CLM.Vy.set(CLM.VEC[CLM.VINDEX].V.icdf.yname)
         CLM.Vz.set(CLM.VEC[CLM.VINDEX].V.icdf.zname)
         CLM.Vt.set(CLM.VEC[CLM.VINDEX].V.icdf.tname)
         CLM.Vv.set(CLM.VEC[CLM.VINDEX].V.icdf.vname[vid])

     def select_tfile():
       if empty(CLM.TFILE.get()):
         CLM.TINDEX = None
         CLM.Tx.set('')
         CLM.Ty.set('')
         CLM.Tz.set('')
         CLM.Tt.set('')
         CLM.Tvname.set('')
       else:
         CLM.TINDEX = self.ULIST.index(CLM.TFILE.get())
         CLM.Tx.set(CLM.VEC[CLM.TINDEX].U.icdf.xname)
         CLM.Ty.set(CLM.VEC[CLM.TINDEX].U.icdf.yname)
         CLM.Tz.set(CLM.VEC[CLM.TINDEX].U.icdf.zname)
         CLM.Tt.set(CLM.VEC[CLM.TINDEX].U.icdf.tname)

     def loadconf():
       print('Loading default configuration')
       CLM.load(CLM.FILECONF)


     def saveconf():
       print('Saving default configuration')
       CLM.save(CLM.FILECONF)


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
     self.nb.add(self.page3,text='Advected scalars')
     self.nb.add(self.page4,text='Input/Output files')
     self.nb.add(self.page5,text='Time management')
     self.nb.add(self.page6,text='Cloud simulation')

     self.nb.grid()

     # Initialize the CLM files and variables:
     # AAA

     CLM.UFILE.set(self.ULIST[CLM.UINDEX])
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

     # The main window
     #
     F1 = ttk.Frame(self.page1,padding=5)
     ttk.Label(F1,text='Zonal velocity file -U',width=25, \
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
     ttk.Label(F2,text='Meridional velocity file -V',width=25, \
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

     F3 = ttk.Frame(self.page3,padding=5)
     ttk.Label(F3,text='Advected parameter file -T',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Label(F3,text='file =') \
        .grid(row=1,column=0,padx=3,sticky='e')
     tbox = ttk.Combobox(F3,textvariable=CLM.TFILE,width=80, \
                                values=self.ULIST)
     tbox.grid(row=1,column=1,columnspan=8)
     tbox.bind('<<ComboboxSelected>>',lambda e: select_tfile())
     #ttk.Entry(F3,textvariable=CLM.TFILE,width=80).grid(row=1,column=1,columnspan=8)
     ttk.Label(F3,text='x =')    \
        .grid(row=2,column=0,padx=3,sticky='e')
     ttk.Entry(F3,textvariable=CLM.Tx,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F3,text='y =')    \
        .grid(row=3,column=0,padx=3,sticky='e')
     ttk.Entry(F3,textvariable=CLM.Ty,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F3,text='z =')    \
        .grid(row=4,column=0,padx=3,sticky='e')
     ttk.Entry(F3,textvariable=CLM.Tz,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F3,text='t =')    \
        .grid(row=5,column=0,padx=3,sticky='e')
     ttk.Entry(F3,textvariable=CLM.Tt,width=50).grid(row=5,column=1,columnspan=5)
     ttk.Label(F3,text='variable =',width=8,justify='right') \
        .grid(row=6,column=0,padx=3,sticky='e')
     ttk.Entry(F3,textvariable=CLM.Tvname,width=50).grid(row=6,column=1,columnspan=5)
     F3.grid(pady=5)

     F4 = ttk.Frame(master,padding=5)
     ttk.Label(F4,text='Initial float position',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Checkbutton(F4,text='Use INIT file (See Input/Output files tab)', \
                     variable=CLM.INI_USE,command=switch_mode).grid(row=1,column=1,padx=3)
     ttk.Label(F4,text='x0 =').grid(row=2,column=0,padx=3)
     self.wxo = ttk.Entry(F4,textvariable=CLM.xo,width=50)
     self.wxo.grid(row=2,column=1,columnspan=5)
     ttk.Label(F4,text='y0 =').grid(row=3,column=0,padx=3)
     self.wyo = ttk.Entry(F4,textvariable=CLM.yo,width=50)
     self.wyo.grid(row=3,column=1,columnspan=5)
     ttk.Label(F4,text='t0 =').grid(row=4,column=0,padx=3)
     self.wto = ttk.Entry(F4,textvariable=CLM.to,width=50)
     self.wto.grid(row=4,column=1,columnspan=5)
     #ttk.Checkbutton(F4,text='Use initial time',variable=CLM.to_use,command=select_to) \
     #   .grid(row=4,column=6,padx=[5,1])
     #ttk.Label(F4,text='do =').grid(row=5,column=0,padx=3)
     #self.wdo = ttk.Entry(F4,textvariable=CLM.do,width=50)
     #self.wdo.grid(row=5,column=1,columnspan=5)
     F4.grid(pady=5)

     #if CLM.to_use.get():
     #  self.wto.configure(state='!disabled')
     #  self.wdo.configure(state='disabled')
     #else:
     #  self.wto.configure(state='disabled')
     #  self.wdo.configure(state='!disabled')

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
     F5.grid(pady=5)

     F6 = ttk.Frame(self.page5,padding=5)
     ttk.Label(F6,text='Time Management',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     #ttk.Label(F6,text='Stationary =').grid(row=1,column=0,padx=3)
     #ttk.Entry(F6,textvariable=CLM.stationary,width=50).grid(row=1,column=1,columnspan=5)
     #ttk.Label(F6,text='Record =').grid(row=2,column=0,padx=3)
     #self.wrn = ttk.Entry(F6,textvariable=CLM.record,width=50)
     #self.wrn.grid(row=2,column=1,columnspan=5)
     #ttk.Checkbutton(F6,text='Use record',variable=CLM.record_use,command=select_record) \
     #   .grid(row=2,column=6,padx=[5,1])
     #ttk.Label(F6,text='External dt =').grid(row=3,column=0,padx=3)
     #ttk.Entry(F6,textvariable=CLM.edt,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F6,text='Simulation time =').grid(row=3,column=0,padx=3)
     ttk.Entry(F6,textvariable=CLM.time_sim,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F6,text='Internal dt =').grid(row=4,column=0,padx=3)
     ttk.Entry(F6,textvariable=CLM.idt,width=50).grid(row=4,column=1,columnspan=5)
     F6.grid(pady=5)
     #if CLM.record_use.get():
     #  self.wrn.configure(state='!disabled')
     #else:
     #  self.wrn.configure(state='disabled')

     F7 = ttk.Frame(self.page6,padding=5)
     ttk.Label(F7,text='Time Management',width=25, \
               font=font_bold).grid(row=0,column=0,columnspan=4)
     ttk.Label(F7,text='Random Seed =').grid(row=1,column=0,padx=3)
     ttk.Entry(F7,textvariable=CLM.seed,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Label(F7,text='Num. Floats =').grid(row=2,column=0,padx=3)
     ttk.Entry(F7,textvariable=CLM.nfloats,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F7,text='X Radius =').grid(row=3,column=0,padx=3)
     ttk.Entry(F7,textvariable=CLM.Rx,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F7,text='Y Radius =').grid(row=4,column=0,padx=3)
     ttk.Entry(F7,textvariable=CLM.Ry,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F7,text='T Radius =').grid(row=5,column=0,padx=3)
     ttk.Entry(F7,textvariable=CLM.Rt,width=50).grid(row=5,column=1,columnspan=5)
     F7.grid(pady=5)

