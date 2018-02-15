# Module for the applications built for the COSMO project 
# Quim Ballabrera, May 2017

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

from cosmo import *

# ===================
class parameters:
# ===================
  ''' Class whose attributes will contain the options for running
      the COSMO Lagrangian Model'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "January 2018"

  def __init__(self):

    self.PATH       = tk.StringVar()
    self.BIN        = tk.StringVar()

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
    self.do         = tk.StringVar()
    self.to_use     = tk.BooleanVar()
    self.to_use.set(False)

    self.reverse    = tk.BooleanVar()
    self.stationary = tk.BooleanVar()
    self.record_use = tk.BooleanVar()
    self.record     = tk.IntVar()
    self.idt        = tk.IntVar()
    self.edt        = tk.IntVar()
    self.record_use.set(False)

    self.seed       = tk.IntVar()
    self.nfloats    = tk.IntVar()
    self.Rx         = tk.DoubleVar()
    self.Ry         = tk.DoubleVar()
    self.Rt         = tk.DoubleVar()

    try:
      f = open('clm.conf','r')
      ii = -1 
      for line in f:
        ii += 1
        if ii == 0:
          self.PATH.set(line[0:-1])
        elif ii == 1:
          self.BIN.set(line[0:-1])
        elif ii == 2:
          self.TRAJECTORY.set(line[0:-1])
        elif ii == 3:
          self.INI.set(line[0:-1])
        elif ii == 4:
          self.FIN.set(line[0:-1])
      f.close()
    except:
      # Default COSMO LAGRANGIAN MDOEL PATHS
      self.PATH.set('/home/joaquim/cosmo/bin/')
      self.BIN.set('lagrangian')
      self.TRAJECTORY.set('out.nc')
      self.INI.set('')
      self.FIN.set('out.end')

    self.xo.set(None)
    self.yo.set(None)
    self.to.set(None)
    self.do.set('')
    self.record.set(None)
    self.stationary.set(False)
    self.edt.set(None)
    self.idt.set(3600)

    self.seed.set(None)
    self.nfloats.set(10)
    self.Rx.set(0.10)
    self.Ry.set(0.10)
    self.Rt.set(0)

   
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

  options += ' vel=%s' % CLM.Uu.get()

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

  options += ' vel=%s' % CLM.Vv.get()

  if empty(CLM.TRAJECTORY.get()):
    pass
  else:
    options += ' -traj %s' % CLM.TRAJECTORY.get()

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
    aa = ' -idt %s' % CLM.idt.get()
    options += aa
  except:
    pass

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

     def switch_mode():
       if CLM.INI_USE.get():
         self.wxo.configure(state='disabled')
         self.wyo.configure(state='disabled')
         self.wto.configure(state='disabled')
         self.wdo.configure(state='disabled')
       else:
         self.wxo.configure(state='!disabled')
         self.wyo.configure(state='!disabled')
         if CLM.to_use.get():
           self.wto.configure(state='!disabled')
           self.wdo.configure(state='disabled')
         else:
           self.wto.configure(state='disabled')
           self.wdo.configure(state='!disabled')

     def open_traj():
       nn = filedialog.askopenfile()
       if nn is None:
         pass
       else:
         CLM.TRAJECTORY.set(nn.name)

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

     def select_record():
       if CLM.record_use.get():
         self.wrn.configure(state='!disabled')
       else:
         self.wrn.configure(state='disabled')

     F0 = ttk.Frame(master,padding=5)
     ttk.Label(F0,text='PATH',font='bold').grid(row=0,column=0,padx=3,pady=3)
     ttk.Entry(F0,textvariable=CLM.PATH,width=50).grid(row=0,column=1,columnspan=5)
     ttk.Label(F0,text='BIN',font='bold').grid(row=1,column=0,padx=3,pady=3)
     ttk.Entry(F0,textvariable=CLM.BIN,width=50).grid(row=1,column=1,columnspan=5)
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
     self.nb.add(self.page2,text='Merdidional Velocity')
     self.nb.add(self.page3,text='Advected scalars')
     self.nb.add(self.page4,text='Input/Output files')
     self.nb.add(self.page5,text='Time management')
     self.nb.add(self.page6,text='Cloud simulation')

     self.nb.grid()

     F1 = ttk.Frame(self.page1,padding=5)
     ttk.Label(F1,text='Zonal velocity file -U',width=25, \
               font="Helvetica 12 bold").grid(row=0,column=0,columnspan=4)
     ttk.Label(F1,text='file =',width=8).grid(row=1,column=0,padx=3)
     ttk.Entry(F1,textvariable=CLM.UFILE,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Label(F1,text='x =').grid(row=2,column=0,padx=3)
     ttk.Entry(F1,textvariable=CLM.Ux,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F1,text='y =').grid(row=3,column=0,padx=3)
     ttk.Entry(F1,textvariable=CLM.Uy,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F1,text='z =').grid(row=4,column=0,padx=3)
     ttk.Entry(F1,textvariable=CLM.Uz,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F1,text='t =').grid(row=5,column=0,padx=3)
     ttk.Entry(F1,textvariable=CLM.Ut,width=50).grid(row=5,column=1,columnspan=5)
     ttk.Label(F1,text='vel =').grid(row=6,column=0,padx=3)
     ttk.Entry(F1,textvariable=CLM.Uu,width=50).grid(row=6,column=1,columnspan=5)
     F1.grid(pady=5)


     F2 = ttk.Frame(self.page2,padding=5)
     ttk.Label(F2,text='Meridional velocity file -V',width=25, \
               font="Helvetica 12 bold").grid(row=0,column=0,columnspan=4)
     ttk.Label(F2,text='file =',width=8).grid(row=1,column=0,padx=3)
     ttk.Entry(F2,textvariable=CLM.VFILE,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Label(F2,text='x =').grid(row=2,column=0,padx=3)
     ttk.Entry(F2,textvariable=CLM.Vx,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F2,text='y =').grid(row=3,column=0,padx=3)
     ttk.Entry(F2,textvariable=CLM.Vy,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F2,text='z =').grid(row=4,column=0,padx=3)
     ttk.Entry(F2,textvariable=CLM.Vz,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F2,text='t =').grid(row=5,column=0,padx=3)
     ttk.Entry(F2,textvariable=CLM.Vt,width=50).grid(row=5,column=1,columnspan=5)
     ttk.Label(F2,text='vel =').grid(row=6,column=0,padx=3)
     ttk.Entry(F2,textvariable=CLM.Vv,width=50).grid(row=6,column=1,columnspan=5)
     F2.grid(pady=5)

     F3 = ttk.Frame(self.page3,padding=5)
     ttk.Label(F3,text='Advected parameter file -T',width=25, \
               font="Helvetica 12 bold").grid(row=0,column=0,columnspan=4)
     ttk.Label(F3,text='file =',width=8).grid(row=1,column=0,padx=3)
     ttk.Entry(F3,textvariable=CLM.TFILE,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Label(F3,text='x =').grid(row=2,column=0,padx=3)
     ttk.Entry(F3,textvariable=CLM.Tx,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Label(F3,text='y =').grid(row=3,column=0,padx=3)
     ttk.Entry(F3,textvariable=CLM.Ty,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F3,text='z =').grid(row=4,column=0,padx=3)
     ttk.Entry(F3,textvariable=CLM.Tz,width=50).grid(row=4,column=1,columnspan=5)
     ttk.Label(F3,text='t =').grid(row=5,column=0,padx=3)
     ttk.Entry(F3,textvariable=CLM.Tt,width=50).grid(row=5,column=1,columnspan=5)
     ttk.Label(F3,text='variable =').grid(row=6,column=0,padx=3)
     ttk.Entry(F3,textvariable=CLM.Tvname,width=50).grid(row=6,column=1,columnspan=5)
     F3.grid(pady=5)

     F4 = ttk.Frame(master,padding=5)
     ttk.Label(F4,text='Initial float position',width=25, \
               font="Helvetica 12 bold").grid(row=0,column=0,columnspan=4)
     ttk.Checkbutton(F4,text='Use INIT file (See Input/Output files tab)', \
                     variable=CLM.INI_USE,command=switch_mode).grid(row=1,column=1,padx=3)
     ttk.Label(F4,text='xo =').grid(row=2,column=0,padx=3)
     self.wxo = ttk.Entry(F4,textvariable=CLM.xo,width=50)
     self.wxo.grid(row=2,column=1,columnspan=5)
     ttk.Label(F4,text='yo =').grid(row=3,column=0,padx=3)
     self.wyo = ttk.Entry(F4,textvariable=CLM.yo,width=50)
     self.wyo.grid(row=3,column=1,columnspan=5)
     ttk.Label(F4,text='to =').grid(row=4,column=0,padx=3)
     self.wto = ttk.Entry(F4,textvariable=CLM.to,width=50)
     self.wto.grid(row=4,column=1,columnspan=5)
     ttk.Checkbutton(F4,text='Use initial time',variable=CLM.to_use,command=select_to) \
        .grid(row=4,column=6,padx=[5,1])
     ttk.Label(F4,text='do =').grid(row=5,column=0,padx=3)
     self.wdo = ttk.Entry(F4,textvariable=CLM.do,width=50)
     self.wdo.grid(row=5,column=1,columnspan=5)
     F4.grid(pady=5)
     if CLM.to_use.get():
       self.wto.configure(state='!disabled')
       self.wdo.configure(state='disabled')
     else:
       self.wto.configure(state='disabled')
       self.wdo.configure(state='!disabled')

     F5 = ttk.Frame(self.page4,padding=5)
     ttk.Label(F5,text='Input/Output files',width=25, \
               font="Helvetica 12 bold").grid(row=0,column=0,columnspan=4)
     ttk.Label(F5,text='Trajectory =').grid(row=1,column=0,padx=3)
     ttk.Entry(F5,textvariable=CLM.TRAJECTORY,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Button(F5,text='Open',padding=3,command=open_traj).grid(row=1,column=6,padx=[5,1])
     ttk.Label(F5,text='Initial positions =').grid(row=2,column=0,padx=3)
     ttk.Entry(F5,textvariable=CLM.INI,width=50).grid(row=2,column=1,columnspan=5)
     ttk.Button(F5,text='Open',padding=3,command=open_init).grid(row=2,column=6,padx=[5,1])
     ttk.Label(F5,text='Final positions =').grid(row=3,column=0,padx=3)
     ttk.Entry(F5,textvariable=CLM.FIN,width=50).grid(row=3,column=1,columnspan=5)
     F5.grid(pady=5)

     F6 = ttk.Frame(self.page5,padding=5)
     ttk.Label(F6,text='Time Management',width=25, \
               font="Helvetica 12 bold").grid(row=0,column=0,columnspan=4)
     ttk.Label(F6,text='Stationary =').grid(row=1,column=0,padx=3)
     ttk.Entry(F6,textvariable=CLM.stationary,width=50).grid(row=1,column=1,columnspan=5)
     ttk.Label(F6,text='Record =').grid(row=2,column=0,padx=3)
     self.wrn = ttk.Entry(F6,textvariable=CLM.record,width=50)
     self.wrn.grid(row=2,column=1,columnspan=5)
     ttk.Checkbutton(F6,text='Use record',variable=CLM.record_use,command=select_record) \
        .grid(row=2,column=6,padx=[5,1])
     ttk.Label(F6,text='External dt =').grid(row=3,column=0,padx=3)
     ttk.Entry(F6,textvariable=CLM.edt,width=50).grid(row=3,column=1,columnspan=5)
     ttk.Label(F6,text='Internal dt =').grid(row=4,column=0,padx=3)
     ttk.Entry(F6,textvariable=CLM.idt,width=50).grid(row=4,column=1,columnspan=5)
     F6.grid(pady=5)
     if CLM.record_use.get():
       self.wrn.configure(state='!disabled')
     else:
       self.wrn.configure(state='disabled')

     F7 = ttk.Frame(self.page6,padding=5)
     ttk.Label(F7,text='Time Management',width=25, \
               font="Helvetica 12 bold").grid(row=0,column=0,columnspan=4)
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

