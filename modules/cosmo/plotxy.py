''' plotxy.py
    Joaquim Ballabrera, July 2020.
'''

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog
from tkinter import font as tkfont


import numpy as np
import datetime
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
from matplotlib.dates import date2num

def rot(phi):
# -----------
  ''' Returns the complex rotation factor: exp(j*phi)'''

  return np.exp(1j*phi)

def plot_ellipse(ax,xo,yo,a,b,mphi,ephi):
# ---------------------------------------

  phi = ephi

  # Rotation matrix
  R = np.array([[np.cos(phi), -np.sin(phi)],[np.sin(phi), np.cos(phi)]])
  
  # Parametric centered ellipse
  n = 100
  t = np.linspace(0,2*np.pi,n)
  E = np.array([a*np.cos(t), b*np.sin(t)])

  Er = np.zeros((2,n))
  for i in range(n):
    Er[:,i] = np.dot(R,E[:,i])

  ax.plot(xo+Er[0,:], yo+Er[1,:],linestyle='-')

  C = np.array([b*np.cos(t), b*np.sin(t)])
  ax.plot(xo+C[0,:], yo+C[1,:],linestyle='--')


  # - Direction mean flow
  tc = np.arctan2(np.tan(mphi)*a,b)
  xmax = np.abs(a*np.cos(tc))
  xmin = -np.abs(a*np.cos(tc))

  x = np.linspace(xmin,xmax,20)
  y = np.tan(mphi)*x

  ax.plot(xo+x, yo+y,linestyle='--',color='brown',linewidth=1.5,label='Mean flow direction')

  # - Eddy orientation
  tc = np.arctan2(np.tan(ephi)*a,b)
  xmax = np.abs(a*np.cos(tc))
  xmin = -np.abs(a*np.cos(tc))

  x = np.linspace(xmin,xmax,20)
  y = np.tan(ephi)*x

  ax.plot(xo+x, yo+y,linestyle='--',color='red',linewidth=0.8,label='Eddy orientation')


  ax.legend()



# ===========
class PLOTXY:
# ===========
  ''' Launches a widget for time series plotting '''

  def __init__(self,master,t=None,u=None,v=None,plot_type=None,exit_mode=None,**args):
  # ----------------------------------------------------------------------------------

    try:
      wid = args['wid']
    except:
      wid = None

    self.master    = master
    self.exit_mode = exit_mode
    self.t         = t           # Working data
    self.u         = u           # Working data
    self.v         = v           # Working data
    self.u_orig    = u           # Data backup
    self.v_orig    = v           # Data backup

    ucolor = args.pop('ucolor','blue')
    vcolor = args.pop('vcolor','red')
    uthick = args.pop('uthick',1)
    vthick = args.pop('vthick',1)
    ulabel = args.pop('ulabel','u')
    vlabel = args.pop('vlabel','v')
    title  = args.pop('title','')
    scale  = args.pop('scale',10)
    tcolor = args.pop('tcolor','green')
    twidth = args.pop('twidth',0.004)

    if t is None:
      t = np.arange(len(u))

    self.umin   = tk.DoubleVar()
    self.umax   = tk.DoubleVar()
    self.vmin   = tk.DoubleVar()
    self.vmax   = tk.DoubleVar()
    self.ucolor = tk.StringVar()
    self.vcolor = tk.StringVar()
    self.uthick = tk.DoubleVar()
    self.vthick = tk.DoubleVar()
    self.xlabel = tk.StringVar()
    self.ulabel = tk.StringVar()
    self.vlabel = tk.StringVar()
    self.tscale = tk.DoubleVar()
    self.tcolor = tk.StringVar()
    self.twidth = tk.DoubleVar()
    self.title  = tk.StringVar()

    self.Vgrid  = tk.BooleanVar()
    self.Hgrid  = tk.BooleanVar()

    self.type = tk.StringVar()
    self.type_options = ['u plot','uv plot','stick plot','rotated uv','var ellipse']

    if v is None:
      self.type_options = ['u plot']
      selt.type.set('u plot')

    self.ucolor.set(ucolor)
    self.vcolor.set(vcolor)
    self.uthick.set(uthick)
    self.vthick.set(vthick)
    self.ulabel.set(ulabel)
    self.vlabel.set(vlabel)
    self.Vgrid.set(True)
    self.Hgrid.set(False)
    self.tscale.set(scale)
    self.tcolor.set(tcolor)
    self.twidth.set(twidth)
    self.title.set(title)

    try:
      value = args['umin']
      self.umin.set(float(value))
    except:
      umin = np.nanmin(u)
      self.umin.set(umin)
    try:
      value = args['umax']
      self.umax.set(float(value))
    except:
      umax = np.nanmax(u)
      self.umax.set(umax)

    if v is None:
      self.type.set('u plot')
    else:
      self.type.set('uv plot')

      # Modify the default values:
      try:
        value = args['vmin']
        self.vmin.set(float(value))
      except:
        vmin = np.nanmin(v)
        self.vmin.set(vmin)
      try:
        value = args['vmax']
        self.vmax.set(float(value))
      except:
        vmax = np.nanmax(v)
        self.vmax.set(vmax)



    # Define widget
    self.main = tk.Frame(master)

    F0 = ttk.Frame(self.main,padding=5)
    ttk.Label(F0,text='Plot type',width=12,padding=3).grid(row=0,column=0,sticky='e')
    self.wtype = ttk.Combobox(F0,textvariable=self.type,values=self.type_options,width=12)
    self.wtype.grid(row=0,column=1,sticky='w')
    self.wtype.bind("<<ComboboxSelected>>", lambda f: self.select_plot())


    FU = ttk.Frame(F0)
    ttk.Label(FU,text='U').grid(row=0,column=0,pady=5,sticky='w')

    ttk.Label(FU,text='Max value',padding=3).grid(row=1,column=0,sticky='e')
    self.wumax = ttk.Entry(FU,textvariable=self.umax,width=12)
    self.wumax.grid(row=1,column=1,sticky='ew')
    self.wumax.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FU,text='Min value',padding=3).grid(row=2,column=0,sticky='e')
    self.wumin = ttk.Entry(FU,textvariable=self.umin,width=12)
    self.wumin.grid(row=2,column=1,sticky='ew')
    self.wumin.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FU,text='Line color',padding=3).grid(row=3,column=0,sticky='e')
    self.wucol = ttk.Entry(FU,textvariable=self.ucolor,width=12)
    self.wucol.grid(row=3,column=1,sticky='ew')
    self.wucol.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FU,text='Line thickness',padding=3).grid(row=4,column=0,sticky='e')
    self.wuthk = ttk.Entry(FU,textvariable=self.uthick,width=12)
    self.wuthk.grid(row=4,column=1,sticky='ew')
    self.wuthk.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FU,text='Label',padding=3).grid(row=5,column=0,sticky='e')
    self.wulab = ttk.Entry(FU,textvariable=self.ulabel,width=12)
    self.wulab.grid(row=5,column=1,sticky='ew')
    self.wulab.bind("<Return>", lambda f: self.make_plot())

    FU.grid(row=1,column=0,columnspan=2)

    FV = ttk.Frame(F0)
    ttk.Label(FV,text='V').grid(row=0,column=0,pady=5,sticky='w')

    ttk.Label(FV,text='Max value',padding=3).grid(row=1,column=0,sticky='e')
    self.wvmax = ttk.Entry(FV,textvariable=self.vmax,width=12)
    self.wvmax.grid(row=1,column=1,sticky='ew')
    self.wvmax.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FV,text='Min value',padding=3).grid(row=2,column=0,sticky='e')
    self.wvmin = ttk.Entry(FV,textvariable=self.vmin,width=12)
    self.wvmin.grid(row=2,column=1,sticky='ew')
    self.wvmin.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FV,text='Line color',padding=3).grid(row=3,column=0,sticky='e')
    self.wvcol = ttk.Entry(FV,textvariable=self.vcolor,width=12)
    self.wvcol.grid(row=3,column=1,sticky='ew')
    self.wvcol.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FV,text='Line thickness',padding=3).grid(row=4,column=0,sticky='e')
    self.wvthk = ttk.Entry(FV,textvariable=self.vthick,width=12)
    self.wvthk.grid(row=4,column=1,sticky='ew')
    self.wvthk.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FV,text='Label',padding=3).grid(row=5,column=0,sticky='e')
    self.wvlab = ttk.Entry(FV,textvariable=self.vlabel,width=12)
    self.wvlab.grid(row=5,column=1,sticky='ew')
    self.wvlab.bind("<Return>", lambda f: self.make_plot())

    FV.grid(row=2,column=0,columnspan=2)

    FT = ttk.Frame(F0)
    ttk.Label(FT,text='Stick plot options').grid(row=0,column=0,pady=5,sticky='w')
    ttk.Label(FT,text='Scale',padding=3).grid(row=1,column=0,sticky='e')
    self.wtscl = ttk.Entry(FT,textvariable=self.tscale,width=12)
    self.wtscl.grid(row=1,column=1,sticky='ew')
    self.wtscl.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FT,text='Color',padding=3).grid(row=2,column=0,sticky='e')
    self.wtcol = ttk.Entry(FT,textvariable=self.tcolor,width=12)
    self.wtcol.grid(row=2,column=1,sticky='ew')
    self.wtcol.bind("<Return>", lambda f: self.make_plot())
    ttk.Label(FT,text='Thickness',padding=3).grid(row=3,column=0,sticky='e')
    self.wtthk = ttk.Entry(FT,textvariable=self.twidth,width=12)
    self.wtthk.grid(row=3,column=1,sticky='ew')
    self.wtthk.bind("<Return>", lambda f: self.make_plot())
    FT.grid(row=3,column=0,columnspan=2)

    FM = ttk.Frame(F0)
    ttk.Label(FM,text='Grid options').grid(row=0,column=0,pady=5,sticky='w')
    ttk.Label(FM,text='Show vertical grid',padding=3).grid(row=1,column=0,sticky='e')
    ttk.Checkbutton(FM,variable=self.Vgrid,command = lambda : self.make_plot(),padding=3).grid(row=1,column=1,sticky='we')
    ttk.Label(FM,text='Show horizontal grid',padding=3).grid(row=2,column=0,sticky='e')
    ttk.Checkbutton(FM,variable=self.Hgrid,command = lambda : self.make_plot(),padding=3).grid(row=2,column=1,sticky='we')

    ttk.Label(FM,text='Title').grid(row=3,column=0,pady=5,sticky='w')
    self.wtitle = ttk.Entry(FM,textvariable=self.title,width=30)
    self.wtitle.grid(row=4,column=0,columnspan=2,sticky='ew')
    self.wtitle.bind("<Return>", lambda f: self.make_plot())

    tk.Button(FM,text='Save',command=self.save_plot).grid(row=5,column=0,padx=5,pady=5)
    tk.Button(FM,text='Quit',command=self.quit_plot).grid(row=5,column=1,padx=5,pady=5)

    FM.grid(row=4,column=0,columnspan=2)


    F0.grid()

    FG = ttk.Frame(self.main)
    self.fig = Figure(dpi=100)

    if self.type.get() == 'uv plot':
      self.ax1 = self.fig.add_subplot(211)
      self.ax2 = self.fig.add_subplot(212)
    else:
      self.ax1 = self.fig.add_subplot(111)

    self.canvas = FigureCanvasTkAgg(self.fig,master=FG)
    self.canvas.draw()
    self.canvas.get_tk_widget().grid(sticky='nsew')
    self.canvas._tkcanvas.grid(sticky='nsew')

    FG.grid(row=0,column=1,rowspan=5,sticky='nsew')
    #FG.grid_columnconfigure(0,weight=1)
    #FG.grid_columnconfigure(1,weight=1)

    self.main.grid()
    self.main.grid_columnconfigure(0,weight=1)
    self.main.grid_columnconfigure(1,weight=1)
    self.main.grid_columnconfigure(2,weight=1)

    self.make_plot()

    if v is None:
      self.wvmax.configure(state='disabled')
      self.wvmin.configure(state='disabled')
      self.wvcol.configure(state='disabled')
      self.wvthk.configure(state='disabled')
      self.wvlab.configure(state='disabled')
    
    if self.type != 'stick plot':
      self.wtscl.configure(state='disabled')
      self.wtcol.configure(state='disabled')
      self.wtthk.configure(state='disabled')



  def save_plot(self):
  # -------------------  
      filetypes = [('PNG file','.png'),('JPG file','.jpg'),('PDF file','.pdf')]
      nn = tk.filedialog.asksaveasfilename(title='Save',
                                           initialdir='./',
                                           filetypes=filetypes,
                                           confirmoverwrite=True)
      if len(nn) > 0:
        filename = '%s' % nn
        self.fig.savefig(filename,
                         dpi=180,
                         bbox_inches='tight')


  def quit_plot(self):
  # -------------------  
    if self.exit_mode == 'quit':
      ''' Closing the main widget '''
      messagebox.askquestion('Close','Are you sure?',icon='warning')
      if 'yes':
        quit()
    else:
      self.master.destroy()
      return


  def select_plot(self):
  # ---------------------------------------

    self.fig.clear()

    if self.type.get() == 'u plot':
      self.u = self.u_orig
      self.wumax.configure(state='normal')
      self.wumin.configure(state='normal')
      self.wucol.configure(state='normal')
      self.wuthk.configure(state='normal')
      self.wulab.configure(state='normal')
      self.wvmax.configure(state='disabled')
      self.wvmin.configure(state='disabled')
      self.wvcol.configure(state='disabled')
      self.wvthk.configure(state='disabled')
      self.wvlab.configure(state='disabled')
      self.wtscl.configure(state='disabled')
      self.wtcol.configure(state='disabled')
      self.wtthk.configure(state='disabled')
      self.ax1 = self.fig.add_subplot(111)
      self.make_plot()
      return
      
    if self.type.get() == 'uv plot':
      self.u = self.u_orig
      self.v = self.v_orig
      self.wumax.configure(state='normal')
      self.wumin.configure(state='normal')
      self.wucol.configure(state='normal')
      self.wuthk.configure(state='normal')
      self.wulab.configure(state='normal')
      self.wvmax.configure(state='normal')
      self.wvmin.configure(state='normal')
      self.wvcol.configure(state='normal')
      self.wvthk.configure(state='normal')
      self.wvlab.configure(state='normal')
      self.wtscl.configure(state='disabled')
      self.wtcol.configure(state='disabled')
      self.wtthk.configure(state='disabled')
      self.ax1 = self.fig.add_subplot(211)
      self.ax2 = self.fig.add_subplot(212)
      self.make_plot()
      return

    if self.type.get() == 'stick plot':
      self.u = self.u_orig
      self.v = self.v_orig
      self.wumax.configure(state='disabled')
      self.wumin.configure(state='disabled')
      self.wucol.configure(state='disabled')
      self.wuthk.configure(state='disabled')
      self.wulab.configure(state='disabled')
      self.wvmax.configure(state='disabled')
      self.wvmin.configure(state='disabled')
      self.wvcol.configure(state='disabled')
      self.wvthk.configure(state='disabled')
      self.wvlab.configure(state='disabled')
      self.wtscl.configure(state='normal')
      self.wtcol.configure(state='normal')
      self.wtthk.configure(state='normal')
      self.ax1 = self.fig.add_subplot(111)
      self.make_plot()
      return
      
    if self.type.get() == 'rotated uv':
      self.ax1 = self.fig.add_subplot(211)
      self.ax2 = self.fig.add_subplot(212)
      self.rotated_uv()

    if self.type.get() == 'var ellipse':
      u = self.u
      v = self.v

      # Calculation of the anomalies
      mu = np.mean(u)
      mv = np.mean(v)
      mphi = np.angle(mu+1j*mv)
      print('Angle mean current = ', mphi, 180*mphi/np.pi)
      u = u - np.mean(u)
      v = v - np.mean(v)
      suu = np.dot(u,u)
      svv = np.dot(v,v)
      suv = np.dot(u,v)
      Tra = suu + svv
      Det = suu*svv - suv*suv
      a2  = 0.5*(Tra + np.sqrt(Tra*Tra - 4*Det))
      b2  = 0.5*(Tra - np.sqrt(Tra*Tra - 4*Det))
      aphi = 0.5*np.arctan2(2*suv,suu-svv)
      print('Test: ',2*suv/(suu-svv), np.tan(2*aphi))
      print('Eddy kinetic energy: ', 0.5*Tra)
      print('Total eddy variance: ', a2 + b2, Tra)
      print('Directional eddy variance: ', a2 - b2)
      print('Isotropic eddy variance: ', 2*b2)
      print('Polarization factor: ', (a2-b2)/(a2+b2))
      print('Variance angle: ', aphi, 180*aphi/np.pi)

      self.ax1 = self.fig.add_subplot(111)
      self.ax1.axis('equal')
      plt.subplots_adjust(bottom=0.4)
      self.ax1.set_xlim(-np.sqrt(Tra),np.sqrt(Tra))
      self.ax1.set_ylim(-np.sqrt(Tra),np.sqrt(Tra))
      plot_ellipse(self.ax1,0,0,np.sqrt(a2),np.sqrt(b2),mphi,aphi)
      txt1 = 'Mean velocity components: %.2f, %.2f ' %(mu,mv)
      txt2 = 'Mean velocity angle: %.2f ' %(180*mphi/np.pi)
      txt3 = 'Total anomaly variance: %.2f ' %(Tra)
      txt4 = 'Directional anomaly variance: %.2f ' %(a2-b2)
      txt5 = 'Isotropic anomaly variance: %.2f ' %(2*b2)
      txt6 = 'Polarization factor: %.2f ' %((a2-b2)/Tra)
      txt7 = 'Eddy orientation: %.2f ' %(180*aphi/np.pi)
      print(txt1)
      print(txt2)
      print(txt3)
      print(txt4)
      print(txt5)
      print(txt6)
      print(txt7)

      self.ax1.annotate(txt1,(0.11,0.98),xycoords='figure fraction')
      self.ax1.annotate(txt2,(0.55,0.98),xycoords='figure fraction')
      self.ax1.annotate(txt3,(0.11,0.95),xycoords='figure fraction')
      self.ax1.annotate(txt4,(0.11,0.92),xycoords='figure fraction')
      self.ax1.annotate(txt5,(0.55,0.92),xycoords='figure fraction')
      self.ax1.annotate(txt6,(0.11,0.89),xycoords='figure fraction')
      self.ax1.annotate(txt7,(0.55,0.89),xycoords='figure fraction')

      #self.ax1.text(0.05,0.01,'Mean velocity components: %.2f, %.2f' % (mu,mv), \
      #            ha='right',fontsize=8)
      #plt.figtext(0.05,0.01,'Mean velocity components: '+str(mu)+', '+str(mv), \
      #            horizontalalignment='right',fontsize=8)
      #plt.figtext(0.18,0.92,'Mean velocity angle: '+str(180*mphi/np.pi), \
      #            horizontalalignment='right',fontsize=8)
      #plt.figtext(0.18,0.89,'Total eddy variance: '+str(Tra), \
      #            horizontalalignment='right',fontsize=8)
      self.canvas.draw()
      #self.make_plot()

  def rotated_uv(self):
  # -----------------------------
    c = self.u + 1j*self.v          # Complex velocity
    mc = np.mean(c)
    print('Mean current: ',mc)
    angle_rad = np.angle(mc)
    angle_deg = 180*angle_rad/np.pi
    print('Mean current angle (Rad, Deg): ',angle_rad,angle_deg)
    rc = c*rot(-angle_rad)  # Rotated current
    print('Mean rotated current: ', np.mean(rc))
    #print(np.angle(np.mean(rc)))
    self.u = rc.real
    self.v = rc.imag
    self.make_plot()

      
  def stick_plot(self,time,u,v,**kw):
  # -----------------------------
    width = kw.pop('width', 0.002)
    headwidth = kw.pop('headwidth', 0)
    headlength = kw.pop('headlength', 0)
    headaxislength = kw.pop('headaxislength', 0)
    ax = kw.pop('ax', None)

    time, u, v = map(np.asanyarray, (time, u, v))
    if not ax:
        fig, ax = plt.subplots()

    q = ax.quiver(date2num(time), [[0]*len(time)], u, v,
                  angles='uv', width=width, headwidth=headwidth,
                  headlength=headlength, headaxislength=headaxislength,
                  **kw)

    ax.axes.get_yaxis().set_visible(False)
    ax.xaxis_date()
    return q

  def make_plot(self):
  # ---------------------------------------

    if self.type.get() == 'u plot':
    # - - - - - - - - - - - - - - - -
      self.ax1.clear()
      self.ax1.plot(self.t,self.u,                        \
                   color=self.ucolor.get(),   \
                   linewidth=self.uthick.get())
      self.ax1.set_ylim(self.umin.get(),self.umax.get())
      self.ax1.set_ylabel(self.ulabel.get())
      self.ax1.axhline(color='black')
      if self.Vgrid.get():
        self.ax1.xaxis.grid()
      if self.Hgrid.get():
        self.ax1.yaxis.grid()
      if isinstance(self.t[0],datetime.datetime):
        self.ax1.tick_params(axis='x',rotation=35)

    elif self.type.get() == 'uv plot' or self.type.get() == 'rotated uv':
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      self.ax1.clear()
      self.ax1.plot(self.t,self.u,                        \
                   color=self.ucolor.get(),   \
                   linewidth=self.uthick.get())
      self.ax1.set_ylim(self.umin.get(),self.umax.get())
      self.ax1.set_ylabel(self.ulabel.get())
      self.ax1.axhline(color='black')
      if self.Vgrid.get():
        self.ax1.xaxis.grid()
      if self.Hgrid.get():
        self.ax1.yaxis.grid()

      # Hide bottom labels from top plot
      self.ax1.tick_params(labelbottom=False) 

      self.ax2.clear()
      self.ax2.plot(self.t,self.v,                        \
                   color=self.vcolor.get(),   \
                   linewidth=self.vthick.get())
      # Set vertical limits
      self.ax2.set_ylim(self.vmin.get(),self.vmax.get())
      # Print vertical label
      self.ax2.set_ylabel(self.vlabel.get())
      self.ax2.axhline(color='black')

      if isinstance(self.t[0],datetime.datetime):
        self.ax2.tick_params(axis='x',rotation=35)

      # Show (or not) Vertical grid
      if self.Vgrid.get():
        self.ax2.xaxis.grid()
      # Show (or not) Horizontal grid
      if self.Hgrid.get():
        self.ax2.yaxis.grid()

    elif self.type.get() == 'stick plot':
    # - - - - - - - - - - - - - - - - - -
      self.ax1.clear()
      q = self.stick_plot(self.t,self.u,self.v,                     \
                          ax=self.ax1,               \
                          width=self.twidth.get(),   \
                          scale=self.tscale.get(),   \
                          color=self.tcolor.get())
      if self.Vgrid.get():
        self.ax1.xaxis.grid()
      if self.Hgrid.get():
        self.ax1.yaxis.grid()
      if isinstance(self.t[0],datetime.datetime):
        self.ax1.tick_params(axis='x',rotation=35)

    self.ax1.set_title(self.title.get())
    self.canvas.draw()

def main():
  from datetime import datetime, timedelta

  def _close():
    quit()

  # Random data to plot
  u = np.random.rand(100)
  v = np.random.rand(100)
  start = datetime.now()
  time = [start + timedelta(days=n) for n in range(len(u))]

  root = tk.Tk()
  root.title('PLOTXY')
  root.resizable(width=True,height=False)
  root.protocol('WM_DELETE_WINDOW',_close)
  app = PLOTXY(root,t=time,u=u,v=v,exit_mode='quit')
  root.mainloop()

if __name__ == '__main__':
  main()
