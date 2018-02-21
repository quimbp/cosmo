''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems'''

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

import datetime 
import dateutil.parser as dparser
from calendar import monthrange
import wget
import os
__version__ = "1.0"
__author__  = "Quim Ballabrera"
__date__    = "February 2018"

def empty(string):
  if bool(string.strip()):
    return False
  else:
    return True

class WinTracking():

  def __init__ (self,master):

    self.master         = master

    self.PATH           = tk.StringVar()
    self.FILE           = tk.StringVar()
    self.USER_FILE      = tk.StringVar()


    # Configure the access credentials
    #
    try:
      f = open('copernicus.conf','r')
      ii = -1
      for line in f:
        ii += 1
        if ii == 0:
          self.python = '%s' % line[0:-1]
        elif ii == 1:
          self.motu = '%s' % line[0:-1]
        elif ii == 2:
          self.user = '%s' % line[0:-1]
        elif ii == 3:
          self.password = '%s' % line[0:-1]
        elif ii == 4:
          self.PATH.set(line[0:-1])
        elif ii == 5:
          self.USER_FILE.set(line[0:-1])
      f.close()
    except:
      self.python = '/usr/bin/python'
      self.motu = '/usr/local/src/motu-client-python/motu-client.py'
      self.user = 'USERNAME_REQUIRED'
      self.password = 'PASSWORD_REQUIRED'
      self.PATH.set('./')
      self.USER_FILE.set('')


    # Define basins: Example: Global, Mediterraneans
    self.Nbasins = 0
    self.BASIN_LIST    = []
    self.BASIN_PRODUCT = []
    self.BASIN_WEST    = []
    self.BASIN_EAST    = []
    self.BASIN_SOUTH   = []
    self.BASIN_NORTH   = []
    self.BASIN_ZMIN    = []
    self.BASIN_ZMAX    = []
    self.BASIN_DATEMIN = []
    self.BASIN_DATEO   = []
    self.BASIN_MOTU    = []
    self.BASIN_SERVICE = []
    self.BASIN_VARS    = []
    try:
      f = open('products.conf','r')
      read_basins = True
    except:
      read_basins = False

    if read_basins:
      print('Loading products.conf')
      for line in f:
        try:
          TMP = list(line.split(',')[:])
          self.BASIN_LIST.append(TMP[0])
          #print(self.BASIN_LIST)
          self.BASIN_PRODUCT.append(TMP[1])
          #print(self.BASIN_PRODUCT)
          self.BASIN_WEST.append(float(TMP[2]))
          #print(self.BASIN_WEST)
          self.BASIN_EAST.append(float(TMP[3]))
          #print(self.BASIN_EAST)
          self.BASIN_SOUTH.append(float(TMP[4]))
          #print(self.BASIN_SOUTH)
          self.BASIN_NORTH.append(float(TMP[5]))
          #print(self.BASIN_NORTH)
          try: 
            self.BASIN_ZMIN.append(float(TMP[6]))
          except:
            self.BASIN_ZMIN.append(None)
          #print(self.BASIN_ZMIN)
          try:
            self.BASIN_ZMAX.append(float(TMP[7]))
          except:
            self.BASIN_ZMAX.append(None)
          #print(self.BASIN_ZMAX)
          self.BASIN_DATEMIN.append(TMP[8])
          #print(self.BASIN_DATEMIN)
          self.BASIN_DATEO.append(dparser.parse(TMP[8]))
          #print(self.BASIN_DATEO)
          self.BASIN_MOTU.append(TMP[9])
          #print(self.BASIN_MOTU)
          self.BASIN_SERVICE.append(TMP[10])
          #print(self.BASIN_SERVICE)
          self.BASIN_VARS.append(TMP[11][:-1])
          #print(self.BASIN_VARS)
          self.Nbasins += 1
        except:
          pass
      print('done')

      f.close()
      print('Closing products.conf')

    else:
      self.BASIN_LIST     = ['ATLANTIC','MEDITERRANEAN']
      self.BASIN_PRODUCT  = ['GLOBAL_ANALYSIS_FORECAST_PHY_001_024',  \
                             'MEDSEA_ANALYSIS_FORECAST_PHY_006_013-TDS']
      self.BASIN_WEST     = [-180.0, -15.0]
      self.BASIN_EAST     = [ 179.9,  36.0]
      self.BASIN_SOUTH    = [ -80.0,  30.5]
      self.BASIN_NORTH    = [  90.0,  45.9]
      self.BASIN_ZMIN     = [ 0.492, 1.017]
      self.BASIN_ZMAX     = [ 0.494, 1.019]
      

    self.BASIN          = tk.StringVar()
    self.PRODUCT        = tk.StringVar()
    self.WEST           = tk.StringVar()
    self.EAST           = tk.StringVar()
    self.SOUTH          = tk.StringVar()
    self.NORTH          = tk.StringVar()
    self.ZMIN           = tk.StringVar()
    self.ZMAX           = tk.StringVar()
    self.INITIAL        = tk.StringVar()
    self.FINAL          = tk.StringVar()

    self.BASIN_ID = 0
    self.BASIN.set(self.BASIN_LIST[self.BASIN_ID])
    self.PRODUCT.set(self.BASIN_PRODUCT[self.BASIN_ID])
    self.WEST.set(self.BASIN_WEST[self.BASIN_ID])
    self.EAST.set(self.BASIN_EAST[self.BASIN_ID])
    self.SOUTH.set(self.BASIN_SOUTH[self.BASIN_ID])
    self.NORTH.set(self.BASIN_NORTH[self.BASIN_ID])
    self.ZMIN.set(self.BASIN_ZMIN[self.BASIN_ID])
    self.ZMAX.set(self.BASIN_ZMAX[self.BASIN_ID])

    self.Nregions = 0
    self.Rname  = []
    self.Rwest  = []
    self.Reast  = []
    self.Rsouth = []
    self.Rnorth = []

    self.Nregions = 1
    self.Rindex   = 0
    self.Rname.append('Default')
    self.Rwest.append(None)
    self.Reast.append(None)
    self.Rsouth.append(None)
    self.Rnorth.append(None)
    try:
      f = open('regions.conf','r')
      for line in f:
        self.Nregions += 1
        self.Rname.append(line.split(',')[0])
        self.Rwest.append(float(line.split(',')[1]))
        self.Reast.append(float(line.split(',')[2]))
        self.Rsouth.append(float(line.split(',')[3]))
        self.Rnorth.append(float(line[0:-1].split(',')[4]))
      f.close()
      sekf.Rname.append('')
    except:
      pass
      
    self.REGION = tk.StringVar()
    self.REGION.set(self.Rname[self.Rindex])


    self.DATA           = tk.StringVar()
    self.VAR            = tk.StringVar()


    self.YEAR           = tk.IntVar()
    self.MONTH          = tk.IntVar()
    self.DAY            = tk.IntVar()
    self.HOUR           = tk.IntVar()
    self.INDEX          = 0
    self.PROD_LIST      = ['SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001-TDS', \
                           'MEDESEA_ANALYSIS_FORECAST_PHYS_006_001-TDS']
    self.DATA_LIST      = ['METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2', \
                           'cmemsv02-med-ingv-cur-an-fc-h']
    self.VAR_LIST       = ['-v mask -v analysed_sst', \
                           '-v sowavenu -v somestdy -v vomecrty -v sozostdx -v vozocrtx']

    # Get today's date: year, month, day and hour (set to zero):
    self.TODAY = datetime.datetime.now()
    self.YESTERDAY = datetime.datetime.now() - datetime.timedelta(days=1)
    self.THIS_YEAR  = self.YESTERDAY.date().year
    self.THIS_MONTH = self.YESTERDAY.date().month
    self.THIS_DAY   = self.YESTERDAY.date().day

    self.INITIAL.set('"%4d-%02d-%02d 00:00:00"' % (self.THIS_YEAR,  \
                                                 self.THIS_MONTH, \
                                                 self.THIS_DAY))
    self.FINAL.set('"%4d-%02d-%02d 12:00:00"' % (self.THIS_YEAR,  \
                                                 self.THIS_MONTH, \
                                                 self.THIS_DAY))

    # File menu:
    self.menubar = tk.Menu(self.master)
    menu = tk.Menu(self.menubar,tearoff=0)
    self.menubar.add_cascade(label='Configuration',menu=menu)
    menu.add_command(label='python motu client',command=self.paths)
    menu.add_command(label='Copernicus credentials',command=self.credentials)
    menu.add_command(label='Output file',command=self.output)
    try:
      self.master.config(menu=self.menubar)
    except AttributeError:
      # master is a toplevel window (Python 2.4/Tkinter 1.63)
      self.master.tk.call(master, "config", "-menu", self.menubar)


    #image = Image.open('cosmo-logo.png')
    #photo = ImageTk.PhotoImage(image)

    # Window construction:
    frame1 = tk.Frame(self.master,background='#87CEEB')

    ttk.Label(frame1,text='PRODUCT',font='Helvetica 12 bold',padding=3,background='#87CEEB').grid(row=0,column=0,sticky='w',padx=3)
    wprod = ttk.Combobox(frame1,textvariable=self.BASIN,width=15)
    wprod.grid(row=0,column=1,columnspan=3,sticky='ew',padx=3)
    wprod['values'] = self.BASIN_LIST
    wprod.bind('<<ComboboxSelected>>',lambda e: self.product_selection())
    ttk.Label(frame1,text='Name',padding=3,background='#87CEEB').grid(row=1,column=0,sticky='w',padx=3)
    ttk.Entry(frame1,textvariable=self.PRODUCT,width=60) \
       .grid(row=1,column=1,columnspan=12,sticky='ew',padx=3)


    ttk.Label(frame1,text='Region',pad=3,background='#87CEEB').grid(row=2,column=0,padx=3)
    wregion = ttk.Combobox(frame1,textvariable=self.REGION,width=20)
    wregion.grid(row=2,column=1,columnspan=4,sticky='ew',padx=3)
    wregion['values'] = self.Rname
    wregion.bind('<<ComboboxSelected>>',lambda e: self.region())
    frame1.grid(row=0,column=0,sticky='w')



    frame3 = tk.Frame(self.master,background='#87CEEB')
    ttk.Label(frame3,text='Longitude from',padding=3,background='#87CEEB').grid(row=0,column=0,sticky='w',padx=3)
    wwest = ttk.Entry(frame3,textvariable=self.WEST,width=10)
    wwest.grid(row=0,column=1,sticky='ew',padx=3)
    ttk.Label(frame3,text='to',padding=3,background='#87CEEB').grid(row=0,column=3,sticky='w',padx=3)
    weast = ttk.Entry(frame3,textvariable=self.EAST,width=10)
    weast.grid(row=0,column=4,sticky='ew',padx=3)


    ttk.Label(frame3,text='Latitude from',padding=3,background='#87CEEB')  \
       .grid(row=1,column=0,sticky='w',padx=3)
    wwest = ttk.Entry(frame3,textvariable=self.SOUTH,width=10)
    wwest.grid(row=1,column=1,sticky='ew',padx=3)
    ttk.Label(frame3,text='to',padding=3,background='#87CEEB')  \
       .grid(row=1,column=3,sticky='w',padx=3)
    weast = ttk.Entry(frame3,textvariable=self.NORTH,width=5)
    weast.grid(row=1,column=4,sticky='ew',padx=3)

    ttk.Label(frame3,text='Depth from',padding=3,background='#87CEEB')  \
       .grid(row=2,column=0,sticky='w',padx=3)
    wwest = ttk.Entry(frame3,textvariable=self.ZMIN,width=10)
    wwest.grid(row=2,column=1,sticky='ew',padx=3)
    ttk.Label(frame3,text='to',padding=3,background='#87CEEB')  \
       .grid(row=2,column=3,sticky='w',padx=3)
    weast = ttk.Entry(frame3,textvariable=self.ZMAX,width=10)
    weast.grid(row=2,column=4,sticky='ew',padx=3)

    ttk.Label(frame3,text='Time from',padding=3,background='#87CEEB')  \
       .grid(row=3,column=0,sticky='w',padx=3)
    wwest = ttk.Entry(frame3,textvariable=self.INITIAL,width=20)
    wwest.grid(row=3,column=1,sticky='ew',padx=3)
    ttk.Label(frame3,text='to',padding=3,background='#87CEEB')  \
       .grid(row=3,column=3,sticky='w',padx=3)
    weast = ttk.Entry(frame3,textvariable=self.FINAL,width=20)
    weast.grid(row=3,column=4,sticky='ew',padx=3)
    frame3.grid()


    frame4 = tk.Frame(self.master)
    ttk.Button(frame4,text='Save script',      \
                          command=self.script,\
                          padding=5).grid(row=1,column=3,sticky='e')
    ttk.Button(frame4,text='Get data',          \
                          command=self.get,    \
                          padding=5).grid(row=1,column=4,sticky='e')
    ttk.Button(frame4,text='Quit',        \
                          command=self.master.destroy,  \
                          padding=5).grid(row=1,column=5,sticky='e')
    frame4.grid(padx=5,pady=5)

    self.Window_pref = None
    self.Window_user = None
    self.Window_out  = None

  def out(self):
  #=============

    if empty(self.USER_FILE.get()):
      print('Here ...')
      print(self.BASIN.get())
      idate = self.INITIAL.get().replace(" ","").replace("-","") \
                                .replace('"','').replace(":","")
      fdate = self.FINAL.get().replace(" ","").replace("-","") \
                                .replace('"','').replace(":","")
      ofile = '%s-%s-%s.nc' % (self.BASIN.get().lower(),idate,fdate)
      self.FILE.set(ofile)
    else:
      self.FILE.set(self.USER_FILE.get())

    return self.FILE.get()

  def product_selection(self):
  # ==========================
    '''Select the product'''

    self.BASIN_ID = self.BASIN_LIST.index(self.BASIN.get()) 
    self.PRODUCT.set(self.BASIN_PRODUCT[self.BASIN_ID])
    self.WEST.set(self.BASIN_WEST[self.BASIN_ID])
    self.EAST.set(self.BASIN_EAST[self.BASIN_ID])
    self.SOUTH.set(self.BASIN_SOUTH[self.BASIN_ID])
    self.NORTH.set(self.BASIN_NORTH[self.BASIN_ID])
    self.ZMIN.set(self.BASIN_ZMIN[self.BASIN_ID])
    self.ZMAX.set(self.BASIN_ZMAX[self.BASIN_ID])


  def region(self):
  # ===============
    ''' Select a region '''

    ind = self.Rname.index(self.REGION.get())
    if ind == 0:
      self.WEST.set(self.BASIN_WEST[self.BASIN_ID])
      self.EAST.set(self.BASIN_EAST[self.BASIN_ID])
      self.SOUTH.set(self.BASIN_SOUTH[self.BASIN_ID])
      self.NORTH.set(self.BASIN_NORTH[self.BASIN_ID])
    else:
      self.WEST.set(self.Rwest[ind])
      self.EAST.set(self.Reast[ind])
      self.SOUTH.set(self.Rsouth[ind])
      self.NORTH.set(self.Rnorth[ind])


  def save(self):
  # =============
    try:
      self.python   = self.wpe.get()
      self.motu     = self.wmc.get()
    except:
      pass
    try:
      self.user     = self.wus.get()
      self.password = self.wpa.get()
    except:
      pass
    f = open('copernicus.conf','w')
    f.write('%s\n' % self.python)
    f.write('%s\n' % self.motu)
    f.write('%s\n' % self.user)
    f.write('%s\n' % self.password)
    f.write('%s\n' % self.PATH.get())
    f.write('%s\n' % self.FILE.get())
    f.close()

  def output(self):
    '''Change output path and filename'''
    def close():
      self.Window_out.destroy()
      self.Window_out = None

    if self.Window_out is None:
      self.Window_out = tk.Toplevel(self.master)
      self.Window_out.title('Output filename')
      self.Window_out.resizable(False,False)
      self.Window_out.protocol('WM_DELETE_WINDOW',close)
    else:
      self.Window_out.lift()
    tk.Label(self.Window_out,text='Output folder',font='bold').grid(row=0,column=0,padx=3,pady=3,sticky='w')
    self.wop = tk.Entry(self.Window_out,textvariable=self.PATH,width=60)
    self.wop.grid(row=0,column=1,columnspan=2,padx=3,pady=3,sticky='w')

    tk.Label(self.Window_out,text='Output filename',font='bold').grid(row=1,column=0,padx=3,pady=3,sticky='w')
    self.wof = tk.Entry(self.Window_out,textvariable=self.USER_FILE,width=60)
    self.wof.grid(row=1,column=1,columnspan=2,padx=3,pady=3,sticky='w')

    ttk.Button(self.Window_out,text='Save',command=self.save).grid(row=2,column=2,padx=3,pady=3)
    ttk.Button(self.Window_out,text='Done',command=close).grid(row=2,column=3,padx=3,pady=3)


  def credentials(self):
    '''Change user and password'''
    def close():
      self.Window_user.destroy()
      self.Window_user = None
    def entry_user():
      self.user = self.wus.get()
    def entry_pass():
      self.password = self.wpa.get()

    if self.Window_user is None:
      self.Window_user = tk.Toplevel(self.master)
      self.Window_user.title('COPERNICUS CREDENTIALS')
      self.Window_user.resizable(False,False)
      self.Window_user.protocol('WM_DELETE_WINDOW',close)
    else:
      self.Window_user.lift()
    tk.Label(self.Window_user,text='Username',font='bold').grid(row=0,column=0,padx=3,pady=3,sticky='w')
    self.wus = tk.Entry(self.Window_user,width=60)
    self.wus.grid(row=0,column=1,columnspan=2,padx=3,pady=3,sticky='w')
    self.wus.insert(0,self.user)
    self.wus.bind('<Enter>', lambda f: entry_user())

    tk.Label(self.Window_user,text='Password',font='bold').grid(row=1,column=0,padx=3,pady=3,sticky='w')
    self.wpa = tk.Entry(self.Window_user,width=60)
    self.wpa.grid(row=1,column=1,columnspan=2,padx=3,pady=3,sticky='w')
    self.wpa.insert(0,self.password)
    self.wpa.bind('<Enter>', lambda f: entry_pass())

    ttk.Button(self.Window_user,text='Save',command=self.save).grid(row=2,column=2,padx=3,pady=3)
    ttk.Button(self.Window_user,text='Done',command=close).grid(row=2,column=3,padx=3,pady=3)


  def paths(self):
    def close():
      self.Window_pref.destroy()
      self.Window_pref = None
    def entry_python():
      self.python = self.wpe.get()
    def entry_motu():
      self.motu = self.wmc.get()

    if self.Window_pref is None:
      self.Window_pref = tk.Toplevel(self.master)
      self.Window_pref.title('PYTHON MOTU CLIENT PATHS')
      self.Window_pref.resizable(False,False)
      self.Window_pref.protocol('WM_DELETE_WINDOW',close)
    else:
      self.Window_pref.lift()
    tk.Label(self.Window_pref,text='python',font='bold').grid(row=0,column=0,padx=3,pady=3,sticky='w')
    self.wpe = tk.Entry(self.Window_pref,width=60)
    self.wpe.grid(row=0,column=1,columnspan=2,padx=3,pady=3,sticky='w')
    self.wpe.insert(0,self.python)
    self.wpe.bind('<Enter>', lambda f: entry_python())

    tk.Label(self.Window_pref,text='motu client',font='bold').grid(row=1,column=0,padx=3,pady=3,sticky='w')
    self.wmc = tk.Entry(self.Window_pref,width=60)
    self.wmc.grid(row=1,column=1,columnspan=2,padx=3,pady=3,sticky='w')
    self.wmc.insert(0,self.motu)
    self.wmc.bind('<Enter>', lambda f: entry_motu())

    ttk.Button(self.Window_pref,text='Save',command=self.save).grid(row=2,column=2,padx=3,pady=3)
    ttk.Button(self.Window_pref,text='Done',command=close).grid(row=2,column=3,padx=3,pady=3)
    

  def script(self,filename=None):
  # =============================
    if filename is None:
      nn = filedialog.asksaveasfilename(title='Save',confirmoverwrite=True)
      if nn is not None:
        filename = '%s' % nn
        print('Saving in ',filename)
      else:
        return

    f = open(filename,'w')
    f.write('#\n')
    f.write('%s' % self.python)
    f.write(' %s' % self.motu)
    f.write(' --user %s' % self.user)
    f.write(' --pwd %s' % self.password)
    f.write(' --motu %s' % self.BASIN_MOTU[self.BASIN_ID])
    f.write(' --service-id %s' % self.BASIN_PRODUCT[self.BASIN_ID])
    f.write(' --product-id %s' % self.BASIN_SERVICE[self.BASIN_ID])
    f.write(' --longitude-min %s' % self.WEST.get())
    f.write(' --longitude-max %s' % self.EAST.get())
    f.write(' --latitude-min %s' % self.SOUTH.get())
    f.write(' --latitude-max %s' % self.NORTH.get())
    try:
      zmin = float(self.ZMIN.get())
      f.write(' --depth-min %s' % self.ZMIN.get())
    except:
      pass
    try:
      zmax = float(self.ZMAX.get())
      f.write(' --depth-max %s' % self.ZMAX.get())
    except:
      pass
    f.write(' --date-min %s' % self.INITIAL.get())
    f.write(' --date-max %s' % self.FINAL.get())
    f.write(' %s' % self.BASIN_VARS[self.BASIN_ID])
    f.write(' -o %s' % self.PATH.get())

    # Get the output file:
    self.out()
    f.write(' -f %s' % self.FILE.get())
    f.write('\n')
    f.close()
    os.system('chmod +x '+filename)


  def get(self):
  # ============
    self.script(filename='.tmp.sh')
    os.system('cat .tmp.sh')
    os.system('chmod +x .tmp.sh')
    os.system('./.tmp.sh')
    os.system('rm -f .tmp.sh')
    messagebox.showinfo(message='Download finished')


def main():

  image = Image.open('cosmo-logo.png')

  root = tk.Tk()
  root.title('COPERNICUS')
  root.resizable(False,False)
  root.configure(background='#87CEEB')
  root.protocol('WM_DELETE_WINDOW',quit)
  photo = ImageTk.PhotoImage(image)
  root.tk.call('wm','iconphoto',root._w,photo)

  WinTracking(root)
  root.mainloop()

if __name__ == '__main__':
  main()


 
