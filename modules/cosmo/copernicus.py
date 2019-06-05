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
import io
import json
import ftplib

__version__ = "1.0"
__author__  = "Quim Ballabrera"
__date__    = "July 2018"

HOME = os.path.expanduser('~') + os.sep
COSMO_ROOT = os.environ.get('COSMO',HOME+'cosmo') + os.sep
COSMO_CONF = COSMO_ROOT + 'conf' + os.sep 

try:
  to_unicode = unicode
except:
  to_unicode = str

def empty(string):
  if bool(string.strip()):
    return False
  else:
    return True


def conf_save(conf,filename):
# ======================
  with io.open(filename,'w',encoding='utf8') as outfile:
    str_ = json.dumps(conf,ensure_ascii=False,    \
                           sort_keys=True,        \
                           indent=2,              \
                           separators=(',',': '))
    outfile.write(to_unicode(str_))


class WinTracking():

  def __init__ (self,master):

    self.master         = master

    self.PATH           = tk.StringVar()
    self.FILE           = tk.StringVar()
    self.USER_FILE      = tk.StringVar()
    self.USER_FILE.set('')

    # Connection credentials
    #
    print('Loading credentials')
    try:
      with open(COSMO_CONF+'copernicus-credentials.conf','r') as infile:
        conf = json.load(infile)
      self.python = conf['PYTHON']
      self.motu = conf['MOTU-CLIENT']
      self.user = conf['USER']
      self.password = conf['PASSWD']
      self.PATH.set(conf['PATH'])
    except:
      print('Failed')
      self.python = '/usr/bin/python'
      self.motu = '/usr/local/src/motu-client-python/motu-client.py'
      self.user = 'USERNAME_REQUIRED'
      self.password = 'PASSWORD_REQUIRED'
      self.PATH.set('.' + os.sep)


    # Define PRODUCTS: Example: GLOBAL, IBI, etc.
    #
    self.Nprod = 0
    self.PROD_NAME    = []
    self.PROD_MOTU    = []
    self.PROD_PRODUCT = []
    self.PROD_WEST    = []
    self.PROD_EAST    = []
    self.PROD_SOUTH   = []
    self.PROD_NORTH   = []
    self.PROD_ZMIN    = []
    self.PROD_ZMAX    = []
    self.PROD_DATEMIN = []
    self.PROD_DATE    = []
    self.PROD_MOTU    = []
    self.PROD_SERVICE = []
    self.PROD_VARS    = []

    print('Loading product list')
    if os.path.isfile(COSMO_CONF+'copernicus-products.conf'):
      with open(COSMO_CONF+'copernicus-products.conf','r') as infile:
        conf = json.load(infile)
      self.Nprod = len(conf)
      for i in range(self.Nprod):
        self.PROD_NAME.append(conf[i]['NAME'])
        self.PROD_MOTU.append(conf[i]['MOTU-SERVER'])
        self.PROD_SERVICE.append(conf[i]['SERVICE-ID'])
        self.PROD_PRODUCT.append(conf[i]['PRODUCT-ID'])
        self.PROD_WEST.append(float(conf[i]['BBOX'][0]))
        self.PROD_EAST.append(float(conf[i]['BBOX'][1]))
        self.PROD_SOUTH.append(float(conf[i]['BBOX'][2]))
        self.PROD_NORTH.append(float(conf[i]['BBOX'][3]))
        try:
          self.PROD_ZMIN.append(float(conf[i]['DEPTH-RANGE'][0]))
        except:
          self.PROD_ZMIN.append(None)
        try:
          self.PROD_ZMAX.append(float(conf[i]['DEPTH-RANGE'][1]))
        except:
          self.PROD_ZMAX.append(None)
        self.PROD_DATE.append(dparser.parse(conf[i]['INITIAL-DATE']))
        self.PROD_VARS.append(conf[i]['VARIABLES'])

    else:
      print('Products list not found')
      self.PROD_NAME     = ['GLOBAL','IBI','MEDSEA']
      self.PROD_SERVICE  = ['GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS',  \
                             'IBI_ANALYSIS_FORECAST_PHYS_005_001-TDS', \
                             'MEDSEA_ANALYSIS_FORECAST_PHY_006_013-TDS']
      self.PROD_WEST     = [-180.0, -19.0, -17.29]
      self.PROD_EAST     = [ 179.92, 5.0, 36.29]
      self.PROD_SOUTH    = [ -80.0, 26.0, 30.1875]
      self.PROD_NORTH    = [  90.0, 56.0,  45.98]
      self.PROD_ZMIN     = [ 0.492, None, 1.017]
      self.PROD_ZMAX     = [ 0.494, None, 1.019]
      

    self.PROD_NAME.append('In-situ Moorings')
    self.PROD           = tk.StringVar()
    self.MOTU           = tk.StringVar()
    self.PRODUCT        = tk.StringVar()
    self.SERVICE        = tk.StringVar()
    self.WEST           = tk.StringVar()
    self.EAST           = tk.StringVar()
    self.SOUTH          = tk.StringVar()
    self.NORTH          = tk.StringVar()
    self.ZMIN           = tk.StringVar()
    self.ZMAX           = tk.StringVar()
    self.INITIAL        = tk.StringVar()
    self.FINAL          = tk.StringVar()

    self.PROD_ID = 0
    self.PROD.set(self.PROD_NAME[self.PROD_ID])
    self.MOTU.set(self.PROD_MOTU[self.PROD_ID])
    self.PRODUCT.set(self.PROD_PRODUCT[self.PROD_ID])
    self.SERVICE.set(self.PROD_SERVICE[self.PROD_ID])
    self.WEST.set(self.PROD_WEST[self.PROD_ID])
    self.EAST.set(self.PROD_EAST[self.PROD_ID])
    self.SOUTH.set(self.PROD_SOUTH[self.PROD_ID])
    self.NORTH.set(self.PROD_NORTH[self.PROD_ID])
    self.ZMIN.set(self.PROD_ZMIN[self.PROD_ID])
    self.ZMAX.set(self.PROD_ZMAX[self.PROD_ID])

    # Define Regions: Example: Mediterranean, West Mediterranean
    #
    self.Rname  = ['Default']
    self.Rwest  = [None]
    self.Reast  = [None]
    self.Rsouth = [None]
    self.Rnorth = [None]

    print('Loading regions list')
    if os.path.isfile(COSMO_CONF+'copernicus-regions.conf'):
      with open(COSMO_CONF+'copernicus-regions.conf','r') as infile:
        conf = json.load(infile)
      for i in range(len(conf)):
        self.Rname.append(conf[i]['NAME'])
        self.Rwest.append(float(conf[i]['BBOX'][0]))
        self.Reast.append(float(conf[i]['BBOX'][1]))
        self.Rsouth.append(float(conf[i]['BBOX'][2]))
        self.Rnorth.append(float(conf[i]['BBOX'][3]))
    else:
      print('Regions list not found')

    self.Nregions = len(self.Rname)
    self.Rindex   = 0

    self.REGION = tk.StringVar()
    self.REGION.set(self.Rname[self.Rindex])


    self.DATA           = tk.StringVar()
    self.VAR            = tk.StringVar()

    self.TODAY = datetime.datetime.now()
    self.YESTERDAY  = datetime.datetime.now() - datetime.timedelta(days=1)
    self.PREDICTION = datetime.datetime.now() + datetime.timedelta(days=4)
    self.INIT_YEAR  = self.TODAY.date().year
    self.INIT_MONTH = self.TODAY.date().month
    self.INIT_DAY   = self.TODAY.date().day
    self.NEXT_YEAR  = self.PREDICTION.date().year
    self.NEXT_MONTH = self.PREDICTION.date().month
    self.NEXT_DAY   = self.PREDICTION.date().day

    self.INITIAL.set('"%4d-%02d-%02d 00:00:00"' % (self.INIT_YEAR,  \
                                                   self.INIT_MONTH, \
                                                   self.INIT_DAY))
    self.FINAL.set('"%4d-%02d-%02d 12:00:00"' % (self.NEXT_YEAR,  \
                                                 self.NEXT_MONTH, \
                                                 self.NEXT_DAY))

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


    # Window construction:
    frame1 = tk.Frame(self.master,background='#87CEEB')

    ttk.Label(frame1,text='PRODUCT',font='Helvetica 12 bold',padding=3,background='#87CEEB').grid(row=0,column=0,sticky='w',padx=3)
    wprod = ttk.Combobox(frame1,textvariable=self.PROD,width=15)
    wprod.grid(row=0,column=1,columnspan=3,sticky='ew',padx=3)
    wprod['values'] = self.PROD_NAME
    wprod.bind('<<ComboboxSelected>>',lambda e: self.product_selection())
    ttk.Label(frame1,text='Name',padding=3,background='#87CEEB').grid(row=1,column=0,sticky='e',padx=3)
    ttk.Entry(frame1,textvariable=self.PRODUCT,width=60) \
       .grid(row=1,column=1,columnspan=12,sticky='ew',padx=3)


    ttk.Label(frame1,text='Region',pad=3,background='#87CEEB').grid(row=2,column=0,padx=3,sticky='e')
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
      print(self.PROD.get())
      idate = self.INITIAL.get().replace(" ","").replace("-","") \
                                .replace('"','').replace(":","")
      fdate = self.FINAL.get().replace(" ","").replace("-","") \
                                .replace('"','').replace(":","")
      ofile = '%s-%s-%s.nc' % (self.PROD.get().lower(),idate,fdate)
      self.FILE.set(ofile)
    else:
      self.FILE.set(self.USER_FILE.get())

    return self.FILE.get()

  def product_selection(self):
  # ==========================
    '''Select the product'''

    FTP  = 'nrt.cmems-du.eu'
    PATH = 'Core/INSITU_MED_NRT_OBSERVATIONS_013_035/history/mooring/' 
    self.station     = tk.StringVar()
    self.pathstation = tk.StringVar()

    def close():
      ftp.quit()
      self.Window_ftp.destroy()
      self.Window_ftp = None
    def select():
      self.pathstation.set(PATH+self.station.get())
    def get():
      print('Downloading file: ',self.pathstation.get())
      filename = self.station.get()
      with open(filename,'wb') as local_file:
        ftp.retrbinary('RETR '+self.station.get(),local_file.write)
      print('Done !')
       

    if self.PROD.get() == 'In-situ Moorings' :
      print('Retrieving folder contents ...')
      ftp = ftplib.FTP(FTP,self.user,self.password)
      ftp.cwd(PATH)
      mooring_list = ftp.nlst()

      self.station.set(mooring_list[0])
      self.pathstation.set(PATH+self.station.get())

      self.Window_ftp = tk.Toplevel(self.master)
      self.Window_ftp.title('In-situ MED Mooring sites')
      self.Window_ftp.resizable(False,False)
      self.Window_ftp.protocol('WM_DELETE_WINDOW',close)
      f0 = ttk.Frame(self.Window_ftp)
      ttk.Label(f0,text='Select mooring station').grid(row=0,column=0,
                columnspan=2,padx=3,sticky='e')
      wms = ttk.Combobox(f0,textvariable=self.station,
                   values=mooring_list,width=40,
                   justify='left')
      wms.grid(row=0,column=2,columnspan=4,padx=3,sticky='w')
      wms.bind('<<ComboboxSelected>>',lambda e: select())
      #ttk.Entry(f0,textvariable=self.pathstation,
      #          width=90,justify='left').grid(row=1,column=2,columnspan=9)
      ttk.Button(f0,text='Get a copy',command=get).grid(row=1,column=5,padx=3)
      ttk.Button(f0,text='Close',command=close).grid(row=1,column=6,padx=3)
      f0.grid()

    else:
      self.PROD_ID = self.PROD_NAME.index(self.PROD.get()) 
      self.PRODUCT.set(self.PROD_PRODUCT[self.PROD_ID])
      self.WEST.set(self.PROD_WEST[self.PROD_ID])
      self.EAST.set(self.PROD_EAST[self.PROD_ID])
      self.SOUTH.set(self.PROD_SOUTH[self.PROD_ID])
      self.NORTH.set(self.PROD_NORTH[self.PROD_ID])
      self.ZMIN.set(self.PROD_ZMIN[self.PROD_ID])
      self.ZMAX.set(self.PROD_ZMAX[self.PROD_ID])
      self.Rindex = 0
      self.REGION.set(self.Rname[self.Rindex])


  def region(self):
  # ===============
    ''' Select a region '''

    ind = self.Rname.index(self.REGION.get())
    if ind == 0:
      self.WEST.set(self.PROD_WEST[self.PROD_ID])
      self.EAST.set(self.PROD_EAST[self.PROD_ID])
      self.SOUTH.set(self.PROD_SOUTH[self.PROD_ID])
      self.NORTH.set(self.PROD_NORTH[self.PROD_ID])
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
   
    with open(COSMO_CONF+'copernicus-credentials.conf','r') as infile:
      conf = json.load(infile)

    conf['PYTHON'] = self.python
    conf['MOTU-CLIENT'] = self.motu
    conf['USER'] = self.user
    conf['PASSWD'] = self.password
    conf['PATH'] = self.PATH.get()
    conf_save(conf,COSMO_CONF+'copernicus-credentials.conf')
 

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
    ttk.Button(self.Window_out,text='Close',command=close).grid(row=2,column=3,padx=3,pady=3)


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
    def check_python():
      print('Checking the python command ...')
      aa = os.system(self.wpe.get()+' --version')
      if int(aa) == 0:
        pass
      else:
        print('WARNING: Python interpreter not found')
    def check_motu():
      print('Checking the motu client ...')
      aa = os.system(self.wpe.get()+' '+self.wmc.get()+' --version')
      if int(aa) == 0:
        pass
      else:
        print('WARNING: Motu client not found')


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
    ttk.Button(self.Window_pref,text='Check',
               command=check_python).grid(row=0,column=3,padx=3)

    tk.Label(self.Window_pref,text='motu client',font='bold').grid(row=1,column=0,padx=3,pady=3,sticky='w')
    self.wmc = tk.Entry(self.Window_pref,width=60)
    self.wmc.grid(row=1,column=1,columnspan=2,padx=3,pady=3,sticky='w')
    self.wmc.insert(0,self.motu)
    self.wmc.bind('<Enter>', lambda f: entry_motu())
    ttk.Button(self.Window_pref,text='Check',
               command=check_motu).grid(row=1,column=3,padx=3)

    ttk.Button(self.Window_pref,text='Save',command=self.save).grid(row=2,column=2,padx=3,pady=3)
    ttk.Button(self.Window_pref,text='Close',command=close).grid(row=2,column=3,padx=3,pady=3)
    

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
    f.write(' --motu %s' % self.PROD_MOTU[self.PROD_ID])
    f.write(' --service-id %s' % self.PROD_SERVICE[self.PROD_ID])
    f.write(' --product-id %s' % self.PROD_PRODUCT[self.PROD_ID])
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
    f.write(' %s' % self.PROD_VARS[self.PROD_ID])
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

  image = Image.open(COSMO_ROOT + 'conf' + os.sep + 'cosmo-logo.png')

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


 
