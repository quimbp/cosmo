''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems'''

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox


import datetime
from datetime import timedelta
from calendar import monthrange
import wget


# =====================
class PROVIDER_CLASS():
# =====================
  ''' Parameters for operational system's providors'''

  __version__ = "1.2"
  __author__  = "Quim Ballabrera"
  __date__    = "December 2017"

  def __init__(self):
  # =================

    self.NAME = tk.StringVar()
    self.URL      = tk.StringVar()
    self.FILENAME = tk.StringVar()

    self.NAME.set('')
    self.URL.set('')
    self.FILENAME.set('')

    self.TODAY = datetime.datetime.now().date()
    self.THIS_YEAR  = self.TODAY.year
    self.THIS_MONTH = self.TODAY.month
    self.THIS_DAY   = self.TODAY.day
    self.YEAR  = self.TODAY.year
    self.MONTH = self.TODAY.month
    self.DAY   = self.TODAY.day
    self.HOUR  = 0

    # Parameters relevant to the data providers:
    self.LIST = ['SAMPA','SOCIB']
    self.INIT_YEAR  = [2016, 2013]
    self.INIT_MONTH = [11, 8]
    self.DPATH = ['http://opendap.puertos.es/thredds/fileServer/circulation_regional_gib/', \
                 'http://thredds.socib.es/thredds/fileServer/operational_models/oceanographical/hydrodynamics/wmop/']
    self.PATH = ['http://opendap.puertos.es/thredds/dodsC/circulation_regional_gib/', \
                 'http://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop/']

    self.NAME  = self.LIST[0]
    self.YEAR_INI  = self.INIT_YEAR[0]
    self.MONTH_INI = self.INIT_MONTH[0]

    # Fill the default pull-down menu lists to select the data of the 
    # simulation.
    self.YEAR_LIST   = list(range(self.YEAR_INI,self.THIS_YEAR+1))
    self.MONTH_LIST = list(range(1,self.THIS_MONTH+1))
    self.DAY_LIST   = list(range(1,self.THIS_DAY+1))
    self.HOUR_LIST = ('00','01','02','03','04','05','06','07','08','09','10',
            '11','12','13','14','15','16','17','18','19','20','21','22','23')

    # Some providers allow retrieving hourly-averaged or day-averaed
    # maps
    self.FREQUENCY_MODES = [ ('Daily','D'), ('Hourly','H') ]
    self.FREQUENCY = 'D'

    # Window widget
    self.Window_id = None


# ===============
class WinOpendap:
# ===============
  '''Provider selection widget'''

  def __init__ (self,master,PARAMS):
  # ================================

    # Backup parameters
    OLD_PROVIDER = PARAMS.NAME
    OLD_YEAR     = PARAMS.YEAR
    OLD_MONTH    = PARAMS.MONTH
    OLD_DAY      = PARAMS.DAY
    OLD_HOUR     = PARAMS.HOUR

    self.master = master
    self.frame = tk.Frame(self.master)
    #ttk.Label(self.frame,text='URL',font='Helvetica 12 bold').grid(row=0,column=0)
    #Wurl = ttk.Entry(self.frame,textvariable=PARAMS.URL,justify='left',width=63)
    #Wurl.grid(row=0,column=1,columnspan=5,sticky='E'+'W')

    ttk.Label(self.frame,text='Provider',font='Helvetica 12 bold').grid(row=1,column=0,sticky='w')

    self.provider_var = tk.StringVar()
    self.providerbox = ttk.Combobox(self.frame,textvariable=self.provider_var,width=12)
    self.providerbox['values'] = PARAMS.LIST
    self.provider_var.set(PARAMS.NAME)
    self.providerbox.bind('<<ComboboxSelected>>',lambda e: self.provider_selection(PARAMS))
    self.providerbox.grid(row=1,column=1,sticky='w')

    #self.year_var = tk.StringVar()
    self.year_var = tk.IntVar()
    self.yearbox = ttk.Combobox(self.frame,textvariable=self.year_var,width=12)
    self.yearbox['values'] = PARAMS.YEAR_LIST
    self.year_var.set(PARAMS.YEAR)
    self.yearbox.bind('<<ComboboxSelected>>',lambda e: self.provider_year(PARAMS))
    self.yearbox.grid(row=1,column=2,sticky='w')

    #self.month_var = tk.StringVar()
    self.month_var = tk.IntVar()
    self.monthbox = ttk.Combobox(self.frame,textvariable=self.month_var,width=12)
    self.monthbox.bind('<<ComboboxSelected>>',lambda e: self.provider_month(PARAMS))
    self.monthbox['values'] = PARAMS.MONTH_LIST
    self.month_var.set(PARAMS.MONTH)
    self.monthbox.grid(row=1,column=3,sticky='w')

    #self.day_var = tk.StringVar()
    self.day_var = tk.IntVar()
    self.daybox = ttk.Combobox(self.frame,textvariable=self.day_var,width=12)
    self.daybox.bind('<<ComboboxSelected>>',lambda e: self.provider_day(PARAMS))
    self.daybox['values'] = PARAMS.DAY_LIST
    self.day_var.set(PARAMS.DAY)
    self.daybox.grid(row=1,column=4,sticky='w')

    #self.hour_var = tk.StringVar()
    self.hour_var = tk.IntVar()
    self.hourbox = ttk.Combobox(self.frame,textvariable=self.hour_var,width=12)
    self.hourbox.bind('<<ComboboxSelected>>',lambda e: self.provider_hour(PARAMS))
    self.hourbox['values'] = PARAMS.HOUR_LIST
    self.hour_var.set(PARAMS.HOUR)
    self.hourbox.grid(row=1,column=5,sticky='w')

    self.freq_var = tk.StringVar()
    self.daily = ttk.Radiobutton(self.frame,text='Daily',variable=self.freq_var,value='D')
    self.hourly = ttk.Radiobutton(self.frame,text='Hourly',variable=self.freq_var,value='H')
    self.freq_var.set(PARAMS.FREQUENCY)
    self.daily.grid(row=2,column=2)
    self.hourly.grid(row=2,column=3)


    ttk.Button(self.frame,text='Cancel',      \
                          command=lambda : self.cancel(master,PARAMS),\
                          padding=5).grid(row=3,column=3,sticky='e')
    ttk.Button(self.frame,text='Download',      \
                          command=lambda : self.download(PARAMS),\
                          padding=5).grid(row=3,column=4,sticky='e')
    ttk.Button(self.frame,text='Done',        \
                          command=lambda : self.done(master,PARAMS),padding=5).grid(row=3,column=5,sticky='e')
    #ttk.Button(self.frame,text='Load',        \
    #                      command=lambda : self.done(master,PARAMS),padding=5).grid(row=3,column=5,sticky='e')

    self.frame.grid(padx=5,pady=5)

  def cancel(self,master,PARAMS):
    PARAMS.FILENAME.set('')
    master.destroy()
    master = None
    

  def download(self,PARAMS):
    if PARAMS.NAME == 'SOCIB':
      pp = 1
    else:
      pp = 0
    theurl = self.filename(PARAMS.DPATH[pp],PARAMS)
    print('Fetching ',theurl) 
    print('')
    try: 
      filename = wget.download(theurl)
      messagebox.showinfo(message='Download complete')
    except:
      messagebox.showinfo(message='Unable to download complete')


  def done(self,master,PARAMS):

    if PARAMS.NAME == 'SOCIB':
      pp = 1
    else:
      pp = 0
    
    theurl = self.filename(PARAMS.PATH[pp],PARAMS)
    PARAMS.FILENAME.set(theurl)
    try:
      PARAMS.Button_load.configure(state='!disabled')
    except:
      pass
    master.destroy()
    master = None


  def filename(self,PATH,PARAMS):

    PARAMS.NAME      = self.provider_var.get()
    PARAMS.YEAR      = self.year_var.get()
    PARAMS.MONTH     = self.month_var.get()
    PARAMS.DAY       = self.day_var.get()
    PARAMS.HOUR      = self.hour_var.get()
    PARAMS.FREQUENCY = self.freq_var.get()
    if PARAMS.NAME     == 'SOCIB':
      theurl = PATH+str(PARAMS.YEAR)+'/' + '%02d'%PARAMS.MONTH+'/roms_wmop_' + \
               str(PARAMS.YEAR)+'%02d'%PARAMS.MONTH+'%02d'%PARAMS.DAY+'.nc'
    else:
      dini = datetime.date(PARAMS.YEAR,PARAMS.MONTH,PARAMS.DAY)
      dmid = dini + timedelta(days=1)
      dfin = dini + timedelta(days=2)
      if PARAMS.FREQUENCY == 'H':
        theurl = PATH+str(PARAMS.YEAR) + '/' + '%02d' % PARAMS.MONTH + \
                '/SAMGIB-PdE-hm-'+str(PARAMS.YEAR)+'%02d'%PARAMS.MONTH+\
                '%02d' % PARAMS.DAY+'00-'+str(dfin.year)+ \
                '%02d'%dfin.month+'%02d'%dfin.day+'23-B'+ \
                str(PARAMS.YEAR)+'%02d'%PARAMS.MONTH+ \
                '%02d'%PARAMS.DAY+'00-FC.nc'
      else:
        theurl = PATH+str(PARAMS.YEAR)+'/'+'%02d'%PARAMS.MONTH + \
                '/SAMGIB-PdE-dm-'+str(PARAMS.YEAR)+'%02d'%PARAMS.MONTH+\
                '%02d'%PARAMS.DAY+'00-'+str(dfin.year)+ \
                '%02d'%dfin.month+'%02d'%dfin.day+'23-B'+ \
                str(PARAMS.YEAR)+'%02d'%PARAMS.MONTH+\
                '%02d'%PARAMS.DAY+'00-FC.nc'
    print(theurl)
    return theurl

  def provider_selection(self,PARAMS):

    PARAMS.NAME = self.providerbox.get()

    if PARAMS.NAME == 'SAMPA':
      PARAMS.YEAR_INI  = PARAMS.INIT_YEAR[0]
      PARAMS.MONTH_INI = PARAMS.INIT_MONTH[0]
      self.freq_var.set(PARAMS.FREQUENCY)
      self.daily.configure(state='!disabled')
      self.hourly.configure(state='!disabled')

    if PARAMS.NAME == 'SOCIB':
      PARAMS.YEAR_INI  = PARAMS.INIT_YEAR[1]
      PARAMS.MONTH_INI = PARAMS.INIT_MONTH[1]
      self.freq_var.set('D')
      self.daily.configure(state='disabled')
      self.hourly.configure(state='disabled')

    PARAMS.YEAR_LIST   = list(range(PARAMS.YEAR_INI,PARAMS.THIS_YEAR+1))
    self.yearbox['values'] = PARAMS.YEAR_LIST

    PARAMS.YEAR = PARAMS.YEAR_INI if PARAMS.YEAR < PARAMS.YEAR_INI else PARAMS.YEAR
    self.year_var.set(PARAMS.YEAR)

    if PARAMS.YEAR == PARAMS.THIS_YEAR:
      PARAMS.MONTH = PARAMS.THIS_MONTH if PARAMS.MONTH > PARAMS.THIS_MONTH else PARAMS.MONTH
      PARAMS.MONTH_LIST = list(range(1,PARAMS.THIS_MONTH+1))
    elif PARAMS.YEAR == PARAMS.YEAR_INI:
      if PARAMS.MONTH < PARAMS.MONTH_INI:
        PARAMS.MONTH = PARAMS.MONTH_INI
      PARAMS.MONTH_LIST = list(range(PARAMS.MONTH_INI,13))
    else:
      PARAMS.MONTH_LIST = list(range(1,13))

    self.month_var.set(PARAMS.MONTH)
    self.monthbox['values'] = PARAMS.MONTH_LIST

    PARAMS.DAY_LIST = list(range(1,monthrange(PARAMS.YEAR,PARAMS.MONTH)[1]+1))
    if PARAMS.YEAR == PARAMS.THIS_YEAR and PARAMS.MONTH == PARAMS.THIS_MONTH:
      PARAMS.DAY_LIST   = list(range(1,PARAMS.THIS_DAY+1))
      if PARAMS.DAY > PARAMS.THIS_DAY:
        PARAMS.DAY = PARAMS.THIS_DAY
    else:
      PARAMS.DAY_LIST = list(range(1,monthrange(PARAMS.YEAR,PARAMS.MONTH)[1]+1))

    self.day_var.set(PARAMS.DAY)
    self.daybox['values'] = PARAMS.DAY_LIST
        
   
  def provider_year(self,PARAMS):

    PARAMS.YEAR = int(self.yearbox.get())

    if PARAMS.YEAR == PARAMS.THIS_YEAR:
      PARAMS.MONTH = PARAMS.THIS_MONTH if PARAMS.MONTH > PARAMS.THIS_MONTH else PARAMS.MONTH
      PARAMS.MONTH_LIST = list(range(1,PARAMS.THIS_MONTH+1))
    elif PARAMS.YEAR == PARAMS.YEAR_INI:
      if PARAMS.MONTH < PARAMS.MONTH_INI:
        PARAMS.MONTH = PARAMS.MONTH_INI
      PARAMS.MONTH_LIST = list(range(PARAMS.MONTH_INI,13))
    else:
      PARAMS.MONTH_LIST = list(range(1,13))

    self.month_var.set(PARAMS.MONTH)
    self.monthbox['values'] = PARAMS.MONTH_LIST

    PARAMS.DAY_LIST = list(range(1,monthrange(PARAMS.YEAR,PARAMS.MONTH)[1]+1))
    if PARAMS.YEAR == PARAMS.THIS_YEAR and PARAMS.MONTH == PARAMS.THIS_MONTH:
      PARAMS.DAY_LIST   = list(range(1,PARAMS.THIS_DAY+1))
      if PARAMS.DAY > PARAMS.THIS_DAY:
        PARAMS.DAY = PARAMS.THIS_DAY
    else:
      PARAMS.DAY_LIST = list(range(1,monthrange(PARAMS.YEAR,PARAMS.MONTH)[1]+1))

    self.day_var.set(PARAMS.DAY)
    self.daybox['values'] = PARAMS.DAY_LIST
        
  def provider_month(self,PARAMS):

    PARAMS.MONTH = int(self.monthbox.get())

    if PARAMS.YEAR == PARAMS.THIS_YEAR and PARAMS.MONTH == PARAMS.THIS_MONTH:
      PARAMS.DAY_LIST   = list(range(1,PARAMS.THIS_DAY+1))
      if PARAMS.DAY > PARAMS.THIS_DAY:
        PARAMS.DAY = PARAMS.THIS_DAY
        self.day_val.set(PARAMS.DAY)
    else:
      PARAMS.DAY_LIST = list(range(1,monthrange(PARAMS.YEAR,PARAMS.MONTH)[1]+1))
    self.daybox['values'] = PARAMS.DAY_LIST
        
        
  def provider_day(self,PARAMS):

    PARAMS.DAY = int(self.daybox.get())
    
  def provider_hour(self,PARAMS):

    PARAMS.HOUR = int(self.hourbox.get())

def main():

  global PARAMS

  root = tk.Tk()
  root.title('Load Opendap File')
  PARAMS = parameters()
  
  app = WinOpendap(root,PARAMS)
  root.mainloop()


if __name__ == '__main__':
  main()


 
