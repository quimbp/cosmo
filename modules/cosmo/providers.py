''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems
    EGL, 06/2020: Changes:
      A heap variable MESSAGE has been introduce to store "print" messages      
'''

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

import datetime
from datetime import timedelta
from calendar import monthrange
import wget
import os


# =====================
class parameters():
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
                 'http://thredds.socib.es/thredds/fileServer/operational_models/oceanographical/hydrodynamics/wmop_surface/']
    self.PATH = ['http://opendap.puertos.es/thredds/dodsC/circulation_regional_gib/', \
                 'http://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/']

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
    self.FREQUENCY = 'H'

# ===============
class WinOpendap:
# ===============
  '''Provider selection widget'''

  def __init__ (self,master):
  # ================================

    self.MESSAGE = ""

    self.PARAMS = parameters()

    self.HTML = ['http://opendap.puertos.es/thredds/catalog/circulation_regional_gib/catalog.html', \
                 'http://thredds.socib.es/thredds/catalog/operational_models/oceanographical/hydrodynamics/wmop_surface/catalog.html']

    self.master = master
    self.frame = tk.Frame(self.master)

    ttk.Label(self.frame,text='Provider',font='Helvetica 12 bold').grid(row=1,column=0,sticky='w')

    self.provider_var = tk.StringVar()
    self.providerbox = ttk.Combobox(self.frame,textvariable=self.provider_var,width=12)
    self.providerbox['values'] = self.PARAMS.LIST
    self.provider_var.set(self.PARAMS.NAME)
    self.providerbox.bind('<<ComboboxSelected>>',lambda e: self.provider_selection())
    self.providerbox.grid(row=1,column=1,sticky='w')

    #self.year_var = tk.StringVar()
    self.year_var = tk.IntVar()
    self.yearbox = ttk.Combobox(self.frame,textvariable=self.year_var,width=12)
    self.yearbox['values'] = self.PARAMS.YEAR_LIST
    self.year_var.set(self.PARAMS.YEAR)
    self.yearbox.bind('<<ComboboxSelected>>',lambda e: self.provider_year())
    self.yearbox.grid(row=1,column=2,sticky='w')

    #self.month_var = tk.StringVar()
    self.month_var = tk.IntVar()
    self.monthbox = ttk.Combobox(self.frame,textvariable=self.month_var,width=12)
    self.monthbox.bind('<<ComboboxSelected>>',lambda e: self.provider_month())
    self.monthbox['values'] = self.PARAMS.MONTH_LIST
    self.month_var.set(self.PARAMS.MONTH)
    self.monthbox.grid(row=1,column=3,sticky='w')

    #self.day_var = tk.StringVar()
    self.day_var = tk.IntVar()
    self.daybox = ttk.Combobox(self.frame,textvariable=self.day_var,width=12)
    self.daybox.bind('<<ComboboxSelected>>',lambda e: self.provider_day())
    self.daybox['values'] = self.PARAMS.DAY_LIST
    self.day_var.set(self.PARAMS.DAY)
    self.daybox.grid(row=1,column=4,sticky='w')

    #self.hour_var = tk.StringVar()
    self.hour_var = tk.IntVar()
    self.hourbox = ttk.Combobox(self.frame,textvariable=self.hour_var,width=12)
    self.hourbox.bind('<<ComboboxSelected>>',lambda e: self.provider_hour())
    self.hourbox['values'] = self.PARAMS.HOUR_LIST
    self.hour_var.set(self.PARAMS.HOUR)
    self.hourbox.grid(row=1,column=5,sticky='w')

    self.freq_var = tk.StringVar()
    self.daily = ttk.Radiobutton(self.frame,text='Daily',variable=self.freq_var,value='D')
    self.hourly = ttk.Radiobutton(self.frame,text='Hourly',variable=self.freq_var,value='H')
    self.freq_var.set(self.PARAMS.FREQUENCY)
    self.daily.grid(row=2,column=2)
    self.hourly.grid(row=2,column=3)


    ttk.Button(self.frame,text='Catalogue',                            \
                          command=self.visit,                          \
                          padding=5).grid(row=3,column=2,sticky='e')
    ttk.Button(self.frame,text='Cancel',                               \
                          command=self.cancel,                         \
                          padding=5).grid(row=3,column=3,sticky='e')
    ttk.Button(self.frame,text='Save Locally',                         \
                          command=self.download,                       \
                          padding=5).grid(row=3,column=4,sticky='e')
    ttk.Button(self.frame,text='Select',                               \
                          command=self.done,                           \
                          padding=5).grid(row=3,column=5,sticky='e')

    self.frame.grid(padx=5,pady=5)

  def cancel(self):
    self.PARAMS.FILENAME.set('')
    self.master.destroy()
    self.master = None
    
  def visit(self):
    if self.PARAMS.NAME == 'SOCIB':
      pp = 1
    else:
      pp = 0

    command = 'firefox ' + self.HTML[pp]
    os.system(command)

  def download(self):
    if self.PARAMS.NAME == 'SOCIB':
      pp = 1
    else:
      pp = 0
    theurl = self.filename(self.PARAMS.DPATH[pp])
    self.MESSAGE += 'Fetching '+ theurl
    #print('Fetching ',theurl) 
    #print('')
    try: 
      filename = wget.download(theurl)
      messagebox.showinfo(parent=self.master,message='Download complete')
    except:
      messagebox.showinfo(parent=self.master,message='Unable to complete download')

  def get_filename(self):
    return self.PARAMS.FILENAME.get()

  def done(self):

    if self.PARAMS.NAME == 'SOCIB':
      pp = 1
    else:
      pp = 0
    
    theurl = self.filename(self.PARAMS.PATH[pp])
    self.PARAMS.FILENAME.set(theurl)
    self.master.destroy()
    self.master = None

  def filename(self,PATH):

    self.PARAMS.NAME      = self.provider_var.get()
    self.PARAMS.YEAR      = self.year_var.get()
    self.PARAMS.MONTH     = self.month_var.get()
    self.PARAMS.DAY       = int(self.day_var.get())
    self.PARAMS.HOUR      = int(self.hour_var.get())
    self.PARAMS.FREQUENCY = self.freq_var.get()

    dini = datetime.date(self.PARAMS.YEAR,self.PARAMS.MONTH,self.PARAMS.DAY)
    if self.PARAMS.NAME     == 'SOCIB':
      if dini < datetime.date(2017,12,1):
        theurl = PATH+str(self.PARAMS.YEAR)+'/'+'%02d'%self.PARAMS.MONTH+ \
                '/roms_wmop_surface_' + str(self.PARAMS.YEAR)+               \
                '%02d'%self.PARAMS.MONTH+ '%02d'%self.PARAMS.DAY+'.nc'
      else:
        theurl = PATH+str(self.PARAMS.YEAR)+'/'+'%02d'%self.PARAMS.MONTH+ \
                '/roms_wmop_surface_' + str(self.PARAMS.YEAR)+               \
                '%02d'%self.PARAMS.MONTH+ '%02d'%self.PARAMS.DAY+'.nc'
    else:
      dmid = dini + timedelta(days=1)
      dfin = dini + timedelta(days=2)
      if self.PARAMS.FREQUENCY == 'H':
        theurl = PATH+str(self.PARAMS.YEAR) + '/' + '%02d' % self.PARAMS.MONTH + \
                '/SAMGIB-PdE-hm-'+str(self.PARAMS.YEAR)+'%02d'%self.PARAMS.MONTH+\
                '%02d' % self.PARAMS.DAY+'00-'+str(dfin.year)+ \
                '%02d'%dfin.month+'%02d'%dfin.day+'23-B'+ \
                str(self.PARAMS.YEAR)+'%02d'%self.PARAMS.MONTH+ \
                '%02d'%self.PARAMS.DAY+'00-FC.nc'
      else:
        theurl = PATH+str(self.PARAMS.YEAR)+'/'+'%02d'%self.PARAMS.MONTH + \
                '/SAMGIB-PdE-dm-'+str(self.PARAMS.YEAR)+'%02d'%self.PARAMS.MONTH+\
                '%02d'%self.PARAMS.DAY+'00-'+str(dfin.year)+ \
                '%02d'%dfin.month+'%02d'%dfin.day+'23-B'+ \
                str(self.PARAMS.YEAR)+'%02d'%self.PARAMS.MONTH+\
                '%02d'%self.PARAMS.DAY+'00-FC.nc'
    
    self.MESSAGE += theurl
    #print(theurl)
    return theurl

  def provider_selection(self):

    self.PARAMS.NAME = self.providerbox.get()

    if self.PARAMS.NAME == 'SAMPA':
      self.PARAMS.YEAR_INI  = self.PARAMS.INIT_YEAR[0]
      self.PARAMS.MONTH_INI = self.PARAMS.INIT_MONTH[0]
      self.freq_var.set(self.PARAMS.FREQUENCY)
      self.daily.configure(state='!disabled')
      self.hourly.configure(state='!disabled')

    if self.PARAMS.NAME == 'SOCIB':
      self.PARAMS.YEAR_INI  = self.PARAMS.INIT_YEAR[1]
      self.PARAMS.MONTH_INI = self.PARAMS.INIT_MONTH[1]
      self.freq_var.set('D')
      self.daily.configure(state='disabled')
      self.hourly.configure(state='disabled')

    self.PARAMS.YEAR_LIST   = list(range(self.PARAMS.YEAR_INI,self.PARAMS.THIS_YEAR+1))
    self.yearbox['values'] = self.PARAMS.YEAR_LIST

    self.PARAMS.YEAR = self.PARAMS.YEAR_INI if self.PARAMS.YEAR < self.PARAMS.YEAR_INI else self.PARAMS.YEAR
    self.year_var.set(self.PARAMS.YEAR)

    if self.PARAMS.YEAR == self.PARAMS.THIS_YEAR:
      self.PARAMS.MONTH = self.PARAMS.THIS_MONTH if self.PARAMS.MONTH > self.PARAMS.THIS_MONTH else self.PARAMS.MONTH
      self.PARAMS.MONTH_LIST = list(range(1,self.PARAMS.THIS_MONTH+1))
    elif self.PARAMS.YEAR == self.PARAMS.YEAR_INI:
      if self.PARAMS.MONTH < self.PARAMS.MONTH_INI:
        self.PARAMS.MONTH = self.PARAMS.MONTH_INI
      self.PARAMS.MONTH_LIST = list(range(self.PARAMS.MONTH_INI,13))
    else:
      self.PARAMS.MONTH_LIST = list(range(1,13))

    self.month_var.set(self.PARAMS.MONTH)
    self.monthbox['values'] = self.PARAMS.MONTH_LIST

    self.PARAMS.DAY_LIST = list(range(1,monthrange(self.PARAMS.YEAR,self.PARAMS.MONTH)[1]+1))
    if self.PARAMS.YEAR == self.PARAMS.THIS_YEAR and self.PARAMS.MONTH == self.PARAMS.THIS_MONTH:
      self.PARAMS.DAY_LIST   = list(range(1,self.PARAMS.THIS_DAY+1))
      if self.PARAMS.DAY > self.PARAMS.THIS_DAY:
        self.PARAMS.DAY = self.PARAMS.THIS_DAY
    else:
      self.PARAMS.DAY_LIST = list(range(1,monthrange(self.PARAMS.YEAR,self.PARAMS.MONTH)[1]+1))

    self.day_var.set(self.PARAMS.DAY)
    self.daybox['values'] = self.PARAMS.DAY_LIST
        
  def provider_year(self):

    self.PARAMS.YEAR = int(self.yearbox.get())

    if self.PARAMS.YEAR == self.PARAMS.THIS_YEAR:
      self.PARAMS.MONTH = self.PARAMS.THIS_MONTH if self.PARAMS.MONTH > self.PARAMS.THIS_MONTH else self.PARAMS.MONTH
      self.PARAMS.MONTH_LIST = list(range(1,self.PARAMS.THIS_MONTH+1))
    elif self.PARAMS.YEAR == self.PARAMS.YEAR_INI:
      if self.PARAMS.MONTH < self.PARAMS.MONTH_INI:
        self.PARAMS.MONTH = self.PARAMS.MONTH_INI
      self.PARAMS.MONTH_LIST = list(range(self.PARAMS.MONTH_INI,13))
    else:
      self.PARAMS.MONTH_LIST = list(range(1,13))

    self.month_var.set(self.PARAMS.MONTH)
    self.monthbox['values'] = self.PARAMS.MONTH_LIST

    self.PARAMS.DAY_LIST = list(range(1,monthrange(self.PARAMS.YEAR,self.PARAMS.MONTH)[1]+1))
    if self.PARAMS.YEAR == self.PARAMS.THIS_YEAR and self.PARAMS.MONTH == self.PARAMS.THIS_MONTH:
      self.PARAMS.DAY_LIST   = list(range(1,self.PARAMS.THIS_DAY+1))
      if self.PARAMS.DAY > self.PARAMS.THIS_DAY:
        self.PARAMS.DAY = self.PARAMS.THIS_DAY
    else:
      self.PARAMS.DAY_LIST = list(range(1,monthrange(self.PARAMS.YEAR,self.PARAMS.MONTH)[1]+1))

    self.day_var.set(self.PARAMS.DAY)
    self.daybox['values'] = self.PARAMS.DAY_LIST
        
  def provider_month(self):

    self.PARAMS.MONTH = int(self.monthbox.get())

    if self.PARAMS.YEAR == self.PARAMS.THIS_YEAR and self.PARAMS.MONTH == self.PARAMS.THIS_MONTH:
      self.PARAMS.DAY_LIST   = list(range(1,self.PARAMS.THIS_DAY+1))
      if self.PARAMS.DAY > self.PARAMS.THIS_DAY:
        self.PARAMS.DAY = self.PARAMS.THIS_DAY
        self.day_val.set(self.PARAMS.DAY)
    else:
      self.PARAMS.DAY_LIST = list(range(1,monthrange(self.PARAMS.YEAR,self.PARAMS.MONTH)[1]+1))
    self.daybox['values'] = self.PARAMS.DAY_LIST
           
  def provider_day(self):

    self.PARAMS.DAY = int(self.daybox.get())
    
  def provider_hour(self):

    self.PARAMS.HOUR = int(self.hourbox.get())

def main():

  root = tk.Tk()
  root.title('Load Opendap File')
  app = WinOpendap(root)
  root.mainloop()


if __name__ == '__main__':
  main()


 
