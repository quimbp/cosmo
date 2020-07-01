''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems
    EGL, 06/2020:
		A heap variable MESSAGE has been introduce to store "print" messages 
'''

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox

import datetime 
from calendar import monthrange
import wget

__version__ = "1.1"
__author__  = "Quim Ballabrera"
__date__    = "July 2017"

class parameters():
# =================
  ''' Parameters for CODAR stations'''
  
  __version__ = "1.2"
  __author__  = "Quim Ballabrera"
  __date__    = "July 2017"

  # Version 1.0 (June 2017) : Initial version
  # Version 1.1 (July 2017) : Allows multiple time steps in CODAR file
  # Version 1.2 (December 2017) : Update path names
  # Version 1.3 (March 2018) : Compact version

  def __init__ (self):
    self.STATION       = tk.StringVar()
    self.URL           = tk.StringVar()
    self.FILENAME      = tk.StringVar()

    TODAY = datetime.datetime.now().date()
    self.YEAR           = tk.IntVar()
    self.MONTH          = tk.IntVar()
    self.DAY            = tk.IntVar()
    self.HOUR           = tk.IntVar()
    self.YEAR.set(TODAY.year)
    self.MONTH.set(TODAY.month)
    self.DAY.set(TODAY.day)
    self.HOUR.set(datetime.datetime.now().time().hour)

# =============
class WinCodar:
# =============

  def __init__ (self,master):

    self.MESSAGE = ""

    self.master = master
    self.PARAMS = parameters()
    self.frame = tk.Frame(self.master)


    self.HFR_LIST       = ['Ebre','Gibraltar','Galicia','Cadiz','Eivissa']
    self.HFR_INIT_YEAR  = [2013,2011,2010,2013,2012]
    self.HFR_INIT_MONTH = [12,5,7,5,6]
    self.HFR_PROVIDER   = ['Puertos','Puertos','Puertos','Puertos','SOCIB']

    # Paths as a function of the provider
    # 0 = Puertos
    # 1 = SOCIB
    self.OPATH = []
    self.FPATH = []
    self.OPATH.append('http://opendap.puertos.es/thredds/dodsC/radar_local_')
    self.FPATH.append('http://opendap.puertos.es/thredds/fileServer/radar_local_')
    self.OPATH.append('http://thredds.socib.es/thredds/dodsC/observational/hf_radar/hf_radar_ibiza-scb_codarssproc001_L1_agg/files/')
    self.FPATH.append('http://thredds.socib.es/thredds/fileServer/observational/hf_radar/hf_radar_ibiza-scb_codarssproc001_L1_agg/files/')

    # Initialize to Gibraltar
    self.PARAMS.STATION.set(self.HFR_LIST[1])

    self.INIT_YEAR      = self.HFR_INIT_YEAR[1]
    self.INIT_MONTH     = self.HFR_INIT_MONTH[1]
    self.PROVIDER       = self.HFR_PROVIDER[1]

    if self.PROVIDER == 'Puertos':
      self.pp = 0
    elif self.PROVIDER == 'SOCIB':
      self.pp = 1

    # Get today's date: year, month, day and hour (set to zero):
    TODAY = datetime.datetime.now().date()
    self.THIS_YEAR  = TODAY.year
    self.THIS_MONTH = TODAY.month
    self.THIS_DAY   = TODAY.day

    # Fill the default pull-down menu lists to select the data of the 
    # simulation.
    self.YEAR_LIST   = list(range(self.INIT_YEAR,self.THIS_YEAR+1))
    self.MONTH_LIST = list(range(1,self.THIS_MONTH+1))
    self.DAY_LIST   = list(range(1,self.THIS_DAY+1))
    self.HOUR_LIST = ('00','01','02','03','04','05','06','07','08','09','10',
            '11','12','13','14','15','16','17','18','19','20','21','22','23')

    # Window construction:
    ttk.Label(self.frame,text='CODAR station',font='Helvetica 12 bold',padding=3).grid(row=0,column=0,sticky='w',padx=3)

    self.stationbox = ttk.Combobox(self.frame,textvariable=self.PARAMS.STATION,width=14)
    self.stationbox['values'] = self.HFR_LIST
    self.stationbox.bind('<<ComboboxSelected>>',lambda e: self.station_selection())
    self.stationbox.grid(row=1,column=0,sticky='w')

    ttk.Label(self.frame,text='Year',font='Helvetica 12 bold',padding=3).grid(row=0,column=1,sticky='w',padx=3)
    self.yearbox = ttk.Combobox(self.frame,textvariable=self.PARAMS.YEAR,width=8)
    self.yearbox['values'] = self.YEAR_LIST
    self.yearbox.bind('<<ComboboxSelected>>',lambda e: self.station_year())
    self.yearbox.grid(row=1,column=1,sticky='w')

    ttk.Label(self.frame,text='Month',font='Helvetica 12 bold',padding=3).grid(row=0,column=2,sticky='we',padx=3)
    self.monthbox = ttk.Combobox(self.frame,textvariable=self.PARAMS.MONTH,width=8)
    self.monthbox.bind('<<ComboboxSelected>>',lambda e: self.station_month())
    self.monthbox['values'] = self.MONTH_LIST
    self.monthbox.grid(row=1,column=2,sticky='w')

    ttk.Label(self.frame,text='Day',font='Helvetica 12 bold',padding=3).grid(row=0,column=3,sticky='we',padx=3)
    self.daybox = ttk.Combobox(self.frame,textvariable=self.PARAMS.DAY,width=8)
    self.daybox.bind('<<ComboboxSelected>>',lambda e: self.station_day())
    self.daybox['values'] = self.DAY_LIST
    self.daybox.grid(row=1,column=3,sticky='w')

    ttk.Label(self.frame,text='Hour',font='Helvetica 12 bold',padding=3).grid(row=0,column=4,sticky='we',padx=3)
    self.hourbox = ttk.Combobox(self.frame,textvariable=self.PARAMS.HOUR,width=8)
    self.hourbox.bind('<<ComboboxSelected>>',lambda e: self.station_hour())
    self.hourbox['values'] = self.HOUR_LIST
    self.hourbox.grid(row=1,column=4,sticky='w')

    _wc = ttk.Button(self.frame,text='Cancel',        \
                     command=self.cancel,padding=3)
    _wc.grid(row=2,column=2,padx=3,pady=5,sticky='e')
    _wc.bind("<Return>",lambda e: self.cancel())

    _wd = ttk.Button(self.frame,text='Download',      \
                     command=self.download,padding=3)
    _wd.grid(row=2,column=3,padx=3,pady=5,sticky='e')
    _wd.bind("<Return>",lambda e: self.download())

    _wD = ttk.Button(self.frame,text='Done',          \
                     command=self.done,padding=3)
    _wD.grid(row=2,column=4,padx=3,pady=5,sticky='e')
    _wD.bind("<Return>",lambda e: self.done())

    self.frame.grid(padx=5,pady=5)

  def cancel(self):

    self.PARAMS.FILENAME.set('')
    self.pp      = None
    self.PARAMS.xname = ''
    self.PARAMS.yname = ''
    self.PARAMS.tname = ''
    self.PARAMS.uname = ''
    self.PARAMS.vname = ''
    self.master.destroy()
    self.master = None

  def filename(self,PATH):

    if self.pp is None:
      return ''

    if self.PARAMS.STATION.get() == 'Ebre':
      long_name = 'deltaebro'
      short_name = 'EBRO'
    elif self.PARAMS.STATION.get() == 'Gibraltar':
      long_name = 'gibraltar'
      short_name = 'GIBR'
    elif self.PARAMS.STATION.get() == 'Galicia':
      long_name = 'GALICIA'
      short_name = 'GALI'
    elif self.PARAMS.STATION.get() == 'Cadiz':
      long_name = 'HUELVA'
      short_name = 'TRAD'
    elif self.PARAMS.STATION.get() == 'Eivissa':
      long_name = 'ibiza'
      short_name = 'ibiza'
    else:
      return ''

    if self.pp == 0:
      self.PARAMS.xname = 'lon'
      self.PARAMS.yname = 'lat'
      self.PARAMS.tname = 'time'
      self.PARAMS.uname = 'u'
      self.PARAMS.vname = 'v'

      theurl = PATH + long_name + '/%d' % self.PARAMS.YEAR.get() + \
           '/%02d/CODAR_' % self.PARAMS.MONTH.get() +              \
         short_name +                                       \
         '_%d_%02d_%02d_' % (self.PARAMS.YEAR.get(),self.PARAMS.MONTH.get(),self.PARAMS.DAY.get())+\
              '%02d00.nc' % self.PARAMS.HOUR.get()
    elif self.pp == 1:
      self.PARAMS.xname = 'LON'
      self.PARAMS.yname = 'LAT'
      self.PARAMS.tname = 'time'
      self.PARAMS.uname = 'U'
      self.PARAMS.vname = 'V'
      theurl = PATH + '%d/' % self.PARAMS.YEAR.get() +              \
         'dep0001_hf-radar-%s_scb-codarssproc001_L1_' % short_name + \
         '%d-%02d.nc' % (self.PARAMS.YEAR.get(),self.PARAMS.MONTH.get())

    return theurl
    
  def download(self):
    theurl = self.filename(self.FPATH[self.pp])
    self.MESSAGE += 'Fetching '+theurl
    #print('Fetching ',theurl)
    #print('')
    try:
      filename = wget.download(theurl)
      messagebox.showinfo(message='Download complete')
    except:
      messagebox.showinfo(message='Unable to download file')

  def get_url(self):
    theurl = self.filename(self.OPATH[self.pp])
    return theurl

  def done(self):
    theurl = self.filename(self.OPATH[self.pp])
    self.MESSAGE += 'Filename '+ theurl
    #print('Filename ',theurl)
    self.PARAMS.FILENAME.set(theurl)
    self.master.destroy()
    self.master = None

  def get_filename(self):
    return self.PARAMS.FILENAME.get()

  def station_selection(self):

    self.PARAMS.STATION.set(self.stationbox.get())

    indx = self.HFR_LIST.index(self.PARAMS.STATION.get())
    self.INIT_YEAR  = self.HFR_INIT_YEAR[indx]
    self.INIT_MONTH = self.HFR_INIT_MONTH[indx]
    self.PROVIDER   = self.HFR_PROVIDER[indx]

    if self.PROVIDER == 'Puertos':
      self.pp = 0
    elif self.PROVIDER == 'SOCIB':
      self.pp = 1

    #print(self.INIT_YEAR,self.INIT_MONTH,self.PROVIDER,self.pp)

    year = self.INIT_YEAR if self.PARAMS.YEAR.get() < self.INIT_YEAR else self.PARAMS.YEAR.get()
    self.YEAR_LIST = list(range(self.INIT_YEAR,self.THIS_YEAR+1))

    if year == self.THIS_YEAR:
      month = self.THIS_MONTH if self.PARAMS.MONTH.get() > self.THIS_MONTH else self.PARAMS.MONTH.get()
      self.MONTH_LIST = list(range(1,self.THIS_MONTH+1))
    elif year == self.INIT_YEAR:
      month = self.INIT_MONTH if self.PARAMS.MONTH.get() < self.INIT_MONTH else self.PARAMS.MONTH.get()
      self.MONTH_LIST = list(range(self.INIT_MONTH,13))
    else:
      month = self.PARAMS.MONTH.get()
      self.MONTH_LIST = list(range(1,13))

    if year == self.THIS_YEAR and month == self.THIS_MONTH:
      day = self.THIS_DAY if self.PARAMS.DAY.get() > self.THIS_DAY else self.PARAMS.DAY.get()
      self.DAY_LIST   = list(range(1,self.THIS_DAY+1))
    else:
      day = self.PARAMS.DAY.get()
      self.DAY_LIST = list(range(1,monthrange(year,month)[1]+1))

    self.PARAMS.YEAR.set(year)
    self.PARAMS.MONTH.set(month)
    self.PARAMS.DAY.set(day)
    self.yearbox['values'] = self.YEAR_LIST
    self.monthbox['values'] = self.MONTH_LIST
    self.daybox['values'] = self.DAY_LIST
    if self.PARAMS.STATION.get() == 'Eivissa':
      self.daybox['state']  = 'disabled'
      self.hourbox['state'] = 'disabled'
    else:
      self.daybox['state']  = '!disabled'
      self.hourbox['state'] = '!disabled'
        
  def station_year(self):

    year = int(self.yearbox.get())

    if year == self.THIS_YEAR:
      month = self.THIS_MONTH if self.PARAMS.MONTH.get() > self.THIS_MONTH else self.PARAMS.MONTH.get()
      self.MONTH_LIST = list(range(1,self.THIS_MONTH+1))
    elif year == self.INIT_YEAR:
      month = self.INIT_MONTH if self.PARAMS.MONTH.get() < self.INIT_MONTH else self.PARAMS.MONTH.get()
      self.MONTH_LIST = list(range(self.INIT_MONTH,13))
    else:
      month = self.PARAMS.MONTH.get()
      self.MONTH_LIST = list(range(1,13))

    if year == self.THIS_YEAR and month == self.THIS_MONTH:
      day = self.THIS_DAY if self.PARAMS.DAY.get() > self.THIS_DAY else self.PARAMS.DAY.get()
      self.DAY_LIST   = list(range(1,self.THIS_DAY+1))
    else:
      day = self.PARAMS.DAY.get()
      self.DAY_LIST = list(range(1,monthrange(year,month)[1]+1))

    self.PARAMS.YEAR.set(year)
    self.PARAMS.MONTH.set(month)
    self.PARAMS.DAY.set(day)
    self.monthbox['values'] = self.MONTH_LIST
    self.daybox['values'] = self.DAY_LIST
        
  def station_month(self):

    year  = self.PARAMS.YEAR.get()
    month = int(self.monthbox.get())

    if year == self.THIS_YEAR and month == self.THIS_MONTH:
      day = self.THIS_DAY if self.PARAMS.DAY.get() > self.THIS_DAY else self.PARAMS.DAY.get()
      self.DAY_LIST   = list(range(1,self.THIS_DAY+1))
    else:
      day = self.PARAMS.DAY.get()
      self.DAY_LIST = list(range(1,monthrange(year,month)[1]+1))

    self.PARAMS.MONTH.set(month)
    self.PARAMS.DAY.set(day)
    self.daybox['values'] = self.DAY_LIST
               
  def station_day(self):

    self.PARAMS.DAY.set(int(self.daybox.get()))
    
  def station_hour(self):

    self.PARAMS.HOUR.set(int(self.hourbox.get()))


def main():

  root = tk.Tk()
  root.title('Load CODAR File')
  
  app = WinCodar(root)
  root.mainloop()


if __name__ == '__main__':
  main()


 
