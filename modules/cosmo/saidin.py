''' COSMO-VIEW
    Script: saidin.py    
       Script to request and load SST near real time from SAIDIN system.
    Changes:
    Quim Ballabrera, May 2017, Original version   
    EGL, February 2020
       No further support to Python 2.X
       Migration to cartopy. pyplot added
       Deprecated show() by draw() 
       contourf() has been replaced by pcolormesh() is faster when data
       is on regular grids.
       Added: icdf.close()
       Addes self.region to select between available westmed and eastmed
       images.
       An additional variable "wid" in the constructor has been added to pass
	   the reference of the master consola to send "print" messages
'''

import numpy as np
import wget
import requests
import datetime 
from netCDF4 import Dataset
from calendar import monthrange

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkinter import filedialog as filedialog
from tkinter import font as tkfont

#import matplotlib
#matplotlib.use('TkAgg')

# EG cartopy
import cartopy.crs as ccrs
#from mpl_toolkits.basemap import Basemap

from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt

import cosmo.contourplot as contourplot
from cosmo.tools import empty
from cosmo.tools import toconsola

__version__ = "1.2"
__author__  = "Quim Ballabrerera"
__date__    = "June 2017"

#===================
class parameters():
#===================

  def __init__ (self):
  #===================
    self.FILENAME = tk.StringVar()
    self.missing  = tk.DoubleVar()
    self.mask     = tk.BooleanVar()
    self.PLOT     = contourplot.parameters()
    self.mask.set(True)

    self.lon           = None
    self.lat           = None
    self.xx            = None
    self.yy            = None
    self.sst           = None
    self.minval        = None
    self.maxval        = None
    self.varname       = None
    self.units         = None
    self.missing_value = None
    self.cbar          = None
    self.PLOT.CONTOUR_MODE.set(1)

#============================
class WinSaidin(tk.Toplevel):
#============================
  def __init__ (self,parent=None,**args):
  #===============================
    tk.Toplevel.__init__(self, parent)
    self.title('SAIDIN Selector')
    self.resizable(False, False)
    self.lift()

    self.cons = args['consola']

    font_bold = tkfont.Font(font='TkDefaultFont').copy()
    font_bold['weight']='bold'

    self.FILENAME = tk.StringVar()
    #EG Self.REGION 
    self.REGION   = tk.StringVar()
    self.YEAR     = tk.IntVar()
    self.MONTH    = tk.IntVar()
    self.DAY      = tk.IntVar()
    self.HOUR     = tk.IntVar()
 
    self.FILENAME.set('')
    self.REGION.set("westmed")

    self.TODAY = datetime.datetime.now().date()
    self.THIS_YEAR  = self.TODAY.year
    self.THIS_MONTH = self.TODAY.month
    self.THIS_DAY   = self.TODAY.day

    self.YEAR.set(self.TODAY.year)
    self.MONTH.set(self.TODAY.month)
    self.DAY.set(self.TODAY.day)
    self.HOUR.set(0)

    self.YEAR_INI   = 2001
    self.MONTH_INI  = 5
    
    #EG Self.REGION =western Mediterranean> 
    self.REGION_LIST =["westmed","eastmed"]

    self.PATH       = 'http://cooweb.cmima.csic.es/thredds/dodsC/'
    self.PATH_LOAD  = 'http://cooweb.cmima.csic.es/thredds/fileServer/'
    self.PATH       += self.REGION.get()+'/'
    self.PATH_LOAD  += self.REGION.get()+'/'

    # Fill the default pull-down menu lists to select the data of the 
    # simulation.
    self.YEAR_LIST  = list(range(self.YEAR_INI,self.THIS_YEAR+1))
    self.MONTH_LIST = list(range(1,self.THIS_MONTH+1))
    self.DAY_LIST   = list(range(1,self.THIS_DAY+1))

    self.SEL        = None
    self.SAT        = None
    self.dict = {"n19":'noaa19',"m02":'metop02'}
    self.revdict = {"noaa19":'n19',"metop02":'m02'}

    self.frame = tk.Frame(self)
    ttk.Label(self.frame,text='Select Region',font=font_bold,width=12).\
              grid(row=0,column=0,columnspan=2)
    self.regbox = ttk.Combobox(self.frame,textvariable=self.REGION,width=8,justify="center")
    self.regbox['values'] = self.REGION_LIST
    self.regbox.bind('<<ComboboxSelected>>',lambda e: self.saidin_region())
    self.regbox.grid(row=0,column=2,sticky='w')
              
    ttk.Label(self.frame,text='Select date',font=font_bold,width=12). \
              grid(row=1,column=0,columnspan=2)
    self.yearbox = ttk.Combobox(self.frame,textvariable=self.YEAR,width=8,justify="center")
    self.yearbox['values'] = self.YEAR_LIST
    self.yearbox.bind('<<ComboboxSelected>>',lambda e: self.saidin_year())
    self.yearbox.grid(row=1,column=2,sticky='w')

    self.monthbox = ttk.Combobox(self.frame,textvariable=self.MONTH,width=5,justify="center")
    self.monthbox.bind('<<ComboboxSelected>>',lambda e: self.saidin_month())
    self.monthbox['values'] = self.MONTH_LIST
    self.monthbox.grid(row=1,column=3,sticky='w')

    self.daybox = ttk.Combobox(self.frame,textvariable=self.DAY,width=5,justify="center")
    self.daybox.bind('<<ComboboxSelected>>',lambda e: self.saidin_day())
    self.daybox['values'] = self.DAY_LIST
    self.daybox.grid(row=1,column=4,sticky='w')

    ttk.Button(self.frame,text='Check for images',                 \
                          command=lambda : self.request(),   \
                          padding=3,width=15).grid(row=1,column=5, \
                          columnspan=2,sticky='e')

    # Menu for image selection
    self.hour_var = tk.StringVar()
    self.hour_var.set(' ')

    FrH = ttk.Frame(self.frame,padding=5,borderwidth=5,relief='sunken')
    FrH.grid(row=2,column=0,columnspan=3,sticky='wens')

    ttk.Label(FrH,text='Select image:',font=font_bold).grid(row=0,column=0,columnspan=3)
    self.hourbox = ttk.Combobox(FrH,textvariable=self.hour_var,width=20)
    self.hourbox.grid(row=1,column=0,columnspan=3)
    self.hourbox.configure(state='disabled')
    self.hourbox.bind('<<ComboboxSelected>>',lambda e: self.saidin_hour())

    # Preview Button
    #self.Bprev = ttk.Button(self.frame,text='Preview', \
    #                        command=lambda : self.preview(), padding=5)
    #self.Bprev.grid(row=2,column=3)
    #self.Bprev.configure(state='disabled')

    # Canvas:
    self.fig = Figure(figsize=(6,4),dpi=36)
    #EG 
    #self.ax1 = self.fig.add_axes([left, bottom, width, height],projection=ccrs.Mercator())
    self.ax1 = self.fig.add_axes([0.01, 0.01, 0.98, 0.98],projection=ccrs.PlateCarree())
    
    self.canvas = FigureCanvasTkAgg(self.fig,master=self.frame)
    self.canvas.draw()
    
    self.canvas.get_tk_widget().grid(row=2,column=3,rowspan=3,columnspan=3)
    self.canvas._tkcanvas.grid()

    ttk.Button(self.frame,text='Cancel',command=lambda : self.cancel(),padding=5).grid(row=5,column=3,sticky='we',padx=3)
    ttk.Button(self.frame,text='Save Locally',command=self.download,padding=5).grid(row=5,column=4,sticky='we',padx=3)
    ttk.Button(self.frame,text='Select',command=self.done,padding=5).grid(row=5,column=5,sticky='we',padx=3)

    self.frame.grid(padx=5,pady=5)

  def filename(self,PATH):
  #=======================
    nmonth = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    # Get the selected hour:
    value_selected = self.hourbox.get()
    if (empty(value_selected)):
      messagebox.showinfo(message='No image selected')
      theurl = ''
      return theurl
    else:
      for ind,name in enumerate(self.SEL):
        if name == value_selected:
          thesat = self.SAT[ind]

    year      = self.YEAR.get()
    month     = self.MONTH.get()
    day       = self.DAY.get()
    hour      = self.hour_var.get()

    theurl = PATH + str(year) + '/' + '%02d-' % month +                   nmonth[month-1]+ '/%02d/' % day +                                 str(year) + '%02d' % month + '%02d' % day +                '.' + hour[0:2]+hour[3:5] + '.' + thesat + '.nc'
    return theurl

  def request(self):
  #=================
    '''Sends the request to scan the page contents'''
    if self.regbox.get() == "eastmed":
      toconsola("Eastern Mediterranean soon available",wid=self.cons)
      #print("Eastern Mediterranean soon available")
      region ="westmed"
      self.REGION.set(region)
      self.PATH       = 'http://cooweb.cmima.csic.es/thredds/dodsC/'+region+"/"
      self.PATH_LOAD  = 'http://cooweb.cmima.csic.es/thredds/fileServer/'+region+"/"
      self.request()
      return
    
    PATH = 'http://cooweb.cmima.csic.es/saidin-js/jsoncontrolservlet?service=search&'
    year  = self.YEAR.get()
    month = self.MONTH.get()
    day   = self.DAY.get()

    startDate = 'startDate=' + '%02d/' % month + '%02d/' % day + str(year)
    endDate = 'endDate=' + '%02d/' % month + '%02d/' % day + str(year)
    URL = PATH + startDate + '&' + endDate
    toconsola('Requesting '+URL,wid=self.cons)
    #print('Requesting ',URL)
    res = requests.get(URL)
    r = res.json()
    r['search'].pop('startDate')
    r['search'].pop('endDate')
    files = [row for row in r['search'].keys()]

    if len(files) == 0:
      messagebox.showinfo(message='No images available')
      return

    hour = [files[i].split('.')[1]  for i in range(0,len(files))]
    sat  = [files[i].split('.')[2]  for i in range(0,len(files))]
    s = sorted(zip(hour,sat))
    self.SEL = [ e[0][0:2]+':'+e[0][2:4]+' UTC (%s)' % self.dict[e[1]] for e in s]
    self.SAT = [ e[1] for e in s] 
    self.hour_var.set(self.SEL[0])
    self.hourbox['values'] = self.SEL
    self.hourbox.configure(state='!disabled')
    #self.Bprev.configure(state='!disabled')
    self.saidin_hour()

  def preview(self):
  #=================
    '''Preview map in window'''
    
    theurl = self.FILENAME.get()
    toconsola('TEST SHOW theurl >>'+theurl,wid=self.cons)
    #print('TEST SHOW theurl >>',theurl)
    if empty(theurl):
        messagebox.showinfo(message='No image selected')
        return
    self.ax1.clear()

    toconsola('Preview of '+theurl,wid=self.cons)
    #print('Preview of ',theurl)
    icdf = Dataset(theurl,'r')
    lon = icdf.variables['lon'][:]
    lat = icdf.variables['lat'][:]
    sst = icdf.variables['mcsst'][0,:,:].squeeze()
    icdf.close()
    
    xxT,yyT = np.meshgrid(lon,lat)

    SOUTH = np.min(lat)
    NORTH = np.max(lat)
    WEST  = np.min(lon)
    EAST  = np.max(lon)

    #print([EAST, WEST,SOUTH, NORTH])
    
    #EG mod faltan los detalles line,fill
    self.ax1.set_extent([EAST, WEST, SOUTH, NORTH])
    self.ax1.coastlines('110m',linewidth=2)
    #EG pcolormesh is faster 
    #self.ax1.contourf(xxT,yyT,sst,transform=ccrs.PlateCarree())
    self.ax1.pcolormesh(xxT,yyT,sst,transform=ccrs.PlateCarree())
    
    #m = Basemap(llcrnrlat=SOUTH,urcrnrlat=NORTH, \
                #llcrnrlon=WEST,urcrnrlon=EAST,  \
                #ax=self.ax1)
    #m.fillcontinents(color='coral')
    #m.drawcoastlines(linewidth=1)             
    #m.contourf(xxT,yyT,sst)
    #EG
                      
    self.canvas.draw()
    toconsola('done',wid=self.cons)
    #print('done')

  def cancel(self):
  #=================
    '''Returns the original values and closes the app'''
    #self.parent.destroy()
    self.destroy()

  def download(self):
  #=================
    theurl = self.filename(self.PATH_LOAD)
    toconsola('FETCHING '+theurl,wid=self.cons)
    #print('FETCHING ',theurl)
    filename = wget.download(theurl)
    print('')

  def get_url(self):
  #=================
    return self.FILENAME.get()

  def done(self):
  #=================
    '''Gets the selected options and gets the corresponding URL before closing the app.
       The URL is saved in self.FILENAME'''

    if empty(self.FILENAME.get()):
        messagebox.showinfo(message='No image selected')
        return
    toconsola('SAIDIN FILE = '+self.FILENAME.get(),wid=self.cons)
    #print('SAIDIN FILE = ',self.FILENAME.get())
    self.destroy()

  def saidin_year(self):
  #====================
    '''Select the year: It distinguishes between the initial year, current year or
       other years'''
    year = int(self.yearbox.get())

    if year == self.THIS_YEAR:
      month = self.THIS_MONTH if self.MONTH.get() > self.THIS_MONTH else self.MONTH.get()
      self.MONTH_LIST = list(range(1,self.THIS_MONTH+1))
    elif year == self.YEAR_INI:
      month = self.MONTH_INI if self.MONTH.get() < self.MONTH_INI else self.MONTH.get()
      self.MONTH_LIST = list(range(self.MONTH_INI,13))
    else:
      month = self.MONTH.get()
      self.MONTH_LIST = list(range(1,13))

    if year == self.THIS_YEAR and month == self.THIS_MONTH:
      day = self.THIS_DAY if self.DAY.get() > self.THIS_DAY else self.DAY.get()
      self.DAY_LIST   = list(range(1,self.THIS_DAY+1))
    else:
      day = self.DAY.get()
      self.DAY_LIST = list(range(1,monthrange(year,month)[1]+1))

    self.YEAR.set(year)
    self.MONTH.set(month)
    self.DAY.set(day)

    self.monthbox['values'] = self.MONTH_LIST
    self.daybox['values'] = self.DAY_LIST

  def saidin_month(self):
  #======================
    '''Select the month: The range of months depends on the selected year'''

    year = self.YEAR.get()
    month = int(self.monthbox.get())

    if year == self.THIS_YEAR and month == self.THIS_MONTH:
      day = self.THIS_DAY if self.DAY.get() > self.THIS_DAY else self.DAY.get()
      self.DAY_LIST   = list(range(1,self.THIS_DAY+1))
    else:
      day = self.DAY.get()
      self.DAY_LIST = list(range(1,monthrange(year,month)[1]+1))

    self.MONTH.set(month)
    self.DAY.set(day)
    self.daybox['values'] = self.DAY_LIST

  def saidin_day(self):
  #====================
    self.DAY.set(int(self.daybox.get()))
    
  def saidin_hour(self):
  #====================
    name = self.filename(self.PATH)
    if not empty(name):
      self.FILENAME.set(name)
      self.preview()
    else:
      toconsola('No image selected',wid=self.cons)
      #print('No image selected')
      self.FILENAME.set('')
      
  def saidin_region(self):
  #=======================
    region = self.regbox.get()
    self.PATH       = 'http://cooweb.cmima.csic.es/thredds/dodsC/'+region+"/"
    self.PATH_LOAD  = 'http://cooweb.cmima.csic.es/thredds/fileServer/'+region+"/"
    end = len(self.hourbox['values'])-1
    self.hourbox.delete(0,end)
    self.hour_var.set(' ')
    self.hourbox.configure(state='disabled')
    self.ax1.clear()
    self.canvas.draw()
    
#=======================================
def saidin_selector(parent=None,**args):
#=======================================
  consola = args['wid']
  col = WinSaidin(parent, consola=consola)
  col.wait_window(col)
  res = col.get_url()
  return res

#==========
def main():
#==========
  root = tk.Tk()
  app = WinSaidin(root)
  root.mainloop()

if __name__ == '__main__':
  main()

