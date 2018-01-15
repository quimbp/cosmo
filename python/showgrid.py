import numpy as np

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkinter import filedialog as filedialog
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  import tkFileDialog as filedialog


import sys
import datetime 
from datetime import timedelta
from calendar import monthrange
from netCDF4 import Dataset,num2date

from cosmo import *
#from cosmo import view_parameters as parameters


class WinShowgrid:

  def __init__ (self,master,ncid,icdf):

    strnx = tk.StringVar()
    strny = tk.StringVar()
    strnz = tk.StringVar()
    strnt = tk.StringVar()

    strnx.set(str(icdf.nx))
    strny.set(str(icdf.ny))
    strnz.set(str(icdf.nz))
    strnt.set(str(icdf.nt))

    self.master = master
    self.frame = tk.Frame(self.master)
 
    lx = ttk.Label(self.frame,text='X',width=12,padding=5,font='Helvertica 12 bold')
    lx.config(justify='center')
    lx.grid(row=0,column=1,sticky='we')

    ly = ttk.Label(self.frame,text='Y',width=12,padding=5,font='Helvertica 12 bold')
    ly.config(justify='center')
    ly.grid(row=0,column=2,sticky='we')

    lz = ttk.Label(self.frame,text='Z',width=12,padding=5,font='Helvertica 12 bold')
    lz.config(justify='center')
    lz.grid(row=0,column=3,sticky='we')

    lt = ttk.Label(self.frame,text='T',width=12,padding=5,font='Helvertica 12 bold')
    lt.config(justify='center')
    lt.grid(row=0,column=4,sticky='we')


    ln = ttk.Label(self.frame,text='n',width=20,padding=5,font='Helvertica 12 bold italic')
    ln.config(justify='center')
    ln.grid(row=1,column=0,sticky='e')

    lmin = ttk.Label(self.frame,text='Minimum',width=20,padding=5,font='Helvertica 12 bold')
    lmin.config(justify='center')
    lmin.grid(row=2,column=0,sticky='e')

    lmax = ttk.Label(self.frame,text='Maximum',width=20,padding=5,font='Helvertica 12 bold')
    lmax.config(justify='center')
    lmax.grid(row=3,column=0,sticky='e')

    lmax = ttk.Label(self.frame,text='Mean interval',width=20,padding=5,font='Helvertica 12 bold')
    lmax.config(justify='center')
    lmax.grid(row=4,column=0,sticky='e')

    lmax = ttk.Label(self.frame,text='Minimum interval',width=20,padding=5,font='Helvertica 12 bold')
    lmax.config(justify='center')
    lmax.grid(row=5,column=0,sticky='e')

    lmax = ttk.Label(self.frame,text='Maximum interval',width=20,padding=5,font='Helvertica 12 bold')
    lmax.config(justify='center')
    lmax.grid(row=6,column=0,sticky='e')

    lmax = ttk.Label(self.frame,text='Variable',width=20,padding=5,font='Helvertica 12 bold')
    lmax.config(justify='center')
    lmax.grid(row=7,column=0,sticky='e')

    lmax = ttk.Label(self.frame,text='Units',width=20,padding=5,font='Helvertica 12 bold')
    lmax.config(justify='center')
    lmax.grid(row=8,column=0,sticky='e')

    global var

    if icdf.idx > -1:
      vname = icdf.VAR_LIST[icdf.idx]
      wrk   = ncid.variables[vname][:]
      dd    = np.diff(wrk)
      ttk.Label(self.frame,text=strnx.get()).grid(row=1,column=1)
      ttk.Label(self.frame,text=str(min(wrk))).grid(row=2,column=1,sticky='w')
      ttk.Label(self.frame,text=str(max(wrk))).grid(row=3,column=1,sticky='w')
      ttk.Label(self.frame,text=str(np.mean(dd))).grid(row=4,column=1,sticky='w')
      ttk.Label(self.frame,text=str(min(dd))).grid(row=5,column=1,sticky='w')
      ttk.Label(self.frame,text=str(max(dd))).grid(row=6,column=1,sticky='w')
      ttk.Label(self.frame,text=vname).grid(row=7,column=1,sticky='w')
      var = ncid.variables[vname]
      try:
        units = var.getncattr('units')
        ttk.Label(self.frame,text=units).grid(row=8,column=1,sticky='w')
      except:
        pass
      ttk.Button(self.frame,text='Inspect',command=lambda : self.viewx(ncid,icdf)).grid(row=9,column=1)
    else:
      ttk.Label(self.frame,text='--').grid(row=1,column=1)
    
    if icdf.idy > -1:
      vname = icdf.VAR_LIST[icdf.idy]
      wrk   = ncid.variables[vname][:]
      dd    = np.diff(wrk)
      ttk.Label(self.frame,text=strny.get()).grid(row=1,column=2)
      ttk.Label(self.frame,text=str(min(wrk))).grid(row=2,column=2,sticky='w')
      ttk.Label(self.frame,text=str(max(wrk))).grid(row=3,column=2,sticky='w')
      ttk.Label(self.frame,text=str(np.mean(dd))).grid(row=4,column=2,sticky='w')
      ttk.Label(self.frame,text=str(min(dd))).grid(row=5,column=2,sticky='w')
      ttk.Label(self.frame,text=str(max(dd))).grid(row=6,column=2,sticky='w')
      ttk.Label(self.frame,text=vname).grid(row=7,column=2,sticky='w')
      var = ncid.variables[vname]
      try:
        units = var.getncattr('units')
        ttk.Label(self.frame,text=units).grid(row=8,column=2,sticky='w')
      except:
        pass
      ttk.Button(self.frame,text='Inspect',command=lambda : self.viewy(ncid,icdf)).grid(row=9,column=2)
    else:
      ttk.Label(self.frame,text='--').grid(row=1,column=2)
    
    if icdf.idz > -1:
      vname = icdf.VAR_LIST[icdf.idz]
      wrk   = ncid.variables[vname][:]
      dd    = np.diff(wrk)
      ttk.Label(self.frame,text=strnz.get()).grid(row=1,column=3)
      ttk.Label(self.frame,text=str(min(wrk))).grid(row=2,column=3,sticky='w')
      ttk.Label(self.frame,text=str(max(wrk))).grid(row=3,column=3,sticky='w')
      ttk.Label(self.frame,text=str(np.mean(dd))).grid(row=4,column=3,sticky='w')
      ttk.Label(self.frame,text=str(min(dd))).grid(row=5,column=3,sticky='w')
      ttk.Label(self.frame,text=str(max(dd))).grid(row=6,column=3,sticky='w')
      ttk.Label(self.frame,text=vname).grid(row=7,column=3,sticky='w')
      var = ncid.variables[vname]
      try:
        units = var.getncattr('units')
        ttk.Label(self.frame,text=units).grid(row=8,column=3,sticky='w')
      except:
        pass
      ttk.Button(self.frame,text='Inspect',command=lambda : self.viewz(ncid,icdf)).grid(row=9,column=3)
    else:
      ttk.Label(self.frame,text='--').grid(row=1,column=3)
    
    if icdf.idt > -1:
      vname = icdf.VAR_LIST[icdf.idt]
      wrk   = ncid.variables[vname][:]
      if icdf.nt > 1:
        dd    = np.diff(wrk)
      else:
        dd = [0.0]

      ttk.Label(self.frame,text=strnt.get()).grid(row=1,column=4)
      ttk.Label(self.frame,text=str(min(wrk))).grid(row=2,column=4,sticky='w')
      ttk.Label(self.frame,text=str(max(wrk))).grid(row=3,column=4,sticky='w')
      ttk.Label(self.frame,text=str(np.mean(dd))).grid(row=4,column=4,sticky='w')
      ttk.Label(self.frame,text=str(min(dd))).grid(row=5,column=4,sticky='w')
      ttk.Label(self.frame,text=str(max(dd))).grid(row=6,column=4,sticky='w')
      ttk.Label(self.frame,text=vname).grid(row=7,column=4,sticky='w')
      var = ncid.variables[vname]
      try:
        units = var.getncattr('units')
        ttk.Label(self.frame,text=units).grid(row=8,column=4,sticky='w')
      except:
        pass
      ttk.Button(self.frame,text='Inspect',command=lambda : self.viewt(ncid,icdf)).grid(row=9,column=4)
    else:
      ttk.Label(self.frame,text='--').grid(row=1,column=4)

    self.frame.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')

  def viewx(self,ncid,icdf):
    vname = icdf.VAR_LIST[icdf.idx]
    self.wv = tk.Toplevel(self.master)
    self.wv.title(vname)
    self.wv.resizable(width=True, height=True)
    self.wv.protocol('WM_DELETE_WINDOW', self.wv.destroy)
    wrk   = ncid.variables[vname][:]
    log = tk.Text(self.wv)
    log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')
    # Scrollbar
    scrollb = tk.Scrollbar(self.wv,command=log.yview)
    scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
    log['yscrollcommand'] = scrollb.set

    for index, value in enumerate(wrk):
      string = '\t {} \t  {} \n'.format(index,value)
      log.insert('end',string)
    
  def viewy(self,ncid,icdf):
    vname = icdf.VAR_LIST[icdf.idy]
    self.wv = tk.Toplevel(self.master)
    self.wv.title(vname)
    self.wv.resizable(width=True, height=True)
    self.wv.protocol('WM_DELETE_WINDOW', self.wv.destroy)
    wrk   = ncid.variables[vname][:]
    log = tk.Text(self.wv)
    log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')
    # Scrollbar
    scrollb = tk.Scrollbar(self.wv,command=log.yview)
    scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
    log['yscrollcommand'] = scrollb.set

    for index, value in enumerate(wrk):
      string = '\t {} \t  {} \n'.format(index,value)
      log.insert('end',string)
    
  def viewz(self,ncid,icdf):
    vname = icdf.VAR_LIST[icdf.idz]
    self.wv = tk.Toplevel(self.master)
    self.wv.title(vname)
    self.wv.resizable(width=True, height=True)
    self.wv.protocol('WM_DELETE_WINDOW', self.wv.destroy)
    wrk   = ncid.variables[vname][:]
    log = tk.Text(self.wv)
    log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')
    # Scrollbar
    scrollb = tk.Scrollbar(self.wv,command=log.yview)
    scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
    log['yscrollcommand'] = scrollb.set

    for index, value in enumerate(wrk):
      string = '\t {} \t  {} \n'.format(index,value)
      log.insert('end',string)
    
  def viewt(self,ncid,icdf):
    vname = icdf.VAR_LIST[icdf.idt]
    self.wv = tk.Toplevel(self.master)
    self.wv.title(vname)
    self.wv.resizable(width=True, height=True)
    self.wv.protocol('WM_DELETE_WINDOW', self.wv.destroy)
    wrk   = ncid.variables[vname][:]

    log = tk.Text(self.wv)
    log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')
    # Scrollbar
    scrollb = tk.Scrollbar(self.wv,command=log.yview)
    scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
    log['yscrollcommand'] = scrollb.set

    for index, value in enumerate(wrk):
      string = '\t {} \t  {} \t {} \n'.format(index,value,num2date(value,units=icdf.time_units, \
                                                                   calendar=icdf.time_calendar))
      log.insert('end',string)
   

def main():
 
  # Open filename:
  try:
    filename = str(sys.argv[1])
  except:
    nn = filedialog.askopenfile()
    filename = nn.name

  ncid = Dataset(filename)
  icdf = geocdf(filename)

  root = tk.Tk()
  root.title('showgrid')
  app = WinShowgrid(root,ncid,icdf)
  root.mainloop()

if __name__ == '__main__':
  main()
