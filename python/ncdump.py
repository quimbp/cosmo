''' COSMO-VIEW,
    Quim Ballabrera, May 2017
    Script for visualizing model outputs provided by various operational
      systems'''

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import filedialog as filedialog
except:
  import Tkinter as tk
  import ttk
  import tkFileDialog as filedialog

from netCDF4 import Dataset
from cosmo import *
import sys


class WinNcdump:

  def __init__ (self,master,ncid):

    self.master = master
    #self.frame = tk.Frame(self.master)
 
    log = tk.Text(self.master)
    log.grid(row=0,column=0,padx=10,pady=10,sticky='nsew')

    # Scrollbar
    scrollb = tk.Scrollbar(self.master,command=log.yview)
    scrollb.grid(row=0,column=1,sticky='nsew',padx=2,pady=2)
    log['yscrollcommand'] = scrollb.set


    # Dimensions
    log.insert('end','Dimensions:\n')
    nc_dims = [dim for dim in ncid.dimensions]  # List of Dimensions
    for dim in nc_dims:
      string = '\t {} = {} ;\n'.format(dim,len(ncid.dimensions[dim]))
      log.insert('end',string)

    # Variables (and their attributes)
    log.insert('end','\nVariables:\n')
    nc_vars = [var for var in ncid.variables]  # list of variables
    for var in nc_vars:
      string = '\t {} {} {} \n'.format(ncid.variables[var].dtype,
                                       var,
                                       ncid.variables[var].dimensions)
      log.insert('end',string)
      try:
        for ncattr in ncid.variables[var].ncattrs():
          string = '\t\t {}: {} \n'.format(ncattr,ncid.variables[var].getncattr(ncattr))
          log.insert('end',string)
      except:
        pass
                                       
    # Global Attributes
    log.insert('end','\nGlobal Attributes:\n')
    nc_attrs = ncid.ncattrs()
    for nc_attr in nc_attrs:
      string = '\t{}: {}\n'.format(nc_attr,ncid.getncattr(nc_attr))
      log.insert('end',string)
    #log.see(tk.END)

    log.configure(state='disabled')
    #self.frame.grid(sticky='nsew')
  

def main():

  # Open filename:
  try:
    filename = str(sys.argv[1])
  except:
    nn = filedialog.askopenfile()
    filename = nn.name

  ncid = Dataset(filename)

  root = tk.Tk()
  root.title('Ncdump')
  app = WinNcdump(root,ncid)
  root.mainloop()

if __name__ == '__main__':
  main()
