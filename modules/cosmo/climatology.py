''' COSMO-VIEW,
    Quim Ballabrera, April 2021
    Script for downloading COSMO climatologies model outputs provided by various operational
'''

import tkinter as tk
from tkinter import ttk
from PIL import Image, ImageTk
import requests
import os


from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_ROOT
from cosmo.tools import toconsola


__version__ = "1.0"
__author__  = "Quim Ballabrera"
__date__    = "April 2021"


# ==============
class winClim():
# ==============

  def __init__(self,master,wid=None):
  # ==================================

    self.master         = master

    self.MESSAGE        = ""

    self.MEDSEA         = tk.BooleanVar()
    self.GLOBCURRENT    = tk.BooleanVar()
    self.SEALEVEL       = tk.BooleanVar()
    self.MEDSEA.set(True)
    self.GLOBCURRENT.set(False)
    self.SEALEVEL.set(False)

    def cancel():
    # ===============
      self.master.destroy()

    def download():
    # ===============
      if self.MEDSEA.get():
        url = 'https://cosmo.icm.csic.es/climatology/MEDSEA_100.nc'
        message = 'Downloading ' + url
        toconsola(message,wid=wid)
        r = requests.get(url,verify=False)
        with open('MEDSEA_100.nc','wb') as f:
          f.write(r.content)

      if self.GLOBCURRENT.get():
        url = 'https://cosmo.icm.csic.es/climatology/GLOBCURRENT_100.nc'
        message = 'Downloading ' + url
        toconsola(message,wid=wid)
        r = requests.get(url,verify=False)
        with open('GLOBCURRENT_100.nc','wb') as f:
          f.write(r.content)

      if self.SEALEVEL.get():
        url = 'https://cosmo.icm.csic.es/climatology/SEALEVEL_100.nc'
        message = 'Downloading ' + url
        toconsola(message,wid=wid)
        r = requests.get(url,verify=False)
        with open('SEALEVEL_100.nc','wb') as f:
          f.write(r.content)

      toconsola('Done !',wid=wid)


    # Window construction:
    F0 = tk.Frame(self.master)
    ttk.Checkbutton(F0,text='MEDSEA',variable=self.MEDSEA).grid(row=0,column=0,sticky='w',padx=3)
    ttk.Checkbutton(F0,text='GLOBCURRENT',variable=self.GLOBCURRENT).grid(row=1,column=0,sticky='w',padx=3)
    ttk.Checkbutton(F0,text='SEA_LEVEL',variable=self.SEALEVEL).grid(row=2,column=0,sticky='w',padx=3)
    F0.grid()
 
    F1 = tk.Frame(self.master)
    ttk.Button(F1,text='Cancel',command=cancel).grid(row=0,column=0,padx=3,pady=3)
    ttk.Button(F1,text='Download',command=download).grid(row=0,column=1,padx=3,pady=3)
    F1.grid()
  

# =========
def main():
# =========

  #image = Image.open(COSMO_ROOT + os.sep + 'conf' + os.sep + 'cosmo-logo.png')

  root = tk.Tk()
  root.title('Atlas of Climatological Currents')
  root.resizable(False,False)
  root.protocol('WM_DELETE_WINDOW',quit)
  #photo = ImageTk.PhotoImage(image)
  #root.tk.call('wm','iconphoto',root._w,photo)

  winClim(root)
  root.mainloop()


if __name__ == '__main__':
  main()








