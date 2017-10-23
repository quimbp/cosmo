'''
=======================================================================
Plot streamfunction and velocity vectors
=======================================================================

Quim Ballabrera, October 2017.
COSMO project.
Takes an A-grid or a C-grid output from the program streamfunction
and plot de velocity field.
Version 0.1, released October 2017
'''

import matplotlib.pyplot as plt
import numpy as np
from netCDF4 import Dataset
from scipy.interpolate import interp2d
import sys

__progname__ = 'STREAMLINE'
__version__  = '0.1'
__date__     = 'October 2017'
__author__   = 'Quim Ballabrera (ICM/CSIC)'

def usage():
# ==========
  print('Error: No input file')
  print('Usage:')
  print('> python streamline FILENAME')
  quit()

def empty(string):
# ================
  '''Logical function that checks if a string is empty:
     empty('     ') = True'''

  if bool(string.strip()):
    return False
  else:
    return True

def main():
# =========
  try:
    if empty(sys.argv[1]) :
      usage()
  except:
    usage()
  filename = sys.argv[1]
  print('Opening file',filename)

  try:
    ncid = Dataset(filename)
  except:
    print('Unable to open input file')
    return

  # Check if A- or C- grid:
  #
  cgrid = False
  for name,variable in ncid.variables.items():
    if name == 'x_p':
      cgrid = True
  agrid = not cgrid

  if agrid:
    print('Reading an Arakawa A-grid file')
    x = ncid.variables['x'][:]
    y = ncid.variables['y'][:]
    p = ncid.variables['psi'][:,:]
    u = ncid.variables['u'][:,:]
    v = ncid.variables['v'][:,:]
    xx,yy = np.meshgrid(x,y)

    plt.figure()
    plt.title(filename)
    C = plt.contour(xx,yy,p,colors='r')
    Q = plt.quiver(xx[::4],yy[::4],u[::4],v[::4],pivot='mid',units='x',
                   width=0.025,scale=1/0.08)
    plt.clabel(C,inline=1,fontsize=12,fmt='%1.1f')
    plt.quiverkey(Q,0.9,0.9,1,r'$1 \frac{m}{s}$',labelpos='E',
                  coordinates='figure')
    plt.show()
  else:
    print('Reading an Arakawa C-grid file')
    xp = ncid.variables['x_p'][:]
    yp = ncid.variables['y_p'][:]
    p = ncid.variables['psi'][:,:]

    xu = ncid.variables['x_u'][:]
    yu = ncid.variables['y_u'][:]
    u = ncid.variables['u'][:,:]

    xv = ncid.variables['x_v'][:]
    yv = ncid.variables['y_v'][:]
    v = ncid.variables['v'][:,:]
 
    xx,yy   = np.meshgrid(xp,yp)
    xxu,yyu = np.meshgrid(xu,yu)
    xxv,yyv = np.meshgrid(xv,yv)

    # Interpolation to the Psi points:
    #
    FU = interp2d(xxu,yyu,u,kind='cubic')
    FV = interp2d(xxv,yyv,v,kind='cubic')

    up = FU(xp,yp)
    vp = FV(xp,yp)
 

    plt.figure()
    plt.title(filename)
    C = plt.contour(xx,yy,p,colors='r')
    Q = plt.quiver(xx[::4],yy[::4],up[::4],vp[::4],pivot='mid',units='x',
                   width=0.025,scale=1/0.08)
    plt.clabel(C,inline=1,fontsize=12,fmt='%1.1f')
    plt.quiverkey(Q,0.9,0.9,1,r'$1 \frac{m}{s}$',labelpos='E',
                  coordinates='figure')
    plt.show()
   

if __name__ == "__main__":
  main()
