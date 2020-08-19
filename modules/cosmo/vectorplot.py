''' Module for plotting vectors, built for the COSMO project 
# Quim Ballabrera, May 2017
	EGL, 06/2020: Changes:
		No more support to python 2.X 
		Basemap drawing system deprecated and substituted by Cartopy
		Limited support to geographical projections. Everything is 
		plotted in PlateCarree.
		NaN values are explicitely set to np.nan. Some tested datasets 
		do not set this constant appropriately when loading. This is
		particularly cumbersome for the streamfucntion.
		Streamfunction with Cartopy 0.17 is implemented by calling previously
		the quiver function with negative zorder. This deserves further
		analysis.
		When regridding option is set the procedure is implemented by
		first making a linear interpolation of the max-min coordinates
		and then values are interpolated with scipy interpolation. This
		only works correctly by fixing the method keyword in the 
		interpol.griddata() function to "linear".
		A reordering of the if elif structure has been done. 
		Adjustments in text fonts
		All color selections are now managed through tools.colsel() function
		Cartopy projections can be accessed through tools.map_proj()
		A heap variable MESSAGE has been introduce to store "print" messages
        QB, 07/2020: Changes
                Support for different Arakawa grids
'''
import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from tkcolorpicker import askcolor
from tkinter import font as tkfont

import json
import os
import io
try:
  to_unicode = unicode
except:
  to_unicode = str

from matplotlib.font_manager import FontProperties
#EG
import scipy.interpolate as interpol

from cosmo.tools import exists
from cosmo.tools import colsel
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA

#EG To manage cartopy projections
#EG from cosmo.tools import map_proj

# ============================
class parameters():
# ============================
  '''Class for vector plots'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  def __init__ (self):
  # ==================
    ''' Define and initialize class attributes'''

    self.MESSAGE = ""

    with open(COSMO_CONF_DATA) as infile:
      conf = json.load(infile)
    #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
    COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
    COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep

    self.FILECONF          = COSMO_CONF + 'vector.conf'

    self.DRAWING_MODE      = tk.IntVar()
    self.GRID_MODE         = tk.IntVar()

    self.CURRENT_DX        = tk.IntVar()
    self.CURRENT_DY        = tk.IntVar()
    self.CURRENT_NX        = tk.IntVar()
    self.CURRENT_NY        = tk.IntVar()
    self.CURRENT_SCALE     = tk.DoubleVar()
    self.CURRENT_WIDTH     = tk.DoubleVar()
    self.CURRENT_HEADLENGTH= tk.IntVar()
    self.CURRENT_HEADWIDTH = tk.IntVar()
    self.CURRENT_COLOR     = tk.StringVar()

    self.BARB_LENGTH       = tk.IntVar()
    self.BARB_PIVOT        = tk.StringVar()
    self.BARB_BARBCOLOR    = tk.StringVar()
    self.BARB_FLAGCOLOR    = tk.StringVar()
    self.BARB_SPACING      = tk.DoubleVar()
    self.BARB_HEIGHT       = tk.DoubleVar()
    self.BARB_WIDTH        = tk.DoubleVar()
    self.BARB_EMPTYBARB    = tk.DoubleVar()
    self.BARB_HALF         = tk.IntVar()
    self.BARB_FULL         = tk.IntVar()
    self.BARB_FLAG         = tk.IntVar()
    self.BARB_LINEWIDTH    = tk.DoubleVar()
    self.BARB_SCALE        = tk.DoubleVar()

    self.STREAM_DENSITY    = tk.DoubleVar()
    self.STREAM_WIDTH      = tk.DoubleVar()
    self.STREAM_COLOR      = tk.StringVar()

    self.ALPHA             = tk.DoubleVar()
    self.ZORDER            = tk.IntVar()
    self.COLOR_BY_SPEED    = tk.BooleanVar()

    self.KEY_SHOW  = tk.BooleanVar()
    self.KEY_LABEL = tk.StringVar()
    self.KEY_X     = tk.DoubleVar()
    self.KEY_Y     = tk.DoubleVar()
    self.KEY_VALUE = tk.DoubleVar()
    self.KEY_POS   = tk.StringVar()
    self.KEY_COLOR = tk.StringVar()
    self.KEY_SIZE  = tk.IntVar()
    self.KEY_GETXY = False
    self.KEY_OBJ   = None

    self.ARAKAWA   = tk.StringVar() 

    # Defautl attribute values
    #
    self.DRAWING_MODE.set(0)     # 0 - Vector, 1 - Barb, 2 - Streamplot
    self.GRID_MODE.set(0)
    self.CURRENT_DX.set(4)
    self.CURRENT_DY.set(4)
    self.CURRENT_NX.set(41)
    self.CURRENT_NY.set(41)
    self.CURRENT_SCALE.set(20)
    self.CURRENT_WIDTH.set(0.002)
    self.CURRENT_HEADLENGTH.set(5)
    self.CURRENT_HEADWIDTH.set(3)
    self.CURRENT_COLOR.set('black')
    self.ALPHA.set(1.0)
    self.ZORDER.set(3)
    self.BARB_PIVOT.set('middle')
    self.BARB_LENGTH.set(7)
    self.BARB_BARBCOLOR.set('black')
    self.BARB_FLAGCOLOR.set('black')
    self.BARB_LINEWIDTH.set(1.0)
    self.BARB_SCALE.set(1.9438445)
    self.BARB_HALF.set(5)
    self.BARB_FULL.set(10)
    self.BARB_FLAG.set(50)
    self.BARB_SPACING.set(0.125)
    self.BARB_HEIGHT.set(0.40)
    self.BARB_WIDTH.set(0.25)
    self.BARB_EMPTYBARB.set(0.15)
    self.STREAM_DENSITY.set(1)
    self.STREAM_WIDTH.set(2)
    self.STREAM_DENSITY.set(1)
    self.STREAM_COLOR.set('black')
    self.COLOR_BY_SPEED.set(False)
    self.KEY_SHOW.set(True)
    self.KEY_VALUE.set(1)
    self.KEY_LABEL.set('m/s')
    self.KEY_X.set(0.85)
    self.KEY_Y.set(0.10)
    self.KEY_POS.set('E')
    self.KEY_COLOR.set('black')
    self.KEY_SIZE.set(12)
    self.ARAKAWA.set('A')

    # If configuration file exists, it is read and
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.
    #
    if exists(self.FILECONF):
      try:
        self.MESSAGE += '\nVECTORPLOT: Loading VECTOR configuration file '+self.FILECONF
        self.load(self.FILECONF)
      except:
        self.MESSAGE += '\nVECTORPLOT: Error: Saving default DOT configuration '+self.FILECONF
        self.save(self.FILECONF)
    else:
      self.MESSAGE += '\nVECTORPLOT: Saving default DOT configuration '+self.FILECONF
      self.save(self.FILECONF)

  def conf_get(self):
  # =================
    ''' Set class dictionnary from class attributes'''

    conf = {}
    conf['DRAWING_MODE'] = self.DRAWING_MODE.get()
    conf['GRID_MODE'] = self.GRID_MODE.get()
    conf['CURRENT_DX'] = self.CURRENT_DX.get()
    conf['CURRENT_DY'] = self.CURRENT_DY.get()
    conf['CURRENT_NX'] = self.CURRENT_NX.get()
    conf['CURRENT_NY'] = self.CURRENT_NY.get()
    conf['CURRENT_SCALE'] = self.CURRENT_SCALE.get()
    conf['CURRENT_WIDTH'] = self.CURRENT_WIDTH.get()
    conf['CURRENT_HEADLENGTH'] = self.CURRENT_HEADLENGTH.get()
    conf['CURRENT_HEADWIDTH'] = self.CURRENT_HEADWIDTH.get()
    conf['CURRENT_COLOR'] = self.CURRENT_COLOR.get()
    conf['ALPHA'] = self.ALPHA.get()
    conf['ZORDER'] = self.ZORDER.get()

    conf['BARB_LENGTH'] = self.BARB_LENGTH.get()
    conf['BARB_PIVOT'] = self.BARB_PIVOT.get()
    conf['BARB_BARBCOLOR'] = self.BARB_BARBCOLOR.get()
    conf['BARB_FLAGCOLOR'] = self.BARB_FLAGCOLOR.get()
    conf['BARB_SPACING'] = self.BARB_SPACING.get()
    conf['BARB_HEIGHT'] = self.BARB_HEIGHT.get()
    conf['BARB_WIDTH'] = self.BARB_WIDTH.get()
    conf['BARB_EMPTYBARB'] = self.BARB_EMPTYBARB.get()
    conf['BARB_HALF'] = self.BARB_HALF.get()
    conf['BARB_FULL'] = self.BARB_FULL.get()
    conf['BARB_FLAG'] = self.BARB_FLAG.get()
    conf['BARB_LINEWIDTH'] = self.BARB_LINEWIDTH.get()
    conf['BARB_SCALE'] = self.BARB_SCALE.get()

    conf['STREAM_DENSITY'] = self.STREAM_DENSITY.get()
    conf['STREAM_WIDTH'] = self.STREAM_WIDTH.get()
    conf['STREAM_COLOR'] = self.STREAM_COLOR.get()

    conf['COLOR_BY_SPEED'] = self.COLOR_BY_SPEED.get()
    conf['KEY_SHOW'] = self.KEY_SHOW.get()
    conf['KEY_LABEL'] = self.KEY_LABEL.get()
    conf['KEY_X'] = self.KEY_X.get()
    conf['KEY_Y'] = self.KEY_Y.get()
    conf['KEY_VALUE'] = self.KEY_VALUE.get()
    conf['KEY_POS'] = self.KEY_POS.get()
    conf['KEY_COLOR'] = self.KEY_COLOR.get()
    conf['KEY_SIZE'] = self.KEY_SIZE.get()
    conf['ARAKAWA'] = self.ARAKAWA.get()
    return conf

  def conf_set(self,conf):
  # ======================
    ''' Set class attributes from conf class dictionary'''

    self.DRAWING_MODE.set(conf['DRAWING_MODE'])
    self.GRID_MODE.set(conf['GRID_MODE'])
    self.CURRENT_DX.set(conf['CURRENT_DX'])
    self.CURRENT_DY.set(conf['CURRENT_DY'])
    self.CURRENT_NX.set(conf['CURRENT_NX'])
    self.CURRENT_NY.set(conf['CURRENT_NY'])
    self.CURRENT_SCALE.set(conf['CURRENT_SCALE'])
    self.CURRENT_WIDTH.set(conf['CURRENT_WIDTH'])
    self.CURRENT_HEADLENGTH.set(conf['CURRENT_HEADLENGTH'])
    self.CURRENT_HEADWIDTH.set(conf['CURRENT_HEADWIDTH'])
    self.CURRENT_COLOR.set(conf['CURRENT_COLOR'])
    self.ALPHA.set(conf['ALPHA'])
    self.ZORDER.set(conf['ZORDER'])

    self.BARB_LENGTH.set(conf['BARB_LENGTH'])
    self.BARB_PIVOT.set(conf['BARB_PIVOT'])
    self.BARB_BARBCOLOR.set(conf['BARB_BARBCOLOR'])
    self.BARB_FLAGCOLOR.set(conf['BARB_FLAGCOLOR'])
    self.BARB_SPACING.set(conf['BARB_SPACING'])
    self.BARB_HEIGHT.set(conf['BARB_HEIGHT'])
    self.BARB_WIDTH.set(conf['BARB_WIDTH'])
    self.BARB_EMPTYBARB.set(conf['BARB_EMPTYBARB'])
    self.BARB_HALF.set(conf['BARB_HALF'])
    self.BARB_FULL.set(conf['BARB_FULL'])
    self.BARB_FLAG.set(conf['BARB_FLAG'])
    self.BARB_LINEWIDTH.set(conf['BARB_LINEWIDTH'])
    self.BARB_SCALE.set(conf['BARB_SCALE'])

    self.STREAM_DENSITY.set(conf['STREAM_DENSITY'])
    self.STREAM_WIDTH.set(conf['STREAM_WIDTH'])
    self.STREAM_COLOR.set(conf['STREAM_COLOR'])
    self.COLOR_BY_SPEED.set(conf['COLOR_BY_SPEED'])
    self.KEY_SHOW.set(conf['KEY_SHOW'])
    self.KEY_VALUE.set(conf['KEY_VALUE'])
    self.KEY_LABEL.set(conf['KEY_LABEL'])
    self.KEY_X.set(conf['KEY_X'])
    self.KEY_Y.set(conf['KEY_Y'])
    self.KEY_POS.set(conf['KEY_POS'])
    self.KEY_COLOR.set(conf['KEY_COLOR'])
    self.KEY_SIZE.set(conf['KEY_SIZE'])
    self.ARAKAWA.set(conf['ARAKAWA'])

  def conf_load(self,filename):
  # ============================
    ''' Get class dictionnary from file'''

    conf = json.load(open(filename))
    return conf

  def conf_save(self,conf,filename):
  # ================================
    ''' Save class dictionnary to a file'''

    with io.open(filename,'w',encoding='utf8') as outfile:
      str_ = json.dumps(conf,ensure_ascii=False,    \
                             sort_keys=True,        \
                             indent=2,              \
                             separators=(',',': '))
      outfile.write(to_unicode(str_))

  def load(self,filename):
  # ======================
    ''' Set class attributes from configuration file'''

    conf = json.load(open(filename))
    self.conf_set(conf)

  def save(self,filename):
  # ======================
    ''' Save class attributes to a configuration file'''

    conf = self.conf_get()
    self.conf_save(conf,filename)

# =============================
def Configuration(parent,PLOT):
# ========================================
  ''' Interactive widget to modify the options of 2D vector plots'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "December 2017"

  def switchframes():
  # =================
    if PLOT.DRAWING_MODE.get() == 0:
      nb.select(page1)
    elif PLOT.DRAWING_MODE.get() == 1:
      nb.select(page2)
    else:
      nb.select(page3)

  font_bold = tkfont.Font(font='TkDefaultFont').copy()
  font_bold['weight']='bold'

  frame0 = ttk.Frame(parent,borderwidth=10,padding=2,relief='raised')
  ttk.Radiobutton(frame0,text='Quiver plot',
                  variable=PLOT.DRAWING_MODE,
                  command=switchframes,
                  value=0).grid(row=0,column=0,columnspan=2,padx=5)
  ttk.Radiobutton(frame0,text='Barbs plot',
                  variable=PLOT.DRAWING_MODE,
                  command=switchframes,
                  value=1).grid(row=0,column=2,columnspan=2,padx=5)
  ttk.Radiobutton(frame0,text='Streamlines plot',
                  variable=PLOT.DRAWING_MODE,
                  command=switchframes,
                  value=2).grid(row=0,column=4,columnspan=2,padx=5)
  frame0.grid()

  frame = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame,text='Currents grid Mode',font=font_bold)\
      .grid(row=0,column=0,columnspan=4,sticky='ew')

  fleft = ttk.Frame(frame,padding=15)

  ttk.Label(fleft,text='Original Grid').grid(row=0, \
                                             column=0, \
                                             columnspan=1,
                                             sticky='we')
  ttk.Radiobutton(fleft, 
                  variable=PLOT.GRID_MODE,
                  value=0).grid(row=0,column=1,sticky='w')
  ttk.Label(fleft,
            text='X stride',
            width=10).grid(row=1,column=0,
                                  padx=3,
                                  sticky='w')
  ttk.Entry(fleft,
            textvariable=PLOT.CURRENT_DX,
            width=7).grid(row=1,column=1,
                          padx=3,
                          sticky='e')
  ttk.Label(fleft,
            text='Y stride',
            width=10).grid(row=2,column=0,
                           columnspan=1,
                           padx=3,
                           sticky='w')
  ttk.Entry(fleft,
            textvariable=PLOT.CURRENT_DY,
            width=7).grid(row=2,column=1,
                          padx=3,
                          sticky='e')
  fleft.grid(row=1,rowspan=3,column=0,columnspan=2,padx=3)

  fright = ttk.Frame(frame,padding=15)
  ttk.Label(fright,text='Interpolated Grid').grid(row=0, \
                                             column=0, \
                                             columnspan=1,
                                             sticky='we')
  ttk.Radiobutton(fright,\
                  variable=PLOT.GRID_MODE,
                  value=1).grid(row=0,column=1,sticky='w')
  ttk.Label(fright,text='Number X points',
                   width=15).grid(row=1,column=0,padx=3,sticky='w')
  ttk.Entry(fright,
            textvariable=PLOT.CURRENT_NX,
            width=7).grid(row=1,column=1,sticky='e',padx=3)
  ttk.Label(fright,text='Number Y points',
                   width=15).grid(row=6,column=0,columnspan=1,padx=3,sticky='w')
  ttk.Entry(fright,
            textvariable=PLOT.CURRENT_NY,
            width=7).grid(row=6,column=1,sticky='e',padx=3)
  fright.grid(row=1,rowspan=3,column=2,columnspan=2)
  frame.grid()

  # Define tabs:
  nb = ttk.Notebook(parent)
  page1 = ttk.Frame(nb)
  page2 = ttk.Frame(nb)
  page3 = ttk.Frame(nb)
  nb.add(page1,text='Quiver options')
  nb.add(page2,text='Barbs options')
  nb.add(page3,text='Streamlines options')
  nb.grid()

  frame1 = ttk.Frame(page1,borderwidth=5,padding=5)
  salabel = ttk.Style()
  salabel.configure("salabel.TLabel",background=PLOT.CURRENT_COLOR.get(),anchor="center")

  ttk.Label(frame1,
            text='Arrow options',
            font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame1,
            text='Scale').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_SCALE,
            width=8).grid(row=1,column=1,sticky='w')
  ttk.Label(frame1,
            text='Width').grid(row=2,column=0,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_WIDTH,
            width=8).grid(row=2,column=1,sticky='w')
  ttk.Label(frame1,
            text='Head length').grid(row=3,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_HEADLENGTH,width=8). \
            grid(row=3,column=1,sticky='w')
  ttk.Label(frame1,
            text='Head width').grid(row=4,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_HEADWIDTH,
            width=8).grid(row=4,column=1,sticky='w')
  ttk.Label(frame1,text='Color').grid(row=5,column=0,columnspan=1,sticky='w')
  ALabel = ttk.Label(frame1,textvariable=PLOT.CURRENT_COLOR,width=8,style="salabel.TLabel")
  ALabel.grid(row=5,column=1)
  ttk.Button(frame1,text='Select',command=lambda:colsel(PLOT.CURRENT_COLOR, \
            salabel,ALabel,"salabel.TLabel",master=parent)).grid(row=5,column=2)
  ttk.Label(frame1,
            text='Additional arguments',
            font=font_bold).grid(row=6,column=0,sticky='w',pady=[10,1])
  ttk.Label(frame1,
            text='Alpha').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.ALPHA,
            justify='left',
            width=8).grid(row=7,column=1,sticky='w')
  ttk.Label(frame1,
            text='Zorder').grid(row=8,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.ZORDER,
            justify='left',width=8). \
            grid(row=8,column=1,sticky='w')
  frame1.grid()

  frame2 = ttk.Frame(page1,borderwidth=5,padding=5)
  sklabel = ttk.Style()
  sklabel.configure("sklabel.TLabel",background=PLOT.KEY_COLOR.get(),anchor="center")
  ttk.Label(frame2,text='Current Key',
            font=font_bold).grid(row=0,column=0,sticky='w')
  tk.Checkbutton(frame2,text='Show',variable=PLOT.KEY_SHOW,\
                        font=font_bold).grid(row=0,column=2)
  ttk.Label(frame2,text='Label').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_LABEL, \
                   width=20).grid(row=1,column=1,columnspan=2,sticky='w')
  ttk.Label(frame2,text='Value').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_VALUE, \
                   width=8).grid(row=2,column=1,sticky='w')

  def getpos():
  # ===========
    PLOT.KEY_GETXY = True

  ttk.Label(frame2,text='X').grid(row=3,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_X, \
                   width=8).grid(row=3,column=1,sticky='w')
  ttk.Button(frame2,text='Select',command=getpos).grid(row=3,column=2)
  ttk.Label(frame2,text='Y').grid(row=4,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_Y, \
                   width=8).grid(row=4,column=1,sticky='w')
  ttk.Label(frame2,text='Label side').grid(row=5,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_POS, \
                   width=8).grid(row=5,column=1,sticky='w')                  
  ttk.Label(frame2,text='Label color').grid(row=6,column=0,sticky='w')               
  KLabel = ttk.Label(frame2,textvariable=PLOT.KEY_COLOR,width=8,style="sklabel.TLabel")
  KLabel.grid(row=6,column=1,sticky='w')
  ttk.Button(frame2,text='Select',command=lambda:colsel(PLOT.KEY_COLOR, \
            salabel,KLabel,"sklabel.TLabel",master=parent)). \
            grid(row=6,column=2,sticky='w')               
  ttk.Label(frame2,text='Label size').grid(row=7,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_SIZE,width=8). \
            grid(row=7,column=1,sticky='w')
  frame2.grid(row=9,column=0)

  frame5 = ttk.Frame(page2,borderwidth=5,padding=5)
  
  sbalabel, sfllabel = ttk.Style(),ttk.Style()
  sbalabel.configure("sbalabel.TLabel",background=PLOT.BARB_BARBCOLOR.get(),anchor="center")
  sfllabel.configure("sfllabel.TLabel",background=PLOT.BARB_FLAGCOLOR.get(),anchor="center")
  ttk.Label(frame5,
            text='Barb options',
            font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame5,
            text='Length').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_LENGTH,
            width=8).grid(row=1,column=1,sticky='w')
  ttk.Label(frame5,
            text='Pivot').grid(row=2,column=0,sticky='w')
  ttk.Combobox(frame5,
            textvariable=PLOT.BARB_PIVOT,
            width=8,
            values=['tip','middle']).grid(row=2,column=1,sticky='w')
  ttk.Label(frame5,\
            text='Barbcolor').grid(row=3,column=0,sticky='w')
  BLabel = ttk.Label(frame5,textvariable=PLOT.BARB_BARBCOLOR,width=8,style="sbalabel.TLabel")
  BLabel.grid(row=3,column=1,sticky='w')
  ttk.Button(frame5,text='Select',command=lambda:colsel(PLOT.BARB_BARBCOLOR, \
            sbalabel,BLabel,"sbalabel.TLabel",master=parent)). \
            grid(row=3,column=2,sticky='w')   
  ttk.Label(frame5,
            text='Flagcolor').grid(row=4,column=0,sticky='w')
  FLabel = ttk.Label(frame5,textvariable=PLOT.BARB_FLAGCOLOR,width=8,style="sfllabel.TLabel")
  FLabel.grid(row=4,column=1,sticky='w')
  ttk.Button(frame5,text='Select',command=lambda:colsel(PLOT.BARB_FLAGCOLOR, \
            sfllabel,FLabel,"sfllabel.TLabel",master=parent)). \
            grid(row=4,column=2,sticky='w') 

  ttk.Label(frame5,
            text='Linewidth').grid(row=5,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_LINEWIDTH,
            width=7).grid(row=5,column=1,sticky='w')
  ttk.Label(frame5,
            text='Barb increments',
            font=font_bold).grid(row=6,column=0,sticky='w',pady=[10,1])
  ttk.Label(frame5,
            text='Half').grid(row=7,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_HALF,
            width=7).grid(row=7,column=1,sticky='w')
  ttk.Label(frame5,
            text='Full').grid(row=8,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_FULL,
            width=7).grid(row=8,column=1,sticky='w')
  ttk.Label(frame5,
            text='Flag').grid(row=9,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_FLAG,
            width=7).grid(row=9,column=1,sticky='w')
  ttk.Label(frame5,
            text='Barb sizes',
            font=font_bold).grid(row=10,column=0,sticky='w',pady=[10,1])
  ttk.Label(frame5,
            text='Spacing').grid(row=11,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_SPACING,
            width=7).grid(row=11,column=1,sticky='w')
  ttk.Label(frame5,
            text='Height').grid(row=12,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_HEIGHT,
            width=7).grid(row=12,column=1,sticky='w')
  ttk.Label(frame5,
            text='Width').grid(row=13,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_WIDTH,
            width=7).grid(row=13,column=1,sticky='w')
  ttk.Label(frame5,
            text='Emptybarb').grid(row=14,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_EMPTYBARB,
            width=7).grid(row=14,column=1,sticky='w')
  ttk.Label(frame5,
            text='Additional arguments',
            font=font_bold).grid(row=15,column=0,sticky='w',pady=[10,1])
  ttk.Label(frame5,
            text='Alpha').grid(row=16,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.ALPHA,
            width=7).grid(row=16,column=1,sticky='w')
  ttk.Label(frame5,
            text='Zorder').grid(row=17,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.ZORDER,
            width=7).grid(row=17,column=1,sticky='w')
  ttk.Label(frame5,
            text='Scale factor').grid(row=18,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_SCALE,
            width=10).grid(row=18,column=1,sticky='w')
  ttk.Label(frame5,
            text=' To convert to knots (If data '+ \
                   'in m/s: 1.9438445)').grid(row=18, 
                                             column=2, 
                                             columnspan=3, 
                                             sticky='ew')
  frame5.grid()

  frame3 = ttk.Frame(page3,borderwidth=5,padding=5)
  sstlabel = ttk.Style()
  sstlabel.configure("sstlabel.TLabel",background=PLOT.STREAM_COLOR.get(),anchor="center")
  ttk.Label(frame3,text='Stream line options', \
                   font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame3,text='Density').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_DENSITY, \
                   width=8).grid(row=1,column=1,sticky='w')
  ttk.Label(frame3,text='Width').grid(row=2,column=0,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_WIDTH, \
                   width=8).grid(row=2,column=1,sticky='w')
                   
  ttk.Label(frame3,text='Color').grid(row=3,column=0,columnspan=1,sticky='w')
  STLabel = ttk.Label(frame3,textvariable=PLOT.STREAM_COLOR,width=8,style="sstlabel.TLabel")
  STLabel.grid(row=3,column=1,padx=3,sticky='w')
  ttk.Button(frame3,
            text='Select',command=lambda:colsel(PLOT.STREAM_COLOR, \
            sstlabel,STLabel,"sstlabel.TLabel",master=parent)). \
            grid(row=3,column=2,padx=3) 
  ttk.Label(frame3,
            text='Additional arguments',
            font=font_bold).grid(row=4,column=0,sticky='w',pady=[10,1])
  ttk.Label(frame3,
            text='Zorder').grid(row=5,column=0,sticky='w')
  ttk.Entry(frame3,
            textvariable=PLOT.ZORDER,
            width=7).grid(row=5,column=1,sticky='w')
  frame3.grid()

  frame6 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame6,text='Color given by speed', \
            font=font_bold).grid(row=0,column=0,columnspan=2,padx=3,sticky='w')
  ttk.Checkbutton(frame6,variable=PLOT.COLOR_BY_SPEED).grid(row=0,column=2,padx=3)
  frame6.grid()

  switchframes()  

# ===============================
#EG def drawing(fig,ax,m,CFIELD):
def drawing(ax,proj,CFIELD):
# ==========================
  ''' Draw a 2D vector plot. Option of vectors or stream function'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  import numpy as np
  import cartopy

  #EG Remember that CFIELD.VEL.PLOT.MESSAGE may serve to collect
  # messages

  fnt0 = FontProperties()
  font = fnt0.copy()
  font.set_size('x-large')
  
  if CFIELD.PLOT.DRAWING_MODE.get() == 2:
  # -------------------------------------------- STREAMFUNCTION
    CFIELD.PLOT.MESSAGE += "Plot STREAMFUNCTION"+str(CFIELD.PLOT.MESSAGE)
  
    if isinstance(proj,cartopy.crs.PlateCarree):
     #EG this call to quiver with zorder negative is needed to proceed
     #EG with the streamplot. Need further analysis. Cartopy 0.17
     #EG Matplotlib 3.1.1
     #ax.quiver(CFIELD.U.xx, CFIELD.U.yy, CFIELD.U.data, CFIELD.V.data,  \
     #              transform=proj,zorder=-1)
     ax.streamplot(CFIELD.U.xx, CFIELD.U.yy,                  \
                   CFIELD.U.data, CFIELD.V.data,              \
                   color=CFIELD.PLOT.STREAM_COLOR.get(),      \
                   linewidth=CFIELD.PLOT.STREAM_WIDTH.get(),  \
                   density=CFIELD.PLOT.STREAM_DENSITY.get(),  \
                   zorder=CFIELD.PLOT.ZORDER.get(),    \
                   transform=proj)
    else:
     CFIELD.PLOT.MESSAGE += 'VECTORPLOT:  WARNING: Streamplot only works with Cylindircal Projection'
     #print('VECTORPLOT:  WARNING: Streamplot only works with Cylindircal Projection')   
  
  else:

    # Check if reprocessing data is required:
    if CFIELD.reprocess == False:
      if CFIELD.PLOT.GRID_MODE.get()  != CFIELD.GRID_MODE_0: 
        CFIELD.reprocess = True
      if CFIELD.PLOT.CURRENT_DX.get() != CFIELD.CURRENT_DX_0: 
        CFIELD.reprocess = True
      if CFIELD.PLOT.CURRENT_DY.get() != CFIELD.CURRENT_DY_0: 
        CFIELD.reprocess = True
      if CFIELD.PLOT.CURRENT_NX.get() != CFIELD.CURRENT_NX_0: 
        CFIELD.reprocess = True
      if CFIELD.PLOT.CURRENT_NY.get() != CFIELD.CURRENT_NY_0: 
        CFIELD.reprocess = True
      

    if CFIELD.reprocess:
      CFIELD.PLOT.MESSAGE += 'VECTORPLOT: processing data\n'
      CFIELD.reprocess = False
      CFIELD.GRID_MODE_0  = CFIELD.PLOT.GRID_MODE.get()
      CFIELD.CURRENT_DX_0 = CFIELD.PLOT.CURRENT_DX.get()
      CFIELD.CURRENT_DY_0 = CFIELD.PLOT.CURRENT_DY.get()
      CFIELD.CURRENT_NX_0 = CFIELD.PLOT.CURRENT_NX.get()
      CFIELD.CURRENT_NY_0 = CFIELD.PLOT.CURRENT_NY.get()

      if CFIELD.PLOT.GRID_MODE.get() == 0:
        CFIELD.PLOT.MESSAGE += "EG VECTORS: original or decimated grid"	
        #print("EG VECTORS: original or decimated grid")
        dx = CFIELD.PLOT.CURRENT_DX.get()
        dy = CFIELD.PLOT.CURRENT_DY.get()
        CFIELD.xplt, CFIELD.yplt = CFIELD.U.xx[::dy,::dx],   CFIELD.U.yy[::dy,::dx]    # Grid stored in U
        CFIELD.uplt, CFIELD.vplt = CFIELD.U.data[::dy,::dx], CFIELD.V.data[::dy,::dx]
      else:
        CFIELD.PLOT.MESSAGE += "EG VECTORS: fixed grid"
        #print("EG VECTORS: fixed grid")
        #EG The method "linear" in interpol.griddata is mandatory. Other
        #EG choices provides strange behaviour
        lonmin, lonmax, latmin, latmax = ax.get_extent()
        CFIELD.xplt = np.linspace(lonmin,lonmax,CFIELD.PLOT.CURRENT_NX.get())
        CFIELD.yplt = np.linspace(latmin,latmax,CFIELD.PLOT.CURRENT_NY.get())
        n_lon, n_lat = np.meshgrid(CFIELD.xplt,CFIELD.yplt)

        CFIELD.uplt = interpol.griddata((CFIELD.U.xx.flatten(),CFIELD.U.yy.flatten()), \
                  CFIELD.U.data.flatten(),(n_lon,n_lat), method='linear')
        CFIELD.vplt = interpol.griddata((CFIELD.U.xx.flatten(),CFIELD.U.yy.flatten()), \
                  CFIELD.V.data.flatten(),(n_lon,n_lat), method='linear')
      
      CFIELD.speed = np.sqrt(CFIELD.uplt**2+CFIELD.vplt**2)
      CFIELD.reprocess = False
    
    if CFIELD.PLOT.DRAWING_MODE.get() == 0:
    # -------------------------------------------- VECTORS
      CFIELD.PLOT.MESSAGE += "EG VECTORPLOT: Arrows"
      #print("EG VECTORPLOT: Arrows")
      if CFIELD.PLOT.COLOR_BY_SPEED.get():
        quiver = ax.quiver(CFIELD.xplt,CFIELD.yplt,CFIELD.uplt,CFIELD.vplt,CFIELD.speed,                       \
                      transform=proj, \
                      color=CFIELD.PLOT.CURRENT_COLOR.get(),           \
                      width=CFIELD.PLOT.CURRENT_WIDTH.get(),           \
                      headwidth=CFIELD.PLOT.CURRENT_HEADWIDTH.get(),   \
                      headlength=CFIELD.PLOT.CURRENT_HEADLENGTH.get(), \
                      scale=CFIELD.PLOT.CURRENT_SCALE.get(),
                      alpha=CFIELD.PLOT.ALPHA.get(),
                      zorder=CFIELD.PLOT.ZORDER.get())
      else:
        quiver = ax.quiver(CFIELD.xplt,CFIELD.yplt,CFIELD.uplt,CFIELD.vplt,  \
                      transform=proj, \
                      color=CFIELD.PLOT.CURRENT_COLOR.get(),           \
                      width=CFIELD.PLOT.CURRENT_WIDTH.get(),           \
                      headwidth=CFIELD.PLOT.CURRENT_HEADWIDTH.get(),   \
                      headlength=CFIELD.PLOT.CURRENT_HEADLENGTH.get(), \
                      scale=CFIELD.PLOT.CURRENT_SCALE.get(),
                      alpha=CFIELD.PLOT.ALPHA.get(),
                      zorder=CFIELD.PLOT.ZORDER.get())
                      
      if CFIELD.PLOT.KEY_SHOW.get():
        CFIELD.PLOT.KEY_OBJ = ax.quiverkey(quiver,
                        CFIELD.PLOT.KEY_X.get(),
                        CFIELD.PLOT.KEY_Y.get(),
                        CFIELD.PLOT.KEY_VALUE.get(),
                        CFIELD.PLOT.KEY_LABEL.get(),
                        labelpos=CFIELD.PLOT.KEY_POS.get(),
                        coordinates='figure',
                        transform=proj,
                        labelcolor=CFIELD.PLOT.KEY_COLOR.get(),
                        fontproperties={'size':CFIELD.PLOT.KEY_SIZE.get()})

    elif CFIELD.PLOT.DRAWING_MODE.get() == 1:
    # -------------------------------------------- BARBS
      CFIELD.PLOT.MESSAGE += "EG VECTORPLOT: BARBS"
      #print("EG VECTORPLOT: BARBS")
      # Barb plot assumes knots
      knots = CFIELD.PLOT.BARB_SCALE.get()
      barb_increments = {'half': CFIELD.PLOT.BARB_HALF.get(),
                       'full': CFIELD.PLOT.BARB_FULL.get(),
                       'flag': CFIELD.PLOT.BARB_FLAG.get()}
      sizes = {'spacing': CFIELD.PLOT.BARB_SPACING.get(),
             'height': CFIELD.PLOT.BARB_HEIGHT.get(),
             'width': CFIELD.PLOT.BARB_WIDTH.get(),
             'emptybarb': CFIELD.PLOT.BARB_EMPTYBARB.get()}
             
      if CFIELD.PLOT.COLOR_BY_SPEED.get():
        speedk = knots*speed
        barbs = ax.barbs(CFIELD.xplt,CFIELD.yplt,knots*CFIELD.uplt,knots*CFIELD.vplt,speedk,
                        pivot=CFIELD.PLOT.BARB_PIVOT.get(),
                        length=CFIELD.PLOT.BARB_LENGTH.get(),
                        linewidth=CFIELD.PLOT.BARB_LINEWIDTH.get(),
                        barb_increments=barb_increments,
                        sizes=sizes,
                        transform=proj, 
                        alpha=CFIELD.PLOT.ALPHA.get(),
                        zorder=CFIELD.PLOT.ZORDER.get())  
      else:
        barbs = ax.barbs(CFIELD.xplt,CFIELD.yplt,knots*CFIELD.uplt,knots*CFIELD.vplt,
                        pivot=CFIELD.PLOT.BARB_PIVOT.get(),
                        length=CFIELD.PLOT.BARB_LENGTH.get(),
                        barbcolor=CFIELD.PLOT.BARB_BARBCOLOR.get(),
                        flagcolor=CFIELD.PLOT.BARB_FLAGCOLOR.get(),
                        linewidth=CFIELD.PLOT.BARB_LINEWIDTH.get(),
                        barb_increments=barb_increments,
                        sizes=sizes,
                        transform=proj, 
                        alpha=CFIELD.PLOT.ALPHA.get(),
                        zorder=CFIELD.PLOT.ZORDER.get())
