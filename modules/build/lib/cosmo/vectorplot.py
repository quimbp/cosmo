# Module for plotting vectors, built for the COSMO project 
# Quim Ballabrera, May 2017

try:
  import tkinter as tk
  from tkinter import ttk
  from tkinter import messagebox
  from tkcolorpicker import askcolor
  from tkinter import font as tkfont
except:
  import Tkinter as tk
  import ttk
  import tkMessageBox as messagebox
  from tkColorChooser import askcolor
  import tkFont as tkfont

import json
import os
import io
try:
  to_unicode = unicode
except:
  to_unicode = str

from matplotlib.font_manager import FontProperties
from cosmo.tools import exists
from cosmo import COSMO_CONF_PATH
from cosmo import COSMO_CONF_DATA


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
    self.CURRENT_ALPHA     = tk.DoubleVar()
    self.CURRENT_ZORDER    = tk.IntVar()

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
    self.BARB_ALPHA        = tk.DoubleVar()
    self.BARB_LINEWIDTH    = tk.DoubleVar()
    self.BARB_ZORDER       = tk.IntVar()
    self.BARB_SCALE        = tk.DoubleVar()

    self.STREAM_DENSITY    = tk.DoubleVar()
    self.STREAM_WIDTH      = tk.DoubleVar()
    self.STREAM_COLOR      = tk.StringVar()
    self.STREAM_ZORDER     = tk.IntVar()

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

    # Defautl attribute values
    #
    self.DRAWING_MODE.set(0)     # 0 - Vector, 1 - Barb, 2 - Streamplot
    self.GRID_MODE.set(1)
    self.CURRENT_DX.set(4)
    self.CURRENT_DY.set(4)
    self.CURRENT_NX.set(41)
    self.CURRENT_NY.set(41)
    self.CURRENT_SCALE.set(20)
    self.CURRENT_WIDTH.set(0.002)
    self.CURRENT_HEADLENGTH.set(5)
    self.CURRENT_HEADWIDTH.set(3)
    self.CURRENT_COLOR.set('black')
    self.CURRENT_ALPHA.set(1.0)
    self.CURRENT_ZORDER.set(1)
    self.BARB_PIVOT.set('middle')
    self.BARB_LENGTH.set(7)
    self.BARB_BARBCOLOR.set('black')
    self.BARB_FLAGCOLOR.set('black')
    self.BARB_LINEWIDTH.set(1.0)
    self.BARB_ALPHA.set(1.0)
    self.BARB_ZORDER.set(2)
    self.BARB_SCALE.set(1.9438445)
    self.BARB_HALF.set(5)
    self.BARB_FULL.set(10)
    self.BARB_FLAG.set(50)
    self.BARB_SPACING.set(0.125)
    self.BARB_HEIGHT.set(0.40)
    self.BARB_WIDTH.set(0.25)
    self.BARB_EMPTYBARB.set(0.15)
    self.STREAM_WIDTH.set(2)
    self.STREAM_COLOR.set('black')
    self.STREAM_ZORDER.set(1)
    self.COLOR_BY_SPEED.set(False)
    self.KEY_SHOW.set(True)
    self.KEY_VALUE.set(1)
    self.KEY_LABEL.set('m/s')
    self.KEY_X.set(0.85)
    self.KEY_Y.set(0.12)
    self.KEY_POS.set('E')
    self.KEY_COLOR.set('black')
    self.KEY_SIZE.set(12)

    # If configuration file exists, it is read and
    # the default values are overrided. If the configuration
    # file does not exist, it is created using the default values.
    #
    if exists(self.FILECONF):
      try:
        print('Loading VECTOR configuration file '+self.FILECONF)
        self.load(self.FILECONF)
      except:
        print('Error: Saving default DOT configuration '+self.FILECONF)
        self.save(self.FILECONF)
    else:
      print('Saving default DOT configuration '+self.FILECONF)
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
    conf['CURRENT_ALPHA'] = self.CURRENT_ALPHA.get()
    conf['CURRENT_ZORDER'] = self.CURRENT_ZORDER.get()

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
    conf['BARB_ALPHA'] = self.BARB_ALPHA.get()
    conf['BARB_LINEWIDTH'] = self.BARB_LINEWIDTH.get()
    conf['BARB_ZORDER'] = self.BARB_ZORDER.get()
    conf['BARB_SCALE'] = self.BARB_SCALE.get()

    conf['STREAM_DENSITY'] = self.STREAM_DENSITY.get()
    conf['STREAM_WIDTH'] = self.STREAM_WIDTH.get()
    conf['STREAM_COLOR'] = self.STREAM_COLOR.get()
    conf['STREAM_ZORDER'] = self.STREAM_ZORDER.get()

    conf['COLOR_BY_SPEED'] = self.COLOR_BY_SPEED.get()
    conf['KEY_SHOW'] = self.KEY_SHOW.get()
    conf['KEY_LABEL'] = self.KEY_LABEL.get()
    conf['KEY_X'] = self.KEY_X.get()
    conf['KEY_Y'] = self.KEY_Y.get()
    conf['KEY_VALUE'] = self.KEY_VALUE.get()
    conf['KEY_POS'] = self.KEY_POS.get()
    conf['KEY_COLOR'] = self.KEY_COLOR.get()
    conf['KEY_SIZE'] = self.KEY_SIZE.get()
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
    self.CURRENT_ALPHA.set(conf['CURRENT_ALPHA'])
    self.CURRENT_ZORDER.set(conf['CURRENT_ZORDER'])

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
    self.BARB_ALPHA.set(conf['BARB_ALPHA'])
    self.BARB_LINEWIDTH.set(conf['BARB_LINEWIDTH'])
    self.BARB_ZORDER.set(conf['BARB_ZORDER'])
    self.BARB_SCALE.set(conf['BARB_SCALE'])

    self.STREAM_DENSITY.set(conf['STREAM_DENSITY'])
    self.STREAM_WIDTH.set(conf['STREAM_WIDTH'])
    self.STREAM_COLOR.set(conf['STREAM_COLOR'])
    self.STREAM_ZORDER.set(conf['STREAM_ZORDER'])
    self.COLOR_BY_SPEED.set(conf['COLOR_BY_SPEED'])
    self.KEY_SHOW.set(conf['KEY_SHOW'])
    self.KEY_VALUE.set(conf['KEY_VALUE'])
    self.KEY_LABEL.set(conf['KEY_LABEL'])
    self.KEY_X.set(conf['KEY_X'])
    self.KEY_Y.set(conf['KEY_Y'])
    self.KEY_POS.set(conf['KEY_POS'])
    self.KEY_COLOR.set(conf['KEY_COLOR'])
    self.KEY_SIZE.set(conf['KEY_SIZE'])

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


# ========================================
def Configuration(parent,PLOT):
# ========================================
  ''' Interactive widget to modify the options of 2D vector plots'''

  __version__ = "1.1"
  __author__  = "Quim Ballabrerera"
  __date__    = "December 2017"

  def cselection():
    if PLOT.CURRENT_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.CURRENT_COLOR.get(),parent=parent)
    PLOT.CURRENT_COLOR.set(hx)


  def tselection():
    if PLOT.STREAM_COLOR.get() == 'None':
      rgb, hx = askcolor(parent=parent)
    else:
      rgb, hx = askcolor(color=PLOT.STREAM_COLOR.get(),parent=parent)
    PLOT.STREAM_COLOR.set(hx)

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
  ttk.Radiobutton(frame0,
                  text='Quiver plot',
                  variable=PLOT.DRAWING_MODE,
                  command=switchframes,
                  value=0).grid(row=0,column=0,columnspan=2,padx=5)
  ttk.Radiobutton(frame0,
                  text='Barbs plot',
                  variable=PLOT.DRAWING_MODE,
                  command=switchframes,
                  value=1).grid(row=0,column=2,columnspan=2,padx=5)
  ttk.Radiobutton(frame0,
                  text='Streamlines plot',
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
            width=10).grid(row=1,
                                  column=0,
                                  padx=3,
                                  sticky='w')
  ttk.Entry(fleft,
            textvariable=PLOT.CURRENT_DX,
            width=7).grid(row=1,
                          column=1,
                          padx=3,
                          sticky='e')
  ttk.Label(fleft,
            text='Y stride',
            width=10).grid(row=2,
                           column=0,
                           columnspan=1,
                           padx=3,
                           sticky='w')
  ttk.Entry(fleft,
            textvariable=PLOT.CURRENT_DY,
            width=7).grid(row=2,
                          column=1,
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
  ttk.Label(frame1,
            text='Arrow options',
            font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame1,
            text='Scale').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_SCALE,
            width=7).grid(row=1,column=1,sticky='w')
  ttk.Label(frame1,
            text='Width').grid(row=2,column=0,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_WIDTH,
            width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(frame1,
            text='Head length').grid(row=3,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_HEADLENGTH,
            width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(frame1,
            text='Head width').grid(row=4,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_HEADWIDTH,
            width=7).grid(row=4,column=1,sticky='w')
  ttk.Label(frame1,
            text='Color').grid(row=5,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_COLOR,
            justify='left',
            width=7).grid(row=5,column=1,sticky='w')
  ttk.Button(frame1,
            text='Select',
            command=cselection).grid(row=5,column=2,padx=3,sticky='ew')
  ttk.Label(frame1,
            text='Additional arguments',
            font=font_bold).grid(row=6,column=0,sticky='w',pady=[10,1])
  ttk.Label(frame1,
            text='Alpha').grid(row=7,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_ALPHA,
            justify='left',
            width=7).grid(row=7,column=1,sticky='w')
  ttk.Label(frame1,
            text='Zorder').grid(row=8,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame1,
            textvariable=PLOT.CURRENT_ZORDER,
            justify='left',
            width=7).grid(row=8,column=1,sticky='w')
  frame1.grid()

  frame2 = ttk.Frame(page1,borderwidth=5,padding=5)
  ttk.Label(frame2,
            text='Current Key',
            font=font_bold).grid(row=0,column=0,sticky='w')
  tk.Checkbutton(frame2,text='Show',variable=PLOT.KEY_SHOW,\
                        font=font_bold).grid(row=0,column=2)
  ttk.Label(frame2,text='Label').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_LABEL, \
                   width=20).grid(row=1,column=1,columnspan=2,sticky='w')
  ttk.Label(frame2,text='Value').grid(row=2,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_VALUE, \
                   width=7).grid(row=2,column=1,sticky='w')

  def getpos():
  # ===========
    PLOT.KEY_GETXY = True

  ttk.Label(frame2,text='X').grid(row=3,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_X, \
                   width=7).grid(row=3,column=1,sticky='w')
  ttk.Button(frame2,text='Select',command=getpos).grid(row=3,column=2)
  ttk.Label(frame2,text='Y').grid(row=4,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_Y, \
                   width=7).grid(row=4,column=1,sticky='w')
  ttk.Label(frame2,text='Label side').grid(row=5,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_POS, \
                   width=7).grid(row=5,column=1,sticky='w')
  ttk.Label(frame2,text='Label color').grid(row=6,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_COLOR, \
                   width=7).grid(row=6,column=1,sticky='w')
  ttk.Label(frame2,text='Label size').grid(row=7,column=0,sticky='w')
  ttk.Entry(frame2,textvariable=PLOT.KEY_SIZE, \
                   width=7).grid(row=7,column=1,sticky='w')
  frame2.grid(row=9,column=0)

  frame5 = ttk.Frame(page2,borderwidth=5,padding=5)
  ttk.Label(frame5,
            text='Barb options',
            font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame5,
            text='Length').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_LENGTH,
            width=7).grid(row=1,column=1,sticky='w')
  ttk.Label(frame5,
            text='Pivot').grid(row=2,column=0,sticky='w')
  ttk.Combobox(frame5,
            textvariable=PLOT.BARB_PIVOT,
            width=7,
            values=['tip','middle']).grid(row=2,column=1,sticky='w')
  ttk.Label(frame5,
            text='Barbcolor').grid(row=3,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_BARBCOLOR,
            width=7).grid(row=3,column=1,sticky='w')
  ttk.Label(frame5,
            text='Flagcolor').grid(row=4,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_FLAGCOLOR,
            width=7).grid(row=4,column=1,sticky='w')
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
            textvariable=PLOT.BARB_ALPHA,
            width=7).grid(row=16,column=1,sticky='w')
  ttk.Label(frame5,
            text='Zorder').grid(row=17,column=0,sticky='w')
  ttk.Entry(frame5,
            textvariable=PLOT.BARB_ZORDER,
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
  ttk.Label(frame3,text='Stream line options', \
                   font=font_bold).grid(row=0,column=0,sticky='w')
  ttk.Label(frame3,text='Density').grid(row=1,column=0,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_DENSITY, \
                   width=7).grid(row=1,column=1,sticky='w')
  ttk.Label(frame3,text='Width').grid(row=2,column=0,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_WIDTH, \
                   width=7).grid(row=2,column=1,sticky='w')
  ttk.Label(frame3,text='Color').grid(row=3,column=0,columnspan=1,sticky='w')
  ttk.Entry(frame3,textvariable=PLOT.STREAM_COLOR, \
                   justify='left',width=7).grid(row=3,column=1,sticky='w')
  ttk.Button(frame3,text='Select',command=tselection).grid(row=3,column=2,padx=3,sticky='ew')
  ttk.Label(frame3,
            text='Additional arguments',
            font=font_bold).grid(row=4,column=0,sticky='w',pady=[10,1])
  ttk.Label(frame3,
            text='Zorder').grid(row=5,column=0,sticky='w')
  ttk.Entry(frame3,
            textvariable=PLOT.STREAM_ZORDER,
            width=7).grid(row=5,column=1,sticky='w')
  frame3.grid()

  frame6 = ttk.Frame(parent,borderwidth=5,padding=5)
  ttk.Label(frame6,text='Color given by speed', \
            font=font_bold).grid(row=0,column=0,columnspan=2,padx=3,sticky='w')
  ttk.Checkbutton(frame6,variable=PLOT.COLOR_BY_SPEED).grid(row=0,column=2,padx=3)
  frame6.grid()

  switchframes()  

# ======================================
def drawing(fig,ax,m,CFIELD):
# ======================================
  ''' Draw a 2D vector plot. Option of vectors or stream function'''

  __version__ = "1.0"
  __author__  = "Quim Ballabrerera"
  __date__    = "June 2017"

  import numpy as np
  fnt0 = FontProperties()
  font = fnt0.copy()
  font.set_size('x-large')

  if CFIELD.VEL.PLOT.DRAWING_MODE.get() == 0:
  # -------------------------------------------- VECTORS

    if CFIELD.VEL.PLOT.GRID_MODE.get() == 0:
      dx = CFIELD.VEL.PLOT.CURRENT_DX.get()
      dy = CFIELD.VEL.PLOT.CURRENT_DY.get()
      uplt,vplt,xplt,yplt = m.rotate_vector(CFIELD.VEL.u[::dy,::dx], \
                                            CFIELD.VEL.v[::dy,::dx], \
                                            CFIELD.lon[::dx],   \
                                            CFIELD.lat[::dy],   \
                                            returnxy=True)
    else:
      uplt,vplt,xplt,yplt = m.transform_vector(CFIELD.VEL.u, \
                                           CFIELD.VEL.v, \
                                           CFIELD.lon,   \
                                           CFIELD.lat,   \
                                           CFIELD.VEL.PLOT.CURRENT_NX.get(), \
                                           CFIELD.VEL.PLOT.CURRENT_NY.get(), \
                                           returnxy=True,                \
                                           masked=True)

    if CFIELD.VEL.PLOT.COLOR_BY_SPEED.get():
      speed = np.sqrt(uplt**2+vplt**2)
      quiver = m.quiver(xplt,yplt,uplt,vplt,speed,                       \
                      color=CFIELD.VEL.PLOT.CURRENT_COLOR.get(),           \
                      width=CFIELD.VEL.PLOT.CURRENT_WIDTH.get(),           \
                      headwidth=CFIELD.VEL.PLOT.CURRENT_HEADWIDTH.get(),   \
                      headlength=CFIELD.VEL.PLOT.CURRENT_HEADLENGTH.get(), \
                      scale=CFIELD.VEL.PLOT.CURRENT_SCALE.get(),
                      alpha=CFIELD.VEL.PLOT.CURRENT_ALPHA.get(),
                      zorder=CFIELD.VEL.PLOT.CURRENT_ZORDER.get(),
                      )

    else:
      quiver = m.quiver(xplt,yplt,uplt,vplt,                             \
                      color=CFIELD.VEL.PLOT.CURRENT_COLOR.get(),           \
                      width=CFIELD.VEL.PLOT.CURRENT_WIDTH.get(),           \
                      headwidth=CFIELD.VEL.PLOT.CURRENT_HEADWIDTH.get(),   \
                      headlength=CFIELD.VEL.PLOT.CURRENT_HEADLENGTH.get(), \
                      scale=CFIELD.VEL.PLOT.CURRENT_SCALE.get(),
                      alpha=CFIELD.VEL.PLOT.CURRENT_ALPHA.get(),
                      zorder=CFIELD.VEL.PLOT.CURRENT_ZORDER.get(),
                      )

    if CFIELD.VEL.PLOT.KEY_SHOW.get():
      CFIELD.VEL.PLOT.KEY_OBJ = ax.quiverkey(quiver,
                          CFIELD.VEL.PLOT.KEY_X.get(),
                          CFIELD.VEL.PLOT.KEY_Y.get(),
                          CFIELD.VEL.PLOT.KEY_VALUE.get(),
                          CFIELD.VEL.PLOT.KEY_LABEL.get(),
                          labelpos=CFIELD.VEL.PLOT.KEY_POS.get(),
                          coordinates='figure',
                          labelcolor=CFIELD.VEL.PLOT.KEY_COLOR.get(),
                          fontproperties={'size':
                                           CFIELD.VEL.PLOT.KEY_SIZE.get()},
                  )


  elif CFIELD.VEL.PLOT.DRAWING_MODE.get() == 1:
  # -------------------------------------------- BARBS

    # Barb plot assumes knots
    knots = CFIELD.VEL.PLOT.BARB_SCALE.get()
    barb_increments = {'half': CFIELD.VEL.PLOT.BARB_HALF.get(),
                       'full': CFIELD.VEL.PLOT.BARB_FULL.get(),
                       'flag': CFIELD.VEL.PLOT.BARB_FLAG.get()}
    sizes = {'spacing': CFIELD.VEL.PLOT.BARB_SPACING.get(),
             'height': CFIELD.VEL.PLOT.BARB_HEIGHT.get(),
             'width': CFIELD.VEL.PLOT.BARB_WIDTH.get(),
             'emptybarb': CFIELD.VEL.PLOT.BARB_EMPTYBARB.get()}


    if CFIELD.VEL.PLOT.GRID_MODE.get() == 0:
      dx = CFIELD.VEL.PLOT.CURRENT_DX.get()
      dy = CFIELD.VEL.PLOT.CURRENT_DY.get()
      uplt,vplt,xplt,yplt = m.rotate_vector(CFIELD.VEL.u[::dy,::dx], \
                                            CFIELD.VEL.v[::dy,::dx], \
                                            CFIELD.lon[::dx],   \
                                            CFIELD.lat[::dy],   \
                                            returnxy=True)
    else:
      uplt,vplt,xplt,yplt = m.transform_vector(CFIELD.VEL.u, \
                                           CFIELD.VEL.v, \
                                           CFIELD.lon,   \
                                           CFIELD.lat,   \
                                           CFIELD.VEL.PLOT.CURRENT_NX.get(), \
                                           CFIELD.VEL.PLOT.CURRENT_NY.get(), \
                                           returnxy=True,                \
                                           masked=True)

      if CFIELD.VEL.PLOT.COLOR_BY_SPEED.get():
        speed = knots*np.sqrt(uplt**2+vplt**2)
        barbs = m.barbs(xplt,yplt,knots*uplt,knots*vplt,speed,
                        pivot=CFIELD.VEL.PLOT.BARB_PIVOT.get(),
                        length=CFIELD.VEL.PLOT.BARB_LENGTH.get(),
                        linewidth=CFIELD.VEL.PLOT.BARB_LINEWIDTH.get(),
                        barb_increments=barb_increments,
                        sizes=sizes,
                        alpha=CFIELD.VEL.PLOT.BARB_ALPHA.get(),
                        zorder=CFIELD.VEL.PLOT.BARB_ZORDER.get(),
                       )
      else:
        barbs = m.barbs(xplt,yplt,knots*uplt,knots*vplt,
                        pivot=CFIELD.VEL.PLOT.BARB_PIVOT.get(),
                        length=CFIELD.VEL.PLOT.BARB_LENGTH.get(),
                        barbcolor=CFIELD.VEL.PLOT.BARB_BARBCOLOR.get(),
                        flagcolor=CFIELD.VEL.PLOT.BARB_FLAGCOLOR.get(),
                        barb_increments=barb_increments,
                        sizes=sizes,
                        linewidth=CFIELD.VEL.PLOT.BARB_LINEWIDTH.get(),
                        alpha=CFIELD.VEL.PLOT.BARB_ALPHA.get(),
                        zorder=CFIELD.VEL.PLOT.BARB_ZORDER.get(),
                       )


  elif CFIELD.VEL.PLOT.DRAWING_MODE.get() == 2:
  # -------------------------------------------- STREAMFUNCTION 
    if m.projparams['proj'] == 'cyl':
      # -------------------
      ax.streamplot(CFIELD.xx, CFIELD.yy, CFIELD.VEL.u, CFIELD.VEL.v,      \
                   color=CFIELD.VEL.PLOT.STREAM_COLOR.get(),           \
                   linewidth=CFIELD.VEL.PLOT.STREAM_WIDTH.get(),       \
                   zorder=CFIELD.VEL.PLOT.STREAM_ZORDER.get(),       \
                   density=CFIELD.VEL.PLOT.STREAM_DENSITY.get())
    else:
      print('WARNING: Streamplot only works with Cylindircal Projection') 
