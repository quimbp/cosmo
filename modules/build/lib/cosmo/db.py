# POSTGRESQL DB with Lagrangian trajectories

try:
  import tkinter as tk
  from tkinter import ttk
except:
  import Tkinter as tk
  import ttk

#import postgresql
import psycopg2
import wget
from os.path import isfile, join
from cosmo.tools import simple_form
from cosmo.tools import empty

def conectadb():
    #
    #    user = "dof_read"
    #    passwd = "eU9Za2ofNGfGUGe8"
    #    server = "bbdd.cmima.csic.es"
    #    datab = "dof"
    #    db =
    #    postgresql.open("pq://"+user+":"+passwd+"@"+server+"/"+datab)
    #    db.connect()
    params = {'database': 'dof',
              'user': 'dof_read',
              'password': 'eU9Za2ofNGfGUGe8',
              'host': 'bbdd.cmima.csic.es',
              'port': '5432'}
    conn = psycopg2.connect(**params)
    db = conn.cursor()
    return db

# A partir d'un nom d'experiment i del conector a la db recuperem les
# boies
def get_buoys_from_exp(db,name):
    query = "SELECT a.name FROM d_experiment as a INNER JOIN drifter_exp_test as b ON b.id = a.exp_id " \
    "WHERE b.name='CONECTA-2016' ORDER by a.id"
    #ps = db.prepare(query)
    db.execute(query)
    items = db.fetchall()
    return [items[i][0] for i in range(len(items))]

# Recuperem tots els experiment fets a l'ICM
def get_exp(db):
    query = "SELECT * FROM drifter_exp_test ORDER by id"
    #ps = db.prepare(query)
    db.execute(query)
    items = db.fetchall()
    return [items[i][:] for i in range(len(items))]

def select_exp():
# =================

  # Connect to DB
  # Obtain experiments
  DB = conectadb()
  a = get_exp(DB)

  DBNAME = tk.StringVar()
  URL = tk.StringVar()

  BDLIST = []
  URLIST = []
  for i in range(len(a)):
    BDLIST.append(a[i][1])
    URLIST.append(a[i][8])

  DBNAME.set(BDLIST[0])
  URL.set(URLIST[0])

  a = get_buoys_from_exp(DB,DBNAME.get())
  FLIST = []
  for i in range(len(a)):
    FLIST.append(join(URL.get(),a[i]+'.geojson'))

  def _cancel():
  # ===========
    FLIST  = []
    win.destroy()

  def _get():
  # ===========
    out = tk.filedialog.askdirectory(parent=win,title='Target folder')

    #out = simple_form('Output folder for download files','path','./')
    if empty(out):
      out = None
    for i in range(len(FLIST)):
      bb = wget.download(FLIST[i],out=out)

  def _done():
  # ===========
    win.destroy()

  def _selection():
  # ===============
    i = BDLIST.index(DBNAME.get())
    URL.set(URLIST[i])
    a = get_buoys_from_exp(DB,DBNAME.get())
    FLIST = []
    for i in range(len(a)):
      FLIST.append(join(URL.get(),a[i]+'.geojson'))


  win = tk.Toplevel()
  win.title('Database Selector')
  win.resizable(False,False)
  win.protocol('WM_DELETE_WINDOW',_cancel)

  F0 = ttk.Frame(win,padding=5)
  _source = ttk.Combobox(F0,textvariable=DBNAME, \
                         values=BDLIST,width=20)
  _source.grid(row=0,column=0,columnspan=2,padx=3)
  _source.bind('<<ComboboxSelected>>',lambda e: _selection())
  _entry = ttk.Entry(F0,textvariable=URL,width=60)
  _entry.grid(row=0,column=2,columnspan=6,padx=3)
  _entry.configure(state='readonly')
  ttk.Button(F0,text='Cancel',command=_cancel).grid(row=1,column=3,padx=0)
  ttk.Button(F0,text='Download a copy',
             command=_get).grid(row=1,column=4,columnspan=3,padx=0)
  ttk.Button(F0,text='Done',command=_done).grid(row=1,column=7,padx=0)
  F0.grid()

  win.wait_window(win)
  return FLIST
