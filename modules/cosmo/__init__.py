import os
import json
import io

try:
  to_unicode = unicode
except:
  to_unicode = str

VERSION = '1.0'


COSMO_ROOT="/home/joaquim/projects/cosmo"
COSMO_CONF_PATH = COSMO_ROOT + '/conf/'
COSMO_CONF_NAME = 'default'
COSMO_CONF_DATA = COSMO_CONF_PATH+'data.conf'

if os.path.isfile(COSMO_CONF_DATA):
  with open(COSMO_CONF_DATA) as infile:
    conf = json.load(infile)
  #COSMO_CONF_PATH = conf['COSMO_CONF_PATH']
  COSMO_CONF_NAME = conf['COSMO_CONF_NAME']
else:
  conf = {}
  #conf['COSMO_CONF_PATH']=COSMO_CONF_PATH
  conf['COSMO_CONF_NAME']=COSMO_CONF_NAME
  with io.open(COSMO_CONF_DATA,'w',encoding='utf8') as outfile:
    _str = json.dumps(conf,ensure_ascii=False,
                           sort_keys=False,
                           indent=2,
                           separators=(',',': '))
    outfile.write(to_unicode(_str))
    outfile.close()

COSMO_CONF = COSMO_CONF_PATH + COSMO_CONF_NAME + os.sep


from cosmo import ncdump
from cosmo import tools
from cosmo import contourplot
from cosmo import vectorplot
from cosmo import lineplot
from cosmo import providers
from cosmo import codar
from cosmo import copernicus
from cosmo import saidin
from cosmo import lagrangian
from cosmo import blm
from cosmo import mlm
from cosmo import geomarker
from cosmo import drawing
from cosmo import db
from cosmo import json_editor
from cosmo import legend

