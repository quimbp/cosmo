'''
=======================================================================
Obtain release position from GEOJSON trajectory
=======================================================================

Quim Ballabrera, December 2017.
COSMO project
Reads a COSMO-project GEOJSON trajectory and returns the initial 
position and date.
Version 0.2, released December 2017
'''
import json
import sys

__progname__ = 'RELEASE_POINT'
__version__  = '0.2'
__date__     = 'December 2017'
__author__   = 'Quim Ballabrera (ICM/CSIC)'

def usage():
# ==========
  print('Error: No input file')
  print('Usage:')
  print('> python release_point [-label] FILENAME')
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
    if empty(sys.argv[1]):
      usage()
  except:
    usage()

  if len(sys.argv) > 3:
    usage()

  if len(sys.argv) == 3:
    output_format = sys.argv[1]
    filename = sys.argv[2]
  else:
    filename = sys.argv[1]

  print('Opening file',filename)

  try:
    with open(filename) as data_file:
      DATA = json.load(data_file)
      for i in range(len(DATA["features"])):
        if DATA["features"][i]["geometry"]["type"] == "Point":
          if DATA["features"][i]["properties"]["event"] == 0:
            lon = DATA["features"][i]["geometry"]["coordinates"][0]
            lat = DATA["features"][i]["geometry"]["coordinates"][1]
            date = DATA["features"][i]["properties"]["time"]["data"][0][:-1]
  except:
    print('Unable to open input file')

  if len(sys.argv) == 3:
    print('-xo ',lon,'-yo ',lat,'-do ',date)
  else:
    print(lon,lat,date)


if __name__ == "__main__":
  main()

