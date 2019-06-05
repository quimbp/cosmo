import json

with open('toto.geojson') as infile:
  data = json.load(infile)

print ('File       : ', data['file'])
print ('Source     : ', data['source'])
print ('Convenstion: ', data['convention'])
print ('Variables  : ', data['varnames'])
