use cosmo

implicit none


real(dp) x1,x2,y1,y2

x1 = -4.0839520530704121 * deg2rad
y1 = 36.130845150889883* deg2rad

x2 = -4.0858035726343953* deg2rad
y2 = 36.130983323439665* deg2rad

print*, haversine(x1,y1,x2,y2)


x2 = -4.0955060401875754* deg2rad
y2 = 36.130122356975214 * deg2rad

print*, haversine(x1,y1,x2,y2)

x1 = -4.23_dp* deg2rad
y1 = 35.79_dp* deg2rad

x2 =  -4.2308994570560667 * deg2rad
y2 =  35.795425066464134 * deg2rad

print*, haversine(x1,y1,x2,y2)

x2 =  -4.2281751602129303  * deg2rad
y2 =  35.810390085363039* deg2rad

print*, haversine(x1,y1,x2,y2)
x2 =  -4.2302049720819550 * deg2rad
y2 =  35.801124105899596* deg2rad

print*, haversine(x1,y1,x2,y2)

x2 = -4.2307104265931397  * deg2rad
y2 =  35.783852846582519 * deg2rad
print*, haversine(x1,y1,x2,y2)

end
