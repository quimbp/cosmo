use cosmo

implicit none

integer i
type(geojson_file) gg
type(geojson_feature) p
!type(date_type) d

print*, rangestr(9,1)
print*, rangestr(99,1)
print*, rangestr(100,1)
print*, rangestr(25,1)
print*, rangestr(250,1)
print*, rangestr(2500,1)

stop
gg%origin   = 'Copernicus'
gg%source   = 'model'
gg%name     = '001'
gg%creator  = 'blm'
gg%varnames = '["time", "depth"]'

call geojson_create(gg,'toto.geojson','FeatureCollection')

allocate(p%line(10))
do i=1,10
  p%line(i)%coordinates = [one*i,23.d0+half*i]
  p%line(i)%date = date_inc(cal2date(2019,1,1,12,30*(i-1),0),1,'hour')
enddo

! ... Feature p: The initial point
p%type = 'Point'
p%point%coordinates = p%line(1)%coordinates
p%point%date = p%line(1)%date
call feature_point_properties(p,'0')
call geojson_add_feature(gg,p)

! ... Feature p: A line
p%type = 'LineString'
call feature_line_properties(p)
call geojson_add_feature(gg,p)


! ... Feature p: The final point
p%type = 'Point'
p%point%coordinates = p%line(10)%coordinates
p%point%date = p%line(10)%date
call feature_point_properties(p,'1')
call geojson_add_feature(gg,p)

end

