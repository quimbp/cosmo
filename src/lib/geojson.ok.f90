! =============================================================================
! ... geojson.f90
! ... Quim Ballabrera, April 2019
! ... GEOJSON modules
! =============================================================================

module geojson

use types, only: sp,dp
use dates
use utils

type, public :: geojson_file
  character(len=:), allocatable           :: name
  character(len=:), allocatable           :: type
  integer                                 :: nfeatures = 0
  integer                                 :: iu = 0
end type geojson_file


type, public :: geojson_point
   real(dp), dimension(2)                 :: coordinates  ! 2D
   real(dp)                               :: depth
   type(date_type)                        :: date
end type geojson_point

type, public :: geojson_properties
   character(len=360)                    :: label
   character(len=6000)                   :: value
end type geojson_properties

type, public :: geojson_feature
  character(len=:), allocatable                   :: type
  type(geojson_point)                             :: point
  type(geojson_point), dimension(:), allocatable  :: line
  integer                                         :: nprop = 0
  type(geojson_properties), dimension(100)        :: properties 
end type geojson_feature

contains 

subroutine geojson_create (gf,filename,filetype)

implicit none

type(geojson_file), intent(inout) :: gf
character(len=*), intent(in)      :: filename
character(len=*), intent(in)      :: filetype

gf%name = trim(filename)
gf%type = trim(filetype)

gf%iu = unitfree()

open(unit=gf%iu,file=filename,status='unknown')
rewind(gf%iu)

if (filetype.eq.'FeatureCollection') then
  write(gf%iu,'(A)') '{'
  write(gf%iu,'(T2,A)') '"type": "FeatureCollection",'
  write(gf%iu,'(T2,A)') '"file": "'//trim(filename)//'",'
  write(gf%iu,'(T2,A)') '"convention": "COSMO v.1.5",'
  write(gf%iu,'(T2,A)') '"author": "Joaquim Ballabrera",'
  write(gf%iu,'(T2,A)') '"e-mail": "joaquim@icm.csic.es",'
  write(gf%iu,'(T2,A)') '"source": "model",'
  write(gf%iu,'(T2,A)') '"features":'
  write(gf%iu,'(T2,A)') '['
  write(gf%iu,'(T2,A)') ']'
  call geojson_close(gf%iu)
endif



end subroutine geojson_create
! ...
! =======================================================================
! ...
subroutine geojson_add_feature (gf,feature)

implicit none

type(geojson_file), intent(inout) :: gf
type(geojson_feature), intent(in) :: feature

integer nl,i,nskip
character(len=1000) line


if (gf%iu.le.0) call stop_error(1,'GeoJSON file not opened')

open(unit=gf%iu,file=gf%name,status='old',action='readwrite')
nl = numlines(gf%iu)

! ... Skip initial records
! ...
if (gf%nfeatures.eq.0) then
  nskip = nl - 1
  if (gf%type.eq.'FeatureCollection') nskip = nskip - 1
  rewind(gf%iu)
  do i=1,nskip
    read(gf%iu,*)
  enddo
else
  nskip = nl - 2
  if (gf%type.eq.'FeatureCollection') nskip = nskip - 1
  rewind(gf%iu)
  do i=1,nskip
    read(gf%iu,*)
  enddo
  write(gf%iu,'(T4,A)') '},'
endif

gf%nfeatures = gf%nfeatures + 1
write(gf%iu,'(T4,A)') '{"type": "Feature",'

if (feature%type.eq.'Point') then
  write(gf%iu,'(T5,A)') '"geometry":'
  write(gf%iu,'(T8,A)')   '{'
  write(gf%iu,'(T8,A)')   '"type": "Point",'
  write(gf%iu,'(T8,A)')   '"coordinates": ' // ff2str(feature%point%coordinates)
  write(gf%iu,'(T8,A)')   '},'
else if (feature%type.eq.'LineString') then
  write(gf%iu,'(T5,A)') '"geometry":'
  write(gf%iu,'(T8,A)')   '{'
  write(gf%iu,'(T8,A)')   '"type": "LineString",'
  write(gf%iu,'(T8,A)')   '"coordinates": '
  write(gf%iu,'(T8,A)')   '  ['
  do i=1,size(feature%line)
    if (i.eq.size(feature%line)) then
      write(gf%iu,'(T11,A)')   ff2str(feature%line(i)%coordinates)
    else
      write(gf%iu,'(T11,A)')   ff2str(feature%line(i)%coordinates)//','
    endif
  enddo
  write(gf%iu,'(T8,A)')   '  ]'
  write(gf%iu,'(T8,A)')   '},'
endif
if (feature%nprop.gt.0) then
  write(gf%iu,'(T5,A)') '"properties":' 
  write(gf%iu,'(T8,A)') '{' 
  do i=1,feature%nprop
    if ((feature%properties(i)%value(1:1).eq.'{').or. &
        (feature%properties(i)%value(1:1).eq.'[')) then
      line = '"'//trim(compress(feature%properties(i)%label))//'": ' // &
                  trim(compress(feature%properties(i)%value))
    else
      line = '"'//trim(compress(feature%properties(i)%label))//'": "' // &
                  trim(compress(feature%properties(i)%value))//'"'
    endif
    if (i.eq.feature%nprop) then
      write(gf%iu,'(T8,A)')   trim(line)
    else
      write(gf%iu,'(T8,A)')   trim(line)//','
    endif
  enddo
  write(gf%iu,'(T8,A)')   '}'
else
  write(gf%iu,'(T5,A)') '"properties":' 
  write(gf%iu,'(T8,A)') '{' 
  write(gf%iu,'(T8,A)') '"prop0": "value0" '
  write(gf%iu,'(T8,A)') '}' 
endif

write(gf%iu,'(T4,A)') '}'

if (gf%type.eq.'FeatureCollection') write(gf%iu,'(T2,A)') ']'
call geojson_close(gf%iu)

  
end subroutine geojson_add_feature
! ...
! =======================================================================
! ...
subroutine feature_property_add (feature,lab,val)

type(geojson_feature), intent(inout) :: feature
character(len=*), intent(in)         :: lab
character(len=*), intent(in)         :: val

if (feature%nprop.eq.size(feature%properties)) then
  call stop_error(1,'Reached maximum number of features')
endif

feature%nprop = feature%nprop + 1
feature%properties(feature%nprop)%label = trim(lab)
feature%properties(feature%nprop)%value = trim(val)

end subroutine feature_property_add
! ...
! =======================================================================
! ...
subroutine feature_property_clear(feature)

type(geojson_feature), intent(inout) :: feature

feature%nprop = 0
feature%properties(:)%label = ''
feature%properties(:)%value = ''

end subroutine feature_property_clear
! ...
! =======================================================================
! ...
subroutine feature_point_properties (feature,exp,name,source,event)

type(geojson_feature), intent(inout)   :: feature
character(len=*), intent(in)           :: exp
character(len=*), intent(in)           :: name
character(len=*), intent(in)           :: source
character(len=*), intent(in), optional :: event

character(len=1000) line

call feature_property_clear(feature)

call feature_property_add(feature,'exp',exp)
call feature_property_add(feature,'name',name)
call feature_property_add(feature,'source',source)
if (present(event)) call feature_property_add(feature,'event',event)
call feature_property_add(feature,'varnames','["time", "depth"]')

line = '{ "units": "ISO8601", "qc_data": [], &
      &"fill_value": "0000-00-00T00:00:00Z", &
      &"short_name": "Time", &
      &"long_name": "Time", &
      &"data": ["' // trim(date_string(feature%point%date,'iso','extended'))//&
      &'"]}'
call feature_property_add(feature,'time',trim(line))

line = '{ "units": "meter", "qc_data": [], &
      &"fill_value": "NaN", &
      &"short_name": "Depth", &
      &"long_name": "Floater depth", &
      &"data": [' // trim(f2str(feature%point%depth,'f6.1'))//&
      &']}'
call feature_property_add(feature,'depth',trim(line))


end subroutine feature_point_properties
! ...
! =======================================================================
! ...
subroutine feature_line_properties (feature)

type(geojson_feature), intent(inout)   :: feature

integer i,n
character(len=10000) line,line0

call feature_property_clear(feature)
call feature_property_add(feature,'varnames','["time", "depth"]')

n = size(feature%line)
line0 = ''
do i=1,n
  if (i.eq.1) then
    line0 = '"' // trim(date_string(feature%line(i)%date,'iso','extended'))//'"'
  else
    line0 = trim(line0) &
          // ', "' &
          // trim(date_string(feature%line(i)%date,'iso','extended'))//'"'
  endif
enddo


line = '{ "units": "ISO8601", "qc_data": [], &
      &"fill_value": "0000-00-00T00:00:00Z", &
      &"short_name": "Time", &
      &"long_name": "Time", &
      &"data": [' // trim(line0)//&
      &']}'
call feature_property_add(feature,'time',trim(line))

line0 = ''
do i=1,n
  if (i.eq.1) then
    line0 = trim(f2str(feature%line(i)%depth,'f6.1'))
  else
    line0 = trim(line0) &
          // ', ' &
          // trim(f2str(feature%line(i)%depth,'f6.1'))
  endif
enddo

line = '{ "units": "meter", "qc_data": [], &
      &"fill_value": "NaN", &
      &"short_name": "Depth", &
      &"long_name": "Floater depth", &
      &"data": [' // trim(line0)//&
      &']}'
call feature_property_add(feature,'depth',trim(line))


end subroutine feature_line_properties
! ...
! =======================================================================
! ...
subroutine geojson_close (iu)

write(iu,'(A)') '}'
close(iu)

end subroutine geojson_close


end module geojson

