! =============================================================================
! ... geojson.f90
! ... Quim Ballabrera, April 2019
! ... GEOJSON modules
! =============================================================================

module geojson

use types, only: sp,dp
use dates
use utils

!character(len=:), parameter                :: CONVENTION = 'COSMO v.1.5'
!character(len=:), parameter                :: CONVENTION = 'COSMO v.1.6'

type, public :: geojson_file
  character(len=180)                       :: filename    = ''
  character(len=24)                        :: filetype    = ''
  character(len=24)                        :: convention  = ''
  character(len=1800)                      :: commandline = ''
  character(len=180)                       :: origin      = ''
  character(len=180)                       :: source      = ''
  character(len=180)                       :: name        = ''
  character(len=180)                       :: creator     = ''
  character(len=180)                       :: varnames    = ''
  character(len=180)                       :: contact     = ''
  character(len=180)                       :: experiment  = ''
  character(len=180)                       :: sn          = ''
  integer                                  :: iu          = 0
  integer                                  :: numfloats   = 0
  integer                                  :: nfeatures   = 0
  type(date_type)                          :: date
end type geojson_file

type, public :: geojson_vars
   character(len=80)                      :: name       = ''
   character(len=80)                      :: units      = ''
   character(len=80)                      :: qc_data    = '[]'
   character(len=80)                      :: fill_value = ''
   character(len=80)                      :: short_name = ''
   character(len=80)                      :: long_name  = ''
end type geojson_vars

type, public :: geojson_point
   real(dp), dimension(2)                 :: coordinates  ! 2D
   real(dp)                               :: depth
   type(date_type)                        :: date
   integer                                :: nvars
   real(dp), dimension(7)                 :: var_data
end type geojson_point

type, public :: geojson_properties
   character(len=360)                    :: label
   character(len=6000)                   :: value
end type geojson_properties

type, public :: geojson_feature
  character(len=24)                               :: ftype
  type(geojson_point)                             :: point
  type(geojson_point), dimension(:), allocatable  :: line
  type(geojson_vars)                              :: time_defs
  type(geojson_vars)                              :: depth_defs
  integer                                         :: nvars = 0
  type(geojson_vars), dimension(:), allocatable   :: var_defs
  integer                                         :: nprop = 0
  type(geojson_properties), dimension(100)        :: properties 
end type geojson_feature

contains 
! ...
! =======================================================================
! ...
subroutine geojson_create (gf,filename,filetype,ver)

implicit none

type(geojson_file), intent(inout)      :: gf
character(len=*), intent(in)           :: filename
character(len=*), intent(in)           :: filetype
character(len=*), intent(in), optional :: ver

! ... Local variables:
character(len=1800) word

if (present(ver)) then
  gf%convention = 'COSMO v.'//trim(ver)
else
  gf%convention = 'COSMO v.1.5'
endif

gf%filename = trim(filename)
gf%filetype = trim(filetype)
gf%date     = date_today('utm')

gf%iu = unitfree()

if (len_trim(gf%commandline).eq.0) then
  call get_commandline(word)
  gf%commandline = word
endif

!if (.not.allocated(gf%source)) gf%source = ''
!if (.not.allocated(gf%origin)) gf%origin = ''
!if (.not.allocated(gf%experiment)) gf%experiment = ''
!if (.not.allocated(gf%name)) gf%name = ''
!if (.not.allocated(gf%creator)) gf%creator = ''
!if (.not.allocated(gf%contact)) gf%contact = ''
!if (.not.allocated(gf%varnames)) gf%varnames = ''
!if (.not.allocated(gf%sn)) gf%sn = ''
!
open(unit=gf%iu,file=trim(filename),status='unknown')
rewind(gf%iu)

if (filetype.eq.'FeatureCollection') then
  write(gf%iu,'(A)') '{'
  write(gf%iu,'(T2,A)') '"type": "FeatureCollection",'

  if (gf%convention.eq.'COSMO v.1.6') then
    write(gf%iu,'(T2,A)') '"file": "'//trim(filename)//'",'
    write(gf%iu,'(T2,A)') '"convention": "'//trim(gf%convention)//'",'
    write(gf%iu,'(T2,A)') '"command": "'//trim(gf%commandline)//'",'
    if (len_trim(gf%source).gt.0) then
      write(gf%iu,'(T2,A)') '"source": "'//trim(gf%source)//'",'
    endif
    if (len_trim(gf%origin).gt.0) then
      write(gf%iu,'(T2,A)') '"origin": "'//trim(gf%origin)//'",'
    endif
    if (len_trim(gf%experiment).gt.0) then
      write(gf%iu,'(T2,A)') '"exp": "'//trim(gf%experiment)//'",'
    endif
    if (len_trim(gf%sn).gt.0) then
      write(gf%iu,'(T2,A)') '"code_sn": "'//trim(gf%sn)//'",'
    endif
    if (gf%numfloats.gt.0) then
      write(gf%iu,'(T2,A)') '"numfloats": '//trim(i2str(gf%numfloats))//','
    endif
    if (len_trim(gf%name).gt.0) then
      write(gf%iu,'(T2,A)') '"name": "'//trim(gf%name)//'",'
    endif
    if (len_trim(gf%creator).gt.0) then
      write(gf%iu,'(T2,A)') '"creator": "'//trim(gf%creator)//'",'
    endif
    if (len_trim(gf%contact).gt.0) then
      write(gf%iu,'(T2,A)') '"contact": '//trim(gf%contact)//','
    endif
    if (len_trim(gf%varnames).gt.0) then
      write(gf%iu,'(T2,A)') '"varnames": '//trim(gf%varnames)//','
    endif
    write(gf%iu,'(T2,A)') '"creationtime": "'// &
                  trim(date_string(gf%date,'iso','extended'))//'",'
  endif

  write(gf%iu,'(T2,A)') '"features":'
  write(gf%iu,'(T2,A)') '['
  write(gf%iu,'(T2,A)') ']'
  call geojson_close(gf%iu)
endif

end subroutine geojson_create
! ...
! =======================================================================
! ...
subroutine geojson_add_feature (gf,feature,kk1,kk2)

implicit none

type(geojson_file), intent(inout) :: gf
type(geojson_feature), intent(in) :: feature
integer, intent(in), optional     :: kk1
integer, intent(in), optional     :: kk2

integer nl,i,nskip,k1,k2,vv
character(len=1000) line


if (gf%iu.le.0) call stop_error(1,'GeoJSON file not opened')

open(unit=gf%iu,file=gf%filename,status='old',action='readwrite')
nl = numlines(gf%iu)

! ... Skip initial records
! ...
if (gf%nfeatures.eq.0) then
  nskip = nl - 1
  if (gf%filetype.eq.'FeatureCollection') nskip = nskip - 1
  rewind(gf%iu)
  do i=1,nskip
    read(gf%iu,*)
  enddo
else
  nskip = nl - 2
  if (gf%filetype.eq.'FeatureCollection') nskip = nskip - 1
  rewind(gf%iu)
  do i=1,nskip
    read(gf%iu,*)
  enddo
  write(gf%iu,'(T4,A)') '},'
endif

gf%nfeatures = gf%nfeatures + 1
write(gf%iu,'(T4,A)') '{"type": "Feature",'

if (feature%ftype.eq.'Point') then
  write(gf%iu,'(T5,A)') '"geometry":'
  write(gf%iu,'(T8,A)')   '{'
  write(gf%iu,'(T8,A)')   '"type": "Point",'
  write(gf%iu,'(T8,A)')   '"coordinates": ' // ff2str(feature%point%coordinates)
  write(gf%iu,'(T8,A)')   '},'
else if (feature%ftype.eq.'LineString') then
  k1 = 1
  k2 = size(feature%line)
  if (present(kk1)) k1 = kk1
  if (present(kk2)) k2 = kk2
  write(gf%iu,'(T5,A)') '"geometry":'
  write(gf%iu,'(T8,A)')   '{'
  write(gf%iu,'(T8,A)')   '"type": "LineString",'
  write(gf%iu,'(T8,A)')   '"coordinates": '
  write(gf%iu,'(T8,A)')   '  ['
  do i=k1,k2
    if (i.eq.k2) then
      write(gf%iu,'(T11,A)')   ff2str(feature%line(i)%coordinates)
    else
      write(gf%iu,'(T11,A)')   ff2str(feature%line(i)%coordinates)//','
    endif
  enddo
  write(gf%iu,'(T8,A)')   '  ]'
  write(gf%iu,'(T8,A)')   '},'
endif

if (feature%nprop.gt.0.or.gf%convention.eq.'COSMO v.1.5') then
  write(gf%iu,'(T5,A)') '"properties":' 
  write(gf%iu,'(T8,A)') '{' 

  if (gf%convention.eq.'COSMO v.1.5') then
    write(gf%iu,'(T8,A)') '"file": "'//trim(gf%filename)//'",'
    write(gf%iu,'(T8,A)') '"convention": "'//trim(gf%convention)//'",'
    write(gf%iu,'(T8,A)') '"command": "'//trim(gf%commandline)//'",'
    if (len_trim(gf%source).gt.0) then
      write(gf%iu,'(T8,A)') '"source": "'//trim(gf%source)//'",'
    endif
    if (len_trim(gf%origin).gt.0) then
      write(gf%iu,'(T8,A)') '"origin": "'//trim(gf%origin)//'",'
    endif
    if (len_trim(gf%experiment).gt.0) then
      write(gf%iu,'(T8,A)') '"exp": "'//trim(gf%experiment)//'",'
    endif
    if (len_trim(gf%sn).gt.0) then
      write(gf%iu,'(T8,A)') '"code_sn": "'//trim(gf%sn)//'",'
    endif
    if (gf%numfloats.gt.0) then
      write(gf%iu,'(T8,A)') '"numfloats": '//trim(i2str(gf%numfloats))//','
    endif
    if (len_trim(gf%name).gt.0) then
      write(gf%iu,'(T8,A)') '"name": "'//trim(gf%name)//'",'
    endif
    if (len_trim(gf%creator).gt.0) then
      write(gf%iu,'(T8,A)') '"creator": "'//trim(gf%creator)//'",'
    endif
    if (len_trim(gf%contact).gt.0) then
      write(gf%iu,'(T8,A)') '"contact": '//trim(gf%contact)//','
    endif
    if (len_trim(gf%varnames).gt.0) then
      write(gf%iu,'(T8,A)') '"varnames": '//trim(gf%varnames)//','
    endif
    write(gf%iu,'(T8,A)') '"creationtime": "'// &
                  trim(date_string(gf%date,'iso','extended'))//'",'
  endif

  do i=1,feature%nprop
    if ((feature%properties(i)%value(1:1).eq.'{').or. &
        (feature%properties(i)%value(1:1).eq.'[')) then
      line = '"'//trim(compress(feature%properties(i)%label))//'": ' // &
                  trim(compress(feature%properties(i)%value))
    else
      line = '"'//trim(compress(feature%properties(i)%label))//'": "' // &
                  trim(compress(feature%properties(i)%value))//'"'
    endif
    !if (i.eq.feature%nprop) then
    !  write(gf%iu,'(T8,A)')   trim(line)
    !else
      write(gf%iu,'(T8,A)')   trim(line)//','
    !endif
  enddo

  ! ... Time:
  ! ...
  write(gf%iu,'(T8,A)') '"time": '
  write(gf%iu,'(T10,A)') '{'
  write(gf%iu,'(T10,A)') '"units": "'// &
                  trim(feature%time_defs%units)//'",'
  write(gf%iu,'(T10,A)') '"qc_data": '// &
                  trim(feature%time_defs%qc_data)//','
  write(gf%iu,'(T10,A)') '"fill_value": '// &
                  trim(feature%time_defs%fill_value)//','
  write(gf%iu,'(T10,A)') '"short_name": "'// &
                  trim(feature%time_defs%short_name)//'",'
  write(gf%iu,'(T10,A)') '"long_name": "'// &
                  trim(feature%time_defs%long_name)//'",'
  write(gf%iu,'(T10,A)') '"data": '
  write(gf%iu,'(T12,A)') '['
  if (feature%ftype.eq.'LineString') then
  do i=k1,k2
      if (i.eq.k2) then
        write(gf%iu,'(T12,A)') '"'//trim(date_string(feature%line(i)%date,'iso','extended'))//'"'
      else
        write(gf%iu,'(T12,A)') '"'//trim(date_string(feature%line(i)%date,'iso','extended'))//'",'
      endif
    enddo
  else if (feature%ftype.eq.'Point') then
    write(gf%iu,'(T12,A)') '"'//trim(date_string(feature%point%date,'iso','extended'))//'"'
  endif
  write(gf%iu,'(T12,A)') ']'
  write(gf%iu,'(T10,A)') '},'

  ! ... Depth:
  ! ...
  write(gf%iu,'(T8,A)') '"depth": '
  write(gf%iu,'(T10,A)') '{'
  write(gf%iu,'(T10,A)') '"units": "'// &
                  trim(feature%depth_defs%units)//'",'
  write(gf%iu,'(T10,A)') '"qc_data": '// &
                  trim(feature%depth_defs%qc_data)//','
  write(gf%iu,'(T10,A)') '"fill_value": '// &
                  trim(feature%depth_defs%fill_value)//','
  write(gf%iu,'(T10,A)') '"short_name": "'// &
                  trim(feature%depth_defs%short_name)//'",'
  write(gf%iu,'(T10,A)') '"long_name": "'// &
                  trim(feature%depth_defs%long_name)//'",'
  write(gf%iu,'(T10,A)') '"data": '
  write(gf%iu,'(T12,A)') '['
  if (feature%ftype.eq.'Point') then
    write(gf%iu,'(T12,A)') trim(f2str(feature%point%depth,'f6.1'))
  else
   do i=k1,k2
    if (i.eq.k2) then
      write(gf%iu,'(T12,A)') trim(f2str(feature%line(i)%depth,'f6.1'))
    else
      write(gf%iu,'(T12,A)') trim(f2str(feature%line(i)%depth,'f6.1'))//','
    endif
   enddo
  endif
  write(gf%iu,'(T12,A)') ']'
  if (feature%nvars.eq.0) then
    write(gf%iu,'(T10,A)') '}'
  else
    write(gf%iu,'(T10,A)') '},'
  endif

  ! ... VARS
  ! ... AAA
  do vv=1,feature%nvars
    write(gf%iu,'(T8,A)') '"'//trim(feature%var_defs(vv)%name)//'": '
    write(gf%iu,'(T10,A)') '{'
    write(gf%iu,'(T10,A)') '"units": "'// &
                    trim(feature%var_defs(vv)%units)//'",'
    write(gf%iu,'(T10,A)') '"qc_data": '// &
                    trim(feature%var_defs(vv)%qc_data)//','
    write(gf%iu,'(T10,A)') '"fill_value": '// &
                    trim(feature%var_defs(vv)%fill_value)//','
    write(gf%iu,'(T10,A)') '"short_name": "'// &
                    trim(feature%var_defs(vv)%short_name)//'",'
    write(gf%iu,'(T10,A)') '"long_name": "'// &
                    trim(feature%var_defs(vv)%long_name)//'",'
    write(gf%iu,'(T10,A)') '"data": '
    write(gf%iu,'(T12,A)') '['
    if (feature%ftype.eq.'Point') then
     write(gf%iu,'(T12,A)') trim(f2str(feature%point%var_data(vv),'f7.2'))
    else
     do i=k1,k2
      if (i.eq.k2) then
        write(gf%iu,'(T12,A)') trim(f2str(feature%line(i)%var_data(vv),'f7.2'))
      else
        write(gf%iu,'(T12,A)') trim(f2str(feature%line(i)%var_data(vv),'f7.2'))//','
      endif
     enddo
    endif
    write(gf%iu,'(T12,A)') ']'

    if (vv.eq.feature%nvars) then
      write(gf%iu,'(T10,A)') '}'
    else
      write(gf%iu,'(T10,A)') '},'
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

if (gf%filetype.eq.'FeatureCollection') write(gf%iu,'(T2,A)') ']'
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
subroutine feature_point_properties (feature,event)

type(geojson_feature), intent(inout)   :: feature
character(len=*), intent(in)           :: event

character(len=1000) line

call feature_property_clear(feature)

!call feature_property_add(feature,'exp',exp)
!call feature_property_add(feature,'name',name)
!call feature_property_add(feature,'source',source)
!call feature_property_add(feature,'varnames','["time", "depth"]')
call feature_property_add(feature,'event',event)

!line = '{ "units": "ISO8601", "qc_data": [], &
!      &"fill_value": "0000-00-00T00:00:00Z", &
!      &"short_name": "Time", &
!      &"long_name": "Time", &
!      &"data": ["' // trim(date_string(feature%point%date,'iso','extended'))//&
!      &'"]}'
!call feature_property_add(feature,'time',trim(line))
!
!line = '{ "units": "meter", "qc_data": [], &
!      &"fill_value": "NaN", &
!      &"short_name": "Depth", &
!      &"long_name": "Floater depth", &
!      &"data": [' // trim(f2str(feature%point%depth,'f6.1'))//&
!      &']}'
!call feature_property_add(feature,'depth',trim(line))


end subroutine feature_point_properties
! ...
! =======================================================================
! ...
subroutine feature_line_properties (feature)

type(geojson_feature), intent(inout)   :: feature

integer i,n
character(len=10000) line,line0

call feature_property_clear(feature)

n = size(feature%line)

end subroutine feature_line_properties
! ...
! =======================================================================
! ...
subroutine geojson_close (iu)

write(iu,'(A)') '}'
close(iu)

end subroutine geojson_close


end module geojson

