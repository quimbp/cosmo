! ****************************************************************************
! ... out.f90
! ... Quim Ballabrera, April 2017
! ... COSMO Lagrangian model
! ... Routines to write the model trajectory in netcdf or Geojson
! ... Subroutines:
! ...   out_create
! ...   out_save
! ...   out_exitmode
! ...   out_close
! ... Version 0.1, released October 2017
! ... Version 0.3, released February 2018
! ...              Output position set to missing for unreleased buoys.
! ... Version 2.0, released May 2019
! ...              Optional output in geojson
! ****************************************************************************

module mod_out

!use cosmo, only: dp,jd2date
use cosmo
use mod_floats
use netcdf

implicit none

integer                               :: ncId
integer                               :: odi
integer                               :: odl
integer                               :: ods
integer                               :: odmodtime
integer                               :: odsystime
integer                               :: odsysdate
integer                               :: odt
integer                               :: odx
integer                               :: ody
integer                               :: odz
integer                               :: oddist
integer                               :: odrel
integer                               :: odtemp
integer                               :: odsalt
integer                               :: oddens
integer                               :: odudf
integer                               :: ode

type(geojson_file), dimension(:), allocatable      :: Geofile
type(geojson_feature), dimension(:,:), allocatable :: Geofeature

contains
! ...
! =====================================================================
! ...
subroutine out_create (ofile,FLT,Ufid,Uidt)

character(len=*), intent(in)          :: ofile
type(floater), intent(in)             :: FLT
integer, intent(in)                   :: Ufid,Uidt

! ... Local variables
! ...
integer                               :: err,natts,i
character(len=280)                    :: root_name
character(len=280)                    :: ext_name
character(len=280)                    :: filename
character(len=80)                     :: sn


if (gjson) then

  sn = i2str(time())
  write(*,*) 'Creating output GeoJSON files: ', trim(ofile)//'<'
  root_name = trim(ofile)
  do i=len_trim(ofile),1,-1
    if (ofile(i:i).eq.'.') then
      root_name = ofile(1:i-1)
      ext_name  = ofile(i:)
      exit
    endif
  enddo

  ! ... GEOJSON output
  ! ... A file for each floater
  allocate(Geofile(FLT%n))
  allocate(Geofeature(FLT%n,3))
  
  do i=1,FLT%n
    if (FLT%n.eq.1) then
      filename = trim(ofile)
    else
      filename = trim(root_name)//'-'// &
                 rangestr(FLT%n,i)//trim(ext_name)
    endif
    Geofile(i)%source     = gj_source
    Geofile(i)%creator    = gj_creator
    Geofile(i)%experiment = gj_exp
    Geofile(i)%origin     = gj_origin
    Geofile(i)%name       = gj_name   
    Geofile(i)%sn         = trim(sn)//'-'//rangestr(FLT%n,i)
    Geofile(i)%numfloats  = FLT%n
    call geojson_create(Geofile(i),filename,'FeatureCollection')
  enddo

  return
endif

! ... Default Netcdf output
! ...
write(*,*)
write(*,*) 'Creating output file: ', trim(ofile)
err = NF90_CREATE(ofile,NF90_CLOBBER,ncId)
call cdf_error(err,'Unable to create output file')

err = NF90_DEF_DIM(ncId,'floats',FLT%n,odi)
err = NF90_DEF_DIM(ncId,'string15',15,ods)
err = NF90_DEF_DIM(ncId,'time',NF90_UNLIMITED,odl)

err = NF90_DEF_VAR(ncId,'time_model',NF90_DOUBLE,(/odl/),odmodtime)
call cdf_error(err,'Unable to define variable time')
if (Uidt.gt.0) then
  call cdf_copyatts(.false.,Ufid,Uidt,ncId,odmodtime,natts)
else
  write(*,*) 'Input file has no time'
endif


err = NF90_DEF_VAR(ncId,'time',NF90_DOUBLE,(/odl/),odsystime)
call cdf_error(err,'Unable to define variable time_jd')
  err = NF90_PUT_ATT(ncId,odsystime,'units','Julian days')

err = NF90_DEF_VAR(ncId,'date',NF90_CHAR,(/ods,odl/),odsysdate)
call cdf_error(err,'Unable to define variable date')
  err = NF90_PUT_ATT(ncId,odsysdate,'units','ISO 8601 date and time')

err = NF90_DEF_VAR(ncId,'lon',NF90_REAL,(/odi,odl/),odx)
  err = NF90_PUT_ATT(ncId,odx,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'lat',NF90_REAL,(/odi,odl/),ody)
  err = NF90_PUT_ATT(ncId,ody,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'depth',NF90_REAL,(/odi,odl/),odz)
  err = NF90_PUT_ATT(ncId,odz,'long_name','Depth')
  err = NF90_PUT_ATT(ncId,odz,'units','meter')
!  err = NF90_PUT_ATT(ncId,odz,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'time_navigation',NF90_DOUBLE,(/odi,odl/),odt)
  err = NF90_PUT_ATT(ncId,odt,'long_name','Navigation time')
  err = NF90_PUT_ATT(ncId,odt,'units','seconds')
!  err = NF90_PUT_ATT(ncId,odt,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'distance',NF90_REAL,(/odi,odl/),oddist)
  err = NF90_PUT_ATT(ncId,oddist,'units','kilometer')
  err = NF90_PUT_ATT(ncId,oddist,'long_name','Travelled distance since deployment')
!  err = NF90_PUT_ATT(ncId,oddist,'_FillValue',sngl(FLT%missing))

if (FLT%Tset) then
  err = NF90_DEF_VAR(ncId,'temp',NF90_REAL,(/odi,odl/),odtemp)
    err = NF90_PUT_ATT(ncId,odtemp,'long_name','Sea temperature in situ ITS-90 Scale')
    err = NF90_PUT_ATT(ncId,odtemp,'units','degree_Celsius')
    err = NF90_PUT_ATT(ncId,odtemp,'_FillValue',sngl(FLT%missing))
endif

if (FLT%Sset) then
  err = NF90_DEF_VAR(ncId,'psal',NF90_REAL,(/odi,odl/),odsalt)
    err = NF90_PUT_ATT(ncId,odsalt,'long_name','Practical salinity')
    err = NF90_PUT_ATT(ncId,odsalt,'units','psu')
    err = NF90_PUT_ATT(ncId,odsalt,'_FillValue',sngl(FLT%missing))
endif

if (FLT%run_udf) then
  err = NF90_DEF_VAR(ncId,'udf',NF90_REAL,(/odi,odl/),odudf)
    err = NF90_PUT_ATT(ncId,odudf,'long_name','User Defined Function')
    err = NF90_PUT_ATT(ncId,odudf,'_FillValue',sngl(FLT%missing))
endif

err = NF90_DEF_VAR(ncId,'release_time',NF90_DOUBLE,(/odi/),odrel)
  err = NF90_PUT_ATT(ncId,odrel,'long_name','Time the particle is released')

err = NF90_DEF_VAR(ncId,'exitcode',NF90_INT,(/odi/),ode)
call cdf_error(err,'Unable to define exitcode')
  err = NF90_PUT_ATT(ncId,ode,'long_name','Status with which the particle exits')
  err = NF90_PUT_ATT(ncId,ode,'-1','Particle has not been released')
  err = NF90_PUT_ATT(ncId,ode,'0','Particle was moving')
  err = NF90_PUT_ATT(ncId,ode,'1','Particle left the model area')
  err = NF90_PUT_ATT(ncId,ode,'2','Particle too close to land')

err = NF90_PUT_ATT(ncId,0,'source','blm')

err = cdf_put_command(ncId)
call cdf_error(err,'Unable to write CommandLine attribute')

err = NF90_ENDDEF(ncId)
call cdf_error(err,'Unable to leave DEF mode')

err = NF90_PUT_VAR(ncId,odrel,FLT%release_time(1:FLT%n))   ! Release date
call cdf_error(err,'Unable to write release time')


end subroutine out_create
! ...
! =====================================================================
! ...
subroutine out_save (FLT,kk,time_model,time)


type(floater), intent(in)               :: FLT
integer, intent(in)                     :: kk
real(dp), intent(in)                    :: time_model,time

integer err,flo,k,nv

if (gjson) then

  do flo=1,FLT%n
    if (FLT%released(flo)) then
      k = kk - FLT%lo(1) + 1
      Geofeature(flo,1)%line(k)%coordinates = [FLT%lon(flo), FLT%lat(flo)]
      Geofeature(flo,1)%line(k)%depth = FLT%depth(flo)
      Geofeature(flo,1)%line(k)%date  = FLT%date
      nv = 0
      if (FLT%Tset) then 
        nv = nv + 1
        Geofeature(flo,1)%line(k)%var_data(nv)  = FLT%temp(flo)
      endif
      if (FLT%Sset) then 
        nv = nv + 1
        Geofeature(flo,1)%line(k)%var_data(nv)  = FLT%salt(flo)
      endif
    endif
  enddo

  return
endif

! ... Default Netcdf output
! ...
err = NF90_PUT_VAR(ncId,odmodtime,(/time_model/),(/kk/),(/1/))
call cdf_error(err,'Unable to save time_model')

err = NF90_PUT_VAR(ncId,odsystime,(/time/),(/kk/),(/1/))
call cdf_error(err,'Unable to save time')

err = NF90_PUT_VAR(ncId,odsysdate,(/date_string(FLT%date,'iso')/),(/1,kk/),(/15,1/))
call cdf_error(err,'Unable to save date')

do flo=1,FLT%n
  if (FLT%released(flo)) then
    err = NF90_PUT_VAR(ncId,odx,(/FLT%lon(flo)/),(/flo,kk/),(/1,1/))
    call cdf_error(err,'Unable to save lon')
    err = NF90_PUT_VAR(ncId,ody,(/FLT%lat(flo)/),(/flo,kk/),(/1,1/))
    call cdf_error(err,'Unable to save lat')
  else
    err = NF90_PUT_VAR(ncId,odx,(/FLT%missing/),(/flo,kk/),(/1,1/))
    call cdf_error(err,'Unable to save lon')
    err = NF90_PUT_VAR(ncId,ody,(/FLT%missing/),(/flo,kk/),(/1,1/))
    call cdf_error(err,'Unable to save lat')
  endif
  err = NF90_PUT_VAR(ncId,odz,(/FLT%depth(flo)/),(/flo,kk/),(/1,1/))
  call cdf_error(err,'Unable to save depth')
  err = NF90_PUT_VAR(ncId,odt,(/max(0d0,FLT%time(flo))/),(/flo,kk/),(/1,1/))
  call cdf_error(err,'Unable to save navegation time')
  err = NF90_PUT_VAR(ncId,oddist,(/max(0d0,FLT%dist(flo))/),(/flo,kk/),(/1,1/))
  call cdf_error(err,'Unable to save distance')
  if (FLT%tracer) then
    err = NF90_PUT_VAR(ncId,odtemp,(/FLT%temp(flo)/),(/flo,kk/),(/1,1/))
    call cdf_error(err,'Unable to save temperature')
  endif
  if (FLT%run_udf) then
    err = NF90_PUT_VAR(ncId,odudf,(/FLT%UDF(flo)/),(/flo,kk/),(/1,1/))
    call cdf_error(err,'Unable to save user-defined function')
  endif
enddo

end subroutine out_save
! ...
! =====================================================================
! ...
subroutine out_exitmode(FLT)

type(floater), intent(in)               :: FLT

! ... Local variables
! ...
integer err,flo
integer, dimension(FLT%n)  :: emode

if (gjson) then
  return
endif

! ... Default Netcdf output
! ...
do flo=1,FLT%n
  if (FLT%released(flo)) then
    if (FLT%floating(flo)) THEN
      emode(flo) = 0
    else if (FLT%outside(flo)) then
      emode(flo) = 1
    else if (FLT%stranded(flo)) then
      emode(flo) = 2
    else
      stop 'Why am I here?'
    endif
  else
    emode(flo) = -1
  endif
enddo

err = NF90_PUT_VAR(ncId,ode,emode)
call cdf_error(err,'Unable to save user-defined function')

end subroutine out_exitmode
! ...
! =====================================================================
! ...
subroutine out_close(FLT)

type(floater), intent(in)               :: FLT
!character(len=*), intent(in)            :: ofile

! ... Local variables
! ...
integer err,flo,nsteps

if (gjson) then

  ! ... First feature is the Line
  ! ...
  nsteps = size(Geofeature(1,1)%line)

  do flo=1,FLT%n
    if (FLT%lf(flo).lt.0) FLT%lf(flo) = nsteps

    !call feature_line_properties(Geofeature(flo,1))
    Geofeature(flo,2)%point%date = Geofeature(flo,1)%line(FLT%lo(flo))%date
    Geofeature(flo,2)%point%depth = Geofeature(flo,1)%line(FLT%lo(flo))%depth
    Geofeature(flo,2)%point%coordinates = Geofeature(flo,1)%line(FLT%lo(flo))%coordinates
    Geofeature(flo,2)%point%var_data = Geofeature(flo,1)%line(FLT%lo(flo))%var_data
    call feature_property_add(Geofeature(flo,2),'event','0')
    call feature_property_add(Geofeature(flo,2),'marker-size','large')

    Geofeature(flo,3)%point%date = Geofeature(flo,1)%line(FLT%lf(flo))%date
    Geofeature(flo,3)%point%depth = Geofeature(flo,1)%line(FLT%lf(flo))%depth
    Geofeature(flo,3)%point%coordinates = Geofeature(flo,1)%line(FLT%lf(flo))%coordinates
    Geofeature(flo,3)%point%var_data = Geofeature(flo,1)%line(FLT%lf(flo))%var_data
    call feature_property_add(Geofeature(flo,3),'event','1')
    call feature_property_add(Geofeature(flo,3),'marker-size','small')

    call geojson_add_feature(Geofile(flo),Geofeature(flo,1), &
                             FLT%lo(flo),FLT%lf(flo))
    call geojson_add_feature(Geofile(flo),Geofeature(flo,2),1,1)
    call geojson_add_feature(Geofile(flo),Geofeature(flo,3),1,1)
    
  enddo

  return
endif

! ... Default Netcdf output
! ...
!write(*,*) 'Closing output file: ', trim(ofile)
err = NF90_CLOSE(ncId)
call cdf_error(err,'Unable to close file')

end subroutine  out_close

end module mod_out
