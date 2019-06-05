! ****************************************************************************
! ... out.f90
! ... Quim Ballabrera, April 2017
! ... COSMO Lagrangian model
! ... Routines to write the model trajectory in netcdf.
! ... Subroutines:
! ...   out_create
! ...   out_save
! ...   out_exitmode
! ...   out_close
! ... Version 0.1, released October 2017
! ... Version 0.3, released February 2018
! ...              Output position set to missing for unreleased buoys.
! ****************************************************************************

module mod_out

use cosmo, only: dp,jd2date
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
integer                               :: err,natts

write(*,*)
write(*,*) 'Creating output file: ', trim(ofile)
err = NF90_CREATE(ofile,NF90_CLOBBER,ncId)
call cdf_error(err,'Unable to create output file')

err = NF90_DEF_DIM(ncId,'floats',FLT%n,odi)
err = NF90_DEF_DIM(ncId,'string15',15,ods)
err = NF90_DEF_DIM(ncId,'time',NF90_UNLIMITED,odl)

err = NF90_DEF_VAR(ncId,'time_model',NF90_DOUBLE,(/odl/),odmodtime)
call cdf_error(err,'Unable to define variable time')
call cdf_copyatts(.false.,Ufid,Uidt,ncId,odmodtime,natts)

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
  err = NF90_PUT_ATT(ncId,odz,'units','meter')
  err = NF90_PUT_ATT(ncId,odz,'long_name','Depth')
!  err = NF90_PUT_ATT(ncId,odz,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'time_navigation',NF90_DOUBLE,(/odi,odl/),odt)
  err = NF90_PUT_ATT(ncId,odt,'units','seconds')
  err = NF90_PUT_ATT(ncId,odt,'long_name','Navigation time')
!  err = NF90_PUT_ATT(ncId,odt,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'distance',NF90_REAL,(/odi,odl/),oddist)
  err = NF90_PUT_ATT(ncId,oddist,'units','kilometer')
  err = NF90_PUT_ATT(ncId,oddist,'long_name','Travelled distance since deployment')
!  err = NF90_PUT_ATT(ncId,oddist,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'temperature',NF90_REAL,(/odi,odl/),odtemp)
  err = NF90_PUT_ATT(ncId,odtemp,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'salinity',NF90_REAL,(/odi,odl/),odsalt)
  err = NF90_PUT_ATT(ncId,odsalt,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'density',NF90_REAL,(/odi,odl/),oddens)
  err = NF90_PUT_ATT(ncId,oddens,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'udf',NF90_REAL,(/odi,odl/),odudf)
  err = NF90_PUT_ATT(ncId,odudf,'_FillValue',sngl(FLT%missing))

err = NF90_DEF_VAR(ncId,'release_time',NF90_DOUBLE,(/odi/),odrel)
  err = NF90_PUT_ATT(ncId,odrel,'long_name','Time the particle is released')

err = NF90_DEF_VAR(ncId,'exitcode',NF90_INT,(/odi/),ode)
call cdf_error(err,'Unable to define exitcode')
  err = NF90_PUT_ATT(ncId,ode,'long_name','Status with which the particle exits')
  err = NF90_PUT_ATT(ncId,ode,'-1','Particle has not been released')
  err = NF90_PUT_ATT(ncId,ode,'0','Particle was moving')
  err = NF90_PUT_ATT(ncId,ode,'1','Particle left the model area')
  err = NF90_PUT_ATT(ncId,ode,'2','Particle too close to land')

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

integer err,flo

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
  err = NF90_PUT_VAR(ncId,odtemp,(/FLT%temp(flo)/),(/flo,kk/),(/1,1/))
  call cdf_error(err,'Unable to save temperature')
  err = NF90_PUT_VAR(ncId,odsalt,(/FLT%salt(flo)/),(/flo,kk/),(/1,1/))
  call cdf_error(err,'Unable to save salinity')
  err = NF90_PUT_VAR(ncId,oddens,(/FLT%dens(flo)/),(/flo,kk/),(/1,1/))
  call cdf_error(err,'Unable to save density')
  err = NF90_PUT_VAR(ncId,odudf,(/FLT%UDF(flo)/),(/flo,kk/),(/1,1/))
  call cdf_error(err,'Unable to save user-defined function')
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
subroutine out_close(ofile)

character(len=*), intent(in)          :: ofile

! ... Local variables
! ...
integer err

write(*,*) 'Closing output file: ', trim(ofile)
err = NF90_CLOSE(ncId)
call cdf_error(err,'Unable to close file')

end subroutine  out_close

end module mod_out
