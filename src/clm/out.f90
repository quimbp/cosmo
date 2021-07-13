module module_out

use module_types
use module_status
use module_utils
use module_datetime
use module_options
use netcdf
use module_float
use module_forcing, only: NLAYER,LAYER


implicit none

character(len=maxlen)                    :: output_mode = ""
character(len=maxlen)                    :: output_name = ""
integer                                  :: output_id   = 34
integer                                  :: output_record = 0
integer                                  :: output_nid
!integer                                  :: output_strid
integer                                  :: output_idl
integer                                  :: output_timeid
!integer                                  :: output_dateid
integer                                  :: output_lonid 
integer                                  :: output_latid 
integer                                  :: output_zid 
integer                                  :: output_uid 
integer                                  :: output_vid 
integer                                  :: output_rid 
integer                                  :: output_nfloats
integer                                  :: output_xoid
integer                                  :: output_yoid
integer                                  :: output_zoid
integer                                  :: output_toid
integer                                  :: output_status
integer                                  :: output_kid
real(dp)                                 :: output_missing

contains
! ...
! ====================================================================
! ...
subroutine trajectory_open(filename,Nfloats,missing)

character(len=*), intent(in)               :: filename
integer, intent(in)                        :: Nfloats
real(dp), intent(in)                       :: missing

! ... Local variables
! ...
integer i
character(len=maxlen) word,ext
character(len=1000) lcom

STATUS_ERROR = 0; STATUS_TEXT = ""

output_nfloats = Nfloats
output_missing = missing
output_record = 0

! ... Check for extension
! ...
do i=len(filename),1,-1
  if (filename(i:i).eq.'.') then
    ext = filename(i+1:)
    exit
  endif
enddo

if (lowercase(ext).eq.'nc') then
  output_mode = 'nc'
  output_name = trim(filename)

  STATUS_ERROR = NF90_CREATE(filename,NF90_CLOBBER,output_id); call cdf_error()

  STATUS_ERROR = NF90_DEF_DIM(output_id,'floats',Nfloats,output_nid);      call cdf_error()
!  STATUS_ERROR = NF90_DEF_DIM(output_id,'string19',19,output_strid);       call cdf_error()
  STATUS_ERROR = NF90_DEF_DIM(output_id,'time',NF90_UNLIMITED,output_idl); call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'release_longitude',NF90_DOUBLE,(/output_nid/),output_xoid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_xoid,'units','degrees_east')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'release_latitude',NF90_DOUBLE,(/output_nid/),output_yoid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_yoid,'units','degrees_east')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'release_depth',NF90_DOUBLE,(/output_nid/),output_zoid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_zoid,'units','meters')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'model_level',NF90_INT,(/output_nid/),output_kid)
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'release_time',NF90_DOUBLE,(/output_nid/),output_toid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_toid,'units','seconds since initial time')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'time',NF90_DOUBLE,(/output_idl/),output_timeid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_timeid,'units','Julian days')
  call cdf_error()

!  STATUS_ERROR = NF90_DEF_VAR(output_id,'date',NF90_CHAR,(/output_strid,output_idl/),output_dateid)
!  call cdf_error()
!  STATUS_ERROR = NF90_PUT_ATT(output_id,output_dateid,'units','ISO 8601 date and time')
!  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'lon',NF90_REAL,(/output_nid,output_idl/),output_lonid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_lonid,'units','degrees_east')
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_lonid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'lat',NF90_REAL,(/output_nid,output_idl/),output_latid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_latid,'units','degrees_north')
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_latid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'depth',NF90_REAL,(/output_nid,output_idl/),output_zid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_zid,'units','meters')
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_zid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'u',NF90_REAL,(/output_nid,output_idl/),output_uid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_uid,'units','m/s')
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_uid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'v',NF90_REAL,(/output_nid,output_idl/),output_vid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_vid,'units','m/s')
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_vid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'distance',NF90_REAL,(/output_nid,output_idl/),output_rid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_rid,'units','m')
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_rid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'exitcode',NF90_INT,(/output_nid/),output_status)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,'long_name','Float status at the end of the simulation')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is-1",'float has not been released')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is0",'float was moving')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is1",'float left the model area')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is2",'float was stranded')
  call cdf_error()

  call get_commandline(lcom)
  STATUS_ERROR = NF90_PUT_ATT(output_id,0,'CommandLine',TRIM(lcom))
  call cdf_error()


  STATUS_ERROR = NF90_ENDDEF(output_id)
  call cdf_error()

endif
  
end subroutine trajectory_open
! ...
! ====================================================================
! ...
subroutine param_write(p1,p2,p3,p4,p5,p6,p7)

real(dp), intent(in)         :: p1,p2,p3,p4,p5,p6,p7

STATUS_ERROR = NF90_REDEF(output_id); call cdf_error()
STATUS_ERROR = NF90_PUT_ATT(output_id,0,'alpha',p1);   call cdf_error()
STATUS_ERROR = NF90_PUT_ATT(output_id,0,'A11',p2);   call cdf_error()
STATUS_ERROR = NF90_PUT_ATT(output_id,0,'A12',p3);   call cdf_error()
STATUS_ERROR = NF90_PUT_ATT(output_id,0,'A21',p4);   call cdf_error()
STATUS_ERROR = NF90_PUT_ATT(output_id,0,'A22',p5);   call cdf_error()
STATUS_ERROR = NF90_PUT_ATT(output_id,0,'mu',p6);   call cdf_error()
STATUS_ERROR = NF90_PUT_ATT(output_id,0,'va',p7);   call cdf_error()
STATUS_ERROR = NF90_ENDDEF(output_id); call cdf_error()

end subroutine param_write
! ...
! ====================================================================
! ...
subroutine release_write()

STATUS_ERROR = NF90_PUT_VAR(output_id,output_xoid,FLT%release_lon);   call cdf_error()
STATUS_ERROR = NF90_PUT_VAR(output_id,output_yoid,FLT%release_lat);   call cdf_error()
STATUS_ERROR = NF90_PUT_VAR(output_id,output_zoid,FLT%release_depth); call cdf_error()
STATUS_ERROR = NF90_PUT_VAR(output_id,output_toid,FLT%release_time);  call cdf_error()
STATUS_ERROR = NF90_PUT_VAR(output_id,output_kid,LAYER(FLT%k));  call cdf_error()

end subroutine release_write
! ...
! ====================================================================
! ...
subroutine exitcode_write()

! ... Local variables:
! ...
integer i, code(FLT%Nfloats)

do i=1,FLT%Nfloats
  if (.not.FLT%released(i)) then
    code(i) = -1
  else if (FLT%outside(i)) then
    code(i) = 1
  else if (FLT%stranded(i)) then
    code(i) = 2
  else
    code(i) = 0
  endif
enddo
STATUS_ERROR = NF90_PUT_VAR(output_id,output_status,code);   call cdf_error()


end subroutine exitcode_write
! ...
! ====================================================================
! ...
subroutine trajectory_write(time)

real(dp), intent(in)                                :: time

! ... Local variables
! ...
type(type_date) date
integer i
real(dp) jd, xx(output_nfloats)
character(len=19) date_txt


STATUS_ERROR = 0; STATUS_TEXT = ""

jd = time/86400.0_dp + UserTini
date = jd2date(jd)
date_txt = trim(date%iso())

output_record = output_record + 1

STATUS_ERROR = NF90_PUT_VAR(output_id,output_timeid,[jd],[output_record],[1])
!STATUS_ERROR = NF90_PUT_VAR(output_id,output_dateid,[date_txt],[1,output_record],[19,1])

xx(:) = output_missing
do i=1,FLT%Nfloats
  if (FLT%released(i)) xx(i) = FLT%lon(i)
enddo
STATUS_ERROR = NF90_PUT_VAR(output_id,output_lonid,xx,[1,output_record],[output_nfloats,1])


xx(:) = output_missing
do i=1,FLT%Nfloats
  if (FLT%released(i)) xx(i) = FLT%lat(i)
enddo
STATUS_ERROR = NF90_PUT_VAR(output_id,output_latid,xx,[1,output_record],[output_nfloats,1])
call cdf_error()

xx(:) = output_missing
do i=1,FLT%Nfloats
  if (FLT%released(i)) xx(i) = FLT%z(i)
enddo
STATUS_ERROR = NF90_PUT_VAR(output_id,output_zid,xx,[1,output_record],[output_nfloats,1])
call cdf_error()

xx(:) = output_missing
do i=1,FLT%Nfloats
  if (FLT%released(i)) xx(i) = FLT%dist(i)
enddo
STATUS_ERROR = NF90_PUT_VAR(output_id,output_rid,xx,[1,output_record],[output_nfloats,1])
call cdf_error()

end subroutine trajectory_write
! ....
! ====================================================================
! ...
subroutine velocity_write()

! ... Local variables
! ...
integer i
real(dp) xx(output_nfloats)

xx(:) = output_missing
do i=1,FLT%Nfloats
  if (FLT%released(i)) xx(i) = FLT%u(i)
enddo
STATUS_ERROR = NF90_PUT_VAR(output_id,output_uid,xx,[1,output_record],[output_nfloats,1])
call cdf_error()

xx(:) = output_missing
do i=1,FLT%Nfloats
  if (FLT%released(i)) xx(i) = FLT%v(i)
enddo
STATUS_ERROR = NF90_PUT_VAR(output_id,output_vid,xx,[1,output_record],[output_nfloats,1])
call cdf_error()

end subroutine velocity_write
! ....
! ====================================================================
! ...
subroutine trajectory_close()

! ... Local variables
! ...

STATUS_ERROR = 0; STATUS_TEXT = ""

if (trim(output_mode).eq.'text') then
  close(output_id)
else if (trim(output_mode).eq.'json') then
  close(output_id)
else if (trim(output_mode).eq.'nc') then
  STATUS_ERROR = NF90_CLOSE(output_id)
  if (STATUS_ERROR.ne.NF90_NOERR) then
    STATUS_TEXT = trim(NF90_STRERROR(STATUS_ERROR))
    return
  endif
endif

return

end subroutine trajectory_close
! ....
! ====================================================================
! ...
!subroutine cdf_error()
!
!if (STATUS_ERROR.ne.NF90_NOERR) then
!  STATUS_TEXT = trim(NF90_STRERROR(STATUS_ERROR))
!  return
!endif
!
!end subroutine cdf_error
! ....
! ====================================================================
! ...

end module module_out
