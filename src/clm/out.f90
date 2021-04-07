module module_out

use module_types
use module_status
use module_utils
use module_datetime
use module_options
use netcdf

implicit none

private cdf_error


character(len=maxlen)                    :: output_mode = ""
character(len=maxlen)                    :: output_name = ""
integer                                  :: output_id   = 34
integer                                  :: output_record = 0
integer                                  :: output_nid
integer                                  :: output_strid
integer                                  :: output_idl
integer                                  :: output_timeid
integer                                  :: output_dateid
integer                                  :: output_lonid 
integer                                  :: output_latid 
integer                                  :: output_nfloats
integer                                  :: output_toid
integer                                  :: output_zoid
integer                                  :: output_status
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
  STATUS_ERROR = NF90_DEF_DIM(output_id,'string19',19,output_strid);       call cdf_error()
  STATUS_ERROR = NF90_DEF_DIM(output_id,'time',NF90_UNLIMITED,output_idl); call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'release_time',NF90_DOUBLE,(/output_nid/),output_toid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_toid,'units','seconds since initial time')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'release_depth',NF90_DOUBLE,(/output_nid/),output_zoid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_zoid,'units','meters')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'time',NF90_DOUBLE,(/output_idl/),output_timeid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_timeid,'units','Julian days')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'date',NF90_CHAR,(/output_strid,output_idl/),output_dateid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_dateid,'units','ISO 8601 date and time')
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'lon',NF90_REAL,(/output_nid,output_idl/),output_lonid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_lonid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'lat',NF90_REAL,(/output_nid,output_idl/),output_latid)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_latid,'_FillValue',sngl(missing))
  call cdf_error()

  STATUS_ERROR = NF90_DEF_VAR(output_id,'exitcode',NF90_INT,(/output_nid/),output_status)
  call cdf_error()
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,'long_name','Float status at the end of the simulation')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is-1",'float has not been released')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is0",'float was moving')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is1",'float left the model area')
  STATUS_ERROR = NF90_PUT_ATT(output_id,output_status,"is2",'float was stranded')
  call cdf_error()


  STATUS_ERROR = NF90_ENDDEF(output_id)
  call cdf_error()

endif
  
end subroutine trajectory_open
! ...
! ====================================================================
! ...
subroutine trajectory_write(time,X)

real(dp), intent(in)                                :: time
real(dp), dimension(2,output_nfloats), intent(in)   :: X

! ... Local variables
! ...
type(type_date) date
integer n
real(dp) jd, xx(output_nfloats)
character(len=19) date_txt


STATUS_ERROR = 0; STATUS_TEXT = ""

jd = time/86400.0_dp + UserTini
date = jd2date(jd)
date_txt = trim(date%iso())

output_record = output_record + 1

STATUS_ERROR = NF90_PUT_VAR(output_id,output_timeid,[jd],[output_record],[1])
STATUS_ERROR = NF90_PUT_VAR(output_id,output_dateid,[date_txt],[1,output_record],[19,1])

xx(:) = output_missing
where (X(1,:).ne.output_missing) xx = rad2deg*X(1,:)
STATUS_ERROR = NF90_PUT_VAR(output_id,output_lonid,xx,[1,output_record],[output_nfloats,1])


xx(:) = output_missing
where (X(2,:).ne.output_missing) xx = rad2deg*X(2,:)
STATUS_ERROR = NF90_PUT_VAR(output_id,output_latid,xx,[1,output_record],[output_nfloats,1])
call cdf_error()



n = size(x)


end subroutine trajectory_write
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
subroutine cdf_error()

if (STATUS_ERROR.ne.NF90_NOERR) then
  STATUS_TEXT = trim(NF90_STRERROR(STATUS_ERROR))
  return
endif

end subroutine cdf_error
! ....
! ====================================================================
! ...

end module module_out
