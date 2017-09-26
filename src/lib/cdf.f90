! *****************************************************************************
! ... cdf.f90
! ... Routines to simplify generic NetCDF operations-
! ... COSMO project
! ... Quim Ballabrera, March 2017
! *****************************************************************************

module cdf

use netcdf
use types, only: sp,dp
use utils, only: stop_error,uppercase,get_commandline,line_replace, &
                 numwords,line_word
use dates, only: date_type

implicit none
private
public cdf_copyatts,cdf_error,cdf_missing,cdf_timeref, &
       cdf_put_command


contains
! ...
! ============================================================================
! ...
subroutine cdf_copyatts (ver,id1,v1,id2,v2,natts)
! ... Copies the attributes from variable v1 in file id1 to the
! ... variable v2 in file id2.

logical, intent(in)                     :: ver
integer, intent(in)                     :: id1,v1,id2,v2
integer, intent(out)                    :: natts

! ... Local variables:
! ...
integer err,vtype,ndim,j,att_type,att_len
character(len=120) name,att_name,word
integer, dimension(100)  :: dimids

character(len=180)                      :: tmpt
integer, dimension(:), allocatable      :: tmpi
real(sp), dimension(:), allocatable     :: tmp4
real(dp), dimension(:), allocatable     :: tmp8

! ... Information from first file:
! ..
if (v1.eq.NF90_GLOBAL) then
  err = NF90_INQUIRE (id1,nAttributes=natts)
  if (ver) write(*,*) 'Number of global attributes ', natts
else
  err = NF90_INQUIRE_VARIABLE (id1,v1,name,vtype,ndim,dimids,natts)
  call cdf_error (err,'CDF_COPYATTS Error: Unable to inquire variable')
  if (ver) write(*,*) 'Variable ', v1, ' has ', natts, ' attributes'
endif

do j=1,natts
  err = NF90_INQ_ATTNAME (id1,v1,j,att_name)
  err = NF90_INQUIRE_ATTRIBUTE (id1,v1,att_name,xtype=att_type)
  err = NF90_INQUIRE_ATTRIBUTE (id1,v1,att_name,len=att_len)
  if (att_type.eq.NF90_BYTE) then
    allocate (tmpi(att_len))
    err = NF90_GET_ATT(id1,v1,att_name,tmpi)
    err = NF90_PUT_ATT(id2,v2,TRIM(att_name),tmpi)
    deallocate (tmpi)
  endif
  if (att_type.EQ.NF90_CHAR) then
    if (att_len.gt.len(tmpt)) call stop_error(1,'Increase size tmpt')
    err = NF90_GET_ATT(id1,v1,att_name,tmpt)
    call cdf_error (err,'Unable to get text attribute')
    err = NF90_PUT_ATT(id2,v2,TRIM(att_name),tmpt(1:att_len))
    call cdf_error (err,'Unable to write text attribute')
  endif
  if (att_type.eq.NF90_SHORT) then
    allocate (tmpi(att_len))
    err = NF90_GET_ATT(id1,v1,att_name,tmpi)
    CALL cdf_error (err,'Unable to get short attribute')
    if (TRIM(att_name).NE.'_FillValue') then
      err = NF90_PUT_ATT(id2,v2,TRIM(att_name),tmpi)
    ELSE
      err = NF90_PUT_ATT(id2,v2,TRIM(att_name),tmpi)
    endif
    CALL cdf_error (err,'Unable to write short attribute')
    deallocate (tmpi)
  endif
  if (att_type.EQ.NF90_INT) then
    allocate (tmpi(att_len))
    err = NF90_GET_ATT(id1,v1,att_name,tmpi)
    err = NF90_PUT_ATT(id2,v2,TRIM(att_name),tmpi)
    deallocate (tmpi)
  endif
  if (att_type.EQ.NF90_FLOAT) then
    allocate (tmp4(att_len))
    err = NF90_GET_ATT(id1,v1,att_name,tmp4)
    err = NF90_PUT_ATT(id2,v2,TRIM(att_name),tmp4)
    deallocate (tmp4)
  endif
  if (att_type.EQ.NF90_DOUBLE) then
    allocate (tmp8(att_len))
    err = NF90_GET_ATT(id1,v1,att_name,tmp8)
    err = NF90_PUT_ATT(id2,v2,TRIM(att_name),tmp8)
    deallocate (tmp8)
  endif
enddo

end subroutine cdf_copyatts
! ...
! ============================================================================
! ...
subroutine cdf_error (err,message)
! ... Check if error has occurred. In case it has happened, it sends
! ... the approppriate message.

integer, intent(in)                      :: err
character(len=*), intent(in)             :: message

if (err.ne.NF90_NOERR) then
   write(*,*) NF90_STRERROR(err)
   call stop_error (1,message)
endif

end subroutine cdf_error
! ...
! ============================================================================
! ...
subroutine cdf_missing (fid,idv,missing,ia)
! ... Retrieve the missing value of variable idv.
! ... If ia = 1, the missing value is from the missing_value attribute
! ... if ia = 2, the missing value is from the _FillValue attribute
! ... ia = 0 means no missing value has been found.

integer, intent(in)                    :: fid,idv
integer, optional, intent(out)         :: ia
real(dp), intent(out)                  :: missing

! ... Local variables
! ...
integer err

! ... Default value
! ...
ia = 0

err = NF90_GET_ATT (fid,idv,'missing_value',missing)
if (err.eq.NF90_NOERR) then
  ia = 1
  return
endif

err = NF90_GET_ATT (fid,idv,'_FillValue',missing)
if (err.eq.NF90_NOERR) then
  ia = 2
  return
endif

end subroutine cdf_missing
! ...
! ============================================================================
! ...
function cdf_put_command (fid) result(err)
! ... Write the commandline

integer, intent(in)                    :: fid
integer                                :: err

! ... Local variables
! ...
integer defmode,err1,err2
character(len=1000) commandline

defmode = nf90_redef(fid)
call get_commandline(commandline)
err = nf90_put_att (fid,NF90_GLOBAL,'CommandLine',trim(commandline))
if (defmode.eq.NF90_NOERR) err = err + nf90_enddef(fid)

end function cdf_put_command
! ...
! ============================================================================
! ...
function cdf_timeref (fid,idt,date) result(err)
! ... Retrieve the reference date for time axis

integer, intent(in)                    :: fid
integer, intent(in)                    :: idt
type(date_type), intent(out)           :: date
integer                                :: err

! ... Local variables
! ...
integer i,nw
character(len=80) att,word

err = NF90_GET_ATT(fid,idt,'units',att)
if (err.ne.NF90_NOERR) then
  write(*,*) 'cdf_timeref WARNING: No units attribute'
  return
endif

att = uppercase(att)
i = index(att,'SINCE')

if (i.le.0) then
  write(*,*) 'cdf_timeref WARNING: No reference date'
  err = 1
  return
endif

att = att(i+6:)
i = len_trim(att)
if (att(i:i).EQ.'Z') att(i:) = ''


att = line_replace(att,'-',' ')
att = line_replace(att,':',' ')
att = line_replace(att,'T',' ')
nw  = numwords(att)
if ((nw.ne.7).and.(nw.ne.6).and.(nw.ne.3)) then
  print*, trim(att)
  call stop_error(1,'Invalid units attribute')
  err = 1
  return
endif

call line_word(att,1,word)
read(word,*) date%year
call line_word(att,2,word)
read(word,*) date%month
call line_word(att,3,word)
read(word,*) date%day

if (nw.ge.6) then
  call line_word(att,4,word)
  read(word,*) date%hour
  call line_word(att,5,word)
  read(word,*) date%minute
  call line_word(att,6,word)
  read(word,*) date%second
else
  date%hour   = 0
  date%minute = 0
  date%second = 0
endif

!call line_word(att,2,date%month)
!read(att(i:i+3),*)     date%year
!read(att(i+5:i+6),*)   date%month
!read(att(i+8:i+9),*)   date%day
!read(att(i+11:i+12),*) date%hour
!read(att(i+14:i+15),*) date%minute
!read(att(i+17:i+18),*) date%second

err = NF90_GET_ATT(fid,idt,'calendar',att)
if (err.eq.NF90_NOERR) then
  date%calendar = trim(att)
else
  date%calendar = 'unknown'
endif

err = 0

end function cdf_timeref
! ...
! ============================================================================
! ...
end module cdf
