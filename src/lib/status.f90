module module_status

use netcdf

integer                      :: STATUS_ERROR = 0
character(len=180)           :: STATUS_TEXT  = ""

contains 
! ...
! ====================================================================
! ====================================================================
! ...
subroutine check_status()

if (STATUS_ERROR.eq.0) return

write(0,*) 'ERROR: '//trim(STATUS_TEXT)
stop -1

end subroutine check_status
! ....
! ====================================================================
! ...
subroutine cdf_error()

if (STATUS_ERROR.ne.NF90_NOERR) then
  STATUS_TEXT = trim(NF90_STRERROR(STATUS_ERROR))
  return
endif

end subroutine cdf_error
! ...
! ====================================================================
! ...
end module module_status
