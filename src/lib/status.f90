module module_status

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
! ...
! ====================================================================
! ...
end module module_status
