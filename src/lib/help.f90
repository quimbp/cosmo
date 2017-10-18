! ********************************************************************
! ... help.f90
! ... Routines to print help information on screen.
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! ********************************************************************

module help

use utils, only: stop_error,compress,say

implicit none

integer, parameter                               :: hlp_maxoptions = 30
integer                                          :: hlp_numoptions = 0

character(len=80)                                :: hlp_progname = ''
character(len=10)                                :: hlp_version = 'v0.0'
character(len=80)                                :: hlp_author = 'anonymous'
character(len=10000)                             :: hlp_summary = ''
character(len=1000)                              :: hlp_command = ''
character(len=1000)                              :: hlp_example = ''

character(len=80), dimension(hlp_maxoptions)     :: hlp_option
character(len=180), dimension(hlp_maxoptions)    :: hlp_description
character(len=80), dimension(hlp_maxoptions)     :: hlp_default

contains

subroutine help_progname(word)

implicit none
character(len=*), intent(in)           :: word

hlp_progname = compress(word)

return
end subroutine help_progname
! ...
! =============================================================================
! ...
subroutine help_version(word)

implicit none
character(len=*), intent(in)           :: word

hlp_version = compress(word)

return
end subroutine help_version
! ...
! =============================================================================
! ...
subroutine help_command(word)

implicit none
character(len=*), intent(in)           :: word

hlp_command = trim(word)
if (hlp_command(1:1).EQ.'>') hlp_command(1:1) = ''
hlp_command = compress(hlp_command)

return
end subroutine help_command
! ...
! =============================================================================
! ...
subroutine help_example(word)

implicit none
character(len=*), intent(in)           :: word

hlp_example = trim(word)
if (hlp_example(1:1).EQ.'>') hlp_example(1:1) = ''
hlp_example = compress(hlp_example)

return
end subroutine help_example
! ...
! =============================================================================
! ...
subroutine help_summary(word)

implicit none
character(len=*), intent(in)           :: word

hlp_summary = compress(word)

return
end subroutine help_summary
! ...
! =============================================================================
! ...
subroutine help_author(word)

implicit none
character(len=*), intent(in)           :: word

hlp_author = compress(word)

return
end subroutine help_author
! ...
! =============================================================================
! ...
subroutine help_option(option,description,default)

implicit none
character(len=*), intent(in)           :: option,description,default

if (len_trim(option).EQ.0) return
if (hlp_numoptions.eq.hlp_maxoptions) then
  call stop_error(1,'Increase the value off hlp_maxoptions')
endif

! ... New option
! ...
hlp_numoptions = hlp_numoptions + 1

hlp_option(hlp_numoptions)      = trim(option)

if (len_trim(option).GT.0) then
  hlp_description(hlp_numoptions) = trim(description)
ELSE
  hlp_description(hlp_numoptions) = ''
endif

if (len_trim(option).GT.0) then
  hlp_default(hlp_numoptions)     = trim(default)
ELSE
  hlp_default(hlp_numoptions)     = ''
endif

return
end subroutine help_option
! ...
! =============================================================================
! ...
subroutine help_write

INTEGER i

write(*,*) '======================================================================='
write(*,*) 'Program: ' // trim(hlp_progname)
write(*,*) 'Version: ' // trim(hlp_version)
write(*,*) 'Author : ' // trim(hlp_author)

if (len_trim(hlp_summary).GT.0) then
  write(*,*)
  call say('Summary: '//trim(hlp_summary))
endif
if (len_trim(hlp_command).GT.0) then
  write(*,*)
  write(*,*) 'Command: '
  write(*,*) '> '//trim(hlp_command)
endif
if (hlp_numoptions.GT.0) then
  write(*,*)
  write(*,*) 'Options:'
  do i=1,hlp_numoptions
    write(*,'(T5,A)') trim(hlp_option(i))
    call say(hlp_description(i),30)
    if (len_trim(hlp_default(i)).GT.0) then
      write(*,'(T30,"Default: ",A)') trim(hlp_default(i))
    endif
  enddo
endif
if (len_trim(hlp_example).GT.0) then
  write(*,*)
  write(*,*) 'Example: '
  write(*,*) '> '//trim(hlp_example)
endif
write(*,*) '======================================================================='
  
STOP
end subroutine help_write
! ...
! =============================================================================
! ...
subroutine header

write(*,*) '======================================================================='
write(*,*) 'Program: ' // trim(hlp_progname)
write(*,*) 'Version: ' // trim(hlp_version)
write(*,*) 'By ' // trim(hlp_author)
write(*,*) '======================================================================='

end subroutine header
! ...
! =============================================================================
! ...

end module help
