! ********************************************************************
! ... help.f90
! ... Routines to print help information on screen.
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! ********************************************************************

module module_help

use module_utils, only: stop_error,compress,say

implicit none
integer, parameter                                 :: MAXOPTIONS = 50

type type_help
  integer                                          :: numoptions = 0

  character(len=80)                                :: progname = ''
  character(len=10)                                :: version = 'v0.0'
  character(len=80)                                :: author = 'anonymous'
  character(len=10000)                             :: summary = ''
  character(len=1000)                              :: command = ''
  character(len=1000)                              :: example = ''

  character(len=80), dimension(MAXOPTIONS)         :: option
  character(len=180), dimension(MAXOPTIONS)        :: description
  character(len=80), dimension(MAXOPTIONS)         :: default

  contains

    procedure        :: set_progname   => help_progname
    procedure        :: set_version    => help_version
    procedure        :: set_command    => help_command
    procedure        :: set_example    => help_example
    procedure        :: set_summary    => help_example
    procedure        :: add_option     => help_option 
    procedure        :: write          => help_write

end type type_help

type(type_help) help

contains
! ...
! =============================================================================
! ...
subroutine help_progname(HLP,word)

class(type_help), intent(inout)        :: HLP
character(len=*), intent(in)           :: word

HLP%progname = compress(word)

return
end subroutine help_progname
! ...
! =============================================================================
! ...
subroutine help_version(HLP,word)

class(type_help), intent(inout)        :: HLP
character(len=*), intent(in)           :: word

HLP%version = compress(word)

return
end subroutine help_version
! ...
! =============================================================================
! ...
subroutine help_command(HLP,word)

class(type_help), intent(inout)        :: HLP
character(len=*), intent(in)           :: word

HLP%command = trim(word)
if (HLP%command(1:1).EQ.'>') HLP%command(1:1) = ''
HLP%command = compress(HLP%command)

return
end subroutine help_command
! ...
! =============================================================================
! ...
subroutine help_example(HLP,word)

class(type_help), intent(inout)        :: HLP
character(len=*), intent(in)           :: word

HLP%example = trim(word)
if (HLP%example(1:1).EQ.'>') HLP%example(1:1) = ''
HLP%example = compress(HLP%example)

return
end subroutine help_example
! ...
! =============================================================================
! ...
subroutine help_summary(HLP,word)

class(type_help), intent(inout)        :: HLP
character(len=*), intent(in)           :: word

HLP%summary = compress(word)

return
end subroutine help_summary
! ...
! =============================================================================
! ...
subroutine help_author(HLP,word)

class(type_help), intent(inout)        :: HLP
character(len=*), intent(in)           :: word

HLP%author = compress(word)

return
end subroutine help_author
! ...
! =============================================================================
! ...
subroutine help_option(HLP,option,description,default)

class(type_help), intent(inout)        :: HLP
character(len=*), intent(in)           :: option,description,default

if (len_trim(option).EQ.0) return
if (HLP%numoptions.eq.MAXOPTIONS) then
  call stop_error(1,'Increase the value of MAXOPTIONS in COSMO_ROOT/src/lib/help.f90')
endif

! ... New option
! ...
HLP%numoptions = HLP%numoptions + 1

HLP%option(HLP%numoptions)      = trim(option)

if (len_trim(option).GT.0) then
  HLP%description(HLP%numoptions) = trim(description)
ELSE
  HLP%description(HLP%numoptions) = ''
endif

if (len_trim(option).GT.0) then
  HLP%default(HLP%numoptions)     = trim(default)
ELSE
  HLP%default(HLP%numoptions)     = ''
endif

return
end subroutine help_option
! ...
! =============================================================================
! ...
subroutine help_write(HLP)

class(type_help), intent(in)           :: HLP

! ... Local variables
! ...
integer i

write(*,*) '======================================================================='
write(*,*) 'Program: ' // trim(HLP%progname)
write(*,*) 'Version: ' // trim(HLP%version)
write(*,*) 'Author : ' // trim(HLP%author)

if (len_trim(HLP%summary).GT.0) then
  write(*,*)
  call say('Summary: '//trim(HLP%summary))
endif
if (len_trim(HLP%command).GT.0) then
  write(*,*)
  write(*,*) 'Command: '
  write(*,*) '> '//trim(HLP%command)
endif
if (HLP%numoptions.GT.0) then
  write(*,*)
  write(*,*) 'Options:'
  do i=1,HLP%numoptions
    write(*,'(T5,A)') trim(HLP%option(i))
    call say(HLP%description(i),30)
    if (len_trim(HLP%default(i)).GT.0) then
      write(*,'(T30,"Default: ",A)') trim(HLP%default(i))
    endif
  enddo
endif
if (len_trim(HLP%example).GT.0) then
  write(*,*)
  write(*,*) 'Example: '
  write(*,*) '> '//trim(HLP%example)
endif
write(*,*) '======================================================================='
  
STOP
end subroutine help_write
! ...
! =============================================================================
! ...
subroutine header(HLP)

class(type_help), intent(in)           :: HLP

write(*,*) '======================================================================='
write(*,*) 'Program: ' // trim(HLP%progname)
write(*,*) 'Version: ' // trim(HLP%version)
write(*,*) 'By ' // trim(HLP%author)
write(*,*) '======================================================================='

end subroutine header
! ...
! =============================================================================
! ...

end module module_help
