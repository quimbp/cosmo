! *****************************************************************************
! ... lineargs.f90
! ... Routines to simplify the processing of input arguments.
! ... COSMO project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! *****************************************************************************

module module_lineargs

use module_types, only: sp,dp,maxlen
use module_constants, only: nan,nan4
use module_utils

implicit none

integer                                          :: lineargs_nargs = 0
integer, dimension(:), allocatable               :: lineargs_used
character(len=maxlen), dimension(:), allocatable :: lineargs_val

contains
! ...
! ========================================================================
! ...
integer function lineargs () result(na)

integer i,j,jj,nn,iu
logical read_from_file
character(len=maxlen) word,line

na = iargc()
if (na.eq.0) return

read_from_file = .false.
call getarg(1,word)
if (word(1:3).eq.'--o') read_from_file = .true.
if (word(1:3).eq.'--O') read_from_file = .true.

if (read_from_file) then
! ... Read execution options from a file
! ...
  call getarg(2,word)
  iu = unitfree()
  open(iu,file=word,status='old')
  nn = numlines(iu,'ascii')
  na = 0
  do i=1,nn
    read(iu,'(A)') line
    if (line(1:1).ne.'#') na = na + numwords(line)
  enddo
  lineargs_nargs = na
  allocate (lineargs_val(na))
  allocate (lineargs_used(na))
  lineargs_used(:)  = 0

  rewind(iu)
  jj = 0
  do i=1,nn
    read(iu,'(A)') line
    if (line(1:1).ne.'#') then
      do j=1,numwords(line)
        jj = jj + 1
        call line_word(line,j,lineargs_val(jj))
      enddo
    endif
  enddo
  if (jj.NE.na) call stop_error (1,'something wrong in lineargs_ini')
  close(iu)
else
! ... Read execution options from command line
! ...
  lineargs_nargs = na
  allocate (lineargs_val(na))
  allocate (lineargs_used(na))
  lineargs_used(:)  = 0
  lineargs_val(:) = ''
  do i=1,lineargs_nargs
    call getarg(i,lineargs_val(i))
  enddo
endif

return
end function lineargs
! ...
! ========================================================================
! ...
subroutine argflg (label,flag)

character(len=*), intent(in)     :: label
logical, intent(inout)           :: flag

integer n,k

n = len_trim(label) 

do k=1,lineargs_nargs
 if (lineargs_used(k).EQ.0) then
 if (lineargs_val(k)(:n).EQ.label(:n)) then 
  lineargs_used(k) = 1
  flag = .true.
  return
 endif
 endif
enddo

return
end subroutine argflg
! ...
! ====================================================================
! ...
subroutine argflt (label,flag,val)
! ... Returns a float value from user arguments

character(len=*), intent(in)        :: label
logical, intent(inout)              :: flag
real(sp), intent(out)               :: val

! ... Local variables
! ...
integer n,k
character lett

if (flag) return

n = len_trim(label)
DO k=1,lineargs_nargs
 if (lineargs_used(k).EQ.0) then
 if (lineargs_val(k)(:n).EQ.label(:n)) then
  if (k+1.GT.lineargs_nargs) call stop_error(1,'Invalid option')
  if (lineargs_used(k+1).EQ.1) then
    write(6,*) 'Option ',TRIM(lineargs_val(k+1)),' already used.'
    call stop_error (1,'Invalid option')
  endif
  flag      = .true.
  lett      = lineargs_val(k+1)(1:1)
  if (lett.EQ.'N'.OR.lett.EQ.'n') then
  val     = nan4
  else 
    read(lineargs_val(k+1),*) val
  endif
  lineargs_used(k)   = 1
  lineargs_used(k+1) = 1
  return
 endif
 endif
enddo

return
end subroutine argflt
! ...
! ====================================================================
! ...
subroutine argint (label,flag,val)
! ... Returns an integer value from user arguments

character(len=*), intent(in)        :: label
logical, intent(inout)              :: flag
integer, intent(out)                :: val

integer n,k

if (flag) return

n = len_trim(label)
DO k=1,lineargs_nargs
 if (lineargs_used(k).EQ.0) then
 if (lineargs_val(k)(:n).EQ.label(:n)) then
  if (k+1.GT.lineargs_nargs) call stop_error(1,'Invalid option')
  if (lineargs_used(k+1).EQ.1) then
    write(6,*) 'Option ',TRIM(lineargs_val(k+1)),' already used.'
    call stop_error (1,'Invalid option')
  endif
  flag      = .true.
  read(lineargs_val(k+1),*) val
  lineargs_used(k)   = 1
  lineargs_used(k+1) = 1
  return
 endif
 endif
enddo

return
end subroutine argint
! ...
! ====================================================================
! ...
subroutine argstr (label,flag,val)

character(len=*), intent(in)        :: label
logical, intent(inout)              :: flag
character(LEN=*), intent(out)       :: val

integer n,k

if (flag) return
n = len_trim(label)

DO k=1,lineargs_nargs
 if (lineargs_used(k).EQ.0) then
 if (lineargs_val(k)(:n).EQ.label(:n)) then
  if (k+1.GT.lineargs_nargs) call stop_error (1,'Invalid option')
  if (lineargs_used(k+1).EQ.1) then
    write(6,*) 'Option ',TRIM(lineargs_val(k+1)),' already used.'
    call stop_error (1,'Invalid option')
  endif
  flag      = .true.
  val       = TRIM(lineargs_val(k+1))
  lineargs_used(k)   = 1
  lineargs_used(k+1) = 1
  return
 endif
 endif
enddo

return
end subroutine argstr
! ...
! ====================================================================
! ...
subroutine arglst (label,flag,list)

character(LEN=*), intent(in)        :: label
logical, intent(inout)              :: flag
character(LEN=*), intent(out)       :: list

integer n,j,k

if (flag) return

list = ''
n = len_trim(label)

DO k=1,lineargs_nargs
 if (lineargs_used(k).EQ.0) then
 if (lineargs_val(k)(:n).EQ.label(:n)) then
  if (k+1.GT.lineargs_nargs) call stop_error(1,'Invalid option')
  flag = .true.
  lineargs_used(k) = 1
  DO j=1,lineargs_nargs-k
    if (lineargs_val(k+j)(1:1).EQ.'-') EXIT
    call strcat (list,lineargs_val(k+j))
    lineargs_used(k+j) = 1
  enddo
  return
 endif
 endif
enddo

return
end subroutine arglst
! ...
! ====================================================================
! ...
subroutine argdbl (label,flag,val)

character(LEN=*), intent(in)        :: label
logical, intent(inout)              :: flag
REAL(dp), intent(out)               :: val

character(LEN=1) lett
integer n,k

if (flag) return

n = len_trim(label)
DO k=1,lineargs_nargs
 if (lineargs_used(k).EQ.0) then
 if (lineargs_val(k)(:n).EQ.label(:n)) then
  if (k+1.GT.lineargs_nargs) call stop_error(1,'Invalid option')
  if (lineargs_used(k+1).EQ.1) then
    write(6,*) 'Option ',TRIM(lineargs_val(k+1)),' already used.'
    call stop_error (1,'Invalid option')
  endif
  flag      = .true.
  lett      = lineargs_val(k+1)(1:1)
  if (lett.EQ.'N'.OR.lett.EQ.'n') then
    val     = nan
  else 
    read(lineargs_val(k+1),*) val
  endif
  lineargs_used(k)   = 1
  lineargs_used(k+1) = 1
  return
 endif
 endif
enddo

return
end subroutine argdbl 
! ...
! ====================================================================
! ...
subroutine checkopts 

integer i

if (lineargs_nargs.EQ.0) return

if (SUM(lineargs_used).NE.lineargs_nargs) then
  write(*,'(A)',ADVANCE="no") 'Unknown options : '
  DO i=1,lineargs_nargs
    if (lineargs_used(i).EQ.0) write(*,'(A)',ADVANCE="no") TRIM(lineargs_val(i))//' '
  enddo
  write(*,*)
  call stop_error(1,'Invalid options')
endif

end subroutine checkopts
! ...
! =======================================================================
! ...
subroutine arglast (file)

character(LEN=*), intent(out)  :: file

if (lineargs_used(lineargs_nargs).EQ.0) then
  file = lineargs_val(lineargs_nargs)
  lineargs_used(lineargs_nargs) = 1
else
  call stop_error(1,'Last argument already used in arglast')
endif

return
end subroutine arglast
! ...
! ========================================================================
! ...
end module module_lineargs
