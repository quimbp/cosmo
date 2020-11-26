! ***************************************************************************
! ... plot.f90!
! ... Quim Ballabrera
! ... COSMO project
! ***************************************************************************

module pyplot

use types, only: dp
use utils

implicit none

character(len=80)                      :: plotpyname_
integer                                :: plotpyfid_

type Mplot
   character(len=20)                   :: fname
   integer                             :: iu
   integer, dimension(2)               :: figsize


end type

contains 
! ...
! ====================================================================
! ...
subroutine plotx(x,linestyle,title,xlabel,ylabel,showgrid, &
                 linewidth,xlim,ylim)

real(dp), dimension(:), intent(in)           :: x
character(len=*), intent(in), optional       :: linestyle
character(len=*), intent(in), optional       :: title
character(len=*), intent(in), optional       :: xlabel
character(len=*), intent(in), optional       :: ylabel
logical, intent(in), optional                :: showgrid
integer, intent(in), optional                :: linewidth
real(dp), dimension(2), intent(in), optional :: xlim
real(dp), dimension(2), intent(in), optional :: ylim


! ... Local variables
! ...
integer iu,i,n
character(80) filename,word,lims
character(18) lfmt,lwidth

if (present(linestyle)) then
 lfmt = '"'//trim(linestyle)//'"'
else
 lfmt = "'-r'"
endif

n = size(x)

filename = trim(rndname())//'.py'
iu = unitfree()
open(iu,file=filename,status='new',form='formatted')
write(iu,'(A)') 'import matplotlib.pyplot as plt'

call write_vector(iu,'x',x)

if (present(linewidth)) then
 write(lwidth,'(",linewidth=",i2)') linewidth
else
 lwidth = ''
endif

write(iu,'(A)') 'plt.plot(x,'//trim(lfmt)//trim(lwidth)//')'

if (present(title)) write(iu,'(A)') 'plt.title("'//trim(title)//'")'
if (present(xlabel)) write(iu,'(A)') 'plt.xlabel("'//trim(xlabel)//'")'
if (present(ylabel)) write(iu,'(A)') 'plt.ylabel("'//trim(ylabel)//'")'
if (present(showgrid)) then
  if (showgrid) write(iu,'(A)') 'plt.grid(True)'
endif
if (present(xlim)) then
  write(word,*) xlim(1)
  lims = '('//trim(word)//','
  write(word,*) xlim(2)
  lims = trim(lims)//trim(word)//')'
  write(iu,'(A)') 'plt.xlim('//trim(lims)//')'
endif
if (present(ylim)) then
  write(word,*) ylim(1)
  lims = '('//trim(word)//','
  write(word,*) ylim(2)
  lims = trim(lims)//trim(word)//')'
  write(iu,'(A)') 'plt.ylim('//trim(lims)//')'
endif


write(iu,'(A)') 'plt.show()'
close(iu)
call system('python3 '//trim(filename))
call system('rm -f '//trim(filename))


end subroutine plotx
! ...
! ======================================================================
! ...
subroutine plotxy(x,y,linestyle,title,xlabel,ylabel,showgrid,  &
                  linewidth,xlim,ylim)

real(dp), dimension(:), intent(in)           :: x
real(dp), dimension(:), intent(in)           :: y
character(len=*), intent(in), optional       :: linestyle
character(len=*), intent(in), optional       :: title
character(len=*), intent(in), optional       :: xlabel
character(len=*), intent(in), optional       :: ylabel
logical, intent(in), optional                :: showgrid
integer, intent(in), optional                :: linewidth
real(dp), dimension(2), intent(in), optional :: xlim
real(dp), dimension(2), intent(in), optional :: ylim


! ... Local variables
! ...
integer iu,i,n
character(80) filename,word,lims
character(18) lfmt,lwidth


if (present(linestyle)) then
 lfmt = '"'//trim(linestyle)//'"'
else
 lfmt = "'-r'"
endif

n = size(x)

filename = trim(rndname())//'.py'
iu = unitfree()
open(iu,file=filename,status='new',form='formatted')
write(iu,'(A)') 'import matplotlib.pyplot as plt'

call write_vector(iu,'x',x)
call write_vector(iu,'y',y)

if (present(linewidth)) then
 write(lwidth,'(",linewidth=",i2)') linewidth
else
 lwidth = ''
endif

write(iu,'(A)') 'plt.plot(x,y,'//trim(lfmt)//trim(lwidth)//')'


if (present(title)) write(iu,'(A)') 'plt.title("'//trim(title)//'")'
if (present(xlabel)) write(iu,'(A)') 'plt.xlabel("'//trim(xlabel)//'")'
if (present(ylabel)) write(iu,'(A)') 'plt.ylabel("'//trim(ylabel)//'")'
if (present(showgrid)) then
  if (showgrid) write(iu,'(A)') 'plt.grid(True)'
endif
if (present(xlim)) then
  write(word,*) xlim(1)
  lims = '('//trim(word)//','
  write(word,*) xlim(2)
  lims = trim(lims)//trim(word)//')'
  write(iu,'(A)') 'plt.xlim('//trim(lims)//')'
endif
if (present(ylim)) then
  write(word,*) ylim(1)
  lims = '('//trim(word)//','
  write(word,*) ylim(2)
  lims = trim(lims)//trim(word)//')'
  write(iu,'(A)') 'plt.ylim('//trim(lims)//')'
endif


write(iu,'(A)') 'plt.show()'
close(iu)
call system('python3 '//trim(filename))
call system('rm -f '//trim(filename))


end subroutine plotxy
! ...
! ======================================================================
! ...
subroutine write_vector(iu,label,x)

implicit none

integer, intent(in)                  :: iu
character(len=*), intent(in)         :: label
real(dp), dimension(:), intent(in)   :: x

integer i,n
character(80) word

n = size(x)

write(iu,'(A)',advance='no') trim(label) // ' = ['

do i=1,n
  write(word,*) x(i)
  if (i.lt.n) then
    write(iu,'(A)',advance="no") trim(word)
    write(iu,'(",")',advance="no")
  else
    write(iu,'(A)',advance="no") trim(word)
    write(iu,'("]")')
  endif
enddo

end subroutine write_vector
! ...
! ======================================================================
! ...
subroutine open_plot()

plotpyname_ = trim(rndname())//'.py'
plotpyfid_  = unitfree()

open(plotpyfid_,file=plotpyname_,status='new',form='formatted')
write(plotpyfid_,'(A)') 'import matplotlib.pyplot as plt'

end subroutine open_plot
! ...
! ======================================================================
! ...
subroutine add_plotx(x,linestyle)

real(dp), dimension(:), intent(in)      :: x
character(len=*), intent(in), optional :: linestyle


! ... Local variables
! ...
integer i,n
character(80) word
character(10) lfmt

if (present(linestyle)) then
 lfmt = '"'//trim(linestyle)//'"'
else
 lfmt = "'-r'"
endif

call write_vector(plotpyfid_,'x',x)

write(plotpyfid_,'(A)') 'plt.plot(x,'//trim(lfmt)//')'

end subroutine add_plotx
! ...
! ======================================================================
! ...
subroutine show_plot()

write(plotpyfid_,'(A)') 'plt.show()'
close(plotpyfid_)
call system('python3 '//trim(plotpyname_))
call system('rm -f '//trim(plotpyname_))


end subroutine show_plot



end module pyplot


