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

contains 

subroutine plotx(x,linestyle)

real(dp), dimension(:), intent(in)      :: x
character(len=*), intent(in), optional :: linestyle


! ... Local variables
! ...
integer iu,i,n
character(80) filename,word
character(10) lfmt

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
write(iu,'("x = [")',advance="no")

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
write(iu,'(A)') 'plt.plot(x,'//trim(lfmt)//')'
write(iu,'(A)') 'plt.show()'
close(iu)
call system('python3 '//trim(filename))
call system('rm -f '//trim(filename))


end subroutine plotx
! ...
! ======================================================================
! ...
subroutine plotxy(x,y)

real(dp), dimension(:), intent(in)      :: x
real(dp), dimension(:), intent(in)      :: y

end subroutine plotxy
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

n = size(x)

write(plotpyfid_,'("x = [")',advance="no")

do i=1,n
  write(word,*) x(i)
  if (i.lt.n) then
    write(plotpyfid_,'(A)',advance="no") trim(word)
    write(plotpyfid_,'(",")',advance="no")
  else
    write(plotpyfid_,'(A)',advance="no") trim(word)
    write(plotpyfid_,'("]")')
  endif
enddo
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


