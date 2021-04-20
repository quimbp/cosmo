! ****************************************************************************
! ... Fortran routine to read 1-,2- and 3-columnar ASCII files
! ... COSMO Project
! ... Quim Ballabrera, February 2021
! ... Version 1.0, Initial version, 2021-02
! ****************************************************************************

module module_ascii

use module_types, only: dp
use module_utils

implicit none

contains
! ...
! =====================================================================
! ...
function read3cols(filename,x,y,z) result(n)
! ... Reads an ascii file with two columns: x,y
! ... returns and integer, n = readcols(filename,x,y)
! ... n > 0, the size of the vectors
! ... n = 0, file not found
! ... n < 0, invalid format

character(len=*), intent(in)                      :: filename
real(dp), dimension(:), allocatable, intent(out)  :: x
real(dp), dimension(:), allocatable, intent(out)  :: y
real(dp), dimension(:), allocatable, intent(out)  :: z
integer                                           :: n

integer i,iu,err

n  = 0
iu = unitfree()

open(iu,file=filename,status='old',form='formatted',iostat=err)
if (err.ne.0) return


! ... Read number of lines:
! ...
n = 0
rewind(iu)
10 read(iu,*,end=20,err=60)
   n = n + 1
   goto 10
20 continue

allocate(x(n))
allocate(y(n))
allocate(z(n))

rewind(iu)
do i=1,n
  read(iu,*,err=60) x(i), y(i), z(i)
enddo
close(iu)
return

60 n = -1
close(iu)
return

end function read3cols
! ...
! =====================================================================
! ...
function read2cols(filename,x,y) result(n)
! ... Reads an ascii file with two columns: x,y
! ... returns and integer, n = readcols(filename,x,y)
! ... n > 0, the size of the vectors
! ... n = 0, file not found
! ... n < 0, invalid format

character(len=*), intent(in)                      :: filename
real(dp), dimension(:), allocatable, intent(out)  :: x
real(dp), dimension(:), allocatable, intent(out)  :: y
integer                                           :: n

integer i,iu,err

n  = 0
iu = unitfree()

open(iu,file=filename,status='old',form='formatted',iostat=err)
if (err.ne.0) return


! ... Read number of lines:
! ...
n = 0
rewind(iu)
10 read(iu,*,end=20,err=60)
   n = n + 1
   goto 10
20 continue

allocate(x(n))
allocate(y(n))

rewind(iu)
do i=1,n
  read(iu,*,err=60) x(i), y(i)
enddo
close(iu)
return

60 n = -1
close(iu)
return

end function read2cols
! ...
! =====================================================================
! ...
function read1col(filename,x) result(n)
! ... Reads an ascii file with one column: x
! ... returns and integer, n = readcols(filename,x)
! ... n > 0, the size of the vector
! ... n = 0, file not found
! ... n < 0, invalid format

character(len=*), intent(in)                      :: filename
real(dp), dimension(:), allocatable, intent(out)  :: x
integer                                           :: n

integer i,iu,err

n  = 0
iu = unitfree()

open(iu,file=filename,status='old',form='formatted',iostat=err)
if (err.ne.0) return


! ... Read number of lines:
! ...
n = 0
rewind(iu)
10 read(iu,*,end=20,err=60)
   n = n + 1
   goto 10
20 continue

allocate(x(n))

rewind(iu)
do i=1,n
  read(iu,*,err=60) x(i)
enddo
close(iu)
return


60 n = -1
close(iu)
return

end function read1col
! ...
! =====================================================================
! ...
function readArray(filename,x) result(dims)
! ... Reads an ascii file with one column: x
! ... returns and integer, n = readcols(filename,x)
! ... n > 0, the size of the vector
! ... n = 0, file not found
! ... n < 0, invalid format

character(len=*), intent(in)                        :: filename
real(dp), dimension(:,:), allocatable, intent(out)  :: x
integer, dimension(2)                               :: dims

integer l,m,i,iu,err
character(len=1800) line

l  = 0
m  = 0
iu = unitfree()

open(iu,file=filename,status='old',form='formatted',iostat=err)
if (err.ne.0) return

! ... Get number of lines:
! ...
l = 0
rewind(iu)
10 read(iu,*,end=20,err=60)
   l = l + 1
   goto 10
20 continue

! ... Get number of columns
rewind(iu)
read(iu,'(A)') line
m = numwords(line)

allocate(x(l,m))

! ... Read data
! ...
rewind(iu)
do i=1,l
  read(iu,*,err=60) x(i,:)
enddo
close(iu)

dims = [l,m]
return

60 dims = [-1,-1]
close(iu)
return
end function readArray
! ...
! =====================================================================
! ...
end module module_ascii
