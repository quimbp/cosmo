! ****************************************************************************
! ... Fortran general utilitites
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ****************************************************************************

module utils

use types, only: dp

implicit none
private
public compress,lowercase,line_word,menu,numlines,now,numwords, &
       stop_error,strcat,unitfree,uppercase,say,whitechar, &
       coords2index,index2coords,locate,filetype,mkdir, newfilename, &
       get_commandline,line_replace

contains
! ...
! =============================================================================
! ...
pure function coords2index(x,n) result(ind)
! ... Gets the x=(i,j,k) and returns its index = (k-1)*nx*ny + (j-1)*nx + i

integer, dimension(3), intent(in)   :: x  ! = (i,j,k)
integer, dimension(3), intent(in)   :: n  ! = (nx,ny,nz)
integer                             :: ind

ind = (x(3)-1)*n(2)*n(1) + (x(2)-1)*n(1) + x(1)

end function coords2index
! ...
! =============================================================================
! ...
pure function index2coords(ind,n) result(x)
! ... Gets the x=(i,j,k) and returns its index = (k-1)*nx*ny + (j-1)*nx + i

integer, intent(in)                 :: ind
integer, dimension(3), intent(in)   :: n  ! = (nx,ny,nz)
integer, dimension(3)               :: x  ! = (i,j,k)

! ...
integer residual
!ind = (x(3)-1)*n(2)*n(1) + (x(2)-1)*n(1) + x(1)
x(1) = mod(ind-1,n(1)) + 1

residual = (ind - x(1))/n(1) + 1
x(2) = mod(residual-1,n(2)) + 1

x(3) = (residual - x(2))/n(2) + 1

end function index2coords
! ...
! =============================================================================
! ...
pure function line_replace(A,pattern1,pattern2,ntimes) result(t)
! ... Takes pattern1 in A and replaces it by pattern2. Does it ntimes
! ... If ntimes < 0 it does it for all appearences

character(len=*), intent(in)   :: A
character(len=*), intent(in)   :: pattern1
character(len=*), intent(in)   :: pattern2
integer, intent(in), optional  :: ntimes
character(len=len(A))          :: t

! ... Local variables
! ...
integer j,n,n1,n2,nc,ncm

n = len_trim(A)
n1 = len(pattern1)
n2 = len(pattern2)
if (.not.present(ntimes)) then
  ncm = -1
else
  ncm = ntimes
endif

if (n.eq.0) return
if (n1.eq.0) return

t = A
nc = 0
10  j = index (t(:n),pattern1)
    nc = nc + 1
    if (j.gt.0) then
      t(j+n2:) = t(j+n1:n)
      t(j:j+n2-1) = pattern2(:n2)
      n = len_trim(t)
      if ((ncm.lt.0).or.(nc.lt.ncm)) goto 10
    endif

end function line_replace
! ...
! =============================================================================
! ...
pure function compress(A) result(t)
! ... Removes all double whites and spaces before a comma or dot

character(len=*), intent(in)   :: A
character(len=len(A))          :: t

! ... Local variables
! ...
integer i,n

! ... Remove leading whitespaces
! ...
t = adjustl(A)
n = len_trim(t)

! ... Replace tabs by whitespaces
! ...
do i=1,n
  if (iachar(t(i:i)).eq.9) t(i:i) = ' '
enddo

! ... Remove all double whites
! ...
10 i = index(t,'  ')
   if ((i.gt.0).and.(i.lt.n)) then
     t(i+1:) = t(i+2:)
     n = n - 1
     goto 10
   endif

! ... Remove space-commas
! ...
20 i = index(t,' ,')
   if ((i.gt.0).and.(i.lt.n)) then
     t(i:) = t(i+1:)
     n = n - 1
     goto 20
   endif

! ... Remove space-dots
! ...
30 i = index(t,' .')
   if ((i.gt.0).and.(i.lt.n)) then
     t(i:) = t(i+1:)
     n = n - 1
     goto 30
   endif

end function compress
! ...
! =============================================================================
! ...
function filetype(ifile) result(ftype)

character(len=3)                  :: ftype
character(len=*), intent(in)      :: ifile

! ... Local variables
! ...
character(len=4), parameter       :: scdf = 'CDF'//char(1)
character(len=4), parameter       :: self = char(127)//'ELF'
logical                           :: done
integer                           :: i,l,nl
character(len=180)                :: line

open(10,file=ifile,status='old')
nl = numlines(10)

l = 0
done = .false.
do while (.not.done)
   l = l + 1
   read(10,'(A)') line
   if (len_trim(line).gt.0) done = .true.
enddo
close(10)

if (l.eq.1) then
  if (line(1:4).eq.scdf) then
    ftype = 'cdf'
    return
  endif
  if (line(1:4).eq.self) then
    ftype = 'elf'
    return
  endif
  if (line(1:1).eq.char(11)) then
    ftype = 'bin'
    return
  endif
endif

done = .true.
do i=1,len_trim(line)
  l = ichar(line(i:i))
  if (l.gt.123) done = .false.
  if (l.lt.32) then
    if ((l.ne.9).and.(l.ne.13)) done = .false.
  endif
  if (.not.done) exit
enddo

if (done) then 
  ftype = 'asc'
  return
endif

ftype = 'bin'

end function filetype
! ...
! =============================================================================
! ...
subroutine get_commandline (commandline)

character(len=*), intent(out)   :: commandline

! ... Local variables
! ...
integer i
character(len=280) word

commandline = ''
do i=0,iargc()
  word = ''
  call getarg(i,word)
  commandline = trim(commandline)//' '//trim(adjustl(word))
enddo

end subroutine get_commandline
! ...
! =============================================================================
! ...
subroutine line_word(line,nw,word)
! ... Returns the nw-th word in line

character(len=*), intent(in)   :: line
integer, intent(in)            :: nw
character(len=*)               :: word

! ... Local variables
! ...
integer i,j,n,nm,nn,jmax
character ai
character(len=len(line))       :: t

jmax = len(word)
word = ''

t    = compress(line)
nm   = numwords(t)
if (nw.GT.nm) return

n    = len_trim(t)
j = 0
nn = 0
do i=1,n-1
  ai = t(i:i)
  if ((ai.ne.' ').and.(ai.ne.',')) then
    j = j + 1
    if (j.le.jmax) word(j:j) = ai
  else
    if (len_trim(word).gt.0) then
      nn = nn + 1
      if (nn.eq.nw) return
      j = 0
      word = ''
    endif
  endif
enddo

ai = t(n:n)
if ((ai.ne.' ').and.(ai.ne.',')) then
  j = j + 1
  if (j.le.jmax) word(j:j) = ai
endif

return
end subroutine line_word
! ...
! =============================================================================
! ...
integer pure function locate(x,xo) result(j)
! ... Returns the location of the array x(1:n), such that x(j) < xo < x(j+1)
real(dp), dimension(:), intent(in)   :: x
real(dp), intent(in)                 :: xo

! ... Local variables
! ...
logical slope
integer n,jl,jm,ju
real(dp) xn,x1

n = size(x)
x1 = x(1)
xn = x(n)
slope = x(n).gt.x(1)

jl = 0
ju = n+1
do while (ju-jl.gt.1)
  jm = (ju+jl)/2
  if (slope.eqv.(xo.gt.x(jm))) then
    jl = jm
  else
    ju = jm
  endif
enddo
j = jl

end function locate
! ...
! =============================================================================
! ...
pure function lowercase(A) result(t)
! ... Returns string in lowercase

character(len=*), intent(in)   :: A
character(len=len(A))          :: t

! ... Local variables
! ...
integer i,nal,nau,nzl,ii,l

t   = A
nal = ICHAR('A')
nau = ICHAR('a')
nzl = ICHAR('Z')
l   = nau - nal

do i=1,len(A)
  ii = ichar(A(i:i))
  if ((ii.ge.nal).and.(ii.le.nzl)) then
    ii = ii + l
    t(i:i) = char(ii)
  endif
enddo            

return
end function lowercase
! ...
! =============================================================================
! ...
!pure function ltrim(A) result(t)
! ... Returns string without heading blanks
!
!character(len=*), intent(in)   :: A
!character(len=len(A))          :: t
!
! ... Local variables
! ...
!integer i,io
!
!t = A
!
!io = 1
!do i=1,len(A)
!  if (.NOT.whitechar(A(i:i))) exit
!  io = io + 1
!enddo
!t(:) = A(io:)
!
!return
!end function ltrim
! ...
! =============================================================================
! ...
subroutine menu (title,options,Noptions,option)
! ... Proposes a menu on the screen and waits for the response.

integer Noptions,option
character(len=*) Title
character(len=*) options(Noptions)

! ... Local variables
! ...
integer nmax,nn,i,ind
character(len=78) line

write(*,*)
nmax = 1
do i=1,Noptions
   nn = LEN_TRIM(options(i))
   if (nn.gt.nmax) nmax = nn
enddo

ind = (78 - 2*nmax)/2
if (ind.lt.0) ind = 1
write(*,'(T2,A)') Title
do i=1,Noptions
   line = ' '
   write(line(ind+1:ind+4),'(i2,'' -'')') i
   line(ind+6:) = options(i)
   print*, line
enddo
line = 'OPTION : '

10   write(*,'(T2,A)',advance="no") TRIM(line)
     read(*,'(I6)',err=10) option
     if (option.le.0) goto 10
     if (option.gt.Noptions) goto 10

return
end subroutine menu 
! ...
! =============================================================================
! ...
subroutine mkdir(dirname)

character(len=*), intent(in)    :: dirname
call system('mkdir -p '//compress(dirname))

end subroutine mkdir
! ...
! =============================================================================
! ...
function now ()
! ... Returns the current date and time
! ... The output format according to the ISO 8601 suggestion

character(len=15) now

character(len=8) date
character(len=10) time

CALL date_and_time (date,time)

now = date//'T'//time(1:6)

return
end function now
! ...
! =============================================================================
! ...
subroutine newfilename (filename)

character(len=*), intent(inout)          :: filename

! ... Local variables
! ...
logical exist
integer n,i,im,j,jj
character(len(filename))                 :: rot
character(len(filename))                 :: ext

inquire(file=filename,exist=exist)
if (.not.exist) return

n = len_trim(filename)
do i=n,1,-1
  if (filename(i:i).eq.'.') exit
enddo
im = i - 1
if (im.le.0) im = n
rot = filename(1:im)
ext = trim(filename(im+1:))

i = 0
do while (exist)
  i = i + 1
  write(filename,'(A,".",I4,A)') trim(rot),i,trim(ext)
  jj = 0 
  do j=1,len_trim(filename)
    if (filename(j:j).ne.' ') then
      jj = jj + 1
      filename(jj:jj) = filename(j:j)
    endif
  enddo
  filename(jj+1:) = ''
  inquire(file=filename,exist=exist)
enddo

end subroutine newfilename
! ...
! =============================================================================
! ...
integer function numlines (iu,type)
! ... Returns the number of records in an ASCII or in an UNFORMATTED file
! ... For an unformatted file, type must start by 'b' or 'B'.

integer, intent(in)                      :: iu
character(len=*), intent(in), optional   :: type

! ... Local variables:
! ...
logical ascii
integer ii

if (.not.present(type)) then
  ascii = .true.
else
  if ((type(1:1).eq.'b').or.(type(1:1).eq.'B')) then
    ascii = .false.
  else
    ascii = .true.
  endif
endif

rewind(iu)

ii = 0
if (ascii) then
  10 read(iu,*,end=20)
     ii = ii + 1
     goto 10
  20 continue
else
  30 read(iu,end=40)
     ii = ii + 1
     goto 30
  40 continue
endif

numlines = ii
rewind(iu)

return
end function numlines
! ...
! =============================================================================
! ...
pure integer function numwords(A)
! ... Counts the number of words in a string

character(len=*), intent(in)   :: A

! ... Local variables
! ...
integer i,n
character(len=len(A))          :: t
character                      :: ai,an,ap

numwords = 0

t = compress(A)
n = len_trim(t)
if (n.eq.0) return

numwords = 1
do i=2,n-1
  ai = t(i:i)
  an = t(i+1:i+1)
  ap = t(i-1:i-1)
  if ((ai.eq.',').and.(an.eq.' ')) then
    numwords = numwords + 1
  else if (ai.eq.',') then
    numwords = numwords + 1
  else if ((ai.eq.' ').and.(ap.ne.',')) then
    numwords = numwords + 1
  endif
enddo

return
end function numwords
! ...
! =============================================================================
! ...
subroutine stop_error(err,msg)
! ... Stops the program with option to signal abnormal termination sending 
! ... an exit code of 1.

integer, intent(in)                    :: err
character(len=*), intent(in), optional :: msg

if (present(msg)) THEN
  if (len_trim(msg).GT.0) WRITE(0,*) trim(msg)
endif

if (err.eq.0) then
  stop 0
else
  stop 1
endif

end subroutine stop_error
! ...
! =============================================================================
! ...
subroutine strcat(s1,s2)
! ... Concatenates s2 into s1. A white character is placed in between.

character(len=*), intent(inout) :: s1
character(len=*), intent(in)    :: s2

s1 = trim(s1)//' '//trim(adjustl(s2))

end subroutine strcat
! ...
! =============================================================================
! ...
integer function unitfree()
! ... Returns a unit not yet assigned

! ... Local variables
! ...
LOGICAL flag

unitfree = 10
flag = .TRUE.
do while (flag)
  unitfree = unitfree + 1
  inquire(unitfree,opened=flag)
enddo

return
end function unitfree
! ...
! =============================================================================
! ...
pure function uppercase(A) result(t)
! ... Returns string in uppercase

character(len=*), intent(in)   :: A
character(len=len(A))          :: t

! ... Local variables
! ...
integer i,nal,nau,nzl,ii,l

t   = A
nal = ICHAR('a')
nau = ICHAR('A')
nzl = ICHAR('z')
l   = nau - nal

do i=1,len(A)
  ii = ichar(A(i:i))
  if ((ii.ge.nal).and.(ii.le.nzl)) then
    ii = ii + l
    t(i:i) = char(ii)
  endif
enddo            

return
end function uppercase
! ...
! =============================================================================
! ...
subroutine say(text,pos)
! ... Writes a text in the screen, using as many lines as required
! ... without splitting words.
! ... The value fo pos is used for indentation.

character(len=*), intent(in)         :: text
integer, intent(in), optional        :: pos

character(len=180)  :: ww
character(len=7)  :: fmt
integer nlen,ipos
integer i,io,il,n

if (present(pos)) then
  ipos = pos
else
  ipos = 1
endif

write(fmt,'("(T",i2.2,",A)")') ipos

ww = ''
nlen = 80 - ipos
n = len_trim(text)

io = 1

10 continue
  il = io + nlen
  if (il.ge.n) then
    il = n
  else
    do i=il,io,-1
      if (text(i-1:i-1).ne.' '.and.text(i:i).EQ.' ') exit
    enddo
    il = i - 1
  endif
  write(6,fmt) text(io:il)
  do i=il,n
    if (text(i-1:i-1).EQ.' '.AND.text(i:i).NE.' ') EXIT
  enddo
  io = i
  if (io.LT.n) goto 10

return
end subroutine say
! ...
! =============================================================================
! ...
logical pure function whitechar(char)
! ... Returns .true. if char is space (32) or tab (9), .false. otherwise

character, intent(in)          :: char

if ((iachar(char).eq.32).or.(iachar(char).eq.9)) then
  whitechar = .true.
else
  whitechar = .false.
endif

end function whitechar
! ...
! =============================================================================
! ...

end module utils

