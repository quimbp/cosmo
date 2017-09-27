module tokens

use utils

implicit none

contains

function token_read(line,prompt) result(ans)


character(len=*), intent(in)           :: line
character(len=*), intent(in)           :: prompt
character(len=:), allocatable          :: ans

! ... Local variables
! ...
integer plen,ip,iw,ic
character(len=len(line)) lline

lline = compress(line)
plen = len_trim(prompt)

ip = index(line,' '//prompt)
if (ip.le.0) then
  ans = ''
  return
else
  ip = ip + 1
endif


! ... Get the input line on the right of the token:
! ...
lline = compress(lline(ip+plen-1:))

! ... Remove the ':' or the '=' characters:
! ...
!if (lline(1:1).eq.'=') lline(1:1) = ''
!if (lline(1:1).eq.':') lline(1:1) = ''

! ... Remove left white spaces:
! ...
lline = adjustl(lline)

! ... Check for the first white space or the end of line:
! ...
iw = index(lline,' ') - 1
ic = index(lline,',') - 1
if (iw.le.0.and.ic.gt.0) then
  ip = ic
else if (iw.gt.0.and.ic.le.0) then
  ip = iw
else if (iw.gt.0.and.ic.gt.0) then
  ip = min(iw,ic)
else
  ip = 0
endif
if (ip.le.0) ip = len_trim(lline)

! ... Get the first word:
! ...
ans = lline(1:ip)

if (trim(ans).eq.'none') then
  ans = ''
else if (trim(ans).eq.'None') then
  ans = ''
else if (trim(ans).eq.'NONE') then
  ans = ''
endif

end function token_read
! ...
! =====================================================================
! ...


end module tokens

