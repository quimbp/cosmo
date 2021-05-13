! *********************************************************************
! ... datetime.f90
! ... Quim Ballabrera
! ...
! *********************************************************************

module module_datetime

use module_types
use module_utils

implicit none

private cmp_

integer, dimension(12) :: DAYS_IN_MONTH_ = [31,28,31,30,31,30,31,31,30,31,30,31]
integer, dimension(12) :: DAYS_BEFORE_MONTH_ = [0,31,59,90,120,151,181,212,243,273,304,334]
character(len=3), dimension(7) :: DAYNAMES_ = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
character(len=3), dimension(12) :: MONTHNAMES_ = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", &
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

type type_date
  integer                  :: year     = 0
  integer                  :: month    = 0
  integer                  :: day      = 0
  integer                  :: hour     = 0
  integer                  :: minute   = 0
  integer                  :: second   = 0
  integer                  :: weekday  = 0           ! 1 to 7
  integer                  :: yearday  = 0           ! 1 to 365/366
  character(len=20)        :: calendar = 'gregorian'

  contains
    procedure              :: iso           => date_iso
    procedure              :: now           => date_now
    procedure              :: is            => date_set
    procedure              :: jd            => date2jd
    procedure              :: timedelta     => date_increment

end type type_date

! ... A 4-year cycle has an extra leap day over what we'd get from 
! ... pasting together 4 single years.
! ...
integer, parameter                :: DI4Y = 4 * 365 + 1

! ... Similarly, a 100-year cycle has one fewer leap day than we'd get from
! ... pasting together 25 4-year cycles.
! ...
integer, parameter                :: DI100Y = 25 * DI4Y - 1

! ... Finally, a 400-year cycle has an extra leap day over what we'd 
! ... get from pasting together 4 100-year cycles.
! ...
integer, parameter                :: DI400Y = 4 * DI100Y + 1

contains

integer pure function cmp_(i,j)

integer, intent(in)       :: i,j

if (i.eq.j) then
  cmp_ = 0
else if (i.gt.j) then
  cmp_ = 1
else
  cmp_ = -1
endif

return
end function cmp_
! ...
! =====================================================================
! ...

logical pure function isleap(year)

integer, intent(in)             :: year

isleap = .false.
if (MOD(year,400).eq.0) isleap = .true.
if ((MOD(year,4).eq.0).and.(mod(year,100).ne.0)) isleap = .true.

return
end function isleap
! ...
! =====================================================================
! ...
integer pure function days_before_year(year)
! ... Number of days before January 1st of year
! ...
integer, intent(in)             :: year

! ... Local variables
! ...
integer y

y = year - 1
days_before_year = y*365 + y/4 - y/100 + y/400

return
end function days_before_year
! ...
! =====================================================================
! ...
integer pure function days_in_month(year,month)
! ... Number of days in that month in that year
! ...
integer, intent(in)             :: year,month

if (month.eq.2.and.isleap(year)) then
  days_in_month = 29
else
  days_in_month = DAYS_IN_MONTH_(month)
endif

return

end function days_in_month
! ...
! =====================================================================
! ...
integer pure function days_before_month(year, month)
! ... Number of fays in year preceding first day of month
! ...
integer, intent(in)              :: year,month

if (month.gt.2.and.isleap(year)) then
  days_before_month = DAYS_BEFORE_MONTH_(month) + 1
else
  days_before_month = DAYS_BEFORE_MONTH_(month)
endif

return 
end function days_before_month
! ...
! =====================================================================
! ...
integer pure function ymd2ord(year,month,day)
! ...  Returns an ordinal, assuming that 01-Jan-0001 is day 1
! ...
integer, intent(in)              :: year,month,day

ymd2ord = days_before_year(year)         +  &
          days_before_month(year, month) +  &
          day

return 
end function ymd2ord
! ...
! =====================================================================
! ...
subroutine ord2ymd(nn,year,month,day)
! ... Returns (year, month, day) from ordinal, if 01-Jan-0001 is day 1
! ...
! ... n is a 1-based index, starting at 1-Jan-1.  The pattern of leap years
! ... repeats exactly every 400 years.  The basic strategy is to find the
! ... closest 400-year boundary at or before n, then work with the offset
! ... from that boundary to n.  Life is much clearer if we subtract 1 from
! ... n first -- then the values of n at 400-year boundaries are exactly
! ... those divisible by _DI400Y:
! ...
! ...     D  M   Y            n              n-1
! ...     -- --- ----        ----------     ----------------
! ...     31 Dec -400        -_DI400Y       -_DI400Y -1
! ...      1 Jan -399         -_DI400Y +1   -_DI400Y      400-year boundary
! ...     ...
! ...     30 Dec  000        -1             -2
! ...     31 Dec  000         0             -1
! ...      1 Jan  001         1              0            400-year boundary
! ...      2 Jan  001         2              1
! ...      3 Jan  001         3              2
! ...     ...
! ...     31 Dec  400         _DI400Y        _DI400Y -1
! ...      1 Jan  401         _DI400Y +1     _DI400Y      400-year boundary
! ...
integer, intent(in)             :: nn
integer, intent(out)            :: year,month,day
!dimension                       :: p(3)    ! (year,month,day)

! ... Local variables
! ...
logical leapyear
integer n,n400,n100,n1,n4,preceding

n = nn - 1

n400 = n / DI400Y
n    = mod(n,DI400Y)

year = n400 * 400 + 1   ! ..., -399, 1, 401, ...

! ... Now n is the (non-negative) offset, in days, from January 1 of year, to
! ... the desired date.  Now compute how many 100-year cycles precede n.
! ... Note that it's possible for n100 to equal 4!  In that case 4 full
! ... 100-year cycles precede the desired day, which implies the desired
! ... day is December 31 at the end of a 400-year cycle.
! ...
n100 = n / DI100Y
n    = mod(n,DI100Y)

! ... Now compute how many 4-year cycles precede it.
! ...
n4 = n / DI4Y
n  = mod(n,DI4Y)

! ... And now how many single years.  Again n1 can be 4, and again meaning
! ... that the desired day is December 31 at the end of the 4-year cycle.
! ...
n1 = n / 365
n  = mod(n,365)

year = year + n100 * 100 + n4 * 4 + n1

if (n1.eq.4.or.n100.eq.4) then
  if (n.ne.0) stop "ERROR in ord2ymd"
  year = year-1; month = 12; day = 31
  !p = [year-1, 12, 31]
  return
endif

! ... Now the year is correct, and n is the offset from January 1.  We find
! ... the month via an estimate that's either exact or one too large.
! ...
leapyear = isleap(year)
month = (n+50) / (2**5)
if (month.gt.2.and.leapyear) then
  preceding = DAYS_BEFORE_MONTH_(month) + 1
else
  preceding = DAYS_BEFORE_MONTH_(month)
endif

if (preceding.gt.n) then
! ... Estimate is too large
  month = month - 1
  if (month.eq.2.and.leapyear) then
    preceding = preceding - (DAYS_IN_MONTH_(month) + 1)
  else
    preceding = preceding - DAYS_IN_MONTH_(month)
  endif
endif

n = n - preceding

! ... the year and month are correct, and n is the offset from the
! ... start of that month:  we're done!
! ...
!p = [year, month, n+1]
day = n+1
return

end subroutine ord2ymd
! ...
! =====================================================================
! ...
type(type_date) function date_is(y,m,d,hh,mm,ss,cal) result(p)

integer, intent(in)                     :: y,m,d
integer, intent(in), optional           :: hh
integer, intent(in), optional           :: mm
integer, intent(in), optional           :: ss
character(len=*), intent(in), optional  :: cal

! ... Local variables
! ...
integer wday,dnum

wday = mod(ymd2ord(y,m,d)+6,7) + 1
dnum = days_before_month(y,m) + d

p%year    = y
p%month   = m
p%day     = d

if (present(hh)) then
  p%hour    = hh
else
  p%hour    = 0
endif
if (present(mm)) then
  p%minute  = mm
else
  p%minute  = 0
endif
if (present(ss)) then
  p%second  = ss
else
  p%second  = 0
endif

p%weekday = wday
p%yearday = dnum
if (present(cal).and.len_trim(cal).gt.0) then
  p%calendar = trim(cal)
else
  p%calendar = 'gregorian'
endif

return
end function date_is
! ...
! =====================================================================
! ...
subroutine date_set(p,y,m,d,hh,mm,ss,cal) 

class(type_date), intent(inout)         :: p
integer, intent(in)                     :: y,m,d
integer, intent(in), optional           :: hh
integer, intent(in), optional           :: mm
integer, intent(in), optional           :: ss
character(len=*), intent(in), optional  :: cal

! ... Local variables
! ...
integer wday,dnum

wday = mod(ymd2ord(y,m,d)+6,7) + 1
dnum = days_before_month(y,m) + d

p%year    = y
p%month   = m
p%day     = d

if (present(hh)) then
  p%hour    = hh
else
  p%hour    = 0
endif
if (present(mm)) then
  p%minute  = mm
else
  p%minute  = 0
endif
if (present(ss)) then
  p%second  = ss
else
  p%second  = 0
endif

p%weekday = wday
p%yearday = dnum
if (present(cal).and.len_trim(cal).gt.0) then
  p%calendar = trim(cal)
else
  p%calendar = 'gregorian'
endif

return
end subroutine date_set
! ...
! =====================================================================
! ...
character(len=8) function format_time(hh,mm,ss) result(timestr)

integer, intent(in)              :: hh,mm,ss     ! hour, minute, second

write(timestr,'(T1,I2.2,":",I2.2,":",I2.2)') hh,mm,ss

return
end function format_time
! ...
! =====================================================================
! ...
character(len=25) function date_iso(date,Z) result(text)

class(type_date), intent(in)              :: date  ! hour, minute, second
character(len=*), optional, intent(in)    :: Z     ! 'z' or 'Z'

if (present(Z)) then
  write(text,'(T1,I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)') &
      date%year, date%month, date%day, &
      date%hour, date%minute, date%second
  text=trim(text)//trim(uppercase(Z))
else
  write(text,'(T1,I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)') &
      date%year, date%month, date%day, &
      date%hour, date%minute, date%second
endif

return
end function date_iso
! ...
! =====================================================================
! ...
integer pure function julday(year,month,day) result(jd)
! ...  Returns the JD, Numerical recipes
! ...
integer, intent(in)              :: year,month,day

! ... Local variables
! ...
integer, parameter                      :: IGREG=15+31*(10+12*1582)
integer ja,jm,jy

jd = -999

jy = year
if (jy.eq.0) return
if (jy.lt.0) jy = jy + 1
if (month.gt.2) then
  jm = month + 1
else
  jy = jy - 1
  jm = month + 13
endif
jd = int(365.25d0*jy)+int(30.6001d0*jm)+day+1720995
if (day+31*(month+12*year).ge.IGREG) then
  ja = int(0.01d0*jy)
  jd = jd + 2 - ja + int(0.25d0*ja)
endif

return 
end function julday
! ...
! =====================================================================
! ...
subroutine caldat(julian,year,month,day)

integer, intent(in)                     :: julian
integer, intent(out)                    :: year,month,day

! ... Local variables
! ...
integer, parameter                      :: IGREG=2299161
integer ja,jalpha,jb,jc,jd,je

if (julian.ge.IGREG) then
  jalpha = int(((julian-1867216)-0.25d0)/36524.25d0)
  ja = julian+1+jalpha-int(0.25d0*jalpha)
else
  ja = julian
endif

jb = ja + 1524
jc = int(6680.d0+((jb-2439870)-122.1d0)/365.25d0)
jd = 365*jc + int(0.25d0*jc)
je = int((jb-jd)/30.6001d0)
day = jb - jd - int(30.6001d0*je)
month = je - 1
if (month.gt.12) month = month-12
year = jc - 4715
if (month.gt.2) year = year-1
if (year.le.0) year = year-1

end subroutine caldat
! ...
! =====================================================================
! ...
type(type_date) function strpreftime (string) result(date)
! ... Retrieve the reference date from string

character(len=*), intent(in)           :: string

! ... Local variables
! ...
integer i,nw
character(len=len(string)) att,word

att = uppercase(string)
i = index(att,'SINCE')

if (i.le.0) then
  write(*,*) trim(string)
  write(*,*) 'STRPTIMEREF WARNING: No reference date'
  return
endif

att = att(i+6:)
i = len_trim(att)
if (att(i:i).EQ.'Z') att(i:) = ''


att = line_replace(att,'UTC',' ')
att = line_replace(att,'-',' ')
att = line_replace(att,':',' ')
att = line_replace(att,'T',' ')
nw  = numwords(att)
if ((nw.ne.7).and.(nw.ne.6).and.(nw.ne.3)) then
  write(*,*) trim(string)
  call stop_error(1,'Invalid units attribute')
endif

call line_word(att,1,word)
read(word,*) date%year
call line_word(att,2,word)
read(word,*) date%month
call line_word(att,3,word)
read(word,*) date%day

if (nw.ge.6) then
  call line_word(att,4,word)
  read(word,*) date%hour
  call line_word(att,5,word)
  read(word,*) date%minute
  call line_word(att,6,word)
  read(word,*) date%second
else
  date%hour   = 0
  date%minute = 0
  date%second = 0
endif

!call line_word(att,2,date%month)
!read(att(i:i+3),*)     date%year
!read(att(i+5:i+6),*)   date%month
!read(att(i+8:i+9),*)   date%day
!read(att(i+11:i+12),*) date%hour
!read(att(i+14:i+15),*) date%minute
!read(att(i+17:i+18),*) date%second

date%weekday = mod(ymd2ord(date%year,date%month,date%day)+6,7) + 1
date%yearday = days_before_month(date%year,date%month) + date%day
date%calendar = 'gregorian'

return
end function strpreftime
! ...
! =====================================================================
! ...
subroutine date_now(now) 
! ... Returns the current date and time

class(type_date), intent(inout)        :: now

! ... Local variables:
! ...
character(len=8) date
character(len=10) time

CALL date_and_time (date,time)

now%calendar = 'gregorian'

read(date(1:4),*) now%year
read(date(5:6),*) now%month
read(date(7:8),*) now%day

read(time(1:2),*) now%hour
read(time(3:4),*) now%minute
read(time(5:6),*) now%second

now%weekday = mod(ymd2ord(now%year,now%month,now%day)+6,7) + 1
now%yearday = days_before_month(now%year,now%month) + now%day

return
end subroutine date_now
! ...
! =====================================================================
! ...
real(dp) function date2jd(date) result(jd)

class(type_date), intent(in)            :: date

! ... Local variables
! ...
real(dp) seconds

seconds = 60.0_dp*(date%hour*60.0_dp + date%minute) + date%second

jd = julday(date%year,date%month,date%day)
jd = jd + seconds/86400.0_dp

end function date2jd
! ...
! =====================================================================
! ...
type(type_date) function jd2date(jd) result(date)

real(dp), intent(in)                    :: jd

! ... Local variables:
! ...
integer ijday
integer(kind=8) isecs           ! Double precision integer
character(len=20) calendar

ijday = INT(jd)
isecs = NINT((jd - ijday)*86400_8)

date%calendar = 'gregorian'
call caldat(ijday,date%year,date%month,date%day)

date%second = mod(isecs,60_8)
isecs       = (isecs-date%second)/60_8
date%minute = mod(isecs,60_8)
date%hour   = (isecs-date%minute)/60_8

end function jd2date
! ...
! =====================================================================
! ...
type(type_date) function strptime(string) result(date)
! ... Retrieve a date from string

character(len=*), intent(in)           :: string

! ... Local variables
! ...
integer i,nw
character(len=len(string)) att,word

att = uppercase(string)

att = line_replace(att,'UTC',' ')
att = line_replace(att,'-',' ')
att = line_replace(att,':',' ')
att = line_replace(att,'T',' ')
att = line_replace(att,'Z',' ')
att = line_replace(att,'"',' ')
att = line_replace(att,'[',' ')
att = line_replace(att,']',' ')
nw  = numwords(att)
if (nw.ne.6) then
  write(*,*) 'Error in strptime'
  write(*,*) 'input string    : ', trim(string)
  write(*,*) 'processed string: ', trim(att)
  write(*,*) 'number of terms : ', nw
  call stop_error(1,'Invalid iso date')
endif

call line_word(att,1,word)
read(word,*) date%year
call line_word(att,2,word)
read(word,*) date%month
call line_word(att,3,word)
read(word,*) date%day

call line_word(att,4,word)
read(word,*) date%hour
call line_word(att,5,word)
read(word,*) date%minute
call line_word(att,6,word)
read(word,*) date%second

date%weekday = mod(ymd2ord(date%year,date%month,date%day)+6,7) + 1
date%yearday = days_before_month(date%year,date%month) + date%day
date%calendar = 'gregorian'

return
end function strptime
! ...
! =====================================================================
! ...
type(type_date) function date_increment(date,days,hours,minutes,seconds) result(new_date)

class(type_date), intent(in)            :: date
integer, intent(in), optional           :: days,hours,minutes,seconds

! ... Local variables
! ...
integer i,num_days,num_hours,num_minutes,num_seconds,dmax


num_days = 0
num_hours = 0
num_minutes = 0
num_seconds = 0

if (present(seconds)) then
  num_minutes = num_minutes + seconds/60
  num_seconds = mod(seconds,60)
endif
if (present(minutes)) then
  num_minutes = num_minutes + minutes
  num_hours = num_hours + num_minutes/60
  num_minutes = mod(num_minutes,60)
endif
if (present(hours)) then
  num_hours = num_hours + hours
  num_days = num_days + num_hours/24
  num_hours = mod(num_hours,24)
endif
if (present(days)) num_days = num_days + days

new_date = date

if (num_days.gt.0) then
  do i=1,num_days
    call add_one_day(new_date)
  enddo
else if (num_days.lt.0) then
  ! negative days
  do i=1,abs(num_days)
    call minus_one_day(new_date)
  enddo
endif

if (num_hours.gt.0) then
  do i=1,num_hours
    call add_one_hour(new_date)
  enddo
else if (num_hours.lt.0) then
  do i=1,abs(num_hours)
    call minus_one_hour(new_date)
  enddo
endif

if (num_minutes.gt.0) then
  do i=1,num_minutes
    call add_one_minute(new_date)
  enddo
else if (num_minutes.lt.0) then
  do i=1,abs(num_minutes)
    call minus_one_minute(new_date)
  enddo
endif

if (num_seconds.gt.0) then
  do i=1,num_seconds
    new_date%second = new_date%second + 1
    if (new_date%second.eq.60) then
      new_date%second = 0
      call add_one_minute(new_date)
    endif
  enddo
else if (num_seconds.lt.0) then
  do i=1,abs(num_seconds)
    new_date%second = new_date%second - 1
    if (new_date%second.eq.-1) then
      new_date%second = 59
      call minus_one_minute(new_date)
    endif
  enddo
endif

  contains

    subroutine add_one_day(date)
    ! --------------------------
    type(type_date), intent(inout)   :: date
    integer dmax
    dmax = days_in_month(date%year,date%month)
    date%day = date%day + 1
    if (date%day.gt.dmax) then
      date%day = 1
      date%month = date%month + 1
      if (date%month.eq.13) then
        date%month = 1
        date%year = date%year + 1
      endif
    endif
    return
    end subroutine add_one_day

    subroutine minus_one_day(date)
    ! -------------------------------
    type(type_date), intent(inout)   :: date
    integer dmax
    if (date%month.eq.1) then
      dmax = 31
    else
      dmax = days_in_month(date%year,date%month-1)
    endif
    date%day = date%day - 1
    if (date%day.lt.1) then
      date%day = dmax
      date%month = date%month - 1
      if (date%month.eq.0) then
        date%month = 12
        date%year = date%year - 1
      endif
    endif
    return
    end subroutine minus_one_day

    subroutine add_one_hour(date)
    ! ---------------------------
    type(type_date), intent(inout)   :: date
    date%hour = date%hour + 1
    if (date%hour.eq.24) then
      date%hour = 0
      call add_one_day(date)
    endif
    return
    end subroutine add_one_hour

    subroutine minus_one_hour(date)
    ! -----------------------------
    type(type_date), intent(inout)   :: date
    date%hour = date%hour - 1
    if (date%hour.eq.-1) then
      date%hour = 23
      call minus_one_day(date)
    endif
    return
    end subroutine minus_one_hour

    subroutine add_one_minute(date)
    ! ---------------------------
    type(type_date), intent(inout)   :: date
    date%minute = date%minute + 1
    if (date%minute.eq.60) then
      date%minute = 0
      call add_one_hour(date)
    endif
    return
    end subroutine add_one_minute

    subroutine minus_one_minute(date)
    ! -----------------------------
    type(type_date), intent(inout)   :: date
    date%minute = date%minute - 1
    if (date%minute.eq.-1) then
      date%minute = 59
      call minus_one_hour(date)
    endif
    return
    end subroutine minus_one_minute


end function date_increment  
! ...
! =====================================================================
! ...
end module module_datetime
