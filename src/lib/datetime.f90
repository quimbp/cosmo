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

  character(len=20), dimension(4)           :: calendars_ = ['standard ',  &
                                                             'gregorian', &
                                                             '365_day  ', &
                                                             'noleap   ']
  character(len=7), dimension(4)            :: units_     = ['seconds',  &
                                                             'minutes', &
                                                             'hours  ', &
                                                             'days   ']


  type type_date
    integer                  :: year     = 0
    integer                  :: month    = 0
    integer                  :: day      = 0
    integer                  :: hour     = 0
    integer                  :: minute   = 0
    integer                  :: second   = 0
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
! ...
! ====================================================================
! ====================================================================
! ====================================================================
! ...
integer function cmp_(i,j)

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
! ====================================================================
! ...
subroutine check_calendar (calendar)

  character(len=*), intent(inout)            :: calendar

  calendar = lowercase(calendar)

  if (count(calendar.eq.calendars_).eq.0) then
    stop 'Unsupported calendar'
  else
    if (index(calendar,'365').gt.0) calendar = 'noleap'
    if (index(calendar,'noleap').gt.0) calendar = 'noleap'
    if (index(calendar,'standard').gt.0) calendar = 'gregorian'
    if (index(calendar,'gregorian').gt.0) calendar = 'gregorian'
  endif

end subroutine check_calendar
! ...
! ====================================================================
! ...
subroutine check_units (units)

  character(len=*), intent(inout)            :: units

  units = lowercase(units)

  if (units(1:1).eq.'s') then
    units = 'seconds'
  else  if (units(1:1).eq.'m') then
    units = 'minutes'
  else  if (units(1:1).eq.'h') then
    units = 'hours'
  else  if (units(1:1).eq.'d') then
    units = 'days'
  else
    call stop_error(1,'Unsupported units')
  endif

end subroutine check_units
! ...
! ====================================================================
! ...
logical function isleap(year,calendar)

  integer, intent(in)                    :: year
  character(len=*), intent(in), optional :: calendar

  ! ... Local variable
  ! ...
  character(len=20) cal
 
  if (present(calendar)) then
    cal = trim(calendar)
    call check_calendar(cal)
    if (trim(cal).eq.'noleap') then
      isleap = .false.
      return
    endif
  endif

  isleap = .false.
  if (MOD(year,400).eq.0) isleap = .true.
  if ((MOD(year,4).eq.0).and.(mod(year,100).ne.0)) isleap = .true.

  return
end function isleap
! ...
! ====================================================================
! ...
subroutine split_units(units,time_units,RefDate)

  character(len=*), intent(in)                :: units
  character(len=20), intent(out)              :: time_units
  character(len=20), intent(out)              :: RefDate

  ! ... Local variables
  ! ...
  integer i
  character(len=len(units)) att

  if (len_trim(units).eq.0) then
    time_units = 'none'
    RefDate = 'none'
    return
  endif

  att = lowercase(units)
  i = index(att,'since')
  if (i.eq.0) then
    RefDate = ''
    time_units = compress(att)
  else
    RefDate = compress(att(i+6:))
    time_units = compress(att(1:i-1))
  endif
  call check_units(time_units)

end subroutine split_units
! ...
! ====================================================================
! ...
function unit_conversion_factor(units) result(factor)

  character(len=*), intent(in)               :: units
  real(dp)                                   :: factor

  if (index(units,'sec').gt.0) then
    factor = 1.0D0
  else if (index(units,'min').gt.0) then
    factor = 60.0D0
  else if (index(units,'hou').gt.0) then
    factor = 3600.0D0
  else if (index(units,'day').gt.0) then
    factor = 86400.0D0
  endif

end function unit_conversion_factor
! ...
! =====================================================================
! ...
integer function days_before_year(year,calendar)
  ! ... Number of days before January 1st of year
  ! ...
  integer, intent(in)                    :: year
  character(len=*), intent(in), optional :: calendar

  ! ... Local variables
  ! ...
  integer y
  character(len=20) cal

  y = year - 1

  if (present(calendar)) then
    cal = trim(calendar)
    call check_calendar(cal)
    if (trim(cal).eq.'noleap') then
      days_before_year = y*365
      return
    endif
  endif

  days_before_year = y*365 + y/4 - y/100 + y/400

  return
end function days_before_year
! ...
! =====================================================================
! ...
integer function days_in_month(year,month,calendar)
  ! ... Number of days in that month in that year
  ! ...
  integer, intent(in)                    :: year,month
  character(len=*), intent(in), optional :: calendar

  ! ... Local variables
  ! ...
  character(len=20) cal

  if (present(calendar)) then
    cal = trim(calendar)
    call check_calendar(cal)
    if (trim(cal).eq.'noleap') then
      days_in_month = DAYS_IN_MONTH_(month)
      return
    endif
  endif

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
integer function days_before_month(year,month,calendar)
! ... Number of fays in year preceding first day of month
! ...
  integer, intent(in)                    :: year,month
  character(len=*), intent(in), optional :: calendar

  ! ... Local variables
  ! ...
  character(len=20) cal

  if (present(calendar)) then
    cal = trim(calendar)
    call check_calendar(cal)
    if (trim(cal).eq.'noleap') then
      days_before_month = DAYS_BEFORE_MONTH_(month)
      return
    endif
  endif

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
integer function ymd2ord(year,month,day,calendar)
! ...  Returns an ordinal, assuming that 01-Jan-0001 is day 1
! ...
  integer, intent(in)                    :: year,month,day
  character(len=*), intent(in), optional :: calendar

  if (present(calendar)) then
    ymd2ord = days_before_year(year,calendar)         +  &
              days_before_month(year,month,calendar)  +  &
              day
  else
    ymd2ord = days_before_year(year)         +  &
              days_before_month(year,month)  +  &
              day
  endif

  return 
end function ymd2ord
! ...
! =====================================================================
! ...
subroutine ord2ymd(nn,year,month,day,calendar)
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
  character(len=*), intent(in), optional :: calendar

  ! ... Local variables
  ! ...
  logical leapyear
  integer n,n400,n100,n1,n4,preceding
  integer remainder
  character(len=20) cal

  if (present(calendar)) then
    cal = trim(calendar)
    call check_calendar(cal)
    if (trim(cal).eq.'noleap') then
      n = nn
      remainder = mod(n,365)
      n = n - remainder
      year = n / 365 + 1
      do month=1,12
        if (DAYS_BEFORE_MONTH_(month).ge.remainder) exit
      enddo
      month = month - 1
      day = remainder - DAYS_BEFORE_MONTH_(month)
      return
    endif
  endif

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
type(type_date) function date_is(y,m,d,hh,mm,ss,calendar) result(p)

  integer, intent(in)                     :: y,m,d
  integer, intent(in), optional           :: hh
  integer, intent(in), optional           :: mm
  integer, intent(in), optional           :: ss
  character(len=*), intent(in), optional  :: calendar

  ! ... Local variables
  ! ...
  integer dnum
  character(len=20) cal

  if (present(calendar)) then
    cal = trim(calendar)
    call check_calendar(cal)
    p%calendar = trim(cal)
  else
    p%calendar = 'gregorian'
  endif

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

  dnum = days_before_month(y,m,p%calendar) + d
  p%yearday = dnum

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
  character(len=*), intent(inout), optional  :: cal

  ! ... Local variables
  ! ...
  integer dnum

  if (present(cal)) then
    call check_calendar(cal)
    p%calendar = trim(cal)
  else
    p%calendar = 'gregorian'
  endif

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

  dnum = days_before_month(y,m,p%calendar) + d
  p%yearday = dnum
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
integer function julday(year,month,day) result(jd)
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
function num2date(time,units,calendar) result(date)

  ! ... Function returning a date
  ! ... The internal calculations are done in seconds
  ! ...

  real(dp), intent(in)                       :: time
  character(len=*), intent(in), optional     :: units      ! Default: Julian days
  character(len=*), intent(in), optional     :: calendar   ! Default: standard
  type(type_date)                            :: date

  ! ... Local variables
  ! ...
  integer ndays
  integer(kind=8) isecs,remainder
  integer(kind=8) TimeRef,TimeSec
  integer year,month,day,hour,minute,second
  real(dp) factor
  character(len=20) lcal,time_units,IsoRef
  type(type_date) DateRef

  if (.not.present(calendar)) then
    lcal = 'gregorian'
  else
    lcal = lowercase(calendar)
    call check_calendar(lcal)
  endif

  ! ... Check the units and the reference date:
  ! ...
  if (present(units)) then
    call split_units(units,time_units,IsoRef)
    factor = unit_conversion_factor(time_units)

    if (len_trim(IsoRef).gt.0) then
      DateRef = strpreftime(units)
      DateRef%calendar = trim(lcal)
      TimeRef = date2num(DateRef) * 86400.0D0  ! Seconds
    else
      TimeRef = 0.0D0
    endif
  else
    TimeRef = 0.0D0
    factor  = 86400.0D0   ! The default input is in days
  endif

  ! ... Fraction
  ! ...
  TimeSec = TimeRef + time*factor

  isecs = mod(TimeSec,86400_8)
  remainder = TimeSec - isecs

  ndays = remainder / 86400_8     ! Days

  if (trim(lcal).eq.'gregorian') then

    call caldat(ndays,year,month,day)

  else if (trim(lcal).eq.'noleap') then

    remainder = mod(ndays,365)
    ndays = ndays - remainder
    year = ndays / 365 + 1

    do month=1,12
      if (DAYS_BEFORE_MONTH_(month).ge.remainder) exit
    enddo
    month = month - 1

    day = remainder - DAYS_BEFORE_MONTH_(month)

  endif

  date%year   = year
  date%month  = month
  date%day    = day

  ! ... Process the isecs:
  ! ...

  date%second = mod(isecs,60_8)
  isecs       = (isecs-date%second)/60_8
  date%minute = mod(isecs,60_8)
  date%hour   = (isecs-date%minute)/60_8

  date%calendar = trim(lcal)
  date%yearday = days_before_month(year,month,date%calendar) + day
  return

end function num2date
! ...
! =====================================================================
! ...
recursive function date2num(date,units) result(time)

  ! ... Function date2num
  ! ... Returns a real number whith the integer part accounting for
  ! ... days and the fraction part accounting for a frational part
  ! ... of the day.
  ! ... Input:
  ! ...         date (date_type)
  ! ...         units (string)
  ! ... Output:
  ! ...         time (real) days

  type(type_date), intent(in)                :: date
  character(len=*), intent(in), optional     :: units
  real(dp)                                   :: time

  ! ... Local variables
  ! ...
  real(dp) TimeRef
  character(len=20) lcal,time_units,IsoRef
  type(type_date) DateRef


  lcal = lowercase(date%calendar)
  call check_calendar(lcal)

  if (trim(lcal).eq.'gregorian') then

    time = julday(date%year,date%month,date%day) + &
           (3600.0D0*date%hour + 60.0*date%minute + date%second)/86400.0D0

  else if (trim(lcal).eq.'noleap') then

    if (date%month.eq.2.and.date%day.eq.29) call stop_error(1,'Is not a leap year')

    time = 365D0*(date%year-1)            + &
           DAYS_BEFORE_MONTH_(date%month) + &
           date%day +                       &
           (3600.0D0*date%hour + 60.0*date%minute + date%second)/86400.0D0

  endif

  ! ... At this point, the time is in days with a fraction part:
  ! ... Check if units are requested
  ! ...
  if (present(units)) then

    call split_units(units,time_units,IsoRef)

    if (len_trim(IsoRef).gt.0) then
      DateRef = strpreftime(units)
      DateRef%calendar = trim(lcal)
      TimeRef = date2num(DateRef)  ! Days
    else
      TimeRef = 0.0D0
    endif
    time = time - TimeRef

    if (trim(time_units).eq.'seconds') then
      time = time * 86400.0D0
    else if (trim(time_units).eq.'minutes') then
      time = time * 1440.0D0
    else if (trim(time_units).eq.'hours') then
      time = time * 24.0D0
    endif

  endif

  return

end function date2num
! ...
! =====================================================================
! ...
end module module_datetime
