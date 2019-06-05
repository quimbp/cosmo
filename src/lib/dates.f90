! *****************************************************************************
! ... date.f90
! ... Routines to manipulate date and time variables
! ... COSMO project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! *****************************************************************************

module dates

use types, only: dp
use utils, only: stop_error,line_replace,uppercase
use constants,  only: zero,one

implicit none
!private
!public date_type
!public date_string,string2date,cal2date,cal2jd,jd2cal, &
!       jd2date,date2jd,caldat,julday,sec2time, &
!       date_today
!public :: assignment(<)
!public :: assignment(>)

type date_type
  integer                  :: year   = 0
  integer                  :: month  = 0
  integer                  :: day    = 0
  integer                  :: hour   = 0
  integer                  :: minute = 0
  integer                  :: second = 0
  character(len=10)        :: calendar = 'gregorian'
end type date_type


interface operator (<)
  module procedure before
end interface

interface operator (>)
  module procedure after
end interface

interface operator (==)
  module procedure is
end interface

INTEGER, DIMENSION(12)     :: LenMonth
INTEGER, DIMENSION(12)     :: LenMonthLeap
INTEGER, DIMENSION(12)     :: CumMonth
INTEGER, DIMENSION(12)     :: CumMonthLeap

DATA LenMonth /31,28,31,30,31,30,31,31,30,31,30,31/
DATA LenMonthLeap /31,29,31,30,31,30,31,31,30,31,30,31/
DATA CumMonth /0,31,59,90,120,151,181,212,243,273,304,334/
DATA CumMonthLeap /0,31,60,91,121,152,182,213,244,274,305,335/

contains
! ...
! ============================================================================
! ...
logical pure function isleap (year)

INTEGER, INTENT(in)             :: year

isleap = .false.
if (MOD(year,400).eq.0) isleap = .true.
if ((MOD(year,4).eq.0).and.(mod(year,100).ne.0)) isleap = .true.

return
end function isleap
! ...
! ============================================================================
! ...
function date_string(date,iso,extended) result(text)
! ... Write the date as a string "YYYY-MM-DD HH:MM:SS"

type(date_type), intent(in)             :: date
character(len=20)                       :: text
character(len=*), optional              :: iso
character(len=*), optional              :: extended

! ... Local variables:
! ...
if (present(iso)) then
  if (present(extended)) then
  write(text,'(T1,I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")') &
      date%year, date%month, date%day, &
      date%hour, date%minute, date%second
  else
  write(text,'(T1,I4.4,I2.2,I2.2,"T",I2.2,I2.2,I2.2)') &
      date%year, date%month, date%day, &
      date%hour, date%minute, date%second
  endif
else
  write(text,'(T1,I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
      date%year, date%month, date%day, &
      date%hour, date%minute, date%second
endif

end function date_string
! ...
! ============================================================================
! ...
function string2date(sdate) result(date)

character(len=*), intent(in)            :: sdate
type(date_type)                         :: date

! ... Local variables
! ...
integer ns
character(len=20) ldate

ldate = trim(sdate)
ldate = line_replace(ldate,' ','')
ldate = line_replace(ldate,'-','')
ldate = line_replace(ldate,':','')

ns = len_trim(ldate)
if (ns.lt.8) call stop_error(1,'Too short date. Use YYYYMMDD[THHMMDD]')

if (ns.eq.8) then
  ldate = trim(ldate)//'T000000'
else if (ns.ne.15) then
  call stop_error(1,'Invalid date. Use YYYYMMDD[THHMMDD]')
endif

read(ldate(1:4),*)   date%year 
read(ldate(5:6),*)   date%month
read(ldate(7:8),*)   date%day
read(ldate(10:11),*) date%hour
read(ldate(12:13),*) date%minute
read(ldate(14:15),*) date%second

end function string2date
! ...
! ============================================================================
! ...
function cal2date(year,month,day,hour,minute,second) result(date)
! ... Builds the date structure

integer, intent(in)                     :: year
integer, intent(in)                     :: month
integer, intent(in)                     :: day
integer, intent(in), optional           :: hour
integer, intent(in), optional           :: minute
integer, intent(in), optional           :: second
type(date_type)                         :: date

date%year = year
date%month = month
date%day = day
if (present(hour)) then
  date%hour = hour
else
  date%hour = 0
endif
if (present(minute)) then
  date%minute = minute
else
  date%minute = 0
endif
if (present(second)) then
  date%second = second
else
  date%second = 0
endif

end function cal2date
! ...
! ============================================================================
! ...
function cal2jd(year,month,day,hour,minute,second) result(jd)
! ... Julian day for a gregorian calendar day: Year-Month-Day

integer, intent(in)                     :: year
integer, intent(in)                     :: month
integer, intent(in)                     :: day
integer, intent(in), optional           :: hour
integer, intent(in), optional           :: minute
integer, intent(in), optional           :: second
real(dp)                                :: jd

! ... Local variables
! ...
real(dp) seconds,hh,mm,ss

jd = julday(year,month,day)

if (present(hour)) then
  hh = hour
else
  hh = 0.0_dp
endif
if (present(minute)) then
  mm = minute
else
  mm = 0.0_dp
endif
if (present(second)) then
  ss = second
else
  ss = 0.0_dp
endif
seconds = hh*3600.0_dp + mm*60.0_dp + ss

jd = jd + seconds/86400.0_dp

end function cal2jd
! ...
! ============================================================================
! ...
subroutine jd2cal(jd,year,month,day,hour,minute,second)
! ... Gregorian calendar day: Year-Month-Day for a Julian day

real(dp), intent(in)                    :: jd
integer, intent(out)                    :: year,month,day
integer, intent(out)                    :: hour,minute,second

! ... Local variables:
! ...
integer ijday,isecs

ijday = INT(jd)
isecs = NINT((jd - ijday)*86400)

call caldat(ijday,year,month,day)

second = mod(isecs,60)
isecs = (isecs-second)/60
minute = mod(isecs,60)
hour = (isecs-minute)/60

end subroutine jd2cal
! ...
! ============================================================================
! ...
function date2jd(date) result(jd)

type(date_type), intent(in)             :: date
real(dp)                                :: jd

! ... Local variables
! ...
real(dp) seconds

seconds = date%hour*3600.0_dp + date%minute*60.0_dp + date%second

if ((date%calendar.eq.'gregorian').or. &
    (date%calendar.eq.'standard').or. &
    (len_trim(date%calendar).eq.0)) then
  jd = julday(date%year,date%month,date%day)
else if ((date%calendar.eq.'365_day').or. &
    (date%calendar.eq.'365d')) then
  jd = 365*(date%year-1) + CumMonth(date%month) + (date%day-1)
else if ((date%calendar.eq.'360_day').or. &
    (date%calendar.eq.'360d')) then
  jd = 360*(date%year-1) + 30*(date%month-1) + (date%day-1)
else
  call stop_error(1,'Unknown calendar')
endif

jd = jd + seconds/86400.0_dp

end function date2jd
! ...
! ============================================================================
! ...
function jd2date(jd,cal) result(date)

real(dp), intent(in)                    :: jd
type(date_type)                         :: date
character(len=*), optional              :: cal

! ... Local variables:
! ...
integer ijday,isecs,ijmon,mm
character(len=20) calendar

ijday = INT(jd)
isecs = NINT((jd - ijday)*86400)

if (.not.present(cal)) then
  calendar = 'gregorian'
else
  calendar = trim(cal)
endif

if ((calendar.eq.'gregorian').or. &
    (calendar.eq.'standard').or. &
    (len_trim(calendar).eq.0)) then
  call caldat(ijday,date%year,date%month,date%day)
else if ((calendar.eq.'365_day').or. &
    (calendar.eq.'365d')) then
  ijmon = mod(ijday,365)
  do mm=1,12
    if (CumMonth(mm).gt.ijmon) exit
  enddo
  date%year = (ijday-ijmon)/365 + 1
  date%month = mm - 1
  date%day = ijmon - CumMonth(date%month) + 1
else if ((calendar.eq.'360_day').or. &
    (calendar.eq.'360d')) then
  ijmon = mod(ijday,360)
  date%year = (ijday-ijmon)/360 + 1
  date%day = mod(ijmon,30) + 1
  date%month = (ijmon - date%day+1)/30 + 1
else
  call stop_error(1,'Unknown calendar')
endif


date%second = mod(isecs,60)
isecs       = (isecs-date%second)/60
date%minute = mod(isecs,60)
date%hour   = (isecs-date%minute)/60
date%calendar = trim(calendar) 

end function jd2date
! ...
! ============================================================================
! ...
function julday(year,month,day) result(jd)

integer, intent(in)                     :: year
integer, intent(in)                     :: month
integer, intent(in)                     :: day
integer                                 :: jd

! ... Local variables 
! ...
integer, parameter                      :: IGREG=15+31*(10+12*1582)
integer ja,jm,jy

jy = year
if (jy.eq.0) stop  'julday: there is no year zero'
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

end function julday
! ...
! ============================================================================
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
! ============================================================================
! ...
subroutine sec2time (isecs,hour,minute,second)

integer, intent(in)                      :: isecs
integer, intent(out)                     :: hour,minute,second

! ... Local variables
! ...
integer irest

second = mod(isecs,60)
irest = (isecs-second)/60
minute = mod(irest,60)
hour = (irest-minute)/60

end subroutine sec2time
! ...
! ============================================================================
! ...
function date_today(utm) result(date)

character(len=*), optional              :: utm
type(date_type)                         :: date

! ... Local variables
! ...
integer hh,mm,ss,seconds
real(dp) jd
character(5)           :: zone
integer,dimension(8)   :: values

call date_and_time(VALUES=values)

if (present(utm)) then
  jd = julday(values(1),values(2),values(3))
  hh = values(5)
  mm = values(6)
  ss = values(7)
  seconds = hh*3600.0_dp + (mm-values(4))*60.0_dp + ss
  jd = jd + seconds/86400.0_dp
  date = jd2date(jd)
else
  date%year   = values(1)
  date%month  = values(2)
  date%day    = values(3)
  date%hour   = values(5)
  date%minute = values(6)
  date%second = values(7)
endif

end function date_today
! ...
! ============================================================================
! ...
function before(d1,d2) result(q)

type(date_type), intent(in)             :: d1,d2
logical                                 :: q

real(dp) j1,j2

j1 = date2jd(d1)
j2 = date2jd(d2)

q = j1 .lt. j2

end function before
! ...
! ============================================================================
! ...
function after(d1,d2) result(q)

type(date_type), intent(in)             :: d1,d2
logical                                 :: q

real(dp) j1,j2

j1 = date2jd(d1)
j2 = date2jd(d2)

q = j1 .gt. j2

end function after
! ...
! ============================================================================
! ...
function is(d1,d2) result(q)

type(date_type), intent(in)             :: d1,d2
logical                                 :: q

real(dp) j1,j2

j1 = date2jd(d1)
j2 = date2jd(d2)

q = j1 .eq. j2

end function is
! ...
! ============================================================================
! ...
function date_inc(datein,val,units) result(dateout)

type(date_type), intent(in)             :: datein
integer, intent(in)                     :: val
character(len=*), intent(in), optional  :: units
type(date_type)                         :: dateout

! ... Local variables
! ...
integer year,month,day,hour,minute,second
real(dp) jd,scale_value
character(2) U

if (present(units)) then
  U = uppercase(units(1:2))
  if (U.eq.'SE') then
    scale_value = one
  else if (U.eq.'MI') then
    scale_value = 60.0_dp
  else if (U.eq.'HO') then
    scale_value = 3600.0_dp
  else
    scale_value = -one
  endif
else
  U = 'SE'
  scale_value = one
endif

if (U.EQ.'DA') then
  jd = date2jd(datein) + val
  dateout = jd2date(jd)
else if (scale_value.gt.zero) then
  jd = date2jd(datein) + scale_value*val/86400.0_dp
  dateout = jd2date(jd)
else
  year   = datein%year
  month  = datein%month
  day    = datein%day
  hour   = datein%hour
  minute = datein%minute
  second = datein%second
  if (U.eq.'MO') then
    if (month.eq.12) then
      month = 1
      year = year + 1
    else
      month = month + 1
    endif
  else if (U.EQ.'YE') then
    year = year + 1
  else
    call stop_error(1,'In date_inc: Why am I here?')
  endif
  dateout = cal2date(year,month,day,hour,minute,second)
endif

jd = date2jd(dateout)
dateout = jd2date(jd)

end function date_inc
! ...
! ============================================================================
! ...
end module dates
