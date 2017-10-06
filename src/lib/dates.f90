! *****************************************************************************
! ... date.f90
! ... Routines to manipulate date and time variables
! ... COSMO project
! ... Quim Ballabrera, March 2017
! *****************************************************************************

module dates

use types, only: dp
use utils, only: stop_error,line_replace

implicit none
private
public date_type
public date_string,string2date,cal2date,cal2jd,jd2cal, &
       jd2date,date2jd,caldat,julday

type date_type
  integer                  :: year   = 0
  integer                  :: month  = 0
  integer                  :: day    = 0
  integer                  :: hour   = 0
  integer                  :: minute = 0
  integer                  :: second = 0
  character(len=10)        :: calendar = 'gregorian'
end type date_type

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
function date_string(date,iso) result(text)
! ... Write the date as a string "YYYY-MM-DD HH:MM:SS"

type(date_type), intent(in)             :: date
character(len=19)                       :: text
character(len=*), optional              :: iso

! ... Local variables:
! ...
if (present(iso)) then
  write(text,'(T1,I4.4,I2.2,I2.2,"T",I2.2,I2.2,I2.2)') &
      date%year, date%month, date%day, &
      date%hour, date%minute, date%second
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
  date%year = (jd-ijmon)/365 + 1
  date%month = mm - 1
  date%day = ijmon - CumMonth(date%month) + 1
else if ((calendar.eq.'360_day').or. &
    (calendar.eq.'360d')) then
  ijmon = mod(ijday,360)
  date%year = (jd-ijmon)/360 + 1
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
end module dates
