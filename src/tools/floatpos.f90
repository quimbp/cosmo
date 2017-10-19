! *********************************************************************
! ... floatloc.f90
! ... Quim Ballabrera, October 2017
! ... COSMO project
! ... Tool to print the spatial coordinates of the floats generated
! ... by the lagrangian model.
! ... Version 0.1, released October 2017
! *********************************************************************

use cosmo
use netcdf

implicit none

character(len=*), parameter             :: version = 'v0.0'
character(len=*), parameter             :: author  = 'Quim Ballabrera'

! ... I/O flags and options
! ...
logical                                 :: inp = .false.
logical                                 :: hlp = .false.
logical                                 :: fst = .false.
logical                                 :: fsd = .false.
logical                                 :: fnt = .false.
logical                                 :: fxo = .false.
logical                                 :: fyo = .false.
logical                                 :: fdx = .false.
logical                                 :: fdy = .false.
logical                                 :: frn = .false.

! ...
type(date_type) dateo
integer na,nf,ff,err,step,i,record
integer, dimension(:), allocatable            :: fid,vid,np,nt,xid,yid
integer, dimension(:), allocatable            :: did
real(dp) xo,yo,dx,dy,otime,dxo,dyo
real(dp), dimension(:), allocatable           :: xf,yf,ftime
character(len=20) sdate,vname,dname
character(len=19) odate,fdate
character(len=180), dimension(:), allocatable :: fname
character(len=1800) ilist

! ... The help
! ...
call help_version(version)
call help_progname('FLOATLOC')
call help_author(author)
call help_summary('Reads one or various trajectory files and displays &
 &the location of the floaters at a given time or the time at which a &
 &the float passes in the vicinity of a given location.')
call help_option('-inp            FILENAME(s)','Input trajectory file(s)','')
call help_option('-system_time    TIME','Selected system time','')
call help_option('-system_date    DATE','Selected system date','')
call help_option('-nav_time       TIME','Selected navegation time','')
call help_option('-record         RECORD','Selected record','')
call help_option('-x              LON','Selected longitude of interest','')
call help_option('-y              LAT','Selected latitude of interest','')
call help_option('-dx             dLON','Half longitude range','0.1')
call help_option('-dy             dLAT','Half latitude range','0.1')
call help_option ('--options  filename','To read the commandline options from a file.','')
call help_option ('--help','To show this help','')

! ... Default options
! ...
sdate = ''
dxo   = 0.1_dp
dyo   = 0.1_dp

! ... Lineargs
! ...
call lineargs_ini(na)
if (na.eq.0) call help_write()

call argflg('--h',hlp)
call argflg('-help',hlp)
if (hlp) then
  call help_write()
else
  call header()
endif

call arglst('-inp',inp,ilist)
call argstr('-system_d',fsd,sdate)
call argdbl('-system_t',fst,otime)
call argdbl('-nav_t',fnt,otime)
call argdbl('-x',fxo,xo)
call argdbl('-y',fyo,yo)
call argdbl('-dx',fdx,dxo)
call argdbl('-dy',fdy,dyo)
call argint('-re',frn,record)

if (.not.inp) call arglast(ilist)
call checkopts()

if (count((/fxo,fyo/)).eq.1) then
  call stop_error(1,'Both options -x and -y required')
endif

! ... Check options compatibility
! ...
dname = 'system_date'

if (fsd) then
  if (any((/fst,fxo,fyo,fnt,frn/))) call stop_error(1,'Incompatible option with -system_date')
  dateo = string2date(sdate)
  odate = date_string(dateo,'iso')
  vname = 'system_date'
endif

if (fst) then
  if (any((/fsd,fxo,fyo,fnt,frn/))) call stop_error(1,'Incompatible option with -system_time')
  vname = 'system_time'
endif
  
if (fnt) then
  if (any((/fsd,fxo,fyo,fst,frn/))) call stop_error(1,'Incompatible option with -nav_time')
  vname = 'nav_time'
endif

if (frn) then
  if (any((/fsd,fxo,fyo,fst,fnt/))) call stop_error(1,'Incompatible option with -record')
  vname = 'system_date'
endif
  
if (fxo) then
  if (any((/fsd,fnt,fst,frn/))) call stop_error(1,'Incompatible option with -x/-y')
  vname = 'system_date'
endif

write(*,*) 
write(*,*) 'Requesting variable : ', trim(vname)
write(*,*) 

! ... Opening input files
! ...
nf = numwords(ilist)

write(*,*) 'Opening ', nf, ' files:'
write(*,*) 

allocate (fname(nf))
allocate (fid(nf))
allocate (vid(nf))
allocate (did(nf))
allocate (xid(nf))
allocate (yid(nf))
allocate (np(nf))
allocate (nt(nf))

do ff=1,nf
  call line_word(ilist,ff,fname(ff))
  write(*,*) 'Opening file: ', trim(fname(ff)),' as trajectory number ', ff
  err = nf90_open(fname(ff),NF90_NOWRITE,fid(ff))
  call cdf_error(err,'Unable to open file')
enddo


! ... Get the relevant information
! ...
write(*,*)
do ff=1,nf
  write(*,*) 'File: ', trim(fname(ff))
  err = nf90_inquire_dimension(fid(ff),1,len=np(ff))
  call cdf_error(err,'Unable to get number of floats')
  err = nf90_inquire_dimension(fid(ff),3,len=nt(ff))
  call cdf_error(err,'Unable to get number of time steps')
  err = nf90_inq_varid(fid(ff),trim(vname),vid(ff))
  call cdf_error(err,'Unable to get variable id')
  err = nf90_inq_varid(fid(ff),'lon',xid(ff))
  call cdf_error(err,'Unable to get lon id')
  err = nf90_inq_varid(fid(ff),'lat',yid(ff))
  call cdf_error(err,'Unable to get lat id')
  print*, 'floats, nt  = ', np(ff), nt(ff)
  print*, 'Variable id = ', vid(ff)
  ! ... System date:
  ! ...
  err = nf90_inq_varid(fid(ff),trim(dname),did(ff))
  call cdf_error(err,'Unable to get system_date id')
enddo



! ... Get the variable and compare:
! ...
if (fsd) then
  write(*,*)
  write(*,*) 'System date : ', trim(odate)
  write(*,*) '============================='
  do ff=1,nf
    allocate (xf(np(nf)))
    allocate (yf(np(nf)))
    write(*,*)
    write(*,*) 'Trajectory : ', ff

    do step=1,nt(ff)
      err = nf90_get_var(fid(ff),vid(ff),fdate,(/1,step/),(/15,1/))
      call cdf_error(err,'Unable to read system_date')
      if (fdate.eq.odate) then
        err = nf90_get_var(fid(ff),xid(ff),xf,(/1,step/),(/np(ff),1/))
        call cdf_error(err,'Unable to read lon')
        err = nf90_get_var(fid(ff),yid(ff),yf,(/1,step/),(/np(ff),1/))
        call cdf_error(err,'Unable to read lon')
        write(*,'("         Step ",I5," : ",100(2F9.3,";"))') step, &
                                                     (xf(i),yf(i),i=1,np(ff))
      endif
    enddo

    deallocate (xf) 
    deallocate (yf) 
  enddo
endif

if (fst) then
  write(*,*)
  write(*,*) 'System time : ', otime
  write(*,*) '==================================='
  do ff=1,nf
    allocate (xf(np(nf)))
    allocate (yf(np(nf)))
    allocate (ftime(1))
    write(*,*)
    write(*,*) 'Trajectory : ', ff

    do step=1,nt(ff)
      err = nf90_get_var(fid(ff),vid(ff),ftime,(/step/),(/1/))
      call cdf_error(err,'Unable to read system_time')
      if (ftime(1).eq.otime) then
        err = nf90_get_var(fid(ff),xid(ff),xf,(/1,step/),(/np(ff),1/))
        call cdf_error(err,'Unable to read lon')
        err = nf90_get_var(fid(ff),yid(ff),yf,(/1,step/),(/np(ff),1/))
        call cdf_error(err,'Unable to read lon')
        write(*,'("         Step ",I5," : ",100(2F9.3,";"))') step, &
                                                     (xf(i),yf(i),i=1,np(ff))
      endif
    enddo

    deallocate (xf) 
    deallocate (yf) 
    deallocate (ftime) 
  enddo
endif

if (fnt) then
  write(*,*)
  write(*,*) 'Navigation time : ', otime
  write(*,*) '======================================='
  do ff=1,nf
    allocate (xf(1))
    allocate (yf(1))
    allocate (ftime(np(nf)))
    write(*,*)
    write(*,*) 'Trajectory : ', ff

    do step=1,nt(ff)
      err = nf90_get_var(fid(ff),vid(ff),ftime,(/1,step/),(/np(ff),1/))
      call cdf_error(err,'Unable to read nav_time')
      err = nf90_get_var(fid(ff),did(ff),fdate,(/1,step/),(/15,1/))
      call cdf_error(err,'Unable to read system_date')
      do i=1,np(ff)
        if (ftime(i).eq.otime) then
        err = nf90_get_var(fid(ff),xid(ff),xf,(/i,step/),(/1,1/))
        call cdf_error(err,'Unable to read lon')
        err = nf90_get_var(fid(ff),yid(ff),yf,(/i,step/),(/1,1/))
        call cdf_error(err,'Unable to read lon')
        write(*,'("      Float ", I4.4, ", Date ",A," : ",100(2F9.3,";"))') &
                                                    i,trim(fdate), &
                                                    xf(1),yf(1)
        endif
      enddo
    enddo

    deallocate (xf) 
    deallocate (yf) 
    deallocate (ftime) 
  enddo
endif


if (frn) then
  write(*,*)
  write(*,*) 'Record : ', record
  write(*,*) '============================='
  do ff=1,nf
    allocate (xf(np(nf)))
    allocate (yf(np(nf)))
    write(*,*)
    write(*,*) 'Trajectory : ', ff

    step = min(record,nt(ff))

    err = nf90_get_var(fid(ff),vid(ff),fdate,(/1,step/),(/15,1/))
    call cdf_error(err,'Unable to read system_date')
      err = nf90_get_var(fid(ff),xid(ff),xf,(/1,step/),(/np(ff),1/))
      call cdf_error(err,'Unable to read lon')
      err = nf90_get_var(fid(ff),yid(ff),yf,(/1,step/),(/np(ff),1/))
      call cdf_error(err,'Unable to read lon')
      write(*,'("         Date ",A," : ",100(2F9.3,";"))') trim(fdate), &
                                                  (xf(i),yf(i),i=1,np(ff))

    deallocate (xf) 
    deallocate (yf) 
  enddo
endif

if (fxo) then
  write(*,*)
  write(*,*) 'Xo,Yo : ', xo, yo
  write(*,*) 'dX,dY : ', dxo, dyo
  write(*,*) '======================================================='
  do ff=1,nf
    allocate (xf(1))
    allocate (yf(1))
    allocate (ftime(np(nf)))
    write(*,*)
    write(*,*) 'Trajectory : ', ff

    do step=1,nt(ff)
      if (np(ff).gt.1) write(*,*)
      err = nf90_get_var(fid(ff),did(ff),fdate,(/1,step/),(/15,1/))
      call cdf_error(err,'Unable to read system_date')

      do i=1,np(ff)
        err = nf90_get_var(fid(ff),xid(ff),xf,(/i,step/),(/1,1/))
        call cdf_error(err,'Unable to read lon')
        err = nf90_get_var(fid(ff),yid(ff),yf,(/i,step/),(/1,1/))
        call cdf_error(err,'Unable to read lon')
        dx = abs(xo - xf(1))
        dy = abs(yo - yf(1))
        if (dx.le.dxo.and.dy.le.dyo) then
        write(*,'("      Step ", I5.5, "  Float ", I4.4, ",  Date ",    &
                                              A," : ",100(2F9.3,";"))') &
                                                    step,i,trim(fdate), &
                                                    xf(1),yf(1)
        endif
      enddo
    enddo

    deallocate (xf) 
    deallocate (yf) 
    deallocate (ftime) 
  enddo
endif



call stop_error(0,'Ok')
end


