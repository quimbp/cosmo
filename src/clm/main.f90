! ********************************************************************
! ... main.f90
! ... COSMO Lagrangian Model
! ... Joaquim Ballabrera, February 2021
! ... Main program
! ... Version 1.0 - Initial version
! ...
! ********************************************************************

program main

use cosmo, only: COSMO_VERSION, COSMO_AUTHOR
use module_datetime

use module_constants
use module_options
use module_forcing
use module_model
use module_float
use module_out

implicit none

! ... Local variables
! ...
integer                                 :: pou,pov,pau,pav
real(dp)                                :: west,east,south,north
real(dp)                                :: tmin,tmax,maxdt
type(type_date)                         :: datemin,datemax

write(*,*) 
write(*,*) '=========================================================='
write(*,*) 'COSMO LAGRANGIAN MODEL'
write(*,*) 'COSMO VERSION: ', trim(COSMO_VERSION)
write(*,*) 'by: ', trim(COSMO_AUTHOR)
write(*,*) 'Date: ', trim(COSMO_DATE)
write(*,*) '=========================================================='
write(*,*) 

call read_options()
if (verb) call view_options(6)

if (.not.withOcex) call stop_error(1,'ERROR: The zonal meridional file must be indicated')

! ... Forcing initialization and obtain common grid
! ... Coordintes in radians or meters
! ...
call forcing_ini(west,south,east,north)
call forcing_time(tmin,tmax)

! ... Now, take the zonal ocean current time units and calendar
! ... as the time units and calendar of the CLM
! ...
clm_calendar = GOU%calendar

out_units      = GOU%time_units       ! For output
model_units    = trim(clm_units)      ! For output
model_calendar = trim(clm_calendar)   ! For output

Reference_time = tmin
Reference_date = num2date(tmin,units=clm_units,calendar=clm_calendar)

maxdt = minval([GOU%dt,GOV%dt])
if (userRKdt.gt.maxdt) call stop_error(1,'Time step too large')


!do j=GAU%nj,1,-1
!  write(*,'(120I1)') GAU%var%mask(:,j,1)
!enddo


!do j=1,100
!  i = GOU%point_type(deg2rad*(-4.487_dp),deg2rad*(35.19_dp+(j-1)*0.02_dp))
!  print*, -4.487_dp,35.19_dp+(j-1)*0.02_dp, 'point_type = ', i
!enddo

write(*,*) 'Simulation period: '
datemin = num2date(tmin,units=clm_units,calendar=GOU%calendar)
datemax = num2date(tmax,units=clm_units,calendar=GOU%calendar)
!datemin = jd2date(tmin)
!datemax = jd2date(tmax)


write(*,*) 'Initial time : ', tmin, datemin%iso()
write(*,*) 'Final time   : ', tmax, datemax%iso()
write(*,*) 'Ocean zonal velocity record pointer: ', pou
write(*,*) 'Ocean meridional velocity record pointer: ', pou

! ... Check that we can do a cubic time interpolation.
! ... It requires, at least, that the simulation time (0.0) starts at the
! ... second time step.
! ...
pou = locate(GOU%s,0.0_dp)
pov = locate(GOV%s,0.0_dp)
!pou = locate(GOU%s,tmin)
!pov = locate(GOV%s,tmin)
write(*,*) 'Ocean zonal velocity record pointer: ', pou
write(*,*) 'Ocean meridional velocity record pointer: ', pov
if (withAtmx) then
  !pau = locate(GAU%s,tmin)
  !pav = locate(GAV%s,tmin)
  pau = locate(GAU%s,0.0_dp+userRKdt)
  pav = locate(GAV%s,0.0_dp+userRKdt)
  write(*,*) 'Wind zonal velocity record pointer: ', pau
  write(*,*) 'Wind meridional velocity record pointer: ', pav
endif

if (pou.lt.2) call stop_error(1,'ERROR: OCE U not enough time steps for cubic interpolation')
if (pov.lt.2) call stop_error(1,'ERROR: OCE V not enough time steps for cubic interpolation')
if (withAtmx) then
  if (pau.lt.2) call stop_error(1,'ERROR: ATM U not enough time steps for cubic interpolation')
  if (pav.lt.2) call stop_error(1,'ERROR: ATM V not enough time steps for cubic interpolation')
endif

write(*,*)
write(*,'(T2,A,4F9.3)') 'Domain limits W,S,E,N: ', rad2deg*[west,south,east,north]

call float_ini(Reference_time,clm_units,clm_calendar)
if (FLT%Nfloats.le.0) call stop_error(0,'No floats !')

! ... Select the model layers used for advection
! ...
call forcing_layers(FLT%z,FLT%k)

if (verb) then
  write(*,*) 'Float release depth     : ', FLT%z
  write(*,*) 'Advected by model layer : ', LAYER(FLT%k)
endif

call model_ini(west,south,east,north,tmin,tmax)
write(*,*) 'Number time steps: ', model_Nstep
write(*,*) 'Time step (s): ', rk_dt
!write(*,*) 'Integration time (s and days): ', model_time(model_Nstep), &
!                                           model_time(model_Nstep)/86400.0_dp

input_id = GOU%fid
input_timeid = GOU%idt
call trajectory_open(Oname,FLT%Nfloats,FLT%missing)

call release_write()
call param_write(wsf,A11,A12,A21,A22,noise_mul,noise_add)

call model_run()

call exitcode_write()
call trajectory_close()

if (verb) then
  write(*,*) 
  write(*,*) '      Input release file: ', trim(releaseName)
  write(*,*) '  Output trajectory file: ', trim(Oname)
  write(*,*) 'Output end position file: ', trim(FinalName)
endif

call stop_error(0,'CLM: Normal termination')
end program main
