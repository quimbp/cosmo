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
integer                                 :: i,j
integer                                 :: step,pou,pov,pau,pav
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
call forcing_ini(west,south,east,north,tmin,tmax)

maxdt = minval([GOU%dt,GOV%dt])
if (userRKdt.gt.maxdt) call stop_error(1,'Time step too large')


!do j=GAU%nj,1,-1
!  write(*,'(120I1)') GAU%var%mask(:,j,1)
!enddo


!do j=1,100
!  i = GOU%point_type(deg2rad*(-4.487_dp),deg2rad*(35.19_dp+(j-1)*0.02_dp))
!  print*, -4.487_dp,35.19_dp+(j-1)*0.02_dp, 'point_type = ', i
!enddo

! ... Check that we can do a cubic time interpolation.
! ... It requires, at least, that the simulation time (0.0) starts at the
! ... second time step.
! ...
!if (GOU%t(1).ge.0.0_dp) call stop_error(1,'ERROR: OCE U not enough time steps for cubic interpolation')
!if (GOV%t(1).ge.0.0_dp) call stop_error(1,'ERROR: OCE V not enough time steps for cubic interpolation')
!if (withAtmx) then
!  if (GAU%t(1).ge.0.0_dp) call stop_error(1,'ERROR: ATM U not enough time steps for cubic interpolation')
!  if (GAV%t(1).ge.0.0_dp) call stop_error(1,'ERROR: ATM V not enough time steps for cubic interpolation')
!endif

write(*,*) 'Simulation period: '
datemin = jd2date(tmin)
datemax = jd2date(tmax)

pou = locate(GOU%t,0.0_dp)
pov = locate(GOV%t,0.0_dp)

write(*,*) 'Initial time : ', tmin, datemin%iso()
write(*,*) 'Final time   : ', tmax, datemax%iso()
write(*,*) 'Ocean zonal velocity record pointer: ', pou
write(*,*) 'Ocean meridional velocity record pointer: ', pou
if (withAtmx) then
  pau = locate(GAU%t,0.0_dp+userRKdt)
  pav = locate(GAV%t,0.0_dp+userRKdt)
  write(*,*) 'Wind zonal velocity record pointer: ', pau
  write(*,*) 'Wind meridional velocity record pointer: ', pau
endif

if (pou.lt.2) call stop_error(1,'ERROR: OCE U not enough time steps for cubic interpolation')
if (pov.lt.2) call stop_error(1,'ERROR: OCE V not enough time steps for cubic interpolation')
if (withAtmx) then
  if (pau.lt.2) call stop_error(1,'ERROR: ATM U not enough time steps for cubic interpolation')
  if (pav.lt.2) call stop_error(1,'ERROR: ATM V not enough time steps for cubic interpolation')
endif

write(*,*)
write(*,'(T2,A,4F9.3)') 'Domain limits W,S,E,N: ', rad2deg*[west,south,east,north]
model_jdref = tmin
call float_ini(model_jdref)
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
