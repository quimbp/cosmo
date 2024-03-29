module module_model

use module_types, only: dp
use module_grid
use module_math, only: haversine

use module_options
use module_forcing
use module_float
use module_out

implicit none

real(dp)                               :: model_west  = 0.0_dp
real(dp)                               :: model_east  = 0.0_dp
real(dp)                               :: model_south = 0.0_dp
real(dp)                               :: model_north = 0.0_dp

! ... Runge-Kutta Time steps:
! ... rk_time, the time at the beginning of the RK integration (e.g. 0, 3600, etc.)
! ... rk_dt, the time step used for the RK integration (e.g. 120 seconds)
! ...
real(dp)                               :: rk_t
real(dp)                               :: rk_dt

! ... Forcing terms:
! ... Right Hand Side fields interpolated in time to the 
! ... 5th-order Runge-Kutta function evaluations, i.e.
! ...
! ...                 rk_t 
! ...                 rk_t +   rk_dt/4
! ...                 rk_t +   rk_dt/2
! ...                 rk_t + 3*rk_dt/4
! ...                 rk_t +   rk_dt
! ...
integer                                :: Hnx,Hny      ! Grid size common hor grid
integer                                :: Unz,Vnz,Wnz  ! Vertical levels of ocean
real(dp)                               :: Hdx,Hdy      ! Horizontal regular grid spacing (rad)
real(dp), dimension(:,:), pointer      :: Hx,Hy        ! Horizontal grid (rad)
real(dp), dimension(:,:), pointer      :: Hm,Hn        ! Horizontal grid metrics (common), 1/dx, 1/dy
real(dp), dimension(:), pointer        :: Uz,Vz,Wz     ! Vertical grids

integer                                :: model_Nstep
real(dp), dimension(:), pointer        :: model_time   ! Model time axis (s)

real(dp), dimension(:,:,:,:), pointer  :: Ourhs        ! (Hnx,Hny,Unz,5)
real(dp), dimension(:,:,:,:), pointer  :: Ovrhs        ! (Hnx,Hny,Vnz,5)
real(dp), dimension(:,:,:,:), pointer  :: Owrhs        ! (Hnx,Hny,Wnz,5)
real(dp), dimension(:,:,:), pointer    :: Aurhs        ! (Hnx,Hny,5)
real(dp), dimension(:,:,:), pointer    :: Avrhs        ! (Hnx,Hny,5)

! ... Response Matrix, accounting for direct atmosphere
! ... drag and Stokes drift.
! ... Pereiro et al. (2019).
! ... The atmospheric term only applies near the surface, thus
! ... the use of the surface flag.
! ...
logical                                :: surface = .True.
real(dp)                               :: A11     = 0.0_dp
real(dp)                               :: A12     = 0.0_dp
real(dp)                               :: A21     = 0.0_dp
real(dp)                               :: A22     = 0.0_dp

! ... Advection model layer
! ...
integer                                :: ADVECTION_LAYER = 1

! ... Water speed fraction (alpha)
! ... Unrealistic term to perform experiments such as when the 
! ... drifter is driven only by the wind (alpha = 0.0).
! ...
real(dp)                               :: alpha   = 1.0_dp  ! Not physical

! ... Time calendar
! ... The model will assume the time units and calendar of the zonal ocean
! ... current's file.
character(len=180)                     :: clm_units = 'seconds since 1970-01-01 00:00:00'
character(len=20)                      :: clm_calendar = ''
real(dp)                               :: Reference_time
type(type_date)                        :: Reference_date

! ... Middle of the month day (for climatological forcing)
! ...
real(dp), dimension(12) :: HalfMonth = [16.5D0,15.0D0,16.5D0,16.0D0,16.5D0,16.0D0, &
                                        16.5D0,16.5D0,16.0D0,16.5D0,16.0D0,16.5D0]

! ... Random Walk subgrid parameterization
! ...
logical                                :: Gaussian_noise
real(dp)                               :: noise_mu
real(dp)                               :: noise_K1
real(dp)                               :: noise_K0

contains
! ...
! ====================================================================
! ====================================================================
! ...
subroutine RHS2D(n,t,x,dxdt)

integer, intent(in)                    :: n
real(dp), intent(in)                   :: t
real(dp), dimension(n), intent(inout)  :: x
real(dp), dimension(n), intent(out)    :: dxdt

! ... Local variables
! ...
real(dp), parameter                    :: RNstdev = sqrt(12.0D0)
integer ll
real(dp) o1,o2,a1,a2
real(dp) n1,n2
real(dp) Unoise(2)


ll = nint(4.0_dp*(t-rk_t)/rk_dt) + 1

! ... Interpolation at the float location
! ...
! ...  dxdt = alpha*u + A * w
! ...

!x = [0.2394422D0,   0.7959301D0]

if (alpha.eq.0) then   ! We do not interpolate unnecessarily
  o1 = 0.0D0
  o2 = 0.0D0
else
  o1 = GOU%hinterpol(Ourhs(:,:,ADVECTION_LAYER,ll),x(1),x(2))  ! Only one layer
  o2 = GOV%hinterpol(Ovrhs(:,:,ADVECTION_LAYER,ll),x(1),x(2))  ! Only one layer
endif

if (withAtmx.and.surface) then
  a1     = GAU%hinterpol(Aurhs(:,:,ll),x(1),x(2))
  a2     = GAV%hinterpol(Avrhs(:,:,ll),x(1),x(2))
  dxdt(1) = alpha*o1  +  A11*a1 + A12*a2
  dxdt(2) = alpha*o2  +  A21*a1 + A22*a2
else
  dxdt(1) = alpha*o1
  dxdt(2) = alpha*o2
endif

! ... Add the First order Markov model for the subgrid-scale processes
! ... By multiplying the uniform distribution by sqrt(12), we ensure that
! ... its standard deviation is r = 1.
! ... The value of 
! ...                             noise_K1 = sqrt(2*K1/dt)
! ... has been set in model_ini
! ...
if (withK1) then
  if (Gaussian_noise) then
    Unoise(:) = randn(n)                                  ! Gaussian noise
  else
    call random_number(Unoise)                            ! Uniform noise
    Unoise(:) = (Unoise(:)-0.5D0)*RNStdev
  endif
  dxdt(:) = dxdt(:) + noise_K1*Unoise(:)
endif

end subroutine RHS2D
! ...
! ====================================================================
! ...
subroutine model_ini(west,south,east,north,tmin,tmax)


real(dp), intent(in)                  :: east,south,west,north   ! Radians
real(dp), intent(in)                  :: tmin,tmax               ! Jday

! ... Local variables
! ...
integer Nsteps
real(dp), parameter                   :: Qvar = sqrt(12.0D0)

! ... Allocating space for the RK5 intermediate spaces
! ...
allocate(Ourhs(GOU%ni,GOU%nj,NLAYER,5)) 
allocate(Ovrhs(GOV%ni,GOV%nj,NLAYER,5)) 
if (withAtmx) then
  allocate(AUrhs(GAU%ni,GAU%nj,5)) 
  allocate(AVrhs(GAU%ni,GAU%nj,5)) 
endif

! ... Model domain
! ...
model_west  = west
model_east  = east
model_south = south
model_north = north

Nsteps = nint(abs(tmax-tmin)/userRKdt)   ! tmin and tmax in seconds

Reference_time = tmin
rk_dt          = reverse*userRKdt
model_Nstep    = Nsteps

! ... Stochastic terms
! ...
Gaussian_noise = .NOT.withUniform

userMu = abs(userMu)
userK1 = abs(userK1)
userK0 = abs(userK0)

noise_mu = userMu
noise_K1 = sqrt(2.0D0*userK1/abs(rk_dt))
noise_K0 = sqrt(2.0D0*userK0/abs(rk_dt))

write(*,*) 
write(*,*) '**************************'
write(*,*) 'Stochastic forcing terms: '
write(*,*) 'Gaussian noise = ', Gaussian_noise
write(*,*) 'noise_Mu = ', noise_mu
write(*,*) 'noise_K1 = ', userK1, ' => ', noise_K1
write(*,*) 'noise_K0 = ', userK0, ' => ', noise_K0
write(*,*) '**************************'
write(*,*) 

! ... Drift velocity
! ... Alpha and Atmosphere response matrix
! ...
alpha = userAlfa
A11   = userA11
A12   = userA12
A21   = userA21
A22   = userA22

end subroutine model_ini
! ...
! ====================================================================
! ...
subroutine model_run()

! ... Local variables
! ...
integer step,flo,nfreq
integer pk,pn,io,ptu,ptv
type(type_date)                        :: model_date
real(dp)                               :: model_time
!real(dp)                               :: XY(2,FLT%Nfloats,2)   ! X and Y positions
real(dp)                               :: uf(2)
real(dp)                               :: xp(2),xn(2)

! ... Memory space for forcing cubic interpolation
! ...
integer, dimension(4)                  :: ourecords
integer, dimension(4)                  :: ovrecords
integer, dimension(4)                  :: aurecords
integer, dimension(4)                  :: avrecords

real(dp), dimension(GOU%ni,GOU%nj,NLAYER,4)  :: outab
real(dp), dimension(GOV%ni,GOV%nj,NLAYER,4)  :: ovtab
real(dp), dimension(GAU%ni,GAU%nj,4)         :: autab
real(dp), dimension(GAV%ni,GAV%nj,4)         :: avtab

! ... Memory space for forcing linear interpolation
! ...
integer                                :: y1,m1,y2,m2
real(dp)                               :: s1,s2
!integer, dimension(2)                  :: ouLinRecords
!integer, dimension(2)                  :: ovLinRecords
real(dp), dimension(GOU%ni,GOU%nj,NLAYER,2)  :: ouLintab
real(dp), dimension(GOV%ni,GOV%nj,NLAYER,2)  :: ovLintab

! ... Memory space for cubic time interpolation coeffs
! ...
real(dp), dimension(GOU%ni,GOU%nj,NLAYER,4)  :: oucoef
real(dp), dimension(GOV%ni,GOV%nj,NLAYER,4)  :: ovcoef
real(dp), dimension(GAU%ni,GAU%nj,4)         :: aucoef
real(dp), dimension(GAV%ni,GAV%nj,4)         :: avcoef

nfreq = nint(SaveInt/rk_dt)

pk = 1; pn = 2
do step=1,model_Nstep

  rk_t = (step-1)*rk_dt

  model_time = Reference_time + rk_t
  model_date = num2date(model_time,clm_units,clm_calendar)
  !jd   = rk_t/86400.0_dp + UserTini
  !date = jd2date(jd)

  if (verb) then
    write(*,*)
    write(*,*) 'step, rk_t, date :: ', step, rk_t, model_date%iso()
  endif

  ! ... Initialize float velocities to 0
  ! ...
  FLT%u(:) = 0.0D0
  FLT%v(:) = 0.0D0
  
  if (step.eq.1) then

    ! ... Initialize floats:
    ! ... Attention: It is assumed that, at initialization
    ! ... all floats are inside the domain and not stranded.
    ! ...
    do flo=1,FLT%Nfloats
      if (FLT%release_time(flo).le.rk_t) then
        FLT%released(flo) = .True.
        FLT%floating(flo) = .True.
        FLT%lon(flo) = FLT%release_lon(flo)
        FLT%lat(flo) = FLT%release_lat(flo)
        FLT%z(flo)   = FLT%release_depth(flo)
        FLT%x(flo)   = deg2rad*FLT%lon(flo)
        FLT%y(flo)   = deg2rad*FLT%lat(flo)
      else
        FLT%released(flo) = .False.
        FLT%floating(flo) = .False.
        FLT%lon(flo)      = FLT%missing
        FLT%lat(flo)      = FLT%missing
        FLT%z(flo)        = FLT%missing
        FLT%x(flo)        = FLT%missing
        FLT%y(flo)        = FLT%missing
      endif
    enddo
    call trajectory_write(model_time); call check_status()

  endif
        
  ! ... At each time step, check for ithe appropriate forcing
  ! ... snapshots. And interpolate to the Runge-Kutta 
  ! ... time realizations.
  ! ...
  call clm_coeffs()

  do flo=1,FLT%Nfloats

    if (FLT%floating(flo)) then

      surface = FLT%surface(flo)
      ADVECTION_LAYER = FLT%k(flo)

      ! ... Position before advection
      ! ...
      xp(:) = [FLT%x(flo), FLT%y(flo)]

      ! ... Call Runge-Kutta to advance the solution:
      ! ...
      call spherical_rk5 (2,xp,rk_t,rk_dt,xn,uf,RHS2D)

      FLT%x(flo) = xn(1)
      FLT%y(flo) = xn(2)
      FLT%lon(flo) = rad2deg*xn(1)
      FLT%lat(flo) = rad2deg*xn(2)
      FLT%u(flo)   = uf(1)
      FLT%v(flo)   = uf(2)

      !print*, uf

      !write(66,'(2F9.3)') uf(1), Rearth*(xn(1)-xp(1))/rk_dt 
      !write(67,'(2F9.3)') uf(2), cos(0.5d0*(xn(2)+xp(2)))*Rearth*(xn(2)-xp(2))/rk_dt 

      ! ... Check if, after being advecter, the floater leaves
      ! ... the domain or becomes stranded:
      ! ... pn = new step
      ! ... pk = previous step
      ! ...
      if (outside(FLT%x(flo),FLT%y(flo))) then
        FLT%outside(flo) = .True.
        FLT%floating(flo) = .False.
        FLT%x(flo) = FLT%missing
        FLT%y(flo) = FLT%missing
        FLT%z(flo) = FLT%missing
        FLT%lon(flo) = FLT%missing
        FLT%lat(flo) = FLT%missing
      else
        ptu = GOU%point_type(xn(1),xn(2))   ! 0:land, 1:water
        ptv = GOV%point_type(xn(1),xn(2))   ! 0:land, 1:water
        if (ptu.eq.0.and.ptv.eq.0) then
           FLT%stranded(flo) = .True.
           FLT%floating(flo) = .False.
        endif
      endif

      if (FLT%floating(flo)) then
        FLT%dist(flo) = FLT%dist(flo) + haversine(xp(1),xp(2),xn(1),xn(2))
      endif

    endif

  enddo

  ! ... Check if any not-released float should be released now:
  ! ...
  rk_t = step*rk_dt

  do flo=1,FLT%Nfloats
    if (.not.FLT%released(flo)) then
      if (verb) write(*,*) 'Float ', flo,' not yet released '
      if (FLT%release_time(flo).le.rk_t) then
        ! ... Again, it is assumed that the initial position
        ! ... of the floats is a position in the water.
        ! ...
        FLT%released(flo) = .True.
        FLT%floating(flo) = .True.
        FLT%lon(flo) = FLT%release_lon(flo)
        FLT%lat(flo) = FLT%release_lat(flo)
        FLT%z(flo)   = FLT%release_depth(flo)
        FLT%x(flo)   = deg2rad*FLT%lon(flo)
        FLT%y(flo)   = deg2rad*FLT%lat(flo)
      endif
    endif
  enddo

  if (mod(step,nfreq).eq.0) then
    if (verb) write(*,*) "Saving trajectories' positions"
    !call trajectory_write(step*rk_dt,XY(:,:,pk)); call check_status()
    call trajectory_write(Reference_time+step*rk_dt); call check_status()
    call velocity_write()
  endif


enddo

write(*,*) 
write(*,*) 'Floaters Released: ', count(FLT%released(:))
write(*,*) 'Floaters Outside : ', count(FLT%outside(:))
write(*,*) 'Floaters Stranded: ', count(FLT%stranded(:))
write(*,*) 'Floaters Floating: ', count(FLT%floating(:))
write(*,*) 'Floaters Distance: ', FLT%dist(:)
write(*,*) 'Average floaters distance: ', sum(FLT%dist(:))/FLT%Nfloats

! ... Save last postions:
! ...
io = unitfree()

open(io,file=FinalName,status='unknown')
rewind(io)

do flo=1,FLT%Nfloats
!  ! ... Update, if necessary, the last valid position
!  ! ...
!  if (FLT%floating(flo)) then
!    FLT%x(flo) = xn(1,flo,pk)      ! Last position
!    FLT%y(flo) = XY(2,flo,pk)      ! Last position
!    FLT%t(flo) = model_Nstep*rk_dt ! Last time step
!  endif

  ! ... Save with the last valid date:
  ! ...
  model_time = Reference_time + model_Nstep*rk_dt
  model_date = num2date(model_time,clm_units,clm_calendar)
  !jd = model_Nstep*rk_dt/86400.0_dp + UserTini
  !date = jd2date(jd)
  write(io,*) FLT%lon(flo), FLT%lat(flo), FLT%z(flo), trim(model_date%iso())
enddo

close(io)

return

contains

  subroutine clm_coeffs()
  ! ----------------------

  logical OU_updated,OV_updated,AU_updated,AV_updated
  integer pou,pov,pau,pav,i,j,kk,ll
  real(dp) trk,ti,EDT,dm
  type(type_date) dd

  OU_updated = .False.; OV_updated = .False.
  AU_updated = .False.; AV_updated = .False.

  if (step.eq.1) then
  ! ......................................... step == 1

    !print*, 'First step ...'

    if (OceClim)  then
      ! We get the date and time from the rk_t value !!!
      !
      dm = model_date%day + &
          (model_date%hour+model_date%minute/60+model_date%second/3600.0D0)/24.0D0
      y1 = model_date%year
      m1 = model_date%month

      if (dm.lt.HalfMonth(model_date%month)) then
        y2 = y1
        m2 = m1
        m1 = m1 - 1
        if (m1.eq.0) then
          y1 = y1 - 1
          m1 = 12
        endif
      else
        y2 = y1
        m2 = m1 + 1
        if (m2.eq.13) then
          y2 = y2 + 1
          m2 = 1
        endif
      endif

      call dd%is(y1,m1,1,0,0,0,model_calendar)
      s1 = date2num(dd,units=model_units) + (HalfMonth(m1)-1)*86400.0D0
      call dd%is(y2,m2,1,0,0,0,model_calendar)
      s2 = date2num(dd,units=model_units) + (HalfMonth(m2)-1)*86400.0D0
    else
      pou = locate(GOU%s,rk_t); !if (reverse.lt.0) pou = pou + 1
      pov = locate(GOV%s,rk_t); !if (reverse.lt.0) pov = pov + 1
      ourecords = [pou-reverse,pou,pou+reverse,pou+2*reverse]
      ovrecords = [pov-reverse,pov,pov+reverse,pov+2*reverse]
      OU_updated = .True.; OV_updated = .True.
    endif
    if (withAtmx) then
      pau = locate(GAU%s,rk_t); !if (reverse.lt.0) pau = pau + 1
      pav = locate(GAV%s,rk_t); !if (reverse.lt.0) pav = pav + 1
      aurecords = [pau-reverse,pau,pau+reverse,pau+2*reverse]
      avrecords = [pav-reverse,pav,pav+reverse,pav+2*reverse]
      AU_updated = .True.; AV_updated = .True.
    endif

    ! ... Read the fields:
    ! ...
    write(*,*) 'Reading ocean layers: ', LAYER(:)
    if (OceClim) then
      do kk=1,NLAYER
        ouLintab(:,:,kk,1) = GOU%read(layer=LAYER(kk),step=m1)
        ovLintab(:,:,kk,1) = GOV%read(layer=LAYER(kk),step=m1)
        ouLintab(:,:,kk,2) = GOU%read(layer=LAYER(kk),step=m2)
        ovLintab(:,:,kk,2) = GOV%read(layer=LAYER(kk),step=m2)
      enddo
    else
      do ll=1,4
        do kk=1,NLAYER
          outab(:,:,kk,ll) = GOU%read(layer=LAYER(kk),step=ourecords(ll))
          ovtab(:,:,kk,ll) = GOV%read(layer=LAYER(kk),step=ovrecords(ll))
        enddo
      enddo
    endif
    if (withAtmx) then
      do ll=1,4
        autab(:,:,ll) = GAU%read(step=aurecords(ll))
        avtab(:,:,ll) = GAV%read(step=avrecords(ll))
      enddo
    endif

  else
  ! ......................................... step > 1
    if (OceClim) then
      ! ... If climatological simulation
      ! ... The U and V are expected to share the time grid: Jan to Dec
      ! ...
      if (reverse.gt.0) then                 ! Forward model
        if (model_time.gt.s2) then
          !print*, 'Update clim forward'

          OU_updated = .True.
          OV_updated = .True.
          y1 = y2
          m1 = m2
          m2 = m2 + 1
          if (m2.eq.13) then
            y2 = y2 + 1
            m2 = 1
          endif
          s1 = s2
          call dd%is(y2,m2,1,0,0,0,model_calendar)
          s2 = date2num(dd,units=model_units) + (HalfMonth(m2)-1)*86400.0D0
          ouLintab(:,:,:,1) = ouLintab(:,:,:,2)
          ovLintab(:,:,:,1) = ovLintab(:,:,:,2)
          !print*, 'm1, m2 : ', m1, m2
          do kk=1,NLAYER
            ouLintab(:,:,kk,2) = GOU%read(layer=LAYER(kk),step=m2)
            ovLintab(:,:,kk,2) = GOV%read(layer=LAYER(kk),step=m2)
          enddo
        endif
      else                                   ! Backward model
        if (model_time.lt.s1) then
          !print*, 'Update clim backward'
          OU_updated = .True.
          OV_updated = .True.
          y2 = y1
          m2 = m1
          m1 = m1 - 1
          if (m1.eq.0) then
            y1 = y1 - 1
            m1 = 12
          endif
          s2 = s1
          call dd%is(y1,m1,1,0,0,0,model_calendar)
          s1 = date2num(dd,units=model_units) + (HalfMonth(m1)-1)*86400.0D0
          ouLintab(:,:,:,2) = ouLintab(:,:,:,1)
          ovLintab(:,:,:,2) = ovLintab(:,:,:,1)
          !print*, 'm1, m2 : ', m1, m2
          do kk=1,NLAYER
            ouLintab(:,:,kk,1) = GOU%read(layer=LAYER(kk),step=m1)
            ovLintab(:,:,kk,1) = GOV%read(layer=LAYER(kk),step=m1)
          enddo
        endif
      endif

    else
      ! ... Regular simulation: not climatological
      ! ...
      if (abs(rk_t).ge.abs(GOU%s(ourecords(3)))) then
        OU_updated = .True.
        do kk=1,3
          ourecords(kk)   = ourecords(kk+1)
          outab(:,:,:,kk) = outab(:,:,:,kk+1)
        enddo
        ourecords(4) = ourecords(4) + reverse
        !print*, 'Time to update OCE U: ', ourecords(:)
        do kk=1,NLAYER
          outab(:,:,kk,4) = GOU%read(layer=LAYER(kk),step=ourecords(4))
        enddo
      endif

      if (abs(rk_t).ge.abs(GOV%s(ovrecords(3)))) then
        OV_updated = .True.
        do kk=1,3
          ovrecords(kk)   = ovrecords(kk+1)
          ovtab(:,:,:,kk) = ovtab(:,:,:,kk+1)
        enddo
        ovrecords(4) = ovrecords(4) + reverse
        !print*, 'Time to update OCE V: ', ovrecords(:)
        do kk=1,NLAYER
          ovtab(:,:,kk,4) = GOV%read(layer=LAYER(kk),step=ovrecords(4))
        enddo
      endif

    endif

    if (withAtmx) then
      if (abs(rk_t).ge.abs(GAU%s(aurecords(3)))) then
        AU_updated = .True.
        do kk=1,3
          aurecords(kk) = aurecords(kk+1)
          autab(:,:,kk) = autab(:,:,kk+1)
        enddo
        aurecords(4) = aurecords(4) + reverse
        !print*, 'Time to update ATM U: ', aurecords(:)
        autab(:,:,4) = GAU%read(step=aurecords(4))
      endif
      if (abs(rk_t).ge.abs(GAV%s(avrecords(3)))) then
        AV_updated = .True.
        do kk=1,3
          avrecords(kk) = avrecords(kk+1)
          avtab(:,:,kk) = avtab(:,:,kk+1)
        enddo
        avrecords(4) = avrecords(4) + reverse
        !print*, 'Time to update ATM V: ', avrecords(:)
        avtab(:,:,4) = GAV%read(step=avrecords(4))
      endif
    endif
      
  endif

  ! ... Now, if fields have been updated, recalculate the cubic
  ! ... coefficients for forcing fields:
  ! ...
  if (.not.OceClim) then
    if (OU_updated) then
      do kk=1,NLAYER
        !print*, kk, LAYER(kk)
        call i3coeffs(GOU,LAYER(kk),outab(:,:,kk,1:4),oucoef(:,:,kk,1:4))
      enddo
    endif
    if (OV_updated) then
      do kk=1,NLAYER
        call i3coeffs(GOV,LAYER(kk),ovtab(:,:,kk,1:4),ovcoef(:,:,kk,1:4))
      enddo
    endif
  endif
  if (AU_updated) call i3coeffs(GAU,1,autab(:,:,1:4),aucoef(:,:,1:4))
  if (AV_updated) call i3coeffs(GAV,1,avtab(:,:,1:4),avcoef(:,:,1:4))



  ! ... With the interpolation coefficients, now get the five 
  ! ... fields estimated at the times required by the fifth-order 
  ! ... Runge-Kutta algorithm:
  ! ...
  if (OceClim) then
    ! ... Climatological simulation
    ! ...
    EDT = s2 - s1
    do ll=1,5
      trk = model_time + 0.25_dp*(ll-1)*rk_dt
      ti  = (trk - s1)/EDT
      !print*, 'trk, ll, ti = ', trk, ll, ti

      ! ... Zonal velocity interpolation
      ! ...
      do j=1,GOU%nj
      do i=1,GOU%ni
      do kk=1,NLAYER
        if (GOU%var%mask(i,j,LAYER(kk)).eq.0) then
          Ourhs(i,j,kk,ll) = 0.0_dp
        else
          Ourhs(i,j,kk,ll) = (1.0D0-ti)*ouLintab(i,j,kk,1) + ti*ouLintab(i,j,kk,2)
        endif
      enddo
      enddo
      enddo
      !print*, 'Interpolation time clim OU: ', Ourhs(34,20,1,ll)

      ! ... Meridional velocity interpolation
      ! ...
      do j=1,GOV%nj
      do i=1,GOV%ni
      do kk=1,NLAYER
        if (GOV%var%mask(i,j,LAYER(kk)).eq.0) then
          Ovrhs(i,j,kk,ll) = 0.0_dp
        else
          Ovrhs(i,j,kk,ll) = (1.0D0-ti)*ovLintab(i,j,kk,1) + ti*ovLintab(i,j,kk,2)
        endif
      enddo
      enddo
      enddo
      !print*, 'Interpolation time clim OV: ', Ovrhs(34,20,1,ll)
    enddo

  else
    ! ... Regular simulation: not climatological
    ! ...

    ! ... Zonal velocity interpolation
    ! ...
    EDT = GOU%s(ourecords(3)) - GOU%s(ourecords(2))
    !print*, ourecords(:)
    do ll=1,5
      trk = rk_t + 0.25_dp*(ll-1)*rk_dt
      ti = abs((trk-GOU%s(ourecords(2)))/EDT)
      do j=1,GOU%nj
      do i=1,GOU%ni
      do kk=1,NLAYER
        if (GOU%var%mask(i,j,LAYER(kk)).eq.0) then
          Ourhs(i,j,kk,ll) = 0.0_dp
        else
          Ourhs(i,j,kk,ll) = cubic(oucoef(i,j,kk,:),ti)
        endif
      enddo
      enddo
      enddo
      !print*, 'Interpolation time OU: ', ti, Ourhs(34,20,1,ll)
    enddo

    ! ... Meridional velocity interpolation
    ! ...
    EDT = GOV%s(ovrecords(3)) - GOV%s(ovrecords(2))
    !print*, ovrecords(:)
    do ll=1,5
      trk = rk_t + 0.25_dp*(ll-1)*rk_dt
      ti = abs((trk-GOV%s(ovrecords(2)))/EDT)
      do j=1,GOV%nj
      do i=1,GOV%ni
      do kk=1,NLAYER
        if (GOV%var%mask(i,j,LAYER(kk)).eq.0) then
          Ovrhs(i,j,kk,ll) = 0.0_dp
        else
          Ovrhs(i,j,kk,ll) = cubic(ovcoef(i,j,kk,:),ti)
        endif
      enddo
      enddo
      enddo
      !print*, 'Interpolation time OV: ', ti, Ovrhs(34,20,1,ll)
    enddo

  endif

  if (withAtmx) then
    EDT = GAU%s(aurecords(3)) - GAU%s(aurecords(2))
    !print*, aurecords(:)
    do ll=1,5
      trk = rk_t + 0.25_dp*(ll-1)*rk_dt
      ti = abs((trk-GAU%s(aurecords(2)))/EDT)
      do j=1,GAU%nj
      do i=1,GAU%ni
        if (GAU%var%mask(i,j,1).eq.0) then
          Aurhs(i,j,ll) = 0.0_dp
        else
          Aurhs(i,j,ll) = cubic(aucoef(i,j,:),ti)
        endif
      enddo
      enddo
      !print*, 'Interpolation time AU: ', ti, Aurhs(15,5,ll)
    enddo

    EDT = GAV%s(avrecords(3)) - GAV%s(avrecords(2))
    !print*, avrecords(:)
    do ll=1,5
      trk = rk_t + 0.25_dp*(ll-1)*rk_dt
      ti = abs((trk-GAV%s(avrecords(2)))/EDT)
      do j=1,GAV%nj
      do i=1,GAV%ni
        if (GAV%var%mask(i,j,1).eq.0) then
          Avrhs(i,j,ll) = 0.0_dp
        else
          Avrhs(i,j,ll) = cubic(avcoef(i,j,:),ti)
        endif
      enddo
      enddo
      !print*, 'Interpolation time AV: ', ti, Avrhs(15,5,ll)
    enddo


  endif



  end subroutine clm_coeffs
  ! ...
  ! ==================================================================
  ! ...


end subroutine model_run
! ...
! ====================================================================
! ...
subroutine i3coeffs(GRD,layer,uu,cc)

type(type_grid), intent(in)                        :: GRD
integer, intent(in)                                :: layer
real(dp), dimension(GRD%ni,GRD%nj,4), intent(in)   :: uu
real(dp), dimension(GRD%ni,GRD%nj,4), intent(out)  :: cc

! ... Local coefficients
! ...
integer i,j

do j=1,GRD%nj
do i=1,GRD%ni
  if (GRD%var%mask(i,j,layer).eq.0) then
    cc(i,j,:) = 0.0_dp
  else
    cc(i,j,1) = uu(i,j,2)
    cc(i,j,2) = 0.50_dp*(uu(i,j,3)-uu(i,j,1))
    cc(i,j,3) = uu(i,j,1) - 2.50_dp*uu(i,j,2) + two*uu(i,j,3) - 0.50_dp*uu(i,j,4)
    cc(i,j,4) = -0.50_dp*uu(i,j,1) + 1.50_dp*(uu(i,j,2)-uu(i,j,3)) + 0.50_dp*uu(i,j,4)
  endif
enddo
enddo

end subroutine i3coeffs
! ...
! ====================================================================
! ...
real(dp) function cubic(a,t) result(f)

real(dp), dimension(4), intent(in)   :: a
real(dp), intent(in)                 :: t

f = a(1) + t*(a(2)+t*(a(3)+t*a(4)))

end function cubic
! ...
! ====================================================================
! ...
logical function outside(xo,yo)

real(dp), intent(in)                 :: xo,yo

outside = .true.
if (xo.le.model_west) return
if (xo.ge.model_east) return
if (yo.le.model_south) return
if (yo.ge.model_north) return

outside = .false.
return

end function outside


! ...
! ====================================================================
! ...
end module module_model
