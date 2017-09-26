! ****************************************************************************
! ... lagrangian.f90
! ... Quim Ballabrera, April 2017
! ... Module for the Lagrangian program
! ... v1.0
! ****************************************************************************

module lagrangian

use cosmo

implicit none

!character(len=*), parameter             :: version = 'v1.0'
!character(len=*), parameter             :: author  = 'Quim Ballabrera'

! ... Time Forward/Backward
! ...
!integer                                 :: timedirection     ! +1, -1
integer                                 :: record  = 1


!real(dp), parameter                     :: deg2m = deg2rad*Rearth  ! deg -> m
!real(dp), parameter                     :: m2deg = one/deg2m       ! m -> deg
!real(dp)                                :: tscale  = one  ! Pass time to secs.


! ... Command line options
! ...
logical                                 :: grd = .false.  ! Flag for grid
logical                                 :: fte = .false.  ! Flag for temp
logical                                 :: fsa = .false.  ! Flag for psal
logical                                 :: frh = .false.  ! Flag for dens
logical                                 :: fsh = .false.  ! Flag for ssh
logical                                 :: eos = .false.  ! Flag for dens calc.

! ... Input field information
! ...
type(gcdf)                              :: icdf
real(dp), dimension(:), pointer         :: cdfx,cdfy,cdfz,cdft
integer, dimension(:), pointer          :: po,pf
real(dp)                                :: zsurf
real(dp)                                :: time_ref
real(dp)                                :: missing
logical                                 :: missingisnan

integer                                 :: nx, ny  ! System size
real(dp)                                :: dt      ! Time step value
real(dp), dimension(:), allocatable     :: xd,yd   ! Location in degrees
real(dp), dimension(:), pointer         :: xm, ym  ! Location in meters
real(dp), dimension(:,:), pointer       :: cdf2d   ! Temporary field input cdf
character(len=80)                       :: calendar = 'gregorian'

! ... Land/Sea masks
! ...
logical, dimension(:,:), pointer        :: land,sea
integer, dimension(:,:), pointer        :: iw,ie,js,jn
real(dp), dimension(:,:), pointer       :: umask



! ... Input variable IDs
! ...
integer                                 :: uid,vid      ! Required u/v id
integer                                 :: temid = -1   ! Optional tem id
integer                                 :: salid = -1   ! Optional sal id
integer                                 :: rhoid = -1   ! Optional rho id
integer                                 :: sshid = -1   ! Optional ssh id

! ... Floats
! ...
type(date_type)                         :: dateo
logical                                 :: ffx   = .false.  ! Flag fixed lon
logical                                 :: ffy   = .false.  ! Flag fixed lat
logical                                 :: fir   = .false.  ! Flag input cdf rec
logical                                 :: fre = .false.  ! Flag float release
integer                                 :: frec  = -1       ! Float cdf rec
character(len=maxlen)                   :: rfile   = ''   ! release filename
real(dp)                                :: fxo   = nan  ! Initial float pos
real(dp)                                :: fyo   = nan  ! Initial float pos

integer                                 :: Nfloats = 10   ! Default num. floats
logical, dimension(:), pointer          :: deployed
logical, dimension(:), pointer          :: floating
integer, dimension(:), allocatable      :: fle
real(dp), dimension(:), pointer         :: flx,fly,flz,flt,fld
real(dp), dimension(:), pointer         :: flr
real(dp), dimension(:), pointer         :: fltem,flsal,flrho,flssh

! ... Initial cloud of floats
! ...
logical                                 :: fcl = .false.  ! Flag cloud radius 
real(dp)                                :: radius  = zero ! Default radius

! ... Output file
! ...
integer                                 :: ncId,odi,odl
integer                                 :: odx,ody,odz,odt,odd,ode,odr
integer                                 :: odtem,odsal
integer                                 :: ksvd,ksvd_all

real(dp), dimension(:,:,:), pointer     :: urhs    ! u(t),u(t+dt),...
real(dp), dimension(:,:,:), pointer     :: vrhs    ! v(t),v(t+dt),...
real(dp), dimension(:,:,:), pointer     :: trhs    ! t(t),t(t+dt),...
real(dp), dimension(:,:,:), pointer     :: srhs    ! s(t),s(t+dt),...
real(dp), dimension(:,:,:), pointer     :: rrhs    ! r(t),r(t+dt),...
real(dp), dimension(:,:,:), pointer     :: hrhs    ! h(t),h(t+dt),...


end module lagrangian
