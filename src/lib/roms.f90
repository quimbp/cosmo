MODULE roms

use types
use constants
use netcdf
use dates
use cdf

implicit none

TYPE vsc
! ... Vertical S-coordinate
  integer                             :: Vtransform  = 0
  integer                             :: Vstretching = 0
  integer                             :: N           = 0
  real(dp)                            :: theta_s     = 0
  real(dp)                            :: theta_b     = 0
  real(dp)                            :: Tcline      = 0
  real(dp)                            :: hc
  real(dp), dimension(:), pointer     :: sc_r
  real(dp), dimension(:), pointer     :: sc_w
  real(dp), dimension(:), pointer     :: Cs_r
  real(dp), dimension(:), pointer     :: Cs_w
END TYPE vsc

TYPE MHgrid
  integer                             :: xi_rho  = -1
  integer                             :: eta_rho = -1
  integer                             :: xi_u    = -1
  integer                             :: eta_u   = -1
  integer                             :: xi_v    = -1
  integer                             :: eta_v   = -1
  integer                             :: xi_psi  = -1
  integer                             :: eta_psi = -1
  character(1)                        :: spherical = "T"
  real(dp)                            :: xl      = 0.0D0
  real(dp)                            :: el      = 0.0D0
  real(dp), dimension(:,:), pointer   :: lon_rho
  real(dp), dimension(:,:), pointer   :: lat_rho
  real(dp), dimension(:,:), pointer   :: lon_u
  real(dp), dimension(:,:), pointer   :: lat_u
  real(dp), dimension(:,:), pointer   :: lon_v
  real(dp), dimension(:,:), pointer   :: lat_v
  real(dp), dimension(:,:), pointer   :: lon_psi
  real(dp), dimension(:,:), pointer   :: lat_psi
  real(dp), dimension(:,:), pointer   :: angle
  real(dp), dimension(:,:), pointer   :: pm
  real(dp), dimension(:,:), pointer   :: pn
  real(dp), dimension(:,:), pointer   :: h
  real(dp), dimension(:,:), pointer   :: f
  real(dp), dimension(:,:), pointer   :: mask_rho
  real(dp), dimension(:,:), pointer   :: mask_u
  real(dp), dimension(:,:), pointer   :: mask_v
  real(dp), dimension(:,:), pointer   :: mask_psi
END TYPE MHgrid

TYPE MVgrid
  type(vsc)                           :: S
  integer                             :: N   = 0
  integer                             :: N_r = 0
  integer                             :: N_w = 0
  real(dp), dimension(:,:,:), pointer :: z0_r
  real(dp), dimension(:,:,:), pointer :: z0_w
  real(dp), dimension(:,:,:), pointer :: z_r
  real(dp), dimension(:,:,:), pointer :: z_w
  real(dp), dimension(:,:,:), pointer :: Hz
END TYPE MVgrid

TYPE Mroms
  integer                             :: fid
  character(len=180)                  :: filename
  type(MHgrid)                        :: hgrid
  type(MVgrid)                        :: vgrid
  integer                             :: time_id
  integer                             :: Nt
  integer                             :: idtemp
  integer                             :: idpsal
  integer                             :: idu
  integer                             :: idv
  integer                             :: idzeta
  integer                             :: idubar
  integer                             :: idvbar
  contains
    procedure                         :: open  => roms_open
    procedure                         :: close => roms_close
    procedure                         :: alloc => roms_alloc
END TYPE Mroms

CONTAINS
  ! ...
  ! =========================================================================
  ! ...
  function roms_alloc (r,grid) result(A)

  class(Mroms), intent(in)                 :: r
  character(len=1), intent(in), optional   :: grid
  real(dp), dimension(:,:,:), allocatable  :: A

  if (.NOT.present(grid)) then
    allocate(A(r%Hgrid%xi_rho,r%Hgrid%eta_rho,r%Vgrid%N))
  else
    if (grid.eq.'r'.OR.grid.eq.'R') then
      allocate(A(r%Hgrid%xi_rho,r%Hgrid%eta_rho,r%Vgrid%N))
    else if (grid.eq.'u'.OR.grid.eq.'U') then
      allocate(A(r%Hgrid%xi_u,r%Hgrid%eta_u,r%Vgrid%N))
    else if (grid.eq.'v'.OR.grid.eq.'V') then
      allocate(A(r%Hgrid%xi_v,r%Hgrid%eta_v,r%Vgrid%N))
    else if (grid.eq.'w'.OR.grid.eq.'W') then
      allocate(A(r%Hgrid%xi_rho,r%Hgrid%eta_rho,r%Vgrid%N_w))
    else
      call stop_error(1,'Grid not defined in roms_alloc')
    endif
  endif

  end function roms_alloc 
  ! ...
  ! =========================================================================
  ! ...
  function roms_open (r,ifile) result(err)

  character(len=*), intent(in)        :: ifile
  class(Mroms), intent(inout)         :: r
  integer                             :: err

  ! ... Local variables
  ! ...
  integer fndims,fnvars,fnatts,unlimid,ii

  ! ... Open file
  ! ...
  write(*,*) 'Opening ROMS file ', trim(ifile)
  err = NF90_OPEN(ifile,NF90_NOWRITE,r%fid)
  if (err.NE.0) return

  err = NF90_INQUIRE(r%fid,fndims,fnvars,fnatts,unlimid)
  if (err.NE.0) return

  r%time_id = unlimid
  err = NF90_INQUIRE_DIMENSION(r%fid,unlimid,len=r%Nt)

  ! ... Get dimensions' ID
  ! ... Not all of them are mandatory
  ! ...
  err = NF90_INQ_DIMID(r%fid,'xi_rho',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension xi_rho not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%xi_rho)

  err = NF90_INQ_DIMID(r%fid,'eta_rho',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension eta_rho not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%eta_rho)

  err = NF90_INQ_DIMID(r%fid,'xi_u',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension xi_u not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%xi_u)

  err = NF90_INQ_DIMID(r%fid,'eta_u',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension eta_u not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%eta_u)

  err = NF90_INQ_DIMID(r%fid,'xi_v',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension xi_v not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%xi_v)

  err = NF90_INQ_DIMID(r%fid,'eta_v',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension eta_v not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%eta_v)

  err = NF90_INQ_DIMID(r%fid,'xi_psi',r%hgrid%xi_psi)
  err = NF90_INQ_DIMID(r%fid,'eta_psi',r%hgrid%eta_psi)
  
  err = NF90_INQ_DIMID(r%fid,'N',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension N not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%vgrid%N)
  r%vgrid%S%N = r%vgrid%N

  err = NF90_INQ_DIMID(r%fid,'s_w',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension s_w not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%vgrid%N_w)

  err = NF90_INQ_DIMID(r%fid,'s_rho',ii)
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%vgrid%N_r)
  if (err.ne.NF90_NOERR) then
    r%vgrid%N_r = r%vgrid%N
  endif


  nullify(r%hgrid%lon_rho)
  nullify(r%hgrid%lat_rho)
  nullify(r%hgrid%lon_u)
  nullify(r%hgrid%lat_u)
  nullify(r%hgrid%lon_v)
  nullify(r%hgrid%lat_v)
  nullify(r%hgrid%lon_psi)
  nullify(r%hgrid%lat_psi)
  nullify(r%hgrid%pm)
  nullify(r%hgrid%pn)
  nullify(r%hgrid%h)
  nullify(r%hgrid%f)
  nullify(r%hgrid%mask_rho)
  nullify(r%hgrid%mask_u)
  nullify(r%hgrid%mask_v)
  nullify(r%hgrid%mask_psi)

  call roms_read_hgrid(r%fid,r%hgrid,err)

  nullify(r%vgrid%S%sc_r)
  nullify(r%vgrid%S%sc_w)
  nullify(r%vgrid%S%Cs_r)
  nullify(r%vgrid%S%Cs_w)

  call roms_read_vgrid(r%fid,r%vgrid,err)

  allocate(r%vgrid%z0_r(r%hgrid%xi_rho, r%hgrid%eta_rho,   r%vgrid%N))
  allocate(r%vgrid%z0_w(r%hgrid%xi_rho, r%hgrid%eta_rho, 0:r%vgrid%N))
  allocate(r%vgrid%z_r (r%hgrid%xi_rho, r%hgrid%eta_rho,   r%vgrid%N))
  allocate(r%vgrid%z_w (r%hgrid%xi_rho, r%hgrid%eta_rho, 0:r%vgrid%N))
  allocate(r%vgrid%Hz  (r%hgrid%xi_rho, r%hgrid%eta_rho,   r%vgrid%N))

  call roms_set_depth(r%hgrid,r%vgrid)

  r%vgrid%z0_r(:,:,:) = r%vgrid%z_r(:,:,:)
  r%vgrid%z0_w(:,:,:) = r%vgrid%z_w(:,:,:)

  ! ... Get the ID of the other variables:
  ! ...
  err = NF90_INQ_VARID(r%fid,'temp',r%idtemp)
  err = NF90_INQ_VARID(r%fid,'salt',r%idpsal)
  err = NF90_INQ_VARID(r%fid,'u',r%idu)
  err = NF90_INQ_VARID(r%fid,'v',r%idv)
  err = NF90_INQ_VARID(r%fid,'zeta',r%idzeta)
  err = NF90_INQ_VARID(r%fid,'ubar',r%idubar)
  err = NF90_INQ_VARID(r%fid,'vbar',r%idvbar)

  end function roms_open
  ! ...
  ! =========================================================================
  ! ...
  function roms_close (r) result(err)

  class(Mroms), intent(inout)         :: r
  integer                             :: err

  err = NF90_CLOSE(r%fid) 
  if (err.eq.0) then
    nullify(r%hgrid%lon_rho)
    nullify(r%hgrid%lat_rho)
    nullify(r%hgrid%lon_u)
    nullify(r%hgrid%lat_u)
    nullify(r%hgrid%lon_v)
    nullify(r%hgrid%lat_v)
    nullify(r%hgrid%lon_psi)
    nullify(r%hgrid%lat_psi)
    nullify(r%hgrid%pm)
    nullify(r%hgrid%pn)
    nullify(r%hgrid%h)
    nullify(r%hgrid%f)
    nullify(r%hgrid%mask_rho)
    nullify(r%hgrid%mask_u)
    nullify(r%hgrid%mask_v)
    nullify(r%hgrid%mask_psi)

    nullify(r%vgrid%S%sc_r)
    nullify(r%vgrid%S%sc_w)
    nullify(r%vgrid%S%Cs_r)
    nullify(r%vgrid%S%Cs_w)

    nullify(r%vgrid%z0_r)
    nullify(r%vgrid%z0_w)
    nullify(r%vgrid%z_r)
    nullify(r%vgrid%z_w)
    nullify(r%vgrid%Hz)
  endif
  
  end function roms_close
  ! ...
  ! =========================================================================
  ! ...
  subroutine roms_read_hgrid(fid,hgrid,err)

  type(MHgrid), intent(inout)         :: hgrid
  integer, intent(in)                 :: fid
  integer, intent(out)                :: err

  integer ii

  allocate(hgrid%lon_rho(hgrid%xi_rho, hgrid%eta_rho))
  allocate(hgrid%lat_rho(hgrid%xi_rho, hgrid%eta_rho))
  allocate(hgrid%lon_u  (hgrid%xi_u,   hgrid%eta_u))
  allocate(hgrid%lat_u  (hgrid%xi_u,   hgrid%eta_u))
  allocate(hgrid%lon_v  (hgrid%xi_v,   hgrid%eta_v))
  allocate(hgrid%lat_v  (hgrid%xi_v,   hgrid%eta_v))

  err = NF90_INQ_VARID(fid,'lon_rho',ii)
  err = NF90_GET_VAR(fid,ii,hgrid%lon_rho)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lon_rho not found'

  err = NF90_INQ_VARID(fid,'lat_rho',ii)
  err = NF90_GET_VAR(fid,ii,hgrid%lat_rho)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lat_rho not found'

  err = NF90_INQ_VARID(fid,'lon_u',ii)
  err = NF90_GET_VAR(fid,ii,hgrid%lon_u)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lon_u not found'

  err = NF90_INQ_VARID(fid,'lat_u',ii)
  err = NF90_GET_VAR(fid,ii,hgrid%lat_u)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lat_u not found'

  err = NF90_INQ_VARID(fid,'lon_v',ii)
  err = NF90_GET_VAR(fid,ii,hgrid%lon_v)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lon_v not found'

  err = NF90_INQ_VARID(fid,'lat_v',ii)
  err = NF90_GET_VAR(fid,ii,hgrid%lat_v)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lat_v not found'

  err = NF90_INQ_VARID(fid,'lon_psi',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%lon_psi(hgrid%xi_psi, hgrid%eta_psi))
    err = NF90_GET_VAR(fid,ii,hgrid%lon_psi)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable lon_psi'
  endif

  err = NF90_INQ_VARID(fid,'lat_psi',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%lat_psi(hgrid%xi_psi, hgrid%eta_psi))
    err = NF90_GET_VAR(fid,ii,hgrid%lat_psi)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable lat_psi'
  endif

  err = NF90_INQ_VARID(fid,'pm',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%pm     (hgrid%xi_rho, hgrid%eta_rho))
    err = NF90_GET_VAR(fid,ii,hgrid%pm)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable pm'
  endif

  err = NF90_INQ_VARID(fid,'pn',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%pn     (hgrid%xi_rho, hgrid%eta_rho))
    err = NF90_GET_VAR(fid,ii,hgrid%pn)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable pn'
  endif

  err = NF90_INQ_VARID(fid,'h',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%h      (hgrid%xi_rho, hgrid%eta_rho))
    err = NF90_GET_VAR(fid,ii,hgrid%h)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable h'
  endif

  err = NF90_INQ_VARID(fid,'f',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%f      (hgrid%xi_rho, hgrid%eta_rho))
    err = NF90_GET_VAR(fid,ii,hgrid%f)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable f'
  endif

  err = NF90_INQ_VARID(fid,'mask_rho',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%mask_rho(hgrid%xi_rho, hgrid%eta_rho))
    err = NF90_GET_VAR(fid,ii,hgrid%mask_rho)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable mask_rho'
  endif

  err = NF90_INQ_VARID(fid,'mask_u',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%mask_u  (hgrid%xi_u, hgrid%eta_u))
    err = NF90_GET_VAR(fid,ii,hgrid%mask_u)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable mask_u'
  endif

  err = NF90_INQ_VARID(fid,'mask_v',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%mask_v  (hgrid%xi_v, hgrid%eta_v))
    err = NF90_GET_VAR(fid,ii,hgrid%mask_v)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable mask_v'
  endif

  err = NF90_INQ_VARID(fid,'mask_psi',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(hgrid%mask_psi(hgrid%xi_psi, hgrid%eta_psi))
    err = NF90_GET_VAR(fid,ii,hgrid%mask_psi)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable mask_psi'
  endif

  end subroutine roms_read_hgrid
  ! ...
  ! =========================================================================
  ! ...
  subroutine roms_read_vgrid(fid,vgrid,err)

  type(MVgrid), intent(inout)         :: vgrid
  integer, intent(in)                 :: fid
  integer, intent(out)                :: err

  integer ii

  allocate(vgrid%S%sc_r(vgrid%N_r))
  allocate(vgrid%S%sc_w(vgrid%N_w))
  allocate(vgrid%S%Cs_r(vgrid%N_r))
  allocate(vgrid%S%Cs_w(vgrid%N_w))

  err = NF90_INQ_VARID(fid,'Vtransform',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%Vtransform)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Vtransform not found'

  err = NF90_INQ_VARID(fid,'Vstretching',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%Vstretching)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Vstretching not found'

  err = NF90_INQ_VARID(fid,'theta_s',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%theta_s)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable theta_s not found'

  err = NF90_INQ_VARID(fid,'theta_b',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%theta_b)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable theta_b not found'

  err = NF90_INQ_VARID(fid,'Tcline',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%Tcline)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Tcline not found'

  err = NF90_INQ_VARID(fid,'hc',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%hc)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable hc not found'

  err = NF90_INQ_VARID(fid,'s_rho',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%sc_r)
  write(*,*) NF90_STRERROR(err)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable s_rho not found'
  

  err = NF90_INQ_VARID(fid,'s_w',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%sc_w)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable s_w not found'

  err = NF90_INQ_VARID(fid,'Cs_r',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%Cs_r)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Cs_r not found'

  err = NF90_INQ_VARID(fid,'Cs_w',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%S%Cs_w)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Cs_w not found'

  return

  end subroutine roms_read_vgrid
  ! ...
  ! =========================================================================
  ! ...
  subroutine set_scoord(S,hmin)

  type(vsc), intent(inout)       :: S
  real(dp), intent(in), optional :: hmin

  ! ... Local variables:
  ! ...
  integer k,N
  real(dp) theta_b,theta_s
  real(dp) cff1,cff2,ds,A,B,C
  real(dp) Cbot,Csur,Cweight,exp_bot,exp_sur,Hscale
  real(dp) rk,rN,sc_r,sc_w

  if (S%Vtransform.le.0) stop 'Vtransform not set'
  if (S%Vstretching.le.0) stop 'Vtransform not set'

  ! ... Set hc:
  ! ...
  if (S%Vtransform.eq.1) then
    if (.not.present(hmin)) stop 'Vtransform 1 requires specifying hmin value'
    S%hc = min(hmin,S%Tcline)
  else if (S%Vtransform.eq.2) then
    S%hc = S%Tcline
  endif

  ! ... Allocate sigma levels and stretching functions
  ! ...
  N       = S%N
  theta_s = S%theta_s
  theta_b = S%theta_b

  allocate(S%sc_r(N))
  allocate(S%Cs_r(N))
  allocate(S%sc_w(0:N))
  allocate(S%Cs_w(0:N))

  !-----------------------------------------------------------------------
  !  Original vertical strectching function, Song and Haidvogel (1994).
  !-----------------------------------------------------------------------
  if (S%Vstretching.eq.1) then

    if (theta_s.ne.zero) then
      cff1 = one/SINH(theta_s)
      cff2 = half/TANH(half*theta_s)
    endif
    S%sc_w(0) = -one
    S%Cs_w(0) = -one
    ds = one/REAL(N,dp)
    do k=1,N
      S%sc_w(k) = ds*REAL(k-N,dp)
      S%sc_r(k) = ds*(REAL(k-N,dp)-half)
      if (theta_s.ne.zero) then
        S%Cs_w(k) = (one-theta_b)*cff1*SINH(theta_s*S%sc_w(k)) +   &
                     theta_b*(cff2*TANH(theta_s*(S%sc_w(k)+half))-half)
        S%Cs_r(k) = (one-theta_b)*cff1*SINH(theta_s*S%sc_r(k))+    &
                     theta_b*(cff2*TANH(theta_s*(S%sc_r(k)+half))-half)
      else
       S%Cs_w(k) = S%sc_w(k)
       S%Cs_r(k) = S%sc_r(k)
      endif
    enddo

  !-----------------------------------------------------------------------
  !  A. Shchepetkin vertical stretching function. This function was
  !  improved further to allow bottom refiment (see Vstretching=4).
  !-----------------------------------------------------------------------
  else if (S%Vstretching.eq.2) then

    A  = one
    B  = one
    ds = one/REAL(N,dp)

    S%sc_w(N) = zero
    S%Cs_w(N) = zero
    do k=N-1,1,-1
      sc_w = ds*REAL(k-N,dp)
      S%sc_w(k) = sc_w
      if (theta_s.gt.zero) then
        Csur = (one-COSH(theta_s*sc_w))/(COSH(theta_s)-one)
        if (theta_b.gt.zero) then
          Cbot      = SINH(theta_b*(sc_w+one))/SINH(theta_b)-one
          C         = (sc_w+one)**A*(one+(A/B)*(one-(sc_w+one)**B))
          S%Cs_w(k) = C*Csur+(one-C)*Cbot
        else
          S%Cs_w(k) = Csur
        endif
      else
        S%Cs_w(k) = sc_w
      endif
    enddo
    S%sc_w(0) = -one
    S%Cs_w(0) = -one

    do k=1,N
      sc_r=ds*(REAL(k-N,dp)-half)
      S%sc_r(k)=sc_r
      if (theta_s.gt.zero) then
        Csur = (one-COSH(theta_s*sc_r))/(COSH(theta_s)-one)
        if (theta_b.gt.zero) then
          Cbot      = SINH(theta_b*(sc_r+one))/SINH(theta_b)-one
          C         = (sc_r+one)**A*(one+(A/B)*(one-(sc_r+one)**B))
          S%Cs_r(k) = C*Csur+(one-C)*Cbot
        else
          S%Cs_r(k) = Csur
        endif
      else
        S%Cs_r(k) = sc_r
      endif
    enddo 

  !-----------------------------------------------------------------------
  !  R. Geyer stretching function for high bottom boundary layer
  !  resolution.
  !-----------------------------------------------------------------------
  else if (S%Vstretching.eq.3) then

    exp_sur = theta_s
    exp_bot = theta_b
    Hscale  = 3.0_dp
    ds = one/REAL(N,dp)

    S%sc_w(N) = zero
    S%Cs_w(N) = zero
    do k=N-1,1,-1
      sc_w = ds*REAL(k-N,dp)
      S%sc_w(k) = sc_w
      Cbot =  LOG(COSH(Hscale*(sc_w+one)**exp_bot))/LOG(COSH(Hscale))-one
      Csur = -LOG(COSH(Hscale*ABS(sc_w)**exp_sur))/LOG(COSH(Hscale))
      Cweight = half*(one-TANH(Hscale*(sc_w+half)))
      S%Cs_w(k) = Cweight*Cbot+(one-Cweight)*Csur
    enddo
    S%sc_w(0) = -one
    S%Cs_w(0) = -one

    do k=1,N
      sc_r = ds*(REAL(k-N,dp)-half)
      S%sc_r(k) = sc_r
      Cbot =  LOG(COSH(Hscale*(sc_r+one)**exp_bot))/LOG(COSH(Hscale))-one
      Csur = -LOG(COSH(Hscale*ABS(sc_r)**exp_sur))/LOG(COSH(Hscale))
      Cweight = half*(one-TANH(Hscale*(sc_r+half)))
      S%Cs_r(k) = Cweight*Cbot+(one-Cweight)*Csur
    enddo

  !-----------------------------------------------------------------------
  !  A. Shchepetkin improved double vertical stretching functions with
  !  bottom refiment.
  !-----------------------------------------------------------------------
  else if (S%Vstretching.eq.4) then
  
    ds = one/REAL(N,dp)

    S%sc_w(N) = zero
    S%Cs_w(N) = zero
    do k=N-1,1,-1
      sc_w      = ds*REAL(k-N,dp)
      S%sc_w(k) = sc_w
      if (theta_s.gt.zero) then
        Csur = (one-COSH(theta_s*sc_w))/(COSH(theta_s)-one)
      else
        Csur = -sc_w**2
      endif
      if (theta_b.gt.zero) then
        Cbot      = (EXP(theta_b*Csur)-one)/(one-EXP(-theta_b))
        S%Cs_w(k) = Cbot
      else
        S%Cs_w(k) = Csur
      endif
    enddo
    S%sc_w(0) = -one
    S%Cs_w(0) = -one

    do k=1,N
      sc_r      = ds*(REAL(k-N,dp)-half)
      S%sc_r(k) = sc_r
      if (theta_s.gt.zero) then
        Csur = (one-COSH(theta_s*sc_r))/(COSH(theta_s)-one)
      else
        Csur = -sc_r**2
      endif
      if (theta_b.gt.zero) then
        Cbot = (EXP(theta_b*Csur)-one)/(one-EXP(-theta_b))
        S%Cs_r(k) = Cbot
      else
        S%Cs_r(k) = Csur
      endif
    enddo

  !----------------------------------------------------------------------
  ! Stretching 5 case using a quadratic Legendre polynomial function
  ! aproach for the s-coordinate to enhance the surface exchange layer.
  !-----------------------------------------------------------------------
  else if (S%Vstretching.eq.5) then

    S%sc_w(N) = zero
    S%Cs_w(N) = zero
    do k=N-1,1,-1
      rk = REAL(k,dp)
      rN = REAL(N,dp)
      sc_w = -(rk*rk - 2.0_dp*rk*rN + rk + rN*rN - rN)/(rN*rN - rN) -  &
              0.01_dp*(rk*rk - rk*rN)/(one - rN)
      S%sc_w(k) = sc_w
      if (theta_s.gt.zero) then
        Csur = (one-COSH(theta_s*sc_w))/(COSH(theta_s)-one)
      else
        Csur = -sc_w**2
      endif
      if (theta_b.gt.zero) then
        Cbot      = (EXP(theta_b*Csur)-one)/(one-EXP(-theta_b))
        S%Cs_w(k) = Cbot
      else
        S%Cs_w(k) = Csur
      endif
    enddo
    S%sc_w(0) = -one
    S%Cs_w(0) = -one

    do k=1,N
      rk = REAL(k,dp)-half
      rN = REAL(N,dp)
      sc_r = -(rk*rk - 2.0_dp*rk*rN + rk + rN*rN - rN)/(rN*rN - rN)-  &
                 0.01_dp*(rk*rk - rk*rN)/(one - rN)
      S%sc_r(k) = sc_r
      if (theta_s.gt.zero) then
        Csur = (one-COSH(theta_s*sc_r))/(COSH(theta_s)-one)
      else
        Csur = -sc_r**2
      endif
      if (theta_b.gt.zero) then
        Cbot = (EXP(theta_b*Csur)-one)/(one-EXP(-theta_b))
        S%Cs_r(k) = Cbot
      else
        S%Cs_r(k) = Csur
      endif
    enddo

  endif

  end subroutine set_scoord
  ! ...
  ! =========================================================================
  ! ...
  subroutine roms_set_depth(hgrid,vgrid,zeta_t)

  type(MHgrid), intent(in)            :: hgrid
  type(MVgrid), intent(inout)         :: vgrid
  real(dp), dimension(hgrid%xi_rho,hgrid%eta_rho), optional :: zeta_t

  ! ... Local variables
  ! ...
  integer i,j,k
  real(dp) cff_r,cff_w,cff1_r,cff1_w,cff2_r,cff2_w
  real(dp) hwater,hinv,z_w0,z_r0
  real(dp), dimension(hgrid%xi_rho,hgrid%eta_rho) :: zeta


  if (present(zeta_t)) then
    zeta(:,:) = zeta_t(:,:)
  else
    zeta(:,:) = zero
  endif
 
  if (vgrid%S%Vtransform.eq.1) then

    do j=1,hgrid%eta_rho
      do i=1,hgrid%xi_rho
        vgrid%z_w(i,j,0) = -hgrid%h(i,j)
      enddo
      do k=1,vgrid%N
        cff_r  = vgrid%S%hc*(vgrid%S%sc_r(k)-vgrid%S%Cs_r(k))
        cff_w  = vgrid%S%hc*(vgrid%S%sc_w(k)-vgrid%S%Cs_w(k))
        cff1_r = vgrid%S%Cs_r(k)
        cff1_w = vgrid%S%Cs_w(k)
        do i=1,hgrid%xi_rho
          hwater = hgrid%h(i,j)
          hinv   = one / hwater
          z_r0   = cff_r + cff1_r*hwater
          z_w0   = cff_w + cff1_w*hwater
          vgrid%z_r(i,j,k) = z_r0 + zeta(i,j)*( one + z_r0*hinv)
          vgrid%z_w(i,j,k) = z_w0 + zeta(i,j)*( one + z_w0*hinv)
          vgrid%Hz(i,j,k)  = vgrid%z_w(i,j,k) - vgrid%z_w(i,j,k-1)
        enddo
      enddo
    enddo

  else if (vgrid%S%Vtransform.eq.2) then

    do j=1,hgrid%eta_rho
      do i=1,hgrid%xi_rho
        vgrid%z_w(i,j,0) = -hgrid%h(i,j)
      enddo
      do k=1,vgrid%N
        cff_r  = vgrid%S%hc*vgrid%S%sc_r(k)
        cff_w  = vgrid%S%hc*vgrid%S%sc_w(k)
        cff1_r = vgrid%S%Cs_r(k)
        cff1_w = vgrid%S%Cs_w(k)
        do i=1,hgrid%xi_rho
          hwater = hgrid%h(i,j)
          hinv   = one / (vgrid%S%hc + hwater)
          cff2_r = (cff_r + cff1_r*hwater)*hinv
          cff2_w = (cff_w + cff1_w*hwater)*hinv
          vgrid%z_r(i,j,k) = zeta(i,j) + (zeta(i,j) + hwater)*cff2_r
          vgrid%z_w(i,j,k) = zeta(i,j) + (zeta(i,j) + hwater)*cff2_w
          vgrid%Hz(i,j,k)  = vgrid%z_w(i,j,k) - vgrid%z_w(i,j,k-1)
        enddo
      enddo
    enddo

  else

    stop 'Why am I here?'

  endif 

  end subroutine roms_set_depth
  ! ...
  ! =========================================================================
  ! ...
  function roms_read3d(r,vname,step) result (fld)

  type(Mroms), intent(in)             :: r
  character(len=*), intent(in)        :: vname
  integer, intent(in), optional       :: step
  real(dp), dimension(:,:,:), pointer :: fld

  ! ... Local variables
  ! ...
  integer err,ii,xtype,ndims,dimids(1:10),natts
  integer i,j,pt,pot
  integer po(4),pf(4),nn(4),n(3)
  character(len=180) lname

  err = NF90_INQ_VARID(r%fid,trim(vname),ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'ERROR: while loooking for variable ', trim(vname)
    stop 'variable not found'
  endif

  if (present(step)) then
    pot = step
  else
    ! ... Read first record:
    pot = 1
  endif

  ! ... Now we got the variable id.
  ! ...
  err = NF90_INQUIRE_VARIABLE (r%fid, ii, lname, xtype, ndims, dimids, natts)

  ! ... Find one dimension is time and keep track of which one is:
  pt = -1
  do i=1,ndims
    if (dimids(i).eq.r%time_id) then
      pt = i
      exit
    endif
  enddo

  if (pt.gt.0) then
    ! ... There are four dimensions
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(1),len=nn(1))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(2),len=nn(2))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(3),len=nn(3))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(4),len=nn(4))

    j = 0
    do i=1,4
      if (dimids(i).NE.r%time_id) then
        j = j + 1
        n(j) = nn(i)
      endif
    enddo
    
    po(:) = (/1,1,1,1/)
    po(pt) = pot
    pf(:) = nn(:)
    pf(pt) = 1
  else
    ! ... There are only three dimensions
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(1),len=n(1))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(2),len=n(2))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(3),len=n(3))
  endif

  ! ... Allocate pointer:
  ! ...
  allocate(fld(n(1),n(2),n(3)),stat=err)
  if (err.ne.0) stop 'Error while allocating fld memory in roms_read3'

  if (pt.gt.0) then
    err = NF90_GET_VAR(r%fid,ii,fld,po,pf)
  else
    err = NF90_GET_VAR(r%fid,ii,fld)
  endif
  if (err.NE.NF90_NOERR) then
    print*, NF90_STRERROR(err)
    stop 'ERROR in roms_read3 subroutine'
  endif

  return
  end function roms_read3d
  ! ...
  ! =========================================================================
  ! ...
  function roms_read2d(r,vname,step) result (fld)

  type(Mroms), intent(in)             :: r
  character(len=*), intent(in)        :: vname
  integer, intent(in), optional       :: step
  real(dp), dimension(:,:), pointer   :: fld

  ! ... Local variables
  ! ...
  integer err,ii,xtype,ndims,dimids(1:10),natts
  integer i,j,pt,pot
  integer po(3),pf(3),nn(3),n(2)
  character(len=180) lname

  err = NF90_INQ_VARID(r%fid,trim(vname),ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'ERROR: while loooking for variable ', trim(vname)
    stop 'variable not found'
  endif

  if (present(step)) then
    pot = step
  else
    ! ... Read first record:
    pot = 1
  endif

  ! ... Now we got the variable id.
  ! ...
  err = NF90_INQUIRE_VARIABLE (r%fid, ii, lname, xtype, ndims, dimids, natts)

  ! ... Find one dimension is time and keep track of which one is:
  pt = -1
  do i=1,ndims
    if (dimids(i).eq.r%time_id) then
      pt = i
      exit
    endif
  enddo

  if (pt.gt.0) then
    ! ... There are three dimensions
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(1),len=nn(1))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(2),len=nn(2))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(3),len=nn(3))

    j = 0
    do i=1,3
      if (dimids(i).NE.r%time_id) then
        j = j + 1
        n(j) = nn(i)
      endif
    enddo
    
    po(:) = (/1,1,1/)
    po(pt) = pot
    pf(:) = nn(:)
    pf(pt) = 1
  else
    ! ... There are only two dimensions
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(1),len=n(1))
    err = NF90_INQUIRE_DIMENSION(r%fid,dimids(2),len=n(2))
  endif

  ! ... Allocate pointer:
  ! ...
  allocate(fld(n(1),n(2)),stat=err)
  if (err.ne.0) stop 'Error while allocating fld memory in roms_read2'

  if (pt.gt.0) then
    err = NF90_GET_VAR(r%fid,ii,fld,po,pf)
  else
    err = NF90_GET_VAR(r%fid,ii,fld)
  endif
  if (err.NE.NF90_NOERR) then
    print*, NF90_STRERROR(err)
    stop 'ERROR in roms_read2 subroutine'
  endif

  return
  end function roms_read2d
  ! ...
  ! =========================================================================
  ! ...
  function roms_missing(r,vname) result(spv)

  type(Mroms), intent(in)             :: r
  character(len=*), intent(in)        :: vname
  real(dp)                            :: spv

  ! ... Local variables
  ! ...
  integer err,ii,filled

  err = NF90_INQ_VARID(r%fid,trim(vname),ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'ERROR: while loooking for variable ', trim(vname)
    stop 'variable not found'
  endif

  !err = NF90_INQ_VAR_FILL(r%fid,ii,filled,spv)
  err = NF90_GET_ATT(r%fid,ii,'_FillValue',spv)
  call cdf_error(err,'No _FillValue attribute')

  return

  end function roms_missing
  ! ...
  ! =========================================================================
  ! ...
  subroutine roms_read_grid (ifile,r)

  character(len=*), intent(in)        :: ifile
  type(Mroms), intent(inout)          :: r

  ! ... Local variables
  ! ...
  integer fndims,fnvars,fnatts,unlimid,ii,err

  ! ... Open file
  ! ...
  write(*,*) 'Opening Grid file ', trim(ifile)
  err = NF90_OPEN(ifile,NF90_NOWRITE,r%fid)
  if (err.NE.0) then
    write(*,*) 'Grid file not found'
    stop 'ERROR in roms_read_grid'
  endif

  err = NF90_INQUIRE(r%fid,fndims,fnvars,fnatts,unlimid)
  if (err.NE.0) return

  ! ... Get dimensions' ID
  ! ... Not all of them are mandatory
  ! ...
  err = NF90_INQ_DIMID(r%fid,'xi_rho',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension xi_rho not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%xi_rho)

  err = NF90_INQ_DIMID(r%fid,'eta_rho',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension eta_rho not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%eta_rho)

  err = NF90_INQ_DIMID(r%fid,'xi_u',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension xi_u not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%xi_u)

  err = NF90_INQ_DIMID(r%fid,'eta_u',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension eta_u not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%eta_u)

  err = NF90_INQ_DIMID(r%fid,'xi_v',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension xi_v not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%xi_v)

  err = NF90_INQ_DIMID(r%fid,'eta_v',ii)
  if (err.ne.NF90_NOERR) then
    write(*,*) 'Dimension eta_v not found'
    return
  endif
  err = NF90_INQUIRE_DIMENSION(r%fid,ii,len=r%hgrid%eta_v)

  err = NF90_INQ_DIMID(r%fid,'xi_psi',r%hgrid%xi_psi)
  err = NF90_INQ_DIMID(r%fid,'eta_psi',r%hgrid%eta_psi)

  allocate(r%hgrid%lon_rho(r%hgrid%xi_rho, r%hgrid%eta_rho))
  allocate(r%hgrid%lat_rho(r%hgrid%xi_rho, r%hgrid%eta_rho))
  allocate(r%hgrid%lon_u  (r%hgrid%xi_u,   r%hgrid%eta_u))
  allocate(r%hgrid%lat_u  (r%hgrid%xi_u,   r%hgrid%eta_u))
  allocate(r%hgrid%lon_v  (r%hgrid%xi_v,   r%hgrid%eta_v))
  allocate(r%hgrid%lat_v  (r%hgrid%xi_v,   r%hgrid%eta_v))

  err = NF90_INQ_VARID(r%fid,'lon_rho',ii)
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%lon_rho)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lon_rho not found'

  err = NF90_INQ_VARID(r%fid,'lat_rho',ii)
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%lat_rho)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lat_rho not found'

  err = NF90_INQ_VARID(r%fid,'lon_u',ii)
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%lon_u)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lon_u not found'

  err = NF90_INQ_VARID(r%fid,'lat_u',ii)
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%lat_u)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lat_u not found'

  err = NF90_INQ_VARID(r%fid,'lon_v',ii)
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%lon_v)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lon_v not found'

  err = NF90_INQ_VARID(r%fid,'lat_v',ii)
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%lat_v)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable lat_v not found'

  err = NF90_INQ_VARID(r%fid,'lon_psi',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(r%hgrid%lon_psi(r%hgrid%xi_psi, r%hgrid%eta_psi))
    err = NF90_GET_VAR(r%fid,ii,r%hgrid%lon_psi)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable lon_psi'
  endif

  err = NF90_INQ_VARID(r%fid,'lat_psi',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(r%hgrid%lat_psi(r%hgrid%xi_psi, r%hgrid%eta_psi))
    err = NF90_GET_VAR(r%fid,ii,r%hgrid%lat_psi)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable lat_psi'
  endif

  err = NF90_INQ_VARID(r%fid,'pm',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(r%hgrid%pm     (r%hgrid%xi_rho, r%hgrid%eta_rho))
    err = NF90_GET_VAR(r%fid,ii,r%hgrid%pm)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable pm'
  endif

  err = NF90_INQ_VARID(r%fid,'pn',ii)
  if (err.EQ.NF90_NOERR) then
    allocate(r%hgrid%pn     (r%hgrid%xi_rho, r%hgrid%eta_rho))
    err = NF90_GET_VAR(r%fid,ii,r%hgrid%pn)
    if (err.NE.NF90_NOERR) stop 'ERROR: Reading variable pn'
  endif


  ! ... Read h
  ! ...
  err = NF90_INQ_VARID(r%fid,'h',ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'ERROR: while loooking for variable h'
    stop 'variable not found'
  endif
  allocate(r%hgrid%h(r%hgrid%xi_rho,r%hgrid%eta_rho))
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%h)
  if (err.NE.NF90_NOERR) stop 'ERROR while reading h'

  ! ... masks
  ! ...
  err = NF90_INQ_VARID(r%fid,'mask_rho',ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'ERROR: while loooking for variable mask_rho'
    stop 'variable not found'
  endif
  allocate(r%hgrid%mask_rho(r%hgrid%xi_rho,r%hgrid%eta_rho))
  err = NF90_GET_VAR(r%fid,ii,r%hgrid%mask_rho)
  if (err.NE.NF90_NOERR) stop 'ERROR while reading mask_rho'

  err = NF90_INQ_VARID(r%fid,'mask_u',ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'WARNING: variable mask_u not found'
  else
    allocate(r%hgrid%mask_u(r%hgrid%xi_u,r%hgrid%eta_u))
    err = NF90_GET_VAR(r%fid,ii,r%hgrid%mask_u)
    if (err.NE.NF90_NOERR) stop 'ERROR while reading mask_u'
  endif

  err = NF90_INQ_VARID(r%fid,'mask_v',ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'ERROR: variable mask_v not found'
  else
    allocate(r%hgrid%mask_v(r%hgrid%xi_v,r%hgrid%eta_v))
    err = NF90_GET_VAR(r%fid,ii,r%hgrid%mask_v)
    if (err.NE.NF90_NOERR) stop 'ERROR while reading mask_v'
  endif

  err = NF90_INQ_VARID(r%fid,'mask_psi',ii)
  if (err.NE.NF90_NOERR) then
    write(*,*) 'WARNING: variable mask_psi not found'
  else
    allocate(r%hgrid%mask_psi(r%hgrid%xi_psi,r%hgrid%eta_psi))
    err = NF90_GET_VAR(r%fid,ii,r%hgrid%mask_psi)
    if (err.NE.NF90_NOERR) stop 'ERROR while reading mask_psi'
  endif

  err = NF90_CLOSE(r%fid) 
  return

  end subroutine roms_read_grid
  ! ...
  ! =========================================================================
  ! ...
  subroutine read_roms_time(filename,ftime,fdate)

  character(len=*), intent(in)                 :: filename
  real(dp), intent(out)                        :: ftime
  type(date_type), intent(out)                 :: fdate

  integer err,fid,ii
  real(dp) tt(1),reftime
  type(date_type) refdate

  err = NF90_OPEN(filename,NF90_NOWRITE,fid)
  if (err.NE.NF90_NOERR) stop 'unable to open roms file'

  err = NF90_INQ_VARID(fid,'ocean_time',ii)
  if (err.NE.NF90_NOERR) stop 'unable to get ocean_time id'

  err = NF90_GET_VAR(fid,ii,tt)
  if (err.NE.NF90_NOERR) stop 'unable to read ocean_time'
  

  ftime = tt(1)
  ! ... The time units of ROMS files are seconds since initialization
  ! ... We need to retrieve initialization date
  err     = cdf_timeref(fid,ii,refdate)
  reftime = date2jd(refdate)
  fdate   = jd2date(reftime+ftime/86400.0D0)

  return
  end subroutine read_roms_time
  ! ...
  ! =========================================================================
  ! ...
  subroutine zdepth(S,h,z_r,z_w,dz,eta)

  type(vsc), intent(in)               :: S
  real(dp), intent(in)                :: h
  real(dp), intent(out)               :: z_r(S%N),dz(S%N),z_w(0:S%N)
  real(dp), intent(in), optional      :: eta

  ! ... Local variables
  ! ...
  integer k
  real(dp) cff_r,cff_w,cff1_r,cff1_w,cff2_r,cff2_w
  real(dp) hwater,hinv,z_w0,z_r0,zeta

  if (present(eta)) then
    zeta = eta
  else
    zeta = zero
  endif

  if (S%Vtransform.eq.1) then

    z_w(0) = -h
    do k=1,S%N
      cff_r  = S%hc*(S%sc_r(k)-S%Cs_r(k))
      cff_w  = S%hc*(S%sc_w(k)-S%Cs_w(k))
      cff1_r = S%Cs_r(k)
      cff1_w = S%Cs_w(k)
      hwater = h
      hinv   = one / hwater
      z_r0   = cff_r + cff1_r*hwater
      z_w0   = cff_w + cff1_w*hwater
      z_r(k) = z_r0 + zeta*(one + z_r0*hinv)
      z_w(k) = z_w0 + zeta*(one + z_w0*hinv)
      dz(k)  = z_w(k) - z_w(k-1)
    enddo

  else if (S%Vtransform.eq.2) then

    z_w(0) = -h
    do k=1,S%N
      cff_r  = S%hc*S%sc_r(k)
      cff_w  = S%hc*S%sc_w(k)
      cff1_r = S%Cs_r(k)
      cff1_w = S%Cs_w(k)
      hwater = h
      hinv   = one / (S%hc + hwater)
      cff2_r = (cff_r + cff1_r*hwater)*hinv
      cff2_w = (cff_w + cff1_w*hwater)*hinv
      z_r(k) = zeta + (zeta + hwater)*cff2_r
      z_w(k) = zeta + (zeta + hwater)*cff2_w
      dz(k)  = z_w(k) - z_w(k-1)
    enddo

  else

    stop 'Why am I here?'

  endif 

  end subroutine zdepth
  ! ...
  ! =========================================================================
  ! ...
  subroutine roms_create_grid (ifile,r)

  implicit none

  character(len=*), intent(in)        :: ifile
  type(Mroms), intent(inout)          :: r

  ! ... Local variables
  ! ...
  integer err
  integer idir,idiu,idiv,idip,id1
  integer idjr,idju,idjv,idjp
  integer idxl,idel,iddmin,iddmax,idsph,idang,idh,idhraw,idf
  integer idpm,idpn,iddndx,iddmde,idxr,idyr,idxu,idyu
  integer idxv,idyv,idxp,idyp,idmr,idmu,idmv,idmp

  ! ... Open file
  ! ...
  write(*,*) 'Creating Grid file ', trim(ifile)
  err = NF90_CREATE(ifile,NF90_CLOBBER,r%fid)
  if (err.NE.0) then
    write(*,*) 'Unable to create grid file'
    stop 'ERROR in roms_create_grid'
  endif

  err = NF90_DEF_DIM(r%fid,'xi_rho',r%hgrid%xi_rho,idir)
  err = NF90_DEF_DIM(r%fid,'eta_rho',r%hgrid%eta_rho,idjr)

  r%hgrid%xi_u    =  r%hgrid%xi_rho - 1
  r%hgrid%eta_u   =  r%hgrid%eta_rho
  r%hgrid%xi_v    =  r%hgrid%xi_rho
  r%hgrid%eta_v   =  r%hgrid%eta_rho - 1
  r%hgrid%xi_psi  =  r%hgrid%xi_rho  - 1
  r%hgrid%eta_psi =  r%hgrid%eta_rho - 1

  ! ... Dimensions:
  ! ...
  err = NF90_DEF_DIM(r%fid,'xi_u',r%hgrid%xi_u,idiu)
  err = NF90_DEF_DIM(r%fid,'eta_u',r%hgrid%eta_u,idju)
  err = NF90_DEF_DIM(r%fid,'xi_v',r%hgrid%xi_v,idiv)
  err = NF90_DEF_DIM(r%fid,'eta_v',r%hgrid%eta_v,idjv)
  err = NF90_DEF_DIM(r%fid,'xi_psi',r%hgrid%xi_psi,idip)
  err = NF90_DEF_DIM(r%fid,'eta_psi',r%hgrid%eta_psi,idjp)
  err = NF90_DEF_DIM(r%fid,'one',1,id1)

  ! ... Variables:
  ! ...
  err = NF90_DEF_VAR(r%fid,'xl',NF90_DOUBLE,(/id1/),idxl)
  err = NF90_DEF_VAR(r%fid,'el',NF90_DOUBLE,(/id1/),idel)
  err = NF90_DEF_VAR(r%fid,'depthmin',NF90_DOUBLE,(/id1/),iddmin)
  err = NF90_DEF_VAR(r%fid,'depthmax',NF90_DOUBLE,(/id1/),iddmax)
  err = NF90_DEF_VAR(r%fid,'spherical',NF90_CHAR,(/id1/),idsph)
  err = NF90_DEF_VAR(r%fid,'angle',NF90_DOUBLE,(/idir,idjr/),idang)
  err = NF90_DEF_VAR(r%fid,'h',NF90_DOUBLE,(/idir,idjr/),idh)
  err = NF90_DEF_VAR(r%fid,'hraw',NF90_DOUBLE,(/idir,idjr/),idhraw)
  err = NF90_DEF_VAR(r%fid,'f',NF90_DOUBLE,(/idir,idjr/),idf)
  err = NF90_DEF_VAR(r%fid,'pm',NF90_DOUBLE,(/idir,idjr/),idpm)
  err = NF90_DEF_VAR(r%fid,'pn',NF90_DOUBLE,(/idir,idjr/),idpn)
  err = NF90_DEF_VAR(r%fid,'dndx',NF90_DOUBLE,(/idir,idjr/),iddndx)
  err = NF90_DEF_VAR(r%fid,'dmde',NF90_DOUBLE,(/idir,idjr/),iddndx)
  err = NF90_DEF_VAR(r%fid,'lon_rho',NF90_DOUBLE,(/idir,idjr/),idxr)
  err = NF90_DEF_VAR(r%fid,'lat_rho',NF90_DOUBLE,(/idir,idjr/),idyr)
  err = NF90_DEF_VAR(r%fid,'lon_u',NF90_DOUBLE,(/idiu,idju/),idxu)
  err = NF90_DEF_VAR(r%fid,'lat_u',NF90_DOUBLE,(/idiu,idju/),idyu)
  err = NF90_DEF_VAR(r%fid,'lon_v',NF90_DOUBLE,(/idiv,idjv/),idxv)
  err = NF90_DEF_VAR(r%fid,'lat_v',NF90_DOUBLE,(/idiv,idjv/),idyv)
  err = NF90_DEF_VAR(r%fid,'lon_psi',NF90_DOUBLE,(/idip,idjp/),idxp)
  err = NF90_DEF_VAR(r%fid,'lat_psi',NF90_DOUBLE,(/idip,idjp/),idyp)
  err = NF90_DEF_VAR(r%fid,'mask_rho',NF90_DOUBLE,(/idir,idjr/),idmr)
  err = NF90_DEF_VAR(r%fid,'mask_u',NF90_DOUBLE,(/idiu,idju/),idmu)
  err = NF90_DEF_VAR(r%fid,'mask_v',NF90_DOUBLE,(/idiv,idjv/),idmv)
  err = NF90_DEF_VAR(r%fid,'mask_psi',NF90_DOUBLE,(/idip,idjp/),idmp)


  err = NF90_ENDDEF(r%fid)

  !err = NF90_PUT_VAR(r%fid,idxl,[0.0d0])
  err = NF90_PUT_VAR(r%fid,idxl,r%hgrid%xl)
  err = NF90_PUT_VAR(r%fid,idel,r%hgrid%el)
  err = NF90_PUT_VAR(r%fid,idsph,r%hgrid%spherical)
  err = NF90_PUT_VAR(r%fid,idxr,r%hgrid%lon_rho)
  err = NF90_PUT_VAR(r%fid,idyr,r%hgrid%lat_rho)
  err = NF90_PUT_VAR(r%fid,idang,r%hgrid%angle)


  err = NF90_CLOSE(r%fid)


  return
  end subroutine roms_create_grid
  ! ...
  ! =========================================================================
  ! ...

end MODULE roms
