MODULE roms

use types
use constants
use netcdf
use dates
use cdf

implicit none

TYPE MHgrid
  integer                             :: xi_rho  = -1
  integer                             :: eta_rho = -1
  integer                             :: xi_u    = -1
  integer                             :: eta_u   = -1
  integer                             :: xi_v    = -1
  integer                             :: eta_v   = -1
  integer                             :: xi_psi  = -1
  integer                             :: eta_psi = -1
  real(dp), dimension(:,:), pointer   :: lon_rho
  real(dp), dimension(:,:), pointer   :: lat_rho
  real(dp), dimension(:,:), pointer   :: lon_u
  real(dp), dimension(:,:), pointer   :: lat_u
  real(dp), dimension(:,:), pointer   :: lon_v
  real(dp), dimension(:,:), pointer   :: lat_v
  real(dp), dimension(:,:), pointer   :: lon_psi
  real(dp), dimension(:,:), pointer   :: lat_psi
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
  integer                             :: N   = -1
  integer                             :: N_r = -1
  integer                             :: N_w = -1
  integer                             :: Vtransform
  integer                             :: Vstretching
  real(dp)                            :: theta_s
  real(dp)                            :: theta_b
  real(dp)                            :: Tcline
  real(dp)                            :: hc
  real(dp), dimension(:), pointer     :: s_r
  real(dp), dimension(:), pointer     :: s_w
  real(dp), dimension(:), pointer     :: Cs_r
  real(dp), dimension(:), pointer     :: Cs_w
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
END TYPE Mroms

CONTAINS
  ! ...
  ! =========================================================================
  ! ...
  subroutine roms_open (ifile,r,err)

  character(len=*), intent(in)        :: ifile
  type(Mroms), intent(inout)          :: r
  integer, intent(out)                :: err

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

  nullify(r%vgrid%s_r)
  nullify(r%vgrid%s_w)
  nullify(r%vgrid%Cs_r)
  nullify(r%vgrid%Cs_w)

  call roms_read_vgrid(r%fid,r%vgrid,err)

  allocate(r%vgrid%z0_r(r%hgrid%xi_rho, r%hgrid%eta_rho,   r%vgrid%N))
  allocate(r%vgrid%z0_w(r%hgrid%xi_rho, r%hgrid%eta_rho, 0:r%vgrid%N))
  allocate(r%vgrid%z_r (r%hgrid%xi_rho, r%hgrid%eta_rho,   r%vgrid%N))
  allocate(r%vgrid%z_w (r%hgrid%xi_rho, r%hgrid%eta_rho, 0:r%vgrid%N))
  allocate(r%vgrid%Hz  (r%hgrid%xi_rho, r%hgrid%eta_rho,   r%vgrid%N))

  call roms_set_depth(r%hgrid,r%vgrid)

  r%vgrid%z0_r(:,:,:) = r%vgrid%z_r(:,:,:)
  r%vgrid%z0_w(:,:,:) = r%vgrid%z_w(:,:,:)

  end subroutine roms_open
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

  allocate(vgrid%s_r(vgrid%N_r))
  allocate(vgrid%s_w(vgrid%N_w))
  allocate(vgrid%Cs_r(vgrid%N_r))
  allocate(vgrid%Cs_w(vgrid%N_w))

  err = NF90_INQ_VARID(fid,'Vtransform',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%Vtransform)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Vtransform not found'

  err = NF90_INQ_VARID(fid,'Vstretching',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%Vstretching)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Vstretching not found'

  err = NF90_INQ_VARID(fid,'theta_s',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%theta_s)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable theta_s not found'

  err = NF90_INQ_VARID(fid,'theta_b',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%theta_b)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable theta_b not found'

  err = NF90_INQ_VARID(fid,'Tcline',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%Tcline)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Tcline not found'

  err = NF90_INQ_VARID(fid,'hc',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%hc)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable hc not found'

  err = NF90_INQ_VARID(fid,'s_rho',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%s_r)
  write(*,*) NF90_STRERROR(err)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable s_rho not found'
  

  err = NF90_INQ_VARID(fid,'s_w',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%s_w)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable s_w not found'

  err = NF90_INQ_VARID(fid,'Cs_r',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%Cs_r)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Cs_r not found'

  err = NF90_INQ_VARID(fid,'Cs_w',ii)
  err = NF90_GET_VAR(fid,ii,vgrid%Cs_w)
  if (err.NE.NF90_NOERR) stop 'ERROR: Variable Cs_w not found'

  return

  end subroutine roms_read_vgrid
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
 
  if (vgrid%Vtransform.eq.1) then

    do j=1,hgrid%eta_rho
      do i=1,hgrid%xi_rho
        vgrid%z_w(i,j,0) = -hgrid%h(i,j)
      enddo
      do k=1,vgrid%N
        cff_r  = vgrid%hc*(vgrid%s_r(k)-vgrid%Cs_r(k))
        cff_w  = vgrid%hc*(vgrid%s_w(k)-vgrid%Cs_w(k))
        cff1_r = vgrid%Cs_r(k)
        cff1_w = vgrid%Cs_w(k)
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

  else if (vgrid%Vtransform.eq.2) then

    print*, 'Transform 2'
    do j=1,hgrid%eta_rho
      do i=1,hgrid%xi_rho
        vgrid%z_w(i,j,0) = -hgrid%h(i,j)
      enddo
      do k=1,vgrid%N
        cff_r  = vgrid%hc*vgrid%s_r(k)
        cff_w  = vgrid%hc*vgrid%s_w(k)
        cff1_r = vgrid%Cs_r(k)
        cff1_w = vgrid%Cs_w(k)
        do i=1,hgrid%xi_rho
          hwater = hgrid%h(i,j)
          hinv   = one / (vgrid%hc + hwater)
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

  err = NF90_INQ_VAR_FILL(r%fid,ii,filled,spv)

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

end MODULE roms
