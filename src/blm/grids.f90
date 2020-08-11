! *********************************************************************
! ... grids.f90
! *********************************************************************

module grids

use cosmo


type field
  ! This structure contains the necessary information for a
  ! variable to be read and processed
  integer                               :: fid               ! ncdf file id
  integer                               :: id                ! ncdf Var id
  integer                               :: nx
  integer                               :: ny
  integer                               :: nz
  integer                               :: ndims
  integer                               :: ppi
  integer                               :: ppj
  integer                               :: ppk
  integer                               :: ppl
  real(dp)                              :: time
  logical                               :: missingdefined
  logical                               :: missingisnan
  real(dp)                              :: missing_value
  real(dp)                              :: add_offset
  real(dp)                              :: scale_factor
  real(dp), dimension(:,:,:), pointer   :: data
end type field


! ... Input grids:
! ...
type cdf_vgrid
  logical                               :: defined  = .false.
  integer                               :: fid      = -1
  integer                               :: ndims    = -1
  integer                               :: idx      = -1
  integer                               :: idy      = -1
  integer                               :: idz      = -1
  integer                               :: idt      = -1
  integer                               :: idvel    = -1
  integer                               :: nx       =  1
  integer                               :: ny       =  1
  integer                               :: nz       =  1
  integer                               :: nt       =  1
  character(len=maxlen)                 :: filename = ''
  character(len=80)                     :: varname  = ''
  character(len=80)                     :: xname    = ''
  character(len=80)                     :: yname    = ''
  character(len=80)                     :: zname    = ''
  character(len=80)                     :: tname    = ''
  character(len=80)                     :: calendar = ''
  integer, dimension(:), pointer        :: po
  integer, dimension(:), pointer        :: pf
  real(dp), dimension(:), pointer       :: x
  real(dp), dimension(:), pointer       :: y
  real(dp), dimension(:), pointer       :: z
  real(dp), dimension(:), pointer       :: t
  real(dp), dimension(:), pointer       :: xm
  real(dp), dimension(:), pointer       :: ym
  real(dp)                              :: time_ref
  type(date_type)                       :: date_ref
  type(field)                           :: vel
  logical, dimension(:,:,:), pointer    :: land
  logical, dimension(:,:,:), pointer    :: sea
end type cdf_vgrid

type cdf_tgrid
  logical                               :: defined  = .false.
  integer                               :: fid      = -1
  integer                               :: ndims    = -1
  integer                               :: idx      = -1
  integer                               :: idy      = -1
  integer                               :: idz      = -1
  integer                               :: idt      = -1
  integer                               :: idtemp   = -1
  integer                               :: idsalt   = -1
  integer                               :: iddens   = -1
  integer                               :: nx       =  1
  integer                               :: ny       =  1
  integer                               :: nz       =  1
  integer                               :: nt       =  1
  character(len=maxlen)                 :: filename = ''
  character(len=80)                     :: tempname = ''
  character(len=80)                     :: saltname = ''
  character(len=80)                     :: densname = ''
  character(len=80)                     :: xname    = ''
  character(len=80)                     :: yname    = ''
  character(len=80)                     :: zname    = ''
  character(len=80)                     :: tname    = ''
  character(len=80)                     :: calendar = ''
  integer, dimension(:), pointer        :: po
  integer, dimension(:), pointer        :: pf
  real(dp), dimension(:), pointer       :: x
  real(dp), dimension(:), pointer       :: y
  real(dp), dimension(:), pointer       :: z
  real(dp), dimension(:), pointer       :: t
  real(dp), dimension(:), pointer       :: xm
  real(dp), dimension(:), pointer       :: ym
  real(dp)                              :: time_ref
  type(date_type)                       :: date_ref
  type(field)                           :: temp
  type(field)                           :: salt
  type(field)                           :: dens
  logical, dimension(:,:,:), pointer    :: land
  logical, dimension(:,:,:), pointer    :: sea
end type cdf_tgrid


end module grids
