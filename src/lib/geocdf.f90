! *****************************************************************************
! ... geocdf.f90
! ... Routines to simplify the lecture of the netcdf files containing 
! ... geophysical fields.
! ... COSMO project
! ... Quim Ballabrera, March 2017
! ... April, 2017: Add add_factor, scale_factor, fill_value and missing_value
! ... Version 0.1, released October 2017
! *****************************************************************************

module geocdf

use netcdf
use types, only: sp,dp
use utils, only: uppercase,menu,stop_error
use lineargs, only: argstr,argflg
use cdf, only: cdf_copyatts,cdf_error

implicit none
private

public gcdf
public gcdf_open
public gcdf_close
public gcdf_show
public gcdf_replicate
public gcdf_copy_var
public gcdf_getfield
public gcdf_replicate_head

type gcdf
  integer                                       :: fid=-1
  integer                                       :: idi=-1,idj=-1,idk=-1,idl=-1
  integer                                       :: idx=-1,idy=-1,idz=-1,idt=-1
  integer                                       :: nx=0,ny=0,nz=0,nt=0
  integer                                       :: fndims,fnvars,unlimid
  character(LEN=80)                             :: iname=''
  character(LEN=80)                             :: jname=''
  character(LEN=80)                             :: kname=''
  character(LEN=80)                             :: lname=''
  character(LEN=80)                             :: xname=''
  character(LEN=80)                             :: yname=''
  character(LEN=80)                             :: zname=''
  character(LEN=80)                             :: tname=''
  integer, dimension(:), pointer                :: dlen
  character(LEN=80), dimension(:), pointer      :: vnames
  integer, dimension(:), pointer                :: vndims
  integer, dimension(:), pointer                :: vtype
  integer(dp), dimension(:), pointer            :: size,tsize
  integer, dimension(:,:), pointer              :: dimids
  integer, dimension(:), pointer                :: ppi,ppj,ppk,ppl
  real(dp), dimension(:), pointer               :: add_offset
  real(dp), dimension(:), pointer               :: scale_factor
  real(dp), dimension(:), pointer               :: fill_value
  real(dp), dimension(:), pointer               :: missing_value
  logical, dimension(:), pointer                :: fill,missing
  logical, dimension(:), pointer                :: missingisnan
END TYPE gcdf

contains
! ...
! ============================================================================
! ...
subroutine gcdf_open (ifile,cdf,err)

character(LEN=*), INTENT(in)               :: ifile
TYPE(gcdf), INTENT(inout)                  :: cdf
integer, INTENT(out)                       :: err

integer(dp) nsize
integer fndims,fnvars,fnatts,unlimid
integer var,dim,itype,ndims,natts,dimids(10)
character(LEN=80) word

! ... First, get the variable names:
! ...
call gcdf_varnames (.false.,ifile,cdf,err)
if (err.NE.0) return

! ... Open file
! ...
write(*,*) 'Opening file ', trim(ifile)
err = NF90_OPEN(ifile,NF90_NOWRITE,cdf%fid)
if (err.NE.0) return

err = NF90_INQUIRE (cdf%fid,fndims,fnvars,fnatts,unlimid)
if (err.NE.0) return

cdf%fndims  = fndims
cdf%fnvars  = fnvars
cdf%unlimid = unlimid

cdf%idi=-1; cdf%idj=-1; cdf%idk=-1; cdf%idl=-1
if (len_trim(cdf%iname).GT.0) &
              err = NF90_INQ_DIMID (cdf%fid,trim(cdf%iname),cdf%idi)
if (len_trim(cdf%jname).GT.0) &
              err = NF90_INQ_DIMID (cdf%fid,trim(cdf%jname),cdf%idj)
if (len_trim(cdf%kname).GT.0) &
              err = NF90_INQ_DIMID (cdf%fid,trim(cdf%kname),cdf%idk)
if (len_trim(cdf%lname).GT.0) &
              err = NF90_INQ_DIMID (cdf%fid,trim(cdf%lname),cdf%idl)

cdf%nx=1; cdf%ny=1; cdf%nz=1; cdf%nt=1
if (cdf%idi.GT.0) err = NF90_INQUIRE_DIMENSION(cdf%fid,cdf%idi,len=cdf%nx)
if (cdf%idj.GT.0) err = NF90_INQUIRE_DIMENSION(cdf%fid,cdf%idj,len=cdf%ny)
if (cdf%idk.GT.0) err = NF90_INQUIRE_DIMENSION(cdf%fid,cdf%idk,len=cdf%nz)
if (cdf%idl.GT.0) err = NF90_INQUIRE_DIMENSION(cdf%fid,cdf%idl,len=cdf%nt)

cdf%idx=-1; cdf%idy=-1; cdf%idz=-1; cdf%idt=-1
if (len_trim(cdf%xname).GT.0) &
              err = NF90_INQ_VARID (cdf%fid,trim(cdf%xname),cdf%idx)
if (len_trim(cdf%yname).GT.0) &
              err = NF90_INQ_VARID (cdf%fid,trim(cdf%yname),cdf%idy)
if (len_trim(cdf%zname).GT.0) &
              err = NF90_INQ_VARID (cdf%fid,trim(cdf%zname),cdf%idz)
if (len_trim(cdf%tname).GT.0) &
              err = NF90_INQ_VARID (cdf%fid,trim(cdf%tname),cdf%idt)

allocate (cdf%dlen(fndims))
allocate (cdf%vnames(fnvars))
allocate (cdf%vtype(fnvars))
allocate (cdf%vndims(fnvars))
allocate (cdf%dimids(fndims,fnvars))
allocate (cdf%ppi(fnvars))
allocate (cdf%ppj(fnvars))
allocate (cdf%ppk(fnvars))
allocate (cdf%ppl(fnvars))
allocate (cdf%size(fnvars))
allocate (cdf%tsize(fnvars))

allocate(cdf%add_offset(fnvars))
allocate(cdf%scale_factor(fnvars))
allocate(cdf%fill_value(fnvars))
allocate(cdf%missing_value(fnvars))
allocate(cdf%fill(fnvars))
allocate(cdf%missing(fnvars))
allocate(cdf%missingisnan(fnvars))

cdf%vnames(:)   = ''
cdf%vtype(:)    = 0
cdf%vndims(:)   = 0
cdf%dimids(:,:) = -1
cdf%ppi(:)      = -1
cdf%ppj(:)      = -1
cdf%ppk(:)      = -1
cdf%ppl(:)      = -1
cdf%fill(:)     = .false.
cdf%missing(:)  = .false.
cdf%missingisnan(:)  = .false.

write(*,*) '====> Dimensions (name, size):'
do dim=1,fndims
  word = ''
  err = NF90_INQUIRE_DIMENSION (cdf%fid,dim,word,cdf%dlen(dim))
  write(*,'(T2,I2,T6,":  ",A,T30,I6)') dim, trim(word), cdf%dlen(dim)
  if (err.NE.0) return
enddo

write(*,*) '====> Variables (name,type,number of dims):'
do var=1,fnvars
  word = ''
  err = NF90_INQUIRE_VARIABLE (cdf%fid,var,word,itype,ndims,dimids,natts)
  write(*,'(T2,I2,T6,":  ",A,T34,I2,T40,10I2)') var,trim(word), itype,dimids(1:ndims)
  if (err.NE.0) return
  cdf%vnames(var) = trim(word)
  cdf%vtype(var)  = itype
  cdf%vndims(var) = ndims
  cdf%dimids(1:ndims,var) = dimids(1:ndims) 
  nsize = 1
  do dim=1,ndims
    if (dimids(dim).eq.cdf%idi) cdf%ppi(var) = dim
    if (dimids(dim).eq.cdf%idj) cdf%ppj(var) = dim
    if (dimids(dim).eq.cdf%idk) cdf%ppk(var) = dim
    if (dimids(dim).eq.cdf%idl) cdf%ppl(var) = dim
    nsize = nsize * cdf%dlen(dimids(dim))
  enddo
  cdf%tsize(var) = nsize
  cdf%size(var)  = nsize
  if (cdf%ppl(var).GT.0) cdf%size(var) = nsize/cdf%nt
  err = NF90_GET_ATT (cdf%fid,var,'add_offset',cdf%add_offset(var))
  if (err.ne.NF90_NOERR) cdf%add_offset(var) = 0.0_dp
  err = NF90_GET_ATT (cdf%fid,var,'scale_factor',cdf%scale_factor(var))
  if (err.ne.NF90_NOERR) cdf%scale_factor(var) = 1.0_dp
  err = NF90_GET_ATT (cdf%fid,var,'_FillValue',cdf%fill_value(var))
  if (err.eq.NF90_NOERR) cdf%fill(var) = .true.
  err = NF90_GET_ATT (cdf%fid,var,'missing_value',cdf%missing_value(var))
  if (err.eq.NF90_NOERR) cdf%missing(var) = .true.
  if (cdf%fill(var)) then
    if (isnan(cdf%fill_value(var))) cdf%missingisnan(var) = .true.
  else if (cdf%missing(var)) then
    if (isnan(cdf%missing_value(var))) cdf%missingisnan(var) = .true.
  endif
  
enddo

err = 0
return

end subroutine gcdf_open
! ...
! ======================================================================
! ...
subroutine gcdf_close(cdf,err)

type(gcdf), intent(inout)                  :: cdf
integer, intent(out)                       :: err

cdf%iname = ''; cdf%jname = ''; cdf%kname = ''; cdf%lname = '' 
cdf%xname = ''; cdf%yname = ''; cdf%zname = ''; cdf%tname = ''

deallocate (cdf%vnames)
deallocate (cdf%vtype)
deallocate (cdf%vndims)
deallocate (cdf%dimids)
deallocate (cdf%ppi)
deallocate (cdf%ppj)
deallocate (cdf%ppk)
deallocate (cdf%ppl)
deallocate (cdf%dlen)
deallocate (cdf%size)
deallocate (cdf%tsize)

err = NF90_CLOSE (cdf%fid)

cdf%fid = -1
cdf%idi = -1
cdf%idj = -1
cdf%idk = -1
cdf%idl = -1
cdf%idx = -1
cdf%idy = -1
cdf%idz = -1
cdf%idt = -1
cdf%nx  =  0
cdf%ny  =  0
cdf%nz  =  0
cdf%nt  =  0
cdf%fndims  =  0
cdf%fnvars  =  0
cdf%unlimid = -1

return
end subroutine gcdf_close
! ...
! ======================================================================
! ...
subroutine gcdf_show(cdf)

TYPE(gcdf), INTENT(in)                     :: cdf

integer i

write(*,*)
write(*,*) 'idi, idj, idk, idl = ', cdf%idi, cdf%idj, cdf%idk, cdf%idl
write(*,*) 'nx,  ny,  nz,  nt  = ', cdf%nx, cdf%ny, cdf%nz, cdf%nt
write(*,*)
do i=1,cdf%fnvars
  write(*,*)  trim(cdf%vnames(i))
  write(*,*) 'ppi, ppj, ppk, ppl = ',cdf%ppi(i),cdf%ppj(i),cdf%ppk(i),cdf%ppl(i)
enddo

return
end subroutine gcdf_show
! ...
! ======================================================================
! ...
subroutine gcdf_replicate(icdf,ofile,ocdf,err)

character(LEN=*), INTENT(in)               :: ofile
TYPE(gcdf), INTENT(in)                     :: icdf
TYPE(gcdf), INTENT(out)                    :: ocdf
integer, INTENT(out)                       :: err

integer dim,var,dlen,ii,ntype,ndims,dimids(10),natts,i
character(LEN=80) dname,vname

write(*,*)
write(*,*) 'Opening output file : ', trim(ofile)
err = NF90_CREATE(trim(ofile),NF90_CLOBBER,ocdf%fid) ! Overwrites
if (err.NE.NF90_NOERR) return

do dim=1,icdf%fndims
  dname = ''
  err = NF90_INQUIRE_DIMENSION(icdf%fid,dim,dname,dlen)
  if (dim.eq.icdf%unlimid) then
    err = NF90_DEF_DIM (ocdf%fid,trim(dname),NF90_UNLIMITED,ii)
    if (err.NE.NF90_NOERR) return
  else
    err = NF90_DEF_DIM (ocdf%fid,trim(dname),dlen,ii)
    ocdf%idl = dim
    if (err.NE.NF90_NOERR) return
  endif
enddo

ocdf%fndims = icdf%fndims
ocdf%idi = icdf%idi
ocdf%idj = icdf%idj
ocdf%idk = icdf%idk
ocdf%idl = icdf%idl
ocdf%nx  = icdf%nx
ocdf%ny  = icdf%ny
ocdf%nz  = icdf%nz
ocdf%nt  = icdf%nt
ocdf%unlimid = icdf%unlimid
ocdf%iname = trim(icdf%iname)
ocdf%jname = trim(icdf%jname)
ocdf%kname = trim(icdf%kname)
ocdf%lname = trim(icdf%lname)

allocate (ocdf%dlen(ocdf%fndims))
ocdf%dlen = icdf%dlen

do var=1,icdf%fnvars
  vname = ''
  err = NF90_INQUIRE_VARIABLE (icdf%fid,var,vname,ntype,ndims,dimids,natts)
  !if (ntype.eq.NF_USHORT) ntype = 3
  err = NF90_DEF_VAR (ocdf%fid,trim(vname),ntype,dimids(1:ndims),ii)
  call cdf_error (err,'Unable to define variable')
  call cdf_copyatts(.false.,icdf%fid,var,ocdf%fid,ii,natts)
enddo

ocdf%fnvars = icdf%fnvars
ocdf%idx    = icdf%idx
ocdf%idy    = icdf%idy
ocdf%idz    = icdf%idz
ocdf%idt    = icdf%idt
ocdf%xname = trim(icdf%xname)
ocdf%yname = trim(icdf%yname)
ocdf%zname = trim(icdf%zname)
ocdf%tname = trim(icdf%tname)

allocate (ocdf%vnames(ocdf%fnvars))
allocate (ocdf%vtype(ocdf%fnvars))
allocate (ocdf%vndims(ocdf%fnvars))
allocate (ocdf%ppi(ocdf%fnvars))
allocate (ocdf%ppj(ocdf%fnvars))
allocate (ocdf%ppk(ocdf%fnvars))
allocate (ocdf%ppl(ocdf%fnvars))
allocate (ocdf%dimids(ocdf%fndims,ocdf%fnvars))
allocate (ocdf%size(ocdf%fnvars))
allocate (ocdf%tsize(ocdf%fnvars))

do i=1,ocdf%fnvars
  ocdf%vnames(i)   = trim(icdf%vnames(i))
  ocdf%vtype(i)    = icdf%vtype(i)
  ocdf%vndims(i)   = icdf%vndims(i)
  ocdf%ppi(i)      = icdf%ppi(i)
  ocdf%ppj(i)      = icdf%ppj(i)
  ocdf%ppk(i)      = icdf%ppk(i)
  ocdf%ppl(i)      = icdf%ppl(i)
  ocdf%size(i)     = icdf%size(i)
  ocdf%tsize(i)    = icdf%tsize(i)
  ocdf%dimids(:,i) = icdf%dimids(:,i)
enddo

call cdf_copyatts(.false.,icdf%fid,0,ocdf%fid,0,natts)

err = NF90_ENDDEF (ocdf%fid)
call cdf_error (err,'Unable to leave def mode')

end subroutine gcdf_replicate 
! ...
! =============================================================================
! ...
subroutine gcdf_copy_var(icdf,idv,ocdf,odv,err)

TYPE(gcdf), INTENT(in)                     :: icdf
TYPE(gcdf), INTENT(in)                     :: ocdf
integer, INTENT(in)                        :: idv,odv
integer, INTENT(out)                       :: err

integer ndims,ntype,dimids(10),natts,step
integer(dp) nsize
integer, dimension(:), ALLOCATABLE     :: iwork,po,pf
real(sp), dimension(:), ALLOCATABLE    :: rwork
real(dp), dimension(:), ALLOCATABLE    :: dwork
character(LEN=80) word

err = -1
if (idv.LE.0) return
if (odv.LE.0) return

err = NF90_INQUIRE_VARIABLE (icdf%fid,idv,word,ntype,ndims,dimids,natts)

! ... Variable size
! ...
nsize = icdf%size(idv)

if (icdf%ppl(idv).LE.0) then ! No time in variable
  if (ntype.eq.NF90_DOUBLE) then
    allocate (dwork(nsize),stat=err)
    if (err.ne.0) call stop_error(1,'Unable to allocate variable')
    err = NF90_GET_VAR (icdf%fid,idv,dwork)
    call cdf_error (err,'Unable to read double variable')
    err = NF90_PUT_VAR (ocdf%fid,odv,dwork)
    call cdf_error (err,'Unable to write double variable')
    deallocate (dwork)
  else if (ntype.eq.NF90_FLOAT) then
    allocate (rwork(nsize),stat=err)
    if (err.ne.0) call stop_error(1,'Unable to allocate variable')
    err = NF90_GET_VAR (icdf%fid,idv,rwork)
    call cdf_error (err,'Unable to read real variable')
    err = NF90_PUT_VAR (ocdf%fid,odv,rwork)
    call cdf_error (err,'Unable to write real variable')
    deallocate (rwork)
  else if (ntype.eq.NF90_INT.OR.ntype.eq.NF90_SHORT) then
    allocate (iwork(nsize),stat=err)
    if (err.ne.0) call stop_error(1,'Unable to allocate variable')
    err = NF90_GET_VAR (icdf%fid,idv,iwork)
    call cdf_error (err,'Unable to read integer variable')
    err = NF90_PUT_VAR (ocdf%fid,odv,iwork)
    call cdf_error (err,'Unable to write integer variable')
    deallocate (iwork)
  else
    call cdf_error (1,'Type variable not tabulated')
  endif

else
  nsize = nsize / icdf%nt
  allocate (po(icdf%vndims(idv)))
  allocate (pf(icdf%vndims(idv)))
  po(:) = 1
  pf(:) = 1
  if (icdf%ppi(idv).GT.0) pf(icdf%ppi(idv)) = icdf%nx
  if (icdf%ppj(idv).GT.0) pf(icdf%ppj(idv)) = icdf%ny
  if (icdf%ppk(idv).GT.0) pf(icdf%ppk(idv)) = icdf%nz
  if (ntype.eq.NF90_DOUBLE) then
    allocate (dwork(nsize),stat=err)
    if (err.ne.0) call stop_error(1,'Unable to allocate variable')
    do step=1,icdf%nt
      po(icdf%ppl(idv)) = step
      err = NF90_GET_VAR (icdf%fid,idv,dwork,po,pf)
      call cdf_error (err,'Unable to read double variable')
      err = NF90_PUT_VAR (ocdf%fid,odv,dwork,po,pf)
      call cdf_error (err,'Unable to write double variable')
    enddo
    deallocate (dwork)
  else if (ntype.eq.NF90_FLOAT) then
    allocate (rwork(nsize),stat=err)
    if (err.ne.0) call stop_error(1,'Unable to allocate variable')
    do step=1,icdf%nt
      po(icdf%ppl(idv)) = step
      err = NF90_GET_VAR (icdf%fid,idv,rwork,po,pf)
      call cdf_error (err,'Unable to read real variable')
      err = NF90_PUT_VAR (ocdf%fid,odv,rwork,po,pf)
      call cdf_error (err,'Unable to write real variable')
    enddo
    deallocate (rwork)
  else if (ntype.eq.NF90_INT.OR.ntype.eq.NF90_SHORT) then
    allocate (iwork(nsize),stat=err)
    if (err.ne.0) call stop_error(1,'Unable to allocate variable')
    do step=1,icdf%nt
      po(icdf%ppl(idv)) = step
      err = NF90_GET_VAR (icdf%fid,idv,iwork,po,pf)
      call cdf_error (err,'Unable to read integer variable')
      err = NF90_PUT_VAR (ocdf%fid,odv,iwork,po,pf)
      call cdf_error (err,'Unable to write integer variable')
    enddo
    deallocate (iwork)
  else
    call cdf_error (1,'Type variable not tabulated')
  endif
endif

end subroutine gcdf_copy_var
! ...
! =======================================================================
! ...
subroutine gcdf_varnames(verb,ifile,cdf,err)

LOGICAL, INTENT(in)                        :: verb
character(LEN=*), INTENT(in)               :: ifile
TYPE(gcdf), INTENT(inout)                  :: cdf
integer, INTENT(out)                       :: err

LOGICAL fdi,fdj,fdk,fdl,fvx,fvy,fvz,fvt,nemo
integer idi,idj,idk,idl,idx,idy,idz,idt
integer fid,ndims,nvars,natts,unlimid,dim,var
integer ntype,dimids(10)
character(LEN=80) word

!cdf%iname = ''; cdf%jname = ''; cdf%kname = ''; cdf%lname = '' 
!cdf%xname = ''; cdf%yname = ''; cdf%zname = ''; cdf%tname = ''

! ... First, check if the user has specified the names:
! ...
fdi = .false.
fdj = .false.
fdk = .false.
fdl = .false.
fvx = .false.
fvy = .false.
fvz = .false.
fvt = .false.
nemo = .false.

call argflg("-nemo",nemo)

if (nemo) then
  cdf%iname = 'x'
  cdf%jname = 'y'
  cdf%kname = 'deptht'
  cdf%lname = 'time_counter'
  cdf%xname = 'nav_lon'
  cdf%yname = 'nav_lat'
  cdf%zname = 'deptht'
  cdf%tname = 'time_counter'
  return
else
  ! ... Overwrite any variable name by a user-specified name
  ! ...
  call argstr('-iname',fdi,cdf%iname)
  call argstr('-jname',fdj,cdf%jname)
  call argstr('-kname',fdk,cdf%kname)
  call argstr('-lname',fdl,cdf%lname)
  call argstr('-xname',fvx,cdf%xname)
  call argstr('-yname',fvy,cdf%yname)
  call argstr('-zname',fvz,cdf%zname)
  call argstr('-tname',fvt,cdf%tname)
endif

! ... Check file contents
! ... 
err = NF90_OPEN(ifile,NF90_NOWRITE,fid)
call cdf_error (err,'Unable to open file')

err = NF90_INQUIRE (fid,ndims,nvars,natts,unlimid)
call cdf_error (err,'Unable to inquire file')


idx = -1
if (len_trim(cdf%xname).gt.0) then
  err = NF90_INQ_VARID(fid,cdf%xname,idx)
  call cdf_error(err,'Variable '//trim(cdf%xname)//' not found')
else
  do var=1,nvars
    word = ''
    err  = NF90_INQUIRE_VARIABLE (fid,var,word,ntype,ndims,dimids,natts)
    word = uppercase(word)
    if (idx.LT.0.AND.word(1:1).eq.'X')       idx = var
    if (idx.LT.0.AND.word(1:3).eq.'LON')     idx = var
    if (idx.LT.0.AND.word(1:7).eq.'NAV_LON') idx = var
  enddo
  if (idx.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idx,name=cdf%xname)
endif
if (idx.gt.0) then
  err = NF90_INQUIRE_VARIABLE (fid,idx,ndims=ndims,dimids=dimids)
  call cdf_error(err,'Error inquiring lon variable')
  if (ndims.eq.1) then
    err = NF90_INQUIRE_DIMENSION(fid,dimids(1),name=cdf%iname)
   call cdf_error(err,'Error inquiring dimension')
  else
    call stop_error(1,'software not yet ready to use irregular grids')
  endif
endif

idy = -1
if (len_trim(cdf%yname).gt.0) then
  err = NF90_INQ_VARID(fid,cdf%yname,idy)
  call cdf_error(err,'Variable '//trim(cdf%yname)//' not found')
else
  do var=1,nvars
    word = ''
    err  = NF90_INQUIRE_VARIABLE (fid,var,word,ntype,ndims,dimids,natts)
    word = uppercase(word)
    if (idy.LT.0.AND.word(1:1).eq.'Y')       idy = var
    if (idy.LT.0.AND.word(1:3).eq.'LAT')     idy = var
    if (idy.LT.0.AND.word(1:7).eq.'NAV_LAT') idy = var
  enddo
  if (idy.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idy,name=cdf%yname)
endif
if (idy.gt.0) then
  err = NF90_INQUIRE_VARIABLE (fid,idy,ndims=ndims,dimids=dimids)
  call cdf_error(err,'Error inquiring lat variable')
  if (ndims.eq.1) then
    err = NF90_INQUIRE_DIMENSION(fid,dimids(1),name=cdf%jname)
   call cdf_error(err,'Error inquiring dimension')
  else
    call stop_error(1,'software not yet ready to use irregular grids')
  endif
endif

idz = -1
if (len_trim(cdf%zname).gt.0) then
  err = NF90_INQ_VARID(fid,cdf%zname,idz)
  call cdf_error(err,'Variable '//trim(cdf%zname)//' not found')
else
  do var=1,nvars
    word = ''
    err  = NF90_INQUIRE_VARIABLE (fid,var,word,ntype,ndims,dimids,natts)
    word = uppercase(word)
    if (idz.LT.0.AND.word(1:1).eq.'Z')       idz = var
    if (idz.LT.0.AND.word(1:3).eq.'DEP')     idz = var
  enddo
  if (idz.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idz,name=cdf%zname)
endif
if (idz.gt.0) then
  err = NF90_INQUIRE_VARIABLE (fid,idz,ndims=ndims,dimids=dimids)
  call cdf_error(err,'Error inquiring depth variable')
  if (ndims.eq.1) then
    err = NF90_INQUIRE_DIMENSION(fid,dimids(1),name=cdf%kname)
   call cdf_error(err,'Error inquiring dimension')
  else
    call stop_error(1,'Error: Only one depth dimenion allowed')
  endif
endif



idt = -1
if (len_trim(cdf%tname).gt.0) then
  err = NF90_INQ_VARID(fid,cdf%tname,idt)
  call cdf_error(err,'Variable '//trim(cdf%tname)//' not found')
else
  do var=1,nvars
    word = ''
    err  = NF90_INQUIRE_VARIABLE (fid,var,word,ntype,ndims,dimids,natts)
    word = uppercase(word)
    if (idt.LT.0.AND.word(1:1).eq.'T')       idt = var
    if (index(word,'TIME').GT.0)             idt = var
  enddo
  if (idt.GT.0) err = NF90_INQUIRE_VARIABLE (fid,idt,name=cdf%tname)
endif
if (idt.gt.0) then
  err = NF90_INQUIRE_VARIABLE (fid,idt,ndims=ndims,dimids=dimids)
  call cdf_error(err,'Error inquiring time variable')
  if (ndims.eq.1) then
    err = NF90_INQUIRE_DIMENSION(fid,dimids(1),name=cdf%lname)
   call cdf_error(err,'Error inquiring dimension')
  else
    call stop_error(1,'Error: Only one time dimenion allowed')
  endif
endif

if (verb) then
  write(*,*) 'Dimension names :'
  write(*,*) 'iname = ', trim(cdf%iname)
  write(*,*) 'jname = ', trim(cdf%jname)
  write(*,*) 'kname = ', trim(cdf%kname)
  write(*,*) 'lname = ', trim(cdf%lname)
  write(*,*) 'Variable names :'
  write(*,*) 'xname = ', trim(cdf%xname)
  write(*,*) 'yname = ', trim(cdf%yname)
  write(*,*) 'zname = ', trim(cdf%zname)
  write(*,*) 'tname = ', trim(cdf%tname)
endif

end subroutine gcdf_varnames
! ...
! ===========================================================================
! ...
subroutine gcdf_getfield(icdf,fvv,vname,idv,err)

TYPE(gcdf), INTENT(in)                     :: icdf
LOGICAL, INTENT(in)                        :: fvv
character(LEN=*), INTENT(out)              :: vname
integer, INTENT(out)                       :: idv,err

! ... Local variables
! ...
integer var,nv,vv,opt
integer, dimension(:), ALLOCATABLE               :: vmap
character(LEN=60), dimension(:), ALLOCATABLE     :: option

err = 1

! ... Check if vname is already set !!!
! ... In that case, get the variable ID.
! ...
if (fvv) then
  err = NF90_INQ_VARID (icdf%fid,trim(vname),idv)
  return
endif
vname = ''

! ... Check how many variables are not axes
! ...
nv = 0
do var=1,icdf%fnvars
  if (any((/icdf%idx,icdf%idy,icdf%idz,icdf%idt/).eq.var)) then
    ! ... Do nothing: variable is an axis
  else
    ! ... Not an axis
    nv = nv + 1
    if (nv.eq.1) idv = var
  endif
enddo

! ... If not variables, get back
! ...
if (nv.eq.0) return

! ... If only one variable is not an axe, that is the one
! ...
if (nv.eq.1) then
  vname = trim(icdf%vnames(idv))
  err   = 0
  return
endif

! ... If here, there are more than one variable:
! ...
allocate (option(nv+1))
allocate (vmap(nv+1))

vv = 0
do var=1,icdf%fnvars
  if (any((/icdf%idx,icdf%idy,icdf%idz,icdf%idt/).eq.var)) then
    ! ... Do nothing: variable is an axis
  else
    ! ... Not an axis
    vv = vv + 1
    option(vv) = trim(icdf%vnames(var))
    vmap(vv)    = var
  endif
enddo
option(nv+1) = 'Quit'
vmap(nv+1)   = 0

call menu('Select variable',option,nv+1,opt)
if (opt.LE.nv) then
  idv = vmap(opt)
  vname = trim(icdf%vnames(idv))
  err   = 0
endif

deallocate (option)
deallocate (vmap)

return
end subroutine gcdf_getfield
! ...
! =======================================================================
! ...
subroutine gcdf_replicate_head(icdf,ofile,ocdf,err)

character(LEN=*), INTENT(in)               :: ofile
TYPE(gcdf), INTENT(in)                     :: icdf
TYPE(gcdf), INTENT(out)                    :: ocdf
integer, INTENT(out)                       :: err

integer dim,var,dlen,ii,ntype,ndims,dimids(10),natts
character(LEN=80) dname,vname

write(*,*)
write(*,*) 'Opening output file : ', trim(ofile)
err = NF90_CREATE(trim(ofile),NF90_CLOBBER,ocdf%fid)  ! Overwrite
if (err.NE.NF90_NOERR) return

do dim=1,icdf%fndims
  dname = ''
  err = NF90_INQUIRE_DIMENSION(icdf%fid,dim,dname,dlen)
  if (dim.eq.icdf%unlimid) then
    err = NF90_DEF_DIM (ocdf%fid,trim(dname),NF90_UNLIMITED,ii)
    if (err.NE.NF90_NOERR) return
  else
    err = NF90_DEF_DIM (ocdf%fid,trim(dname),dlen,ii)
    ocdf%idl = dim
    if (err.NE.NF90_NOERR) return
  endif
enddo

ocdf%fndims = icdf%fndims
ocdf%idi = icdf%idi
ocdf%idj = icdf%idj
ocdf%idk = icdf%idk
ocdf%idl = icdf%idl
ocdf%nx  = icdf%nx
ocdf%ny  = icdf%ny
ocdf%nz  = icdf%nz
ocdf%nt  = icdf%nt
ocdf%unlimid = icdf%unlimid
ocdf%iname = trim(icdf%iname)
ocdf%jname = trim(icdf%jname)
ocdf%kname = trim(icdf%kname)
ocdf%lname = trim(icdf%lname)

allocate (ocdf%dlen(ocdf%fndims))
ocdf%dlen = icdf%dlen

ocdf%fnvars = 0
do var=1,icdf%fnvars
  vname = ''
  err = NF90_INQUIRE_VARIABLE (icdf%fid,var,vname,ntype,ndims,dimids,natts)
  !if (ntype.eq.NF_USHORT) ntype = 3
  if (any((/icdf%idx,icdf%idy,icdf%idz,icdf%idt/).eq.var)) then
    err = NF90_DEF_VAR (ocdf%fid,trim(vname),ntype,dimids(1:ndims),ii)
    call cdf_error (err,'Unable to define variable')
    if (var.NE.icdf%idt) call cdf_copyatts(.false.,icdf%fid,var,ocdf%fid,ii,natts)
    if (var.eq.icdf%idx) ocdf%idx = ii
    if (var.eq.icdf%idy) ocdf%idy = ii
    if (var.eq.icdf%idz) ocdf%idz = ii
    if (var.eq.icdf%idt) ocdf%idt = ii
    ocdf%fnvars = ocdf%fnvars + 1
  endif
enddo

ocdf%xname = trim(icdf%xname)
ocdf%yname = trim(icdf%yname)
ocdf%zname = trim(icdf%zname)
ocdf%tname = trim(icdf%tname)

err = NF90_ENDDEF (ocdf%fid)
call cdf_error (err,'Unable to leave def mode')

end subroutine gcdf_replicate_head
! ...
! ============================================================================
! ...
end module geocdf
