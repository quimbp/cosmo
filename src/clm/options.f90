module module_options

use cosmo

implicit none


character(len=*), parameter  :: version   = 'v3.0'
character(len=*), parameter  :: author    = 'Quim Ballabrera'

logical                      :: fhlp      = .False.
type(type_help)              :: HLP

! ... Input files:
! ...
logical                      :: verb      = .False.
logical                      :: withOcex  = .False.
logical                      :: withOcey  = .False.
character(maxlen)            :: OcexFname = ""
character(maxlen)            :: OceyFname = ""
character(maxlen)            :: OcexVname = "u"
character(maxlen)            :: OceyVname = "v"

logical                      :: withAtmx  = .False.
logical                      :: withAtmy  = .False.
character(maxlen)            :: AtmxFname = ""
character(maxlen)            :: AtmyFname = ""
character(maxlen)            :: AtmxVname = "eastward_wind"
character(maxlen)            :: AtmyVname = "northward_wind"

logical                      :: withAtmxMaskName  = .True.
logical                      :: withAtmxMaskValue = .True.
character(maxlen)            :: AtmxMaskName  = "surface_type"
integer                      :: AtmxMaskValue = 1

logical                      :: withAtmyMaskName  = .True.
logical                      :: withAtmyMaskValue = .True.
character(maxlen)            :: AtmyMaskName  = "surface_type"
integer                      :: AtmyMaskValue = 1

logical                      :: withRelease = .False.
character(maxlen)            :: releaseName = ""

! ... The command-line specified options
! ...
logical                      :: withObservation = .False.
logical                      :: fxo = .False.
logical                      :: fyo = .False.
logical                      :: fzo = .False.
logical                      :: fto = .False.
real(dp)                     :: Release_xo
real(dp)                     :: Release_yo
real(dp)                     :: Release_zo = 0.0D0
real(dp)                     :: Release_to = 0.0D0
type(type_date)              :: Release_do
character(maxlen)            :: Release_tstr = ""

logical                      :: withA11 = .False.
logical                      :: withA12 = .False.
logical                      :: withA21 = .False.
logical                      :: withA22 = .False.
real(dp)                     :: userA11 = 0.0D0
real(dp)                     :: userA12 = 0.0D0
real(dp)                     :: userA21 = 0.0D0
real(dp)                     :: userA22 = 0.0D0

! ... Cropping area:
! ...
logical                      :: CropXmin  = .False.
logical                      :: CropXmax  = .False.
logical                      :: CropYmin  = .False.
logical                      :: CropYmax  = .False.
real(dp)                     :: userXmin  
real(dp)                     :: userXmax  
real(dp)                     :: userYmin  
real(dp)                     :: userYmax  

! ... Reference time and simulation length
! ...
logical                      :: withTini  = .False.
logical                      :: withTlen  = .False.
logical                      :: withReverse = .False.
integer                      :: reverse   = 1
real(dp)                     :: userTini  = 0.0D0
real(dp)                     :: userTlen  = 0.0D0       ! days
type(type_date)              :: userDini 

! ... Runge-Kutta time-step
! ...
logical                      :: withRKdt  = .False.
real(dp)                     :: userRKdt  = 600.0_dp   ! s (10 minutes)

! ... Output filename
! ...
logical                      :: withOutFile   = .False.
logical                      :: withFinalFile = .False.
character(maxlen)            :: Oname         = "clm-out.nc"
character(maxlen)            :: FinalName     = "clm-final.dat"

logical                      :: withSaveInt = .False.
integer                      :: SaveInt = 3600

! ... Random terms
! ...
logical                      :: WithNoiseMul
logical                      :: WithNoiseAdd
logical                      :: WithWsf
real(dp)                     :: userNoise_mul = 0.0_dp
real(dp)                     :: userNoise_add = 0.0_dp
real(dp)                     :: userWsf       = 1.0_dp

! ... Ensemble
! ...
logical                      :: withRandom
logical                      :: withRx
logical                      :: withRy
integer                      :: userNrandom
real(dp)                     :: userRx
real(dp)                     :: userRy
contains
! ...
! ====================================================================
! ...
subroutine read_options()

integer na,i
character(len=4000)                     :: OUlist=''
character(len=4000)                     :: OVlist=''
character(len=4000)                     :: AUlist=''
character(len=4000)                     :: AVlist=''
character(len=maxlen)                   :: tlen,word


! ... Fill in the help information
! ...
call program_help(HLP,version,author)

na = lineargs()

call argflg('-he',fhlp)
call argflg('--he',fhlp)

if (fhlp) then
  call HLP%write()
  stop -1
endif

call argflg('-ver',verb)
call argstr('-rel',withRelease,releaseName)
call argstr('-traj',withOutFile,Oname)
call argstr('-out',withOutFile,Oname)
call argstr('-rel',withRelease,releaseName)
call argstr('-ini',withRelease,releaseName)
call argstr('-end',withFinalFile,FinalName)
call argstr('-fin',withFinalFile,FinalName)

call argdbl('-xo',fxo,Release_xo)
call argdbl('-yo',fyo,Release_yo)
call argdbl('-zo',fzo,Release_zo)
call argstr('-to',fto,Release_tstr)

call arglst('-OU',withOcex,OUlist)
call arglst('-OV',withOcey,OVlist)
call arglst('-AU',withAtmx,AUlist)
call arglst('-AV',withAtmy,AVlist)

call argdbl('-a11',withA11,userA11)
call argdbl('-A11',withA11,userA11)
call argdbl('-a12',withA12,userA12)
call argdbl('-A12',withA12,userA12)
call argdbl('-a21',withA21,userA21)
call argdbl('-A21',withA21,userA21)
call argdbl('-a22',withA22,userA22)
call argdbl('-A22',withA22,userA22)

if (.not.withOcex) call stop_error(1,'Error. Options -OU is required')

if (count((/withAtmx,withAtmy/)).eq.1) &
   call stop_error(1,'Error. None or both -AU and -AV options are required')

OcexFname = token_read(OUlist,'file=')
word = token_read(OUlist,'var='); if (len_trim(word).gt.0) OcexVname = trim(word)

if (withOcey) then
  OceyFname = token_read(OVlist,'file='); if (len_trim(OceyFname).eq.0) OceyFname = trim(OcexFname)
  word = token_read(OVlist,'var='); if (len_trim(word).gt.0) OceyVname = trim(word)
else
  OceyFname = trim(OcexFname)
endif


if (withAtmx) then
  AtmxFname = token_read(AUlist,'file=')
  word = token_read(AUlist,'var='); if (len_trim(word).gt.0) AtmxVname = trim(word)

!  word = token_read(AUlist,'maskvar=') 
!  if (len_trim(word).gt.0) then
!    withAtmxMaskName = .True.
!    AtmxMaskName = trim(word)
!  endif
!  word = token_read(AUlist,'maskval=') 
!  if (len_trim(word).gt.0) then
!    withAtmxMaskValue = .True.
!    read(word,*) AtmxMaskValue
!  endif

  if (withAtmy) then  
    AtmyFname = token_read(AVlist,'file='); if (len_trim(AtmyFname).eq.0) AtmyFname = trim(AtmxFname)
    word = token_read(AVlist,'var='); if (len_trim(word).gt.0) AtmyVname = trim(word)
!    word = token_read(AVlist,'maskvar=') 
!    if (len_trim(word).gt.0) then
!      withAtmyMaskName = .True.
!      AtmyMaskName = trim(word)
!    else if (withAtmxMaskName) then
!      withAtmyMaskName = .True.
!      AtmyMaskName = trim(AtmxMaskName)
!    endif
!    word = token_read(AVlist,'maskval=') 
!    if (len_trim(word).gt.0) then
!      withAtmyMaskValue = .True.
!      read(word,*) AtmyMaskValue
!    else if (withAtmyMaskValue) then
!      withAtmyMaskValue = .True.
!      AtmyMaskValue = AtmxMaskValue
!    endif
  else
    AtmyFname = trim(AtmxFname)
  endif

  if (count([withAtmxMaskName,withAtmxMaskValue]).eq.1) &
    call stop_error(1,'Atmospheric mask specification requires both maskvar and maskval')
  if (count([withAtmyMaskName,withAtmyMaskValue]).eq.1) &
    call stop_error(1,'Atmospheric mask specification requires both maskvar and maskval')

endif

! ... Cropping model domain
! ...
call argdbl('-xmin',CropXmin,UserXmin)
call argdbl('-xmax',CropXmax,UserXmax)
call argdbl('-ymin',CropYmin,UserYmin)
call argdbl('-ymax',CropYmax,UserYmax)

! ... Initial time
! ...
call argstr('-from',withTini,word)
if (withTini) then
  userDini = strptime(word)
  userTini = userDini%jd()
else
  write(*,*) 'No initial date proposed by the user'
  userTini = 0.0D0
endif

call argstr('-for',withTlen,tlen)
if (withTlen) then
  if (is_numeric(tlen)) then
    ! ... No units provided. Default units "days"
    ! ...
    read(tlen,*) userTlen                ! Simulation length given in days
  else
    tlen = uppercase(tlen)
    i = index(tlen,'D')
    if (i.gt.0) then
      read(tlen(1:i-1),*) userTlen       ! Simulation length given in days
    else
      i = index(tlen,'H')
      if (i.gt.0) then
        read(tlen(1:i-1),*) userTlen
        userTlen = userTlen/24.0_dp      ! Simulation length given in hours
      else
        i = index(tlen,'S')
        if (i.gt.0) then
          read(tlen(1:i-1),*) userTlen
          userTlen = userTlen/86400.0_dp ! Simulation length given in seconds
        else
          stop 'Invalid duration units. Use only D,H,S'
        endif
      endif
    endif
  endif
endif

! ... Release positions
! ...
if (count([fxo,fyo]).eq.1) call stop_error(1,'Both options -xo and -yo must be used')
if (fxo) withObservation = .True.
if (count([withObservation,withRelease]).eq.0) call stop_error(1,'Missing release information')
if (count([withObservation,withRelease]).ne.1) call stop_error(1,'Incompatible release information')

! ... Runge-Kutta time step
! ... 
call argdbl('-dt',withRKdt,userRKdt)


! ... Reverse integration
! ... By default, reverse = 1
! ...
call argflg('-rev',withReverse)
if (withReverse) reverse = -1

! ... Observations
! ...

! ... Random terms
! ...
call argdbl('-mu',withNoiseMul,userNoise_mul)
call argdbl('-va',withNoiseAdd,userNoise_add)
call argdbl('-alpha',withWsf,userWsf)
  
! ... Ensemble
! ...
call argint('-random',withRandom,userNrandom)
call argint('-cloud',withRandom,userNrandom)
call argint('-ensemble',withRandom,userNrandom)
call argdbl('-Rx',withRx,userRx)
call argdbl('-RX',withRx,userRx)
call argdbl('-rx',withRx,userRx)
call argdbl('-rX',withRx,userRx)
call argdbl('-Ry',withRy,userRy)
call argdbl('-RY',withRy,userRy)
call argdbl('-ry',withRy,userRy)
call argdbl('-rY',withRy,userRy)

! ... Output frequency
! ...
call argint('-saveint',withSaveInt,SaveInt)


! ... Check options
! ...
call checkopts()

end subroutine read_options
! ...
! ====================================================================
! ...
subroutine view_options(iu)

integer, intent(in)                        :: iu

write(iu,*) 'Ocean velocity fields'
write(iu,*) '---------------------'
write(iu,'(T2,A)') 'Zonal = '//trim(OcexFname)//':'//trim(OcexVname)
write(iu,'(T2,A)') 'Meridional = '//trim(OceyFname)//':'//trim(OceyVname)
write(iu,*)
write(iu,*) 'Atmosphere velocity fields'
write(iu,*) '--------------------------'
if (withAtmx) then
  write(iu,'(T2,A)') 'Zonal = '//trim(AtmxFname)//':'//trim(AtmxVname)
  if (withAtmxMaskName) then
  write(iu,*) '  mask variable = ', trim(AtmxMaskName)
  write(iu,*) '  mask value = ', AtmxMaskValue
  endif
  write(iu,'(T2,A)') 'Meridional = '//trim(AtmyFname)//':'//trim(AtmyVname)
  if (withAtmyMaskName) then
  write(iu,*) '  mask variable = ', trim(AtmyMaskName)
  write(iu,*) '  mask value = ', AtmyMaskValue
  endif


else
  write(iu,'(T2,A)') 'Zonal = '
  write(iu,'(T2,A)') 'Meridional = '
endif

write(iu,*) 
write(iu,*) 'Model domain user options'
write(iu,*) '-----------------------------'
if (CropXmin) write(iu,*) 'Longitude min = ', userXmin
if (CropXmax) write(iu,*) 'Longitude max = ', userXmax
if (CropYmin) write(iu,*) 'Latitude min = ', userYmin
if (CropYmax) write(iu,*) 'Latitude max = ', userYmax

write(iu,*) 
write(iu,*) 'Model simulation user options'
write(iu,*) '-----------------------------'
if (withReverse) then
  write(iu,*) 'Backward time integration'
else
  write(iu,*) 'Forward time integration'
endif
if (withTini) write(iu,*) 'Initial time = ', UserDini%iso(), UserTini, ' (jd)'
if (withRKdt) write(iu,*) 'Time stepime = ', UserRKdt, '(s)'
if (withTlen) write(iu,*) 'Integration time = ', UserTlen, '(d)', nint(86400*UserTlen), ' (s)'
              write(iu,*) 'Saving interval  = ', SaveInt, ' (seconds)'
if (withOutfile) write(iu,*) 'Output trajectory file = ', trim(Oname)

write(iu,*) 
write(iu,*) 'Float release user options'
write(iu,*) '--------------------------'
if (withRelease) write(iu,*) 'Float release file = ', trim(ReleaseName)
if (withFinalfile) write(iu,*) 'Final float position file = ', trim(FinalName)



end subroutine view_options
! ...
! ====================================================================
! ...
end module module_options

