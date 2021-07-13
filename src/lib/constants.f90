! ****************************************************************************
! ... Module for variable types
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! ****************************************************************************

module module_constants

use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan, ieee_positive_inf

use module_types, only: sp,dp

implicit none

! ... Logical constants
! ...
logical, parameter     :: True    = .true.
logical, parameter     :: False   = .false.

! ... Mathematical constants
! ...
real(dp), parameter    :: minus   =-1.0_dp
real(dp), parameter    :: zero    = 0.0_dp
real(dp), parameter    :: one     = 1.0_dp
real(dp), parameter    :: two     = 2.0_dp
real(dp), parameter    :: three   = 3.0_dp
real(dp), parameter    :: four    = 4.0_dp
real(dp), parameter    :: five    = 5.0_dp
real(dp), parameter    :: six     = 6.0_dp
real(dp), parameter    :: seven   = 7.0_dp
real(dp), parameter    :: eight   = 8.0_dp
real(dp), parameter    :: nine    = 9.0_dp
real(dp), parameter    :: ten     = 10.0_dp
real(dp), parameter    :: half    = 0.5_dp
real(dp), parameter    :: quarter = 0.25_dp
real(dp), parameter    :: hundred = 100.0_dp
real(dp), parameter    :: pi      = 3.1415926535897932384626433832795_dp
real(dp), parameter    :: dpi     = 2.0_dp*pi
real(dp), parameter    :: hpi     = 0.5_dp*pi
real(dp), parameter    :: rpi     = 1.0D0/pi
real(dp), parameter    :: e_      = 2.7182818284590452353602874713527_dp
real(DP), parameter    :: deg2rad = pi/180.0_dp
real(DP), parameter    :: rad2deg = 180.0_dp/pi
COMPLEX(dp), parameter :: zj      = (0.0_dp, 1.0_dp)

! ... Nan and Inf
! ...
real(sp), parameter    :: nan4    = 0.0_sp/0.0_sp
real(sp), parameter    :: inf4    = 1.0_sp/0.0_sp
real(dp), parameter    :: nan     = 0.0_dp/0.0_dp
real(dp), parameter    :: inf     = 1.0_dp/0.0_dp

! ... Physical constants
! ...
real(dp), parameter    :: grav    = 9.80665_dp     ! m / s^2
real(dp), parameter    :: Omega   = 7.292E-5_dp    ! 1/s
real(dp), parameter    :: Rearth  = 6371315.0_dp   ! m


end module module_constants
