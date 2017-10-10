! ****************************************************************************
! ... Module for variable types
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ****************************************************************************

MODULE constants

USE types, ONLY: sp,dp

IMPLICIT NONE

PRIVATE
PUBLIC pi,e_,nan,inf,deg2rad,rad2deg,i_,dpi,hpi
PUBLIC zero,one,half,two,ten,hundred
PUBLIC nan4,inf4
PUBLIC grav,Rearth,Omega

! ... Mathematical constants
! ...
REAL(dp), PARAMETER    :: zero    = 0.0_dp
REAL(dp), PARAMETER    :: one     = 1.0_dp
REAL(dp), PARAMETER    :: two     = 2.0_dp
REAL(dp), PARAMETER    :: half    = 0.5_dp
REAL(dp), PARAMETER    :: ten     = 10.0_dp
REAL(dp), PARAMETER    :: hundred = 100.0_dp
REAL(dp), PARAMETER    :: pi      = 3.1415926535897932384626433832795_dp
REAL(dp), PARAMETER    :: dpi     = 2.0_dp*pi
REAL(dp), PARAMETER    :: hpi     = 0.5_dp*pi
REAL(dp), PARAMETER    :: e_      = 2.7182818284590452353602874713527_dp
REAL(dp), PARAMETER    :: nan     = 0.0_dp/0.0_dp
REAL(dp), PARAMETER    :: inf     = 1.0_dp/0.0_dp
REAL(DP), PARAMETER    :: deg2rad = pi/180.0_dp
REAL(DP), PARAMETER    :: rad2deg = 180.0_dp/pi
COMPLEX(dp), PARAMETER :: i_      = (0.0_dp, 1.0_dp)

REAL(sp), PARAMETER    :: nan4    = 0.0_sp/0.0_sp
REAL(sp), PARAMETER    :: inf4    = 1.0_sp/0.0_sp

! ... Physical constants
! ...
REAL(DP), PARAMETER    :: grav    = 9.80665_dp     ! m / s^2
REAL(DP), PARAMETER    :: Rearth  = 6371229.0_dp   ! m
REAL(DP), PARAMETER    :: Omega   = 7.292E-5_dp    ! 1/s

END MODULE constants

