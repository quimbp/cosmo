! ****************************************************************************
! ... Module for variable types
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! ****************************************************************************

module types

implicit none

private
public sp,dp

integer, parameter                 :: sp = kind(0.0)    ! Single precision
integer, parameter                 :: dp = kind(0.0D0)  ! Double precision

end module types

