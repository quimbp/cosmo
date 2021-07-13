! ****************************************************************************
! ... Module for variable types
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! ****************************************************************************

module module_types

implicit none

public sp,dp,qp,maxlen

integer, parameter                 :: sp = kind(0.0)    ! Single precision
integer, parameter                 :: dp = kind(0.0D0)  ! Double precision
integer, parameter                 :: qp = kind(0.0_16) ! Quadruple precision


integer,  parameter                :: maxlen  = 180     ! Max char length

end module module_types

