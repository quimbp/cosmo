! ****************************************************************************
! ... Module for variable types
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! ****************************************************************************

MODULE types

IMPLICIT NONE

PRIVATE
PUBLIC sp,dp

INTEGER, PARAMETER                 :: sp = KIND(0.0)    ! Single precision
INTEGER, PARAMETER                 :: dp = KIND(0.0D0)  ! Double precision

END MODULE types

