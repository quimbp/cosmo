! ****************************************************************************
! ... Ocean utilitites
! ... COSMO Project
! ... Quim Ballabrera, October 2017
! ****************************************************************************

module ocean

use types, only: dp

implicit none
private
public sigma0

contains
! ...
! =============================================================================
! ...
pure function sigma0 (t,s) result(theta)

! ... Function to calculate density with temperature and salinity
! ... Uses sigma-0 so does not work for all data
! ... Modified after CMS v 2.0 Paris et al., March 2017.
! ...
real(dp), intent(in)                 :: t    ! temperature
real(dp), intent(in)                 :: s    ! salinity
real(dp)                             :: theta

! ... Coefficients for sigma-0 (based on Brydon & Sun fit)
! ...
real(dp), parameter                  :: thbase = 1000.0_dp
real(dp), parameter                  :: c1 =-1.36471D-01, &
                                        c2 = 4.68181D-02, &
                                        c3 = 8.07004D-01, &
                                        c4 =-7.45353D-03, &
                                        c5 =-2.94418D-03, &
                                        c6 = 3.43570D-05, &
                                        c7 = 3.48658D-05

theta =  c1 + c3*s + t*(c2+c5*s+t*(c4+c7*s+c6*t))
theta = thbase + theta

end function sigma0
! ...
! =============================================================================
! ...
end module ocean
