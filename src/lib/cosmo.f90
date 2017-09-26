! COSMO module
! Quim Ballabrera, March 2017

module cosmo

  use types, only: sp,dp
  use constants
  use utils
  use lineargs
  use help
  use cdf
  use geocdf
  use math
  use runge_kutta
  use dates
  use grids
  use tokens

  implicit none

  INTEGER, DIMENSION(12)               :: dpm,cpm,dpml,cpml
  DATA dpm /31,28,31,30,31,30,31,31,30,31,30,31/
  DATA cpm /0,31,59,90,120,151,181,212,243,273,304,334/
  DATA dpml /31,29,31,30,31,30,31,31,30,31,30,31/
  DATA cpml /0,31,60,91,121,152,182,213,244,274,305,335/

END MODULE cosmo
