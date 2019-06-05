! ***************************************************************************
! ... udf.f90
! ... Quim Ballabrera, October 2017
! ... COSMO Lagrangian model
! ... User defined function
! ... Version 0.1, released October 2017
! ***************************************************************************

pure function udf(n,p) result(f)

use cosmo

integer, intent(in)                  :: n
real(dp), dimension(n), intent(in)   :: p
real(dp)                             :: f

! ... Density = sigma0(Temp,Salt)
! ... Temp = p(1)
! ... Salt = p(2)
! ...
! f = sigma0(p(1),p(2)) 

! ... Constant value = C, eg. missing, nan, etc.
! ... C = p(1)
! ...
!  f = p(1)

! ... Analytical function = 0.5 * (x^2 + y^2)
! ... x = p(1)
! ... y = p(2)
! ...
  f = half*(p(1)*p(1)+p(2)*p(2))

end function udf
