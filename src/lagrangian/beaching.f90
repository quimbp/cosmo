logical pure function beaching (xo,yo)

use lagrangian

implicit none

real(dp), intent(in)     :: xo,yo

! ... Local variable
! ...
integer ii,jj

call closest_gp (xo,yo,ii,jj)
beaching = land(ii,jj)

contains
  ! ...
  ! =======================================================
  ! ...
  pure subroutine closest_gp (xo,yo,io,jo)

  implicit none

  real(dp), intent(in)             :: xo,yo
  integer, intent(out)             :: io,jo

  integer iref,jref,imin(1)
  real(dp) d(4)

  iref = locate(xm,xo)
  jref = locate(ym,yo)

  d(1) = (xm(iref)-xo)**2   + (ym(jref)-yo)**2
  d(2) = (xm(iref+1)-xo)**2 + (ym(jref)-yo)**2
  d(3) = (xm(iref+1)-xo)**2 + (ym(jref+1)-yo)**2
  d(4) = (xm(iref)-xo)**2   + (ym(jref+1)-yo)**2

  imin = minloc(d)
  select case (imin(1))
  case (1)
    io = iref
    jo = jref
  case (2)
    io = iref + 1
    jo = jref
  case (3)
    io = iref + 1
    jo = jref + 1
  case (4)
    io = iref
    jo = jref + 1
  end select
  
  return

  end subroutine closest_gp

end function beaching
