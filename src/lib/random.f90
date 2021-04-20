module module_random

use module_types, only: dp
use module_constants

contains
! ...
! =====================================================================
! =====================================================================
! ...
function randn (m) result(ff)

implicit none

integer, intent(in)                        :: m
real(dp), dimension(m)                     :: ff

! ... Local variables
! ...
integer i
real(dp), parameter                    :: s  =  0.449871D0
real(dp), parameter                    :: t  = -0.386595D0
real(dp), parameter                    :: a  =  0.19600D0
real(dp), parameter                    :: b  =  0.25472D0
real(dp), parameter                    :: r1 =  0.27597D0
real(dp), parameter                    :: r2 =  0.27846D0
real(dp) u,v,x,y,q

do i=1,m

!    call RANDOM_NUMBER(u)  ! GNU RANDOM GENERATOR
!    call RANDOM_NUMBER(v)  ! GNU RANDOM GENERATOR
!    ff(i) = sqrt(-2.0_dp*log(u)) * cos(2.0_dp*pi*v)
  
  do
    call RANDOM_NUMBER(u)  ! GNU RANDOM GENERATOR
    call RANDOM_NUMBER(v)  ! GNU RANDOM GENERATOR
    v = 1.7156D0 * (v - 0.5D0)

    ! ... Evaluate the quadratic form
    ! ...
    x = u - s
    y = ABS(v) - t
    q = x*x + y*(a*y - b*x)

    if (q .lt. r1) exit
    if (q .gt. r2) cycle
    if (v**2 .LT. -4D0*LOG(u)*u*u) exit
  enddo
  ff(i) = v/u

enddo

end function randn
! ...
! =====================================================================
! ...
function rndname(len,iseed) result(name)

character(len=len)             :: name
integer, optional              :: iseed

! ... Local variables
integer i,io,il,j,n
integer, dimension(:), allocatable :: seed
real(dp) r

if (present(iseed)) then
  call random_seed(size=n)
  allocate(seed(n))
  seed(:) = iseed
  call random_seed(put=seed)
endif

io = ichar('A')
il = ichar('Z') - io

do i=1,len
  call random_number(r)
  j = int(io + il*r)
  name(i:i) = char(j)
enddo

return
end function rndname
! ...
! =====================================================================
! ...
end module module_random
