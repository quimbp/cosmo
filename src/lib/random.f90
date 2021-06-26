module module_random

use module_types, only: dp
use module_constants

interface randn
  module procedure randn_r,randn_v,randn_a
end interface randn

contains
! ...
! =====================================================================
! ...
function randn_r () result(ff)

  real(dp)                                   :: ff

  ! ... Local variables
  ! ...
  real(dp), parameter                    :: s  =  0.449871D0
  real(dp), parameter                    :: t  = -0.386595D0
  real(dp), parameter                    :: a  =  0.19600D0
  real(dp), parameter                    :: b  =  0.25472D0
  real(dp), parameter                    :: r1 =  0.27597D0
  real(dp), parameter                    :: r2 =  0.27846D0
  real(dp) u,v,x,y,q

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
  ff = v/u

end function randn_r
! ...
! =====================================================================
! ...
function randn_v (m) result(ff)

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

end function randn_v
! ...
! =====================================================================
! ...
function randn_a (m,n) result(ff)

  integer, intent(in)                        :: m
  integer, intent(in)                        :: n
  real(dp), dimension(:,:), pointer          :: ff

  ! ... Local variables
  ! ...
  integer i,j
  real(dp), parameter                    :: s  =  0.449871D0
  real(dp), parameter                    :: t  = -0.386595D0
  real(dp), parameter                    :: a  =  0.19600D0
  real(dp), parameter                    :: b  =  0.25472D0
  real(dp), parameter                    :: r1 =  0.27597D0
  real(dp), parameter                    :: r2 =  0.27846D0
  real(dp) u,v,x,y,q

  if (.not.associated(ff)) allocate(ff(m,n))

  do j=1,n
  do i=1,m
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
    ff(i,j) = v/u
  enddo
  enddo

end function randn_a
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
