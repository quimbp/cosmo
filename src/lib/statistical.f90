! ****************************************************************************
! ... Statistical class and utilitites
! ... COSMO Project
! ... Quim Ballabrera, November 2020
! ... Version 0.1, released November 2020
! ****************************************************************************

module Mstat

use types
use constants
use math

implicit none

type Stats
  integer                           :: n = 0
  real(dp)                          :: min                  ! Minimum
  real(dp)                          :: max                  ! Maximum
  real(dp)                          :: median               ! Median
  real(dp)                          :: mean                 ! Average
  real(dp)                          :: trimmed_mean         ! Trimmed Mean
  real(dp)                          :: adev                 ! Average deviation
  real(dp)                          :: iqr                  ! Int. Quart. Rang.
  real(dp)                          :: std                  ! Stand. dev.
  real(dp)                          :: var                  ! Variance
  real(dp)                          :: rms                  ! Root Mean Square
  real(dp)                          :: skew                 ! Skewness
  real(dp)                          :: kurt                 ! Curtosis
  real(dp)                          :: p01                  !  1% percentile
  real(dp)                          :: p05                  !  5% percentile
  real(dp)                          :: p25                  ! 25% percentile
  real(dp)                          :: p75                  ! 75% percentile
  real(dp)                          :: p95                  ! 95% percentile
  real(dp)                          :: p99                  ! 99% percentile
  real(dp)                          :: lower_inner_fence
  real(dp)                          :: lower_outer_fence
  real(dp)                          :: upper_inner_fence
  real(dp)                          :: upper_outer_fence

  contains
    procedure                 :: eval         => get_stats
    procedure                 :: print        => show_stats

end type Stats

contains
! ...
! =============================================================================
! ...
subroutine get_stats(S,x)

implicit none

class(Stats), intent(inout)                  :: S
real(dp), dimension(:), intent(in), allocatable  :: x

integer n

n = size(x)
S%n = n

S%min          = minval(x)
S%max          = maxval(x)
S%mean         = sum(x)/n
S%median       = median(x)
S%trimmed_mean = trimmed_mean(x,1)

call moments (x,S%mean,S%adev,S%std,S%var,S%skew,S%kurt)

S%rms  = sqrt(dot_product(x,x)/n)

S%p01  = percentile(x,01D0)
S%p05  = percentile(x,05D0)
S%p95  = percentile(x,95D0)
S%p99  = percentile(x,99D0)

S%p25  = percentile(x,25D0)
S%p75  = percentile(x,75D0)
S%IQR  = S%p75 - S%p25

S%lower_outer_fence = S%p25 - 3.0D0*S%IQR
S%lower_inner_fence = S%p25 - 1.5D0*S%IQR
S%upper_inner_fence = S%p75 + 1.5D0*S%IQR
S%upper_outer_fence = S%p75 + 3.0D0*S%IQR



end subroutine get_stats
! ...
! =============================================================================
! ...
subroutine show_stats(S,iunit)

class(Stats), intent(in)                     :: S
integer, intent(in), optional                :: iunit

! ... Local variables
! ...
integer iu
character(len=1) minflag,maxflag

if (present(iunit)) then
  iu = iunit
else
  iu = 6
endif

minflag = ''
maxflag = ''
if (S%min.lt.S%lower_outer_fence) minflag = '*'
if (S%max.gt.S%upper_outer_fence) maxflag = '*'
write(iu,30) S%n,                                      &
             S%min, minflag, S%p01, S%p05,             &
             S%p95, S%p99, S%max, maxflag,             &
             S%mean, S%trimmed_mean, S%std,            &
             S%var, S%rms,                             &
             S%adev, S%p25, S%median, S%p75,           &
             S%iqr, S%skew, S%kurt,                    &
             S%lower_inner_fence, S%upper_inner_fence, &
             S%lower_outer_fence, S%upper_outer_fence

30 format (10x,'     Number of points = ', i9,/,          &
           10x,'              Minimum = ',F9.3, 5X,A1,/,  &
           10x,'        Percentil 01% = ',F9.3,/,         &
           10x,'        Percentil 05% = ',F9.3,/,         &
           10x,'        Percentil 95% = ',F9.3,/,         &
           10x,'        Percentil 99% = ',F9.3,/,         &
           10x,'              Maximum = ',F9.3, 5X,A1,/,  &
           10x,'                 Mean = ',F9.3,/,         &
           10x,'         Trimmed Mean = ',F9.3,/,         &
           10x,'   Standard Deviation = ',F9.3,/,         &
           10x,'             Variance = ',F9.3,/,         &
           10x,'                  RMS = ',F9.3,/,         &
           10x,'    Average Deviation = ',F9.3,/,         &
           10x,'        Percentil 25% = ',F9.3,/,         &
           10x,'               Median = ',F9.3,/,         &
           10x,'        Percentil 75% = ',F9.3,/,         &
           10x,'                  IQR = ',F9.3,/,         &
           10x,'             Skewness = ',F9.3,/,         &
           10x,'             Kurtosis = ',F9.3,/,         &
           10x,'   Inner Fences ',/,                      &
           10x,'                Lower = ',F9.3/,          &
           10x,'                Upper = ',F9.3/,          &
           10x,'   Outer Fences ',/,                      &
           10x,'                Lower = ',F9.3/,          &
           10x,'                Upper = ',F9.3/)


end subroutine show_stats
! ...
! =============================================================================
! ...
function trimmed_mean (A,p)

implicit none

real(dp)                            :: trimmed_mean
integer, intent(in)                 :: p
real(dp), dimension(:), intent(in)  :: A

integer N,i
real(dp) den,Ai,plo,phi,pp

trimmed_mean = zero
den          = 0

n = size(A)
if (n.le.0) return

pp  = p
plo = percentile (A,pp)
pp  = 100D0 - p
phi = percentile (A,pp)

do i=1,N
  Ai = A(i)
  if (Ai.ge.plo.and.Ai.le.phi) then
    trimmed_mean = trimmed_mean + Ai
    den          = den + 1
  endif
enddo

if (den.gt.0.0D0) then
  trimmed_mean = trimmed_mean/den
else
  trimmed_mean = zero
endif

return
end function trimmed_mean
! ...
! =============================================================================
! ...
subroutine moments (data,ave,adev,sdev,var,skew,curt)

implicit none

real(dp), dimension(:), intent(in)           ::  data
real(dp), intent(out)                        ::  adev,ave,curt,sdev,skew,var

integer n,j
real(dp) p,s,ep

n = size(data)

ave  = zero
adev = zero
sdev = zero
var  = zero
skew = zero
curt = zero

if (n.le.1) return

ep  = zero
ave = sum(data)/n
do j=1,n
  s    = data(j)-ave
  ep   = ep+s
  adev = adev+abs(s)
  p    = s*s
  var  = var+p
  p    = p*s
  skew = skew+p
  p    = p*s
  curt = curt+p
enddo

adev = adev/n
var  = (var-ep**2/n)/(n-1)
sdev = sqrt(var)
if(var.ne.0.d0)then
  skew = skew/(n*sdev**3)
  curt = curt/(n*var**2)-3.d0
else
  skew = zero
  curt = zero
endif

return
end subroutine moments
! ...
! =============================================================================
! ...
end module Mstat
