! ****************************************************************************
! ... Mathematical utilitites
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... About norm2: This has become a standard function after Fortran 2008.
! ... Version 0.1, released October 2017
! ****************************************************************************

module math

use types, only: dp
use constants, only: zero,one,two,nan,half,Rearth,deg2rad
use utils, only: locate,stop_error

implicit none
!private
!public identity,akima,dakima,arange,mean,indexx,interplin,haversine
!public d_interpol
!public grnn1
!public norm2

interface akima
  module procedure akimas,akimav
end interface akima

contains
! ...
! =============================================================================
! ...
function interplin (x,f,xx) result(ff)

real(dp), dimension(:), intent(in)  :: x,f
real(dp), intent(in)                :: xx
real(dp)                            :: ff

! ... Local variables
! ...
integer i


ff = nan
if (size(x).ne.size(f)) call stop_error(1,'Error interplin: Incompatible sizes')
if (size(x).le.1) call stop_error(1,'Error interplin: N must be > 1')

if (abs(xx-x(1)).lt.1d-5*abs(x(2)-x(1))) then
  ff = f(1)
  return
endif

i = locate(x,xx)
if (i.lt.1.or.i.ge.size(x)) then
  ff = nan
  return
endif

ff = f(i) + (f(i+1)-f(i))*(xx-x(i))/(x(i+1)-x(i))

end function interplin
! ...
! =============================================================================
! ...
function akimas (x,f,xx) result(ff)
! ... Akima interpolation for a single point: xx

real(dp), dimension(:), intent(in)  :: x,f
real(dp), intent(in)                :: xx
real(dp)                            :: ff

! ... Local variables
! ...
real(dp) df(size(x))

df = dakima(x,f)
ff = evlak(xx,x,f,df)

end function akimas
! ...
! =============================================================================
! ...
function akimav (x,f,xx) result(ff)
! ... Akima interpolation for a vector of points: xx(1:nn)

real(dp), dimension(:), intent(in)  :: x,f
real(dp), dimension(:), intent(in)  :: xx
real(dp), dimension(size(xx))       :: ff

! ... Local variables
! ...
integer i,n,nn
real(dp) df(size(x))

n = size(x)
nn = size(xx)

df = dakima(x,f)
DO i=1,nn
  ff(i) = evlak(xx(i),x,f,df)
ENDDO

end function akimav
! ...
! =============================================================================
! ...
real(dp) function evlak(xc,x,f,df) 
! ... Given a montone increasing array of x values (Note: routine does not 
! ... check this), samples of values of the function f and 1st derivative  
! ... df at the xs, interpolates by cubic polynomial.  The array df can be 
! ... found by calling subroutine akima.                                   
!                                                                       
! ... If y falls outside the interval, the polynomial on the               
! ... nearest interval is used to extrapolate.                             

real(dp), intent(in)                  :: xc
real(dp), dimension(:), INTENT(in)    :: x,f,df

! ... Local variables
! ...
integer k,n
integer init         ! Search for proper interval initiated at previous call
save init
data init/1/
real(dp) dx,t,s

n = size(x)

! ... Locate sample interval containing xc:  after xc lies in [x(init),      
! ... x(init+1)), unless xc lies outside [x(1), x(n)] when the interval     
! ... is the intervals containing the apprpriate end point.                

init = min(init, n)
if (xc.gt.x(init)) then
  do k=init,n
    if (x(k).gt.xc) then
      init = k-1
      goto 1300
    endif
  enddo
  init = n-1
else
  do k=init,1,-1
    if (x(k).le.xc) then
      init = k
      goto 1300
    endif
  enddo
  init = 1
endif

1300 continue
dx = x(init+1) - x(init)

! ...  Evaluate the cubic interpolator                                      
! ...
t = (xc - x(init))/dx
s = 1.0D0 - t
evlak = s**2*((1.0D0 + 2.0D0*t)*f(init)   + t*dx*df(init))   +     &
        t**2*((1.0D0 + 2.0D0*s)*f(init+1) - s*dx*df(init+1))
end function evlak
! ...
! =============================================================================
! ...
function dakima (x,f) result(df)
! ... Given the array of samples of a function f and sample points x,      
! ... assumed to be monotone, generates slopes for an interpolating rule    
! ... according to Akima's algorithm (Lancaster and Salkauskas,            
! ... Curve and surface fitting, 1986 academic press, p 82).       

real(dp), dimension(:), intent(in)  :: x,f
real(dp), dimension(size(x))        :: df

! ... Local variables
! ...
integer                             :: i,n
real(dp)                            :: Sn,eps,D1,D2
real(dp), DIMENSION(4)              :: S

n = size(x)
eps  = 1.0e-6*ABS(x(n) - x(1))

S(1) = (f(2) - f(1))/(x(2) - x(1))
S(2) = S(1)
S(3) = S(1)
S(4) = (f(3) - f(2))/(x(3) - x(2))
Sn   = (f(n) - f(n-1))/(x(n) - x(n-1))

do i=1, n
  D1 = abs(S(2) - S(1))
  D2 = abs(S(4) - S(3))
  df(i) = (D2*S(2) + D1*S(3))/(D1 + D2 + eps)

  S(1) = S(2)
  S(2) = S(3)
  S(3) = S(4)
  if (i+3.le.n) then
    S(4) = (f(i+3) - f(i+2))/(x(i+3)-x(i+2))
  else
    S(4) = Sn
  endif
enddo

if (n.eq.2) return

! ... If 3 or more points use gradient from a parabola for 1st & last df   
! ...
df(1) = qakima(f(2)-f(1),x(2)-x(1),f(3)-f(1),x(3)-x(1))
df(n) = qakima(f(n-1)-f(n),x(n-1)-x(n),f(n-2)-f(n),x(n-2)-x(n))

contains
  real(dp) pure function qakima(u1,x1,u2,x2)
  real(dp), intent(in)       :: u1,x1,u2,x2
  qakima = (u1/x1**2-u2/x2**2)/(1.0_dp/x1-1.0_dp/x2)
  end function qakima

end function dakima
! ...
! =============================================================================
! ...
function arange(xo,xf,n)
! ... Returns the n-dimensional vector (xo,xo+dx,xo+2dx,...,xf)

integer, intent(in)     :: n
real(dp), intent(in)    :: xo,xf
real(dp), dimension(n)  :: arange

! ... Local variables
! ...
integer i
real(dp) dx

dx = (xf-xo)/(n-1)
do i=1,n
  arange(i) = (i-1)*dx + xo
enddo

end function arange
! ...
! =============================================================================
! ...
function identity(n)
! ... Returns the (n x n) identity matrix

integer, intent(in)          :: n
real(dp), dimension(n,n)     :: identity

! ... Local variables
! ...
integer i

identity = zero
do i=1,n
  identity(i,i) = one
enddo

end function identity
! ...
! =============================================================================
! ...
real(dp) function mean(A,W)
! ... Calculates the weighted mean = 1/N * Sum W(i)*A(i)
! ... Weights are optional.

real(dp), dimension(:), intent(in)     :: A
real(dp), dimension(:), optional       :: W

integer n
real(dp) Sw

mean = nan
n = size(A)
if (n.eq.0) return

if (present(W)) then
  Sw   = sum(W)
  mean = dot_product(W,A)/Sw
else
  mean = sum(A)/N
endif

end function mean
! ...
! =============================================================================
! ...
!real(dp) function norm2(A)
! ... Calculates the weighted mean = 1/N * Sum W(i)*A(i)
! ... Weights are optional.

!real(dp), dimension(:), intent(in)     :: A

!norm2 = sqrt(dot_product(A,A))

!end function norm2
! ...
! =============================================================================
! ...
! ... Subroutine indexx from Numerical Recipes in Fortran.
! ... The Art of Scientific Computing. Press et al., 1992.
! ... 
      subroutine indexx(arr,indx)

      real(dp), dimension(:), intent(in)          :: arr
      integer, dimension(size(arr)), intent(out)  :: indx
      
      ! ... Local varibles:
      ! ...
      integer, parameter                          :: M=7
      INTEGER                                     :: n,i,indxt,ir,itemp,j,jstack,k,l
      INTEGER, dimension(size(arr))               :: istack
      real(dp)                                    :: a            

      n = size(arr)

      do j=1,n
        indx(j)=j
      enddo

      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do i=j-1,1,-1
            if(arr(indx(i)).le.a) goto 2
            indx(i+1)=indx(i)
          enddo
          i=0
2         indx(i+1)=indxt
        enddo
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      end subroutine indexx
!  (C) Copr. 1986-92 Numerical Recipes Software *5sV1.
! ...
! =============================================================================
! ...
! ************************************************************************
! ... haversine.f90
! ... Quim Ballabrera, May 2015
! ...
! ... Calculates the great circle distance using the haversine distance
! ...  6371.0 km is the authalic radius based on/extracted from surface area;
! ...  6372.8 km is an approximation of the radius of the average circumference
! ...   (i.e., the average great-elliptic or great-circle radius), where the
! ...   boundaries are the meridian (6367.45 km) and the equator (6378.14 km).
! ...
! ************************************************************************

real(dp) pure function haversine_rad (x1,phi1,x2,phi2)

! ... Haversine function: Input in radians

implicit none

real(dp), intent(in)          :: x1, phi1
real(dp), intent(in)          :: x2, phi2

! ... Local variables
! ...
real(dp) dlam,dphi
real(dp) SINPHI,SINLAM,a,c

dphi = (phi2 - phi1)
dlam = (x2 - x1)

SINPHI = sin(half*dphi)
SINLAM = sin(half*dlam)

a = SINPHI*SINPHI + cos(phi1)*cos(phi2)*SINLAM*SINLAM
c = two * asin(sqrt(a))
haversine_rad = Rearth * c

end function haversine_rad
! ...
! =============================================================================
! ...
real(dp) pure function haversine (x1,y1,x2,y2)

implicit none

real(dp), intent(in)          :: x1, y1
real(dp), intent(in)          :: x2, y2

! ... Local variables
! ...
real(dp) phi1,phi2
real(dp) dlam,dphi
real(dp) SINPHI,SINLAM,a,c

phi1 = deg2rad * y1
phi2 = deg2rad * y2
dphi = deg2rad * (y2 - y1)
dlam = deg2rad * (x2 - x1)

SINPHI = sin(half*dphi)
SINLAM = sin(half*dlam)

a = SINPHI*SINPHI + cos(phi1)*cos(phi2)*SINLAM*SINLAM
c = two * asin(sqrt(a))
haversine = Rearth * c

end function haversine
! ...
! =============================================================================
! ...
real(dp) function d_interpol (d,f)

implicit none

real(dp), dimension(:), intent(in)     :: d
real(dp), dimension(:), intent(in)     :: f

! ... Local variables
! ...
integer                                :: n,i,j
real(dp), dimension(size(d))           :: a
real(dp)                               :: dc,sw

dc = 1.0D-7 * mean(d)

do i=1,n
  if (d(i).lt.dc) then
    d_interpol = f(i)
    return
  endif
enddo

do i=1,n
  sw = zero
  do j=1,n
    if (j.ne.i) sw = sw + d(i)/d(j)
  enddo
  a(i) = one / (one + sw)
enddo

d_interpol = dot_product(a,f)

end function d_interpol
! ...
! =============================================================================
! ...
function grnn1 (x,y,xo,sigma) result(yo)

! ... Generalized Regression Neural Network
! ... Wasserman, P.D., Advanced Methods in Neural Computing, New York, 
! ...   Van Nostrand Reinhold, 1993, pp. 155–61
! ...
! ... [x,y] :: training data pairs (Real numbers)
! ... sigma :: spread
! ... xo    :: input     (Real)
! ... yo    :: output    (Real)

real(dp), dimension(:), intent(in)   :: x
real(dp), dimension(:), intent(in)   :: y
real(dp), intent(in)                 :: xo
real(dp), intent(in)                 :: sigma
real(dp)                             :: yo

real(dp), dimension(size(x))         :: w,d
real(dp) num,den

d(:) = (x(:) - xo)/sigma
w(:) = exp(-0.5*d**2)

num = dot_product(w,y)
den = sum(w)

if (den.eq.zero) then
  yo = zero
else
  yo = num/den
endif

end function grnn1
! ...
! =============================================================================
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

  do
    call RANDOM_NUMBER(u)  ! GNU RANDOM GENERATOR
    call RANDOM_NUMBER(v)  ! GNU RANDOM GENERATOR
    v = 1.7156D0 * (v - 0.5D0)

    ! ... Evaluate the quadratic form
    ! ...
    x = u - s
    y = ABS(v) - t
    q = x**2 + y*(a*y - b*x)

    if (q .lt. r1) exit
    if (q .gt. r2) cycle
    if (v**2 .LT. -4D0*LOG(u)*u**2) exit
  enddo
  ff(i) = v/u

enddo

end function randn
! ...
! =============================================================================
! ...
function percentile (x,p,METHOD)

! ...
! ... EXCEL method (default)
! ... NIST method
! ...

IMPLICIT NONE

REAL(KIND=8), DIMENSION(:), INTENT(in)   :: x
REAL(KIND=8),               INTENT(in)   :: p
REAL(KIND=8)                             :: percentile
CHARACTER(LEN=*), OPTIONAL               :: METHOD

LOGICAL excel, nist
INTEGER                                  :: n
INTEGER, DIMENSION(SIZE(x))              :: indx

INTEGER i,kk
REAL(KIND=8) rr,dd

excel = .true.
IF (PRESENT(METHOD)) THEN
  IF ((METHOD(1:1).EQ.'N').OR.(METHOD(1:1).EQ.'n')) THEN
    nist = .true.
    excel = .false.
  ENDIF
ENDIF

n  = SIZE(x)
CALL indexx(x,indx)

IF (excel) THEN
  rr = p*(n-1.0D0)/100.0D0 + 1.0D0
ELSE
  rr = p*(n+1.0D0)/100.0D0
ENDIF

kk = FLOOR(rr)
dd = rr - kk
IF (kk.EQ.0) THEN
  percentile = x(indx(1))
ELSE IF (kk.EQ.n) THEN
  percentile = x(indx(n))
ELSE
  percentile = x(indx(kk)) + dd*(x(indx(kk+1))-x(indx(kk)))
ENDIF

RETURN
end function percentile
! ...
! =============================================================================
! ...
fUnction median (A)

implicit none

real(dp)                            :: median
real(dp), dimension(:), intent(in)  :: A

integer N,n1,n2
integer, dimension(SIZE(A))    :: IWRK

median = nan

N = SIZE(A)
IF (N.LE.0) return

call indexx(A,IWRK)

if (MOD(N,2).EQ.0) then
  n1 = N/2
  n2 = n1 + 1
  median = 0.5D0*(A(IWRK(n1))+A(IWRK(n2)))
else
  n1 = (N+1)/2
  median = A(IWRK(n1))
endif

return
end function median
! ...
! ==========================================================================
! ...
function nanmedian (A)

implicit none

real(dp)                            :: nanmedian
real(dp), dimension(:), intent(in)  :: A

integer N,n1,n2,i
real(dp), dimension(SIZE(A))        :: AA
integer, dimension(SIZE(A))         :: IWRK

nanmedian = nan

N = 0
do i=1,SIZE(A)
  if (isnan(A(i))) THEN
    ! Skip value
  else
    N = N + 1
    AA(N) = A(i)
  endif
enddo

if (N.le.0) return

call indexx(AA(1:N),IWRK)

if (mod(N,2).eq.0) then
  n1 = N/2
  n2 = n1 + 1
  nanmedian = 0.5D0*(AA(IWRK(n1))+AA(IWRK(n2)))
else
  n1 = (N+1)/2
  nanmedian = AA(IWRK(n1))
endif

return
end function nanmedian
! ...
! ==========================================================================
! ...
function wmedian (A,W)
! ... Weighted median

implicit none

real(dp) wmedian
real(dp), dimension(:), intent(in)       :: A,W

integer                                  :: N,i,j
integer, dimension(SIZE(A))              :: IWRK
real(dp)                                 :: psum,hsum

wmedian = nan

N = SIZE(A)
if (N.le.0) return

hsum = 0.5D0*sum(W)

call indexx(A,IWRK)

psum = 0D0
do i=1,N
  psum = psum + W(IWRK(i))
  if (psum.GE.hsum) then
    if (psum.EQ.hsum) then
      wmedian = 0.5D0*A(IWRK(i))
      do j=1,N-i
        if (W(IWRK(i+j)).ne.0) then
          wmedian = wmedian + 0.5D0*A(IWRK(i+j))
          return
        endif
      enddo
    else
      wmedian = A(IWRK(i))
      return
    endif
  endif
enddo

return
end function wmedian
! ...
! =============================================================================
! ...
function loess (t,x,wr,g,lambda,deg,periodic,period) result(err)

implicit none

integer                                 :: err      ! Output error flag
real(dp), dimension(:), intent(in)      :: t        ! Sample time
real(dp), dimension(:), intent(in)      :: x        ! Sample values
real(dp), dimension(:), intent(in)      :: wr       ! Additional weight
real(dp), dimension(size(t)), intent(out)  :: g        ! Filtered series
real(dp), intent(in)                    :: lambda   ! time-lag parameter
integer, intent(in)                     :: deg      ! Polynolial fit
logical, intent(in)                     :: periodic ! Polynolial fit
real(dp), intent(in), optional          :: period   ! Polynolial fit


! ... Local variables:
! ...
integer                                 :: N        ! Number of points
integer point,i,j,ii,ll,np
real(dp) tt,wi,xsum

integer, dimension(size(t))               :: map
real(dp), dimension(size(t))              :: d,w
real(dp), dimension(:), allocatable       :: YY,WW
real(dp), dimension(:,:), allocatable     :: XX,MM

integer                                   :: M
real(dp), dimension(deg+1)                :: RHS,DD,c
real(dp), dimension(deg+1,deg+1)          :: A,V

N = size(t)

M = deg + 1

! ... Default value:
! ...
g(:) = 0
err  = 1

IF (lambda.LE.0) THEN
  WRITE(*,*) 'Invalid negatime time-lag parameter'
  RETURN
ENDIF

do point=1,N
  ! ... For each point, calculate its distance to the other points
  ! ...
  if (periodic) then 
    d = perdist(t,t(point),period)
  else
    d = regdist(t,t(point))
  endif

  ! ... Weighting according to the distance
  ! ... Only the weights different from zero will be retained
  ! ...
  call Wloess (N,lambda,3,d,w)
  np = 0
  do i=1,N
    if (w(i).gt.1E-8) THEN
      np = np + 1
      map(np) = i
    endif
  enddo

  ! ... Weight multiplication by user-provided additional weight
  ! ... Usually, the user provides wr(:) = 1
  ! ...
  w(:) = wr(:) * w(:)

  if (np.EQ.0) then
    write(*,*) 'No points for regression'
    return
  endif

  ! ... Fitting the weighted local polynomial: orders 0,1 or 2.
  ! ... [XT W X] alpha = XT W y
  ! ...
  if (np.EQ.1) then
     g(point) = x(map(1))
     cycle
  endif

  allocate (XX(np,deg+1))
  allocate (YY(np))

  do i=1,np
    ii    = map(i)
    tt    = d(ii)                ! Distance
    wi    = SQRT(w(ii))
    YY(i) = wi*x(ii)
    do ll=0,deg
      XX(i,1+ll) = wi*tt**ll
    enddo
  enddo

  ! ... Solve the system:
  ! ...
  do i=1,M
    !xsum = 0D0
    !do ii=1,np
    !  xsum = xsum + XX(ii,i)*YY(ii)
    !enddo
    !RHS(i) = xsum
    RHS(i) = DOT_PRODUCT(XX(1:np,i),YY(1:np))
    do j=i,M
      !xsum =0D0
      !do ii=1,np
      !  xsum = xsum + XX(ii,i)*XX(ii,j)
      !enddo
      xsum = DOT_PRODUCT(XX(1:np,i),XX(1:np,j))
      A(i,j) = xsum
      A(j,i) = xsum
    enddo
  enddo

  call svdcmp (A,M,M,M,M,DD,V)
  where(DD.LE.0) DD = 0D0

  if (count(DD.eq.0).gt.0) then
    write(*,*) 'Singular matrix'
    deallocate (XX,YY)
    return
  endif

  call svbksb (A,DD,V,M,M,M,M,RHS,c)

  g(point) = c(1)

  deallocate(XX)  
  deallocate(YY)  

enddo

err = 0
return  

end function loess
! ...
! =========================================================================
! ...
function regdist (t,to) result(d)

implicit none

real(dp), dimension(:), intent(in)             :: t
real(dp), intent(in)                           :: to
real(dp), dimension(size(t))                   :: d

d(:) = t(:) - to

return
end function regdist
! ...
! =========================================================================
! ...
function perdist (t,to,period) result(d)

implicit none

real(dp), dimension(:), intent(in)             :: t
real(dp), intent(in)                           :: to
real(dp), intent(in)                           :: period 
real(dp), dimension(size(t))                   :: d

integer i

do i=1,size(t)
  if (t(i).lt.to) then
    d(i) = -(MOD(to - t(i) + 0.5D0*period, period) - 0.5D0*period)
  else
    d(i) = MOD(t(i) - to + 0.5D0*period, period) - 0.5D0*period
  endif
enddo

return
end function perdist
! ...
! =========================================================================
! ...
subroutine Wloess (N,lambda,power,Dist,W)

implicit none

integer, intent(in)                       :: N
integer, intent(in)                       :: power
real(dp), INTENT(in)                      :: lambda
real(dp), dimension(N), intent(in)        :: Dist
real(dp), dimension(N), intent(out)       :: W

integer i
real(dp) adist

do i=1,N
  adist = ABS(Dist(i))
  if (adist.gt.lambda) then
    W(i) = 0D0
  else
    W(i) = (1D0 - (adist/lambda)**power)**power
  endiF
enddo

return
end subroutine Wloess
! ...
! =============================================================================
! ...
real(dp) function pythag(a,b)

real(dp) a,b
real(dp) absa,absb

absa=abs(a)
absb=abs(b)
if(absa.gt.absb)then
  pythag=absa*sqrt(1.d0+(absb/absa)**2)
else
  if(absb.eq.0.d0)then
    pythag=0.d0
  else
    pythag=absb*sqrt(1.d0+(absa/absb)**2)
  endif
endif

return
end function pythag
!  (C) Copr. 1986-92 Numerical Recipes Software *5sV1.
! ...
! =============================================================================
! ...
      subroutine svdcmp(a,m,n,mp,np,w,v)
      integer m,mp,n,np
      real(dp) a(mp,np),v(np,np),w(np)
!     PARAMETER (NMAX=750)
!U    USES pythag
      integer i,its,j,jj,k,l,nm
      real(dp) anorm,c,f,g,h,s,scale,x,y,z,rv1(n)
      !real(dp) pythag
      g=0.0d0
      scale=0.0d0
      anorm=0.0d0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+abs(a(k,i))
11        continue
          if(scale.ne.0.0d0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0.0d0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0.0d0
        s=0.0d0
        scale=0.0d0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+abs(a(i,k))
17        continue
          if(scale.ne.0.0d0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0.0d0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0d0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0d0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0d0
            v(j,i)=0.0d0
31        continue
        endif
        v(i,i)=1.0d0
        g=rv1(i)
        l=i
32    continue
      do 39 i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0.0d0
33      continue
        if(g.ne.0.0d0)then
          g=1.0d0/g
          do 36 j=l,n
            s=0.0d0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.0d0
38        continue
        endif
        a(i,i)=a(i,i)+1.0d0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((abs(rv1(l))+anorm).eq.anorm)  goto 2
            if((abs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0.0d0
          s=1.0d0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=pythag(f,g)
            w(i)=h
            h=1.0d0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0d0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
          if(its.eq.30) STOP 'no convergence in svdcmp'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0d0*h*y)
          g=pythag(f,1.0d0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0d0
          s=1.0d0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=pythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=pythag(f,h)
            w(j)=z
            if(z.ne.0.0d0)then
              z=1.0d0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0d0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END
!  (C) Copr. 1986-92 Numerical Recipes Software *5sV1.
! ...
! ====================================================================
! ...
subroutine svbksb(u,w,v,m,n,mp,np,b,x)

integer, intent(in)                      :: m,mp,n,np
real(dp), intent(in), dimension(mp,np)   :: u
real(dp), intent(in), dimension(np,np)   :: v
real(dp), intent(in), dimension(mp)      :: b
real(dp), intent(in), dimension(np)      :: w
real(dp), intent(out), dimension(np)     :: x

! ... Local variables
! ...
integer i,j,jj
real(dp) s,tmp(n)

do j=1,n
  s = 0.d0
  if(w(j).ne.0.d0)then
    do i=1,m
      s = s + u(i,j)*b(i)
    enddo
    s = s/w(j)
  endif
  tmp(j) = s
enddo

do j=1,n
  x(j)=DOT_PRODUCT(v(j,1:n),tmp(1:n))
enddo

return
end subroutine svbksb
! ... (C) Copr. 1986-92 Numerical Recipes Software *5sV1.
! ...
! =============================================================================
! ...
subroutine eigsort(d,r,v,n)

implicit none
integer n,r
REAL(dp) d(r),v(n,r)

integer i,j,k
real(dp) p

do i=1,r-1
  k=i
  p=d(i)
  do j=i+1,r
    if (d(j).ge.p) then
      k=j
      p=d(j)
    endif
  enddo
  if (k.ne.i) then
    d(k)=d(i)
    d(i)=p
    do j=1,n
      p=v(j,i)
      v(j,i)=v(j,k)
      v(j,k)=p
    enddo
  endif
enddo

return
end subroutine eigsort
! ...
! =============================================================================
! ...
end module math
