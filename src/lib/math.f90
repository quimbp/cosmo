! ****************************************************************************
! ... Mathematical utilitites
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... About norm2: This has become a standard function after Fortran 2008.
! ****************************************************************************

module math

use types, only: dp
use constants, only: zero,one,nan

implicit none
private
public identity,akima,dakima,arange,mean,indexx
!public norm2

interface akima
  module procedure akimas,akimav
end interface akima

contains
! ...
! =============================================================================
! ...
function akimas (x,f,xx) result(ff)

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
end module math
