module module_math

  use, intrinsic :: IEEE_ARITHMETIC, ONLY : IEEE_VALUE, IEEE_QUIET_NAN
  use module_types, only: dp
  use module_constants
  use module_utils

  implicit none

  type type_point
    real(dp)                          :: x = 0.d0
    real(dp)                          :: y = 0.d0
    real(dp)                          :: z = 0.d0
  end type type_point

  interface interp1d
    module procedure interp1ds,interp1dv
  end interface interp1d

  interface swap
    module procedure swap_i,swap_d,swap_dv,swap_c,swap_cv
  end interface swap

contains
! ...
! =====================================================================
! ...
subroutine swap_i(a,b)  ! Integer swap
  integer, intent(inout)        :: a,b
  integer dum
  dum = a; a = b; b = dum
end subroutine swap_i
! ...
! =====================================================================
! ...
subroutine swap_d(a,b)  ! Double precision swap
  real(dp), intent(inout)        :: a,b
  real(dp) dum
  dum = a; a = b; b = dum
end subroutine swap_d
! ...
! =====================================================================
! ...
subroutine swap_dv(a,b)  ! Double precision vector swap
  real(dp), dimension(:), intent(inout)        :: a,b
  real(dp) dum(size(a))
  dum = a; a = b; b = dum
end subroutine swap_dv
! ...
! =====================================================================
! ...
subroutine swap_c(a,b)  ! Complex swap
  complex(dp), intent(inout)        :: a,b
  complex(dp) dum
  dum = a; a = b; b = dum
end subroutine swap_c
! ...
! =====================================================================
! ...
subroutine swap_cv(a,b) ! Complex vector swap
  complex(dp), dimension(:), intent(inout)        :: a,b
  complex(dp) dum(size(a))
  dum = a; a = b; b = dum
end subroutine swap_cv
! ...
! =====================================================================
! ...
pure function outerprod(a,b)
  real(dp), dimension(:), intent(in)              :: a,b
  real(dp), dimension(size(a),size(b))            :: outerprod
  outerprod = spread(a,dim=2,ncopies=size(b))*spread(b,dim=1,ncopies=size(a))
end function outerprod
! ...
! =====================================================================
! ...
real(dp) pure function vabs(v) ! Returns the length (ordinary L2 norm) of a vector.
  real(dp), dimension(:), intent(in)  :: v
  vabs = sqrt(dot_product(v,v))
end function vabs
! ...
! =====================================================================
! ...
pure real(dp) function haversine (lon1,lat1,lon2,lat2)
  ! ...
  ! ... Function Haversine
  ! ... Determines the great-circle distance between two points in a
  ! ... sphere. The input lngitudes and latitudes are given in radians
  ! ... The retruned distance is in meters.
  ! ... Rearth = 6371315.0_dp      ! m
  ! ...
  real(dp), intent(in)                  :: lon1,lat1
  real(dp), intent(in)                  :: lon2,lat2
  
  ! ... Local variables
  ! ...
  real(dp) sindx,sindy,dang
  
  sindx = sin(0.5D0*(lon2-lon1))
  sindy = sin(0.5D0*(lat2-lat1))
  
  dang = 2.0d0*asin(sqrt(sindy*sindy + cos(lat2)*cos(lat1)*sindx*sindx))
  haversine = Rearth * dang

  return
end function haversine
! ...
! =====================================================================
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
      integer                                     :: n,i,indxt,ir,itemp,j,jstack,k,l
      integer, dimension(size(arr))               :: istack
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
! =====================================================================
! ...
! ...
! ... Subroutine indexx from Numerical Recipes in Fortran.
! ... The Art of Scientific Computing. Press et al., 1992.
! ...
      subroutine indexi(arr,indx)

      integer, dimension(:), intent(in)          :: arr
      integer, dimension(size(arr)), intent(out)  :: indx

      ! ... Local varibles:
      ! ...
      integer, parameter                          :: M=7
      integer                                     :: n,i,indxt,ir,itemp,j,jstack,k,l
      integer, dimension(size(arr))               :: istack
      integer                                     :: a

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
      end subroutine indexi
!  (C) Copr. 1986-92 Numerical Recipes Software *5sV1.
! ...
! =====================================================================
! ...
function interp1ds(x,f,xx,method,bounds_error,fill_value) result(ff)

real(dp), dimension(:), intent(in)             :: x,f
real(dp), intent(in)                           :: xx
character(len=*), intent(in), optional         :: method
logical, intent(in), optional                  :: bounds_error
real(dp), intent(in), optional                 :: fill_value
real(dp)                                       :: ff

logical bounds
integer i
real(dp) nan,d1,d2,df(size(x))
character(1) lmethod

! ... Check valid data dimensions
! ...
if (size(x).ne.size(f)) call stop_error(1,'Error in interp1d: Incompatible sizes')
if (size(x).le.1) call stop_error(1,'Error in interp1d: N must be > 1')

! ... nan:
! ...
nan = ieee_value(1.0_dp,ieee_quiet_nan)

! ... Interpolation method: 'linar', 'nearest', 'spline'
! ...
lmethod = 'L'
if (present(method)) lmethod = method(1:1)

! ... Fill_value
! ...
ff = nan
if (present(fill_value)) ff = fill_value

! ... Boundaries
! ...
bounds = .True.
if (present(bounds_error)) bounds = bounds_error

if ((xx.lt.minval(x)).or.(xx.gt.maxval(x))) then
  if (bounds) call stop_error(1,'Error in interp1d: Value out of bounds')
  return
endif

! ... Linear interpolation
! ...
if ((lmethod.eq.'L'.or.lmethod.eq.'l')) then
  ! ... Linear interpolation
  ! ...
  i = locate(x,xx)
  if (i.eq.0) then
    ff = f(1)
  else
    ff = f(i) + (f(i+1)-f(i))*(xx-x(i))/(x(i+1)-x(i))
  endif
  return
else if ((lmethod.eq.'N'.or.lmethod.eq.'n')) then
  ! ... Nearest neighbor
  ! ...
  i = locate(x,xx)
  d1 = abs(x(i)-xx)
  d2 = abs(x(i+1)-xx)
  ff = f(i)
  if (d2.lt.d1) ff = f(i+1)
  return
else if ((lmethod.eq.'A'.or.lmethod.eq.'a')) then
  ! ... Akima splines
  ! ...
  df = dakima(x,f)
  ff = evlak(xx,x,f,df)
  return
else if ((lmethod.eq.'C'.or.lmethod.eq.'c')) then
  ! ... Cubic splines
  ! ...
  df = spline (x,f,2.0d30,2.0d30)
  ff = splint (x,f,df,xx)
  return
else
  call stop_error(1,'Error in interp1d: Method not valid')
endif
  
end function interp1ds
! ...
! =====================================================================
! ...
function interp1dv(x,f,xx,method,bounds_error,fill_value) result(ff)

real(dp), dimension(:), intent(in)             :: x,f
real(dp), dimension(:), intent(in)             :: xx
character(len=*), intent(in), optional         :: method
logical, intent(in), optional                  :: bounds_error
real(dp), intent(in), optional                 :: fill_value
real(dp), dimension(size(xx))                  :: ff

logical bounds
integer i,k,n
real(dp) nan,d1,d2,df(size(x))
real(dp) xmin,xmax
character(1) lmethod

n = size(xx)
if (n.eq.0) return

! ... Check valid data dimensions
! ...
if (size(x).ne.size(f)) call stop_error(1,'Error in interp1d: Incompatible sizes')
if (size(x).le.1) call stop_error(1,'Error in interp1d: N must be > 1')

! ... nan:
! ...
nan = ieee_value(1.0_dp,ieee_quiet_nan)

! ... Interpolation method: 'linar', 'nearest', 'spline'
! ...
lmethod = 'L'
if (present(method)) lmethod = method(1:1)

! ... Fill_value
! ...
ff(:) = nan
if (present(fill_value)) ff(:) = fill_value

! ... Boundaries
! ...
bounds = .True.
if (present(bounds_error)) bounds = bounds_error

xmin = minval(x)
xmax = maxval(x)
do i=1,n
  if ((xx(i).lt.xmin).or.(xx(i).gt.xmax)) then
    if (bounds) call stop_error(1,'Error in interp1d: Value out of bounds')
    return
  endif
enddo

if ((lmethod.eq.'L'.or.lmethod.eq.'l')) then
  ! ... Linear interpolation
  ! ...
  do k=1,n
    i = locate(x,xx(k))
    if (i.eq.0) then
      ff(k) = f(1)
    else
      ff(k) = f(i) + (f(i+1)-f(i))*(xx(k)-x(i))/(x(i+1)-x(i))
    endif
  enddo
  return
else if ((lmethod.eq.'N'.or.lmethod.eq.'n')) then
  ! ... Nearest point
  ! ...
  do k=1,n
    i = locate(x,xx(k))
    d1 = abs(x(i)-xx(k))
    d2 = abs(x(i+1)-xx(k))
    ff(k) = f(i)
    if (d2.lt.d1) ff(k) = f(i+1)
  enddo
  return
else if ((lmethod.eq.'A'.or.lmethod.eq.'a')) then
  ! ... Akima splines
  ! ...
  df = dakima(x,f)
  do k=1,n
    ff(k) = evlak(xx(k),x,f,df)
  enddo
  return
else if ((lmethod.eq.'C'.or.lmethod.eq.'c')) then
  ! ... Cubic splines
  ! ...
  df = spline (x,f,2.0d30,2.0d30)
  do k=1,n
    ff(k) = splint (x,f,df,xx(k))
  enddo
  return
else
  call stop_error(1,'Error in interp1d: Method not valid')
endif
  
end function interp1dv
! ...
! =====================================================================
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
real(dp), dimension(4)              :: S

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

  ! -----------------------------------------
  real(dp) pure function qakima(u1,x1,u2,x2)
  ! -----------------------------------------
  real(dp), intent(in)       :: u1,x1,u2,x2
  qakima = (u1/x1**2-u2/x2**2)/(1.0_dp/x1-1.0_dp/x2)
  end function qakima

end function dakima
! ...
! =====================================================================
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
real(dp), dimension(:), intent(in)    :: x,f,df

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
! =====================================================================
! ...
function spline(x,y,yp1,ypn) result(y2)

  real(dp), dimension(:), intent(in)    :: x,y
  real(dp), intent(in)                  :: yp1,ypn
  real(dp), dimension(size(x))          :: y2

  ! ... Local variables
  ! ...
  integer i,k,n
  real(dp) p,qn,sig,un,u(size(x))

  n = size(x)

  if (yp1.gt.0.99d30) then
    y2(1) = 0.d0
    u(1)  = 0.d0
  else
    y2(1) = -0.5d0
    u(1)  = (3.d0/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
  endif

  do i=2,n-1
    sig   = (x(i)-x(i-1))/(x(i+1)-x(i-1))
    p     = sig*y2(i-1)+2.d0
    y2(i) = (sig-1.d0)/p
    u(i)  = (6.d0*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/ &
             (x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
  enddo

  if (ypn.gt.0.99d30) then
    qn = 0.d0
    un = 0.d0
  else
    qn = 0.5d0
    un = (3.d0/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
  endif
  
  y2(n) = (un-qn*u(n-1))/(qn*y2(n-1)+1.d0)
  do k=n-1,1,-1
    y2(k) = y2(k)*y2(k+1) + u(k)
  enddo

  return
! ...  (C) Copr. 1986-92 Numerical Recipes Software *5sV1.
end function spline
! ...
! =====================================================================
! ...
function splint(xa,ya,y2a,x) result(y)

  real(dp), dimension(:), intent(in)      :: xa,ya,y2a
  real(dp), intent(in)                    :: x
  real(dp)                                :: y

  integer n,k,khi,klo
  real(dp) a,b,h

  n = size(xa)

  klo = 1
  khi = n

  klo = max(min(locate(xa,x),n-1),1)
  khi = klo + 1

  h = xa(khi)-xa(klo)
  if (h.eq.0.d0) stop  'bad xa input in splint'

  a = (xa(khi)-x)/h
  b = (x-xa(klo))/h
  y = a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.d0

  return

!C  (C) Copr. 1986-92 Numerical Recipes Software *5sV1.
end function splint
! ...
! ====================================================================
! ...
subroutine mnbrak(ax,bx,cx,fa,fb,fc,func)

  real(dp), intent(inout)           :: ax,bx
  real(dp), intent(out)             :: cx,fa,fb,fc

  interface 
    function func(x)
      use module_types, only: dp
      real(dp), intent(in)  :: x
      real(dp) func
    end function func
  end interface

  ! ... Local variables
  ! ...
  real(dp), parameter               :: GOLD=1.618034D0
  real(dp), parameter               :: GLIMIT=100.0D0
  real(dp), parameter               :: TINY=1.0D-20
  real(dp) fu,q,r,u,ulim

  fa = func(ax)
  fb = func(bx)
  if (fb > fa) then
    call swap(ax,bx)
    call swap(fa,fb)
  endif
  cx = bx + GOLD*(bx-ax)
  fc = func(cx)
  do
    if (fb < fc) return
    r = (bx-ax)*(fb-fc)
    q = (bx-cx)*(fb-fa)
    u = bx-0.5D0*((bx-cx)*q-(bx-ax)*r)/sign(max(abs(q-r),TINY),q-r)
    ulim = bx + GLIMIT*(cx-bx)
    if ((bx-u)*(u-cx) > 0.0D0) then
      fu = func(u)
      if (fu < fc) then
        ax = bx
        fa = fb
        bx = u
        fb = fu
        return
      else if (fu > fb) then
        cx = u
        fc = fu
        return
      endif
      u = cx + GOLD*(cx-bx)
      fu = func(u)
    else if ((cx-u)*(u-ulim) > 0.0D0) then
      fu = func(u)
      if (fu < fc) then
        bx = cx
        cx = u
        u  = cx + GOLD*(cx-bx)
        call shft(fb,fc,fu,func(u))
      endif
    else if ((u-ulim)*(ulim-cx) >= 0.0D0) then
      u  = ulim
      fu = func(u)
    else
      u  = cx + GOLD*(cx-bx)
      fu = func(u)
    endif
    call shft(ax,bx,cx,u)
    call shft(fa,fb,fc,fu)
  enddo

  contains
    subroutine shft(a,b,c,d)
      use module_types, only: dp
      real(dp), intent(out)   :: a
      real(dp), intent(inout) :: b,c
      real(dp), intent(in)    :: d
      a = b; b = c; c = d
    end subroutine shft

end subroutine mnbrak
! ...
! =====================================================================
! ...
real(dp) function brent(ax,bx,cx,func,tol,xmin)

  real(dp), intent(in)                 :: ax,bx,cx,tol
  real(dp), intent(out)                :: xmin
  interface
    function func(x)
      use module_types, only: dp
      real(dp), intent(in)    :: x
      real(dp)                :: func
    end function func
  end interface

  ! ... Local variables
  integer, parameter                   :: ITMAX=100
  real(dp), parameter                  :: CGOLD=0.3819660_dp,ZEPS=1.0D-3*epsilon(ax)
  integer iter
  real(dp) a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm

  a = min(ax,cx)
  b = max(ax,cx)
  v = bx
  w = v
  x = v
  e = 0.0D0
  fx =func(x)
  fv = fx
  fw = fx
  do iter=1,ITMAX
    xm = 0.5D0*(a+b)
    tol1 = tol*abs(x) + ZEPS
    tol2 = 2.0D0*tol1
    if (abs(x-xm) <= (tol2-0.5D0*(b-a))) then
      ! ... Exit with best values
      xmin = x
      brent = fx
      return
    endif
    if (abs(e) > tol1) then
      r = (x-w)*(fx-fv)
      q = (x-v)*(fx-fw)
      p = (x-v)*q - (x-w)*r
      q = 2.0D0*(q-r)
      if (q > 0.0D0) p = -p
      q = abs(q)
      etemp = e
      e = d
      if (abs(q) >= abs(0.5D0*q*etemp) .or. &
          p <= q*(a-x) .or. p >= q*(b-x)) then
        e = merge(a-x,b-x,x >= xm)
        d = CGOLD*e
      else
        d = p/q
        u = x + d
        if (u-a < tol2 .or. b-u < tol2) d = sign(tol1,xm-x)
      endif
    else
      e = merge(a-x,b-x, x>= xm)
      d = CGOLD*e
    endif
    u = merge(x+d,x+sign(tol1,d), abs(d) >= tol1 )
    fu = func(u)
    if (fu <= fx) then
      if ( u >= x) then
        a = x
      else
        b = x
      endif
      call shft(v,w,x,u)
      call shft(fv,fw,fx,fu)
    else
      if (u < x) then
        a = u
      else
        b = u
      endif
      if (fu <= fw .or. w == x) then
        v  = w
        fv = fw
        w  = u
        fw = fu
      else if (fu <= fv .or. v == x .or. v == w) then
        v  = u
        fv = fu
      endif
    endif
  enddo
  stop 'brent: exceed maximum iterations'
  contains
    subroutine shft(a,b,c,d)
      use module_types, only: dp
      real(dp), intent(out)   :: a
      real(dp), intent(inout) :: b,c
      real(dp), intent(in)    :: d
      a = b; b = c; c = d
    end subroutine shft
end function brent
! ...
! =====================================================================
! ...
subroutine lnsrch(xold,fold,g,p,x,f,stpmax,check,func)

  real(dp), dimension(:), intent(in)              :: xold,g
  real(dp), intent(in)                            :: fold,stpmax
  real(dp), dimension(:), intent(inout)           :: p
  real(dp), dimension(:), intent(out)             :: x
  real(dp), intent(out)                           :: f
  logical, intent(out)                            :: check

  interface
    function func(x)
      use module_types, only: dp
      implicit none
      real(dp), dimension(:), intent(in)   :: x
      real(dp) func
    end function func
  end interface

  ! ... Local variables
  ! ...
  real(dp), parameter                             ::  ALF = 1.0D-4
  real(dp), parameter                             :: TOLX = epsilon(x)
  integer ndum
  real(dp) a,alam,alam2,alamin,b,disc,f2,pabs,rhs1,rhs2,slope,tmplam

  ndum = size(g)
  if (any([size(p),size(x),size(xold)].ne.ndum)) &
             call stop_error(1,'lnsrch: incompatible dimensions')

  check = .False.
  pabs  = vabs(p(:))
  if (pabs > stpmax) p(:) = p(:)*stpmax/pabs ! Scaling data 
  slope = dot_product(g,p)
  if (slope >= zero) call stop_error(1,'lnsrch: roundoff error')

  alamin = TOLX/maxval(abs(p(:))/max(abs(xold(:)),one)) ! Lambda min
  alam   = one
  do
    x(:) = xold(:) + alam*p(:)
    f    = func(x)
    if (alam < alamin) then
      x(:) = xold(:)
      check = .True.
      return
    else if ( f <= fold+ALF*alam*slope) then
      return    ! Sufficient function decrease
    else
      if (alam == one) then
        tmplam = -half*slope/(f-fold-slope)
      else
        rhs1 = f - fold - alam*slope
        rhs2 = f2 - fold - alam2*slope
           a = (rhs1/alam**2 - rhs2/alam2**2)/(alam-alam2)
           b = (-alam2*rhs1/alam**2 + alam*rhs2/alam2**2)/(alam-alam2)
        if (a == zero) then
          tmplam = -half*slope/b
        else
          disc = b*b - three*a*slope
          if (disc < zero) then
            tmplam = half*alam
          else if (b <= zero) then
            tmplam = (-b+sqrt(disc))/(three*a)
          else
            tmplam = -slope/(b+sqrt(disc))
          endif
        endif
        if (tmplam > half*alam) tmplam = 0.5*alam ! Lambda <= 0.5 Lambda_1
      endif
    endif
    alam2 = alam
       f2 = f
     alam = max(tmplam,0.1D0*alam)
  enddo

end subroutine lnsrch
! ...
! =====================================================================
! ...
subroutine dfpmin (p,gtol,iter,fret,func,dfunc)

  real(dp), dimension(:), intent(inout)           :: p
  real(dp), intent(in)                            :: gtol
  integer, intent(out)                            :: iter
  real(dp), intent(out)                           :: fret

  interface 
    function func(p)
      use module_types, only: dp
      real(dp), dimension(:), intent(in) :: p
      real(dp) func
    end function func

    function dfunc(p)
      use module_types, only: dp
      real(dp), dimension(:), intent(in) :: p
      real(dp), dimension(size(p))       :: dfunc
    end function dfunc
  end interface
   
  ! ... Local variables
  ! ....
  integer, parameter                 :: ITMAX = 200
  real(dp), parameter                :: STPMX = 100.D0
  real(dp), parameter                ::   EPS = epsilon(p)
  real(dp), parameter                ::  TOLX = 4.0D0*EPS

  logical check
  integer its,i,n
  real(dp)  den,fac,fad,fae,fp,stpmax,sumdg,sumxi
  real(dp), dimension(size(p))          :: dg,g,hdg,pnew,xi
  real(dp), dimension(size(p),size(p))  :: hessin

   n = size(p)
  fp = func(p)
   g = dfunc(p)

  hessin(:,:) = zero
  do i=1,n
    hessin(i,i) = one
  enddo

  xi = -g
  stpmax = STPMX*max(vabs(p),real(n,dp))
  do its=1,ITMAX
    iter = its
    call lnsrch(p,fp,g,xi,pnew,fret,stpmax,check,func)
    fp = fret
    xi = pnew - p
    p  = pnew
    if (maxval(abs(xi)/max(abs(p),one)) < TOLX) return
    
    dg = g
     g = dfunc(p)
   den = max(fret,one)
   if (maxval(abs(g)*max(abs(p),one)/den) < gtol) return

       dg = g - dg
      hdg = matmul(hessin,dg)
      fac = dot_product(dg,xi)
      fae = dot_product(dg,hdg)
    sumdg = dot_product(dg,dg)
    sumxi = dot_product(xi,xi)
    if (fac > sqrt(EPS*sumdg*sumxi)) then
      fac = one/fac
      fad = one/fae
      dg = fac*xi - fad*hdg
      hessin = hessin + fac*outerprod(xi,xi)   - &
                        fad*outerprod(hdg,hdg) + &
                        fae*outerprod(dg,dg)
    endif
    xi = -matmul(hessin,g)
  enddo 
  call stop_error(1,'dfpmin: too many iterations')

end subroutine dfpmin
! ...
! =====================================================================
! ...
end module module_math
