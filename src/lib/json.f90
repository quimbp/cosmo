module module_json

  implicit none

  type type_attribute
    character(len=180)                         :: name
    character(len=10)                          :: type
    integer                                    :: level
    integer                                    :: from
    integer                                    :: to
  end type type_attribute

!  type type_struct
!    integer                                     :: level
!    integer                                     :: natt
!    type(type_attribute), dimension(:), pointer :: att
!  end type type_struct

  type type_json
    character(len=180)                          :: filename
    character(len=:), pointer                   :: root
    integer                                     :: natt
    type(type_attribute), dimension(:), pointer :: att

    contains
          procedure ::     load                => json_load
!         procedure ::     get                 => json_get

  end type type_json

  type type_word
    character(len=180)                         :: text
    integer                                    :: from
    integer                                    :: to
    integer                                    :: type
    integer                                    :: level
  end type type_word

  type type_key
    integer             :: type                ! 0: Brace; 1: Bracket, 2: "
    integer             :: level
    integer             :: from
    integer             :: to
  end type type_key

contains

subroutine json_load(JS,filename)

  class(type_json), intent(inout)   :: JS
  character(len=*), intent(in)      :: filename

  integer ns,iu,i,j,k,nroot
  integer brace_open,brace_close,brace_level,nbraces
  character(len=:), allocatable                :: line

  type(type_key), dimension(:), allocatable    :: braces

  
  inquire(file=filename,size=ns)

  allocate(character(ns) :: JS%root)
  allocate(character(ns) :: line)
  !allocate(JS%clevel(ns))

  JS%root = ''
  open(iu,file=filename,status='old')
  do 
    read(iu,'(T1,A)',end=2) line
    JS%root = trim(JS%root)//trim(adjustl(line))
  enddo
2 continue
  close(iu)
  ns = len_trim(JS%root)

  ! ... Get the position of the structures and vectors
  ! ...
  brace_open = 0
  brace_close = 0

  if (JS%root(1:1).ne.'{') stop 'invalid format 1'
  if (JS%root(ns:ns).ne.'}') stop 'invalid format 2'

  do i=1,ns
    if (JS%root(i:i).eq.'{') then
      brace_open = brace_open + 1
    else if (JS%root(i:i).eq.'}') then
      brace_close = brace_close + 1
    endif
  enddo

  if (brace_open.ne.brace_close) stop 'Unbalanced braces in input file'
  call get_attributes(1,JS%root,JS%natt,JS%att)
  return

end subroutine json_load
! ...
! ==================================================================================
! ...
subroutine get_attributes(level,line,natts,atts)

integer, intent(in)                                       :: level
character(len=*), intent(in)                              :: line
integer, intent(out)                                      :: natts
type(type_attribute), dimension(:), pointer, intent(out)  :: atts

! ... Local attributes
! ...
integer ns,iu,i,j,k,io,m
integer brace_open,brace_close,brace_level,nbraces
integer bracket_open,bracket_close,bracket_level,nbrackets
integer nwords,wopen
integer, dimension(:), allocatable :: blevel
character(len=1) a1

type(type_key), dimension(:), allocatable    :: braces
type(type_key), dimension(:), allocatable    :: brackets
type(type_word), dimension(:), allocatable   :: words

ns = len_trim(line)
allocate(blevel(ns))

brace_open = 0
brace_close = 0
bracket_open = 0
bracket_close = 0

do i=1,ns
  if (line(i:i).eq.'{') then
    brace_open = brace_open + 1
  else if (line(i:i).eq.'}') then
    brace_close = brace_close + 1
  else if (line(i:i).eq.'[') then
    bracket_open = bracket_open + 1
  else if (line(i:i).eq.']') then
    bracket_close = bracket_close + 1
  endif
enddo

if (brace_open.ne.brace_close) stop 'Unbalanced number of braces'
if (bracket_open.ne.bracket_close) stop 'Unbalanced number of brackets'

nbraces = brace_open
nbrackets = bracket_open

allocate(braces(nbraces))
allocate(brackets(nbrackets))

brace_open = 0
bracket_open = 0
brace_level = 0
bracket_level = 0

do i=1,ns
  blevel(i) = brace_level
  if (line(i:i).eq.'{') then
    brace_open = brace_open + 1
    brace_level = brace_level + 1
    braces(brace_open)%level = brace_level
    braces(brace_open)%from  = i
    braces(brace_open)%to    = 0
  else if (line(i:i).eq.'}') then
    ! When here, we have a level. What is the latest brace with that level?
    do j=brace_open,1,-1
      if (braces(j)%level.eq.brace_level) then
        braces(j)%to = i
        exit
      endif
    enddo
    brace_level = brace_level - 1
  else if (line(i:i).eq.'[') then
    bracket_open = bracket_open + 1
    bracket_level = bracket_level + 1
    brackets(bracket_open)%level = bracket_level
    brackets(bracket_open)%from  = i
    brackets(bracket_open)%to    = 0
  else if (line(i:i).eq.']') then
    ! When here, we have a level. What is the latest bracket with that level?
    do j=bracket_open,1,-1
      if (brackets(j)%level.eq.bracket_level) then
        brackets(j)%to = i
        exit
      endif
    enddo
    bracket_level = bracket_level - 1
  endif
enddo

! ... Get the number of words
! ...
nwords = 0
wopen  = 0
do i=1,ns
  if (line(i:i).eq.'"') then
    if (wopen.eq.0) then
      nwords = nwords + 1
      wopen  = 1
    else
      wopen = 0
    endif
  endif
enddo

allocate(words(nwords))

j = 0
wopen  = 0
do i=1,ns
  if (line(i:i).eq.'"') then
    if (wopen.eq.0) then
      j = j + 1
      wopen  = 1
      io     = i+1
    else
      words(j)%text = line(io:i-1)
      words(j)%from = io
      words(j)%to   = i-1
      words(j)%level = blevel(i-1)
      do k=1,ns
        if (line(i+k:i+k).ne.' ') then
          if (line(i+k:i+k).eq.':') then
            words(j)%type = 1
          else
            words(j)%type = 0
          endif
          exit
        endif
      enddo
      wopen = 0
    endif
  endif
enddo

natts = 0
do i=1,nwords
  !print*, i, trim(words(i)%text), words(i)%type, words(i)%level, line(words(i)%from:words(i)%to)
  if ((words(i)%type.eq.1).and.(words(i)%level.eq.1)) natts = natts + 1
enddo

allocate(atts(natts))
j = 0
do i=1,nwords

  if ((words(i)%type.eq.1).and.(words(i)%level.eq.1)) then
    j = j + 1
    atts(j)%name = trim(words(i)%text)
    atts(j)%level = level
    do k=2,ns
      io = words(i)%to + k
      a1 = line(io:io)
      if (a1.ne.' '.and.a1.ne.':') then
        if (a1.eq.'[') then
          do m=1,nbrackets
            if (brackets(m)%from.eq.io) then
              atts(j)%type = 'vector'
              atts(j)%from = brackets(m)%from+1
              atts(j)%to   = brackets(m)%to-1
              exit
            endif
          enddo
        else if (a1.eq.'{') then
          do m=1,nbraces
            if (braces(m)%from.eq.io) then
              atts(j)%type = 'struct'
              atts(j)%from = braces(m)%from
              atts(j)%to   = braces(m)%to
              exit
            endif
          enddo
        else if (a1.eq.'"') then
          atts(j)%type = 'text'
          atts(j)%from = words(i+1)%from
          atts(j)%to   = words(i+1)%to
        else
          atts(j)%type = 'other'
          atts(j)%from = io
          do m=1,ns
            if ((line(io+m:io+m).eq.',').or. &
                (line(io+m:io+m).eq.']').or. &
                (line(io+m:io+m).eq.'}')) then
              atts(j)%to = io+m-1
              exit
            endif
          enddo
        endif
        exit
      endif
    enddo
!   print*, '>> ', atts(j)%level, atts(j)%type, trim(atts(j)%name), '  ::  ', line(atts(j)%from:atts(j)%to)
  endif
enddo

deallocate(words)
deallocate(braces)
deallocate(brackets)

return
end subroutine get_attributes
! ...
! =====================================================================
! ...
function json_attribute(line,attribute) result(out)

character(len=*), intent(in)                :: line
character(len=*), intent(in)                :: attribute
character(len=:), pointer                   :: out

integer i,ns,natts
type(type_attribute), dimension(:), pointer :: atts

call get_attributes(1,line,natts,atts)
do i=1,natts
  if (trim(atts(i)%name).eq.trim(attribute)) then
    ns = atts(i)%to - atts(i)%from + 1
    allocate(character(ns) :: out)
    out = line(atts(i)%from:atts(i)%to)
    return
  endif
enddo
stop 'Attribute not found'

end function json_attribute
! ...
! =====================================================================
! ...
recursive subroutine vector_show(level,line)

integer, intent(in)                         :: level
character(len=*), intent(in)                :: line

integer ns,i,j,prev,elen,maxlen
integer nbraces,nbrackets,ncommas
logical structures,vectors,other
character(len=1) icomma
character(len=1) a
character(len=300) :: spaces = ' '

! ... Vector variables
! ...
integer nv
character(len=:), dimension(:), pointer     :: V

ns = len_trim(line)

structures = .false.
vectors    = .false.
other      = .false.
a = line(1:1)
if (a.eq.'{') then 
  structures = .true.
else if (a.eq.'[') then
  vectors = .true.
else
  other = .true.
endif

nbraces = 0
nbrackets = 0
ncommas = 0
do i=1,ns
  a = line(i:i)
  if (a.eq.'{') then
    nbraces = nbraces + 1
  else if (a.eq.'[') then
    nbrackets = nbrackets + 1
  else if (a.eq.',') then
    ncommas = ncommas + 1
  endif
enddo

if (structures) then
! -------------------
  call attribute_vector(line,nv,V)
  do i=1,nv
    write(*,'(T1,A)') spaces(1:1+level)//'{'
    call attribute_show(level+1,V(i),'{','}',i.eq.nv)
  enddo

else if (vectors) then
! --------------------
  call attribute_vector(line,nv,V)
  do i=1,nv
    icomma = ','
    if (i.eq.nv) icomma = ''
    write(*,'(T1,A)') spaces(1:2*level+2)//V(i)//icomma
  enddo


else
! ---
  nv = ncommas + 1
  maxlen = 0
  prev = 0
  ! ... Calculate the maximum element length
  ! ...
  do i=1,ns 
    a = line(i:i)
    if (a.eq.',') then
      elen = i - prev - 1
      if (elen.gt.maxlen) maxlen = elen
      prev = i
    endif
  enddo
  elen = ns - prev
  if (elen.gt.maxlen) maxlen = elen

  ! ... Allocate the appropriate array of strings
  ! ...
  allocate(character(maxlen) :: V(nv))

  ! ... Fill the strings
  prev = 0
  j = 0
  do i=1,ns 
    a = line(i:i)
    if (a.eq.',') then
      j = j + 1
      V(j) = line(prev+1:i-1)
      prev = i
    endif
  enddo
  j = j + 1
  V(j) = line(prev+1:ns)

  do j=1,nv
    if (j.lt.nv) then
      write(*,'(T1,A,A)') spaces(1:2*level), trim(V(j))//','
    else
      write(*,'(T1,A,A)') spaces(1:2*level), trim(V(j))
    endif
  enddo

endif

end subroutine vector_show
! ...
! =====================================================================
! ...
recursive subroutine attribute_vector(line,nv,V)

character(len=*), intent(in)                           :: line
integer, intent(out)                                   :: nv
character(len=:), dimension(:), pointer, intent(out)   :: V

! ... Local variables
! ...
integer ns,i,j,l,elen,maxlen,ncommas
integer quote_open
integer key_open,key_close,key_level,nkeys
character(1) a

type(type_key), dimension(:), allocatable    :: keys

ns = len_trim(line)

!print*, 'line: ', trim(line)
!print*, 'ns = ',ns

key_open = 0
key_close = 0
quote_open = 0
ncommas = 0
do i=1,ns
  a = line(i:i)
  if ((a.eq.'{').or.(a.eq.'[')) key_open = key_open + 1
  if ((a.eq.'}').or.(a.eq.']')) key_close = key_close + 1
  if (a.eq.'"') then
    if (quote_open.eq.0) then
      quote_open = 1
      key_open = key_open + 1
    else 
      quote_open = 0
      key_close = key_close + 1
    endif
  endif
enddo
if (key_open.ne.key_close) stop 'Unbalanced number of keys'
nkeys = key_open
allocate(keys(nkeys))

!print*, 'nkeys: ', nkeys

l = 0
key_level = 0
quote_open = 0
do i=1,ns
  a = line(i:i)
  if ((a.eq.'{').or.(a.eq.'[')) then
    l  = l  + 1
    key_level = key_level + 1
    keys(l)%level = key_level
    keys(l)%from  = i
    keys(l)%to    = 0
    if (a.eq.'{') keys(l)%type = 0
    if (a.eq.'[') keys(l)%type = 1
  else if ((a.eq.'}').or.(a.eq.']')) then
    ! When here, we have a level. What is the latest brace with that level?
    do j=l,1,-1
      if (keys(j)%level.eq.key_level) then
        keys(j)%to = i
        exit
      endif
    enddo
    key_level = key_level - 1
  else if (a.eq.'"') then
    if (quote_open.eq.0) then
      quote_open = 1
      l = l + 1
      key_level = key_level + 1
      keys(l)%level = key_level
      keys(l)%type  = 2
      keys(l)%from  = i
      keys(l)%to    = 0
    else
      quote_open = 0
      do j=l,1,-1
        if (keys(j)%level.eq.key_level) then
          keys(j)%to = i
          exit
        endif
      enddo
      key_level = key_level - 1
    endif
  endif
enddo

nv = 0
do i=1,nkeys
  if (keys(i)%level.eq.1) nv = nv  + 1
enddo

maxlen = 0
do i=1,nkeys
  if (keys(i)%level.eq.1) then
    elen = keys(i)%to - keys(i)%from + 1
    if (elen.gt.maxlen) maxlen = elen
  endif
enddo

allocate(character(maxlen) :: V(nv))
  
j = 0
do i=1,nkeys
  if (keys(i)%level.eq.1) then
    j = j + 1
    V(j) = line(keys(i)%from:keys(i)%to)
  endif
enddo

return 

end subroutine attribute_vector
! ...
! =====================================================================
! ...
recursive subroutine attribute_show(level,line,opener,closer,last)

integer, intent(in)                         :: level
character(len=*), intent(in)                :: line
character(len=1), intent(in)                :: opener
character(len=1), intent(in)                :: closer
logical, intent(in)                         :: last

integer                                     :: ns
integer                                     :: natt,natt2
integer                                     :: i,j,i2,j2
integer                                     :: level2
type(type_attribute), dimension(:), pointer :: att,att2
character(len=1)   :: comma,icomma
character(len=300) :: spaces = ' '

! ... Vector variables
! ...
integer nv
character(len=:), dimension(:), pointer     :: V

if (last) then
  comma = ''
else
  comma = ','
endif

!print*, '>>>>>       Attribute_show, level: ', level, opener, closer, last

ns = len_trim(line)
!if ((line(1:1).ne.'{').or.(line(ns:ns).ne.'}')) stop 'error: invalid json structure'

call get_attributes(1,line,natt,att)

if (level.eq.1) then
  write(*,'(T1,A)') '{'
  !write(*,'(T1,A)') spaces(0:level)//'{'
endif

do i=1,natt
   icomma = ','
   if (i.eq.natt) icomma = ''

   if (att(i)%type.eq.'text') then
   ! ------------------------------
     write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": '//'"'//line(att(i)%from:att(i)%to)//'"'//icomma

   else if (att(i)%type.eq.'other') then
   ! -----------------------------------
!    if (i.lt.natt) then
!      write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": '//line(att(i)%from:att(i)%to)//','
!    else
       write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": '//line(att(i)%from:att(i)%to)//icomma
!    endif
   else if (att(i)%type.eq.'vector') then
   ! -------------------------------------
     ! ... Check for empty vector
     ! ...
     if (att(i)%to-att(i)%from + 1.le.0) then
       write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": []'//icomma
       !if (i.lt.natt) then
       !  write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": [],'
       !else
       !  write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": []'
       !endif
     else
       write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": ['
       call vector_show(level+1,line(att(i)%from:att(i)%to))

       write(*,'(T1,A)') spaces(1:2*level)//']'//icomma
       !if (i.lt.natt) then
       !  write(*,'(T1,A)') spaces(1:2*level)//'],'
       !else
       !  write(*,'(T1,A)') spaces(1:2*level)//']'
       !endif
     endif
   else if (att(i)%type.eq.'struct') then
   ! -------------------------------------
     write(*,'(T1,A,A)') spaces(1:2*level), '"'//trim(att(i)%name)//'": {'
     call attribute_show(level+1,line(att(i)%from:att(i)%to),'{','}',i.eq.natt)
   endif
enddo

if (level.eq.1) then
  write(*,'(T1,A)') '}'
else
  write(*,'(T1,A)') spaces(1:level)//'}'//comma
endif


end subroutine attribute_show
! ...
! ==================================================================================

subroutine json_show(line)

character(len=*), intent(in)                :: line

call attribute_show(1,line,'{','}',.True.)

end subroutine json_show
! ...
! ==================================================================================
! ...
end module module_json
