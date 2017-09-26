! *****************************************************************************
! ... menu.f90
! ... Routine to prose a menu with options(1:Noptions), returning the 
! ... answer.
! ... COSMO project
! ... Quim Ballabrera, March 2017
! *****************************************************************************

SUBROUTINE menu (title,options,Noptions,option)

IMPLICIT NONE
INTEGER Noptions,option
CHARACTER(LEN=*) Title
CHARACTER(LEN=*) options(Noptions)

! ... Local variables
! ...
INTEGER nmax,nn,i,ind,j
CHARACTER(LEN=78) line

WRITE(*,*)
nmax = 1
DO i=1,Noptions
   nn = LEN_TRIM(options(i))
   IF (nn.GT.nmax) nmax = nn
ENDDO

ind = (78 - 2*nmax)/2
IF (ind.LT.0) ind = 1
WRITE(*,'(T2,A)') Title
DO i=1,Noptions
   line = ' '
   WRITE(line(ind+1:ind+4),'(i2,'' -'')') i
   line(ind+6:) = options(i)
   print*, line
ENDDO
line = 'OPTION : '

10   WRITE(*,'(T2,A)',ADVANCE="no") TRIM(line)
     READ(*,'(I6)',err=10) option
     IF (option.LE.0) GOTO 10
     IF (option.GT.Noptions) GOTO 10

RETURN
END SUBROUTINE menu 
! ...
! ============================================================================
! ...
