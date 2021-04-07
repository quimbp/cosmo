! ****************************************************************************
! ... Fortran general utilitites
! ... COSMO Project
! ... Quim Ballabrera, March 2017
! ... Version 0.1, released October 2017
! ...   function compress(A)
! ...   function coords2index(X(3),N(3))
! ...   function filetype(FILENAME)
! ...
! ****************************************************************************

module cosmo

use module_types
use module_constants
use module_ascii
use module_status
use module_utils
use module_help
use module_math
use module_datetime
use module_grid
use module_rk
use module_lineargs
use module_random

implicit none

character(len=4)  :: COSMO_VERSION = "3.0"
character(len=20) :: COSMO_AUTHOR  = "Joaquim Ballabrera" 
character(len=20) :: COSMO_DATE    = "February, 2021"

end module cosmo
