

PROGRAM MLM

  USE netcdf        !Required to manage NetCDF files
 

  IMPLICIT NONE     !para que no se declaren variables de forma implicita


  !!**********************************************************************
  !!*************  Constantes del programa *******************************
  !!**********************************************************************
  INTEGER, PARAMETER :: MAXLEN= 400  !! longitud máxima de caracteres a leer
  CHARACTER (len = *), PARAMETER :: VERSION = "MLM version 0.1"
  DOUBLE PRECISION, PARAMETER :: CERO = 1.0E-30 !! Valor que considero nulo
  DOUBLE PRECISION, PARAMETER :: CEROP = 1.0E-20 !! Valor de distancia^2 a partir del cual se consideran dos puntos iguales
  DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846D0 !! Valor de PI
  DOUBLE PRECISION, PARAMETER :: RADIOTIERRA = 6371000.0D0 !Radio medio de la Tierra en metros


  CHARACTER (len = *), PARAMETER :: UNITS = "units" !To check the units attributes
  CHARACTER (len = *), PARAMETER :: VELOCITY_UNITS = "m s-1" ! To check velocity units
  CHARACTER (len = *), PARAMETER :: LATITUDE_UNITS = "degrees_north" !To check latitude units 
  CHARACTER (len = *), PARAMETER :: LONGITUDE_UNITS = "degrees_east" !To check longitude units
  CHARACTER (len = *), PARAMETER :: FILLVALUE_ATTRIB ="_FillValue" !Name of FillValue attribute
  CHARACTER (len = *), PARAMETER :: SCALE_ATTRIB = "scale_factor" ! Name of scale factor attribute
  CHARACTER (len = *), PARAMETER :: OFFSET_ATTRIB = "add_offset"  ! Name for offset attribute


  !!**********************************************************************
  !!********** TIPOS DE DATOS QUE UTILIZO ********************************
  !!**********************************************************************

  TYPE Punto                   !defino un tipo para manejar puntos 2D
     DOUBLE PRECISION :: x,y
  END TYPE Punto

  TYPE PuntoConFallo           !defino un tipo para manejar puntos 2D y guardar información sobre errores
     TYPE (Punto) :: p
     INTEGER :: fallo
  END TYPE PuntoConFallo


  TYPE TrajectoryPoint           !defino un tipo para manejar puntos 2D de una trayectoria
     TYPE (Punto) :: p
     DOUBLE PRECISION :: time
  END TYPE TrajectoryPoint
 
  !!*********************************************************************
  !!*****   variables globales del programa  ****************************
  !!*********************************************************************
  

  ! 
  LOGICAL :: debugging  !! activate debugging messages
  INTEGER :: status     !! to manage errors when working with files
  CHARACTER (len = MAXLEN) :: time_units  ! To get time units for velocity field
  DOUBLE PRECISION :: start, finish  ! To get the total computation time

  ! variables to manage velocity fields
  DOUBLE PRECISION :: FACTOR = 1.0D0 !! para pasar de m/s a m/snapshot
  DOUBLE PRECISION :: InitialJulianDate  !Julian Date of start date of time variable in velocity fields
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: mivx, mivy !! matrices donde leo U y V de los datos.
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: lat, lon !! vectores para la latitud y longitud de la región de estudio.
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: time !! vector para tener los tiempos de cada fotico
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: auxlat !! matriz auxiliar
  ! para la latitud, la cual necesitaré para deshacer el cambio de coordenadas
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:,:,:) :: vx, vy !! matrices 
  !donde guardo U y V, tras llevar el dominio a la región [0,1]x[0,1] y la rejilla
  !de la dada en la esfera a una rejilla con geometría plana.
  INTEGER :: nx, ny         !! números de datos a lo largo del eje X e Y  
  INTEGER :: time_I, time_F    !! Dias inicial y final de los datos que cargo

  LOGICAL, ALLOCATABLE, DIMENSION (:,:) :: tierra  !! matriz donde guardo las posiciones de tierra

  !Matrices para guardar la velocidad en 4 tiempos para interpolar
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: vx1,vx2,vx3,vx4,vy1,vy2,vy3,vy4

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: milat, milon !! matrices para
  ! los nuevos valores de latitud y longitud, tras cambio de variable a [0,1] 
  
!  INTEGER, DIMENSION(16,16) :: weights  !matriz de pesos para la interpolación bicúbica
  DOUBLE PRECISION, DIMENSION(16,16) :: weights  !matriz de pesos para la interpolación bicúbica
  
  !! Variables para el tratamiento de los parámetros de entrada
  !CHARACTER(:), ALLOCATABLE :: arg   !! argumentos introducidos en línea de comandos
  CHARACTER(len = MAXLEN) :: arg   !! argumentos introducidos en línea de comandos

  CHARACTER(:), ALLOCATABLE :: outfile !! output file for the simulation
  CHARACTER(:), ALLOCATABLE :: ufile !! input file for U field
  CHARACTER(nf90_max_name) :: uname !! name for variable containing U field in nc file
  CHARACTER(nf90_max_name) :: tnameu !! name for variable containing time in U nc file
  CHARACTER(nf90_max_name) :: lonnameu !! name for variable containing longitudes in U nc file
  CHARACTER(nf90_max_name) :: latnameu !! name for variable containing latitudes in U nc file

  CHARACTER(:), ALLOCATABLE :: vfile !! input file for V field
  CHARACTER(nf90_max_name) :: vname !! name for variable containing V field in nc file
  CHARACTER(nf90_max_name) :: tnamev !! name for variable containing time in U nc file
  CHARACTER(nf90_max_name) :: lonnamev !! name for variable containing longitudes in U nc file
  CHARACTER(nf90_max_name) :: latnamev !! name for variable containing latitudes in U nc file

   CHARACTER(:), ALLOCATABLE :: releasefile !! release file for trajectories 

  !required variables to define simulation to perform
  TYPE (Punto) :: x0  ! initial point of simulation
  DOUBLE PRECISION :: paso   ! time step to simulate trajectories
  DOUBLE PRECISION :: t0   !! initial time to compute trayectories
  DOUBLE PRECISION :: t1   !! length of simulation to compute trayectories
  INTEGER :: seed          !!
  INTEGER :: nfloats       !! number of floats to generate as release points
  DOUBLE PRECISION :: Rx   !! Size of X radius to generate uniformly random initial positions around (X0,Y0)
  DOUBLE PRECISION :: Ry   !! Size of Y radius to generate uniformly random initial positions around (X0,Y0)
  LOGICAL :: readFile      !! To know when the initial points must be read from file "releasefile"

!#################################################
!# The main program starts here
!#################################################


  CALL cpu_time(start)
  CALL defaultParameters() !! To stablish default values
  CALL getCommandLine() 
  CALL ReadNetCDF()     !read nc file
  CALL datosIniciales()  !! Inicializa variables que siempre se usan a sus valores


  CALL convertirDatos(time_F) !! Cambio coordenadas a [0,1]x[0,1]

  CALL SimulateTrayectories()
  PRINT*, " SIMULATION COMPLETED !!"
  CALL cpu_time(finish)
  PRINT '("Computation time = ",f10.3," seconds.")',finish-start
  

!*********************************************************************
!********** Definition of local functions and procedures *************
!*********************************************************************

CONTAINS
  
!**************************************************************
!*** Rutina para analizar le línea de comandos y **************
!*** obtener los parámetros para hacer la simulación **********
!*************************************************************
SUBROUTINE getCommandLine()
  LOGICAL :: finalizar  !! boolean variable to control end condition in loops
  INTEGER :: pos


  pos = 0
  DO
     CALL get_command_argument(pos, arg)
     IF (LEN_TRIM(arg) == 0) EXIT  
     SELECT CASE (arg(1:2))
     CASE ('-U')
        finalizar=.FALSE.
        DO WHILE (.NOT.(finalizar))
           CALL get_command_argument(pos + 1, arg)
           IF (LEN_TRIM(arg) == 0) EXIT
           IF (arg(1:5).EQ.'file=') THEN
              ufile=arg(6:)
              finalizar=.TRUE.
           ELSE
              SELECT CASE (arg(1:2))
              CASE ('u=')
                 uname=arg(3:)
                 finalizar=.TRUE.   
              CASE ('x=')
                 lonnameu=arg(3:)
                 finalizar=.TRUE.
              CASE ('y=')
                 latnameu=arg(3:)
                 finalizar=.TRUE.
              CASE ('t=')
                 tnameu=arg(3:)
                 finalizar=.TRUE.
              CASE DEFAULT
                 EXIT  !The next parameter is not valid as a U option 
              END SELECT
           ENDIF
           IF (finalizar) THEN
              finalizar=.FALSE.  ! I have read a valid parameter, then I try to read the next
              pos = pos + 1
           END IF
        END DO 
     CASE ('-V')
        finalizar=.FALSE.
        DO WHILE (.NOT.(finalizar))
           CALL get_command_argument(pos + 1, arg)
           IF (LEN_TRIM(arg) == 0) EXIT
           IF (arg(1:5).EQ.'file=') THEN
              vfile=arg(6:)
              finalizar=.TRUE.
           ELSE
              SELECT CASE (arg(1:2))
              CASE ('v=')
                 vname=arg(3:)
                 finalizar=.TRUE.   
              CASE ('x=')
                 lonnamev=arg(3:)
                 finalizar=.TRUE.
              CASE ('y=')
                 latnamev=arg(3:)
                 finalizar=.TRUE.
              CASE ('t=')
                 tnamev=arg(3:)
                 finalizar=.TRUE.
              CASE DEFAULT
                 EXIT  !The next parameter is not valid as a V option 
              END SELECT
           ENDIF
           IF (finalizar) THEN
              finalizar=.FALSE.  ! I have read a valid parameter, then I try to read the next
              pos = pos + 1
           ENDIF
        END DO  
     CASE('-d')
        debugging = .TRUE.
     CASE('-i')
        IF (arg(2:).EQ. 'idt') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) paso
        ENDIF
     CASE('-n')
        IF (arg(2:).EQ. 'nfloats') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) nfloats
        ENDIF
     CASE ('-o')
        IF (arg(2:4).EQ. 'out') THEN
           CALL get_command_argument(pos + 1, arg)
           outfile = arg
        ENDIF
     CASE ('-R')
        IF (arg(2:3).EQ. 'Rx') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) Rx
        ENDIF
        IF (arg(2:3).EQ. 'Ry') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) Ry
        ENDIF
     CASE ('-r')
        IF (arg(2:8).EQ. 'release') THEN
           CALL get_command_argument(pos + 1, arg)
           releasefile = arg
           readfile = .TRUE.
        ENDIF        
     CASE ('-t')
        IF (arg(2:3).EQ. 't0') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) t0
        ENDIF
        IF (arg(2:).EQ. 'time_sim') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) t1  !this value is in days
           t1 = t1 * 24.0D0 * 60.0D0 * 60.0D0 !conversion to seconds
        ENDIF
     CASE ('-s')
        IF (arg(2:).EQ. 'seed') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) seed
        ENDIF
     CASE ('-x')
        IF (arg(2:3).EQ. 'x0') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) x0%x
        ENDIF
     CASE ('-y')
        IF (arg(2:3).EQ. 'y0') THEN
           CALL get_command_argument(pos + 1, arg)
           READ(arg,*) x0%y
        ENDIF
     END SELECT
     pos = pos + 1
  END DO
  

  IF (debugging) THEN
     PRINT*, "file for U field --> ", TRIM(ufile)
     PRINT*, "name for variable U in the file --> ", TRIM(uname) 
     PRINT*, "name for variable lon in the file for U field --> ", TRIM(lonnameU)
     PRINT*, "name for variable lat in the file for U field --> ", TRIM(latnameU)
     PRINT*, "name for time variable in the file for U field --> ", TRIM(tnameU)
     PRINT*, "file for V field --> ", TRIM(vfile)
     PRINT*, "name for variable V in the file --> ", TRIM(vname) 
     PRINT*, "name for variable lon in the file for V field --> ", TRIM(lonnameV)
     PRINT*, "name for variable lat in the file for V field --> ", TRIM(latnameV)
     PRINT*, "name for time variable in the file for V field --> ", TRIM(tnameV)
     PRINT*, "name for release file --> ", TRIM(releasefile)
     IF (readFile) THEN
        PRINT*, "Simulation will load initial positions from file--> ", TRIM(releasefile)
     ELSE
        PRINT*, "Simulation will NOT load initial positions from file--> ", TRIM(releasefile)
     ENDIF
     PRINT*, "name for output file --> ", TRIM(outfile)
     PRINT*, "time step for simulation (in seconds) --> ", paso
     PRINT*, "initial time, t0, to start simulation (seconds after initial simulation time) --> ", t0
     PRINT*, "lenght of simulaiton (in seconds) --> ", t1
     PRINT*, "initial release point x0 --> ", x0
     PRINT*, "number of floats to be randomly generated --> ", nfloats
     PRINT*, "Random Rx radius --> ", Rx
     PRINT*, "Random Ry radius --> ", Ry
     PRINT*, "seed for pseudorandom generator --> ", seed
  ENDIF

  
END SUBROUTINE getCommandLine


  
!*********************************************************************
!**** Es mi modificación para leer campos de velocidad  U y V   ******
!**** Esta función reserva memoria de forma dinámica tanto para ******
!**** el campo de velocidades final a usar como para la         ******
!**** máscara de tierra (que la intenta deducir del primer mes  ******
!**** leído de los datos. El primer número indica el primer mes ******
!**** que lee y el segundo selecciona el último mes de lectura  ******
!**** de los datos. Carga los datos del Mediterráneo completo.  ******
!**** En el programa principal hay que seleccionar Time_I de    ******
!**** forma que coincida con el primer día del primer mes       ******
!**** leído y time_F de forma que sea mayor al último día leído.******
!*********************************************************************

SUBROUTINE ReadNetCDF()

  CHARACTER (len = MAXLEN)  :: attribute  !! to check unit of variables

  DOUBLE PRECISION :: FillValue !! FillValue in nc file
  DOUBLE PRECISION :: scale ! scale factor to read data from nc file
  DOUBLE PRECISION :: offset ! offset to add to read data from nc file
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: data_in_velo !array to read vector field values (U,V)

  ! Esto es para guardar la IDentidad para el archivo, la variable y su tamaño.
  INTEGER :: ncid, varid, dimID!, ndims_in,nvars_in,ngatts_in,unlimdimid_in,numerodiasmes
  INTEGER :: i,j,k




  ! Abrimos el archivo. NF90_NOWRITE indica que abrimos el archivo como solo lectura.
  status = nf90_open(ufile, NF90_NOWRITE, ncid) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while opening nc file --> "// NF90_STRERROR(status))
  
!******************* LONGITUDES ************************************
  ! Obtenemos la identidad de la variable usando su nombre.
  status = nf90_inq_varid(ncid, lonnameu, varid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting longitude variable id --> "// NF90_STRERROR(status))
  ! Getting ID for dimension of array
  status = nf90_inq_dimid(ncid, lonnameu, DimID)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting longitude dimension id --> "// NF90_STRERROR(status))
  ! How many values of "longitude" are there?
  status = nf90_inquire_dimension(ncid, DimID, len = nx)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting longitude size --> "// NF90_STRERROR(status))
  
  ALLOCATE(lon(0:nx-1),STAT=status)   ! Assigning memory for longitude vector
  IF (status.NE.0) CALL Handle_error("A problem happened allocating memory for longitudes")
  ! Read longitude values
  status = nf90_get_var(ncid, varid, lon)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened reading longitude values --> "// NF90_STRERROR(status))
  ! Checking longitude units
  status = nf90_get_att(ncid, varid, UNITS, attribute)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting longitude units --> "// NF90_STRERROR(status))
!  IF (attribute.NE.LONGITUDE_UNITS) CALL Handle_error("Longitude units must be degrees_east")
  IF (debugging) THEN
     PRINT*, "Size of longitude vector --> ", nx
     PRINT*, "Longitude units --> ", TRIM(attribute)
  END IF

!************* LATITUDES *********************************
  ! Obtenemos la identidad de la variable usando su nombre.
  status = nf90_inq_varid(ncid, latnameu, varid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting latitude variable id --> "// NF90_STRERROR(status))
  ! Getting ID for dimension of array
  status = nf90_inq_dimid(ncid, latnameu, DimID)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting latitude dimension id --> "// NF90_STRERROR(status))
  ! How many values of "latitude" are there?
  status = nf90_inquire_dimension(ncid, DimID, len = ny)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting latitude size --> "// NF90_STRERROR(status))
  
  ALLOCATE(lat(0:ny-1),STAT=status)   ! Assigning memory for latitude vector
  IF (status.NE.0) CALL Handle_error("A problem happened allocating memory for latitudes")
  ! Read latitude values
  status = nf90_get_var(ncid, varid, lat)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened reading latitude values --> "// NF90_STRERROR(status))
    ! Checking latitude units
  status = nf90_get_att(ncid, varid, UNITS, attribute)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting latitude units --> "// NF90_STRERROR(status))
!  IF (attribute.NE.LATITUDE_UNITS) CALL Handle_error("Latitude units must be degrees_north")
  IF (debugging) THEN
     PRINT*, "Size of latitude vector --> ", ny
     PRINT*, "Latitude units --> ", TRIM(attribute)
  END IF

!******** TIME variable ************************************

  ! Obtenemos la identidad de la variable usando su nombre.
  status = nf90_inq_varid(ncid, tnameu, varid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting time variable id --> " // NF90_STRERROR(status))
  ! Getting ID for dimension of array
  status = nf90_inq_dimid(ncid, tnameu, DimID)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting time dimension id --> "// NF90_STRERROR(status))
  ! How many snapshots are there?
  status = nf90_inquire_dimension(ncid, DimID, len = time_F)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting time size --> "// NF90_STRERROR(status))
  IF (time_F.LT.4) CALL Handle_error("Bicubic interpolation cannot be used with less than FOUR snapshots !!")
  !*******************************************
  time_I = 0 !apaño para que rule con mi código
  time_F = time_F-1  
  !*********************************************
  ALLOCATE(time(0:time_F),STAT=status)   ! Assigning memory for time vector
  IF (status.NE.0) CALL Handle_error("A problem happened allocating memory for latitudes")
  ! Read time values
  status = nf90_get_var(ncid, varid, time)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened reading time values --> "// NF90_STRERROR(status))
  ! Checking time units
  status = nf90_get_att(ncid, varid, UNITS, time_units)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting time units --> "// NF90_STRERROR(status))
  factor = SetFactor(time_units)
  InitialJulianDate = SetInitialJulianDate(time_units)
  IF (debugging) THEN
     PRINT*, "Size of time vector --> ", time_F
     PRINT*, "Time units --> ", TRIM(time_units)
     PRINT*, "Seconds between snapshots --> ", factor, " seconds"
     PRINT*, "Initial Julian date --> ", InitialJulianDate, " days"
  END IF

!! AHORA LEEMOS U y V **************************************

  ALLOCATE(mivx(0:nx-1,0:ny-1,time_I:time_F), STAT = status) !Reservo memoria para la componente X de la velocidad
    IF (status.NE.0) CALL Handle_error("A problem happened allocating memory for U field")
    ALLOCATE(mivy(0:nx-1,0:ny-1,time_I:time_F), STAT = status) !Reservo memoria para la componente Y de la velocidad
      IF (status.NE.0) CALL Handle_error("A problem happened allocating memory for V field")
  ALLOCATE(tierra(0:nx-1,0:ny-1), STAT = status)  ! Reservo memoria para la máscara de tierra
  IF (status.NE.0) CALL Handle_error("A problem happened allocating memory for land mask")

  DO i=0,nx-1
     DO j=0,ny-1
        tierra(i,j)=.FALSE.   !Lo pongo todo a agua
     END DO
  END DO
  
  

  ALLOCATE(data_in_velo(0:nx-1,0:ny-1,time_I:time_F), STAT = status)
  IF (status.NE.0) CALL Handle_error("A problem happened allocating memory before reading U field")

  
  ! Obtenemos la identidad de la variable usando su nombre.
  status = nf90_inq_varid(ncid, uname, varid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting U field ID --> "// NF90_STRERROR(status))
  ! Leemos los datos de velocidad
  status = nf90_get_var(ncid, varid, data_in_velo) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened reading U field --> "// NF90_STRERROR(status))
  ! Checking U field units
  status = nf90_get_att(ncid, varid, UNITS, attribute)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting U field units --> "// NF90_STRERROR(status))
  !IF (attribute.NE.VELOCITY_UNITS) CALL Handle_error("U field units must be m s-1")
  ! Getting the FillValue attribute for U field
  status = nf90_get_att(ncid, varid, FILLVALUE_ATTRIB, FillValue)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting FillValue for U field --> "// NF90_STRERROR(status))
  ! Getting the scale attribute for U field
  status = nf90_get_att(ncid, varid, SCALE_ATTRIB, scale)
  !IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting scale factor for U field --> "// NF90_STRERROR(status))
  IF (status /= nf90_noerr) scale = 1.0D0
  ! Getting the offset attribute for U field
  status = nf90_get_att(ncid, varid, OFFSET_ATTRIB, offset)
  !IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting offset for U field --> "// NF90_STRERROR(status))
  IF (status /= nf90_noerr) offset = 0.0D0

  IF (debugging) THEN
     PRINT*, "Size of U field --> ", nx,"x",ny,"x",time_F 
     PRINT*, "U field units --> ", TRIM(attribute)
     PRINT*, "FillValue for U field --> ", FillValue
     PRINT*, "scale for U field --> ", scale
     PRINT*, "offset to add to U field --> ", offset
  END IF
  DO k= time_I, time_F
     DO i=0, nx-1
        DO j=0, ny-1
           !IF ((IEEE_IS_NAN(data_in_velo(i,j,k))).OR.(DABS(data_in_velo(i,j,k)-FillValue).LT.CEROP)) THEN
           IF ((data_in_velo(i,j,k) .NE. data_in_velo(i,j,k)).OR.(DABS(data_in_velo(i,j,k)-FillValue).LT.CEROP)) THEN
              !The first condition is to check whether the value is NaN or not. 
              mivx(i,j,k) = CERO
              tierra(i,j) = .TRUE.
           ELSE
              mivx(i,j,k) = DBLE(data_in_velo(i,j,k) * scale + offset)
           ENDIF
        END DO
     END DO
  END DO

  DEALLOCATE(data_in_velo, STAT = status) ! Liberamos memoria
  IF (status /= 0) CALL Handle_error("A problem happened freeing memory after reading U field")

  
  ! Ahora leemos la V
  ALLOCATE(data_in_velo(0:nx-1,0:ny-1,time_I:time_F), STAT = status)
  IF (status.NE.0) CALL Handle_error("A problem happened allocating memory before reading V field")
  ! Obtenemos la identidad de la variable usando su nombre.
  status = nf90_inq_varid(ncid, vname, varid) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting V field ID --> "// NF90_STRERROR(status))
  ! Leemos los datos de velocidad
  status = nf90_get_var(ncid, varid, data_in_velo)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened reading V field --> "// NF90_STRERROR(status))
  ! Checking V field units
  status = nf90_get_att(ncid, varid, UNITS, attribute)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting V field units --> "// NF90_STRERROR(status))
  !IF (attribute.NE.VELOCITY_UNITS) CALL Handle_error("V field units must be m s-1")
  ! Getting the FillValue attribute for V field
  status = nf90_get_att(ncid, varid, FILLVALUE_ATTRIB, FillValue)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting FillValue for V field --> "// NF90_STRERROR(status))
  ! Getting the scale attribute for U field
  status = nf90_get_att(ncid, varid, SCALE_ATTRIB, scale)
  !IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting scale factor for V field --> "// NF90_STRERROR(status))
  IF (status /= nf90_noerr) scale = 1.0D0
  ! Getting the offset attribute for U field
  status = nf90_get_att(ncid, varid, OFFSET_ATTRIB, offset)
  !IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting offset for V field --> "// NF90_STRERROR(status))
  IF (status /= nf90_noerr) offset = 0.0D0
  IF (debugging) THEN
     PRINT*, "Size of V field --> ", nx,"x",ny,"x",time_F 
     PRINT*, "V field units --> ", TRIM(attribute)
     PRINT*, "FillValue for V field --> ", FillValue
     PRINT*, "scale for V field --> ", scale
     PRINT*, "offset to add to V field --> ", offset
  END IF
  DO k= time_I, time_F
     DO i=0, nx-1
        DO j=0, ny-1
           !IF ((IEEE_IS_NAN(data_in_velo(i,j,k))).OR.(DABS(data_in_velo(i,j,k)-FillValue).LT.CEROP)) THEN
           IF ((data_in_velo(i,j,k) .NE. data_in_velo(i,j,k)).OR.(DABS(data_in_velo(i,j,k)-FillValue).LT.CEROP)) THEN 
              !The first condition is to check whether the value is NaN or not. 
              mivy(i,j,k) = CERO
              tierra(i,j) = .TRUE.
           ELSE
              mivy(i,j,k) = DBLE(data_in_velo(i,j,k) * scale + offset)
           ENDIF
        END DO
     END DO
  END DO

 
 
  ! Cerramos el archivo y liberamos los recuros que ya no son necesarios.
  status = nf90_close(ncid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while closing nc file --> "// NF90_STRERROR(status))
  DEALLOCATE(data_in_velo,STAT = status) ! Liberamos memoria
  IF (status /= 0) CALL Handle_error("A problem happened freeing memory after reading V field")

END SUBROUTINE ReadNetCDF


!*******************************************************************
!********** Routine to print error messages ************************
!********** and STOP the program ***********************************
!*******************************************************************
SUBROUTINE Handle_error(message)
  CHARACTER (len = *) :: message

     PRINT*, message
     STOP
END SUBROUTINE Handle_error


!*******************************************************************
!********** para establecer el factor que convierte ****************
!********** de m/s a m/fotico y que las unidades sean las **********
!********** correctas. En message paso la cadena de las ************
!********** unidades de tiempo que obtengo del fichero nc  *********
!********** tambien comprueba que el vector de tiempo es ***********
!********** homogeneo y no hay cambios en el delta temporal ********
!********** entre las foticos del campo de velocidad       *********
!*******************************************************************
FUNCTION SetFactor(message)
  DOUBLE PRECISION :: SetFactor
  CHARACTER (len = *) :: message
  INTEGER :: pos
  DOUBLE PRECISION :: deltaTiempo  ! para calcular la diferencia de tiempo entre foticos
  LOGICAL :: finalizar !To stop the loop when needed

  deltaTiempo = time(time_I+1)-time(time_I)
  
  DO pos = (time_I+1), (time_F-1)
     IF (DABS(time(pos+1)-time(pos)-deltaTiempo).GT.CERO)  THEN
        PRINT*, time_I, time_F
        PRINT*, pos
        PRINT*, time(pos+1)
        PRINT*, time(pos)
        PRINT*, deltaTiempo
        PRINT*, DABS(time(pos+1)-time(pos)-deltaTiempo)
        CALL Handle_error("Time passed between snapshots must be equal. TO BE GENERALISED SOON IN NEW VERSION")
     ENDIF
  ENDDO
  ! So the time between snapshots is the same, we can continue
  IF (LEN_TRIM(message) .EQ. 0) CALL Handle_error("Time units NOT found in nc file")
  ! There are some text in time attribute, we can look for time units
  finalizar = .FALSE.
  pos = 1
  DO WHILE (.NOT.(finalizar))
     SELECT CASE (message(pos:pos))
     CASE ('s')
        IF (message(pos:pos+LEN('second')-1).EQ.'second') THEN
           SetFactor=1.0D0
           finalizar = .TRUE.
        ENDIF
     CASE ('m')
        IF (message(pos:pos+LEN('minute')-1).EQ.'minute') THEN
           SetFactor=60.0D0
           finalizar = .TRUE.
        ENDIF
     CASE ('h')
        IF (message(pos:pos+LEN('hour')-1).EQ.'hour') THEN
           SetFactor=60.0D0*60.0D0
           finalizar = .TRUE.
        ENDIF
     CASE ('d')
        IF (message(pos:pos+LEN('day')-1).EQ.'day') THEN
           SetFactor=60.0D0*60.0D0*24.0D0
           finalizar = .TRUE.
        ENDIF
     END SELECT
     pos = pos + 1 ! check the next letter
     IF (pos.GT.(LEN(message)-LEN('second'))) CALL Handle_error("Time units NOT found in nc file")
  ENDDO
  SetFactor = SetFactor*deltaTiempo ! time in second between snapshots
END FUNCTION SetFactor





!*********************************************************************
!*** Esta rutina advecta una partícula situada en el tiemo inicial tiempo0 
!*** en la posición punto0. Lo hace hasta el instante de tiempo tiempo1
!*** (o hasta que se salga del dominio de los datos, que si la salida es por
!*** pantalla, lo indicará. En caso de salida a archivo .kml cerrará
!*** el archivo en ese momento).
!*** En el caso que tiempo1 sea anterior a tiempo0, la rutina hará
!*** una advección hacia atrás en el tiempo.
!*** Se le puede pasar el tamaño del Runge-Kutta en la variable pasoRunge.
!*** Si escribirKML es TRUE, la salida la hará al archivo kml que se le
!*** proporcione en la variable nombreKML. En caso que escribirKML sea FALSE,
!*** la salida se hará por pantalla, donde la primera columna es el tiempo,
!*** la segunda la coordenada X de la posición de la partícula y la
!*** tercera la coordenada Y.
!***********************************************************************
SUBROUTINE AdvectaParticula(tiempo0,tiempo1,punto0,pasoRunge, trajectory)

  DOUBLE PRECISION :: tiempo0, tiempo1  ! Tiempo inicial y final para el cálculo
  DOUBLE PRECISION :: tiempoaux, tiempo1aux ! tiempos convertidos en el intervalo [time_I,time_F] ARREGLAR ESTE CHANCHULLO
  DOUBLE PRECISION :: pasoRunge,pasoaux ! Valor para el paso del Runge-Kutta
  TYPE (TrajectoryPoint),ALLOCATABLE, DIMENSION(:) :: trajectory  ! vector con la trayectoria simulada
  TYPE (Punto) :: punto0  !Punto inicial de la trayectoria
  INTEGER :: codigoerror  ! Para controlar si la trayectoria se sale del dominio de los datos
  TYPE (PuntoConFallo) :: puntoyfallo ! Estructura para controlar el punto y los códigos de errores
  INTEGER :: nsteps !! número de puntos que guardo para cada trayectoria
  INTEGER :: i

  pasoaux =pasoRunge  ! Para evitar que se modifique la variable original en caso que tenga que cambiar el signo
  tiempoaux = tiempo0  !Para evitar que se modifique la variable original en los cálculos.
  tiempo1aux = tiempo1 !Para evitar que se modifique la variable original en los cálculos.
 
  IF (tiempo1.LT.tiempoaux) THEN 
     pasoaux=-pasoaux  ! Para que advecte hacia atrás en el tiempo
  ENDIF

  ! Para advectar una particula
  puntoyfallo%p=convertir(punto0)
  IF (debugging)  PRINT*, (tiempoaux), desconvertir(puntoyfallo%p)
  
  !CUIDADO, Hago transformación para pasar tiempos al intervalo [time_I,time_F]
  tiempoaux = (tiempoaux - time(time_I)) / (time(time_I+1) - time(time_I))
  tiempo1aux = (tiempo1aux - time(time_I)) / (time(time_I+1) - time(time_I))
  pasoaux = pasoaux 
  !! Chanchullo que hago para asegurarme que el tiempo inicial está dentro del intervalo temporal
  !IF ((tiempoaux.LT.cero).OR.(tiempoaux.GT.time_F)) CALL Handle_error("Initial time out of time interval for U,V,W")
  IF (tiempoaux.LT.cero) THEN
     tiempoaux = cero
  ENDIF
  IF (tiempoaux.GT.time_F) THEN
     tiempoaux = time_F - cero
  ENDIF
  IF (tiempo1aux.LT.cerop) THEN
     tiempo1aux = 0.0D0 + pasoaux  ! mi primera fotico es tiempo 0, y tiempo final negativo, así que me quedo en 0
  ENDIF
  IF (tiempo1aux.GT.time_F) THEN 
     tiempo1aux = time_F - pasoaux
  ENDIF
  nsteps = INT(DABS((tiempo1aux-tiempoaux)/pasoaux))
  ALLOCATE(trajectory(nsteps + 2), STAT = status)
  IF (status .NE. 0) CALL Handle_error("Problem allocating memory for trajectory in AdvectaParticula")
  ! Fill all the data with the _Fill_Value by default
  trajectory(:)%p%x = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default
  trajectory(:)%p%y = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default
  trajectory(:)%time = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default
  i = 1
  trajectory(i)%p = desconvertir(puntoyfallo%p)
  trajectory(i)%time = (tiempoaux * (time(time_I+1)-time(time_I))) + time(time_I)
  codigoerror=0
  puntoyfallo%fallo=codigoerror
  ! La condición siguiente es para que se tenga en cuenta qué tiempo es mayor y se haga el cálculo en
  ! el intervalo temporal correcto, para ello usamos la función SIGN que multiplica por el signo de pasoaux
  DO WHILE ((SIGN(tiempoaux,pasoaux).LE.SIGN(tiempo1aux,pasoaux)))  !.AND.(codigoerror.EQ.0))
     puntoyfallo=runge5nav(puntoyfallo%p,tiempoaux,pasoaux)
     !IF (puntoyfallo%fallo.NE.0) THEN
        IF (debugging) THEN
           codigoerror=puntoyfallo%fallo ! miramos qué error se ha producido
           IF (codigoerror.EQ.-1) THEN
              PRINT*, "%ERROR: La partícula se ha salido del intervalo temporal en el pasado."
           ELSEIF (codigoerror.EQ.1) THEN
              PRINT*, "%ERROR: La partícula se ha salido del intervalo temporal en el futuro."
           ELSEIF (codigoerror.EQ.2) THEN
              PRINT*, "%ERROR: La partícula se ha salido del dominio de datos por el este."
           ELSEIF (codigoerror.EQ.3) THEN
              PRINT*, "%ERROR: La partícula se ha salido del dominio de datos por el norte."
           ELSEIF (codigoerror.EQ.4) THEN
              PRINT*, "%ERROR: La partícula se ha salido del dominio de datos por el oeste."
           ELSEIF (codigoerror.EQ.5) THEN
              PRINT*, "%ERROR: La partícula se ha salido del dominio de datos por el sur."
           ENDIF
        ENDIF
     !ELSE
        tiempoaux=tiempoaux+pasoaux
        i = i + 1
        trajectory(i)%p = desconvertir(puntoyfallo%p)
        trajectory(i)%time = (tiempoaux * (time(time_I+1)-time(time_I))) + time(time_I)
        IF (debugging) THEN
           PRINT*, (tiempoaux * (time(time_I+1)-time(time_I))) + time(time_I), desconvertir(puntoyfallo%p) !CUIDADO, vuelvo a tiempos del modelo
        ENDIF
     !ENDIF
  END DO
END SUBROUTINE AdvectaParticula



!******************************************************************
!*** Esta función toma un punto x, dado como (longitud,latitud) y
!*** me lo lleva a la región de trabajo [0,1]x[0,1].
!*** ANTENCIÓN: La conversión que se está haciendo NO funciona
!*** en todo el globo, así que cuidado, funciona sin problemas
!*** en el hemisferio norte.
!******************************************************************
FUNCTION convertir(x)
  TYPE (Punto) :: x
  TYPE (Punto) :: convertir

  convertir%x=((x%x*PI/180.0D0)-lon(0))/(lon(nx-1)-lon(0))
  convertir%y=x%y*PI/180.0D0
  convertir%y=DLOG(DABS(1.0D0/DCOS(convertir%y)+DTAN(convertir%y)))
  convertir%y=(convertir%y-auxlat(0))/(auxlat(ny-1)-auxlat(0))

END FUNCTION convertir



!***************************************************************
!*** Esta función me toma un punto de la región de trabajo [0,1]x[0,1]
!*** y me lo devuelve en la forma (longitud,latitud)
!*** ANTENCIÓN: La conversión que se está haciendo NO funciona
!*** en todo el globo, así que cuidado, funciona sin problemas
!*** en el hemisferio norte.
!**************************************************************
FUNCTION desconvertir(x)
  TYPE (Punto) :: x
  TYPE (Punto) :: desconvertir

  desconvertir%x=((x%x*(lon(nx-1)-lon(0)))+lon(0))*180.0D0/PI
  desconvertir%y=(x%y*(auxlat(ny-1)-auxlat(0)))+auxlat(0)
  desconvertir%y=(PI/2.0D0)-(2.0D0*DATAN(DEXP(-desconvertir%y)))
  desconvertir%y=desconvertir%y*180.0D0/PI
END FUNCTION desconvertir



!********************************************************************
!*** Runge-Kutta de orden 5. Se le ha como entrada el punto (y en el código),
!*** el tiempo (representado por x en el código) y el tamaño del paso (h en el 
!*** código). La función devuelve el nuevo valor del punto.
!*** En caso de error, la función devuelve el punto de partida y 
!*** el código del error, los cuales son:
!***  0: Indica que no se ha producido ningún error y la función ha acabado correctamente
!*** -1: Indica que nos hemos salido del intervalo temporal en el pasado
!***  1: Indica que nos hemos salido del intervalo temporal en el futuro
!***  2: Indica que nos hemos salido del dominio de datos por el este
!***  3: Indica que nos hemos salido del dominio de datos por el norte
!***  4: Indica que nos hemos salido del dominio de datos por el oeste
!+++  5: Indica que nos hemos salido del dominio de datos por el sur
!*********************************************************************
FUNCTION runge5nav(y,x,h)
  TYPE (Punto) :: y, ytemp, ak1, ak2, ak3, ak4, ak5, ak6!, yerr
  TYPE (PuntoConFallo) :: pfaux
  DOUBLE PRECISION :: x, h, A2, A3, A4, A5, A6, B21, B31, B32, B41
  DOUBLE PRECISION :: B42, B43, B51, B52, B53, B54, B61, B62, B63
  DOUBLE PRECISION :: B64, B65, C1, C3, C4, C6, DC1, DC3, DC4, DC5, DC6
  TYPE (PuntoConFallo) :: runge5nav



  A2=0.2D0
  A3=0.3D0
  A4=0.6D0
  A5=1.0D0
  A6=0.875D0
  B21=0.2D0
  B31=3D0/40D0
  B32=9D0/40D0
  B41=0.3D0
  B42=-0.9D0
  B43=1.2D0
  B51=-11D0/54D0
  B52=2.5D0
  B53=-70D0/27D0
  B54=35D0/27D0
  B61=1631D0/55296D0
  B62=175D0/512D0
  B63=575D0/13824D0
  B64=44275D0/110592D0
  B65=253D0/4096D0
  C1=37D0/378D0
  C3=250D0/621D0
  C4=125D0/594D0
  C6=512D0/1771D0
  DC1=C1-2825D0/27648D0
  DC3=C3-18575D0/48384D0
  DC4=C4-13525D0/55296D0
  DC5=-277D0/14336D0
  DC6=C6-0.25D0
  pfaux=f(y,x)  !! Chanchullo para que el compilador me deje trabajar solo con la parte del punto
  ak1=pfaux%p
  IF (pfaux%fallo.NE.0) THEN
     runge5nav%p=y    !Devuelvo el punto de partida y el error correspondiente
     runge5nav%fallo=pfaux%fallo
  ELSE
     ytemp%x=y%x+B21*h*ak1%x
     ytemp%y=y%y+B21*h*ak1%y
     pfaux=f(ytemp,x+A2*h)!! Chanchullo para que el compilador me deje trabajar solo con la parte del punto
     ak2=pfaux%p
     IF  (pfaux%fallo.NE.0) THEN
        runge5nav%p=y    !Devuelvo el punto de partida y el error correspondiente
        runge5nav%fallo=pfaux%fallo
     ELSE
        ytemp%x=y%x+h*(B31*ak1%x+B32*ak2%x)
        ytemp%y=y%y+h*(B31*ak1%y+B32*ak2%y)
        pfaux=f(ytemp,x+A3*h)!! Chanchullo para que el compilador me deje trabajar solo con la parte del punto
        ak3=pfaux%p
        IF  (pfaux%fallo.NE.0) THEN
           runge5nav%p=y    !Devuelvo el punto de partida y el error correspondiente
           runge5nav%fallo=pfaux%fallo
        ELSE              
           ytemp%x=y%x+h*(B41*ak1%x+B42*ak2%x+B43*ak3%x)
           ytemp%y=y%y+h*(B41*ak1%y+B42*ak2%y+B43*ak3%y)
           pfaux=f(ytemp,x+A4*h)!! Chanchullo para que el compilador me deje trabajar solo con la parte del punto
           ak4=pfaux%p
           IF  (pfaux%fallo.NE.0) THEN
              runge5nav%p=y    !Devuelvo el punto de partida y el error correspondiente
              runge5nav%fallo=pfaux%fallo
           ELSE              
              ytemp%x=y%x+h*(B51*ak1%x+B52*ak2%x+B53*ak3%x+B54*ak4%x)
              ytemp%y=y%y+h*(B51*ak1%y+B52*ak2%y+B53*ak3%y+B54*ak4%y)
              pfaux=f(ytemp,x+A5*h)!! Chanchullo para que el compilador me deje trabajar solo con la parte del punto
              ak5=pfaux%p
              IF  (pfaux%fallo.NE.0) THEN
                 runge5nav%p=y    !Devuelvo el punto de partida y el error correspondiente
                 runge5nav%fallo=pfaux%fallo
              ELSE         
                 ytemp%x=y%x+h*(B61*ak1%x+B62*ak2%x+B63*ak3%x+B64*ak4%x+B65*ak5%x)
                 ytemp%y=y%y+h*(B61*ak1%y+B62*ak2%y+B63*ak3%y+B64*ak4%y+B65*ak5%y)
                 pfaux=f(ytemp,x+A6*h)!! Chanchullo para que el compilador me deje trabajar solo con la parte del punto
                 ak6=pfaux%p
                 IF  (pfaux%fallo.NE.0) THEN
                    runge5nav%p=y    !Devuelvo el punto de partida y el error correspondiente
                    runge5nav%fallo=pfaux%fallo
                 ELSE         
                    ytemp%x=y%x+h*(C1*ak1%x+C3*ak3%x+C4*ak4%x+C6*ak6%x)
                    ytemp%y=y%y+h*(C1*ak1%y+C3*ak3%y+C4*ak4%y+C6*ak6%y)
                    !        yerr%x=h*(DC1*ak1%x+DC3*ak3%x+DC4*ak4%x+DC5*ak5%x+DC6*ak6%x) !error estimado
                    !        yerr%y=h*(DC1*ak1%y+DC3*ak3%y+DC4*ak4%y+DC5*ak5%y+DC6*ak6%y)
                    runge5nav%p=ytemp !Como todo ha ido bien devuelvo el punto advectado
                    runge5nav%fallo=0
                    !        runge5nav(1)=yerr !error estimado en cada coordenada
                 ENDIF
              ENDIF
           ENDIF
        ENDIF
     ENDIF
  ENDIF
  
END FUNCTION runge5nav



!!************************************************************************
!*** Esta función es la que dado un punto x y un tiempo t, me da la
!*** velocidad (u,v) interpolada para ese punto x y tiempo t.
!*** En caso de error, la función devuelve una velocidad nula y 
!*** el código del error, los cuales son:
!***  0: Indica que no se ha producido ningún error y la función ha acabado correctamente
!*** -1: Indica que nos hemos salido del intervalo temporal en el pasado
!***  1: Indica que nos hemos salido del intervalo temporal en el futuro
!***  2: Indica que nos hemos salido del dominio de datos por el este
!***  3: Indica que nos hemos salido del dominio de datos por el norte
!***  4: Indica que nos hemos salido del dominio de datos por el oeste
!+++  5: Indica que nos hemos salido del dominio de datos por el sur
!*************************************************************************
TYPE (PuntoConFallo) FUNCTION f(x,t)
  TYPE (Punto) :: x, xequivalente
  DOUBLE PRECISION :: t, ffa, ffb
  INTEGER :: imin, jmin
  DOUBLE PRECISION, DIMENSION(0:1,0:1,0:2) :: dvx1, dvx2, dvx3, dvx4
  DOUBLE PRECISION, DIMENSION(0:1,0:1,0:2) :: dvy1, dvy2, dvy3, dvy4
  DOUBLE PRECISION, DIMENSION (0:2) :: ffa1, ffa2, ffa3, ffa4
  DOUBLE PRECISION, DIMENSION (0:2) :: ffb1, ffb2, ffb3, ffb4
  DOUBLE PRECISION, DIMENSION (0:3) :: t1, ya, yb
  
  !#Identificamos  imin, jmin para saber en qué parte de la rejilla está el punto  
  xequivalente=x  ! usamos un punto auxiliar para no modificar el de partida
  imin=BusquedaBinaria(milon(:),xequivalente%x)  ! Busco la esquina inferior izquierda del cuadro de malla donde
  jmin=BusquedaBinaria(milat(:),xequivalente%y)  ! está el punto donde quiero calcular, uso milon y milat, ya que uso datos en [0,1]x[0,1]
  
  IF (t.LT.time_I) THEN
     f%p%x=0.0D0   ! devuelvo una velocidad nula
     f%p%y=0.0D0
     f%fallo=-1     ! indico que me he salido del tiempo en el pasado
  ELSEIF (t.GT.time_F) THEN
     f%p%x=0.0D0   ! devuelvo una velocidad nula
     f%p%y=0.0D0
     f%fallo=1     ! indico que me he salido del tiempo en el futuro
  ELSEIF (imin.GE.nx-1) THEN
     f%p%x=0.0D0   ! devuelvo una velocidad nula
     f%p%y=0.0D0
     f%fallo=2     ! indico que me he salido del dominio de datos por el este
  ELSEIF (jmin.GE.ny-1) THEN
     f%p%x=0.0D0   ! devuelvo una velocidad nula
     f%p%y=0.0D0
     f%fallo=3     ! indico que me he salido del dominio de datos por el norte
  ELSEIF (imin.LT.0) THEN
     f%p%x=0.0D0   ! devuelvo una velocidad nula
     f%p%y=0.0D0
     f%fallo=4     ! indico que me he salido del dominio de datos por el oeste
  ELSEIF (jmin.LT.0) THEN  
     f%p%x=0.0D0   ! devuelvo una velocidad nula
     f%p%y=0.0D0
     f%fallo=5     ! indico que me he salido del dominio de datos por el sur
  ELSE            
     CALL readqgn(t)
     dvx1=derivative(vx1,imin,jmin)
     dvx2=derivative(vx2,imin,jmin)
     dvx3=derivative(vx3,imin,jmin)
     dvx4=derivative(vx4,imin,jmin)         
     dvy1=derivative(vy1,imin,jmin)
     dvy2=derivative(vy2,imin,jmin)
     dvy3=derivative(vy3,imin,jmin)
     dvy4=derivative(vy4,imin,jmin)               
     
     ffa1=prep_lagrange(vx1,dvx1,imin,jmin,xequivalente)
     ffa2=prep_lagrange(vx2,dvx2,imin,jmin,xequivalente)
     ffa3=prep_lagrange(vx3,dvx3,imin,jmin,xequivalente)
     ffa4=prep_lagrange(vx4,dvx4,imin,jmin,xequivalente)
     
     ffb1=prep_lagrange(vy1,dvy1,imin,jmin,xequivalente)
     ffb2=prep_lagrange(vy2,dvy2,imin,jmin,xequivalente)
     ffb3=prep_lagrange(vy3,dvy3,imin,jmin,xequivalente)
     ffb4=prep_lagrange(vy4,dvy4,imin,jmin,xequivalente)
     
     !#interpolamos el valor de f en el tiempo t
     t1(0)=INT(t)-1
     t1(1)=INT(t)
     t1(2)=INT(t)+1
     t1(3)=INT(t)+2
     ya(0)=ffa1(0)
     ya(1)=ffa2(0)
     ya(2)=ffa3(0)
     ya(3)=ffa4(0)
     yb(0)=ffb1(0)
     yb(1)=ffb2(0)
     yb(2)=ffb3(0)
     yb(3)=ffb4(0)
     ffa=polint(t1,ya,4,t)
     ffb=polint(t1,yb,4,t)
     
     f%p%x=ffa ! devuelvo la velocidad interpolada
     f%p%y=ffb
     f%fallo=0
  ENDIF
END FUNCTION f



!*************************************************************************
!******* Esta función calcula las derivadas de la matriz psi con respecto
!******* a X, a Y y la derivada cruzada respecto a X y a Y.
!******* Lo hace en las cuatro esquinas de la casilla de la malla
!******* indicada por imin, jmin, que son los índices de la matriz
!******* que proporcionan la esquina inferior izquierda de la casilla
!************************************************************************
FUNCTION derivative(psi,imin,jmin)
  DOUBLE PRECISION, DIMENSION(0:nx-1,0:ny-1) :: psi
  DOUBLE PRECISION, DIMENSION(0:1,0:1) :: dxpsi, dypsi, dxypsi 
  INTEGER :: imin,jmin, ind, jnd
  INTEGER :: hxe, hxo, hyn,hys  ! Para controlar las derivadas cuando estoy en el borde
  DOUBLE PRECISION, DIMENSION(0:1,0:1,0:2) :: derivative

  
  !Deriva psi respecto de X
  !          dxpsi(0:1,0:1)=(psi(imin+1:imin+2,jmin:jmin+1)-psi(imin-1:imin,jmin:jmin+1))*r2dx

 
  !Deriva psi respecto de Y
  !          dypsi(0:1,0:1)=(psi(imin:imin+1,jmin+1:jmin+2)-psi(imin:imin+1,jmin-1:jmin))*r2dy
  

  !Derivamos psi respecto de X, de Y y de X y de Y en los cuatro puntos del rectángulo de malla donde cae nuestro punto de interés
  ind=imin
  jnd=jmin
  IF (ind.EQ.0) THEN    !! Comprobando si estoy en el borde
     hxo=0
  ELSE
     hxo=1
  END IF
  IF (ind.EQ.nx-1) THEN
     hxe=0
  ELSE
     hxe=1
  END IF
  IF (jnd.EQ.0) THEN
     hys=0
  ELSE
     hys=1
  END IF
  IF (jnd.EQ.ny-1) THEN
     hyn=0
  ELSE
     hyn=1
  END IF
  dxpsi(0,0)=(psi(ind+hxe,jnd)-psi(ind-hxo,jnd))/(milon(ind+hxe)-milon(ind-hxo)) ! derivo respecto de X
  dypsi(0,0)=(psi(ind,jnd+hyn)-psi(ind,jnd-hys))/(milat(jnd+hyn)-milat(jnd-hys)) ! derivo respecto de Y
  dxypsi(0,0)=(psi(ind+hxe,jnd+hyn)-psi(ind-hxo,jnd+hyn)+psi(ind-hxo,jnd-hys)-psi(ind+hxe,jnd-hys))/&
       &((milon(ind+hxe)-milon(ind-hxo))*(milat(jnd+hyn)-milat(jnd-hys))) !Respecto de X y de Y


  
  ind=imin
  jnd=jmin+1
  IF (ind.EQ.0) THEN  !! Comprobando si estoy en el borde
     hxo=0
  ELSE
     hxo=1
  END IF
  IF (ind.EQ.nx-1) THEN
     hxe=0
  ELSE
     hxe=1
  END IF
  IF (jnd.EQ.0) THEN
     hys=0
  ELSE
     hys=1
  END IF
  IF (jnd.EQ.ny-1) THEN
     hyn=0
  ELSE
     hyn=1
  END IF
  dxpsi(0,1)=(psi(ind+hxe,jnd)-psi(ind-hxo,jnd))/(milon(ind+hxe)-milon(ind-hxo)) ! derivo respecto de X
  dypsi(0,1)=(psi(ind,jnd+hyn)-psi(ind,jnd-hys))/(milat(jnd+hyn)-milat(jnd-hys)) ! derivo respecto de Y
  dxypsi(0,1)=(psi(ind+hxe,jnd+hyn)-psi(ind-hxo,jnd+hyn)+psi(ind-hxo,jnd-hys)-psi(ind+hxe,jnd-hys))/&
       &((milon(ind+hxe)-milon(ind-hxo))*(milat(jnd+hyn)-milat(jnd-hys))) !Respecto de X y de Y



  
  ind=imin+1
  jnd=jmin+1  
  IF (ind.EQ.0) THEN  !! Comprobando si estoy en el borde
     hxo=0
  ELSE
     hxo=1
  END IF
  IF (ind.EQ.nx-1) THEN
     hxe=0
  ELSE
     hxe=1
  END IF
  IF (jnd.EQ.0) THEN
     hys=0
  ELSE
     hys=1
  END IF
  IF (jnd.EQ.ny-1) THEN
     hyn=0
  ELSE
     hyn=1
  END IF
  dxpsi(1,1)=(psi(ind+hxe,jnd)-psi(ind-hxo,jnd))/(milon(ind+hxe)-milon(ind-hxo)) ! derivo respecto de X
  dypsi(1,1)=(psi(ind,jnd+hyn)-psi(ind,jnd-hys))/(milat(jnd+hyn)-milat(jnd-hys)) ! derivo respecto de Y
  dxypsi(1,1)=(psi(ind+hxe,jnd+hyn)-psi(ind-hxo,jnd+hyn)+psi(ind-hxo,jnd-hys)-psi(ind+hxe,jnd-hys))/&
       &((milon(ind+hxe)-milon(ind-hxo))*(milat(jnd+hyn)-milat(jnd-hys))) !Respecto de X y de Y
         

  
  ind=imin+1
  jnd=jmin   
  IF (ind.EQ.0) THEN  !! Comprobando si estoy en el borde
     hxo=0
  ELSE
     hxo=1
  END IF
  IF (ind.EQ.nx-1) THEN
     hxe=0
  ELSE
     hxe=1
  END IF
  IF (jnd.EQ.0) THEN
     hys=0
  ELSE
     hys=1
  END IF
  IF (jnd.EQ.ny-1) THEN
     hyn=0
  ELSE
     hyn=1
  END IF
  dxpsi(1,0)=(psi(ind+hxe,jnd)-psi(ind-hxo,jnd))/(milon(ind+hxe)-milon(ind-hxo)) ! derivo respecto de X
  dypsi(1,0)=(psi(ind,jnd+hyn)-psi(ind,jnd-hys))/(milat(jnd+hyn)-milat(jnd-hys)) ! derivo respecto de Y
  dxypsi(1,0)=(psi(ind+hxe,jnd+hyn)-psi(ind-hxo,jnd+hyn)+psi(ind-hxo,jnd-hys)-psi(ind+hxe,jnd-hys))/&
       &((milon(ind+hxe)-milon(ind-hxo))*(milat(jnd+hyn)-milat(jnd-hys))) !Respecto de X y de Y
  


  derivative(:,:,0)=dxpsi(:,:)
  derivative(:,:,1)=dypsi(:,:)
  derivative(:,:,2)=dxypsi(:,:)
END FUNCTION derivative





!*********************************************************************
!*** Esta función hace una búsqueda binaria en el vector vector[0..n]
!*** devolviendo un valor i que verifica que vector(i)<=valor<vector(i+1)
!*** (estamos suponiendo que el vector está ordenado de forma 
!*** estrictamente creciente, es decir, los valores son diferentes
!*** para determinar de forma unívoca la posición "i").
!*** La función devuelve -1 en el caso que el valor sea menor que vector(0)
!*** y devuelve n en el caso que sea mayor que vector(n)
!***********************************************************************
INTEGER FUNCTION BusquedaBinaria(vector,valor)
  DOUBLE PRECISION, DIMENSION(:) :: vector  ! vector de valores donde busco un valor
  DOUBLE PRECISION :: valor                     ! valor que busco en el vector
  INTEGER :: i,j,k                                ! índices para saber en qué subvector estoy buscando

  i=1
  j=SIZE(vector)
  
  IF (valor.GE.vector(j)) THEN 
     BusquedaBinaria=j-1  !! El valor es mayor que todos los valores del vector
  ELSE
     DO WHILE (i.LT.j)
        k=(i+j)/2
        
        IF (valor.LT.vector(k)) THEN 
           j=k
        ELSE 
           i=k+1
        END IF
        
     END DO
     BusquedaBinaria=i-2
     !! CUIDADO: Esta parte del algoritmo da un i tal que vector(i-1)<=valor<vector(i) 
     !! (ver pág 257 de "Fundamentos de Algoritmia" de Brassard y Bratley)
     !! (se ha intentado modificar para que proporcione directamente el valor inferior, pero
     !! hay problemas con la terminación del algoritmo, así que lo que hago es usar el
     !! algoritmo documentado que funciona y luego le resto 1.
     !! PELIGRO: En realidad le resto 2 porque FORTRAN al pasar el vector que tengo en
     !! el código numerado de 0 a n-1, me lo reenumera de 1 a n (a menos que le especifique los
     !! índices, pero como voy a usarlo en vectores de diferentes tamaños, pues no puedo
     !! especificarlos), por eso le 
     !! resto otra posición, así los índices que me devuelve esta función se corresponden
     !! exactamente con los que estoy usando en las demás funciones del programa TOSCA
  END IF
END FUNCTION BusquedaBinaria



!***************************************************************
!**** Función intermedia que hace cálculos necesarios para
!**** la interpolación de la velocidad 
!**** (busca en la teoría y explica mejor esta función)
!*************************************************************
FUNCTION prep_lagrange(psiaux,dpsi2,imin,jmin,x)
  DOUBLE PRECISION, DIMENSION(0:3) :: y, y1, y2, y12
  DOUBLE PRECISION :: td, ud
  TYPE (Punto) :: x
  DOUBLE PRECISION, DIMENSION (0:2) :: prep_lagrange
  INTEGER :: imin, jmin
  DOUBLE PRECISION, DIMENSION(0:nx-1,0:ny-1) :: psiaux
  DOUBLE PRECISION, DIMENSION(0:1,0:1,0:2) :: dpsi2
  DOUBLE PRECISION, DIMENSION(0:1,0:1) :: dxpsi, dypsi, dxypsi
  ! To remove Fortran runtime warning: An array temporary was created for argument 'arr' of procedure 'weirdifyarray'
  DOUBLE PRECISION, DIMENSION(0:1,0:1) :: dpsiaux

         
  dxpsi = dpsi2(:,:,0)
  dypsi = dpsi2(:,:,1)
  dxypsi = dpsi2(:,:,2)
  dpsiaux(:,:) =  psiaux(imin:imin+1,jmin:jmin+1) !To remove warning about creation of temporary array
  y=weirdifyArray(dpsiaux)
  y1=weirdifyArray(dxpsi)
  y2=weirdifyArray(dypsi)
  y12=weirdifyArray(dxypsi)
  td=x%x-milon(imin)    ! Lo lejos que estoy en el eje X de la esquina inferior izquierda de la casilla donde cae el punto 
  ud=x%y-milat(jmin)    ! Lo lejos que estoy en el eje Y de la esquina inferior izquierda de la casilla donde cae el punto
  !         td=x%x-imin*deltax   !!!! Esto es lo que había antes, que se ha modificado para que pueda 
  !         ud=x%y-jmin*deltay    !!! interpolar incluso cuando la rejilla no es homogénea
  !         prep_lagrange=interpolate(y,y1,y2,y12,deltax,deltay,td,ud)
  prep_lagrange=interpolate(y,y1,y2,y12,milon(imin+1)-milon(imin),milat(jmin+1)-milat(jmin),td,ud)
END FUNCTION prep_lagrange



!************************************************************************
!** Esta función reordena una matriz 2x2 dando una matrix 4x1 haciendo
!** en forma antihorario y empezando desde abajo a la izquierda
!*******************************************************************
FUNCTION weirdifyArray(arr)
  DOUBLE PRECISION, DIMENSION(0:1,0:1) :: arr
  DOUBLE PRECISION, DIMENSION(0:3) :: weirdifyArray
  !    if arr.shape != (2,2):
  !        print "shape: ", arr.shape
  !        raise "WrongShapeError"
  weirdifyArray(0)= arr(0,0)
  weirdifyArray(1)= arr(1,0)
  weirdifyArray(2)= arr(1,1)
  weirdifyArray(3)= arr(0,1)
END FUNCTION weirdifyArray



!********************************************************
!*** Esta función hace la interpolación propiamente dicha
!*** (mirar en la bibliografía para detalles)
!*******************************************************
FUNCTION interpolate(y, y1, y2, y12, d1, d2,td,ud)
  DOUBLE PRECISION, DIMENSION(0:3) :: y, y1, y2, y12
  DOUBLE PRECISION :: d1, d2,td, ud, t, u, interp, interp1, interp2 
  DOUBLE PRECISION, DIMENSION (0:3,0:3) :: c
  DOUBLE PRECISION, DIMENSION (0:2) :: interpolate
  INTEGER :: k

  t = td/d1
  u = ud/d2
  IF ((t.GT.1.0D0).OR.(t.LT.0.0D0).OR.(u.GT.1.0D0).OR.(u.LT.0.0D0)) THEN !Comprobación superflua que pongo, vaya que se escape algo por ahí
     STOP "La cagaste, la interpolaste :P. Revisa la funcion interpolate"
  END IF
  c = getc(y, y1, y2, y12, d1, d2)
  interp =0D0
  interp1=0D0
  interp2=0D0
  DO k=3,0,-1 !El primero interpola la función y lo demás derivadas
     interp = t*interp + ((c(k,3)*u+c(k,2))*u+c(k,1))*u+c(k,0)
     !            interp2 = t*interp2 + (3D0*c(k,3)*u+2D0*c(k,2))*u+c(k,1)
     !            interp1 = u*interp1 + (3D0*c(3,k)*t+2D0*c(2,k))*t+c(1,k)
  END DO
  interp1=0.0D0 ! interp1/d1
  interp2=0.0D0 ! interp2/d2   **** LOS HE QUITADO YA QUE NO LOS NECESITO AL SER DERIVADAS
  interpolate(0)=interp
  interpolate(1)=interp1
  interpolate(2)=interp2
END FUNCTION interpolate



!********************************************************
!*** Función intermedia que se usa para interpolar
!*** la velocidad. Mire la bibliografía para detalles
!*******************************************************
FUNCTION getc(y, y1, y2, y12, d1, d2)
  DOUBLE PRECISION, DIMENSION(0:3) :: y, y1, y2, y12
  DOUBLE PRECISION :: d1, d2 
  DOUBLE PRECISION, DIMENSION(0:15) :: rhs, temp
  DOUBLE PRECISION, DIMENSION (0:3,0:3) :: getc

  rhs(0:3)=y(0:3)
  rhs(4:7)=y1(0:3)*d1
  rhs(8:11)=y2(0:3)*d2
  rhs(12:15)=y12(0:3)*d1*d2
  temp=MATMUL(weights, rhs)
  getc=RESHAPE(temp,(/4,4/))
  getc=TRANSPOSE(getc)
END FUNCTION getc



!*********************************************************
!**** Esta función toma las cuatro velocidades interpoladas
!**** y proporciona la velocidad final que busco. Mire
!**** la bibliografía para detalles
!*********************************************************
FUNCTION polint(xa,ya,n,x)
  DOUBLE PRECISION, DIMENSION(0:3) :: xa, ya, c, d
  INTEGER :: n, ns, k, m, i
  DOUBLE PRECISION :: x, dif, dift, y, ho, hp, w, den, dy
  DOUBLE PRECISION :: polint
  
  ns=1
  dif=ABS(x-xa(0))
  
  DO k=1,n,1
     dift=ABS(x-xa(k-1))
     IF (dift <= dif) THEN
        ns=k
        dif=dift    
     END IF
  END DO
  c=ya
  d=ya
  y=ya(ns-1)
  ns=ns-1
  DO m=1,n-1,1
     DO i=1,n-m,1
        ho=xa(i-1)-x
        hp=xa(i+m-1)-x
        w=c(i)-d(i-1)
        den=ho-hp
        IF (den==0) THEN
           PRINT*, 'failure in polint'
        END IF
        den=w/den
        c(i-1)=ho*den
        d(i-1)=hp*den
     END DO
     
     IF (2*ns<=n-m) THEN
        dy=c(ns)
     ELSE 
        dy=d(ns-1)
        ns=ns-1
     END IF
     y=y+dy         
  END DO
  polint=y
END FUNCTION polint



!**********************************************************************
!**** Este procedimiento, toma el tiempo t en que se requiere la interpolacion
!**** y decide qué 4 matrices vx e vy se necesitan 
!*********************************************************************
SUBROUTINE readqgn(t)

  DOUBLE PRECISION :: t
  INTEGER :: ind 
  
  
  ind=INT(t)
  IF (ind==time_I) THEN
     ind=ind+1
  END IF
  IF (ind==time_F) THEN
     ind=ind-2
  END IF
  IF (ind==time_F-1) THEN
     ind=ind-1
  END IF
  vx1(:,:)=vx(:,:,ind-1)     
  vx2(:,:)=vx(:,:,ind)
  vx3(:,:)=vx(:,:,ind+1)
  vx4(:,:)=vx(:,:,ind+2)
  vy1(:,:)=vy(:,:,ind-1)
  vy2(:,:)=vy(:,:,ind)
  vy3(:,:)=vy(:,:,ind+1)
  vy4(:,:)=vy(:,:,ind+2)
END SUBROUTINE readqgn


!********************************************************************
!**** Este procedimiento transforma las coordenadas de latitud y
!**** longitud a una rejilla uniformemente espaciada, necesaria
!**** para la interpolación. La región de trabajo se transforma
!**** en la región [0,1]x[0,1]. También modifica los valores de la
!**** velocidad convenientemente para que los cálculos sean correctos
!*****************************************************************          
SUBROUTINE convertirDatos (ndatasel)
  INTEGER :: ndatasel !! para seleccionar el último día de la simulación
  INTEGER :: i,j,jj,error !! variable auxiliar para los bucles

  
  ALLOCATE(milon(0:nx-1),STAT = error)   ! Guardo memoria para el nuevo vector de longitudes
  IF (error .NE. 0) CALL Handle_error("A problem happened creating milon in convertirDatos")
  ALLOCATE(milat(0:ny-1),STAT = error)  ! Guardo memoria para el nuevo vector de latitudes
  IF (error .NE. 0) CALL Handle_error("A problem happened creating milat in convertirDatos")
  ALLOCATE(auxlat(0:ny-1),STAT = error) !! Matriz auxiliar para el cambio de coordenadas
  IF (error .NE. 0) CALL Handle_error("A problem happened creating auxlat in convertirDatos")
  DO i=0,nx-1  !! Convierto grados a radianes
     lon(i)=lon(i)*PI/180.0D0 
  END DO
  DO i=0,nx-1  !! Llevo las longitudes al intervalo [0,1]
     milon(i)=(lon(i)-lon(0))/(lon(nx-1)-lon(0))
  END DO
  DO i=0,ny-1  !! Convierto grados a radianes
     lat(i)=lat(i)*PI/180.0D0 !! Convierto grados a radianes
     auxlat(i)=DLOG(DABS(1.0D0/DCOS(lat(i))+DTAN(lat(i)))) !! Cambio a mi nueva latitud
  END DO
  DO i=0,ny-1  !! LLevo las latitudes al [0,1]
     milat(i)=(auxlat(i)-auxlat(0))/(auxlat(ny-1)-auxlat(0))
  END DO
  ALLOCATE(vx(0:nx-1,0:ny-1,time_I:ndatasel),STAT=error) !Guardo memoria para las
  IF (error .NE. 0) CALL Handle_error("A problem happened creating vx in convertirDatos")
  ALLOCATE(vy(0:nx-1,0:ny-1,time_I:ndatasel),STAT=error) ! velocidades ajustadas al cambio de coordenadas
  IF (error .NE. 0) CALL Handle_error("A problem happened creating vy in convertirDatos")
  DO i=0,nx-1
     DO j=0,ny-1
        DO jj=time_I,ndatasel
           vx(i,j,jj)=mivx(i,j,jj)*FACTOR/(RADIOTIERRA*DCOS(lat(j))*(lon(nx-1)-lon(0)))
           vy(i,j,jj)=mivy(i,j,jj)*FACTOR/(RADIOTIERRA*DCOS(lat(j))*(auxlat(ny-1)-auxlat(0)))
        END DO
     END DO
  END DO
END SUBROUTINE convertirDatos


!***************************************************************
!*** Este procedimiento asigna los valores iniciales a ciertas variables 
!*** que se usan a lo largo del programa, sobre todo en la parte
!*** de la interpolación bicúbica. Aquí ponemos las variables que
!*** siempre se usan y hay que inicializar a su valor correcto para
!*** que el algoritmo funcione de manera correcta
!**************************************************************
SUBROUTINE datosIniciales

  ! Doy valores a la matriz de pesos de la interpolación bicúbica
  weights(1,:)=(/ 1D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0/)
  weights(2,:)=(/ 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 1D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0/)
  weights(3,:)=(/ -3D0, 0D0, 0D0, 3D0, 0D0, 0D0, 0D0, 0D0,-2D0, 0D0, 0D0,-1D0, 0D0, 0D0, 0D0, 0D0/)
  weights(4,:)=(/ 2D0, 0D0, 0D0,-2D0, 0D0, 0D0, 0D0, 0D0, 1D0, 0D0, 0D0, 1D0, 0D0, 0D0, 0D0, 0D0/)
  weights(5,:)=(/ 0D0, 0D0, 0D0, 0D0, 1D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0/)
  weights(6,:)=(/ 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 1D0, 0D0, 0D0, 0D0/)
  weights(7,:)=(/ 0D0, 0D0, 0D0, 0D0,-3D0, 0D0, 0D0, 3D0, 0D0, 0D0, 0D0, 0D0,-2D0, 0D0, 0D0,-1D0/)
  weights(8,:)=(/ 0D0, 0D0, 0D0, 0D0, 2D0, 0D0, 0D0,-2D0, 0D0, 0D0, 0D0, 0D0, 1D0, 0D0, 0D0, 1D0/)
  weights(9,:)=(/-3D0, 3D0, 0D0, 0D0,-2D0,-1D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0/)
  weights(10,:)=(/0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0,-3D0, 3D0, 0D0, 0D0,-2D0,-1D0, 0D0, 0D0/)
  weights(11,:)=(/9D0,-9D0, 9D0,-9D0, 6D0, 3D0,-3D0,-6D0, 6D0,-6D0,-3D0, 3D0, 4D0, 2D0, 1D0, 2D0/)
  weights(12,:)=(/-6D0, 6D0,-6D0, 6D0,-4D0,-2D0, 2D0, 4D0,-3D0, 3D0, 3D0,-3D0,-2D0,-1D0,-1D0,-2D0/)
  weights(13,:)=(/2D0,-2D0, 0D0, 0D0, 1D0, 1D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0/)
  weights(14,:)=(/0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 0D0, 2D0,-2D0, 0D0, 0D0, 1D0, 1D0, 0D0, 0D0/)
  weights(15,:)=(/-6D0,6D0,-6D0, 6D0,-3D0,-3D0, 3D0, 3D0,-4D0, 4D0, 2D0,-2D0,-2D0,-2D0,-1D0,-1D0/)
  weights(16,:)=(/4D0,-4D0, 4D0,-4D0, 2D0, 2D0,-2D0,-2D0, 2D0,-2D0,-2D0, 2D0, 1D0, 1D0, 1D0, 1D0/)
  weights=weights

  
  
  ! para interpolar velocidades, se toma U y V en cuatro instantes de tiempo para realizar la interpolación.
  ALLOCATE(vx1(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vx1 in datosIniciales")
  ALLOCATE(vx2(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vx2 in datosIniciales")
  ALLOCATE(vx3(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vx3 in datosIniciales")
  ALLOCATE(vx4(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vx4 in datosIniciales")
  ALLOCATE(vy1(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vy1 in datosIniciales")
  ALLOCATE(vy2(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vy2 in datosIniciales")
  ALLOCATE(vy3(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vy3 in datosIniciales")
  ALLOCATE(vy4(0:nx-1,0:ny-1), STAT = status)
  IF (status .NE. 0) CALL Handle_error("A problem happened creating vy4 in datosIniciales")
  
END SUBROUTINE datosIniciales



!***************************************************************
!*** Este procedimiento asigna los valores iniciales a ciertas variables 
!*** que se usan a lo largo del programa, como los archivos de entrada
!*** y salida de datos, opciones de depuración,...
!*** siempre se usan y hay que inicializar a su valor
!*** por omisión
!**************************************************************
SUBROUTINE defaultParameters

  debugging = .FALSE.
  outfile = 'out.nc'
  readFile = .FALSE.
  releasefile = 'release.ini'
  ufile = ''
  vfile = ''
  uname = 'u'
  vname = 'v'
  tnameu = 'time'
  lonnameu = 'longitude'
  latnameu = 'latitude'
  tnamev = 'time'
  lonnamev = 'longitude'
  latnamev = 'latitude'
  paso = 60.0D0 * 60.0D0 * 24.0D0
  x0%x = 0.0D0       !longitud for default point when not provided
  x0%y = 0.0D0       !latitude for default point when not provided
  t0 = CERO         ! initial time to perform simulations
  t1 = 7.0D0 * 24.0D0 * 60.0D0 * 60.0D0 ! length of simulation in seconds
  nfloats = 1   ! Number of floats to be randomly generated
  Rx = 0.1D0   !! Size of X radius to generate uniformly random initial positions around (X0,Y0)
  Ry = 0.1D0  !! Size of Y radius to generate uniformly random initial positions around (X0,Y0)
  CALL SYSTEM_CLOCK(seed) ! to get de seed for pseudorandom generator from system clock

  
END SUBROUTINE defaultParameters



!***************************************************************
!*** Este procedimiento es el que lee el archivo "releasefile"
!*** de puntos iniciales de salida de las partículas
!*** y sus tiempos de comienzo, 
!*** en el caso de que no existe se genera el archivo con 
!*** los puntos iniciales, que serán aleatorios en un entorno
!*** de los valores de X0, Y0 proporcionados
!**************************************************************
SUBROUTINE GenerateInitialPoints(initialPositions)
  INTEGER :: numberFloats  ! number of particles to advect
  INTEGER :: i
  DOUBLE PRECISION :: basura !! para leer la coordenada Z, ARREGLARLO EN LA VERSIon final
  TYPE (TrajectoryPoint), ALLOCATABLE, DIMENSION(:) :: initialPositions ! initial position for particles
 
  ! Leo el número de puntos que hay
  OPEN(UNIT=11, FILE = releasefile, ACTION='READ', IOSTAT = status, &
       & STATUS='OLD')
  IF ((status/=0) .OR. (.NOT.(readFile))) THEN
     IF (status .EQ. 0) CLOSE(11)
     IF (status/=0) PRINT*, "Error opening initial release file --> ",releasefile
     PRINT*, "Creating NEW initial release file !! "
     OPEN(UNIT=11, FILE = releasefile, ACTION='WRITE', IOSTAT = status, &
          & STATUS='REPLACE')
     IF (status/=0) THEN
        PRINT*, status
        CALL Handle_error("An error happened while creating initial release file in GenerateInitialPoints")
     ELSE
        ALLOCATE(initialPositions(nfloats), STAT = status)
        IF (status/=0) CALL Handle_error("An error happened while allocating memory for initialPositions in GenerateInitialPoints")
        initialPositions(1)%p = x0
        initialPositions(1)%time = t0
        WRITE (UNIT = 11, FMT = *) initialPositions(1)%p, 0.0,  initialPositions(1)%time  ! REMOVE THE LAST 0.0, ASK THE REASON OF FOURTH COLUMN IN RELEASE.INI file
        DO i=2, nfloats                                     ! Se supone que el fichero es X, Y, Z y tiempo, CORREGIR
           initialPositions(i)%p%x = x0%x + (Rx * (2*ran(seed) - 1))
           initialPositions(i)%p%y = x0%y + (Ry * (2*ran(seed) - 1))
           initialPositions(i)%time = t0
           WRITE (UNIT = 11, FMT = *) initialPositions(i)%p, 0.0, initialPositions(i)%time ! REMOVE THE LAST 0.0, ASK THE REASON OF FOURTH COLUMN IN RELEASE.INI file
        ENDDO
        CLOSE(UNIT=11)      !! Cierro el archivo, ya que lo he escrito entero
     ENDIF
  ELSE
     numberFloats = 0 
     DO WHILE (status == 0) ! Cuento el número de tramos que hay en el fichero    
        READ (UNIT=11, FMT=*, IOSTAT= status) ! Leo la línea sin guardar los datos
        numberFloats = numberFloats + 1
     ENDDO
     numberFloats = numberFloats - 1 ! quito 1 porque ha contado el último intento de lectura, que da error al llegar al final
     REWIND 11  ! Vuelvo al inicio del fichero
     ALLOCATE(initialPositions(numberFloats), STAT = status)
     IF (status/=0) CALL Handle_error("An error happened while allocating memory for initialPositions in GenerateInitialPoints") 
     DO i=1, numberFloats
        READ (UNIT=11, FMT=*, IOSTAT = status) initialPositions(i)%p, basura, initialPositions(i)%time
     ENDDO
     CLOSE(UNIT=11) !! Cierro el archivo, ya que lo he leido entero
  ENDIF
      
END SUBROUTINE GenerateInitialPoints



!***************************************************
!**** procedimiento que toma los puntos iniciales
!**** y llama a advectaParticula para generar
!***** las trayectorias para finalmente escribirlas
!***************************************************
SUBROUTINE SimulateTrayectories
  TYPE (TrajectoryPoint), ALLOCATABLE, DIMENSION(:) :: initialPoints ! initial position for particles
  TYPE (TrajectoryPoint), ALLOCATABLE, DIMENSION(:) :: trajectory ! to have all the values for a trajectory
  TYPE (TrajectoryPoint), ALLOCATABLE, DIMENSION(:,:) :: trajectories ! to manage all the trajectories
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: auxmatrix ! to perform computation with time variable 
  TYPE (Punto) :: p0  !! initial point of the trajectory
  DOUBLE PRECISION :: t00 !! initial time of the trajectory
  DOUBLE PRECISION :: deltaT !! time between snapshots (in time model)
  INTEGER :: i, j
  INTEGER :: numberTraj   ! number of trajectories
  INTEGER :: numberPoints ! number of points

  ! When we create netCDF files, variables and dimensions, we get back
  ! an ID for each one.
  INTEGER :: ncid, lonid, latid, timeid, timenavid, timejdid
  INTEGER, DIMENSION(2) :: dimids 
  INTEGER :: Floats_dimid, time_dimid 

  deltaT = time(time_I + 1) - time(time_I)
  CALL GenerateInitialPoints(initialPoints)
  numberTraj = SIZE(initialPoints)
  IF (numberTraj .EQ. 0) CALL Handle_error("ERROR: NO RELEASE POINTS, please use a NON EMPTY FILE !!")
  IF (debugging) THEN
     PRINT*, "===================================================="
     PRINT*, "Intitial positions for ", numberTraj, " floats"
     PRINT*, "===================================================="
     DO i=1, SIZE(initialPoints)
        PRINT*, initialPoints(i)
     ENDDO
     PRINT*, "===================================================="
  ENDIF
  ! Pongo en paso el paso/factor para pasar a nueva escala de tiempos CORREGIR cuando se generalice el tiempo
  i = 1
  t00 = t0 + initialPoints(i)%time
  p0 = initialPoints(i)%p
  IF (debugging) THEN
     PRINT*, "===================================================="
     PRINT*, "Trayectory number ", i
     PRINT*, "===================================================="
  ENDIF
  CALL AdvectaParticula(time(time_I) + (t00*deltaT)/factor,time(time_I) + ((t00+t1)*deltaT)/factor, p0, paso/factor,trajectory) 
  IF (debugging)  PRINT*, "===================================================="
  numberPoints = SIZE(trajectory)
  ALLOCATE(auxmatrix(numberTraj,numberPoints), STAT = status)
  IF (status .NE. 0) CALL Handle_error("An error happened while allocating memory for auxmatrix in SimulateTrayectories")
  ALLOCATE(trajectories(numberTraj,numberPoints), STAT = status)
  IF (status .NE. 0) CALL Handle_error("An error happened while allocating memory for trajectories in SimulateTrayectories")
  trajectories(:,:)%p%x = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default
  trajectories(:,:)%p%y = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default
  trajectories(:,:)%time = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default

  trajectories(i,:) = trajectory(:)
  DEALLOCATE(trajectory, STAT = status)
  IF (status .NE. 0) CALL Handle_error("An error happened while freeing memory for trajectory in SimulateTrayectories")
  DO i = 2, numberTraj
     PRINT '(F5.2,A40)', 100.0D0*DBLE(i-1)/DBLE(numberTraj),"% of simulation completed !!"
     t00 = t0 + initialPoints(i)%time
     p0 = initialPoints(i)%p
     IF (debugging) THEN
        PRINT*, "===================================================="
        PRINT*, "Trayectory number ", i
        PRINT*, "===================================================="
     ENDIF
     CALL AdvectaParticula(time(time_I) + (t00*deltaT)/factor,time(time_I) + ((t00+t1)*deltaT)/factor,p0,paso/factor,trajectory) 
     IF (debugging)  PRINT*, "===================================================="
     DO j = 1, SIZE(trajectory)
        trajectories(i,j) = trajectory(j)
     ENDDO
     DEALLOCATE(trajectory, STAT = status)
  ENDDO

  
  ! We write trajectories to the out file in nc format
  ! Create the netCDF file. The nf90_noclobber parameter prevent netCDF to
  ! overwrite this file, if it already exists.
  status = nf90_create(outfile, NF90_CLOBBER, ncid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened creating out file --> " // NF90_STRERROR(status))

  ! Define the dimensions. NetCDF will hand back an ID for each. 
  status= nf90_def_dim(ncid, "floats",numberTraj, Floats_dimid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting dimid for floats --> " // NF90_STRERROR(status))  
  status = nf90_def_dim(ncid, "time", numberPoints, time_dimid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened getting dimid for time --> " // NF90_STRERROR(status))

    ! The dimids array is used to pass the IDs of the dimensions of
  ! the variables. 
  dimids =  (/ Floats_dimid, time_dimid /)

   ! Define the longitude variable. The type of the variable in this case is NF90_DOUBLE.
  status = nf90_def_var(ncid, "longitude", nf90_double , dimids, lonid) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining longitude variable --> " // NF90_STRERROR(status))
  status = nf90_put_att(ncid, lonid, "standard_name","longitude")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining standard name in longitude variable --> "&
       & // NF90_STRERROR(status))  
  status = nf90_put_att(ncid, lonid, "long_name","Longitude")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining long name in longitude variable --> "&
       &// NF90_STRERROR(status))
  status = nf90_put_att(ncid, lonid, UNITS, LONGITUDE_UNITS)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining units in longitude variable --> "&
       &// NF90_STRERROR(status))
  status = nf90_put_att(ncid, lonid, "_FillValue", NF90_FILL_DOUBLE)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining _FillValue in longitude variable --> "&
       &// NF90_STRERROR(status))  

   ! Define the latitude variable. The type of the variable in this case is NF90_DOUBLE.
  status = nf90_def_var(ncid, "latitude", nf90_double , dimids, latid) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining latitude variable --> " // NF90_STRERROR(status))
  status = nf90_put_att(ncid, latid, "standard_name","latitude")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining standard name in latitude variable --> "&
       & // NF90_STRERROR(status))  
  status = nf90_put_att(ncid, latid, "long_name","Latitude")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining long name in latitude variable --> "&
       &// NF90_STRERROR(status))
  status = nf90_put_att(ncid, latid, UNITS, LATITUDE_UNITS)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining units in latitude variable --> "&
       &// NF90_STRERROR(status))
  status = nf90_put_att(ncid, latid, "_FillValue", NF90_FILL_DOUBLE)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining _FillValue in latitude variable --> "&
       &// NF90_STRERROR(status))  


  ! Define the time_model variable. The type of the variable in this case is NF90_DOUBLE.
  status = nf90_def_var(ncid, "time_model", nf90_double , dimids, timeid) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining time_model variable --> " // NF90_STRERROR(status))
  status = nf90_put_att(ncid, timeid, "long_name","model time")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining long_name in time_model variable --> "&
       & // NF90_STRERROR(status))  
  status = nf90_put_att(ncid, timeid, "calendar","standard")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining calendar in time_model variable --> "&
       &// NF90_STRERROR(status))
  status = nf90_put_att(ncid, timeid, UNITS,time_units)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining units in time_model variable --> "&
       &// NF90_STRERROR(status))    
  status = nf90_put_att(ncid, timeid, "_FillValue", NF90_FILL_DOUBLE)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining _FillValue in time_model variable --> "&
       &// NF90_STRERROR(status))  


  ! Define the time_navigation variable. The type of the variable in this case is NF90_DOUBLE.
  status = nf90_def_var(ncid, "time_navigation", nf90_double , dimids, timenavid) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining time_navigation variable --> "&
       &// NF90_STRERROR(status))
  status = nf90_put_att(ncid, timenavid, "long_name","Navigation time")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining long_name in time_navigation variable --> "&
       & // NF90_STRERROR(status))  
  status = nf90_put_att(ncid, timenavid, UNITS,"s")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining units in time_navigation variable --> "&
       &// NF90_STRERROR(status))    
  status = nf90_put_att(ncid, timenavid, "unit_long", "seconds")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining units in time_navigation variable --> "&
       &// NF90_STRERROR(status))    
  status = nf90_put_att(ncid, timenavid, "_FillValue", NF90_FILL_DOUBLE)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining _FillValue in time_navigation variable --> "&
       &// NF90_STRERROR(status))  

  ! Define the time variable. The type of the variable in this case is NF90_DOUBLE.
  status = nf90_def_var(ncid, "time", nf90_double , dimids, timejdid) 
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining time variable --> "&
       &// NF90_STRERROR(status))
  status = nf90_put_att(ncid, timejdid, "long_name","time in Julian days")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining long name in time variable --> "&
       & // NF90_STRERROR(status))  
  status = nf90_put_att(ncid, timejdid, UNITS,"days")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining units in time variable --> "&
       &// NF90_STRERROR(status))    
  status = nf90_put_att(ncid, timejdid, "unit_long", "Julian days")
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining unit_long in time variable --> "&
       &// NF90_STRERROR(status))    
  status = nf90_put_att(ncid, timejdid, "_FillValue", NF90_FILL_DOUBLE)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while defining _FillValue in time variable --> "&
       &// NF90_STRERROR(status))  


  
  !Global attributes
  status = nf90_put_att(ncid, nf90_global, "Title","Simulation generated by "//VERSION)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while stablishing title of nc file --> "&
       &// NF90_STRERROR(status))
  CALL GET_COMMAND(arg)
   status = nf90_put_att(ncid, nf90_global, "CommandLine",TRIM(arg))
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while stablishing title of nc file --> "&
       &// NF90_STRERROR(status))
  
  ! End define mode. This tells netCDF we are done defining metadata.
  status = nf90_enddef(ncid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while finishing the define mode of nc file --> " &
       &// NF90_STRERROR(status))

  ! Write longitude data to the file.
  status = nf90_put_var(ncid, lonid, trajectories(:,:)%p%x)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while writing longitudes to file --> " // NF90_STRERROR(status))
  ! Write latitude data to the file.
  status = nf90_put_var(ncid, latid, trajectories(:,:)%p%y)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while writing latitudes to file --> " // NF90_STRERROR(status))
  ! Write time data to the file.
  status = nf90_put_var(ncid, timeid, trajectories(:,:)%time)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while writing time to file --> " // NF90_STRERROR(status))

  ! Write time_navigation data to the file.
  auxmatrix = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default
  DO i = 1, numberTraj
     DO j = 1,numberPoints
        IF (DABS(trajectories(i,j)%time - NF90_FILL_DOUBLE).GT.CERO) THEN
           auxmatrix(i,j) = factor * (trajectories(i,j)%time - trajectories(i,1)%time) / deltaT
        ENDIF
     ENDDO
  ENDDO
  status = nf90_put_var(ncid, timenavid, auxmatrix)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while writing time_navigation to file --> "&
       &// NF90_STRERROR(status))

  ! Write time_jd data to the file.
  auxmatrix = NF90_FILL_DOUBLE  ! Fill all the data with the _Fill_Value by default
  DO i = 1, numberTraj
     DO j = 1,numberPoints
        IF (DABS(trajectories(i,j)%time - NF90_FILL_DOUBLE).GT.CERO) THEN
           auxmatrix(i,j) = InitialJulianDate + (factor * (trajectories(i,j)%time) / (24.0D0 * 60.0D0 * 60.0D0 * deltaT))
        ENDIF
     ENDDO
  ENDDO
  status = nf90_put_var(ncid, timejdid, auxmatrix)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while writing time_jd to file --> " // NF90_STRERROR(status))

    
 
  ! Close the file. This frees up any internal netCDF resources
  ! associated with the file, and flushes any buffers.
  status = nf90_close(ncid)
  IF (status /= nf90_noerr) CALL Handle_error("A problem happened while closing nc file --> " // NF90_STRERROR(status))
  IF (debugging) THEN
     PRINT *, "*** SUCCESS writing " // TRIM(outfile)//" !!! "
  ENDIF
 
END SUBROUTINE SimulateTrayectories




!********************************************************************************************************
!** FROM  NUMERICAL RECIPES IN FORTRAN 90: THE Art of PARALLEL Scientific Computing (ISBN 0-521-57439-0)
!*** Function to generate a random number following a Uniform distribution in [0,1)
!********************************************************************************************************
FUNCTION ran(idum)
IMPLICIT NONE
INTEGER, PARAMETER :: K4B=selected_int_kind(9)
INTEGER(K4B), INTENT(INOUT) :: idum
REAL :: ran
   !"Minimal" random number generator of Park and Miller combined with a Marsaglia shift
   !sequence. Returns a uniform random deviate between 0.0 and 1.0 (exclusive of the endpoint
   !values). This fully portable, scalar generator has the "traditional" (not Fortran 90) calling
   !sequence with a random deviate as the returned function value: call with idum a negative
   !integer to initialize; thereafter, do not alter idum except to reinitialize. The period of this
   !generator is about 3.1x 10^18 .
INTEGER(K4B), PARAMETER :: IA=16807,IM=2147483647,IQ=127773,IR=2836
REAL, SAVE :: am
INTEGER(K4B), SAVE :: ix=-1,iy=-1,k
if (idum <= 0 .or. iy < 0) then                     !Initialize.
    am=nearest(1.0,-1.0)/IM
    iy=ior(ieor(888889999,abs(idum)),1)
    ix=ieor(777755555,abs(idum))
    idum=abs(idum)+1                                !Set idum positive.
end if
ix=ieor(ix,ishft(ix,13))                            !Marsaglia shift sequence with period 2^32 - 1.
ix=ieor(ix,ishft(ix,-17))
ix=ieor(ix,ishft(ix,5))
k=iy/IQ                                             !Park-Miller sequence by Schrage's method,
iy=IA*(iy-k*IQ)-IR*k                                     !period 2^31 - 2.
if (iy < 0) iy=iy+IM
ran=am*ior(iand(IM,ieor(ix,iy)),1)                  !Combine the two generators with masking to
END FUNCTION ran                                    !     ensure nonzero value.



!********************************************************************************************************
!** To stablish the Julian Date of origin of time variable in the velocity field
!*** time unist must have the date in the order YYYY - MM -DD  and later HH:MM:SS
!*** separator symbols are not important, but only SIX number can appear in time units !!
!********************************************************************************************************
DOUBLE PRECISION FUNCTION SetInitialJulianDate(cad)
  CHARACTER (len = *) :: cad
  INTEGER :: pos, i
  INTEGER, DIMENSION(7) :: initialdate ! YY,MM,DD,HH,MM,SS of initial date of velocity field
  INTEGER :: Y, M, D ! Yeard, Month, Date
  INTEGER :: H, MIN, S ! Hour, Minute, Second
  LOGICAL :: finalizar !To stop the loop when needed
  INTEGER :: JDN ! Julian Date Number

  IF (LEN_TRIM(cad) .EQ. 0) CALL Handle_error("Time units NOT found in nc file")
  ! There are some text in time attribute, we can look for time the date
  finalizar = .FALSE.
  pos = 1
  i = 1
  initialdate = 0
  DO WHILE (pos.LE. LEN_TRIM(cad))
     IF ((cad(pos:pos).GE.'0').AND. (cad(pos:pos).LE.'9')) THEN
        initialdate(i) = initialdate(i)*10 + (ICHAR(cad(pos:pos))-ICHAR('0'))
        finalizar = .TRUE. !to finish reading integer value when the character is not a numeric one
     ELSE
        IF (finalizar) i = i + 1  !look for the next number
        finalizar = .FALSE.
        IF (i .GT.SIZE(initialdate)) CALL Handle_error("Time units must have format like YYYY-MM-DD and HH:MM-SS")
     ENDIF
     pos = pos + 1 ! check the next letter
  ENDDO
  Y = initialdate(1)
  M = initialdate(2)
  D = initialdate(3)
  H = initialdate(4)
  MIN = initialdate(5)
  S = initialdate(6)
  IF (initialdate(7).NE.0) CALL Handle_error("Time units must have format like YYYY-MM-DD and HH:MM-SS")
  
  

  !! Expresion from wikipedia to compute the Julian Date Number
  JDN = 0
  JDN = JDN + (1461 * (Y + 4800 + (M - 14)/12))/4
  JDN = JDN + (367 * (M - 2 - 12 * ((M - 14)/12)))/12
  JDN = JDN - (3 * ((Y + 4900 + (M - 14)/12)/100))/4 + D - 32075

  ! ATENCION HE puesto la chapuza de Quim de no restar las 12 horas para que lo pinte igual, pero hay que CORREGIRLO
  !SetInitialJulianDate = DBLE(JDN) + DBLE(H - 12)/24.0D0 + DBLE(MIN)/1440.0D0 + DBLE(S)/86400.0D0
  SetInitialJulianDate = DBLE(JDN) + DBLE(H)/24.0D0 + DBLE(MIN)/1440.0D0 + DBLE(S)/86400.0D0  
END FUNCTION SetInitialJulianDate





END PROGRAM MLM
