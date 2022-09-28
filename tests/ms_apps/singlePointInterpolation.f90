!
! Copyright 1981-2016 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! ******************************
! Modified by: Cristian Simarro, ported from GRIBEX to GRIB_API
! DATE: 17-02-2016
! ******************************
!

PROGRAM singlePointInterpolation
!
  USE grib_api

  IMPLICIT NONE
  INTEGER                                     :: IPROD
  INTEGER, DIMENSION(4)                       :: INTV
  REAL, DIMENSION(4)                          :: REALV
  CHARACTER(Len=20), DIMENSION(4)             :: CHARV
  LOGICAL                                     :: ASCII
  LOGICAL                                     :: WRITE_INTERIM_GG
  CHARACTER(Len=20)                           :: TMPGNAME
  CHARACTER(Len=20)                           :: GNAME

  INTEGER, PARAMETER                          :: JPGRIB=10000000
  INTEGER, PARAMETER                          :: JPBYTES=4
  INTEGER, PARAMETER                          :: MISSING_VALUE=-9999999
!
!     JPBYTES is the size in bytes on an 'INTEGER'
!     Set JPBYTES = 8 on a 64-bit machine.
!
  CHARACTER (LEN=1),DIMENSION(:), ALLOCATABLE :: WORK, OUTGRIB
  CHARACTER                                   :: HTYPE
  CHARACTER                                   :: GAUSSIAN
  REAL , DIMENSION(1000)                      :: LATS, LONS
  REAL                                        :: LLVALUES(1)
!
  INTEGER                                     :: isOctahedral
  CHARACTER(Len=20)                           :: gridType
  INTEGER                                     :: bitsPerValue
  INTEGER                                     :: KPTS(10024),PLATS(10024)
  INTEGER                                     :: IUNIT1, IUNIT2,IUNIT3,IUNITASCII
  INTEGER                                     :: IGRIB, IGRIB2
  INTEGER                                     :: IRET, NARGS
  INTEGER (KIND=KINDOFINT)                    :: INLEN, NEWLEN, NSIZE
  INTEGER                                     :: J
  INTEGER                                     :: NLATLON, LOOP, LOOPO
  INTEGER                                     :: EOF
  INTEGER                                     :: IFILEN
  INTEGER                                     :: NGAUSS, NPTS, NLON, NLAT
  REAL                                        :: AREA(4), GRID(2), POLE(2)
  REAL,DIMENSION(:), ALLOCATABLE              :: GLATS,GLONS,GVALUES

!
!     Externals
  INTEGER                                     :: INTOUT, INTF2, IARGC, HIRLSM
  INTEGER                                     :: HSP2GG
  EXTERNAL                                    :: HIRLSM, HSP2GG

  CHARACTER(Len=128)                          :: LLFILE, INFILE, OUTFILE
  CHARACTER(Len=128)                          :: MESSAGE = ' '
!
! **********************************************************************
!
!  Definitions and parameters
!
  DATA GRID/1.0,1.0/
  DATA POLE/-90.0,0.0/
!
!  GAUSSIAN: Set the intermediate type of the reduced grid in case of converting from SH
!  [O,N]
!
  GAUSSIAN = 'O'
!
!  Write the intermediate gaussian file?
!
  WRITE_INTERIM_GG = .FALSE.
  TMPGNAME = GAUSSIAN//'TMP'//'.grib'
!
!  Unit to use to write the ascii file if needed
!
  IUNITASCII = 16
  ASCII = .FALSE.
!
!  Accuracy in the intermediate gaussian if source is SH
!
  bitsPerValue = 16

! **********************************************************************
!
!  Pick up file names from command line.
!
  NARGS = IARGC()
  IF ( NARGS < 3 .OR. NARGS > 4) THEN
     print*,'Usage: singlePointInterpolation llfile, inputfile outputfile [a]'
     STOP
  END IF

  CALL GETARG(1,LLFILE)
  CALL GETARG(2,INFILE)
  CALL GETARG(3,OUTFILE)
  IF (NARGS == 4) THEN
    ASCII = .TRUE.
  ENDIF
!
!  Open input and output GRIB files.
!
  CALL GRIB_OPEN_FILE (IUNIT1, TRIM(INFILE), 'r', IRET)
  IF ( IRET /= GRIB_SUCCESS) STOP ' GRIB_OPEN failed'

  CALL GRIB_OPEN_FILE (IUNIT2, TRIM(OUTFILE), 'w', IRET)
  IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_OPEN failed'
  IPROD = 0
  IF ( ASCII ) THEN
    OPEN(UNIT=IUNITASCII,FILE=TRIM(OUTFILE)//'.txt',IOSTAT=IRET)
    IF (IRET == 0) THEN
      WRITE(*,*)'File:',TRIM(OUTFILE),' opened'
    ELSE
      WRITE(*,*)'File:',TRIM(OUTFILE),' not opened'
      WRITE(*,*)'Input/output status = ',IRET
      WRITE(*,*)'Program will stop'
      STOP
    END IF
  END IF
  IF (WRITE_INTERIM_GG) THEN
    CALL GRIB_OPEN_FILE (IUNIT3, TMPGNAME, 'w', IRET)
    IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_OPEN failed'
  ENDIF
!
! Open file containing lat-lon coordinates
!
  IFILEN = INDEX(LLFILE,' ') - 1
  OPEN(UNIT=1,FILE=LLFILE,IOSTAT=IRET)
  IF (IRET /= 0) STOP 'ERROR on open of lat-lon file'
!
! Read the latitude-longitude points
!
  EOF = 0
  LOOP = 1
  NLATLON = 0
  DO WHILE (EOF == 0) 
    READ(1,*,IOSTAT=IRET) LATS(LOOP), LONS(LOOP)
    IF (IRET > 0) THEN
      STOP 'Error reading lat-lon file'
    ELSEIF (IRET < 0) THEN
      EOF=0
      EXIT
    ENDIF
    IF (LATS(LOOP) > 90.0 .OR. LATS(LOOP) < -90.0) THEN
      WRITE(*,*) 'Latitude ', LOOP, ' is faulty: (',LATS(LOOP),')'
      STOP ' Fail'
    ENDIF
    IF (LONS(LOOP) < -360.0 .OR. LONS(LOOP) > 360.0) THEN
      WRITE(*,*) 'Longitude ', LOOP, ' is faulty: (',LONS(LOOP),')'
      STOP ' Fail'
    ENDIF
    NLATLON = NLATLON + 1
    LOOP = LOOP + 1
  END DO
  CLOSE(UNIT=1)
  IF (NLATLON < 1) STOP 'No latitude/longitude pairs given'
!
  WRITE(*,*) 'Number of latitude/longitude pairs = ', NLATLON
!
! Start of loop through input GRIB-coded fields creating a HANDLER
!
  CALL GRIB_NEW_FROM_FILE(IUNIT1,IGRIB,IRET)
  WRITE (*,*) 'Iterating over the messages...'
  
  DO WHILE (IRET /= GRIB_END_OF_FILE)
    IF (IRET /= GRIB_SUCCESS) THEN
      PRINT *, ' GRIB_READ_FROM_FILE failed with error code = ', IRET
      CALL GRIB_GET_ERROR_STRING(IRET,MESSAGE)
      PRINT *, TRIM(MESSAGE)
      PRINT *, 'Skipping to next message ...'
      CYCLE
    ENDIF
!    IPROD = IPROD + 1
!    WRITE(*,*) 'Message number' , IPROD

!
!  Get the gridType
!
    CALL GRIB_GET(IGRIB,'gridType',gridType,IRET)
    IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_GET failed for gridType'
!
!  Check if the input is SH, if it is, interpolate to GG
!
    IF ( gridType == 'sh' ) THEN
!
!  Transform to a reduced Gaussian grid
!  First copy message from HANDLER ( IGRIB ) into MEMORY ( WORK ).
!
!  WRITE (*,*) 'Found spherical harmonics input, interpolating to ',GAUSSIAN, &
!  ' GG'
      CALL GRIB_GET_MESSAGE_SIZE(IGRIB,INLEN,IRET)
      IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_MESSAGE_SIZE failed'
      ALLOCATE(WORK(INLEN))
      CALL GRIB_COPY_MESSAGE(IGRIB,WORK,IRET)
      IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_COPY_MESSAGE failed'
!      
!  Get the proper values for the new reduced grib
!
      CALL GRIB_GET(IGRIB,'pentagonalResolutionParameterJ',J,IRET)
      IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_GET pentagonalResolutionParameterJ failed'
      CALL GRIB_GET_SIZE(IGRIB,'values',NPTS)
      IRET = HSP2GG(J,NGAUSS,KPTS,PLATS,NSIZE)
      IF( IRET.NE.0 ) THEN
          WRITE(*,*) 'Failed to find suitable gaussian grid for the'
          WRITE(*,*) 'spectral input GRIB'
          STOP 'FAIL'
      ENDIF
!      WRITE (*,*) J,NGAUSS,NSIZE,NPTS,INLEN
!
!  Create GNAME string with [N,O]NGAUSS to send to INTOUT gridname
!
      WRITE (GNAME,*) NGAUSS
      GNAME=GAUSSIAN//adjustl(GNAME)
!
!  Set the INTOUT parameters to be reduced, octahedral?
!
      IRET = INTOUT('gridname',INTV,REALV,GNAME)
      IF( IRET.NE.0 ) THEN
          WRITE(*,*) 'Failed to setup gaussian grid interpolation'
          STOP 'FAIL'
      ENDIF
      IRET = INTOUT('accuracy',bitsPerValue,REALV,CHARV)
      IF( IRET.NE.0 ) THEN
          WRITE(*,*) 'Failed to setup gaussian grid interpolation'
          STOP 'FAIL'
      ENDIF
!
!  From sh message in WORK we will interpolate to reduced OUTGRIB
!  We need to figure out NEWLEN to avoid memory waste
!
      NEWLEN = JPGRIB
      ALLOCATE(OUTGRIB(NEWLEN))
      IRET = INTF2(WORK,INLEN,OUTGRIB,NEWLEN)
      IF ( IRET /= 0 ) THEN
        WRITE(*,*) ' INTF failed, use environment JDCNDBG=1'
        STOP
      ENDIF
!
!  Create new GRIB handle from the new message OUTGRIB
!
      CALL GRIB_NEW_FROM_MESSAGE(IGRIB,OUTGRIB,IRET)
      IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_NEW_FROM_MESSAGE failed'
      IF (WRITE_INTERIM_GG) THEN
        CALL GRIB_WRITE(IGRIB,IUNIT3,IRET)
        IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_WRITE failed'
      ENDIF
      DEALLOCATE(OUTGRIB,WORK)
!
!  Finish section SH -> GG
!
    ENDIF
    
!
!  Interpolate the gaussian field to lat/long
!  At this point IGRIB is either the handler to the source or the generated interim GG
!  First let's find out the type of grid to use in the HIRLSM
!
    CALL GRIB_GET(IGRIB,'gridType',gridType,IRET)
    IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_GET failed for gridType'
    CALL GRIB_GET(IGRIB,'isOctahedral',isOctahedral)
    IF ( gridType == 'regular_gg' ) THEN
        HTYPE = 'F'
    ELSEIF ( isOctahedral == 1 ) THEN
        HTYPE = 'O'
    ELSE
        HTYPE = 'R'
    ENDIF
!   WRITE (*,*) HTYPE
!
!  NGAUSS: Get the grib key N (old GRIBEX ISEC2(10))
!  NPTS:   Get the size of values (old GRIBEX ISEC4(1))
!  GLATS,GLONS,GVALUES: Get the data (old GRIBEX ZSEC(4))
!
    CALL GRIB_GET(IGRIB,'N',NGAUSS,IRET)
    IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_GET failed'
    CALL GRIB_GET_SIZE(IGRIB,'values',NPTS)
    IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_GET_SIZE failed'
    ALLOCATE(GLATS(NPTS))
    ALLOCATE(GLONS(NPTS))
    ALLOCATE(GVALUES(NPTS))
    CALL GRIB_GET_DATA(IGRIB,GLATS,GLONS,GVALUES)
    IF ( IRET /= GRIB_SUCCESS ) STOP ' GRIB_GET_DATA failed'
    
!
!  Interpolate to a single-point regular_ll as many times as lat/lon in the input file
!
    DO LOOPO = 1, NLATLON
        AREA(1) = LATS(LOOPO)
        AREA(2) = LONS(LOOPO)
        AREA(3) = LATS(LOOPO)
        AREA(4) = LONS(LOOPO)
        
        IRET = HIRLSM(.TRUE.,GVALUES,NPTS,NGAUSS,HTYPE,AREA,POLE,GRID,LLVALUES,1, &
          NLON,NLAT)
        IF( IRET.NE.0 ) THEN
          WRITE (*,*) IRET
          STOP 'Interpolation from gaussian grid to lat/lon grid failed be sure that you are using emoslib double precission r64'
        ENDIF
        IF (ASCII) THEN
          WRITE(IUNITASCII,"(1x,i5,2x,2(F8.2,1X),f8.2)") LOOPO, LATS(LOOPO), LONS(LOOPO), LLVALUES(1)
        ENDIF
    
!    
!  Set the keys and write the new values to OUT file.
!       ISEC2( 1) = 0
!       ISEC2( 2) = 1
!       ISEC2( 3) = 1
!       ISEC2( 4) = NINT(LATS(LOOPO)*1000)
!       ISEC2( 5) = NINT(LONS(LOOPO)*1000)
!       ISEC2( 6) = 128
!       ISEC2( 7) = NINT(LATS(LOOPO)*1000)
!       ISEC2( 8) = NINT(LONS(LOOPO)*1000)
!       ISEC2( 9) = 1000
!       ISEC2(10) = 1000
!       ISEC2(11) = 0
!       ISEC2(12) = 0
!       ISEC2(13) = 0
!       ISEC2(14) = 0
!       ISEC2(15) = 0
!       ISEC2(16) = 0
!       ISEC2(17) = 0
!       ISEC2(18) = 0
!       ISEC2(19) = 0
!
        CALL GRIB_CLONE(IGRIB,IGRIB2)
        IF ( IRET /= GRIB_SUCCESS)  STOP ' GRIB_CLONE failed'
        CALL GRIB_SET(IGRIB2,'gridType','regular_ll')
        CALL GRIB_SET(IGRIB2,'Ni',1)
        CALL GRIB_SET(IGRIB2,'Nj',1)
        CALL GRIB_SET(IGRIB2,'latitudeOfFirstGridPointInDegrees',LATS(LOOPO))
        CALL GRIB_SET(IGRIB2,'longitudeOfFirstGridPointInDegrees',LONS(LOOPO))
        CALL GRIB_SET(IGRIB2,'latitudeOfLastGridPointInDegrees',LATS(LOOPO))
        CALL GRIB_SET(IGRIB2,'longitudeOfLastGridPointInDegrees',LONS(LOOPO))
        CALL GRIB_SET(IGRIB2,'iDirectionIncrementInDegrees',GRID(1))
        CALL GRIB_SET(IGRIB2,'jDirectionIncrementInDegrees',GRID(2))
        CALL GRIB_SET(IGRIB2,'missingValue',MISSING_VALUE)
        CALL GRIB_SET(IGRIB2,'bitsPerValue',bitsPerValue)
        CALL GRIB_SET(IGRIB2,'values',LLVALUES)
!
!  Finally write the data into the OUTFILE
!
        CALL GRIB_WRITE(IGRIB2,IUNIT2,IRET)
        IF ( IRET /= GRIB_SUCCESS)  STOP ' GRIB_WRITE failed'
    END DO
!
!  Free the memory.
! 
    CALL GRIB_RELEASE(IGRIB)
    CALL GRIB_RELEASE(IGRIB2)
    DEALLOCATE(GLATS,GLONS,GVALUES)
!
!  Loop back for next product.
!
    CALL GRIB_NEW_FROM_FILE(IUNIT1,IGRIB,IRET)    
  END DO
!
!  Close input and output files.
! 
  CALL GRIB_CLOSE_FILE(IUNIT1, IRET)
  CALL GRIB_CLOSE_FILE(IUNIT2, IRET)
  IF (WRITE_INTERIM_GG) THEN
    CALL GRIB_CLOSE_FILE(IUNIT3,IRET)
  ENDIF
  CLOSE(UNIT=IUNITASCII)
!
  WRITE (*,*) 'Finished, check the output file ' , OUTFILE
  STOP
END PROGRAM singlePointInterpolation

