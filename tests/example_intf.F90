!
! Copyright 1981-2016 ECMWF.
!
! This software is licensed under the terms of the Apache Licence
! Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
PROGRAM EXAMPLE_INTF

  USE grib_api
  IMPLICIT NONE
  INTEGER, DIMENSION(4)                       :: INTV
  REAL, DIMENSION(4)                          :: REALV
  CHARACTER(Len=20), DIMENSION(4)             :: CHARV

  INTEGER (KIND=4), DIMENSION(1)              :: INGRIB(1), NEWFLD(1)

  INTEGER                                     :: IUNIT1, IUNIT2
  INTEGER                                     :: IGRIB
  INTEGER                                     :: IRET, NARGS
  INTEGER (KIND=KINDOFINT)                    :: INLEN, NEWLEN
  INTEGER                                     :: J
  INTEGER                                     :: NRESOL
  INTEGER                                     :: Ni, Nj
  INTEGER                                     :: IBPV
  INTEGER                                     :: len_in, len_out
  INTEGER                                     :: IPARAM, ITABLE
  REAL, DIMENSION(:), ALLOCATABLE             :: VALUES_IN, VALUES_OUT
  REAL                                        :: DLAT, DLON
  REAL                                        :: VALMISS

!     Externals
  INTEGER                                     :: INTIN, INTOUT, INTF, IARGC

  CHARACTER(Len=128)                          :: INFILE, OUTFILE
  CHARACTER(Len=128), DIMENSION(20)           :: CARG
  CHARACTER(Len=128)                          :: MESSAGE = ' '
  CHARACTER(Len=32)                           :: CINGRIDT = ' ', CINGRIDN = ' '

! **********************************************************************
! Default values

  IBPV = 16          ! bitsPerValue / accuracy
  NRESOL = 0         ! Gaussian resolution
  DLAT = 0.5         ! Latitude increment (in degrees)
  DLON = 0.5         ! Longitude increment (in degrees)
  VALMISS = -99999.9 ! Missing value indicator

!     Pick up file names from command line.

  NARGS = 0
  NARGS = IARGC()
  IF ( NARGS .NE. 4) THEN
     PRINT *, 'NARGS is incorrect'
     CALL USAGE
     STOP
  END IF

  DO J = 1, NARGS
     CALL GETARG(J,CARG(J))
  END DO

  DO J = 1, NARGS,2
    IF (CARG(J) == '-i') THEN
      INFILE=TRIM(CARG(J+1))
    ELSEIF (CARG(J) == '-o') THEN
      OUTFILE=TRIM(CARG(J+1))
    ELSE
      CALL USAGE
      STOP
    END IF
  END DO

  PRINT *, '*** Open input and output files'

  CALL GRIB_OPEN_FILE (IUNIT1, INFILE, 'r', IRET)
  IF ( IRET /= GRIB_SUCCESS) STOP ' GRIB_OPEN of INFILE failed'

  CALL GRIB_OPEN_FILE (IUNIT2, OUTFILE, 'w', IRET)
  IF ( IRET /= GRIB_SUCCESS) STOP ' GRIB_OPEN of OUTFILE failed'

  PRINT *, '*** Start of loop through input GRIB-coded fields'

  CALL GRIB_NEW_FROM_FILE(IUNIT1, IGRIB, IRET)

  LOOP: DO WHILE (IRET /= GRIB_END_OF_FILE)

    IF (IRET /= GRIB_SUCCESS) THEN
      PRINT *, ' GRIB_NEW_FROM_FILE failed with error code = ', IRET
      CALL GRIB_GET_ERROR_STRING(IRET,MESSAGE)
      PRINT *, TRIM(MESSAGE)
      PRINT *, 'Skipping to next message ...'
      CALL GRIB_NEW_FROM_FILE(IUNIT1, IGRIB, IRET)
      CYCLE
    ENDIF

    PRINT *, '*** Unpack the values in the input GRIB'

    CALL GRIB_GET_SIZE(IGRIB,'values',len_in)
    PRINT *,  '     Allocating array for input field of size=',len_in
    ALLOCATE(VALUES_IN(len_in))
    CALL GRIB_SET(IGRIB,'missingValue',VALMISS)
    CALL GRIB_GET(IGRIB,'values',VALUES_IN)

! Define the input field via calls to INTIN
    CHARV(1) = 'unpacked'
    IRET = INTIN('form',INTV, REALV, CHARV)
    IF ( IRET /= 0 ) THEN
      PRINT *,  ' INTIN failed to set format'
      STOP
    ENDIF

! Get parameter information (assumes unportable ParamId structure)
    CALL GRIB_GET(IGRIB,'paramId',IPARAM)
    ITABLE = 128
    IF (IPARAM.GT.1000) THEN
      ITABLE = IPARAM / 1000
      IPARAM = MOD(IPARAM,1000)
    ENDIF

    INTV(1) = ITABLE
    IRET = INTIN('table',INTV, REALV, CHARV)
    IF ( IRET /= 0 ) THEN
      PRINT *, ' INTIN failed to set table'
      STOP
    ENDIF

    INTV(1) = IPARAM
    IRET = INTIN('parameter',INTV, REALV, CHARV)
    IF ( IRET /= 0 ) THEN
      PRINT *, ' INTIN failed to set parameter'
      STOP
    ENDIF

! Check it's a Gaussian grid and set resolution
    CALL GRIB_GET(IGRIB,'gridType',CINGRIDT)
    IF ((TRIM(CINGRIDT) ==  'regular_gg') .OR. &
        (TRIM(CINGRIDT) ==  'reduced_gg')) THEN
      CALL GRIB_GET(IGRIB,'gridName',CINGRIDN)
      CHARV(1) = CINGRIDN(1:20)
      PRINT *, '     gridName = ', CHARV(1)
      IRET = INTIN('gridname',INTV, REALV, CHARV)
      IF ( IRET /= 0 ) THEN
        PRINT *, ' INTIN failed to set gridname'
        STOP
      ENDIF
    ELSE
      PRINT *, 'Input grid type ', TRIM(CINGRIDT), ' not regular_gg/reduced_gg'
      CYCLE
    ENDIF

! Set the missing value
    CHARV(1) = 'yes'
    REALV(1) = VALMISS
    IRET = INTIN('missingvalue',INTV, REALV, CHARV)
    IF ( IRET /= 0 ) THEN
      PRINT *, ' INTIN failed to set missingValue'
      STOP
    ENDIF

! Define the output field with calls to INTOUT
    CHARV(1) = 'unpacked'
    IRET = INTOUT('form',INTV, REALV, CHARV)
    IF ( IRET /= 0 ) THEN
      PRINT *, ' INTOUT failed to set format'
      STOP
    ENDIF

    PRINT *, '*** Define the grid interval for the new field(s)'
    REALV(1) = DLAT
    REALV(2) = DLON
    IRET = INTOUT('grid', INTV, REALV, CHARV)
    IF ( IRET /= 0 ) THEN
      PRINT *, ' INTOUT failed to set grid resolution'
      STOP
    ENDIF
    PRINT *, '    GRID TYPE = lat-lon'
    PRINT *, '    RESOL = ', REALV(1), 'x', REALV(2)

! Calculate the number of points (assumes global grid)
    Ni = INT(360.0/REALV(2))
    Nj = INT(180.0/REALV(1)) + 1
    len_out = Ni*Nj
    PRINT *, '    Allocating output array of size = ', len_out
    ALLOCATE(VALUES_OUT(LEN_OUT))

! Define the packing accuracy for the new field(s).
    INTV(1) = IBPV
    IRET = INTOUT('accuracy', INTV, REALV, CHARV)
    IF ( IRET /= 0 ) THEN
      PRINT *, ' INTOUT failed to set accuracy'
      STOP
    ENDIF
    PRINT *, '    ACCURACY = ', IBPV

    PRINT *, '*** Interpolate'
    INLEN =1
    NEWLEN = 1
    IRET = INTF(INGRIB,len_in,VALUES_IN,NEWFLD,len_out,VALUES_OUT)
    IF ( IRET /= 0 ) THEN
      WRITE(*,*) ' INTF failed for product '
      WRITE(*,*) ' Skipping to next message ...'
      CALL GRIB_RELEASE(IGRIB)
      DEALLOCATE(VALUES_IN,VALUES_OUT)
      CYCLE
    ENDIF

    PRINT *, '*** Set-up and write output GRIB'

    CALL GRIB_SET(IGRIB,'gridType','regular_ll')
    CALL GRIB_SET(IGRIB,'latitudeOfFirstGridPointInDegrees',90.0)
    CALL GRIB_SET(IGRIB,'latitudeOfLastGridPointInDegrees',-90.0)
    CALL GRIB_SET(IGRIB,'longitudeOfFirstGridPointInDegrees',0.0)
    CALL GRIB_SET(IGRIB,'longitudeOfLastGridPointInDegrees',360.0 - DLON)
    CALL GRIB_SET(IGRIB,'iDirectionIncrementInDegrees',DLON)
    CALL GRIB_SET(IGRIB,'jDirectionIncrementInDegrees',DLON)
    CALL GRIB_SET(IGRIB,'bitsPerValue',IBPV)
    CALL GRIB_SET(IGRIB,'Ni',Ni)
    CALL GRIB_SET(IGRIB,'Nj',Nj)
    CALL GRIB_SET(IGRIB,'values',VALUES_OUT)
    CALL GRIB_WRITE(IGRIB,IUNIT2)

!   Free the memory.
    CALL GRIB_RELEASE(IGRIB)
    DEALLOCATE(VALUES_IN, VALUES_OUT)

!   Loop back for next product.
    CALL GRIB_NEW_FROM_FILE(IUNIT1, IGRIB, IRET)

  END DO LOOP

! Close input and output files.
  CALL GRIB_CLOSE_FILE(IUNIT1, IRET)
  CALL GRIB_CLOSE_FILE(IUNIT2, IRET)

  STOP

CONTAINS

  SUBROUTINE USAGE

   PRINT *, 'Usage: example_intf -i inputfile -o outputfile'
   PRINT *,  '   -i inputfile      input file name (required)'
   PRINT *,  '   -o outputfile     output file name (required)'

   RETURN

  END SUBROUTINE USAGE

END PROGRAM EXAMPLE_INTF

