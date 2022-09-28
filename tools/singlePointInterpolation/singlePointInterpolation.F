#define MISSING_VALUE -9999999
      PROGRAM SINGLEPT
      IMPLICIT NONE
C
      INTEGER JPACK, JPBYTES
      PARAMETER (JPACK = 5000000)
      PARAMETER (JPBYTES = 4)
C
      INTEGER ISEC0(2)
      INTEGER ISEC1(1024)
      INTEGER ISEC2(2048)
      INTEGER ISEC3(2)
      INTEGER ISEC4(1024)
      INTEGER INBUFF(JPACK), OUTGRIB(JPACK)
      INTEGER KPTS(1024)
      INTEGER NSIZE, OUTLEN
      INTEGER IRET, FILE1, FILE2
      INTEGER ILENB, LENOUT, IPUNP, IWORD, LOOP, LOOPO
      INTEGER NLON, NLAT, NPTS, NGAUSS, NLATLON
      INTEGER IFILEN, IOS, NARGS

C
      REAL ZSEC2(1024)
      REAL ZSEC3(2)
      REAL ZSEC4(JPACK*4)
      REAL USEC4(JPACK*4)

      LOGICAL ASCII
      CHARACTER*1 HTYPE
C
      REAL AREA(4), GRID(2), POLE(2), LATS(1000), LONS(1000)
      REAL PLATS(1024), REALV(4), DUMMY(1)
C
      CHARACTER*10 CHARV(1)
      CHARACTER*128 POINTS, INFILE, OUTFILE
C
      INTEGER  HRG2LL, HIRLSM, HSH2GG, INTOUT, INTF, IARGC
      EXTERNAL HRG2LL, HIRLSM, HSH2GG, INTOUT, INTF
C
C     Commented out for gfortran
C
CCC      EXTERNAL IARGC
C
C     Fix dummy grid step and Souh pole position
C
      DATA GRID/1.0,1.0/
      DATA POLE/-90.0,0.0/
C
C     -----------------------------------------------------------------|
C     Pick up the command line file names and open the files
C     -----------------------------------------------------------------|
C
      ASCII = .false.
      NARGS = IARGC()
      IF( NARGS.LT.3 ) THEN
        WRITE(*,*) ' '
        WRITE(*,*) 'Usage: singlePointInterpolation  llfile'
     X     ,'  infile  outfile a'
        WRITE(*,*) ' '
        WRITE(*,*) '  latlonfile is a list of latitude/longitude values'
        WRITE(*,*) ' '
        WRITE(*,*) '  inpfile is a file of global GRIB fields'
        WRITE(*,*) ' '
        WRITE(*,*) '  outfile is a file of single-point GRIBs or ascii'
        WRITE(*,*) ' '
        WRITE(*,*) '  a optional ascii output'
        WRITE(*,*) ' '
        STOP 'FAIL'
      ENDIF
C
        CALL GETARG(1,POINTS)
        CALL GETARG(2,INFILE)
        CALL GETARG(3,OUTFILE)

         if(NARGS.eq.4) ASCII = .TRUE.

C
C     Open the input GRIB file
C
      IFILEN = INDEX(INFILE,' ') - 1
      CALL PBOPEN(FILE1, INFILE (1:IFILEN), "r", IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*, *) 'Return code from PBOPEN = ',IRET
        CALL PBCLOSE(FILE1, IRET)
        STOP 'Fault in PBOPEN for input file'
      ENDIF
C
C     Open the output file of single-point GRIBs
C
       IFILEN = INDEX(OUTFILE,' ') - 1
      IF(ASCII) THEN
       OPEN (UNIT=16,FILE=OUTFILE(1:IFILEN)//'.txt',STATUS='UNKNOWN',
     &          FORM='FORMATTED',IOSTAT=IOS)

            IF (IOS.EQ.0) THEN
                WRITE(*,*)'File:',OUTFILE(1:IFILEN),' opened'
            ELSE
               WRITE(*,*)'File:',OUTFILE(1:IFILEN),' not opened'
               WRITE(*,*)'Input/output status = ',IOS
               WRITE(*,*)'Program will stop'
               STOP
            END IF
      END IF

      CALL PBOPEN(FILE2, outfile, "w", IRET)
      IF( IRET.NE.0 ) THEN
        WRITE (*, *) 'Return code from PBOPEN = ',IRET
        STOP 'Fault in PBOPEN for output file'
      ENDIF
C
C     Open the file of lat/longs
C
      IFILEN = INDEX(POINTS,' ') - 1
      OPEN(UNIT=1,FILE=POINTS(1:IFILEN),STATUS='OLD'
     X ,FORM='FORMATTED',ERR=990)
C
C     Read the latitude/longitude pairs
C
      NLATLON = 0
      DO LOOP = 1, 1000
        READ(1,9000,ERR=980,END=110) LATS(LOOP), LONS(LOOP)
        IF( (LATS(LOOP).GT.90.0).OR.(LATS(LOOP).LT.-90.0) ) THEN
          WRITE (*, *) 'Latitude ',LOOP,' is faulty (',LATS(LOOP),')'
          STOP 'FAIL'
        ENDIF
c        WRITE (*,9000) LATS(LOOP), LONS(LOOP)
C
        IF( (LONS(LOOP).GT.360.0).OR.(LONS(LOOP).LT.-360.0) ) THEN
          WRITE (*, *) 'Longitude ',LOOP,' is faulty (',LONS(LOOP),')'
          STOP 'FAIL'
        ENDIF
C
        NLATLON = NLATLON + 1
C
      ENDDO
C
  110 CONTINUE
      CLOSE(UNIT=1)
      IF( NLATLON.LT.1 ) STOP 'No latitude/longitude pairs given'
C
      WRITE(*,*) 'Number of latitude/longitude pairs = ', NLATLON
C
C     -----------------------------------------------------------------|
C     Read the input GRIB fields one at a time and generate an
C     interpolated value at each latitude/longitude point.
C     -----------------------------------------------------------------|
C
  150 CONTINUE
C
C     Read next GRIB field.
C
      ILENB = JPACK*JPBYTES
      CALL PBGRIB(FILE1, INBUFF, ILENB, LENOUT, IRET )
      IF( IRET.LT.0 ) THEN
        IF( IRET.NE.-1 ) WRITE (*, *) 'Return code from PBGRIB = ',IRET
C
        IF( IRET.EQ.-1 ) THEN
          CALL PBCLOSE(FILE2, IRET)
          IF( IRET.LT.0 ) THEN
            WRITE(*, *) 'Return code from PBCLOSE = ',IRET
            WRITE(*, *) 'Fault in PBCLOSE for output file'
          ENDIF
C
          CALL PBCLOSE(FILE1, IRET)
          STOP 'Interpolation completed'
        ELSE
          CALL PBCLOSE(FILE1, IRET)
          STOP 'Fault in PBCLOSE for input file'
        ENDIF
      ENDIF
C
C     Unpack the GRIB message.
C
      ZSEC3(2) = MISSING_VALUE
      ISEC3(2) = NINT(ZSEC3(2))
      IRET  = 1
      IPUNP = JPACK * JPBYTES
      ILENB = (LENOUT + (JPBYTES-1))/JPBYTES
      CALL GRIBEX(ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     X            ZSEC4,IPUNP,INBUFF,ILENB,IWORD,'D',IRET)
      IF( IRET.GT.0 ) THEN
        WRITE(*,*) 'GRIBEX return code = ',IRET
        STOP 'GRIBEX error in decoding'
      ENDIF
C
C     If the input GRIB field is spectral, convert to a suitable
C     gaussian grid before interpolation
C
      IF( ISEC2(1).EQ.50 ) THEN
C
        NGAUSS = 0
        HTYPE  = ''
        IRET = HSH2GG(0,0,ISEC2(2),NGAUSS,HTYPE,KPTS,PLATS,NSIZE)
        IF( IRET.NE.0 ) THEN
          WRITE(*,*) 'Failed to find suitable gaussian grid for the'
          WRITE(*,*) 'spectral input GRIB'
          STOP 'FAIL'
        ENDIF
C
        IRET = INTOUT('REDUCED',NGAUSS,REALV,CHARV)
        IF( IRET.NE.0 ) THEN
          WRITE(*,*) 'Failed to setup gaussian grid interpolation'
          STOP 'FAIL'
        ENDIF
C
        OUTLEN = JPACK
        IRET = INTF(INBUFF,ILENB,DUMMY,OUTGRIB,OUTLEN,DUMMY)
        IF( IRET.NE.0 ) THEN
          WRITE(*,*) 'Gaussian grid interpolation failed'
          STOP 'FAIL'
        ENDIF
C
C        Unpack the interpolated GRIB message.
C
         ZSEC3(2) = MISSING_VALUE
         ISEC3(2) = NINT(ZSEC3(2))
         IRET = 1
         IPUNP = JPACK * JPBYTES
         ILENB = (LENOUT + (JPBYTES-1))/JPBYTES
         CALL GRIBEX(ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     X               ZSEC4,IPUNP,OUTGRIB,OUTLEN,IWORD,'D',IRET)
         IF( IRET.GT.0 ) THEN
           WRITE(*,*) 'GRIBEX return code = ',IRET
           STOP 'GRIBEX error in decoding'
         ENDIF
      ENDIF
C
C     Interpolate the gaussian field to lat/long
C
      NGAUSS = ISEC2(10)
      NPTS   = ISEC4(1)
C
C     Loop through the latitude/longitude points
C
      DO LOOPO = 1, NLATLON
        AREA(1) = LATS(LOOPO)
        AREA(2) = LONS(LOOPO)
        AREA(3) = LATS(LOOPO)
        AREA(4) = LONS(LOOPO)
C
        IRET = HIRLSM(.TRUE.,ZSEC4,NPTS,NGAUSS,AREA,POLE,GRID,USEC4,1,
     X                NLON,NLAT)
        IF( IRET.NE.0 )
     X    STOP 'Interpolation from gaussian grid to lat/lon grid failed'
C
      IF(ASCII) THEN
        WRITE (16,9001)LOOPO, LATS(LOOPO), LONS(LOOPO), USEC4(1)
      ENDIF
C       Repack the field and write to results file
C
        IRET = 1
        IPUNP = ISEC4(1)
        ILENB = JPACK
        DO LOOP = 1, ILENB
          INBUFF(LOOP) = 0
        ENDDO
C
C       Setup output GRIB header information
C
        ISEC2( 1) = 0
        ISEC2( 2) = 1
        ISEC2( 3) = 1
        ISEC2( 4) = NINT(LATS(LOOPO)*1000)
        ISEC2( 5) = NINT(LONS(LOOPO)*1000)
        ISEC2( 6) = 128
        ISEC2( 7) = NINT(LATS(LOOPO)*1000)
        ISEC2( 8) = NINT(LONS(LOOPO)*1000)
        ISEC2( 9) = 1000
        ISEC2(10) = 1000
        ISEC2(11) = 0
        ISEC2(12) = 0
        ISEC2(13) = 0
        ISEC2(14) = 0
        ISEC2(15) = 0
        ISEC2(16) = 0
        ISEC2(17) = 0
        ISEC2(18) = 0
        ISEC2(19) = 0
C
        ZSEC3(2) = MISSING_VALUE
        ISEC3(2) = NINT(ZSEC3(2))
C
        ISEC4( 1) = 1
C
        CALL GRIBEX(ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     X              USEC4,IPUNP,INBUFF,ILENB,IWORD,'C',IRET)
        IF( IRET.GT.0 ) THEN
          WRITE(*,*) 'GRIBEX return code = ',IRET
          STOP 'GRIBEX error on packing'
        ENDIF
C
        LENOUT = ISEC0(1)
        CALL PBWRITE(FILE2, INBUFF, LENOUT, IRET)
        IF( IRET.LT.LENOUT ) THEN
          WRITE(*, *) 'Return code from PBWRITE = ',IRET
          STOP 'Fault in PBWRITE'
        ENDIF
C
      ENDDO
C
C     Go for next input GRIB field
C
      GOTO 150
C
C     -----------------------------------------------------------------|
C     Problem reports
C     -----------------------------------------------------------------|
C
  980 STOP 'READ error for lat/long file'
C
  990 STOP 'OPEN error for lat/long file'
C
9000  FORMAT(2(F8.2,1X))
9001  FORMAT(1x,i5,2x,2(F8.2,1X),f8.2)
      END
