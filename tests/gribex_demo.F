C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

       PROGRAM GRDEMO
C
      IMPLICIT NONE
C
C**** GRDEMO - Program to demonstrate use of GRIBEX routine.
C
C     Purpose.
C     --------
C
C     Demonstrates use of GRIBEX routine to unpack
C     GRIB coded data.
C
C**   Interface.
C     ----------
C
C     Method.
C     -------
C
C     Prints sections 0, 1, 2, 3 and 4 of GRIB message.
C
C     Externals.
C     ----------
C
C     GRIBEX
C     GRPRS0
C     GRPRS1
C     GRPRS2
C     GRPRS3
C     GRPRS4
C     PBOPEN
C     PBGRIB
C     PBCLOSE
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes for GRIB definition.
C     WMO Publication No. 9, Volume B, for grid catalogue numbers.
C
C     Modifications
C     -------------
C
C     K. Fielding     ECMWF     Dec 2000
C     Set missing value for bit controlled fields.
C
C     Comments.
C     ---------
C
C     GRIBEX provides a number of packing/unpacking options.
C     See documentation for details.
C
C
C     -----------------------------------------------------------------
C
C     Arrays are dimensioned to accommodate T213/N160 data volumes.
C     JPBYTE is number of bytes per REAL
C
      INTEGER JPACK, JPBYTE
C
      PARAMETER (JPACK = 280000)
      PARAMETER (JPBYTE = 4)
C
C     Array for integer parameters from section 0 of GRIB message.
C
      INTEGER ISEC0 (2)
C
C     Array for integer parameters from section 1 of GRIB message.
C
      INTEGER ISEC1 (1024)
C
C     Array for integer parameters from section 2 of GRIB message.
C
      INTEGER ISEC2 (1024)
C
C     Array for integer parameters from section 3 of GRIB message.
C
      INTEGER ISEC3 (2)
C
C     Array for integer parameters from section 4 of GRIB message.
C
      INTEGER ISEC4 (512)
C
C     Array for real parameters from section 2 of GRIB message.
C
      REAL ZSEC2 (512)
C
C     Array for real parameters from section 3 of GRIB message.
C
      REAL ZSEC3 (2)
C
C     Array for real parameters from section 4 of GRIB message.
C     This is the binary data section and the array to hold
C     the unpacked data may need to be 4 times as long as that
C     for the packed data.
C
      REAL ZSEC4 (JPACK * 4)
C
C     Array to read in packed data.
C
      INTEGER INBUFF (JPACK)
C
C     GRIBEX routine has a number of encoding/decoding options.
C
      CHARACTER*256 YDATAFILE
      CHARACTER*2 INPUT
      CHARACTER*1 YOPER
      INTEGER IFILE, NUMERR, IPBLEN, IWORD, IFILEN, IRET, IARGC, NARGS
      INTEGER JCOUNT, LENOUT
CC    INTEGER JLAT, JLONG, JSTART, JEND
CC    REAL ZFIRST, ZLAST, ZSTEP, ZLAT
CC    REAL ZMAX
C
C     Pick up file names from command line.
C
      NARGS = IARGC()
      IF( NARGS.LT.2 ) THEN
         print*,'Usage: agrdemo -i inputfile'
         CALL EXIT(2)
      END IF

      CALL GETARG(1,INPUT)

      IF(INPUT.EQ.'-i') THEN
         CALL GETARG(2,YDATAFILE)
      ELSE
         print*,'Usage: agrdemo -i inputfile'
         CALL EXIT(2)
      END IF

C     Clear error counter.
C
      NUMERR = 0
C
C     Lengths of INBUFF and PSEC4
C
      IPBLEN = JPACK * JPBYTE
C
      IFILEN = INDEX(YDATAFILE,' ') - 1
C
      CALL PBOPEN (IFILE, YDATAFILE (1: IFILEN), 'R', IRET)
C
      IF ( IRET .NE. 0 ) THEN
        WRITE (*, *) ' Return code from PBOPEN = ', IRET
C       CALL PBCLOSE(IFILE, IRET)  ! (problem if PBOPEN failed)
        WRITE (*, *) 'Fault in PBOPEN'
        CALL EXIT(2)
      ENDIF
C
C     Loop through GRIB products in file.
      JCOUNT = 0
C
   50 CONTINUE
      JCOUNT = JCOUNT + 1
C
C     Read packed field into INBUFF.
      CALL PBGRIB (IFILE, INBUFF, IPBLEN, LENOUT, IRET )
C
      IF ( IRET .LT. 0 ) THEN
        WRITE (*, *) ' Return code from pbgrib = ', IRET
        IF ( IRET .EQ. -1) THEN
          WRITE  (*, *) ' End of file. Number of products =', (JCOUNT-1)
          CALL PBCLOSE (IFILE, IRET)
          WRITE (*,*) ' GRDEMO : Number of decoding errors = ', NUMERR
          STOP 'EOF'
        ELSE
          WRITE (*, *) ' kret = ',IRET,' after ', JCOUNT,' products.'
          CALL PBCLOSE (IFILE, IRET)
          WRITE (*, *) 'Fault in PBGRIB'
          CALL EXIT(2)
        ENDIF
      ENDIF
C
C     'D' function to unpack entire GRIB message.
C
      YOPER = 'D'
      WRITE (*,*) ' GRDEMO : Function code = ', YOPER
C
      IRET = 1
      LENOUT = LENOUT / JPBYTE
C
C     Set missing data values
C
      ISEC3 (2) = -99999
      ZSEC3 (2) = -99999.0
C
      CALL GRIBEX (ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X             ZSEC4, IPBLEN, INBUFF, LENOUT, IWORD, YOPER, IRET)
C
C     Check return code.
C
      WRITE (*,*) ' GRDEMO : GRIBEX return code = ', IRET
      IF (IRET .EQ. - 6) WRITE (*,*) ' GRDEMO : Pseudo-grib data found.'
      IF (IRET .GT. 0) THEN
        NUMERR = NUMERR + 1
        GO TO 50
      ENDIF
C
C     Print section 0 , 1 , 2 and 3 (if present) and 4.
C     Section 1 is the product definition section.
C     Section 2 is the grid definition section.
C     Section 3 is the bit-map section.
C     Section 4 is the data section.
C
      CALL GRPRS0 (ISEC0)
      CALL GRPRS1 (ISEC0, ISEC1)
C
      IF (ISEC1 (5) .EQ. 0 .OR. ISEC1 (5) .EQ. 64) THEN
        WRITE (*,*) ' GRDEMO : No section 2 in GRIB message.'
      ELSE
        CALL GRPRS2 (ISEC0, ISEC2, ZSEC2)
      ENDIF
C
      IF (ISEC1 (5) .EQ. 0 .OR. ISEC1 (5) .EQ. 128) THEN
        WRITE (*,*) ' GRDEMO : No section 3 in GRIB message.'
      ELSE
        CALL GRPRS3 (ISEC0, ISEC3, ZSEC3)
      ENDIF
C
      CALL GRPRS4 (ISEC0, ISEC4, ZSEC4)
C
C     Sample code to print maximum and minimum value in data.
C
C     ZMAX = ZSEC4 (1)
C     ZMIN = ZSEC4 (1)
C
C     DO JLAT = 2, ISEC4 (1)
C       ZMAX = MAX (ZMAX, ZSEC4 (JLAT) )
C       ZMIN = MIN (ZMIN, ZSEC4 (JLAT) )
C     ENDDO
C     WRITE (*, *)
C     WRITE (*, *) 'Maximum value in field is ', ZMAX
C     WRITE (*, *) 'Minimum value in field is ', ZMIN
C     WRITE (*, *)
C
C     Sample code to print all values along lines of
C     latitude for a lat/long grid only.
C
C     ZFIRST = REAL (ISEC2 (5) ) / 1000.0
C     ZLAST = REAL (ISEC2 (8) ) / 1000.0
C     ZSTEP = REAL (ISEC2 (9) ) / 1000.0
C     WRITE (*, *) 'Longitudes from ', ZFIRST, ' to ', ZLAST,
C    1  ' with stride ', ZSTEP
C
C     DO JLAT = 1, ISEC2 (3)
C       ZLAT = REAL (ISEC2 (4) - (JLAT - 1) * ISEC2 (10) ) / 1000.0
C       JSTART = (JLAT - 1) * ISEC2 (2) + 1
C       JEND = JLAT * ISEC2 (2)
C       WRITE (*, *)
C       WRITE (*, *) 'Values at latitude ', ZLAT
C       WRITE (*, *)
C       WRITE(*, '(1H , 5E16.5)' )
C    1    (ZSEC4 (JLONG), JLONG = JSTART, JEND)
C     ENDDO
C
      GO TO 50
C
      END

