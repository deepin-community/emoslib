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
C---->
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
C     File of GRIB coded data in "datafile".
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
C     Comments.
C     ---------
C
C     GRIBEX provides a number of packing/unpacking options.
C     See documentation for details.
C
C
C     -----------------------------------------------------------------
C----<
C
C     Arrays are dimensioned to accommodate T213/N160 data volumes.
C
      INTEGER JPACK
      PARAMETER (JPACK=70000)
      INTEGER ISEC0, ISEC1, ISEC2, ISEC3, ISEC4
C
C     Array for integer parameters from section 0 of GRIB message.
C
      DIMENSION ISEC0(2)
C
C     Array for integer parameters from section 1 of GRIB message.
C
      DIMENSION ISEC1(1024)
C
C     Array for integer parameters from section 2 of GRIB message.
C
      DIMENSION ISEC2(1024)
C
C     Array for integer parameters from section 3 of GRIB message.
C
      DIMENSION ISEC3(2)
C
C     Array for integer parameters from section 4 of GRIB message.
C
      DIMENSION ISEC4(512)
C
      REAL ZSEC2, ZSEC3, ZSEC4
C
C     Array for real parameters from section 2 of GRIB message.
C
      DIMENSION ZSEC2(512)
C
C     Array for real parameters from section 3 of GRIB message.
C
      DIMENSION ZSEC3(2)
C
C     Array for real parameters from section 4 of GRIB message.
C     This is the binary data section and the array to hold the unpacked
C     data may need to be a number of times as long as that for the
C     packed data. Here it is set arbitrarily to four time longer.
C
      DIMENSION ZSEC4(JPACK*2)
C
C     Array to read in packed data.
C
      INTEGER INBUFF
      DIMENSION INBUFF(JPACK)
C
      INTEGER FILE, JCOUNT, NUMERR, IRET, IPUNP, IWORD
      INTEGER LOOP, LENOUT, ILENB
C
C     Clear counters.
C
      NUMERR = 0
      JCOUNT = 0
C
C     Open the data file
C
      CALL PBOPEN( FILE, "datafile", "R", IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*, *) 'Return code from PBOPEN = ',IRET
        CALL PBCLOSE(FILE, IRET)
        STOP 'Fault in PBOPEN'
      ENDIF
      ILENB = ILENB*4
C
C
C     Loop through GRIB products in file.
C
   50 CONTINUE
C
        JCOUNT = JCOUNT + 1
C
C       Read packed field into INBUFF.
C
        CALL PBSIZE(FILE, LENOUT)
        WRITE(*,*) 'Next GRIB field size = ', lenout
C
        ILENB = JPACK * 4
        CALL PBGRIB( FILE, INBUFF, ILENB, LENOUT, IRET )
        IF( IRET.LT.0 ) THEN
          WRITE(*, *) 'Return code from PBGRIB = ',IRET
          IF( IRET.EQ.-1 ) THEN
            WRITE(*, *) 'End of file. Number of products = ',(JCOUNT-1)
            CALL PBCLOSE(FILE, IRET)
            WRITE(*,*) 'Number of decoding errors = ',NUMERR
            STOP 'EOF'
          ELSE
            WRITE(*, *)
     X        'Return code = ',IRET,' after ',JCOUNT,' products.'
            CALL PBCLOSE(FILE, IRET)
            STOP 'Fault in PBGRIB'
          ENDIF
        ENDIF
        WRITE(*, *) 'Return length from PBGRIB = ',lenout
C
C       Decode the GRIB product.
C
        IRET = 1
        ILENB = JPACK
        IPUNP = JPACK * 4
        CALL GRIBEX(ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     X              ZSEC4,IPUNP,INBUFF,ILENB,IWORD,'D',IRET)
C
C       Check the return code.
c
        WRITE(*,*) 'GRIBEX return code = ',IRET
        IF( IRET.EQ.-6 ) WRITE(*,*) 'Pseudo-grib data found.'
C
C       Update error count if necessary.
C
        IF( IRET.GT.0 ) THEN
          NUMERR = NUMERR + 1
          GOTO 50
        ENDIF
C
C       Print section 0, 1, 2 and 3 (if present) and 4.
C       Section 1 is the product definition section.
C       Section 2 is the grid definition section.
C       Section 3 is the bit-map section.
C       Section 4 is the data section.
C
        CALL GRPRS0(ISEC0)
        CALL GRPRS1(ISEC0,ISEC1)
C
        IF( (ISEC1(5).EQ.0).OR.(ISEC1(5).EQ.64) ) THEN
          WRITE(*,*) 'No section 2 in GRIB message.'
        ELSE
          CALL GRPRS2(ISEC0,ISEC2,ZSEC2)
        ENDIF
C
        IF( (ISEC1(5).EQ.0).OR.(ISEC1(5).EQ.128) ) THEN
          WRITE(*,*) 'No section 3 in GRIB message.'
        ELSE
          CALL GRPRS3(ISEC0,ISEC3,ZSEC3)
        ENDIF
C
        CALL GRPRS4(ISEC0,ISEC4,ZSEC4)
C
C       Print some vales
        DO LOOP = 1,ISEC4(1)
          WRITE(*,*) 'zsec4(', loop, ') = ', zsec4(loop)
        ENDDO
C
      GOTO 50
C
      END
