C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION ESVSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS,ILEN2)
C
C---->
C**** ESVSEC2
C
C     PURPOSE
C     _______
C
C     Encodes GRIB section 2 values for space view perspective or
C     orthographic grid fields.
C
C     INTERFACE
C     _________
C
C     IRET = ESVSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS)
C
C     Input parameters
C     ________________
C
C     KGRIB   - GRIB product built so far (upto octet 6). 
C     KLENG   - Length of KGRIB.
C     INSPT   - Bit position of GRIB product built so far.
C     KSEC2   - GRIB section 2 description.
C     IBITS   - Number of bits per element in KGRIB.
C     ILEN2   - Length in bytes of section 2
C     
C
C     Output parameters
C     ________________
C
C     KGRIB   - Updated GRIB product to end of section 2.
C     INSPT   - Updated bit position of GRIB product built so far.
C     IRET    - Function return status code, 0 = OK
C
C     Common block usage
C     __________________
C
C     None.
C
C     Method
C     ______
C
C     Packs values given in KSEC2 into KGRIB and updates bit
C     pointer INSPT.
C
C     Externals
C     _________
C
C     CSGNBT  - Set the sign bit.
C     INXBIT  - Insert bit data.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers       ECMWF     February 1999
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
C     _______________________________________________________
C
C     Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
C
C
C     Function arguments
C
      INTEGER KGRIB,KLENG,INSPT,KSEC2,IBITS,ILEN2
      DIMENSION KGRIB(*),KSEC2(*)
C
C     Local variables
C
      INTEGER ILALO(2), IRET, IRESOL, IZERO, LOOP
      DATA IZERO/0/
C
C     _______________________________________________________
C
C     Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      ESVSEC2 = 0
C
C     _______________________________________________________
C
C     Section 2.    Pack section 2 octets.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Octets 7 - 8  : Nx - number of points along X-axis.
C     Octets 9 - 10 : Ny - number of points along Y-axis.
C     Two 16 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(2),2,IBITS, 16,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*) 'ESVSEC2: number of points along X or Y axis.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 11 - 13 : Lap - latitude of sub-satellite point.
C     Octets 14 - 16 : Lop - longitude of sub-satellite point.
C     Two 24 bit fields.
C
C     Set sign bit to 1, if value is negative.
C
      CALL CSGNBT( ILALO(1), KSEC2(4), 24, IRET)
      CALL CSGNBT( ILALO(2), KSEC2(5), 24, IRET)
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*)
     X    'ESVSEC2: latitude/longitude of sub-satellite pt.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octet 17 : Resolution and components flag.
C     One 8 bit field.
C
C     Resolution flag ( KSEC2(6) ) is not applicable.
C
      IRESOL = KSEC2(18)+KSEC2(19)
C
      CALL INXBIT(KGRIB,KLENG,INSPT,IRESOL,1,IBITS, 8,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*) 'ESVSEC2: components flag.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 18 - 20 : dx Apparent diameter of earth in grid
C                         lengths in x direction.
C     Octets 21 - 23 : dy Apparent diameter of earth in grid
C                         lengths in y direction.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(7),2,IBITS, 24,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*) 'ESVSEC2: number of points along X or Y axis.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 24 - 25 : Xp X-coordinate of sub-satellite point.
C     Octets 26 - 27 : Yp Y-coordinate of sub-satellite point.
C     Two 16 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(9),2,IBITS, 16,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*)
     X    'ESVSEC2: X or Y coordinate of sub-satellite point.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octet 28 : Scanning mode flags.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(11),1,IBITS, 8,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*) 'ESVSEC2: scanning mode flags.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 29 - 31 : The orientation of the grid.
C     Octets 32 - 34 : nr the altitude of the camera.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(13),2,IBITS, 24,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*)
     X    'ESVSEC2: orientation of the grid or camera angle.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 35 - 36 : Xo - X coordinate of origin of sector image.
C     Octets 37 - 38 : Yo - Y coordinate of origin of sector image.
C     Two 16 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(15),2,IBITS, 16,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESVSEC2 = 1
        WRITE(GRPRSM,*) 'ESVSEC2: Error inserting'
        WRITE(GRPRSM,*)
     X    'ESVSEC2: X or Y coordinates of origin of sector.'
        WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 39 - 40 : Reserved (ECMWF).
C     Octets 39 - 44 : Reserved (GRIB specification).
C
C     Fill reserved octets with 0.
C
      DO LOOP = 1, (ILEN2 - 38)
        CALL INXBIT(KGRIB,KLENG,INSPT, IZERO, 1,IBITS, 8,'C',IRET)
        IF( IRET.NE.0 ) THEN
          IRET = 810
          WRITE(GRPRSM,*) 'ESVSEC2: Error inserting dummy zero.'
          WRITE(GRPRSM,*) 'ESVSEC2: Return code = ', IRET
          GOTO 900
        ENDIF
      ENDDO
C
C     _______________________________________________________
C
C     Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
      RETURN
      END
