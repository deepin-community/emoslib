C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION DLASEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS)
C
C---->
C**** DLASEC2P
C
C     PURPOSE
C     _______
C
C     Decodes GRIB section 2 values for lambert conformal fields.
C
C     INTERFACE
C     _________
C
C     IRET = DLASEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS)
C
C     Input parameters
C     ________________
C
C     KGRIB   - GRIB product unpacked so far (upto octet 6). 
C     KLENG   - Length of KGRIB.
C     INSPT   - Bit position of GRIB product unpacked so far.
C     KSEC0   - GRIB section 0 description.
C     KSEC2   - GRIB section 2 description.
C     IBITS   - Number of bits per element in KGRIB.
C     
C
C     Output parameters
C     ________________
C
C     KSEC2   - Updated GRIB section 2 description.
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
C     Unpacks values given in KGRIB into KSEC2 and updates bit
C     pointer INSPT.
C
C     Externals
C     _________
C
C     DSGNBT  - Get the sign bit and adjust value to +/-.
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
C     Function arguments
C
      INTEGER KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS
      DIMENSION KGRIB(*),KSEC0(*),KSEC2(*)
C
C     Local variables
C
      INTEGER ILALO(2), IRET, IRESOL
C
C     _______________________________________________________
C
C     Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      DLASEC2 = 0
C
C     _______________________________________________________
C
C     Section 2.    Unpack section 2 octets.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Octets 7 - 8  : Ni - number of points along X-axis.
C     Octets 9 - 10 : Nj - number of points along Y-axis.
C     Two 16 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(2),2,IBITS, 16,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*) 'GRIBEX: number of points along X or Y axis.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 11 - 13 : La1 - latitude of first grid point.
C     Octets 14 - 16 : Lo1 - longitude of first grid point.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*)
     X    'GRIBEX: latitude or longitude of first grid pt.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     If sign bit is 1, value is negative.
C 
      CALL DSGNBT( KSEC2(4), ILALO(1), 24, IRET)
      CALL DSGNBT( KSEC2(5), ILALO(2), 24, IRET)
C
C     Octet 17 : Resolution and components flag.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,IRESOL,1,IBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*) 'GRIBEX: components flag.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     If all flag fields are already set to 0, bypass.
C 
      IF( IRESOL.EQ.0 ) GOTO 515
C 
C     Resolution flag is not applicable.
C 
      IF( IRESOL.GE.128) THEN
        KSEC2(6) = 128
        IRESOL   = IRESOL - 128
      ENDIF
C 
C     Set earth flag.
C 
      IF( IRESOL.GE.64) THEN
        KSEC2(18) = 64
        IRESOL    = IRESOL - 64
      ENDIF
C 
C     Set components flag.
C
      KSEC2(19) = IRESOL
C 
  515 CONTINUE
C
C     Octets 18 - 20 : LoV - orientation of the grid.
C     One 24 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),1,IBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*)
     X    'GRIBEX: latitude or longitude of last grid point.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     If sign bit is 1, value is negative.
C 
      CALL DSGNBT( KSEC2(7), ILALO(1), 24, IRET)
C
C     Octets 21 - 23 : Dx - X direction grid length.
C     Octets 24 - 26 : Dy - Y direction grid length.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(9),2,IBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*) 'GRIBEX: X or Y axis grid length.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octet 27 : Projection centre flag.
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(13),1,IBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*) 'GRIBEX: Projection centre flag.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octet 28 : Scanning mode flags.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(11),1,IBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*) 'GRIBEX: scanning mode flags.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Fix-up for flag which was different in Experimental
C            Edition.
C
      IF( KSEC0(2).EQ.-1.AND.KSEC2(11).EQ.1) KSEC2(11) = 0
C
C     Octets 29 - 31 : Latin 1.
C     Octets 32 - 34 : Latin 2.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*) 'GRIBEX: Latin1 or Latin2 of secants points.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     If sign bit is 1, value is negative.
C
      CALL DSGNBT( KSEC2(14), ILALO(1), 24, IRET)
      CALL DSGNBT( KSEC2(15), ILALO(2), 24, IRET)
C
C     Octets 35 - 37 : Latitude of the southern pole.
C     Octets 38 - 40 : Longitude of the southern pole.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DLASEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error extracting'
        WRITE(GRPRSM,*) 'GRIBEX: latitude/longitude of southern pole.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     If sign bit is 1, value is negative.
C 
      CALL DSGNBT( KSEC2(20), ILALO(1), 24, IRET)
      CALL DSGNBT( KSEC2(21), ILALO(2), 24, IRET)
C
C     Octets 41 - 42 : Reserved.
C     One 16 bit fields.
C
C     Update bit pointer.
C 
      INSPT = INSPT + 16
C
C     _______________________________________________________
C
C     Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
      RETURN
      END
