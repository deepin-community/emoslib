C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION DPSSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS)
C
C---->
C**** DPSSEC2P
C
C     PURPOSE
C     _______
C
C     Decodes GRIB section 2 values for polar stereographic fields.
C
C     INTERFACE
C     _________
C
C     IRET = DPSSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS)
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
C     J.D.Chambers       ECMWF     March 1999
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
      DPSSEC2 = 0
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
        DPSSEC2 = 1
        WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
        WRITE(GRPRSM,*) 'DPSSEC2: number of points along X or Y axis.'
        WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 11 - 13 : La1 - latitude of first grid point.
C     Octets 14 - 16 : Lo1 - longitude of first grid point.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DPSSEC2 = 1
        WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
        WRITE(GRPRSM,*)
     X    'DPSSEC2: latitude or longitude of first grid pt.'
        WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
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
C     Resolution flag ( KSEC2(6) ) is not applicable.
C
      KSEC2(6) = 0
C
      CALL INXBIT(KGRIB,KLENG,INSPT,IRESOL,1,IBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DPSSEC2 = 1
        WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
        WRITE(GRPRSM,*) 'DPSSEC2: components flag.'
        WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     All flag fields are already set to 0, so
C 
      IF( IRESOL.EQ.0 ) GOTO 213
C 
C     Fix up for flag which was different in Experimental edition.
C 
      IF( KSEC0(2).EQ.-1.AND.(IRESOL.EQ.1.OR.IRESOL.EQ.3)) IRESOL = 128
C 
C     Resolution flag is not applicable.
C
      IF( IRESOL.GE.128 ) IRESOL = IRESOL - 128
C 
C     Set earth flag.
C
      IF( IRESOL.GE.64 ) THEN
        KSEC2(18) = 64
        IRESOL    = IRESOL - 64
      ENDIF
C 
C     Set components flag.
C
      KSEC2(19) = IRESOL
C 
  213 CONTINUE
C
C     CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(6),1,IBITS, 8,'D',IRET)
C     IF( IRET.NE.0 ) THEN
C       DPSSEC2 = 1
C       WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
C       WRITE(GRPRSM,*) 'DPSSEC2: components flag.'
C       WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
C       GOTO 900
C     ENDIF
C
C     Octets 18 - 20 : LoV - orientation of the grid.
C     One 24 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),1,IBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DPSSEC2 = 1
        WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
        WRITE(GRPRSM,*)
     X    'DPSSEC2: latitude or longitude of last grid point.'
        WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
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
        DPSSEC2 = 1
        WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
        WRITE(GRPRSM,*) 'DPSSEC2: X or Y axis grid length.'
        WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octet 27 : Projection centre flag.
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(13),1,IBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DPSSEC2 = 1
        WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
        WRITE(GRPRSM,*) 'DPSSEC2: Projection centre flag.'
        WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octet 28 : Scanning mode flags.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(11),1,IBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DPSSEC2 = 1
        WRITE(GRPRSM,*) 'DPSSEC2: Error extracting'
        WRITE(GRPRSM,*) 'DPSSEC2: scanning mode flags.'
        WRITE(GRPRSM,*) 'DPSSEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Fix-up for flag which was different in Experimental Edition.
C
      IF( KSEC0(2).EQ.-1.AND.KSEC2(11).EQ.1) KSEC2(11) = 0
C
C     Octets 29 - 32 : Reserved.
C     Two 16 bit fields.
C
C     Update bit pointer.
C
      INSPT = INSPT + 32
C
C     _______________________________________________________
C
C     Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
      RETURN
      END
