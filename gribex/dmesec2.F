C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION DMESEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,KBITS,
     X                         KMISNG)
C
C---->
C**** DMESEC2
C
C     PURPOSE
C     _______
C
C     Decodes GRIB section 2 values for Mercator grid fields.
C
C     INTERFACE
C     _________
C
C     IRET = DMESEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,KBITS,KMISNG)
C
C     Input parameters
C     ________________
C
C     KGRIB   - GRIB product unpacked so far (upto octet 6). 
C     KLENG   - Length of KGRIB.
C     INSPT   - Bit position of GRIB product unpacked so far.
C     KSEC0   - GRIB section 0 description.
C     KSEC2   - GRIB section 2 description.
C     KBITS   - Number of bits per element in KGRIB.
C     KMISNG  - Missing data value to be used.
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
C     /GRPRSCM/ for GRPRSM variable.
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
C     INXBIT  - Extract bit data.
C
C
C     AUTHOR
C     ______
C
C     J. Clochard, Meteo France, for ECMWF - October 2000.
C     (derived from DLLSEC2)
C
C     MODIFICATIONS
C     _____________
C
C     J. Clochard, June 2008.
C     Use of GRPRSM instead of standard output in error/debug printouts.
C     Doctorize all dummy-arguments (K for integers, O for logicals).
C
C----<
C     _______________________________________________________
C
C     Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JP16SET, JP24SET
      PARAMETER ( JP16SET = 2**16 - 1 ) 
C                            ^---> 65535    =   FFFF(hex)
      PARAMETER ( JP24SET = 2**24 - 1 ) 
C                           ^---> 16777215 = FFFFFF(hex)
C     Function arguments
C
      INTEGER KGRIB,KLENG,INSPT,KSEC0,KSEC2,KBITS,KMISNG
      DIMENSION KGRIB(*),KSEC0(*),KSEC2(*)
C
C     Global variables
C
#include "common/grprs.h"
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
      DMESEC2 = 0
C
C     _______________________________________________________
C
C     Section 2.    Unpack section 2 octets.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Octets 7 - 8  : Ni - number of points along a parallel.
C     Octets 9 - 10 : Nj - number of points along a meridian.
C     Two 16 bit fields.
C
C     WARNING!!
C     For quasi-regular grids, it is here assumed that Ni is variable,
C     i.e. that the number of points along a parallel is variable but
C     that the number of points along a meridian is fixed.
C
C     For quasi-regular grids Ni is set to all 1 bits, as
C     the number of points is different on different parallels.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(2),2,KBITS, 16,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting number of pts '
        WRITE(GRPRSM,*) 'DMESEC2: along parallel or meridian.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Octets 11 - 13 : La1 - latitude of first grid point.
C     Octets 14 - 16 : Lo1 - longitude of first grid point.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO,2,KBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting'
        WRITE(GRPRSM,*) 
     X    'DMESEC2: latitude or longitude of first grid pt.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
      CALL DSGNBT( KSEC2(4), ILALO(1), 24, IRET)
      CALL DSGNBT( KSEC2(5), ILALO(2), 24, IRET)
C
C     Octet 17 : Resolution and components flag.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,IRESOL,1,KBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting'
        WRITE(GRPRSM,*) 'DMESEC2: resolution and components flag.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     All flag fields are already set to 0, so
C
      IF( IRESOL.EQ.0 ) GOTO 211
C
C     Fix up for flag which was different in Experimental edition.
C
      IF( KSEC0(2).EQ.-1.AND.(IRESOL.EQ.1.OR.IRESOL.EQ.3)) IRESOL = 128
C
C     Set Resolution flag.
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
  211 CONTINUE
C
C     Octets 18 - 20 : La2 - latitude of last grid point.
C     Octets 21 - 23 : Lo2 - longitude of last grid point.
C     Two 24 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO,2,KBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting'
        WRITE(GRPRSM,*)
     X    'DMESEC2: latitude or longitude of last grid point.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
      CALL DSGNBT( KSEC2(7), ILALO(1), 24, IRET)
      CALL DSGNBT( KSEC2(8), ILALO(2), 24, IRET)
C
C     Octets 24 - 26 : Latin - latitude(s) at which the Mercator
C                      projection cylinder intersects the Earth.
C     One 24 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILALO,1,KBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting'
        WRITE(GRPRSM,*) 'DMESEC2: latitude of intersection.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
      IF( ILALO(1).EQ.JP24SET) THEN
C
C     Change 1 bits to missing data value.
        KSEC2(9) = KMISNG
      ELSE
C
C     Take into account sign bit if any.
        CALL DSGNBT( KSEC2(9), ILALO(1), 24, IRET)
      ENDIF
C
C     Octet 27 : Reserved. Set to zero.
C     One 8 bit field.
C
      KSEC2(10) = 0
C
C     Update bit pointer.
      INSPT = INSPT + 8
C
C     Octet 28 : Scanning mode flags.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(11),1,KBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting'
        WRITE(GRPRSM,*) 'DMESEC2: scanning mode flags.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Fix-up for flag which was different in Experimental Edition.
      IF( KSEC0(2).EQ.-1.AND.KSEC2(11).EQ.1) KSEC2(11) = 0
C
C     Octets 29 - 31 : Di - i direction increment.
C     One 24 bit field.
C
C     For quasi-regular grids all Di bits are set to 1, as
C     the increment is different on different parallels.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(13),1,KBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting'
        WRITE(GRPRSM,*) 'DMESEC2: i direction increment.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Change 1 bits to missing data value.
C
      IF( KSEC2(13).EQ.JP24SET) KSEC2(13) = KMISNG
C
C
C     Octets 32 - 34 : Dj - j direction increment.
C     One 24 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(14),1,KBITS, 24,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DMESEC2 = 1
        WRITE(GRPRSM,*) 'DMESEC2: Error extracting'
        WRITE(GRPRSM,*) 'DMESEC2: j direction increment.'
        WRITE(GRPRSM,*) 'DMESEC2: Return code = ', IRET
        GOTO 900
      ENDIF
C
C     Change 1 bits to missing data value.
C
      IF( KSEC2(14).EQ.JP24SET) KSEC2(14) = KMISNG
C
C     Octets 35 - 42 : Reserved.
C     Four 16 bit fields.
C
C     Update bit pointer.
      INSPT = INSPT + 64
C
C     _______________________________________________________
C
C     Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
      RETURN
      END
