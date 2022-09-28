C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION EMESEC2(KGRIB,KLENG,KNSPT,KSEC0,KSEC2,KBITS,
     X                         OQUASI)
C
C---->
C**** EMESEC2
C
C     PURPOSE
C     _______
C
C     Encodes GRIB section 2 values for lat/long grid fields.
C
C     INTERFACE
C     _________
C
C     IRET = EMESEC2(KGRIB,KLENG,KNSPT,KSEC0,KSEC2,KBITS,OQUASI)
C
C     Input parameters
C     ________________
C
C     KGRIB   - GRIB product built so far (upto octet 6).
C     KLENG   - Length of KGRIB.
C     KNSPT   - Bit position of GRIB product built so far.
C     KSEC0   - GRIB section 0 description.
C     KSEC2   - GRIB section 2 description.
C     KBITS   - Number of bits per element in KGRIB.
C     OQUASI  - .TRUE. if gaussian grid is quasi-regular (reduced).
C
C
C     Output parameters
C     ________________
C
C     KGRIB   - Updated GRIB product to end of section 2.
C     KNSPT   - Updated bit position of GRIB product built so far.
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
C     Packs values given in KSEC2 into KGRIB and updates bit
C     pointer KNSPT.
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
C     J. Clochard, Meteo France, for ECMWF - October 2000.
C     (derived from ELLSEC2)
C
C     MODIFICATIONS
C     _____________
C
C     J. Clochard, June 2008.
C     Use of GRPRSM instead of standard output in error/debug printouts.
C     Doctorize all dummy-arguments (K for integers, O for logicals).
C     Return-code set to 810 for errors on final padding.
C     Do not enforce Di/j increments to missing values when resolution
C     flag is off.
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
      INTEGER KGRIB,KLENG,KNSPT,KSEC0,KSEC2,KBITS, JLOOP
      DIMENSION KGRIB(*),KSEC0(*),KSEC2(*)
      LOGICAL OQUASI
C
C     Global variables
C
#include "common/grprs.h"
C
C     Local variables
C
      INTEGER ILALO(2), IRET, IRESOL, IZERO
      SAVE IZERO
      DATA IZERO /0/
C
C     _______________________________________________________
C
C     Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      EMESEC2 = 0
C
C     _______________________________________________________
C
C     Section 2.    Pack section 2 octets.
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
C     Set to all 1 bits.
C
      IF( OQUASI ) KSEC2(2) = JP16SET
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC2(2),2,KBITS, 16,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting number of pts'
        WRITE(GRPRSM,*) 'EMESEC2: along parallel or meridian.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octets 11 - 13 : La1 - latitude of first grid point.
C     Octets 14 - 16 : Lo1 - longitude of first grid point.
C     Two 24 bit fields.
C
C     Set sign bit to 1, if value is negative.
C
      CALL CSGNBT( ILALO(1), KSEC2(4), 24, IRET)
      CALL CSGNBT( ILALO(2), KSEC2(5), 24, IRET)
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO,2,KBITS, 24,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting'
        WRITE(GRPRSM,*)
     X    'EMESEC2: latitude or longitude of first grid pt.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octet 17 : Resolution and components flag.
C     One 8 bit field.
C
      IRESOL = KSEC2(6)+KSEC2(18)+KSEC2(19)
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IRESOL,1,KBITS, 8,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting'
        WRITE(GRPRSM,*) 'EMESEC2: components flag.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octets 18 - 20 : La2 - latitude of last grid point.
C     Octets 21 - 23 : Lo2 - longitude of last grid point.
C     Two 24 bit fields.
C
C     Set sign bit to 1, if value is negative.
C
      CALL CSGNBT( ILALO(1), KSEC2(7), 24, IRET)
      CALL CSGNBT( ILALO(2), KSEC2(8), 24, IRET)
C
C     Insert / extract fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO,2,KBITS, 24,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting'
        WRITE(GRPRSM,*) 'EMESEC2: latitude/longitude of last grid point'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octets 24 - 26 : Latin - latitude(s) at which the Mercator
C                      projection cylinder intersects the Earth.
C     One 24 bit field.
C
C     Set sign bit to 1, if value is negative.
C
      CALL CSGNBT( ILALO(1), KSEC2(9), 24, IRET)
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO,1,KBITS, 24,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting'
        WRITE(GRPRSM,*) 'EMESEC2: latitude of intersection.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octet 27 : Reserved. Set to zero.
C     One 8 bit field.
C
Ce    Set bits to 0.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT, IZERO, 1,KBITS, 8,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting dummy zero.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octet 28 : Scanning mode flags.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC2(11),1,KBITS, 8,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting'
        WRITE(GRPRSM,*) 'EMESEC2: scanning mode flags.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Fix-up for flag which was different in Experimental Edition.
C
      IF( KSEC0(2).EQ.-1.AND.KSEC2(11).EQ.1) KSEC2(11) = 0
C
C     Octets 29 - 31 : Di - i direction increment.
C     One 24 bit field.
C
C     For quasi-regular grids all Di bits are set to 1, as
C     the increment is different on different parallels.
C
C     If field not given, set to all bits to 1.
C
Ce    Set to all 1 bits.
      IF( OQUASI) KSEC2(13) = JP24SET
Ce
Ce    If increments not given, set all bits to 1.
Ce    IF( KSEC2(6).EQ.0) KSEC2(13) = JP24SET
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC2(13),1,KBITS, 24,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting'
        WRITE(GRPRSM,*) 'EMESEC2: i direction increment.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octets 32 - 34 : Dj - j direction increment.
C     One 24 bit field.
C
C     If field not given, set to all bits to 1.
C
C     IF( KSEC2(6).EQ.0) KSEC2(14) = JP24SET
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC2(14),1,KBITS, 24,'C',IRET)
      IF( IRET.NE.0) THEN
        EMESEC2 = 1
        WRITE(GRPRSM,*) 'EMESEC2: Error inserting'
        WRITE(GRPRSM,*) 'EMESEC2: j direction increment.'
        WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octets 35 - 42 : Reserved.
C     Eight 8 bit fields.
C
Ce    Set bits to 0.
C
      DO 221 JLOOP = 1, 8
C
        CALL INXBIT(KGRIB,KLENG,KNSPT, IZERO, 1,KBITS, 8,'C',IRET)
        IF( IRET.NE.0) THEN
          IRET = 810
          EMESEC2 = IRET
          WRITE(GRPRSM,*) 'EMESEC2: Error inserting dummy zero.'
          WRITE(GRPRSM,*) 'EMESEC2: Return code = ', IRET
          GO TO 900
        ENDIF
C
  221 CONTINUE
C
C     _______________________________________________________
C
C     Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
      RETURN
      END
