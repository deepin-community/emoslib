C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION ESHSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS)
C
C---->
C**** ESHSEC2P
C
C     PURPOSE
C     _______
C
C     Encodes GRIB section 2 values for spherical harmonic fields.
C
C     INTERFACE
C     _________
C
C     IRET = ESHSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS)
C
C     Input parameters
C     ________________
C
C     KGRIB   - GRIB product built so far (upto octet 6). 
C     KLENG   - Length of KGRIB.
C     INSPT   - Bit position of GRIB product built so far.
C     KSEC0   - GRIB section 0 description.
C     KSEC2   - GRIB section 2 description.
C     IBITS   - Number of bits per element in KGRIB.
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
      INTEGER JP16SET
      PARAMETER ( JP16SET = 2**16 - 1 ) 
C                            ^---> 65535    =   FFFF(hex)
C
C     Function arguments
C
      INTEGER KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS
      DIMENSION KGRIB(*),KSEC0(*),KSEC2(*)
C
C     Local variables
C
      INTEGER IRET, ILOOP
C
C     _______________________________________________________
C
C     Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      ESHSEC2 = 0
C
C     _______________________________________________________
C
C     Section 2.    Pack section 2 octets.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Octets 7 - 8   : J pentagonal resolution parameter.
C     Octets 9 - 10  : K pentagonal resolution parameter.
C     Octets 11 - 12 : M pentagonal resolution parameter.
C     Three 16 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(2),3,IBITS, 16,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESHSEC2 = 1
        WRITE(GRPRSM,*) 'ESHSEC2: Error inserting/extracting'
        WRITE(GRPRSM,*)
     X    'ESHSEC2: J,K,M pentagonal resolution parameters.'
        WRITE(GRPRSM,*) 'ESHSEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octet 13 : Representation type.
C     Octet 14 : Representation mode.
C     Two 8 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(5),2,IBITS, 8,'C',IRET)
      IF( IRET.NE.0 ) THEN
        ESHSEC2 = 1
        WRITE(GRPRSM,*) 'ESHSEC2: Error inserting/extracting'
        WRITE(GRPRSM,*) 'ESHSEC2: representation type or mode.'
        WRITE(GRPRSM,*) 'ESHSEC2: Return code = ', IRET
        GO TO 900
      ENDIF
C
C
C     Octets 15 - 32 : Reserved.
C     Nine 16 bit fields.
C
C     Fill reserved octets with 0.
C
      DO ILOOP = 1,9
        CALL INXBIT(KGRIB,KLENG,INSPT, 0, 1,IBITS, 16,'C',IRET)
        IF( IRET.NE.0 ) THEN
          ESHSEC2 = 1
          WRITE(GRPRSM,*) 'ESHSEC2: Error inserting dummy zero.'
          WRITE(GRPRSM,*) 'ESHSEC2: Return code = ', IRET
          GO TO 900
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
