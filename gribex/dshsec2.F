C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION DSHSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS)
C
C---->
C**** DSHSEC2P
C
C     PURPOSE
C     _______
C
C     Decodes GRIB section 2 values for spherical harmonic fields.
C
C     INTERFACE
C     _________
C
C     IRET = DSHSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS)
C
C     Input parameters
C     ________________
C
C     KGRIB   - GRIB product unpacked so far (upto octet 6). 
C     KLENG   - Length of KGRIB.
C     INSPT   - Bit position of GRIB product unpacked so far.
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
C     Parameters
C
      INTEGER JP16SET
      PARAMETER ( JP16SET = 2**16 - 1 ) 
C                            ^---> 65535    =   FFFF(hex)
C
C     Function arguments
C
      INTEGER KGRIB,KLENG,INSPT,KSEC2,IBITS
      DIMENSION KGRIB(*),KSEC2(*)
C
C     Local variables
C
      INTEGER IRET
C
C     _______________________________________________________
C
C     Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      DSHSEC2 = 0
C
C     _______________________________________________________
C
C     Section 2.    Unpack section 2 octets.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Octets 7 - 8   : J pentagonal resolution parameter.
C     Octets 9 - 10  : K pentagonal resolution parameter.
C     Octets 11 - 12 : M pentagonal resolution parameter.
C     Three 16 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(2),3,IBITS, 16,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DSHSEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
        WRITE(GRPRSM,*)
     X    'GRIBEX: J,K,M pentagonal resolution parameters.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GO TO 900
      ENDIF
C
C     Octet 13 : Representation type.
C     Octet 14 : Representation mode.
C     Two 8 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(5),2,IBITS, 8,'D',IRET)
      IF( IRET.NE.0 ) THEN
        DSHSEC2 = 1
        WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
        WRITE(GRPRSM,*) 'GRIBEX: representation type or mode.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', IRET
        GO TO 900
      ENDIF
C
C
C     Octets 15 - 32 : Reserved.
C     Nine 16 bit fields.
C
C     Move pointer past reserved octets.
      INSPT = INSPT + 144
C
C     _______________________________________________________
C
C     Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
      RETURN
      END
