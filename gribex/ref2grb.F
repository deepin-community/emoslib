C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION REF2GRB(PREF, KEXP, KMANT, KBITS)
C
C---->
C**** REF2GRB
C
C     Purpose.
C     --------
C
C     Code and check reference value in IBM format
C
C
C**   Interface.
C     ----------
C
C     CALL REF2GRB(PREF, KEXP, KMANT, KBITS)
C
C
C     Input Parameters.
C     -----------------
C
C     PREF       - Reference value
C     KBITS      - Number of bits per computer word.
C
C
C     Output Parameters.
C     -----------------
C
C     KEXP       - 8-bit exponent
C     KMANT      - 24-bit mantissa
C
C     Function returns 0 if no problems encountered, otherwise -1.
C
C
C     Method.
C     -------
C
C     Codes in IBM format, then decides to ensure that reference 
C     value used for packing is not different from that stored
C     because of packing differences.
C
C
C     Externals.
C     ----------
C
C     CONFP3    - Encode into IBM floating point format.
C     DECFP     - Decode from IBM floating point format.
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     --------
C
C     None.
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      17:05:94
C
C
C     Modifications.
C     _____________
C
C     Change to function return status code.
C     J.D.Chambers     ECMWF      August, 2000
C
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
#include "grbcom.h"
C
C     Subroutine arguments
C
      REAL PREF
      INTEGER KEXP, KMANT, KBITS
C
C     Local variables
C
      INTEGER ITRND
      REAL ZTEMP
      LOGICAL LDEBUG
C
C     -----------------------------------------------------------------|
C*    Section 1. Convert to and from IBM format.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      REF2GRB = 0
C
      LDEBUG = (NDBG.EQ.1)
C 
C     Convert floating point reference value to IBM representation.
C
      ITRND = 1
      ZTEMP = PREF
      CALL CONFP3 (PREF,KEXP,KMANT,KBITS,ITRND)
C 
C     Set reference value to that actually stored in the GRIB code.
C
      CALL DECFP2 (PREF,KEXP,KMANT)
C 
C     If the nearest number which can be represented in
C     GRIB format is greater than the reference value,
C     find the nearest number in GRIB format lower
C     than the reference value.
C 
      IF (ZTEMP.LT.PREF) THEN
C 
C       Convert floating point to GRIB representation
C       using truncation to ensure that the converted
C       number is smaller than the original one.
C
        ITRND = 0
        PREF  = ZTEMP
        CALL CONFP3 (PREF,KEXP,KMANT,KBITS,ITRND)
C 
C       Set reference value to that stored in the GRIB code.
C
        CALL DECFP2 (PREF,KEXP,KMANT)
C 
        IF (ZTEMP.LT.PREF) THEN
          IF( LDEBUG ) THEN
            WRITE(GRPRSM,*) 'REF2GRB: Reference value error.'
            WRITE(GRPRSM,*) 'REF2GRB: Notify Met.Applications Section.'
            WRITE(GRPRSM,*) 'REF2GRB: ZTEMP = ', ZTEMP
#ifdef REAL_8
            WRITE(GRPRSM,'(1X,Z16)') ZTEMP
#else
            WRITE(GRPRSM,'(1X,Z8)') ZTEMP
#endif
            WRITE(GRPRSM,*) 'REF2GRB: PREF = ', PREF
#ifdef REAL_8
            WRITE(GRPRSM,'(1X,Z16)') PREF
#else
            WRITE(GRPRSM,'(1X,Z8)') PREF
#endif
            REF2GRB = -1
          ENDIF
C
          PREF = ZTEMP
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9. Return.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
