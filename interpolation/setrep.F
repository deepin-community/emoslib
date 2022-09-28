C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
#define INT_MISSING -2147483647

      INTEGER FUNCTION SETREP(OUTREP)
C
C---->
C*****SEREP*
C
C     PURPOSE
C     -------
C     reset LNOREPR to original value
C     reset some other values in common block
C     LNOREPR      - Flag to say whether or not user specified NOREPR 
C
C     Function argument
C
      INTEGER OUTREP

#include "parim.h"
#include "nifld.common"
#include "nofld.common"

       SETREP = 0
      IF(OUTREP.EQ.1) THEN
         LNOREPR = .FALSE.
        IF(.NOT.LNOAREA) THEN
           NOAREA(1) = 0
           NOAREA(2) = 0
           NOAREA(3) = 0
           NOAREA(4) = 0
         CALL INTLOG(JP_DEBUG,'SETREP: AREA has been reseted'
     X   ,JPQUIET)
        ENDIF
         CALL INTLOG(JP_DEBUG,'SETREP: LNOREPR has been reseted'
     X   ,JPQUIET)
      ENDIF
       NIWE = 0
       NINS = 0
         CALL INTLOG(JP_DEBUG,'SETREP: NIWE,NINS has been reseted'
     X   ,JPQUIET)
cs       NOACC = 0
cs         CALL INTLOG(JP_DEBUG,'SETREP: NOACC has been reseted'
cs     X   ,JPQUIET)
       NOLEVEL = 0
         CALL INTLOG(JP_DEBUG,'SETREP: NOLEVEL has been reseted'
     X   ,JPQUIET)
       LOCEAN = .FALSE.
         CALL INTLOG(JP_DEBUG,'SETREP: LOCEAN has been reseted'
     X   ,JPQUIET)
       LPREC = .FALSE.
         CALL INTLOG(JP_DEBUG,'SETREP: LPREC has been reseted'
     X   ,JPQUIET)
       LIMISSA = .FALSE.
       LIMISSV = .FALSE.
       RMISSGV   = REAL(INT_MISSING)
         CALL INTLOG(JP_DEBUG,'SETREP: LIMISSV has been reseted'
     X   ,JPQUIET)
cs       LNOROTA = .FALSE.
cs         CALL INTLOG(JP_DEBUG,'SETREP: LNOROTA has been reseted'
cs     X   ,JPQUIET)
C
      RETURN
      END
