C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION OUTREP()
C
C---->
C*****OUTREP*
C
C     PURPOSE
C     -------
C
C     LNOREPR      - Flag to say whether or not user specified NOREPR 

#include "parim.h"
#include "nifld.common"
#include "nofld.common"

      OUTREP = 1

      IF(LNOREPR) OUTREP = 0

      IF( OUTREP.EQ.1 ) THEN
        CALL INTLOG(JP_DEBUG,
     X 'OUTREP: Output Representation is Not specified',JPQUIET)
      ENDIF
C
      RETURN
      END
