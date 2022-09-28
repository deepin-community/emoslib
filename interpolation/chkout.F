C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION CHKOUT()
C
C---->
C*****CHKOUT*
C
C     PURPOSE
C     -------
C     check if intout  has been called
C     LINTOUT      - Flag to say whether or not intout  has been called
C
C     Function argument
C

#include "parim.h"
#include "nofld.common"

      CHKOUT = 0
      IF(LINTOUT) THEN
         CHKOUT = 1
         CALL INTLOG(JP_DEBUG,'CHKOUT: INTOUT has been called'
     X   ,JPQUIET)
      ENDIF
C
      RETURN
      END
