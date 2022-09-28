C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION GLOBAL()
C
C---->
C*****GLOBAL*
C
C     PURPOSE
C     -------
C     LGLOBL      - Flag to say if output grid is global
C
C     Function argument
C

#include "parim.h"
#include "nofld.common"

      GLOBAL = 0
      IF(LGLOBL) THEN
         GLOBAL = 1
         CALL INTLOG(JP_DEBUG,'GLOBAL: Output Grid is Global'
     X   ,JPQUIET)
      ENDIF
C
      RETURN
      END
