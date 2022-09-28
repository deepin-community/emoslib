C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      INTEGER FUNCTION RTB (CHAR)
C
C---->
C*
C*    NAME      : RTB
C*
C*    FUNCTION  : COUNT THE NUMBER OF CHARACTERS IN A CHARACTER
C*                STRING, EXCLUDING TRAILING SPACES.
C*
C*    INPUT     : CHAR - CHARACTER STRING
C*
C*    OUTPUT    : RTB  - NO OF CHARACTERS
C*
C*    GENERAL   : RTB CALLS  -----
C*
C*    AUTHOR    : J.HENNESSY  15.4.85
C*
C     ---------------------------------------------------------------
C----<
C
      CHARACTER*(*) CHAR
C
      J = LEN (CHAR)
C
C     No trailing blanks.
C
      IF (CHAR(J:J).NE.' ')
     C     THEN
               RTB = J
               RETURN
           ENDIF
C
C     String with trailing blanks.
C
      DO 100 I = J,1,-1
         IF (CHAR(I:I).NE.' ')
     C          THEN
                    RTB = I
                    RETURN
                ENDIF
  100 CONTINUE
C
C     String consists of only blanks.
C
      RTB = 0
C
      RETURN
      END
