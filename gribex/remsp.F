C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE REMSP (STRING)
C
C---->
C**** REMSP - Remove leading spaces from character string.
C     NAME       : REMSP
C
C     FUNCTION   : LEFT JUSTIFY CHARACTER STRING BY REMOVING
C                  ANY LEADING SPACES.
C
C     INPUT      : STRING = CHARACTER STRING, WITH OR WITHOUT
C                           LEADING SPACES.
C
C     OUTPUT     : STRING = CHARACTER STRING LEFT JUSTIFIED.
C
C         JOHN HENNESSY, ECMWF, 16TH APRIL 1985
C
C     ---------------------------------------------------------------
C----<
C
      CHARACTER*(*) STRING
C
      DO 100 I=1,LEN(STRING)
         IF (STRING(I:I).NE.' ') GO TO 200
  100 CONTINUE
C
C     Blank line.
C
      RETURN
C
C     Left justify string.
C
  200 STRING(1:) = STRING(I:)
C
      RETURN
      END
