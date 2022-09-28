C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

C---->
C
C     Functions used in ocean field interpolation.
C
C     FUNCTION INTDN(XVAL)
C
C     Rounds XVAL to the largest integer less than or equal to XVAL
C     ie truncates downwards rather than towards zero.
C
C     FUNCTION LENA(HSTRING)
C
C     Returns the length of a string, ignoring blank characters at end
C
C     FUNCTION HDEGS(RVAL)
C
C     Writes a real number into a string, in a way suitable for degs lat/long.
C     RVAL   The real number
C
C     FUNCTION HMETRES(RVAL)
C
C     Writes a real number into a string, in a way suitable for metres depth.
C     RVAL   The real number
C
C----<

      INTEGER FUNCTION INTDN(XVAL)
C
C   Rounds XVAL to the largest integer less than or equal to XVAL
C   ie truncates downwards rather than towards zero.
C
      IF((XVAL.GE.0.0).OR.(XVAL.EQ.FLOAT(INT(XVAL)))) THEN
         INTDN = INT(XVAL)
      ELSE
         INTDN = INT(XVAL) - 1
      ENDIF
      RETURN
      END

      INTEGER FUNCTION INTUP(XVAL)
C
C   Rounds XVAL to the largest integer greater than or equal to XVAL
C   ie truncates upwards rather than away from zero.
C
      IF(XVAL.EQ.FLOAT(INT(XVAL))) THEN
         INTUP=INT(XVAL)
      ELSEIF(XVAL.GE.0) THEN
         INTUP=INT(XVAL+1)
      ELSE
         INTUP=INT(XVAL)
      ENDIF
      RETURN
      END

      INTEGER FUNCTION LENA(HSTRING)
C
C   Returns the length of a string, ignoring blank characters at end
C
      CHARACTER*(*) HSTRING

      DO 100 J=LEN(HSTRING),1,-1
         IF(HSTRING(J:J).NE.' ') GOTO 150
  100 CONTINUE
      J=0
  150 CONTINUE
      LENA=J

      RETURN
      END

      FUNCTION HDEGS(RVAL)
C   Writes a real number into a string, in a way suitable for degs lat/long.
C     RVAL   The real number
C
      CHARACTER*6 HDEGS

      IF(RVAL.GE.0) THEN
         IF(RVAL.LT.10.0) THEN
            WRITE(HDEGS,'(F4.2)') RVAL
         ELSEIF(RVAL.LT.100.0) THEN
            WRITE(HDEGS,'(F4.1)') RVAL
         ELSEIF(RVAL.LT.1000.0) THEN
            WRITE(HDEGS,'(F5.1)') RVAL
         ELSE
            WRITE(HDEGS,'(F5.0)') RVAL
         ENDIF
      ELSE
         IF(RVAL.GT.-10.0) THEN
            WRITE(HDEGS,'(F5.2)') RVAL
         ELSEIF(RVAL.GT.-100.0) THEN
            WRITE(HDEGS,'(F5.1)') RVAL
         ELSEIF(RVAL.GT.-1000.0) THEN
            WRITE(HDEGS,'(F6.1)') RVAL
         ELSE
            WRITE(HDEGS,'(F6.0)') RVAL
         ENDIF
      ENDIF

      RETURN
      END


      FUNCTION HMETRES(RVAL)
C   Writes a real number into a string, in a way suitable for metres depth.
C     RVAL   The real number
C
      CHARACTER*6 HMETRES

      IF(RVAL.GE.0) THEN
         IF(RVAL.LE.9.99) THEN
            IF(INT(RVAL*10)*10.EQ.INT(RVAL*100)) THEN
               WRITE(HMETRES,'(F3.1)') RVAL
            ELSE
               WRITE(HMETRES,'(F4.2)') RVAL
            ENDIF
         ELSEIF(RVAL.LE.99.9) THEN
            WRITE(HMETRES,'(F4.1)') RVAL
         ELSEIF(RVAL.LE.999.0) THEN
            WRITE(HMETRES,'(F4.0)') RVAL
         ELSE
            WRITE(HMETRES,'(F5.0)') RVAL
         ENDIF
      ELSE
         IF(RVAL.GE.-9.99) THEN
            WRITE(HMETRES,'(F5.2)') RVAL
         ELSEIF(RVAL.GE.-99.9) THEN
            WRITE(HMETRES,'(F5.1)') RVAL
         ELSEIF(RVAL.GE.-999.0) THEN
            WRITE(HMETRES,'(F5.0)') RVAL
         ELSE
            WRITE(HMETRES,'(F6.0)') RVAL
         ENDIF
      ENDIF

      RETURN
      END
