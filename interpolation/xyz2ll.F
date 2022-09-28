C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE XYZ2LL(X,Y,Z,RLAT,RLONG,INROW)
C---->
C**** XYZ2LL
C
C     Purpose
C     -------
C
C     Converts an array of (x,y,z) coordinates to an
C     array of latitude/longitude values.
C
C
C     Interface
C     ---------
C
C     CALL XYZ2LL(X,Y,Z,RLAT,RLONG,INROW)
C
C     Input
C     -----
C
C     (X,Y,Z) - Array of coordinates.
C     INROW   - Count of coordinates.
C
C
C     Output
C     ------
C
C     RLAT  - Array of latitudes.
C     RLONG - Array of corresponding longitudes.
C
C
C     Method
C     ------
C
C     Z axis is the polar axis directed from south to north.
C     X and Y axes are in the equatorial plane and have origin at the
C     centre of the Earth. The X axis passes through (O north, 0 east).
C     Longitude values are in range (0,360).
C
C
C     Externals
C     ---------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF
C
C----<
C
      IMPLICIT NONE
C
C     Subroutine arguments
C
      REAL X,Y,Z,RLAT,RLONG
      DIMENSION X(*),Y(*),Z(*),RLAT(*),RLONG(*)
      INTEGER INROW
C
C     Local variables
C
      REAL*8 FACTOR
      DATA FACTOR/57.2957763671875/
      INTEGER LOOP
C
      DO LOOP = 1, INROW
        RLAT(LOOP)  = ASIN(Z(LOOP)) * FACTOR
      ENDDO
C
      DO LOOP = 1, INROW
        IF( ABS(X(LOOP)).GT.0.0001 ) THEN
          RLONG(LOOP) = ATAN2(Y(LOOP),X(LOOP)) * FACTOR
        ELSE IF ( Y(LOOP).GT.0.0 ) THEN
          RLONG(LOOP) = 90.0
        ELSE
          RLONG(LOOP) = -90.0
        ENDIF
      ENDDO
C
      DO LOOP = 1, INROW
        IF(RLONG(LOOP).LT.0.0) RLONG(LOOP) = RLONG(LOOP) + 360.0
      ENDDO
C
      RETURN
      END

