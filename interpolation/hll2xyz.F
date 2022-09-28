C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE HLL2XYZ(RLAT,RLONG,X,Y,Z,INROW)
C---->
C**** HLL2XYZ
C
C     Purpose
C     -------
C
C     Converts an array of latitude/longitude values to an
C     array of (x,y,z) coordinates.
C
C
C     Interface
C     ---------
C
C     CALL HLL2XYZ(RLAT,RLONG,X,Y,Z,INROW)
C
C     Input
C     -----
C
C     RLAT  - Array of latitudes.
C     RLONG - Array of corresponding longitudes.
C     INROW - Count of latitudes/longitudes.
C
C
C     Output
C     ------
C
C     (X,Y,Z) - Array of coordinates.
C
C
C     Method
C     ------
C
C     Z axis is the polar axis directed from south to north.
C     X and Y axes are in the equatorial plane and have origin at the
C     centre of the Earth. The X axis passes through (O north, 0 east).
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
      REAL RLAT,RLONG,X,Y,Z
      DIMENSION RLAT(*),RLONG(*),X(*),Y(*),Z(*)
      INTEGER INROW
C
C     Local variables
C
      REAL FACTOR
      DATA FACTOR/0.017453293/
      INTEGER LOOP
C
      DO LOOP = 1, INROW
        X(LOOP) = COS(RLAT(LOOP)*FACTOR) * COS(RLONG(LOOP)*FACTOR)
        Y(LOOP) = COS(RLAT(LOOP)*FACTOR) * SIN(RLONG(LOOP)*FACTOR)
        Z(LOOP) = SIN(RLAT(LOOP)*FACTOR)
      ENDDO
C
      RETURN
      END

