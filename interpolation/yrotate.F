C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE YROTATE(RANGLE,X,Y,Z,RX,RY,RZ,INROW)
C---->
C**** YROTATE
C
C     Purpose
C     -------
C
C     Rotate an array of (x,y,z) values about the y-axis.
C
C
C     Interface
C     ---------
C
C     CALL YROTATE(RANGLE,X,Y,Z,RX,RY,RZ,INROW)
C
C     Input
C     -----
C
C     RANGLE  - The angle of rotation (degrees)
C     (X,Y,Z) - Arrays of input (x,y,z) values.
C     INROW   - Number of coordinate values.
C
C
C     Output
C     ------
C
C     (RX,RY,RZ) - Arrays of rotated (x,y,z) values.
C
C
C     Method
C     ------
C
C     Y remains unchanged.
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
      REAL RANGLE,X,Y,Z,RX,RY,RZ
      DIMENSION X(*),Y(*),Z(*),RX(*),RY(*),RZ(*)
      INTEGER INROW
C
C     Local variables
C
      REAL FACTOR, ANGLE, SINA, COSA
      DATA FACTOR/0.017453293/
      INTEGER LOOP
C
      ANGLE = RANGLE * FACTOR
      SINA  = SIN(ANGLE)
      COSA  = COS(ANGLE)
C
      DO LOOP = 1, INROW
        RY(LOOP) = Y(LOOP)
        RX(LOOP) =  X(LOOP)*COSA + Z(LOOP)*SINA
        RZ(LOOP) = -X(LOOP)*SINA + Z(LOOP)*COSA
      ENDDO
C
      RETURN
      END
