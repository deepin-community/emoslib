C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE NGVALUE(NEWFLD, NEXT, INROW, RLAT, RLON,
     X                   POLELAT, POLELON, NGAUSS, HTYPE, FIELD,
     X                   GLATS, NGPTS)
C
C---->
C**** NGVALUE
C
C     PURPOSE
C     _______
C
C     Rotates one line of latitude in a gaussian grid.
C
C     INTERFACE
C     _________
C
C     CALL NGVALUE(NEWFLD, NEXT, INROW, RLAT, RLON,
C    X             POLELAT,POLELON,NGAUSS,HTYPE,FIELD,GLATS,NGPTS)
C
C
C     Input parameters
C     ________________
C
C     NEWFLD  - The field of values being created
C     NEXT    - Start offset of the latitude row being created
C     INROW   - The number of points in the latitude row
C     RLAT    - Target point latitude in the rotated grid
C     RLON    - Target point longitude in the rotated grid
C     POLELAT - Latitude of the rotated South Pole
C     POLELON - Longitude of the rotated South Pole
C     NGAUSS  - Gaussian grid number (original, unrotated)
C     HTYPE   - Gaussian grid type (original, unrotated)
C               = 'R' for reduced ("quasi-regular"),
C               = 'O' for reduced/octahedral,
C               = 'F' for full,
C               = 'U' for user-defined reduced gaussian grid
C     FIELD   - Original unrotated gaussian field values
C     GLATS   - Gaussian latitudes for original field
C     NGPTS   - Number of points along original field latitudes
C
C     Output parameters
C     ________________
C
C     NEWFLD(NEXT) to NEWFLD(NEXT+INROW-1) contains the latitude row
C     values.
C
C
C     METHOD
C     ______
C
C     Rotates the lat/long positions of points along a latitude row in
C     a rotated gaussian grid back to positions in the unrotated grid
C     and then interpolates values from the original grid.
C
C
C     EXTERNALS
C     _________
C
C     LL2XYZ  - Converts a latitude/longitude position to (x,y,z)
C               wrt axes through the centre of the globe. The z-axis
C               runs from the south to north pole. The x- and y-axes
C               are in the plane of the equator with the x-axis
C               pointing out through lat/long (0,0).
C     YROTATE - Rotates the globe about the new y-axis.
C     XYZ2LL  - Converts an (x,y,z) position to a latitude/longitude.
C     NGINTRP - Interpolates a value from an existing grid.
C
C
C     REFERENCE
C     _________
C
C     None
C
C
C     COMMENTS
C     ________
C
C     The input field is assumed to be global in extent, and a known
C     definition, either N160 or N80 (reduced or regular).
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      ECMWF       April 1996
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPMAXGG
      PARAMETER( JPMAXGG = 1280 )
C
C     Function arguments
C
      REAL RLAT, POLELAT, POLELON
      REAL NEWFLD(*), RLON(*), FIELD(*), GLATS(*)
      INTEGER NEXT, INROW, NGAUSS, NGPTS(*)
      CHARACTER*1 HTYPE
C
C     Local variables
C
      REAL X(JPMAXGG), Y(JPMAXGG), Z(JPMAXGG)
      REAL RX(JPMAXGG),RY(JPMAXGG),RZ(JPMAXGG)
      REAL OLAT(JPMAXGG),OLON(JPMAXGG)
      INTEGER LOOP
C
C     Convert the rotated row points lat/longs to (x,y,z) coordinates
C
      CALL LL2XYZ(RLAT,RLON,X,Y,Z,INROW)
C
C     Rotate the rotated row points back through the original latitude
C     rotation
C
      CALL YROTATE(-(90.0+POLELAT),X,Y,Z,RX,RY,RZ,INROW)
C
C     Convert the rotated row points adjusted (x,y,z) coordinates to
C     lat/long in the original grid (after longitude rotation)
C
      CALL XYZ2LL(RX,RY,RZ,OLAT,OLON,INROW)
C
C     Adjust the rotated line longitudes to remove the longitude
C     rotation
C
      DO LOOP = 1, INROW
        OLON(LOOP) = OLON(LOOP) + POLELON
        IF( OLON(LOOP).LT.0.0 )   OLON(LOOP) = OLON(LOOP) + 360.0
        IF( OLON(LOOP).GE.360.0 ) OLON(LOOP) = OLON(LOOP) - 360.0
      ENDDO
C
C     Interpolate the original unrotated grid to the points in the
C     rotated line
C
      CALL NGINTRP(NEWFLD,NEXT,INROW,OLAT,OLON,NGAUSS,HTYPE,FIELD,
     X             GLATS,NGPTS)
C
      RETURN
      END
