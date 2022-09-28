C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HGENLL(AREA,POLE,GRID,NLON,NLAT,RLAT,RLON)
C
C---->
C**** HGENLL
C
C     Purpose
C     -------
C
C     This routine calculates the original latitude and longitude
C     values (before rotation) for a rotatedlat/long grid.
C
C
C     Interface
C     ---------
C
C     IRET = HGENLL(AREA,POLE,GRID,NLON,NLAT,RLAT,RLON)
C
C
C     Input parameters
C     ----------------
C
C     AREA - Limits of area (N/W/S/E)
C     POLE - Pole of rotation (lat/long)
C     GRID - Grid increments (i/j)
C
C
C     Output parameters
C     -----------------
C
C     NLON  - Number of points along a parallel in the rotated grid.
C     NLAT  - Number of points along a meridien in the rotated grid.
C     RLAT  - Vector of orginal latitude values for the points in
C             the rotated grid.
C     RLON  - Vector of orginal longitude values for the points in
C             the rotated grid.
C
C     Returns 0 if function successful, non-zero otherwise.
C
C     Common block usage
C     ------------------
C
C     None.
C
C
C     Method
C     ------
C
C     The vector of points runs from West to East in rows, the rows
C     run from North to South.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message.
C     JDEBUG  - Tests if debug output required.
C     HLL2XYZ - Converts a latitude/longitude position to (x,y,z)
C               wrt axes through the centre of the globe. The z-axis
C               runs from the south to north pole. The x- and y-axes
C               are in the plane of the equator with the x-axis
C               pointing out through lat/long (0,0).
C     YROTATE - Rotates the globe about the new y-axis.
C     XYZ2LL  - Converts an (x,y,z) position to a latitude/longitude.
C     JMALLOC - Dynamically allocate memory
C     JFREE   - Free dynamically allocated memory
C
C
C     Reference
C     ---------
C
C     None.
C
C
C     Comments
C     --------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers      ECMWF      January 2001
C
C
C     Modifications
C     -------------
C
C     None.
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
#include "parim.h"
C
C     Parameters
C
      INTEGER JNORTH, JSOUTH, JWEST, JEAST, JW_E, JN_S, JLAT, JLON
      REAL JSCALE
      PARAMETER (JNORTH = 1 )
      PARAMETER (JWEST  = 2 )
      PARAMETER (JSOUTH = 3 )
      PARAMETER (JEAST  = 4 )
      PARAMETER (JW_E  = 1 )
      PARAMETER (JN_S  = 2 )
      PARAMETER (JLAT  = 1 )
      PARAMETER (JLON  = 2 )
      PARAMETER (JSCALE  = 1000.0 )
C
C     Function arguments
C
      REAL AREA(4),POLE(2),GRID(2), RLAT(*),RLON(*)
      INTEGER NLON,NLAT
C
C     Local variables
C
      INTEGER NEXT, LOOP, LOOPI, LOOPO, IGRIDNI, IGRIDNJ
      INTEGER ISIZE, ISIZOLD, NBYTES
      REAL ZLAT, X(1), Y(1), Z(1)
      REAL RX(1), RY(1), RZ(1)
      POINTER( IPX, X )
      POINTER( IPY, Y )
      POINTER( IPZ, Z )
      POINTER( IPRX, RX )
      POINTER( IPRY, RY )
      POINTER( IPRZ, RZ )
C
      DATA ISIZOLD/-1/
      SAVE ISIZOLD,IPX,IPY,IPZ,IPRX,IPRY,IPRZ
C
C     Externals
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HGENLL = 0
C
      CALL JDEBUG()
C
C     -----------------------------------------------------------------|
C     Section 2.  Calculate current grid latitudes and longitudes
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      NLON = 1 + NINT((AREA(JEAST) - AREA(JWEST)) / GRID(JW_E))
      NLAT = 1 + NINT((AREA(JNORTH) - AREA(JSOUTH)) / GRID(JN_S))
      IGRIDNI = NINT(GRID(JW_E)*JSCALE)
      IGRIDNJ = NINT(GRID(JN_S)*JSCALE)
C
      NEXT = 1
      DO LOOPO = 1, NLAT
        ZLAT = AREA(JNORTH) - REAL((LOOPO-1)*IGRIDNJ)/JSCALE
        DO LOOPI = 1, NLON
          RLON(NEXT) = AREA(JWEST) + REAL((LOOPI-1)*IGRIDNI)/JSCALE
          IF( RLON(NEXT).LT.0.0 ) RLON(NEXT) = RLON(NEXT) + 360.0
          RLAT(NEXT) = ZLAT
          NEXT = NEXT + 1
        ENDDO
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 3.  Get some space for rotating the lat/longs
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      ISIZE = NLAT *NLON
      IF( ISIZE.GT.ISIZOLD ) THEN
C
        IF( ISIZOLD.GT.0 ) CALL JFREE(IPX)
C
        NBYTES = 6*ISIZE*JPRLEN
C
        IPX = JMALLOC(NBYTES)
#ifdef hpR64
        IPX = IPX/(1024*1024*1024*4)
#endif
        IF( IPX.EQ.0 ) THEN
          CALL INTLOG(JP_WARN,'HGENLL: Memory allocate fail',JPQUIET)
          HGENLL = 1
          GOTO 900
        ENDIF
C
        IPY  = IPX  + (ISIZE*JPRLEN)
        IPZ  = IPY  + (ISIZE*JPRLEN)
        IPRX = IPZ  + (ISIZE*JPRLEN)
        IPRY = IPRX + (ISIZE*JPRLEN)
        IPRZ = IPRY + (ISIZE*JPRLEN)
C
        ISIZOLD = ISIZE
C
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 4.  Calculate the lat/longs before rotation
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Convert the rotated row points lat/longs to (x,y,z) coordinates
C
      CALL HLL2XYZ(RLAT,RLON,X,Y,Z,ISIZE)
C
C     Rotate the rotated row points back through the original latitude
C     rotation
C
      CALL YROTATE(-(90.0+POLE(JLAT)),X,Y,Z,RX,RY,RZ,ISIZE)
C
C     Convert the rotated row points adjusted (x,y,z) coordinates to
C     lat/long in the original grid (but still containing the
C     longitude rotation)
C
      CALL XYZ2LL(RX,RY,RZ,RLAT,RLON,ISIZE)
C
C     Adjust the rotated line longitudes to remove the longitude
C     rotation
C
      DO LOOP = 1, ISIZE
        RLON(LOOP) = RLON(LOOP) + POLE(JLON)
        IF( RLON(LOOP).LT.0.0 )   RLON(LOOP) = RLON(LOOP) + 360.0
        IF( RLON(LOOP).GE.360.0 ) RLON(LOOP) = RLON(LOOP) - 360.0
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 9.  Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
