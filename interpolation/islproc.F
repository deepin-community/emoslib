C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      REAL FUNCTION ISLPROC(RLAT,RLON,NEWTYPE,OLAT,OLON,TYPE,PT,OLDFLD)
C
C---->
C**** ISLPROC
C
C     Purpose
C     -------
C
C     Generates a new field point from its four nearest neighbours
C     taking account of matching land-sea mask types.
C
C
C     Interface
C     ---------
C
C     VALUE = ISLPROC(RLAT,RLON,NEWTYPE,OLAT,OLON,TYPE,PT,OLDFLD)
C
C     Input
C     -----
C
C     RLAT    - Latitude of the new point
C     RLON    - Longitude of the new point
C     NEWTYPE - Land-sea mask type of the new point (land/sea)
C     OLAT    - Latitudes of the four neighbouring points
C     OLON    - Longitudes of the four neighbouring points
C     TYPE    - Land-sea mask types of the four neighbouring points
C     PT      - Positions in old field array of the four neighbours
C     OLDFLD  - Array of old field values
C
C
C     Output
C     ------
C
C     Function returns a value for the new field point.
C
C
C     Method
C     ------
C
C     Find which neighbours have same land-sea mask type and use
C     them to calculate a new value by bi-linear interpolation.
C
C     Use all four neighbours if none of them have the same
C     land-sea mask type as the new point.
C
C
C     Externals
C     ---------
C
C     FORCED_NEAREST_NEIGHBOUR - check forced interpolation method
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     August 2000
C
C     Modifications
C     -------------
C
C     Force nearest neighbour processing with env variable or
C     INTOUT parameter
C     S.Curic           ECMWF    September 2005
C

C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      REAL RLAT,RLON,OLAT(2),OLON(4),OLDFLD(*)
      INTEGER NEWTYPE,TYPE(4),PT(4)
C
#include "intisl.h"
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Local variables
C
      LOGICAL LVEGGY
      INTEGER MCOUNT, COUNT, LOOP, NEAREST
      REAL WEIGHT(4), MATCH(4), NWEIGHT, SWEIGHT, NORMAL
C
C     Externals
C
      LOGICAL FORCED_NEAREST_NEIGHBOUR
C
C     -----------------------------------------------------------------|
C*    Section 1.   Find which neighbours have same land-sea mask type
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      COUNT  = 0
      DO LOOP = 1, 4
        IF( TYPE(LOOP).EQ.NEWTYPE ) THEN
          COUNT = COUNT + 1
          MATCH(LOOP) = 1.0
        ELSE
          MATCH(LOOP) = 0.0
        ENDIF
      ENDDO
C
      IF( COUNT.EQ.0 ) THEN
C
C       Use all four neighbours if none of them have the same
C       land-sea mask type as the new point
C
        COUNT = 4
        DO LOOP = 1, 4
          MATCH(LOOP) = 1.0
        ENDDO
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Handle case when missing values are present
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Identify matching neighbours with missing values
C
      IF( LIMISSV ) THEN
        MCOUNT = 0
        DO LOOP = 1, 4
          IF((MATCH(LOOP).EQ.1.0).AND.(OLDFLD(PT(LOOP)).EQ.RMISSGV))THEN
            MCOUNT = MCOUNT + 1
            MATCH(LOOP) = 0.0
            COUNT = COUNT - 1
          ENDIF
        ENDDO
C
C       Set new value to 'missing' if all neighbours are missing
C
        IF( (MCOUNT.EQ.4).OR.(COUNT.EQ.0) ) THEN
          ISLPROC = RMISSGV
          GOTO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Use neighbours that match to calculate the new value
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Calculate the north/south weighting values allowing for
C     southern latitude to be same as northern latitude
C
      NWEIGHT = OLAT(NORTH) - RLAT
      SWEIGHT = RLAT - OLAT(SOUTH)
      IF( SWEIGHT.EQ.0.0 ) NWEIGHT = 1.0
C
C     Calculate the weighting values for each matching neighbour
C
      WEIGHT(NWEST) = ABS((OLON(SEAST) - RLON) * SWEIGHT)
      WEIGHT(NEAST) = ABS((RLON - OLON(SWEST)) * SWEIGHT)
      WEIGHT(SWEST) = ABS((OLON(NEAST) - RLON) * NWEIGHT)
      WEIGHT(SEAST) = ABS((RLON - OLON(NWEST)) * NWEIGHT)
C
C     Normalise the weights since not all neighbours necessarily used
C
      NORMAL = ( WEIGHT(NWEST) * MATCH(NWEST) +
     X           WEIGHT(NEAST) * MATCH(NEAST) +
     X           WEIGHT(SWEST) * MATCH(SWEST) +
     X           WEIGHT(SEAST) * MATCH(SEAST) )
C
C     Special cases when matching points have no weight
C
      IF( NORMAL.EQ.0.0 )  THEN
C
C       Only one matching neighbour: set its weight so that is used
C
        IF( COUNT.EQ.1 ) THEN
          NORMAL = 1.0
          IF( MATCH(NWEST).EQ.1.0 ) WEIGHT(NWEST) = 1.0
          IF( MATCH(NEAST).EQ.1.0 ) WEIGHT(NEAST) = 1.0
          IF( MATCH(SWEST).EQ.1.0 ) WEIGHT(SWEST) = 1.0
          IF( MATCH(SEAST).EQ.1.0 ) WEIGHT(SEAST) = 1.0
C
C       Two matching neighbours: set their weights to those of the
C       neighbours on the opposite side of the interpolation rectangle
C
        ELSEIF( COUNT.EQ.2 ) THEN
          IF( (MATCH(NWEST).EQ.1.0).AND.(MATCH(NEAST).EQ.1.0) ) THEN
            WEIGHT(NWEST) = WEIGHT(SWEST)
            WEIGHT(NEAST) = WEIGHT(SWEST)
            NORMAL = WEIGHT(NWEST) + WEIGHT(NEAST)
          ELSEIF( (MATCH(NWEST).EQ.1.0).AND.(MATCH(SWEST).EQ.1.0) ) THEN
            WEIGHT(NWEST) = WEIGHT(NEAST)
            WEIGHT(SWEST) = WEIGHT(SEAST)
            NORMAL = WEIGHT(NWEST) + WEIGHT(SWEST)
          ELSEIF( (MATCH(NEAST).EQ.1.0).AND.(MATCH(SEAST).EQ.1.0) ) THEN
            WEIGHT(NEAST) = WEIGHT(NWEST)
            WEIGHT(SEAST) = WEIGHT(SWEST)
            NORMAL = WEIGHT(NEAST) + WEIGHT(SEAST)
          ELSEIF( (MATCH(SWEST).EQ.1.0).AND.(MATCH(SEAST).EQ.1.0) ) THEN
            WEIGHT(SWEST) = WEIGHT(NWEST)
            WEIGHT(SEAST) = WEIGHT(NEAST)
            NORMAL = WEIGHT(SWEST) + WEIGHT(SEAST)
          ELSEIF( (MATCH(NWEST).EQ.1.0).AND.(MATCH(SEAST).EQ.1.0) ) THEN
            WEIGHT(NWEST) = WEIGHT(NEAST)
            WEIGHT(SEAST) = WEIGHT(SWEST)
            NORMAL = WEIGHT(NWEST) + WEIGHT(SEAST)
          ELSE
            WEIGHT(NEAST) = WEIGHT(SEAST)
            WEIGHT(SWEST) = WEIGHT(NWEST)
            NORMAL = WEIGHT(NEAST) + WEIGHT(SWEST)
          ENDIF
C
C       Three matching neighbours with no weight, ie the interpolation
C       point is coincident with a grid point, but the land-sea masks
C       disagree about its type. Use it anyway.
C
        ELSE
          IF( MATCH(NWEST).EQ.0.0 ) THEN
            MATCH(NWEST) = 1.0
            NORMAL = WEIGHT(NWEST)
          ELSE IF( MATCH(NEAST).EQ.0.0 ) THEN
            MATCH(NEAST) = 1.0
            NORMAL = WEIGHT(NEAST)
          ELSE IF( MATCH(SWEST).EQ.0.0 ) THEN
            MATCH(SWEST) = 1.0
            NORMAL = WEIGHT(SWEST)
          ELSE
            MATCH(SEAST) = 1.0
            NORMAL = WEIGHT(SEAST)
          ENDIF
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Interpolate
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      WEIGHT(NWEST) = WEIGHT(NWEST)/NORMAL
      WEIGHT(NEAST) = WEIGHT(NEAST)/NORMAL
      WEIGHT(SWEST) = WEIGHT(SWEST)/NORMAL
      WEIGHT(SEAST) = WEIGHT(SEAST)/NORMAL

C     Use nearest neighbour if required
      LVEGGY = FORCED_NEAREST_NEIGHBOUR(LMETHOD,NITABLE,NIPARAM)
C
      IF( LVEGGY ) THEN
        NEAREST = NWEST
        IF( WEIGHT(NEAST).GT.WEIGHT(NEAREST) ) NEAREST = NEAST
        IF( WEIGHT(SWEST).GT.WEIGHT(NEAREST) ) NEAREST = SWEST
        IF( WEIGHT(SEAST).GT.WEIGHT(NEAREST) ) NEAREST = SEAST
        ISLPROC = OLDFLD(PT(NEAREST))
C
      ELSE
C
        ISLPROC = ( (OLDFLD(PT(NWEST)) * WEIGHT(NWEST) * MATCH(NWEST)) +
     X              (OLDFLD(PT(NEAST)) * WEIGHT(NEAST) * MATCH(NEAST)) +
     X              (OLDFLD(PT(SWEST)) * WEIGHT(SWEST) * MATCH(SWEST)) +
     X              (OLDFLD(PT(SEAST)) * WEIGHT(SEAST) * MATCH(SEAST)) )
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END

