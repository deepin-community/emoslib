C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      REAL FUNCTION KNFROM4(LVEGGY,RLAT,RLON,OLAT,OLON,PT,OLDFLD)
C
C---->
C**** KNFROM4
C
C     Purpose
C     -------
C
C     Generates a new field point from its four nearest neighbours.
C
C
C     Interface
C     ---------
C
C     VALUE = KNFROM4(RLAT,RLON,OLAT,OLON,PT,OLDFLD)
C
C     Input
C     -----
C
C     LVEGGY - 'True' if 'nearest neighbour' processing required
C     RLAT   - Latitude of the new point
C     RLON   - Longitude of the new point
C     OLAT   - Latitudes of the upper and lower lines
C     OLON   - Longitudes of the four neighbouring points
C     PT     - Positions in old field array of the four neighbours
C     OLDFLD - Array of old field values
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
C     Uses neighbours to calculate a value by bi-linear interpolation.
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
C     J.D.Chambers     ECMWF     November 2000
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      LOGICAL LVEGGY
      REAL RLAT,RLON,OLAT(2),OLON(4),OLDFLD(*)
      INTEGER PT(4)
C
#include "intisl.h"
#include "nifld.common"
C
C     Local variables
C
      INTEGER COUNT, NEAREST
      REAL WEIGHT(4), NWEIGHT, SWEIGHT, NORMAL

C     Additional local variables for calculation
C     of corrected bilinear interpolation
C     that works for non-regular grids
      REAL NWGHT, SWGHT 
      REAL SVALUE, SWEIGHT2, SWEIGHT1 
      REAL NVALUE, NWEIGHT2, NWEIGHT1
C
C     Externals
C
C
C     Statement function
C
      REAL A, B
      LOGICAL NOTEQ
      NOTEQ(A,B) = (ABS((A)-(B)).GT.(ABS(A)*1E-3))
C
C     -----------------------------------------------------------------|
C*    Section 1. Calculate weighting values for each neighbour
C     -----------------------------------------------------------------|
C


  100 CONTINUE
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
c     new method
      WEIGHT(NWEST) = ABS((OLON(NEAST) - RLON) * SWEIGHT)
      WEIGHT(NEAST) = ABS((RLON - OLON(NWEST)) * SWEIGHT)
      WEIGHT(SWEST) = ABS((OLON(SEAST) - RLON) * NWEIGHT)
      WEIGHT(SEAST) = ABS((RLON - OLON(SWEST)) * NWEIGHT)
      
C
C     Normalise the weights
C
      NORMAL = ( WEIGHT(NWEST) + WEIGHT(NEAST) +
     X           WEIGHT(SWEST) + WEIGHT(SEAST) )
C
      WEIGHT(NWEST) = WEIGHT(NWEST)/NORMAL
      WEIGHT(NEAST) = WEIGHT(NEAST)/NORMAL
      WEIGHT(SWEST) = WEIGHT(SWEST)/NORMAL
      WEIGHT(SEAST) = WEIGHT(SEAST)/NORMAL

C
C     -----------------------------------------------------------------|
C*    Section 3.   Interpolate
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Use nearest neighbour for vegetation field
C
      IF( LVEGGY ) THEN
        NEAREST = NWEST
        IF( WEIGHT(NEAST).GT.WEIGHT(NEAREST) ) NEAREST = NEAST
        IF( WEIGHT(SWEST).GT.WEIGHT(NEAREST) ) NEAREST = SWEST
        IF( WEIGHT(SEAST).GT.WEIGHT(NEAREST) ) NEAREST = SEAST
        KNFROM4 = OLDFLD(PT(NEAREST))
C
      ELSE

C
C       Count non-missing data values
C
        COUNT = 0
        IF( NOTEQ(OLDFLD(PT(NWEST)),RMISSGV) ) COUNT = COUNT + 1
        IF( NOTEQ(OLDFLD(PT(NEAST)),RMISSGV) ) COUNT = COUNT + 1
        IF( NOTEQ(OLDFLD(PT(SWEST)),RMISSGV) ) COUNT = COUNT + 1
        IF( NOTEQ(OLDFLD(PT(SEAST)),RMISSGV) ) COUNT = COUNT + 1
C
C       Interpolate using four neighbours if none are missing
C
        IF( COUNT.EQ.4 ) THEN

C         An entirely different calculaton (a correct one) using 
C         a bilinear method that works on reduced Gaussian grids

C         work out the interpolated value along the top line of latitude

          NWEIGHT1 = (RLON - OLON(NWEST)) / (OLON(NEAST) - OLON(NWEST))
          NWEIGHT2 = 1.0 - NWEIGHT1
          NVALUE = OLDFLD(PT(NWEST)) * NWEIGHT2 +
     X             OLDFLD(PT(NEAST)) * NWEIGHT1

C         do the same for the southern line of latitude
          SWEIGHT1 = (RLON - OLON(SWEST)) / (OLON(SEAST) - OLON(SWEST))
          SWEIGHT2 = 1.0 - SWEIGHT1
          SVALUE = OLDFLD(PT(SWEST)) * SWEIGHT2 + 
     x             OLDFLD(PT(SEAST)) * SWEIGHT1

          IF (OLAT(NORTH).EQ.OLAT(SOUTH)) THEN
C             we can't work with these numbers in our
c             new calculation so revert to what emoslib
c             used to do as it is at least robust to what
c             appears to be an accidental call to this function
              KNFROM4 = ( (OLDFLD(PT(NWEST)) * WEIGHT(NWEST)) +
     X                      (OLDFLD(PT(NEAST)) * WEIGHT(NEAST)) +
     X                      (OLDFLD(PT(SWEST)) * WEIGHT(SWEST)) +
     X                      (OLDFLD(PT(SEAST)) * WEIGHT(SEAST)) )

          ELSE
C         now interpolate between these values according to the 
C         output latitude value
              NWGHT = (RLAT - OLAT(SOUTH)) / (OLAT(NORTH) - OLAT(SOUTH))
              SWGHT = 1.0 - NWGHT
              KNFROM4 = NWGHT * NVALUE + SWGHT * SVALUE
          END IF
C
C       Set missing if all neighbours are missing
C
        ELSE IF( COUNT.EQ.0 ) THEN
          KNFROM4 = RMISSGV
C
C       Otherwise, use the nearest neighbour
C
        ELSE
          NEAREST = NWEST
          IF( WEIGHT(NEAST).GT.WEIGHT(NEAREST) ) NEAREST = NEAST
          IF( WEIGHT(SWEST).GT.WEIGHT(NEAREST) ) NEAREST = SWEST
          IF( WEIGHT(SEAST).GT.WEIGHT(NEAREST) ) NEAREST = SEAST
          KNFROM4 = OLDFLD(PT(NEAREST))
        ENDIF
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
