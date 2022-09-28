C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HNEILL
     X  (L12PNT,KLEN,RLAT,RLON,OLDGRID,
     X   KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
C
C---->
C**** HNEILL
C
C     Purpose
C     -------
C
C     This routine accepts a vector of points and finds the 12
C     neighbours for each point suitable for horizontal interpolation.
C
C
C     Interface
C     ---------
C
C     IRET = HNEILL(L12PNT,KLEN,RLAT,RLON,OLDGRID,
C    X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT  - Chooses between 12-point and 4-point interpolation
C               = .TRUE. for 12-point horizontal
C               = .FALSE. for 4-point
C     KLEN    - Number of points along the vector
C     RLAT    - List of latitudes for points.
C     RLON    - List of longitudes for points.
C     OLDGRID - Grid increments (i/j) for the global lat/long field
C
C
C     Output parameters
C     -----------------
C
C     KSCHEME - Flag showing interpolation scheme to use for point
C               0 = 12-point
C               1 = 4-point bilinear
C               2 = nearest neighbour
C     PDLAT   - Meridian linear weight.
C     PDLO0   - Zonal linear weight for the latitude of point 5.
C     PDLO1   - Zonal linear weight for the latitude of point 1.
C     PDLO2   - Zonal linear weight for the latitude of point 3.
C     PDLO3   - Zonal linear weight for the latitude of point 11.
C     KLA     - Latitude number in original field of latitude north of
C               each point in the vector.
C     NEIGH   - List of indices in the original field of neighbouring
C               point values for each point in the vector.
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
C     Numbering of the points (I is the interpolation point):
C
C                   13       5       6      14
C
C                    7       1       2       8
C                               (I)
C                    9       3       4      10
C
C                   15      11      12      16
C
C     The 12-point interpolation is not possible if either of the top
C     two rows is above the original field northern latitude. The
C     nearest neighbour is used if both rows are above, and a 4-pt
C     bilinear interpolation is used if the top row is above.
C     Similarily, if either of the bottom two rows is below the original
C     field southern latitude.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message.
C     JDEBUG  - Tests if debug output required.
C     JFREE   - Free dynamically allocated memory
C
C
C     Reference
C     ---------
C
C     ECMWF Meteorological Bulletin M1.6/7
C     IFS Documentation
C     Part VI: Technical and Computational Procedures (CY21R4)
C     March 2000
C     Section 2.3
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
      INTEGER JP12PT, JP4PT, JPNEARN
      PARAMETER (JP12PT  = 0)
      PARAMETER (JP4PT   = 1)
      PARAMETER (JPNEARN = 2)
C
C     Function arguments
C
      LOGICAL L12PNT
      INTEGER KLEN
      REAL OLDGRID(2), RLAT(KLEN), RLON(KLEN)
C
      INTEGER KLA(KLEN), KSCHEME(KLEN)
      REAL PDLAT(KLEN),PDLO0(KLEN),PDLO1(KLEN),PDLO2(KLEN),PDLO3(KLEN)
      INTEGER NEIGH(12,*)
C
C     Local variables
C
      INTEGER NEXT, LOOP, NOLDLAT, NOLDLON
      INTEGER NLAT0, NLAT1, NLAT2, NLAT3, NLAT(12), NLONG(12)
      INTEGER NLONG1, NLONG2, NLONG5
      REAL PLATINC, PLONINC
C
C     Statement functions
C
      INTEGER ILAT, NUMLAT, NUMLON
      REAL LAT, LON, PLATIN, LATSTEP, LONSTEP
C
      PLATIN(ILAT,LATSTEP) = 90.0 - REAL(ILAT-1)*LATSTEP
      NUMLAT(LAT,LATSTEP)  = 1 + NINT((90.0 - LAT)/LATSTEP)
      NUMLON(LON,LONSTEP)  = 1 + NINT(LON/LONSTEP )
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HNEILL = 0
C
      CALL JDEBUG()
C
      PLATINC = OLDGRID(1)
      PLONINC = OLDGRID(2)
      NOLDLAT = 1 + NINT(180.0/PLATINC)
      NOLDLON = NINT(360.0/PLONINC)
C
C     -----------------------------------------------------------------|
C       Section 2.  Loop through points along the vector.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      DO NEXT = 1,KLEN
C
C       Clear current neighbour indices
C
        DO LOOP = 1, 12
          NEIGH(LOOP,NEXT) = 0
        ENDDO
C
C     -----------------------------------------------------------------|
C       Section 3. Find latitude numbers for neighbours.
C     -----------------------------------------------------------------|
C
  300   CONTINUE
C
C       First find latitude numbers to north and south of the point
C
        NLAT1 = NUMLAT(RLAT(NEXT),PLATINC)
        NLAT2 = NLAT1 + 1
C
C       Find outer latitude numbers further north and south of the point
C
        NLAT0 = NLAT1 - 1
        NLAT3 = NLAT2 + 1
C
C     -----------------------------------------------------------------|
C       Section 4. Nearest neighbour to be used.
C     -----------------------------------------------------------------|
C
  400   CONTINUE
C
        IF( (NLAT1.LT.1).OR.(NLAT2.GT.NOLDLAT) ) THEN
C
C         On northern/southern latitude of original field, so use
C         nearest neighbours (point 1 or 2)
C
          IF( NLAT1.LT.1 ) NLAT1 = 1
C
          IF( NLAT2.GT.NOLDLAT ) NLAT1 = NOLDLAT
C
          NLONG(1) = NUMLON(RLON(NEXT),PLONINC)
          NLONG1   = NLONG(1)
          IF( NLONG(1).GT.NOLDLON ) NLONG(1) = 1
          NLONG(2) = NLONG(1) + 1
          NLONG2   = NLONG(2)
          IF( NLONG(2).GT.NOLDLON ) NLONG(2) = 1
C
          IF( ABS((RLON(NEXT) - REAL(NLONG1-1)*PLONINC )) .LE.
     X        ABS((RLON(NEXT) - REAL(NLONG2-1)*PLONINC )) ) THEN
            NEIGH(1,NEXT) = (NLAT1-1)*NOLDLON + NLONG(1)
          ELSE
            NEIGH(1,NEXT) = (NLAT1-1)*NOLDLON + NLONG(2)
          ENDIF
          KSCHEME(NEXT) = JPNEARN
          GOTO 700
        ENDIF
C
C     -----------------------------------------------------------------|
C       Section 5. 4-point bilinear to be used.
C     -----------------------------------------------------------------|
C
  500   CONTINUE
C
        IF( (NLAT0.LT.1).OR.(NLAT3.GT.NOLDLAT).OR.
     X      (.NOT.L12PNT) ) THEN
C
          CALL INTLOG(JP_DEBUG,
     X      'HNEILL: 4-pt bilinear interpolation used for point ',NEXT)
C
C         Between two northern latitudes or two southern latitudes of
C         original field, so use 4-point bilinear interpolation
C         (points 1, 2, 3 and 4)
C
C         Points 1 and 2
C
          NLONG(1) = NUMLON(RLON(NEXT),PLONINC)
          NLONG1   = NLONG(1)
          IF( NLONG(1).GT.NOLDLON ) NLONG(1) = 1
C
          NEIGH(1,NEXT) = (NLAT1-1)*NOLDLON + NLONG(1) + 1
          NEIGH(2,NEXT) = NEIGH(1,NEXT) + 1
C
C         Points 3 and 4
C
          NLONG( 3) = NLONG(1)
C
          NEIGH(3,NEXT) = (NLAT2-1)*NOLDLON + NLONG(3) + 1
          NEIGH(4,NEXT) = NEIGH(3,NEXT) + 1
C
C         Calculate zonal linear weights for the latitudes.
C
          PDLO1(NEXT) = (RLON(NEXT)-REAL(NLONG1-1)*PLONINC) / PLONINC
          PDLO2(NEXT) = PDLO1(NEXT)
C
          KLA(NEXT) = NLAT1
          PDLAT(NEXT) = (PLATIN(NLAT1,PLATINC)-RLAT(NEXT)) / PLATINC
C
          KSCHEME(NEXT) = JP4PT
          GOTO 700
        ENDIF
C
C     -----------------------------------------------------------------|
C       Section 6. 12-point interpolation to be used.
C     -----------------------------------------------------------------|
C
  600   CONTINUE
C
        KSCHEME(NEXT) = JP12PT
C
        KLA(NEXT) = NLAT1
        PDLAT(NEXT) = (PLATIN(NLAT1,PLATINC)-RLAT(NEXT)) / PLATINC
C
C       Northernmost
C
        NLAT(5) = NLAT0
        NLAT(6) = NLAT0
C
C       Northern
C
        NLAT(7) = NLAT1
        NLAT(1) = NLAT1
        NLAT(2) = NLAT1
        NLAT(8) = NLAT1
C
C       Southern
C
        NLAT( 9) = NLAT2
        NLAT( 3) = NLAT2
        NLAT( 4) = NLAT2
        NLAT(10) = NLAT2
C
C       Southernmost
C
        NLAT(11) = NLAT3
        NLAT(12) = NLAT3
C
C       Find longitude numbers for neighbours.
C
C       Northernmost
C
        NLONG(5) = NUMLON(RLON(NEXT),PLONINC)
        NLONG5 = NLONG(5)
        IF( NLONG(5).GT.NOLDLON ) NLONG(5) = 1
        NLONG(6) = NLONG(5) + 1
        IF( NLONG(6).GT.NOLDLON ) NLONG(6) = 1
C
C       Northern
C
        NLONG(1) = NLONG(5)
        NLONG(7) = NLONG(1) - 1
        IF( NLONG(7).LT.1 ) NLONG(7) = NOLDLON
        NLONG(2) = NLONG(1) + 1
        IF( NLONG(2).GT.NOLDLON ) NLONG(2) = 1
        NLONG(8) = NLONG(2) + 1
        IF( NLONG(8).GT.NOLDLON ) NLONG(8) = 1
C
C       Southern
C
        NLONG( 3) = NLONG(1)
        NLONG( 9) = NLONG(7)
        NLONG( 4) = NLONG(2)
        NLONG(10) = NLONG(8)
C
C       Southernmost
C
        NLONG(11) = NLONG(5)
        NLONG(12) = NLONG(11) + 1
        IF( NLONG(12).GT.NOLDLON ) NLONG(12) = 1
C
C       Calculate zonal linear weights for the latitudes.
C
        PDLO0(NEXT) = (RLON(NEXT)-REAL(NLONG5-1)*PLONINC) / PLONINC
        PDLO1(NEXT) = PDLO0(NEXT)
        PDLO2(NEXT) = PDLO0(NEXT)
        PDLO3(NEXT) = PDLO0(NEXT)
C
C       Store indices of the neighbours.
C
        DO LOOP = 1, 12
          NEIGH(LOOP,NEXT) = NOLDLON*(NLAT(LOOP)-1) + NLONG(LOOP)
        ENDDO
C
  700  CONTINUE
C
      ENDDO
C
C     End of loop along vector of points.
C
C     -----------------------------------------------------------------|
C     Section 9.  Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
