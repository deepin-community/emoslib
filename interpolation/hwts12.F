C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE HWTS12
     X  (KLEN,KSCHEME,KLA,PDLAT,PLATIN,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
     X   PWTS)
C
C---->
C**** HWTS12
C
C     Purpose
C     -------
C
C     This routine accepts a vector of points and calculates the
C     interpolation weightings for each point suitable for the
C     horizontal interpolation.
C
C
C     Interface
C     ---------
C
C     CALL HWTS12
C    X  (KLEN,KSCHEME,KLA,PDLAT,PLATIN,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
C    X   PWTS)
C
C
C     Input parameters
C     ----------------
C
C     KLEN    - Number of points along the vector.
C     KSCHEME - Flag showing interpolation scheme to use for point
C               0 = 12-point
C               1 = 4-point bilinear
C               2 = nearest neighbour
C     KLA     - Latitude number in original field of latitude north of
C               point in the vector.
C     PDLAT   - Meridian linear weight.
C     PLATIN  - Gaussian latitudes.
C     PDLO0   - Zonal linear weight for the latitude of point 5.
C     PDLO1   - Zonal linear weight for the latitude of point 1.
C     PDLO2   - Zonal linear weight for the latitude of point 3.
C     PDLO3   - Zonal linear weight for the latitude of point 11.
C     NEIGH   - List of indices in the original field of neighbouring
C               point values.
C
C
C     Output parameters
C     -----------------
C
C     PWTS   - Weights for interpolation.
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
C
C     Externals
C     ---------
C
C     None.
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
C*    Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
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
      INTEGER KLEN, KSCHEME(KLEN), KLA(KLEN)
      REAL PDLAT(KLEN), PLATIN(*)
      REAL PDLO0(KLEN), PDLO1(KLEN), PDLO2(KLEN), PDLO3(KLEN)
      INTEGER NEIGH(12,*)
      REAL PWTS(12,*)
C
C     Local variables
C
      INTEGER NEXT, LOOP
      INTEGER ILA0G, ILA1G, ILA2G, ILA3G
      REAL ZDLAT, ZDY, ZDY10, ZDY21, ZDY32
      REAL ZDLO1, ZDLO2, ZWXN0, ZWXN1, ZWXN2, ZWXN3
      REAL ZWY0, ZWY1, ZWY2, ZWY3
      REAL ZWXNN, ZCXNN, ZWXSS, ZCXSS
      REAL ZWXS0, ZWXS1, ZWXS2, ZWXS3
C
C     Inline functions
C
      REAL ALPHA, F2, F3, F4
C
      F2(ALPHA) = ((ALPHA+1.0) * (ALPHA-2.0) * (ALPHA-1.0))/2.0
C
      F3(ALPHA) = - ((ALPHA+1.0) * (ALPHA-2.0) * ALPHA)/2.0
C
      F4(ALPHA) = (ALPHA * (ALPHA-1.0) * (ALPHA+1.0))/6.0
C
C     -----------------------------------------------------------------|
C     Process each point in the vector.
C     -----------------------------------------------------------------|
C
      DO NEXT = 1,KLEN
C
C       Clear the weights for the current point
C
        DO LOOP = 1, 12
          PWTS(LOOP,NEXT) = 0.0
        ENDDO
C
C       Use appropriate interpolation scheme for the current point
C
C     -----------------------------------------------------------------|
C       Nearest point selection
C     -----------------------------------------------------------------|
C
        IF( KSCHEME(NEXT).EQ.JPNEARN ) THEN
C
          DO LOOP = 1, 4
            IF( NEIGH(LOOP,NEXT).NE.0 ) PWTS(LOOP,NEXT) = 1.0
          ENDDO
C
          GOTO 900
C
C     -----------------------------------------------------------------|
C       4-point interpolation
C     -----------------------------------------------------------------|
C
        ELSE IF( KSCHEME(NEXT).EQ.JP4PT ) THEN
C
          PWTS(1,NEXT) = (1.0 - PDLO2(NEXT)) * (1.0 - PDLAT(NEXT))
          PWTS(2,NEXT) = PDLO2(NEXT) * (1.0 - PDLAT(NEXT))
          PWTS(3,NEXT) = (1.0 - PDLO1(NEXT)) * PDLAT(NEXT)
          PWTS(4,NEXT) = PDLO1(NEXT) * PDLAT(NEXT)
C
          GOTO 900
C
C     -----------------------------------------------------------------|
C       12-point interpolation
C     -----------------------------------------------------------------|
C
        ELSE
C
C         Setup latitude numbers for the 4 rows.
C
          ILA1G = KLA(NEXT)
          ILA0G = ILA1G - 1
          ILA2G = ILA1G + 1
          ILA3G = ILA1G + 2
C
C         Setup the weights between rows.
C
          ZDLAT = PDLAT(NEXT)
          ZDY   = ZDLAT * (PLATIN(ILA2G) - PLATIN(ILA1G))
          ZDY10 =          PLATIN(ILA1G) - PLATIN(ILA0G)
          ZDY21 =          PLATIN(ILA2G) - PLATIN(ILA1G)
          ZDY32 =          PLATIN(ILA3G) - PLATIN(ILA2G)
C
C     -----------------------------------------------------------------|
C         Polynomial in x-direction.
C     -----------------------------------------------------------------|
C
C         Northern parallel
C
          ZDLO1 = PDLO1(NEXT)
          ZWXN1 = F2(ZDLO1)
          ZWXN2 = F3(ZDLO1)
          ZWXN3 = F4(ZDLO1)
          ZWXN0 = 1.0 - ZWXN1 - ZWXN2 - ZWXN3
C
C         Southern parallel
C
          ZDLO2 = PDLO2(NEXT)
          ZWXS1 = F2(ZDLO2)
          ZWXS2 = F3(ZDLO2)
          ZWXS3 = F4(ZDLO2)
          ZWXS0 = 1.0 - ZWXS1 - ZWXS2 - ZWXS3
C
C     -----------------------------------------------------------------|
C         Polynomial in y-direction.
C     -----------------------------------------------------------------|
C
          ZWY3 = ((ZDY+ZDY10) * (ZDY) * (ZDY-ZDY21)) /
     X           ((ZDY10+ZDY21+ZDY32) * (ZDY21+ZDY32) * (ZDY32))
          ZWY2 = ((ZDY+ZDY10) * (ZDY) * (ZDY-ZDY21-ZDY32)) /
     X           ((ZDY10+ZDY21) * (ZDY21) * (-ZDY32))
          ZWY1 = ((ZDY+ZDY10) * (ZDY-ZDY21) * (ZDY-ZDY21-ZDY32)) /
     X           ((ZDY10) * (-ZDY21) * (-ZDY21-ZDY32))
          ZWY0 = 1.0 - ZWY1 - ZWY2 - ZWY3
C
C     -----------------------------------------------------------------|
C         Linear parts for extreme rows.
C     -----------------------------------------------------------------|
C
C         Northernmost
C
          ZWXNN = PDLO0(NEXT)
          ZCXNN = 1.0 - PDLO0(NEXT)
C
C         Southernmost
C
          ZWXSS = PDLO3(NEXT)
          ZCXSS = 1.0 - PDLO3(NEXT)
C
C     -----------------------------------------------------------------|
C         Weights for 12 points interpolation.
C     -----------------------------------------------------------------|
C
          PWTS( 1,NEXT) = ZWXN1 * ZWY1
          PWTS( 2,NEXT) = ZWXN2 * ZWY1
          PWTS( 3,NEXT) = ZWXS1 * ZWY2
          PWTS( 4,NEXT) = ZWXS2 * ZWY2
          PWTS( 5,NEXT) = ZCXNN * ZWY0
          PWTS( 6,NEXT) = ZWXNN * ZWY0
          PWTS( 7,NEXT) = ZWXN0 * ZWY1
          PWTS( 8,NEXT) = ZWXN3 * ZWY1
          PWTS( 9,NEXT) = ZWXS0 * ZWY2
          PWTS(10,NEXT) = ZWXS3 * ZWY2
          PWTS(11,NEXT) = ZCXSS * ZWY3
          PWTS(12,NEXT) = ZWXSS * ZWY3
C
          GOTO 900
C
        ENDIF
C
C     -----------------------------------------------------------------|
C       End of processing for current point in vector.
C     -----------------------------------------------------------------|
C
  900   CONTINUE
C
      ENDDO
C
      RETURN
      END
