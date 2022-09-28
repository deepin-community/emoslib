C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE HWTSLSM
     X  (KLEN,KSCHEME,KLA,PDLAT,PLATIN,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
     X   LSMOLD,LSMNEW,PWTS)
C
C---->
C**** HWTSLSM
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
C     CALL HWTSLSM
C    X  (KLEN,KSCHEME,KLA,PDLAT,PLATIN,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
C    X   LSMOLD,LSMNEW,PWTS)
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
C     LSMOLD  - Array of land-sea mask values ( a reduced gaussian
C               field which matches the reduced gaussian field to be
C               used for interpolation)
C     LSMNEW  - Vector of land-sea mask values for the input points
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
C     J.D.Chambers      ECMWF      March 2001
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
      REAL LSMNEW(KLEN)
      INTEGER NEIGH(12,*)
      REAL LSMOLD(*),PWTS(12,*)
C
C     Local variables
C
      INTEGER NEXT, LOOP
      INTEGER ILA0G, ILA1G, ILA2G, ILA3G
      INTEGER IMKOUT, IMKIN1, IMKIN2, IMKIN3, IMKIN4, IMKIN5, IMKIN6
      INTEGER IMKIN7, IMKIN8, IMKIN9, IMKIN10, IMKIN11, IMKIN12
      REAL ZDLAT, ZDY, ZDY10, ZDY21, ZDY32
      REAL ZDLO, ZDLO1, ZDLO2, ZDLO3, ZWXN0, ZWXN1, ZWXN2, ZWXN3
      REAL ZWY0, ZWY1, ZWY2, ZWY3
      REAL ZWXNN, ZCXNN, ZWXSS, ZCXSS
      REAL ZWXS0, ZWXS1, ZWXS2, ZWXS3
      REAL ZWY, ZCY, ZWXN, ZCXN, ZWXS, ZCXS
      REAL ZWLSI1, ZWLSI2, ZWLSI3, ZWLSI4, ZWLSI5, ZWLSI6
      REAL ZWLSI7, ZWLSI8, ZWLSI9, ZWLSI10, ZWLSI11, ZWLSI12
      REAL ZWLSI13, ZWLSI14, ZWLSI15, ZWLSI16, ZWLSI17
      REAL ZEPSIL
      DATA ZEPSIL/1.E-5/
C
C     Inline functions
C
      REAL A, B, ADJUST
      REAL ALPHA, BETA, GAMMA, DELTA, EPSILON, F2, F3, F4
C
      ADJUST(A,B) = 1.0 - INT(B + ZEPSIL)*(1.0 - A)
C
      F2(ALPHA,BETA,GAMMA,DELTA,EPSILON) =
     X  ALPHA * ((1.0 + BETA*GAMMA) *
     X  (1.0 + DELTA*(GAMMA-2.0)) *
     X  (1.0 + EPSILON*(GAMMA-3.0))) /
     X  ((1.0 - 2.0*DELTA) * (1.0 - 3.0*EPSILON))
C
      F3(ALPHA,BETA,GAMMA,DELTA,EPSILON) =
     X  ALPHA * ((1.0 + BETA*GAMMA) *
     X  (1.0 + DELTA*(GAMMA - 1.0)) *
     X  (1.0 + EPSILON*(GAMMA - 3.0))) /
     X  ((1.0 + BETA) * (1.0 - 2.0*EPSILON))
C
      F4(ALPHA,BETA,GAMMA,DELTA,EPSILON) =
     X  ALPHA * ((1.0 + BETA*GAMMA) *
     X  (1.0 + DELTA*(GAMMA - 1.0)) *
     X  (1.0 + EPSILON*(GAMMA - 2.0))) /
     X  ((1.0 + 2.0*BETA) * (1.0 + DELTA))
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
C
C       Nearest point selection
C
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
C
C       4-point interpolation
C
C     -----------------------------------------------------------------|
C
        ELSE IF( KSCHEME(NEXT).EQ.JP4PT ) THEN
C
C     -----------------------------------------------------------------|
C         Initialise the lsm weights
C     -----------------------------------------------------------------|
C
          ZWLSI1  = 1.0
          ZWLSI2  = 1.0
          ZWLSI3  = 1.0
          ZWLSI4  = 1.0
C
          ZWLSI14 = 0.0
          ZWLSI15 = 0.0
          ZWLSI17 = 0.0
C
C         Modify the weights according to whether or not the target
C         point matches its neighbours
C
          IMKOUT = NINT(LSMNEW(NEXT))
C
          IMKIN1 = NINT(LSMOLD(NEIGH(1,NEXT)))
          IF( IMKIN1.NE.IMKOUT ) ZWLSI1 = 0.0
C
          IMKIN2 = NINT(LSMOLD(NEIGH(2,NEXT)))
          IF( IMKIN2.NE.IMKOUT ) ZWLSI2 = 0.0
C
          IMKIN3 = NINT(LSMOLD(NEIGH(3,NEXT)))
          IF( IMKIN3.NE.IMKOUT ) ZWLSI3 = 0.0
C
          IMKIN4 = NINT(LSMOLD(NEIGH(4,NEXT)))
          IF( IMKIN4.NE.IMKOUT ) ZWLSI4 = 0.0
C
          IF( IMKIN1.NE.IMKOUT) ZWLSI1 = 0.0
          IF( IMKIN2.NE.IMKOUT) ZWLSI2 = 0.0
          IF( IMKIN3.NE.IMKOUT) ZWLSI3 = 0.0
          IF( IMKIN4.NE.IMKOUT) ZWLSI4 = 0.0
C
          ZWLSI14 = MIN(ZWLSI1 + ZWLSI2, 1.0)
          ZWLSI15 = MIN(ZWLSI3 + ZWLSI4, 1.0)
          ZWLSI17 = MIN(ZWLSI14 + ZWLSI15, 1.0)
C
          ZWLSI1 = ADJUST(ZWLSI1,ZWLSI14)
          ZWLSI2 = ADJUST(ZWLSI2,ZWLSI14)
          ZWLSI3 = ADJUST(ZWLSI3,ZWLSI15)
          ZWLSI4 = ADJUST(ZWLSI4,ZWLSI15)
C
          ZWLSI14 = ADJUST(ZWLSI17,ZWLSI14)
          ZWLSI15 = ADJUST(ZWLSI17,ZWLSI15)
C
C     -----------------------------------------------------------------|
C         Weights for bilinear interpolation.
C     -----------------------------------------------------------------|
C
      
          ZWY = ZWLSI15 * (1.0 + ZWLSI14 * (PDLAT(NEXT) - 1.0))
          ZCY = 1.0 - ZWY
C
          ZWXN = ZWLSI2 * (1.0 + ZWLSI1 * (PDLO1(NEXT) - 1.0))
          ZCXN = 1.0 - ZWXN
C
          ZWXS = ZWLSI4 * (1.0 + ZWLSI3 * (PDLO2(NEXT) - 1.0))
          ZCXS = 1.0 - ZWXS
C
          PWTS(1,NEXT) = ZCXN * ZCY
          PWTS(2,NEXT) = ZWXN * ZCY
          PWTS(3,NEXT) = ZCXS * ZWY
          PWTS(4,NEXT) = ZWXS * ZWY
C
          GOTO 900
C
C     -----------------------------------------------------------------|
C
C       12-point interpolation
C
C     -----------------------------------------------------------------|
C
        ELSE
C
C     -----------------------------------------------------------------|
C         Initialise the lsm weights
C     -----------------------------------------------------------------|
C
          ZWLSI1  = 1.0
          ZWLSI2  = 1.0
          ZWLSI3  = 1.0
          ZWLSI4  = 1.0
          ZWLSI5  = 1.0
          ZWLSI6  = 1.0
          ZWLSI7  = 1.0
          ZWLSI8  = 1.0
          ZWLSI9  = 1.0
          ZWLSI10 = 1.0
          ZWLSI11 = 1.0
          ZWLSI12 = 1.0
C
          ZWLSI13 = 0.0
          ZWLSI14 = 0.0
          ZWLSI15 = 0.0
          ZWLSI16 = 0.0
          ZWLSI17 = 0.0
C
C         Modify the weights according to whether or not the target
C         point matches its neighbours
C
          IMKOUT = NINT(LSMNEW(NEXT))
C
          IMKIN1 = NINT(LSMOLD(NEIGH(1,NEXT)))
          IF( IMKIN1.NE.IMKOUT ) ZWLSI1 = 0.0
C
          IMKIN2 = NINT(LSMOLD(NEIGH(2,NEXT)))
          IF( IMKIN2.NE.IMKOUT ) ZWLSI2 = 0.0
C
          IMKIN3 = NINT(LSMOLD(NEIGH(3,NEXT)))
          IF( IMKIN3.NE.IMKOUT ) ZWLSI3 = 0.0
C
          IMKIN4 = NINT(LSMOLD(NEIGH(4,NEXT)))
          IF( IMKIN4.NE.IMKOUT ) ZWLSI4 = 0.0
C
          IMKIN5 = NINT(LSMOLD(NEIGH(5,NEXT)))
          IF( IMKIN5.NE.IMKOUT ) ZWLSI5 = 0.0
C
          IMKIN6 = NINT(LSMOLD(NEIGH(6,NEXT)))
          IF( IMKIN6.NE.IMKOUT ) ZWLSI6 = 0.0
C
          IMKIN7 = NINT(LSMOLD(NEIGH(7,NEXT)))
          IF( IMKIN7.NE.IMKOUT ) ZWLSI7 = 0.0
C
          IMKIN8 = NINT(LSMOLD(NEIGH(8,NEXT)))
          IF( IMKIN8.NE.IMKOUT ) ZWLSI8 = 0.0
C
          IMKIN9 = NINT(LSMOLD(NEIGH(9,NEXT)))
          IF( IMKIN9.NE.IMKOUT ) ZWLSI9 = 0.0
C
          IMKIN10 = NINT(LSMOLD(NEIGH(10,NEXT)))
          IF( IMKIN10.NE.IMKOUT ) ZWLSI10 = 0.0
C
          IMKIN11 = NINT(LSMOLD(NEIGH(11,NEXT)))
          IF( IMKIN11.NE.IMKOUT ) ZWLSI11 = 0.0
C
          IMKIN12 = NINT(LSMOLD(NEIGH(12,NEXT)))
          IF( IMKIN12.NE.IMKOUT ) ZWLSI12 = 0.0
C
          ZWLSI13 = MIN(ZWLSI5+ZWLSI6                  ,1.0)
          ZWLSI14 = MIN(ZWLSI7+ZWLSI1+ZWLSI2+ZWLSI8    ,1.0)
          ZWLSI15 = MIN(ZWLSI9+ZWLSI3+ZWLSI4+ZWLSI10   ,1.0)
          ZWLSI16 = MIN(ZWLSI11+ZWLSI12                ,1.0)
          ZWLSI17 = MIN(ZWLSI13+ZWLSI14+ZWLSI15+ZWLSI16,1.0)
C
          ZWLSI1  = ADJUST(ZWLSI1,ZWLSI14)
          ZWLSI2  = ADJUST(ZWLSI2,ZWLSI14)
          ZWLSI3  = ADJUST(ZWLSI3,ZWLSI15)
          ZWLSI4  = ADJUST(ZWLSI4,ZWLSI15)
          ZWLSI5  = ADJUST(ZWLSI5,ZWLSI13)
          ZWLSI6  = ADJUST(ZWLSI6,ZWLSI13)
          ZWLSI7  = ADJUST(ZWLSI7,ZWLSI14)
          ZWLSI8  = ADJUST(ZWLSI8,ZWLSI14)
          ZWLSI9  = ADJUST(ZWLSI9,ZWLSI15)
          ZWLSI10 = ADJUST(ZWLSI10,ZWLSI15)
          ZWLSI11 = ADJUST(ZWLSI11,ZWLSI16)
          ZWLSI12 = ADJUST(ZWLSI12,ZWLSI16)
C
          ZWLSI13 = ADJUST(ZWLSI13,ZWLSI17)
          ZWLSI14 = ADJUST(ZWLSI14,ZWLSI17)
          ZWLSI15 = ADJUST(ZWLSI15,ZWLSI17)
          ZWLSI16 = ADJUST(ZWLSI16,ZWLSI17)
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
          ZWXN1 = F2(ZWLSI1,ZWLSI7,ZDLO1,ZWLSI2,ZWLSI8)
          ZWXN2 = F3(ZWLSI2,ZWLSI7,ZDLO1,ZWLSI1,ZWLSI8)
          ZWXN3 = F4(ZWLSI8,ZWLSI7,ZDLO1,ZWLSI1,ZWLSI2)
          ZWXN0 = 1.0 - ZWXN1 - ZWXN2 - ZWXN3
C
C         Southern parallel
C
          ZDLO2 = PDLO2(NEXT)
          ZWXS1 = F2(ZWLSI3,ZWLSI9,ZDLO2,ZWLSI4,ZWLSI10)
          ZWXS2 = F3(ZWLSI4,ZWLSI9,ZDLO2,ZWLSI3,ZWLSI10)
          ZWXS3 = F4(ZWLSI10,ZWLSI9,ZDLO2,ZWLSI3,ZWLSI4)
          ZWXS0 = 1.0 - ZWXS1 - ZWXS2 - ZWXS3
C
C     -----------------------------------------------------------------|
C         Polynomial in y-direction.
C     -----------------------------------------------------------------|
C
          ZWY1 = ZWLSI14*((1.0 + ZWLSI13*(ZDY + ZDY10 - 1.0)) *
     X           (1.0 + ZWLSI15*(ZDY - ZDY21 - 1.0)) *
     X           (1.0 + ZWLSI16*(ZDY - ZDY21 - ZDY32 - 1.0))) /
     X           ((1.0 + ZWLSI13*(ZDY10 - 1.0)) *
     X           (1.0 + ZWLSI15*( -ZDY21 - 1.0)) *
     X           (1.0 + ZWLSI16*( -ZDY21 - ZDY32 - 1.0)))
          ZWY2 = ZWLSI15*((1.0 + ZWLSI13*(ZDY + ZDY10 - 1.0)) *
     X           (1.0 + ZWLSI14*(ZDY - 1.0)) *
     X           (1.0 + ZWLSI16*(ZDY - ZDY21 - ZDY32 - 1.0))) /
     X           ((1.0 + ZWLSI13*(ZDY10 + ZDY21 - 1.0)) *
     X           (1.0 + ZWLSI14*(ZDY21 - 1.0)) *
     X           (1.0 + ZWLSI16*( -ZDY32 - 1.0)))
          ZWY3 = ZWLSI16*((1.0 + ZWLSI13*(ZDY + ZDY10 - 1.0)) *
     X           (1.0 + ZWLSI14*(ZDY - 1.0)) *
     X           (1.0 + ZWLSI15*(ZDY - ZDY21 - 1.0))) /
     X           ((1.0 + ZWLSI13*(ZDY10 + ZDY21 + ZDY32 - 1.0)) *
     X           (1.0 + ZWLSI14*(ZDY21 + ZDY32 - 1.0)) *
     X           (1.0 + ZWLSI15*(ZDY32 - 1.0)))
          ZWY0 = 1.0 - ZWY1 - ZWY2 - ZWY3
C
C     -----------------------------------------------------------------|
C         Linear parts for extreme rows.
C     -----------------------------------------------------------------|
C
C         Northernmost
C
          ZDLO  = PDLO0(NEXT)
          ZWXNN = ZWLSI6 * (1.0 + ZWLSI5 * (ZDLO - 1.0))
          ZCXNN = 1.0 - ZWXNN
C
C         Southernmost
C
          ZDLO3 = PDLO3(NEXT)
          ZWXSS = ZWLSI12 * (1.0 + ZWLSI11 * (ZDLO3 - 1.0))
          ZCXSS = 1.0 - ZWXSS
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
