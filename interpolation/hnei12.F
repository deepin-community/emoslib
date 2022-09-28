C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HNEI12
     X  (L12PNT,KLEN,RLAT,RLON,KGAUSS,KPTS,PLATIN,
     X   KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
C
C---->
C**** HNEI12
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
C     IRET = HNEI12(L12PNT,KLEN,RLAT,RLON,KGAUSS,KPTS,PLATIN,
C    X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT - Chooses between 12-point and 4-point interpolation
C              = .TRUE. for 12-point horizontal
C              = .FALSE. for 4-point
C     KLEN   - Number of points along the vector
C     RLAT   - List of latitudes for points.
C     RLON   - List of longitudes for points.
C     KGAUSS - Original gaussian field number.
C     KPTS   - List giving number of points in each latitude in original
C              gaussian field.
C     PLATIN - Original gaussian field latitudes (north and south).
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
C     JNORSGG - Finds gaussian latitude north or south of given latitude
C     JDEBUG  - Tests if debug output required.
C     JMALLOC - Dynamically allocate memory
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
      INTEGER KLEN, KGAUSS, KPTS(KGAUSS*2)
      REAL RLAT(KLEN), RLON(KLEN), PLATIN(KGAUSS*2)
C
      INTEGER KLA(KLEN), KSCHEME(KLEN)
      REAL PDLAT(KLEN),PDLO0(KLEN),PDLO1(KLEN),PDLO2(KLEN),PDLO3(KLEN)
      INTEGER NEIGH(12,*)
C
C     Local variables
C
      INTEGER NEXT, LOOP, NBYTES
      INTEGER NLAT0, NLAT1, NLAT2, NLAT3, NLAT(12), NLONG(12)
      REAL STEP0, STEP1, STEP2, STEP3
      INTEGER NPREV, ISIZE, ISIZOLD, IOFFSET(1)
      POINTER( IPOFF, IOFFSET )
C
      DATA ISIZOLD/-1/, NPREV/-1/
      SAVE ISIZOLD,IPOFF,NPREV
C
C     Externals
C
      INTEGER JNORSGG
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
      HNEI12 = 0
C
      CALL JDEBUG()
C
C     Get memory for offset array.
C
      ISIZE = KGAUSS*2 + 1
      IF( ISIZE.GT.ISIZOLD ) THEN
C
        IF( ISIZOLD.GT.0 ) CALL JFREE(IPOFF)
C
        NBYTES = ISIZE * JPBYTES
        IPOFF = JMALLOC(NBYTES)
#ifdef hpR64
        IPOFF = IPOFF/(1024*1024*1024*4)
#endif
        IF( IPOFF.EQ.0 ) THEN
          CALL INTLOG(JP_WARN,'HNEI12: Memory allocate fail ',JPQUIET)
          HNEI12 = 1
          GOTO 900
        ENDIF
C
        ISIZOLD = ISIZE
        NPREV = -1
C
      ENDIF
C
C     Build up offsets to start of each latitude in the original field.
C
      IF( KGAUSS.NE.NPREV ) THEN
        IOFFSET(1) = 1
        DO LOOP = 2, (KGAUSS*2+1)
          IOFFSET(LOOP) = IOFFSET(LOOP-1) + KPTS(LOOP-1)
        ENDDO
        NPREV = KGAUSS
      ENDIF
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
C       Find latitude numbers for neighbours.
C
C       First find latitude numbers to north and south of the point
C
C     -----------------------------------------------------------------|
C       Section 3. Nearest neighbour to be used.
C     -----------------------------------------------------------------|
C
  300   CONTINUE
C
        IF( RLAT(NEXT).GT.PLATIN(1) ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'HNEI12: Nearest neighbour to south used for point ',NEXT)
C
C         Above northern latitude of original field, so use nearest
C         neighbour (point 3 or 4)
C
          STEP2 = 360.0 / REAL(KPTS(1))
          NLONG( 3) = 1 + INT(RLON(NEXT)/STEP2)
          IF( NLONG( 3).GT.KPTS(1) ) NLONG( 3) = 1
          NLONG( 4) = NLONG(3) + 1
          IF( NLONG( 4).GT.KPTS(1) ) NLONG( 4) = 1
C
          IF( ABS((RLON(NEXT) - REAL(NLONG(3)-1)*STEP2 )) .LE.
     X        ABS((RLON(NEXT) - REAL(NLONG(4)-1)*STEP2 )) ) THEN
            NEIGH(3,NEXT) = NLONG(3)
          ELSE
            NEIGH(4,NEXT) = NLONG(4)
          ENDIF
          KSCHEME(NEXT) = JPNEARN
          GOTO 600
        ENDIF
C
        IF( RLAT(NEXT).LT.PLATIN(KGAUSS*2) ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'HNEI12: Nearest neighbour to north used for point ',NEXT)
C
C         Below southern latitude of original field, so use nearest
C         neighbour (point 1 or 2)
C
          STEP1 = 360.0 / REAL(KPTS(KGAUSS*2))
          NLONG( 1) = 1 + INT(RLON(NEXT)/STEP1)
          IF( NLONG( 1).GT.KPTS(KGAUSS*2) ) NLONG( 1) = 1
          NLONG( 2) = NLONG(1) + 1
          IF( NLONG( 2).GT.KPTS(KGAUSS*2) ) NLONG( 2) = 1
C
          IF( ABS((RLON(NEXT) - REAL(NLONG(1)-1)*STEP1 )) .LE.
     X        ABS((RLON(NEXT) - REAL(NLONG(2)-1)*STEP1 )) ) THEN
            NEIGH(1,NEXT) = IOFFSET(KGAUSS*2) + NLONG(1) - 1
          ELSE
            NEIGH(2,NEXT) = IOFFSET(KGAUSS*2) + NLONG(2) - 1
          ENDIF
          KSCHEME(NEXT) = JPNEARN
          GOTO 600
        ENDIF
C
        NLAT1 = JNORSGG(RLAT(NEXT),PLATIN,KGAUSS,1)
        NLAT2 = JNORSGG(RLAT(NEXT),PLATIN,KGAUSS,0)
C
        IF( NLAT2.EQ.NLAT1) NLAT2 = NLAT1 + 1
C
C       Find outer latitude numbers to north and south of the point
C
        NLAT0 = NLAT1 - 1
        NLAT3 = NLAT2 + 1
C
C     -----------------------------------------------------------------|
C       Section 4. 4-point bilinear to be used.
C     -----------------------------------------------------------------|
C
  400   CONTINUE
C
        IF( (NLAT0.EQ.0).OR.(NLAT3.GT.KGAUSS*2).OR.(.NOT.L12PNT) ) THEN
Cjdc      CALL INTLOG(JP_DEBUG,
Cjdc X      'HNEI12: 4-pt bilinear interpolation used for point ',NEXT)
C
C         Between two northern latitudes or two southern latitudes of
C         original field, so use 4-point bilinear interpolation
C         (points 1, 2, 3 and 4)
C
C         Points 1 and 2
C
          STEP1 = 360.0 / REAL(KPTS(NLAT1))
          NLONG(1) = 1 + INT(RLON(NEXT)/STEP1)
          IF( NLONG(1).GT.KPTS(NLAT1) ) NLONG(1) = 1
          NLONG(2) = NLONG(1) + 1
          IF( NLONG(2).GT.KPTS(NLAT1) ) NLONG(2) = 1
C
          NEIGH(1,NEXT) = IOFFSET(NLAT1) + NLONG(1) - 1
          NEIGH(2,NEXT) = IOFFSET(NLAT1) + NLONG(2) - 1
C
C         Points 3 and 4
C
          STEP2 = 360.0 / REAL(KPTS(NLAT2))
          NLONG( 3) = 1 + INT(RLON(NEXT)/STEP2)
          IF( NLONG( 3).GT.KPTS(NLAT2) ) NLONG( 3) = 1
          NLONG( 4) = NLONG(3) + 1
          IF( NLONG( 4).GT.KPTS(NLAT2) ) NLONG( 4) = 1
C
          NEIGH(3,NEXT) = IOFFSET(NLAT2) + NLONG(3) - 1
          NEIGH(4,NEXT) = IOFFSET(NLAT2) + NLONG(4) - 1
C
C         Calculate zonal linear weights for the latitudes.
C
          PDLO1(NEXT) = ( RLON(NEXT) - REAL(NLONG(1)-1)*STEP1 ) / STEP1
          PDLO2(NEXT) = ( RLON(NEXT) - REAL(NLONG(3)-1)*STEP2 ) / STEP2
C
          KLA(NEXT) = NLAT1
          PDLAT(NEXT) = ( RLAT(NEXT) - PLATIN(NLAT1) ) /
     X                  ( PLATIN(NLAT2) - PLATIN(NLAT1) )
C
          KSCHEME(NEXT) = JP4PT
          GOTO 600
        ENDIF
C
C     -----------------------------------------------------------------|
C       Section 5. 12-point interpolation to be used.
C     -----------------------------------------------------------------|
C
  500   CONTINUE
C
        KSCHEME(NEXT) = JP12PT
C
        KLA(NEXT) = NLAT1
        PDLAT(NEXT) = ( RLAT(NEXT) - PLATIN(NLAT1) ) /
     X                ( PLATIN(NLAT2) - PLATIN(NLAT1) )
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
C     -----------------------------------------------------------------|
C       Find longitude numbers for neighbours.
C     -----------------------------------------------------------------|
C
C       Northernmost
C
        STEP0 = 360.0 / REAL(KPTS(NLAT0))
        NLONG(5) = 1 + INT(RLON(NEXT)/STEP0)
        IF( NLONG(5).GT.KPTS(NLAT0) ) NLONG(5) = 1
        NLONG(6) = NLONG(5) + 1
        IF( NLONG(6).GT.KPTS(NLAT0) ) NLONG(6) = 1
C
C       Northern
C
        STEP1 = 360.0 / REAL(KPTS(NLAT1))
        NLONG(1) = 1 + INT(RLON(NEXT)/STEP1)
        IF( NLONG(1).GT.KPTS(NLAT1) ) NLONG(1) = 1
        NLONG(7) = NLONG(1) - 1
        IF( NLONG(7).LT.1 ) NLONG(7) = KPTS(NLAT1)
        NLONG(2) = NLONG(1) + 1
        IF( NLONG(2).GT.KPTS(NLAT1) ) NLONG(2) = 1
        NLONG(8) = NLONG(2) + 1
        IF( NLONG(8).GT.KPTS(NLAT1) ) NLONG(8) = 1
C
C       Southern
C
        STEP2 = 360.0 / REAL(KPTS(NLAT2))
        NLONG( 3) = 1 + INT(RLON(NEXT)/STEP2)
        IF( NLONG( 3).GT.KPTS(NLAT2) ) NLONG( 3) = 1
        NLONG( 9) = NLONG(3) - 1
        IF( NLONG( 9).LT.1 ) NLONG( 9) = KPTS(NLAT2)
        NLONG( 4) = NLONG(3) + 1
        IF( NLONG( 4).GT.KPTS(NLAT2) ) NLONG( 4) = 1
        NLONG(10) = NLONG(4) + 1
        IF( NLONG(10).GT.KPTS(NLAT2) ) NLONG(10) = 1
C
C       Southernmost
C
        STEP3 = 360.0 / REAL(KPTS(NLAT3))
        NLONG(11) = 1 + INT(RLON(NEXT)/STEP3)
        IF( NLONG(11).GT.KPTS(NLAT3) ) NLONG(11) = 1
        NLONG(12) = NLONG(11) + 1
        IF( NLONG(12).GT.KPTS(NLAT3) ) NLONG(12) = 1
C
C     -----------------------------------------------------------------|
C       Calculate zonal linear weights for the latitudes.
C     -----------------------------------------------------------------|
C
        PDLO0(NEXT) = ( RLON(NEXT) - REAL(NLONG(5)-1)*STEP0 ) / STEP0
C
        PDLO1(NEXT) = ( RLON(NEXT) - REAL(NLONG(1)-1)*STEP1 ) / STEP1
C
        PDLO2(NEXT) = ( RLON(NEXT) - REAL(NLONG(3)-1)*STEP2 ) / STEP2
C
        PDLO3(NEXT) = ( RLON(NEXT) - REAL(NLONG(11)-1)*STEP3 ) / STEP3
C
C     -----------------------------------------------------------------|
C       Store indices of the neighbours.
C     -----------------------------------------------------------------|
C
        DO LOOP = 1, 12
          NEIGH(LOOP,NEXT) = IOFFSET(NLAT(LOOP)) + NLONG(LOOP) - 1
        ENDDO
C
C     -----------------------------------------------------------------|
C     Section 6.  End of loop along vector of points.
C     -----------------------------------------------------------------|
C
  600   CONTINUE
C
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
