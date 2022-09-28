C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION KINTRG(OLDGEO,NEWGEO,OLDFLD,NEWFLD)
C
C---->
C**** KINTRG
C
C     Purpose
C     -------
C     Interpolate a reduced gaussian field a reduced gaussian field
C     with a different resolution.
C
C
C     Interface
C     ---------
C     IRET = KINTRG(OLDGEO,NEWGEO,OLDFLD,NEWFLD)
C
C     Input
C     -----
C     OLDGEO - GRIB section 2 describing grid of old field.
C     NEWGEO - GRIB section 2 describing grid of new field.
C     OLDFLD - Array of values for old field.
C
C
C     Output
C     ------
C     NEWFLD - Array of values for new field.
C
C     Function returns:
C       - number of points in the new field, if all is well
C       - negative, otherwise.
C
C
C     Method
C     ------
C     Build up field offsets from input geometries.
C     For each point of the new field, find in the old field the four
C     nearest neighbours' positions and values.
C     Calculate new value from the neighbours.
C
C
C     Externals
C     ---------
C
C     IGGLAT  - Compute gaussian lines of latitude
C     JNORSGG - Find gaussian latitudes to north and south of latitude
C     KNFROM4 - Calculate value of new field point
C     INTLOG  - Logs messages
C     FORCED_NEAREST_NEIGHBOUR - check forced interpolation method
C
C
C     Author
C     ------
C     J.D.Chambers     ECMWF     November 2000
C
C     Modifications
C     -------------
C
C     Force nearest neighbour processing with env variable or
C     INTOUT parameter
C     S.Curic           ECMWF    September 2005
C
C     Add fudging factor for computation of index in original field
C     S.Curic           ECMWF    October 2006
C
C     Leave comments after adding support for octahedral grids
C     S.Siemen          ECMWF    April 2015
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER OLDGEO(*),NEWGEO(*)
      REAL OLDFLD(*),NEWFLD(*)
C
#include "intisl.h"
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Local variables
C
      LOGICAL LVEGGY
      INTEGER TOTAL, NEXT, LOOP, NEWOFF(JPMAXNG), OLDOFF(JPMAXNG)
      INTEGER LATIT, LONG, IRET, NPTS
      INTEGER LAT(2), LON(4)
      INTEGER PT(4)
      REAL OLAT(2), OLON(4)
      REAL RLATOLD(JPMAXNG), RLATNEW(JPMAXNG), RLAT, RLON
C
C     Externals
C
      LOGICAL FORCED_NEAREST_NEIGHBOUR
      INTEGER IGGLAT, JNORSGG
      REAL KNFROM4
      EXTERNAL IGGLAT, JNORSGG, KNFROM4
C
C     -----------------------------------------------------------------|
C*    Section 1.   Build working values using input geometries.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      KINTRG = 0
C
      IF( (NEWGEO(NGAUSS)*2).GT.JPMAXNG ) THEN
        CALL INTLOG(JP_ERROR,
     X    'KINTRG: Gaussian N too big = ',NEWGEO(NGAUSS))
        CALL INTLOG(JP_ERROR,'KINTRG: Maximum handled = ',(JPMAXNG/2))
        KINTRG = -1
        GOTO 900
      ENDIF
C
cs   set global for grib_api
      LGLOBL = .TRUE.
cs
C     Use nearest neighbour if required
      LVEGGY = FORCED_NEAREST_NEIGHBOUR(LMETHOD,NITABLE,NIPARAM)
      IF( LVEGGY ) CALL INTLOG(JP_DEBUG,
     X  'KINTRG: nearest neighbour processing',JPQUIET)
C
C     -----------------------------------------------------------------|
C*    Section 2.   Calculate number of points in new field and offset
C                  to start of each latitude in the new grid
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      NEWOFF(1) = 0
      TOTAL = NEWGEO(NPOINTS)
C
C     For all latitudes ...
C
      DO LOOP = 2, NEWGEO(NJ)
        NEWOFF(LOOP) = NEWOFF(LOOP-1) + NEWGEO(NPOINTS-2+LOOP)
        TOTAL = TOTAL + NEWGEO(NPOINTS-1+LOOP)
      ENDDO
C
C     Get the gaussian latitudes for the new field
C
      IRET = IGGLAT(NEWGEO(NGAUSS)*2,RLATNEW,0,-1)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KINTRG: Problem call igglat for new grid'
        KINTRG = -1
        RETURN
      ENDIF
C
      CALL INTLOG(JP_DEBUG,'KINTRG: No. of pts in new field = ',TOTAL)
C
C     -----------------------------------------------------------------|
C*    Section 3.   Get the gaussian latitudes for the old field and
C                  setup the offsets to the start of each latitude.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      OLDOFF(1) = 0
      DO LOOP = 2, OLDGEO(NJ)
        OLDOFF(LOOP) = OLDOFF(LOOP-1) + OLDGEO(NPOINTS-2+LOOP)
      ENDDO
C
      IRET = IGGLAT(OLDGEO(NGAUSS)*2,RLATOLD,0,-1)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KINTRG: Problem call igglat for old grid'
        KINTRG = -1
        RETURN
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Work through the points in the new field.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      DO NEXT = 1, TOTAL
C
C       Calculate lat/long
C
C        For all latitudes ...
C
        DO LOOP = 1, NEWGEO(NJ)
          IF( NEWOFF(LOOP).GE.NEXT ) THEN
            LATIT = LOOP - 1
            GOTO 410
          ENDIF
        ENDDO
        LATIT = NEWGEO(NJ)
C
  410   CONTINUE
C
        LONG = NEXT - NEWOFF(LATIT)
C
        RLAT = RLATNEW(LATIT)
        RLON = (REAL(LONG-1)*360.0)/REAL(NEWGEO(NPOINTS+LATIT-1))
C
C       Find four neighbours in the old field with their types
C       (Find NW neighbour and deduce the others).
C
        LAT(NORTH) = JNORSGG(RLAT,RLATOLD,OLDGEO(NGAUSS),1)
        LAT(SOUTH) = JNORSGG(RLAT,RLATOLD,OLDGEO(NGAUSS),0)
C
        OLAT(NORTH) = RLATOLD(LAT(NORTH))
        OLAT(SOUTH) = RLATOLD(LAT(SOUTH))
C
C
C
C
CCCCCCCCCCCCCCCCC  N O R T H  CCCCCCCCCCCCCCCC
C
C       How many points in a row
C
        NPTS = OLDGEO(NPOINTS-1+LAT(NORTH))
C
C       How far in is the longitude
C
        LON(NWEST) = 1 + INT(RLON/(360.0/REAL(NPTS)))
        LON(NEAST) = LON(NWEST) + 1
        IF( LON(NEAST).GT.NPTS ) LON(NEAST) = 1
C
C       Longitude in degree
C
        OLON(NWEST) = (REAL(LON(NWEST)-1)*360.0)/REAL(NPTS)
        IF( LON(NEAST).EQ.1 ) THEN
          OLON(NEAST) = 360.0
        ELSE
          OLON(NEAST) = (REAL(LON(NEAST)-1)*360.0)/REAL(NPTS)
        ENDIF
C
CCCCCCCCCCCCCCCCC  S O U T H  CCCCCCCCCCCCCCCC
C
        NPTS = OLDGEO(NPOINTS-1+LAT(SOUTH))

        LON(SWEST) = 1 + INT(RLON/(360.0/REAL(NPTS)))

        LON(SEAST) = LON(SWEST) + 1
        IF( LON(SEAST).GT.NPTS ) LON(SEAST) = 1
C
        OLON(SWEST) = (REAL(LON(SWEST)-1)*360.0)/REAL(NPTS)
        IF( LON(SEAST).EQ.1 ) THEN
          OLON(SEAST) = 360.0
        ELSE
          OLON(SEAST) = (REAL(LON(SEAST)-1)*360.0)/REAL(NPTS)
        ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        PT(NWEST) = OLDOFF(LAT(NORTH)) + LON(NWEST)
        PT(NEAST) = OLDOFF(LAT(NORTH)) + LON(NEAST)
        PT(SWEST) = OLDOFF(LAT(SOUTH)) + LON(SWEST)
        PT(SEAST) = OLDOFF(LAT(SOUTH)) + LON(SEAST)
C
C        IF (NEXT .LT. 100) THEN
c        WRITE(*,*) 'KINTRG: XXX ',NEXT,' > ',RLAT,' - ',RLON,' > ',NPTS
c        WRITE(*,*) 'KINTRG:  I> : ',PT(NWEST),' ',PT(NEAST),' ',PT(SWEST),' ',PT(SEAST)
C        ENDIF
C
C       Interpolate the new value
C
        NEWFLD(NEXT) = KNFROM4(LVEGGY,RLAT,RLON,OLAT,OLON,PT,OLDFLD)
C
      ENDDO
C
      KINTRG = TOTAL
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END

