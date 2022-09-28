C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION NEWISL(OLDGEO,NEWGEO,OLDLSM,OLDFLD,MASTER,NEWFLD)
C
C---->
C**** NEWISL
C
C     Purpose
C     -------
C
C     Interpolate a field based on an old land-sea mask to a field
C     based on a different land-sea mask.
C
C
C     Interface
C     ---------
C
C     IRET = NEWISL(OLDGEO,NEWGEO,OLDLSM,OLDFLD,MASTER,NEWFLD)
C
C     Input
C     -----
C
C     OLDGEO - GRIB section 2 describing grid of old field.
C     NEWGEO - GRIB section 2 describing grid of new field.
C     OLDLSM - Array of land-sea mask values for old field.
C     OLDFLD - Array of values for old field.
C     MASTER - Array of land-sea mask values for new field.
C
C
C     Output
C     ------
C
C     NEWFLD - Array of values for new field.
C
C     Function returns:
C       - 0 if all is well
C       - 1, otherwise.
C
C
C     Method
C     ------
C
C     Build up field offsets from input geometries.
C     For each point of the new field, find in the old field the four
C     nearest neighbours' positions, values and types.
C     Calculate new value from the neighbours.
C
C
C     Externals
C     ---------
C
C     IGGLAT  - Compute gaussian lines of latitude.
C     JNORSGG - Find gaussian latitudes to north and south of latitude.
C     ISLPROC - Calculate value of new field point.
C     INTLOG  - Log messages
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     August 2000
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER OLDGEO(*),NEWGEO(*)
      REAL OLDLSM(*),OLDFLD(*),MASTER(*),NEWFLD(*)
C
#include "intisl.h"
#include "parim.h"
C
C     Parameters
C
      INTEGER JPMAXLT, JPGAUSS, JP1000
      PARAMETER (JPMAXLT=721)
      PARAMETER (JPGAUSS=4)
      PARAMETER (JP1000=1000)
C
C     Local variables
C
      INTEGER TOTAL, NEXT, LOOP, NEWOFF(JPMAXLT), OLDOFF(JPMAXLT)
      INTEGER LATIT, LONG, NEWTYPE, IRET, NPTS
      INTEGER LAT(2), LON(4)
      INTEGER PT(4), TYPE(4)
      REAL OLAT(2), OLON(4)
      REAL RLATOLD(JPMAXLT), RLATNEW(JPMAXLT), RLAT, RLON
C
C     Externals
C
      INTEGER IGGLAT, JNORSGG
      REAL ISLPROC
      EXTERNAL IGGLAT, JNORSGG, ISLPROC
C
C     -----------------------------------------------------------------|
C*    Section 1.   Build working values using input geometries.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      NEWISL = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Calculate number of points in new field and offset
C                  to start of each latitude in the new grid
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( NEWGEO(1).EQ.JPGAUSS ) THEN
C
C       New field is gaussian
C
        CALL INTLOG(JP_DEBUG,'NEWISL: New field is gaussian',JPQUIET)
C
        IF( NEWGEO(17).EQ.0 ) THEN
          CALL INTLOG(JP_DEBUG,'NEWISL: New field is regular',JPQUIET)
          TOTAL = NEWGEO(2)*NEWGEO(3)
          NEWOFF(1) = 0
          DO LOOP = 2, NEWGEO(NJ)
            NEWOFF(LOOP) = NEWOFF(LOOP-1) + NEWGEO(2)
          ENDDO
        ELSE
          CALL INTLOG(JP_DEBUG,'NEWISL: New field is reduced',JPQUIET)
          NEWOFF(1) = 0
          TOTAL = NEWGEO(NPOINTS)
          DO LOOP = 2, NEWGEO(NJ)
            NEWOFF(LOOP) = NEWOFF(LOOP-1) + NEWGEO(NPOINTS-2+LOOP)
            TOTAL = TOTAL + NEWGEO(NPOINTS-1+LOOP)
          ENDDO
        ENDIF
C
C       Get the gaussian latitudes for the new field
C
        IRET = IGGLAT(NEWGEO(NGAUSS)*2,RLATNEW,0,-1)
        IF( IRET.NE.0 ) THEN
          WRITE(*,*) 'NEWISL: Problem call igglat for new grid'
          NEWISL = 1
          RETURN
        ENDIF
C
      ELSE
C
C       New field is lat/long
C
        CALL INTLOG(JP_DEBUG,'NEWISL: New field is lat/long',JPQUIET)
        TOTAL = NEWGEO(2)*NEWGEO(3)
        DO LOOP = 1, NEWGEO(3)
          NEWOFF(LOOP) = NEWGEO(2)*(LOOP-1)
          RLATNEW(LOOP) = 90.0 - (REAL((LOOP-1)*NEWGEO(10))/JP1000)
        ENDDO
C
      ENDIF
C
      CALL INTLOG(JP_DEBUG,'NEWISL: No. of pts in new field = ',TOTAL)
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
        WRITE(*,*) 'NEWISL: Problem call igglat for old grid'
        NEWISL = 1
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
        IF( NEWGEO(1).EQ.JPGAUSS ) THEN
          IF( NEWGEO(17).EQ.0 ) THEN
            RLON = REAL((LONG-1)*NEWGEO(9))/JP1000
          ELSE
            RLON = (REAL(LONG-1)*360.0)/REAL(NEWGEO(NPOINTS+LATIT-1))
          ENDIF
        ELSE
          RLON = REAL((LONG-1)*NEWGEO(9))/JP1000
        ENDIF
C
C       Find type of point (land or sea)
C
        IF( MASTER(NEXT).GT.MASTERTHRESHOLD ) THEN
          NEWTYPE = LAND
        ELSE
          NEWTYPE = SEA
        ENDIF
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
        NPTS = OLDGEO(NPOINTS-1+LAT(NORTH))
        LON(NWEST) = 1 + INT(RLON/(360.0/REAL(NPTS)))
        LON(NEAST) = LON(NWEST) + 1
        IF( LON(NEAST).GT.NPTS ) LON(NEAST) = 1
C
        OLON(NWEST) = (REAL(LON(NWEST)-1)*360.0)/REAL(NPTS)
        IF( LON(NEAST).EQ.1 ) THEN
          OLON(NEAST) = 360.0
        ELSE
          OLON(NEAST) = (REAL(LON(NEAST)-1)*360.0)/REAL(NPTS)
        ENDIF
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
        PT(NWEST) = OLDOFF(LAT(NORTH)) + LON(NWEST)
        IF( OLDLSM(PT(NWEST)).GT.OLDLSMTHRESHOLD ) THEN
          TYPE(NWEST) = LAND
        ELSE
          TYPE(NWEST) = SEA
        ENDIF
C
        PT(NEAST) = OLDOFF(LAT(NORTH)) + LON(NEAST)
        IF( OLDLSM(PT(NEAST)).GT.OLDLSMTHRESHOLD ) THEN
          TYPE(NEAST) = LAND
        ELSE
          TYPE(NEAST) = SEA
        ENDIF
C
        PT(SWEST) = OLDOFF(LAT(SOUTH)) + LON(SWEST)
        IF( OLDLSM(PT(SWEST)).GT.OLDLSMTHRESHOLD ) THEN
          TYPE(SWEST) = LAND
        ELSE
          TYPE(SWEST) = SEA
        ENDIF
C
        PT(SEAST) = OLDOFF(LAT(SOUTH)) + LON(SEAST)
        IF( OLDLSM(PT(SEAST)).GT.OLDLSMTHRESHOLD ) THEN
          TYPE(SEAST) = LAND
        ELSE
          TYPE(SEAST) = SEA
        ENDIF
C
C       Interpolate the new value
C
        NEWFLD(NEXT) =
     X    ISLPROC(RLAT,RLON,NEWTYPE,OLAT,OLON,TYPE,PT,OLDFLD)
C
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
C
      RETURN
      END

