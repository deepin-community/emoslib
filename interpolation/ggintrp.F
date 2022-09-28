C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GGINTRP(NEWFLD,NEXT,INROW,OLAT,OLON,NGAUSS,HTYPE,FIELD)
C
C---->
C**** GGINTRP
C
C     PURPOSE
C     _______
C
C     Finds values at the points of a row by interpolation within a
C     gaussian field.
C
C     INTERFACE
C     _________
C
C     CALL GGINTRP(NEWFLD,NEXT,INROW,OLAT,OLON,NGAUSS,HTYPE,FIELD)
C
C
C     Input parameters
C     ________________
C
C     NEWFLD   - The field of values being created
C     NEXT     - Start offset of the latitude row being created
C     INROW    - The number of points in the latitude row
C     OLAT     - Latitude of target point
C     OLON     - Longitude of target point
C     FIELD    - Original gaussian field
C     NGAUSS   - Gaussian field number
C     HTYPE    - Gaussian grid type (original, unrotated)
C                = 'R' for reduced ("quasi-regular") Gaussian field,
C                = 'O' for reduced/octahedral Gaussian field,
C                = 'F' for full (regular) gaussian field
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
C     Uses distance weighted values of 4 neighbouring points unless
C     within small distance of one of the neighbours, in which case
C     the value at that point is used.
C
C
C     EXTERNALS
C     _________
C
C     JGETGG  - Gets gaussian grid definition.
C     JMKOFGG - Builds offsets to start of each latitude row in the
C               gaussian grid.
C     JNORSGG - Find nearest north/south latitude in the gaussian grid.
C     INTLOG  - Logs output messages
C
C
C     REFERENCE
C     _________
C
C     None.
C
C
C     COMMENTS
C     ________
C
C     Some loops have been split and simplified to help with
C     vectorisation on VPP.
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
#include "parim.h"
C
C     Parameters
C
      INTEGER JPFACTR, JPMAXGG
      PARAMETER( JPFACTR = 10000 )
      PARAMETER( JPMAXGG = 4096 )
      REAL JPEPS
      PARAMETER( JPEPS = 1E-2)
C
C     Function arguments
C
      REAL NEWFLD, OLAT, OLON, FIELD
      DIMENSION NEWFLD(*), OLAT(*), OLON(*), FIELD(*)
      INTEGER NEXT, INROW, NGAUSS
      CHARACTER*1 HTYPE
C
C     Local variables
C
      INTEGER LOOP, IVALUE
      INTEGER NORTH, SOUTH, IWESTN, IWESTS, IEASTN, IEASTS
      REAL NSTEP, SSTEP, PEAST
      DIMENSION NORTH(JPMAXGG), SOUTH(JPMAXGG)
      DIMENSION IWESTN(JPMAXGG), IWESTS(JPMAXGG)
      DIMENSION IEASTN(JPMAXGG), IEASTS(JPMAXGG)
      DIMENSION NSTEP(JPMAXGG), SSTEP(JPMAXGG)
      REAL DNORTH, DSOUTH, DWESTN, DWESTS, DEASTN, DEASTS
      DIMENSION DNORTH(JPMAXGG), DSOUTH(JPMAXGG)
      DIMENSION DWESTN(JPMAXGG), DWESTS(JPMAXGG)
      DIMENSION DEASTN(JPMAXGG), DEASTS(JPMAXGG)
      REAL GLATS(JPMAXGG/2)
      INTEGER NGPTS(JPMAXGG/2)
      INTEGER KOFSET(JPMAXGG/2)
      INTEGER KRET, OLDGAUS
      DATA OLDGAUS/0/
      CHARACTER*1 OLDTYPE
      DATA OLDTYPE/' '/
      SAVE OLDGAUS, OLDTYPE, GLATS, NGPTS, KOFSET
C
C     Externals
C
      INTEGER JNORSGG
      EXTERNAL JNORSGG
C
C     Statement function
C
      INTEGER X, Y
      REAL OFIELD
      OFIELD(X,Y) = FIELD(KOFSET(X)+Y)
C
C     _______________________________________________________
C
C*    Section 1. Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
C     Get gaussian grid definition (first time only)
C
      IF ( (OLDGAUS.NE.NGAUSS).OR.(OLDTYPE.NE.HTYPE) ) THEN
C
        CALL INTLOG(JP_DEBUG,
     X    'GGINTRP: Get gaussian grid type:',JPQUIET)
        CALL INTLOG(JP_DEBUG,HTYPE,JPQUIET)
        CALL INTLOG(JP_DEBUG, 'GGINTRP: grid number:',NGAUSS)
C
        CALL JGETGG(NGAUSS,HTYPE,GLATS,NGPTS,KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'GGINTRP: Failed to get gaussian grid definition:',JPQUIET)
          CALL INTLOG(JP_ERROR, 'GGINTRP: grid type:',JPQUIET)
          CALL INTLOG(JP_ERROR, HTYPE,JPQUIET)
          CALL INTLOG(JP_ERROR, 'GGINTRP: grid number:',NGAUSS)
          CALL ABORTX ('GGINTRP')
        ENDIF
C
C       Build array of offsets in field to start of each latitude line
C
        CALL INTLOG(JP_DEBUG,'GGINTRP: Build row offsets',JPQUIET)
        IF( HTYPE.EQ.'R' .OR. HTYPE.EQ.'r' .OR.
     X      HTYPE.EQ.'O' .OR. HTYPE.EQ.'o' .OR.
     X      HTYPE.EQ.'U' .OR. HTYPE.EQ.'u' ) THEN
C         (this value will not be used by JMKOFGG)
          PEAST = 360.
        ELSE
          PEAST = 360. - (90./NGAUSS)
        ENDIF
        CALL JMKOFGG(HTYPE,1,NGAUSS*2,0.0,PEAST,NGAUSS,NGPTS,KOFSET)
        OLDGAUS = NGAUSS
        OLDTYPE = HTYPE
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. Find latitudes of grid lines to north and south of
C                the points and calculate grid interval along lines.
C     _______________________________________________________
C
  200 CONTINUE
C
      DO LOOP = 1, INROW
        NORTH(LOOP) = JNORSGG( OLAT(LOOP), GLATS, NGAUSS, 1)
      ENDDO
C
      DO LOOP = 1, INROW
        SOUTH(LOOP) = MIN( NORTH(LOOP)+1, NGAUSS*2)
        DNORTH(LOOP) = ABS(GLATS(NORTH(LOOP)) - OLAT(LOOP))
      ENDDO
C
      DO LOOP = 1, INROW
        DSOUTH(LOOP) = 1.0 - DNORTH(LOOP)
        NSTEP(LOOP) = 360.0/FLOAT(NGPTS(NORTH(LOOP)))
        SSTEP(LOOP) = 360.0/FLOAT(NGPTS(SOUTH(LOOP)))
      ENDDO
C
C     _______________________________________________________
C
C*    Section 3. Find neighbouring points.
C     _______________________________________________________
C
  300 CONTINUE
C
      DO LOOP = 1, INROW
C
C       Calculate normalised distance west and east to neighbouring
C       points on lines to north and south of the point
C
        IWESTN(LOOP) = INT(OLON(LOOP)/NSTEP(LOOP))
        IWESTS(LOOP) = INT(OLON(LOOP)/SSTEP(LOOP))
        IEASTN(LOOP) = IWESTN(LOOP) + 1
        IEASTS(LOOP) = IWESTS(LOOP) + 1
C
        DWESTN(LOOP) =
     X   ABS((OLON(LOOP) - FLOAT(IWESTN(LOOP))*NSTEP(LOOP))/NSTEP(LOOP))
        DEASTN(LOOP) = 1.0 - DWESTN(LOOP)
C
        DWESTS(LOOP) =
     X   ABS((OLON(LOOP) - FLOAT(IWESTS(LOOP))*SSTEP(LOOP))/SSTEP(LOOP))
        DEASTS(LOOP) = 1.0 - DWESTS(LOOP)
C
C       Adjust west and east for longitude wrap-around
C
        IVALUE=NGPTS(NORTH(LOOP))
        IF( IEASTN(LOOP).GE.IVALUE) THEN
          IEASTN(LOOP) = IEASTN(LOOP) - IVALUE
          IF( IWESTN(LOOP).GE.IVALUE) IWESTN(LOOP) = IWESTN(LOOP)-IVALUE
        ENDIF
C
        IVALUE=NGPTS(SOUTH(LOOP))
        IF( IEASTS(LOOP).GE.IVALUE) THEN
          IEASTS(LOOP) = IEASTS(LOOP) - IVALUE
          IF( IWESTS(LOOP).GE.IVALUE) IWESTS(LOOP) = IWESTS(LOOP)-IVALUE
        ENDIF
C
      ENDDO
C
C     _______________________________________________________
C
C*    Section 4. Interpolate using neighbouring points.
C     _______________________________________________________
C
  400 CONTINUE
C
C       Use bi-linear interpolation from four neighbouring points.
C
      DO LOOP = 1, INROW
        NEWFLD(NEXT+LOOP-1) =
     X    ( ( OFIELD(NORTH(LOOP),IWESTN(LOOP)) * DEASTN(LOOP) +
     X        OFIELD(NORTH(LOOP),IEASTN(LOOP)) * DWESTN(LOOP) )
     X      * DSOUTH(LOOP) +
     X      ( OFIELD(SOUTH(LOOP),IWESTS(LOOP)) * DEASTS(LOOP) +
     X        OFIELD(SOUTH(LOOP),IEASTS(LOOP)) * DWESTS(LOOP) )
     X      * DNORTH(LOOP) )
      ENDDO
C
C     _______________________________________________________
C
C*    Section 9. Closedown.
C     _______________________________________________________
C
  900 CONTINUE
C
      RETURN
      END
