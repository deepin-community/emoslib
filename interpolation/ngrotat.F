C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION NGROTAT(OLDFLD, NEWFLD, NUMPTS, POLELAT, POLELON,
     X                         HTYPE, NGAUSS)
C
C---->
C**** NGROTAT
C
C     PURPOSE
C     _______
C
C     Generates a rotated gaussian grid.
C
C     INTERFACE
C     _________
C
C     IRET = NGROTAT(OLDFLD,NEWFLD,NUMPTS,POLELAT,POLELON,HTYPE,NGAUSS)
C
C
C     Input parameters
C     ________________
C
C     OLDFLD  - Original gaussian grid field.
C     NUMPTS  - Total number of points in the original field.
C     POLELAT - Latitude of the South Pole of rotation.
C     POLELON - Longitude of the South Pole of rotation.
C     NGAUSS  - Gaussian grid number
C     HTYPE   - Gaussian grid type
C               = 'R' for reduced ("quasi-regular"),
C               = 'O' for reduced/octahedral,
C               = 'F' for full,
C               = 'U' for user-defined reduced gaussian grid
C
C     Output parameters
C     ________________
C
C     NEWFLD  = New gaussian grid field.
C               (This can be the same array as OLDFLD).
C
C     Function returns zero if interpolation ran OK.
C
C
C     METHOD
C     ______
C
C     The nearest gaussian latitude row is found.
C
C     Work space is dynamically allocated. It is retained for future
C     calls.
C
C     EXTERNALS
C     _________
C
C     IGGLAT  - Compute Gaussian latitudes for a given truncation.
C     NGVALUE - Calculates the interpolated value at a point of the new
C               gaussian grid.
C     INTLOG  - Logs output messages
C     INTLOGR - Logs output messages
C     JMEMHAN - Dynamically allocate work array
C
C
C     REFERENCE
C     _________
C
C     None
C
C
C     COMMENTS
C     ________
C
C     The input field is assumed to be global in extent, and a known
C     definition, either N160 or N80 (reduced or regular).
C     The output field has the same type and dimensions as the input
C     field.
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
#include "intf.h"
C
C     Parameters
C
      INTEGER JPROUTINE, JPMAXGG
      PARAMETER( JPROUTINE = 38500 )
      PARAMETER (JPMAXGG = 1280)
C
C     Subroutine arguments
C
      REAL OLDFLD, NEWFLD, POLELAT, POLELON
      INTEGER NUMPTS, NGAUSS
      DIMENSION OLDFLD(NUMPTS), NEWFLD(NUMPTS)
      CHARACTER*1 HTYPE
C
C     Local variables
C
      REAL GLATS(JPMAXGG)
      INTEGER NGPTS(JPMAXGG)
      INTEGER OLDGAUS
      DATA OLDGAUS/0/
      CHARACTER*1 OLDTYPE
      DATA OLDTYPE/' '/
      SAVE GLATS, NGPTS, OLDGAUS, OLDTYPE
C
      INTEGER NEXT, NEXLAT, INROW, LOOP, KRET
      REAL STEPLON, RLAT, RLON
      DIMENSION RLON(JPMAXGG*2)
C
      INTEGER ISIZE
      DATA ISIZE/0/
      REAL DUMI
      POINTER (IDUMI, DUMI)
      DIMENSION DUMI(1)
      SAVE IDUMI, ISIZE
C
C     Externals
C
      INTEGER IGGLAT
C
C     _______________________________________________________
C
C*    Section 1. Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      NGROTAT = 0
C
      CALL INTLOG(JP_DEBUG,
     X  'NGROTAT: Number of pts in original field = ',NUMPTS)
      CALL INTLOGR(JP_DEBUG,
     X  'NGROTAT: Latitude of the South Pole of rotation = ',POLELAT)
      CALL INTLOGR(JP_DEBUG,
     X  'NGROTAT: Longitude of the South Pole of rotation = ',POLELON)
      CALL INTLOG(JP_DEBUG,'NGROTAT: Get gaussian grid type:',JPQUIET)
      CALL INTLOG(JP_DEBUG,HTYPE,JPQUIET)
      CALL INTLOG(JP_DEBUG,'NGROTAT: grid number:',NGAUSS)
C
C     Get gaussian grid definition (first time only)
C
      IF( (OLDGAUS.NE.NGAUSS).OR.(OLDTYPE.NE.HTYPE)) THEN
        CALL INTLOG(JP_DEBUG,
     X    'NGROTAT: Get definitions for gaussian grid type:',JPQUIET)
        CALL INTLOG(JP_DEBUG,HTYPE,JPQUIET)
        CALL INTLOG(JP_DEBUG,'NGROTAT: grid number:',NGAUSS)
C
        KRET = IGGLAT(NGAUSS*2, GLATS, 0, 1)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR, 'NGROTAT: IGGLAT call failed',KRET)
          NGROTAT = JPROUTINE + 2
          GOTO 900
        ENDIF
        DO LOOP = 1, NGAUSS
          NGPTS(LOOP) = ISEC2(22+LOOP)
          NGPTS(2*NGAUSS+1-LOOP) = NGPTS(LOOP)
        ENDDO
        OLDGAUS = NGAUSS
        OLDTYPE = HTYPE
      ENDIF
C
C     Allocate work space if not already done.
C
      IF( ISIZE.LT.NUMPTS ) THEN
        ISIZE = NUMPTS
        CALL JMEMHAN(18,IDUMI,ISIZE,1,KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'NGROTAT: Memory allocation failed: status = ',KRET)
          NGROTAT = JPROUTINE + 2
          GOTO 900
        ENDIF
      ENDIF
C
C     Move the original field to the work array
C
      DO LOOP = 1, NUMPTS
        DUMI(LOOP) = OLDFLD(LOOP)
      ENDDO
C     _______________________________________________________
C
C*    Section 2. Work through latitude lines from north to south
C     _______________________________________________________
C
  200 CONTINUE
C
      NEXT = 1
C
      DO NEXLAT = 1, NGAUSS*2
        INROW = NGPTS(NEXLAT)
        STEPLON = 360.0/INROW
        RLAT = GLATS(NEXLAT)
C
C       Fill in longitudes along current line of latitude
C
        DO LOOP = 1, INROW
          RLON(LOOP) = (LOOP-1)*STEPLON
        ENDDO
C
C       Rotate the current line of latitude
C
        CALL NGVALUE(NEWFLD,NEXT,INROW,RLAT,RLON,
     X               POLELAT,POLELON,NGAUSS,HTYPE,DUMI,GLATS,NGPTS)
C
        NEXT = NEXT + INROW
C
      ENDDO
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
C
      CALL INTLOG(JP_DEBUG,'NGROTAT: Return from NGROTAT:',JPQUIET)
C
      RETURN
      END
