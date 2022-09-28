C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

        SUBROUTINE JUVPOLE(PSHUP, KTRUNC, PZFA, KLONO, KRET)
C
C---->
C**** JUVPOLE
C
C     Purpose
C     -------
C     This routine creates polar wind U or V components for lat/long
C     grid fields.
C
C
C     Interface
C     ---------
C     CALL JUVPOLE(PSHUP, KTRUNC, PZFA, KLONO, KRET)
C
C
C     Input parameters
C     ----------------
C     PSHUP   - Spherical harmonics field, unpacked
C     KTRUNC  - Truncation number of spherical harmonics field
C     KLONO   - Number of longitude points in output field
C
C
C     Output parameters
C     -----------------
C     PZFA - Output grid point field; contains upto 32 each of
C            North and South latitude rows symmetrically.
C     KRET - Return status code
C            0 = OK
C
C
C     Common block usage
C     ------------------
C     JDCNDBG
C
C
C     Method
C     ------
C     For each pole, creates the reduced gaussian grid lines nearest to
C     the pole and does a linear interpolation to the polar longitude.
C
C
C     Externals
C     ---------
C     JAGGGP  - converts spectral input to a gaussian grid
C     INTLOG  - Output log message
C
C
C     Reference
C     ---------
C     None.
C
C     Comments
C     --------
C     None.
C
C
C     Author
C     ------
C     J.D.Chambers      ECMWF    October 2002
C
C
C     Modifications
C     -------------
C     None.
C
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "jparams.h"
#include "parim.h"
#include "current.h"
#include "nofld.common"
#include "grfixed.h"
C
C     Subroutine arguments
C
      COMPLEX PSHUP(*)
      REAL PZFA(JPLONO + 2, 64)
      INTEGER KTRUNC, KLONO, KRET
C
C     Parameters
C
C     Local variables
C
      REAL EAST, WEST, LAT, POUTF(18), ALATS(320)
      REAL XLONG, WLEFT, XSTEP, GSTEP
      INTEGER KPTS(320), NUM, LOOP, NLEFT, NRIGHT, LUV
      CHARACTER*1 HTYPE
C
C     Externals
C
      INTEGER  IGGLAT
      EXTERNAL IGGLAT
C
C     -----------------------------------------------------------------|
C*    Section 1.  Initialization.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      KRET  = 0
C
      NUM = 160
      GSTEP = 360.0 / 18.0
      XSTEP = 360.0 / REAL(KLONO)
      EAST  = 360.0 - GSTEP
      WEST  = 0.0
C
C     -----------------------------------------------------------------|
C*    Section 2.  Create North pole values
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Form the N160 reduced gaussian grid latitude near the North pole
C
      LAT = 90.0
      HTYPE = 'R'
      LUV = 1
      CALL JAGGGP(PSHUP, KTRUNC, LAT, LAT, WEST, EAST,
     X            NUM, HTYPE, KPTS, POUTF, LUV, KRET)
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'JUVPOLE: problem forming Northern gaussian grid latitude',
     X    JPQUIET)
        GOTO 900
      ENDIF
C
C     Interpolate to the lat/long northern latitude grid points
C
      DO LOOP = 1, KLONO
C
C       Find the gaussian neighbours on either side of lat/long point
C       and their weights
C
        XLONG = XSTEP * REAL(LOOP-1)
C
        NLEFT  = 1 + INT(XLONG/GSTEP)
        NRIGHT = NLEFT + 1
        IF( NRIGHT.GT.18 ) NRIGHT = 1
C
        WLEFT = 1.0 - (ABS((XLONG - (NLEFT-1)*GSTEP))/GSTEP)
C
        PZFA(LOOP,1) = POUTF(NLEFT)*WLEFT + POUTF(NRIGHT)*(1.0-WLEFT)
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 3.  Create South pole values
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Store latitude and point count values for the next call to JAGGGP
C     which uses the 'U' option (user supplied values).
C
      NOGAUSO = NUM
C
      KRET  = IGGLAT(NUM*2, ALATS, 1, 1)
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'JUVPOLE: IGGLAT call failed',KRET)
        KRET = KRET
        GOTO 900
      ENDIF
C
      DO LOOP = 1, NUM*2
        NOLPTS(LOOP)  = KPTS(LOOP)
        ROGAUSS(LOOP) = ALATS(LOOP)
      ENDDO
C
C     Form the N160 reduced gaussian grid latitude near the South pole
C
      LAT = -90.0
      HTYPE = 'U'
      LUV = 1
      CALL JAGGGP(PSHUP, KTRUNC, LAT, LAT, WEST, EAST,
     X            NUM, HTYPE, KPTS, POUTF, LUV, KRET)
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'JUVPOLE: problem forming Southern gaussian grid latitude',
     X    JPQUIET)
        GOTO 900
      ENDIF
C
C     Interpolate to the lat/long southern latitude grid points
C
      DO LOOP = 1, KLONO
C
C       Find the gaussian neighbours on either side of lat/long point
C       and their weights
C
        XLONG = XSTEP * REAL(LOOP-1)
C
        NLEFT  = 1 + INT(XLONG/GSTEP)
        NRIGHT = NLEFT + 1
        IF( NRIGHT.GT.18 ) NRIGHT = 1
C
        WLEFT = 1.0 - (ABS((XLONG - (NLEFT-1)*GSTEP))/GSTEP)
C
        PZFA(LOOP,2) = POUTF(NLEFT)*WLEFT + POUTF(NRIGHT)*(1.0-WLEFT)
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9.  Return.
C     -----------------------------------------------------------------|
C
 900  CONTINUE
C
      RETURN
      END
