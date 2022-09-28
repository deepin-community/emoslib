C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION FIXAREA( )
C
C---->
C**** FIXAREA
C
C     Purpose
C     -------
C
C     Fixup input/output field area definitions.
C
C
C     Interface
C     ---------
C
C     IRET = FIXAREA( )
C
C     Input
C     -----
C
C     None.
C
C
C     Output
C     ------
C
C     None.
C
C
C     Method
C     ------
C
C     If default (0/0/0/0) selected for input, input area is set
C       - to global for spherical harmonics
C       - to global for lat/long grid
C       - to global for gaussian grid
C
C     If default (0/0/0/0) selected for output, output area is set
C       - to same as input area for lat/long grid
C       - to same as input area for regular gaussian grid
C       - to global for reduced gaussian grid
C
C     (Currently, subareas are not supported for reduced gaussian fields.
C      Should work OK; but need to correct setup of values in GRIB
C      product for number of points in each latitude row)
C
C     Output area is adjusted to fit the given grid step.
C
C
C     Externals
C     ---------
C
C     AREACHK - Match input/output field area definitions according to
C               grid specification.
C     GETENV   - Get value of an environment variable
C     INTLOG   - Logs output messages
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Jan 1995
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "intf.h"
#include "current.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 19200 )
C
C     Local variables
C
      INTEGER INORTH, ISOUTH, IEAST
      INTEGER ITEMP, IRET
      REAL EW, NS, NORTH, SOUTH, EAST, WEST, FACTOR3, FACTOR4
      LOGICAL LDEFIN, LDEFOUT, LGLOBAL, LOVERDE
      CHARACTER*20 OVERIDE
      INTEGER IBLANK
C
C     Externals
C
      INTEGER AREACHK
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
      FIXAREA = 0
      FACTOR3 = 0.
      FACTOR4 = 0.
      LDEFIN  = .FALSE.
      LGLOBAL = .FALSE.
C
C ------------------------------------------------------------------
C*    Section 2.   Fixup input area if default (0/0/0/0).
C ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Fixup input ocean area definition
C
      IF( NILOCAL.EQ.4 ) THEN
        FACTOR3 = 0.0
        FACTOR4 = 0.0
C
        IF( NIFORM.EQ.1 ) THEN
          IF( ISEC1(60).EQ.1 ) FACTOR3 = 1.0
          IF( ISEC1(60).EQ.2 ) THEN
            IF( ISEC1(47).EQ.160 ) FACTOR3 = 1000.0
          ENDIF
          IF( ISEC1(60).EQ.3 ) FACTOR3 = 1000000.0
          IF( ISEC1(60).EQ.4 ) FACTOR3 = 1000000.0
C
          IF( ISEC1(61).EQ.1 ) FACTOR4 = 1.0
          IF( ISEC1(61).EQ.2 ) THEN
            IF( ISEC1(47).EQ.160 ) FACTOR4 = 1000.0
          ENDIF
          IF( ISEC1(61).EQ.3 ) FACTOR4 = 1000000.0
          IF( ISEC1(61).EQ.4 ) FACTOR4 = 1000000.0
C
          IF( (FACTOR3.EQ.0).OR.(FACTOR4.EQ.0) ) THEN
            FIXAREA = 2
            GOTO 900
          ENDIF
          NORTH = REAL(ISEC1(62))/FACTOR4
          WEST  = REAL(ISEC1(63))/FACTOR3
          SOUTH = REAL(ISEC1(64))/FACTOR4
          EAST  = REAL(ISEC1(65))/FACTOR3
        ELSE
          IF( NIOCO3.EQ.1 ) FACTOR3 = 1.0
          IF( NIOCO3.EQ.2 ) THEN
            IF( NIVCDEF.EQ.160 ) FACTOR3 = 1000.0
          ENDIF
          IF( NIOCO3.EQ.3 ) FACTOR3 = 1000000.0
          IF( NIOCO3.EQ.4 ) FACTOR3 = 1000000.0
C
          IF( NIOCO4.EQ.1 ) FACTOR4 = 1.0
          IF( NIOCO4.EQ.2 ) THEN
            IF( NIVCDEF.EQ.160 ) FACTOR4 = 1000.0
          ENDIF
          IF( NIOCO4.EQ.3 ) FACTOR4 = 1000000.0
          IF( NIOCO4.EQ.4 ) FACTOR4 = 1000000.0
C
          IF( (FACTOR3.EQ.0).OR.(FACTOR4.EQ.0) ) THEN
            FIXAREA = 2
            GOTO 900
          ENDIF
          NORTH = REAL(NIOCO4F)/FACTOR4
          WEST  = REAL(NIOCO3F)/FACTOR3
          SOUTH = REAL(NIOCO4L)/FACTOR4
          EAST  = REAL(NIOCO3L)/FACTOR3
        ENDIF
        NIAREA(1) = NINT( NORTH * PPMULT + 0.1)
        NIAREA(2) = NINT( WEST  * PPMULT + 0.1)
        NIAREA(3) = NINT( SOUTH * PPMULT + 0.1)
        NIAREA(4) = NINT( EAST  * PPMULT + 0.1)
        GOTO 300
      ENDIF
C
C Sinisa bug fix for grid global fields
C       If input is lat/lon, check if it is global to within
C       a tolerance of 0.1 degrees
       IF ( NIREPR .EQ. JPREGULAR ) THEN
          IF ( NIAREA(4).GT.0 ) THEN
            IEAST = NIAREA(4)
          ELSE
            IEAST = JP360 + NIAREA(4)
          ENDIF
         IF(NIAREA(1).EQ.JP90.AND.NIAREA(3).EQ.-JP90.AND.
     X      NIAREA(2).EQ.0.AND.
     X      IABS(JP360 - IEAST - NIGRID(1)).LT.1000) THEN
              NIAREA(1) = 0
              NIAREA(2) = 0
              NIAREA(3) = 0
              NIAREA(4) = 0
csinisa
cs          ELSEIF(IABS(JP360 - IEAST - NIGRID(1)).LT.1000) THEN
cs              NOAREA(1) = NIAREA(1)
cs              NOAREA(2) = 0
cs              NOAREA(3) = NIAREA(3)
cs              NOAREA(4) = 36000000
          ENDIF
        ENDIF

      LDEFIN = ( (NIAREA(1) .EQ. 0) .AND. (NIAREA(2) .EQ. 0) .AND.
     X           (NIAREA(3) .EQ. 0) .AND. (NIAREA(4) .EQ. 0) )

C
      IF( LDEFIN ) THEN
C
        IF ( (NIREPR.EQ.JPSPHERE) .OR. (NIREPR.EQ.JPSPHROT) ) THEN
C
C         Spectral input ..
          EW = 0.0
          NS = 0.0
C
        ELSE IF ( NIREPR .EQ. JPREGULAR ) THEN
C
C         Regular lat/long grid ..
          EW = FLOAT( NIGRID(1) ) / PPMULT
          NS = FLOAT( NIGRID(2) ) / PPMULT
C
        ELSE
C
C         Gaussian grid ..
          EW = FLOAT( NIGAUSS )
          NS = 0.0
        ENDIF
C
        NORTH = 0.0
        WEST  = 0.0
        SOUTH = 0.0
        EAST  = 0.0
        IRET = AREACHK( EW, NS, NORTH, WEST, SOUTH, EAST )
        IF( IRET.NE.0 ) THEN
          FIXAREA = IRET
          GOTO 900
        ENDIF
        NIAREA(1) = NINT( NORTH * PPMULT + 0.1)
        NIAREA(2) = NINT( WEST  * PPMULT + 0.1)
        NIAREA(3) = NINT( SOUTH * PPMULT + 0.1)
        NIAREA(4) = NINT( EAST  * PPMULT + 0.1)
C
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 3.   Fixup output area if default (0/0/0/0).
C ------------------------------------------------------------------
C
  300 CONTINUE
C
C     See if environment variable has been specified to override
C     output area specification
C
      LOVERDE = .FALSE.
      CALL GETENV('OVERRIDE_OUTPUT_AREA', OVERIDE)
      IBLANK = INDEX(OVERIDE, ' ')
      IF( IBLANK.GT.1 ) THEN
        IF( OVERIDE(1:2).EQ.'ON' ) THEN
          LOVERDE = .TRUE.
          CALL INTLOG(JP_DEBUG,
     X      'FIXAREA: OVERRIDE_OUTPUT_AREA is ON',JPQUIET)
        ENDIF
      ENDIF
C
      LDEFOUT = ( (NOAREA(1) .EQ. 0) .AND. (NOAREA(2) .EQ. 0) .AND.
     X            (NOAREA(3) .EQ. 0) .AND. (NOAREA(4) .EQ. 0) )
     X          .OR. LOVERDE
C
C     Fixup output ocean area definition
C
      IF( NILOCAL.EQ.4 ) THEN
        IF( LDEFOUT ) THEN
          IF( NIFORM.EQ.1 ) THEN
              SOUTH  = REAL(ISEC1(64))/FACTOR4
              NORTH  = REAL(ISEC1(62))/FACTOR4
              WEST   = REAL(ISEC1(63))/FACTOR3
              EAST   = REAL(ISEC1(65))/FACTOR3

              EW = FLOAT( NOGRID(1) ) / PPMULT
              NS = FLOAT( NOGRID(2) ) / PPMULT

            IF( ISEC1(60).EQ.3 ) THEN
              WEST  = 0.0
              EAST  = 360.0
              IRET = AREACHK( EW, NS, NORTH, WEST, SOUTH, EAST )
              IF ( IRET .NE. 0 ) THEN
                 FIXAREA = IRET
                 GOTO 900
              ENDIF
            ELSEIF( ISEC1(60).EQ.4 ) THEN
              WEST  = -90.0
              EAST  =  90.0
            ENDIF

            IF( ISEC1(61).EQ.3 ) THEN
              SOUTH  = 0.0
              NORTH  = 360.0
            ELSEIF( ISEC1(61).EQ.4 ) THEN
              SOUTH  = -90.0
              NORTH  =  90.0
            ENDIF
          ELSE
              NORTH = REAL(NIOCO4F)/FACTOR4
              WEST  = REAL(NIOCO3F)/FACTOR3
              SOUTH = REAL(NIOCO4L)/FACTOR4
              EAST  = REAL(NIOCO3L)/FACTOR3
              EW = FLOAT( NOGRID(1) ) / PPMULT
              NS = FLOAT( NOGRID(2) ) / PPMULT
            IF( NIOCO3.EQ.3 ) THEN
              WEST  = 0.0
              EAST  = 360.0
              IRET = AREACHK( EW, NS, NORTH, WEST, SOUTH, EAST )
              IF ( IRET .NE. 0 ) THEN
                 FIXAREA = IRET
                 GOTO 900
              ENDIF
            ELSEIF( NIOCO3.EQ.4 ) THEN
              WEST  = -90.0
              EAST  =  90.0
            ENDIF

            IF( NIOCO4.EQ.3 ) THEN
              SOUTH  = 0.0
              NORTH  = 360.0
            ELSEIF( NIOCO4.EQ.4 ) THEN
              SOUTH  = -90.0
              NORTH  =  90.0
            ENDIF
          ENDIF

          NOAREA(1) = NINT( NORTH * PPMULT + 0.1)
          NOAREA(2) = NINT( WEST  * PPMULT + 0.1)
          NOAREA(3) = NINT( SOUTH * PPMULT + 0.1)
          NOAREA(4) = NINT( EAST  * PPMULT + 0.1)
        ELSE
          NORTH = REAL(NOAREA(1)) / PPMULT
          WEST  = REAL(NOAREA(2)) / PPMULT
          SOUTH = REAL(NOAREA(3)) / PPMULT
          EAST  = REAL(NOAREA(4)) / PPMULT
        ENDIF
        GOTO 900
      ENDIF
C
      IF( LDEFOUT ) THEN
C
C       If input is gaussian, check if it is global to within
C       a tolerance of 0.1 degrees.
C
        IF ( (NIREPR.EQ.JPGAUSSIAN) .OR. (NIREPR.EQ.JPQUASI) ) THEN
          INORTH = NINT( RIGAUSS(1) * PPMULT )
          ISOUTH = -INORTH
c         EMOS-199: adjusted for reduced_gg/octahedral
c         IEAST  = JP360 - (JP90/NIGAUSS)
          IEAST  = JP360 - (JP360/MILLEN(NIGAUSS))
          LGLOBAL = (NIAREA(2).EQ.0)
     X      .AND. (IABS(NIAREA(1)-INORTH).LE.1000)
     X      .AND. (IABS(NIAREA(3)-ISOUTH).LE.1000)
     X      .AND. (IABS(NIAREA(4)-IEAST ).LE.1000 .OR.
     X        NIREPR.EQ.JPGAUSSIAN .OR. .TRUE. )
C         NOTE: a similar (but useless) test was done here before adding
C         .TRUE. be replicate the same (useless) behaviour
        ENDIF
C
C       Spectral output ..
C
C       .. skip section fitting output area to grid
C
        IF ( (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) GOTO 900
C
C       Regular lat/long grid ..
C
        IF ( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
          IF( LDEFIN .OR. LGLOBAL.OR.(NIREPR.EQ.JPREDLL) ) THEN
            EW = FLOAT( NOGRID(1) ) / PPMULT
            NS = FLOAT( NOGRID(2) ) / PPMULT
            NORTH = 90.0
            WEST  = 0.0
            SOUTH = -NORTH
            EAST  = 360.0
C
C           Use GRIB header values for reduced lat/long grids
C           (maybe 'Mediterranean' sub-area).
C
            IF( NIREPR.EQ.JPREDLL ) THEN
              NORTH = REAL(NIAREA(1))/PPMULT
              WEST  = REAL(NIAREA(2))/PPMULT
              SOUTH = REAL(NIAREA(3))/PPMULT
              EAST  = REAL(NIAREA(4))/PPMULT
            ENDIF
            IRET = AREACHK( EW, NS, NORTH, WEST, SOUTH, EAST )
            IF ( IRET .NE. 0 ) THEN
               FIXAREA = IRET
               GOTO 900
            ENDIF
            NOAREA(1) = NINT( NORTH * PPMULT + 0.1)
            NOAREA(2) = NINT( WEST  * PPMULT + 0.1)
            NOAREA(3) = NINT( SOUTH * PPMULT + 0.1)
            NOAREA(4) = NINT( EAST  * PPMULT + 0.1)
C
          ELSE
            NOAREA(1) = NIAREA(1)
            NOAREA(2) = NIAREA(2)
            NOAREA(3) = NIAREA(3)
            NOAREA(4) = NIAREA(4)
          ENDIF
C
        ENDIF
C
C       Gaussian ..
C
        IF ( (NOREPR.EQ.JPGAUSSIAN) .OR. (NOREPR.EQ.JPQUASI) ) THEN
C
C         Is output grid specification the same as the input?
          IF( NIGAUSS.EQ.NOGAUSS) THEN
            IF( (NOREPR.EQ.JPGAUSSIAN) .AND. LGLOBAL ) THEN
              NOAREA(1) = JP90
              NOAREA(2) = 0
              NOAREA(3) = -JP90
c     EMOS-199: adjusted for reduced_gg/octahedral
c             NOAREA(4) = JP360 - (JP90/NOGAUSS)
              NOAREA(4) = JP360 - (JP360/NOLPTS(NOGAUSS))
            ELSE
              NOAREA(1) = NIAREA(1)
              NOAREA(2) = NIAREA(2)
              NOAREA(3) = NIAREA(3)
              NOAREA(4) = NIAREA(4)
           ENDIF
C
C           Skip section fitting output area to grid
            GOTO 900
C
          ELSE
C
C           Different grid resolutions
            EW = FLOAT( NOGAUSS )
            NS = 0.0
            IF( (NOREPR.EQ.JPGAUSSIAN) .AND. LGLOBAL ) THEN
              NORTH =  90.0
              WEST  =   0.0
              SOUTH = -90.0
c     EMOS-199: adjusted for reduced_gg/octahedral
c             EAST  = 360.0 - (360.0/(EW*4.0))
              EAST  = 360.0 - (360.0/FLOAT(NOLPTS(NOGAUSS)))
            ELSE
              NORTH = FLOAT( NIAREA(1) ) / PPMULT
              WEST  = FLOAT( NIAREA(2) ) / PPMULT
              SOUTH = FLOAT( NIAREA(3) ) / PPMULT
              EAST  = FLOAT( NIAREA(4) ) / PPMULT
            ENDIF
            IRET = AREACHK( EW, NS, NORTH, WEST, SOUTH, EAST )
            IF ( IRET .NE. 0 ) THEN
               FIXAREA = IRET
               GOTO 900
            ENDIF
            NOAREA(1) = NINT( NORTH * PPMULT + 0.1)
            NOAREA(2) = NINT( WEST  * PPMULT + 0.1)
            NOAREA(3) = NINT( SOUTH * PPMULT + 0.1)
            NOAREA(4) = NINT( EAST  * PPMULT + 0.1)
C
C           If regular lat/long input, check if output west longitude
C           reachs to full globe
            ITEMP = NIAREA(4)+NIGRID(1)
            IF( (NIREPR.EQ.JPREGULAR .AND. ITEMP.EQ.JP360) .OR.
     X          (NIREPR.EQ.JPSPHERE) .OR.
     X          (NIREPR.EQ.JPSPHROT) ) THEN
c     EMOS-199: adjusted for reduced_gg/octahedral
c             NOAREA(4) = JP360 - (JP90/NOGAUSS)
              NOAREA(4) = JP360 - (JP360/NOLPTS(NOGAUSS))
            ENDIF
          ENDIF
        ENDIF
C
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 4.   Now fixup output areas to correspond to the grid.
C ------------------------------------------------------------------
C
  400 CONTINUE
C
      IF ( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
C
C       Regular lat/long grid ..
        EW = FLOAT( NOGRID(1) ) / PPMULT
        NS = FLOAT( NOGRID(2) ) / PPMULT
C
      ELSE
C
C       Gaussian grid ..
        EW = FLOAT( NOGAUSS )
        NS = 0.0
      ENDIF
C
      NORTH = FLOAT( NOAREA(1) ) / PPMULT
      WEST  = FLOAT( NOAREA(2) ) / PPMULT
      SOUTH = FLOAT( NOAREA(3) ) / PPMULT
      EAST  = FLOAT( NOAREA(4) ) / PPMULT
      IRET = AREACHK( EW, NS, NORTH, WEST, SOUTH, EAST )
      IF ( IRET .NE. 0 ) THEN
         FIXAREA = IRET
         GOTO 900
      ENDIF
      NOAREA(1) = NINT( NORTH * PPMULT + 0.1)
      NOAREA(2) = NINT( WEST  * PPMULT + 0.1)
      NOAREA(3) = NINT( SOUTH * PPMULT + 0.1)
      NOAREA(4) = NINT( EAST  * PPMULT + 0.1)

C EMOS-199: [F|N] grids assume max(pl)=4*N, however O grids have
C max(pl)=4*+16 and AREACHK doesn't know of the difference
C The code below only checks if the adjusted EAST matches what
C would happen on a [F|N] global grid, and corrects it for an O grid.
      IF ( (NOREPR.EQ.JPQUASI)
     X  .AND. (HOGAUST.EQ.'O'
     X  .AND. NOAREA(2).EQ.0
     X  .AND. NOAREA(4).EQ.(JP360-JP90/NOGAUSS) ))
     X  NOAREA(4) = JP360 - (JP360/NOLPTS(NOGAUSS))

C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
