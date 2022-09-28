C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INSANE( )
C
C---->
C**** INSANE
C
C     Purpose
C     -------
C
C     Check interpolate options before processing.
C
C
C     Interface
C     ---------
C
C     IRET = INSANE( )
C
C     Input
C     -----
C
C     Common blocks nifld.common and nofld.common
C
C
C     Output
C     ------
C
C     Returns: 0 if no problems spotted.
C              -1 if output is same as the input
C
C
C     Method
C     ------
C
C     See below!
C
C
C     Externals
C     ---------
C
C     ISSAME  - Checks if input specification is same as output.
C     INTLOG  - Logs error messages.
C     INTLOGR - Logs error messages.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Feb 1995
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
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 27200 )
      INTEGER JPMAXRES
      PARAMETER (JPMAXRES = JPSTRUNC)
C
C     Local variables
C
      CHARACTER*12 YFLAG
      INTEGER IERR, MINGRID
      REAL GRIDINT
      LOGICAL LINCHECK,LNLIMIT
C
C     Externals
      LOGICAL ISSAME
      INTEGER FFTCHK
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      INSANE = 0
      IERR = 0
C
C     Exit if output is same as input ...
C
      IF ( ISSAME() ) THEN
        INSANE = -1
        GOTO 900
      ENDIF
C
C     Check output packing accuracy requested (packed output only)
C
      IF ( NOFORM .EQ. 1 ) THEN
        IF ( (NOACC .GT. 30) .OR. (NOACC .LT. 1) ) THEN
          CALL INTLOG(JP_DEBUG,
     X        'INSANE: Invalid output packing given ',NOACC)
          NOACC = 16
          CALL INTLOG(JP_DEBUG,
     X        'INSANE: Changed to ',NOACC)
cs          CALL INTLOG(JP_ERROR,
cs     X      'Sorry!: Invalid output packing given',NOACC)
cs          IERR   = JPROUTINE + 1
cs          CALL INTLOG(JP_ERROR,
cs     X      '        Must be between 1 and 30 bits per value.',IERR)
cs          INSANE = IERR
cs          GOTO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Check valid input/output representations.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Input
C
      IF ( (NIREPR .NE. JPREGULAR ) .AND.
     X     (NIREPR .NE. JPREDLL   ) .AND.
     X     (NIREPR .NE. JPSPHERE  ) .AND.
     X     (NIREPR .NE. JPSPHROT  ) .AND.
     X     (NIREPR .NE. JPGAUSSIAN) .AND.
     X     (NIREPR .NE. JPSTRSH   ) .AND.
     X     (NIREPR .NE. JPSTRGG   ) .AND.
     X     (NIREPR .NE. JPQUASI   ) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Invalid input representation.', NIREPR)
        INSANE = JPROUTINE + 21
        GOTO 900
      ENDIF
C
C     Output
C
      IF ( (NOREPR .NE. JPREGULAR ) .AND.
     X     (NOREPR .NE. JPREGROT  ) .AND.
     X     (NOREPR .NE. JPSPHERE  ) .AND.
     X     (NOREPR .NE. JPSPHROT  ) .AND.
     X     (NOREPR .NE. JPGAUSSIAN) .AND.
     X     (NOREPR .NE. JPSTRSH   ) .AND.
     X     (NOREPR .NE. JPSTRGG   ) .AND.
     X     (NOREPR .NE. JPQUASI   ) .AND.
     X     (NOREPR .NE. JPFGGROT  ) .AND.
     X     (NOREPR .NE. JPQGGROT  ) .AND.
     X     (NOREPR .NE. JPREDLL   ) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Invalid output representation.', NOREPR)
        INSANE = JPROUTINE + 22
        GOTO 900
      ENDIF
C
C     Input / output combination
C
      IF ( (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) THEN
        IF ( (NIREPR.NE.JPSPHERE).AND.(NIREPR.NE.JPSPHROT) ) THEN
C
C         Set flag to discontinue attempts at interpolation, ie
C         force output to be same as input
C
          INSANE = -1
          GOTO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Check spectral to grid options.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      MINGRID = JP0P5 / 10
C
      IF ( (NIREPR.EQ.JPSPHERE).OR.(NIREPR.EQ.JPSPHROT) ) THEN
C
C       Check resolution not too large
C
        IF( (NOREPR.NE.JPSPHERE).AND.(NIRESO.GT.JPMAXRES) ) THEN
C
C         Problem unless a suitable truncation is on-the-way
C
          IF( LARESOL.OR.LNORESO ) THEN
            IF( MIN(NOGRID(1),NOGRID(2)).LT.MINGRID ) THEN
              CALL INTLOG(JP_ERROR,
     X        'Sorry!: Despite truncation,', JPQUIET)
              CALL INTLOG(JP_ERROR,
     X        '        spectral to grid transformation', JPQUIET)
              CALL INTLOG(JP_ERROR,
     X        '        cannot be done (grid too fine).', JPQUIET)
              CALL INTLOG(JP_ERROR,
     X        '        Grid west-east (* 10**-5)   = ', NOGRID(1))
              CALL INTLOG(JP_ERROR,
     X        '        Grid north-south (* 10**-5) = ', NOGRID(2))
              CALL INTLOG(JP_ERROR,
     X        '        Minimum allowed (* 10**-5)  = ', MINGRID)
              IERR   = JPROUTINE + 31
              INSANE = IERR
              GOTO 900
            ENDIF
C
          ELSE
C
            CALL INTLOG(JP_ERROR,
     X      'Sorry!: Spectral truncation too large for ', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X      '        spectral to grid transformation.', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X      '        Truncation given = ', NIRESO)
            CALL INTLOG(JP_ERROR,
     X      '        Max. truncation handled = ', JPMAXRES)
            IERR   = JPROUTINE + 31
            INSANE = IERR
            GOTO 900
          ENDIF
C
        ENDIF
C
C       Check resolution not too small
C
        IF ( (NOREPR .NE. JPSPHERE) .AND. (NIRESO .LE. 0) ) THEN
          IERR   = JPROUTINE + 32
          CALL INTLOG(JP_ERROR,
     X      'Sorry!: Spectral truncation too small for ', JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      '        spectral to grid transformation.', JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      '        Truncation given = ', NIRESO)
          CALL INTLOG(JP_ERROR,
     X      '        Truncation must be > ', 0)
          INSANE = IERR
          GOTO 900
        ENDIF
C
C       Check grid interval
C
        IF ( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
C
C         Not too small
C
C if env variable 1 no check of interval
      LNLIMIT = .TRUE.
      CALL GETENV('INCREMENT_NO_LIMIT', YFLAG)
      IF(YFLAG(1:1).EQ.'1' ) LNLIMIT = .FALSE.

C
        IF(LNLIMIT) THEN
          IF( (NOGRID(1).LT.MINGRID).OR.(NOGRID(2).LT.MINGRID) ) THEN
            IERR   = JPROUTINE + 33
            CALL INTLOG(JP_ERROR,
     X        'Sorry!: Grid interval too small for ', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        '        spectral to grid transformation.', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        '         Latitude interval given = ', NOGRID(1))
            CALL INTLOG(JP_ERROR,
     X        '        Longitude interval given = ', NOGRID(2))
            CALL INTLOG(JP_ERROR,
     X        '         Min. interval handled = ', MINGRID)
            INSANE = IERR
            GOTO 900
          ENDIF
       ELSE
          CALL INTLOG(JP_DEBUG,
     X    'INSANE: Increments checking disabled',JPQUIET)
       ENDIF
C
C         Not too large
C
          IF ( (NOGRID(1).GT.JP90) .OR. (NOGRID(2).GT.JP90) ) THEN
            IERR   = JPROUTINE + 34
            CALL INTLOG(JP_ERROR,
     X        'Sorry!: Grid interval too large for ', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        '        spectral to grid transformation.', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        '         Latitude interval given = ', NOGRID(1))
            CALL INTLOG(JP_ERROR,
     X        '        Longitude interval given = ', NOGRID(2))
            CALL INTLOG(JP_ERROR,
     X        '         Max. interval handled = ', JP90)
            INSANE = IERR
            GOTO 900
          ENDIF
C
C         A multiple of 0.0005
C
C if env variable 1 no check of increments
      LINCHECK = .FALSE.
      CALL GETENV('INCREMENT_CHECK', YFLAG)
      IF(YFLAG(1:1).EQ.'1' ) LINCHECK = .TRUE.

      IF( LINCHECK ) CALL INTLOG(JP_DEBUG,
     X  'INSANE: Increments checking disabled',JPQUIET)
C
        IF(.NOT.LINCHECK) THEN
          IF ( (MOD(NOGRID(1),5).NE.0) .OR.
     X         (MOD(NOGRID(2),5).NE.0) ) THEN
            IERR   = JPROUTINE + 35
            CALL INTLOG(JP_ERROR,
     X        'Sorry!: Grid must be a multiple of 0.0005 ', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        '         Latitude interval given = ', NOGRID(1))
            CALL INTLOG(JP_ERROR,
     X        '        Longitude interval given = ', NOGRID(2))
            INSANE = IERR
            GOTO 900
          ENDIF
        ENDIF
      ENDIF
C
C       Check if combination of truncation and longitude grid interval
C       can be handled by the FFT algorithm.
C
        IERR = 1
        GRIDINT = -1.0
C
        IF ( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
          GRIDINT = FLOAT(NOGRID(2))/FLOAT(JPMULT)
          IERR = FFTCHK( NIRESO, GRIDINT )
          IF ( IERR.NE.1 ) THEN
            CALL INTLOG(JP_ERROR,
     X      'Sorry!: Invalid combination of spectral number = ', NIRESO)
            CALL INTLOGR(JP_ERROR,
     X        '        and longitude grid interval = ', GRIDINT)
            CALL INTLOG(JP_ERROR,
     X        '        for FFT algorithm in use.', JPQUIET)
            INSANE = JPROUTINE + 36
            GOTO 900
          ENDIF
        ENDIF
C
        IF ( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPQUASI) ) THEN
          GRIDINT = 360.0/FLOAT(NOGAUSS)
          IERR = FFTCHK( NIRESO, GRIDINT )
          IF ( IERR.NE.1 ) THEN
            CALL INTLOG(JP_ERROR,
     X      'Sorry!: Invalid combination of spectral number = ', NIRESO)
            CALL INTLOGR(JP_ERROR,
     X        '        and longitude grid intvl = ', GRIDINT)
            CALL INTLOG(JP_ERROR,
     X        '        from gaussian number = ', NOGAUSS)
              CALL INTLOG(JP_ERROR,
     X          '        for FFT algorithm in use.', JPQUIET)
            INSANE = JPROUTINE + 37
            GOTO 900
          ENDIF
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Check legal area definition.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Don't check area for ocean products
C
      IF( NILOCAL.EQ.4 ) GOTO 900
C
      IF ( (NOAREA(1).GT.JP90).OR.(NOAREA(1).LT.-JP90) ) THEN
        IERR   = JPROUTINE + 41
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Output area definition illegal',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    '        Northern limit given as ', NOAREA(1))
        INSANE = IERR
        GOTO 900
      ENDIF
C
      IF ( (NOAREA(3).GT.JP90).OR.(NOAREA(3).LT.-JP90) ) THEN
        IERR   = JPROUTINE + 42
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Output area definition illegal',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    '        Southern limit given as ', NOAREA(3))
        INSANE = IERR
        GOTO 900
      ENDIF
C
      IF ( (NOAREA(2).GT.JP360).OR.(NOAREA(2).LT.-JP360) ) THEN
        IERR   = JPROUTINE + 43
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Output area definition illegal',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    '        Western limit given as ', NOAREA(2))
        INSANE = IERR
        GOTO 900
      ENDIF
C
      IF ( (NOAREA(4).GT.JP360).OR.(NOAREA(4).LT.-JP360) ) THEN
        IERR   = JPROUTINE + 44
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Output area definition illegal',JPQUIET)
        CALL INTLOG(JP_ERROR,
     X    '        Eastern limit given as ', NOAREA(4))
        INSANE = IERR
        GOTO 900
      ENDIF
C
C     Subarea not supported for reduced gaussian grids
C
      IF( LNOAREA.AND.
     X    ((NOREPR.EQ.JPQUASI).OR.
     X     (NOREPR.EQ.JPQGGROT).OR.
     X     (NOREPR.EQ.JPSTRGG)) ) THEN
        IERR   = JPROUTINE + 45
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Subarea not supported for reduced gaussian grids',
     X    JPQUIET)
        INSANE = IERR
        GOTO 900
      ENDIF
C
C     Subarea not supported for reduced lat/lon to reduced lat/lon
C
      IF( LNOAREA.AND.
     X    ((NIREPR.EQ.JPREDLL).AND.(NOREPR.EQ.JPREDLL)) ) THEN
        IERR   = JPROUTINE + 46
        CALL INTLOG(JP_ERROR,
     X    'Sorry!: Subarea not supported for reduced lat/lon '//
     X    'to reduced lat/lon grids',
     X    JPQUIET)
        INSANE = IERR
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5.   Check legal gaussian grid definition.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
C     If input is quasi- regular gaussian, a grid definition must be
C     supplied in a GRIB record or by the caller, or the grid must be
C     a known definition.
C
      IF ( (NIREPR.EQ.JPQUASI) ) THEN
        IF ( (.NOT. (LIGIVEL.AND.LIGIVEP)) .AND.
     X       (.NOT. (NIFORM.EQ.1))       ) THEN
          IF (
     x         (NIGAUSS.NE.32) .AND.
     x         (NIGAUSS.NE.48) .AND.
     x         (NIGAUSS.NE.64) .AND.
     x         (NIGAUSS.NE.80) .AND.
     x         (NIGAUSS.NE.96) .AND.
     x         (NIGAUSS.NE.128).AND.
     X         (NIGAUSS.NE.160).AND.
     X         (NIGAUSS.NE.200).AND.
     X         (NIGAUSS.NE.256).AND.
     X         (NIGAUSS.NE.320).AND.
     X         (NIGAUSS.NE.400).AND.
     X         (NIGAUSS.NE.512).AND.
     X         (NIGAUSS.NE.640).AND.
     X         (NIGAUSS.NE.1024).AND.
     X         (NIGAUSS.NE.1280) ) THEN
            CALL INTLOG(JP_ERROR,
     X        'Sorry!: No definition for quasi gauss. input',NIGAUSS)
            INSANE = JPROUTINE + 51
            GOTO 900
          ENDIF
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 6.   Check legal rotation definition.
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
C     Check for rotation.
C
      IF ( LNOROTA ) THEN
C
C       Only allowed rotations are:
C        - spectral to regular lat/long
C        - regular gaussian to regular lat/long
C        - reduced gaussian to regular lat/long
C
        IF( ((NOREPR.NE.JPREGROT).AND.(NOREPR.NE.JPREGULAR)).OR.
     X      ((NIREPR.NE.JPSPHERE)  .AND.
     X       (NIREPR.NE.JPGAUSSIAN).AND.
     X       (NIREPR.NE.JPQUASI)) ) THEN
          INSANE = JPROUTINE + 61
          GOTO 900
        ENDIF
C
C       Check rotation angles.
C
        IF ( (NOROTA(1).LT.-JP360) .OR. (NOROTA(1).GT.JP360) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'Sorry!: Rotation longitude out of range = ',NOROTA(1))
          INSANE = JPROUTINE + 62
          GOTO 900
        ENDIF
C
        IF ( (NOROTA(2).LT.-JP180) .OR. (NOROTA(2).GT.JP180) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'Sorry!: Rotation longitude out of range = ',NOROTA(2))
          INSANE = JPROUTINE + 63
          GOTO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 7.   Check legal definition for table, parameter, level
C                  type.
C     -----------------------------------------------------------------|
C
  700 CONTINUE
C
C     Table
C
c      IF ( (NOTABLE.LT.0) .OR. (NOTABLE.GT.255) ) THEN
      IF (NOTABLE.LT.0) THEN
        CALL INTLOG(JP_ERROR,
     X      'Sorry!: Table definition out of range = ', NOTABLE)
        INSANE = JPROUTINE + 71
        GOTO 900
      ENDIF
C
C     Parameter
C
cs      IF ( (NOPARAM.LT.0) .OR. (NOPARAM.GT.255) ) THEN
      IF ( NOPARAM.LT.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X      'Sorry!: Parameter definition out of range = ', NOPARAM)
        INSANE = JPROUTINE + 72
        GOTO 900
      ENDIF
C
C     Level type
C
      IF ( (NOLEVT.LT.0) .OR. (NOLEVT.GT.255) ) THEN
        CALL INTLOG(JP_ERROR,
     X      'Sorry!:  Level type definition out of range = ', NOLEVT)
        INSANE = JPROUTINE + 73
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
