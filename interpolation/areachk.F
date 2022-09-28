C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION AREACHK(EW,NS,NORTH,WEST,SOUTH,EAST)
C
C---->
C**** AREACHK
C
C     Purpose
C     -------
C
C     Fixup input/output field area definitions according to grid
C     specification.
C
C
C     Interface
C     ---------
C
C     IRET = AREACHK(EW,NS,NORTH,WEST,SOUTH,EAST)
C
C     Input
C     -----
C
C     For latitude/longitude grids:
C     EW    =  East-west grid interval (degrees)
C     NS    =  North-south grid interval (degrees)
C
C     For gaussian grids:
C     EW    =  gaussian grid number
C     NS    =  0
C
C     NORTH =  North latitude (degrees)
C     WEST  =  West longitude (degrees)
C     SOUTH =  South latitude (degrees)
C     EAST  =  East longitude (degrees)
C
C     For spherical harmonics:
C     EW    =  0
C     NS    =  0
C     NORTH =  0
C     WEST  =  0
C     SOUTH =  0
C     EAST  =  0
C
C
C     Output
C     ------
C
C     NORTH =  North latitude, adjusted if necessary (degrees)
C     WEST  =  West longitude, adjusted if necessary (degrees)
C     SOUTH =  South latitude, adjusted if necessary (degrees)
C     EAST  =  East longitude, adjusted if necessary (degrees)
C
C
C     Method
C     ------
C
C     If default (0/0/0/0) selected for input, input area is set
C       - to global for lat/long grid
C       - to global for gaussian grid
C
C     Output area is adjusted to fit the given grid by expanding
C     the area if necessary.
C
C
C     Externals
C     ---------
C
C     DSSAREA - Fixup input/output field area (dissemination style).
C     JGETGG  - Reads the definition of a gaussian grid.
C     JNORSGG - Finds the lat row in a gauss grid N or S of given lat
C     INTLOG  - Logs error messages.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Apr 1996
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 19200 )
      REAL RRMULT
#if (defined hp) && (!defined INTEGER_8)
      INTEGER NP90, NP360, NFACTOR
      PARAMETER ( NFACTOR = 10 )
#else
      INTEGER*8 NP90, NP360, NFACTOR
      PARAMETER ( NFACTOR = 1000 )
#endif
      PARAMETER ( NP90    = JP90*NFACTOR )
      PARAMETER ( NP360   = JP360*NFACTOR )
      PARAMETER ( RRMULT  = (PPMULT*NFACTOR) )
C
C     Function arguments
C
      REAL EW, NS, NORTH, WEST, SOUTH, EAST
C
C     Local variables
C
#if (defined hp) && (!defined INTEGER_8)
      INTEGER INORTH, IWEST, ISOUTH, IEAST, IEW, INS
      INTEGER TNORTH, TWEST, TSOUTH, TEAST
      INTEGER NWEST, NEAST
#else
      INTEGER*8 INORTH, IWEST, ISOUTH, IEAST, IEW, INS
      INTEGER*8 TNORTH, TWEST, TSOUTH, TEAST
      INTEGER*8 NWEST, NEAST
#endif
      CHARACTER*12 YFLAG
      CHARACTER*1 HTYPE
      INTEGER NGAUSS, NOLD, IRET, KPTS
      DIMENSION KPTS(JPGTRUNC*2)
      DATA NOLD /0/
      REAL RNORTH, RSOUTH
      LOGICAL LDEFIN, LGAUSS, LSPHERE, LSUBAR, LINSIDE
      REAL RGAUSS
      DIMENSION RGAUSS(JPGTRUNC*2)
      REAL*8 DEW, DNS, DNORTH, DWEST, DSOUTH, DEAST
C
C     Externals
C
      INTEGER JNORSGG, DSSAREA
C
      SAVE NOLD, RGAUSS
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      AREACHK = 0
      LINSIDE=.FALSE.
      LGAUSS  = ( NS.EQ.0.0 )
      LSPHERE = ( (EW.EQ.0.0).AND.(NS.EQ.0.0) )

C     Force grid points inside required area
       CALL GETENV('MARS_INTERPOLATION_INWARDS', YFLAG)
       IF( YFLAG(1:1).EQ.'1' ) LINSIDE = .TRUE.

C     Don't adjust area if rotation specified
C
      IF( LNOROTA ) GOTO 900
C
C     If dissemination style areas required ...
C
      IF( (LNOSTYLE.AND.(NOSTYLE.EQ.JPSDISM)).OR.LINSIDE ) THEN
        AREACHK = DSSAREA( EW, NS, NORTH, WEST, SOUTH, EAST )
        GOTO 900
      ENDIF
C
C     If it's gaussian, pick up the gaussian grid definitions unless
C     they have already been retrieved.
C
      IF( LGAUSS.AND..NOT.LSPHERE ) THEN
        NGAUSS = NINT( EW )
#if (defined hp) && (!defined INTEGER_8)
        IF( NGAUSS.GE.256 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'AREACHK: unsafe to use 32-bit HP version for gaussian = ',
     X      NGAUSS)
          AREACHK = JPROUTINE + 1
          GOTO 900
        ENDIF
#endif
        IF( NGAUSS.GT.JPGTRUNC ) THEN
          CALL INTLOG(JP_ERROR,
     X      'AREACHK: Gaussian grid number ', NGAUSS)
          CALL INTLOG(JP_ERROR,
     X      'AREACHK: exceeds current expected maximum ', JPGTRUNC)
          AREACHK = JPROUTINE + 1
          GOTO 900
        ENDIF
C
        IF( NGAUSS.NE.NOLD ) THEN
          NOLD = NGAUSS
          HTYPE = 'F'
          CALL JGETGG(NOLD,HTYPE,RGAUSS,KPTS,IRET)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'AREACHK: Failed to pick up gaussian ', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        'AREACHK: definitions for resolution = ', NOLD)
            AREACHK = JPROUTINE + 2
            GOTO 900
          ENDIF
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Fixup area if default (0/0/0/0).
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C


C Sinisa added if input area is not global because of stagered grids
C WARNING: the staggered grid condition does not seem good...
      LSUBAR  = ( (NIAREA(1).NE.0).AND.(NIAREA(2).NE.0).AND.
     X            (NIAREA(3).NE.0).AND.(NIAREA(4).NE.0) )
      LSTAGGL = ( (LSUBAR).AND.
     X            (IABS(JP360 - NIAREA(4) - NIAREA(2)).LT.1000) )
      LDEFIN  = ( (NORTH.EQ.0).AND.(WEST.EQ.0).AND.
     X            (SOUTH.EQ.0).AND.(EAST.EQ.0) )
C
      IF( LDEFIN ) THEN
C
        IF( .NOT.LGAUSS ) THEN
C
C         Regular lat/long grid ..
C
          NORTH = 90.0
          WEST  = 0
          SOUTH = -NORTH
          EAST  = 360.0 - EW
C
        ELSE IF( LSPHERE ) THEN
C
C         Spherical harmonics
C
          NORTH = 90.0
          WEST  = 0
          SOUTH = -90.0
          EAST  = 360.0
C
C
        ELSE
C
C         Gaussian grid ..
C WARN: (issue EMOS-199) EAST calculation is only for [F|N] grids, O
C (octahedral) has a finer resolution at the equator for the same NGAUSS
C
          INORTH = JNORSGG( 90.0, RGAUSS, NGAUSS, 0)
          NORTH  = RGAUSS(INORTH)
          WEST   = 0
          ISOUTH = JNORSGG( -90.0, RGAUSS, NGAUSS, 1)
          SOUTH  = -NORTH
          EAST   = 360.0 - 90.0/FLOAT(NGAUSS)
        ENDIF
      ENDIF
C
      IF( LSPHERE ) GOTO 900
C
C     Don't fixup area for ocean products
C
CTim      IF( NILOCAL.EQ.4 ) GOTO 900
C
C     -----------------------------------------------------------------|
C*    Section 3.   Fixup west-east area to correspond to the grid.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( .NOT.LGAUSS ) THEN
C
C       Regular lat/long grid only ..
C
        DEW = EW
        IEW = DNINT( DBLE(DEW * RRMULT) )
C
      ELSE
C
C       Gaussian grid ..
C
C WARN: (issue EMOS-199) EAST calculation is only for [F|N] grids, O
C (octahedral) has a finer resolution at the equator for the same NGAUSS
        IEW = DNINT( (DBLE(NP90) / DBLE(NGAUSS)) )
C
      ENDIF
C
C     Standardise range so that WEST and EAST are >= 0,
C     and EAST > WEST.
C
      IF( WEST.LT.0.0 ) THEN
        WEST = WEST + 360.0
        EAST = EAST + 360.0
      ENDIF
      IF( EAST.GE.WEST ) THEN
        DEAST = EAST
      ELSE
        DEAST = EAST + 360.0
      ENDIF
C
      DWEST = WEST
      IEAST = DNINT( DEAST * DBLE(RRMULT) + 0.1 )
      TEAST = IEAST
      IWEST =  DINT( DWEST * DBLE(RRMULT) + 0.1 )
      TWEST = IWEST
C
C     Round east boundary to nearest gridpoint eastwards,
C     and west boundary to nearest gridpoint westwards.
C
      NEAST = (IEAST+IEW-1) / IEW
      IEAST = NEAST * IEW


        IF(.NOT.LSTAGGL.AND.LSUBAR.AND.IEAST.GT.TEAST) THEN
           IEAST = IEAST - IEW
        ENDIF


      NWEST = IWEST / IEW
      IWEST = NWEST * IEW

c Sinisa to add 1 point if we have stagered global field
        IF(.NOT.LSTAGGL.AND.LSUBAR.AND.IWEST.LT.TWEST) THEN
             IWEST = IWEST + IEW
        ENDIF
C
C     Try to catch special case: global west-east with wrap-around.
C
      IF( (IEAST-IWEST).GE.NP360 ) THEN
        IF( LGAUSS) THEN
          IEAST = IWEST + (4*NGAUSS-1)*IEW
        ELSE
          IEAST = IWEST + NP360 - IEW
        ENDIF
      ENDIF
C
C     Ensure East and West still in range (0,360)
C
      IF( IEAST.GT.NP360 ) THEN
        IEAST = IEAST - NP360
        IWEST = IWEST - NP360
      ENDIF
      IF( IWEST.LT.-NP360 ) THEN
        IEAST = IEAST + NP360
        IWEST = IWEST + NP360
      ENDIF
C
C     Ensure East-West spread is not greater than 360
C
      IF( (IEAST-IWEST).GT.NP360 ) THEN
        IF( LGAUSS) THEN
          IEAST = IWEST + (4*NGAUSS-1)*IEW
        ELSE
          IEAST = IWEST + NP360 - IEW
        ENDIF
      ENDIF
C
C     Apply adjustment when east and west boundaries are the same
C     longitude (0/360) to give a global grid.
C
      IF( ( (IWEST.EQ.-NP360).AND.(IEAST.EQ.0)     ).OR.
     X    ( (IWEST.EQ.0)     .AND.(IEAST.EQ.NP360) ).OR.
     X    ( (IWEST.EQ.NP360) .AND.(IEAST.EQ.NP360) ) ) THEN
        IWEST = 0
        IF( LGAUSS) THEN
          IEAST = (4*NGAUSS-1)*IEW
        ELSE
          IEAST = NP360 - IEW
        ENDIF
      ENDIF
C
      WEST  = DBLE(IWEST) / RRMULT
      EAST  = DBLE(IEAST) / RRMULT
C
      IF( (EAST.LT.WEST).OR.(EAST.GT.360.0) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'AREACHK: Faulty longitude limits', JPQUIET)
        CALL INTLOGR(JP_ERROR,'AREACHK: East = ', EAST)
        CALL INTLOGR(JP_ERROR,'AREACHK: West = ', WEST)
        AREACHK = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Fixup north-south area to correspond to the grid.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
c for ocean fields, adjust ew but not ns boundaries
      IF( NILOCAL.EQ.4 ) GOTO 900
C
      IF( .NOT.LGAUSS ) THEN
C
C       Regular lat/long grid only ..
C
C       Round north boundary to nearest gridpoint northwards.
C
        DNS = NS
        INS = DNINT( DBLE(DNS * RRMULT) )
        DNORTH = NORTH
        INORTH = DNINT( DBLE(DNORTH * RRMULT) )
        TNORTH = INORTH
        IF( INORTH.GE.0 ) THEN
          INORTH = (INORTH+INS-1) / INS
        ELSE
          INORTH = INORTH / INS
        ENDIF
        INORTH = INORTH * INS

        IF( INORTH.GT.NP90 ) INORTH = NP90
        IF( INORTH.LT.-NP90 ) INORTH = -NP90

        IF(.NOT.LSTAGGL.AND.LSUBAR.AND.INORTH.GT.TNORTH) THEN
             INORTH = INORTH - INS
        ENDIF

        NORTH = DBLE(INORTH) / RRMULT
C
C       Round south boundary to nearest gridpoint southwards.
C
        DSOUTH = SOUTH
        ISOUTH = DNINT( DBLE(DSOUTH * RRMULT) )
        TSOUTH = ISOUTH
        IF( ISOUTH.GE.0 ) THEN
          ISOUTH = ISOUTH / INS
        ELSE
          ISOUTH = (ISOUTH-INS+1) / INS
        ENDIF
        ISOUTH = ISOUTH * INS

        IF( ISOUTH.GT.NP90 ) ISOUTH = NP90
        IF( ISOUTH.LT.-NP90 ) ISOUTH = -NP90

        IF(.NOT.LSTAGGL.AND.LSUBAR.AND.ISOUTH.LT.TSOUTH) THEN
             ISOUTH = ISOUTH + INS
        ENDIF


        SOUTH = DBLE(ISOUTH) / RRMULT
C
      ELSE
C
C       Gaussian grid ..
C
C       Round north boundary to nearest gridpoint northwards.
C
        RNORTH = NORTH
        INORTH = JNORSGG( RNORTH, RGAUSS, NGAUSS, 1)
        NORTH  = RGAUSS(INORTH)
C
C       Round south boundary to nearest gridpoint southwards.
C
        RSOUTH = SOUTH
        ISOUTH = JNORSGG( RSOUTH, RGAUSS, NGAUSS, 0)
        SOUTH  = RGAUSS(ISOUTH)
C
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
