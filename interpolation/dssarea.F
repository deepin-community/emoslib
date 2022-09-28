C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION DSSAREA(EW,NS,NORTH,WEST,SOUTH,EAST)
C
C---->
C**** DSSAREA
C
C     Purpose
C     -------
C
C     Fixup input/output field area definitions according to grid
C     specification (dissemination style).
C
C
C     Interface
C     ---------
C
C     IRET = DSSAREA(EW,NS,NORTH,WEST,SOUTH,EAST)
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
C     The function returns 0 if OK.
C     Otherwise a non-zero is returned and the values NORTH, WEST,
C     SOUTH, and EAST are left unchanged.
C
C
C     Method
C     ------
C
C     If default (0/0/0/0) selected for input, input area is set
C       - to global for lat/long grid
C       - to global for gaussian grid
C
C     Output area is adjusted:
C
C     (1) by shrinking to fit the default dissemination 0.5x0.5 grid
C         on its north and west boundaries.
C     (2) by calculating the east boundary from the new west as an
C         integral number of new grid west-east increments to fit
C         inside the specified area.
C     (3) by calculating the south boundary from the new north as an
C         integral number of new grid north-south increments to fit
C         inside the specified area.
C
C
C     Externals
C     ---------
C
C     JGETGG  - Reads the definition of a gaussian grid.
C     JNORSGG - Finds the lat row in a gauss grid N or S of given lat
C     INTLOG  - Logs error messages.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     May 1999
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 19200 )
      REAL*8 RRMULT
#if (defined hp) && (!defined INTEGER_8)
      INTEGER NP90, NP360, NFACTOR
      PARAMETER ( NFACTOR = 10 )
#else
      INTEGER*8 NP90, NP360, NFACTOR
      PARAMETER ( NFACTOR = 1000 )
#endif
      INTEGER NR90
      PARAMETER ( NR90    = 90 )
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
      INTEGER NWEST
#else
      INTEGER*8 INORTH, IWEST, ISOUTH, IEAST, IEW, INS
      INTEGER*8 NWEST
#endif
      CHARACTER*1 HTYPE
      INTEGER NGAUSS, NOLD, IRET, KPTS, NWEPTS, NNSPTS
      REAL RNORTH, RSOUTH
      REAL ONORTH, OWEST, OSOUTH, OEAST
      REAL RGAUSS, RESEW
      REAL*8 DEW, DNS, DNORTH, DWEST, DSOUTH, DEAST
      REAL*8 EPS
      LOGICAL LDEFIN, LGAUSS, LSPHERE
      DIMENSION KPTS(JPGTRUNC*2)
      DIMENSION RGAUSS(JPGTRUNC*2)
      DATA NOLD /0/
C
C     Externals
C
      INTEGER JNORSGG
C
      SAVE NOLD, RGAUSS
C
C     Statement functions
C
      REAL*8 A
      REAL B
      REAL*8 X
      REAL Y
C
      LOGICAL GREATER
      LOGICAL SMALLER
C
      GREATER(A,B) = ( (A-DBLE(B)).GT.EPS )
      SMALLER(X,Y) = ( (DBLE(Y)-X).GT.EPS )
C
      DATA EPS/1E-5/
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      CALL INTLOGR(JP_DEBUG,'DSSAREA: input w-e   = ', EW)
      CALL INTLOGR(JP_DEBUG,'DSSAREA: input n-s   = ', NS)
      CALL INTLOGR(JP_DEBUG,'DSSAREA: input north = ', NORTH)
      CALL INTLOGR(JP_DEBUG,'DSSAREA: input south = ', SOUTH)
      CALL INTLOGR(JP_DEBUG,'DSSAREA: input west  = ', WEST)
      CALL INTLOGR(JP_DEBUG,'DSSAREA: input east  = ', EAST)
C
      ONORTH = NORTH
      OSOUTH = SOUTH
      OWEST  = WEST
      OEAST  = EAST
C
      DSSAREA = 0
      LGAUSS  = ( NS.EQ.0.0 )
      LSPHERE = ( (EW.EQ.0.0).AND.(NS.EQ.0.0) )
C
C     If it's gaussian, pick up the gaussian grid definitions unless
C     they have already been retrieved.
C
      IF( LGAUSS.AND..NOT.LSPHERE ) THEN
        NGAUSS = NINT( EW )
#if (defined hp) && (!defined INTEGER_8)
        IF( NGAUSS.GE.256 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'DSSAREA: unsafe to use 32-bit HP version for gaussian = ',
     X      NGAUSS)
          DSSAREA = JPROUTINE + 1
          GOTO 900
        ENDIF
#endif
        IF( NGAUSS.GT.JPGTRUNC ) THEN
          CALL INTLOG(JP_ERROR,
     X      'DSSAREA: Gaussian grid number ', NGAUSS)
          CALL INTLOG(JP_ERROR,
     X      'DSSAREA: exceeds current expected maximum ', JPGTRUNC)
          DSSAREA = JPROUTINE + 1
          GOTO 900
        ENDIF
C
        IF( NGAUSS.NE.NOLD ) THEN
          NOLD = NGAUSS
          HTYPE = 'F'
          CALL JGETGG(NOLD,HTYPE,RGAUSS,KPTS,IRET)
          IF( IRET.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'DSSAREA: Failed to pick up gaussian ', JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        'DSSAREA: definitions for resolution = ', NOLD)
            DSSAREA = JPROUTINE + 1
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
      LDEFIN = ( (NORTH.EQ.0).AND.(WEST.EQ.0).AND.
     X           (SOUTH.EQ.0).AND.(EAST.EQ.0) )
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
          SOUTH  = RGAUSS(ISOUTH)
          EAST   = 360.0 - 90.0/REAL(NGAUSS)
        ENDIF
C
        GOTO 900
      ENDIF
C
      IF( LSPHERE ) GOTO 900
C
C     Don't fixup area for ocean products
C
      IF( NILOCAL.EQ.4 ) GOTO 900
C
C     -----------------------------------------------------------------|
C*    Section 3.   Fixup west-east area to correspond to the grid.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( EAST.GT.360.0.AND.EAST.LT.361.0 ) THEN
        CALL INTLOGR(JP_WARN,
     X    'DSSAREA: Faulty East lon limits', EAST)
        EAST = 360.0
        CALL INTLOGR(JP_WARN,
     X    'DSSAREA: Changed East to', EAST)
      ENDIF
c
      IF( (WEST.LT.-360.0).OR.(WEST.GT.360.0).OR.
     X    (EAST.LT.-360.0).OR.(EAST.GT.360.0) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'DSSAREA: Faulty input langitude limits', JPQUIET)
        CALL INTLOGR(JP_ERROR,'DSSAREA: West = ', WEST)
        CALL INTLOGR(JP_ERROR,'DSSAREA: East = ', EAST)
        NORTH = ONORTH
        SOUTH = OSOUTH
        WEST  = OWEST
        EAST  = OEAST
        DSSAREA = JPROUTINE + 4
        GOTO 900
      ENDIF
C
      IF( .NOT. LGAUSS ) THEN
C
C       Regular lat/long grid only ..
C
        DEW = JPDISTP * RRMULT
        IEW = DNINT( DBLE(DEW) )
        RESEW = EW
C
      ELSE
C
C       Gaussian grid ..
C
        DEW = DBLE(NP90) / DBLE(NGAUSS)
        IEW = DNINT( DBLE(DEW) )
        RESEW = DBLE(NR90) / DBLE(NGAUSS)
C
      ENDIF
C
C     Ensure West < East
C
      IF( WEST.GT.EAST ) WEST = WEST - 360.0
      DWEST = WEST
      IWEST = DNINT( DBLE(DWEST * RRMULT) )
C
C     Find west boundary to nearest dissemination gridpoint eastwards.
C     From this value, calculate the east limit using the requested
C     grid increment.
C
      IF( IWEST.GT.0 ) THEN
        NWEST =  DINT(DBLE(IWEST + IEW - 1) / DEW)
        IWEST = DNINT(DBLE(NWEST) * DEW)
      ELSE
        NWEST =  DINT(DBLE(-IWEST) / DEW)
        IWEST = DNINT(DBLE(-NWEST) * DEW)
      ENDIF
C
      DWEST = DBLE(IWEST)/RRMULT
C
      DEAST = EAST
C
      NWEPTS = NINT( (DEAST - DWEST) / RESEW )
      DEAST  = DWEST + (NWEPTS*RESEW)
      IF( GREATER(DEAST,EAST) ) DEAST = DEAST - RESEW
      CALL INTLOGR(JP_DEBUG,'DSSAREA: calculated west  = ', DWEST)
      CALL INTLOGR(JP_DEBUG,'DSSAREA: calculated east  = ', DEAST)
      IEAST = DNINT( DBLE(DEAST * RRMULT) )
C
C     Try to catch special case: global west-east with wrap-around.
C
      IF( (IEAST-IWEST).GE.NP360 ) THEN
        IF( LGAUSS) THEN
          IEAST = IWEST + (4*NGAUSS-1)*RESEW*RRMULT
        ELSE
          IEAST = IWEST + NP360 - RESEW*RRMULT
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
      IF( ( (IWEST.EQ.-NP360).AND.(IEAST.EQ.0)     ) .OR.
     X    ( (IWEST.EQ.0)     .AND.(IEAST.EQ.NP360) ) .OR.
     X    ( (IWEST.EQ.NP360) .AND.(IEAST.EQ.NP360) )  ) THEN
        IWEST = 0
        IF( LGAUSS) THEN
          IEAST = (4*NGAUSS-1)*IEW
        ELSE
          IEAST = NP360 - IEW
        ENDIF
      ENDIF
C
      DWEST  = DBLE(IWEST) / RRMULT
      DEAST  = DBLE(IEAST) / RRMULT
      WEST  = DBLE(IWEST) / RRMULT
      EAST  = DBLE(IEAST) / RRMULT
C
      IF( (EAST.LT.WEST).OR.(EAST.GT.360.0) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'DSSAREA: Faulty calculated longitude limits', JPQUIET)
        CALL INTLOGR(JP_ERROR,'DSSAREA: East = ', EAST)
        CALL INTLOGR(JP_ERROR,'DSSAREA: West = ', WEST)
        NORTH = ONORTH
        SOUTH = OSOUTH
        WEST  = OWEST
        EAST  = OEAST
        DSSAREA = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Fixup north-south area to correspond to the grid.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IF( NORTH.LT.SOUTH ) THEN
        CALL INTLOG(JP_ERROR,
     X    'DSSAREA: Faulty input latitude limits', JPQUIET)
        CALL INTLOGR(JP_ERROR,'DSSAREA: North = ', NORTH)
        CALL INTLOGR(JP_ERROR,'DSSAREA: South = ', SOUTH)
        DSSAREA = JPROUTINE + 1
        GOTO 900
      ENDIF
cs
      IF( NORTH.GT.90.0 ) THEN
        CALL INTLOGR(JP_WARN,
     X    'DSSAREA: Faulty North lat limits', NORTH)
        NORTH = 90.0
        CALL INTLOGR(JP_WARN,
     X    'DSSAREA: Changed North to', NORTH)
      ENDIF
cs
      IF( SOUTH.LT.-90.0 ) THEN
        CALL INTLOGR(JP_WARN,
     X    'DSSAREA: Faulty South lat limits', SOUTH)
        SOUTH = -90.0
        CALL INTLOGR(JP_WARN,
     X    'DSSAREA: Changed South to', SOUTH)
      ENDIF

      IF( .NOT.LGAUSS ) THEN
C
C       Regular lat/long grid only ..
C
        DNS = JPDISTP * RRMULT
        INS = DNINT( DBLE(DNS) )
C
C       Find north boundary to nearest dissemination gridpoint south.
C       From this value, calculate the south limit using the requested
C       grid increment.
C
        DNORTH = NORTH
        INORTH = DNINT( DBLE(DNORTH * RRMULT) )
        IF( INORTH.GE.0 ) THEN
          INORTH = INORTH / INS
        ELSE
          INORTH = (INORTH-INS+1) / INS
        ENDIF
        INORTH = INORTH * INS
        IF( INORTH .GT. NP90 ) INORTH = NP90
        IF( INORTH .LT. -NP90 ) INORTH = -NP90
        NORTH = DBLE(INORTH) / RRMULT
C
        DNORTH = NORTH
        DSOUTH = SOUTH
        NNSPTS = NINT( (DNORTH - DSOUTH) / NS )
        DSOUTH = DNORTH - (NNSPTS* NS)
        IF( SMALLER(DSOUTH,SOUTH) ) DSOUTH = DSOUTH + NS
        CALL INTLOGR(JP_DEBUG,'DSSAREA: calculated north = ', DNORTH)
        CALL INTLOGR(JP_DEBUG,'DSSAREA: calculated south = ', DSOUTH)
        ISOUTH = DNINT( DBLE(DSOUTH * RRMULT) )

        IF( ISOUTH .GT. NP90 ) ISOUTH = NP90
        IF( ISOUTH .LT. -NP90 ) ISOUTH = -NP90
        SOUTH = DBLE(ISOUTH) / RRMULT
C
      ELSE
C
C       Gaussian grid ..
C
C       Round north boundary to nearest gridpoint southwards.
C
        RNORTH = NORTH
        INORTH = JNORSGG( RNORTH, RGAUSS, NGAUSS, 0)
        NORTH  = RGAUSS(INORTH)
C
C       Round south boundary to nearest gridpoint northwards.
C
        RSOUTH = SOUTH
        ISOUTH = JNORSGG( RSOUTH, RGAUSS, NGAUSS, 1)
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
