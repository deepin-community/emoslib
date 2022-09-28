C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IAGCNTL(PIFELD, KILEN, POFELD, KOLEN, KILN,
     X   OIWEGLOBE, OINPOLE, OISPOLE, KOLN, OOWEGLOBE, OONPOLE,
     X   OOSPOLE, KPR, KERR)
C
C---->
C**** *IAGCNTL*
C
C     PURPOSE
C     _______
C
C     The global controlling routine for interpolating between
C     regular Gaussian or latitude/longitude fields.
C
C     INTERFACE
C     _________
C
C     IERR = IAGCNTL(PIFELD, KILEN, POFELD, KOLEN, KILN, OIWEGLOBE,
C    X   OINPOLE, OISPOLE, KOLN, OOWEGLOBE, OONPOLE, OOSPOLE, KPR,
C    X   KERR)
C
C     Input parameters
C     ________________
C
C     PIFELD     - The input field provided by the calling routine.
C
C     KILEN      - The length of the input field.
C
C     KOLEN      - The length of the output field.
C
C     KILN       - The Northern line number of the input field within
C                  its Gaussian field.
C
C     OIWEGLOBE  - A flag indicating whether the input field is
C                  global West to East.
C
C     OINPOLE    - A flag indicating whether the North pole is
C                  included in the input field.
C
C     OISPOLE    - A flag indicating whether the South pole is
C                  include in the input field.
C
C     KOLN       - The Northern line number of the output field
C                  within its Gaussian field.
C
C     OOWEGLOBE  - A flag indicating whether the output field is
C                  global West to East.
C
C     OONPOLE    - A flag indicating whether the North pole is
C                  include in the output field.
C
C     OOSPOLE    - A flag indicating whether the South pole is
C                  include in the output field.
C
C     KPR        - The debug print switch.
C                  0  , No debugging output.
C                  1  , Produce debugging output.
C
C     KERR       - The error control flag.
C                  -ve, No error message. Return error code.
C                  0  , Hard failure with error message.
C                  +ve, Print error message. Return error code.
C
C     Output parameters
C     ________________
C
C     POFELD     - The output field returned to the calling routine.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     21801 The input data representation was not valid when the
C           grid was generated. This should not happen as the data
C           representation has been previously validated.
C     21802 The output data representation was not valid when the
C           grid was generated. This should not happen as the data
C           representation has been previously validated.
C     21803 Attempt to produce global West-East field where the input
C           field was not global.
C     21804 Attempt to produce global North-South field where the
C           input field was not global.
C
C     Common block usage
C     __________________
C
C     grspace.h    - The include file contains all the array space
C                    for grid to grid interpolation.
C
C     RIGAUSS      - Real array of input field Gaussian latitudes is
C                    used.
C     ROGAUSS      - Real array of output field Gaussian latitudes is
C                    used.
C
C     MILATG       - The input field latitudes.
C     MILONG       - The input field longitudes.
C     MNSDIST      - The distances to neighbouring latitude lines of
C                    the input field from the associated line of
C                    latitude in the output field.
C     MNSIND       - The latitude line numbers (array offset) of the
C                    input field associated with each line of
C                    latitude in the output field.
C     MOLATG       - The output field latitudes.
C     MOLONG       - The output field longitudes.
C     MWEDIST      - This array holds the distances to neighbouring
C                    longitude points of the input field from the
C                    associated longitude points in the output field.
C     MWEIND       - This array holds the longitude points (array
C                    offset) from the input field associated with
C                    each longitude point in the output field.
C     RINPNT       - Array used to aid vectorisation in processing
C                    precipitation fields.
C     RMAX         - Array used to aid vectorisation in normalising
C                    interpolation weights and processing
C                    precipitation fields.
C     WFACT        - The array of interpolation weights for each
C                    point in the output field.
C
C     nifld.common - This file contains all the input field
C                    definition variables.
C
C     LCHANGE      - Process change flag is used and set.
C     LSMCHNG      - LSM change flag is used and set.
C     LSM          - LSM usage flag is used.
C     LSMPAR       - LSM parameter flag is used.
C     LPREC        - Precipitation flag is used.
C     LWIND        - Wind flag is used.
C     LSTAGGL      - Staggered grid flag
C
C     NIAREA       - Input field area definition (N/W/S/E) is used.
C     NIGAUSS      - Input field Gaussian truncation is used.
C     NIGRID       - Input field grid definition (WE/NS) is used.
C     NINS         - Number of grid points in NS direction for input
C                    field is used.
C     NIREPR       - Input field representation is used.
C     NIWE         - Number of grid points in WE direction for input
C                    field is used.
C
C     nofld.common - This file contains all the output field
C                    definition variables.
C
C     NOAREA       - Output field area definition (N/W/S/E) is used.
C     NOGAUSS      - Output field Gaussian truncation is used.
C     NOGRID       - Output field grid definition (WE/NS) is used.
C     NONS         - Number of grid points in NS direction for output
C                    field is used.
C     NOREPR       - Output field representation is used.
C     NOWE         - Number of grid points in WE direction for output
C                    field is used.
C
C     EXTERNALS
C     _________
C
C     IGDINS     - Calculate the distances between points in an
C                  output latitude array and its North and South
C                  neighbours in the input latitude array.
C     IGDIWE     - Calculate the distances between points in an
C                  output longitude array and its West and East
C                  neighbours in the input longitude array.
C     IGGMEM     - This routine acquires heap space.
C     IGGRID     - Generate the arrays of latitude and longitude
C                  points for a Gaussian truncation and area.
C     IGINT      - Calculate the basic unnormalised interpolation
C                  weights.
C     IGLGRID    - Generate the arrays of latitude and longitude
C                  points for a regular latitude/longitude grid.
C     IGLSMB     - Calculate the effects of the land-sea masks on
C                  the unnormalised interpolation weights.
C     IGLSMD     - Generate the land sea mask file information.
C     IGNORM     - Normalise the array of interpolation weights.
C     IGPLSM     - Force an interpolated land sea mask field back
C                  to a real 0-1 field.
C     IGPOLEG    - Calculates the values at the pole of a regular
C                  latitude/longitude field when the input is a
C                  Gaussian field.
C     IGPOLEW    - Calculates the values at the pole of a regular
C                  latitude/longitude wind field when the input is
C                  a Gaussian field.
C     IGPREC     - Perform additional interpolation for
C                  precipitation fields.
C     ZPREC      - Perform additional interpolation processes
C                  for precipitation
C     IGTOG      - Perform basic interpolation between regular input
C                  and output fields.
C     INTLOG       - Logs messages.
C
C     METHOD
C     ______
C
C     This is purely a controlling routine with all the work being
C     performed in the external routines.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     None
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      May 1994
C
C     MODIFICATIONS
C     _____________
C
C     J.Chambers        ECMWF       Oct 1998
C     Allow field type for rotated grids.
C
C     S.Curic           ECMWF       Oct 2005
C     Add zprec routine that Perform checking
C     of precipitation

C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grspace.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 21800)
C
C     Function arguments
C
      LOGICAL OIWEGLOBE, OINPOLE, OISPOLE, OOWEGLOBE, OONPOLE, OOSPOLE
      INTEGER KILEN, KOLEN, KILN, KOLN, KPR, KERR
      REAL PIFELD(KILEN), POFELD(KOLEN)
C
C     Local variables
C
      LOGICAL LFAIL, LERROR, LDEBUG, LREGIN, LREGOUT
      LOGICAL GINSPOLE, GIGLOBE, GISTAND, GONSPOLE, GOGLOBE, GOSTAND
      LOGICAL GPREC,LSM_VALUES
      CHARACTER*256 YIFILE, YOFILE
      CHARACTER*12 YFLAG
      INTEGER IIOFF, IIREC, IOOFF, IOREC, IERR
C
C     Working copies of area and grid arrays that may be changed
C
      INTEGER IILGRID(2), IILLINE(2), IOLGRID(2), IOLLINE(2)
C
C     External functions
C
      INTEGER IGDINS, IGDIWE, IGGMEM, IGGRID, IGINT, IGLGRID,
     X   IGLSMB, IGLSMD, IGNORM, IGPLSM, IGPOLEG, IGPOLEW, IGPREC,
     X   IGTOG, ZPREC
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialisation
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IAGCNTL = 0
      IERR = 0
C
C     Set all flag variables
C
      LDEBUG  = (KPR.GE.1)
      LFAIL   = (KERR.EQ.0)
      LERROR  = (KERR.GE.0)
      LREGIN  = (NIREPR.EQ.JPREGULAR).OR.(NIREPR.EQ.JPREGROT)
      LREGOUT = (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT)
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'IAGCNTL: Section 1.',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'IAGCNTL: Input field definition parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IAGCNTL: Representation = ',NIREPR)
        IF( NIWE.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IAGCNTL: Number W-E = ',NIWE)
        IF( NINS.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IAGCNTL: Number N-S = ',NINS)
        IF( (NIREPR.EQ.JPGAUSSIAN).OR.(NIREPR.EQ.JPQUASI) )
     X    CALL INTLOG(JP_DEBUG,'IAGCNTL: Gaussian truncation= ',NIGAUSS)
        IF( LREGIN.AND.(NIGRID(1).NE.0) ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Grid WE = ',NIGRID(1))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Grid NS = ',NIGRID(2))
        ENDIF
        IF( NIAREA(1).NE.0 ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area North = ',NIAREA(1))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area West  = ',NIAREA(2))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area South = ',NIAREA(3))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area East  = ',NIAREA(4))
        ENDIF
C
        CALL INTLOG(JP_DEBUG,
     X    'IAGCNTL: Output field definition parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IAGCNTL: Representation = ',NOREPR)
        IF( NOWE.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IAGCNTL: Number W-E = ',NOWE)
        IF( NONS.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IAGCNTL: Number N-S = ',NONS)
        IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPQUASI) )
     X    CALL INTLOG(JP_DEBUG,'IAGCNTL: Gaussian truncation= ',NOGAUSS)
        IF( LREGOUT.AND.(NOGRID(1).NE.0) ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Grid WE = ',NOGRID(1))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Grid NS = ',NOGRID(2))
        ENDIF
        IF( NOAREA(1).NE.0 ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area North = ',NOAREA(1))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area West  = ',NOAREA(2))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area South = ',NOAREA(3))
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Area East  = ',NOAREA(4))
        ENDIF
C
        CALL INTLOG(JP_DEBUG,'IAGCNTL: Control flag status:',JPQUIET)
C
        IF( LSMCHNG ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: LSM change TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IAGCNTL: LSM change FALSE',JPQUIET)
        ENDIF
C
        IF( LSM ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: LSM flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IAGCNTL: LSM flag FALSE',JPQUIET)
        ENDIF
C
        IF( LWIND ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Wind flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Wind flag FALSE',JPQUIET)
        ENDIF
C
        IF( LPREC ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Precipitn flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Precipitn flag FALSE',JPQUIET)
        ENDIF
C
        IF( LSMPAR ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: LSM param flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IAGCNTL: LSM param flag FALSE',JPQUIET)
        ENDIF
        IF( LSTAGGL ) THEN
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Staggered flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IAGCNTL: Staggered flag FALSE',JPQUIET)
        ENDIF
      ENDIF
C
C     The change of definition option spans sections 2 to 3
C
      IF( LCHANGE ) THEN
C
C     -----------------------------------------------------------------|
C*    Section 2. Calculate space requirement.
C     -----------------------------------------------------------------|
C
  200   CONTINUE
C
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IAGCNTL: Section 2.',JPQUIET)
C
C       Get the required memory
C
        IAGCNTL = IGGMEM(KPR, KERR)
        IF( IAGCNTL.GT.0 ) GOTO 900
C
C       Input grid definition
C
        IF( NIREPR.EQ.JPGAUSSIAN ) THEN
C
          IAGCNTL = IGGRID(NIGAUSS, NIAREA, KILN, RIGAUSS,
     X                     MILONG,NIWE,MILATG,NINS,OIWEGLOBE,KPR,KERR)
          IF( IAGCNTL.GT.0 ) GOTO 900
C
        ELSEIF( LREGIN ) THEN
C
          IAGCNTL = IGLGRID(NIGRID, NIAREA, MILONG, NIWE, MILATG,
     X                      NINS,OIWEGLOBE,KPR,KERR)
          IF( IAGCNTL.GT.0 ) GOTO 900
C
        ELSE
C
          IAGCNTL = JPROUTINE + 1
          IF( LERROR ) THEN
            CALL INTLOG(JP_ERROR,
     X        'IAGCNTL: Illegal input representation = ',NIREPR)
            IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X        'IAGCNTL: Interpolation failing.',JPQUIET)
          ENDIF
          GOTO 900
C
        ENDIF
C
C       Is the input field global
C
        GINSPOLE = OINPOLE.AND.OISPOLE
        GIGLOBE  = OIWEGLOBE.AND.GINSPOLE
C
C       Output grid definition
C
C       Is the output field global North South
C
        GONSPOLE = OONPOLE.AND.OOSPOLE
        GOGLOBE  = OOWEGLOBE.AND.GONSPOLE
        LGLOBL   = (GOGLOBE.AND.NOAREA(2).EQ.0)
C
        IF( NOREPR.EQ.JPGAUSSIAN ) THEN
C
          IAGCNTL = IGGRID(NOGAUSS, NOAREA, KOLN, ROGAUSS,
     X                     MOLONG,NOWE,MOLATG,NONS,OOWEGLOBE,KPR,KERR)
          IF( IAGCNTL.GT.0 ) GOTO 900
C
        ELSEIF( LREGOUT ) THEN
C
          IAGCNTL = IGLGRID(NOGRID, NOAREA, MOLONG, NOWE, MOLATG,
     X                      NONS,OOWEGLOBE,KPR,KERR)
          IF( IAGCNTL.GT.0 ) GOTO 900
C
        ELSE
C
          IAGCNTL = JPROUTINE + 2
          IF( LERROR ) THEN
            CALL INTLOG(JP_ERROR,
     X        'IAGCNTL: Illegal output representation = ',NOREPR)
            IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X        'IAGCNTL: Interpolation failing.',JPQUIET)
          ENDIF
          GOTO 900
C
        ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3. Create output to input grid mapping and distance arrays
C     -----------------------------------------------------------------|
C
  300   CONTINUE
C
        IF( LDEBUG )
     X    CALL INTLOG(JP_DEBUG,'IAGCNTL: Section 3.',JPQUIET)
C
C       First check that the input and output fields are compatible.
C
        IF( .NOT.GIGLOBE ) THEN
C
          IF( .NOT.OIWEGLOBE ) THEN
C
C           Cannot produce global output field if input not global
C
            IF( OOWEGLOBE ) THEN
              IAGCNTL = JPROUTINE + 3
              IF( LERROR ) THEN
                CALL INTLOG(JP_ERROR,
     X           'IAGCNTL: Cannot produce global output from',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IAGCNTL: non-global input.  The W-E input',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IAGCNTL: direction was not global.',JPQUIET)
                IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X            'IAGCNTL: Interpolation failing.',JPQUIET)
              ENDIF
              GOTO 900
            ENDIF
C
          ENDIF
C
c Sinisa added NOT.LSTAGGL just for the sake of staggered grids
C
          IF( .NOT.GINSPOLE.AND.(.NOT.LSTAGGL) ) THEN
C
C           Cannot produce global output field if input not global
C
            IF( GONSPOLE ) THEN
              IAGCNTL = JPROUTINE + 4
              IF( LERROR ) THEN
                CALL INTLOG(JP_ERROR,
     X           'IAGCNTL: Cannot produce global output from',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IAGCNTL: non-global input.  The N-S input',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IAGCNTL: direction was not global.',JPQUIET)
                IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X            'IAGCNTL: Interpolation failing.',JPQUIET)
              ENDIF
              GOTO 900
            ENDIF
C
          ENDIF
C
        ENDIF
C
        IAGCNTL = IGDINS(MILATG, NINS, MOLATG, NONS, OINPOLE, OISPOLE,
     X                   MNSIND, MNSDIST, KPR, KERR)
        IF( IAGCNTL.GT.0 ) GOTO 900
C
        IAGCNTL = IGDIWE(NIWE, NOWE, MWEIND, MWEDIST, NIAREA, NOAREA,
     X    MILONG, MOLONG, OIWEGLOBE )  ! FIXME: remove
        IF( IAGCNTL.GT.0 ) GOTO 900
C
C       The end of primary initialisation
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4. Create normalised weight arrays
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IAGCNTL: Section 4.',JPQUIET)
C
C     The following code is affected by the Land Sea mask flag
C
      IF( LCHANGE.OR.LSMCHNG ) THEN
C
        IAGCNTL = IGINT(MWEDIST, NOWE, MNSDIST, NONS, WFACT, KPR, KERR)
        IF( IAGCNTL.GT.0 ) GOTO 900
C
        IF( LSM ) THEN
C
          IAGCNTL = IGLSMD(NIDATE,NIREPR,NIGRID,NIGAUSS,NIAREA,KILN,
     X                    YIFILE,GISTAND,IILGRID,IILLINE,IIREC,KPR,KERR)
          IF( IAGCNTL.GT.0 ) GOTO 900
C
          IAGCNTL = IGLSMD(NODATE,NOREPR,NOGRID,NOGAUSS,NOAREA,KOLN,
     X                    YOFILE,GOSTAND,IOLGRID,IOLLINE,IOREC,KPR,KERR)
          IF( IAGCNTL.GT.0 ) GOTO 900
C
          IAGCNTL = IGLSMB(GISTAND, YIFILE, IIREC, IILGRID, IILLINE,
     X                    GOSTAND,YOFILE,IOREC,IOLGRID,IOLLINE,KPR,KERR)
          IF( IAGCNTL.GT.0 ) GOTO 900
C
        ENDIF
C
C     Normalise the weights
C
        IAGCNTL = IGNORM(WFACT, RMAX, NOWE, NONS, KPR, KERR)
        IF( IAGCNTL.GT.0 ) GOTO 900
C
C     This marks the end of a change to the specification
C     Clear the Change flags LCHANGE and LSMCHNG
C
        LCHANGE = .FALSE.
        LSMCHNG = .FALSE.
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5. Basic interpolation from input to output field
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IAGCNTL: Section 5.',JPQUIET)
C
      IAGCNTL = IGTOG(PIFELD, NIWE, NINS, NOWE, NONS, MWEIND, MNSIND,
     X                WFACT, POFELD, KPR, KERR)
      IF( IAGCNTL.GT.0 ) GOTO 900
C
C     -----------------------------------------------------------------|
C*    Section 6. Additional interpolations from input to output field
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IAGCNTL: Section 6.',JPQUIET)
C
C     Precipitation field
C
C     Force processing without neighbour check
        GPREC = .FALSE.
        CALL GETENV('PRECIPITATION_NEIGHBOUR_CHECK', YFLAG)
        IF( YFLAG(1:1).EQ.'0' ) GPREC = .TRUE.

      IF( LPREC ) THEN
C
        IF( GPREC ) THEN
          IAGCNTL = ZPREC(POFELD, NOWE, NONS, KPR, KERR)
        ELSE
          IAGCNTL = IGPREC(PIFELD, NIWE, NINS, POFELD, NOWE, NONS,
     X                   MWEIND, MNSIND, WFACT, RMAX, RINPNT, KPR, KERR)
        ENDIF
C
        IF( IAGCNTL.GT.0 ) GOTO 900
      ENDIF
C
C     Special code for poles if Gaussian input and regular output
C
C     North pole
C
C    Sinisa add pole point for staggerd lat-lon grids
      IF( (NIREPR.EQ.JPREGULAR.AND.LSTAGGL).AND.
     X    LREGOUT               .AND.
     X    OINPOLE               .AND.
     X    OONPOLE ) THEN
C
        IIOFF = 1
        IOOFF = 1
C
        IF( LWIND ) THEN
          IAGCNTL = IGPOLEW(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                      NOWE, MWEIND, MWEDIST, KPR, KERR)
        ELSE
           IAGCNTL = IGPOLEG(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                       NOWE, KPR, KERR)
        ENDIF
      ENDIF

      IF( (NIREPR.EQ.JPGAUSSIAN).AND.
     X    LREGOUT               .AND.
     X    OINPOLE               .AND.
     X    OONPOLE ) THEN
C
        IIOFF = 1
        IOOFF = 1
C
        IF( LWIND ) THEN
          IAGCNTL = IGPOLEW(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                      NOWE, MWEIND, MWEDIST, KPR, KERR)
        ELSE
           IAGCNTL = IGPOLEG(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                       NOWE, KPR, KERR)
        ENDIF
        IF( IAGCNTL.GT.0 ) GOTO 900
C
      ENDIF
C
C     South pole
C
C    Sinisa add south pole point for staggerd lat-lon grids
      IF( (NIREPR.EQ.JPREGULAR.AND.LSTAGGL).AND.
     X    LREGOUT               .AND.
     X    OISPOLE               .AND.
     X    OOSPOLE ) THEN
C
        IIOFF = (NINS - 1) * NIWE + 1
        IOOFF = (NONS - 1) * NOWE + 1
C
        IF( LWIND ) THEN
          IAGCNTL = IGPOLEW(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                      NOWE, MWEIND, MWEDIST, KPR, KERR)
        ELSE
          IAGCNTL = IGPOLEG(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                      NOWE, KPR, KERR)
C
        ENDIF
      ENDIF
      IF( (NIREPR.EQ.JPGAUSSIAN).AND.
     X    LREGOUT               .AND.
     X    OISPOLE               .AND.
     X    OOSPOLE ) THEN
C
        IIOFF = (NINS - 1) * NIWE + 1
        IOOFF = (NONS - 1) * NOWE + 1
C
        IF( LWIND ) THEN
          IAGCNTL = IGPOLEW(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                      NOWE, MWEIND, MWEDIST, KPR, KERR)
        ELSE
          IAGCNTL = IGPOLEG(PIFELD(IIOFF), NIWE, POFELD(IOOFF),
     X                      NOWE, KPR, KERR)
        ENDIF
        IF( IAGCNTL.GT.0 ) GOTO 900
C
      ENDIF
C
C     LSM field - must be 0 or 1
C
C     Force processing real LSM
        LSM_VALUES = .FALSE.
        CALL GETENV('LSM_VALUES', YFLAG)
        IF( YFLAG(1:1).EQ.'1' ) LSM_VALUES = .TRUE.

      IF( .NOT.LSM_VALUES.AND.LSMPAR ) THEN
        IAGCNTL = IGPLSM(POFELD, NOWE * NONS, KPR, KERR)
        IF( IAGCNTL.GT.0 ) GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IAGCNTL: Section 9.',JPQUIET)
C
      RETURN
      END
