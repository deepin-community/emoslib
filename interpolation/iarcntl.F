C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IARCNTL(PIFELD, KILEN, POFELD, KOLEN, KILN,
     1   OIWEGLOBE, OINPOLE, OISPOLE, KOLN, OOWEGLOBE, OONPOLE,
     2   OOSPOLE, KPR, KERR)
C
C---->
C**** *IARCNTL*
C
C     Purpose
C     -------
C
C     The global controlling routine for interpolating between a quasi
C     regular Gaussian input field and a regular Gaussian or
C     latitude/longitude output field.
C
C
C     Interface
C     ---------
C
C     IERR = IARCNTL(PIFELD, KILEN, POFELD, KOLEN, KILN, OIWEGLOBE,
C    1   OINPOLE, OISPOLE, KOLN, OOWEGLOBE, OONPOLE, OOSPOLE, KPR,
C    2   KERR)
C
C
C     Input parameters
C     ----------------
C
C     PIFELD    - The input field provided by the calling routine.
C
C     KILEN     - The length of the input field.
C
C     KOLEN     - The length of the output field.
C
C     KILN      - The Northern line number of the input field within
C                  its Gaussian field.
C
C     OIWEGLOBE - A flag indicating whether the input field is
C                 global West to East.
C
C     OINPOLE   - A flag indicating whether the North pole is
C                 included in the input field.
C
C     OISPOLE   - A flag indicating whether the South pole is
C                 include in the input field.
C
C     KOLN      - The Northern line number of the output field
C                 within its Gaussian field.
C
C     OOWEGLOBE - A flag indicating whether the output field is
C                 global West to East.
C
C     OONPOLE   - A flag indicating whether the North pole is
C                 include in the output field.
C
C     OOSPOLE   - A flag indicating whether the South pole is
C                 include in the output field.
C
C     KPR       - The debug print switch.
C                  0  , No debugging output.
C                  1  , Produce debugging output.
C
C     KERR      - The error control flag.
C                  -ve, No error message. Return error code.
C                  0  , Hard failure with error message.
C                  +ve, Print error message. Return error code.
C
C
C     Output parameters
C     -----------------
C
C     POFELD - The output field returned to the calling routine.
C
C
C     Return value
C     ------------
C
C     The error indicator (INTEGER).
C
C
C     Error and Warning Return Values
C     -------------------------------
C
C     21901 The input data representation was not valid when the
C           grid was generated. This should not happen as the data
C           representation has been previously validated.
C     21902 The output data representation was not valid when the
C           grid was generated. This should not happen as the data
C           representation has been previously validated.
C     21903 Attempt to produce global West-East field where the input
C           field was not global.
C     21904 Attempt to produce global North-South field where the
C           input field was not global.
C
C
C     Common block usage
C     ------------------
C
C     grspace.h    - The include file contains all the array space
C                    for grid to grid interpolation.
C
C     MILLEN       - The array of quasi regular Gaussian field
C                    latitude line length definition.
C     RIGAUSS      - Real array of input field Gaussian latitudes is
C                    used.
C     ROGAUSS      - Real array of output field Gaussian latitudes is
C                    used.
C
C     MILATG       - The input field latitudes.
C     MILONG       - The input field longitudes.
C     MISTRT       - The array offsets for the start of each latitude
C                    line for a quasi regular Gaussian input field.
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
C
C     Externals
C     ---------
C
C     ABORTX     - Standard routine to kill task.
C     IGDINS     - Calculate the distances between points in an
C                  output latitude array and its North and South
C                  neighbours in the input latitude array.
C     IGGRID     - Generate the arrays of latitude and longitude
C                  points for a Gaussian truncation and area.
C     IGLGRID    - Generate the arrays of latitude and longitude
C                  points for a regular latitude longitude grid.
C     IGLSIZE    - Evaluate the array sizes for a regular
C                  latitude/longitude field.
C     IGLSMD     - Generate the land sea mask file information.
C     IGNORM     - Normalise the array of interpolation weights.
C     IGPLSM     - Force an interpolated land sea mask field back to
C                  a real 0-1 field.
C     IGPOLEG    - Calculate the values at the pole of a regular
C                  latitude/longitude field when the input is a
C                  Gaussian field.
C     IGPOLEW    - Calculates the values at the pole of a regular
C                  latitude/longitude wind field when the input is a
C                  Gaussian field.
C     IGSIZE     - Evaluate the array sizes for a Gaussian field.
C     IRDIWE     - Calculate the distances between points in an
C                  output longitude array and its West and East
C                  neighbours in the input longitude array for a
C                  quasi regular input field and regular output field.
C     IRGMEM     - This routine acquires heap space for quasi regular
C                  to regular interpolation.
C     IRGRID     - Generate the arrays of latitude points and the
C                  starting points of the lines of latitude for a
C                  quasi regular Gaussian grid.
C     IRGTOG     - Perform basic interpolation between the input and
C                  output fields for a quasi regular input field and
C                  a regular output field.
C     IRINT      - Calculate the basic unnormalised interpolation
C                  weights when interpolating from a quasi regular
C                  grid to a regular grid.
C     IRLSMB     - Calculate the effects of the land-sea masks on the
C                  unnormalised interpolation weights for a quasi
C                  regular input field and regular output field.
C     IRPREC     - Perform additional interpolation processes for
C                  precipitation fields when the input field is Quasi
C                  regular.
C     ZPREC      - Perform additional interpolation processes
C                  for precipitation
C     IRSIZE     - Evaluate the array sizes for a quasi regular
C                  Gaussian field.
C     INTLOG     - Logs messages.
C
C
C     Method
C     ------
C     This is purely a controlling routine with all the work being
C     performed in the external routines.
C
C
C     Reference
C     ---------
C     None
C
C
C     Comments
C     --------
C     None
C
C
C     Author
C     ------
C     K. Fielding      *ECMWF*      Nov 1993
C
C
C     Modifications
C     -------------
C     J.Chambers        ECMWF       Oct 1998
C     Allow field type for rotated grids.
C
C     S.Curic           ECMWF       Oct 2005
C     Add zprec routine that Perform checking
C     of precipitation
C
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
      PARAMETER (JPROUTINE = 21900)
C
C     Function arguments
C
      LOGICAL OIWEGLOBE, OINPOLE, OISPOLE, OOWEGLOBE, OONPOLE, OOSPOLE
      INTEGER KILEN, KOLEN, KILN, KOLN, KPR, KERR
      REAL PIFELD(KILEN), POFELD(KOLEN)
C
C     Local variables
C
      LOGICAL LFAIL, LERROR, LDEBUG, LREGOUT
      LOGICAL GINSPOLE, GIGLOBE, GISTAND, GONSPOLE, GOGLOBE, GOSTAND
      LOGICAL GPREC,LSM_VALUES
      CHARACTER*256 YIFILE, YOFILE
      CHARACTER*12 YFLAG
      INTEGER IIOFF, IINDEX, IOOFF
      INTEGER IIREC, IOREC, IERR
C
C     Working copies of area and grid arrays that may be changed
C
      INTEGER IILGRID(2), IILLINE(2), IOLGRID(2), IOLLINE(2)
C
C     External functions
C
      INTEGER IGDINS, IGGRID, IGLGRID, IGLSMD, IGNORM, IGPLSM,
     X   IGPOLEG, IGPOLEW,
     X   IRDIWE, IRGMEM, IRGRID, IRGTOG, IRINT, IRLSMB, IRPREC,
     X   ZPREC
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialisation
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IARCNTL = 0
      IERR = 0
C
C     Set all flag variables
C
      LDEBUG  = (KPR.GE.1)
      LFAIL   = (KERR.EQ.0)
      LERROR  = (KERR.GE.0)
      LREGOUT = (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT)
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'IARCNTL: Section 1.',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'IARCNTL: Input field definition parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IARCNTL: Representation = ',NIREPR)
        IF( NINS.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IARCNTL: Number N-S = ',NINS)
        CALL INTLOG(JP_DEBUG,'IARCNTL: Gaussian truncation= ',NIGAUSS)
        IF( NIAREA(1).NE.0 ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area North = ',NIAREA(1))
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area West  = ',NIAREA(2))
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area South = ',NIAREA(3))
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area East  = ',NIAREA(4))
        ENDIF
C
        CALL INTLOG(JP_DEBUG,
     X    'IARCNTL: Output field definition parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IARCNTL: Representation = ',NOREPR)
        IF( NOWE.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IARCNTL: Number W-E = ',NOWE)
        IF( NONS.NE.0 ) CALL INTLOG(JP_DEBUG,
     X    'IARCNTL: Number N-S = ',NONS)
        IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPQUASI) )
     X    CALL INTLOG(JP_DEBUG,'IARCNTL: Gaussian truncation= ',NOGAUSS)
        IF( LREGOUT.AND.NOGRID(1).NE.0 ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: Grid WE = ',NOGRID(1))
          CALL INTLOG(JP_DEBUG,'IARCNTL: Grid NS = ',NOGRID(2))
        ENDIF
        IF( NOAREA(1).NE.0 ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area North = ',NOAREA(1))
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area West  = ',NOAREA(2))
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area South = ',NOAREA(3))
          CALL INTLOG(JP_DEBUG,'IARCNTL: Area East  = ',NOAREA(4))
        ENDIF
C
        CALL INTLOG(JP_DEBUG,'IARCNTL: Control flag status:',JPQUIET)
C
        IF( LSMCHNG ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: LSM change TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IARCNTL: LSM change FALSE',JPQUIET)
        ENDIF
C
        IF( LSM ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: LSM flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IARCNTL: LSM flag FALSE',JPQUIET)
        ENDIF
C
        IF( LWIND ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: Wind flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IARCNTL: Wind flag FALSE',JPQUIET)
        ENDIF
C
        IF( LPREC ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: Precipitn flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IARCNTL: Precipitn flag FALSE',JPQUIET)
        ENDIF
C
        IF( LSMPAR ) THEN
          CALL INTLOG(JP_DEBUG,'IARCNTL: LSM param flag TRUE',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IARCNTL: LSM param flag FALSE',JPQUIET)
        ENDIF
      ENDIF
C
C     The change of parameter option spans sections 2 to 3
C
      IF( LCHANGE ) THEN
C
C     -----------------------------------------------------------------|
C*    Section 2. Get space and define grids.
C     -----------------------------------------------------------------|
C
  200   CONTINUE
C
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IARCNTL: Section 2.',JPQUIET)
C
C       Get the required memory
C
        IARCNTL = IRGMEM(KPR, KERR)
        IF( IARCNTL.GT.0 ) GOTO 900
C
C       Input field definition
C
        IF( NIREPR.EQ.JPQUASI ) THEN
C
          IARCNTL = IRGRID(NIAREA, MILLEN, KILN, RIGAUSS, MISTRT,
     X                     MILATG, NINS, OIWEGLOBE, KPR, KERR)
          IF( IARCNTL.GT.0 ) GOTO 900
C
C         Is the input field global
C
          GINSPOLE = OINPOLE.AND.OISPOLE
          GIGLOBE  = OIWEGLOBE.AND.GINSPOLE
C
        ELSE
C
          IARCNTL = JPROUTINE + 1
          IF( LERROR ) THEN
            CALL INTLOG(JP_ERROR,
     X        'IARCNTL: Illegal input data representation = ',NIREPR)
            IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X        'IARCNTL: Interpolation failing.',JPQUIET)
          ENDIF
          GOTO 900
C
        ENDIF
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
          IARCNTL = IGGRID(NOGAUSS, NOAREA, KOLN, ROGAUSS,
     X                     MOLONG,NOWE,MOLATG,NONS,OOWEGLOBE,KPR,KERR)
          IF( IARCNTL.GT.0 ) GOTO 900
C
        ELSEIF ( LREGOUT ) THEN
C
          IARCNTL = IGLGRID(NOGRID, NOAREA, MOLONG, NOWE, MOLATG,
     X                      NONS, OOWEGLOBE, KPR, KERR)
          IF( IARCNTL.GT.0 ) GOTO 900
C
        ELSE
C
          IARCNTL = JPROUTINE + 2
          IF( LERROR ) THEN
            CALL INTLOG(JP_ERROR,
     X        'IARCNTL: Illegal output data representation = ',NIREPR)
            IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X        'IARCNTL: Interpolation failing.',JPQUIET)
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
     X    CALL INTLOG(JP_DEBUG,'IARCNTL: Section 3.',JPQUIET)
C
C       First check that the input and output fields are compatible.
C
        IF( .NOT.GIGLOBE ) THEN
C
          IF( .NOT.OIWEGLOBE ) THEN
C
C           Cannot produce global output field if input field not global
C
            IF( OOWEGLOBE ) THEN
              IARCNTL = JPROUTINE + 3
              IF( LERROR ) THEN
                CALL INTLOG(JP_ERROR,
     X            'IARCNTL: Cannot produce global output from',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IARCNTL: non-global input.  The W-E input',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IARCNTL: direction was not global.',JPQUIET)
                IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X            'IARCNTL: Interpolation failing.',JPQUIET)
              ENDIF
              GOTO 900
            ENDIF
C
          ENDIF
C
          IF( .NOT.GINSPOLE ) THEN
C
C           Cannot produce global output field if input field not global
C
            IF( GONSPOLE ) THEN
              IARCNTL = JPROUTINE + 4
              IF( LERROR ) THEN
                CALL INTLOG(JP_ERROR,
     X            'IARCNTL: Cannot produce global output from',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IARCNTL: non-global input.  The N-S input',JPQUIET)
                CALL INTLOG(JP_ERROR,
     X            'IARCNTL: direction was not global.',JPQUIET)
                IF( LFAIL ) CALL INTLOG(JP_FATAL,
     X            'IARCNTL: Interpolation failing.',JPQUIET)
              ENDIF
              GOTO 900
            ENDIF
C
          ENDIF
C
        ENDIF
C
        IARCNTL = IGDINS(MILATG, NINS, MOLATG, NONS, OINPOLE,
     X                   OISPOLE, MNSIND, MNSDIST, KPR, KERR)
        IF( IARCNTL.GT.0 ) GOTO 900
C
        IARCNTL = IRDIWE(MILLEN, NINS, MNSIND, NONS, MILONG, NIWE,
     X                   MOLONG, NOWE, MWEIND, MWEDIST, NIAREA, NOAREA,
     X                   KPR, KERR)
        IF( IARCNTL.GT.0 ) GOTO 900
C
C     The end of primary initialisation
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4. Create normalised weight arrays
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IARCNTL: Section 4.',JPQUIET)
C
C     The following code is affected by the Land Sea mask flag
C
      IF( LCHANGE.OR.LSMCHNG ) THEN
C
        IARCNTL = IRINT(MWEDIST, NOWE, MNSDIST, NONS, WFACT, KPR, KERR)
        IF( IARCNTL.GT.0 ) GOTO 900
C
        IF( LSM ) THEN
C
          IARCNTL = IGLSMD(NIDATE,NIREPR,NIGRID,NIGAUSS,NIAREA,KILN,
     X                    YIFILE,GISTAND,IILGRID,IILLINE,IIREC,KPR,KERR)
          IF( IARCNTL.GT.0 ) GOTO 900
C
          IARCNTL = IGLSMD(NODATE,NOREPR,NOGRID,NOGAUSS,NOAREA,KOLN,
     X                    YOFILE,GOSTAND,IOLGRID,IOLLINE,IOREC,KPR,KERR)
          IF( IARCNTL.GT.0 ) GOTO 900
C
          IARCNTL = IRLSMB(GISTAND, YIFILE, IIREC, IILGRID, IILLINE,
     X                    GOSTAND,YOFILE,IOREC,IOLGRID,IOLLINE,KPR,KERR)
          IF( IARCNTL.GT.0 ) GOTO 900
C
        ENDIF
C
C       Normalise the weights
C
        IARCNTL = IGNORM(WFACT, RMAX, NOWE, NONS, KPR, KERR)
        IF( IARCNTL.GT.0 ) GOTO 900
C
C       This marks the end of a change to the specfication
C       Clear the Change flags LCHANGE and LSMCHNG
C
        LCHANGE = .FALSE.
        LSMCHNG = .FALSE.
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5. Interpolate from input to output field
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IARCNTL: Section 5.',JPQUIET)
C
C     Perform basic interpolation
C
      IARCNTL = IRGTOG(PIFELD, MISTRT, NINS, NOWE, NONS, MWEIND,
     X                 MNSIND, WFACT, POFELD, KPR, KERR)
      IF( IARCNTL.GT.0 ) GOTO 900
C
C     -----------------------------------------------------------------|
C*    Section 6. Additional interpolations from input to output field
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IARCNTL: Section 6.',JPQUIET)
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
          IARCNTL = ZPREC(POFELD, NOWE, NONS, KPR, KERR)
        ELSE
          IARCNTL = IRPREC(PIFELD, MISTRT, NINS, POFELD, NOWE, NONS,
     X                   MWEIND, MNSIND, WFACT, RMAX, RINPNT, KPR, KERR)
        ENDIF
        IF( IARCNTL.GT.0 ) GOTO 900
C
      ENDIF
C
C     Special code for poles if Gaussian input and regular output
C
C     North pole
C
      IF( (NIREPR.EQ.JPQUASI).AND.
     X    LREGOUT            .AND.
     X    OINPOLE            .AND.
     X    OONPOLE ) THEN
C
        IIOFF = MISTRT(1)
        IOOFF = 1
        IINDEX = (JP_I_N - 1) * NOWE + 1
C
        IF( LWIND ) THEN
          IARCNTL = IGPOLEW(PIFELD(IIOFF), MILLEN(1),
     X                      POFELD(IOOFF), NOWE, MWEIND(1,IINDEX),
     X                      MWEDIST(1,IINDEX), KPR, KERR)
        ELSE
          IARCNTL = IGPOLEG(PIFELD(IIOFF), MILLEN(1),
     X                      POFELD(IOOFF), NOWE, KPR, KERR)
        ENDIF
        IF( IARCNTL.GT.0 ) GOTO 900
C
      ENDIF
C
C     South pole
C
      IF( (NIREPR.EQ.JPQUASI).AND.
     X    LREGOUT            .AND.
     X    OISPOLE            .AND.
     X    OOSPOLE ) THEN
C
        IIOFF = MISTRT(NINS)
        IOOFF = (NONS - 1) * NOWE + 1
        IINDEX = ( (NONS - 1) * 2 + JP_I_S - 1) * NOWE + 1
C
        IF( LWIND ) THEN
          IARCNTL = IGPOLEW(PIFELD(IIOFF), MILLEN(NINS),
     X                      POFELD(IOOFF), NOWE, MWEIND(1,IINDEX),
     X                      MWEDIST(1,IINDEX), KPR, KERR)
        ELSE
          IARCNTL = IGPOLEG(PIFELD(IIOFF), MILLEN(NINS),
     X                      POFELD(IOOFF), NOWE, KPR, KERR)
        ENDIF
C
        IF( IARCNTL.GT.0 ) GOTO 900
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
        IARCNTL = IGPLSM(POFELD, NOWE * NONS, KPR, KERR)
        IF( IARCNTL.GT.0 ) GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IARCNTL: Section 9.',JPQUIET)
C
      RETURN
      END
