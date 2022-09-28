C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGSETUP(KSEC1,KSEC2,KSEC3,KSEC4)
C
C---->
C**** IGSETUP
C
C     Purpose
C     -------
C
C     Setup GRIB sections 1, 2, 3 & 4 for the new interpolation product.
C     For GRIBex.
C
C     Interface
C     ---------
C
C     IRET IGSETUP(KSEC1,KSEC2,KSEC3,KSEC4)
C
C     Input
C     -----
C
C     KSEC1   - Section 1 values
C     KSEC2   - Section 2 values
C     KSEC3   - Section 3 values
C     KSEC4   - Section 4 values
C
C     Output
C     ------
C
C     KSEC1   - Section 1 values (modified)
C     KSEC2   - Section 2 values (modified)
C     KSEC3   - Section 3 values (modified)
C     KSEC4   - Section 4 values (modified)
C
C     Method
C     ------
C
C     Use common block values and existing values from the original GRIB
C     product used for interpolation.
C
C
C     Externals
C     ---------
C
C     INTLOG   - Logs messages.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Aug 1994
C
C----<
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Parameter(s)
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 19100 )
C
C     Function arguments
C
      INTEGER KSEC1(*), KSEC2(*), KSEC3(*), KSEC4(*)
C
C     Local variables
C
      LOGICAL LIROTAT, LOROTAT
      INTEGER ITEMP
      REAL RTEMP
      REAL RSEC2(JPGRIB_ISEC2)  ! (not an argument, not necessary outside)
C
C***********************************************************************
C     Section 1.   Startup.
C***********************************************************************
C
 100  CONTINUE
C
      IGSETUP = 0
C
C     Check a recognized data representation type is being processed
C
      IF ( (NOREPR .NE.JPREGULAR  ) .AND.
     X     (NOREPR .NE.JPREGROT   ) .AND.
     X     (NOREPR .NE.JPGAUSSIAN ) .AND.
     X     (NOREPR .NE.JPFGGROT   ) .AND.
     X     (NOREPR .NE.JPQUASI    ) .AND.
     X     (NOREPR .NE.JPQGGROT   ) .AND.
     X     (NOREPR .NE.JPSPHROT   ) .AND.
     X     (NOREPR .NE.JPSPHERE   ) ) GOTO 910
C
C     Set flags if input is rotated or if a rotation angle has been given
C
      LIROTAT = (NIREPR.EQ.JPSPHROT).OR.
     X          (NIREPR.EQ.JPREGROT).OR.
     X          (NIREPR.EQ.JPFGGROT).OR.
     X          (NIREPR.EQ.JPQGGROT)
C
      LOROTAT = ((NOROTA(1).NE.-9000000).AND.(NOROTA(1).NE.0)).OR.
     X          (NOROTA(2).NE.0)
     X          .OR. (NOREPR.EQ.JPSPHROT .OR.
     X                NOREPR.EQ.JPREGROT .OR.
     X                NOREPR.EQ.JPFGGROT .OR.
     X                NOREPR.EQ.JPQGGROT)
C
C***********************************************************************
C     Section 2.   Setup output product section 1.
C***********************************************************************
C
 200  CONTINUE
C
C     Setup output if not given by input GRIB message
C     (ie input is 'unpacked').
C
      IF ( NIFORM .NE. 1 ) THEN
C
C       Code table ( same as input )
C
        KSEC1(1) = NOTABLE
C
C       Originating centre = ECMWF
C
        KSEC1(2) = 98
C
C       Generating process ID = 255
C       Grid definition       = 255
C
        KSEC1(3) = 255
        KSEC1(4) = 255
C
C       Parameter
C
        KSEC1(6) = NOPARAM
C
C       Level
C
        KSEC1(7) = NOLEVT
        KSEC1(8) = NOLEVEL
        KSEC1(9) = 0
C
C       Dummy date and time ( 1 Jan 1990, 1200 )
C
        KSEC1(10) = 90
        KSEC1(11) = 1
        KSEC1(12) = 1
        KSEC1(13) = 12
        KSEC1(14) = 0
C
C       Dummy verifying time (initialised analysis)
C
        KSEC1(15) = 1
        KSEC1(16) = 0
        KSEC1(17) = 0
        KSEC1(18) = 1
        KSEC1(19) = 0
        KSEC1(20) = 0
C
C       Century ( = 20 )
C
        KSEC1(21) = 20
C
C       Lots of zeroes
C
        KSEC1(22:36) = 0
C
C     Allow for user specified output field values when input is packed
C
      ELSE
C
C       Code table
C
        KSEC1(1) = NOTABLE
C
C       Parameter
C
        IF( LNOPARA ) KSEC1(6) = NOPARAM
C
C       Level
C
        IF( LNOLEVT ) KSEC1(7) = NOLEVT
        IF( LNOLEVL ) THEN
          KSEC1(8) = NOLEVEL
          KSEC1(9) = 0
        ENDIF
      ENDIF
C
C     Section 2 included, section 3 omitted
C
      KSEC1(5) = 128
C
C     Rest of section 1 kept unchanged from input product
C
C***********************************************************************
C     Section 3.   Setup output product section 2.
C***********************************************************************
C
 300  CONTINUE
C
C     Data representation type
C
C     Adjust output representation type if field is rotated
C
      IF( LIROTAT.OR.LOROTAT ) THEN
        KSEC2(1) = NOREPR
        IF(NOREPR.EQ.JPREGULAR )  KSEC2(1) = JPREGROT
        IF(NOREPR.EQ.JPSPHERE )   KSEC2(1) = JPSPHROT
        IF(NOREPR.EQ.JPQUASI )    KSEC2(1) = JPFGGROT
        IF(NOREPR.EQ.JPQGGROT )   KSEC2(1) = JPFGGROT
        IF(NOREPR.EQ.JPGAUSSIAN ) KSEC2(1) = JPFGGROT
C
      ELSE
C
        IF( (NOREPR.EQ.JPQUASI).OR.(NOREPR.EQ.JPQGGROT) ) THEN
          KSEC2(1) = JPGAUSSIAN
        ELSE
          KSEC2(1) = NOREPR
        ENDIF
      ENDIF
C
C***********************************************************************
C     Section 3.1. Setup section 2 for lat/long, equidistant
C                  cylindrical or plate carre grids.
C***********************************************************************
C
 310  CONTINUE
C
      IF ( (NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPREGROT) ) THEN
C
C       For south to north grids, only global area allowed (eg KWBC SST)
C
cs        IF( KSEC2(4).LT.KSEC2(7) ) THEN
cs          IF((NOAREA(1).NE.(-NOAREA(3))).AND.(NOAREA(3).NE.JP90)) THEN
cs            CALL INTLOG(JP_ERROR,
cs     X        'IGSETUP: Subareas not allowed interpolating',JPQUIET)
cs            CALL INTLOG(JP_ERROR,
cs     X        'IGSETUP: grids running from south to north',JPQUIET)
cs            IGSETUP = JPROUTINE + 2
cs            GOTO 900
cs          ENDIF
cs        ENDIF
C
C       Number of points along a parallel
C
        KSEC2(2) = NOWE
C
C       Number of points along a meridian
C
        KSEC2(3) = NONS
C
C       Resolution flag, increments are given
C
        KSEC2(6) = 128
C
C       Latitude and longitude of first/last grid point
C       La1/Lo1 - NOAREA(1): north, NOAREA(2): west
C       La2/Lo2 - NOAREA(3): south, NOAREA(4): east
C       EMOS-214: store REAL values in parallel data structure (real precision), for values that requiring sub-millidegree precision
C
        IF( NOAREA(1).GE.0 ) THEN
          KSEC2(4) = (NOAREA(1)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(4) = (NOAREA(1)-(JPMICRO/2)) / JPMICRO
        ENDIF
        IF( NOAREA(2).GE.0 ) THEN
          KSEC2(5) = (NOAREA(2)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(5) = (NOAREA(2)-(JPMICRO/2)) / JPMICRO
        ENDIF
        IF( NOAREA(3).GE.0 ) THEN
          KSEC2(7) = (NOAREA(3)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(7) = (NOAREA(3)-(JPMICRO/2)) / JPMICRO
        ENDIF
        IF( NOAREA(4).GE.0 ) THEN
          KSEC2(8) = (NOAREA(4)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(8) = (NOAREA(4)-(JPMICRO/2)) / JPMICRO
        ENDIF
        RSEC2(4:5) = FLOAT(NOAREA(1:2))/JPMULT
        RSEC2(7:8) = FLOAT(NOAREA(3:4))/JPMULT
C
C       Flip latitudes if they run from south to north
C       Set scanning mode flags
C
        IF( KSEC2(4).LT.KSEC2(7) ) THEN
          IF( LNOGRID ) THEN
            ITEMP = KSEC2(7)
            KSEC2(7) = KSEC2(4)
            KSEC2(4) = ITEMP
            RTEMP = RSEC2(7)
            RSEC2(7) = RSEC2(4)
            RSEC2(4) = RTEMP
            KSEC2(11) = 0
          ELSE
            KSEC2(11) = 64
          ENDIF
        ELSE
          KSEC2(11) = NOSCNM
        ENDIF
C
C       i and j  direction increments
C       EMOS-214: store REAL values in parallel data structure (real precision), for values that requiring sub-millidegree precision
C
        KSEC2( 9) = (NOGRID(1) + (JPMICRO/2)) / JPMICRO
        KSEC2(10) = (NOGRID(2) + (JPMICRO/2)) / JPMICRO
        RSEC2( 9) = FLOAT(NOGRID(1))/JPMULT
        RSEC2(10) = FLOAT(NOGRID(2))/JPMULT
C
C       Following left same as for input product
C       - no.of vertical coordinate parameters,
C       - lat/long of southern pole of rotation
C       - lat/long of pole of stretching
C
C       If rotated, put in new lat/long of southern pole of rotation
C       EMOS-214: store REAL values in parallel data structure (real precision), for values that requiring sub-millidegree precision
C
        IF( LIROTAT.OR.LOROTAT ) THEN
          IF( NOROTA(1).GE.0 ) THEN
            KSEC2(13) = (NOROTA(1) + (JPMICRO/2)) / JPMICRO
          ELSE
            KSEC2(13) = (NOROTA(1) - (JPMICRO/2)) / JPMICRO
          ENDIF
          IF( NOROTA(2).GE.0 ) THEN
            KSEC2(14) = (NOROTA(2) + (JPMICRO/2)) / JPMICRO
          ELSE
            KSEC2(14) = (NOROTA(2) - (JPMICRO/2)) / JPMICRO
          ENDIF
          RSEC2(13:14) = FLOAT(NOROTA(1:2))/JPMULT
        ENDIF
C
C       Regular grid flag
C
        KSEC2(17) = 0
C
C       Following left same as for input product
C       - earth flag
C       - components flag
C       - reserved fields
C
      ENDIF
C
C***********************************************************************
C     Section 3.2. Setup section 2 for gaussian grids.
C***********************************************************************
C
 320  CONTINUE
C
      IF ( NOREPR.EQ.JPQUASI    .OR. NOREPR.EQ.JPQGGROT .OR.
     X     NOREPR.EQ.JPGAUSSIAN .OR. NOREPR.EQ.JPFGGROT ) THEN
C
C       Number of points along a parallel
C
C       For a regular gaussian grid only
        IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPFGGROT) ) THEN
          KSEC2(2) = NOWE
        ELSE
          KSEC2(2) = 0
        ENDIF
C
C       Number of points along a meridian
C
        KSEC2(3) = NONS
C
C       Resolution flag
C       For a regular gaussian grid only, increments are given
C
        IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPFGGROT) ) THEN
          KSEC2(6) = 128
        ELSE
          KSEC2(6) = 0
        ENDIF
C
C       Latitude and longitude of first/last grid point
C       La1/Lo1 - NOAREA(1): north, NOAREA(2): west
C       La2/Lo2 - NOAREA(3): south, NOAREA(4): east
C       EMOS-214: store REAL values in parallel data structure (real precision), for values that requiring sub-millidegree precision
C
        IF( NOAREA(1).GE.0 ) THEN
          KSEC2(4) = (NOAREA(1)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(4) = (NOAREA(1)-(JPMICRO/2)) / JPMICRO
        ENDIF
        IF( NOAREA(2).GE.0 ) THEN
          KSEC2(5) = (NOAREA(2)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(5) = (NOAREA(2)-(JPMICRO/2)) / JPMICRO
        ENDIF
        IF( NOAREA(3).GE.0 ) THEN
          KSEC2(7) = (NOAREA(3)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(7) = (NOAREA(3)-(JPMICRO/2)) / JPMICRO
        ENDIF
        IF( NOAREA(4).GE.0 ) THEN
          KSEC2(8) = (NOAREA(4)+(JPMICRO/2)) / JPMICRO
        ELSE
          KSEC2(8) = (NOAREA(4)-(JPMICRO/2)) / JPMICRO
        ENDIF
        RSEC2(4:5) = FLOAT(NOAREA(1:2))/JPMULT
        RSEC2(7:8) = FLOAT(NOAREA(3:4))/JPMULT
C
C       i direction increment
C
C       For a regular gaussian grid, only
C
        KSEC2(9) = 0
        RSEC2(9) = 0.
        IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPFGGROT) ) THEN
          KSEC2(9) = (((JP90)/NOGAUSS)+(JPMICRO/2)) / JPMICRO
          RSEC2(9) = (FLOAT(JP90)/FLOAT(NOGAUSS))/JPMULT
        ENDIF
C
C       Number of parallels between a pole and the Equator
C
        KSEC2(10) = NOGAUSS
C
C       Scanning mode flags
C
        KSEC2(11) = NOSCNM
C
C       Following left same as for input product
C       - no.of vertical coordinate parameters,
C       - lat/long of southern pole of rotation
C       - lat/long of pole of stretching
C
C       If rotated, put in new lat/long of southern pole of rotation
C       EMOS-214: store REAL values in parallel data structure (real precision), for values that requiring sub-millidegree precision
C
        IF( LIROTAT.OR.LOROTAT ) THEN
          IF( NOROTA(1).GE.0 ) THEN
            KSEC2(13) = (NOROTA(1) + (JPMICRO/2)) / JPMICRO
          ELSE
            KSEC2(13) = (NOROTA(1) - (JPMICRO/2)) / JPMICRO
          ENDIF
          IF( NOROTA(2).GE.0 ) THEN
            KSEC2(14) = (NOROTA(2) + (JPMICRO/2)) / JPMICRO
          ELSE
            KSEC2(14) = (NOROTA(2) - (JPMICRO/2)) / JPMICRO
          ENDIF
          RSEC2(13:14) = FLOAT(NOROTA(1:2))/JPMULT
        ENDIF
C
C       Regular grid flag
C
        IF( (NOREPR.EQ.JPGAUSSIAN).OR.(NOREPR.EQ.JPFGGROT) ) THEN
          KSEC2(17) = 0
        ELSE
          KSEC2(17) = 1
        ENDIF
C
C       Following left same as for input product
C       - earth flag
C       - components flag
C       - reserved fields
C
C       For reduced grid, move in number of points along each parallel
C
          IF( (NOREPR.EQ.JPQUASI) .OR.
     X        (NOREPR.EQ.JPQGGROT) ) THEN
          DO ITEMP = 1, KSEC2(3)
            KSEC2(22+ITEMP) = NOLPTS(ITEMP+NO1NS-1)
          ENDDO
        ENDIF
C
      ENDIF
C
C***********************************************************************
C     Section 3.3. Setup section 2 for spherical harmonic coefficients.
C***********************************************************************
C
 330  CONTINUE
C
      IF ( (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) THEN
C
C       I, K, M pentagonal resolution parameters
C
        KSEC2(2) = NORESO
        KSEC2(3) = NORESO
        KSEC2(4) = NORESO
C
C       Representation type, associated legendre functions of the
C                            first kind
        KSEC2(5) = 1
C
C       Representation mode, complex packing
C
        KSEC2(6) = 2
C
C       Following left same as for input product
C       - reserved words 7-11
C       - no.of vertical coordinate parameters,
C       - lat/long of southern pole of rotation
C       - lat/long of pole of stretching
C       - reserved words 17-22
C
C       If rotated, put in new lat/long of southern pole of rotation
C       EMOS-214: store REAL values in parallel data structure (real precision), for values that requiring sub-millidegree precision
C
        IF( LIROTAT.OR.LOROTAT ) THEN
          IF( NOROTA(1).GE.0 ) THEN
            KSEC2(13) = (NOROTA(1) + (JPMICRO/2)) / JPMICRO
          ELSE
            KSEC2(13) = (NOROTA(1) - (JPMICRO/2)) / JPMICRO
          ENDIF
          IF( NOROTA(2).GE.0 ) THEN
            KSEC2(14) = (NOROTA(2) + (JPMICRO/2)) / JPMICRO
          ELSE
            KSEC2(14) = (NOROTA(2) - (JPMICRO/2)) / JPMICRO
          ENDIF
          RSEC2(13:14) = FLOAT(NOROTA(1:2))/JPMULT
        ENDIF
      ENDIF
C
C***********************************************************************
C     Section 4.   Setup output product section 4.
C***********************************************************************
C
 400  CONTINUE
C
C     Number of values to be packed
C
      IF ( (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) THEN
        KSEC4(1) = (NORESO + 1) * (NORESO + 2)
      ELSE IF( (NOREPR.EQ.JPQUASI).OR.(NOREPR.EQ.JPQGGROT) ) THEN
        KSEC4(1) = NOPCNT
      ELSE
        KSEC4(1) = NOWE * NONS
      ENDIF
C
C     If all the values are missing, set the count of values negative
C
      IF(NOMISS.NE.0) THEN
        KSEC4(1) = -KSEC4(1)
      ENDIF
C
C     Number of bits used for each packed value
C
      KSEC4(2) = NOACC
C
C     Type of data, spherical harmonic = 128, grid point = 0
C
      IF ( (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) THEN
        KSEC4(3) = 128
      ELSE
        KSEC4(3) = 0
      ENDIF
C
C     Type of packing, spherical harmonic = complex
C
      IF ( (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) THEN
        KSEC4(4) = 64
      ELSE
        KSEC4(4) = 0
      ENDIF
C
C     Type of data = floating point
C
      KSEC4(5) = 0
C
C     Additional flags indicator = none
C
      KSEC4(6) = 0
C
C     Reserved word, set to 0
C
      KSEC4(7) = 0
C
C     Following set to simplest case
C     - no.of values indicator
C     - secondary bitmaps indicator
C     - values width indicator
C     - no.of bits for 2nd order values
C
      KSEC4(8)  = 0
      KSEC4(9)  = 0
      KSEC4(10) = 0
      KSEC4(11) = 0
C
C     Reserved words, set to 0
C
      KSEC4(12) = 0
      KSEC4(13) = 0
      KSEC4(14) = 0
      KSEC4(15) = 0
C
C     If spherical harmonics, set following for complex packing
C
      IF ( (NOREPR.EQ.JPSPHERE).OR.(NOREPR.EQ.JPSPHROT) ) THEN
C
C       KSEC4(16), pointer to start of packed data, setup by packing
C       KSEC4(17), scaling factor P, unchanged from input
C       KSEC4(18), pentagonal resolution parameter J for packing of subset,
C                  unchanged from input
C       KSEC4(19), pentagonal resolution parameter K for packing of subset,
C                  unchanged from input
C       KSEC4(20), pentagonal resolution parameter M for packing of subset,
C                  unchanged from input
C
      ELSE
        KSEC4(16:20) = 0
      ENDIF
C
C     Words 21-33 reserved, set to zero.
C
      KSEC4(21:33) = 0
C
C***********************************************************************
C     Section 9.   Closedown
C***********************************************************************
C
 900  CONTINUE
C
      RETURN
C
C     Invalid data represntation type
C
 910  CONTINUE
      CALL INTLOG(JP_ERROR,
     X  'IGSETUP: Data representation type not recognized: ',NOREPR)
      CALL INTLOG(JP_ERROR,
     X  'IGSETUP: Only the following types are recognized:',JPQUIET)
      CALL INTLOG(JP_ERROR,'IGSETUP: Lat/long grid = ', JPREGULAR)
      CALL INTLOG(JP_ERROR,'IGSETUP: Lat/long (rotated) = ', JPREGROT)
      CALL INTLOG(JP_ERROR,'IGSETUP: regular gaussian = ', JPGAUSSIAN)
      CALL INTLOG(JP_ERROR,'IGSETUP: reg gauss rotated = ', JPFGGROT)
      CALL INTLOG(JP_ERROR,'IGSETUP: reduced gaussian = ', JPQUASI)
      CALL INTLOG(JP_ERROR,'IGSETUP: red gauss rotated = ', JPQUASI)
      CALL INTLOG(JP_ERROR,'IGSETUP: spherical harmonic = ', JPSPHERE)
      CALL INTLOG(JP_ERROR,'IGSETUP: spectral (rotated) = ', JPSPHROT)
C
      IGSETUP = JPROUTINE + 1
      RETURN
      END
