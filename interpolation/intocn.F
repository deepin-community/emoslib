C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTOCN(HICODE,HSCODE,RTOP,RBOTT,RRIGHT,RLEFT,
     *            RYRES,RXRES,
     *            KSEC1,KSEC2,PSEC4,KLENP,KLENI,TARRAY,
     *            XARR,YARR,NXP,NYP,PARRAY,IINTPOL,ISTAGP)
C
C---->
C****  INTOCN  - interpolates grib ocean data
C
C
C     PURPOSE
C     -------
C     Interpolates data to new grid, according to instructions.
C     Missing data is flagged with -999.9; this information is
C     passed to the new field.
C
C
C     Interface.
C     ----------
C
C     IRET = INTOCN(....)
C
C     Input parameters.
C     -----------------
C
C     HICODE   Interpolation code
C               'N'  No interpolation
C               'R'  Interpolate only to make grid Regular
C               'X'  Interpolate in x only
C               'Y'  Interpolate in y only
C               'F'  Interpolate in x and y (Full)
C     HSCODE   Staggered grid code
C               'D'  Drop every second row ('even' points)
C               'X'  Augment to standard grid by x interpolation
C               'Y'  Augment to standard grid by y interpolation
C               'F'  Augment to standard grid by x-y interpolation
C     RTOP     Top of output field domain        (degrees/metres/days)
C     RBOTT    Bottom of output field domain              ( " )
C     RRIGHT   Right of output field domain               ( " )
C     RLEFT    Left of output field domain                ( " )
C     RYRES    Resolution for y-interpolation     ( " )
C     RXRES    Resolution for x-interpolation     ( " )
C     KSEC1    Product definition block (+ high res. grid data)
C     KSEC2    Grid description block
C     PSEC4    Input data array
C     KLENP    Size of input array
C     KLENI    Size of scratch array (and also output array PARRAY)
C     TARRAY   Scratch array for use during interpolation
C
C     Output parameters.
C     ------------------
C
C     Returns 0 if the interpolation is successful, otherwise non-zero.
C
C     XARR     x cordinates of data values, or XO,XSTEP,
C              0.0 for regular spacing
C     YARR     y cordinates of data values, or YO,YSTEP,
C              0.0 for regular spacing
C     NXP      x dimension of output field
C     NYP      y dimension of output field
C     PARRAY   output field array
C     IINTPOL  Flag for interpolation done
C              = 0 if none,
C              = 1 if x only,
C              = 2 if y only,
C              = 3 if x and y.
C     ISTAGP   Flag to show processing done for staggered grid:
C              = 0 if none
C              = 2 if every second row dropped.
C              = 3 if interpolated in x only.
C              = 4 if interpolated in y only.
C              = 5 if interpolated in x and y.
C
C
C     Method.
C     -------
C     Convention is always to have output field array starting in bottom
C     left hand corner of the field. The positional information for the
C     data and for the axes is also given starting in the bottom left
C     hand corner.  This means YARR and PSEC4 must be inverted for the
C     default grib scanning, which starts in the top left hand corner.
C
C
C     Externals.
C     ----------
C
C     INTLOG  - Logs warning messages.
C     INTLOGR - Logs warning messages.
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Author.
C     -------
C
C     T. Stockdale    ECMWF  23.02.93
C
C
C     Modifications.
C     --------------
C
C     T. Stockdale    ECMWF  20.05.93
C     Revised for local grib.
C
C     J.D.Chambers    ECMWF  04.10.95
C     Make into a function (remove STOPs).
C
C     T. Stockdale    ECMWF  26.09.02
C     Drop odd points (not even) from E grid, for better land sea mask
C
C     T. Stockdale    ECMWF  07.10.02
C     New standard code (matches latest rd version)
C
C----<
C     ------------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Parameters
      INTEGER JPROUTINE, JP_LONG
      PARAMETER (JPROUTINE = 33300 )
      PARAMETER (JP_LONG = 3 )
C
#include "parim.h"
C
C     Subroutine arguments.
      CHARACTER*1 HICODE,HSCODE
      REAL RTOP, RBOTT, RRIGHT, RLEFT, RYRES, RXRES
      INTEGER KSEC1, KSEC2, KLENP, KLENI, NXP, NYP, IINTPOL, ISTAGP
      DIMENSION KSEC1(*)
      DIMENSION KSEC2(*)
      REAL PSEC4, TARRAY, XARR, YARR, PARRAY
      DIMENSION PSEC4(KLENP)
      DIMENSION XARR(*), YARR(*)
      DIMENSION TARRAY(KLENI), PARRAY(KLENI)
C
C     Local variables
      INTEGER NX, NY, KLAT1, KLAT2, KLON1, KXINC, KYINC, ISTAG
      INTEGER IIRREG, JGCL, J, I, JSKIP, NXLEFT, NXRIGH, IWRAP, JI
      INTEGER JIP, JJ, IOFFSET, NEWPTS, JN, IPOS, IPOSM, JPOS
      INTEGER IPOSP1M, JMIN, JMAX, JLEFT, JJP, JJEFF
      INTEGER NYLEFTI, NYLEFT, NYRIGH
      REAL XUNIT, YUNIT, RRVAL, RLVAL, RUVAL, RDVAL, RXIRES
      REAL XSIGN, XRSIGN, XFPREV, XFPOSN, RPOS, RVALL, RVALR
      REAL XVAL, XOWEST, XORES, XPSIGN, RVAL, RYIRES, YFPREV, YFPOSN
      REAL YSIGN, YPSIGN, YRSIGN, RMISS, EPS
      REAL RVALB, RVALT, XOSOUT
      LOGICAL LNEAREST
C
C     YSIGN    Flag for direction of y co-ords in data:
C              +1 if co-ords are increasing with index of PSEC4,
C              -1 if decreasing.
C     YPSIGN   Flag for direction of y co-ords in output field:
C              +1 if co-ords are increasing up the page,
C              -1 if decreasing.
C     YRSIGN   Flag for direction of y co-ords in output field relative
C              to that in data: YSIGN*YPSIGN
C     IINTPOL  Flag for interpolation:
C              0 = none,
C              1 = x only,
C              2 = y only,
C              3 = x and y
C     RMISS    Value for missing data
C     EPS      Tolerance on missing data flag
C
C
      INTEGER  INTUP, INTDN
      EXTERNAL INTUP, INTDN
C
C     Inline function - tests TRUE if A is a missing value
      REAL A
      LOGICAL MISSING
      MISSING(A) = ( ABS(A-RMISS) .LT. EPS )
C
C     ------------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IWRAP  = 0
      JMAX   = 0
      NXLEFT = 0
      NYLEFT = 0
      INTOCN = 0
C
C     Set key variables from GRIB header blocks.
C
      NX     = KSEC2(2)
      NY     = KSEC2(3)
C
      KLAT1  = KSEC1(62)
      KLAT2  = KSEC1(64)
      KLON1  = KSEC1(63)
      KXINC  = KSEC1(66)
      KYINC  = KSEC1(67)
C
      ISTAG  = KSEC1(69)
      IIRREG = KSEC1(68)
      JGCL   = 74+KSEC1(71)+KSEC1(72)
C
      IF(KSEC1(60).EQ.1) THEN
         XUNIT = 1.0/3600.0
      ELSE
         XUNIT = 1.0E-6
      ENDIF
C
      IF(KSEC1(61).EQ.2) THEN
         YUNIT = 1.0E-3
      ELSE IF(KSEC1(61).EQ.1) THEN
         YUNIT = 1.0/3600.0
      ELSE
         YUNIT = 1.0E-6
      ENDIF
C
      IINTPOL = 0
      RMISS   = -999.9
      EPS     = 0.01
C
C     ------------------------------------------------------------------
C*    Section 2 . Deal with staggered grid if present.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C     The approach in this section is to replace the staggered grid by
C     a non-staggered version, either by interpolating the data onto new
C     grid points or by simply dropping half of the data points. A new
C     version of PSEC4 is created, and parts of KSEC1 are modified.
C     This enables the rest of INTOCN to treat the field in a consistent
C     manner, BUT it means that KSEC1 and PSEC4 will be altered on
C     leaving this subroutine (and presently in an inconsistent way).
C
C     The interpolations coded in this section ignore the metric of the
C     staggered grid, that is they simply average neighbouring values to
C     find a new grid point value. The more general case can be added
C     later if desired.
C
C     Note: we prefer to drop odd points and keep even points, because
C     the standard version of HOPE2 uses an external mask to define the
C     land-sea mask of its even points. The mask for odd points then has
C     to be interpolated, which is less accurate, so the plots look
C     better if the even points are kept.
C
C
      IF(ISTAG.EQ.1) THEN
C
C       Drop odd row points ...
C
        IF(HSCODE.EQ.'D') THEN
          ISTAGP = 2
          NY     = NY/2
          KYINC  = KYINC*2
          DO 210 J = 1,NY
            DO 210 I = 1,NX
              PSEC4(I+NX*(J-1)) = PSEC4(I+NX*((J-1)*2+1))
  210     CONTINUE
C
C         Reset the grid coordinate list.
C         Remove the y coordinates of the even rows,
C         the x coordinates of the even rows, and also the first x
C         coordinate of an even regular row.
C
          IF(IIRREG.EQ.2) THEN
            KLON1=KSEC1(JGCL+1)
            DO 212 J = 1,NY
              KSEC1(JGCL+J) = KSEC1(JGCL+1+2*J)
  212       CONTINUE
C
          ELSE IF(IIRREG.EQ.3) THEN
            KLON1=KSEC1(JGCL+1)
            JSKIP = JGCL+NX
            DO 214 J = 1,NY
              KSEC1(JSKIP+J) = KSEC1(JSKIP+NX+2*J)
  214       CONTINUE
          ENDIF
C
C*      Staggered grid: interpolation in x.
C
        ELSE IF(HSCODE.EQ.'X') THEN
          ISTAGP = 3
          NX     = NX*2
          KXINC  = KXINC/2
          IF(((NX*NY).GT.KLENI).OR.((NX*NY).GT.KLENP)) THEN
            CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
            CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NY)
            CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENP)
            CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
            INTOCN = JPROUTINE + 17
            GOTO 900
          ENDIF
C
C         Expand the data into a temporary array, and then copy back
C         into PSEC4. Note the treatment of the end points, which
C         assumes a longitude wrap-round. Remember to mask land points
C         so that either both points are missing or neither.
C
          DO 220 J = 1,NY,2
            RRVAL = PSEC4(1+(J-1)*NX/2)
            RLVAL = PSEC4(NX/2+(J-1)*NX/2)
            IF(MISSING(RRVAL)) RLVAL = RMISS
            IF(MISSING(RLVAL)) RRVAL = RMISS
            TARRAY(1+(J-1)*NX) = (RRVAL+RLVAL)*0.5
            TARRAY(2+(J-1)*NX) = PSEC4(1+(J-1)*NX/2)
            DO 220 I = 2,NX/2
              RRVAL = PSEC4(I  +(J-1)*NX/2)
              RLVAL = PSEC4(I-1+(J-1)*NX/2)
              IF(MISSING(RRVAL)) RLVAL = RMISS
              IF(MISSING(RLVAL)) RRVAL = RMISS
              TARRAY(I*2-1+(J-1)*NX) = (RRVAL+RLVAL)*0.5
              TARRAY(I*2  +(J-1)*NX) = PSEC4(I  +(J-1)*NX/2)
  220     CONTINUE
C
          DO 222 J = 2,NY,2
            RRVAL = PSEC4(1+(J-1)*NX/2)
            RLVAL = PSEC4(NX/2+(J-1)*NX/2)
            IF(MISSING(RRVAL)) RLVAL = RMISS
            IF(MISSING(RLVAL)) RRVAL = RMISS
            TARRAY(NX+(J-1)*NX) = (RLVAL+RRVAL)*0.5
            TARRAY(NX-1+(J-1)*NX) = PSEC4(NX/2+(J-1)*NX/2)
            DO 222 I = 1,NX/2-1
              RRVAL = PSEC4(I+1+(J-1)*NX/2)
              RLVAL = PSEC4(I  +(J-1)*NX/2)
              IF(MISSING(RRVAL)) RLVAL = RMISS
              IF(MISSING(RLVAL)) RRVAL = RMISS
              TARRAY(I*2  +(J-1)*NX) = (RLVAL+RRVAL)*0.5
              TARRAY(I*2-1+(J-1)*NX) = PSEC4(I  +(J-1)*NX/2)
  222     CONTINUE
          DO 224 J = 1,NY
            DO 224 I = 1,NX
              PSEC4(I+(J-1)*NX) = TARRAY(I+(J-1)*NX)
  224     CONTINUE
C
C         Reset the grid coordinate list if necessary, again making
C         use of a temporary array. Expand the x-coordinate list,
C         and remove the x coordinate of the first even point if grid
C         is regular in x.
C
          KLON1 = KSEC1(JGCL+1)
          IF((IIRREG.EQ.1).OR.(IIRREG.EQ.3)) THEN
            JSKIP = JGCL
            DO 226 J = 1,NX/2
              TARRAY(J*2-1) = KSEC1(JSKIP+J)
              TARRAY(J*2)   = KSEC1(JSKIP+NX/2+J)
  226       CONTINUE
            DO 227 J = 1,NX
              KSEC1(JSKIP+J) = TARRAY(J)
  227       CONTINUE
          ELSE IF((IIRREG.EQ.2)) THEN
            DO 228 J = 1,NY
              KSEC1(JGCL+J) = KSEC1(JGCL+1+J)
  228       CONTINUE
          ENDIF
C
C       Staggered grid: interpolation in y.
C
        ELSE IF(HSCODE.EQ.'Y') THEN
          ISTAGP = 4
          NX     = NX*2
          KXINC  = KXINC/2
          IF(((NX*NY).GT.KLENI).OR.((NX*NY).GT.KLENP)) THEN
            CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
            CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NY)
            CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENP)
            CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
            INTOCN = JPROUTINE + 18
            GOTO 900
          ENDIF
C
C         Expand the data into a temporary array, and then copy back
C         into PSEC4. For the polarmost rows, use X interpolation.
C
C         First the even rows...
          DO 230 J = 2,NY-1,2
            DO 230 I = 1,NX/2
              RUVAL = PSEC4(I+(J-2)*NX/2)
              RDVAL = PSEC4(I+(J  )*NX/2)
              IF(MISSING(RUVAL)) RDVAL = RMISS
              IF(MISSING(RDVAL)) RUVAL = RMISS
              TARRAY(I*2  +(J-1)*NX) = (RDVAL+RUVAL)*0.5
              TARRAY(I*2-1+(J-1)*NX) =  PSEC4(I+(J-1)*NX/2)
  230     CONTINUE
          RRVAL = PSEC4(1+(NY-1)*NX/2)
          RLVAL = PSEC4(NX/2+(NY-1)*NX/2)
          IF(MISSING(RRVAL)) RLVAL = RMISS
          IF(MISSING(RLVAL)) RRVAL = RMISS
          TARRAY(NX+(NY-1)*NX) = (RLVAL+RRVAL)*0.5
          TARRAY(NX-1+(NY-1)*NX) = PSEC4(NX/2+(NY-1)*NX/2)
          DO 231 I = 2,NX/2
            RRVAL = PSEC4(I+1+(NY-1)*NX/2)
            RLVAL = PSEC4(I+(NY-1)*NX/2)
            IF(MISSING(RRVAL)) RLVAL = RMISS
            IF(MISSING(RLVAL)) RRVAL = RMISS
            TARRAY(I*2+(NY-1)*NX) = (RRVAL+RLVAL)*0.5
            TARRAY(I*2-1+(NY-1)*NX) = PSEC4(I+(NY-1)*NX/2)
  231     CONTINUE
C
C         Then the odd rows
          DO 232 J = 3,NY-1,2
            RUVAL = PSEC4(NX/2+(J-2)*NX/2)
            RDVAL = PSEC4(NX/2+(J  )*NX/2)
            IF(MISSING(RUVAL)) RDVAL = RMISS
            IF(MISSING(RDVAL)) RUVAL = RMISS
            TARRAY(1+(J-1)*NX) = (RRVAL+RLVAL)*0.5
            TARRAY(2+(J-1)*NX) = PSEC4(1+(J-1)*NX/2)
            DO 232 I = 1,NX/2
              RUVAL = PSEC4(I+(J-2)*NX/2)
              RDVAL = PSEC4(I+(J  )*NX/2)
              IF(MISSING(RUVAL)) RDVAL = RMISS
              IF(MISSING(RDVAL)) RUVAL = RMISS
              TARRAY(I*2-1+(J-1)*NX) = (RDVAL+RUVAL)*0.5
              TARRAY(I*2  +(J-1)*NX) =  PSEC4(I+(J-1)*NX/2)
  232     CONTINUE
C
          RRVAL = PSEC4(1+(1-1)*NX/2)
          RLVAL = PSEC4(NX/2+(1-1)*NX/2)
          IF(MISSING(RRVAL)) RLVAL = RMISS
          IF(MISSING(RLVAL)) RRVAL = RMISS
          TARRAY(1+(1-1)*NX) = (RRVAL+RLVAL)*0.5
          TARRAY(2+(1-1)*NX) = PSEC4(1+(1-1)*NX/2)
          DO 233 I = 2,NX/2
            RRVAL = PSEC4(I+(1-1)*NX/2)
            RLVAL = PSEC4(I-1+(1-1)*NX/2)
            IF(MISSING(RRVAL)) RLVAL = RMISS
            IF(MISSING(RLVAL)) RRVAL = RMISS
            TARRAY(I*2-1+(1-1)*NX) = (RRVAL+RLVAL)*0.5
            TARRAY(I*2+(1-1)*NX) = PSEC4(I+(1-1)*NX/2)
  233     CONTINUE

          DO 234 J = 1,NY
            DO 234 I = 1,NX
              PSEC4(I+(J-1)*NX) = TARRAY(I+(J-1)*NX)
  234     CONTINUE
C
C         Reset the grid coordinate list if necessary, again making
C         use of a temporary array. Expand the x-coordinate list,
C         and remove the x coordinate of the first even point if
C         grid is regular in x.
C
          IF((IIRREG.EQ.1).OR.(IIRREG.EQ.3)) THEN
            JSKIP = JGCL
            DO 235 J = 1,NX/2
              TARRAY(J*2-1) = KSEC1(JSKIP+J)
              TARRAY(J*2)   = KSEC1(JSKIP+NX/2+J)
  235       CONTINUE
            DO 236 J = 1,NX
              KSEC1(JSKIP+J) = TARRAY(J)
  236       CONTINUE
          ELSE IF((IIRREG.EQ.2)) THEN
            DO 237 J = 1,NY
              KSEC1(JGCL+J) = KSEC1(JGCL+1+J)
  237       CONTINUE
          ENDIF
C
C       Staggered grid: interpolation in x and y.
C
        ELSE IF(HSCODE.EQ.'F') THEN
          ISTAGP = 5
          CALL INTLOG(JP_ERROR,'INTOCN: Staggered grid opt. F',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: (interpolate x and y)',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: not yet implemented.',JPQUIET)
          INTOCN = JPROUTINE + 1
          GOTO 900
        ELSE
          CALL INTLOG(JP_ERROR,'INTOCN: Staggered grid option',JPQUIET)
          CALL INTLOG(JP_ERROR,HSCODE,JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: not known.',JPQUIET)
          INTOCN = JPROUTINE + 2
          GOTO 900
        ENDIF
C
        ISTAG = 0
C
      ELSE
         ISTAGP = 0
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 3 . Set X coordinates and interpolate in X.
C     ------------------------------------------------------------------
C
  300 CONTINUE
C
C     The procedure is first to do the x interpolation (if any), and
C     copy the result into a temporary array. The y interpolation is
C     done in the next section.
C
C     The one complication here is to ensure proper longitude wrap-round
C     where appropriate.
C
C     Some plot systems have difficulty controlling the data within the
C     viewport. Because of this, and in the interests of efficiency, the
C     data arrays are truncated even if no interpolation is being done.
C
C
C     No interpolation in X.
C
      IF((HICODE.EQ.'N').OR.
     *   ((HICODE.EQ.'R').AND.((IIRREG.EQ.0).OR.(IIRREG.EQ.2))).OR.
     *   (HICODE.EQ.'Y') )  THEN
C
C       If x is longitude, set parameters up for wrap-round
C       Wrap-round activated only if longitude range of data > 350 deg.
C
        IF((IIRREG.EQ.0).OR.(IIRREG.EQ.2)) THEN
          RXIRES = KXINC*XUNIT
          NXLEFT = INTDN((RLEFT-KLON1*XUNIT)/RXIRES+1)
          NXRIGH = INTUP((RRIGHT-KLON1*XUNIT)/RXIRES+1)
          NXP    = NXRIGH-NXLEFT+1
          IF(NXP.LE.1) THEN
            CALL INTLOG(JP_ERROR,'INTOCN: NXP negative = ',NXP)
            INTOCN = JPROUTINE + 3
            GOTO 900
          ENDIF
          XARR(1) = KLON1*XUNIT+(NXLEFT-1)*RXIRES
          XARR(2) = KXINC*XUNIT
          XARR(3) = 0.0
          IF( (KSEC1(60).EQ.3) .AND. (KXINC*NX*XUNIT.GT.350.0) ) THEN
            IWRAP = 1
          ELSE
            IWRAP = 0
          ENDIF
C
        ELSE IF( (IIRREG.EQ.1) .OR. (IIRREG.EQ.3) ) THEN
          NXP    = NX
          NXLEFT = 1
          JSKIP  = JGCL
          DO 312 JI = 1,NXP
            XARR(JI) = KSEC1(JSKIP+JI)*XUNIT
  312     CONTINUE
          IF( (KSEC1(60).EQ.3) .AND. (KXINC*NX*XUNIT.GT.350.0) ) THEN
            IWRAP  = 1
            INTOCN = JPROUTINE + 4
            GOTO 900
          ELSE
            IWRAP  = 0
          ENDIF
        ENDIF
C
        IF((NXP*NY).GT.KLENI) THEN
          CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NY)
          CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
          INTOCN = JPROUTINE + 5
          GOTO 900
        ENDIF
C
C       Copy the data into the temporary array
C
        DO 318 JI = 1,NXP
          JIP = JI+NXLEFT-1
          IF(IWRAP.EQ.1) THEN
            IF(JIP.LE.0)  JIP = JIP+NX
            IF(JIP.GT.NX) JIP = JIP-NX
          ENDIF
          IF((JIP.GE.1).AND.(JIP.LE.NX)) THEN
            DO 316 JJ = 1,NY
              TARRAY(JI+(JJ-1)*NXP) = PSEC4(JIP+(JJ-1)*NX)
  316       CONTINUE
          ELSE
            DO 317 JJ = 1,NY
              TARRAY(JI+(JJ-1)*NXP) = RMISS
  317       CONTINUE
          ENDIF
  318   CONTINUE
C
C       Interpolation from a grid irregular in X.
C     ---------------------------------------------
C
      ELSE IF(((HICODE.EQ.'R').OR.
     *        (HICODE.EQ.'X').OR.
     *        (HICODE.EQ.'F')    ).AND.
     *       ((IIRREG.EQ.1).OR.(IIRREG.EQ.3))  ) THEN

        XPSIGN = SIGN(1.0,(RRIGHT-RLEFT))
        NXP    = NINT((RRIGHT-RLEFT)/RXRES*XPSIGN)+1
        IF((NXP*NY).GT.KLENI) THEN
          CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NY)
          CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
          INTOCN = JPROUTINE + 6
          GOTO 900
        ENDIF
        RRIGHT  = RLEFT+RXRES*XPSIGN*(NXP-1)
        XARR(1) = RLEFT
        XARR(2) = RXRES*XPSIGN
        XARR(3) = 0.0
        IINTPOL = IINTPOL+1
        IOFFSET = JGCL
        XSIGN   = SIGN(1.0,((KSEC1(IOFFSET+2)-KSEC1(IOFFSET+1))*1.0))
        XRSIGN  = XSIGN*XPSIGN
        DO 321 JI = 1,NXP*NY
          TARRAY(JI) = RMISS
  321   CONTINUE
C
C       Work through the known (irregular) data points.
C       Calculate the position of the point as a fractional number of
C       points on the new regular grid
C       (XFPOSN). If there are any points on the new grid between this
C       point and the previous one (XFPREV), then set these points by
C       interpolation between the two known points.
        XFPREV= (KSEC1(IOFFSET+1)*XUNIT-RLEFT)/RXRES*XPSIGN - 0.5*XRSIGN
        DO 324 JI = 1,NX
          XFPOSN = (KSEC1(IOFFSET+JI)*XUNIT-RLEFT)/(RXRES*XPSIGN)
          NEWPTS = INTDN((XFPOSN*XRSIGN))-INTDN((XFPREV*XRSIGN))
          DO 323 JN = 1,NEWPTS
            IPOS = (INTDN(XFPREV*XRSIGN)+JN)*XRSIGN+1
            RPOS = (IPOS-1)*1.0
            IF((IPOS.GE.1).AND.(IPOS.LE.NXP)) THEN
              DO 322 JJ = 1,NY
                RVALL = PSEC4(JI-1+NX*(JJ-1))
                RVALR = PSEC4(JI+NX*(JJ-1))
                IF(.NOT.MISSING(RVALL) .AND. .NOT.MISSING(RVALR) ) THEN
                  XVAL = (XFPOSN-RPOS)/(XFPOSN-XFPREV)*RVALL
     *                      +(RPOS-XFPREV)/(XFPOSN-XFPREV)*RVALR
                ELSE
                  XVAL = RMISS
                ENDIF
                TARRAY(IPOS+NXP*(JJ-1)) = XVAL
  322         CONTINUE
            ENDIF
  323     CONTINUE
          XFPREV = XFPOSN
  324   CONTINUE

C
C     Interpolation from a grid regular in X.
C
      ELSE IF(((HICODE.EQ.'X').OR.(HICODE.EQ.'F')).AND.
     *       ((IIRREG.EQ.0).OR.(IIRREG.EQ.2))    ) THEN

        XPSIGN = SIGN(1.0,(RRIGHT-RLEFT))
        NXP    = NINT((RRIGHT-RLEFT)/RXRES*XPSIGN)+1
        IF((NXP*NY).GT.KLENI) THEN
          CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NY)
          CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
          INTOCN = JPROUTINE + 7
          GOTO 900
        ENDIF
        RRIGHT  = RLEFT+RXRES*XPSIGN*(NXP-1)
        XARR(1) = RLEFT
        XARR(2) = RXRES*XPSIGN
        XARR(3) = 0.0
        IINTPOL = IINTPOL+1
C
C       Work through the new regular grid points. Calculate the position
C       of the point as a fractional number of points on the old grid.
C       Use this fractional number to estimate the value of the field
C       at the new point
C
C       (Note that just to the right of the first point, RPOS will be
C        eg 1.1, not 0.1)
C
        DO 330 JI = 1,NXP*NY
          TARRAY(JI) = RMISS
  330   CONTINUE
C
        XOWEST = KLON1*XUNIT
        XORES  = KXINC*XUNIT
        DO 3320 JI = 1,NXP
          RPOS = (RLEFT+(JI-1)*RXRES-XOWEST)/XORES + 1
          IPOS = INTDN(RPOS)
C
C         Check for out of bounds, and set to zero unless x is
C         longitude, in which case attempt to wrap round.
          IF( (IPOS.GE.1) .AND. ((IPOS+1).LE.NX) ) THEN
            DO 3310 JJ = 1,NY
              RVALL = PSEC4(IPOS+(JJ-1)*NX)
              RVALR = PSEC4(IPOS+1+(JJ-1)*NX)
              IF(.NOT. MISSING(RVALL) .AND. .NOT. MISSING(RVALR) ) THEN
                RVAL = (IPOS+1-RPOS)*RVALL + (RPOS-IPOS)*RVALR
C
C             Do not set to missing if there is good data close by.
              ELSEIF( (.NOT.MISSING(RVALL)) .AND.
     X                ((IPOS+1-RPOS).GT.0.5) ) THEN
                RVAL = RVALL
              ELSEIF( (.NOT. MISSING(RVALR)).AND.
     X                ((RPOS-IPOS).GT.0.5)) THEN
                RVAL = RVALR
              ELSE
                RVAL = RMISS
              ENDIF
              TARRAY(JI+(JJ-1)*NXP) = RVAL
 3310       CONTINUE
C
          ELSEIF( KSEC1(60).EQ.JP_LONG ) THEN
C
C           This works only if the longitude points are equally spaced
C           around the globe, ie the gap between the first and last
C           points is equal to the gap between any other pair of
C           adjacent points.
C
            IPOSM   = MOD(IPOS,NX)
            IPOSP1M = MOD((IPOS+1),NX)
            IF(IPOSM.LE.0)   IPOSM   = IPOSM+NX
            IF(IPOSP1M.LE.0) IPOSP1M = IPOSP1M+NX
            DO 3313 JJ = 1,NY
              RVALL = PSEC4(IPOSM+(JJ-1)*NX)
              RVALR = PSEC4(IPOSP1M+(JJ-1)*NX)
              IF(.NOT. MISSING(RVALL) .AND. .NOT. MISSING(RVALR) ) THEN
                RVAL = (IPOS+1-RPOS)*RVALL + (RPOS-IPOS)*RVALR
C
C             Do not set to missing if there is good data close by.
              ELSEIF( (.NOT.MISSING(RVALL)) .AND.
     X               ((IPOS+1-RPOS).GT.0.5) ) THEN
                RVAL = RVALL
              ELSEIF( (.NOT. MISSING(RVALR)).AND.
     X                ((RPOS-IPOS).GT.0.5) ) THEN
                RVAL = RVALR
              ELSE
                RVAL = RMISS
              ENDIF
              IF(RVAL.GT.1E6) THEN
                RVAL = RMISS
              ENDIF
              TARRAY(JI+(JJ-1)*NXP) = RVAL
 3313       CONTINUE
          ELSE
            DO 3316 JJ = 1,NY
              TARRAY(JI+(JJ-1)*NXP) = RMISS
 3316       CONTINUE
          ENDIF
 3320    CONTINUE
C
      ELSE
        CALL INTLOG(JP_ERROR,'INTOCN: Error in logic.',JPQUIET)
        CALL INTLOG(JP_ERROR,'INTOCN: HICODE = ',JPQUIET)
        CALL INTLOG(JP_ERROR,HICODE,JPQUIET)
        CALL INTLOG(JP_ERROR,'INTOCN: IIRREG = ',IIRREG)
        INTOCN = JPROUTINE + 8
        GOTO 900
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 4 . Set Y coordinates and interpolate in Y.
C     ------------------------------------------------------------------
C
  400 CONTINUE
C
C     The x-interpolated field in the temporary array is interpolated in
C     y into the final output field array. Note that the field and the
C     coordinate array are both inverted in the y direction during this
C     section.
C
C     Some plot systems have difficulty controlling the data within the
C     viewport. Because of this, and in the interests of efficiency, the
C     data arrays are truncated even if no interpolation is being done.
C
C
C     No interpolation in Y.
C
      IF((HICODE.EQ.'N').OR.
     *   ((HICODE.EQ.'R').AND.((IIRREG.EQ.0).OR.(IIRREG.EQ.1))).OR.
     *   (HICODE.EQ.'X') )  THEN
C
C       If y is longitude, set parameters up for wrap-round
C       Wrap-round activated only if longitude range of data > 350 deg.
C
        IF((IIRREG.EQ.0).OR.(IIRREG.EQ.1)) THEN
C
C         Remember to invert calc of NYP if RYIRES -ve ... 22.04.94
          RYIRES =  KYINC*YUNIT
          IF(RYIRES.GT.0) THEN
            NYLEFT = INTDN((RBOTT-KLAT1*YUNIT)/RYIRES+1)
            NYRIGH = INTUP((RTOP-KLAT1*YUNIT)/RYIRES+1)
            NYP = NYRIGH-NYLEFT+1
          ELSE
            NYRIGH = INTUP((RBOTT-KLAT1*YUNIT)/RYIRES+1)
            NYLEFT = INTDN((RTOP-KLAT1*YUNIT)/RYIRES+1)
            NYP = NYRIGH-NYLEFT+1
          ENDIF
          IF(NYP.LE.1) THEN
            CALL INTLOG(JP_ERROR,'INTOCN: NYP negative = ',NYP)
            INTOCN = JPROUTINE + 9
            GOTO 900
          ENDIF
C
C         Remember to invert yarr coordinates ... 06.07.93
          YARR(1) = KLAT1*YUNIT+(NYRIGH-1)*RYIRES
          YARR(2) = -KYINC*YUNIT
          YARR(3) = 0.0
C
C         Remember NYLEFT is relative to the inverted array ... 19.01.96
C         ... but be careful to do this exactly.   01.04.97
          NYLEFTI=NYLEFT
          NYLEFT=NY+1-NYRIGH
          NYRIGH=NY+1-NYLEFTI
C
          IF((KSEC1(61).EQ.3) .AND. (KYINC*NY*YUNIT.GT.350.0) ) THEN
            IWRAP = 1
          ELSE
            IWRAP = 0
          ENDIF
C
        ELSE IF(IIRREG.EQ.2) THEN
          IOFFSET = JGCL
          YPSIGN  = SIGN(1.0,(RTOP-RBOTT))
          YSIGN   = SIGN(1.0,((KSEC1(IOFFSET+2)-KSEC1(IOFFSET+1))*1.0))
          YRSIGN  = YSIGN*YPSIGN
          JMIN    = 0
          DO 410 JI = 1,NY
            YARR(JI) = KSEC1(IOFFSET+(NY+1-JI))*YUNIT
            IF(YRSIGN.LT.0.0) THEN
              IF((YARR(JI)-RBOTT)*YSIGN .GT. 0.0) JMIN = JI
              IF((YARR(JI)-RTOP )*YSIGN .GT. 0.0) JMAX = JI+1
            ELSE
              IF((YARR(JI)-RTOP )*YSIGN .GT. 0.0) JMIN = JI
              IF((YARR(JI)-RBOTT)*YSIGN .GT. 0.0) JMAX = JI+1
            ENDIF
  410     CONTINUE
          IF(JMIN.EQ.0) THEN
            CALL INTLOG(JP_ERROR,'INTOCN: Domain out of range.',JPQUIET)
            CALL INTLOGR(JP_ERROR,'INTOCN: Requested Y = ',RBOTT)
            CALL INTLOGR(JP_ERROR,'INTOCN: Data limit of Y = ',YARR(1))
            INTOCN = JPROUTINE + 10
            GOTO 900
          ENDIF
          IF(JMAX.EQ.NY+1) THEN
            CALL INTLOG(JP_ERROR,'INTOCN: Domain out of range.',JPQUIET)
            CALL INTLOGR(JP_ERROR,'INTOCN: Requested Y = ',RTOP)
            CALL INTLOGR(JP_ERROR,'INTOCN: Data limit of Y = ',YARR(NY))
            INTOCN = JPROUTINE + 11
            GOTO 900
          ENDIF
C
          NYP    = (JMAX-JMIN)+1
          NYLEFT = JMIN
          DO 411 JI = 1,NYP
            YARR(JI) = YARR(JI+(NYLEFT-1))
  411     CONTINUE
C
          IF((KSEC1(61).EQ.3) .AND. (KYINC*NY*YUNIT.GT.350.0) ) THEN
            IWRAP = 1
            CALL INTLOG(JP_ERROR,'INTOCN: Irregular wrap-round',JPQUIET)
            CALL INTLOG(JP_ERROR,'INTOCN: not yet implemented.',JPQUIET)
            INTOCN = JPROUTINE + 12
            GOTO 900
          ELSE
            IWRAP = 0
          ENDIF
C
        ELSE IF(IIRREG.EQ.3) THEN
          NYP    = NY
          NYLEFT = 1
          JSKIP  = JGCL+NX
          DO 412 JI = 1,NYP
            YARR(JI) = KSEC1(JSKIP+(NY+1-JI))*YUNIT
  412     CONTINUE
          IF((KSEC1(61).EQ.3) .AND. (KYINC*NY*YUNIT.GT.350.0) ) THEN
            IWRAP = 1
            CALL INTLOG(JP_ERROR,'INTOCN: Irregular wrap-round',JPQUIET)
            CALL INTLOG(JP_ERROR,'INTOCN: not yet implemented.',JPQUIET)
            INTOCN = JPROUTINE + 13
            GOTO 900
          ELSE
            IWRAP = 0
          ENDIF
        ENDIF
C
        IF((NXP*NYP).GT.KLENI) THEN
          CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NYP)
          CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
          INTOCN = JPROUTINE + 14
          GOTO 900
        ENDIF
C
C       Copy the data into the temporary array, and invert it at
C       same time.
C       Note: NYLEFT is the starting index in the inverted data array.
C
c       DO 416 JJ = 1,NYP
c         DO 414 JI = 1,NXP
c           PARRAY(JI+(JJ-1)*NXP) = TARRAY(JI+(NY-JJ)*NXP)
c 414     CONTINUE
c 416   CONTINUE
C
        DO 418 JJ = 1,NYP
cc        JJP   = JJ+NYLEFT-1
          JLEFT = NY-(NYLEFT-1)
          JJP   = JLEFT-JJ+1
          IF(IWRAP.EQ.1) THEN
            IF(JJP.LE.0)  JJP = JJP+NY
            IF(JJP.GT.NY) JJP = JJP-NY
          ENDIF
          IF((JJP.GE.1).AND.(JJP.LE.NY)) THEN
            DO 416 JI = 1,NXP
cc            PARRAY(JI+(NYP-JJ)*NXP) = TARRAY(JI+(JJP-1)*NXP)
              PARRAY(JI+(JJ-1)*NXP)   = TARRAY(JI+(JJP-1)*NXP)
  416       CONTINUE
          ELSE
            DO 417 JI = 1,NXP
              PARRAY(JI+(NYP-JJ)*NXP) = RMISS
  417       CONTINUE
          ENDIF
  418   CONTINUE
C
C     Interpolation from a grid irregular in Y.
C
      ELSE IF(((HICODE.EQ.'R').OR.
     *        (HICODE.EQ.'Y').OR.
     *        (HICODE.EQ.'F')    ).AND.
     *       ((IIRREG.EQ.2).OR.(IIRREG.EQ.3))  ) THEN
        YPSIGN = SIGN(1.0,(RTOP-RBOTT))
        IF( YPSIGN.GT.0 ) THEN
          NYP = NINT((RTOP-RBOTT+ 0.0001)/RYRES)+1
        ELSE
          NYP = NINT((RBOTT-RTOP + 0.0001)/RYRES)+1
        ENDIF
Cjdc    NYP = NINT((RTOP-RBOTT)/RYRES*YPSIGN)+1
        IF((NXP*NYP).GT.KLENI) THEN
          CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
          CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NYP)
          CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
          INTOCN = JPROUTINE + 15
          GOTO 900
        ENDIF
        RTOP    = RBOTT+RYRES*YPSIGN*(NYP-1)
        YARR(1) = RBOTT
        YARR(2) = RYRES*YPSIGN
        YARR(3) = 0.0
        IINTPOL = IINTPOL+2
        IF(IIRREG.EQ.2) THEN
            IOFFSET = JGCL
        ELSE IF(IIRREG.EQ.3) THEN
            IOFFSET = JGCL+NX
        ENDIF
        YSIGN  = SIGN(1.0,((KSEC1(IOFFSET+2)-KSEC1(IOFFSET+1))*1.0))
        YRSIGN = YSIGN*YPSIGN
        DO 420 JI = 1,NXP*NYP
          PARRAY(JI) = RMISS
  420   CONTINUE
C
C       Work through the known (irregular) data points. Calculate the
C       position of the point as a fractional number of points on the
C       new regular grid (YFPOSN). If there are any points on the new
C       grid between this point and the previous one (YFPREV), then set
C       these points by interpolation between the two known points.
C       Check that the points are in range.
C       Note that we are willing to overlap slightly at the start, but
C       must fiddle the array arguments to do this.
C
        YFPREV= (KSEC1(IOFFSET+1)*YUNIT-RBOTT)/RYRES*YPSIGN - 0.5*YRSIGN
        DO 428 JJ = 1,NY
          YFPOSN = (KSEC1(IOFFSET+JJ)*YUNIT-RBOTT)/RYRES*YPSIGN
          NEWPTS =  INTDN((YFPOSN*YRSIGN)) -INTDN((YFPREV*YRSIGN))
C
C         INT truncates toward zero;
C         INTDN truncates downwards in all cases
C
          DO 426 JN = 1,NEWPTS
            IPOS = (INTDN(YFPREV*YRSIGN)+JN)*YRSIGN+1
            RPOS = (IPOS-1)*1.0
            IF((IPOS.GE.1).AND.(IPOS.LE.NYP)) THEN
              JJEFF = JJ
              IF(JJ.EQ.1) JJEFF = 2
              DO 424 JI = 1,NXP
                RVALL = TARRAY(JI+NXP*(JJEFF-2))
                RVALR = TARRAY(JI+NXP*(JJ-1))
                IF(.NOT.MISSING(RVALL) .AND. .NOT.MISSING(RVALR) ) THEN
                  XVAL = (YFPOSN-RPOS)/(YFPOSN-YFPREV)*RVALL
     *                      +(RPOS-YFPREV)/(YFPOSN-YFPREV)*RVALR
                ELSE
                  XVAL = RMISS
                ENDIF
                PARRAY(JI+NXP*(IPOS-1)) = XVAL
  424         CONTINUE
            ENDIF
  426     CONTINUE
          YFPREV = YFPOSN
  428   CONTINUE
C
C
C
C     4.3  INTERPOLATION FROM A GRID REGULAR IN Y
C     -------------------------------------------
C
      ELSEIF(((HICODE.EQ.'Y').OR.(HICODE.EQ.'F')).AND.
     *       ((IIRREG.EQ.0).OR.(IIRREG.EQ.1))    ) THEN

            LNEAREST=.TRUE.

            YPSIGN=SIGN(1.,(RTOP-RBOTT))
            NYP=NINT((RTOP-RBOTT)/RYRES*YPSIGN)+1
            IF((NXP*NYP).GT.KLENI) THEN
              CALL INTLOG(JP_ERROR,'INTOCN: Array too small.',JPQUIET)
              CALL INTLOG(JP_ERROR,'INTOCN: Required = ',NXP*NYP)
              CALL INTLOG(JP_ERROR,'INTOCN: Available = ',KLENI)
              INTOCN = JPROUTINE + 19
              GOTO 900
            ENDIF
            RTOP=RBOTT+RYRES*(NYP-1)*YPSIGN
            YARR(1)=RBOTT
            YARR(2)=RYRES*YPSIGN
            YARR(3)=0.0
C  Work through the new regular grid points. Calculate the position of
C  the point as a fractional number of points on the old grid. Use this
C  fractional number to estimate the value of the field at the new point
            IINTPOL=IINTPOL+2
            DO 335 JI=1,NXP*NYP
               PARRAY(JI)=RMISS
  335       CONTINUE
            XOSOUT=KLAT1*YUNIT
            XORES =KYINC*YUNIT
            DO 3370 JJ=1,NYP
               RPOS=(RBOTT+(JJ-1)*RYRES*YPSIGN-XOSOUT)/XORES + 1
               JPOS=INTDN(RPOS)
C  Check for out of bounds, and set to missing if this is the case
               IF((JPOS.GE.1).AND.(JPOS+1.LE.NY)) THEN
                 DO 3360 JI=1,NXP
                    RVALB = TARRAY(JI+(JPOS-1)*NXP)
                    RVALT = TARRAY(JI+(JPOS+1-1)*NXP)
                    IF(.NOT.MISSING(RVALB).AND.
     *                 .NOT.MISSING(RVALT)       ) THEN
                       RVAL=(JPOS+1-RPOS)*RVALB
     *                      +(RPOS-JPOS)*RVALT
                    ELSEIF(LNEAREST) THEN
                      IF((.NOT.MISSING(RVALB)) .AND.
     *                   ((JPOS+1-RPOS).GT.0.5)  ) THEN
                        RVAL=RVALB
                      ELSEIF((.NOT.MISSING(RVALT)).AND.
     *                   ((RPOS-JPOS).GT.0.5)  ) THEN
                        RVAL=RVALT
                      ELSE
                        RVAL=RMISS
                      ENDIF
                    ELSE
                      RVAL=RMISS
                    ENDIF
                    PARRAY(JI+(JJ-1)*NXP)=RVAL
 3360            CONTINUE
               ELSE
                  DO 3365 JI=1,NXP
                     PARRAY(JI+(JJ-1)*NXP)=RMISS
 3365             CONTINUE
               ENDIF
 3370       CONTINUE
C
C
      ELSE
        CALL INTLOG(JP_ERROR,'INTOCN: Error in logic.',JPQUIET)
        CALL INTLOG(JP_ERROR,'INTOCN: HICODE = ',JPQUIET)
        CALL INTLOG(JP_ERROR,HICODE,JPQUIET)
        CALL INTLOG(JP_ERROR,'INTOCN: IIRREG = ',IIRREG)
        INTOCN = JPROUTINE + 16
        GOTO 900
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 9 . Return.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
C
      RETURN
      END
