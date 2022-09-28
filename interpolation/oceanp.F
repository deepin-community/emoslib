C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION OCEANP( INOCEAN, INLEN, OTOCEAN, OUTLEN)
C
C---->
C**** OCEANP
C
C     Purpose
C     -------
C
C     Interpolate GRIB format input ocean field to GRIB format ocean
C     field.
C
C
C     Interface
C     ---------
C
C     IRET = OCEANP( INOCEAN, INLEN, OTOCEAN, OUTLEN)
C
C     Input
C     -----
C
C     INOCEAN - Input ocean field  (GRIB format).
C     INLEN   - Input field length (words).
C
C
C     Output
C     ------
C
C     OTOCEAN - Output ocean field  (GRIB format).
C     OUTLEN  - Output field length (words).
C
C
C     Method
C     ------
C
C     Interpolate ocean field.
C
C     If requested X grid increment does not match input GRIB
C     increment, do a full interpolation in both x and y. Otherwise,
C     interpolate only to make grid regular.
C
C     Externals
C     ---------
C
C     JDEBUG  - Tests if debug output required.
C     GRIBEX  - GRIB decoding/encoding.
C     INTLOG  - Log error message.
C     JMEMHAN - Allocate/deallocate scratch memory.
C     INTOCN  - Carry out ocean field to field interpolation.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     May 1996
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J.D.Chambers     ECMWF        Mar 1997
C     Allow for full or partial interpolation
C     (F or R option in call to INTOCN).
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER INOCEAN, INLEN, OTOCEAN, OUTLEN
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "intf.h"
C
C     Parameters
C
      INTEGER JPROUTINE, JPALLOC, JPDEALL, JPSCR3, JPSCR4
      INTEGER JP_LAT, JP_LONG, JP_GUESS
      PARAMETER (JPROUTINE = 36900 )
      PARAMETER (JPALLOC = 1) 
      PARAMETER (JPDEALL = 0) 
      PARAMETER (JPSCR3 = 3) 
      PARAMETER (JPSCR4 = 4) 
      PARAMETER (JP_LONG = 3) 
      PARAMETER (JP_LAT  = 4) 
      PARAMETER (JP_GUESS  = 18049691) 
cs    PARAMETER (JP_GUESS  = 1675245) 
cs    PARAMETER (JP_GUESS  = 1237104) 
cs    PARAMETER (JP_GUESS  = 1038240) 
C                          `---> allow for 0.25*0.25 interpolated grid
C
C     Local variables
C
      LOGICAL LHORIZN, LFLIP
      CHARACTER*1 HFUNC
      CHARACTER*1 HINTOPT
      INTEGER IOLDN, IOLDW, IOLDS, IOLDE, IOLDEW, IOLDNS
      INTEGER LOOPLAT, LOOPLON, I, J
      REAL ZTEMP
      INTEGER IERR, KPR, IWORD, IOLDSIZ, INEWSIZ, IINTPOL, ISTAGP
      INTEGER NXP, NYP, INEWP, ITEMP
      REAL RTOP, RBOTT, RRIGHT, RLEFT, RYRES, RXRES
      REAL XARR, YARR
      DIMENSION XARR(JPGRIB_ISEC1), YARR(JPGRIB_ISEC1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 INEW, IOLD
#endif
#endif
      REAL NEW, OLD
      POINTER ( INEW, NEW )
      POINTER ( IOLD, OLD )
      DIMENSION NEW( 1 ), OLD( 1 )
C
C     Externals
C
      INTEGER RESET_C, INTOCN, FIXAREA
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
C     Test if debug output required.
C
      CALL JDEBUG()
C
      CALL INTLOG(JP_DEBUG,
     X  'OCEANP: Trying to interpolate an ocean field.',JPQUIET)
C
      IF( .NOT.LNOGRID ) THEN
        CALL INTLOG(JP_FATAL,
     X    'OCEANP: GRID must be specified to interpolate an ocean field'
     X    , JPQUIET)
        OCEANP = JPROUTINE + 1
        GOTO 960
      ENDIF
C
      OCEANP = 0
      IERR   = 0
      KPR    = 0
      LFLIP = .FALSE.
C
C     Save the original area and grid definitions
C
      IOLDN  = NOAREA(1)
      IOLDW  = NOAREA(2)
      IOLDS  = NOAREA(3)
      IOLDE  = NOAREA(4)
      IOLDEW = NOGRID(1)
      IOLDNS = NOGRID(2)
C
C     -----------------------------------------------------------------|
C*    Section 2.   Unpack the input ocean field.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Decode first four sections of GRIB code to find number of values
C
      IWORD   = INLEN
      IERR    = 1
      IOLDSIZ = OUTLEN
      CALL GRIBEX(ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X            OLD, IOLDSIZ, INOCEAN, INLEN, IWORD, 'I',IERR)
C
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANP: GRIBEX option J decoding failed.',IERR)
        OCEANP = IERR
        GOTO 950
      ENDIF
C
C     Check it is an ocean field
C
      IF( (ISEC1(24).NE.1).OR.(ISEC1(37).NE.4) ) THEN
        CALL INTLOG(JP_ERROR, 'OCEANP: Not an ocean field.',JPQUIET)
        OCEANP = JPROUTINE + 3
        GOTO 950
      ENDIF
C
C     Horizontal (lat/long) field GRIB setup different from others
C
      LHORIZN = (ISEC1(60).EQ.JP_LONG).AND.(ISEC1(61).EQ.JP_LAT)
      IF( LHORIZN ) CALL INTLOG(JP_DEBUG,
     X  'OCEANP: Horizontal (lat/long) field interpolation.',JPQUIET)
C
C     Get scratch memory for unpacking the input ocean field.
C
      IOLDSIZ = ISEC2(2)*ISEC2(3)
      CALL JMEMHAN( JPSCR4, IOLD, IOLDSIZ, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANP: Scratch memory(4) allocation failed.',JPQUIET)
        OCEANP = IERR
        GOTO 950
      ENDIF
C
C     Decode data from GRIB code: -999.9 is a missing data value
C
      IWORD = INLEN
      IERR  = 1
      ZSEC3(2) = -999.9
      CALL GRIBEX(ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X            OLD, IOLDSIZ, INOCEAN, INLEN, IWORD, 'D',IERR)
C
      IF( (IERR.NE.0).AND.(IERR.NE.-2).AND.(IERR.NE.-4) ) THEN
        CALL INTLOG(JP_ERROR,'OCEANP: GRIBEX decoding failed.',IERR)
        OCEANP = IERR
        GOTO 940
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Prepare the output ocean field.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Setup interpolation options from input GRIB characteristics.
C
      IERR = RESET_C(ISEC1, ISEC2, ZSEC2, ISEC4)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANP: Setup interp. options from GRIB failed.',JPQUIET)
        OCEANP = IERR
        GOTO 940
      ENDIF
C
C     For horizontal field, match the desired area to the grid
C
      IERR = FIXAREA()
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'OCEANP: FIXAREA failed.',JPQUIET)
        OCEANP = IERR
        GOTO 940
      ENDIF
C
C     Get scratch memory for output ocean field.
C     Have to guess how big the interpolated grid will be.
C
      INEWSIZ = JP_GUESS
      CALL JMEMHAN(JPSCR3, INEW, INEWSIZ*2, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANP: Scratch memory(3) allocation failed.',JPQUIET)
        OCEANP = IERR
        GOTO 940
      ENDIF
      INEWP = 1
      ITEMP = 1 + INEWSIZ
C
C     -----------------------------------------------------------------|
C*    Section 4.   Interpolate ocean field.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      RTOP   = FLOAT( NOAREA(1) ) / PPMULT
      RBOTT  = FLOAT( NOAREA(3) ) / PPMULT
      RRIGHT = FLOAT( NOAREA(4) ) / PPMULT
      RLEFT  = FLOAT( NOAREA(2) ) / PPMULT
C
      RYRES = FLOAT( NOGRID(2) ) / PPMULT
      RXRES = FLOAT( NOGRID(1) ) / PPMULT
C
C     Select the interpolation option
C     (default = 'R' = interpolate only to make grid regular)
C     If requested X grid increment does not match input GRIB
C     increment, interpolate in x and y ('F' = full)
C
      HINTOPT = 'R'
      IF( NOGRID(1).NE.ISEC2(9) ) HINTOPT = 'F'
C
      IERR = INTOCN(HINTOPT,'D',
     X              RTOP, RBOTT, RRIGHT, RLEFT, RYRES, RXRES,
     X              ISEC1, ISEC2, OLD, IOLDSIZ,
     X              INEWSIZ, NEW(ITEMP), XARR, YARR, NXP, NYP,
     X              NEW(INEWP),
     X              IINTPOL, ISTAGP)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'OCEANP: Interpolation failed.',JPQUIET)
        OCEANP = IERR
        GOTO 900
      ENDIF
C
      NOAREA(1) = NINT( (YARR(1) + YARR(2)*(NYP-1)) * PPMULT )
      NOAREA(2) = NINT( XARR(1) * PPMULT )
      NOAREA(3) = NINT( YARR(1) * PPMULT )
      NOAREA(4) = NINT( (XARR(1) + XARR(2)*(NXP-1)) * PPMULT )
C
      NOGRID(1) = NINT( ABS(XARR(2)) * PPMULT )
      NOGRID(2) = NINT( ABS(YARR(2)) * PPMULT )
      NONS = NYP
      NOWE = NXP
C
C     -----------------------------------------------------------------|
C*    Section 5.   Code new field into GRIB.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
C     Re-order the rows from south to north to be north to south
C     (ECMWF convention).
C
      IF( LHORIZN.OR.(NOAREA(1).LT.NOAREA(3)) ) THEN
        LFLIP = .TRUE.
        DO LOOPLAT = 1, NYP/2
          I = (LOOPLAT - 1)*NXP
          J = (NYP - LOOPLAT)*NXP
          DO LOOPLON = 1, NXP
            I = I + 1
            J = J + 1
            ZTEMP  = NEW(I)
            NEW(I) = NEW(J)
            NEW(J) = ZTEMP
          ENDDO
        ENDDO
C
        IF( .NOT.LHORIZN ) THEN
          ITEMP     = NOAREA(1)
          NOAREA(1) = NOAREA(3)
          NOAREA(3) = ITEMP
C
          ITEMP    = ISEC2(4)
          ISEC2(4) = ISEC2(7)
          ISEC2(7) = ITEMP
        ENDIF
      ENDIF
C
C     Setup GRIB sections for the output product:
C
C     Use local definition 4 in GRIB section 1 ...
C
      ISEC1(73) = 0
      ITEMP = 75 + ISEC1(71) + ISEC1(72) + ISEC1(73) + ISEC1(74)
      ISEC1(ITEMP) = 0

C
      IF( LHORIZN ) THEN
        ISEC1(62) = NOAREA(1)*10
        ISEC1(63) = NOAREA(2)*10
        ISEC1(64) = NOAREA(3)*10
        ISEC1(65) = NOAREA(4)*10
        ISEC1(66) = NOGRID(1)*10
        ISEC1(67) = NOGRID(2)*10
        IF( LFLIP ) ISEC1(67) = - ISEC1(67)
        ISEC1(68) = 0
        ISEC1(69) = 0
C
C       Standard section 2 format for horizontal sections
C
        ISEC2(1) = 0
        ISEC2(2) = NXP
        ISEC2(3) = NYP
        ISEC2(4) = NOAREA(1)/100
        ISEC2(5) = NOAREA(2)/100
        ISEC2(6) = 128
        ISEC2(7) = NOAREA(3)/100
        ISEC2(8) = NOAREA(4)/100
        ISEC2(9)  = NOGRID(1)/100
        ISEC2(10) = NOGRID(2)/100
Cjdc    ISEC2(17) = 0
        DO LOOPLAT = 11, 22
          ISEC2(LOOPLAT) = 0
        ENDDO
      ELSE
C Tim-Correct headers for vertical sections
        IF( ISEC1(60).EQ.3 ) THEN
          ISEC1(63) = NOAREA(2)*10
        ELSEIF( ISEC1(60).EQ.4 ) THEN
          ISEC1(63) = NOAREA(2)*10
        ENDIF
        IF( ISEC1(61).EQ.3 ) THEN
          ISEC1(62) = NOAREA(1)*10
        ELSEIF( ISEC1(61).EQ.4 ) THEN
          ISEC1(62) = NOAREA(1)*10
        ENDIF
C
        IF( ISEC1(61).EQ.1 ) THEN
          ISEC1(64) = ISEC1(62) + NINT((NYP-1)*RYRES*1.0)
          ISEC1(65) = ISEC1(63) + NINT((NXP-1)*RXRES*1000000.0)
          ISEC1(66) = NOGRID(1)*NINT(1000000.0/PPMULT)
          ISEC1(67) = NOGRID(2)/(PPMULT/1.0)
        ELSE
          ISEC1(64) = ISEC1(62) + NINT((NYP-1)*RYRES*1000.0)
          ISEC1(65) = ISEC1(63) + NINT((NXP-1)*RXRES*1000000.0)
          ISEC1(66) = NOGRID(1)*NINT(1000000.0/PPMULT)
          ISEC1(67) = NOGRID(2)/(PPMULT/1000.0)
        ENDIF
C
        IF( LFLIP ) ISEC1(67) = - ISEC1(67)
        ISEC1(68) = 0
C
C       Special ECMWF ocean short form of GRIB section 2
C
        ISEC2(1) = 192
        ISEC2(2) = NXP
        ISEC2(3) = NYP
Cjdc    ISEC2(4) = NOAREA(1)/100000
Cjdc    ISEC2(5) = NOAREA(2)/100
Cjdc    ISEC2(6) = 0
Cjdc    ISEC2(7) = NOAREA(3)/100000
Cjdc    ISEC2(8) = NOAREA(4)/100
Cjdc    DO LOOPLAT = 9, 22
Cjdc      ISEC2(LOOPLAT) = 0
Cjdc    ENDDO
      ENDIF
C
C     Ensure there is a section 3 in case there is missing data in the
C     output area
C
      ISEC1(5) = 192
      ISEC3(1) = 0
      ZSEC3(2) = -999.9
C
      ISEC4(1) = NXP *  NYP
C
C     If grid-point output, setup for 2nd order packing if requested.
C
      IF( (NOREPR.NE.JPSPHERE).AND.(NOREPR.NE.JPSPHROT) ) THEN
        HFUNC = 'C'
        IF( NOHFUNC.EQ.'K' ) THEN
          HFUNC = 'K'
          ISEC4(4)  = 64
          ISEC4(6)  = 16
          ISEC4(9)  = 32
          ISEC4(10) = 16
          ISEC4(12) = 8
          ISEC4(13) = 4
          ISEC4(14) = 0
          ISEC4(15) = -1
        ELSE
          ISEC4(4)  = 0
          ISEC4(6)  = 0
        ENDIF
      ELSE
        HFUNC = 'C'
        IF( NOHFUNC.EQ.'C' ) THEN
          ISEC2(6) = 2
          ISEC4(4) = 64
        ELSE IF( NOHFUNC.EQ.'S' ) THEN
          ISEC2(6) = 1
          ISEC4(4) = 0
        ENDIF
      ENDIF
C
C     Switch off GRIBEX checking (sigh)
C
      CALL INTLOG(JP_DEBUG,
     X  'OCEANP: switching OFF GRIBEX checking',JPQUIET)
      CALL GRSVCK(0)
C
      IERR = 1
      CALL GRIBEX( ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X             NEW,INEWSIZ,OTOCEAN,OUTLEN,IWORD,HFUNC,IERR)
C
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'OCEANP: GRIBEX encoding failed.',IERR)
        OCEANP = JPROUTINE + 5
        GOTO 900
      ENDIF
      OUTLEN = IWORD
C
C     Switch on GRIBEX checking (sigh again)
C
      CALL INTLOG(JP_DEBUG,
     X  'OCEANP: switching ON GRIBEX checking',JPQUIET)
      CALL GRSVCK(1)
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
C     Clear change flags for next product processing
C
      LCHANGE = .FALSE.
      LSMCHNG = .FALSE.
C
C     Return the scratch memory.
C
      CALL JMEMHAN( JPSCR3, INEW, INEWSIZ, JPDEALL, IERR)
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANP: Scratch memory(3) reallocation failed.',JPQUIET)
        OCEANP = IERR
      ENDIF
C
  940 CONTINUE
C
      CALL JMEMHAN( JPSCR4, IOLD, IOLDSIZ, JPDEALL, IERR)
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANP: Scratch memory(4) reallocation failed.',JPQUIET)
        OCEANP = IERR
      ENDIF
C
  950 CONTINUE
C
C     Restore the original area and grid definitions
C
      NOAREA(1) = IOLDN
      NOAREA(2) = IOLDW
      NOAREA(3) = IOLDS
      NOAREA(4) = IOLDE
      NOGRID(1) = IOLDEW
      NOGRID(2) = IOLDNS
C
  960 CONTINUE
C
      CALL INTLOG(JP_DEBUG,
     X  'OCEANP: Returning from interpolating an ocean field.',JPQUIET)
C
      RETURN
      END
