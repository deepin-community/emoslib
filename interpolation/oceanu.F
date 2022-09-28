C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION OCEANU( INOCEAN, INLEN, OUTOCEAN, OUTLEN)
C
C---->
C**** OCEANU
C
C     Purpose
C     -------
C
C     Interpolate input ocean field to ocean
C     field.
C
C
C     Interface
C     ---------
C
C     IRET = OCEANU( INOCEAN, INLEN, OUTOCEAN, OUTLEN)
C
C     Input
C     -----
C
C     INOCEAN - Input ocean field
C     INLEN   - Input field length (words).
C
C
C     Output
C     ------
C
C     OUTOCEAN - Output ocean field
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
      INTEGER INLEN, OUTLEN
      REAL INOCEAN, OUTOCEAN
      DIMENSION INOCEAN(*),OUTOCEAN(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
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
c     CHARACTER*1 HFUNC
      CHARACTER*1 HINTOPT
      CHARACTER*6 YFLAG
      INTEGER IOLDN, IOLDW, IOLDS, IOLDE, IOLDEW, IOLDNS
      INTEGER LOOPLAT, LOOPLON, I, J, IOS, LOOP
      REAL ZTEMP
      INTEGER IERR, KPR, IOLDSIZ, INEWSIZ, IINTPOL, ISTAGP
      INTEGER NXP, NYP, INEWP, ITEMP
      REAL RTOP, RBOTT, RRIGHT, RLEFT, RYRES, RXRES
      REAL XARR, YARR
      DIMENSION XARR(JPGRIB_ISEC1), YARR(JPGRIB_ISEC1)
      REAL NEW
      POINTER ( INEW, NEW )
      DIMENSION NEW( 1 )
C
C     Externals
C
      INTEGER INTOCNU, FIXAREA
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
     X  'OCEANU: Trying to interpolate an ocean field.',JPQUIET)
C
      IF( .NOT.LNOGRID ) THEN
        CALL INTLOG(JP_FATAL,
     X    'OCEANU: GRID must be specified to interpolate an ocean field'
     X    , JPQUIET)
        OCEANU = JPROUTINE + 1
        GOTO 960
      ENDIF
C
      OCEANU = 0
      IERR   = 0
      KPR    = 0
      LFLIP = .FALSE.
C
C     Save the original area and grid definitions
C
       NOOCO3  = NIOCO3
       NOOCO4  = NIOCO4
       NOOCO4F = NIOCO4F
       NOOCO3F = NIOCO3F
       NOOCO4L = NIOCO4L
       NOOCO3L = NIOCO3L
       NOOIINC = NIOIINC
       NOOJINC = NIOJINC
       NOOIRGR = NIOIRGR
       NOONOST = NIONOST

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
      LHORIZN = (NIOCO3.EQ.JP_LONG).AND.(NIOCO4.EQ.JP_LAT)
      IF( LHORIZN ) CALL INTLOG(JP_DEBUG,
     X  'OCEANU: Horizontal (lat/long) field interpolation.',JPQUIET)
C
C     Get scratch memory for unpacking the input ocean field.
C
      IOLDSIZ = NINS * NIWE
C
C     For horizontal field, match the desired area to the grid
C
      IERR = FIXAREA()
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'OCEANU: FIXAREA failed.',JPQUIET)
        OCEANU = IERR
        GOTO 940
      ENDIF
C
C     Get scratch memory for output ocean field.
C     Have to guess how big the interpolated grid will be.
C
      IF(NOGRID(1).GT.0.AND.NOGRID(1).GT.0) THEN
         INEWSIZ = (JP360/NOGRID(1) + 1) * (JP180 / NOGRID(2) + 1)*2
      ELSE
         INEWSIZ = JP_GUESS
      ENDIF
cs    print*,'OCEANU INEWSIZ ',INEWSIZ
cs    print*,'OCEANU JP_GUESS ',JP_GUESS
cs    print*,'OCEANU NOGRID(1): ',NOGRID(1),' NOGRID(2): ',NOGRID(2)
      CALL JMEMHAN(JPSCR3, INEW, INEWSIZ*2, JPALLOC, IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANU: Scratch memory(3) allocation failed.',JPQUIET)
        OCEANU = IERR
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
      IF( NOGRID(1).NE.NIGRID(1) ) HINTOPT = 'F'
C
      IERR = INTOCNU(HINTOPT,'D',
     X              RTOP, RBOTT, RRIGHT, RLEFT, RYRES, RXRES,
     X              INOCEAN, IOLDSIZ,
     X              INEWSIZ, NEW(ITEMP), XARR, YARR, NXP, NYP,
     X              NEW(INEWP),
     X              IINTPOL, ISTAGP)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'OCEANU: Interpolation failed.',JPQUIET)
        OCEANU = IERR
        GOTO 900
      ENDIF
C
      NOAREA(1) = NINT( (YARR(1) + YARR(2)*(NYP-1)) * PPMULT )
      NOAREA(2) = NINT( XARR(1) * PPMULT )
      NOAREA(3) = NINT( YARR(1) * PPMULT )
      NOAREA(4) = NINT( (XARR(1) + XARR(2)*(NXP-1)) * PPMULT )
C
      NOAAPI(1) = NOAREA(1)
      NOAAPI(2) = NOAREA(2)
      NOAAPI(3) = NOAREA(3)
      NOAAPI(4) = NOAREA(4)
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
          ITEMP     = NOAAPI(1)
          NOAAPI(1) = NOAAPI(3)
          NOAAPI(3) = ITEMP
        ENDIF
      ENDIF
C
C     Setup GRIB sections for the output product:
C
C
      IF( LHORIZN ) THEN
c
        NOOCO4F = NOAREA(1)*10
        NOOCO3F = NOAREA(2)*10
        NOOCO4L = NOAREA(3)*10
        NOOCO3L = NOAREA(4)*10
        NOOIINC = NOGRID(1)*10
        NOOJINC = NOGRID(2)*10
        IF( LFLIP ) NOOJINC = - NOOJINC
        NOOIRGR = 0
        NOONOST = 0
C
C       Standard section 2 format for horizontal sections
C
        NOAAPI(1) = NOAREA(1)
        NOAAPI(2) = NOAREA(2)
        NOAAPI(3) = NOAREA(3)
        NOAAPI(4) = NOAREA(4)
        NOWE = NXP
        NONS = NYP
      ELSE
C Tim-Correct headers for vertical sections
        IF( NIOCO3.EQ.3 ) THEN
          NOOCO3F = NOAREA(2)*10
        ELSEIF( NIOCO3.EQ.4 ) THEN
          NOOCO3F = NOAREA(2)*10
        ENDIF
        IF( NIOCO4.EQ.3 ) THEN
          NOOCO4F = NOAREA(1)*10
        ELSEIF( NIOCO4.EQ.4 ) THEN
          NOOCO4F = NOAREA(1)*10
        ENDIF
C
        IF( NIOCO4.EQ.1 ) THEN
          NOOCO4L = NOOCO4F + NINT((NYP-1)*RYRES*1.0)
          NOOCO3L = NOOCO3F + NINT((NXP-1)*RXRES*1000000.0)
          NOOIINC = NOGRID(1)*NINT(1000000.0/PPMULT)
          NOOJINC = NOGRID(2)/(PPMULT/1.0)
        ELSE
          NOOCO4L = NOOCO4F + NINT((NYP-1)*RYRES*1000.0)
          NOOCO3L = NOOCO3F + NINT((NXP-1)*RXRES*1000000.0)
          NOOIINC = NOGRID(1)*NINT(1000000.0/PPMULT)
          NOOJINC = NOGRID(2)/(PPMULT/1000.0)
        ENDIF
C
        IF( LFLIP ) NOOJINC = - NOOJINC
        NOOIRGR = 0
C
C       Special ECMWF ocean short form of GRIB section 2
C
cs        ISEC2(1) = 192
cs        ISEC2(2) = NXP
cs        ISEC2(3) = NYP
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
cs      ISEC1(5) = 192
cs      ISEC3(1) = 0
cs      ZSEC3(2) = -999.9
C
cs      ISEC4(1) = NXP *  NYP
C
C     If grid-point output, setup for 2nd order packing if requested.
C
cs      IF( (NOREPR.NE.JPSPHERE).AND.(NOREPR.NE.JPSPHROT) ) THEN
cs        HFUNC = 'C'
cs        IF( NOHFUNC.EQ.'K' ) THEN
cs          HFUNC = 'K'
cs          ISEC4(4)  = 64
cs          ISEC4(6)  = 16
cs          ISEC4(9)  = 32
cs          ISEC4(10) = 16
cs          ISEC4(12) = 8
cs          ISEC4(13) = 4
cs          ISEC4(14) = 0
cs          ISEC4(15) = -1
cs        ELSE
cs          ISEC4(4)  = 0
cs          ISEC4(6)  = 0
cs        ENDIF
cs      ELSE
cs        HFUNC = 'C'
cs        IF( NOHFUNC.EQ.'C' ) THEN
cs          ISEC2(6) = 2
cs          ISEC4(4) = 64
cs        ELSE IF( NOHFUNC.EQ.'S' ) THEN
cs          ISEC2(6) = 1
cs          ISEC4(4) = 0
cs        ENDIF
cs      ENDIF
C
C
      OUTLEN = NXP*NYP
          DO I = 1, OUTLEN
            OUTOCEAN(I)  = NEW(I)
          ENDDO
        CALL GETENV('DATA_CHECK', YFLAG)
        IF( YFLAG(1:1).EQ.'1' ) THEN
          OPEN (UNIT=18,FILE='./out_ocean_gribapi.dat',STATUS='UNKNOWN',
     &    FORM='UNFORMATTED',IOSTAT=IOS)
          IF (IOS.EQ.0) THEN
            WRITE (18)(OUTOCEAN(LOOP),LOOP=1,OUTLEN)
          ENDIF
          CLOSE(18)
        ENDIF
C
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
      CALL JMEMHAN( JPSCR3, INEW, INEWSIZ*2, JPDEALL, IERR)
      IF ( IERR .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'OCEANU: Scratch memory(3) reallocation failed.',JPQUIET)
        OCEANU = IERR
      ENDIF
C
  940 CONTINUE
C
C
  950 CONTINUE
C
C     Restore the original area and grid definitions
C
cs      NOAREA(1) = IOLDN
cs      NOAREA(2) = IOLDW
cs      NOAREA(3) = IOLDS
cs      NOAREA(4) = IOLDE
cs      NOGRID(1) = IOLDEW
cs      NOGRID(2) = IOLDNS
C
  960 CONTINUE
C
      CALL INTLOG(JP_DEBUG,
     X  'OCEANU: Returning from interpolating an ocean field.',JPQUIET)
C
      RETURN
      END
