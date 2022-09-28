C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTWAVE2(INGRIB,INLEN,OUTGRIB,OUTLEN)
C
C---->
C**** INTWAVE2
C
C     Purpose
C     -------
C
C     Interpolate an ECMWF wave field wave field or a reduced
C     latitude-longitude field
C
C
C     Interface
C     ---------
C
C     IRET = INTWAVE2(INGRIB,INLEN,OUTGRIB,OUTLEN)
C
C     Input
C     -----
C
C     INGRIB - Input field (packed).
C     INLEN  - Input field length (words).
C
C
C     Output
C     ------
C
C     OUTGRIB - Output field (packed).
C     OUTLEN  - Output field length (words).
C
C
C     Method
C     ------
C
C     Call interpolation routine; then repack into GRIB format.
C
C
C     Externals
C     ---------
C
C     FIXAREA - Fixup area definition to correspond to grid definitions
C     GRIBEX  - Decode/encode GRIB product.
C     JMEMHAN - Handles memory allocation.
C     JDEBUG  - Checks environment to switch on/off debug
C     INTLOG  - Log error message.
C     WAVEXX1 - Interpolate wave fields (except 2D spectra)
C     WV2DXXX - Interpolate wave 2D spectra fields
C     MKFRAME - Create a 'frame' from a rectangular field
C     MKBITMP - Apply a bitmap to a rectangular field
C
C
C     Author
C     ------
C
C     S.Curic     ECMWF     Jun 2009
C
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
      INTEGER INLEN,OUTLEN
      INTEGER INGRIB(*),OUTGRIB(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "intf.h"
#include "grfixed.h"
#include "jparams.h"
C
C     Parameters
      INTEGER JPROUTINE, JP2DSP, JP2DSPQ, JPMAXLT

      PARAMETER (JPROUTINE = 40200 )
      PARAMETER (JP2DSP  = 250 )  ! for 2D wave spectra (whole)
      PARAMETER (JP2DSPQ = 251 )  ! for 2D wave spectra (single)
      PARAMETER (JPMAXLT = 1801 )
C
C     Local variables
C
      CHARACTER*1 HFUNC

      REAL ZMISS
      DATA ZMISS/-9999999.0/
      INTEGER IWORD, IRET, ISIZE, NSPEC, LAT1, LOOP, IERR
      INTEGER KNUM, NUM_E_W, NUM_N_S, NFULLNS, NFULLEW
      REAL GRIDWE, GRIDNS, NORTH, SOUTH, WEST, EAST
      INTEGER NUMPTS
      DIMENSION NUMPTS(JPMAXLT)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 INEWAVE, IDISTNW, IZ2DSP, INEWIDX
#endif
#endif
      REAL NEWAVE
      POINTER ( INEWAVE, NEWAVE )
      DIMENSION NEWAVE( 1 )
      REAL*4 DISTNEW
      POINTER ( IDISTNW, DISTNEW )
      DIMENSION DISTNEW( 1 )
      REAL Z2DSP
      POINTER ( IZ2DSP, Z2DSP )
      DIMENSION Z2DSP( 1 )
      INTEGER NEWIDX
      POINTER ( INEWIDX, NEWIDX )
      DIMENSION NEWIDX( 1 )
      REAL RNLAT, RSLAT
      INTEGER NUMTABL, NUMPROD
      LOGICAL LCOEFFS, L98WAVE, LFRAME, LBITMP
      LOGICAL LDEBUG
      INTEGER NPARAM
      INTEGER MISSLAT, KOLDNUM
C
C     Externals
      INTEGER WAVEXX2, WV2DXX2, AREACHK, FIXAREA
      INTEGER NUMPTNS, NUMPTWE, JNORSGG, MKBITMP
C
C ------------------------------------------------------------------
C     Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      INTWAVE2 = 0
C
      LCOEFFS = .FALSE.
C
C     Check if debug option turned on
      CALL JDEBUG()
      LDEBUG = NDBG.GT.0
C
C     Allocate work array ZNFELDI if not already done.
C
      IF( IZNJDCI.NE.1952999238 ) THEN
        CALL JMEMHAN( 19, IZNFLDI, JPEXPAND, 1, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_WARN,'INTWAVE2: ZNFELDI allocate fail',JPQUIET)
          INTWAVE2= IRET
          GOTO 900
        ENDIF
        IZNJDCI = 1952999238
      ENDIF
C
C     Unpack GRIB message headers (using ZMISS as missing data value).
C
cs      IWORD   = INLEN
      IRET = 1
      ISEC3(2) = NINT(ZMISS)
      ZSEC3(2) = ZMISS
      CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     X             ZNFELDI,JPEXPAND,INGRIB,INLEN,IWORD,'D',IRET)

      IF(IRET.GT.0) THEN
       CALL INTLOG(JP_ERROR,'INTWAVE2: GRIBEX decoding failed.',JPQUIET)
        INTWAVE2 = JPROUTINE + 1
        GOTO 900
      ENDIF

      NPARAM = ISEC1(6)
C
C     Check that the field can be handled
C
      NUMTABL = ISEC1(2)*1000 + ISEC1(1)
      NUMPROD = NUMTABL*1000 + ISEC1(6)
      L98WAVE = (NUMTABL.EQ.98140).OR.
     X          (NUMPROD.EQ.98131229).OR.
     X          (NUMPROD.EQ.98131232).OR.
     X          (NIREPR.EQ.26)
      IF( .NOT.L98WAVE ) THEN
        CALL INTLOG(JP_WARN,'INTWAVE2: Not an ECMWF wave field',JPQUIET)
        INTWAVE2 = JPROUTINE + 2
        GOTO 900
      ENDIF
      IF( ISEC2(1).NE.0 ) THEN
        CALL INTLOG(JP_WARN,
     X    'INTWAVE2: Not a lat/long field',JPQUIET)
        INTWAVE2 = JPROUTINE + 3
        GOTO 900
      ENDIF
      IF( (ISEC2(6).NE.128) .OR. (ISEC2(10).LT.10) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTWAVE2: Cannot handle longitude increment',ISEC2(10))
        INTWAVE2 = JPROUTINE + 4
        GOTO 900
      ENDIF
C
C ------------------------------------------------------------------
C     Section 2.   Sort out interpolation area.
C ------------------------------------------------------------------
C
  200 CONTINUE
C
      NSPEC = 1
      KNUM  = ISEC2(3)
C
C     Adjust (sub-)area limits to suit the grid
C
      IRET = FIXAREA()
      IF ( IRET .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR, 'INTWAVE2: FIXAREA failed.',JPQUIET)
        INTWAVE2 = JPROUTINE + 5
        GOTO 900
      ENDIF
      NORTH = FLOAT(NOAREA(1))/PPMULT
      WEST  = FLOAT(NOAREA(2))/PPMULT
      SOUTH = FLOAT(NOAREA(3))/PPMULT
      EAST  = FLOAT(NOAREA(4))/PPMULT
C
C     Calculate the number of points E-W and N-S in new grid area
C
      IF( (NOREPR.EQ.JPQUASI) .OR. (NOREPR.EQ.JPGAUSSIAN)) THEN
        GRIDWE  = NOGAUSS
        GRIDNS  = 0
      ELSE IF( NOREPR.EQ.JPREDLL ) THEN
        GRIDWE  = 360.0/NOLPTS((NOREDLL/2)+1)
        GRIDNS  = FLOAT(NOGRID(2))/PPMULT
      ELSE
        GRIDWE  = FLOAT(NOGRID(1))/PPMULT
        GRIDNS  = FLOAT(NOGRID(2))/PPMULT
      ENDIF
C
      IRET = AREACHK(GRIDWE,GRIDNS,NORTH,WEST,SOUTH,EAST)
      IF( IRET.NE.0 ) THEN
        INTWAVE2 = JPROUTINE + 6
        GOTO 900
      ENDIF
      NOAREA(1) = NINT(NORTH*PPMULT)
      NOAREA(2) = NINT(WEST*PPMULT)
      NOAREA(3) = NINT(SOUTH*PPMULT)
      NOAREA(4) = NINT(EAST*PPMULT)
C
      IF( (NOREPR.EQ.JPQUASI) .OR. (NOREPR.EQ.JPGAUSSIAN)) THEN
        GRIDWE  = 360.0/NOLPTS(NOGAUSS)
        NUM_E_W = NUMPTWE(WEST,EAST,GRIDWE)
        NFULLEW = NUMPTWE(0.0,360.0,GRIDWE) - 1
        NUM_N_S = JNORSGG(SOUTH,ROGAUSS,NOGAUSS,1) -
     X            JNORSGG(NORTH,ROGAUSS,NOGAUSS,0) + 1
        NFULLNS = NOGAUSS*2
      ELSE IF( NOREPR.EQ.JPREDLL ) THEN
        NUM_E_W = NUMPTWE(WEST,EAST,GRIDWE)
        NFULLEW = NUMPTWE(0.0,360.0,GRIDWE) - 1
        NUM_N_S = NUMPTNS(NORTH,SOUTH,GRIDNS)
        NFULLNS = NOREDLL
      ELSE
        NUM_E_W = NUMPTWE(WEST,EAST,GRIDWE)
        NFULLEW = NUMPTWE(0.0,360.0,GRIDWE) - 1
        NUM_N_S = NUMPTNS(NORTH,SOUTH,GRIDNS)
        NFULLNS = NUM_N_S
      ENDIF
C
C
C--------------------------------------------------------------------
        KNUM = NINS
cs        KNUM = ISEC2(3)
      IF( ISEC2(17).EQ.1 ) THEN
C
C       Input field is a reduced latitude/longitude grid
C
C       .. but it may be 'pseudo-gaussian' in layout
C       (ie global, symmetric about the equator but no latitude
C        at the equator)
C
        IF( (ISEC2(4).NE.90000).AND.(MOD(ISEC2(3),2).EQ.0) ) THEN
C
          DO LOOP = 1, ISEC2(3)
            NUMPTS(LOOP) = ISEC2(22+LOOP)
          ENDDO
C
        ELSE
C
          MISSLAT = (90000 - ISEC2(4))/ISEC2(10)
          DO LOOP = 1, MISSLAT
            NUMPTS(LOOP)    = 0
          ENDDO
          KOLDNUM = 1 + (90000 - ISEC2(7))/ISEC2(10)
          DO LOOP = 1, (KOLDNUM-MISSLAT)
            NUMPTS(LOOP+MISSLAT) = ISEC2(22+LOOP)
          ENDDO
          DO LOOP = (KOLDNUM+1), KNUM
            NUMPTS(LOOP)    = 0
          ENDDO
        ENDIF
      ENDIF
C
C
C ------------------------------------------------------------------
C     Section 3.   Interpolate wave fields other than 2D spectra.
C ------------------------------------------------------------------
C
  300 CONTINUE
C
C     Handle if not 2D spectra ..
C
      IF( (ISEC1(6).NE.JP2DSP).AND.(ISEC1(6).NE.JP2DSPQ) ) THEN
       IF( LDEBUG ) THEN
       CALL INTLOG(JP_DEBUG,
     X 'INTWAVE2: Interpolate wave flds other than 2D spectra',JPQUIET)
       ENDIF
C
C       Get some scratch space for the interpolated field.
C
        ISIZE = NFULLEW * NUM_N_S
        CALL JMEMHAN( 3, INEWAVE, ISIZE, 1, IRET)
        IF ( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTWAVE2: Get work space fail.',JPQUIET)
          INTWAVE2 = JPROUTINE + 7
          GOTO 900
        ENDIF
C
        ISIZE = NFULLEW * NFULLNS * 4
        CALL JMEMHAN( 4, INEWIDX, ISIZE, 1, IRET)
        IF ( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTWAVE2: Get work space fail.',JPQUIET)
          INTWAVE2 = JPROUTINE + 8
          GOTO 900
        ENDIF
C
        ISIZE = NFULLEW * NFULLNS * 10
        CALL JMEMHAN( 5, IDISTNW, ISIZE, 1, IRET)
        IF ( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTWAVE2: Get work space fail.',JPQUIET)
          INTWAVE2 = JPROUTINE + 9
          GOTO 900
        ENDIF

C       Interpolate the field
C
        IRET = WAVEXX2(NPARAM,KNUM,NUMPTS,NUM_N_S,GRIDNS,GRIDWE,ZNFELDI,
     X               NEWAVE,NORTH,WEST,ZMISS)
        IF( IRET.NE.0 ) THEN
         CALL INTLOG(JP_ERROR,'INTWAVE2: Interpolation failed.',JPQUIET)
          INTWAVE2 = JPROUTINE + 10
          GOTO 900
        ENDIF
C
C ------------------------------------------------------------------
C     Section 4.   Interpolate wave 2D spectra field.
C ------------------------------------------------------------------
C
      ELSE
C
  400   CONTINUE

      IF( LDEBUG ) THEN
      CALL INTLOG(JP_DEBUG,
     X 'INTWAVE2: Interpolate wave 2D spectra field',JPQUIET)
      ENDIF
C
C       Find number of 2D spectra values at each point
C
        IF( ISEC4(8).NE.0 ) LCOEFFS = .TRUE.
        IF( (ISEC4(8).NE.0).AND.(ISEC1(6).NE.JP2DSPQ) ) THEN
          NSPEC = ISEC4(50) * ISEC4(51)
          NIMATR = 1
        ELSE
          NSPEC = 1
          NIMATR = 0
        ENDIF
C
C       Get some scratch space for the interpolated field.
C
        ISIZE = NSPEC * NFULLEW * NUM_N_S
        CALL JMEMHAN( 3, INEWAVE, ISIZE, 1, IRET)
        IF ( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTWAVE2: Get work space fail.',JPQUIET)
          INTWAVE2 = JPROUTINE + 11
          GOTO 900
        ENDIF
C
        ISIZE = NFULLEW * NFULLNS
        CALL JMEMHAN( 4, INEWIDX, ISIZE, 1, IRET)
        IF ( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTWAVE2: Get work space fail.',JPQUIET)
          INTWAVE2 = JPROUTINE + 12
          GOTO 900
        ENDIF
C
C       Interpolate the field
C
        IRET = WV2DXX2(NIMATR,KNUM,NUMPTS,NUM_N_S,GRIDNS,GRIDWE,ZNFELDI,
     X               NEWAVE,NORTH,WEST,ZMISS)
        IF( IRET.NE.0 ) THEN
         CALL INTLOG(JP_ERROR,'INTWAVE2: Interpolation failed.',JPQUIET)
          INTWAVE2 = JPROUTINE + 13
          GOTO 900
        ENDIF
C
      ENDIF
C
C ------------------------------------------------------------------
C     Section 5.   Pack the interpolated field into GRIB.
C ------------------------------------------------------------------
C
  500   CONTINUE

C     If a 'bitmap' has been specified, build the bitmap
C
      LBITMP = LNOBITMP.AND.
     X         ((NOREPR.EQ.JPREGROT).OR.(NOREPR.EQ.JPREGULAR).OR.
     X          (NOREPR.EQ.JPGAUSSIAN))

      IF( LBITMP ) THEN
        CALL INTLOG(JP_DEBUG,'INTWAVE2: MKBITMP is enabled',JPQUIET)
        IERR = MKBITMP(NUM_E_W,NUM_N_S,NEWAVE,ZMISS)
        IF( IERR.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTFB: Problem applying bitmap',JPQUIET)
          GOTO 900
        ENDIF
      ENDIF
C     If a 'frame' has been specified, build the frame
C
       LFRAME = LNOFRAME.AND.
     X         ((NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPGAUSSIAN).OR.
     X          (NOREPR.EQ.JPREGROT ).OR.(NOREPR.EQ.JPFGGROT  ) )

         CALL INTLOG(JP_DEBUG,'INTWAVE2: NUM_E_W ',NUM_E_W)
         CALL INTLOG(JP_DEBUG,'INTWAVE2: NUM_N_S ',NUM_N_S)
      IF( LFRAME ) THEN
           CALL INTLOG(JP_DEBUG,'INTWAVE2: MKFRAME is enabled',JPQUIET)
           CALL INTLOG(JP_DEBUG,'INTWAVE2: NOFRAME ',NOFRAME)
         CALL MKFRAME(NUM_E_W,NUM_N_S,NEWAVE,ZMISS,NOFRAME)
      ENDIF
C
C     GRIB sections 2 and 3 (bitmap) included
C
      ISEC1(5) = 192
C
      IF( (NOREPR.EQ.JPQUASI) .OR. (NOREPR.EQ.JPREDLL) ) THEN
C
C       Reduced gaussian or reduced lat/long with (possible) subarea
C
        IF( NOREPR.EQ.JPQUASI ) THEN
          ISEC2(1) = JPGAUSSIAN
          ISEC2(10)= NOGAUSS
        ELSE
          ISEC2(1) = JPREGULAR
          ISEC2(10)= NINT(GRIDNS*1000.0)
        ENDIF
        ISEC2(2) = 255
        ISEC2(3) = NUM_N_S
        ISEC2(5) = NINT(WEST*1000.0)
        ISEC2(6) = 0
        ISEC2(8) = NINT(EAST*1000.0)
        ISEC2(17)= 1
        ISEC4(1) = 0
C
        LAT1 = 0
 510    CONTINUE
        LAT1 = LAT1 + 1
        IF( NOREPR.EQ.JPQUASI ) THEN
          RNLAT = ROGAUSS(LAT1)
          RSLAT = ROGAUSS(LAT1+NUM_N_S-1)
        ELSE
          RNLAT = 90.0 - (LAT1-1)*GRIDNS
          RSLAT = 90.0 - (LAT1+NUM_N_S-2)*GRIDNS
        ENDIF
        IF( RNLAT.GT.NORTH ) GOTO 510
C
        ISEC2(4) = NINT(RNLAT*1000.0)
        ISEC2(7) = NINT(RSLAT*1000.0)
C
        DO LOOP = LAT1, (NUM_N_S+LAT1-1)
          ISEC2(23+LOOP-LAT1) = NOLPTS(LOOP)
          ISEC4(1) = ISEC4(1) + ISEC2(23+LOOP-LAT1)
        ENDDO
C
      ELSE
C
C       Regular gaussian or lat/long with (possible) subarea
C
        IF( NOREPR.EQ.JPGAUSSIAN ) THEN
          ISEC2(1) = JPGAUSSIAN
        ELSE
          ISEC2(1) = JPREGULAR
        ENDIF
        ISEC2(2) = NUM_E_W
        ISEC2(3) = NUM_N_S
        ISEC2(4) = NINT(NORTH*1000.0)
        ISEC2(5) = NINT(WEST*1000.0)
        ISEC2(6) = 128
        ISEC2(7) = NINT(SOUTH*1000.0)
        ISEC2(8) = NINT(EAST*1000.0)
        ISEC2(9) = NINT(GRIDWE*1000.0)
        IF( NOREPR.EQ.JPGAUSSIAN ) THEN
          ISEC2(10) = NOGAUSS
        ELSE
          ISEC2(10) = NINT(GRIDNS*1000.0)
        ENDIF
        ISEC2(17)= 0
        ISEC4(1) = NUM_E_W * NUM_N_S
      ENDIF
C
C
      ISEC3(2) = NINT(ZMISS)
      ZSEC3(2) = ZMISS
C
      ISEC4(1) = ISEC4(1) * NSPEC
      ISEC4(2) = NOACC
      IF( LCOEFFS ) ISEC4(6)  = 16
C            `-----> wave fields have additional flags for
C                    NC1 and NC2 coefficients (floats stored
C                    in integer array isec4)
      IRET = 1
C
C     If grid-point output, setup for 2nd order packing if requested.
C
      IF( (NOREPR.NE.JPSPHERE) .AND. (NOREPR.NE.JPSPHROT) ) THEN
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
      CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     X             NEWAVE,ISEC4(1),OUTGRIB,OUTLEN,IWORD,HFUNC,IRET)
C
      IF ( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTWAVE2: GRIBEX encoding failed.',IRET)
        INTWAVE2 = JPROUTINE + 14
        GOTO 900
      ENDIF
      OUTLEN = IWORD
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
C     Clear change flags for next product processing
C
      LCHANGE = .FALSE.
      LSMCHNG = .FALSE.
C
      RETURN
      END
