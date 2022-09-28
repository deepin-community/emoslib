C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTWAVU(INFLD,INLEN,OUTFLD,OUTLEN)
C
C---->
C**** INTWAVU
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
C     IRET = INTWAVU(INFLD,INLEN,OUTFLD,OUTLEN)
C
C     Input
C     -----
C
C     INFLD  - Input field (packed).
C     INLEN  - Input field length (words).
C
C
C     Output
C     ------
C
C     OUTFLD  - Output field (packed).
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
C     JMEMHAN - Handles memory allocation.
C     JDEBUG  - Checks environment to switch on/off debug
C     INTLOG  - Log error message.
C     WAVEXX2 - Interpolate wave fields (except 2D spectra)
C     WV2DXX2 - Interpolate wave 2D spectra fields
C     MKFRAME - Create a 'frame' from a rectangular field
C     MKBITMP - Apply a bitmap to a rectangular field
C
C
C     Author
C     ------
C
C     S.Curic     ECMWF     Mar 2009
C
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
      INTEGER INLEN,OUTLEN
      REAL INFLD(*), OUTFLD(*)
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
      REAL ZMISS
cs      DATA ZMISS/-9999999.0/
      REAL RLATINC, ONORTH, OSOUTH
      INTEGER IRET, NSPEC, LOOP, IERR
      INTEGER KNUM, NUM_E_W, NUM_N_S, NFULLNS, NFULLEW
      REAL GRIDWE, GRIDNS, NORTH, SOUTH, WEST, EAST
      REAL INORTH, ISOUTH, IWEST, IEAST, RNS
      INTEGER NUMPTS,MISSLAT,KOLDNUM
      DIMENSION NUMPTS(JPMAXLT)
      INTEGER KNEWNUM
      LOGICAL LCOEFFS, LFRAME, LBITMP
      LOGICAL LDEBUG
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
      INTWAVU = 0
C
      LCOEFFS = .FALSE.
      ZMISS = RMISSGV
      ZSEC3(2) = RMISSGV
      ISEC3(2) = INT(RMISSGV)
      ISEC1(5) = 192

C
C     Check if debug option turned on
      CALL JDEBUG()
      LDEBUG = NDBG.GT.0
C
C
      OUTLEN = 0
      IRET = 1
C
C     Check that the field can be handled
C
C ------------------------------------------------------------------
C     Section 2.   Sort out interpolation area.
C ------------------------------------------------------------------
C
  200 CONTINUE
C
      NSPEC = 1
      INORTH = FLOAT(NIAREA(1))/PPMULT
      IWEST  = FLOAT(NIAREA(2))/PPMULT
      ISOUTH = FLOAT(NIAREA(3))/PPMULT
      IEAST  = FLOAT(NIAREA(4))/PPMULT
      KNUM  = NINS
      RNS = INORTH - ISOUTH
C
C     Adjust (sub-)area limits to suit the grid
C
      IRET = FIXAREA()
      IF ( IRET .NE. 0 ) THEN
        CALL INTLOG(JP_ERROR, 'INTWAVU: FIXAREA failed.',JPQUIET)
        INTWAVU = JPROUTINE + 5
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
        INTWAVU = JPROUTINE + 6
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
C
        NONS    = NFULLNS
        IF(NOREPR.EQ.JPQUASI) THEN
          KNEWNUM = NOGAUSS*2
          DO LOOP = 1, KNEWNUM
            OUTLEN  = OUTLEN + NOLPTS(LOOP)
          ENDDO
          NOWE = 0
        ELSE
          NOWE    = NUM_E_W
          OUTLEN  = NOWE*NONS
        ENDIF
      ELSE IF( NOREPR.EQ.JPREDLL ) THEN
        NUM_E_W = NUMPTWE(WEST,EAST,GRIDWE)
        NFULLEW = NUMPTWE(0.0,360.0,GRIDWE) - 1
        NUM_N_S = NUMPTNS(NORTH,SOUTH,GRIDNS)
        NFULLNS = NOREDLL
        CALL INTLOG(JP_ERROR,'INTWAVU: Reduced ll not posible',JPQUIET)
        GOTO 900
      ELSE
        NUM_E_W = NUMPTWE(WEST,EAST,GRIDWE)
        NFULLEW = NUMPTWE(0.0,360.0,GRIDWE) - 1
        NUM_N_S = NUMPTNS(NORTH,SOUTH,GRIDNS)
        NFULLNS = NUM_N_S
cs calculate outlen for regular lat-lon
        OUTLEN  = NUM_E_W*NUM_N_S
        NOWE    = NUM_E_W
        NONS    = NUM_N_S
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
      IF( (NIPARAM.NE.JP2DSP).AND.(NIPARAM.NE.JP2DSPQ) ) THEN
       IF( LDEBUG ) THEN
       CALL INTLOG(JP_DEBUG,
     X 'INTWAVU: Interpolate wave fields other than 2D spectra',JPQUIET)
       ENDIF
C--------------------------------------------------------------------
        KNUM = NINS
      IF(NIREPR.EQ.JPREDLL) THEN
C
C       Input field is a reduced latitude/longitude grid
C
C       .. but it may be 'pseudo-gaussian' in layout
C       (ie global, symmetric about the equator but no latitude
C        at the equator)
          MISSLAT = 0
        ONORTH = FLOAT(NIAREA(1))/PPMULT
        OSOUTH = FLOAT(NIAREA(3))/PPMULT

        RLATINC = FLOAT(NIGRID(2))/PPMULT


        IF( ( NIAREA(1).NE.90000).AND.(MOD(KNUM,2).EQ.0) )  THEN
C
          DO LOOP = 1, KNUM
            NUMPTS(LOOP) = MILLEN(LOOP)
          ENDDO
C
        ELSE
C
cs          MISSLAT = (90000 - NIAREA(1))/NIGRID(2)
          MISSLAT = (90.0 - ONORTH)/RLATINC
          DO LOOP = 1, MISSLAT
            NUMPTS(LOOP)    = 0
          ENDDO
cs          KOLDNUM = 1 + (90000 - NIAREA(3))/NIGRID(2)
          KOLDNUM = 1 + NINT((90.0 - OSOUTH)/RLATINC)
          DO LOOP = 1, (KOLDNUM-MISSLAT)
            NUMPTS(LOOP+MISSLAT) = MILLEN(LOOP)
          ENDDO
          DO LOOP = (KOLDNUM+1), KNUM
            NUMPTS(LOOP)    = 0
          ENDDO
        ENDIF
      ENDIF
C--------------------------------------------------------------------

C
C       Interpolate the field
C
        IRET = WAVEXX2(NIPARAM,KNUM,NUMPTS,NUM_N_S,GRIDNS,GRIDWE,INFLD,
     X               OUTFLD,NORTH,WEST,ZMISS)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTWAVU: Interpolation failed.',JPQUIET)
          INTWAVU = JPROUTINE + 10
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
     X 'INTWAVU: Interpolate wave 2D spectra field',JPQUIET)
      ENDIF
C
        IRET = WV2DXX2(NIMATR,KNUM,MILLEN,NUM_N_S,GRIDNS,GRIDWE,INFLD,
     X               OUTFLD,NORTH,WEST,ZMISS)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'INTWAVU: Interpolation failed.',JPQUIET)
          INTWAVU = JPROUTINE + 13
          GOTO 900
        ENDIF
C
      ENDIF
C
C ------------------------------------------------------------------
C     Section 5.   Frame and Bitmap
C ------------------------------------------------------------------
C
  500   CONTINUE

C     If a 'bitmap' has been specified, build the bitmap
C
      LBITMP = LNOBITMP.AND.
     X         ((NOREPR.EQ.JPREGROT).OR.(NOREPR.EQ.JPREGULAR).OR.
     X          (NOREPR.EQ.JPGAUSSIAN))

      IF( LBITMP ) THEN
        CALL INTLOG(JP_DEBUG,'INTWAVU: MKBITMP is enabled',JPQUIET)
        IERR = MKBITMP(NUM_E_W,NUM_N_S,OUTFLD,ZMISS)
        IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTWAVU: Problem applying bitmap',JPQUIET)
          GOTO 900
        ENDIF
      ENDIF
C     If a 'frame' has been specified, build the frame
C
       LFRAME = LNOFRAME.AND.
     X         ((NOREPR.EQ.JPREGULAR).OR.(NOREPR.EQ.JPGAUSSIAN).OR.
     X          (NOREPR.EQ.JPREGROT ).OR.(NOREPR.EQ.JPFGGROT  ) )

         CALL INTLOG(JP_DEBUG,'INTWAVU: NUM_E_W ',NUM_E_W)
         CALL INTLOG(JP_DEBUG,'INTWAVU: NUM_N_S ',NUM_N_S)
      IF( LFRAME ) THEN
           CALL INTLOG(JP_DEBUG,'INTWAVU: MKFRAME is enabled',JPQUIET)
           CALL INTLOG(JP_DEBUG,'INTWAVU: NOFRAME ',NOFRAME)
         CALL MKFRAME(NUM_E_W,NUM_N_S,OUTFLD,ZMISS,NOFRAME)
      ENDIF
C
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
