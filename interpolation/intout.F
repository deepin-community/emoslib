C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTOUT( HPARN, INTV, REALV, CHARV)
C
C---->
C**** INTOUT
C
C     Purpose
C     -------
C
C     Change output field parameters in common block.
C
C     Interface
C     ---------
C
C     Input
C     -----
C
C     HPARN - MARS parameter name.
C     INTV  - MARS parameter value(s) (Integer).
C     REALV - MARS parameter value(s) (Real).
C     CHARV - MARS parameter value(s) (Character).
C
C
C     Method
C     ------
C
C     Recognise MARS paramter and reset appropriate common
C     variable value(s).
C
C
C     Externals
C     ---------
C
C     JDEBUG   - Check if debug switched on.
C     IBASINI  - Check basic initialisation of common blocks is done.
C     CHEQUAL  - Compare two ASCII strings without regard to case.
C     INTLOG   - Log error message.
C     IGGLAT   - Compute gaussian latitudes for given truncation.
C
C
C     Author
C     ------
C
C     J.D.Chambers       ECMWF        August 1994.
C
C     Modifications.
C     --------------
C
C     S. Curic      ECMWF      28.05.07
C     added jpeg packing just for merging with grib_api
C
C----<
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 28200)
C
C     Function arguments
C
      CHARACTER*(*) HPARN
      CHARACTER*(*) CHARV
      REAL REALV(*)
      INTEGER INTV(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "current.h"
C
C     Local variables
C
      INTEGER ITEMP, I, IRET, IFILE1
      LOGICAL LFOUND
      CHARACTER*1 CTEMP
      CHARACTER*8 CTEMP8
C
C     Externals
C
      LOGICAL CHEQUAL, ISOCTAHEDRAL, ISGGVALID
      INTEGER IGGLAT, JINDEX
      EXTERNAL CHEQUAL, ISOCTAHEDRAL, ISGGVALID
      EXTERNAL IGGLAT, JINDEX
C
C     -----------------------------------------------------------------|
C     Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      INTOUT = 0
      LFOUND = .FALSE.
C
C     Check if debug turned on
      CALL JDEBUG()
C
C     Ensures basic initialisation of common blocks is done
      CALL IBASINI(0)
C
      CALL INTLOG(JP_DEBUG, 'INTOUT: option = ' // HPARN ,JPQUIET)
C
C     -----------------------------------------------------------------|
C     Section 2.   Decode request
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Output grid name.
C     Note: compared to INTOUT('gridname'), rotation is supported
C
      IF( CHEQUAL( HPARN, 1, 8, 'gridname') ) THEN
        CALL INTLOG(JP_DEBUG, 'INTOUT: gridname = '//CHARV,JPQUIET)
        LFOUND = .TRUE.
C
C       Make sure we read a grid type and grid number from input
        I = MIN(8,LEN(CHARV))
        IF( 0.LT.INDEX(CHARV,CHAR(0)) ) I=MIN(I,INDEX(CHARV,CHAR(0))-1)
        IF( 0.LT.INDEX(CHARV,' '    ) ) I=MIN(I,INDEX(CHARV,' '    )-1)
        CTEMP8(1:8) = ' '
        CTEMP8 = CHARV(1:I)
        CTEMP = ' '
        ITEMP = 0
        IF(   .TRUE.   ) READ(CTEMP8,'(A1,I4)',ERR=203) CTEMP, ITEMP
  203   IF( ITEMP.EQ.0 ) READ(CTEMP8,'(A1,I3)',ERR=204) CTEMP, ITEMP
  204   IF( ITEMP.EQ.0 ) READ(CTEMP8,'(A1,I2)',ERR=205) CTEMP, ITEMP
  205   IF( ITEMP.EQ.0 ) READ(CTEMP8,'(A1,I1)',ERR=206) CTEMP, ITEMP
  206   IF( ITEMP.EQ.0 ) THEN
          PRINT *, "INTOUT: Invalid gridname (interpreting CHARV)"
          PRINT *, "  CHARV='"//CHARV//"'"
          PRINT *, "  CTEMP8='"//CTEMP8//"'"
          PRINT *, "  CTEMP='"//CTEMP//"'"
          PRINT *, "  ITEMP=", ITEMP
          IRET   = JPROUTINE + 1
          INTOUT = IRET
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Invalid gridname: '//TRIM(CTEMP8), JPQUIET)
          GOTO 900
        ENDIF
C
C       Make sure we have a valid grid type, following JGETGG
C       (possible results are [ROF], uppercase)
        IF(     CHEQUAL(CTEMP,1,1,'N') ) THEN
          CTEMP = 'R'
        ELSEIF( CHEQUAL(CTEMP,1,1,'O') ) THEN
          CTEMP = 'O'
        ELSEIF( CHEQUAL(CTEMP,1,1,'F') ) THEN
          CTEMP = 'F'
        ELSE
          IRET   = JPROUTINE + 2
          INTOUT = IRET
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Invalid gridname: '//TRIM(CTEMP8)//
     X      ' (type "'//TRIM(CTEMP)//'" not recognized)', JPQUIET)
          GOTO 900
        ENDIF
C
C       Make sure we have a valid grid number
        IF( .NOT.ISGGVALID(CTEMP,ITEMP) ) THEN
          IRET   = JPROUTINE + 2
          INTOUT = IRET
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Invalid grid: '//TRIM(CTEMP8)//
     X      ' (number not recognized)', JPQUIET)
          GOTO 900
        ENDIF
C
C       Generate output grid structure if necessary
        I = JPNOTYPE
        IF( CTEMP.EQ.'F' .AND. .NOT. LNOROTA ) I = JPGAUSSIAN
        IF( CTEMP.EQ.'F' .AND.       LNOROTA ) I = JPFGGROT
        IF( CTEMP.NE.'F' .AND. .NOT. LNOROTA ) I = JPQUASI
        IF( CTEMP.NE.'F' .AND.       LNOROTA ) I = JPQGGROT
        LCHANGE = (NOREPR .EQ.JPNOTYPE) .OR.
     X            (NOREPR .NE.I)        .OR.
     X            (NOGAUSO.NE.ITEMP)    .OR.
     X            (NOGAUSS.NE.ITEMP)    .OR.
     X            (HOGAUST.NE.CTEMP)
        IF( LCHANGE ) THEN
          CALL JGETGG( ITEMP, CTEMP, ROGAUSS, NOLPTS, IRET)
          IF( IRET .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTOUT: JGETGG fail for NOGAUSS = ',ITEMP)
            INTOUT = IRET
            GOTO 900
          ENDIF
        ENDIF
        NOGAUSO = ITEMP
        NOGAUSS = ITEMP
        HOGAUST = CTEMP
C
C       Compute first and last latlon grid points
        NOAREA(1) = INT( ROGAUSS(1) * PPMULT )
        NOAREA(2) = 0
        NOAREA(3) = INT( ROGAUSS(NOGAUSS*2) * PPMULT )
        NOAREA(4) = JP360 - JP360/NOLPTS(NOGAUSS)
        NOPCNT = 0
        DO I = 1, NOGAUSS*2
          NOPCNT = NOPCNT + NOLPTS(I)
        ENDDO
C
C       Additional global state variables changes
        LOCTAHEDRAL = CTEMP.EQ.'O'
        IF( CTEMP.EQ.'F' ) THEN
          NOREPR = JPGAUSSIAN
          IF( LNOROTA ) NOREPR = JPFGGROT
        ELSE
          NOREPR = JPQUASI
          IF( LNOROTA ) NOREPR = JPQGGROT
        ENDIF
        LNOREPR = .TRUE.
        LNOGRID = .FALSE.
        NOGRID(1) = 0
        NOGRID(2) = 0
C
        GOTO 900
      ENDIF
C
C     Output grid.
C
      IF( CHEQUAL( HPARN, 1, 4, 'grid') ) THEN
        LFOUND = .TRUE.
C       (Ignore the call if the grid values = 0.)
        IF( (REALV(1) .EQ. 0) .OR. (REALV(2) .EQ. 0) ) GOTO 900
        IF( (NOREPR.NE.JPREGULAR).AND.(NOREPR.NE.JPREGROT) ) THEN
          LCHANGE = .TRUE.
          NOGAUSS = 0
        ENDIF
        IF( LNOROTA ) THEN
          NOREPR  = JPREGROT
        ELSE
          NOREPR  = JPREGULAR
        ENDIF
        LNOREPR = .TRUE.
        CALL INTLOGR(JP_DEBUG, 'INTOUT: grid e-w = ',REALV(1))
        CALL INTLOGR(JP_DEBUG, 'INTOUT: grid n-s = ',REALV(2))
        DO I = 1, 2
          ITEMP = INT( REALV(I)*PPMULT)
          IF( NOGRID(I) .NE. ITEMP ) LCHANGE = .TRUE.
          NOGRID(I) = ITEMP
        ENDDO
        LNOGRID = .TRUE.
        GOTO 900
      ENDIF
C
C     Output regular (gaussian).
C
      IF( CHEQUAL( HPARN, 1, 7, 'regular') ) THEN
        LFOUND = .TRUE.
C       (Ignore the call if the gaussian value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
        IF( NOREPR .NE. JPGAUSSIAN ) THEN
          LCHANGE = .TRUE.
          NOGRID(1) = 0
          NOGRID(2) = 0
          LNOGRID = .FALSE.
        ENDIF
        IF( LNOROTA ) THEN
          NOREPR  = JPFGGROT
        ELSE
          NOREPR  = JPGAUSSIAN
        ENDIF
        LNOREPR = .TRUE.
        IF( NOGAUSS .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: regular = ',INTV(1))
        NOGAUSS = INTV(1)
        LNOGAUS = .TRUE.
        IF( (NOGAUSO.NE.NOGAUSS).OR.(HOGAUST.NE.'F') ) THEN
          CTEMP = 'F'
          CALL JGETGG(NOGAUSS,CTEMP,ROGAUSS,NOLPTS,IRET)
          IF( IRET .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTOUT: JGETGG fail for NOGAUSS = ',NOGAUSS)
            INTOUT = IRET
            GOTO 900
          ENDIF
          NOGAUSO = NOGAUSS
          HOGAUST = CTEMP
        ENDIF
C
        NONS = NOGAUSS*2
        GOTO 900
      ENDIF
C
C     Output truncation.
C
      IF( CHEQUAL( HPARN, 1, 10, 'truncation') ) THEN
        LFOUND = .TRUE.
C       (Ignore the call if the truncation value < 0.)
        IF( INTV(1) .LT. 0 ) GOTO 900
        IF( NOREPR .NE. JPSPHERE ) LCHANGE = .TRUE.
        IF( .NOT. LNOREPR ) NOREPR  = JPSPHERE
        LNOREPR = .TRUE.
        IF( NORESO .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: truncation = ',INTV(1))
        NORESO  = INTV(1)
        LNORESO = .TRUE.
        GOTO 900
      ENDIF
C
C
C
C     Output gaussian => same output type as input.
C     Double Interpolation
C
      IF( CHEQUAL( HPARN, 1, 20, 'intermediate_gaussian') ) THEN
        LFOUND = .TRUE.
C       (Ignore the call if the gaussian value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
        IF( NOMEGA .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTOUT: Intermediate gaussian = ',INTV(1))
        NOMEGA = INTV(1)
        LDOUBLE = .TRUE.
c        NOREPR = JPNOTYPE
c        LNOREPR = .TRUE.
c        NONS = NOGAUSS*2
        GOTO 900
      ENDIF
C
C     Interpolation method
C
      IF( CHEQUAL( HPARN, 1, 13, 'interpolation') ) THEN
        LFOUND = .TRUE.
        LNNLSM_CY41R1 = .FALSE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: interpolation = '
     X   // CHARV ,JPQUIET)
        IF( CHEQUAL( CHARV, 1, 17, 'nearest neighbour') ) THEN
          IF( .NOT. LMETHOD ) LCHANGE = .TRUE.
          LMETHOD = .TRUE.
        ELSEIF( CHEQUAL( CHARV, 1, 8, 'bilinear') ) THEN
          IF( .NOT. LO12PT ) LCHANGE = .TRUE.
          LO12PT = .FALSE.
        ELSEIF( CHEQUAL( CHARV, 1, 11, 'nearest lsm') ) THEN
          IF( .NOT. LNNLSM ) LCHANGE = .TRUE.
          LNNLSM        = .TRUE.
          LNNLSM_CY41R1 = .TRUE.
        ELSEIF( CHEQUAL( CHARV, 1, 3, 'off') ) THEN
          LNNLSM  = .FALSE.
          LMETHOD = .FALSE.
        ELSE
          IF( LMETHOD ) LCHANGE = .TRUE.
          LMETHOD = .FALSE.
          LO12PT  = .TRUE.
          LNNLSM  = .FALSE.
        ENDIF
        GOTO 900
      ENDIF
C

C     Output ito be generated by automatically selected truncation
C     (autoresol).
C
      IF( CHEQUAL( HPARN, 1, 9, 'autoresol') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: autoresol (1 = on) = ',INTV(1))
        IF( INTV(1).EQ.1 ) THEN
          LARESOL = .TRUE.
        ELSE
          LARESOL = .FALSE.
        ENDIF
      ENDIF
C
C     Output data format.
C
      IF( CHEQUAL( HPARN, 1, 4, 'form') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: form = ' // CHARV ,JPQUIET)
        IF( CHEQUAL( CHARV, 1, 4, 'grib') ) THEN
          IF( NOFORM .EQ. 0 ) LCHANGE = .TRUE.
          NOFORM = 1
        ELSE
          IF( NOFORM .EQ. 1 ) LCHANGE = .TRUE.
          NOFORM = 0
        ENDIF
        GOTO 900
      ENDIF
C
C     Input level.
C
      IF( CHEQUAL( HPARN, 1, 5, 'level') ) THEN
        LFOUND = .TRUE.
        IF( NOLEVEL .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: level = ',INTV(1))
        NOLEVEL = INTV(1)
        LNOLEVL = .TRUE.
        GOTO 900
      ENDIF
C
C     Input level type.
C
      IF( CHEQUAL( HPARN, 1, 7, 'levtype') ) THEN
        LFOUND = .TRUE.
        IF( NOLEVT .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: levtype = ',INTV(1))
        NOLEVT  = INTV(1)
        LNOLEVT = .TRUE.
        GOTO 900
      ENDIF
C
C     Input table.
C
      IF( CHEQUAL( HPARN, 1, 5, 'table') ) THEN
        LFOUND = .TRUE.
        IF( NOTABLE .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: table = ',INTV(1))
        NOTABLE = INTV(1)
        LNOTABL = .TRUE.
        GOTO 900
      ENDIF
C
C     Input parameter.
C
      IF( CHEQUAL( HPARN, 1, 9, 'parameter') ) THEN
        LFOUND = .TRUE.
        IF( NOPARAM .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: parameter = ',INTV(1))
        NOPARAM = INTV(1)
        LNOPARA = .TRUE.
        GOTO 900
      ENDIF
C
C     Output accuracy.
C
      IF( CHEQUAL( HPARN, 1, 8, 'accuracy') ) THEN
        LFOUND = .TRUE.
C       (Ignore the call if the truncation value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
        IF( NOACC .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: accuracy = ',INTV(1))
        NOACC  = INTV(1)
        LNOACC = .TRUE.
        GOTO 900
      ENDIF
C
C     Output gaussian => same output type as input.
C
      IF( CHEQUAL( HPARN, 1, 8, 'gaussian') ) THEN
        LFOUND = .TRUE.
C       (Ignore the call if the gaussian value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
        IF( NOGAUSS .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: gaussian = ',INTV(1))
        NOGAUSS = INTV(1)
        LNOGAUS = .TRUE.
        NOREPR = JPNOTYPE
        LNOREPR = .TRUE.
        NONS = NOGAUSS*2
        GOTO 900
      ENDIF
C
C     Output area.
C
      IF( CHEQUAL( HPARN, 1, 4, 'area') ) THEN
        LFOUND = .TRUE.
        CALL INTLOGR(JP_DEBUG, 'INTOUT: north = ',REALV(1))
        CALL INTLOGR(JP_DEBUG, 'INTOUT: west  = ',REALV(2))
        CALL INTLOGR(JP_DEBUG, 'INTOUT: south = ',REALV(3))
        CALL INTLOGR(JP_DEBUG, 'INTOUT: east  = ',REALV(4))
        DO I = 1, 4
          ITEMP = INT( REALV(I)*PPMULT)
          IF( NOAREA(I) .NE. ITEMP ) LCHANGE = .TRUE.
          NOAREA(I) = ITEMP
        ENDDO
        LNOAREA = (NOAREA(1).NE.0).OR.(NOAREA(2).NE.0).OR.
     X            (NOAREA(3).NE.0).OR.(NOAREA(4).NE.0)
        GOTO 900
      ENDIF
C
C     Output scan.
C
      IF( CHEQUAL( HPARN, 1, 4, 'scan') ) THEN
        LFOUND = .TRUE.
        IF( NOSCNM .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: scan = ',INTV(1))
        NOSCNM = INTV(1)
        GOTO 900
      ENDIF
C
C     Output packing.
C
C     Values for PACKING
C       SIMPLE
C       COMPLEX
C       SECOND ORDER
C       ARCHIVED VALUE
C
      IF( CHEQUAL( HPARN, 1, 7, 'packing') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: packing = ' // CHARV ,JPQUIET)
        IF( CHEQUAL( CHARV, 1, 6, 'second') ) THEN
          IF( NOHFUNC.NE.'K' ) LCHANGE = .TRUE.
          NOHFUNC = 'K'
        ELSE IF( CHEQUAL( CHARV, 1, 6, 'simple') ) THEN
          IF( NOHFUNC.NE.'S' ) LCHANGE = .TRUE.
          NOHFUNC = 'S'
        ELSE IF( CHEQUAL( CHARV, 1, 7, 'archive') ) THEN
          IF( NOHFUNC.NE.'A' ) LCHANGE = .TRUE.
          NOHFUNC = 'A'
        ELSE IF( CHEQUAL( CHARV, 1, 7, 'complex') ) THEN
          IF( NOHFUNC.NE.'C' ) LCHANGE = .TRUE.
          NOHFUNC = 'C'
        ELSE IF( CHEQUAL( CHARV, 1, 4, 'jpeg') ) THEN
          IF( NOHFUNC.NE.'J' ) LCHANGE = .TRUE.
          NOHFUNC = 'J'
        ELSE
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Invalid value for PACKING: ' // CHARV(1:6),JPQUIET)
          LFOUND = .FALSE.
        ENDIF
        GOTO 900
      ENDIF
C
C     Output reduced.
C
      IF( CHEQUAL( HPARN, 1,  7, 'reduced') ) THEN
        LFOUND = .TRUE.
C
C       (Ignore the call if the reduced value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
C
C       (Check for changes to internal interp. request)
        LCHANGE = ((NOGAUSS.NE.INTV(1)) .OR. LOCTAHEDRAL )
        LOCTAHEDRAL = .FALSE.
C
        CALL INTLOG(JP_DEBUG, 'INTOUT: '//TRIM(HPARN)//' = ',INTV(1))
        NOGAUSS = INTV(1)
        LNOGAUS = .TRUE.
        IF( NOREPR .NE. JPQUASI ) THEN
          LCHANGE = .TRUE.
          NOGRID(1) = 0
          NOGRID(2) = 0
          LNOGRID = .FALSE.
        ENDIF
        IF( LNOROTA ) THEN
          NOREPR  = JPQGGROT
        ELSE
          NOREPR  = JPQUASI
        ENDIF
        LNOREPR = .TRUE.
        IF( (NOGAUSO.NE.NOGAUSS) .OR. (HOGAUST.EQ.'O')
     X      .OR. ((HOGAUST.NE.'R') .AND. (HOGAUST.NE.'O')) ) THEN
          CTEMP = 'R'
          CALL JGETGG(NOGAUSS,CTEMP,ROGAUSS,NOLPTS,IRET)
          IF( IRET .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTOUT: JGETGG fail for NOGAUSS = ',NOGAUSS)
            INTOUT = IRET
            GOTO 900
          ENDIF
          NOGAUSO = NOGAUSS
          HOGAUST = CTEMP
        ENDIF
C
        NOAREA(1) = INT( ROGAUSS(1) * PPMULT )
        NOAREA(2) = 0
        NOAREA(3) = INT( ROGAUSS(NOGAUSS*2) * PPMULT )
        NOAREA(4) = JP360 - JP360/NOLPTS(NOGAUSS)
C
        NOPCNT = 0
        DO I = 1, NOGAUSS*2
          NOPCNT = NOPCNT + NOLPTS(I)
        ENDDO
C
        GOTO 900
      ENDIF
C
C     Output user_regular_gaussian
C
      IF( CHEQUAL( HPARN, 1, 21, 'user_regular_gaussian') ) THEN
        LFOUND = .TRUE.
        IF( NOREPR .NE. JPGAUSSIAN ) THEN
          LCHANGE = .TRUE.
          NOGRID(1) = 0
          NOGRID(2) = 0
          LNOGRID = .FALSE.
        ENDIF
C       (Ignore the call if the user_regular_gaussian value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
        IF( NOGAUSS .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTOUT: user_regular_gaussian = ',INTV(1))
        NOGAUSS = INTV(1)
        LNOGAUS = .TRUE.
        IF( LNOROTA ) THEN
          NOREPR  = JPFGGROT
        ELSE
          NOREPR  = JPGAUSSIAN
        ENDIF
        LNOREPR = .TRUE.
C
        NONS = NOGAUSS*2
        NOLPTS(1:NOGAUSS*2) = NOGAUSS*4
        NOGAUSO = NOGAUSS
        HOGAUST = 'U'
        LOGIVEP = .TRUE.
        LOCTAHEDRAL = .FALSE.
C
        GOTO 900
      ENDIF
C
C     Output user_reduced_gaussian
C
      IF( CHEQUAL( HPARN, 1, 21, 'user_reduced_gaussian') ) THEN
        LFOUND = .TRUE.
        IF( NOREPR .NE. JPQUASI ) THEN
          LCHANGE = .TRUE.
          NOGRID(1) = 0
          NOGRID(2) = 0
          LNOGRID = .FALSE.
        ENDIF
C       (Ignore the call if the user_reduced_gaussian value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
        IF( NOGAUSS .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTOUT: user_reduced_gaussian = ',INTV(1))
        NOGAUSS = INTV(1)
        LNOGAUS = .TRUE.
        IF( LNOROTA ) THEN
          NOREPR  = JPQGGROT
        ELSE
          NOREPR  = JPQUASI
        ENDIF
        LNOREPR = .TRUE.
        GOTO 900
      ENDIF
C
C     Output g_pnts.
C
      IF( CHEQUAL( HPARN, 1, 6, 'g_pnts') ) THEN
        LFOUND = .TRUE.
        IF( NOGAUSS .LE. 0 ) THEN
          IRET   = JPROUTINE + 1
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Impossible number of gaussian latitudes.',NOGAUSS)
          INTOUT = IRET
          GOTO 900
        ENDIF
C
        DO I = 1, NOGAUSS
          NOLPTS(I)                 = INTV(I)
          NOLPTS(NOGAUSS*2 + 1 - I) = INTV(I)
        ENDDO
C
        NOPCNT = 0
        DO I = 1, NOGAUSS*2
          NOPCNT = NOPCNT + NOLPTS(I)
        ENDDO
        NOGAUSO = NOGAUSS
        HOGAUST = 'U'
        LOGIVEP = .TRUE.
        LOCTAHEDRAL = ISOCTAHEDRAL(NOGAUSS,NOLPTS)
        GOTO 900
      ENDIF
C
C     Output g_lats.
C
      IF( CHEQUAL( HPARN, 1, 6, 'g_lats') ) THEN
        LFOUND = .TRUE.
        IF( NOGAUSS .LE. 0 ) THEN
          IRET   = JPROUTINE + 1
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Impossible number of gaussian latitudes.',NOGAUSS)
          INTOUT = IRET
          GOTO 900
        ENDIF
C
        DO I = 1, NOGAUSS
          ROGAUSS(I)                 =  REALV(I)
          ROGAUSS(NOGAUSS*2 + 1 - I) = -REALV(I)
        ENDDO
C
        NOAREA(1) = INT( ROGAUSS(1) * PPMULT )
        NOAREA(2) = 0
        NOAREA(3) = INT( ROGAUSS(NOGAUSS*2) * PPMULT )
        NOAREA(4) = JP360 - JP360/NOLPTS(NOGAUSS)
        NOGAUSO = NOGAUSS
        HOGAUST = 'U'
        LOGIVEL = .TRUE.
        GOTO 900
      ENDIF
C
C     Output rotation.
C
      IF( CHEQUAL( HPARN, 1, 8, 'rotation') ) THEN
        LFOUND = .TRUE.
C
        IF( CHEQUAL(CHARV,1,3,'off') ) THEN
          LNOROTA = .FALSE.
          NOROTA(1) = -99999999
          NOROTA(2) = -99999999
          IF( NOREPR.EQ.JPSPHROT ) NOREPR = JPSPHERE
          IF( NOREPR.EQ.JPREGROT ) NOREPR = JPREGULAR
          IF( NOREPR.EQ.JPQGGROT ) NOREPR = JPQUASI
          IF( NOREPR.EQ.JPFGGROT ) NOREPR = JPGAUSSIAN
        ELSE
          LNOROTA = .TRUE.
          CALL INTLOGR(JP_DEBUG, 'INTOUT: S.pole latitude  = ',REALV(1))
          CALL INTLOGR(JP_DEBUG, 'INTOUT: S.pole longitude = ',REALV(2))
          DO I = 1, 2
            ITEMP = INT( REALV(I)*PPMULT)
            IF( NOROTA(I) .NE. ITEMP ) LCHANGE = .TRUE.
            NOROTA(I) = ITEMP
          ENDDO
          IF( NOREPR.EQ.JPSPHERE )   NOREPR = JPSPHROT
          IF( NOREPR.EQ.JPREGULAR )  NOREPR = JPREGROT
          IF( NOREPR.EQ.JPQUASI )    NOREPR = JPQGGROT
          IF( NOREPR.EQ.JPGAUSSIAN ) NOREPR = JPFGGROT
        ENDIF
        GOTO 900
      ENDIF
C
C     Interpolation scheme to use for rotations
C
      IF( CHEQUAL( HPARN, 1, 8, '12-point') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: 12-point = ' // CHARV ,JPQUIET)
C
        IF( CHEQUAL(CHARV,1,3,'yes') ) THEN
          LO12PT = .TRUE.
        ELSE
          LO12PT = .FALSE.
        ENDIF
        GOTO 900
      ENDIF
C
C     Output user_reduced_latlon
C
      IF( CHEQUAL( HPARN, 1, 19, 'user_reduced_latlon') ) THEN
        LFOUND = .TRUE.
        IF( NOREPR .NE. JPREDLL ) THEN
          LCHANGE = .TRUE.
          NOGRID(1) = 0
Cjdc      NOGRID(2) = REALV(2)
          LNOGRID = .FALSE.
        ENDIF
C       (Ignore the call if the user_reduced_latlon value = 0.)
        IF( INTV(1) .EQ. 0 ) GOTO 900
        IF( NOREDLL .NE. INTV(1) ) LCHANGE = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: user_reduced_latlon = ',INTV(1))
        NOREDLL = INTV(1)
        LNOREDL = .TRUE.
        IF( LNOROTA ) THEN
          NOREPR  = JPREDLROT
        ELSE
          NOREPR  = JPREDLL
        ENDIF
        LNOREPR = .TRUE.
        GOTO 900
      ENDIF
C
C     Output set raduced_laltlon
C
      IF( CHEQUAL( HPARN, 1, 10, 'red_latlon') ) THEN
        LFOUND = .TRUE.
          NOGRID(1) = 0
          NOGRID(2) = REALV(2)
        IF( NOREPR .NE. JPREDLL ) THEN
          LCHANGE = .TRUE.
          LNOGRID = .FALSE.
        ENDIF
C       (Ignore the call if the user_reduced_latlon value = 0.)
cs        IF( INTV(1) .EQ. 0 ) GOTO 900
        CALL INTLOG(JP_DEBUG, 'INTOUT: reduced_latlon ns = ',NOGRID(2))
cs        NOREDLL = INTV(1)
        LNOREDL = .TRUE.
        IF( LNOROTA ) THEN
          NOREPR  = JPREDLROT
        ELSE
          NOREPR  = JPREDLL
        ENDIF
        LNOREPR = .TRUE.
        GOTO 900
      ENDIF
C
C     Output ll_pnts.
C
      IF( CHEQUAL( HPARN, 1, 7, 'll_pnts') ) THEN
        LFOUND = .TRUE.
        IF( NOREDLL .LE. 0 ) THEN
          IRET   = JPROUTINE + 1
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Impossible number of latitudes.',NOREDLL)
          INTOUT = IRET
          GOTO 900
        ENDIF
C
        NOLPTS(1:NOREDLL) = INTV(1:NOREDLL)
C
        NOPCNT = 0
        DO I = 1, NOREDLL-1
          NOPCNT = NOPCNT + NOLPTS(I)
        ENDDO
        LOGIVEP = .TRUE.
        GOTO 900
      ENDIF
C
C     Output ll_lats.
C
      IF( CHEQUAL( HPARN, 1, 7, 'll_lats') ) THEN
        LFOUND = .TRUE.
        IF( NOREDLL .LE. 0 ) THEN
          IRET   = JPROUTINE + 1
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Impossible number of latitudes.',NOREDLL)
          INTOUT = IRET
          GOTO 900
        ENDIF
C
        ROREDLL(1:NOREDLL) = REALV(1:NOREDLL)
C
        NOAREA(1) = INT( ROREDLL(1) * PPMULT )
        NOAREA(2) = 0
        NOAREA(3) = INT( ROREDLL(NOREDLL) * PPMULT )
        NOAREA(4) = JP360 - JP360/NOLPTS(NOREDLL/2)
        LOGIVEL   = .TRUE.
        GOTO 900
      ENDIF
C
C     Reduced gaussian definition from file
C
      IF( CHEQUAL( HPARN, 1, 15, 'gauss_from_file') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG,
     X    'INTOUT: gauss_from_file = ' // CHARV ,JPQUIET)
C
C       Open the input file
C
        CALL PBOPEN(IFILE1, CHARV, 'R', IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Unable to use gaussian definition file:',JPQUIET)
          CALL INTLOG(JP_ERROR,CHARV,JPQUIET)
          INTOUT = JPROUTINE + 1
          GOTO 900
        ENDIF
C
C       Read gaussian number, then number of points at each latitude
C
        CALL PBREAD(IFILE1, NOGAUSS, JPBYTES, IRET)
        IF( IRET.NE.JPBYTES ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Problem reading gaussian no. from file:',JPQUIET)
          CALL PBCLOSE(IFILE1, IRET)
          INTOUT = JPROUTINE + 1
          GOTO 900
        ENDIF
C
        IF( NOGAUSS .LE. 0 ) THEN
          IRET   = JPROUTINE + 1
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Impossible number of gaussian latitudes.',NOGAUSS)
          INTOUT = IRET
          GOTO 900
        ENDIF
C
        CALL PBREAD(IFILE1, NOLPTS, JPBYTES*NOGAUSS, IRET)
        IF( IRET.NE.JPBYTES*NOGAUSS ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Problem reading gaussian pts from file:',JPQUIET)
          CALL PBCLOSE(IFILE1, IRET)
          INTOUT = JPROUTINE + 1
          GOTO 900
        ENDIF
C
        CALL PBCLOSE(IFILE1, IRET)
C
        DO I = 1, NOGAUSS
          NOLPTS(NOGAUSS*2 + 1 - I) = NOLPTS(I)
        ENDDO
C
        NOPCNT = 0
        DO I = 1, NOGAUSS*2
          NOPCNT = NOPCNT + NOLPTS(I)
        ENDDO
        NOGAUSO = NOGAUSS
        HOGAUST = 'U'
        LOGIVEP = .TRUE.
        LOGIVEL = .TRUE.
        LOCTAHEDRAL = ISOCTAHEDRAL(NOGAUSS,NOLPTS)
C
        IRET = IGGLAT(NOGAUSS*2, ROGAUSS, 0, 1)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: Problem calculating gaussian latitudes',JPQUIET)
          INTOUT = JPROUTINE + 1
          GOTO 900
        ENDIF
C
        IF( LNOROTA ) THEN
          NOREPR = JPQGGROT
        ELSE
          NOREPR = JPQUASI
        ENDIF
        LNOREPR = .TRUE.
C
        GOTO 900
      ENDIF
C
C     Output grid creation style
C
      IF( CHEQUAL( HPARN, 1, 5, 'style') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: style = ' // CHARV ,JPQUIET)
        IF( CHEQUAL( CHARV, 1, 13, 'dissemination') ) THEN
          ITEMP = JPSDISM
        ELSE
          ITEMP = JPSMARS
        ENDIF
        IF( NOSTYLE .NE. ITEMP ) LCHANGE = .TRUE.
        NOSTYLE = ITEMP
        LNOSTYLE = .TRUE.
        GOTO 900
      ENDIF
C
C     Output grid frame
C
      IF( CHEQUAL( HPARN, 1, 5, 'frame') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: frame = ',INTV(1))
        ITEMP = INTV(1)
        IF( NOFRAME .NE. ITEMP ) LCHANGE = .TRUE.
        NOFRAME = ITEMP
        LNOFRAME = (NOFRAME.GT.0)
        GOTO 900
      ENDIF
C
C     Output grid bitmap
C
      IF( CHEQUAL( HPARN, 1, 6, 'bitmap') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: bitmap = ' // CHARV ,JPQUIET)
        ITEMP  = JINDEX(CHARV)
        IF( ITEMP.LT.1 ) THEN
          CALL INTLOG(JP_WARN,
     X      'INTOUT: No bitmap filename given.',JPQUIET)
          INTOUT = JPROUTINE + 1
          GOTO 900
        ELSE
          IF( NOBITMP(1:ITEMP).NE.CHARV(1:ITEMP) ) LCHANGE = .TRUE.
          NOBITMP = ' '
          NOBITMP(1:ITEMP) = CHARV(1:ITEMP)
          LNOBITMP = .TRUE.
        ENDIF
        GOTO 900
      ENDIF
C
C     Output gaussian grid specification
C
      IF( CHEQUAL( HPARN, 1, 13, 'specification') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTOUT: specification = ', INTV(1))
        IF( (INTV(1).NE.12).AND.(INTV(1).NE.18) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTOUT: invalid value for specification = ',INTV(1))
          INTOUT = JPROUTINE + 2
          GOTO 900
        ENDIF
C
        IF( NGSPEC.NE.INTV(1) ) THEN
          LCHANGE = .TRUE.
          NGSPEC  = INTV(1)
          LNGSPEC = .TRUE.
        ENDIF
        IF( LNOGAUS ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'INTOUT: replace gaussian specification = ', NOGAUSS)
          CTEMP = 'R'
          CALL JGETGG(NOGAUSS,CTEMP,ROGAUSS,NOLPTS,IRET)
          IF( IRET .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTOUT: JGETGG fail for NOGAUSS = ',NOGAUSS)
            INTOUT = IRET
            GOTO 900
          ENDIF
          NOGAUSO = NOGAUSS
          HOGAUST = CTEMP
C
          IF( .NOT.LNOAREA ) THEN
            CALL INTLOG(JP_DEBUG,
     X        'INTOUT: now replace output area definition',JPQUIET)
            NOAREA(1) = INT( ROGAUSS(1) * PPMULT )
            NOAREA(2) = 0
            NOAREA(3) = INT( ROGAUSS(NOGAUSS*2) * PPMULT )
            NOAREA(4) = JP360 - JP360/NOLPTS(NOGAUSS)
            NOPCNT = 0
            DO I = 1, NOGAUSS*2
              NOPCNT = NOPCNT + NOLPTS(I)
            ENDDO
            GOTO 900
          ENDIF
        ENDIF
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
      IF( .NOT. LFOUND ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTOUT: Unknown keyword or option used:', JPQUIET)
        CALL INTLOG(JP_ERROR, HPARN, JPQUIET)
        INTOUT = JPROUTINE + 2
      ENDIF
C
C     Set flag to show function has been called
      LINTOUT = .TRUE.
      RETURN
      END
