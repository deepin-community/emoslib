C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTIN( HPARN, INTV, REALV, CHARV)
C
C---->
C**** INTIN
C
C     Purpose
C     -------
C
C     Change input field parameters in common block.
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
C     JMALLOC  - Dynamically allocate memory
C     JFREE    - Free dynamically allocated memory
C     GETENV   - Get value of an environment variable
C
C
C     Author
C     ------
C
C     J.D.Chambers       ECMWF        August 1994.
C
C----<
C
C     IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 28100)
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
#include "nifld.extracommon"
#include "nofld.common"
#include "grfixed.h"
#include "current.h"
#include "intf.h"
C
C     Local variables
C
      INTEGER ITEMP, I, IRET, IGSIZE, IASIZE, IBLANK
      INTEGER NEWGRIB(1), OLDGRIB(1)
      POINTER( NIPNGRB, NEWGRIB )
      POINTER( NIPOGRB, OLDGRIB )
      LOGICAL LFOUND
      CHARACTER*256 OLDLSM, ISLDEFL
      CHARACTER*1 CTEMP
      CHARACTER*8 CTEMP8
C
C     Externals
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      LOGICAL CHEQUAL, ISGGVALID
      EXTERNAL CHEQUAL, ISGGVALID, JMALLOC
C
      DATA ISLDEFL/'/home/ma/emos/data/islscp_old_lsm'/
      SAVE ISLDEFL
C
C     -----------------------------------------------------------------|
C     Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      INTIN = 0
      LFOUND = .FALSE.
C
C     Check if debug turned on
      CALL JDEBUG()
C
C     Ensures basic initialisation of common blocks is done
      CALL IBASINI(0)
C
      CALL INTLOG(JP_DEBUG, 'INTIN: option = ' // HPARN ,JPQUIET)
C
C     -----------------------------------------------------------------|
C     Section 2.   Decode request
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Input grid name.
C     Note: compared to INTIN('gridname'), no rotation is supported
C
      IF( CHEQUAL( HPARN, 1, 8, 'gridname') ) THEN
        CALL INTLOG(JP_DEBUG, 'INTIN: gridname = '//CHARV,JPQUIET)
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
          PRINT *, "INTIN: Invalid gridname (interpreting CHARV)"
          PRINT *, "  CHARV='"//CHARV//"'"
          PRINT *, "  CTEMP8='"//CTEMP8//"'"
          PRINT *, "  CTEMP='"//CTEMP//"'"
          PRINT *, "  ITEMP=", ITEMP
          IRET   = JPROUTINE + 1
          INTIN  = IRET
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Invalid gridname: '//TRIM(CTEMP8), JPQUIET)
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
          INTIN  = IRET
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Invalid gridname: '//TRIM(CTEMP8)//
     X      ' (type "'//TRIM(CTEMP)//'" not recognized)', JPQUIET)
          GOTO 900
        ENDIF
C
C       Make sure we have a valid grid number
        IF( .NOT.ISGGVALID(CTEMP,ITEMP) ) THEN
          IRET   = JPROUTINE + 2
          INTIN  = IRET
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Invalid grid: '//TRIM(CTEMP8)//
     X      ' (number not recognized)', JPQUIET)
          GOTO 900
        ENDIF
C
C       Generate output grid structure if necessary
        I = JPQUASI
        IF( CTEMP.EQ.'F' ) I = JPGAUSSIAN
        LCHANGE = (NIREPR .EQ.JPNOTYPE) .OR.
     X            (NIREPR .NE.I)        .OR.
     X            (NIGAUSO.NE.ITEMP)    .OR.
     X            (NIGAUSS.NE.ITEMP)    .OR.
     X            (HIGAUST.NE.CTEMP)
        IF( LCHANGE ) THEN
          CALL JGETGG( ITEMP, CTEMP, RIGAUSS, MILLEN, IRET)
          IF( IRET .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTIN: JGETGG fail for NIGAUSS = ',ITEMP)
            INTIN = IRET
            GOTO 900
          ENDIF
        ENDIF
        NIGAUSO = ITEMP
        NIGAUSS = ITEMP
        HIGAUST = CTEMP
C
C       Compute first and last latlon grid points
        NIAREA(1) = INT( RIGAUSS(1) * PPMULT )
        NIAREA(2) = 0
        NIAREA(3) = INT( RIGAUSS(NIGAUSS*2) * PPMULT )
        NIAREA(4) = JP360 - JP360/MILLEN(NIGAUSS)
        NINS = NIGAUSS*2
        NIPCNT = 0
        DO I= 1, NIGAUSS*2
          NIPCNT = NIPCNT + MILLEN(I)
        ENDDO
C
C       Additional global state variables changes
        NIREPR = JPQUASI
        IF( CTEMP.EQ.'F' ) NIREPR = JPGAUSSIAN
        NIGRID(1:2) = 0
C
        GOTO 900
      ENDIF
C
C     Request ISLSCP processing
C
      IF( CHEQUAL( HPARN, 1, 6, 'islscp') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: islscp = ' // CHARV ,JPQUIET)
C
        IF( CHEQUAL( CHARV, 1, 3, 'off' ) ) THEN
          IF( LNIISCL ) THEN
            CALL JFREE(NIPOGRB)
            CALL JFREE(NIPNGRB)
            NIPOGRS = 0
            NIPNGRS = 0
          ENDIF
          LNIISCL = .FALSE.
          GOTO 900
        ENDIF
C
        LNIISCL = .TRUE.
C
C       Pick up the old land-sea mask GRIB product
C
        CALL GETENV('ISLSCP_OLD_LSM',OLDLSM)
        IBLANK = INDEX(OLDLSM, ' ')
        IF( IBLANK.LT.2 ) THEN
          IBLANK = INDEX(ISLDEFL, ' ') - 1
          OLDLSM(1:IBLANK) = ISLDEFL(1:IBLANK)
          IF( INTV(1).GT.0 ) THEN
            OLDLSM(IBLANK+1:IBLANK+2) = '_n'
            WRITE(CTEMP8,'(I3.3)') INTV(1)
            OLDLSM(IBLANK+3:IBLANK+5) = CTEMP8(1:3)
            IBLANK = IBLANK + 5
          ENDIF
        ENDIF
        CALL INTLOG(JP_DEBUG,
     X    'INTIN: old land-sea mask file name:',JPQUIET)
        CALL INTLOG(JP_DEBUG,'INTIN: ' // OLDLSM(1:IBLANK),JPQUIET)
C
        CALL PBOPEN(NISLSCP,OLDLSM,'r',IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Problem processing islscp option',JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTIN: PBOPEN fail for old land-sea mask file',IRET)
          CALL INTLOG(JP_ERROR,'INTIN: ' // OLDLSM(1:IBLANK),JPQUIET)
          INTIN = IRET
          GOTO 900
        ENDIF
C
        CALL PBSIZE(NISLSCP,IGSIZE)
        IF( IGSIZE.LE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Problem processing islscp option',JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTIN: PBSIZE fail new land-sea mask GRIB product',IGSIZE)
          INTIN = IGSIZE
          GOTO 900
        ENDIF
C
        IF( NIPOGRS.LT.IGSIZE ) THEN
          IF( NIPOGRB.NE.0 ) CALL JFREE(NIPOGRB)
          NIPOGRS = IGSIZE
          NIPOGRB = JMALLOC(NIPOGRS)
#ifdef hpR64
          NIPOGRB = NIPOGRB/(1024*1024*1024*4)
#endif
          IF( NIPOGRB.EQ.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTIN: Problem processing islscp option',JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        'INTIN: JMALLOC fail old land-sea mask GRIB',JPQUIET)
            INTIN = JPROUTINE + 2
            GOTO 900
          ENDIF
        ENDIF
C
        CALL PBGRIB(NISLSCP, OLDGRIB, IGSIZE, IASIZE, IRET)
        IF( (IRET.NE.0).OR.(IGSIZE.NE.IASIZE) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Problem processing islscp option',JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTIN: PBREAD fail old land-sea mask GRIB',IRET)
          INTIN = IRET
          GOTO 900
        ENDIF
C
        CALL PBCLOSE(NISLSCP, IRET)
C
C       Pick up the new land-sea mask GRIB product
C
        IBLANK = INDEX(CHARV, ' ') - 1
        CALL INTLOG(JP_DEBUG,
     X    'INTIN: new land-sea mask file name:',JPQUIET)
        CALL INTLOG(JP_DEBUG,'INTIN: ' // CHARV,JPQUIET)
C
        CALL PBOPEN(NISLSCP,CHARV,'r',IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Problem processing islscp option',JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTIN: PBOPEN fail for new land-sea mask file',IRET)
          CALL INTLOG(JP_ERROR,'INTIN: ' // CHARV,JPQUIET)
          INTIN = IRET
          GOTO 900
        ENDIF
C
        CALL PBSIZE(NISLSCP,IGSIZE)
        IF( IGSIZE.LE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Problem processing islscp option',JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTIN: PBSIZE fail new land-sea mask GRIB product',IGSIZE)
          INTIN = IGSIZE
          GOTO 900
        ENDIF
C
        IF( NIPNGRS.LT.IGSIZE ) THEN
          IF( NIPNGRB.NE.0 ) CALL JFREE(NIPNGRB)
          NIPNGRS = IGSIZE
          NIPNGRB = JMALLOC(NIPNGRS)
#ifdef hpR64
          NIPNGRB = NIPNGRB/(1024*1024*1024*4)
#endif
          IF( NIPNGRB.EQ.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTIN: Problem processing islscp option',JPQUIET)
            CALL INTLOG(JP_ERROR,
     X        'INTIN: JMALLOC fail new land-sea mask GRIB',JPQUIET)
            INTIN = JPROUTINE + 2
            GOTO 900
          ENDIF
        ENDIF
C
        CALL PBGRIB(NISLSCP, NEWGRIB, IGSIZE, IASIZE, IRET)
        IF( (IRET.NE.0).OR.(IGSIZE.NE.IASIZE) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Problem processing islscp option',JPQUIET)
          CALL INTLOG(JP_ERROR,
     X      'INTIN: PBREAD fail new land-sea mask GRIB',IRET)
          INTIN = IRET
          GOTO 900
        ENDIF
C
        CALL PBCLOSE(NISLSCP, IRET)
        GOTO 900
C
      ENDIF
C
C     Input or reset value used for missing points in bitmapped field.
C
      IF( CHEQUAL( HPARN, 1, 12, 'missingvalue') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTIN: missingvalue = ' // CHARV ,JPQUIET)
        IF( CHEQUAL( CHARV, 1, 2, 'no') ) THEN
          IF( LIMISSV ) LCHANGE = .TRUE.
          LIMISSV = .FALSE.
          RMISSGV   = -55555.0
        ELSE
          IF( .NOT.LIMISSV ) LCHANGE = .TRUE.
          LIMISSV = .TRUE.
          RMISSGV = REALV(1)
        ENDIF
        CALL INTLOGR(JP_DEBUG,'INTIN: missingvalue = ',RMISSGV)
        GOTO 900
      ENDIF

C     missingval because can not be applied in Emos lib everywhere...
C
      IF( CHEQUAL( HPARN, 1, 10, 'missingval') ) THEN
        LFOUND = .TRUE.
        LIMISSV = .FALSE.
        CALL INTLOG(JP_DEBUG,'INTIN: missingval = ' // CHARV ,JPQUIET)
        IF( CHEQUAL( CHARV, 1, 2, 'no') ) THEN
          IF( LIMISSV ) LCHANGE = .TRUE.
          RMISSGV   = -55555.0
          LIMISSA = .FALSE.
        ELSE
          IF( .NOT.LIMISSV ) LCHANGE = .TRUE.
          LIMISSA = .TRUE.
          RMISSGV = REALV(1)
        ENDIF
        CALL INTLOGR(JP_DEBUG,'INTIN: missingval = ',RMISSGV)
        ISEC1(5) = 192
        GOTO 900
      ENDIF
C
C     Input uselsm.
C
      IF ( CHEQUAL( HPARN, 1, 6, 'uselsm') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTIN: uselsm = ' // CHARV ,JPQUIET)
        IF ( CHEQUAL( CHARV, 1, 2, 'no') ) THEN
          IF ( LSM ) THEN
            LCHANGE = .TRUE.
            LSMCHNG = .TRUE.
          ENDIF
          LSM = .FALSE.
        ELSE
          IF ( .NOT. LSM ) THEN
            LCHANGE = .TRUE.
            LSMCHNG = .TRUE.
          ENDIF
          LSM = .TRUE.
        ENDIF
        LSMSET = .TRUE.
        GOTO 900
      ENDIF
C
C     Input useprecip.
C
      IF ( CHEQUAL( HPARN, 1, 9, 'useprecip') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTIN: useprecip = ' // CHARV ,JPQUIET)
        IF ( CHEQUAL( CHARV, 1, 3, 'yes') ) THEN
          IF ( .NOT. LPREC ) LCHANGE = .TRUE.
          LPREC = .TRUE.
        ELSE
          IF ( LPREC ) LCHANGE = .TRUE.
          LPREC = .FALSE.
        ENDIF
        LPRECSET = .TRUE.
        GOTO 900
      ENDIF
C
C     Input usewind.
C
      IF ( CHEQUAL( HPARN, 1, 7, 'usewind') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTIN: usewind = ' // CHARV ,JPQUIET)
        IF ( CHEQUAL( CHARV, 1, 3, 'yes') ) THEN
          IF ( .NOT. LWIND ) LCHANGE = .TRUE.
          LWIND = .TRUE.
        ELSE
          IF ( LWIND ) LCHANGE = .TRUE.
          LWIND = .FALSE.
        ENDIF
        LWINDSET = .TRUE.
        GOTO 900
      ENDIF
C
C     Input parameter.
C
      IF ( CHEQUAL( HPARN, 1, 9, 'parameter') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: parameter = ',INTV(1))
        IF ( NIPARAM .NE. INTV(1) ) LCHANGE = .TRUE.
        NIPARAM = INTV(1)
        GOTO 900
      ENDIF
C
C     Input truncation.
C
      IF ( CHEQUAL( HPARN, 1, 10, 'truncation') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: truncation = ',INTV(1))
        IF ( NIREPR .NE. JPSPHERE)  THEN
          LCHANGE = .TRUE.
          NIGAUSS = 0
          NIGRID(1) = 0
          NIGRID(2) = 0
          NIAREA(1) = 0
          NIAREA(2) = 0
          NIAREA(3) = 0
          NIAREA(4) = 0
        ENDIF
        NIREPR = JPSPHERE
        IF ( NIRESO .NE. INTV(1) ) LCHANGE = .TRUE.
        NIRESO = INTV(1)
        GOTO 900
      ENDIF
C
C     Input reduced.
C
      IF( CHEQUAL( HPARN, 1,  7, 'reduced') ) THEN
        LFOUND = .TRUE.
C
C       (Check for changes to internal interp. request)
        LCHANGE = (NIGAUSS.NE.INTV(1))
C
        CALL INTLOG(JP_DEBUG, 'INTIN: '//TRIM(HPARN)//' = ',INTV(1))
        NIGAUSS = INTV(1)
        LNIGAUS = .TRUE.
        IF ( NIREPR .NE. JPQUASI ) THEN
          LCHANGE = .TRUE.
          NIRESO = 0
          NIGRID(1) = 0
          NIGRID(2) = 0
        ENDIF
        NIREPR = JPQUASI
        IF( (NIGAUSO.NE.NIGAUSS) .OR. (HIGAUST.EQ.'O')
     X      .OR. ((HIGAUST.NE.'R') .AND. (HIGAUST.NE.'O')) ) THEN
          CTEMP = 'R'
          CALL JGETGG( NIGAUSS, CTEMP, RIGAUSS, MILLEN, IRET)
          IF ( IRET .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTIN: JGETGG fail for NIGAUSS = ',NIGAUSS)
            INTIN = IRET
            GOTO 900
          ENDIF
          NIGAUSO = NIGAUSS
          HIGAUST = CTEMP
        ENDIF
C
        NIAREA(1) = INT( RIGAUSS(1) * PPMULT )
        NIAREA(2) = 0
        NIAREA(3) = INT( RIGAUSS(NIGAUSS*2) * PPMULT )
c this gives different result from one in the grib header
        NIAREA(4) = JP360 - JP360/MILLEN(NIGAUSS)
C
        NINS = NIGAUSS*2 ! Added by D. Jokic Mar-2005;
        NIPCNT = 0
        DO I= 1, NIGAUSS*2
          NIPCNT = NIPCNT + MILLEN(I)
        ENDDO
        GOTO 900
      ENDIF
C
C     Input grid.
C
      IF ( CHEQUAL( HPARN, 1, 4, 'grid') ) THEN
        LFOUND = .TRUE.
        CALL INTLOGR(JP_DEBUG, 'INTIN: grid e-w = ',REALV(1))
        CALL INTLOGR(JP_DEBUG, 'INTIN: grid n-s = ',REALV(2))
        IF ( NIREPR .NE. JPREGULAR) THEN
          LCHANGE = .TRUE.
          NIGAUSS = 0
          NIRESO = 0
        ENDIF
        NIREPR = JPREGULAR
        DO I = 1, 2
          ITEMP = INT( REALV(I)*PPMULT)
          IF ( NIGRID(I) .NE. ITEMP ) LCHANGE = .TRUE.
          NIGRID(I) = ITEMP
        ENDDO
        GOTO 900
      ENDIF
C
C     Reduced lat-lon Input grid.
C
      IF ( CHEQUAL( HPARN, 1, 7, 'redu_ll') ) THEN
        LFOUND = .TRUE.
cs        CALL INTLOGR(JP_DEBUG, 'INTIN: reduced_ll e-w = ',REALV(1))
        CALL INTLOGR(JP_DEBUG, 'INTIN: reduced_ll n-s = ',REALV(2))
        IF ( NIREPR .NE. JPREGULAR) THEN
          LCHANGE = .TRUE.
          NIGAUSS = 0
          NIRESO = 0
        ENDIF
        NIREPR = JPREDLL
        NINS = INTV(1)
        DO I = 1, 2
          ITEMP = INT( REALV(I)*PPMULT)
          IF ( NIGRID(I) .NE. ITEMP ) LCHANGE = .TRUE.
          NIGRID(I) = ITEMP
        ENDDO
        GOTO 900
      ENDIF
C
      IF ( CHEQUAL( HPARN, 1, 5, 'ocean') ) THEN
        LFOUND = .TRUE.
        LOCEAN = .TRUE.
        CALL INTLOGR(JP_DEBUG, 'INTIN: Ocean field',JPQUIET)
        NIOCO3  = INTV(1)
        NIOCO4  = INTV(2)
        NIOCO4F = INTV(3)
        NIOCO3F = INTV(4)
        NIOCO4L = INTV(5)
        NIOCO3L = INTV(6)
        NIOIINC = INTV(7)
        NIOJINC = INTV(8)
        NIOIRGR = INTV(9)
        NIONOST = INTV(10)
        NICOORN = INTV(11)
        NIVCDEF = INTV(12)
        NILOCAL = 4
        GOTO 900
      ENDIF

      IF ( CHEQUAL( HPARN, 1, 5, 'local') ) THEN
        LFOUND = .TRUE.
        NILOCAL = INTV(1)
        CALL INTLOGR(JP_DEBUG, 'INTIN: Local settings',NILOCAL)
        GOTO 900
      ENDIF
c
      IF ( CHEQUAL( HPARN, 1, 6, 'matrix') ) THEN
        LFOUND = .TRUE.
        NIMATR = INTV(1)
        CALL INTLOGR(JP_DEBUG, 'INTIN: Matrix values',NIMATR)
        GOTO 900
      ENDIF
C
      IF ( CHEQUAL( HPARN, 1, 11, 'coord_ocean') ) THEN
        LFOUND = .TRUE.
        CALL INTLOGR(JP_DEBUG, 'INTIN: Ocean field grid Coordinate'
     X     ,JPQUIET)
        OCCOOR(1:NICOORN) = INTV(1:NICOORN)
        GOTO 900
      ENDIF
C
      IF ( CHEQUAL( HPARN, 1, 6, 'l_npts') ) THEN
        LFOUND = .TRUE.
        MILLEN(1:NINS) = INTV(1:NINS)
        CALL INTLOG(JP_DEBUG, 'INTIN: l_npts (#) = ',NINS)
        GOTO 900
      ENDIF
C
C     Input data format.
C
      IF ( CHEQUAL( HPARN, 1, 4, 'form') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: form = ' // CHARV ,JPQUIET)
        IF ( CHEQUAL( CHARV, 1, 4, 'grib') ) THEN
          IF ( NIFORM .EQ. 0 ) LCHANGE = .TRUE.
          NIFORM = 1
        ELSE
          IF ( NIFORM .EQ. 1 ) LCHANGE = .TRUE.
          NIFORM = 0
        ENDIF
        GOTO 900
      ENDIF
C
C     Number of points along latitude and longitude
C
      IF ( CHEQUAL( HPARN, 1, 4, 'npts') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: pts along latitude = ',INTV(1))
        CALL INTLOG(JP_DEBUG, 'INTIN: pts along longitude = ',INTV(2))
        NIWE = INTV(1)
        NINS = INTV(2)
        GOTO 900
      ENDIF

      IF( CHEQUAL( HPARN, 1, 8, 'accuracy') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: Inpu accuracy = ',INTV(1))
        NIACC = INTV(1)
        GOTO 900
      ENDIF
C
C     Input level.
C
      IF ( CHEQUAL( HPARN, 1, 5, 'level') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: level = ',INTV(1))
        IF ( NILEVEL .NE. INTV(1) ) LCHANGE = .TRUE.
        NILEVEL = INTV(1)
        GOTO 900
      ENDIF
C
C     Input Date.
C
      IF ( CHEQUAL( HPARN, 1, 4, 'date') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: Date = ',INTV(1))
        IF ( NIDATE .NE. INTV(1) ) LCHANGE = .TRUE.
        NIDATE = INTV(1)
        GOTO 900
      ENDIF
C
C     Input level type.
C
      IF ( CHEQUAL( HPARN, 1, 7, 'levtype') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: levtype = ',INTV(1))
        IF ( NILEVT .NE. INTV(1) ) LCHANGE = .TRUE.
        NILEVT  = INTV(1)
        GOTO 900
      ENDIF
C
C     Input table.
C
      IF ( CHEQUAL( HPARN, 1, 5, 'table') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: table = ',INTV(1))
        IF ( NITABLE .NE. INTV(1) ) LCHANGE = .TRUE.
        NITABLE = INTV(1)
        GOTO 900
      ENDIF
C
C     Input regular (gaussian).
C
      IF ( CHEQUAL( HPARN, 1, 7, 'regular') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: regular = ',INTV(1))
        NIREPR = JPGAUSSIAN
        IF ( NIGAUSS .NE. INTV(1) ) LCHANGE = .TRUE.
        NIGAUSS = INTV(1)
        LNIGAUS = .TRUE.
        IF ( NIREPR .NE. JPGAUSSIAN) THEN
          LCHANGE = .TRUE.
          NIRESO = 0
          NIGRID(1) = 0
          NIGRID(2) = 0
        ENDIF
        NIREPR = JPGAUSSIAN
        IF( (NIGAUSO.NE.NIGAUSS).OR.(HIGAUST.NE.'F') ) THEN
          CTEMP = 'F'
          CALL JGETGG(NIGAUSS,CTEMP,RIGAUSS,MILLEN,IRET)
          IF ( IRET .NE. 0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'INTIN: JGETGG fail for NIGAUSS',NIGAUSS)
            INTIN = IRET
            GOTO 900
          ENDIF
          NIGAUSO = NIGAUSS
          HIGAUST = CTEMP
        ENDIF
C
        NIAREA(1) = INT( RIGAUSS(1) * PPMULT )
        NIAREA(2) = 0
        NIAREA(3) = INT( RIGAUSS(NIGAUSS*2) * PPMULT )
        NIAREA(4) = JP360 - JP360/MILLEN(NIGAUSS)
C
        NIPCNT = 0
        DO I= 1, NIGAUSS*2
          NIPCNT = NIPCNT + MILLEN(I)
        ENDDO
        GOTO 900
      ENDIF
C
C     Input area.
C
      IF ( CHEQUAL( HPARN, 1, 4, 'area') ) THEN
        LFOUND = .TRUE.
        CALL INTLOGR(JP_DEBUG, 'INTIN: north = ',REALV(1))
        CALL INTLOGR(JP_DEBUG, 'INTIN: west  = ',REALV(2))
        CALL INTLOGR(JP_DEBUG, 'INTIN: south = ',REALV(3))
        CALL INTLOGR(JP_DEBUG, 'INTIN: east  = ',REALV(4))
        DO I = 1, 4
          ITEMP = INT( REALV(I)*PPMULT)
          IF ( NIAREA(I) .NE. ITEMP ) LCHANGE = .TRUE.
          NIAREA(I) = ITEMP
        ENDDO
        GOTO 900
      ENDIF
C
C     Input scan.
C
      IF ( CHEQUAL( HPARN, 1, 4, 'scan') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG, 'INTIN: scan = ',INTV(1))
        IF ( NISCNM .NE. INTV(1) ) LCHANGE = .TRUE.
        NISCNM = INTV(1)
        GOTO 900
      ENDIF
C
C     Input lsm_param.
C
      IF ( CHEQUAL( HPARN, 1, 9, 'lsm_param') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTIN: lsm_param = ' // CHARV ,JPQUIET)
        IF ( CHEQUAL( CHARV, 1, 3, 'yes') ) THEN
          IF ( .NOT. LSMPAR ) LCHANGE = .TRUE.
          LSMPAR = .TRUE.
        ELSE
          IF ( LSMPAR ) LCHANGE = .TRUE.
          LSMPAR = .FALSE.
        ENDIF
        LSMPARSET = .TRUE.
        GOTO 900
      ENDIF
C
C     Set of LSM - merging with grib_api
      IF ( CHEQUAL( HPARN, 1, 6, 'lsmset') ) THEN
        LFOUND = .TRUE.
        CALL INTLOG(JP_DEBUG,'INTIN: lsmset = ' // CHARV ,JPQUIET)
        IF ( CHEQUAL( CHARV, 1, 2, 'no') ) THEN
          IF ( LSM ) THEN
            LCHANGE = .TRUE.
            LSMCHNG = .TRUE.
          ENDIF
          LSM = .FALSE.
          LSMSET = .TRUE.
        ELSE
          IF ( .NOT. LSM ) THEN
            LCHANGE = .TRUE.
            LSMCHNG = .TRUE.
          ENDIF
          LSM = .TRUE.
          LSMSET = .FALSE.
        ENDIF
        GOTO 900
      ENDIF
C
C
C     Input user_regular_gaussian
C
C     (Ignore the call if the user_regular_gaussian value = 0.)
      IF ( CHEQUAL( HPARN, 1, 21, 'user_regular_gaussian') ) THEN
        LFOUND = .TRUE.
        IF ( NIREPR .NE. JPGAUSSIAN) THEN
          LCHANGE = .TRUE.
          NIRESO = 0
          NIGRID(1) = 0
          NIGRID(2) = 0
        ENDIF
        CALL INTLOG(JP_DEBUG,'INTIN: user_regular_gaussian = ',INTV(1))
        IF ( NIGAUSS .NE. INTV(1) ) LCHANGE = .TRUE.
        NIGAUSS = INTV(1)
        NIREPR  = JPGAUSSIAN
C
        MILLEN(1:NIGAUSS*2) = NOGAUSS*4
        NIGAUSO = NIGAUSS
        HIGAUST = 'U'
        LIGIVEP = .TRUE.
C
        GOTO 900
      ENDIF
C
C     Input user_reduced_gaussian
C     (Ignore the call if the user_reduced_gaussian value = 0.)
C
      IF ( CHEQUAL( HPARN, 1, 21, 'user_reduced_gaussian') ) THEN
        LFOUND = .TRUE.
        IF ( NIREPR .NE. JPQUASI ) THEN
          LCHANGE = .TRUE.
          NIRESO = 0
          NIGRID(1) = 0
          NIGRID(2) = 0
        ENDIF
        IF ( INTV(1) .EQ. 0 ) GOTO 900
        CALL INTLOG(JP_DEBUG,'INTIN: user_reduced_gaussian = ',INTV(1))
        IF ( NIGAUSS .NE. INTV(1) ) LCHANGE = .TRUE.
        NIGAUSS = INTV(1)
        NIREPR  = JPQUASI
        NIGAUSO = NIGAUSS
        HIGAUST = 'U'
        GOTO 900
      ENDIF
C
C     Input g_pnts loading from grib_api.
C
      IF ( CHEQUAL( HPARN, 1, 7, 'ga_pnts') ) THEN
        LFOUND = .TRUE.
        IF ( NIGAUSS .LE. 0 ) THEN
          IRET = JPROUTINE + 2
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Impossible number of gaussian latitudes.',NIGAUSS)
          INTIN = IRET
          GOTO 900
        ENDIF
          CALL INTLOG(JP_DEBUG,
     X      'INTIN: Reduced Gaussian - grib_api ',NIGAUSS)
C
cs        ILAST = NIGAUSS*2 + 1
        MILLEN(1:NIGAUSS*2) = INTV(1:NIGAUSS*2)
C
        NIAREA(4) = JP360 - INT( JP360/MILLEN(NIGAUSS) )
C
        NIPCNT = 0
        DO I= 1, NIGAUSS*2
          NIPCNT = NIPCNT + MILLEN(I)
        ENDDO
        NIGAUSO = NIGAUSS
        HIGAUST = 'U'
        LIGIVEP = .TRUE.
        GOTO 900
      ENDIF
cs end ga

      IF ( CHEQUAL( HPARN, 1, 6, 'g_pnts') ) THEN
        LFOUND = .TRUE.
        IF ( NIGAUSS .LE. 0 ) THEN
          IRET = JPROUTINE + 2
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Impossible number of gaussian latitudes.',NIGAUSS)
          INTIN = IRET
          GOTO 900
        ENDIF
C
        DO I = 1, NIGAUSS
          MILLEN(I)                 = INTV(I)
          MILLEN(NIGAUSS*2 + 1 - I) = INTV(I)
        ENDDO
C
        NIAREA(4) = JP360 - INT( JP360/MILLEN(NIGAUSS) )
C
        NIPCNT = 0
        DO I = 1, NIGAUSS*2
          NIPCNT = NIPCNT + MILLEN(I)
        ENDDO
        NIGAUSO = NIGAUSS
        HIGAUST = 'U'
        LIGIVEP = .TRUE.
        GOTO 900
      ENDIF
C
C     Input g_lats.
C
      IF ( CHEQUAL( HPARN, 1, 6, 'g_lats') ) THEN
        LFOUND = .TRUE.
        IF ( NIGAUSS .LE. 0 ) THEN
          IRET  = JPROUTINE + 2
          CALL INTLOG(JP_ERROR,
     X      'INTIN: Impossible number of gaussian latitudes.',NIGAUSS)
          INTIN = IRET
          GOTO 900
        ENDIF
C
        DO I = 1, NIGAUSS
          RIGAUSS(I)                 =  REALV(I)
          RIGAUSS(NIGAUSS*2 + 1 - I) = -REALV(I)
        ENDDO
C
        NIAREA(1) = INT( RIGAUSS(1) * PPMULT )
        NIAREA(2) = 0
        NIAREA(3) = INT( RIGAUSS(NIGAUSS*2) * PPMULT )
        NIAREA(4) = JP360 - JP360/MILLEN(NIGAUSS)
        NIGAUSO = NIGAUSS
        HIGAUST = 'U'
        LIGIVEL   = .TRUE.
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
      IF( .NOT. LFOUND ) THEN
        CALL INTLOG(JP_ERROR, 'INTIN: Unknown keyword used:', JPQUIET)
        CALL INTLOG(JP_ERROR, HPARN, JPQUIET)
        INTIN = JPROUTINE + 9
      ENDIF
      RETURN
      END
