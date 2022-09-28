C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF1( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                   KPR, KRET)
C
C---->
C**** ECDEF1
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 1.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF1( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     HFUNC      - Requested function.
C                  'C' to code data.
C                  'D' to decode data.
C
C     KSEC1      - Array containing Grib Section 1 data.
C
C     KGRIB      - Array containing Grib coded data.
C
C     KLENG      - Length (words) of KGRIB.
C
C     KNSPT      - Bit number after which insertion/extraction starts.
C
C     KBITS      - Number of bits in computer word.
C
C     KPR        - Debug print switch.
C                  0  , No printout.
C                  >0 , Debug printout.
C
C     KRET       - Response to error indicator.
C                  0        , Abort if error encountered.
C                  Non-zero , Return to calling routine
C                             even if error encountered.
C
C     Output Parameters.
C     ------------------
C
C     KSEC1      - Array containing Grib Section 1 data.(Updated)
C
C     KGRIB      - Array containing Grib coded data.(Updated)
C
C     KNSPT      - Number of bit after last one inserted/extracted.(Updated)
C
C     KRET       - Return code.
C                  0   , No error encountered.
C                  2   , Error reported by routine INXBIT.
C
C
C     Method.
C     -------
C
C     Input data packed/unpacked in accordance with ECMWF usage of
C     local part of section 1 of Grib code, definition 1.
C
C
C     Externals.
C     ----------
C
C     INXBIT
C     ABORTX
C     CSGNBT
C     DSGNBT
C
C
C     Reference.
C     ----------
C
C     WMO Manual On Codes for Grib Code.
C
C
C     Comments.
C     ---------
C
C     Adapted from ECLOC1.F - simplified to handle just definition 1.
#include "ecdef1.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      18th January 1995
C
C
C     Modifications.
C     --------------
C
C     J.D.Chambers     ECMWF       2nd February 1995
C     Add ensemble mean/standard deviation usage of definition 1.
C
C     J.D.Chambers     ECMWF       Jan 2000
C     Allow single 2-byte ensemble number for stream 1090 (ECMWF
C     ensemble seasonal forecasts)
C
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0 . Definition of variables. Data statements.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
      INTEGER IRET, IZERO
C
      LOGICAL LENCODE
C
C     -----------------------------------------------------------------|
C*    Section 1 . Set initial values.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IRET = KRET
      LENCODE = ( HFUNC .EQ. 'C' )
C
C     -----------------------------------------------------------------|
C*    Section 2 . Handle definition 1.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( KSEC1(40).EQ.1090 ) THEN
C
C       If seasonal forecast data ...
C
C       Octet 50-51 : Forecast number.
C       One 16 bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
        IF( .NOT. LENCODE ) KSEC1(43) = 0
C
      ELSE IF (KSEC1(39).EQ.10.OR.
     X         KSEC1(39).EQ.11.OR.
     X         KSEC1(39).EQ.17.OR.
     X         KSEC1(39).EQ.18.OR.
     X         KSEC1(39).EQ.23) THEN
C
C       If type =
C          10 = Control forecast
C          11 = Perturbed forecast
C          17 = Ensemble means
C          18 = Ensemble standard deviations
C          23 = Empirical distribution
C
C       Octet 50 : Forecast number.
C       Octet 51 : Total number of Forecasts.
C       Two 8 bit fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),2,KBITS,8,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
      ELSE
C
C       Octet 50-51 : Reserved.
C       Should be 0 !!
C
        IF ( LENCODE ) THEN
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,16,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,9004)
          ENDIF
        ELSE
          CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),2,KBITS,8,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,9004)
          ENDIF
        ENDIF
      ENDIF
C
C     Octet 52 : Reserved.
C     Set to 0.
C
      IF ( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
      ELSE
        KNSPT = KNSPT + 8
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9 . Abort/return to calling routine. Format statements.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF (KPR.GE.1) THEN
        WRITE(GRPRSM,*) 'ECDEF1 : Section 9.'
        WRITE(GRPRSM,*) '         Output values set -'
        WRITE(GRPRSM,9003) KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF1')
      ELSE
        RETURN
      ENDIF
C
 9003 FORMAT (1H ,'         KNSPT  = ',I12)
C
 9004 FORMAT (' ECDEF1: Error reported by routine INXBIT.')
C
      END
