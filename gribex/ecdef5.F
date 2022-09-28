C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF5( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                    KPR, KRET)
C
C---->
C**** ECDEF5
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 5.
C     (Extensions for forecast probabilities).
C
C
C**   Interface.
C     ----------
C
C     CALL ECDEF5( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
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
C     Adapted from ECLOC1.F - simplified to handle just definition 5.
#include "ecdef5.h"
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
C     None.
C
C
C----<
C     ------------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
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
      INTEGER IRET, IZERO, I16BIT
C
      INTEGER ILALO
      DIMENSION ILALO(8)
C
      LOGICAL LENCODE, LDECODE
C
C     16-bits set to 1
      DATA I16BIT/ 65535 /
C
C     ------------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IRET = KRET
      LENCODE = ( HFUNC .EQ. 'C' )
      LDECODE = .NOT. LENCODE
C
C     ------------------------------------------------------------------
C*    Section 2 . Extensions Forecast probabilities (definition 5).
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C*    Octet 50 : Forecast probability number
C                One 8 bit field.
C
C     Insert/extract field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(42),1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C*    Octet 51 : Total number of forecast probabilities
C                One 8 bit field.
C
C     Insert/extract field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(43),1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C*    Octet 52 : Threshold units decimal scale factor.
C                One 8 bit field.
C
C     When coding data, set sign bit to 1 if value is negative:
      IF ( LENCODE ) CALL CSGNBT( ILALO(1), KSEC1(44),  8, KRET)
C
C     Insert/extract field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,ILALO(1),1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C     When decoding, use sign bit ( =  1 if value is negative)
      IF ( LDECODE ) CALL DSGNBT( KSEC1(44), ILALO(1),  8, KRET)
C
C     Octet 53 : Threshold indicator.
C                One 8 bit field.
C
C     Insert/extract field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(45),1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C
C*    Octet 54-55 : Lower threshold.
C                   One 16 bit field.
C
C     Check if lower threshold is present ..
      IF ( KSEC1(45) .NE. 2 ) THEN
C
C       When coding data, set sign bit to 1 if value is negative:
        IF ( LENCODE ) CALL CSGNBT( ILALO(1), KSEC1(46), 16, KRET)
C
C       Insert/extract fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
          GO TO 900
        ENDIF
C
C       When decoding, use sign bit ( =  1 if value is negative)
        IF ( LDECODE ) CALL DSGNBT( KSEC1(46), ILALO(1), 16, KRET)
C
C     .. if not present, set value to 16 bits all set to 1
      ELSE
          IF ( LENCODE ) THEN
            I16BIT = 65535
            CALL INXBIT(KGRIB,KLENG,KNSPT,I16BIT,1,KBITS,16,HFUNC,KRET)
            IF (KRET.NE.0) THEN
              KRET = 2
              WRITE(GRPRSM,9004)
              GO TO 900
            ENDIF
          ELSE
            I16BIT = 65535
            KSEC1(46) = I16BIT
            KNSPT = KNSPT + 16
          ENDIF
      ENDIF
C
C     Octet 56-57 : Upper threshold.
C                   One 16 bit field.
C
C     Check if upper threshold is present ..
      IF ( KSEC1(45) .NE. 1 ) THEN
C
C       When coding data, set sign bit to 1 if value is negative:
        IF ( LENCODE ) CALL CSGNBT( ILALO(1), KSEC1(47), 16, KRET)
C
C       Insert/extract fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
          GO TO 900
        ENDIF
C
C       When decoding, use sign bit ( =  1 if value is negative)
        IF ( LDECODE ) CALL DSGNBT( KSEC1(47), ILALO(1), 16, KRET)
C
      ELSE
        IF ( LENCODE ) THEN
          I16BIT = 65535
          CALL INXBIT(KGRIB,KLENG,KNSPT,I16BIT,1,KBITS,16,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,9004)
            GO TO 900
          ENDIF
        ELSE
          I16BIT = 65535
          KSEC1(47) = I16BIT
          KNSPT = KNSPT + 16
        ENDIF
      ENDIF
C
C
C     Octet 58 : Reserved - set to zero.
C                One 8 bit field.
C
C     Insert/extract field.
      IZERO = 0
      CALL INXBIT (KGRIB,KLENG,KNSPT, IZERO ,1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 9 . Abort/return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'ECDEF5 : Section 9.'
        WRITE(GRPRSM,*) '         Output values set -'
        WRITE(GRPRSM,9003) KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF5')
      ELSE
        RETURN
      ENDIF
C
 9003 FORMAT (1H ,'         KNSPT  = ',I12)
C
 9004 FORMAT (' ECDEF5: Error reported by routine INXBIT.')
C
      END
