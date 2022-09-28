C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF16(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KPR,KRET)
C
C---->
C**** ECDEF16
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 16.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF16(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KPR,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     HFUNC - Requested function.
C             'C' to code data.
C             'D' to decode data.
C
C     KSEC1 - Array containing Grib Section 1 data.
C
C     KGRIB - Array containing Grib coded data.
C
C     KLENG - Length (words) of KGRIB.
C
C     KNSPT - Bit number after which insertion/extraction starts.
C
C     KBITS - Number of bits in computer word.
C
C     KPR   - Debug print switch.
C             0  , No printout.
C             >0 , Debug printout.
C
C     KRET  - Response to error indicator.
C             0        , Abort if error encountered.
C             Non-zero , Return to calling routine
C                             even if error encountered.
C
C     Output Parameters.
C     ------------------
C
C     KSEC1 - Array containing Grib Section 1 data.(Updated)
C
C     KGRIB - Array containing Grib coded data.(Updated)
C
C     KNSPT - Number of bit after last one inserted/extracted.(Updated)
C
C     KRET  - Return code.
C             0   , No error encountered.
C             2   , Error reported by routine INXBIT.
C
C
C     Method.
C     -------
C
C     Input data packed/unpacked in accordance with ECMWF usage of
C     local part of section 1 of Grib code, definition 16.
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
C     None.
C
#include "ecdef16.h"
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      February 2000
C
C
C     Modifications.
C     --------------
C
C     None.
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
C
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, LOOP
      LOGICAL LENCODE
C
C     -----------------------------------------------------------------|
C*    Section 1 . Set initial values.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IRET = KRET
      LENCODE = ( HFUNC.EQ.'C' )
C
C     -----------------------------------------------------------------|
C*    Section 2 . Handle definition 16.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Octet 50-51 : Ensemble member number.
C     One 16-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),1,KBITS,16,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        GOTO 990
      ENDIF
C
      IF( .NOT. LENCODE ) KSEC1(43) = 0
C
C     Octet 52-53 : System number.
C     One 16-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(44),1,KBITS,16,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        GOTO 990
      ENDIF
C
C     Octet 54-55 : Method number.
C     One 16-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),1,KBITS,16,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        GOTO 990
      ENDIF
C
C     Octet 56-59 : Verifying month (in format YYYYMM).
C     One 32-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46),1,KBITS,32,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        GOTO 990
      ENDIF
C
C     Octet 60 : Averaging period (eg 6-hour, 24-hour).
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(47),1,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        GOTO 990
      ENDIF
C
C     Octet 61-80 : Spare
C     Twenty 8-bit fields.
C
      IZERO = 0
      IF( LENCODE ) THEN
        DO LOOP = 1, 20
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF( KRET.NE.0 ) THEN
            KRET = 2
            GOTO 990
          ENDIF
        ENDDO
      ELSE
        KNSPT = KNSPT + 20*8
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9 . Abort/return to calling routine. Format statements.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) WRITE(GRPRSM,*) 'ECDEF16: Output: KNSPT = ', KNSPT 
C
C     Abort if requested to do so when an error has been encountered.
C
      IF( (IRET.EQ.0).AND.(KRET.NE.0) ) CALL ABORTX ('ECDEF16')
C
      RETURN
C
  990 CONTINUE
      WRITE(GRPRSM,*) 'ECDEF16: Error reported by routine INXBIT.'
      RETURN
C
      END
