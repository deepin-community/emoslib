C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF6( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                   KPLEN, KPR, KRET)
C
C---->
C**** ECDEF6
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 1.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF6( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN,
C    X             KPR, KRET)
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
C     KPLEN      - Pointer to field containing length of Section 1.
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
#include "ecdef6.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF       27th February 1996
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
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
      INTEGER IRET, IZERO, ILOOP, LENS1, INDATE
C
      LOGICAL LENCODE
C
C     ------------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IRET = KRET
      LENCODE = ( HFUNC .EQ. 'C' )
C
C     ------------------------------------------------------------------
C*    Section 2 . Handle definition 1.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Octet 50-51 :Reserved.
C     Should be 0 !!
C
      IF ( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO, 1,KBITS, 16,HFUNC,KRET)
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
C
C     Octet 52-54 : YYMMDD date of the SST field used.
C
      IF( LENCODE) THEN
        INDATE = KSEC1(44)
        IF( INDATE.GT.19000000 ) INDATE = INDATE - 19000000
      ENDIF
      CALL INXBIT(KGRIB,KLENG,KNSPT,INDATE,1,KBITS,24,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
      IF( .NOT. LENCODE) THEN
        IF( (INDATE.LT.19000000).AND.(INDATE.GT.100) )
     X    INDATE = INDATE + 19000000
        KSEC1(44) = INDATE
      ENDIF
C
C     Octet 55 : Type of SST field used.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45), 1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
C
C     Octet 56 : Count of ICE fields used (n, say).
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46), 1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
C
C     Octet 57->54+(n*4) : YYMMDD dates and satellite numbers of the
C                          ICE fields used.
C
      DO 210 ILOOP = 1, KSEC1(46)
C
C       YYMMDD date.
        IF( LENCODE) THEN
          INDATE = KSEC1(45+(ILOOP*2))
          IF( INDATE.GT.19000000 ) INDATE = INDATE - 19000000
        ENDIF
        CALL INXBIT(KGRIB,KLENG,KNSPT,INDATE,1,KBITS,24,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
        IF( .NOT. LENCODE) THEN
          IF( (INDATE.LT.19000000).AND.(INDATE.GT.100) )
     X      INDATE = INDATE + 19000000
          KSEC1(45+(ILOOP*2)) = INDATE
        ENDIF
C
C       Satellite number (ICE data).
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46+(ILOOP*2)),
     X              1,KBITS, 8,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
  210 CONTINUE
C
C     On encoding, adjust length of section 1.
      IF( LENCODE ) THEN
        LENS1 = 56 + KSEC1(46)*4
        CALL INXBIT(KGRIB,KLENG,KPLEN,LENS1, 1,KBITS,24,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF      
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 9 . Abort/return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'ECDEF6 : Section 9.'
        WRITE(GRPRSM,*) '         Output values set -'
        WRITE(GRPRSM,*) '         KNSPT  = ', KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF6')
      ELSE
        RETURN
      ENDIF
C
C
 9004 FORMAT (' ECDEF6: Error reported by routine INXBIT.')
C
      END
