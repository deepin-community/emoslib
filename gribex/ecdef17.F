C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF17(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
C
C---->
C**** ECDEF17
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 17.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF17(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     HFUNC - Requested function.
C              'C' to code data.
C              'D' to decode data.
C     KSEC1 - Array containing Grib Section 1 data.
C     KGRIB - Array containing Grib coded data.
C     KLENG - Length (words) of KGRIB.
C     KNSPT - Bit number after which insertion/extraction starts.
C     KBITS - Number of bits in computer word.
C     KRET  - Response to error indicator.
C              0        , Abort if error encountered.
C              Non-zero , Return to calling routine
C                         even if error encountered.
C
C     Output Parameters.
C     ------------------
C
C     KSEC1 - Array containing Grib Section 1 data.(Updated)
C     KGRIB - Array containing Grib coded data.(Updated)
C     KNSPT - Number of bit after last one inserted/extracted.(Updated)
C     KRET  - Return code.
C              0   , No error encountered.
C              2   , Error reported by routine INXBIT.
C
C
C     Method.
C     -------
C
C     Input data packed/unpacked in accordance with ECMWF usage of
C     local part of section 1 of Grib code, definition 17.
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
C     Adapted from ecdef6.F
C
#include "ecdef17.h"
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF       May 2000
C
C
C     Modifications.
C     --------------
C
C     Set space used for the list of SEA-ICE dates to a multiple of
C     JPPAD to stabilise the length of section 1 of the GRIB header.
C     (Usually the number of dates will be less than 10).
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
C
      INTEGER JPPAD
      PARAMETER( JPPAD = 10 )
C
C     Subroutine arguments
C
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, ILOOP, LENS1, INDATE, INSPT, NSEAICE, NEXTRA
      INTEGER IPLEN
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
      LENCODE = ( HFUNC.EQ.'C' )
      INSPT = KNSPT
C
C     Set bit pointer to start of GRIB section 1
C
      IPLEN = 8*8
C
C     ------------------------------------------------------------------
C*    Section 2 . Handle definition 6.
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
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
      ELSE
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),2,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
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
      IF( KRET.NE.0 ) THEN
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
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
C
C     Octet 56 : Count of ICE fields used (n, say).
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46), 1,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
C
C     Octet 57->54+(n*4) : YYMMDD dates and satellite numbers of the
C                          ICE fields used.
C
      NSEAICE = KSEC1(46)
      DO ILOOP = 1, NSEAICE
C
C       YYMMDD date.
C
        IF( LENCODE) THEN
          INDATE = KSEC1(45+(ILOOP*2))
          IF( INDATE.GT.19000000 ) INDATE = INDATE - 19000000
        ENDIF
        CALL INXBIT(KGRIB,KLENG,KNSPT,INDATE,1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
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
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
      ENDDO
C
C     ------------------------------------------------------------------
C*    Section 3 . Handle padding of the ICE field slots.
C     ------------------------------------------------------------------
C
  300 CONTINUE
C
C     If the number of ICE fields used is not a multiple of JPPAD,
C     pad the ICE field slots upto a multiple of JPPAD.
C
      NEXTRA = JPPAD - MOD(NSEAICE,JPPAD)
      NSEAICE = NSEAICE + NEXTRA
      IF( LENCODE ) THEN
        DO ILOOP = 1, NEXTRA
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,32,HFUNC,KRET)
          IF( KRET.NE.0 ) THEN
            KRET = 2
            WRITE(GRPRSM,9004)
          ENDIF
        ENDDO
C
C       If encoding, set length of section 1.
C
        LENS1 = 56 + NSEAICE*4
        CALL INXBIT(KGRIB,KLENG,IPLEN,LENS1, 1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF      
C
      ELSE
C
C       If decoding, use length of section 1 to step KNSPT pointer
C       past section 1, past any padding slots if any.
C
        CALL INXBIT(KGRIB,KLENG,IPLEN,LENS1, 1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF      
        KNSPT = INSPT - (49*8) + LENS1*8
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 9 . Abort/return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
C     Abort if requested to do so when an error has been encountered.
C
      IF( (IRET.EQ.0).AND.(KRET.NE.0) ) THEN
        CALL ABORTX ('ECDEF17')
      ELSE
        RETURN
      ENDIF
C
 9004 FORMAT (' ECDEF17: Error reported by routine INXBIT.')
C
      END
