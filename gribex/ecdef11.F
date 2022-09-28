C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF11( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                   KPR, KRET)
C
C---->
C**** ECDEF11
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 11.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF11( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
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
C     KNSPT      - Number of bit after last one inserted/extracted.
C                  (Updated)
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
C     local part of section 1 of Grib code, definition 11.
C
C
C     Externals.
C     ----------
C
C     INXBIT  - insert/extract bits
C     ABORTX  - abort the program
C     CSGNBT  - code a signed value
C     DSGNBT  - decode a signed value
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
#include "ecdef11.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      February 1998
C
C
C     Modifications.
C     --------------
C
C     None
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
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, JLOOP
C
      LOGICAL LENCODE, LDECODE
C
      DATA IZERO/0/
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
C*    Section 2 . Handle data for analysis which used the supplementary
C                 data
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Octet 50: Class of analysis
C     Octet 51: Type of analysis
C     Two 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),2,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF11: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 42 - 43.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 50 - 51.'
        ENDIF
      ENDIF
C
C     Octets 52-53: Stream of analysis
C     One 16-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(44),1,KBITS,16,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF11: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, element 44.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 52 - 53.'
        ENDIF
      ENDIF
C
C     Octets 54-57: Version number/experiment identifier of analysis
C     One 32-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),1,KBITS,32,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF11: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, element 45.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 54 - 57.'
        ENDIF
      ENDIF
C
C     Octet 58: Year of analysis
C     Octet 59: Month of analysis
C     Octet 60: Day of analysis
C     Octet 61: Hour of analysis
C     Octet 62: Minute of analysis
C     Octet 63: Century of analysis
C     Octet 64: Originating centre of analysis
C     Octet 65: Sub-centre of analysis
C     Eight 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46),8,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF11: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 46 - 53.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 58 - 65.'
        ENDIF
      ENDIF
C
C     Octets 66-72: Spare - set to zero.
C     Seven 8-bit fields.
C
      IF( LENCODE ) THEN
        DO 210 JLOOP = 1, 7
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF11: Error reported by routine INXBIT:'
              WRITE(GRPRSM,*)
     X  '      encoding section 1, octets 66 - 72.'
          ENDIF
  210   CONTINUE
      ELSE
        KNSPT = KNSPT + (7*8)
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 9 . Abort/return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'ECDEF11: Section 9.'
        WRITE(GRPRSM,*) '        Output values set, KNSPT = ', KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF11')
      ELSE
        RETURN
      ENDIF
C
      END
