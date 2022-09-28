C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF18(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
C
C---->
C**** ECDEF18
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 18.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF18(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
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
C     local part of section 1 of Grib code, definition 18.
C
C
C     Externals.
C     ----------
C
C     INXBIT  - Insert/extract bits to/from bit stream.
C     ABORTX  - Abort processing.
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
#include "ecdef18.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      February 2001
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
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, NUMBER, ILOOP, LOOP
CC    INTEGER INUM
      CHARACTER*4 HBLANK
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
      LENCODE = HFUNC.EQ.'C'
C
C     -----------------------------------------------------------------|
C*    Section 2 . Handle definition 1.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Octet 50 : Ensemble forecast number
C     Octet 51 : Total number of Forecasts.
C     Octet 52 : Data origin
C     Three 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),3,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF18: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '       encoding section 1, octets 50 to 52.'
        ELSE
          WRITE(GRPRSM,*) '       decoding section 1, octets 50 to 52.'
        ENDIF
      ENDIF
C
C     Octets 53 - 56 : Model identifier of forecast used
C     4 Ascii characters, right justified
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),1,KBITS,32,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF18: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '       encoding section 1, octets 53 to 56.'
        ELSE
          WRITE(GRPRSM,*) '       decoding section 1, octets 53 to 56.'
        ENDIF
      ENDIF
C
C     Octet 57 : Consensus count
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46),1,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF18: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '       encoding section 1, octet 57.'
        ELSE
          WRITE(GRPRSM,*) '       decoding section 1, octet 57.'
        ENDIF
      ENDIF
C
C     Octets 58 to 60 : Spare (set to zero)
C
      DO LOOP = 1, 3
        IF ( LENCODE ) THEN
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF18: Error reported by routine INXBIT:'
            WRITE(GRPRSM,*)
     X   '         encoding section 1, octets 58 to 60.'
          ENDIF
        ELSE
          KNSPT = KNSPT + 8
        ENDIF
      ENDDO
C
C     Octets 61 to 120 : List of WMO centre identifiers for analyses used
C     Upto fifteen fields. ( Each 4 Ascii characters, right justified)
C
      NUMBER = KSEC1(46)
      DO LOOP = 1, NUMBER
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46+LOOP),1,
     X              KBITS,32,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF18: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*)
     X   '       encoding section 1, list of WMO ids.'
          ELSE
            WRITE(GRPRSM,*)
     X   '       decoding section 1, list of WMO ids.'
          ENDIF
        ENDIF
      ENDDO
C
C     Handle unused entries (set to blanks)
C
      DO LOOP = NUMBER+1, 15
        HBLANK = '    '
        IF ( LENCODE ) THEN
          CALL INXBIT(KGRIB,KLENG,KNSPT,HBLANK,1,KBITS,32,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF18: Error reported by routine INXBIT:'
            WRITE(GRPRSM,*) '         encoding section 1, unused list.'
          ENDIF
        ELSE
          KSEC1(46+LOOP) = 0
          DO ILOOP = 1, 4
            KSEC1(46+LOOP) = KSEC1(46+LOOP)*256 +
     X                       ICHAR(HBLANK(ILOOP:ILOOP))
          ENDDO
          KNSPT = KNSPT + 32
        ENDIF
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9 . Abort/return to calling routine. Format statements.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF18')
      ELSE
        RETURN
      ENDIF
C
      END
