C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF9( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                   KPR, KRET)
C
C---->
C**** ECDEF9
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 9.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF9( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
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
C     local part of section 1 of Grib code, definition 9.
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
#include "ecdef9.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      June 1996
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
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
      INTEGER IRET, IZERO, JLOOP, ITEMP, IRETA
C
      LOGICAL LENCODE, LDECODE
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
C*    Section 2 . Handle singular vector.
C     ------------------------------------------------------------------
C
  200   CONTINUE
C
      IF( KSEC1(39).EQ.62 ) THEN
C
C       Octets 50-51 : Singular vector number.
C       Octets 52-53 : Number of iterations.
C       Octets 54-55 : Number of singular vectors computed.
C       Three 16 bit fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),3,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*)
     X  '      encoding section 1, elements 42 - 44.'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 50 - 55.'
          ENDIF
        ENDIF
C
C       Octet 56 : Norm used at initial time.
C       Octet 57 : Norm used at final time.
C       Two 8 bit fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),2,KBITS,8,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*)
     X  '      encoding section 1, elements 45 - 46.'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 56 - 57.'
          ENDIF
        ENDIF
C
C       Octets 58-61 : Multiplication factor.
C       One 32 bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(47),1,KBITS,32,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*) '      encoding section 1, element 47.'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 58 - 61.'
          ENDIF
        ENDIF
C
C       Octets 62-65 : Latitude of north-west corner of LPO area.
C       Octets 66-69 : Longitude of north-west corner of LPO area.
C       Octets 70-73 : Latitude of south-east corner of LPO area.
C       Octets 74-77 : Longitude of south-east corner of LPO area.
C       Four signed 32 bit fields.
C
        DO 210 JLOOP = 48, 51
          IF ( LENCODE ) CALL CSGNBT( ITEMP, KSEC1(JLOOP), 32, IRETA)
          CALL INXBIT(KGRIB,KLENG,KNSPT,ITEMP,1,KBITS,32,HFUNC,KRET)
          IF ( LDECODE ) CALL DSGNBT( KSEC1(JLOOP), ITEMP, 32, IRETA)
          IF ( (IRETA.NE.0) .OR. (KRET.NE.0) ) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
            WRITE(GRPRSM,*)
     X  'encoding/decoding section 1, element ', JLOOP
          ENDIF
  210   CONTINUE
C
C       Octets 78-81 : Accuracy.
C       One 32 bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(52),1,KBITS,32,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*) '      encoding section 1, element 52.'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 78 - 81.'
          ENDIF
        ENDIF
C
C       Octets 82-83 : Number of singular vectors evolved.
C       One 16 bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(53),1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*) '      encoding section 1, element 53.'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 82 - 83.'
          ENDIF
        ENDIF
C
C       Octets 84-87 : Ritz number.
C       Octets 88-91 : Ritz number.
C       Two signed 32 bit fields.
C
        DO 220 JLOOP = 54, 55
          IF ( LENCODE ) CALL CSGNBT( ITEMP, KSEC1(JLOOP), 32, IRETA)
          CALL INXBIT(KGRIB,KLENG,KNSPT,ITEMP,1,KBITS,32,HFUNC,KRET)
          IF ( LDECODE ) CALL DSGNBT( KSEC1(JLOOP), ITEMP, 32, IRETA)
          IF ( (IRETA.NE.0) .OR. (KRET.NE.0) ) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
            WRITE(GRPRSM,*)
     X  'encoding/decoding section 1, element ', JLOOP
          ENDIF
  220   CONTINUE
C
C       Octet 92 : Reserved.
C       Set to 0.
C
        IF ( LENCODE ) THEN
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
            IF( LENCODE ) THEN
              WRITE(GRPRSM,*)
     X  '      encoding section 1, reserved element.'
            ELSE
              WRITE(GRPRSM,*) '      decoding section 1, octets 92.'
            ENDIF
          ENDIF
        ELSE
          KNSPT = KNSPT + 8
        ENDIF
C
C     ------------------------------------------------------------------
C*    Section 3 . Handle perturbation analysis or perturbation.
C     ------------------------------------------------------------------
C
      ELSE
C
  300   CONTINUE
C
C       Octets 50-51 : Perturbation analysis or perturbation number.
C       One 16 bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF9: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*) '      encoding section 1, element 42.'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 50 - 51.'
          ENDIF
        ENDIF
C
        IF ( LENCODE ) THEN
C
C         Octets 52-92 set to zero
          DO 310 JLOOP = 52,92
            IZERO = 0
            CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
            IF (KRET.NE.0) THEN
              KRET = 2
              WRITE(GRPRSM,*)
     X  'ECDEF9: Error reported by routine INXBIT:'
              WRITE(GRPRSM,*)
     X  '        encoding section 1, element ',JLOOP
            ENDIF
  310     CONTINUE
C
        ELSE
C
C         KSEC1(43-55) set to zero.
          DO 320 JLOOP = 43, 55
            KSEC1(JLOOP) = 0
  320     CONTINUE
          KNSPT = KNSPT + 328
          KRET = 0
        ENDIF
C
C     ------------------------------------------------------------------
C*    Section 9 . Abort/return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
      ENDIF
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'ECDEF9: Section 9.'
        WRITE(GRPRSM,*) '        Output values set, KNSPT = ', KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF9')
      ELSE
        RETURN
      ENDIF
C
C
      END
