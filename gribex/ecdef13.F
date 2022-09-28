C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF13( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                    KPLEN, KPR, KRET)
C
C---->
C**** ECDEF13
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 13.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF13( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
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
C     KPLEN      - Pointer to field containing length of Section 1
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
C     local part of section 1 of Grib code, definition 13.
C
C
C     Externals.
C     ----------
C
C     INXBIT  - insert/extract bits
C     ABORTX  - abort the program
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
#include "ecdef13.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      April 1998
C
C
C     Modifications.
C     --------------
C
C     Add system and method numbers
C     J.D.Chambers     ECMWF      May 2000
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
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, JLOP, LENS1, ND, NF, IFLAG
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
C*    Section 2 . Handle tube.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Octet 50: As for MARS labelling (zero or ensemble forecast number)
C     Octet 51: As for MARS labelling (zero or total no. in ensembles)
C     Octet 52: Direction number
C     Octet 53: Frequency number
C     Octet 54: Total number of directions (Nd)
C     Octet 55: Total number of frequencies (Nf)
C     Six 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),6,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 42 - 47.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 50 - 55.'
        ENDIF
      ENDIF
C
      ND = KSEC1(46)
      NF = KSEC1(47)
C
C     Octets 56-59: Integer scaling factor applied to directions
C     Octets 60-63: Integer scaling factor applied to frequencies
C     Two 32-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(48),2,KBITS,32,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 48 - 49.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 56 - 63.'
        ENDIF
      ENDIF
C
C     Octet 64: Flag to indicate whether or not system
C               and method number are present.
C                 0 = not present (old style product)
C                 1 = present
C
      IFLAG = 1
      CALL INXBIT(KGRIB,KLENG,KNSPT,IFLAG,1,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, system/method flag'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octet 64'
        ENDIF
      ENDIF
C
C     If the flag is set (new style product)
C
      IF( IFLAG.EQ.1 ) THEN
C
C     Octets 65-66: System number
C     One 16-bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(50+ND+NF),
     X              1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*) '      encoding section 1, system number'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 65 - 66'
          ENDIF
        ENDIF
C
C     Octets 67-68: Method number
C     One 16-bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(51+ND+NF),
     X              1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*) '      encoding section 1, method number'
          ELSE
            WRITE(GRPRSM,*) '      decoding section 1, octets 67 - 68'
          ENDIF
        ENDIF
C
      ELSE
C
C       (Old style)
C       Octets 65-68: Spare (set to zero)
C       Four 8-bit fields.
C
        IF( LENCODE ) THEN
          DO JLOP = 65, 68
            IZERO = 0
            CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
            IF (KRET.NE.0) THEN
              KRET = 2
              WRITE(GRPRSM,*)
     X  'ECDEF13: Error reported by routine INXBIT:'
              WRITE(GRPRSM,*)
     X  '      encoding section 1, octets 69 - 100.'
            ENDIF
          ENDDO
        ELSE
          KNSPT = KNSPT + 4*8
        ENDIF
      ENDIF
C
C     Octets 69-100: Spare (set to zero)
C     Thirty-32 8-bit fields.
C
      IF( LENCODE ) THEN
        DO 210 JLOP = 69, 100
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
            WRITE(GRPRSM,*) '      encoding section 1, octets 69 - 100.'
          ENDIF
  210   CONTINUE
      ELSE
        KNSPT = KNSPT + 32*8
      ENDIF
C
C     Octet 101 onwards: List of Nd scaled directions
C     Nd 32-bit fields.
C
      DO 220 JLOP = 50, (49+ND)
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(JLOP),1,KBITS,32,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*)
     X  '      encoding section 1, elements 50 onwards'
          ELSE
            WRITE(GRPRSM,*)
     X  '      decoding section 1, octets 101 onwards.'
          ENDIF
        ENDIF
  220 CONTINUE
C
C     Octets 101+Nd*4 onwards: List of Nf scaled frequencies
C     Nf 32-bit fields.
C
      DO 230 JLOP = (50+ND), (49+ND+NF)
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(JLOP),1,KBITS,32,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*)
     X  '      encoding section 1, element (50+Nd) etc.'
          ELSE
            WRITE(GRPRSM,*)
     X  '      decoding section 1, octets (101+Nd*4)->'
          ENDIF
        ENDIF
  230 CONTINUE
C
C
C     On encoding, record the length of section 1.
C
      IF( LENCODE ) THEN
        LENS1 = 100 + (ND*4) + (NF*4)
        CALL INXBIT(KGRIB,KLENG,KPLEN,LENS1, 1,KBITS,24,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF13: Error reported by routine INXBIT:'
          WRITE(GRPRSM,*) '         encoding length of section 1.'
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
        WRITE(GRPRSM,*) 'ECDEF13: Section 9.'
        WRITE(GRPRSM,*) '        Output values set, KNSPT = ', KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF13')
      ELSE
        RETURN
      ENDIF
C
      END
