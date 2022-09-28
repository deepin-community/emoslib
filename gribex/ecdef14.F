C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF14( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                    KPLEN, KPR, KRET)
C
C---->
C**** ECDEF14
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 14.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF14( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
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
C     local part of section 1 of Grib code, definition 14.
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
#include "ecdef14.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      August 1998
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
      CHARACTER*1 HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, JLOP, LENS1, NF, NLEFT
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
      LENCODE = ( HFUNC.EQ.'C' )
      LDECODE = .NOT. LENCODE
C
C     ------------------------------------------------------------------
C*    Section 2 . Handle brightness temperature
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Octet 50: As for MARS labelling (zero or ensemble forecast number)
C     Octet 51: As for MARS labelling (zero or total no. in ensembles)
C     Octet 52: Channel number
C     Three 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),3,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF14: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 42 - 44.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 50 - 52.'
        ENDIF
      ENDIF
C
C     Octets 53-56: Integer scaling factor applied to frequencies
C     One 32-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),1,KBITS,32,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF14: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, element 45.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 53 - 56.'
        ENDIF
      ENDIF
C
C     Octet 57: Total number of frequencies (Nf)
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46),1,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF14: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, element 46.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 57.'
        ENDIF
      ENDIF
C
C     Octets 58-60: Spare (set to zero)
C     Three 8-bit fields.
C
      IF( LENCODE ) THEN
        DO 210 JLOP = 58, 60
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF14: Error reported by routine INXBIT:'
            WRITE(GRPRSM,*) '      encoding section 1, octets 58 - 60.'
          ENDIF
  210   CONTINUE
      ELSE
        KNSPT = KNSPT + 3*8
      ENDIF
C
C     Octet 61 onwards: List of Nf scaled directions
C     Nf 32-bit fields.
C
      NF = KSEC1(46)
      DO 220 JLOP = 47, (46+NF)
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(JLOP),1,KBITS,32,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF14: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(GRPRSM,*)
     X  '      encoding section 1, elements 47 onwards'
          ELSE
            WRITE(GRPRSM,*)
     X  '      decoding section 1, octets 61 onwards.'
          ENDIF
        ENDIF
  220 CONTINUE
C
C     Octets at end of header:Spare - set to zero.
C     (255-NF) 32-bit fields.
C
      NLEFT = 255 - NF
      IF( LENCODE ) THEN
        DO 230 JLOP = 1, NLEFT
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,32,HFUNC,KRET)
          IF (KRET.NE.0) THEN
            KRET = 2
            WRITE(GRPRSM,*) 'ECDEF11: Error reported by routine INXBIT:'
            WRITE(GRPRSM,*)
     X  '      encoding section 1, spare octets at end.'
          ENDIF
  230   CONTINUE
      ELSE
        KNSPT = KNSPT + (NLEFT*32)
      ENDIF
C
C     On encoding, record the length of section 1.
C
      IF( LENCODE ) THEN
        LENS1 = 1080
        CALL INXBIT(KGRIB,KLENG,KPLEN,LENS1, 1,KBITS,24,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF14: Error reported by routine INXBIT:'
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
        WRITE(GRPRSM,*) 'ECDEF14: Section 9.'
        WRITE(GRPRSM,*) '        Output values set, KNSPT = ', KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF14')
      ELSE
        RETURN
      ENDIF
C
      END
