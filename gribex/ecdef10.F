C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF10( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                    KPLEN, KPR, KRET)
C
C---->
C**** ECDEF10
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 10.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF10( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
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
C     local part of section 1 of Grib code, definition 10.
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
#include "ecdef10.h"
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
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, LENS1, N, INSPT
      INTEGER ILALO
      DIMENSION ILALO(4)
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
C     Octet 50: Tube number
C     Octet 51: Total number of tubes
C     Octet 52: Central cluster definition
C     Octet 53: Indicator of parameter considered
C     Octet 54: Indicator of type of level
C     Five 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),5,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF10: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 42 - 46.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 50 - 54.'
        ENDIF
      ENDIF
C
C     Octets 55-57: Northern latitude of the domain of tubing
C     Octets 58-60: Western longitude of the domain of tubing
C     Octets 61-63: Southern latitude of the domain of tubing
C     Octets 64-66: Eastern longitude of the domain of tubing
C     Four 24-bit fields.
C
C     When coding values, set sign bit to 1, if value is negative.
C
      IF ( LENCODE ) THEN
        CALL CSGNBT( ILALO(1), KSEC1(47), 24, KRET)
        CALL CSGNBT( ILALO(2), KSEC1(48), 24, KRET)
        CALL CSGNBT( ILALO(3), KSEC1(49), 24, KRET)
        CALL CSGNBT( ILALO(4), KSEC1(50), 24, KRET)
      ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),4,KBITS,24,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF10: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 47 - 50.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 55 - 66.'
        ENDIF
      ENDIF
C
C     When decoding values, if sign bit is 1, value is
C     negative.
C
      IF ( LDECODE ) THEN
        CALL DSGNBT( KSEC1(47), ILALO(1), 24, KRET)
        CALL DSGNBT( KSEC1(48), ILALO(2), 24, KRET)
        CALL DSGNBT( KSEC1(49), ILALO(3), 24, KRET)
        CALL DSGNBT( KSEC1(50), ILALO(4), 24, KRET)
      ENDIF
C
C     Octet 67: Number of tube to which operational forecast belongs
C     Octet 68: Number of tube to which control forecast belongs
C     Two 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(51),2,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF10: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 51 - 52.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 67 - 68.'
        ENDIF
      ENDIF
C
C     Octets 69-70: Height/pressure of level considered
C     Octets 71-72: Reference step considered
C     Octets 73-74: Radius of central cluster
C     Octets 75-76: Ensemble standard deviation
C     Octets 77-78: Distance of the tube extreme to the ensemble mean
C     Five 16-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(53),5,KBITS,16,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF10: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, elements 53 - 57.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octets 69 - 78.'
        ENDIF
      ENDIF
C
C     Octet 79: Number of forecasts belonging to the tube
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(58),1,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF10: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*) '      encoding section 1, element 58.'
        ELSE
          WRITE(GRPRSM,*) '      decoding section 1, octet 79.'
        ENDIF
      ENDIF
C
C*    The number of forecasts belonging to a cluster may vary,
C     but section 1 is kept a fixed length.
C
      INSPT = KNSPT + 255*8
C
C     Octets 80-(79+N): List of N ensemble forecast numbers
C     N 8-bit fields.
C
      N = KSEC1(58)
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(59),N,KBITS,8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,*) 'ECDEF10: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(GRPRSM,*)
     X  '      encoding section 1, element 59 onwards.'
        ELSE
          WRITE(GRPRSM,*)
     X '      decoding section 1, octet 80 onwards.'
        ENDIF
      ENDIF
C
      KNSPT = INSPT
C
C     On encoding, record the length of section 1.
C
      IF( LENCODE ) THEN
        LENS1 = 79 + 255
        CALL INXBIT(KGRIB,KLENG,KPLEN,LENS1, 1,KBITS,24,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,*) 'ECDEF10: Error reported by routine INXBIT:'
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
        WRITE(GRPRSM,*) 'ECDEF10: Section 9.'
        WRITE(GRPRSM,*) '        Output values set, KNSPT = ', KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF10')
      ELSE
        RETURN
      ENDIF
C
      END
