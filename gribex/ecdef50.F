C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF50(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
C
C---->
C**** ECDEF50
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 50.
C
C**   Interface.
C     ----------
C
C     CALL ECDEF50(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
C
C
C     Input
C     -----
C     HFUNC      - Requested function.
C                  'C' to code data.
C                  'D' to decode data.
C     KSEC1      - Array containing Grib Section 1 data.
C     KGRIB      - Array containing Grib coded data.
C     KLENG      - Length (words) of KGRIB.
C     KNSPT      - Bit number after which insertion/extraction starts.
C     KBITS      - Number of bits in computer word.
C     KRET       - Response to error indicator.
C                  0        , Abort if error encountered.
C                  Non-zero , Return to calling routine
C                             even if error encountered.
C
C     Output
C     ------
C     KSEC1      - Array containing Grib Section 1 data.(Updated)
C     KGRIB      - Array containing Grib coded data.(Updated)
C     KNSPT      - Number of bit after last one inserted/extracted.
C                  (Updated)
C     KRET       - Return code.
C                  0   , No error encountered.
C                  2   , Error reported by routine INXBIT.
C
C
C     Method.
C     -------
C     Input data packed/unpacked in accordance with ECMWF usage of
C     local part of section 1 of Grib code, definition 50.
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
#include "ecdef50.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      December 2001
C
C
C     Modifications.
C     --------------
C
C     None
C
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0 . Definition of variables. Data statements.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
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
      INTEGER IRET, IZERO, JLOOP
CC    INTEGER ITEMP, IRETA
C
      LOGICAL LENCODE, LDECODE
C
C     -----------------------------------------------------------------|
C*    Section 1 . Set initial values.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IRET = KRET
      LENCODE = ( HFUNC.EQ.'C' )
      LDECODE = .NOT. LENCODE
C
C     -----------------------------------------------------------------|
C*    Section 2 . Handle the local extension
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Octet 50 : Number
C     Octet 51 : Total
C     Octet 52 : Model identifier
C     Three 8 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),3,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDEF50: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '      encoding section 1, elements 42 - 44.'
        ELSE
          WRITE(*,*) '      decoding section 1, octets 50 - 52.'
        ENDIF
        GOTO 900
      ENDIF
C
C     Octets 53-56: Latitude of North-west corner of area
C     Octets 57-60: Longitude of North-west corner of area
C     Octets 61-64: Latitude of South-east corner of area
C     Octets 65-68: Longitude of South-east corner of area
C     Four 32 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),4,KBITS,32,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDEF50: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '      encoding section 1, elements 45 - 48.'
        ELSE
          WRITE(*,*) '      decoding section 1, octets 53 - 68.'
        ENDIF
        GOTO 900
      ENDIF
C
C     Octets 69-116 : Reserved for ECMWF additions.
C     Octet 69 : Original parameter number
C     Octet 70 : Original parameter table number
C     Octets 71-116 : Reserved for ECMWF additions.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(49),2,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDEF50: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '      encoding section 1, elements 49 - 50.'
        ELSE
          WRITE(*,*) '      decoding section 1, octets 69 - 70.'
        ENDIF
        GOTO 900
      ENDIF
C
      IF( LENCODE ) THEN
        DO JLOOP = 71, 116
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF( KRET.NE.0 ) THEN
            KRET = 2
            WRITE(*,*) 'ECDEF50: Error reported by routine INXBIT:'
            WRITE(*,*) '      encoding section 1, octets 71 - 116'
            GOTO 900
          ENDIF
        ENDDO
      ELSE
        DO JLOOP = 51, 60
          KSEC1(JLOOP) = 0
        ENDDO
        KNSPT = KNSPT + 46*8
      ENDIF
C
C     Octets 117-300 : Optional data.
C     Forty-six 32 bit fields.
C
      DO JLOOP = 61, 106
        CALL INXBIT(
     X    KGRIB,KLENG,KNSPT,KSEC1(JLOOP),1,KBITS,32,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDEF50: Error reported by routine INXBIT:'
          IF( LENCODE ) THEN
            WRITE(*,*) '      encoding section 1, elements 61 - 106.'
          ELSE
            WRITE(*,*) '      decoding section 1, octets 117 - 300.'
          ENDIF
          GOTO 900
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
      IF( (IRET.EQ.0).AND.(KRET.NE.0) ) THEN
        CALL ABORTX('ECDEF50')
      ELSE
        RETURN
      ENDIF
C
      END
