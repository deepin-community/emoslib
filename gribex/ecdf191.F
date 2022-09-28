C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDF191(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KPLEN,KRET)
C
C---->
C**** ECDF191
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 191.
C
C**   Interface.
C     ----------
C
C     CALL ECDF191(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
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
C     KPLEN      - Pointer to field containing length of Section 1.
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
C     local part of section 1 of Grib code, definition 191.
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
#include "ecdf191.h"
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
C     Parameter(s)
C
      INTEGER JPBYTES
C                `-----> Number of bytes in an INTEGER
#ifdef INTEGER_8
      PARAMETER( JPBYTES = 8)
#else
      PARAMETER( JPBYTES = 4)
#endif
C
C     Subroutine arguments
C
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, JLOOP, N, M, ILENS1, N_KNSPT, ICOUNT, IBITS
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
      N_KNSPT = KNSPT
C
C     -----------------------------------------------------------------|
C*    Section 2 . Handle the local extension
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Octet 50 : Zero
C     Octet 51 : Zero
C     Two 8 bit fields.
C
      IF( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
          WRITE(*,*) '          encoding section 1, octet 50'
          GOTO 900
        ENDIF
      ELSE
        KSEC1(42) = 0
        KNSPT = KNSPT + 8
      ENDIF
C
      IF( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
          WRITE(*,*) '          encoding section 1, octet 51'
          GOTO 900
        ENDIF
      ELSE
        KSEC1(43) = 0
        KNSPT = KNSPT + 8
      ENDIF
C
C     Octet 52 : Format version major number
C     Octet 53 : Format version minor number
C     Two 8 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(44),1,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '      encoding section 1, element 44.'
        ELSE
          WRITE(*,*) '      decoding section 1, octet 52.'
        ENDIF
        GOTO 900
      ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),1,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '      encoding section 1, element 45.'
        ELSE
          WRITE(*,*) '      decoding section 1, octet 53.'
        ENDIF
        GOTO 900
      ENDIF
C
C     Octet 54 : Original sub-centre identifier
C     One 8 bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46),1,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '      encoding section 1, element 46.'
        ELSE
          WRITE(*,*) '      decoding section 1, octet 54.'
        ENDIF
        GOTO 900
      ENDIF
C
C     Octet 55-58 : Padding, set to zero
C     Four 8 bit fields.
C
      IF( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
          WRITE(*,*) '          encoding section 1, octet 55'
          GOTO 900
        ENDIF
      ELSE
        KSEC1(47) = 0
        KNSPT = KNSPT + 8
      ENDIF
C
      IF( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
          WRITE(*,*) '          encoding section 1, octet 56'
          GOTO 900
        ENDIF
      ELSE
        KSEC1(48) = 0
        KNSPT = KNSPT + 8
      ENDIF
C
      IF( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
          WRITE(*,*) '          encoding section 1, octet 57'
          GOTO 900
        ENDIF
      ELSE
        KSEC1(49) = 0
        KNSPT = KNSPT + 8
      ENDIF
C
      IF( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
          WRITE(*,*) '          encoding section 1, octet 58'
          GOTO 900
        ENDIF
      ELSE
        KSEC1(50) = 0
        KNSPT = KNSPT + 8
      ENDIF
C
C     Octets 59-60: Number of bytes of free format data descriptor
C     One 16 bit field
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(51),1,KBITS,16,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '      encoding section 1, element 51.'
        ELSE
          WRITE(*,*) '      decoding section 1, octets 59 - 60.'
        ENDIF
        GOTO 900
      ENDIF
C
C     Octets 61-(60+N) : Data bytes
C     N values; 8 bit values packed in integer array element
C
      N = KSEC1(51)
      ICOUNT = (N+JPBYTES-1)/JPBYTES
      IBITS  = JPBYTES*8
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(52),
     X            ICOUNT,KBITS,IBITS,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
        IF( LENCODE ) THEN
          WRITE(*,*) '    encoding section 1, elements 52 - (51+N/4).'
        ELSE
          WRITE(*,*) '    decoding section 1, octets 61 - (60+N).'
        ENDIF
        GOTO 900
      ENDIF
C
C     Octets (60+N) onwards: Padded with zeroes
C     The section will be padded with zeroes to make the overall length
C     of the section = 60 + 80*M for some M > 0.
C
      M = 60+(((ICOUNT*JPBYTES)+79)/80)*80
C
      DO JLOOP = (60+ICOUNT*JPBYTES+1), M
        IF( LENCODE ) THEN
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF( KRET.NE.0 ) THEN
            KRET = 2
            WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
            WRITE(*,*) '      encoding section 1, octets (56+N) onwards'
            GOTO 900
          ENDIF
        ELSE
          KNSPT = KNSPT + 8
        ENDIF
      ENDDO
C
C     Calculate section 1 total length using original and final bit
C     positions. NB packing started at octet 50.
C
      ILENS1 = 49 + (KNSPT - N_KNSPT)/8
C
C*    Insert length of section 1, if coding data.
C     One 24-bit field.
C
      IF( LENCODE ) THEN
        CALL INXBIT(KGRIB,KLENG,KPLEN,ILENS1,1,KBITS, 24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(*,*) 'ECDF191: Error reported by routine INXBIT:'
          WRITE(*,*) '         encoding length of section 1'
          GOTO 900
        ENDIF
      ENDIF
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
        CALL ABORTX('ECDF191')
      ELSE
        RETURN
      ENDIF
C
      END
