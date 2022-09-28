C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF19(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
C
C---->
C**** ECDEF19
C
C     Purpose.
C     --------
C     GRIB coding/decoding of ECMWF local use definition 19.
C
C     Interface.
C     ----------
C     CALL ECDEF19(HFUNC,KSEC1,KGRIB,KLENG,KNSPT,KBITS,KRET)
C
C
C     Input Parameters.
C     -----------------
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
C     Input data packed/unpacked in accordance with ECMWF usage of
C     local part of section 1 of Grib code, definition 19.
C
C
C     Externals.
C     ----------
C     INXBIT
C     ABORTX
C
C
C     Reference.
C     ----------
C     WMO Manual On Codes for Grib Code.
C
C
C     Comments.
C     ---------
C     None
C
C
#include "ecdef19.h"
C
C     Author.
C     -------
C     J.D.Chambers     ECMWF       October 2002
C
C
C     Modifications.
C     --------------
C     None
C
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
C     Subroutine arguments
C
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, IZERO, LENS1, INSPT, IPLEN, ILOOPO
CC    INTEGER NCLIMON, NEXT, ILOOPI, IFULL
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
C*    Section 2 . Handle fixed part of definition 19.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Octet 50 : Zero, for compatibility with MARS labelling
C
      IF ( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
      ELSE
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),1,KBITS,8,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
      ENDIF
C
C     Octet 51 : Ensemble size
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(43),1,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
C
C     Octet 52 : Power of 10 used to scale climate weight = F, say
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(44),1,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
C
C     Octets 53-56 : Climate weight * 10**F, stored as an integer
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(45),1,KBITS,32,HFUNC,KRET)
      IF( KRET.NE.0 ) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 3 . Handle climate month descriptions
C     ------------------------------------------------------------------
C
  300 CONTINUE
C
C     Octets 57-59 : First month used to build climate month 1, YYYYMM
C     Octets 60-62 : Last month used to build climate month 1, YYYYMM
C     Octets 63-65 : First month used to build climate month 2, YYYYMM
C     Octets 66-68 : Last month used to build climate month 2, YYYYMM
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(46),1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(47),1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(48),1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(49),1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
C
C     Handle padding for unused bytes
C
      IF( LENCODE ) THEN
        DO ILOOPO = 69, 80
          IZERO = 0
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,8,HFUNC,KRET)
          IF( KRET.NE.0 ) THEN
            KRET = 2
            WRITE(GRPRSM,9004)
          ENDIF
        ENDDO
C
      ELSE
        KNSPT = KNSPT + 8*12
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 4 . Handle length of section 1
C     ------------------------------------------------------------------
C
  400 CONTINUE
C
      LENS1 = 80
C
      IF( LENCODE ) THEN
        CALL INXBIT(KGRIB,KLENG,IPLEN,LENS1,1,KBITS,24,HFUNC,KRET)
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
        CALL INXBIT(KGRIB,KLENG,IPLEN,LENS1,1,KBITS,24,HFUNC,KRET)
        IF( KRET.NE.0 ) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
        KNSPT = 8*8 + LENS1*8
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
        CALL ABORTX ('ECDEF19')
      ELSE
        RETURN
      ENDIF
C
 9004 FORMAT (' ECDEF19: Error reported by routine INXBIT.')
C
      END
