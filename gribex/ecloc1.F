C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECLOC1(HFUNC,KSEC1,KGRIB,KNSPT,KBITS,KRET)
C
C---->
C**** ECLOC1
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use of Section 1.
C
C**   Interface.
C     ----------
C
C     CALL ECLOC1(HFUNC,KSEC1,KGRIB,KNSPT,KBITS,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     HFUNC      - Requested function.
C                  'C' to code data.
C                  'D' to decode data.
C     KSEC1      - Array containing Grib Section 1 data.
C     KGRIB      - Array containing Grib coded data.
C     KNSPT      - Bit number after which insertion or
C                  extraction starts.
C     KBITS      - Number of bits in computer word.
C
C
C     Output Parameters.
C     ------------------
C
C     KSEC1      - Array containing Grib Section 1 data.
C
C     KGRIB      - Array containing Grib coded data.
C
C     KNSPT      - Number of bit after last one inserted or
C                              extracted.
C
C     KRET       - Return code.
C                  0   , No error encountered.
C                  1   , Invalid local use definition.
C                  2   , Error reported by routine INXBIT.
C                  3   , 3d or 4d analysis not implemented.
C
C     Method.
C     -------
C
C     Input data packed/unpacked in accordance with ECMWF usage of
C     local part of section 1 of Grib code.
C
C
C     Externals.
C     ----------
C
C     INXBIT  - insert/extract bit data
C     ABORTX  - abort program
C     CSGNBT  - encode sign bit
C     DSGNBT  - decode sign bit
C     ELDEFS  - encode ECMWF section 1, local usage definitions
C     DLDEFS  - decode ECMWF section 1, local usage definitions
#ifdef LITTLE_ENDIAN
C     SWAP4   - swap order of characters in the experiment version
#endif
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
C     None.
C
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      03:11:92.
C
C
C     Modifications.
C     --------------
C
C     J. Hennessy      ECMWF      21:05:93.
C     Additional parameter KPLEN added. Local usage definitions
C     3 and 4 added.
C
C     J.D.Chambers     ECMWF      18:10:93.
C     Local usage definition 5 added.
C
C     J.D.Chambers     ECMWF      12.05.94
C     ECMWF local usage definition 7 added.
C     (Sensitivity gradient/Trajectory forecast data).
C     Change to use CSGNBT and DSGNBT to code and decode sign bits
C
C     J.D.Chambers     ECMWF      02.06.94
C     Fix handling of local use section 1 for MARS labelling
C
C     J.D.Chambers     ECMWF      11.11.94
C     Fix overwrite of constant zero
C
C     J.D.Chambers     ECMWF      18.01.95
C     Use subroutine calls (ECDEF1, ... ) to handle ECMWF
C     section 1 local usage definitions.
C
C     J.D.Chambers     ECMWF      September 2004
C     Use ELDEFS and DLDEFS to handle ECMWF
C     section 1 local usage definitions.
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0 . Definition of variables. Data statements.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "grbcom.h"
#include "common/grprs.h"
C
C     Subroutine arguments
C
      CHARACTER*(*) HFUNC
      INTEGER KSEC1, KGRIB, KNSPT, KBITS, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Parameter(s)
C
      INTEGER JPBITS
#ifdef INTEGER_8
      PARAMETER ( JPBITS = 64 )
#else
      PARAMETER ( JPBITS = 32 )
#endif
C
C     Local variables
C
      INTEGER NUMBYTS, NUMVALS, IOFFSET
CC    INTEGER ILEN, LOOP, ITEMP(1000)
      INTEGER NUMBER
C
      LOGICAL LENCODE, LDEBUG
C
C     Function(s)
C
      INTEGER LDEFNUM
C
C     -----------------------------------------------------------------|
C*    Section 1 . Set initial values.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      LDEBUG = (NDBG.GE.1)
C
      IF( LDEBUG ) THEN
        WRITE(GRPRSM,*) 'ECLOC1: Section 1.'
        WRITE(GRPRSM,*) '         Input values used -'
        WRITE(GRPRSM,*) '         HFUNC  = ', HFUNC
        WRITE(GRPRSM,*) '         KNSPT  = ', KNSPT
      ENDIF
C
C     Reset return code to 0.
C
      KRET = 0
C
      LENCODE = ( HFUNC(1:1).EQ.'C' )
C
C     -----------------------------------------------------------------|
C*    Section 2 .  Check input parameters.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( LDEBUG ) WRITE(GRPRSM,*) 'ECLOC1: Section 2.'
C
      IOFFSET = 1 + (KNSPT / KBITS)
C
C     KWBC non-ensemble products have:
C       ksec1( 2) = 7     (originating centre = KWBC)
C       ksec1(22) = 98    (subcentre          = ECMWF)
C     For these, an ECMWF local definition applies.
C
      IF( LENCODE ) THEN
        NUMBER = KSEC1(2)*1000000 + KSEC1(22)*1000 + KSEC1(37)
        CALL ELDEFS(NUMBER,KSEC1(37),KGRIB(IOFFSET),NUMVALS,NUMBYTS)
        IF( NUMBYTS.LE.0 ) THEN
          KRET = 1
          GOTO 900
        ENDIF
        NUMBYTS = NUMBYTS + 40
        CALL ISEC1L(KGRIB,NUMBYTS)
        NUMBYTS = NUMBYTS - 40
      ELSE
        NUMBER = LDEFNUM(KSEC1(2),KSEC1(22),KGRIB(IOFFSET))
        CALL DLDEFS(NUMBER,KGRIB(IOFFSET),KSEC1(37),NUMVALS,NUMBYTS)
        IF( NUMBYTS.LE.0 ) THEN
          KRET = 1
          GOTO 900
        ENDIF
      ENDIF
      KNSPT = KNSPT + (NUMBYTS*8)
C
C     -----------------------------------------------------------------|
C*    Section 9 . Abort/return to calling routine. Format statements.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
