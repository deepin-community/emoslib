C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF4( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                   KPLEN, KPR, KRET)
C
C---->
C**** ECDEF4
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use of definition 4.
C     (Ocean extensions).
C
C**   Interface.
C     ----------
C
C    CALL ECDEF4( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KPR, KRET)
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
C     KPLEN      - Pointer to field containing length of Section 1.
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
C     local part of section 1 of Grib code, definition 4.
C
C
C     Externals.
C     ----------
C
C     INXBIT
C     ABORTX
C     CSGNBT
C     DSGNBT
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
C     Adapted from ECLOC1.F - simplified to handle just definition 4.
#include "ecdef4.h"
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      18th January 1995
C
C     Modifications.
C     --------------
C
C     J.D.Chambers     ECMWF       Jan 2000
C     Allow single 2-byte ensemble number for stream 1090 (ECMWF
C     ensemble seasonal forecasts)
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
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER IRET, ILENS1, INUM, LOOP, IP1, N
CC    INTEGER IZERO
      INTEGER ILALO
      DIMENSION ILALO(8)
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
C*    Section 2 .  Ocean extensions, definition 4.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( KSEC1(40).EQ.1090 ) THEN
C
C       If seasonal forecast data ...
C
C       Octet 50-51 : Forecast number.
C       One 16 bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),1,KBITS,16,HFUNC,KRET)
        IF (KRET.NE.0) GOTO 910
C
        IF( LDECODE ) KSEC1(43) = 0
C
      ELSE
C
C       Octet 50 : Ensemble forecast number
C       Octet 51 : Total number of forecasts in ensemble
C       Two 8-bit fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(42),2,KBITS, 8,HFUNC,KRET)
        IF( KRET.NE.0 ) GOTO 910
C
      ENDIF
C
C     Octet 52 : Flag showing existence of a post-auxiliary array
C     One 8-bit field.
C
      IF( LENCODE ) IP1 = 1
      CALL INXBIT(KGRIB,KLENG,KNSPT, IP1,1,KBITS,8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     -----------------------------------------------------------------|
C*    Section 3 .  Coordinate structure definition.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Octet 53 : Fundamental spatial reference system (Planet flag)
C     Octet 54 : Fundamental time reference
C     Octet 55 : Space unit flag (applies only if lengths NOT
C                otherwise labelled)
C     Octet 56 : Vertical coordinate definition (z)
C     Octet 57 : Horizontal coordinate definition (x,y)
C     Octet 58 : Time unit flag
C     Octet 59 : Time coordinate definition (t)
C     Seven 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(44),7,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     -----------------------------------------------------------------|
C*    Section 4 .  Mixed coordinates.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C*    Position definition.
C     Octet 60 : Mixed coordinate field flag
C     Octet 61 : Coordinate 1 flag  (usually time)
C     Octet 62 : Averaging flag
C     Three 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(51),3,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     Octets 63 - 66 : Position of level 1
C     Octets 67 - 70 : Position of level 2
C     Two 32-bit fields.
C
C     When coding data, set sign bit to 1, if value is negative.
C
      IF( LENCODE ) THEN
        CALL CSGNBT( ILALO(1), KSEC1(54), 32, KRET)
        CALL CSGNBT( ILALO(2), KSEC1(55), 32, KRET)
      ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),2,KBITS, 32,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     When decoding data, if sign bit is 1, value is negative.
C
      IF( LDECODE ) THEN
        CALL DSGNBT( KSEC1(54), ILALO(1), 32, KRET)
        CALL DSGNBT( KSEC1(55), ILALO(2), 32, KRET)
      ENDIF
C
C     Octet 71 : Coordinate 2 flag  (usually z-coordinate)
C     Octet 72 : Averaging flag
C     Two 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(56),2,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     Octets 73 - 76 : Position of level 1
C     Octets 77 - 80 : Position of level 2
C     Two 32-bit fields.
C
C     When coding data, set sign bit to 1, if value is negative.
C
      IF( LENCODE ) THEN
        CALL CSGNBT( ILALO(1), KSEC1(58), 32, KRET)
        CALL CSGNBT( ILALO(2), KSEC1(59), 32, KRET)
      ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),2,KBITS, 32,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     When decoding data, if sign bit is 1, value is negative.
C
      IF( LDECODE ) THEN
        CALL DSGNBT( KSEC1(58), ILALO(1), 32, KRET)
        CALL DSGNBT( KSEC1(59), ILALO(2), 32, KRET)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5 .  Grid definition.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
C     Octets 81 : Coordinate 3 flag (x-axis, usually longitude)
C     Octets 82 : Coordinate 4 flag (y-axis, usually latitude)
C     Two 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(60),2,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     Octets  83 - 86  : Coordinate 4 of first grid point
C     Octets  87 - 90  : Coordinate 3 of first grid point
C     Octets  91 - 94  : Coordinate 4 of last grid point
C     Octets  95 - 98  : Coordinate 3 of last grid point
C     Octets  99 - 102 : i-increment
C     Octets 103 - 106 : j-increment
C     Six 32-bit fields.
C
C     When coding data, set sign bit to 1, if value is negative.
C
      IF( LENCODE ) THEN
        CALL CSGNBT( ILALO(1), KSEC1(62), 32, KRET)
        CALL CSGNBT( ILALO(2), KSEC1(63), 32, KRET)
        CALL CSGNBT( ILALO(3), KSEC1(64), 32, KRET)
        CALL CSGNBT( ILALO(4), KSEC1(65), 32, KRET)
        CALL CSGNBT( ILALO(5), KSEC1(66), 32, KRET)
        CALL CSGNBT( ILALO(6), KSEC1(67), 32, KRET)
      ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),6,KBITS, 32,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     When decoding data, if sign bit is 1, value is negative.
C
      IF( LDECODE ) THEN
        CALL DSGNBT( KSEC1(62), ILALO(1), 32, KRET)
        CALL DSGNBT( KSEC1(63), ILALO(2), 32, KRET)
        CALL DSGNBT( KSEC1(64), ILALO(3), 32, KRET)
        CALL DSGNBT( KSEC1(65), ILALO(4), 32, KRET)
        CALL DSGNBT( KSEC1(66), ILALO(5), 32, KRET)
        CALL DSGNBT( KSEC1(67), ILALO(6), 32, KRET)
      ENDIF
C
C
C     Octet 107 : Flag for irregular grid coordinate list
C     Octet 108 : Flag for normal or staggered grid
C     Two 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(68),2,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     -----------------------------------------------------------------|
C*    Section 6 .  Auxiliary information.
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
C     Octet 109 : Flag for any further information
C     One 8-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(70),1,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C*    Auxiliary information.
C
C     Octet 110 : Number of entries in horizontal coordinate
C                 definition supplement.
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(71),1,KBITS, 8,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     Octets 111 - 112 : Number of entries in mixed coordinate
C                        definition.
C     Octets 113 - 114 : Number of entries in grid coordinate list
C     Octets 115 - 116 : Number of entries in auxiliary array
C     Three 16-bit fields .
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(72),3,KBITS, 16,HFUNC,KRET)
      IF( KRET.NE.0 ) GOTO 910
C
C     At this point length of Section 1 of Grib code is 116 octets.
C
      ILENS1 = 116
      INUM   = 75
C
C     -----------------------------------------------------------------|
C*    Section 7 .  (Optional) Supplements.
C     -----------------------------------------------------------------|
C
  700 CONTINUE
C
C     Horizontal coordinate supplement.
C
      DO LOOP = 1, KSEC1(71)
C
C       KSEC1(71) 32-bit fields.
C
C       When coding values, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) CALL CSGNBT( ILALO(1), KSEC1(INUM), 32, KRET)
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),1,KBITS,32, HFUNC,KRET)
        IF( KRET.NE.0 ) GOTO 910
C
C       When decoding values, if sign bit is 1, value is negative.
C
        IF( LDECODE ) CALL DSGNBT( KSEC1(INUM), ILALO(1), 32, KRET)
        ILENS1 = ILENS1 + 4
        INUM   = INUM + 1
C
      ENDDO
C
C     Mixed coordinate definition.
C
      DO LOOP = 1, KSEC1(72)
C
C       KSEC1(72) 32-bit fields.
C
C       When coding values, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) CALL CSGNBT( ILALO(1), KSEC1(INUM), 32, KRET)
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),1,KBITS, 32,HFUNC,KRET)
        IF( KRET.NE.0 ) GOTO 910
C
C       When decoding values, if sign bit is 1, value is negative.
C
        IF( LDECODE ) CALL DSGNBT( KSEC1(INUM), ILALO(1), 32, KRET)
        ILENS1 = ILENS1 + 4
        INUM   = INUM + 1
C
      ENDDO
C
C     Grid coordinate list.
C
      DO LOOP = 1, KSEC1(73)
C
C       KSEC1(73) 32-bit fields.
C
C       When coding values, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) CALL CSGNBT( ILALO(1), KSEC1(INUM), 32, KRET)
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,ILALO(1),1,KBITS, 32,HFUNC,KRET)
        IF( KRET.NE.0 ) GOTO 910
C
C       When decoding values, if sign bit is 1, value is negative.
C
        IF( LDECODE ) CALL DSGNBT( KSEC1(INUM), ILALO(1), 32, KRET)
        ILENS1 = ILENS1 + 4
        INUM   = INUM + 1
C
      ENDDO
C
C     Auxiliary array.
C
      IF( KSEC1(74).NE.0 ) THEN
C
C       KSEC1(74) 32-bit fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(INUM),KSEC1(74),KBITS,
     X             32,HFUNC,KRET)
        IF( KRET.NE.0 ) GOTO 910
        ILENS1 = ILENS1 + 4 * KSEC1(74)
        INUM   = INUM + KSEC1(74)
      ENDIF
C
C     Post-auxiliary array.
C
      IF( IP1.EQ.1 ) THEN
C
C       Integer count, N say, followed by (N-1) 32-bit fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(INUM),1,KBITS,
     X             32,HFUNC,KRET)
        IF( KRET.NE.0 ) GOTO 910
        N = KSEC1(INUM)
        IF( N.GT.1 ) THEN
          INUM   = INUM + 1
C
          CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC1(INUM),(N-1),KBITS,
     X               32,HFUNC,KRET)
          IF( KRET.NE.0 ) GOTO 910
          ILENS1 = ILENS1 + 4 * N
          INUM   = INUM + N - 1
        ELSE
          KSEC1(INUM) = 0
          ILENS1 = ILENS1 + 4
          KNSPT = KNSPT + 32
        ENDIF
      ELSE
        KSEC1(INUM) = 0
      ENDIF
C
C*    Insert length of section 1, if coding data.
C     One 24-bit field.
C
      IF( LENCODE ) THEN
        CALL INXBIT(KGRIB,KLENG,KPLEN,ILENS1,1,KBITS, 24,HFUNC,KRET)
        IF( KRET.NE.0 ) GOTO 910
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9 . Abort/return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 )
     X  WRITE(GRPRSM,*) 'ECDEF4: Section 9. On output, KNSPT  = ',KNSPT
C
      RETURN
C
  910 CONTINUE
      KRET = 2
      WRITE(GRPRSM,*) 'ECDEF4: Error reported by routine INXBIT.'
C
C     Abort if requested to do so when an error has been encountered.
C
      IF( IRET.EQ.0.AND.KRET.NE.0 ) THEN
        CALL ABORTX ('ECDEF4')
      ELSE
        RETURN
      ENDIF
C
      END
