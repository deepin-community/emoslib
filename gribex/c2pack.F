C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2PACK ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,
     X                          KNSPT,  KBITS,  KLEN,   KREFGR, KLENGR,
     X                          KBITGR, KWORK,  KLWORK, KPOWER, KMXPWR,
     X                          KNBIFO, ODEBUG )
C
C---->
C**** C2PACK
C
C     Purpose.
C     --------
C
C     Packs end of GRIB section 4 (except final padding)
C     for second-order packing methods, and computes some descriptors.
C
C
C**   Interface.
C     ----------
C
C     KRET = C2PACK ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,  KNSPT,
C    X                KBITS,  KLEN,   KREFGR, KLENGR, KBITGR, KWORK,
C    X                KLWORK, KPOWER, KMXPWR, KNBIFO, ODEBUG )
C
C
C     Input Parameters.
C     ----------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C     KLENG      - Length of GRIB product array.
C     KNSPT      - Bit pointer for next value in GRIB product.
C     KBITS      - Number of bits per computer word.
C     KLEN       - Exact number of grid-points to handle.
C     KREFGR     - Work array, first-order values for each group.
C     KLENGR     - Work array, effective length of each group.
C     KBITGR     - Work array, bit number ("width") of each group.
C     KLWORK     - Length of work arrays.
C     KPOWER     - Auxilary array, containing precomputed powers of 2-1.
C     KMXPWR     - Maximum number of bits per original scaled value.
C     KNBIFO     - Number of BIts for coding First-Order values.
C     ODEBUG     - True for some debug printout.
C
C
C     Output Parameters.
C     -----------------
C
C     KDATA      - Used as work array for second-order values,
C                  and secondary bit-map (if any).
C     KSEC4      - Array of GRIB section 4 integer descriptors
C                  (completed).
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product (updated).
C     KREFGR     - Work array, re-used for variable width methods.
C     KBITGR     - Work array, re-used for variable width methods.
C     KWORK      - Work array, used for variable width methods.
C
C
C     Method.
C     -------
C
C     Follows WMO Manual of Codes.
C
C     Externals.
C     ----------
C
C     C2PKVW    - Packs 2nd-order values for variable width methods.
C     INXBIT    - Insert bits into GRIB product.
C     PRTBIN    - Transform integer value in printable "binary" format.
C     CSGNBT    - Codes an integer value and its sign bit.
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     --------
C
C     On entry, KNSPT points to the first bit of section 4 
C               in the GRIB product.
C     On exit,  KNSPT points to the first unused bit of section 4,
C               (or to the first bit of section 5, if unused bit count
C                is zero) in the GRIB product.
C
C     KDATA is overwritten. This is coherent with GRIBEX coding.
C
C         Detailed use of KSEC4 as input/output argument
C         ----------------------------------------------
C
C     KSEC4(4) set to 64 to indicate complex packing.
C     KSEC4(6)  "   " 16  "    "     presence of extended flags.
C
C     KSEC4(9) used to know secondary bit-map use (0=no,32=yes).
C     KSEC4(10)  "   "   "  constant/variable width (0=constant,16=var).
C     KSEC4(11)  "   "   "  for width value, if constant.
C     KSEC4(11) used to know width of spatial differencing descriptors.
C     KSEC4(11) set to width of lengths for general extended method,
C               for consistency with D2ORDR routine.
C     KSEC4(12) used to know general extended packing (0=no,8=yes).
C     KSEC4(13)   "   "   "  boustrophedonic ordering (0=no,4=yes).
C     KSEC4(14) (0 or 2) and KSEC4(15) (0 or 1) used to know spatial
C               differencing through their sum (0=no,1/2/3=order used).
C     KSEC4(16) used to know bias for spatial differencing.
C     KSEC4(17)   "   "   "  width of lengths for general extended
C                            method.
C     KSEC4(16) set to pointer to first-order values.
C     KSEC4(17)  "   "    "    "  2nd-order     "   .
C     KSEC4(18) set to number of first-order values.
C     KSEC4(19)  "   "    "   "  2nd-order     "   .
C     KSEC4(20) used to know width of groups.
C
C
C     Author.
C     -------
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C
C
C     Modifications.
C     _____________
C
C     J. Clochard, April 1998.
C     Update comments.
C     Introduce spatial differencing.
C
C     J. Clochard, September 1998.
C     Optimisation of bit-string encoding for variable width methods,
C     through routine C2PKVW.
C     Suppress dummy-argument KSEC1, and add KWORK/KPOWER/KMXPWR 
C     in calling syntax.
C
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
C
      INTEGER JP16SET
C
      PARAMETER ( JP16SET= 2**16-1 )
C                            `---> 65535    =   FFFF(hex)
C
C     Subroutine arguments
C
      INTEGER KLENP, KLENG, KNSPT, KBITS, KLEN, KLWORK, KNBIFO, KMXPWR
      INTEGER KDATA (KLENP), KSEC4 (*), KGRIB (KLENG)
      INTEGER KREFGR (KLWORK), KLENGR (KLWORK), KBITGR (KLWORK)
      INTEGER KWORK (KLWORK), KPOWER (0:KMXPWR)
C
      LOGICAL ODEBUG
C
C     Local variables.
C
      INTEGER ICOUNT, ILDSTA, IZERO, JGROUP, ILNGTH, IFILL, INSPTB
      INTEGER IRET, J, ILVARB, IBITGW, IREFER, IFLAGX, INSPTA
      INTEGER IPTRFO, IPTRSO, IFIROV, ISECOV, IRETFN, IAUXIL
#ifndef wmogrp2o
      INTEGER IEXTRA
#endif
      INTEGER IBITGS, IPTRGS, IORDER, IWIDSD, IBIAS, ILVARX
C
      INTEGER IWORK (3)
C
      INTEGER C2PKVW
      EXTERNAL C2PKVW
C
      CHARACTER YOPER*1
C
      LOGICAL LROW, LCTWID, LSECBI, LVECTP
      LOGICAL LGENXT
C
C     -----------------------------------------------------------------
C*    Section 1 . Initialisations.
C     -----------------------------------------------------------------
C
  100 CONTINUE
      ILVARX = 0
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      IRETFN = 0
      IZERO=0
      ILDSTA=21
      INSPTA=KNSPT
      YOPER='C'
      LSECBI=KSEC4(9).NE.0
#if (defined CRAY) || (defined FUJITSU)
      LVECTP=.TRUE.
#else
      LVECTP=.FALSE.
#endif
      LROW=.NOT.LSECBI.AND.KSEC4(12).EQ.0
      LGENXT=.NOT.LSECBI.AND.KSEC4(12).NE.0
      IORDER=KSEC4(14)+KSEC4(15)
      LCTWID=KSEC4(10).EQ.0
      IFIROV=KSEC4(18)
      IBITGW=KSEC4(20)
C
C     Enforce right values for flags, for further use by GRIBEX.
C
      KSEC4(4)=64
      KSEC4(6)=16
C
C     Compute length of "variable-length" descriptors.
C
      IF (LROW) THEN
        ILVARB=8*((8+IFIROV*IBITGW-1)/8)
      ELSEIF (LCTWID) THEN
        ILVARB=8+KLEN
      ELSEIF (LGENXT) THEN
        IBITGS=KSEC4(17)
C
        IF (IORDER.EQ.0) THEN
          ILVARX=4
        ELSE
          IWIDSD=KSEC4(11)
          IBIAS=KSEC4(16)
          ILVARX=5+(8+IWIDSD*(IORDER+1)-1)/8
        ENDIF
C
        ILVARB=8*(ILVARX+(8+IFIROV*IBITGW-1)/8)+IFIROV*IBITGS
C
C       IBITGS stored back in KSEC4(11), for consistency with D2ORDR.
C
        KSEC4(11)=IBITGS
      ELSE
        ILVARB=8*((8+IFIROV*IBITGW-1)/8)+KLEN
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Coding of fixed-length descriptors.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
C     The first 10 octets have already been coded in GRIBEX, or will
C     be in a further step of GRIBEX:
C     Length of section, flags+unused bit count, binary scale factor,
C     reference value.
C
C*    Octet 11 : Number of bits containing each first-order value.
C     One 8 bit field.
C
      KNSPT=KNSPT+10*8
      CALL INXBIT(KGRIB,KLENG,KNSPT,KNBIFO,1,KBITS, 8,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 20210
        WRITE(GRPRSM,FMT=9001) 'bit number of first-order values'
        GO TO 900
      ENDIF
C
C     Octets 12 - 13: Octet number of the start of first-order packed 
C                     data.
C     One 16-bit field.
C
      IPTRFO=1+ILDSTA+(8+ILVARB-1)/8
      KSEC4(16)=IPTRFO
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(16),1,KBITS,16,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 20220
        WRITE(GRPRSM,FMT=9001) 'pointer to first-order packed data'
        GO TO 900
      ENDIF
C
C     Octet 14: Extended flags.
C     One 8-bit field.
C
      IFLAGX=KSEC4(9)+KSEC4(10)+KSEC4(12)+KSEC4(13)+KSEC4(14)+KSEC4(15)
      CALL INXBIT(KGRIB,KLENG,KNSPT,IFLAGX,1,KBITS, 8,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 20230
        WRITE(GRPRSM,FMT=9001) 'extended flags'
        GO TO 900
      ENDIF
C
C     Octets 15 - 20: Octet number of the start of second-order packed 
C                     data, Number of first-order packed data,
C                     Number of second-order packed data.
C     Three 16-bit fields.
C
      IPTRSO=IPTRFO+(8+IFIROV*KNBIFO-1)/8
      ISECOV=KLEN-IORDER
C
      KSEC4(17)=IPTRSO
      KSEC4(18)=IFIROV
      KSEC4(19)=ISECOV
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(17),3,KBITS,16,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 20240
        WRITE(GRPRSM,FMT=9001) 'standard second-order descriptors'
        GO TO 900
      ENDIF
C
#ifndef wmogrp2o
C     Octet 21: Reserved by WMO, so should be zero in standard case.
C               Could be used for extra bits of group number.
C     One 8-bit field.
C
      IEXTRA=IFIROV/(JP16SET+1)
C
      IF (IEXTRA.NE.0) THEN
        CALL PRTBIN (IEXTRA,8,IAUXIL,IRET)
C
        IF (IRET.NE.0) THEN
          IAUXIL=IEXTRA
        ENDIF
C
        WRITE(GRPRSM,FMT=9255) IFIROV, JP16SET, IEXTRA, IAUXIL
      ENDIF
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IEXTRA,1,KBITS, 8,YOPER,IRET)
#else
C     Octet 21: Reserved by WMO. Set to zero.
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS, 8,YOPER,IRET)
#endif
C
      IF (IRET.NE.0) THEN
        IRETFN = 20250
        WRITE(GRPRSM,FMT=9001) 'reserved octet (section 4, 21st)'
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3. Coding of variable-length descriptors.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        CALL PRTBIN (IFLAGX,8,IAUXIL,IRET)
C
        IF (IRET.NE.0) THEN
          IAUXIL=IFLAGX
        ENDIF
C
        WRITE(GRPRSM,FMT=9300) KNBIFO, IAUXIL, IFIROV, ISECOV, IPTRFO,
     X                          IPTRSO
        WRITE(GRPRSM,FMT=9310) 'fixed', KNSPT
#ifndef ebug2o
      ENDIF
C
#endif
C
      IF (LCTWID) THEN
C
C     Octet 22: Common width of groups.
C     One 8-bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(11),1,KBITS,8,YOPER,IRET)
C
      ELSEIF (LGENXT) THEN
C
C     Octets 22 - 23: Width of widths, Width of lengths.
C     Two 8-bit fields.
C
        IWORK(1)=IBITGW
        IWORK(2)=IBITGS
        CALL INXBIT(KGRIB,KLENG,KNSPT,IWORK,2,KBITS,8,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 20310
          WRITE(GRPRSM,FMT=9001)
     X            'Extended descriptors, 1st-order values'
          GO TO 900
        ENDIF
C
C     Octets 24-25: Octet number of the start of group sizes (lengths).
C     One 16-bit field.
C
        IPTRGS = 1 + ILDSTA + ILVARX + (8+IFIROV*IBITGW-1)/8
#ifdef ebug2o
        PRINT *,'c2pack - IBITGW =',IBITGW,', IBITGS =',IBITGS,
     X          ', IPTRGS =',IPTRGS
#endif
        CALL INXBIT(KGRIB,KLENG,KNSPT,IPTRGS,1,KBITS,16,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 20315
          WRITE(GRPRSM,FMT=9001) 'General extended group size pointer'
          GO TO 900
        ELSEIF (IORDER.NE.0) THEN
C
C     Octet 26: Width of spatial differencing specific descriptors.
C     One 8-bit field.
C
          CALL INXBIT(KGRIB,KLENG,KNSPT,IWIDSD,1,KBITS,8,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 20316
            WRITE(GRPRSM,FMT=9001)
     X           'Width of spatial differencing descriptors'
            GO TO 900
          ELSEIF (IWIDSD.NE.0) THEN
C
C     Octet 27-onwards: first IORDER original values and diff. bias.
C     (IORDER+1) fields of IWIDSD bits.
C
C              Temporarily use KDATA(IORDER+1) for bias and its sign.
C
            IAUXIL=IORDER+1
            IFILL=KDATA(IAUXIL)
            CALL CSGNBT(KDATA(IAUXIL),IBIAS,IWIDSD,IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 20317
              WRITE(GRPRSM,FMT=9317) IRET
              GO TO 900
            ENDIF
C
            CALL INXBIT(KGRIB,KLENG,KNSPT,KDATA,IAUXIL,KBITS,IWIDSD,
     X                  YOPER,IRET)
C
            KDATA(IAUXIL)=IFILL
C
            IF (IRET.NE.0) THEN
              IRETFN = 20318
              WRITE(GRPRSM,FMT=9001)
     X           'First field value(s) (spatial differencing case)'
              GO TO 900
            ELSEIF (MOD (KNSPT,8).NE.0) THEN
C
C     Fill up with binary zeros till next octet boundary.
C
              IFILL=8*((8+KNSPT-1)/8)-KNSPT
              CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,IFILL,
     X                    YOPER,IRET)
C
              IF (IRET.NE.0) THEN
                IRETFN = 20319
                WRITE(GRPRSM,FMT=9001)
     X             'padding after spatial differencing descriptors'
                GO TO 900
              ENDIF
C
            ENDIF
C
          ENDIF
C
        ENDIF
C
C     Octets XX - XX+(IPTRGS-1): Group widths.
C     IFIROV fields of IBITGW bits.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KBITGR,IFIROV,KBITS,IBITGW,
     X              YOPER,IRET)
      ELSE
C
C     Octets 22 - 21+IFIROV: Group widths.
C     IFIROV fields of IBITGW bits.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KBITGR,IFIROV,KBITS,IBITGW,
     X              YOPER,IRET)
      ENDIF
C
      IF (IRET.NE.0) THEN
        IRETFN = 20320
        WRITE(GRPRSM,FMT=9001) 'inserting group width(s)'
        GO TO 900
      ELSEIF (MOD (KNSPT,8).NE.0) THEN
C
C     Fill up with binary zeros till next octet boundary.
C
        IFILL=8*((8+KNSPT-1)/8)-KNSPT
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,IFILL,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 20330
          WRITE(GRPRSM,FMT=9001) 'padding after group width(s)'
          GO TO 900
        ENDIF
C
      ENDIF
C
      IF (LSECBI) THEN
C
C     Secondary bit-map will be inserted later, after second-order
C     values processing, to use KDATA as work array. Retain pointer.
C
        INSPTB=KNSPT
        KNSPT=KNSPT+KLEN
C
        IF (MOD (KNSPT,8).NE.0) THEN
C
C     Fill up with binary zeros till next octet boundary.
C
          IFILL=8*((8+KNSPT-1)/8)-KNSPT
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,IFILL,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 20340
            WRITE(GRPRSM,FMT=9001) 'padding after secondary bit-map'
            GO TO 900
          ENDIF
C
        ENDIF
C
      ELSEIF (LGENXT) THEN
C
C     Octets IPTRGS - (IPTRFO-1): General extended group sizes(lengths).
C     IFIROV fields of IBITGS bits.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KLENGR,IFIROV,KBITS,IBITGS,
     X              YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 20350
          WRITE(GRPRSM,FMT=9001) 'General extended group sizes'
          GO TO 900
        ELSEIF (MOD (KNSPT,8).NE.0) THEN
C
C     Fill up with binary zeros till next octet boundary.
C
          IFILL=8*((8+KNSPT-1)/8)-KNSPT
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,IFILL,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 20360
            WRITE(GRPRSM,FMT=9001)
     X            'padding after General extended group sizes'
            GO TO 900
          ENDIF
C
        ENDIF
C
      ENDIF
C
      IF (KNBIFO.NE.0) THEN
C
C     Octets IPTRFO - (IPTRSO-1): First-order values (reference values
C                                                    of groups).
C     IFIROV fields of KNBIFO bits.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KREFGR,IFIROV,KBITS,KNBIFO,
     X              YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 20370
          WRITE(GRPRSM,FMT=9001)
     X         'first-order values (reference values of groups)'
          GO TO 900
        ELSEIF (MOD (KNSPT,8).NE.0) THEN
C
C     Fill up with binary zeros till next octet boundary.
C
          IFILL=8*((8+KNSPT-1)/8)-KNSPT
          CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS,IFILL,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 20380
            WRITE(GRPRSM,FMT=9001) 'padding after first-order values'
     S//' of groups.'
            GO TO 900
          ENDIF
C
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 4. Coding of data (second-order values).
C     -----------------------------------------------------------------
C
  400 CONTINUE
C
C     Octets IPTRSO - onwards: Second-order values (deviations from
C                                                  groups references).
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9310) 'variable', KNSPT
#ifndef ebug2o
      ENDIF
C
#endif
      ICOUNT=IORDER
C
      IF (LCTWID) THEN
C
C                      Constant width packing.
C
        IF (KSEC4(11).NE.0) THEN
C
          DO 412 JGROUP=1,IFIROV
C
          ILNGTH=KLENGR(JGROUP)
          IREFER=KREFGR(JGROUP)
C
          DO 411 J=1,ILNGTH
          KDATA(ICOUNT+J)=KDATA(ICOUNT+J)-IREFER
  411     CONTINUE
C
          ICOUNT=ICOUNT+ILNGTH
C
  412     CONTINUE
C
C       Effective coding of second-order values, once for all groups.
C
          CALL INXBIT(KGRIB,KLENG,KNSPT,KDATA,KLEN,KBITS,
     X                KSEC4(11),YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 20410
            WRITE(GRPRSM,FMT=9001)
     X   'second-order values (deviations from 1st-order values)'
            GO TO 900
          ENDIF
C
        ELSE
          ICOUNT=KLEN
        ENDIF
C
      ELSE
C
C             Packing method(s) with variable width.
C
        IRETFN = C2PKVW ( KDATA,  KLENP,  KGRIB,  KLENG,  KNSPT,
     X                    KBITS,  KLEN,   KREFGR, KLENGR, KBITGR,
     X                    KWORK,  KLWORK, IFIROV, ICOUNT, KPOWER,
     X                    KMXPWR, LROW,   LVECTP, ODEBUG )
C
        IF (IRETFN.NE.0) THEN
          GOTO 900
        ENDIF
C
      ENDIF
C
      IF (ICOUNT.NE.KLEN) THEN
        IRETFN = 20420
        WRITE(GRPRSM,FMT=9420) ICOUNT, KLEN
        GO TO 900
      ENDIF      
C
C     KNSPT pointer is intentionally left as is:
C     GRIBEX will use its value back to compute unused bit count...
C     ...no adjustment to nearest octet boundary here, then.
C
C     -----------------------------------------------------------------
C*    Section 5. Compute and code secondary bit-map, if any.
C     -----------------------------------------------------------------
C
  500 CONTINUE
C
      IF (LSECBI) THEN
C
#ifndef ebug2o
        IF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9510) 'Coding secondary bit-map', INSPTB
#ifndef ebug2o
        ENDIF
C
#endif
C
        DO 511 J=1,KLEN
        KDATA(J)=0
  511   CONTINUE
C
        ICOUNT=0
C
        DO 512 JGROUP=1,IFIROV
C
        ILNGTH=KLENGR(JGROUP)
        KDATA(ICOUNT+1)=1
        ICOUNT=ICOUNT+ILNGTH
C
  512   CONTINUE        
C
C       Effective coding of secondary bit-map, once for all groups.
C
        CALL INXBIT(KGRIB,KLENG,INSPTB,KDATA,KLEN,KBITS,
     X              1,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 20510
          WRITE(GRPRSM,FMT=9001) 'secondary bit-map'
          GO TO 900
#ifdef ebug2o
        ELSE
#else
        ELSEIF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9510) 'Secondary bit-map encoded', INSPTB
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      C2PACK = IRETFN
#ifndef ebug2o
C
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9900) IRETFN, KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      RETURN
C
 9001 FORMAT (' C2PACK: Problem inserting ',A,'.')
C
 9100 FORMAT (' C2PACK: Function start, KNSPT =',I10,'.')
#ifndef wmogrp2o
C
 9255 FORMAT (' C2PACK: Group number',I8,' exceeds',I6,'.',/,TR10,
     X        'extra bits stored in 21st octet of section 4 (',I3,
     X        '=>',I8.8,').')
#endif
C
 9300 FORMAT (' C2PACK: KNBIFO =',I3,', IFLAGX =',I9.8,
     X        ', IFIROV =',I8,', ISECOV =',I8,'.',/,
     X        ' C2PACK: IPTRFO =',I8,', IPTRSO =',I8,'.')
 9310 FORMAT (' C2PACK: KNSPT after "',A,'-length" descriptors =',
     X        I10,'.')
 9317 FORMAT (
     X' C2PACK: Bias sign management problem, CSGNBT return is',I5,'.')
C
 9420 FORMAT (' C2PACK: Mismatch: sum of group lengths =',I8,
     X        ', expected = ',I8,'.')
C
 9510 FORMAT (' C2PACK: ',A,', bit pointer =',I10,'.')
C
 9900 FORMAT (' C2PACK: Function return code =',I6,', KNSPT =',I10,'.')
C
      END
