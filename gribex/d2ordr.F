C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

       INTEGER FUNCTION D2ORDR ( KDATA, KLENP, KSEC1, KSEC2, KSEC3, 
     X                          KSEC4, KGRIB, KLENG, KNSPT, KBITS,
     X                          HOPER, KLEN4, KNIL,  PREF , PSCALE,
     X                          KBMAP, KVALS, KDEBUG )
C
C---->
C**** D2ORDR
C
C     Purpose.
C     --------
C
C     Decode GRIB section 4 for complex packing of grid-point
C     values ("second-order packing").
C
C
C**   Interface.
C     ----------
C
C     KRET = D2ORDR ( KDATA, KLENP, KSEC1, KSEC2, KSEC3, KSEC4,
C    X                KGRIB, KLENG, KNSPT, KBITS, HOPER, KLEN4,
C    X                KNIL,  PREF, PSCALE, KBMAP, KVALS, KDEBUG )
C
C
C     Input Parameters.
C     -----------------
C
C     KLENP      - Length of array KDATA.
C     KSEC1      - Array of GRIB section 1 integer descriptors.
C     KSEC2      - Array of GRIB section 2 integer descriptors.
C     KSEC3      - Array of GRIB section 3 integer descriptors.
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C                  (preset to zero, and already decoded for (3:6)
C     KGRIB      - GRIB product array.
C     KLENG      - Length of GRIB product array.
C     KNSPT      - Bit pointer for next value in GRIB product.
C     KBITS      - Number of bits per computer word.
C     HOPER      - Requested function:
C                  'D' or 'R' to decode the whole field,
C                  'X' to decode only a few gridpoints (as described
C                   in KSEC4(34:42),
C                  'J' to decode only section 4 descriptors.
C     KLEN4      - Length (octets) of section 4.
C     KNIL       - Unused bit count at end of section 4.
C     KBMAP      - Bit pointer for start of explicit primary bit-map
C                  (if any).
C     KVALS      - Number of bits in primary bit-map.
C     KDEBUG     - >0 for some debug printout.
C
C
C     Output Parameters.
C     -----------------
C
C     KDATA      - Array of normalized values.
C     KSEC4      - Array of GRIB section 4 values (completed).
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product (updated).
C     PREF       - Reference value of field.
C     PSCALE     - Scale factor (ready to use) of field.
C
C
C     Method.
C     -------
C
C     Follows WMO Manual of Codes, and also DSECT4 structure.
C
C
C     Externals.
C     ----------
C
C     INXBIT    - Extract bits from GRIB product.
C     DECFP2    - Decode from IBM floating point format.
C     PRTBIN    - Print binary value in character format.
C     C2BITW    - Computes bit width of a positive integer value.
C     DSGNBT    - Decodes an integer value and its sign bit.
C     MAXMNI    - Computes extrema of integer array.
C     D2ROSD    - Rebuild original values from spatial differencing.
C     REVERO    - Reverse order of values in even rank rows.
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
C     Update comments, and add more consistency with C2ORDR source code.
C     Inhibit REVERO call for function 'X'.
C     Introduce spatial differencing.
C
C     J. Clochard, September 1998.
C     Optimize management of primary bit-map in row by row packing case.
C     Fixes behaviour for HOPER='J' (KDATA must not be used)
C     and HOPER='X' (KDATA minimum length is KSEC4(34), not full-length.
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
      INTEGER JP15BIT, JP16SET, JPWS1O, JPXMAX, JPXPWR, JPWS2O
C
      PARAMETER ( JP15BIT= 2**15 )
C                            `---> 32768    =   8000(hex)
      PARAMETER ( JP16SET= 2**16-1 )
C                            `---> 65535    =   FFFF(hex)
C     JPWS1O ===> Size of group work arrays.
C     JPXMAX ===> Maximum number of points for 'X' function.
C     JPXPWR ===>    "      "    "  bits number per value/descriptor.
C     JPWS2O ===> Size of work array IWORK2
C                 (must be strictly bigger than JPWS1O).
C
C     IWORK2 array is used to unpack:
C     -group sizes, in case of general extended packing
C                   (JPWS1O last values);
C     -secondary bit-map, if any;
C     -primary bit-map, in case of row by row packing.
C
C     These cases are exclusive.
C
#ifdef JBPW_64
      PARAMETER ( JPWS1O= 5000, JPWS2O= 20000, JPXMAX= 4, JPXPWR= 47 )
#else
      PARAMETER ( JPWS1O= 5000, JPWS2O= 20000, JPXMAX= 4, JPXPWR= 31 )
#endif
C
C     Subroutine arguments
C
      INTEGER KLENP, KLENG, KNSPT, KBITS, KLEN4, KNIL, KBMAP, KVALS
      INTEGER KDEBUG
      INTEGER KDATA (KLENP), KSEC1 (*), KSEC2 (*), KSEC4 (*), KGRIB (*)
      INTEGER KSEC3 (*)
C
      CHARACTER HOPER * (*)
C
      REAL PREF, PSCALE
C
C     Local variables.
C
      INTEGER IRET, IRETA, IRETB, IEXP, IMANT, IFLAGX, IAUXIL, INDKS4
      INTEGER IFIROV, ISECOV, ISCALE, IPTRSO, ILEN, INROWS, J, IRETFN
      INTEGER IPTRFO, IWIDTH, IPTRBM, ILAUXS, IPTRB0, ILNGTH, IRANG
      INTEGER IBITGW, IAUXIS, INBSEQ, JSEQ, IGROFF, IGRNBR, JGROUP
      INTEGER IREFLN, ICOUNT, IMAXVN, INPEXT, INCR, IEXTOR, IVALS
      INTEGER ISKIP, JEXT, IEXT, ICNEXT, INSPTA, IPTRGW, INVALS, ICOUNG
      INTEGER INDROW, INDCOL, IVALSC, IPTRFN, IPTRSN, IVALK4, IREFER
      INTEGER IPRWID, ICOUNS, ILNGTS, INDEXX, INDEXY, JROW, ISEQ
#ifdef ebug2o
      INTEGER IMAXVC
#endif
      INTEGER IBITGS, IPTRGS, IORDER, IWIDSD, IBIAS
#ifndef wmogrp2o
      INTEGER IEXTRA
#endif
C
      INTEGER IWIDGR (JPWS1O), IWORK2 (JPWS2O), IFIRST (JPWS1O)
      INTEGER IRGEXT (JPXMAX+1), INDXOR (JPXMAX), ILENGR (JPWS1O)
      INTEGER IPOWER (0:JPXPWR)
      INTEGER IWORK (3)
C
      CHARACTER YOPER*1
C
      LOGICAL LROW, LQUASI, LCTWID, LSECBI, LPRIBI, LDEBUG, L1CALL
      LOGICAL LGENXT, LBOUST, LPRCLN, LVECTD
C
      INTEGER REVERO, D2ROSD
      EXTERNAL REVERO, D2ROSD
      INTEGER C2BITW
      EXTERNAL C2BITW
C
      SAVE L1CALL, IPOWER
C
      DATA L1CALL / .TRUE. /
C
C     -----------------------------------------------------------------
C*    Section 1 . Decode octets 5 to 13.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      IPTRB0 = 0
      IAUXIS = 0
      ILEN = 0
      IRETFN = 0
      YOPER='D'
      INSPTA=KNSPT
      LDEBUG=KDEBUG.GE.1
C
      IF (L1CALL) THEN
C
        DO 111 J=0,JPXPWR-1
        IPOWER(J)=2**J-1
  111   CONTINUE
#ifdef JBPW_64
        IPOWER(JPXPWR)=140737488355327
#else
        IPOWER(JPXPWR)=2147483647
#endif
C
        L1CALL=.FALSE.
C
      ENDIF
C
#ifndef ebug2o
      IF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) HOPER(:1),KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      KNSPT=KNSPT+4*8
C
C*    Octets 5 - 6 : Scale factor.
C     One 16 bit field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,ISCALE,1,KBITS, 16,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 17110
        WRITE(GRPRSM,FMT=9001) 'scale factor'
        GO TO 900
      ENDIF
C
C     Make negative if sign bit set
      IF ( ISCALE .GT. JP15BIT ) THEN
        ISCALE = ISCALE - JP15BIT
        ISCALE = -ISCALE
      ENDIF
      PSCALE = 2.0**ISCALE
#ifdef ebug2o
      WRITE(GRPRSM,FMT='('' D2ORDR - Binary scale factor ='',
     X       I20,TR1,F30.20,''.'')') ISCALE,PSCALE
#endif
C
C*    Octets 7 - 10 : Reference value.
C     One 8 bit and one 24 bit field.
C
C     Extract reference value exponent and mantissa.
      CALL INXBIT(KGRIB,KLENG,KNSPT,IEXP,1,KBITS, 8,YOPER,IRETA)
      CALL INXBIT(KGRIB,KLENG,KNSPT,IMANT,1,KBITS, 24,YOPER,IRETB)
      IRET = IRETA + IRETB
C
      IF (IRET.NE.0) THEN
        IRETFN = 17120
        WRITE(GRPRSM,FMT=9001) 'reference value'
        GO TO 900
      ENDIF
C
C     Decode the reference value
      CALL DECFP2 (PREF,IEXP,IMANT)
C
C*    Octet 11 : Number of bits containing each first-order value.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(2),1,KBITS, 8,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 17130
        WRITE(GRPRSM,FMT=9001) 'bit number of first-order values'
        GO TO 900
      ELSEIF (KSEC4(2).GT.JPXPWR) THEN
        IRETFN = 17131
        WRITE(GRPRSM,FMT=9002) 'Bit number (of first-order values)',
     X                          JPXPWR
        GO TO 900
#ifdef ebug2o
      ELSE
#else
      ELSEIF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9135) KSEC4(2)
      ENDIF
C
C     Octets 12 - 13: Octet number of the start of first-order packed 
C                     data.
C     One 16-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IPTRFO,1,KBITS,16,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 17140
        WRITE(GRPRSM,FMT=9001) 'pointer to first-order packed data'
        GO TO 900
#ifdef ebug2o
      ELSE
#else
      ELSEIF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9150) PREF, IPTRFO, KNSPT
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Decode octets 14 to 21.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
C     Octet 14: Extended flags.
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IFLAGX,1,KBITS, 8,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 17210
        WRITE(GRPRSM,FMT=9001) 'extended flags'
        GO TO 900
#ifdef ebug2o
      ELSE
#else
      ELSEIF (LDEBUG) THEN
#endif
        CALL PRTBIN (IFLAGX,8,IAUXIL,IRET)
C
        IF (IRET.NE.0) THEN
          IAUXIL=IFLAGX
        ENDIF
C
        WRITE(GRPRSM,FMT=9210) IAUXIL, KNSPT
      ENDIF
C
C         R------- only bit 1 is reserved.
C         -0------ single datum at each grid point.
C         -1------ matrix of values at each grid point.
C         --0----- no secondary bit map.
C         --1----- secondary bit map present.
C         ---0---- second order values have constant width.
C         ---1---- second order values have different widths.
C         ----0--- no general extended second order packing.
C         ----1--- general extended second order packing used.
C         -----0-- standard field ordering in section 4.
C         -----1-- boustrophedonic ordering in section 4.
C         ------00 no spatial differencing used.
C         ------01 1st-order spatial differencing used.
C         ------10 2nd-order    "         "         " .
C         ------11 3rd-order    "         "         " .
C
      IF (IFLAGX.GE.128) THEN
        IRETFN = 17220
        WRITE(GRPRSM,FMT=9220)
        GO TO 900
      ENDIF
C
      IAUXIL = 64
      INDKS4 = 6
C
      IVALK4 = 8
C
      DO 201 J = 2, IVALK4
C
      IF (J.EQ.5) THEN
        INDKS4 = INDKS4 + 1
      ENDIF
C      
      IF (IFLAGX.GE.IAUXIL) THEN
        KSEC4(INDKS4+J) = IAUXIL
        IFLAGX = IFLAGX - IAUXIL
      ENDIF
C
      IAUXIL=IAUXIL/2
  201 CONTINUE
C
#ifndef ebug2o
      IF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9225) (KSEC4(J),J=3,10)
#ifndef ebug2o
      ENDIF
#endif
C
      IF (IFLAGX.NE.0) THEN
        IRETFN = 17230
        WRITE(GRPRSM,FMT=9230)
        GO TO 900
      ELSEIF (KSEC4(8).NE.0) THEN
        IRETFN = 17240
        WRITE(GRPRSM,FMT=9240)
        GO TO 900
      ENDIF
C
      LPRIBI=MOD (KSEC1(5),128).GE.64
      LSECBI=KSEC4(9).NE.0
      LROW=.NOT.LSECBI.AND.KSEC4(12).EQ.0
      LGENXT=.NOT.LSECBI.AND.KSEC4(12).NE.0
      LBOUST=KSEC4(13).NE.0
      IORDER=KSEC4(14)+KSEC4(15)
C
      IF (IORDER.NE.0) THEN
C
        IF (HOPER.EQ.'X') THEN
          IRETFN = 17244
          WRITE(GRPRSM,FMT=9244) HOPER(:1)
          GO TO 900
        ELSEIF (.NOT.LGENXT) THEN
          IRETFN = 17245
          WRITE(GRPRSM,FMT=9245)
          GO TO 900
        ENDIF
C
      ENDIF
C
      LQUASI=KSEC2(17).EQ.1
      LCTWID=KSEC4(10).EQ.0
C
C     Octets 15 - 20: Octet number of the start of second-order packed 
C                     data, Number of first-order packed data,
C                     Number of second-order packed data.
C     Three 16-bit fields.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(17),3,KBITS,16,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 17250
        WRITE(GRPRSM,FMT=9001) 'standard second-order descriptors'
        GO TO 900
      ENDIF
C
      IPTRSO=KSEC4(17)
      IFIROV=KSEC4(18)
      ISECOV=KSEC4(19)
C
#ifndef wmogrp2o
C     Octet 21: Reserved by WMO, so should be zero in standard case.
C               Could be used for extra bits of group number.
C     One 8-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IEXTRA,1,KBITS, 8,YOPER,IRET)
C
      IF (IRET.NE.0) THEN
        IRETFN = 17255
        WRITE(GRPRSM,FMT=9001) '21st octet of section 4'
        GO TO 900
      ELSEIF (IEXTRA.NE.0) THEN
        IFIROV=IFIROV+(JP16SET+1)*IEXTRA
        CALL PRTBIN (IEXTRA,8,IAUXIL,IRET)
C
        IF (IRET.NE.0) THEN
          IAUXIL=IEXTRA
        ENDIF
C
        WRITE(GRPRSM,FMT=9255) IAUXIL, IEXTRA, IFIROV
      ENDIF
C
      KSEC4(18)=IFIROV
C
#else
C     Octet 21: Reserved by WMO.
C     One 8-bit field. Skipped.
C
      KNSPT = KNSPT + 8
C
#endif
      IF (KSEC1(5).GE.128) THEN
C
C     Cross checks between descriptors decoded above,
C     and section 2 descriptors previously decoded.
C
        IF (MOD (KSEC2(11),64).LT.32) THEN
          INROWS=KSEC2(3)
        ELSE
          INROWS=KSEC2(2)
        ENDIF
C
        IF (LROW.AND.((LPRIBI.AND.INROWS.LT.IFIROV).OR.
     X                (.NOT.LPRIBI.AND.INROWS.NE.IFIROV))) THEN
          IRETFN = 17260
          WRITE(GRPRSM,FMT=9260)
          GO TO 900
        ELSEIF (LQUASI) THEN
          IREFLN=22
          ILEN=0
C
          DO 211 J=1,INROWS
          ILEN=ILEN+KSEC2(IREFLN+J)
  211     CONTINUE
C
        ELSE
          ILEN=KSEC2(2)*KSEC2(3)
          ILNGTH=ILEN/INROWS
        ENDIF
C
        IF (LPRIBI.AND.ISECOV.GT.ILEN.AND.HOPER.NE.'J') THEN
          IRETFN = 17265
          WRITE(GRPRSM,FMT=9270) 'too big'
          GO TO 900
        ELSEIF (.NOT.LPRIBI) THEN
C
          IF (ILEN.LE.JP16SET.AND.(IORDER+ISECOV).NE.ILEN) THEN
            IRETFN = 17270
            WRITE(GRPRSM,FMT=9270) 'from section 2'
            GO TO 900
          ELSEIF (ILEN.GT.JP16SET) THEN
C
C           Two octets not wide enough for ISECOV - adjust.
C
            ISECOV=ILEN-IORDER
            KSEC4(19)=ISECOV
          ENDIF
C
        ELSEIF (KVALS.NE.ILEN) THEN
          IRETFN = 17275
          WRITE(GRPRSM,FMT=9275)
          GO TO 900
        ENDIF
C
      ELSE
C
C     No section 2 in the present message.
C
        IF (LROW) THEN
          IRETFN = 17277
          WRITE(GRPRSM,FMT=9277) 'Row by row packing'
          GO TO 900
        ELSEIF (HOPER.EQ.'X') THEN
          IRETFN = 17278
          WRITE(GRPRSM,FMT=9277) 'HOPER=''X'' function'
          GO TO 900
        ELSEIF (LBOUST) THEN
          IRETFN = 17279
          WRITE(GRPRSM,FMT=9277) 'Boustrophedonic ordering'
          GO TO 900
        ENDIF
C
      ENDIF
C
      IF (LPRIBI) THEN
C
        IF (KSEC3(1).EQ.0) THEN
C
C         Explicit primary bit-map included.
C         Computation of effective number of points.
C
          INBSEQ=1+(KVALS-1)/JPWS2O
          INVALS=0
          ISEQ=0
C
          IF (LROW.AND.HOPER.NE.'J') THEN
C
C           In row by row case, it is also necessary for decoding field
C           values to compute real length of rows. Do it now, as far as
C           permitted through ILENGR work array size.
C
            IAUXIS=0
            ILAUXS=MIN (JPWS2O,KVALS)
            IPTRBM=KBMAP
            ISEQ=1
            CALL INXBIT(KGRIB,KLENG,IPTRBM,IWORK2,ILAUXS,KBITS,1,YOPER,
     X                  IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 17280
              WRITE(GRPRSM,FMT=9001) 'primary bit-map, row by row case'
              GO TO 900
            ENDIF
C
            DO 223 JROW=1,MIN (INROWS,JPWS1O)
C
C             Full length of row (unmasked).
C
            IF (LQUASI) THEN
              ILNGTH=KSEC2(IREFLN+JROW)
            ELSE
              ILNGTH=ILEN/INROWS
            ENDIF
C
            IVALS=ILNGTH
            ILNGTH=0
            IVALSC=IVALS
C
  221       CONTINUE
C
            DO 222 J=1,MIN (IVALSC,ILAUXS-IAUXIS)
            ILNGTH=ILNGTH+IWORK2(IAUXIS+J)
  222       CONTINUE
C
            IF (IVALSC.LE.ILAUXS-IAUXIS) THEN
C
              IAUXIS=IAUXIS+IVALSC
C
            ELSEIF (IPTRBM.LT.KBMAP+KVALS) THEN
C
              IVALSC=IVALSC-(ILAUXS-IAUXIS)
              ILAUXS=MIN(KBMAP+KVALS-IPTRBM,JPWS2O)
              IAUXIS=0
              ISEQ=ISEQ+1
              CALL INXBIT(KGRIB,KLENG,IPTRBM,IWORK2,ILAUXS,KBITS,1,
     X                    YOPER,IRET)
C
              IF (IRET.NE.0) THEN
                IRETFN = 17280
                WRITE(GRPRSM,FMT=9001)
     X                   'primary bit-map, row by row case'
                GO TO 900
              ENDIF
C
              GOTO 221
C
            ELSE
C
              IRETFN = 17560
              WRITE(GRPRSM,FMT=9560)
              GO TO 900
C
            ENDIF
C
            ILENGR(JROW)=ILNGTH
            INVALS=INVALS+ILNGTH
C
  223       CONTINUE
C
C             End-up current sequence (active only if bit-map has not
C             been fully processed)...
C
            DO 224 J=IAUXIS+1,ILAUXS
            INVALS=INVALS+IWORK2(J)
  224       CONTINUE
C
            IAUXIL=(ISEQ-1)*JPWS2O+IAUXIS
C
          ENDIF
C
C             Staightforward computing.
C
          DO 228 JSEQ=ISEQ+1,INBSEQ
          IAUXIS=(JSEQ-1)*JPWS2O
          ILAUXS=MIN (JPWS2O,KVALS-IAUXIS)
          IPTRBM=KBMAP+IAUXIS
          CALL INXBIT(KGRIB,KLENG,IPTRBM,IWORK2,ILAUXS,KBITS,1,YOPER,
     X                IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 17280
            WRITE(GRPRSM,FMT=9001) 'primary bit-map'
            GO TO 900
          ENDIF
C
          DO 227 J=1,ILAUXS
          INVALS=INVALS+IWORK2(J)
  227     CONTINUE
C
  228     CONTINUE
C
          IF (LROW.AND.HOPER.NE.'J') THEN
C
            IF (INROWS.GT.JPWS1O) THEN
C
C           ... it is necessary to (re)read primary bit-map, adjusting
C           pointer to start of (JPWS1O+1)th row.
C
              IAUXIS=0
              ILAUXS=MIN (JPWS2O,KVALS-IAUXIL)
              IPTRBM=KBMAP+IAUXIL
              CALL INXBIT(KGRIB,KLENG,IPTRBM,IWORK2,ILAUXS,KBITS,1,
     X                    YOPER,IRET)
C
              IF (IRET.NE.0) THEN
                IRETFN = 17280
                WRITE(GRPRSM,FMT=9001)
     X               'primary bit-map, row by row case'
                GO TO 900
              ENDIF
C
            ENDIF
C
          ENDIF
C
          IF (INVALS.GT.JP16SET) THEN
C
C       Two octets might be not wide enough for ISECOV. Adjust.
C
            ISECOV=INVALS-IORDER
          ENDIF
C
        ELSEIF (LROW) THEN
C
          IRETFN = 17281
          WRITE(GRPRSM,FMT=9281) 'Row by row'
          GO TO 900
C
        ELSEIF (LBOUST) THEN
C
          IRETFN = 17282
          WRITE(GRPRSM,FMT=9281) 'Boustrophedonic ordering'
          GO TO 900
C
        ENDIF
C
C         WARNING:
C         -------
C       Whenever an implicit primary bit-map is included, it is then
C       assumed that ISECOV contains the right number of grid-points
C       really included in section 4. This is valid only if this
C       effective number of points is lower than (or equal to) JP16SET.

      ENDIF
C
      IF (IORDER+ISECOV.GT.KLENP.AND.HOPER.NE.'J'.AND.HOPER.NE.'X') THEN
        IRETFN = 17290
        WRITE(GRPRSM,FMT=9290) IORDER+ISECOV,KLENP
        GO TO 900
      ELSEIF (HOPER.EQ.'X'.AND.KLENP.LE.IORDER) THEN
        IRETFN = 17291
        WRITE(GRPRSM,FMT=9291) IORDER+1
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3 . Decode last octet(s) (22...) before loop on groups.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
      IF (KSEC4(12).NE.0) THEN
C
C               General extended second-order packing used.
C
        IF (LSECBI) THEN
          IRETFN = 17310
          WRITE(GRPRSM,FMT=9310) 'no secondary bit-map'
          GO TO 900
        ELSEIF (LCTWID) THEN
          IRETFN = 17320
          WRITE(GRPRSM,FMT=9310) 'variable group widths'
          GO TO 900
        ENDIF
C
C     Octets 22 - 23: Width of widths, Width of lengths.
C     Two 8-bit fields.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,IWORK,2,KBITS,8,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 17330
          WRITE(GRPRSM,FMT=9001) 'general extended descriptors, widths'
          GO TO 900
        ENDIF
C
        IBITGW=IWORK(1)
        IBITGS=IWORK(2)
C
        IF (MAX (IBITGW,IBITGS).GT.JPXPWR) THEN
          IRETFN = 17340
          WRITE(GRPRSM,FMT=9002)
     X         'general extended width of (group and/or size)',
     X                            JPXPWR
          GO TO 900
        ENDIF
C
        KSEC4(11)=IBITGS
C
C     Octets 24-25: Octet number of the start of group sizes (lengths).
C     One 16-bit field.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,IPTRGS,1,KBITS,16,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 17345
          WRITE(GRPRSM,FMT=9001) 'general extended group size pointer'
          GO TO 900
        ENDIF
C
        IPTRSN=1+(8+(KNSPT-INSPTA)+IFIROV*IBITGW-1)/8
C
        IF (IPTRSN.GT.JP16SET) THEN
C
C           Two octets not wide enough for IPTRGS - adjust.
C
          IPTRGS=IPTRSN
        ENDIF
C
        IF (IORDER.NE.0.AND.HOPER.NE.'J') THEN
C
C     Octet 26: Width of spatial differencing specific descriptors.
C     One 8-bit field.
C
          CALL INXBIT(KGRIB,KLENG,KNSPT,IWIDSD,1,KBITS,8,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 17346
            WRITE(GRPRSM,FMT=9001)
     X           'Width of spatial differencing descriptors'
            GO TO 900
          ELSEIF (IWIDSD.NE.0) THEN
C
C     Octet 27-onwards: first IORDER original values and diff. bias.
C     (IORDER+1) fields of IWIDSD bits.
C
            IAUXIL=IORDER+1
            CALL INXBIT(KGRIB,KLENG,KNSPT,KDATA,IAUXIL,KBITS,IWIDSD,
     X                  YOPER,IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 17347
              WRITE(GRPRSM,FMT=9001)
     X           'First field value(s) (spatial differencing case)'
              GO TO 900
            ENDIF
C
C              Decode bias and its sign.
C
            CALL DSGNBT(IBIAS,KDATA(IAUXIL),IWIDSD,IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 17348
              WRITE(GRPRSM,FMT=9348) IRET
              GO TO 900
            ENDIF
C
C                 Adjust pointer to next octet boundary.
C
            KNSPT=8*((8+KNSPT-1)/8)
C
          ELSE
C
C                 Spatial differencing descriptors are all zero.
C
            DO 311 J=1,IORDER
            KDATA(J)=0
  311       CONTINUE
C
            IBIAS=0
C
          ENDIF
C
        ENDIF
C
      ELSE
C
        IBITGW=8
C
        IF (LCTWID) THEN
C
C     Octet 22: Single value for width of second-order values.
C     One 8-bit field.
C
          CALL INXBIT(KGRIB,KLENG,KNSPT,IWIDTH,1,KBITS,8,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 17350
            WRITE(GRPRSM,FMT=9001) 'constant width of 2nd-order values'
            GO TO 900
          ELSEIF (IWIDTH.GT.JPXPWR) THEN
            IRETFN = 17351
            WRITE(GRPRSM,FMT=9002) 'Constant width', JPXPWR
            GO TO 900
          ENDIF
C
          KSEC4(11)=IWIDTH
C
        ENDIF
C
        IF (LSECBI) THEN
C
C     Secondary bit-map present. Initialize pointer, and decode
C     start of secondary bit-map (if field decoding is requested).
C
          IF (LCTWID) THEN
            IPTRBM=23
          ELSE
            IPTRBM=22+IFIROV
          ENDIF
C
          IPTRBM=(IPTRBM-1)*8+INSPTA
          IPTRB0=IPTRBM
C
          IF (HOPER.NE.'J') THEN
C
            ILAUXS=MIN(ISECOV,JPWS2O)
            IAUXIS=1
            CALL INXBIT(KGRIB,KLENG,IPTRBM,IWORK2,ILAUXS,KBITS,1,YOPER,
     X                  IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 17360
              WRITE(GRPRSM,FMT=9001) 'start of secondary bit-map'
              GO TO 900
            ELSEIF (IWORK2(1).NE.1) THEN
              IRETFN = 17365
              WRITE(GRPRSM,FMT=9365)
              GO TO 900
            ENDIF
C
          ENDIF
C
        ENDIF
C
      ENDIF
C
C     Compute minimum values for pointers. If it leads to value(s)
C     not fitting in 2 octets, then assume these are the right
C     value(s) to use.
C
      IPTRGW=KNSPT
      IPTRFN=1+(8+IPTRGW-INSPTA-1)/8
C
      IF (LCTWID) THEN
        IPTRFN=IPTRFN+(8+ISECOV-1)/8
      ELSEIF (LROW) THEN
        IPTRFN=IPTRFN+(8+IFIROV*IBITGW-1)/8
      ELSEIF (LGENXT) THEN
        IPTRFN=IPTRFN+(8+IFIROV*IBITGW-1)/8+(8+IFIROV*IBITGS-1)/8
      ELSE
        IPTRFN=IPTRFN+(8+IFIROV*IBITGW-1)/8+(8+ISECOV-1)/8
      ENDIF
C
      IF (IPTRFN.GT.JP16SET) THEN
        IPTRFO=IPTRFN
      ENDIF
C
      KSEC4(16)=IPTRFO
      IPTRSN=IPTRFO+(8+IFIROV*KSEC4(2)-1)/8
C
      IF (IPTRSN.GT.JP16SET) THEN
        IPTRSO=IPTRSN
        KSEC4(17)=IPTRSO
      ENDIF
C
#ifndef ebug2o
      IF (LDEBUG) THEN
#endif
C
        IF (LROW) THEN
          WRITE(GRPRSM,FMT=9381) 'row by row'
        ELSEIF (LCTWID) THEN
          WRITE(GRPRSM,FMT=9382) KSEC4(11)
        ELSEIF (LGENXT) THEN
          WRITE(GRPRSM,FMT=9381) 'general extended'
        ELSE
          WRITE(GRPRSM,FMT=9381) 'WMO general 2nd-order'
        ENDIF
C
        IF (LBOUST) THEN
          WRITE(GRPRSM,FMT=9383)
        ENDIF
C
        IF (IORDER.NE.0) THEN
          WRITE(GRPRSM,FMT=9384) IORDER, IBIAS
        ENDIF
C
        WRITE(GRPRSM,FMT=9385) IFIROV, ISECOV, IPTRFO, IPTRSO
#ifndef ebug2o
      ENDIF
C
#endif
      KSEC4(20)=IBITGW
      IPTRFO=(IPTRFO-1)*8+INSPTA
      IPTRSO=(IPTRSO-1)*8+INSPTA
C
C     Cross checks betweens pointers.
C
      IF (LGENXT) THEN
C
        IPTRGS=(IPTRGS-1)*8+INSPTA
C
        IF (IPTRGW+IFIROV*IBITGW.GT.IPTRGS) THEN
          IRETFN = 17391
          WRITE(GRPRSM,FMT=9391)
          GO TO 900
        ELSEIF (IPTRGS+IFIROV*IBITGS.GT.IPTRFO) THEN
          IRETFN = 17392
          WRITE(GRPRSM,FMT=9393)
     X           'general extended group size/1st-order values'
          GO TO 900
        ENDIF
C
      ELSEIF (LSECBI) THEN
C
        IF (IPTRB0+ISECOV.GT.IPTRFO) THEN
          IRETFN = 17393
          WRITE(GRPRSM,FMT=9393) '2ndary bit-map/1st-order values'
          GO TO 900
        ENDIF
C
      ENDIF
C
      IF (IPTRFO+IFIROV*KSEC4(2).GT.IPTRSO) THEN
        IRETFN = 17394
        WRITE(GRPRSM,FMT=9393) 'First-order/Second-order'
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 4 . Additional preprocessing for function 'X'.
C     -----------------------------------------------------------------
C
  400 CONTINUE
C
      IF (HOPER.EQ.'X') THEN
C
C     First, perform some checks.
C
        INDEXX=34
        INDEXY=42
C
#ifndef ebug2o
        IF (LDEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9401) INDEXX, INDEXY,
     X                            (KSEC4(J),J=INDEXX,INDEXY)
#ifndef ebug2o
        ENDIF
#endif
C
        INPEXT=KSEC4(INDEXX)
C
        IF (LPRIBI) THEN
          IRETFN = 17410
          WRITE(GRPRSM,FMT=9410) 'not valid with a (primary) bit-map'
          GO TO 900
        ELSEIF (KSEC2(1).GE.40.OR.
     X          (MOD(KSEC2(1),10).NE.0.AND.MOD(KSEC2(1),10).NE.4)) THEN
          IRETFN = 17411
          WRITE(GRPRSM,FMT=9410)
     X              'valid only with lat/long or gaussian field'
          GO TO 900
        ELSEIF (INPEXT.LT.1.OR.INPEXT.GT.JPXMAX) THEN
          IRETFN = 17412
          WRITE(GRPRSM,FMT=9412) JPXMAX,' points.'
          GO TO 900
        ELSEIF (INPEXT.GT.KLENP) THEN
          IRETFN = 17413
          WRITE(GRPRSM,FMT=9413) INPEXT
          GO TO 900
        ENDIF
C
C       Take into account third bit of scanning modes to know 
C       which dimension is latitude.
C
C       INCR=0 means that longitude is the most rapidly varying.
C
        INCR=MOD (KSEC2(11),64)/32
C
C       Now compute 1-dimensional "addresses" in whole field.
C
        DO 422 JEXT=1,INPEXT
        INDROW=KSEC4(INDEXX+2*JEXT-1+INCR)
        INDCOL=KSEC4(INDEXX+2*JEXT  -INCR)
C
        IF (INDROW.LT.1.OR.INDROW.GT.INROWS) THEN
          IRETFN = 17421
          WRITE(GRPRSM,FMT=9421) JEXT, INCR+1, INROWS
          GO TO 900
        ELSEIF(LQUASI) THEN
          ISKIP=0
C
          DO 421 J=1,INDROW-1
          ISKIP=ISKIP+KSEC2(IREFLN+J)
  421     CONTINUE
C
          ILNGTH=KSEC2(IREFLN+INDROW)
C
        ELSE
          ISKIP=(INDROW-1)*ILNGTH
        ENDIF
C
        IF (INDCOL.LT.1.OR.INDCOL.GT.ILNGTH) THEN
          IRETFN = 17422
          WRITE(GRPRSM,FMT=9421) JEXT, 2-INCR, ILNGTH
          GO TO 900
        ENDIF
C
#ifndef ebug2o
        IF (LDEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9422) JEXT, INDROW, INDCOL, ISKIP, ILNGTH
#ifndef ebug2o
        ENDIF
C
#endif
        IF (LBOUST.AND.MOD(INDROW,2).EQ.0) THEN
          INDCOL=ILNGTH+1-INDCOL
        ENDIF
C
        IRGEXT(JEXT)=ISKIP+INDCOL
        INDXOR(JEXT)=JEXT
  422   CONTINUE
C
C     Finally, sort "addresses" in ascending order.
C
        DO 432 JEXT=1,INPEXT-1
C
        DO 431 J=JEXT+1,INPEXT
C
        IF (IRGEXT(J).LT.IRGEXT(JEXT)) THEN
          IAUXIL=IRGEXT(J)
          IRGEXT(J)=IRGEXT(JEXT)
          IRGEXT(JEXT)=IAUXIL
          IAUXIL=INDXOR(J)
          INDXOR(J)=INDXOR(JEXT)
          INDXOR(JEXT)=IAUXIL
        ENDIF
C
  431   CONTINUE
C
  432   CONTINUE
C
        IRGEXT(INPEXT+1)=0
#ifndef ebug2o
C
        IF (LDEBUG) THEN
#endif
          PRINT *,' D2ORDR: IRGEXT = ',IRGEXT,', INDXOR = ',INDXOR,'.'
#ifndef ebug2o
        ENDIF
C
#endif
      ELSE
        INPEXT=0
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 5 . Decode values, group by group.
C     -----------------------------------------------------------------
C
  500 CONTINUE
C
C     Finished if function 'J' is requested.
C
      IF (HOPER.EQ.'J') THEN
        KSEC4(1)=IORDER+ISECOV
        GO TO 900
      ENDIF
C
      ICOUNT=IORDER
      ICOUNS=IORDER
      ICOUNG=IORDER
      IEXT=1
      INBSEQ=1+(IFIROV-1)/JPWS1O
C
      IF (KSEC4(2).EQ.0) THEN
C
C       In this case, no first-order values are provided: they are
C       all equal to 0 (group references equal to field reference).
C
        DO 501 J=1,MIN (IFIROV,JPWS1O)
        IFIRST(J)=0
  501   CONTINUE
C
      ENDIF
C
      IF (LCTWID.AND.HOPER.NE.'X') THEN
C
C       Constant width packing. Second-order values will be
C       decoded once per sequence.
C
        IPRWID=KSEC4(11)
C
        DO 502 J=1,MIN (IFIROV,JPWS1O)
        IWIDGR(J)=IPRWID
  502   CONTINUE
C
      ENDIF
C
C                 Groups treated as sequences, as large as possible
C                 with respect to group work arrays size (JPWS1O). 
C
      DO 591 JSEQ=1,INBSEQ
C
      IGROFF=(JSEQ-1)*JPWS1O
      IGRNBR=MIN (JPWS1O,IFIROV-IGROFF)
C
      IF (LGENXT) THEN
        CALL INXBIT(KGRIB,KLENG,IPTRGS,IWORK2,IGRNBR,KBITS,IBITGS,YOPER,
     X              IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 17510
          WRITE(GRPRSM,FMT=9510) 'sizes', IGROFF+1, IGROFF+IGRNBR
          GO TO 900
        ENDIF
C
      ENDIF
C
      IF (.NOT.LCTWID) THEN
        CALL INXBIT(KGRIB,KLENG,IPTRGW,IWIDGR,IGRNBR,KBITS,IBITGW,YOPER,
     X              IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 17520
          WRITE(GRPRSM,FMT=9510) 'widths', IGROFF+1, IGROFF+IGRNBR
          GO TO 900
        ENDIF
C
        IPRWID=IWIDGR(1)
C
      ENDIF
C
      IF (KSEC4(2).NE.0) THEN
C
        CALL INXBIT(KGRIB,KLENG,IPTRFO,IFIRST,IGRNBR,KBITS,KSEC4(2),
     X              YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 17530
          WRITE(GRPRSM,FMT=9510) '1st-order values', IGROFF+1,
     X         IGROFF+IGRNBR
          GO TO 900
        ENDIF
C
      ENDIF
C
      ILNGTS=0
C
C     Loop over groups in a given sequence.
C
      DO 551 JGROUP=1,IGRNBR
C
C     Define length of current group: ILNGTH.
C
      IF (LGENXT) THEN
        ILNGTH=IWORK2(JGROUP)
      ELSEIF (LSECBI) THEN
C
        ILNGTH=0
C
  520   CONTINUE
C
        DO 521 J=IAUXIS+1,ILAUXS
C
        IF (IWORK2(J).EQ.1) THEN
          IRANG=J
          GOTO 522
        ENDIF
C
  521   CONTINUE
C
        ILNGTH=ILNGTH+ILAUXS-IAUXIS
C
        IF (IPTRBM.LT.IPTRB0+ISECOV) THEN
          ILAUXS=MIN(IPTRB0+ISECOV-IPTRBM,JPWS2O)
          IAUXIS=0
          CALL INXBIT(KGRIB,KLENG,IPTRBM,IWORK2,ILAUXS,KBITS,1,YOPER,
     X                IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 17540
            WRITE(GRPRSM,FMT=9001) 'secondary bit-map'
            GO TO 900
          ENDIF
C
          GOTO 520
        ELSE
C
C           It was the last group.
C
          ILNGTH=ILNGTH+1
          GOTO 523
        ENDIF
C
  522   CONTINUE
C
        ILNGTH=ILNGTH+IRANG-IAUXIS
        IAUXIS=IRANG
C
  523   CONTINUE
C
      ELSE
C
C        ... that's row by row packing...
C
C            (re)definition of row length.
C
        IF (LQUASI) THEN
          IREFLN=IREFLN+1
          ILNGTH=KSEC2(IREFLN)
        ELSE
          ILNGTH=ILEN/INROWS
        ENDIF
C       
        IF (LPRIBI) THEN
C
C         Row by row packing, but an explicit bit-map is included.
C         The number of values in current row has to be known.
C
          IVALS=ILNGTH
C
          IF (JSEQ.EQ.1) THEN
C
C            Rows of first sequence has been pre-computed in Section 2.
C
            ILNGTH=ILENGR(JGROUP)
C
          ELSE
C
            ILNGTH=0
            IVALSC=IVALS
C
  530       CONTINUE
C
            DO 531 J=1,MIN (IVALSC,ILAUXS-IAUXIS)
            ILNGTH=ILNGTH+IWORK2(IAUXIS+J)
  531       CONTINUE
C
            IF (IVALSC.LE.ILAUXS-IAUXIS) THEN
C
              IAUXIS=IAUXIS+IVALSC
C
            ELSEIF (IPTRBM.LT.KBMAP+KVALS) THEN
C
              IVALSC=IVALSC-(ILAUXS-IAUXIS)
              ILAUXS=MIN(KBMAP+KVALS-IPTRBM,JPWS2O)
              IAUXIS=0
#ifndef ebug2o
C
              IF (LDEBUG) THEN
#endif
                WRITE(GRPRSM,FMT=9540) IGROFF+JGROUP, IPTRBM, ILAUXS
#ifndef ebug2o
              ENDIF
C
#endif
              CALL INXBIT(KGRIB,KLENG,IPTRBM,IWORK2,ILAUXS,KBITS,1,
     X                    YOPER,IRET)
C
              IF (IRET.NE.0) THEN
                IRETFN = 17550
                WRITE(GRPRSM,FMT=9001)
     X                    'primary bit-map in row by row case'
                GO TO 900
              ENDIF
C
              GOTO 530
C
            ELSE
C
              IRETFN = 17560
              WRITE(GRPRSM,FMT=9560)
              GO TO 900
C
            ENDIF
C
          ENDIF
C
#ifndef ebug2o
          IF (LDEBUG) THEN
#endif
            WRITE(GRPRSM,FMT=9565) IGROFF+JGROUP, ILNGTH, IVALS
#ifndef ebug2o
          ENDIF
C
#endif
        ENDIF
C
      ENDIF
C
      ICNEXT=ICOUNT+ILNGTH
C
      IF (ICNEXT.GT.(IORDER+ISECOV)) THEN
        IRETFN = 17570
        WRITE(GRPRSM,FMT=9570) ICNEXT, ISECOV
        GO TO 900
      ENDIF
C
      ILENGR(JGROUP)=ILNGTH
C
C         Go to next group if no point is included in current group.
C         (may only occur for masked field in row by row case)
C
      IF (ILNGTH.EQ.0) THEN
        IWIDGR(JGROUP)=0
        IFIRST(JGROUP)=0
        GOTO 551
      ENDIF
C
C     If necessary, (re)define width of current group: IWIDTH.
C
      IWIDTH=IWIDGR(JGROUP)
C
  540 CONTINUE
C
C     All characteristics of current group are known, so proceed
C     with second-order data.
C
C     Except for 'X' function, neighbour groups of same width are
C     "unpacked" together, at least once per sequence.
C
      IF (HOPER.NE.'X') THEN
C
C         All values of current group will have to be processed.
C
        IF (IWIDTH.NE.IPRWID) THEN
C
C     Current group may not be unpacked together with previous one(s).
C
#ifdef ebug2o
          PRINT *,'d2ordr - groups..',IGROFF+JGROUP-1,
     X          ' IPTRSO/IPRWID/ICOUNS/ILNGTS= ',
     X            IPTRSO, IPRWID, ICOUNS, ILNGTS
#endif
C
          IF (IPRWID.NE.0.AND.ILNGTS.NE.0) THEN
C
C     Effective decoding of second-order values, for previous group(s).
C
            CALL INXBIT(KGRIB,KLENG,IPTRSO,KDATA(ICOUNS+1),
     X                  ILNGTS,KBITS,IPRWID,YOPER,IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 17580
              WRITE(GRPRSM,FMT=9580) 'groups..', IGROFF+JGROUP-1
              GO TO 900
            ENDIF
C
          ENDIF
C
          ICOUNS=ICOUNT
          ILNGTS=0
          IPRWID=IWIDTH
C
        ENDIF
C
        ILNGTS=ILNGTS+ILNGTH
C
      ELSEIF (IRGEXT(IEXT).GT.ICOUNT.AND.IRGEXT(IEXT).LE.ICNEXT) THEN
C
C     Function 'X' - Compute only a single "full" normalized value.
C
        IEXTOR=INDXOR(IEXT)
C
        IF (IWIDTH.NE.0) THEN
          IAUXIL=IPTRSO+(IRGEXT(IEXT)-ICOUNT-1)*IWIDTH
          CALL INXBIT(KGRIB,KLENG,IAUXIL,KDATA(IEXTOR),1,KBITS,
     X                IWIDTH,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 17580
            WRITE(GRPRSM,FMT=9580) 'group', IGROFF+JGROUP
            GO TO 900
          ENDIF
C
          KDATA(IEXTOR)=IFIRST(JGROUP)+KDATA(IEXTOR)
        ELSE
          KDATA(IEXTOR)=IFIRST(JGROUP)
        ENDIF
C
        IEXT=IEXT+1
#ifndef ebug2o
C
        IF (LDEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9585) IEXT, INPEXT
#ifndef ebug2o
        ENDIF
#endif
C
C       Loop back, to allow for extraction of other point(s)
C       within the current group.
C
        GOTO 540
C
      ELSE
C
C       Function 'X', but group contains no requested grid-point.
C       Pointer adjusted for next group.
C
        IPTRSO=IPTRSO+ILNGTH*IWIDTH
C
      ENDIF
C
      ICOUNT=ICNEXT
C
  551 CONTINUE
C
      IF (HOPER.NE.'X') THEN
C
#ifdef ebug2o
        PRINT *,'d2ordr - groups..',IGROFF+IGRNBR,
     X          ' IPTRSO/IPRWID/ICOUNS/ILNGTS= ',
     X            IPTRSO, IPRWID, ICOUNS, ILNGTS
C
#endif
        IF (IPRWID.NE.0.AND.ILNGTS.NE.0) THEN
C
C         Effective decoding of second-order values, for last group(s)
C         of the current sequence.
C
C         In case of constant width packing it means all groups
C         of the current sequence.
C
          CALL INXBIT(KGRIB,KLENG,IPTRSO,KDATA(ICOUNS+1),
     X                ILNGTS,KBITS,IPRWID,YOPER,IRET)
C  
          IF (IRET.NE.0) THEN
            IRETFN = 17580
            WRITE(GRPRSM,FMT=9580) 'groups..', IGROFF+IGRNBR
            GO TO 900
          ENDIF
C
        ENDIF
C
        ICOUNS=ICOUNT
C
C       Add first-order values to rebuild original normalized values.
C
        DO 563 JGROUP=1,IGRNBR
C
#ifdef ebug2o
        IMAXVC=0
#endif
        IREFER=IFIRST(JGROUP)
        ILNGTH=ILENGR(JGROUP)
        IWIDTH=IWIDGR(JGROUP)
C
        IF (IWIDTH.NE.0) THEN
C
          DO 561 J=1,ILNGTH
#ifdef ebug2o
          IMAXVC=MAX(IMAXVC,KDATA(ICOUNG+J))
#endif
          KDATA(ICOUNG+J)=IREFER+KDATA(ICOUNG+J)
  561     CONTINUE
C
        ELSE
C
C     Group with a constant value: no second order value included.
C
          DO 562 J=1,ILNGTH
          KDATA(ICOUNG+J)=IREFER
  562     CONTINUE
C
        ENDIF
C
        ICOUNG=ICOUNG+ILNGTH
#ifdef ebug2o
        PRINT *,'d2ordr - group ',IGROFF+JGROUP,ICOUNG,ILNGTH,IWIDTH,
     X           IREFER,IMAXVC
#endif
C
  563   CONTINUE
C
      ENDIF
C
  591 CONTINUE
C
C     -----------------------------------------------------------------
C*    Section 6 . Last computations.
C     -----------------------------------------------------------------
C
  600 CONTINUE
C
C          From this point and for the remaining part of the routine,
C          ISECOV is the total number of grid-points.
C
      ISECOV=IORDER+ISECOV
C
      IF (IORDER.NE.0) THEN
C
C         Rebuild original field values from spatial differences.
C
#if (defined CRAY) || (defined FUJITSU)
        LVECTD=.TRUE.
#else
        LVECTD=.FALSE.
#endif
C
        IRETFN = D2ROSD ( KDATA,  ISECOV, IORDER, IPOWER, JPXPWR,
     X                    IBIAS,  LVECTD, LDEBUG )
C
        IF (IRETFN.NE.0) THEN
          GOTO 900
        ENDIF
C
      ENDIF
C
      IF (HOPER.NE.'X') THEN
C
C       Recompute original bit number for the whole field.
C
        CALL MAXMNI (KDATA,ISECOV,IMAXVN,IAUXIL)
C
        KSEC4(2) = C2BITW ( IMAXVN, JPXPWR, IPOWER, JPXPWR )
C
#ifndef ebug2o
        IF (LDEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9601) KSEC4(2)
#ifndef ebug2o
        ENDIF
C
#endif
        IF (ICOUNT.NE.ISECOV) THEN
          IRETFN = 17610
          WRITE(GRPRSM,FMT=9610) ICOUNT, ISECOV
          GO TO 900
        ELSE
          KSEC4(1)=ISECOV
        ENDIF        
C
      ELSE
        KSEC4(1)=MIN(IEXT,JPXMAX)
      ENDIF
C
#ifndef ebug2o
      IF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9602) KSEC4(1)
#ifndef ebug2o
      ENDIF
C
#endif
      KNSPT=INSPTA+KLEN4*8-KNIL
C
      IF (IPTRSO.NE.KNSPT) THEN
        IRETFN = 17620
        WRITE(GRPRSM,FMT=9620) IPTRSO, KNSPT
        GO TO 900
      ENDIF        
C
      IF (LBOUST.AND.HOPER.NE.'X') THEN
C
C           Reverse ordering within even rank rows.
C
        LPRCLN=.FALSE.
C
        IRETFN = REVERO ( KDATA,  KLENP,  KSEC2,  KGRIB,  KLENG,
     X                    KBITS,  KBMAP,  KVALS,  LPRIBI, LQUASI,
     X                    INROWS, ISECOV, IFIRST, JPWS2O, LDEBUG,
     X                    LPRCLN, IWORK2 )
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      D2ORDR = IRETFN
#ifndef ebug2o
C
      IF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9900) IRETFN, HOPER(:1), KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      RETURN
C
 9001 FORMAT (' D2ORDR: Problem extracting ',A,'.')
C
 9002 FORMAT (' D2ORDR: ',A,' above',I3,'.')
C
 9100 FORMAT (' D2ORDR: Function start, HOPER = ',A,', KNSPT =',I10,
     X        '.')
 9135 FORMAT (' D2ORDR: Decoded number of bits (for 1st-order values)',
     X        ' =',I3,'.')
 9150 FORMAT (' D2ORDR: PREF =',F30.20,', decoded IPTRFO =',I6,'.',/,
     X        ' D2ORDR: KNSPT at end of "Section 1" =',I10,'.')
 9210 FORMAT (' D2ORDR: Extended flags =',I9.8,', KNSPT =',I10,'.')
 9220 FORMAT (' D2ORDR: First extended flag (reserved) set.')
 9225 FORMAT (' D2ORDR: KSEC4(3:10) =',I4,3I3,I2,3I3,'.')
 9230 FORMAT (' D2ORDR: Last extended flag(s) (reserved) set.')
 9240 FORMAT (' D2ORDR: Matrix of values invalid here.')
 9244 FORMAT (' D2ORDR: Function HOPER = "',A1,
     X        '" not supported for spatial differencing.')
 9245 FORMAT (' D2ORDR: Spatial differencing implies general',
     X                   ' extended 2nd-order packing.')
#ifndef wmogrp2o
 9255 FORMAT (' D2ORDR: 21st octet of section 4 not zero:',I9.8,
     X        '=>',I3,'.',/,TR10,
     X        'interpreted as extra bits for group number:',I8,'.')
#endif
 9260 FORMAT (' D2ORDR: Row by row packing: row/group numbers',
     X        ' not consistent.')
 9270 FORMAT (' D2ORDR: Number of second-order values ',A,'.')
 9275 FORMAT (' D2ORDR: (Primary bit-map size/section 2 contents)',
     X        ' mismatch.')
 9277 FORMAT (' D2ORDR: ',A,' only supported with a section 2.')
 9281 FORMAT (' D2ORDR: ',A,
     S        '/Implicit primary bit-map: not supported.')
 9290 FORMAT (' D2ORDR: Grid-point number exceeds KDATA size:',I9,
     X        ' >',I9,'.')
 9291 FORMAT (' D2ORDR: Function ''X'' - KDATA size must be at least',
     X        I2,' for actual spatial differencing).')
C
 9310 FORMAT (' D2ORDR: General extended packing implies ',A,'.')
 9348 FORMAT (
     X' D2ORDR: Bias sign management problem, DSGNBT return is',I5,'.')
 9365 FORMAT (' D2ORDR: First bit of secondary bit-map is not set.')
 9381 FORMAT (' D2ORDR: Packing method is ',A,'.')
 9382 FORMAT (' D2ORDR: Packing method is constant width, on',I3,
     X        ' bits.')
 9383 FORMAT (' D2ORDR: Boustrophedonic ordering used.')
 9384 FORMAT (' D2ORDR: Spatial differencing used, order is',I2,
     X        ', bias =',I9,'.')
 9385 FORMAT (' D2ORDR: IFIROV, ISECOV, IPTRFO, IPTRSO =',2I8,2I10,
     X        '.')
 9391 FORMAT (
     X ' D2ORDR: Bad pointer to general extended group size info.')
 9393 FORMAT (' D2ORDR: (',A,')',' pointers mismatch.')
C
 9401 FORMAT (' D2ORDR: ''X'' Function - KSEC4(',I2,':',I2,') = ',I3,
     X        8I6,'.')
 9410 FORMAT (' D2ORDR: Function ''X'' ',A,'.')
 9412 FORMAT (' D2ORDR: Function ''X'' valid only for 1 to',I3,
     X        ' points.')
 9413 FORMAT (' D2ORDR: Function ''X'' - KDATA size insufficient for',
     X        I2,' points requested.')
 9421 FORMAT (' D2ORDR: Function ''X'', Point',I6,',',I2,
     X        '-index not in range [1-',I6,'].')
 9422 FORMAT (' D2ORDR: JEXT, INDROW, INDCOL, ISKIP, ILNGTH =',
     X        I3,2I6,I8,I6,'.')
C
 9510 FORMAT (' D2ORDR: ',A,', groups',I6,'..',I6.6,'.')
 9540 FORMAT (' D2ORDR: mask, row',I6,', bit-map pointer =',I10,
     X        ', size requested=',I8,'.')
 9560 FORMAT (' D2ORDR: Pointers problem for primary bit-map,',
     X        ' row by row case.')
 9565 FORMAT (' D2ORDR: mask, row',I6,', length =',I6,' out of',I6,'.')
 9570 FORMAT (' D2ORDR: Grid-point number overflow:',I8,' >',I8,'.')
 9580 FORMAT (' D2ORDR: second-order values, ',A,I8,'.')
 9585 FORMAT (' D2ORDR: IEXT =',I3,', INPEXT =',I3,'.')
C
 9601 FORMAT (
     X  ' D2ORDR: Original number of bits -returned in KSEC4(2)-) =',
     X  I3,'.')
 9602 FORMAT (' D2ORDR: Values decoded -returned in KSEC4(1)- =',
     X        I8,'.')
 9610 FORMAT (' D2ORDR: Mismatch: sum of group lengths =',I8,
     X        ', expected =',I8,'.')
 9620 FORMAT (' D2ORDR: Final bit pointer is',I10,', expected =',I10,
     X        '.')
C
 9900 FORMAT (' D2ORDR: Function return code =',I6,', HOPER = ',A,
     X        ', KNSPT =',I10,'.')
C      
      END
