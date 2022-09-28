C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2ROWS ( KDATA,  KLENP,  KSEC2,  KSEC4,  KGRIB,
     X                          KLENG,  KNSPT,  KBITS,  OCOMGR, OLONLY,
     X                          KLEN4,  KBMAP,  KVALS,  OPRIBI, OQUASI,
     X                          KNROWS, KLEN,   KREFGR, KLENGR, KBITGR,
     X                          KWORK,  KLWORK, KPOWER, KMXPWR, KPCWID,
     X                          KXCWID, KNBIFO, ODEBUG )
C
C---->
C**** C2ROWS
C
C     Purpose.
C     --------
C
C     Performs "row by row packing" method for section 4 of GRIB,
C     and/or computes only length of section 4, with/without coding,
C     with this method.
C
C
C**   Interface.
C     ----------
C
C     KRET = C2ROWS ( KDATA,  KLENP,  KSEC2,  KSEC4,  KGRIB,  KLENG,
C    X                KNSPT,  KBITS,  OCOMGR, OLONLY, KLEN4,  KBMAP,
C    X                KVALS,  OPRIBI, OQUASI, KNROWS, KLEN,   KREFGR,
C    X                KLENGR, KBITGR, KWORK,  KLWORK, KPOWER, KMXPWR,
C    X                KPCWID, KXCWID, KNBIFO, ODEBUG )
C
C
C     Input Parameters for all options.
C     --------------------------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KSEC2      - Array of GRIB section 2 integer descriptors.
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C     KGRIB      - Array containing GRIB product (for bit-map).
C     KLENG      - Length of GRIB product array.
C     KNSPT      - Bit pointer for next value in GRIB product.
C     KBITS      - Number of bits per computer word.
C     OCOMGR     - True if COMputing of GRoup arrays is requested.
C     OLONLY     - True if Length ONLY is requested (no coding).
C     KBMAP      - Bit pointer for start of explicit primary Bit-MAP
C                  (if any).
C     KVALS      - Number of bits in primary bit-map.
C     OPRIBI     - True if a PRImary BIt-map is present.
C     OQUASI     - True if the grid is QUASI-regular.
C     KNROWS     - Number of ROWS (or lines) in the grid.
C     KLEN       - Exact number of grid-points to handle.
C     KLWORK     - Length of work array KWORK.
C     KPOWER     - Auxilary array, containing precomputed powers of 2-1.
C     KMXPWR     - Maximum number of bits per original scaled value.
C     KPCWID     - Auxilary array, precomputed widths.
C     KXCWID     - Maximum width associated to KPCWID.
C     ODEBUG     - True for some debug printout.
C
C
C     Output Parameters for all options.
C     ---------------------------------
C
C     KLEN4      - Length of section 4.
C
C     Output Parameters when OLONLY is false.
C     --------------------------------------
C
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C                  (for second-order descriptors)
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product (updated).
C     KWORK      - Used as work array by C2PACK.
C
C     Output Parameters when OCOMGR is true.
C     -------------------------------------
C
C     KWORK      - Work array, used to unpack a primary bit-map, if any.
C
C     Output Parameters when OCOMGR is true,
C      Input Parameters when OCOMGR is false.
C     --------------------------------------
C
C     KREFGR     - Work array, first-order values for each row.
C     KLENGR     - Work array, effective length of each row.
C     KBITGR     - Work array, bit number ("width") of each row.
C     KNBIFO     - Number of BIts for coding First-Order values.
C
C     Output Parameters when OCOMGR is true,
C      Input parameter in all cases.
C     --------------------------------------
C
C     KDATA      - Used as work array by C2PACK.
C
C     Method.
C     -------
C
C     Follows WMO Manual of Codes.
C
C     If requested through OCOMGR, a first scan computes work arrays,
C     bit number for coding first-order values and length of section 4.
C
C     If requested, coding is applied in a second scan, using
C     descriptors computed in the first scan, possibly in a previous
C     call.
C
C     Externals.
C     ----------
C
C     INXBIT    - Extract bits from GRIB product.
C     MAXMNI    - Computes extrema of integer array.
C     C2BITW    - Computes bit width of a positive integer value.
C     C2PACK    - Encodes descriptors and data.
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
C     KDATA is overwritten when coding is requested.
C     This is coherent with GRIBEX.
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
C     J. Clochard, February 1998.
C     Use C2BITW function for bit width computations.
C
C     J. Clochard, September 1998.
C     Suppress KSEC1/KSEC3 dummy-arguments.
C         "    KSEC1, add KWORK/KPOWER/KMXPWR argument in C2PACK call.
C
C     J. Clochard, June 1999.
C     Use precomputed array to determine widths . Dummy-arguments
C     KPCWID and KXCWID added . Assumption is made that KXCWID
C     is at least (KMXPWR+1)/2 .
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
C     Subroutine arguments
C
      INTEGER KLENP, KLENG, KNSPT, KBITS, KBMAP, KVALS, KLEN4, KNROWS
      INTEGER KLEN, KLWORK, KMXPWR, KNBIFO, KXCWID
      INTEGER KDATA (KLENP), KSEC2 (*), KSEC4 (*), KPCWID (0:*)
      INTEGER KGRIB (KLENG), KREFGR (KNROWS), KLENGR (KNROWS)
      INTEGER KBITGR (KNROWS), KWORK (KLWORK), KPOWER (0:KMXPWR)
C
      LOGICAL OPRIBI, OQUASI, OLONLY, OCOMGR, ODEBUG
C
C     Local variables.
C
      INTEGER IREFLN, ICOUNT, INBITI, INBIFO, ILENSO, ILDSTA, IZERO
      INTEGER ILNGTX, ILNGTH, IPTRBM, INBSEQ, JROW, JSEQ, IPTOFF
      INTEGER IPTNBR, IRET, J, IDMAX, IDMIN, ISOMAX, ILCWID
      INTEGER IRETFN, IBITGW, IFOMAX
C
      INTEGER C2PACK, C2BITW
      EXTERNAL C2PACK, C2BITW
C
      CHARACTER YOPER*1
C
C     -----------------------------------------------------------------
C*    Section 1 . Performs initial checks.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) OCOMGR, OLONLY, KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      IRETFN = 0
      ILCWID = KPOWER(KXCWID)+1
      IREFLN=22
      ICOUNT=0
      INBITI=KSEC4(2)
      IFOMAX=0
      ILENSO=0
      ILDSTA=21
      IZERO=0
      IBITGW=8
      ILNGTX=KSEC4(1)/KNROWS
C
      IF (OPRIBI) THEN
        IPTRBM=KBMAP
      ENDIF
C
      IF (.NOT.OCOMGR) THEN
C
        IF (OLONLY) THEN
          IRETFN = 19110
          WRITE(GRPRSM,FMT=9110)
          GOTO 900
        ELSE
          INBIFO=KNBIFO
          GOTO 300
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Compute effective length, reference, width of rows.
C     -----------------------------------------------------------------
C
  200 CONTINUE
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9200)
#ifndef ebug2o
      ENDIF
C
#endif
C
      YOPER='D'
C
      DO 230 JROW=1,KNROWS
C
      IF (OQUASI) THEN
        ILNGTX=KSEC2(IREFLN+JROW)
      ENDIF
C
      IF (OPRIBI.AND.ILNGTX.NE.0) THEN
C
C         Row by row packing, but an (explicit) bit-map is included.
C         The number of values in current row has to be computed.
C
        ILNGTH=0
        INBSEQ=1+(ILNGTX-1)/KLWORK
C
        DO 212 JSEQ=1,INBSEQ
        IPTOFF=(JSEQ-1)*KLWORK
        IPTNBR=MIN (KLWORK,ILNGTX-IPTOFF)
        CALL INXBIT (KGRIB,KLENG,IPTRBM,KWORK,IPTNBR,KBITS,1,
     X               YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 19210
          WRITE(GRPRSM,FMT=9210) JROW, IPTOFF+1, IPTOFF+IPTNBR
          GOTO 900
        ENDIF
C
        DO 211 J=1,IPTNBR
        ILNGTH=ILNGTH+KWORK(J)
  211   CONTINUE
C
  212   CONTINUE
C
      ELSE
C
        ILNGTH=ILNGTX
C
      ENDIF
C
      KLENGR(JROW)=ILNGTH
C
      IF (ILNGTH.NE.0) THEN
C
C     Width of row (number of bits necessary to code second-order
C                   values).
C
        CALL MAXMNI (KDATA(ICOUNT+1),ILNGTH,IDMAX,IDMIN)
C
        ISOMAX=IDMAX-IDMIN
C
        IF (ISOMAX.LT.ILCWID) THEN
          KBITGR(JROW)=KPCWID(ISOMAX)
        ELSE
          KBITGR(JROW)=KXCWID+KPCWID(ISOMAX/ILCWID)
        ENDIF
#ifdef row1sc2o
C
C     Take the minimum value of row as reference value.
C     Consistant with Bracknell's encoder approach.
C
        KREFGR(JROW)=IDMIN
#else
C
C     Maximum use of width for second-order values: the maximum value
C     of row will be encoded with the biggest value fitting in
C     KBITGR(JROW) bits, and keeping reference value positive or zero.
C
C     The goal is to get the lowest reference value, thus trying to
C     save bit(s) to encode the first-order values.
C
        IF (KBITGR(JROW).NE.0) THEN
          KREFGR(JROW)=MAX (0,IDMIN-(KPOWER(KBITGR(JROW))-ISOMAX))
        ELSE
          KREFGR(JROW)=IDMIN
        ENDIF
#endif
C
        IFOMAX=MAX (IFOMAX,KREFGR(JROW))
        ICOUNT=ICOUNT+ILNGTH
        ILENSO=ILENSO+ILNGTH*KBITGR(JROW)
C
      ELSE
C
        KREFGR(JROW)=0
        KBITGR(JROW)=0
C
      ENDIF
C
#ifdef ebug2o
      PRINT *,'c2rows - group', JROW, ILNGTH, KBITGR(JROW), ' (',
     X KNSPT+8*(ILDSTA+(8+KNROWS*(IBITGW+KSEC4(2))-1)/8)+ILENSO, ')',
     X IDMIN, ISOMAX, KREFGR(JROW), ICOUNT
#endif
C
  230 CONTINUE
C
      IF (ICOUNT.NE.KLEN) THEN
        IRETFN = 19220
        WRITE(GRPRSM,FMT=9220) ICOUNT, KLEN
        GOTO 900
      ENDIF      
C
C     Number of bits necessary to code first-order values.
C
      INBIFO = C2BITW ( IFOMAX, INBITI, KPOWER, KMXPWR )
C
#ifdef row1sc2o
C     To obtain an equivalent to a 1-scan coding, as Bracknell
C     coder seems to do. Such a method is more direct, does not
C     require work arrays, but is less efficient: row references are
C     coded with the same number of bits as specified for packing.
C
      INBIFO=KSEC4(2)
#endif
C
C     Compute length of descriptors, and of full section 4 of GRIB.
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9250) (KSEC4(J),J=1,10), INBIFO
#ifndef ebug2o
      ENDIF
C
#endif
      KLEN4 = ILDSTA + (8+KNROWS*IBITGW-1)/8 + (8+KNROWS*INBIFO-1)/8
     X               + (8+ILENSO-1)/8
      KLEN4=2*((2+KLEN4-1)/2)
      KNBIFO=INBIFO
C
      IF (OLONLY) THEN
C
C       Length has been computed, go back without any coding.
C
        GOTO 900
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3. Definition of fixed-length descriptors and coding.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
C     The first 10 octets have already been coded in GRIBEX, or will
C     be in a further step of GRIBEX:
C     Length of section, flags+unused bit count, binary scale factor,
C     reference value.
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9300) 
#ifndef ebug2o
      ENDIF
C
#endif
C     Enforce right values for extended flags.
C
      KSEC4( 9)=0
      KSEC4(10)=16
      KSEC4(12)=0
      KSEC4(13)=0
C
C     Extra descriptors.
C
      KSEC4(18)=KNROWS
      KSEC4(20)=IBITGW
C
      IRETFN = C2PACK ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,  KNSPT,
     X                  KBITS,  KLEN,   KREFGR, KLENGR, KBITGR, KWORK,
     X                  KLWORK, KPOWER, KMXPWR, KNBIFO, ODEBUG )
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      C2ROWS = IRETFN
#ifndef ebug2o
C
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9900) IRETFN, OCOMGR, OLONLY, KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      RETURN
C
 9100 FORMAT (' C2ROWS: Function start, OCOMGR = ',L1,
     X        ', OLONLY = ',L1,', KNSPT =',I10,'.')
C
 9110 FORMAT (' C2ROWS: Routine called, but has nothing to do.')
C
 9200 FORMAT (' C2ROWS: Computing work arrays.')
C
 9210 FORMAT (' C2ROWS: Problem unpacking primary bit-map,row',I6,
     X        ' points ',I8,'..',I8,'.')
C
 9220 FORMAT (' C2ROWS: Inconsistency: computed g.p. number =',I8,
     X        ', expected =',I8,'.')
C
 9250 FORMAT (' C2ROWS: KSEC4(1:10) =',I9,I3,I4,3I3,I2,3I3,
     X        ', INBIFO =',I3,'.')
C
 9300 FORMAT (' C2ROWS: Coding phase - calling C2PACK.')
C
 9900 FORMAT (' C2ROWS: Function return code =',I6,', OCOMGR= ',L1,
     X        ', OLONLY= ',L1,', KNSPT =',I10,'.')
C
      END
