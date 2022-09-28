C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2PKVW ( KDATA,  KLENP,  KGRIB,  KLENG,  KNSPT,
     X                          KBITS,  KLEN,   KREFGR, KLENGR, KBITGR,
     X                          KWORK,  KLWORK, KFIROV, KCOUNT, KPOWER,
     X                          KMXPWR, OROW,   OVECTA, ODEBUG )
C
C---->
C**** C2PKVW
C
C     Purpose.
C     --------
C
C     Computes and packs second-order values for variable width methods
C     of second-order packing.
C
C
C**   Interface.
C     ----------
C
C     KRET = C2PKVW ( KDATA,  KLENP,  KGRIB,  KLENG,  KNSPT,  KBITS,
C    X                KLEN,   KREFGR, KLENGR, KBITGR, KWORK,  KLWORK,
C    X                KGRIB,  KCOUNT, KPOWER, KMXPWR, OROW,   OVECTA,
C    X                ODEBUG )
C
C
C     Input Parameters.
C     ----------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KLENG      - Length of GRIB product array.
C     KNSPT      - Bit pointer for next value in GRIB product.
C     KBITS      - Number of bits per computer word.
C     KLEN       - Exact number of grid-points to handle.
C     KREFGR     - Work array, first-order values for each group.
C     KLENGR     - Work array, effective length of each group.
C     KBITGR     - Work array, bit number ("width") of each group.
C     KLWORK     - Length of work arrays.
C     KFIROV     - Number of FIRst-Order Values.
C     KCOUNT     - Initial index into KDATA.
C     KPOWER     - Auxilary array, containing precomputed powers of 2-1.
C     KMXPWR     - Maximum number of bits per original scaled value.
C     OROW       - True if row by row packing is active packing method.
C     OVECTA     - True if vector-oriented algorithm has to be used.
C     ODEBUG     - True for some debug printout.
C
C
C     Output Parameters.
C     -----------------
C
C     KDATA      - Used as work array for second-order values.
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product (updated).
C     KREFGR     - Work array, re-used internally.
C     KBITGR     - Work array, re-used internally.
C     KWORK      - Work array.
C     KCOUNT     - Final index into KDATA.
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
C     INXBIT    - Insert bits into GRIB product.
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
C
C     KDATA is overwritten. This is coherent with GRIBEX coding.
C
C
C     Author.
C     -------
C
C     J. Clochard, Meteo France, for ECMWF - September 1998.
C
C
C     Modifications.
C     _____________
C
C     None.
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
      INTEGER KLENP, KLENG, KNSPT, KBITS, KLEN, KLWORK, KFIROV, KCOUNT
      INTEGER KMXPWR
      INTEGER KDATA (KLENP), KGRIB (KLENG), KPOWER (0:KMXPWR)
      INTEGER KREFGR (KLWORK), KLENGR (KLWORK), KBITGR (KLWORK)
      INTEGER KWORK (KLWORK)
C
      LOGICAL OROW, OVECTA, ODEBUG
C
C     Local variables.
C
      INTEGER ICOUNT, JGROUP, ILNGTH, IRET, J, IWIDTH, IREFER, IOFF
      INTEGER IRETFN, IPRWID, JTIMES, ILNGTS, IDATA,  ICOUNB, IBLOCK
      INTEGER JBLOCK, IFIRST, ILBITS, IBIT,   IWIDPR, INTERM, IPOWER
C
      CHARACTER YOPER*1
C
C     -----------------------------------------------------------------
C*    Section 1 . Initialisations.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      IRETFN=0
      YOPER='C'
      ICOUNT=KCOUNT
C
C     -----------------------------------------------------------------
C*    Section 2. Blocks groups of same width, compute 2nd-order values.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
C         Determin blocks of neighbour groups of same width to be
C         "packed" together for coding, and compute 2nd-order values.
C
C         In both cases any constant groups are not taken into account.
C
      ILNGTS=0
      ICOUNB=0
      IBLOCK=0
C
C         Find first non-constant group.
C
      DO 201 J=1,KFIROV
C
        IF (KBITGR(J).NE.0) THEN
          IFIRST=J
          GOTO 202
        ENDIF
C
        ICOUNT=ICOUNT+KLENGR(J)
C
  201 CONTINUE
C
C         All groups are constant (constant field, at least piecewise).
C         In such a case there are no second-order values to code.
C
      IFIRST=KFIROV+1
      ICOUNT=KLEN
C
  202 CONTINUE
C
      IPRWID=KBITGR(MIN (IFIRST,KFIROV))
C
      DO 204 JGROUP=IFIRST,KFIROV
C
        IWIDTH=KBITGR(JGROUP)
        ILNGTH=KLENGR(JGROUP)
        IREFER=KREFGR(JGROUP)
C
        IF (IWIDTH.NE.0) THEN
C
C              Compute second-order values for current group.
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
          DO 203 J=1,ILNGTH
C
            KDATA(ICOUNB+J)=KDATA(ICOUNT+J)-IREFER
C
  203     CONTINUE
C
          IF (IWIDTH.NE.IPRWID) THEN
C
C             Current group may not be blocked with previous one(s).
C
            IF (ILNGTS.NE.0) THEN
C
C               Closes current block.
C
              IBLOCK=IBLOCK+1
              KBITGR(IBLOCK)=IPRWID
              KWORK(IBLOCK)=ILNGTS
              ILNGTS=0
C
            ENDIF
C
            IPRWID=IWIDTH
C
          ENDIF
C
          ICOUNB=ICOUNB+ILNGTH
          ILNGTS=ILNGTS+ILNGTH
C
        ENDIF
C
        ICOUNT=ICOUNT+ILNGTH
C
  204 CONTINUE
C
      IF (IPRWID.NE.0.AND.ILNGTS.NE.0) THEN
C
C           Closing last block of group(s).
C
        IBLOCK=IBLOCK+1
        KBITGR(IBLOCK)=IPRWID
        KWORK(IBLOCK)=ILNGTS
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3. Packs second-order values.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
      ILBITS=0
      ICOUNB=0
      ILNGTS=0
C
      IF (OROW.OR..NOT.OVECTA) THEN
C
C           For such a case, granularity is large enough to make one
C           INXBIT call per block of group(s).
C
        DO 311 JBLOCK=1,IBLOCK
C
          ILNGTH=KWORK(JBLOCK)
          IWIDTH=KBITGR(JBLOCK)
#ifdef ebug2o
          PRINT *,'c2pkvw - block',JBLOCK,
     X              ' KNSPT/IWIDTH/ICOUNB/ILNGTH = ',
     X                KNSPT, IWIDTH, ICOUNB, ILNGTH
#endif
          CALL INXBIT(KGRIB,KLENG,KNSPT,KDATA(ICOUNB+1),ILNGTH,KBITS,
     X                IWIDTH,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 24310
            WRITE(GRPRSM,FMT=9310) 'block', JBLOCK, ILNGTH, IWIDTH
            GO TO 900
          ENDIF
C
          ICOUNB=ICOUNB+ILNGTH
C
  311   CONTINUE
C
      ELSE
C
C       Granularity is generally small. Except for very big groups,
C       split 2nd-order values into individual bits, blocked into
C       KREFGR work array, that is flushed when full.
C
C
        DO 399 JBLOCK=1,IBLOCK
C
          ILNGTH=KWORK(JBLOCK)
          IWIDTH=KBITGR(JBLOCK)
C
          IF ((ILBITS+ILNGTH*IWIDTH).GT.KLWORK.AND.ILBITS.NE.0) THEN
C
C             Work array of split bits has to be flushed. Pack it.
C
#ifdef ebug2o
            PRINT *,'c2pkvw - blocks..',JBLOCK-1,
     X              ' KNSPT/ILBITS = ',
     X                KNSPT, ILBITS
#endif
            CALL INXBIT(KGRIB,KLENG,KNSPT,KREFGR,ILBITS,KBITS,
     X                  1,YOPER,IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 24320
              WRITE(GRPRSM,FMT=9320) JBLOCK-1, ILBITS
              GO TO 900
            ENDIF
C
            ILBITS=0
C
          ENDIF
C
          IF ((ILNGTH*IWIDTH).GT.KLWORK) THEN
C
C             Current block is too large to fit into KREFGR.
C             Pack it straightforward, without intermediate splitting.
C
#ifdef ebug2o
            PRINT *,'c2pkvw - BLOCK',JBLOCK,
     X              ' KNSPT/IWIDTH/ICOUNB/ILNGTH = ',
     X                KNSPT, IWIDTH, ICOUNB, ILNGTH
#endif
            CALL INXBIT(KGRIB,KLENG,KNSPT,KDATA(ICOUNB+1),ILNGTH,KBITS,
     X                  IWIDTH,YOPER,IRET)
C
            IF (IRET.NE.0) THEN
              IRETFN = 24320
              WRITE(GRPRSM,FMT=9310) 'BLOCK', JBLOCK, ILNGTH, IWIDTH
              GO TO 900
            ENDIF          
C
          ELSE
C
C             Split current block into individual bits, stored into
C             array KREFGR (one bit per index value).
C
C             Start processing by "full" packets of 7-bit wide.
C             (even if 7-bit wide, last packet is processed separately).
C
            INTERM=(IWIDTH-1)/7
            IWIDPR=0
C
            DO 322 JTIMES=1,INTERM
C
              IOFF=ILBITS+IWIDPR
              IWIDPR=IWIDPR+7
              IPOWER=KPOWER(IWIDTH-IWIDPR)+1
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 321 J=1,ILNGTH
C
                IDATA=KDATA(ICOUNB+J)/IPOWER
                KDATA(ICOUNB+J)=KDATA(ICOUNB+J)-IPOWER*IDATA
                IBIT=IDATA/64
                KREFGR(IOFF+1)=IBIT
                IDATA=IDATA-64*IBIT
                IBIT=IDATA/32
                KREFGR(IOFF+2)=IBIT
                IDATA=IDATA-32*IBIT
                IBIT=IDATA/16
                KREFGR(IOFF+3)=IBIT
                IDATA=IDATA-16*IBIT
                IBIT=IDATA/8
                KREFGR(IOFF+4)=IBIT
                IDATA=IDATA-8*IBIT
                IBIT=IDATA/4
                KREFGR(IOFF+5)=IBIT
                IDATA=IDATA-4*IBIT
                IBIT=IDATA/2
                KREFGR(IOFF+6)=IBIT
                KREFGR(IOFF+7)=IDATA-2*IBIT
                IOFF=IOFF+IWIDTH
C
  321         CONTINUE
C
  322       CONTINUE
C
C             Now process last packet of bits (least significant ones).
C             No more use to restore KDATA.
C
            IOFF=ILBITS+IWIDPR
            IPRWID=IWIDTH-IWIDPR
C
            IF (IPRWID.EQ.7) THEN
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 323 J=1,ILNGTH
C
                IBIT=KDATA(ICOUNB+J)/64
                KREFGR(IOFF+1)=IBIT
                IDATA=KDATA(ICOUNB+J)-64*IBIT
                IBIT=IDATA/32
                KREFGR(IOFF+2)=IBIT
                IDATA=IDATA-32*IBIT
                IBIT=IDATA/16
                KREFGR(IOFF+3)=IBIT
                IDATA=IDATA-16*IBIT
                IBIT=IDATA/8
                KREFGR(IOFF+4)=IBIT
                IDATA=IDATA-8*IBIT
                IBIT=IDATA/4
                KREFGR(IOFF+5)=IBIT
                IDATA=IDATA-4*IBIT
                IBIT=IDATA/2
                KREFGR(IOFF+6)=IBIT
                KREFGR(IOFF+7)=IDATA-2*IBIT
                IOFF=IOFF+IWIDTH
C
  323         CONTINUE
C
            ELSEIF (IPRWID.EQ.6) THEN
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 324 J=1,ILNGTH
C
                IBIT=KDATA(ICOUNB+J)/32
                KREFGR(IOFF+1)=IBIT
                IDATA=KDATA(ICOUNB+J)-32*IBIT
                IBIT=IDATA/16
                KREFGR(IOFF+2)=IBIT
                IDATA=IDATA-16*IBIT
                IBIT=IDATA/8
                KREFGR(IOFF+3)=IBIT
                IDATA=IDATA-8*IBIT
                IBIT=IDATA/4
                KREFGR(IOFF+4)=IBIT
                IDATA=IDATA-4*IBIT
                IBIT=IDATA/2
                KREFGR(IOFF+5)=IBIT
                KREFGR(IOFF+6)=IDATA-2*IBIT
                IOFF=IOFF+IWIDTH
C
  324         CONTINUE
C
            ELSEIF (IPRWID.EQ.5) THEN
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 325 J=1,ILNGTH
C
                IBIT=KDATA(ICOUNB+J)/16
                KREFGR(IOFF+1)=IBIT
                IDATA=KDATA(ICOUNB+J)-16*IBIT
                IBIT=IDATA/8
                KREFGR(IOFF+2)=IBIT
                IDATA=IDATA-8*IBIT
                IBIT=IDATA/4
                KREFGR(IOFF+3)=IBIT
                IDATA=IDATA-4*IBIT
                IBIT=IDATA/2
                KREFGR(IOFF+4)=IBIT
                KREFGR(IOFF+5)=IDATA-2*IBIT
                IOFF=IOFF+IWIDTH
C
  325         CONTINUE
C
            ELSEIF (IPRWID.EQ.4) THEN
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 326 J=1,ILNGTH
C
                IBIT=KDATA(ICOUNB+J)/8
                KREFGR(IOFF+1)=IBIT
                IDATA=KDATA(ICOUNB+J)-8*IBIT
                IBIT=IDATA/4
                KREFGR(IOFF+2)=IBIT
                IDATA=IDATA-4*IBIT
                IBIT=IDATA/2
                KREFGR(IOFF+3)=IBIT
                KREFGR(IOFF+4)=IDATA-2*IBIT
                IOFF=IOFF+IWIDTH
C
  326         CONTINUE
C
            ELSEIF (IPRWID.EQ.3) THEN
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 327 J=1,ILNGTH
C
                IBIT=KDATA(ICOUNB+J)/4
                KREFGR(IOFF+1)=IBIT
                IDATA=KDATA(ICOUNB+J)-4*IBIT
                IBIT=IDATA/2
                KREFGR(IOFF+2)=IBIT
                KREFGR(IOFF+3)=IDATA-2*IBIT
                IOFF=IOFF+IWIDTH
C
  327         CONTINUE
C
            ELSEIF (IPRWID.EQ.2) THEN
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 328 J=1,ILNGTH
C
                IBIT=KDATA(ICOUNB+J)/2
                KREFGR(IOFF+1)=IBIT
                KREFGR(IOFF+2)=KDATA(ICOUNB+J)-2*IBIT
                IOFF=IOFF+IWIDTH
C
  328         CONTINUE
C
            ELSE
C
              DO 329 J=1,ILNGTH
C
                KREFGR(IOFF+1)=KDATA(ICOUNB+J)
                IOFF=IOFF+IWIDTH
C
  329         CONTINUE
C
            ENDIF
C
            ILBITS=ILBITS+ILNGTH*IWIDTH
C
          ENDIF
C
          ICOUNB=ICOUNB+ILNGTH
C
  399   CONTINUE
C
        IF (ILBITS.NE.0) THEN
C
C             Last flush for work array of split bits. Pack it.
C
#ifdef ebug2o
          PRINT *,'c2pkvw - blocks..',IBLOCK,
     X            ' KNSPT/ILBITS = ',
     X              KNSPT, ILBITS
#endif
          CALL INXBIT(KGRIB,KLENG,KNSPT,KREFGR,ILBITS,KBITS,
     X                1,YOPER,IRET)
C
          IF (IRET.NE.0) THEN
            IRETFN = 24320
            WRITE(GRPRSM,FMT=9320) IBLOCK, ILBITS
            GO TO 900
          ENDIF
C
        ENDIF
C
      ENDIF
C
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      KCOUNT=ICOUNT
      C2PKVW = IRETFN
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
 9100 FORMAT (' C2PKVW: Function start, KNSPT =',I10,'.')
C
 9310 FORMAT (' C2PKVW: Problem inserting 2nd-order values, ',A,I7,
     S        ',',I6,'*',I2,' bits.')
C
 9320 FORMAT (' C2PKVW: Problem inserting 2nd-order values, blocks..',
     S        I7,',',I9,' bits.')
C
 9900 FORMAT (' C2PKVW: Function return code =',I6,', KNSPT =',I10,'.')
C
      END
