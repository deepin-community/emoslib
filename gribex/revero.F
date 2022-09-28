C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION REVERO ( KDATA,  KLENP,  KSEC2,  KGRIB,  KLENG,
     X                          KBITS,  KBMAP,  KVALS,  OPRIBI, OQUASI,
     X                          KNROWS, KLEN,   KWORK,  KLWORK, ODEBUG,
     X                          OPRCLN, KLENGR )
C
C---->
C**** REVERO
C
C     Purpose.
C     --------
C
C     Reverse order of even rows in a field of integer values.
C     This function is used for handling of boustrophedonic ordering
C     in extended second-order packing methods.
C
C
C**   Interface.
C     ----------
C
C     KRET = REVERO ( KDATA,  KLENP,  KSEC2,  KGRIB,  KLENG,  KBITS,
C    X                KBMAP,  KVALS,  OPRIBI, OQUASI, KNROWS, KLEN,
C    X                KWORK,  KLWORK, ODEBUG, OPRCLN, KLENGR )
C
C     Input Parameters.
C     ----------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KSEC2      - Array of GRIB section 2 integer descriptors.
C     KGRIB      - Array containing GRIB product (for bit-map).
C     KLENG      - Length of GRIB product array.
C     KBITS      - Number of bits per computer word.
C     KBMAP      - Bit pointer for start of explicit primary Bit-MAP
C                  (if any).
C     KVALS      - Number of bits in primary bit-map.
C     OPRIBI     - True if a PRImary BIt-map is present.
C     OQUASI     - True if the grid is QUASI-regular.
C     KNROWS     - Number of ROWS (or lines) in the grid.
C     KLEN       - Exact number of grid-points to handle.
C     KLWORK     - Length of work array KWORK.
C     ODEBUG     - True for some debug printout.
C     OPRCLN     - True if row lengthes have been pre-computed.
C
C     Input Parameters if OPRCLN is TRUE.
C     ------------------------------------
C     KLENGR     - Effective LENGth of Rows array.
C
C     Output Parameters if OPRCLN is FALSE.
C     ------------------------------------
C
C     KWORK      - Work array, used to unpack a primary bit-map, if any.
C
C     Method.
C     -------
C
C     Determines effective row length(es), and swaps fields values
C     for rows with an even rank.
C
C     Externals.
C     ----------
C
C     None.
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
C     Note that arrays KLENGR and KWORK are not used in the same call,
C     so they may overlap.
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
C
C     Subroutine arguments
C
      INTEGER KLENP, KBMAP, KVALS, KNROWS, KLEN, KLWORK, KLENG, KBITS
      INTEGER KDATA (KLENP), KSEC2 (*), KWORK (KLWORK), KGRIB (KLENG)
      INTEGER KLENGR (KNROWS)
C
      LOGICAL OPRIBI, OQUASI, ODEBUG, OPRCLN
C
C     Local variables.
C
      INTEGER IREFLN, IPTRBM, IRETFN, JROW, IPTOFF, ILNGTX, INBSEQ, J
      INTEGER JSEQ, IPTNBR, ICOUNT, ILNGTH, ILEN, INDEX1, INDEX2, IRET
      INTEGER IAUXIL
C
      CHARACTER YOPER*1
C
C     -----------------------------------------------------------------
C*    Section 1 . Performs initial computations.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) OPRCLN
#ifndef ebug2o
      ENDIF
#endif
C
      YOPER='D'
C
      IRETFN = 0
      IREFLN=22
      ICOUNT=0
C
      IF (OPRIBI) THEN
        IPTRBM=KBMAP
        ILEN=KVALS
      ELSE
        ILEN=KLEN
      ENDIF
C
      ILNGTX=ILEN/KNROWS
C
C     -----------------------------------------------------------------
C*    Section 2 . Loop on rows, wuth values swap for even rank rows.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      DO 222 JROW=1,KNROWS
C
      IF (OQUASI) THEN
        ILNGTX=KSEC2(IREFLN+JROW)
      ENDIF
C
      IF (.NOT.OPRIBI.OR.ILNGTX.EQ.0) THEN
C
C         Effective length of row = full length.
C
        ILNGTH=ILNGTX
C
      ELSEIF (OPRCLN) THEN
C
        ILNGTH=KLENGR (JROW)
C
      ELSE
C
C         An (explicit) bit-map is included.
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
          IRETFN = 23210
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
      ENDIF
C
      IF (MOD (JROW,2).EQ.0.AND.ILNGTH.GT.1) THEN
C
C             Swap field values within row.
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
        DO 221 J=1,ILNGTH/2
        INDEX1=ICOUNT+J
        INDEX2=ICOUNT+ILNGTH+1-J
        IAUXIL=KDATA(INDEX1)
        KDATA(INDEX1)=KDATA(INDEX2)
        KDATA(INDEX2)=IAUXIL
  221   CONTINUE
C
      ENDIF
C
      ICOUNT=ICOUNT+ILNGTH
C
  222 CONTINUE
C
      IF (ICOUNT.NE.KLEN) THEN
        IRETFN = 23220
        WRITE(GRPRSM,FMT=9220) ICOUNT, KLEN
        GOTO 900
      ENDIF      
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      REVERO = IRETFN
#ifndef ebug2o
C
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9900) IRETFN
#ifndef ebug2o
      ENDIF
#endif
C
      RETURN
C
 9100 FORMAT (' REVERO: Function start, OPRCLN = ',L1,'.')
C
 9210 FORMAT (' REVERO: Problem unpacking primary bit-map,row',I6,
     X        ' points ',I8,'..',I8,'.')
C
 9220 FORMAT (' REVERO: Inconsistency: computed g.p. number =',I8,
     X        ', expected =',I8,'.')
C
 9900 FORMAT (' REVERO: Function return code =',I6,'.')
C
      END
