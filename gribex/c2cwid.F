C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2CWID ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,
     X                          KNSPT,  KBITS,  OCOMGR, OLONLY, KLEN4,
     X                          KFIROV, KLEN,   KREFGR, KLENGR, KLWORK,
     X                          KSTART, KPOWER, KMXPWR, KNBIFO, KNCMIN,
     X                          KLWORX, KIWORK, ODEBUG, OPTLEN )
C
C---->
C**** C2CWID
C
C     Purpose.
C     --------
C
C     Performs "constant width packing" method for section 4 of GRIB,
C     and/or computes only length of section 4, with/without coding,
C     with this method.
C
C
C**   Interface.
C     ----------
C
C     KRET = C2CWID ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,  KNSPT,
C    X                KBITS,  OCOMGR, OLONLY, KLEN4,  KFIROV, KLEN,
C    X                KREFGR, KLENGR, KLWORK, KSTART, KPOWER, KMXPWR,
C    X                KNBIFO, KNCMIN, KLWORX, KIWORK, ODEBUG, OPTLEN )
C
C
C     Input Parameters for all options.
C     --------------------------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C     KLENG      - Length of GRIB product array.
C     KNSPT      - Bit pointer for next value in GRIB product.
C     KBITS      - Number of bits per computer word.
C     OCOMGR     - True if COMputing of GRoup arrays is requested.
C     OLONLY     - True if Length ONLY is requested (no coding).
C     KLEN       - Exact number of grid-points to handle.
C     KLWORK     - First dimension of work arrays.
C     KPOWER     - Auxilary array, containing precomputed powers of 2-1.
C     KMXPWR     - Maximum number of bits per original scaled value.
C     KNCMIN     - Minimum increment to use when determining groups.
C     ODEBUG     - True for some debug printout.
C     OPTLEN     - OPTimise LENgth of section 4 (if OCOMGR is true).
C
C
C     Output Parameters for all options.
C     ---------------------------------
C
C     KSEC4      - Array of GRIB section 4 integer descriptors:
C                  KSEC4(11) when OCOMGR and OPTLEN are true;
C                  other indexes when OLONLY is false.
C     KLEN4      - Length of section 4.
C     KLWORX     - Maximum work space used.
C
C     Output Parameters when OLONLY is false.
C     --------------------------------------
C
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product (updated).
C
C     Output Parameters when OCOMGR is true,
C      Input Parameters when OCOMGR is false.
C     --------------------------------------
C
C     KFIROV     - Number of FIrst-Order Values (and groups) determined.
C     KREFGR     - Work array, first-order values for each group.
C     KLENGR     - Work array, effective length of each group.
C     KNBIFO     - Number of BIts for coding First-Order values.
C     KIWORK     - Second index into work arrays corresponding to
C                  optimal or computed case.
C
C     Output Parameters when OCOMGR is true,
C      Input parameter in all cases.
C     --------------------------------------
C
C     KDATA      - Used as work array by C2PACK.
C
C     Input Parameter when OCOMGR is false.
C     -------------------------------------
C
C     KSTART     - First index value of precomputed work arrays KREFGR
C                  and KLENGR.
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
C     When OPTLEN is true, an iterative process is performed in the
C     first scan, to choose an optimal width leading to minimum length
C     of message (through its section 4).
C
C     Externals.
C     ----------
C
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
C     J. Clochard, April 1998.
C     Use C2BITW function for bit width computations, out of
C     the splitting algorithm itself.
C     When optimisation is required, use -KSEC4(11) as initial and
C     maximum width for iterative search of optimal width.
C
C     J. Clochard, September 1998.
C     Suppress KSEC1/KSEC2/KSEC3 dummy-arguments.
C         "    KSEC1, add KWORK/KPOWER/KMXPWR argument in C2PACK call.
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
      INTEGER KLENP, KLENG, KNSPT, KBITS, KLEN4, KNCMIN, KLWORX, KIWORK
      INTEGER KLEN, KLWORK, KMXPWR, KNBIFO, KFIROV, KSTART
      INTEGER KDATA (KLENP), KSEC4 (*), KGRIB (KLENG), KREFGR (KLWORK,2)
      INTEGER KPOWER (0:KMXPWR), KLENGR (KLWORK,2)
C
      LOGICAL OLONLY, OCOMGR, ODEBUG, OPTLEN
C
C     Local variables.
C
      INTEGER INBITN, ICOUNT, INBITX, INBIFO, ILDSTA
      INTEGER IGROUP, INCR, ISIZEG, INBIT, INCMIR, IPOWER, ICWORK
      INTEGER J, IDMAX, IDMIN, ISOMAX, ICRITR, ICRITO, INCRBI
      INTEGER IRETFN, IBITGW, IFOMAX, INCMIN, INBITO, IDMAXG, IDMING
      INTEGER INBIFX, INBICW, IOFF, IFIROV, ISTART, ILWORK
C
      INTEGER C2PACK, C2BITW
      EXTERNAL C2PACK, C2BITW
C
      INTRINSIC ABS
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
        WRITE(GRPRSM,FMT=9100) OCOMGR, OPTLEN, OLONLY, KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      IRETFN = 0
      ICOUNT=0
      ILDSTA=21
      IBITGW=8
      INBITO=KSEC4(2)
      KLWORX=0
C
      IF (.NOT.OCOMGR) THEN
C
        IF (OLONLY) THEN
          IRETFN = 21110
          WRITE(GRPRSM,FMT=9110)
          GOTO 900
        ELSE
          INBIFO=KNBIFO
          INBICW=KSEC4(11)
          IFIROV=KFIROV
          KLWORX=KFIROV
          ISTART=KSTART
          GOTO 300
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Computing of references and lengths of groups.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      IFIROV=(KLWORK+1)
      INBIFO=INBITO
      INBICW=INBITO
      INCMIN=MIN (MAX (KNCMIN,1),1+KLEN/2)
      ISIZEG=1 + INT ( SQRT ( REAL (KLEN-1) ) /5. )
      ISIZEG=MAX (ISIZEG,INCMIN)
      ICWORK=1
      ISTART=1
C
C     Initial value for criterion to minimize in iterative process.
C     Over-estimated (expressed in bits instead of octets).
C
      ICRITO=8+IFIROV*(INBIFO+1)-1+8+(KLEN+1)*(INBICW+1)-1
C
      IF (OPTLEN) THEN
C
C     In this case, no predetermined number of bits is assigned.
C     An iterative process will compute a number of bits leading to
C     a (relative) minimum length of coded message.
C
        INBITX=ABS (KSEC4(11))
        INBITN=0
        INBIT=MAX (INBITX-1,INBITN)
        INCRBI=-MIN ( 3 , MAX (1,(INBIT-INBITN)/3) )
      ELSE
C
C     Width (number of bits for second-order values) is KSEC4(11).
C
        INBITX=KSEC4(11)
        INBITN=KSEC4(11)
        INBIT=KSEC4(11)
        INCRBI=-1
      ENDIF
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9200) INBITO, (KSEC4(J),J=1,13),
     X                          INCMIN, ISIZEG
#ifndef ebug2o
      ENDIF
C
#endif
C     -----------------------------------------------------------------
C                     Start of iterative process.
C     -----------------------------------------------------------------
C
  210 CONTINUE
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9210) INBITX, INBITN, INBIT, INCRBI
#ifndef ebug2o
      ENDIF
C
#endif
      ICOUNT=0
      IGROUP=0
      IFOMAX=0
      IPOWER=KPOWER(INBIT)
C
C     -----------------------------------------------------------------
C                  Determination of an initial group.
C     -----------------------------------------------------------------
C
  220 CONTINUE
C
      ISIZEG=MIN (ISIZEG,KLEN-ICOUNT)
C
C     Extrema computed in-line for better efficiency.
C
      IDMAX=KDATA(ICOUNT+1)
      IDMIN=KDATA(ICOUNT+1)
C
      DO 221 J=2,ISIZEG
      IDMAX=MAX (IDMAX,KDATA(ICOUNT+J))
      IDMIN=MIN (IDMIN,KDATA(ICOUNT+J))
  221 CONTINUE
C
      ISOMAX=IDMAX-IDMIN
C     PRINT *,'c2cwid - ICOUNT=',ICOUNT,', ISIZEG=',ISIZEG,
C    S        ', IDMIN=',IDMIN,', IDMAX=',IDMAX,', ISOMAX=',ISOMAX
C
      IF (ISOMAX.GT.IPOWER) THEN
C
C     Initial segment does not fit within INBIT bits, reduce and retry.
C
        ISIZEG=ISIZEG/2
        GOTO 220
      ELSEIF (ISIZEG.LT.INCMIN.AND.IGROUP.NE.0) THEN
C
C     Initial group size very small. Try to join previous group...
C
        IDMAXG=MAX (IDMAX,IDMAXG)
        IDMING=MIN (IDMIN,IDMING)
        ISOMAX=IDMAXG-IDMING
C
        IF (ISOMAX.LE.IPOWER) THEN
C
C     ... succesfully.
C
          KLENGR(IGROUP,ICWORK)=KLENGR(IGROUP,ICWORK)+ISIZEG
          ICOUNT=ICOUNT+ISIZEG
          GOTO 241
        ENDIF
C
      ENDIF
C
      IDMAXG=IDMAX
      IDMING=IDMIN
      IOFF=ICOUNT+ISIZEG
      INCR=MIN (ISIZEG,KLEN-IOFF)
C
      IF (INCR.EQ.0) THEN
        GOTO 240
      ENDIF
C
C     -----------------------------------------------------------------
C                Try to add increments to initial group.
C     -----------------------------------------------------------------
C
  230 CONTINUE
C
      IDMAX=IDMAXG
      IDMIN=IDMING
C
#ifdef FUJITSU
!OCL SCALAR
#endif
      DO 231 J=1,INCR
      IDMAX=MAX (IDMAX,KDATA(IOFF+J))
      IDMIN=MIN (IDMIN,KDATA(IOFF+J))
  231 CONTINUE
#ifdef FUJITSU
!OCL VECTOR
#endif
C
      ISOMAX=IDMAX-IDMIN
C     PRINT *,'c2cwid - IOFF=',IOFF,', ISIZEG=',ISIZEG,
C    S      ', INCR=',INCR,', ISOMAX=',ISOMAX,', IGROUP+1=',IGROUP+1
C
C     If end of data very near, relax constraint on minimum increment.
C
      INCMIR=MAX ( 1, MIN (KLEN-IOFF,INCMIN) )
C
      IF (ISOMAX.LE.IPOWER) THEN
C
C     Incremental group added to current group.
C
        IDMAXG=IDMAX
        IDMING=IDMIN
        IOFF=IOFF+INCR
        INCR=MIN (ISIZEG,KLEN-IOFF)
C
        IF (INCR.NE.0) THEN
          GOTO 230
        ENDIF
C
      ELSEIF (INCR.GT.INCMIR) THEN
C
C     Incremental group does not fit, try a smaller one.
C
        INCR=MAX (INCR/2,INCMIR)
        GOTO 230
      ENDIF
C
C     -----------------------------------------------------------------
C     No incremental group may be added. Current group is closed.
C     -----------------------------------------------------------------
C
  240 CONTINUE
C
      IGROUP=IGROUP+1
      KLENGR(IGROUP,ICWORK)=IOFF-ICOUNT
      ICOUNT=IOFF
      ISOMAX=IDMAXG-IDMING
C
C     Maximum use of width for second-order values: the maximum value
C     of group will be encoded with the biggest value fitting in
C     INBIT bits, and keeping reference value positive or zero.
C
C     The goal is to get the lowest reference value, thus trying
C     (desperately) to save bit(s) to encode the first-order values.
C
  241 CONTINUE
C
      KREFGR(IGROUP,ICWORK)=MAX (0,IDMING-(IPOWER-ISOMAX))
      IFOMAX=MAX (IFOMAX,KREFGR(IGROUP,ICWORK))
#ifdef ebug2o
C
      IF (.NOT.OLONLY) THEN
        PRINT *,'c2cwid - group',IGROUP,KLENGR(IGROUP,ICWORK),INBIT
     X         ,' (...)',IDMING,ISOMAX,KREFGR(IGROUP,ICWORK),ICOUNT
      ENDIF
#endif
C
      IF (ICOUNT.EQ.KLEN) THEN
        GOTO 250
      ELSEIF (IGROUP.LT.KLWORK) THEN
        ISIZEG=MAX (INCMIN,ISIZEG,KLENGR(IGROUP,ICWORK)/2)
        GOTO 220
      ELSE
C
C     Out of work space. A message is issued, except in iterative
C     process, if the estimated length is already bigger than
C     previously computed.
C
        KLWORX=MAX (KLWORX,IGROUP)
        ICRITR=(8+(IGROUP+1)*INBIFO-1)/8+(8+KLEN*INBIT-1)/8
C
        IF (ICRITR.LT.ICRITO) THEN
          WRITE(GRPRSM,FMT=9240) KLWORK, INBIT
        ENDIF
C
        IF (INBIT.LT.INBITX.AND.INBIT.GT.INBITN) THEN
C
C     Other bit value(s) may still be tried.
C
          ICRITR=ICRITO+1
          GOTO 260
        ELSEIF (INBICW.NE.INBITO) THEN
C
C     A constant width has been successfully computed before.
C     It will be used as the best computable result.
C
          GOTO 270
        ELSE
          IRETFN = 21240
          GOTO 900
        ENDIF
C
      ENDIF
C
  250 CONTINUE
C
      KLWORX=MAX (KLWORX,IGROUP)
C
C     Number of bits necessary to code first-order values.
C
      INBIFX = C2BITW ( IFOMAX, INBITO, KPOWER, KMXPWR )
C
      ICRITR=(8+IGROUP*INBIFX-1)/8+(8+KLEN*INBIT-1)/8
C
  260 CONTINUE      
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9260) INBIT, INBIFX, IGROUP, ICRITR
#ifndef ebug2o
      ENDIF
C
#endif
      IF (ICRITR.LT.ICRITO) THEN
C
C     INBIT may be retained, at least temporarily, for constant width.
C
        INBICW=INBIT
        IFIROV=IGROUP
        INBIFO=INBIFX
        KIWORK=ICWORK
        ICWORK=1+MOD (ICWORK,2)
        ICRITO=ICRITR
C
        IF (INBIT.GT.INBITN) THEN
C
C     Decrease INBIT for next trial.
C
          IF (INCRBI.LT.0) THEN
            INCRBI=MAX ( INCRBI , MIN (-1,(INBITN-INBIT)/3) )
          ELSE
            INCRBI=MAX ( MIN (-1,1-INCRBI) , INBITN-INBIT )
          ENDIF
C
          INBITX=INBIT-1
          INBIT=INBIT+INCRBI
          GOTO 210
        ENDIF
C
      ENDIF
C
      IF (INBIT.LT.INBITX) THEN
C
C             INBIT not suitable, increase it and retry.
C
        INBITN=INBIT+1
        INCRBI=MIN ( MAX (ABS(1-INCRBI),1) , INBITX-INBIT )
        INBIT=INBIT+INCRBI
        GOTO 210
C
      ENDIF
C
C     Work done, and/or no better length than previously.
C
  270 CONTINUE      
C
C     Compute full length of section 4 of GRIB.
C 
      KLEN4=ILDSTA + 1 + (8+KLEN-1)/8 + ICRITO
      KLEN4=2*((2+KLEN4-1)/2)
      KNBIFO=INBIFO
      KFIROV=IFIROV
C
      IF (OPTLEN) THEN
        KSEC4(11)=INBICW
      ENDIF
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
      KSEC4( 9)=32
      KSEC4(10)=0
      KSEC4(12)=0
C
C     Extra descriptors.
C
      KSEC4(18)=KFIROV
      KSEC4(20)=IBITGW
      ILWORK=KLWORK-ISTART+1
C
C     KREFGR alone in call corresponds to a formal argument of function
C     C2PACK, which has to be there but is not used for constant width
C     packing.
C
      IRETFN = C2PACK ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,  KNSPT,
     X                  KBITS,  KLEN,   KREFGR(ISTART,KIWORK),
     X                  KLENGR(ISTART,KIWORK),          KREFGR,
     X                  KREFGR, ILWORK, KPOWER, KMXPWR, KNBIFO, ODEBUG )
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      C2CWID = IRETFN
#ifndef ebug2o
C
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9900) IRETFN, OCOMGR, OPTLEN, OLONLY, KNSPT
#ifndef ebug2o
      ENDIF
#endif
C
      RETURN
C
 9100 FORMAT (' C2CWID: Function start, OCOMGR = ',L1,
     X        ', OPTLEN = ',L1,', OLONLY = ',L1,', KNSPT =',I10,'.')
C
 9110 FORMAT (' C2CWID: Routine called, but has nothing to do.')
C
 9200 FORMAT (' C2CWID: Computing work arrays, INBITO =',I3,'.',/,
     X        ' C2CWID: KSEC4(1:13)=',I9,I3,I4,3I3,I2,4I3,2I2,'.',/,
     X        ' C2CWID: INCMIN =',I3,', ISIZEG =',I6,'.')
C
 9210 FORMAT (' C2CWID: INBITX =',I3,', INBITN =',I3,', INBIT =',I3,
     X        ', INCRBI =',I3,'.')
C
 9240 FORMAT (' C2CWID: Groups number exceeds work space (',I8,
     X        '), giving up for INBIT =',I3,'.')
C
 9260 FORMAT (' C2CWID: INBIT =',I3,', INBIFX =',I3,', IGROUP =',I8,
     X        ', ICRITR =',I9,'.')
C
 9300 FORMAT (' C2CWID: Coding phase - calling C2PACK.')
C
 9900 FORMAT (' C2CWID: Function return code=',I6,
     X        ', OCOMGR/OPTLEN/OLONLY/KNSPT=',3L1,I10,'.')
C
      END
