C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2GENE ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,
     X                          KNSPT,  KBITS,  OCOMGR, OLONLY, KLEN4,
     X                          KFIROV, KLEN,   KREFGR, KLENGR, KBITGR,
     X                          KWORK,  KLWORK, KSTART, KPOWER, KMXPWR,
     X                          KPCWID, KXCWID, KNBIFO, KNCMIN, KMINPK,
     X                          KLWORX, ODEBUG )
C
C---->
C**** C2GENE
C
C     Purpose.
C     --------
C
C     Performs a general second-order packing method for section 4
C     of GRIB, and/or computes only length of section 4, with/without
C     coding, with this method.
C
C     "General" means with explicit descriptors to describe groups
C     position and width (not row-by-row, nor constant-width).
C
C     When extended 2nd-order packing is enabled, then KSEC4(12:15)
C     values determin if extensions are concerned. Extensions are:
C
C     - general extended 2nd-order packing, which is the same as WMO
C      except for documentation of descriptors (held more economically);
C
C     - boustrophedonic ordering, which is held outside of the routine
C       (field re-ordering, swapping values in even rank rows).
C
C     - spatial differencing, which is held mostly outside of the
C       routine (apply general extended 2nd-order compression algorithm,
C       with or without boustrophedonic ordering, to values obtained
C       through computation of differences between adjacent values,
C       repeated to order wished).
C
C**   Interface.
C     ----------
C
C     KRET = C2GENE ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,  KNSPT,
C    X                KBITS,  OCOMGR, OLONLY, KLEN4,  KFIROV, KLEN,
C    X                KREFGR, KLENGR, KBITGR, KWORK,  KLWORK, KSTART,
C    X                KPOWER, KMXPWR, KPCWID, KXCWID, KNBIFO, KNCMIN,
C    X                KMINPK, KLWORX, ODEBUG )
C
C
C     Input Parameters for all options.
C     --------------------------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C                  KSEC4(12) indicates type of general 2nd-order packing
C                  (applies only to descriptors): 
C                   0 => standard WMO descriptors
C                   8 => extended     descriptors
C                  KSEC4(13) indicates if boustrophedonic ordering
C                  (values swapped in rows of even rank) is selected:
C                   0 => standard        ordering
C                   4 => boustrophedonic ordering
C                  KSEC4(14:15) indicates if spatial differencing is on:
C                   0:0 => no spatial differencing
C                   0:1,2:0,2:1 => spatial differencing applied at order
C                                  equal to KSEC4(14)+KSEC4(15)
C     KLENG      - Length of GRIB product array.
C     KNSPT      - Bit pointer for next value in GRIB product.
C     KBITS      - Number of bits per computer word.
C     OCOMGR     - True if COMputing of GRoup arrays is requested.
C     OLONLY     - True if Length ONLY is requested (no coding).
C     KLEN       - Exact number of grid-points to handle.
C     KLWORK     - Length of work arrays.
C     KPOWER     - Auxilary array, precomputed (powers of 2)-1.
C     KMXPWR     - Maximum number of bits per original scaled value.
C     KPCWID     - Auxilary array, precomputed widths.
C     KXCWID     - Maximum width associated to KPCWID.
C     KNCMIN     - Minimum increment to use when determining groups.
C     KMINPK     - Initial (minimum) length of groups.
C     ODEBUG     - True for some debug printout.
C
C
C     Output Parameters for all options.
C     ---------------------------------
C
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C                  (updated, except when both OCOMGR is false
C                   and OLONLY it true)
C     KLEN4      - Length of section 4.
C     KLWORX     - Maximum work space used.
C
C     Output Parameters when OLONLY is false.
C     --------------------------------------
C
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product (updated).
C     KWORK      - Used as work array by C2PACK.
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
C     Input Parameter when OCOMGR is false.
C     -------------------------------------
C
C     KSTART     - First index value of precomputed work arrays KREFGR,
C                  KLENGR and KBITGR.
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
C     Algorithm used to determine groups is derived from Harry R. Glahn
C     (U.S. Met Service), from a working paper submitted to WMO SGDR&C
C     in 1995.
C
C     If extended 2nd-order packing is enabled, then call with OCOMGR
C     to .FALSE. and OLONLY to .TRUE. is enabled, to support aggressive
C     packing without having to recompute work arrays, which would be
C     the same for general WMO and extended packings (for the same
C     option of field ordering: either both boustrophedonic
C     or both not).
C
C     Externals.
C     ----------
C
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
C     Boustrophedonic ordering, if selected, is formally transparent to
C     the current routine. Pre-processing is held at calling routine
C     level (C2ORDR), coding at C2PACK level.
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
C     Introduce spatial differencing.
C
C     J. Clochard, July 1998.
C     Set KLWORX to KLWORK when out of work space in direct algorithm.
C
C     J. Clochard, September 1998.
C     Update comments.
C     Adjust KLWORX within splitting algorithm.
C     Limit index range used in KWORK for "small" initial groups number.
C     Suppress KSEC1/KSEC2/KSEC3 dummy-arguments.
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
      INTEGER KLENP, KLENG, KNSPT, KBITS, KLEN4, KNCMIN, KLWORX, KXCWID
      INTEGER KLEN, KLWORK, KMXPWR, KNBIFO, KFIROV, KMINPK, KSTART
C
      INTEGER KDATA (KLENP), KSEC4 (*), KPCWID (0:*)
      INTEGER KGRIB (KLENG), KREFGR (KLWORK), KLENGR (KLWORK)
      INTEGER KBITGR (KLWORK), KPOWER (0:KMXPWR), KWORK (KLWORK)
C
      LOGICAL OLONLY, OCOMGR, ODEBUG
C
C     Local variables.
C
      INTEGER IRETFN, ICOUNT, INBIFO, ILDSTA, IDMAX, IDMIN, IXLDST
      INTEGER IGROUP, ISIZEG, IPOWER, INBITO, ILENSO, IXLEN4, IXGRBD
      INTEGER IOFF, J, IFIROV, INCMIN, IBITGW, IMAXAB, IMINAB, IMINPK
      INTEGER IMAXA, IMINA, ISOMAX, INBITA, IMAXC, IMINC, IXBIGW, ILCWID
      INTEGER IMAXAC, IMINAC, IMAXB, IMINB, ISIZEA, ISIZEB, ISIZEC
      INTEGER IXFIRO, ISTART, ILWORK, ILEN
      INTEGER IBITGS, ILENGS, ILDSTG, INDNEW, ILEN4O, IORDER, IWIDSD
      INTEGER IBIAS
      INTEGER IAUXIL, IGRADD, ILEN4S, IGROUS, J2, ILASTG, INGRAD, IPOSGR
C
      INTEGER C2PACK, C2BITW
      EXTERNAL C2PACK, C2BITW
C
      LOGICAL LB2A
      LOGICAL LGENXT
C
      REAL ZGAIN
C
C     Function computing lenth of section 4 (in octets), using as
C     arguments length of static descriptors, number of 1st-order
C     values, width of widthes, and length (bits) of descriptors
C     for groups bounds.
C
C     Length must always be an even number of octets.
C
      IXLEN4 ( IXLDST, IXFIRO, IXBIGW, IXGRBD ) =       2 * ( (
     X   IXLDST + (8+IXFIRO*IXBIGW-1)/8 + (8+IXGRBD-1)/8
     X          + (8+IXFIRO*INBIFO-1)/8 + (8+ILENSO-1)/8
     X                                                        +1 ) / 2 )
C
C
C     -----------------------------------------------------------------
C*    Section 1 . Performs initial checks.
C     -----------------------------------------------------------------
C
  100 CONTINUE
      ILEN4O=0
C
      LGENXT=KSEC4(12).NE.0
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) OCOMGR, OLONLY, KNSPT
C
        IF (LGENXT) THEN 
          WRITE(GRPRSM,FMT=9101) 'general extended'
        ELSE
          WRITE(GRPRSM,FMT=9101) 'general WMO standard'
        ENDIF
C
#ifndef ebug2o
      ENDIF
#endif
C
      IRETFN = 0
      ILCWID = KPOWER(KXCWID)+1
      IORDER=KSEC4(14)+KSEC4(15)
      ICOUNT=IORDER
      ILEN=KLEN-IORDER
      IWIDSD=KSEC4(11)
      IBIAS=KSEC4(16)
      ILDSTA=21
      INBITO=KSEC4(2)
      KLWORX=0
C
      IF (.NOT.OCOMGR) THEN
C
        INBIFO=KNBIFO
        IFIROV=KFIROV
        KLWORX=KFIROV
        ISTART=KSTART
        IBITGW=KSEC4(20)
C
        IF (LGENXT) THEN
          IBITGS=KSEC4(17)
        ENDIF
C
        IF (OLONLY) THEN
          ILENSO=KSEC4(19)
          GOTO 260
        ENDIF
        GOTO 300
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Computing of group characteristics.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      IGROUP=0
      INCMIN=MIN (MAX (KNCMIN,1),1+ILEN/4)
      IMINPK=MIN (MAX (KMINPK,2*INCMIN),1+ILEN/2)
      ISIZEG=IMINPK
      ISTART=1
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9200) INBITO, (KSEC4(J),J=1,15),
     X                          INCMIN, ISIZEG
#ifndef ebug2o
      ENDIF
C
#endif
C
C     -----------------------------------------------------------------
C               Determination of an "initial" group, called A.
C     -----------------------------------------------------------------
C
  210 CONTINUE
C
      ISIZEA=MIN (ISIZEG,KLEN-ICOUNT)
C
C     Extrema computed in-line for better efficiency.
C
      IMAXA=KDATA(ICOUNT+1)
      IMINA=KDATA(ICOUNT+1)
C
      DO 211 J=2,ISIZEA
      IMAXA=MAX (IMAXA,KDATA(ICOUNT+J))
      IMINA=MIN (IMINA,KDATA(ICOUNT+J))
  211 CONTINUE
C
  212 CONTINUE
C
      LB2A=.FALSE.
      ISOMAX=IMAXA-IMINA
C
      IF (ISOMAX.LT.ILCWID) THEN
        INBITA=KPCWID(ISOMAX)
      ELSE
        INBITA=KXCWID+KPCWID(ISOMAX/ILCWID)
      ENDIF
C
      IPOWER=KPOWER(INBITA)
C
      IOFF=ICOUNT+ISIZEA
#ifdef ebug2o
      PRINT *,'c2gene - ICOUNT/ISIZEA/IMINA/IMAXA/INBITA/IPOWER=',
     X                  ICOUNT,ISIZEA,IMINA,IMAXA,INBITA,IPOWER
#endif
C
      IF (IOFF.EQ.KLEN) THEN
C
C       Just finished. Group A is closed.
C
        GOTO 240
C
      ENDIF
C
C     -----------------------------------------------------------------
C               Try to add an increment C to initial group A.
C     -----------------------------------------------------------------
C
  220 CONTINUE
C
      IF (KLEN-(IOFF+INCMIN).LE.ISIZEG/2) THEN
C
C          Avoid too small group at the end.
C
        INCMIN=KLEN-IOFF
      ENDIF
C
      IMAXC=KDATA(IOFF+1)
      IMINC=KDATA(IOFF+1)
C
#ifdef CRAY
CDIR$ NOVECTOR
#endif
#ifdef FUJITSU
!OCL SCALAR
#endif
      DO 221 J=2,INCMIN
      IMAXC=MAX (IMAXC,KDATA(IOFF+J))
      IMINC=MIN (IMINC,KDATA(IOFF+J))
  221 CONTINUE
#ifdef CRAY
CDIR$ VECTOR
#endif
#ifdef FUJITSU
!OCL VECTOR
#endif
C
      IMAXAC=MAX (IMAXA,IMAXC)
      IMINAC=MIN (IMINA,IMINC)
      ISOMAX=IMAXAC-IMINAC
#ifdef ebug2o
      PRINT *,'c2gene - IOFF/INCMIN/IMINC/IMAXC/IMINAC/IMAXAC=',
     X                  IOFF,INCMIN,IMINC,IMAXC,IMINAC,IMAXAC
#endif
C
      IF (ISOMAX.GT.IPOWER) THEN
C
C       Added to A, increment C would not fit in same width as A.
C       Group A is closed.
C
        GOTO 240
C
      ELSEIF ((IOFF+INCMIN).EQ.KLEN) THEN
C
C       Group C is added to group A. Finished.
C
        ISIZEA=ISIZEA+INCMIN
        IMAXA=IMAXAC
        IMINA=IMINAC
        GOTO 240
C
      ENDIF
C
C       Increment C would fit, if added to A.
C
C       See width of group B, following group A, with its length
C       determined in the same way as A.
C
      ISIZEB=MIN (ISIZEG,KLEN-IOFF)
      IMAXB=IMAXC
      IMINB=IMINC
C
      DO 222 J=INCMIN+1,ISIZEB
      IMAXB=MAX (IMAXB,KDATA(IOFF+J))
      IMINB=MIN (IMINB,KDATA(IOFF+J))
  222 CONTINUE
C
      ISOMAX=IMAXB-IMINB
      IMAXAB=MAX (IMAXA,IMAXB)
      IMINAB=MIN (IMINA,IMINB)
#ifdef ebug2o
      PRINT *,'c2gene - IMINB/IMAXB/IMINAB/IMAXAB=',
     X                  IMINB,IMAXB,IMINAB,IMAXAB
#endif
C
      IF (ISOMAX.LE.IPOWER/2.AND.IPOWER.GT.0) THEN
C
C       Group B has a smaller width than A.
C
C       Group A is closed, B replaces A, reminds that new initial group
C       characteristics (with the exception of width) have already been
C       computed.
C
        LB2A=.TRUE.
        GOTO 240
C
      ELSEIF ((IMAXAB-IMINAB).LE.IPOWER) THEN
C
C       Group B just fits with A, and is added to A as an extended
C       C group.
C
        ISIZEC=ISIZEB
        IMAXAC=IMAXAB
        IMINAC=IMINAB
C
      ELSE
C
C       Group B has a width at least equal to A, and would not fit
C       if added to A.
C
C       Group C is added to group A, and another C-type group
C       has to be checked.
C
        ISIZEC=INCMIN
C
      ENDIF
C
C       Incremental group (C or B) added to A.
C
      ISIZEA=ISIZEA+ISIZEC
      IMAXA=IMAXAC
      IMINA=IMINAC
      IOFF=IOFF+ISIZEC
C
      IF (IOFF.LT.KLEN) THEN
C
C       Another C-type group has to be checked...
C
        GOTO 220
C
      ENDIF
C
C       ...otherwise, finished.
C
C     -----------------------------------------------------------------
C     No incremental group may be added. Current A group is closed.
C     -----------------------------------------------------------------
C
  240 CONTINUE
C
      IGROUP=IGROUP+1
      KLENGR(IGROUP)=ISIZEA
      ICOUNT=ICOUNT+ISIZEA
      KBITGR(IGROUP)=INBITA
      ISOMAX=IMAXA-IMINA
C
C     Maximum use of width for second-order values: the maximum value
C     of group will be encoded with the biggest value fitting within
C     INBITA bits, and keeping reference value positive or zero.
C
C     The goal is to get the lowest reference value, thus trying
C     (desperately) to save bit(s) to encode the first-order values.
C
      KREFGR(IGROUP)=MAX (0,IMINA-(IPOWER-ISOMAX))
#ifdef ebug2o
      PRINT *,'c2gene - IGROUP/ISIZEA/INBITA/IMINA/IMAXA/refer=',
     X                  IGROUP,ISIZEA,INBITA,IMINA,IMAXA,KREFGR(IGROUP)
#endif
C
      IF (ICOUNT.EQ.KLEN) THEN
C
C       Work finished.
C
        GOTO 250
C
      ELSEIF (IGROUP.GE.KLWORK) THEN
C
C     Out of work space.
C
        IRETFN = 21240
        WRITE(GRPRSM,FMT=9240) KLWORK
        KLWORX=KLWORK
        GOTO 900
C
      ELSEIF (LB2A) THEN
C
C       Transforms B group into A group.
C
        ISIZEA=ISIZEB
        IMAXA=IMAXB
        IMINA=IMINB
C
C       Go back, starting by width computing.
C
        GOTO 212
C
      ELSE
C
C       Another A group must be determined.
C
        GOTO 210
C
      ENDIF
C
C     -----------------------------------------------------------------
C     Groups have been determined. Computes associated descriptors.
C     -----------------------------------------------------------------
C
  250 CONTINUE
C
      KLWORX=IGROUP
      IFIROV=IGROUP
C
C     Number of bits necessary to code first-order values.
C
      CALL MAXMNI (KREFGR,IFIROV,IDMAX,IDMIN)
      INBIFO = C2BITW ( IDMAX,  INBITO, KPOWER, KMXPWR )
C
C     Length (bits) of second-order values.
C
      ILENSO=0
C
      DO 253 J=1,IFIROV
      ILENSO=ILENSO+KLENGR(J)*KBITGR(J)
  253 CONTINUE
C
C     -----------------------------------------------------------------
C     Descriptors that are computed even if OCOMGR is false.
C     -----------------------------------------------------------------
C
  260 CONTINUE      
C
      IF (LGENXT) THEN
C
C       Compute group width (width of widths), that may be here
C       different than 8.
C 
        CALL MAXMNI (KBITGR,IFIROV,ISOMAX,IAUXIL)
        IBITGW = C2BITW ( ISOMAX, INBITO, KPOWER, KMXPWR )
C
C       Compute width of lengths.
C 
        CALL MAXMNI (KLENGR,IFIROV,IDMAX,IDMIN)
        IBITGS = C2BITW ( IDMAX,  KMXPWR, KPOWER, KMXPWR )
C
        KSEC4(17)=IBITGS
C
        IF (IORDER.EQ.0) THEN
          ILDSTG=ILDSTA+4
        ELSE
          ILDSTG=ILDSTA+5+(8+IWIDSD*(IORDER+1)-1)/8
        ENDIF
C
#ifndef ebug2o
        IF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9260) IAUXIL, ISOMAX, IBITGW, IDMIN, IDMAX,
     X                            IBITGS
#ifndef ebug2o
        ENDIF
C
#endif
      ELSE
C
C      Define group width (width of widths).
C
        IBITGW=8
C
      ENDIF
C
      KSEC4(20)=IBITGW
C
      IF (OCOMGR) THEN
C
        KSEC4(19)=ILENSO
        KNBIFO=INBIFO
        KFIROV=IFIROV
C
      ENDIF
C
C     -----------------------------------------------------------------
C     Compute full length of section 4 of GRIB.
C     -----------------------------------------------------------------
C 
      IF (LGENXT) THEN
C
        ILENGS=IFIROV*IBITGS
        KLEN4=IXLEN4 ( ILDSTG, IFIROV, IBITGW, ILENGS )
C
#ifndef ebug2o
        IF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9270) KLEN4
          ILEN4O=KLEN4
#ifndef ebug2o
        ENDIF
C
#endif
C     -----------------------------------------------------------------
C        Try to reduce size of lengthes descriptors, splitting groups
C        that require exactly IBITGS bits for their lengthes.
C     -----------------------------------------------------------------
C
        IAUXIL=MIN (KLWORK/2,IFIROV)
C
  270   CONTINUE
C
        IGROUP=0
        IGRADD=0
        IDMAX=KPOWER(IBITGS-1)+1
C
        DO 271 J=1,IFIROV
C
        IF (KLENGR(J).GE.IDMAX) THEN
C
          IF (IGROUP.GE.IAUXIL) THEN
C
C           Out of work space. No (further) splitting performed.
C
            KLWORX=MAX (KLWORX,IAUXIL+IGROUP)
            GOTO 280
C
          ELSE
C
C           Compute number of sub-groups that would replace current
C           group, with a length requiring one bit less for encoding.
C           (2, except when original length=2**IBITGS-1, where 3
C            sub-groups are necessary)
C
C
            IGROUP=IGROUP+1
            KWORK(IGROUP)=J
            KWORK(IAUXIL+IGROUP)=1+(KLENGR(J)+1)/IDMAX
            IGRADD=IGRADD+KWORK(IAUXIL+IGROUP)
C  
          ENDIF
C
        ENDIF
C
  271   CONTINUE
C
        IGRADD=IGRADD-IGROUP
        IGROUS=IFIROV+IGRADD
        ILENGS=IGROUS*(IBITGS-1)
        ILEN4S=IXLEN4 ( ILDSTG, IGROUS, IBITGW, ILENGS )
        KLWORX=MAX (KLWORX,IAUXIL+IGROUP)
C
#ifndef ebug2o
        IF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9271) IFIROV, IGRADD, ILEN4S
#ifndef ebug2o
        ENDIF
C
#endif
        IF (IGROUS.LE.KLWORK.AND.ILEN4S.LT.KLEN4) THEN
C
C         Splitting performed, leading to better compression ratio
C         with some shorter groups.
C
          ILASTG=IFIROV
C
          DO 275 J=IGROUP,1,-1
C
          IPOSGR=KWORK(J)
          INGRAD=KWORK(IAUXIL+J)
C
C         Push up groups above current group to split.
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
          DO 273 J2=ILASTG,IPOSGR+1,-1
C
          KREFGR(J2+IGRADD)=KREFGR(J2)
          KBITGR(J2+IGRADD)=KBITGR(J2)
          KLENGR(J2+IGRADD)=KLENGR(J2)
C
  273     CONTINUE
C
#ifdef ebug2o
          PRINT *,'c2gene - after loop 273: ',IPOSGR+1+IGRADD
#endif
C
C         Split current group.
C
#ifdef CRAY
CDIR$ NOVECTOR
#endif
#ifdef FUJITSU
!OCL SCALAR
#endif
          DO 274 J2=INGRAD,1,-1
C
          INDNEW=J2-INGRAD+IGRADD+IPOSGR
#ifdef ebug2o
          PRINT *,INDNEW
#endif
          KREFGR(INDNEW)=KREFGR(IPOSGR)
          KBITGR(INDNEW)=KBITGR(IPOSGR)
          KLENGR(INDNEW)=(KLENGR(IPOSGR)+INGRAD-J2)/INGRAD
C
  274     CONTINUE
#ifdef CRAY
CDIR$ VECTOR
#endif
#ifdef FUJITSU
!OCL VECTOR
#endif
C
#ifdef ebug2o
          PRINT *,'c2gene - J/IPOSGR/INGRAD/ILASTG/IGRADD/len=',
     X             J, IPOSGR, INGRAD, ILASTG, IGRADD,
     X        (KLENGR(J2-INGRAD+IGRADD+IPOSGR),J2=1,INGRAD)
#endif
C
          IGRADD=IGRADD-(INGRAD-1)
          ILASTG=IPOSGR-1
C
  275     CONTINUE
C
#ifndef ebug2o
          IF (ODEBUG) THEN
#endif
            WRITE(GRPRSM,FMT=9272) IGROUS, IBITGS-1
#ifndef ebug2o
          ENDIF
C
#endif
C
C         Update descriptors.
C
          IBITGS=IBITGS-1
          IFIROV=IGROUS
          KLWORX=MAX (KLWORX,IGROUS)
          KSEC4(17)=IBITGS
          KFIROV=IFIROV
          KLEN4=ILEN4S
C
C         Loop back to try another splitting.
C
          GOTO 270
C
        ELSEIF (KLEN4.NE.ILEN4O) THEN
C
#ifndef ebug2o
          IF (ODEBUG) THEN
#endif
            ZGAIN=REAL (100*(ILEN4O-KLEN4)) / REAL (ILEN4O)
            WRITE(GRPRSM,FMT=9273) ZGAIN
#ifndef ebug2o
          ENDIF
C
#endif
        ENDIF
C
      ELSE
C
        KLEN4=IXLEN4 ( ILDSTA, IFIROV, IBITGW, KLEN )
C
      ENDIF
C
  280 CONTINUE
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
C*    Section 3. Definition of fixed-length descriptors, and coding.
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
      IF (LGENXT) THEN
        KSEC4( 9)=0
        KSEC4(12)=8
      ELSE
        KSEC4( 9)=32
        KSEC4(12)=0
      ENDIF      
C
      KSEC4(10)=16
C
C     Extra descriptors.
C
      KSEC4(18)=KFIROV
      KSEC4(20)=IBITGW
      ILWORK=KLWORK-ISTART+1
C
      IRETFN = C2PACK ( KDATA,  KLENP,  KSEC4,  KGRIB,  KLENG,  KNSPT,
     X                  KBITS,  KLEN,   KREFGR(ISTART), KLENGR(ISTART),
     X                  KBITGR(ISTART), KWORK, ILWORK,  KPOWER, KMXPWR,
     X                  KNBIFO, ODEBUG )
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      C2GENE = IRETFN
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
 9100 FORMAT (' C2GENE: Function start, OCOMGR = ',L1,
     X        ', OLONLY = ',L1,', KNSPT =',I10,'.')
 9101 FORMAT (' C2GENE: Type of 2nd order packing requested: ',A,'.')
C
 9200 FORMAT (' C2GENE: Computing work arrays, INBITO =',I3,'.',/,
     X        ' C2GENE: KSEC4(1:15)=',I9,I3,I4,3I3,I2,4I3,4I2,'.',/,
     X        ' C2GENE: INCMIN =',I3,', ISIZEG =',I6,'.')
C
 9240 FORMAT (' C2GENE: Groups number exceeds work space (',I8,') .')
C
 9260 FORMAT (' C2GENE: Min, Max, Width of widths/lengths =',2(I3,','),
     X        I2,' /',2(I8,','),I3,'.')
C
 9270 FORMAT (' C2GENE: Length of section 4 for general extended case:'
     X        ,I9,'.')
 9271 FORMAT (' C2GENE: Group splitting test, from',I8,' groups',SP,
     X        I7,SS,', s4 length',I9,'.')
 9272 FORMAT (' C2GENE: Group splitting performed,',I8,
     X        ' groups, new width of lengths =',I3,'.')
 9273 FORMAT (' C2GENE: Relative gain of group splitting on section 4 ',
     X        'length:',F5.1,' %.')
C
 9300 FORMAT (' C2GENE: Coding phase - calling C2PACK.')
C
 9900 FORMAT (' C2GENE: Function return code=',I6,
     X        ', OCOMGR/OLONLY/KNSPT=',2L1,I10,'.')
C
      END
