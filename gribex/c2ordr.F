C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

       INTEGER FUNCTION C2ORDR ( KDATA, KLENP, KSEC1, KSEC2, KSEC3, 
     X                          KSEC4, KGRIB, KLENG, KNSPT, KBITS,
     X                          HOPER, KBMAP, KVALS, PREF,  PMAX,
     X                          KLEN,  KDEBUG )
C
C---->
C**** C2ORDR
C
C     Purpose.
C     --------
C
C     Encode GRIB section 4 for complex packing of grid-point
C     values ("second-order packing").
C
C
C**   Interface.
C     ----------
C
C     KRET = C2ORDR ( KDATA, KLENP, KSEC1, KSEC2, KSEC3, KSEC4,
C    X                KGRIB, KLENG, KNSPT, KBITS, HOPER, KBMAP,
C    X                KVALS, PREF,  PMAX , KLEN,  KDEBUG )
C
C
C     Input Parameters.
C     -----------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KSEC1      - Array of GRIB section 1 integer descriptors.
C     KSEC2      - Array of GRIB section 2 integer descriptors.
C     KSEC3      - Array of GRIB section 3 integer descriptors.
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C     KLENG      - Length of GRIB product array.
C     KNSPT      - Bit pointer for next value in GRIB product.
C     KBITS      - Number of bits per computer word.
C     HOPER      - Requested function:
C                  'C' for coding, following input descriptors,
C                  'K' for "aggressive pacKing" (choose a solution
C                  that leads to minimum size).
C     KBMAP      - Bit pointer for start of explicit primary bit-map
C                  (if any).
C     KVALS      - Number of bits in primary bit-map.
C     PREF       - Reference of field.
C     PMAX       - Maximum value of field.
C     KLEN       - Exact number of points to handle.
C     KDEBUG     - >0 for some debug printout.
C
C
C     Output Parameters.
C     -----------------
C
C     KDATA      - Used (indirectly) as work array.
C     KSEC4      - Array of GRIB section 4 integer descriptors
C                  (updated).
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product
C                  (updated).
C
C
C     Method.
C     -------
C
C     Follows WMO Manual of Codes, and also CSECT4 structure.
C
C
C     If ordinary packing is requested (HOPER='C'), then the relevant
C     method described through KSEC4 descriptors is applied:
C
C     1) Row by row: KSEC4(9)=0, KSEC4(10)=16 and KSEC4(12)=0;
C
C     2) Constant width: KSEC4(9)=32, KSEC4(10)=0, KSEC4(12)=0,
C        width in KSEC4(11) -see "Comments" paragraph below, at the end.
C
C     3) General WMO 2nd-order: KSEC4(9)=32, KSEC4(10)=16, KSEC4(12)=0.
C
C     4) General extended  "  : KSEC4(9)=32, KSEC4(10)=16, KSEC4(12)=8.
C
C     For constant width and general methods, boustrophedonic ordering
C     is requested by KSEC4(13)=4, provided extensions have been
C     alllowed (disabled by default), and is explicitly inhibited
C     by KSEC4(13)=0.
C     Boustrophedonic ordering has no interest for row by row packing.
C     If explicitly requested a warning message is issued and option
C     set to off.
C
C     For the general extended method, a spatial differencing may be
C     requested through KSEC4(14) and/or KSEC4(15) different from zero:
C
C     -explicitly: KSEC4(14)=0 or 2, KSEC4(15)=0 or 1, order being then
C                  IORDER=KSEC4(14)+KSEC4(15) ;
C
C     -implicitly: KSEC4(14)=-1 and/or KSEC4(15)=-1; in such a case
C                  a diagnostic is performed to estimate which order
C                  should lead to minimaum size.
C
C
C     If aggressive packing (HOPER='K') is selected:
C
C     1) in a first scan, all allowed sub-functions (see Comments
C        below) are called to compute the length of section 4 they
C        would give, but no coding occurs;
C
C     2) in a second phase the most efficient method is applied.
C
C        If no "complex" method is efficient -or applicable-, then a
C        simple packing is performed.
C
C        If the field is constant, a simple packing  with 0 as number
C        of bits (and no data) is applied, without having to perform
C        the steps mentioned above.
C
C     Externals.
C     ----------
C
C     INXBIT    - Insert bits into GRIB product.
C     CONFP3    - Convert to IBM floating point format.
C
C     C2ROWS    - Row by row packing.
C     C2CWID    - Constant width packing.
C     C2GENE    - General WMO second-order packing.
C                + general extended second-order packing.
C     REVERO    - Reverse order of even rank rows.
C     C2DOSD    - Diagnose/compute order of spatial differencing.
C     D2ROSD    - Rebuild original values from spatial differencing.
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
C     When aggressive packing is selected (HOPER='K') :
C
C     1) If exhaustive second-order packing is on, then all possible
C        methods are attempted; note this may lead to expensive CPU
C        cost. Constant width packing will use iterative search of
C        optimal width, starting from (KSEC4(2)-1), regardless of
C        KSEC4(11) value.
C
C        Notes:
C
C        -if extensions switch is not set (disabled by default), then
C         none of the extensions on methods will be promoted;
C
C        -if extensions switch is on, boustrophedonic ordering is then
C         used in conjunction with relevant methods (all 2nd-order but
C         row by row), provided there is a section 2 and no implicit
C         primary bit-map;
C
C        -if both extensions and non-local switches are on (both are
C         disabled by default), estimation of best order of spatial
C         differencing is activated for general extended packing.
C
C     2) If exhaustive second-order packing is off (default), then
C        only explicitly requested methods are tried. More precisely:
C
C        -row by row packing (cheapest method, but least effective)
C         is always attempted, except without a section 2
C         or with an implicit primary bit-map;
C
C        -constant width packing is attempted if KSEC4(10)=0 in input;
C         (with iterative search of optimal width only if KSEC4(11) is
C          negative, using -KSEC4(11) as initial and maximum width)
C
C        -general 2nd-order packing is attempted if KSEC4(9)=32 in
C         input, or if there is no section 2 or in case of an implicit
C         primary bit-map.
C
C        -general extended second-order packing is attempted if
C         extensions switch has been set and KSEC4(12)=8 in input.
C
C        Boustrophedonic ordering is attempted if extensions switch
C        has been set and if KSEC4(13)=4 in input,
C        systematically in conjunction with relevant selected
C        method(s), and provided there is a section 2.
C
C        Spatial differencing is activated for general extended method
C        if this last method is selected (see above), if non-local
C        switch has been set, and if KSEC4(14) and/or KSEC4(15) is not
C        null. Tuning of order is as described earlier for HOPER='C'.
C
C     Iterative method used in C2CWID to find an optimal width (that
C     minimise length of section 4) may also be used with HOPER='C',
C     using a negative value for KSEC4(11) in input; -KSEC4(11) is then
C     used as initial and maximum width.
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
C     For constant width packing, interpret KSEC4(11) negative as
C     request for optimal width search, starting from -KSEC4(11) .
C     When both aggressive mode (HOPER='K') is requested and exhaustive
C     use of 2nd-order packing methods is set, initial width is set to
C     (KSEC4(2)-1) .
C     Fix double computing of work arrays for General WMO 2nd-order,
C     when General extended method is not requested.
C     Introduce spatial differencing.
C     Simplify management of boustrophedonic ordering.
C
C     J. Clochard, August 1998.
C     Raise JPMXGR from 131071 to 220000 to enable proper processing
C     of satellite images for general method.
C     (i.e. METEOSAT full-disk products, 2500x2500)
C
C     J. Clochard, September 1998.
C     Suppress KSEC1/KSEC2/KSEC3 arguments in C2CWID/C2GENE calls.
C     Suppress KSEC1/KSEC3 arguments in C2ROWS call.
C     Fix definition of LGENXT to be able to select both general methods
C     in non-exhaustive use of 2nd-order methods.
C     Fix to enforce re-computation of work arrays when general extended
C     method is not worth and spatial differencing is used.
C     Inhibit boustrophedonic ordering also for aggressive packing,
C     when there is no section 2 and/or an implicit bit-map.
C     If dynamical estimation of spatial differencing leads to greater
C     message length than general WMO 2nd-order, retry general exten-
C     ded method without spatial differencing.
C     Adjustment of granularity parameters for general methods and small
C     grids.
C
C     J. Clochard, June 1999.
C     Simple packing method always available as backup, even in explicit
C     encoding cases (HOPER='C') . 
C     Use precomputed array to determine widths . Arguments IPCWID and
C     JPXWID added in C2GENE/C2DOSD/C2ROWS calls .
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
      INTEGER JP16SET, JPMXGR, JPXPWR, JPMXGF, JPMETH, JPVSE4, JPWORK
      INTEGER JPORDR, JPXWID, JPCWID
C
C     Number of possible packing methods.
C
      PARAMETER ( JPMETH=4, JPORDR=3 )
      PARAMETER ( JP16SET= 2**16-1 )
C                            `---> 65535    =   FFFF(hex)
#ifdef JBPW_64
      PARAMETER ( JPMXGR= 220000, JPXPWR=47, JPVSE4=20 )
#else
      PARAMETER ( JPMXGR= 220000, JPXPWR=31, JPVSE4=20 )
#endif
      PARAMETER ( JPXWID=(JPXPWR+1)/2 )
      PARAMETER ( JPCWID=2**JPXWID )
C
C     Size of work array. Must be at least JPMXGF*4 .
C
      PARAMETER ( JPWORK=JPMXGR*4 )
#ifndef wmogrp2o
      PARAMETER ( JPMXGF= JPMXGR )
#else
C
C     Limit maximum number of groups/first-order values to WMO
C     standard upper limit (descriptor is 16-bit wide).
C
      PARAMETER ( JPMXGF=
     X          ( JPMXGR*(JP16SET/JPMXGR) + JP16SET*(JPMXGR/JP16SET) )
     X           /     ( (JP16SET/JPMXGR) + (JPMXGR/JP16SET) )       )
#endif
C
C     Subroutine arguments.
C
      INTEGER KLENP, KLENG, KNSPT, KBITS, KBMAP, KVALS, KLEN, KDEBUG
      INTEGER KDATA (KLENP), KSEC1 (*), KSEC2 (*), KSEC3 (*), KSEC4 (*)
      INTEGER KGRIB (KLENG)
C
      CHARACTER HOPER * (*)
C
      REAL PREF, PMAX
C
C     Global variables.
C
#include "grbcom.h"
C
C     Local variables.
C
      INTEGER IRET, INROWS, J, IROWMK, INSPTA, ILOWMK, IHIGMK, IRETFN
      INTEGER ILSIMP, IL4OPT, ILMETH, IZERO, ICWORK, IROWBF, ICWIMK, J2
      INTEGER ICWIBF, ICWIFO, IHWM, INCMIN, IOFF, IROWL4, ICWIL4
      INTEGER IGENMK, IGENBF, IGENFO, IGENL4, IMINPK, IMETHO, JMETHO
      INTEGER IMTROW, IMTCWI, IMTGEN, IMTMIN, IMTMAX, ILREFR
C
      INTEGER IWORK (JPWORK), IPOWER (0:JPXPWR), ISEC4 (JPVSE4,JPMETH)
      INTEGER IPCWID (0:JPCWID-1)
C
      INTEGER C2ROWS, C2CWID, C2GENE
      EXTERNAL C2ROWS, C2CWID, C2GENE
C
      CHARACTER YOPER*1, YMETHO*30, YREFER*30
C
      LOGICAL LROW, LQUASI, LCTWID, LSECBI, LPRIBI, LAGGPK, LSIMPK
      LOGICAL L1CALL, LSECT2, LLONLY, LCOMGR, LFIRST, LGEN2O, LDEBUG
      LOGICAL LOPTLN, LGPREC
      LOGICAL LGENXT, LBOUST, LREVER, LPRCLN, LNLOCP, LNLOCF, LDYNOR
      LOGICAL LCOMSD, LCBIAS, LVECTD
C
      INTEGER IWIDSD, INDCLN, IGXTMK, IGXTBF, IGXTFO, IGXTL4, IBOUST
      INTEGER IMTGXT, IORDER, ILWORX, IBIAS, IXBITS
C
      INTEGER REVERO, C2DOSD, D2ROSD
      EXTERNAL REVERO, C2DOSD, D2ROSD
C
      INTRINSIC ABS
C
      REAL ZGAIN
C
      SAVE L1CALL, IPOWER, IMTROW, IMTCWI, IMTGEN, IPCWID
      SAVE IMTGXT
C
C     Function used to adress 4 sub-parts of array IWORK, using
C     first and last argument to specify sub-part.
C     (4 work arrays are joined together to enable use of a single big
C      work array)
C
      INTEGER IXIND, IABCDE, IFGHIJ, IKLMNO
C
      IXIND ( IABCDE, IFGHIJ, IKLMNO ) =
C
     X        IFGHIJ + JPMXGF * ( IKLMNO - 1 + 2 * ( IABCDE - 1 ) )
C
      DATA L1CALL / .TRUE. /
      DATA IMTROW / 1 /
      DATA IMTCWI / 2 /
      DATA IMTGEN / 3 /
      DATA IMTGXT / 4 /
C
C     -----------------------------------------------------------------
C*    Section 1 . Performs initial checks.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      IF (L1CALL) THEN
C
        DO 111 J=0,JPXPWR-1
C
          IPOWER(J)=2**J-1
C
  111   CONTINUE
#ifdef JBPW_64
        IPOWER(JPXPWR)=140737488355327
#else
        IPOWER(JPXPWR)=2147483647
#endif
C
        IPCWID(0)=0
        IOFF=1
C
        DO 113 J=1,JPXWID
C
          DO 112 J2=0,IPOWER(J-1)
C
            IPCWID(IOFF+J2)=J
C
  112     CONTINUE
C
          IOFF=IOFF+IPOWER(J-1)+1
C
  113   CONTINUE
C
        L1CALL=.FALSE.
C
      ENDIF
C
      IRETFN = 0
      IMETHO=0
      INSPTA=KNSPT
      LAGGPK=HOPER.EQ.'K'
      ILSIMP=11+(1+(KLEN*KSEC4(2)-1)/8)
      ILSIMP=2*(1+(ILSIMP-1)/2)
      IL4OPT=ILSIMP
      LSIMPK=LAGGPK
      LDEBUG=KDEBUG.GE.1
      IMINPK=MIN ( 15, MAX ( 2, NINT ( SQRT ( REAL (KLEN/3) ) ) ) )
C     IMINPK=12
      INCMIN=MIN (  3, 1 + KLEN/1000 )
C     INCMIN=1
C
      LSECT2=KSEC1(5).GE.128
      LPRIBI=MOD (KSEC1(5),128).GE.64
#ifndef ebug2o
C
      IF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9100) HOPER(:1), KNSPT, KSEC4(2), LSECT2,
     X                          LPRIBI
        YREFER='simple packing'
#ifndef ebug2o
      ENDIF
C
#endif
      LSECBI=KSEC4(9).NE.0
      LROW=.NOT.LSECBI.AND.KSEC4(12).EQ.0
      LGENXT=(.NOT.LSECBI.OR.LAGGPK).AND.KSEC4(12).NE.0
      LBOUST=KSEC4(13).NE.0
      IORDER=IABS (KSEC4(14))+IABS (KSEC4(15))
      LNLOCP=IORDER.NE.0
      LREVER=.FALSE.
      LGPREC=.FALSE.
#if (defined CRAY) || (defined FUJITSU)
      LVECTD=.TRUE.
#else
      LVECTD=.FALSE.
#endif
      LQUASI=KSEC2(17).EQ.1
      LCTWID=KSEC4(10).EQ.0
      LGEN2O=LSECBI.AND.(.NOT.LCTWID.OR.LAGGPK)
C
      IF (KSEC4(8).NE.0) THEN
        IRETFN = 18110
        WRITE(GRPRSM,FMT=9110)
        GO TO 900
      ELSEIF (LROW.OR.LAGGPK) THEN
C
        IF (.NOT.LSECT2) THEN
C
          IF (LAGGPK) THEN
            LROW=.FALSE.
            LGEN2O=.TRUE.
          ELSE
            IRETFN = 18120
            WRITE(GRPRSM,FMT=9120) 'Row by row packing'
            GO TO 900
          ENDIF
C
        ELSEIF (LPRIBI.AND.KSEC3(1).NE.0) THEN
C
          IF (LAGGPK) THEN
            LROW=.FALSE.
            LGEN2O=.TRUE.
          ELSE
            IRETFN = 18130
            WRITE(GRPRSM,FMT=9130)
     S       'Row by row', 'implicit primary bit-map'
            GO TO 900
          ENDIF
C
        ELSE
C
          LROW=LROW.OR.LAGGPK
C
        ENDIF
C
      ENDIF
C
      IF (LCTWID) THEN
C
        IF (LROW.AND..NOT.LAGGPK) THEN
          IRETFN = 18135
          WRITE(GRPRSM,FMT=9130)
     S       'Row by row', 'constant width'
          GO TO 900
        ENDIF
C
      ENDIF
C
      LCTWID=LCTWID.OR.(LAGGPK.AND.NUM2OK.EQ.1)
      LGEN2O=LGEN2O.OR.(LAGGPK.AND.NUM2OK.EQ.1)
C
      IF (KSEC4(12).NE.0.AND.NEXT2O.EQ.1.AND..NOT.LAGGPK) THEN
C
C            General extended second-order packing explicitly requested.
C
        IF (KSEC4(12).NE.8) THEN
          IRETFN = 18140
          WRITE(GRPRSM,FMT=9140) 12, 8
          GO TO 900
        ELSEIF (LSECBI) THEN
          IRETFN = 18145
          WRITE(GRPRSM,FMT=9150) 'no secondary bit-map'
          GO TO 900
        ELSEIF (LCTWID) THEN
          IRETFN = 18150
          WRITE(GRPRSM,FMT=9150) 'variable group widthes'
          GO TO 900
        ENDIF
C
      ENDIF
C
      LGENXT=NEXT2O.EQ.1.AND.(LGENXT.OR.(LAGGPK.AND.NUM2OK.EQ.1))
C
      IF (KSEC4(13).NE.0.AND.NEXT2O.EQ.1.AND..NOT.LAGGPK) THEN
C
C               Boustrophedonic ordering explicitly requested.
C
        IF (KSEC4(13).NE.4) THEN
          IRETFN = 18151
          WRITE(GRPRSM,FMT=9140) 13, 4
          GO TO 900
        ELSEIF (.NOT.LSECT2) THEN
          IRETFN = 18152
          WRITE(GRPRSM,FMT=9120) 'Boustrophedonic ordering'
          GO TO 900
        ELSEIF (LROW) THEN
          WRITE(GRPRSM,FMT=9153)
          KSEC4(13)=0
          LBOUST=.FALSE.
        ELSEIF (LPRIBI.AND.KSEC3(1).NE.0) THEN
          IRETFN = 18154
          WRITE(GRPRSM,FMT=9130)
     S       'Boustrophedonic ordering', 'implicit primary bit-map'
          GO TO 900
        ENDIF
C
      ENDIF
C
      LBOUST=NEXT2O.EQ.1.AND.(LBOUST.OR.(LAGGPK.AND.NUM2OK.EQ.1)).AND.
     S   LSECT2.AND.(.NOT.LPRIBI.OR.KSEC3(1).EQ.0)
C
#ifndef ebug2o
      IF (LDEBUG) THEN
#endif
        PRINT *,'C2ORDR: LBOUST/LNLOCP/LAGGPK=',
     S                 LBOUST, LNLOCP, LAGGPK
#ifndef ebug2o
      ENDIF
C
#endif
      IF (LNLOCP.AND.NEXT2O.EQ.1.AND.NLOC2O.EQ.1.AND..NOT.LAGGPK) THEN
C
C               Spatial differencing explicitly requested.
C
        IF (KSEC4(14).NE.2.AND.KSEC4(14).NE.0.AND.KSEC4(14).NE.-1) THEN
          IRETFN = 18154
          WRITE(GRPRSM,FMT=9155) 14, 2
          GO TO 900
        ELSEIF (ABS (KSEC4(15)).NE.1.AND.KSEC4(15).NE.0) THEN
          IRETFN = 18155
          WRITE(GRPRSM,FMT=9155) 15, 1
          GO TO 900
        ELSEIF (.NOT.LGENXT) THEN
          WRITE(GRPRSM,FMT=9156)
          KSEC4(14)=0
          KSEC4(15)=0
          IORDER=0
          LNLOCP=.FALSE.
        ENDIF
C
      ENDIF
C
      LNLOCF=NEXT2O.EQ.1.AND.NLOC2O.EQ.1.AND.
     S      (LNLOCP.OR.(LGENXT.AND.LAGGPK.AND.NUM2OK.EQ.1))
C
      IF (LNLOCF.AND..NOT.LNLOCP) THEN
C
C       Spatial differencing implicitly requested.
C       Activation of iterative search for best order.
C
        KSEC4(14)=-1
        KSEC4(15)=-1
      ENDIF
C
      LNLOCP=LNLOCF
C
      IF (.NOT.LNLOCP) THEN
        IORDER=0
      ENDIF
C
      IF (KSEC4(2).GT.JPXPWR) THEN
C
        IRETFN = 18160
        WRITE(GRPRSM,FMT=9160) KSEC4(2), JPXPWR
        GO TO 900
C
      ELSEIF (LCTWID) THEN
C
        IF (ABS (KSEC4(11)).GE.KSEC4(2)) THEN
C
          IF (LAGGPK) THEN
            KSEC4(11)=-(KSEC4(2)-1)
          ELSE
            IRETFN = 18170
            WRITE(GRPRSM,FMT=9170) KSEC4(11), KSEC4(2)-1
            GO TO 900
          ENDIF
C
        ENDIF
C
      ENDIF
C
      IF (PMAX.LE.PREF.AND.LAGGPK) THEN
C
C       Field is constant... code it with zero as bit-number,
C                            and nothing else!              
C
C     Enforce right values for flags.
C
        KSEC4(4)=0
        KSEC4(6)=0
C
C*    Octet 11 : Number of bits containing each first-order value.
C     One 8 bit field.
C
#ifndef ebug2o
        IF (LDEBUG) THEN
C
#endif
          WRITE(GRPRSM,FMT=9175) PMAX
          IL4OPT=12
          YREFER='direct method'
C
C              Switch off LSIMPK to enable printout of gain.
C
          LSIMPK=.FALSE.
#ifndef ebug2o
        ENDIF
C
#endif
        IZERO=0        
        YOPER='C'
        KNSPT=KNSPT+10*8
        CALL INXBIT(KGRIB,KLENG,KNSPT,IZERO,1,KBITS, 8,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 18180
          WRITE(GRPRSM,FMT=9001) 'bit number'
        ENDIF
C
        GO TO 900
C
      ENDIF
C
      IF (LSECT2) THEN
C
        IF (MOD (KSEC2(11),64).LT.32) THEN
          INROWS=KSEC2(3)
        ELSE
          INROWS=KSEC2(2)
        ENDIF
C
#ifndef ebug2o
        IF (LDEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9180) INROWS, KLEN, LQUASI
#ifndef ebug2o
        ENDIF
C
#endif
        IF (.NOT.LPRIBI.AND.KLEN.NE.KSEC4(1)) THEN
          IRETFN = 18185
          WRITE(GRPRSM,FMT=9185) KLEN, KSEC4(1)
          GO TO 900
        ELSEIF (LPRIBI.AND.(KLEN.LE.0.OR.KLEN.GT.KSEC4(1))) THEN
          IRETFN = 18187
          WRITE(GRPRSM,FMT=9187) KLEN, KSEC4(1)
          GO TO 900
        ELSEIF (LROW.AND.INROWS.GT.JPMXGF) THEN
C
          IF (LAGGPK) THEN
            LROW=.FALSE.
          ELSE
            IRETFN = 18190
            WRITE(GRPRSM,FMT=9190) INROWS, JPMXGF
            GO TO 900
          ENDIF
#ifndef wmogrp2o
C
        ELSEIF (LPRIBI.AND.KSEC3(1).NE.0.AND.KLEN.GT.JP16SET) THEN
C
C     Such a case would lead to no possibility to recover the right
C     number of grid points when decoding through D2ORDR routine.
C
          IRETFN = 18192
          WRITE(GRPRSM,FMT=9192) JP16SET
          GO TO 900
#endif
C
        ENDIF
C
      ENDIF
C
      IF (.NOT.(LROW.OR.LCTWID.OR.LGEN2O.OR.LGENXT.OR.LSIMPK)) THEN
        WRITE(GRPRSM,FMT=9195)
        LSIMPK=.TRUE.
        GO TO 600
      ENDIF
C
      LLONLY=LAGGPK
      ILOWMK=JPMXGF+1
      IHIGMK=0
      IROWMK=0
      ICWIMK=1
      IGENMK=1
      IGXTMK=1
      LFIRST=.TRUE.

C          Copy KSEC4 for all possible methods. 
C
      IF (LROW) THEN
        IMTMIN=IMTROW
        IMTMAX=IMTROW
      ELSE
        IMTMIN=JPMETH
        IMTMAX=1
      ENDIF
C
      IF (LCTWID) THEN
        IMTMIN=MIN (IMTCWI,IMTMIN)
        IMTMAX=MAX (IMTCWI,IMTMAX)
      ENDIF
C
      IF (LGEN2O) THEN
        IMTMIN=MIN (IMTGEN,IMTMIN)
        IMTMAX=MAX (IMTGEN,IMTMAX)
      ENDIF
C
      IF (LGENXT) THEN
        IMTMIN=MIN (IMTGXT,IMTMIN)
        IMTMAX=MAX (IMTGXT,IMTMAX)
      ENDIF
C
      DO 122 JMETHO=IMTMIN,IMTMAX
C
      DO 121 J=1,JPVSE4
      ISEC4(J,JMETHO)=KSEC4(J)
  121 CONTINUE
C
  122 CONTINUE
C
      IF (LBOUST.AND.IMTMIN.EQ.IMTROW.AND.IMTMAX.EQ.IMTROW) THEN
C
C         This might be the case in aggressive packing mode.
C
        WRITE(GRPRSM,FMT=9153)
        KSEC4(13)=0
        LBOUST=.FALSE.
      ENDIF
C
      IF (LBOUST) THEN
C
        IBOUST=4
C
C         Reverse field ordering for even rank rows.
C
C         This is done before trying any method or sub-method,
C         for better prognosis of non-local packing interest
C         (if requested). It allows also more simple source code.
C
C         There is no influence on trial of row by row packing,
C         because work arrays computed through C2ROWS with LLONLY set
C         to .TRUE. do not depend on field ordering within each row.
C
C         The only case where field ordering might have to be restored
C         would be in aggressive packing mode, if either row by row
C         or simple packing was found more efficient than all the other
C         2nd-order methods selected/allowed... Probability of such a
C         case is low, but source code enables it.
C
C         Apart from that, the only side effect of this anticipated call
C         to REVERO is, when row by row packing is enabled
C         (as in exhaustive mode), to compute twice the lengths of rows.
C         This is considered as a cheap overhead.
C
        LPRCLN=.FALSE.
C
        IRETFN = REVERO ( KDATA,  KLENP,  KSEC2,  KGRIB,  KLENG,
     X                    KBITS,  KBMAP,  KVALS,  LPRIBI, LQUASI,
     X                    INROWS, KLEN,   IWORK(IXIND (2,1,2)),
     X                    JPMXGF, LDEBUG, LPRCLN, IWORK(IXIND (2,1,1)) )
C
        IF (IRETFN.NE.0) THEN
          GO TO 900
        ENDIF
C
        LREVER=.TRUE.
C
      ELSE
C
        IBOUST=0
C
      ENDIF
C
      DO 123 JMETHO=IMTMIN,IMTMAX
      ISEC4(13,JMETHO)=IBOUST
  123 CONTINUE
C
      LDYNOR=LNLOCP.AND.(KSEC4(14).EQ.-1.OR.KSEC4(15).EQ.-1)
C
      IF (LDYNOR) THEN
C
C         Estimate interest of non-local packing use, and order
C         associated to.
C
C         In non-agressive mode (HOPER='C'), field is also transformed.
C
        LCOMSD=.NOT.LAGGPK
        LCBIAS=.TRUE.
        IORDER = C2DOSD ( KDATA,  KLENP,  KSEC4,  LCOMSD, KLEN,
     X                    IWORK,  JPWORK, IPOWER, JPXPWR, IPCWID,
     X                    JPXWID, IMINPK, ILWORX, IBIAS,  IWIDSD,
     X                    IXBITS, LCBIAS, LDEBUG )
        LCBIAS=.FALSE.    
C
      ELSE
C
        LCOMSD=.FALSE.
        LCBIAS=.TRUE.
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Row by row packing method applied, or tried.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
#ifndef ebug2o
      IF (LDEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9200) LROW, LCTWID, LGEN2O, LGENXT, LBOUST,
     X                          IORDER, LSIMPK
#ifndef ebug2o
      ENDIF
C
#endif
      IF (LROW) THEN
C
C       Upper part of work sub-arrays IWORK(IXIND (1,.,1)),
C       IWORK(IXIND (2,.,1)), IWORK(IXIND (1,.,2))
C       -reference values, effective row lengths, row widths- is used;
C       useful for aggressive packing, if row by row packing turns
C       to be selected as the appropriate method to apply, and if total
C       work space needed by the other methods do not overlap.
C
        IMETHO=IMTROW
        IROWMK=JPMXGF-INROWS+1
        ILOWMK=IROWMK
        LCOMGR=LFIRST.OR.IROWMK.LE.IHIGMK
C
        IF (LREVER.AND..NOT.LFIRST) THEN
C
C         Restore initial field ordering.
C
          LPRCLN=.NOT.LCOMGR
C
          IRETFN = REVERO ( KDATA,  KLENP,  KSEC2,  KGRIB,  KLENG,
     X                      KBITS,  KBMAP,  KVALS,  LPRIBI, LQUASI,
     X                      INROWS, KLEN,   IWORK(IXIND (2,1,2)),
     X                      JPMXGF, LDEBUG, LPRCLN,
     X                      IWORK(IXIND (2,IROWMK,1)) )
C
          IF (IRETFN.NE.0) THEN
            GO TO 900
          ENDIF
C
          LREVER=.FALSE.
C
        ENDIF
C
C         Enforce no extension at all, and especially 
C         no boustrophedonic ordering for row by row (useless).
C
        DO 201 J=12,15
        ISEC4(J,IMETHO)=0
  201   CONTINUE
C
        IRET = C2ROWS ( KDATA,  KLENP,  KSEC2,  ISEC4(1,IMETHO),KGRIB,
     X                  KLENG,  KNSPT,  KBITS,  LCOMGR, LLONLY, IROWL4,
     X                  KBMAP,  KVALS,  LPRIBI, LQUASI, INROWS, KLEN,
     X                  IWORK(IXIND (1,IROWMK,1)),
     X                  IWORK(IXIND (2,IROWMK,1)),
     X                  IWORK(IXIND (1,IROWMK,2)),
     X                  IWORK(IXIND (2,1,2)),   JPMXGF, IPOWER, JPXPWR,
     X                  IPCWID, JPXWID, IROWBF, LDEBUG )
C
        IF (LLONLY) THEN
C
#ifndef ebug2o
          IF (LDEBUG) THEN
#endif
            WRITE(GRPRSM,FMT=9210) IRET, IROWBF, IROWL4, IL4OPT
#ifndef ebug2o
          ENDIF
C
#endif
          IF (IRET.NE.0.OR.IROWL4.GE.IL4OPT) THEN
            LROW=.FALSE.
C
            IF (IRET.NE.0.AND.LBOUST) THEN
C
C               Reset IROWMK to avoid possible misuse of length array
C               in REVERO.
C
              IROWMK=0
            ENDIF
C
          ELSE
            IL4OPT=IROWL4
            LSIMPK=.FALSE.
          ENDIF
C
        ELSEIF (IRET.NE.0) THEN
C
          IRETFN = IRET
          GO TO 900
C
        ENDIF
C
#ifdef ebug2o
        IF (.NOT.LLONLY) THEN
#else
        IF (LDEBUG.AND..NOT.LLONLY) THEN
#endif
C
          YMETHO='Row by row'
          IL4OPT=IROWL4
C
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3 . Constant width packing method applied, or tried.
C     -----------------------------------------------------------------
C
      IF (LCTWID) THEN
C
        IMETHO=IMTCWI
        ILOWMK=MIN (ICWIMK,ILOWMK)
        LCOMGR=LFIRST.OR.ICWIMK.LE.IHIGMK
C
        IF (LCOMGR) THEN
          ICWIMK=1
          ICWORK=1
        ENDIF
C
        IF (LLONLY.AND.NUM2OK.EQ.1) THEN
C
C           Modulates width use, regardless of KSEC4(11).
C
          IF (LGENXT) THEN
C
C     Experience shows that the only cases where constant width packing
C     may compress data better than general extended method are for a
C     null width.
C
            ISEC4(11,IMETHO)=0
          ELSE
            ISEC4(11,IMETHO)=-(ISEC4(2,IMETHO)-1)
          ENDIF
C
        ENDIF
C
        LOPTLN=LFIRST.AND.ISEC4(11,IMETHO).LT.0
C
C         Enforce no extension flag, except boustrophedonic ordering.
C
        ISEC4(12,IMETHO)=0
        ISEC4(14,IMETHO)=0
        ISEC4(15,IMETHO)=0
C
        IRET = C2CWID ( KDATA,  KLENP,  ISEC4(1,IMETHO),KGRIB,  KLENG,
     X                  KNSPT,  KBITS,  LCOMGR, LLONLY, ICWIL4, ICWIFO,
     X                  KLEN,   IWORK(IXIND(1,1,1)),
     X                  IWORK(IXIND(2,1,1)),    JPMXGF, ICWIMK, IPOWER,
     X                  JPXPWR, ICWIBF, INCMIN, IHWM,   ICWORK, LDEBUG,
     X                  LOPTLN )
C
        IF (LLONLY) THEN
C
#ifndef ebug2o
          IF (LDEBUG) THEN
#endif
            WRITE(GRPRSM,FMT=9310) IRET, ICWIBF, ICWIL4, IL4OPT,
     X                              ICWIFO, IHWM, ICWORK
#ifndef ebug2o
          ENDIF
C
#endif
          IF (IRET.NE.0.OR.ICWIL4.GE.IL4OPT) THEN
            LCTWID=.FALSE.
            IHIGMK=MAX (IHIGMK,IHWM)
          ELSE
C
C     Constant width packing better than simple packing,
C     and possibly row by row.
C
            IL4OPT=ICWIL4
            LSIMPK=.FALSE.
            LROW=.FALSE.
C
            IF (LGEN2O.OR.LGENXT) THEN
C
C     Push up useful work arrays, trying to keep data if further
C     method(s) tried are not better.
C
              IOFF=JPMXGF-ICWIFO
              ICWIMK=IOFF+1
              IHIGMK=0
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
              DO 311 J=ICWIFO,1,-1
              IWORK(IXIND (1,IOFF+J,ICWORK))=IWORK(IXIND (1,J,ICWORK))
              IWORK(IXIND (2,IOFF+J,ICWORK))=IWORK(IXIND (2,J,ICWORK))
  311         CONTINUE
C
            ENDIF
C
          ENDIF
C
        ELSEIF (IRET.NE.0) THEN
C
          IRETFN = IRET
          GO TO 900
C
        ENDIF
C
#ifdef ebug2o
        IF (.NOT.LLONLY) THEN
#else
        IF (LDEBUG.AND..NOT.LLONLY) THEN
#endif
C
          WRITE (UNIT=YMETHO,FMT=9320) ISEC4(11,IMETHO)
          IL4OPT=ICWIL4
C
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 4 . General 2nd-order packing method applied, or tried.
C     -----------------------------------------------------------------
C
      IF (LGEN2O) THEN
C
        IMETHO=IMTGEN
        ILOWMK=MIN (IGENMK,ILOWMK)
        LCOMGR=.NOT.LGPREC
C
        IF (LCOMGR) THEN
          IGENMK=1
        ENDIF
C
C         Enforce no extension flag, except boustrophedonic ordering.
C
        ISEC4(12,IMETHO)=0
        ISEC4(14,IMETHO)=0
        ISEC4(15,IMETHO)=0
C
        IRET = C2GENE ( KDATA,  KLENP,  ISEC4(1,IMETHO),KGRIB,  KLENG,
     X                  KNSPT,  KBITS,  LCOMGR, LLONLY, IGENL4, IGENFO,
     X                  KLEN,   IWORK(IXIND (1,1,1)),
     X                  IWORK(IXIND (2,1,1)),   IWORK(IXIND (1,1,2)),
     X                  IWORK(IXIND (2,1,2)),   JPMXGF, IGENMK, IPOWER,
     X                  JPXPWR, IPCWID, JPXWID, IGENBF, INCMIN, IMINPK,
     X                  IHWM,   LDEBUG )
C
        IF (LLONLY) THEN
C
#ifndef ebug2o
          IF (LDEBUG) THEN
#endif
            WRITE(GRPRSM,FMT=9410) IRET, IGENBF, IGENL4, IL4OPT,
     X                              IGENFO, IHWM
#ifndef ebug2o
          ENDIF
C
#endif
          IF (IRET.NE.0.OR.IGENL4.GE.IL4OPT) THEN
            LGEN2O=.FALSE.
          ELSE
C
C     General 2nd-order packing better than simple packing,
C     and possibly row by row and/or constant width.
C
            IL4OPT=IGENL4
            LSIMPK=.FALSE.
            LROW=.FALSE.
            LCTWID=.FALSE.
C
          ENDIF
C
          IHIGMK=MAX (IHIGMK,IHWM)
          LGPREC=IRET.EQ.0
C
          IF (LGPREC.AND.LGENXT) THEN
C
C              Duplicate descriptors for further use by
C              general extended packing trial.
C
            IGXTBF=IGENBF
            IGXTFO=IGENFO
            IGXTMK=IGENMK
C
          ENDIF
C
        ELSEIF (IRET.NE.0) THEN
C
          IRETFN = IRET
          GO TO 900
C
        ENDIF
C
#ifdef ebug2o
        IF (.NOT.LLONLY) THEN
#else
        IF (LDEBUG.AND..NOT.LLONLY) THEN
#endif
C
          YMETHO='General second-order'
          IL4OPT=IGENL4
C
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 5 . General extended 2nd-order packing method applied,
C                 or tried.
C     -----------------------------------------------------------------
C
      IF (LGENXT) THEN
C
        IMETHO=IMTGXT
        ILOWMK=MIN (IGXTMK,ILOWMK)
        LCOMGR=.NOT.LGPREC.OR.(LFIRST.AND.LNLOCP)
C
  510   CONTINUE
C
        IF (LCOMGR) THEN
          IGXTMK=1
        ELSEIF (LFIRST) THEN
C
C         Inherit total length of second-order values from general WMO
C         second-order packing, performed just previously.
C
          ISEC4(19,IMETHO)=ISEC4(19,IMTGEN)
        ENDIF
C
        ISEC4(12,IMETHO)=8
        ISEC4(14,IMETHO)=2*(IORDER/2)
        ISEC4(15,IMETHO)=MOD (IORDER,2)
C
        IF (IORDER.NE.0.AND..NOT.LCOMSD) THEN
C
C           Spatial differencing has to be applied to field.
C
          LCOMSD=.TRUE.
          IORDER = C2DOSD ( KDATA,  KLENP,  ISEC4(1,IMETHO), LCOMSD,
     S                      KLEN,   IWORK,  JPWORK, IPOWER,  JPXPWR,
     S                      IPCWID, JPXWID, IMINPK, ILWORX,  IBIAS,
     S                      IWIDSD, IXBITS, LCBIAS, LDEBUG )     
C
C           Reset ISEC4, because C2DOSD may have reset IORDER.
C
          ISEC4(14,IMETHO)=2*(IORDER/2)
          ISEC4(15,IMETHO)=MOD (IORDER,2)
C
        ENDIF
C
        IF (IORDER.NE.0) THEN
          ISEC4( 2,IMETHO)=IXBITS
          ISEC4(11,IMETHO)=IWIDSD
          ISEC4(16,IMETHO)=IBIAS
        ENDIF
C
        IRET = C2GENE ( KDATA,  KLENP,  ISEC4(1,IMETHO),KGRIB,  KLENG,
     X                  KNSPT,  KBITS,  LCOMGR, LLONLY, IGXTL4, IGXTFO,
     X                  KLEN,   IWORK(IXIND (1,1,1)),
     X                  IWORK(IXIND (2,1,1)),   IWORK(IXIND (1,1,2)),
     X                  IWORK(IXIND (2,1,2)),   JPMXGF, IGXTMK, IPOWER,
     X                  JPXPWR, IPCWID, JPXWID, IGXTBF, INCMIN, IMINPK,
     X                  IHWM,   LDEBUG )
C
        IF (LLONLY) THEN
C
#ifndef ebug2o
          IF (LDEBUG) THEN
#endif
            WRITE(GRPRSM,FMT=9510) IRET, IGXTBF, IGXTL4, IL4OPT,
     X                              IGXTFO, IHWM, IORDER
#ifndef ebug2o
          ENDIF
C
#endif
          IF (IRET.NE.0.OR.IGXTL4.GE.IL4OPT) THEN
C
C             Current method is not successful, and has to be discarded.
C
            LGENXT=.FALSE.
C
            IF (IRET.NE.0.OR..NOT.LGEN2O) THEN
              LGPREC=.FALSE.
            ELSE
              LGPREC=IGXTFO.EQ.IGENFO.AND.IORDER.EQ.0
            ENDIF
C
            IF (LCOMSD.AND.IORDER.NE.0) THEN
C
C              Spatial differencing has to be reversed.
C
              IRETFN = D2ROSD ( KDATA,  KLEN,   IORDER, IPOWER, JPXPWR,
     S                          IBIAS,  LVECTD, LDEBUG )     
C
              IF (IRETFN.NE.0) THEN
                GO TO 900
              ENDIF
C
              LCOMSD=.FALSE.
C
              IF (LGEN2O.AND.LDYNOR) THEN
C
C    General WMO 2nd-order packing was also selected/promoted, and is
C    the best method. This indicates moreless wrong diagnostic for
C    spatial differencing order.
C    Retry then general extended packing without spatial differencing,
C    that in almost all cases behaves better than general WMO method.
C
#ifndef ebug2o
                IF (LDEBUG) THEN
#endif
                  WRITE(GRPRSM,FMT=9520) IORDER
#ifndef ebug2o
                ENDIF
C
                IORDER=0
                LGENXT=.TRUE.
                LGPREC=.FALSE.
                LCOMGR=.TRUE.
                ISEC4(2,IMETHO)=KSEC4(2)
                GOTO 510
C
              ENDIF
C
            ENDIF
C
          ELSE
C
C     General extended 2nd-order packing better than simple packing,
C     and possibly other methods.
C
            IL4OPT=IGXTL4
            LSIMPK=.FALSE.
            LROW=.FALSE.
            LCTWID=.FALSE.
            LGEN2O=.FALSE.
            LGPREC=.TRUE.
C
          ENDIF
C
          IHIGMK=MAX (IHIGMK,IHWM)
C
        ELSEIF (IRET.NE.0) THEN
C
          IRETFN = IRET
          GO TO 900
C
        ENDIF
C
#ifdef ebug2o
        IF (.NOT.LLONLY) THEN
#else
        IF (LDEBUG.AND..NOT.LLONLY) THEN
#endif
C
          YMETHO='Gen.extend.2nd-order'
          IL4OPT=IGXTL4
C
        ENDIF
C
      ENDIF
#endif
C
C     -----------------------------------------------------------------
C*    Section 6 . Effective in case of aggressive packing only.
C     -----------------------------------------------------------------
C
  600 CONTINUE
C
      IF (LSIMPK) THEN
C
C       Simple packing by default: complex packing not efficient,
C                                  or not applicable.
C
C     The first 10 octets have already been coded by GRIBEX, or will
C     be in a further step of GRIBEX:
C     Length of section, flags+unused bit count, binary scale factor,
C     reference value.
C
C     Enforce right values for flags.
C
        KSEC4(4)=0
        KSEC4(6)=0
        IMETHO=0
        YMETHO='Simple'
C
C*    Octet 11 : Number of bits containing each first-order value.
C     One 8 bit field.
C
        YOPER='C'
        KNSPT=KNSPT+10*8
        CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(2),1,KBITS, 8,YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 18610
          WRITE(GRPRSM,FMT=9001) 'bit number'
          GO TO 900
        ENDIF
C
        IF (LREVER) THEN
C
C         Restore initial field ordering.
C
          LPRCLN=(LFIRST.AND.LROW).OR.IROWMK.GT.IHIGMK
          INDCLN=MAX (IROWMK,1)
C
          IRETFN = REVERO ( KDATA,  KLENP,  KSEC2,  KGRIB,  KLENG,
     X                      KBITS,  KBMAP,  KVALS,  LPRIBI, LQUASI,
     X                      INROWS, KLEN,   IWORK(IXIND (2,1,2)),
     X                      JPMXGF, LDEBUG, LPRCLN,
     X                      IWORK(IXIND (2,INDCLN,1)) )
C
          IF (IRETFN.NE.0) THEN
            GO TO 900
          ENDIF
C
          LREVER=.NOT.LREVER
        ENDIF
C
C*    Octet 12 - onwards: Data.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,KDATA,KLEN,KBITS,KSEC4(2),
     X              YOPER,IRET)
C
        IF (IRET.NE.0) THEN
          IRETFN = 18620
          WRITE(GRPRSM,FMT=9001) 'normalized data'
          GO TO 900
        ENDIF
C
      ELSEIF (LLONLY) THEN
C
C       Loop back to apply the most efficient second-order method,
C       as previously diagnosed. And hoping work arrays have not
C       been overwritten, to avoid computing them again.
C
        LLONLY=.FALSE.
        LFIRST=.FALSE.
        GOTO 200
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 7 . Debugging printout.
C     -----------------------------------------------------------------
C
  700 CONTINUE
C
#ifndef ebug2o
      IF (LDEBUG) THEN
C
#endif
        ILMETH=INDEX (YMETHO//'  ','  ')-1
        WRITE(GRPRSM,FMT=9710) HOPER(:1), YMETHO(:ILMETH), IL4OPT
C
        IF (.NOT.LSIMPK) THEN
C
          IF (LBOUST.AND..NOT.LROW) THEN
            WRITE(GRPRSM,FMT=9720)
          ENDIF
C
          IF (LGENXT.AND.IORDER.NE.0) THEN
            WRITE(GRPRSM,FMT=9730) IORDER
          ENDIF
C
          ZGAIN=REAL (100*(ILSIMP-IL4OPT)) / REAL (ILSIMP)
          ILREFR=INDEX (YREFER//'  ','  ')-1
          WRITE(GRPRSM,FMT=9002) YREFER(:ILREFR), ZGAIN
C
#ifndef ebug2o
        ENDIF
C
#endif
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
C     If second-order method was effectively and successfully used,
C     copy back information of section 4 descriptors to KSEC4.
C
      IF (IRETFN.NE.0.AND.IMETHO.NE.0) THEN
C
        DO 901 J=1,JPVSE4
        KSEC4(J)=ISEC4(J,IMETHO)
  901   CONTINUE
C
      ENDIF
C
      C2ORDR = IRETFN
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
 9001 FORMAT (' C2ORDR: Problem inserting ',A,'.')
C
 9002 FORMAT (' C2ORDR: Gain on section 4 length against ',A,':',
     X        F5.1,' %.')
C
 9100 FORMAT (' C2ORDR: Function start, HOPER = ',A1,', KNSPT =',I10,
     X  '.',/,' C2ORDR: Original number of bits =',I3,'.',/,
     X        ' C2ORDR: Presence of sections 2 and 3 = ',L1,' and ',
     X        L1,'.')
C
 9110 FORMAT (' C2ORDR: Matrix of values invalid here.')
C
 9120 FORMAT (' C2ORDR: ',A,' only supported with a section 2.')
 9130 FORMAT (' C2ORDR: ',A,'/',A,': not supported.')
 9131 FORMAT (' C2ORDR: Approx. s4 length, ',A,',',I3,
     X        '-length groups:',I9,'.')
 9132 FORMAT (' C2ORDR: Range diagnostic is ',A,
     X        ' of spatial differences, order',I2,'.')
 9133 FORMAT (' C2ORDR: Selected order:',I2,', range diagnostics:',
     X        4I8,'.')
 9140 FORMAT (' C2ORDR: KSEC4(',I2,') must be 0 or ',I2,'.')
 9150 FORMAT (' C2ORDR: General extended packing implies ',A,'.')
 9153 FORMAT (' C2ORDR: Boustrophedonic ordering ignored with ',
     X        'row by row packing.')
 9155 FORMAT (' C2ORDR: KSEC4(',I2,') must be 0, -1 or ',I2,'.')
 9156 FORMAT (' C2ORDR: Spatial differencing ignored without ',
     X        'general extended 2nd-order packing.')
 9160 FORMAT (' C2ORDR: Bits number =',I3,' - maximum allowed =',I3,
     X        '.')
 9170 FORMAT (' C2ORDR: Constant width =',I3,
     X        ' - out of range [-/+',I2,'(=KSEC4(2)-1)].')
 9175 FORMAT (' C2ORDR: Constant field, value = ',F30.20,'.')
 9180 FORMAT (' C2ORDR: INROWS =',I6,', KLEN =',I9,', LQUASI = ',L1,
     X        '.')
 9185 FORMAT (' C2ORDR: Inconsistency: total g.p. number =',I9,
     X        ' expected =',I9,'.')
 9187 FORMAT (' C2ORDR: ',I9,' bits set in primary bit-map -',
     X        ' not in range [1,KSEC4(1)=',I9,'].')
 9190 FORMAT (' C2ORDR: Too much rows:',I6,' exceeds work space:',I6,
     X        '.')
#ifndef wmogrp2o
 9192 FORMAT (' C2ORDR: Implicit 1ary bit-map/g.p. number >',I6,
     X        ': unsupported.')
#endif
 9195 FORMAT (' C2ORDR: No packing method selected or applicable.',
     X        ' C2ORDR: Simple packing applied as backup.')
C
 9200 FORMAT (' C2ORDR: LROW=',L1,', LCTWID=',L1,', LGEN2O=',L1,
     X        ', LGENXT=',L1,', LBOUST=',L1,', IORDER=',I1,
     X        ', LSIMPK=',L1,'.')
C
 9210 FORMAT (' C2ORDR: (row by row) IRET, IROWBF, IROWL4, IL4OPT=',
     X        I6,I3,I9,I9,'.')
C
 9310 FORMAT (' C2ORDR: (cons.width) IRET, ICWIBF, ICWIL4, IL4OPT=',
     X        I6,I3,I9,I9,'.',/,
     X        ' C2ORDR:              ICWIFO, IHWM, ICWORK=',2I8,I2,'.')
C
 9320 FORMAT ('Constant width (',I2,')')
C
 9410 FORMAT (' C2ORDR: (general 2o) IRET, IGENBF, IGENL4, IL4OPT=',
     X        I6,I3,I9,I9,'.',/,
     X        ' C2ORDR:              IGENFO, IHWM=',2I8,'.')
C
 9510 FORMAT (' C2ORDR: (gen.extend) IRET, IGXTBF, IGXTL4, IL4OPT=',
     X        I6,I3,I9,I9,'.',/,
     X        ' C2ORDR:              IGXTFO, IHWM, IORDER=',2I8,I2,'.')
 9520 FORMAT (' C2ORDR: Non-optimal spatial differencing at order',I2,
     X        ', general 2o is better...',/,
     X' C2ORDR: .. retrying gen.extend. without spatial differencing.')
C
 9710 FORMAT (' C2ORDR: HOPER = ',A1,' - ',A,
     X        ' packing applied, IL4OPT =',I9,'.')
 9720 FORMAT (' C2ORDR: Boustrophedonic ordering has been used.')
 9730 FORMAT (' C2ORDR: Spatial differencing applied, order is',I2,'.')
C
 9900 FORMAT (' C2ORDR: Function return code =',I6,' HOPER = ',A,
     X        ', KNSPT =',I10,'.')
C
      END
