C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2DOSD ( KDATA,  KLENP,  KSEC4,  OCOMSD, KLEN,  
     X                          KWORK,  KLWORK, KPOWER, KMXPWR, KPCWID,
     X                          KXCWID, KMINPK, KLWORX, KBIAS,  KWIDSD,
     X                          KWIDOR, OCBIAS, ODEBUG )
C
C---->
C**** C2DOSD
C
C     Purpose.
C     --------
C
C     Depending on actual arguments, Diagnose Order of Spatial
C     Differences, and/or when requested and order is not null,
C     computes spatial differences for GRIB second-order extended
C     packing.
C
C**   Interface.
C     ----------
C
C     IORDER = C2DOSD ( KDATA,  KLENP,  KSEC4,  OCOMSD, KLEN,   KWORK,
C    X                  KLWORK, KPOWER, KMXPWR, KPCWID, KXCWID, KMINPK,
C    X                  KLWORX, KBIAS,  KWIDSD, KWIDOR, OCBIAS, ODEBUG )
C
C     Function result value is the order of spatial differencing
C     (diagnosed or imposed).
C
C     Input Parameters.
C     -----------------
C
C     KDATA      - Array of normalized values.
C     KLENP      - Length of array KDATA.
C     KSEC4      - Array of GRIB section 4 integer descriptors.
C     OCOMSD     - True for COMputing Spatial Differences.
C     KLEN       - Exact number of grid-points to handle.
C     KLWORK     - Length of work array KWORK.
C     KPOWER     - Auxilary array, precomputed (powers of 2)-1.
C     KMXPWR     - Maximum number of bits allowed.
C     KPCWID     - Auxilary array, precomputed widths.
C     KXCWID     - Maximum width associated to KPCWID.
C     KMINPK     - Initial (minimum) length of groups.
C     KBIAS      - BIAS of spatial differences (if OCBIAS is .FALSE.).
C     OCBIAS     - True for Computation of BIAS.
C     ODEBUG     - True for some debug printout.
C
C
C     Output Parameters (effective only if function value is non zero).
C     ------------------
C
C     KDATA      - Array of normalized values (transformed).
C     KWORK      - Work array.
C     KLWORX     - Upper index used within KWORK array.
C     KBIAS      - BIAS of spatial differences.
C     KWIDSD     - WIDth of Spatial Differencing specific descriptors.
C     KWIDOR     - WIDth of field, after possible transformation.
C
C
C     Method.
C     -------
C
C     If requested (KSEC4(14) or KSEC4(15) equal to -1), first
C     compute "best" order of spatial differencing (up to 3) to be
C     applied to KDATA. Estimation is made through comparison of
C     average ranges of original field and of spatial differencing,
C     starting from order 1.
C
C     If requested through OCOMSD and when order is non zero, computes
C     spatial differences on KDATA (keeping the first IORDER values
C     from original field, for encoding).
C
C
C     Externals.
C     ----------
C
C     MAXMNI    - Computes extrema of integer array.
C     C2RNGE    - Computes range diagnostics.
C     C2BITW    - Computes bit width of a positive integer value.
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
C     Best order is considered reached when average range is found
C     greater than previously diagnosed. This feature is derived from
C     practice, and reduces CPU costs.
C
C     Work array use when diagnostic is requested:
C
C     -if OCOMSD is true and if KLWORK is greater than (or equal to)
C      (2*KLEN-3), there is no recomputation from KDATA itself;
C
C     -if KLWORK is greater than (or equal to) (KLEN-1), then KWORK
C      is used recursively;
C
C     -otherwise, KDATA has to be used in slices and computation is
C      more expensive.
C
C
C     Author.
C     -------
C
C     J. Clochard, Meteo France, for ECMWF - April 1998.
C
C
C     Modifications.
C     _____________
C
C     J. Clochard, June 1999.
C     Use precomputed array to determine widths . Dummy-arguments
C     KPCWID and KXCWID added . Assumption is made that KXCWID
C     is at least (KMXPWR+1)/2 . KPCWID/KXCWID added in C2RNGE calls .
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
      INTEGER JPORDR
C
C       Maximum order of spatial differencing allowed.
C
      PARAMETER ( JPORDR=3 )
C
C     Subroutine arguments
C
      INTEGER KLENP, KLWORX, KLEN, KLWORK, KMXPWR, KMINPK, KBIAS, KWIDOR
      INTEGER KWIDSD, KXCWID
      INTEGER KDATA (KLENP), KSEC4 (*), KPOWER (0:KMXPWR), KPCWID (0:*)
      INTEGER KWORK (KLWORK)
C
      LOGICAL OCOMSD, ODEBUG, OCBIAS
C
C     Local variables.
C
      INTEGER IBASE, IOFFWK, IREFER, J, INPACK, IMIN, IMAX, IXORDR
      INTEGER INBIFO, ILEN, IMAXAB, IWIDAB, IDIAGN, IOFFWP, ILWORX
      INTEGER JORDER, IORDER, IAUX1, IAUX2, IAUX3, IRANGE, IBITGW
      INTEGER INPACW, INBSEQ, JSEQ, ILWORK, IAUXIL, INPACS, IXBITS
C
      INTEGER IDIAGO ( 0:JPORDR ), IMINAB (JPORDR), INBIAB (JPORDR)
C
      INTEGER C2BITW, C2RNGE
      EXTERNAL C2BITW, C2RNGE
C
      INTRINSIC ABS
C
      LOGICAL L1PASS, LCBIAS, LIBIAS
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
        WRITE(GRPRSM,FMT=9100) OCOMSD, OCBIAS
#ifndef ebug2o
      ENDIF
#endif
C
      IXORDR=MIN (JPORDR,KLEN)
      IXBITS=KSEC4(2)
      ILWORX=0
      IBASE=0
      L1PASS=OCOMSD.AND.KLWORK.GE.(KLEN-1)
      LIBIAS=.FALSE.
C
      IF (KSEC4(14).NE.-1.AND.KSEC4(15).NE.-1) THEN
C
C         Order of spatial differencing explicitly specified.
C
        IORDER=MIN ( IXORDR, KSEC4(14)+KSEC4(15) )
        LCBIAS=OCBIAS
        L1PASS=.FALSE.
        GOTO 400
C
      ELSEIF (OCOMSD.AND.KLWORK.GE.2*KLEN-3) THEN
C
        IBASE=KLEN-1
C
      ENDIF
C
      LCBIAS=.FALSE.
C
C     -----------------------------------------------------------------
C*    Section 2 . Computes average range of original normalized values.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      INPACK=KLEN/KMINPK
      IDIAGN=0
      IWIDAB=0
      IREFER=0
C
      IOFFWK = C2RNGE ( INPACK, KMINPK, KSEC4(2), KDATA,  IDIAGN,
     X                  IWIDAB, IREFER, KPOWER,   KMXPWR, KPCWID,
     X                  KXCWID )
C
      INBIFO = C2BITW ( IREFER, KSEC4(2), KPOWER, KMXPWR )
C
      IBITGW = C2BITW ( IWIDAB, KSEC4(2), KPOWER, KMXPWR )
C
C     IDIAGN=(IDIAGN+7)/8
      IDIAGN=(IDIAGN+7)/8+(INPACK*IBITGW+7)/8+(INPACK*INBIFO+7)/8
C     IDIAGN=(IDIAGN+7)/8+(INPACK*IBITGW+7)/8
      IDIAGO(0)=IDIAGN
      IORDER=0
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        PRINT *, 'IDIAGO(0) = ', IDIAGN, INPACK, IBITGW, INBIFO
C
        WRITE(GRPRSM,FMT=9211) 'original values', KMINPK, IDIAGO(0)
#ifndef ebug2o
      ENDIF
C
#endif
C
C     -----------------------------------------------------------------
C*    Section 3 . Computes average range of spatial differences.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
      IOFFWP=0
      INPACW=KLWORK/KMINPK
      ILWORK=INPACW*KMINPK
C
C           Loop on order of spatial differencing.
C
      DO 329 JORDER=1,IXORDR
C
C           Compute spatial differences.
C
      ILEN=KLEN-JORDER
      INPACK=ILEN/KMINPK
      IDIAGO(JORDER)=0
      IWIDAB=0
      IREFER=-KPOWER(KMXPWR)
      IOFFWK=MOD (JORDER-1,2)*IBASE
      IXBITS=MIN(IXBITS+1,KMXPWR)
C
      IF (L1PASS) THEN
C
        IF (JORDER.EQ.1) THEN
C
          DO 301 J=1,ILEN
          KWORK(IOFFWK+J)=KDATA(J+1)-KDATA(J)
  301     CONTINUE
C
        ELSE
C
#ifdef CRAY
CDIR$ IVDEP
#endif
#ifdef FUJITSU
!OCL NOVREC
#endif
          DO 302 J=1,ILEN
          KWORK(IOFFWK+J)=KWORK(IOFFWP+J+1)-KWORK(IOFFWP+J)
  302     CONTINUE
C
        ENDIF
C
        IOFFWP=IOFFWK
        ILWORX = MAX ( ILWORX, IOFFWK+ILEN )
C
        CALL MAXMNI (KWORK(IOFFWK+1),ILEN,IMAXAB,IMINAB(JORDER))
C
C         Check that extrema difference would not lead to overflow.
C
        IRANGE=IMAXAB/2-IMINAB(JORDER)/2
C
        IF (IRANGE.GT.KPOWER(KMXPWR-1)) THEN
          IORDER=JORDER-1
          WRITE(GRPRSM,FMT=9301) JORDER, IORDER
          GOTO 330
        ENDIF
C
        IAUXIL = C2RNGE ( INPACK, KMINPK, IXBITS, KWORK(IOFFWK+1),
     X                    IDIAGO(JORDER), IWIDAB, IREFER, KPOWER,
     X                    KMXPWR, KPCWID, KXCWID )
C
      ELSE
C
C       Work array too short. Data array processed into slices.
C
        INBSEQ=1+(ILEN-1)/ILWORK
        IMAXAB=-KPOWER(KMXPWR)
        IMINAB(JORDER)=KPOWER(KMXPWR)
C
        DO 314 JSEQ=1,INBSEQ
C
        IAUXIL=MIN (ILWORK,ILEN-(JSEQ-1)*ILWORK)
        INPACS=MIN (INPACW,INPACK-(JSEQ-1)*INPACW)
C
        IF (JORDER.EQ.1) THEN
C
          DO 311 J=1,IAUXIL
          KWORK(J)=KDATA(IOFFWK+J+1)-KDATA(IOFFWK+J)
  311     CONTINUE
C
        ELSEIF (JORDER.EQ.2) THEN
C
          DO 312 J=1,IAUXIL
          IAUX1=KDATA(IOFFWK+J+1)-KDATA(IOFFWK+J  )
          IAUX2=KDATA(IOFFWK+J+2)-KDATA(IOFFWK+J+1)
          KWORK(J)=IAUX2-IAUX1
  312     CONTINUE
C
        ELSE
C
          DO 313 J=1,IAUXIL
          IAUX1=KDATA(IOFFWK+J+1)-KDATA(IOFFWK+J  )
          IAUX2=KDATA(IOFFWK+J+2)-KDATA(IOFFWK+J+1)
          IAUX3=KDATA(IOFFWK+J+3)-KDATA(IOFFWK+J+2)
          KWORK(J)=(IAUX3-IAUX2)-(IAUX2-IAUX1)
  313     CONTINUE
C
        ENDIF
C
        ILWORX = MAX ( ILWORX, IAUXIL )
C
        CALL MAXMNI (KWORK,IAUXIL,IMAX,IMIN)
        IMAXAB=MAX (IMAXAB,IMAX)
        IMINAB(JORDER)=MIN (IMINAB(JORDER),IMIN)
C
C         Check that extrema difference would not lead to overflow.
C
        IRANGE=IMAXAB/2-IMINAB(JORDER)/2
C
        IF (IRANGE.GT.KPOWER(KMXPWR-1)) THEN
          IORDER=JORDER-1
          WRITE(GRPRSM,FMT=9301) JORDER, IORDER
          GOTO 330
        ELSEIF (INPACS.GT.0) THEN
C
          IAUXIL = C2RNGE ( INPACS,         KMINPK, IXBITS, KWORK,
     S                      IDIAGO(JORDER), IWIDAB, IREFER,
     S                      KPOWER,         KMXPWR, KPCWID, KXCWID )
          IOFFWK=IOFFWK+IAUXIL
C
        ENDIF
C
  314   CONTINUE
C
      ENDIF
C
      IRANGE=IMAXAB-IMINAB(JORDER)
C
      INBIAB(JORDER) = C2BITW ( IRANGE, KSEC4(2)+JORDER,
     X                          KPOWER, KMXPWR )
C
      INBIFO = C2BITW ( IREFER, IXBITS, KPOWER, KMXPWR )
C
      IBITGW = C2BITW ( IWIDAB, IXBITS, KPOWER, KMXPWR )
C
      IXBITS=INBIAB(JORDER)
C
C     IDIAGO(JORDER)=(IDIAGO(JORDER)+7)/8
      IDIAGO(JORDER)=(IDIAGO(JORDER)+7)/8+(INPACK*IBITGW+7)/8
     S               +(INPACK*INBIFO+7)/8
C     IDIAGO(JORDER)=(IDIAGO(JORDER)+7)/8+(INPACK*IBITGW+7)/8
C
#ifndef ebug2o
      IF (ODEBUG) THEN
#endif
        PRINT *, 'JORDER=', JORDER, ', IDIAGO(.) = ', IDIAGO(JORDER),
     S           INPACK, IBITGW, INBIFO
C
        WRITE(GRPRSM,FMT=9211) 'spatial differences', KMINPK,
     S                            IDIAGO(JORDER)
#ifndef ebug2o
      ENDIF
C
#endif
C
      IF (IDIAGO(JORDER).LT.IDIAGN) THEN
C
#ifndef ebug2o
        IF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9312) 'in favour', JORDER
#ifndef ebug2o
        ENDIF
C
#endif
        IDIAGN=IDIAGO(JORDER)
        IORDER=JORDER
C
      ELSE
C
#ifndef ebug2o
        IF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9312) '*NOT* in favour', JORDER
#ifndef ebug2o
        ENDIF
C
#endif
        GOTO 330
C
      ENDIF
C
  329 CONTINUE
C
  330 CONTINUE
C
      LIBIAS=.TRUE.
#ifndef ebug2o
C
      IF (ODEBUG) THEN
#endif
C
        DO 331 J=IORDER+2,JPORDR
        IDIAGO(J)=0
  331   CONTINUE
C
        WRITE(GRPRSM,FMT=9331) IORDER, IDIAGO
#ifndef ebug2o
      ENDIF
#endif
C
C     -----------------------------------------------------------------
C*    Section 4 . Computes spatial differences back into KDATA.
C     -----------------------------------------------------------------
C
  400 CONTINUE
C
      IF (IORDER.EQ.0) THEN
C
C         Prognosis is that spatial differencing is not worth.
C         Nothing more to do then.
C
        GOTO 900
C
      ELSEIF (.NOT.OCOMSD) THEN
C
C         Computing of spatial differences not requested.
C
        GOTO 500
C
      ELSEIF (IBASE.NE.0.OR.(IORDER.EQ.IXORDR.AND.L1PASS)) THEN
C
C         Required data have been computed above, and are still
C         available.
C
        IOFFWK=MOD(IORDER-1,2)*IBASE
#ifdef ebug2o
        PRINT *,'c2dosd - Execution of loop 401, IORDER= ',IORDER,
     X          ', IOFFWK= ',IOFFWK
#endif
C
        DO 401 J=1,KLEN-IORDER
        KDATA(IORDER+J)=KWORK(IOFFWK+J)-IMINAB(IORDER)
  401   CONTINUE
C
      ELSEIF (LCBIAS) THEN
C
C               Spatial differences first computed without bias.
C
        IF (IORDER.EQ.1) THEN
#ifdef ebug2o
C
          PRINT *,'c2dosd - Execution of loop 402, IORDER= ',IORDER
#endif
C
          DO 402 J=KLEN,2,-1
          KDATA(J)=KDATA(J)-KDATA(J-1)
  402     CONTINUE
C
        ELSEIF (IORDER.EQ.2) THEN
#ifdef ebug2o
C
          PRINT *,'c2dosd - Execution of loop 403, IORDER= ',IORDER
#endif
C
          DO 403 J=KLEN,3,-1
          IAUX1=KDATA(J-1)-KDATA(J-2)
          IAUX2=KDATA(J  )-KDATA(J-1)
          KDATA(J)=IAUX2-IAUX1
  403     CONTINUE
C
        ELSE
#ifdef ebug2o
C
          PRINT *,'c2dosd - Execution of loop 404, IORDER= ',IORDER
#endif
C
          DO 404 J=KLEN,4,-1
          IAUX1=KDATA(J-2)-KDATA(J-3)
          IAUX2=KDATA(J-1)-KDATA(J-2)
          IAUX3=KDATA(J  )-KDATA(J-1)
          KDATA(J)=(IAUX3-IAUX2)-(IAUX2-IAUX1)
  404     CONTINUE
C
        ENDIF
C
C             Now compute and remove bias.
C
        ILEN=KLEN-IORDER
        CALL MAXMNI (KDATA(IORDER+1),ILEN,IMAXAB,IMINAB(IORDER))
#ifdef ebug2o
C
        PRINT *,'c2dosd - Execution of loop 405, IORDER= ',IORDER,
     X           ', Computed bias= ',IMINAB(IORDER)
#endif
C
        DO 405 J=IORDER+1,KLEN
        KDATA(J)=KDATA(J)-IMINAB(IORDER)
  405   CONTINUE
C
C             Compute width of spatial differences.
C
        IRANGE=IMAXAB-IMINAB(IORDER)
C
        INBIAB(IORDER) = C2BITW ( IRANGE, KSEC4(2)+IORDER,
     X                            KPOWER, KMXPWR )
C
      ELSEIF (IORDER.EQ.1) THEN
C
C         Explicit computing for 1st-order spatial differences.
C
#ifdef ebug2o
        PRINT *,'c2dosd - Execution of loop 412, IORDER= ',IORDER,
     X           ', Bias= ',KBIAS
C
#endif
        DO 412 J=KLEN,2,-1
        KDATA(J)=KDATA(J)-KDATA(J-1)-KBIAS
  412   CONTINUE
C
      ELSEIF (IORDER.EQ.2) THEN
C
C         Explicit computing for 2nd-order spatial differences.
C
#ifdef ebug2o
        PRINT *,'c2dosd - Execution of loop 413, IORDER= ',IORDER,
     X           ', Bias= ',KBIAS
C
#endif
        DO 413 J=KLEN,3,-1
        IAUX1=KDATA(J-1)-KDATA(J-2)
        IAUX2=KDATA(J  )-KDATA(J-1)
        KDATA(J)=IAUX2-IAUX1-KBIAS
  413   CONTINUE
C
      ELSE
C
C         Explicit computing for 3rd-order spatial differences.
C
#ifdef ebug2o
        PRINT *,'c2dosd - Execution of loop 414, IORDER= ',IORDER,
     X           ', Bias= ',KBIAS
C
#endif
        DO 414 J=KLEN,4,-1
        IAUX1=KDATA(J-2)-KDATA(J-3)
        IAUX2=KDATA(J-1)-KDATA(J-2)
        IAUX3=KDATA(J  )-KDATA(J-1)
        KDATA(J)=(IAUX3-IAUX2)-(IAUX2-IAUX1)-KBIAS
  414   CONTINUE
C
      ENDIF
C
C       Note that the IORDER first values of KDATA are kept unchanged.
C       These "integration constants" will be packed separately.
C
C     -----------------------------------------------------------------
C*    Section 5 . Computes additional descriptors.
C     -----------------------------------------------------------------
C
  500 CONTINUE
C
      IF (LCBIAS.OR.LIBIAS) THEN
C
        KBIAS=IMINAB(IORDER)
        KWIDOR=INBIAB(IORDER)
C
C          Extra descriptors specific to spatial differencing are
C          the IORDER first values of "original" field, the bias
C          of spatial differences, and the width for all of them.
C
        CALL MAXMNI (KDATA,IORDER,IMAX,IMIN)
        IAUX1 = C2BITW ( IMAX, KMXPWR, KPOWER, KMXPWR )
        IAUX2=IABS (KBIAS)
        IAUX3 = C2BITW ( IAUX2, KMXPWR, KPOWER, KMXPWR )
        KWIDSD=MAX (IAUX1,1+IAUX3)
C
#ifndef ebug2o
        IF (ODEBUG) THEN
#endif
          WRITE(GRPRSM,FMT=9420) IORDER, KWIDOR, KWIDSD,
     X                            (KDATA(J),J=1,IORDER), KBIAS
#ifndef ebug2o
        ENDIF
#endif
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      KLWORX=ILWORX
C
      C2DOSD = IORDER
C
#ifndef ebug2o
C
      IF (ODEBUG) THEN
#endif
        WRITE(GRPRSM,FMT=9900) IORDER
#ifndef ebug2o
      ENDIF
#endif
C
      RETURN
C
 9100 FORMAT (' C2DOSD: Function start, OCOMSD = ',L1,', OCBIAS = ',
     X        L1,'.')
C
 9211 FORMAT (' C2DOSD: Approx. s4 length, ',A,',',I3,
     X        '-length groups:',I9,'.')
C
 9301 FORMAT (' C2DOSD: integer overflow for JORDER =',I2,
     X        '. IORDER set to',I2,'.')
C
 9312 FORMAT (' C2DOSD: Range diagnostic is ',A,
     X        ' of spatial differences, order',I2,'.')
 9331 FORMAT (' C2DOSD: Selected order:',I2,', range diagnostics:',
     X        4I8,'.')
C
 9420 FORMAT (' C2DOSD: KWIDSD/KWIDOR/KDATA(1:',I1,')/KBIAS=',2I3,5I11)
C
 9900 FORMAT (' C2DOSD: Function return code =',I3,'.')
C
      END
