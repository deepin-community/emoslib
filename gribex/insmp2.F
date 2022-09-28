C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE INSMP2 (KGRIB,KLENG,KMAP2,PSEC4,KSIZE,KBITS,
     X                     PMISS,HFUNC,KNUM,KPR,KRET)
C
C---->
C**** INSMP2 - Insertion of secondary bit maps for routine GRIBEX.
C
C     Purpose.
C     --------
C
C     Generates secondary bit-maps (Section 4 bit-maps) and
C     inserts in array of GRIB coded data.
C
C**   Interface.
C     ----------
C
C     CALL INSMP2 (KGRIB,KLENG,KMAP2,PSEC4,KSIZE,KBITS,
C    X                     PMISS,HFUNC,KNUM,KPR,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     KGRIB      - Array into which data is being packed in GRIB code.
C
C     KLENG      - Length of this array.
C
C     KMAP2      - Bit-pointer to start of secondary bit maps
C                  (Section 4 bitmaps) in array KGRIB.
C
C     PSEC4      - Array of data values to be packed in
C                  GRIB code, containing missing data
C                  indicator PMISS where appropriate.
C
C     KSIZE      - Number of values, including missing data
C                  values, in array PSEC4.
C
C     KBITS      - Number of bits in computer word.
C
C     PMISS      - Value indicating missing data in array PSEC4.
C
C     HFUNC      - 'C' , GRIB data packed normally.
C                  'M' , GRIB data being packed in fixed length messages
C
C     KNUM       - Number of values at each grid point.
C
C     KPR        - Debug print switch.
C                  0  , No printout.
C                  >0 , Debug printout.
C
C     Output Parameters.
C     ------------------
C
C     KSIZE      - The number of data values remaining in
C                  array PSEC4 to be packed.
C
C     KRET       - Return code.
C                  0 , No error encountered.
C                  1 , Error in routine INXBIT.
#if (!defined VAX)
C                  2 , Memory allocation problem.
C                  3 , Memory deallocation problem.
#else
C                  4 , (VAX only) Number of values at each grid point
C                      (KNUM) is greater than allowed (JPWORK).
#endif
C
C     Method.
C     -------
C
C     The bit-map contains 1 where valid data exists and 0
C     where data is missing. The corresponding data array
C     contains valid data and the missing data indicator value.
C
C     Externals.
C     ----------
C
C     INXBIT
C
C     Reference.
C     ----------
C
C     See routine GRIBEX.
C
C     Comments.
C     ---------
C
C     None.
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      09:07:92
C
C     Modifications.
C     --------------
C
C     J. Hennessy      ECMWF      19:10:92
C     Maximum sizes increased to allow for packing of
C     2D spectra.
C     Number of data values 120*61*12*25
C
C     J.D.Chambers     ECMWF      09:03:94
C     Maximum size for bitmap array increased to 132000
C     to allow for products 0.5 * 0.5 degrees, whole
C     hemisphere (N or S).
C
C     J.D.Chambers     ECMWF      24:05:95
C     Handle bitmap in strips to avoid problem with very large products
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J.D.Chambers     ECMWF        April 1998
C     Remove dynamic allocation of work array
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C     Strips size (JPWORK) enlarged to 10000.
C
C----<
C     ------------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
C
      INTEGER JPWORK
      PARAMETER (JPWORK=10000)
C
C     Subroutine arguments
C
      INTEGER KBITS, KGRIB, KLENG, KMAP2, KNUM, KPR, KRET, KSIZE
      DIMENSION KGRIB(*)
      REAL PMISS, PSEC4
      DIMENSION PSEC4(*)
      CHARACTER*(*) HFUNC
C
C     Local variables
C
      INTEGER NEXTIN, NXTOUT, IPT, ITEMP
      INTEGER JLOOP, JLOOPI
      INTEGER LENGTH
      REAL VALUE
C
C     Work array.
C
      INTEGER IMAP2
      DIMENSION IMAP2(JPWORK)
C
C     ------------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'INSMP2: Section 1.'
        WRITE(GRPRSM,*) '         Input values used -'
        WRITE(GRPRSM,*) '         HFUNC  = ', HFUNC
        WRITE(GRPRSM,*) '         KSIZE  = ', KSIZE
        WRITE(GRPRSM,*) '         KNUM   = ', KNUM 
      ENDIF
C
C     Reset return code to
      KRET  = 0
C
C     ------------------------------------------------------------------
C*    Section 2 .  Check input parameters.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
      IF( KPR.GE.1 ) WRITE(GRPRSM,*) 'INSMP2: Section 2.'
C
C*    Check function requested.
C
      IF( (HFUNC.NE.'C').AND.(HFUNC.NE.'M') ) THEN
        WRITE(GRPRSM,*) 'INSMP2: Invalid function requested - ', HFUNC
        KRET = 3
        GO TO 900
      ENDIF
C
      LENGTH = KNUM
C
C     Check there is enough space for packing secondary bitmap
C
      IF( LENGTH.GT.JPWORK ) THEN
        WRITE(GRPRSM,*) 'INSMP2: Number of values at each grid point'
        WRITE(GRPRSM,*)
     X    'INSMP2: (KNUM) is greater than allowed (JPWORK).'
        KRET = 4
        GOTO 900
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 3 . Generation of bit-map.
C    -------------------------------------------------------------------
C
  300 CONTINUE
C
      IF( KPR.GE.1 ) WRITE(GRPRSM,*) 'INSMP2: Section 3.'
C
      NEXTIN = 0
      NXTOUT = 0
C
C*    Generate the bit-map in accordance with the missing data
C     values.
C
      DO 320 JLOOP = 1, KSIZE, KNUM
        IPT = 0
C
        DO 310 JLOOPI = 1, KNUM
C
          IF (PSEC4(JLOOPI+NEXTIN).EQ.PMISS) THEN
C
C           If missing data indicator, set secondary bitmap flag to 0
C
            IMAP2(IPT+JLOOPI) = 0
C
          ELSE
C
C           If not missing, set secondary bitmap flag to 1 and
C           move data forwards in data array.
C
            IMAP2(IPT+JLOOPI) = 1
            NXTOUT            = NXTOUT + 1
            PSEC4(NXTOUT)     = PSEC4(NEXTIN+JLOOPI)
          ENDIF
  310   CONTINUE
        IPT = IPT + KNUM
C
C*      Insert bit-map in GRIB coded data.
C
        CALL INXBIT(KGRIB,KLENG,KMAP2,IMAP2,KNUM,KBITS,1,'C',KRET)
        IF (KRET.NE.0) THEN
          WRITE(GRPRSM,9003)
          KRET = 1
          GO TO 900
        ENDIF
C
        NEXTIN = NEXTIN + KNUM
C
  320 CONTINUE
C
C*    Bit map must be a multiple of 8 bits.
C
      ITEMP = (KMAP2+7) / 8
      ITEMP = ITEMP * 8
      IF (ITEMP.NE.KMAP2) THEN
        IMAP2(1) = 0
        ITEMP = ITEMP - KMAP2
        CALL INXBIT(KGRIB,KLENG,KMAP2,IMAP2,1,KBITS,ITEMP,'C',KRET)
        IF (KRET.NE.0) THEN
          WRITE(GRPRSM,9003)
          KRET = 1
          GO TO 900
        ENDIF
      ENDIF
C
C     Is a fixed length message required, even though a bit map is used?
C
      IF (HFUNC.EQ.'M') THEN
C
C       The otherwise unused part of the array is set to a genuine data
C       value so that extraction of minimum and maximum values remain
C       correct. Number of data values includes these padding values.
C
        VALUE = PSEC4(1)
        DO 330 JLOOP = NXTOUT+1, KSIZE
          PSEC4(JLOOP) = VALUE
  330   CONTINUE
C
      ELSE
C
C       Return number of points with values (ie excluding points with
C       missing data values).
C
        KSIZE = NXTOUT
C
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 9 . Return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF (KPR.GE.1) THEN
        WRITE(GRPRSM,*) 'INSMP2: Section 9.'
        WRITE(GRPRSM,*) '         Output values set -'
        WRITE(GRPRSM,*) 'INSMP2: KSIZE = ', KSIZE
      ENDIF
C
      RETURN
C
 9003 FORMAT (1H ,'INSMP2: Error reported by routine INXBIT.')
C
      END
