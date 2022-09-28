C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE INSMP1(KGRIB,KLENG,KMAP1,KSECM,PSEC4,KSIZE,KBITS,
     X                  PMISS,HFUNC,KNUM,KPR,KRET)
C
C---->
C**** INSMP1 - Insertion of primary bit map for routine GRIBEX.
C
C     Purpose.
C     --------
C
C     Generates a primary bit-map (Section 3 bit-map) and
C     inserts in array of GRIB coded data.
C
C**   Interface.
C     ----------
C
C     CALL INSMP1(KGRIB,KLENG,KMAP1,KSECM,PSEC4,KSIZE,KBITS,
C    X            PMISS,HFUNC,KNUM,KPR,KRET)
C
C     Input Parameters.
C     -----------------
C
C     KGRIB      - Array into which data is being packed in GRIB code.
C
C     KLENG      - Length of this array.
C
C     KMAP1      - Bit-pointer to start of primary bit-map
C                  (Section 3 bitmap) in array KGRIB.
C
C     KSECM      - Indicator of use of secondary bitmap
C                  (Section 4 bitmap) in array KGRIB.
C                  0 , No secondary bit-map.
C                  32, Secondary bit-map to be added later.
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
C     HFUNC      - 'C' , GRIB data being packed normally.
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
C     PSEC4      - Array of data values to be packed in
C                  GRIB code. Changes to contents depend
C                  on function performed eg
C
C                  Rn = Real data value at point n.
C                  M  = Missing data indicator.
C                  On input PSEC4 is R1 R2 M M  M R6
C
C                  Output with function 'C', single value per point:
C                    Bit-map is 1  1  0  0  0  1
C                    PSEC4 is   R1 R2 R6 M  M  M
C                                      ^
C                                      |
C                                    KSIZE
C
C                  Output with function 'C' , matrix (2x1) of values
C                  per point:
C                    Bit-map is 1  0  1
C                    PSEC4 is   R1 R2 M  R6 M  M  M
C                                         ^
C                                         |
C                                       KSIZE
C
C                  Output with function 'M' with no secondary bit-maps,
C                  single value:
C                    Bit-map is 1  1  0  0  0  1
C                    PSEC4 is   R1 R2 R6 R1 R1 R1
C                                              ^
C                                              |
C                                            KSIZE
C
C                  Output with function 'M' with secondary bit-maps,
C                  (2x1 matrix)
C                    Bit-map is 1  1  1
C                    PSEC4 is   R1 R2 M  M  M  R6
C                                              ^
C                                              |
C                                            KSIZE
C
C     KSIZE      - The number of data values remaining in
C                  array PSEC4 to be packed.
C
C     KRET       - Return code.
C                  0 , No error encountered.
C                  1 , Error in routine INXBIT.
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
C     Maximum sizes increased to allow for packing of 2D spectra.
C     Number of data values 120*61*12*25
C
C     J.D.Chambers     ECMWF      09:03:94
C     Maximum size for bitmap array increased to 132000
C     to allow for products 0.5 * 0.5 degrees, whole
C     hemisphere (N or S).
C
C     J.D.Chambers     ECMWF      13:07:94
C     Maximum size for bitmap array increased to 140000
C     to allow for N160 products whole globe
C
C     J.D.Chambers     ECMWF      24:05:95
C     Handle bitmap in strips to avoid problem with very large products
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
C     Subroutine arguents
C
      INTEGER KGRIB, KLENG, KMAP1, KSECM, KSIZE, KBITS, KNUM, KPR, KRET
      DIMENSION KGRIB(*)
      REAL PSEC4, PMISS
      DIMENSION PSEC4(*)
      CHARACTER*(*) HFUNC
C
C     Local variables
C
      INTEGER IMAP1, IMISS, NEXTIN, NXTOUT, INOBT, IPT
      DIMENSION IMAP1(JPWORK)
      INTEGER JLOOP, JLOOPI, NEXT, IMANY
      REAL VALUE
C
C     ------------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF (KPR.GE.1) THEN
        WRITE(GRPRSM,*) 'INSMP1: Section 1.'
        WRITE(GRPRSM,*) '         Input values used -'
        WRITE(GRPRSM,*) ' KLENG  = ', KLENG
        WRITE(GRPRSM,*) ' KMAP1  = ', KMAP1
        WRITE(GRPRSM,*) ' KSECM  = ', KSECM
        WRITE(GRPRSM,*) ' KSIZE  = ', KSIZE
        WRITE(GRPRSM,*) ' KBITS  = ', KBITS
        WRITE(GRPRSM,*) ' PMISS  = ', PMISS
        WRITE(GRPRSM,*) ' HFUNC  = ', HFUNC
        WRITE(GRPRSM,*) ' KNUM   = ', KNUM 
      ENDIF
C
C     Reset return code to 0.
C
      KRET = 0
C
C     Check function requested.
C
      IF( (HFUNC.NE.'C') .AND. (HFUNC.NE.'M') ) THEN
        WRITE(GRPRSM,*) 'INSMP1: Invalid function requested - ', HFUNC
        KRET = 3
        GO TO 900
      ENDIF
C
C*    Set number of bits in bitmap.
C
      INOBT = KSIZE / KNUM
C
C     ------------------------------------------------------------------
C*    Section 2 . Handle fixed length records with secondary bit-maps.
C    -------------------------------------------------------------------
C
  200 CONTINUE
C
C     If fixed length records with secondary bit-maps are required,
C     insert a primary bit-map of all 1 bits and finish.
C
      IF( (HFUNC.EQ.'M') .AND. (KSECM.EQ.32) ) THEN
C
        IF( KPR.GE.1 ) WRITE(GRPRSM,*) 'INSMP1: Section 2.'
C
        DO 210 JLOOP = 1,JPWORK
          IMAP1(JLOOP)   = 1
  210   CONTINUE
C
        NEXT = 0
C
  215   CONTINUE
C
C       Pack the primary bitmap in strips of JPWORK values
C
        NEXT  = NEXT + JPWORK
        IMANY = JPWORK
C
C       Check whether end of primary bitmap reached.
C       Adjust if less than JPWORK values left to process
C
        IF( NEXT.GT.INOBT ) IMANY = INOBT + JPWORK - NEXT
C
C       Pack next strip of primary bitmap values.
C
        CALL INXBIT(KGRIB,KLENG,KMAP1,IMAP1,IMANY,KBITS,1,'C',KRET)
        IF( KRET.NE.0 ) THEN
          WRITE(GRPRSM,9003)
          KRET = 1
          GO TO 900
        ENDIF
C
C       Loop back if more strips of primary bitmap to process
C
        IF( NEXT.LT.INOBT ) GOTO 215
C
        GO TO 900
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 3 . Generate the primary bitmap.
C    -------------------------------------------------------------------
C
  300 CONTINUE
C
      IF( KPR.GE.1 ) WRITE(GRPRSM,*) 'INSMP1: Section 3.'
C
      NEXTIN = 0
      NXTOUT = 0
C
      NEXT = 0
C
C     Pack the primary bitmap in strips of JPWORK values
C
  305 CONTINUE
C
      NEXT  = NEXT + JPWORK
      IMANY = JPWORK
C
C     Check whether the end of primary bitmap has been reached.
C     Adjust if less than JPWORK values left to process.
C
      IF( NEXT.GT.INOBT ) IMANY = INOBT + JPWORK - NEXT
C
      IF (KPR.GE.1) THEN
          WRITE(GRPRSM,*) 'INSMP1: NEXT = ', NEXT
          WRITE(GRPRSM,*) 'INSMP1: IMANY = ', IMANY
          WRITE(GRPRSM,*) 'INSMP1: INOBT = ', INOBT
      ENDIF
C
C*    Reset bit-map to all 0 bits.
C
      DO 310 JLOOP = 1, IMANY
        IMAP1(JLOOP) = 0
  310 CONTINUE
C
C     Set a bit to 1 for every KNUM values which contain at
C     least 1 real data value.
C
      IPT = 0
C
      IF( KNUM.EQ.1 ) THEN
!OCL NOVREC
        DO 320 JLOOP = 1, IMANY
          IF (PSEC4(NEXTIN+JLOOP).NE.PMISS) THEN
            IMAP1(IPT+JLOOP) = 1
            NXTOUT           = NXTOUT + 1
            PSEC4(NXTOUT)    = PSEC4(NEXTIN+JLOOP)
          ENDIF
  320   CONTINUE
C
        IPT    = IPT + IMANY
        NEXTIN = NEXTIN + IMANY
C
      ELSE
C
        DO 350 JLOOP = 1, IMANY
C
          IMISS = 0
          DO 330 JLOOPI = 1, KNUM
            IF (PSEC4(NEXTIN+JLOOPI).EQ.PMISS) IMISS = IMISS + 1
  330     CONTINUE
C
C         If point not completely missing, move data forwards in data 
C         array. If no real data value found, the corresponding bit is
C         already 0.
C
          IF( IMISS.NE.KNUM ) THEN
            IMAP1(IPT+JLOOP) = 1
            DO 340 JLOOPI = 1, KNUM
              PSEC4(NXTOUT+JLOOPI) = PSEC4(NEXTIN+JLOOPI)
  340       CONTINUE
            NXTOUT = NXTOUT + KNUM
          ENDIF
C
          NEXTIN = NEXTIN + KNUM
C
  350   CONTINUE
        IPT = IPT + IMANY
C
      ENDIF
C
C     Pack next strip of primary bitmap values in GRIB coded data.
C
      CALL INXBIT(KGRIB,KLENG,KMAP1,IMAP1,IMANY,KBITS,1,'C',KRET)
      IF( KRET.NE.0 ) THEN
        WRITE(GRPRSM,9003)
        KRET = 1
        GO TO 900
      ENDIF
C
C     Loop back if more strips of primary bitmap to process
C
      IF( NEXT.LT.INOBT ) GOTO 305
C
C     Is a fixed length message required, even though a bit map is used?
C
      IF( HFUNC.EQ.'M' ) THEN
C
C       The otherwise unused part of the array is set to a genuine data
C       value so that extraction of minimum and maximum values remain
C       correct. Number of data values includes these padding values.
C
        VALUE = PSEC4(1)
        DO 360 JLOOP = NXTOUT+1, KSIZE
          PSEC4(JLOOP) = VALUE
  360   CONTINUE
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
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'INSMP1: Section 9.'
        WRITE(GRPRSM,*) '         Output values set -'
        WRITE(GRPRSM,*) 'INSMP1: KSIZE = ', KSIZE
      ENDIF
      RETURN
C
 9003 FORMAT (1H ,'INSMP1: Error reported by routine INXBIT.')
C
      END
