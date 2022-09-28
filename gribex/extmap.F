C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE EXTMAP (KGRIB,KLENG,KMAP1,KMAP2,KVALS,PSEC4,KSIZE,
     X                   KBITS,KSBMAP,PMISS,KNUM,KPR,KRET,KNOMIS)
C
C---->
C**** EXTMAP - Extraction of bit maps for routine GRIBEX.
C
C     Purpose.
C     --------
C
C     Extracts bit-maps (primary and secondary,if present)
C     from an array of GRIB coded data and inserts the
C     missing data value indicator in appropriate places in
C     the array of already unpacked data values.
C
C**   Interface.
C     ----------
C
C     CALL EXTMAP (KGRIB,KLENG,KMAP1,KMAP2,KVALS,PSEC4,KSIZE,KBITS,
C    X             KSBMAP,PMISS,KNUM,KPR,KRET,KNOMIS)
C
C
C     Input Parameters.
C     -----------------
C
C     KGRIB      - Array from which data is being unpacked
C                  from GRIB code.
C
C     KLENG      - Length of this array.
C
C     KMAP1      - Bit-pointer to start of primary bit-map
C                  (Section 3 bitmap) in array KGRIB.
C
C     KMAP2      - Bit-pointer to start of secondary bitmap
C                  (Section 4 bitmap) in array KGRIB.
C                  Negative value if no secondary bit maps.
C
C     KVALS      - Number of unpacked values in PSEC4 on entry
C
C     PSEC4      - Array of data values unpacked from GRIB code.
C
C     KSIZE      - Number of bits in the primary bit map.
C
C     KBITS      - Number of bits in computer word.
C
C     PMISS      - Value to be used to indicate missing data
C                  in array PSEC4.
C
C     KNUM       - Number of values at each grid point. This
C                  is normally 1 except when a matrix of value
C                  is represented.
C
C     KPR        - Debug print switch.
C                  0  , No printout.
C                  >0 , Debug printout.
C
C     Output Parameters.
C     ------------------
C
C     KSBMAP     - Bit-map flag.
C                  -2 , All bits in the bit-map set to 1.
C                       There is no missing data.
C                  -4 , Some points have no data. User
C                       supplied value for missing data
C                       indicator in appropriate places in
C                       the array PSEC4.
C
C     KRET       - Return code.
C                  0 , No error encountered.
C                  1 , Error in routine INXBIT.
C                  4 , Number of values at each grid point (KNUM)
C                      is greater than allowed (JPWORK).
C
C     KNOMIS     - Number of non-missing values in PSEC4 entry.
C
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
C     INXBIT   - Extract values from bit-stream
C
C     Reference.
C     ----------
C
C     See routine GRIBEX.
C
C     Comments.
C     ---------
C
C     Routine contains Sections 0 to 3 and Section 9.
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      10:07:92
C
C     Modifications.
C     --------------
C
C     J. Hennessy      ECMWF      29:10:92
C     Maximum sizes increased to allow for packing of
C     2D spectra.
C     Number of data values 120*61*12*25
C
C     J.D.Chambers     ECMWF      09:03:94
C     Maximum size for bitmap array increased to 132000
C     to allow for products 0.5 * 0.5 degrees, whole
C     hemisphere (N or S).
C
C     J.D.Chambers     ECMWF      18:04:94
C     Fix setting of missing values according to bitmap 
C
C     J.D.Chambers     ECMWF      19:07:94
C     Maximum size for bitmap array increased to 140000
C     for reduced gaussian n160.
C
C     J.D.Chambers     ECMWF      23.05.95
C     Use dynamic allocation for secondary bitmap
C     Process primary bitmap in small pieces.  Use PSEC4
C     for expanding data using bitmaps.
C
C     J.D.Chambers     ECMWF      24.10.96
C     Use fixed allocation for secondary bitmap
C     Process primary bitmap in small pieces.  Use PSEC4
C
C     J. Clochard, METEO FRANCE, for ECMWF - January 1998.
C     Dummy-argument KNOMIS added.
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
      INTEGER KBITS, KGRIB, KLENG, KMAP1, KMAP2, KNUM, KVALS, KNOMIS
      DIMENSION KGRIB(*)
      INTEGER KPR, KRET, KSBMAP, KSIZE
      REAL PMISS, PSEC4
      DIMENSION PSEC4(*)
C
C     Local variables
C
      INTEGER ICURR, IPT, LENGTH
      INTEGER JLOOP, JLOOPI, NEXT, IMANY, NABITS, NBITS, NVALS, MYMAP1
      LOGICAL LMOPT
C
      INTEGER IMAP1
      DIMENSION IMAP1(JPWORK)
C
C     Work array.
C
      INTEGER IMAP2
      DIMENSION IMAP2(JPWORK)
C
C
C     ------------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'EXTMAP: Section 1.'
        WRITE(GRPRSM,*) 'EXTMAP:   Input values used -'
        WRITE(GRPRSM,*) 'EXTMAP: KSIZE,KNUM = ', KSIZE,KNUM
        WRITE(GRPRSM,*) 'EXTMAP: KLENG,KMAP1,KMAP2,KBITS, PMISS = '
        WRITE(GRPRSM,*) KLENG,KMAP1,KMAP2,KBITS, PMISS
        WRITE(GRPRSM,*) 'EXTMAP: KVALS = ', KVALS
      ENDIF
C
C     Reset return code to 0.
C
      KRET  = 0
      KNOMIS = 0
      NBITS = 0
      NABITS = 0
      LENGTH = KNUM
C
C     Check there is enough space for unpacking secondary bitmap
C
      IF( LENGTH.GT.JPWORK ) THEN
        WRITE(GRPRSM,*) 'EXTMAP: Number of values at each grid point'
        WRITE(GRPRSM,*)
     X    'EXTMAP: (KNUM) is greater than allowed (JPWORK).'
        KRET = 4
        GOTO 900
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 2 .  Handle 'M' option case
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
C     If the original field has been packed with the 'M' option,
C     have to find out how many true values there are in the field.
C
      LENGTH = KSIZE * KNUM
      LMOPT = (KVALS.EQ.LENGTH)
C
      IF( LMOPT ) THEN
C
C       Count the number of bits in the primary bitmap
C
        NEXT = 0
        NVALS = 0
        MYMAP1 = KMAP1
C
  210   CONTINUE
C
C       Unpack the primary bitmap in strips of JPWORK values
C
        NEXT = NEXT + JPWORK
        IMANY = JPWORK
C
C       Check whether end of primary bitmap reached.
C       Adjust if less than JPWORK values left to process
C
        IF( NEXT.GT.KSIZE ) IMANY = KSIZE + JPWORK - NEXT
C
C       Unpack next strip of primary bitmap values.
C
        CALL INXBIT(KGRIB,KLENG,MYMAP1,IMAP1,IMANY,KBITS,1,'D',KRET)
        IF( KRET.NE.0 ) THEN
          WRITE(GRPRSM,*) 'EXTMAP: Error reported by routine INXBIT.'
          KRET = 1
          GO TO 900
        ENDIF
C
C       Work through the strip of primary bitmap values counting points
C       which are not missing.
C
        DO 220 JLOOP = 1,IMANY
          IF( IMAP1(JLOOP).EQ.1 ) NVALS = NVALS + 1
  220   CONTINUE
C
C       Loop back if more strips of primary bitmap to process
C
        IF( NEXT.LT.KSIZE ) GOTO 210
C
      ELSE
C
        NVALS = KVALS
C
      ENDIF
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'EXTMAP: NVALS = ', NVALS
        WRITE(GRPRSM,*) 'EXTMAP: KVALS = ', KVALS
        WRITE(GRPRSM,*) 'EXTMAP: LENGTH = ', LENGTH
      ENDIF
C
C     Move input field to far end of input array
C
      IF( NVALS.LT.LENGTH ) THEN
        ICURR = NVALS+1
!OCL NOVREC
        DO 230 JLOOP = LENGTH, LENGTH - NVALS + 1, -1
          ICURR = ICURR - 1
          PSEC4(JLOOP) = PSEC4(ICURR)
  230   CONTINUE
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 3 . Decoding of bit-map and data.
C     ------------------------------------------------------------------
C
  300 CONTINUE
C
      IF( KPR.GE.1 ) WRITE(GRPRSM,*) 'EXTMAP: Section 3.'
C
C*    Set unoccupied values in array to missing data indicator.
C
      ICURR = LENGTH - NVALS
      DO 310 JLOOP = 1, ICURR
        PSEC4(JLOOP) = PMISS
  310 CONTINUE
C
C*    Copy data to output array or insert missing data value
C     in accordance with the bit map values.
C
      ICURR = LENGTH - NVALS
      IPT = 1
      NEXT = 0
C
  320 CONTINUE
C
C     The primary bitmap is unpacked and used in strips of JPWORK values
C
      NEXT = NEXT + JPWORK
      IMANY = JPWORK
C
C     Check whether end of primary bitmap reached.
C     Adjust if less than JPWORK values left to process
C
      IF( NEXT.GT.KSIZE ) IMANY = KSIZE + JPWORK - NEXT
C
C     Unpack next strip of primary bitmap values.
C
      CALL INXBIT(KGRIB,KLENG,KMAP1,IMAP1,IMANY,KBITS,1,'D',KRET)
      IF( KRET.NE.0 ) THEN
        WRITE(GRPRSM,*) 'EXTMAP: Error reported by routine INXBIT.'
        KRET = 1
        GO TO 900
      ENDIF
C
C     Work through the strip of primary bitmap values.
C
      IF(KMAP2.LE.0.AND.KNUM.EQ.1) THEN
        IMAP2(1) = 1
!OCL NOVREC
        DO 325 JLOOP = 1,IMANY
          IF( IMAP1(JLOOP).EQ.1 ) THEN
            ICURR      = ICURR + 1
            PSEC4(IPT) = PSEC4(ICURR)
            NABITS     = NABITS + 1
          ELSE
            PSEC4(IPT) = PMISS
            NBITS      = NBITS + 1
          ENDIF
          IPT = IPT + 1
 325    CONTINUE
C
      ELSE
C
        DO 380 JLOOP = 1,IMANY
C
C         If primary bitmap indicates there are values at the point ..
C
          IF( IMAP1(JLOOP).EQ.1 ) THEN
C
C           .. read secondary bitmap, if any.
C
            IF( KMAP2.GT.0 ) THEN
              CALL INXBIT(KGRIB,KLENG,KMAP2,IMAP2,KNUM,KBITS,1,'D',KRET)
              IF( KRET.NE.0 ) THEN
                WRITE(GRPRSM,*)
     X            'EXTMAP: Error reported by routine INXBIT.'
                KRET = 1
                GO TO 900
              ENDIF
C
            ELSE
C
C*            If no secondary bitmaps present, set up dummy secondary
C             bitmap to indicate no missing data values.
C
              IF( KNUM.EQ.1 ) THEN
                IMAP2(1)   = 1
                ICURR      = ICURR + 1
                PSEC4(IPT) = PSEC4(ICURR)
                IPT        = IPT + 1
                NABITS     = NABITS + 1
              ELSE
                DO 330 JLOOPI = 1,KNUM
                  IMAP2(JLOOPI) = 1
  330           CONTINUE
              ENDIF
C
            ENDIF
C
C           Use the bitmap to put in data value or missing data indicator.
C
            IF( KNUM.GT.1 ) THEN
              DO 340 JLOOPI = 1,KNUM
                IF( IMAP2(JLOOPI).EQ.1 ) NABITS = NABITS + 1
  340         CONTINUE
C
              DO 350 JLOOPI = 1,KNUM
                IF( IMAP2(JLOOPI).NE.1 ) NBITS = NBITS + 1
  350         CONTINUE
C
              DO 360 JLOOPI = 1,KNUM
                IF( IMAP2(JLOOPI).EQ.1 ) THEN
                  ICURR              = ICURR + 1
                  PSEC4(IPT+JLOOPI-1) = PSEC4(ICURR)
                ELSE
                  PSEC4(IPT+JLOOPI-1) = PMISS
                ENDIF
  360         CONTINUE
              IPT = IPT + KNUM
C
            ENDIF
C
C         If primary bitmap indicates there are no values at the point ..
C
          ELSE
            IF( KNUM.EQ.1 ) THEN
              PSEC4(IPT) = PMISS
            ELSE
              DO 370 JLOOPI = 1,KNUM
                PSEC4(IPT+JLOOPI-1) = PMISS
  370         CONTINUE
            ENDIF
            IPT   = IPT + KNUM
            NBITS = NBITS + KNUM
          ENDIF
C
  380   CONTINUE
C
      ENDIF
C
C     Loop back if more strips of primary bitmap to process
C
      IF( NEXT.LT.KSIZE ) GOTO 320
C
C*    Set bit-map flag.
C
      IF( NBITS.GT.0 ) THEN
        KSBMAP = -4
      ELSE
        KSBMAP = -2
      ENDIF
C
C*    Return effective number of non-missing values.
C
      KNOMIS=NVALS
C
C     ------------------------------------------------------------------
C*    Section 9 . Return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'EXTMAP: Section 9.'
        WRITE(GRPRSM,*) 'EXTMAP: NBITS = ', NBITS
        WRITE(GRPRSM,*) 'EXTMAP: NABITS = ', NABITS
        WRITE(GRPRSM,*) 'EXTMAP: NVALS = ', NVALS
        WRITE(GRPRSM,*) 'EXTMAP: Output values set -'
        WRITE(GRPRSM,*) 'EXTMAP: KNOMIS = ', KNOMIS
        WRITE(GRPRSM,*) 'EXTMAP: KSIZE = ', KSIZE
        WRITE(GRPRSM,*) 'EXTMAP: KSBMAP = ', KSBMAP
      ENDIF
C
      RETURN
      END
