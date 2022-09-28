C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE INXMAP (KGRIB,KLENG,KMAPT,PSEC4,KSIZE,KBITS,KSBMAP,
     C                     PMISS,HFUNC,KPR,KRET)
C
C---->
C**** INXMAP - Bit map handling for routine GRIBEX.
C
C     Purpose.
C     --------
C
C           1) Extract a bit-map from an array of GRIB coded data and
C              insert missing data value in appropriate places in the
C              array of already decoded data values.
C
C           2) Generate a bit-map and insert in array of GRIB coded
C              data and remove missing data values from the array of
C              values being encoded in GRIB code.
C
C**   Interface.
C     ----------
C
C           CALL INXMAP (KGRIB,KLENG,KMAPT,PSEC4,KSIZE,KBITS,KSBMAP,
C    C                     PMISS,HFUNC,KPR,KRET)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               KGRIB      - Array of GRIB data being coded/decoded.
C
C               KLENG      - Length of this array.
C
C               KMAPT      - Bit-pointer to start of bit-map in
C                            array KGRIB.
C
C               PSEC4      - Array of data values decoded or to be
C                            coded in GRIB.
C
C               KSIZE      - Size of bit-map ie number of values,
C                            including missing data values, in array
C                            PSEC4.
C
C               KBITS      - Number of bits in computer word.
C
C               PMISS      - Value indicating missing data in array
C                            PSEC4.
C
C               HFUNC      - 'C' , GRIB data being coded.
C                            'M' , GRIB data being coded in fixed length
C                                  messages.
C                            'D' , GRIB data being decoded.
C
C               KPR        - Debug print switch.
C                            0 , No printout.
C                            1 , Debug printout.
C
C               Output Parameters.
C               ------------------
C
C               KSBMAP     - Bit-map flag.
C                            Used only when decoding data.
C                            -2 , All bits in the bit-map set to 1.
C                                 There is no missing data.
C                            -4 , Some points have no data. User
C                                 supplied value for missing data
C                                 indicator in appropriate places in
C                                 the array PSEC4.
C
C               KSIZE      - When coding data, the number of real data
C                            values in array PSEC4. Not changed when
C                            decoding.
C
C               KRET       - Return code.
C                            0 , No error encountered.
C                            1 , Error in routine INXBIT.
C                            2 , Bit-map size exceeds maximum.
C                            3 , Invalid function requested.
C
C     Method.
C     -------
C
C           The bit-map contains 1 where valid data exists and 0
C           where data is missing. The corresponding data array
C           contains valid data and the missing data indicator value.
C
C     Externals.
C     ----------
C
C           INXBIT
C
C     Reference.
C     ----------
C
C           See routine GRIBEX.
C
C     Comments.
C     ---------
C
C           Routine contains Sections 0 to 4 and Section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      09:09:91
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      08:11:91
C           Parameter KMACH removed from list of input parameters.
C           Optional abort removed.
C
C940309     J.D.Chambers     ECMWF      09:03:94
C940309     Maximum size for bitmap array increased to 132000
C940309     to allow for products 0.5 * 0.5 degrees, whole
C940309     hemisphere (N or S).
C
C     ------------------------------------------------------------------
C----<
C
C
C
C
C
C
C
C
C
C
C*    Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
C
C*    Prefix conventions for variable names.
C
C     Logical      L (but not LP), global or common.
C                  O, dummy arguments.
C                  G, local variable.
C                  LP, parameter.
C     Character    C, Global or common.
C                  H, dummy arguments.
C                  Y (but not YP), local variables.
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy arguments.
C                  I, local variables.
C                  J (but not JP), loop control.
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy arguments.
C                  Z, local variables.
C                  PP, parameter.
C
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      CHARACTER*(*) HFUNC
      CHARACTER*1   YFUNC
C
      INTEGER IMAP
      INTEGER INEXT
C
      INTEGER JPMAP
C
      INTEGER J320
      INTEGER J330
      INTEGER J420
      INTEGER J430
      INTEGER J431
C
      INTEGER KBITS
      INTEGER KGRIB
      INTEGER KLENG
      INTEGER KMAPT
      INTEGER KPR
      INTEGER KRET
      INTEGER KSBMAP
      INTEGER KSIZE
C
      REAL PMISS
      REAL PSEC4
C
      REAL ZSEC4
C
C*    Maximum size of bit-map which can be handled.
C
C940309 PARAMETER (JPMAP=41472)
      PARAMETER (JPMAP=132000)
C
      DIMENSION IMAP(JPMAP)
C
      DIMENSION PSEC4(*)
      DIMENSION ZSEC4(JPMAP)
C
C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF (KPR.EQ.1)
     C   THEN
             WRITE(GRPRSM,*) 'INXMAP : Section 1.'
             WRITE(GRPRSM,*) '         Input values used -'
             WRITE(GRPRSM,9004) HFUNC
             WRITE(GRPRSM,9005) KSIZE
         ENDIF
C
C     Reset return code to
C
      KRET  = 0
C
      INEXT = 0
C
      YFUNC = HFUNC
C
C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 2 .  Check input parameters.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
      IF (KPR.EQ.1) WRITE(GRPRSM,*) 'INXMAP : Section 2.'
C
C*    Check that bit-map size does not exceed maximum permitted.
C
      IF (JPMAP.LT.KSIZE)
     C   THEN
             WRITE(GRPRSM,9001) KSIZE , JPMAP
             KRET = 2
             GO TO 900
         ENDIF

C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 3 . Decoding of bit-map and data.
C     ------------------------------------------------------------------
C
  300 CONTINUE
C
      IF (KPR.EQ.1) WRITE(GRPRSM,*) 'INXMAP : Section 3.'
C
      IF (YFUNC.EQ.'D')
     C   THEN
C
C*           Extract bit-map from GRIB coded data.
C
             CALL INXBIT (KGRIB,KLENG,KMAPT,IMAP,KSIZE,KBITS,1,
     C                     YFUNC,KRET)
C
             IF (KRET.NE.0)
     C          THEN
                    WRITE(GRPRSM,9003)
                    KRET = 1
                    GO TO 900
                ENDIF
C
C*           Copy data to temporary array and insert missing data
C            indicator in temporary array, in accordance with the
C            bit map values.
C
             DO 320 J320=1,KSIZE
                IF (IMAP(J320).EQ.0)
     C             THEN
                         ZSEC4(J320) = PMISS
                   ELSE
                       INEXT       = INEXT + 1
                       ZSEC4(J320) = PSEC4(INEXT)
                   ENDIF
  320        CONTINUE
C
C*           Set bit-map flag, and if missing data is indicated
C            transfer data to original array.
C
             IF (INEXT.NE.KSIZE)
     C          THEN
                    KSBMAP = -4
                    DO 330 J330=1,KSIZE
                       PSEC4(J330) = ZSEC4(J330)
  330               CONTINUE
                ELSE
                    KSBMAP = -2
                ENDIF
C
             GO TO 900
C
         ENDIF
C
C    -------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 4 . Generation of bit-map.
C    -------------------------------------------------------------------
C
  400 CONTINUE
C
      IF (KPR.EQ.1) WRITE(GRPRSM,*) 'INXMAP : Section 4.'
C
      IF (YFUNC.EQ.'C'.OR.YFUNC.EQ.'M')
     C   THEN
C
C*           Copy data to temporary array and remove missing data
C            indicator in temporary array, generating the bit-map
C            in accordance with the missing data values.
C
             DO 420 J420=1,KSIZE
                IF (PSEC4(J420).EQ.PMISS)
     C             THEN
                       IMAP(J420)   = 0
                   ELSE
                       IMAP(J420)   = 1
                       INEXT        = INEXT + 1
                       ZSEC4(INEXT) = PSEC4(J420)
                   ENDIF
  420        CONTINUE
C
C*           Insert bit-map in GRIB coded data.
C
             CALL INXBIT (KGRIB,KLENG,KMAPT,IMAP,KSIZE,KBITS,1,
     C                     YFUNC,KRET)
C
             IF (KRET.NE.0)
     C          THEN
                    WRITE(GRPRSM,9003)
                    KRET = 1
                    GO TO 900
                ENDIF
C
C*           If missing data is indicated transfer data to original
C            array.
C
             IF (INEXT.NE.KSIZE)
     C          THEN
                    DO 430 J430=1,INEXT
                       PSEC4(J430) = ZSEC4(J430)
  430               CONTINUE
C
                    IF (YFUNC.EQ.'M')
     C                 THEN
C
C                          Fixed length messages required, even though
C                          a bit map is used. The otherwise unused part
C                          of the array is set to a genuine data value
C                          so that extraction of minimum and maximum
C                          values remain correct. Number of data values
C                          includes these padding values.
C
                           DO 431 J431=INEXT+1,KSIZE
                              PSEC4(J431) = PSEC4(1)
  431                      CONTINUE
                       ELSE
C
C                          Return number of real values (excluding
C                          missing data values).
C
                           KSIZE = INEXT
                       ENDIF
                ENDIF
C
             GO TO 900
C
         ENDIF
C
C*    Invalid function requested.
C
      WRITE(GRPRSM,9002) YFUNC
      KRET = 3
C
C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 9 . Return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF (KPR.EQ.1)
     C   THEN
             WRITE(GRPRSM,*) 'INXMAP : Section 9.'
             WRITE(GRPRSM,*) '         Output values set -'
             WRITE(GRPRSM,9005) KSIZE
             WRITE(GRPRSM,9006) KSBMAP
         ENDIF
C
 9001 FORMAT (1H ,'INXMAP : Bit-map size is ',I6,', maximum allowed',
     C            ' is ',I6,'.')
C
 9002 FORMAT (1H ,'INXMAP : Invalid function requested - ',A1)
C
 9003 FORMAT (1H ,'INXMAP : Error reported by routine INXBIT.')
C
 9004 FORMAT (1H ,'         HFUNC  = ',A1)
C
 9005 FORMAT (1H ,'         KSIZE  = ',I6)
C
 9006 FORMAT (1H ,'         KSBMAP = ',I6)
C
      RETURN
C
      END
