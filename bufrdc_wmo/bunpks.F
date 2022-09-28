C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE BUNPKS(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)
C
C**** *BUNPKS*
C
C
C     PURPOSE.
C     --------
C          PURPOSE OF THIS ROUTINE IS TO UNPACK BIT STRING OF
C     KSIZE BITS, STARTED AT WORD KWPT OF ARRAY KSOURC AFTER
C     SKIPPING KBPT BITS. RESULT IS PUT INTO KDEST. AT THE END
C     POINTERS KWPT AND KBPT ARE ADJUSTED.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUNPKS(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)*
C
C        INPUT :
C            *KBPW*      - NUMBER OF BITS PER COMPUTER WORD
C            *KSOURC*    - SOURCE (CONTINUOUS BIT STRING OF
C                          ARBITRARY LENGTH)
C            *KWPT*      - WORD POINTER
C            *KBPT*      - BIT POINTER
C            *KSIZE*     - NUMBER OF BITS TO BE EXTRACTED
C            *KSKIPB*    - NUMBER OF BITS TO SKIP BETWEEN ELEMENTS
C            *K*         - ITERATION
C        OUTPUT:
C            *KDEST*     - DESTINATION
C            *KERR*      - RETURN ERROR CODE
C
C     METHOD.
C     -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C          GBYTES     - UNPACK BIT PATTERN
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION KSOURC(*),KDEST(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KERR.NE.0) RETURN
C
      IF(KSIZE.GT.KBPW) THEN
         KERR=13
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL GBYTES(KSOURC(KWPT),KDEST,KBPT,KSIZE,KSKIPB,K)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + K*(KSIZE+KSKIPB)
C
      IF(KBPT.GE.KBPW) THEN
         IW  = KBPT/ KBPW
         KBPT= KBPT - IW * KBPW
         KWPT= KWPT +IW
      END IF
C
      RETURN
      END
