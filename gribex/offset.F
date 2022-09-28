C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE OFFSET  (IOFF,NVAL,IWORD,IBYTE,NBIT,ILEN,IERR)
C
C
C
C
C
C---->
C*
C*    NAME      : OFFSET
C:
C*    FUNCTION  : CALCULATES THE WORD AND BIT OFFSET OF THE START
C*                OF THE NEXT BIT-FIELD IN AN ARRAY OF BINARY
C*                DATA, FOLLOWING THE INSERTION/EXTRACTION OF ONE
C*                OR MORE CONSECUTIVE BIT-FIELDS.
C*
C*    INPUT     : IOFF  - BIT OFFSET AT WHICH LAST INSERTION OR
C*                        EXTRACTION STARTED.
C*                NVAL  - NUMBER OF FIELDS INSERTED OR EXTRACTED IN
C*                        LAST OPERATION.
C*                IWORD - WORD NUMBER OF ARRAY AT WHICH LAST OPERATION
C*                        STARTED.
C*                IBYTE - LENGTH, IN BITS, OF LAST FIELD(S) INSERTED
C*                        OR EXTRACTED.
C*                NBIT  - NUMBER OF BITS IN COMPUTER WORD.
C*                ILEN  - NUMBER OF WORDS IN ARRAY.
C*
C*    OUTPUT    : IOFF  - BIT OFFSET AT WHICH NEXT INSERTION OR
C*                        EXTRACTION STARTS.
C*                NVAL  - UNCHANGED.
C*                IWORD - WORD NUMBER OF ARRAY AT WHICH NEXT OPERATION
C*                        STARTS.
C*                IBYTE - UNCHANGED.
C*                NBIT  - UNCHANGED.
C*                ILEN  - UNCHANGED.
C*
C*                IERR  - EQUAL 0 IF NO ERROR.
C*                        EQUAL -2 IF NEXT OPERATION IS OUTSIDE ARRAY
C*                        BOUNDS.
C*
C*    GENERAL   : OFFSET CALLS -----
C*
C*    AUTHOR    : J.HENNESSY  15.4.85
C*
C*    MODIFIED  : J.HENNESSY  28.11.85
C*
C
C     ---------------------------------------------------------------
C----<
C
C
C
C     CLEAR ERROR INDICATOR
C
      IERR = 0
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
C     CALCULATE NEXT WORD AND BIT POSITIONS.
C
      IBITL = NVAL  * IBYTE
      INTER = IBITL / NBIT
      IOFFS = IBITL - INTER * NBIT
      IWORD = IWORD + INTER
      IOFF  = IOFF  + IOFFS
C
      IF (IOFF.GE.NBIT)
     C   THEN
             IWORD = IWORD + 1
             IOFF  = IOFF  - NBIT
         END IF
C
C
C
C
C
C
C
C
C     CHECK THAT NEXT WORD TO BE ACCESSED LIES WITHIN THE ARRAY BOUNDS.
C
      IF (IWORD.GT.ILEN)
     C   THEN
             IERR = -2
             WRITE (*,9001) IWORD,ILEN
 9001        FORMAT (1H ,'WORD ',I8,' IS OUTSIDE ARRAY BOUNDS ',I8)
         ENDIF
C
C
      RETURN
C
      END
