C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GBYTE  (SOURCE,DEST,IOFSET,IBYTSZ)
C
C---->
C**** GBYTE  - Extract a single bit field. Cray routine.
C
C*    FUNCTION: GET A SINGLE BIT FIELD FROM SOURCE INTO DEST
C*
C*    INPUT   : SOURCE(1)= WORD CONTAINING START OF BIT FIELD
C*              DEST     = TARGET WORD
C*              IOFSET   = OFFSET IN BITS FOR START OF THE FIELD
C*              IBYTSZ   = LENGTH OF FIELD IN BITS
C*
C*    OUTPUT  : SOURCE,IOFSET,IBYTSZ UNCHANGED
C*              DEST CONTAINS FIELD RIGHT JUSTIFIED
C*
C*    AUTHOR  : M.MIQUEU   08/1981 (REWRITTEN FROM J.MARTELLET'S)
C*
C----<
      PARAMETER(NBPW=64)
      INTEGER SOURCE(1),DEST
      INTEGER SH1
      SH1=IOFSET+IBYTSZ-NBPW

      IF(SH1.GT.0) GO TO 2


C     BYTES DO NOT SPAN WORDS


      SH1=NBPW+SH1


      DEST=AND(
     1          SHIFT(SOURCE(1),SH1),
     2          SHIFT(MASK(IBYTSZ),IBYTSZ)
     3        )

      RETURN

C     BYTE SPANS WORDS

2     CONTINUE


      DEST=OR(
     1        SHIFT(
     2              AND(SOURCE(1),COMPL(MASK(IOFSET)))
     3              ,SH1),
     4        SHIFT(
     5              AND(SOURCE(2),MASK(SH1))
     6              ,SH1)
     7       )



      RETURN
      END
