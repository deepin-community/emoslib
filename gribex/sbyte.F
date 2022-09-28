C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE SBYTE  (DEST,SOURCE,IOFSET,IBYTSZ)
C
C---->
C**** SBYTE  - Insert a single bit field. Cray routine.
C
C*    FUNCTION: STORE A SINGLE BIT FIELD FROM SOURCE INTO DEST
C*
C*    INPUT   : SOURCE   = WORD CONTAINING  BIT FIELD RIGHT JUSTIFIED
C*              DEST(1)  = 1ST TARGET WORD
C*              IOFSET   = OFFSET IN BITS FOR START OF THE FIELD
C*              IBYTSZ   = LENGTH OF FIELD IN BITS ; .LE.WORD SIZE .....
C*
C*    OUTPUT  : SOURCE,IOFSET,IBYTSZ UNCHANGED
C*              DEST(1) AND EVENTUALLY DEST(2) CONTAIN FIELD
C*
C*    AUTHOR  : M.MIQUEU   08/1981 (REWRITTEN FROM J.MARTELLET'S)
C*
C----<
      PARAMETER(NBPW=64)
      INTEGER SOURCE,DEST(1)
      INTEGER SH1,SH2,SH3
      SH1=IOFSET+IBYTSZ
      IF(SH1.GT.NBPW) GO TO 2

      SH2=NBPW-SH1
      IF(SH2.LT.0) SH2=NBPW-SH2

C     BYTE  DOES NOT SPAN WORDS


      DEST(1)=SHIFT(
     1              OR(
     2                 AND(SHIFT(DEST(1),SH1),
     3                     SHIFT(COMPL(MASK(IBYTSZ)),IBYTSZ) )
     4                 ,
     5                 AND(SOURCE,
     6                     COMPL(SHIFT(COMPL(MASK(IBYTSZ)),IBYTSZ)) )
     7                 )
     8              ,SH2)



      RETURN

2     CONTINUE

C     BYTE SPANS 2 WORDS

      SH3=2*NBPW-SH1


      DEST(1)=OR(
     1           AND(DEST(1),MASK(IOFSET))
     2           ,
     3           AND(SHIFT(SOURCE,SH3) , COMPL(MASK(IOFSET)) )
     4           )

      DEST(2)=OR(
     1           AND(DEST(2) , COMPL(MASK(SH1-NBPW)) )
     2           ,
     3           SHIFT( AND(SOURCE , COMPL(MASK(SH3)) ) ,SH3)
     4           )



      RETURN
      END
