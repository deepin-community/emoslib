C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE SBYTES (D,S,ISKIP1,IBSIZ,ISKIP2,NBYTES,KWOFF)
C
C---->
C**** SBYTES - Insert a number of bit fields. Cray routine.
C
C REVERSES THE ACTION OF GBYTES, TAKING FIELDS FROM S AND
C INSERTING THEM INTO A BIT STRING IN D. SEE GBYTES.
C AUTHOR D. ROBERTSON  AUG,1981
C
C----<
      PARAMETER(NBPW=64)
      DIMENSION D(2) , S(NBYTES)
      INTEGER SH1,SH2,SH3
      IGNORE = 0
#ifdef CRAY
      IF(NUMARG().GT.6) IGNORE = KWOFF
#else
      IF(NUMARG(DUMMY).GT.6) IGNORE = KWOFF
#endif
      ID=1+ISKIP1/(NBPW-IGNORE)
      ISKIP = MOD(ISKIP1,NBPW-IGNORE) + IGNORE
      ISTEP = ISKIP2+IBSIZ
      DO 75 IBYTE = 1 , NBYTES
C WITH THE STARTING WORD AND BIT POSITION KNOWN, THE
C DESIRED INSERTION CAN BE DONE BY
C**      CALL SBYTE(D(ID),S(IBYTE),ISKIP,IBSIZ)
C BUT THE CODE IS SHORT ENOUGH TO GO IN-LINE.
         SH1 = ISKIP+IBSIZ
         IF(SH1.GT.NBPW) GO TO 50
         SH2 = NBPW-SH1
         IF(SH2.LT.0) SH2 = NBPW-SH2
C BYTE GOES INTO 1 WORD OF D.
         D(ID) = SHIFT(OR(AND(SHIFT(D(ID),SH1),MASK(NBPW-IBSIZ)),
     1                   AND(S(IBYTE),SHIFT(MASK(IBSIZ),IBSIZ))),SH2)
         GO TO 65
   50    CONTINUE
C BYTE GOES INTO 2 WORDS OF D.
         SH3 = 2*NBPW-SH1
         D(ID)=OR(AND(D(ID),MASK(ISKIP)),
     1               AND(SHIFT(S(IBYTE),SH3),COMPL(MASK(ISKIP))))
         D(ID+1)=OR(AND(D(ID+1),SHIFT(COMPL(MASK(SH1-NBPW)),NBPW-IGNORE)
     1                 ),
     2              SHIFT(AND(S(IBYTE),COMPL(MASK(SH3))),SH3-IGNORE))
   65    CONTINUE
C UPDATE STARTING WORD AND BIT POSITION
         ISKIP = ISKIP+ISTEP
         IF(ISKIP.LT.NBPW) GO TO 75
         ISKIP = ISKIP - NBPW
         ID = ID+1+ISKIP/(NBPW-IGNORE)
         ISKIP = MOD(ISKIP,NBPW-IGNORE) + IGNORE
   75    CONTINUE
      RETURN
      END
