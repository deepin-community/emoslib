
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C
C     STATEMENT FUNCTIONS TO MANIPULATE BITS IN WORDS OF 64 BITS
C
C     DATA ONES/7777777777777777B/
C     DATA OOOS/0B/
C
C     1.  SINGLE BIT MANIPULATIONS
C
C     1.1 SET BIT KBIT IN WORD PW
C
      IBSET(KW,KBIT)=OR(KW,SHIFT(1B,KBIT))
C
C     2.  WORD MANIPULATIONS, BIT BY BIT
C
C     2.1 ARE WORDS PW1 AND PW2 EQUAL?
C
C      LOGICAL NLEQAL
C     NLEQAL(PW1,PW2)=(PW1.XOR.PW2).EQ.0B
C
C     2.2 BITWISE AND AND OR
C
      IAND(K1,K2)=AND(K1,K2)
      IOR (K1,K2)= OR(K1,K2)
C
C     2.3 BITWISE NEGATION
C
      NOT(K)=COMPL(K)
C
C     2.4 SHIFT (LEFT FOR KSH POSITIVE, RIGHT FOR KSH NEGATIVE)
C
      ISHFT(K,KSH)=CVMGP(SHIFTL(K,KSH),SHIFTR(K,-KSH),KSH)
C
C     3.  SPECIAL PURPOSE
C
C     3.1 TAKE 4 LAST BITS OF KW, PUT THEM IN PW AT POS K*4-1
C
C     SETLEV(PW,KW,K)=OR(AND(PW,SHIFT(0B.EQV.17B,K*4-4)),
C    +SHIFT(AND(17B,KW),K*4-4))
C
C     3.2 EXTRACT FIELD K*4-1:4 FROM PW
C
C     MGTLEV(PW,K)=AND(17B,SHIFT(PW,68-K*4))
