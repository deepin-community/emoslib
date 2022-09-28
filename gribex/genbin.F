C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GENBIN (IN,NBIT,OUT,ERR)
C
C---->
C**********************************************************************
C*
C*  NAME      : GENBIN
C*
C*  FUNCTION  : PRODUCES A BINARY NUMBER WITH ONES AND ZEROES
C*              CORRESPONDING TO THE ONES AND ZEROES OF THE INPUT
C*              DECIMAL NUMBER.
C*              EG INPUT NUMBER 1010 DECIMAL , OUTPUT 1010 BINARY.
C*
C*  INPUT     : IN   - INTEGER VARIABLE CONTAINING DECIMAL NUMBER.
C*              NBIT - NUMBER OF DIGITS TO BE USED.
C*
C*  OUTPUT    : OUT  - INTEGER VARIABLE CONTAINING BINARY VALUE
C*                     WITH ONES AND ZEROES CORRESPONDING TO THOSE OF
C*                     THE INPUT DECIMAL NUMBER.
C*
C*              ERR  - 0 IF NO ERROR, 1 IF INVALID NUMBER OF BITS.
C*
C*    JOHN HENNESSY     ECMWF, OCTOBER 1985
C*
C**********************************************************************
C----<
C
      IMPLICIT INTEGER (A-Z)
C
      OUT = 0
      INN = IN
C
C     CHECK SIZE OF INTEGER
C
      IF (NBIT.LT.0.OR.NBIT.GT.15)
     C   THEN
             ERR = 1
             RETURN
         ELSE
             ERR = 0
         ENDIF
C
         DO 100 I=1,NBIT
            TEMP = INN / 10
            DIFF = INN - (TEMP*10)
            K    = I - 1
            MASC = 2**K
            IF (DIFF.NE.0) OUT = OR(OUT,MASC)
            INN = TEMP
  100    CONTINUE
C
      RETURN
      END
