C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CSGNBT( KOUT, KIN, KPOS, KRET)
C---->
C
C     Set the sign bit in position KPOS of word KOUT, and
C     set the rest of bits (KPOS-1) -> using +ve value of 
C     KIN.
C
C     On return, KRET = 0 if all OK.
C
C----<
      IMPLICIT NONE
      INTEGER KOUT, KIN, KPOS, KRET

      INTEGER JPBITS
#ifdef INTEGER_8
      PARAMETER ( JPBITS = 64 )
#else
      PARAMETER ( JPBITS = 32 )
#endif
      INTEGER ISBIT, IVALUE, INSPT, IRETA, IRETB

      IF ( KIN .LT. 0 ) THEN
        IVALUE = -KIN
        ISBIT = 1
      ELSE
        IVALUE = KIN
        ISBIT = 0
      ENDIF

#if defined (VAX) || defined (__alpha)
C
C     VAX bit handling with VAX specific intrinsics
      KOUT = JISHFT( ISBIT, KPOS-1)
      KOUT = KOUT + IVALUE
#else
C
C     Bit handling using INXBIT
      INSPT = 0
      CALL INXBIT( KOUT, 1, INSPT, ISBIT, 1, JPBITS,
     X             (JPBITS-KPOS+1), 'C', IRETA)
      CALL INXBIT( KOUT, 1, INSPT, IVALUE, 1, JPBITS,
     X             (KPOS-1), 'C', IRETB)
      KRET = IRETA+IRETB
      IF ( KRET .NE. 0 ) THEN
        WRITE(*,*) ' CSGNBT - Problem packing bits in value'
        GOTO 900
      ENDIF
#endif

  900 CONTINUE

      RETURN
      END
