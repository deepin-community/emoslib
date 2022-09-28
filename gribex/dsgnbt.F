C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE DSGNBT( KOUT, KIN, KPOS, KRET)
C---->
C
C     Get the sign bit in position KPOS of word KIN, and
C     set value of KOUT from the rest of bits (KPOS-1) ->
C     with the correct sign.
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
      INTEGER ISBIT, INSPT, IRETA, IRETB
#if defined (VAX) || defined (__alpha)
      INTEGER ITEMP, IMASK1,IMASK2
#endif
C
#if defined (VAX) || defined (__alpha)
C
C     VAX bit handling using VAX specific intrinsics
      ITEMP  = 1
      IMASK1 = JISHFT( ITEMP, (KPOS-1))
      ITEMP  = -1
      ITEMP  = JISHFT( ITEMP, (KPOS-1) )
      IMASK2 = JNOT( ITEMP )
      ISBIT  = JIAND( KIN, IMASK1)
      KOUT   = JIAND( KIN, IMASK2)
#else
C
C     Bit handling using INXBIT
      INSPT = 0
      CALL INXBIT( KIN, 1, INSPT, ISBIT, 1, JPBITS,
     X             (JPBITS-KPOS+1), 'D', IRETA)
      CALL INXBIT( KIN, 1, INSPT, KOUT, 1, JPBITS,
     X             (KPOS-1), 'D', IRETB)
      KRET = IRETA+IRETB
      IF ( KRET .NE. 0 ) THEN
        WRITE(*,*) ' DSGNBT - Problem unpacking bits in value'
        GOTO 900
      ENDIF
#endif

      IF ( ISBIT .NE. 0 ) KOUT = -KOUT

  900 CONTINUE

      RETURN
      END
