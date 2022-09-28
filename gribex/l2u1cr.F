C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE L2U1CR (HSTRNG)
C
C---->
C**** L2U1CR - Convert character string to upper case.
C
C     Purpose.
C     --------
C
C           Convert character string to all upper case.
C
C**   Interface.
C     ----------
C
C           CALL L2U1CR (HSTRNG)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input parameters.
C               -----------------
C
C               HSTRNG     - String in upper and/or lower case.
C
C               Output parameters.
C               ------------------
C
C               HSTRNG     - Character string in all upper case.
C
C     Method.
C     -------
C
C           Numeric values of each byte examined and altered if in the
C           range of lower case letters.
C
C     Externals.
C     ----------
C
C           ICHAR
C           CHAR
C
C     Reference.
C     ----------
C
C           Cray Programmer's Library Reference Manual.
C
C     Comments.
C     ---------
C
C           Routine contains sections 0 to 1 and section 9.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      07:04:89.
C
C     Modifications
C     --------------
C
C           J. Hennessy      ECMWF      29.01.90
C           Replace null characters by blanks.
C
C     ----------------------------------------------------------------
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
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
      CHARACTER*(*) HSTRNG
C
C     ASCII representation of lower case characters a and z.
C
      DATA IA  /97/
      DATA IZ  /122/
C
C     ASCII representation of null and blank characters.
C
      DATA INULL /0/
C
C     ----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C*    Section 1 . Change any necessary letters.
C     ----------------------------------------------------------------
C
 1000 CONTINUE
C
C
      ILEN = LEN (HSTRNG)
C
      DO 1100 J=1,ILEN
         ICH = ICHAR (HSTRNG(J:J))
         IF ( (ICH.GE.IA).AND.(ICH.LE.IZ) ) HSTRNG (J:J) = CHAR (ICH-32)
         IF (ICH.EQ.INULL) HSTRNG (J:J) = CHAR (32)
 1100 CONTINUE
C
C     ---------------------------------------------------------------
C
C
C
C
C
C
C*    Section 9 . Return to calling routine.
C     ----------------------------------------------------------------
C
 9000 CONTINUE
C
      RETURN
      END
