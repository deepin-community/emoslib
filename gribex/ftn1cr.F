C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE FTN1CR ( KNUM , KPR ,KRET )
C
C---->
C**** FTN1CR
C
C     PURPOSE.
C     --------
C
C           GIVES FORTRAN UNIT NUMBER NOT CURRENTLY IN USE.
C
C**   INTERFACE.
C     ----------
C
C           CALL FTN1CR ( KNUM , KPR ,KRET )
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               INPUT PARAMETERS.
C               -----------------
C
C               KPR        - DEBUG PRINT SWITCH.
C                            0 , NO PRINTOUT.
C                            1 , DEBUG PRINTOUT.
C
C               OUTPUT PARAMETERS.
C               ------------------
C
C               KNUM       - FREE FORTRAN UNIT NUMBER.
C               KRET       - ERROR RETURN CODE.
C                            0   , NO ERROR ENCOUNTERED.
C                            1   , NO UNIT NUMBER FREE.
C
C     METHOD.
C     -------
C
C           INQUIRE STATEMENT ON CRAY ALIAS USED.
C
C     EXTERNALS.
C     ----------
C
C           NONE.
C
C     REFERENCE.
C     ----------
C
C           NONE.
C
C     COMMENTS.
C     ---------
C
C           ROUTINE CONTAINS SECTIONS 0 , 2  AND SECTION 9.
C
C     AUTHOR.
C     -------
C
C           J. HENNESSY      ECMWF      12.07.89.
C
C     MODIFICATIONS
C     --------------
C
C           NONE.
C
C----<
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
C
C*    SECTION 0 . DEFINITION OF VARIABLES.
C     ----------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
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
C*    SECTION 1 . SET INITIAL VALUES.
C     ----------------------------------------------------------------
C
 1000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'FTN1CR : SECTION 1.'
C
      KRET = 0
C
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
C*    SECTION 2 . INQUIRE FOR FREE UNIT.
C     ----------------------------------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'FTN1CR : SECTION 2.'
C
      DO 2010 J2010 = 99,1,-1
         IF (J2010.EQ.5) GO TO 2010
         IF (J2010.EQ.6) GO TO 2010
         WRITE (YALIAS,'(A,I2.2)') 'FT',J2010
         INQUIRE (FILE=YALIAS,EXIST=LEX)
         IF (LEX) GO TO 2010
         KNUM = J2010
         GO TO 2050
 2010 CONTINUE
C
C     NO FREE UNIT FOUND.
C
      KRET = 1
C
      WRITE (*,9001)
C
 2050 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,9002) KNUM
C
      GO TO 9000
C
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
C
C
C
C
C
C
C*    SECTION 9 . RETURN TO CALLING ROUTINE. FORMAT STATEMENTS.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'FTN1CR : SECTION 9.'
C
C
      RETURN
C
C
 9001 FORMAT (1H ,'FTN1CR : NO FORTRAN UNIT AVAILABLE.')
C
 9002 FORMAT (1H ,'FTN1CR : FORTRAN UNIT ALLOCATED = ',I3)
C
C
C     ------------------------------------------------------------------
C
C
      END
