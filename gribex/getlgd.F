C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GETLGD (KLGD,OLND,KLEN,KARAY,KPR,KRET)
C
C---->
C     ----------------------------------------------------------------
C
C****
C
C     PURPOSE.
C     --------
C
C           GET LOGICAL DATA FROM RECORD OF UNPACKED GRIB
C           OR PSEUDO-GRIB DATA.
C
C**   INTERFACE.
C     ----------
C
C           CALL GETLGD (KLGD,OLND,KLEN,KARAY,KPR,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               INPUT PARAMETERS.
C               -----------------
C
C               OLND       - ARRAY TO RECEIVE LOGICAL DATA.
C               KLEN       - LENGTH OF THIS ARRAY.
C               KARAY      - ARRAY CONTAINING UNPACKED RECORD.
C               KPR        - DEBUG PRINT SWITCH.
C                            0 , NO PRINTOUT.
C                            1 , DEBUG PRINTOUT.
C
C               KRET       - ABORT/NO ABORT WHEN ERROR FOUND.
C                            0   , ABORT IF ERROR FOUND.
C                            NON-ZERO , NO ABORT ON ERROR.
C
C               OUTPUT PARAMETERS.
C               ------------------
C
C               KRET       - ERROR RETURN CODE.
C                            0   , NO ERROR ENCOUNTERED.
C                            1   , TARGET ARRAY TOO SMALL.
C               OLND       - LOGICAL DATA.
C               KLGD       - NUMBER OF VALUES IN THIS ARRAY.
C
C     METHOD.
C     -------
C
C           UNPACKED DATA IS IN SELF-DEFINING RECORDS. THE REQUIRED
C           VALUES ARE LOCATED AND EXTRACTED.
C
C     EXTERNALS.
C     ----------
C
C           NONE.
C
C     REFERENCE.
C     ----------
C
C           SEE MARS FORMAT DOCUMENTATION RE GRIB, PSEUDO-GRIB AND
C           UNPACKED RECORD FORMATS.
C
C     COMMENTS.
C     ---------
C
C           PROGRAM CONTAINS SECTIONS 0 TO 2 AND SECTION 9.
C
C     AUTHOR.
C     -------
C
C           J. HENNESSY      ECMWF      18:06:86.
C
C     MODIFICATIONS
C     --------------
C           NONE.
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
C*    SECTION 0 . DEFINITION OF VARIABLES.
C     ------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
      DIMENSION KARAY(*)
      DIMENSION OLND(KLEN)
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
C*    SECTION 1 . SET INITIAL VALUES. CHECK INPUT PARAMETERS.
C     -------------------------------------------------------
C
 1000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETLGD : SECTION 1.'
C
      IRET = KRET
      KRET = 0
C
C
C*    GO TO SECTION 9 , IF ERROR IN PARAMETERS.
C
      IF (KRET.NE.0) GO TO 9000
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
C*    SECTION 2 . LOCATE AND EXTRACT VALUES.
C     --------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETLGD : SECTION 2.'
C
      ILB1 = KARAY(2)
      ILB2 = KARAY(2+ILB1+1)
      ILF2 = KARAY(2+ILB1+1+ILB2+1)
      ILB3 = KARAY(2+ILB1+1+ILB2+1+ILF2+1)
      ILCD = KARAY(2+ILB1+1+ILB2+1+ILF2+1+ILB3+1)
      ILFD = KARAY(2+ILB1+1+ILB2+1+ILF2+1+ILB3+1+ILCD+1)
      ILID = KARAY(2+ILB1+1+ILB2+1+ILF2+1+ILB3+1+ILCD+1+ILFD+1)
      KLGD = KARAY(2+ILB1+1+ILB2+1+ILF2+1+ILB3+1+ILCD+1+ILFD+1+ILID+1)
C
      IF (KLEN.LT.KLGD)
     C   THEN
             KRET = 1
             WRITE (*,9001) KRET , KLGD
             GO TO 9000
         ENDIF
C
      DO 2010 J=1,KLGD
         IVAL    = KARAY(J+2+ILB1+1+ILB2+1+ILF2+1+ILB3+1+ILCD+1+ILFD+1
     C                    +KLGD+1)
         OLND(J) = .FALSE.
         IF (IVAL.EQ.1) OLND(J) = .TRUE.
 2010 CONTINUE
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
C*    SECTION 9 . RETURN TO CALLING ROUTINE OR ABORT. FORMAT STATEMENTS.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETLGD : SECTION 9.'
C
      IF (KRET.NE.0.AND.IRET.EQ.0) CALL ABORT
C
      RETURN
C
C
 9001 FORMAT (1H ,'GETLGD : ERROR CODE = ',I3,'. TARGET ARRAY MUST ',
     C            'BE AT LEAST ',I5,' WORDS LONG.')
C
C
C     ---------------------------------------------------------------
C
C
      END
C
