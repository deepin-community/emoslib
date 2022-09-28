C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE GETCHD (KLCHD,HCHD,KLENG,KARRAY,KPR,KRET)
C
C---->
C     ----------------------------------------------------------------
C
C****
C
C     PURPOSE.
C     --------
C
C           GET CHARACTER DATA FROM UNPACKED GRIB OR
C           PSEUDO-GRIB RECORDS, RETRIEVED FROM MARS.
C
C**   INTERFACE.
C     ----------
C
C           CALL GETCHD (KLCHD,HCHD,KLENG,KARRAY,KPR,KRET)
C
C           INTEGER    K.
C           REAL       P.
C           LOGICAL    O.
C           CHARACTER  H.
C
C               INPUT PARAMETERS.
C               -----------------
C
C               HCHD       - STRING TO RECEIVE CHARACTER DATA.
C               KLENG      - LENGTH OF THIS STRING.
C               KARRAY     - ARRAY CONTAINING UNPACKED RECORD.
C               KPR        - DEBUG PRINT SWITCH.
C                            0 , NO PRINTOUT.
C                            1 , DEBUG PRINTOUT.
C
C               KRET       - ABORT/NO ABORT WHEN ERROR FOUND.
C                            0   , ABORT IF ERROR FOUND.
C                            NON-ZERO , NO ABORT IF ERROR FOUND.
C
C               OUTPUT PARAMETERS.
C               ------------------
C
C               HCHD       - STRING OF CHARACTER DATA.
C               KLCHD      - NUMBER OF CHARACTERS IN THIS STRING.
C               KRET       - ERROR RETURN CODE.
C                            0   , NO ERROR ENCOUNTERED.
C                            1   , TARGET STRING TOO SHORT.
C
C     METHOD.
C     -------
C
C           UNPACKED DATA IS IN SELF DEFINING RECORDS. THE REQUIRED
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
C           UNPACKED FORMATS.
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
C     ---------------------------------------------------------------
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
      DIMENSION KARRAY(*)
C
      CHARACTER*(*) HCHD
C
C
C     ---------------------------------------------------------------
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
C*    SECTION 1 . SET INITIAL VALUES. CHECK INPUT PARAMETERS.
C     -------------------------------------------------------
C
 1000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETCHD : SECTION 1.'
C
      IRET = KRET
      KRET = 0
C
C     --------------------------------------------------------------
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
C*    SECTION 2 . EXTRACT VALUES . PRINT IF REQUIRED.
C     -----------------------------------------------
C
 2000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETCHD : SECTION 2.'
C
      ILIB1 = KARRAY(2)
      ILIB2 = KARRAY(2+ILIB1+1)
      ILFB2 = KARRAY(2+ILIB1+1+ILIB2+1)
      ILIB3 = KARRAY(2+ILIB1+1+ILIB2+1+ILFB2+1)
      KLCHD = KARRAY(2+ILIB1+1+ILIB2+1+ILFB2+1+ILIB3+1)
C
      IF (KLENG.LT.KLCHD)
     C   THEN
             KRET = 1
             WRITE (*,9001) KRET , KLCHD
             GO TO 9000
         ENDIF
C
      DO 2010 J=1,KLCHD
         IVAL    = KARRAY(J+2+ILIB1+1+ILIB2+1+ILFB2+1+ILIB3+1)
         READ (IVAL,'(A1)') HCHD(J:J)
 2010 CONTINUE
C
C
C     ------------------------------------------------------------------
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
C*    SECTION 9 . RETURN TO CALLING ROUTINE OR ABORT. FORMAT STATEMENTS.
C     ------------------------------------------------------------------
C
 9000 CONTINUE
C
      IF (KPR.EQ.1) WRITE (*,*) 'GETCHD : SECTION 9.'
C
      IF (KRET.NE.0.AND.IRET.EQ.0) CALL ABORT
C
      RETURN
C
C
 9001 FORMAT (1H ,'GETCHD : ERROR CODE = ',I3,'. TARGET STRING MUST BE',
     C             ' AT LEAST ',I5,' CHARACTERS LONG.')
C
C     ------------------------------------------------------------------
C
C
      END
